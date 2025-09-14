/* Compile .zi time zone data into TZif binary files.  */

/*
** This file is in the public domain, so clarified as of
** 2006-07-17 by Arthur David Olson.
*/

/* Use the system 'time' function, instead of any private replacement.
   This avoids creating an unnecessary dependency on localtime.c.  */
#undef EPOCH_LOCAL
#undef EPOCH_OFFSET
#undef RESERVE_STD_EXT_IDS
#undef time_tz

#include "version.h"
#include "private.h"
#include "tzdir.h"
#include "tzfile.h"

#include <fcntl.h>
#include <locale.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>

typedef int_fast64_t	zic_t;
static zic_t const
  ZIC_MIN = INT_FAST64_MIN,
  ZIC_MAX = INT_FAST64_MAX,
  ZIC32_MIN = -1 - (zic_t) 0x7fffffff,
  ZIC32_MAX = 0x7fffffff;
#define SCNdZIC SCNdFAST64

#ifndef ZIC_MAX_ABBR_LEN_WO_WARN
# define ZIC_MAX_ABBR_LEN_WO_WARN 6
#endif /* !defined ZIC_MAX_ABBR_LEN_WO_WARN */

/* Minimum and maximum years, assuming signed 32-bit time_t.  */
enum { YEAR_32BIT_MIN = 1901, YEAR_32BIT_MAX = 2038 };

/* An upper bound on how much a format might grow due to concatenation.  */
enum { FORMAT_LEN_GROWTH_BOUND = 5 };

#ifdef HAVE_DIRECT_H
# include <direct.h>
# include <io.h>
# undef mkdir
# define mkdir(name, mode) _mkdir(name)
#endif

#ifndef HAVE_GETRANDOM
# ifdef __has_include
#  if __has_include(<sys/random.h>)
#   include <sys/random.h>
#  endif
# elif 2 < __GLIBC__ + (25 <= __GLIBC_MINOR__)
#  include <sys/random.h>
# endif
# define HAVE_GETRANDOM GRND_RANDOM
#elif HAVE_GETRANDOM
# include <sys/random.h>
#endif

#if HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
#ifdef S_IRUSR
# define MKDIR_UMASK (S_IRUSR|S_IWUSR|S_IXUSR|S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH)
#else
# define MKDIR_UMASK 0755
#endif

/* The minimum alignment of a type, for pre-C23 platforms.
   The __SUNPRO_C test is because Oracle Developer Studio 12.6 lacks
   <stdalign.h> even though __STDC_VERSION__ == 201112.  */
#if __STDC_VERSION__ < 201112 || defined __SUNPRO_C
# define alignof(type) offsetof(struct { char a; type b; }, b)
#elif __STDC_VERSION__ < 202311
# include <stdalign.h>
#endif

/* The maximum length of a text line, including the trailing newline.  */
#ifndef _POSIX2_LINE_MAX
# define _POSIX2_LINE_MAX 2048
#endif

/* The type for line numbers.  Use PRIdMAX to format them; formerly
   there was also "#define PRIdLINENO PRIdMAX" and formats used
   PRIdLINENO, but xgettext cannot grok that.  */
typedef intmax_t lineno;

struct rule {
	int		r_filenum;
	lineno		r_linenum;
	const char *	r_name;

	zic_t		r_loyear;	/* for example, 1986 */
	zic_t		r_hiyear;	/* for example, 1986 */
	bool		r_hiwasnum;

	int		r_month;	/* 0..11 */

	int		r_dycode;	/* see below */
	int		r_dayofmonth;
	int		r_wday;

	zic_t		r_tod;		/* time from midnight */
	bool		r_todisstd;	/* is r_tod standard time? */
	bool		r_todisut;	/* is r_tod UT? */
	bool		r_isdst;	/* is this daylight saving time? */
	zic_t		r_save;		/* offset from standard time */
	const char *	r_abbrvar;	/* variable part of abbreviation */

	bool		r_todo;		/* a rule to do (used in outzone) */
	zic_t		r_temp;		/* used in outzone */
};

/*
** r_dycode	r_dayofmonth	r_wday
*/
enum {
  DC_DOM,	/* 1..31 */	/* unused */
  DC_DOWGEQ,	/* 1..31 */	/* 0..6 (Sun..Sat) */
  DC_DOWLEQ	/* 1..31 */	/* 0..6 (Sun..Sat) */
};

struct zone {
	int		z_filenum;
	lineno		z_linenum;

	const char *	z_name;
	zic_t		z_stdoff;
	char *		z_rule;
	const char *	z_format;
	char		z_format_specifier;

	bool		z_isdst;
	zic_t		z_save;

	struct rule *	z_rules;
	ptrdiff_t	z_nrules;

	struct rule	z_untilrule;
	zic_t		z_untiltime;
};

#if !HAVE_POSIX_DECLS
extern int	getopt(int argc, char * const argv[],
			const char * options);
extern int	link(const char * target, const char * linkname);
extern char *	optarg;
extern int	optind;
#endif

#if ! HAVE_SYMLINK
static ssize_t
readlink(char const *restrict file, char *restrict buf, size_t size)
{
  (void)file;
  (void)buf;
  (void)size;
  errno = ENOTSUP;
  return -1;
}
static int
symlink(char const *target, char const *linkname)
{
  (void)target;
  (void)linkname;
  errno = ENOTSUP;
  return -1;
}
#endif
#ifndef AT_SYMLINK_FOLLOW
#  define linkat(targetdir, target, linknamedir, linkname, flag) \
     (errno = ENOTSUP, -1)
#endif

static void	addtt(zic_t starttime, int type);
static int	addtype(zic_t, char const *, bool, bool, bool);
static void	leapadd(zic_t, int, int);
static void	adjleap(void);
static void	associate(void);
static void	dolink(const char *, const char *, bool);
static int	getfields(char *, char **, int);
static zic_t	gethms(const char * string, const char * errstring);
static zic_t	getsave(char *, bool *);
static void	inexpires(char **, int);
static void	infile(int, char const *);
static void	inleap(char ** fields, int nfields);
static void	inlink(char ** fields, int nfields);
static void	inrule(char ** fields, int nfields);
static bool	inzcont(char ** fields, int nfields);
static bool	inzone(char ** fields, int nfields);
static bool	inzsub(char **, int, bool);
static int	itssymlink(char const *, int *);
static bool	is_alpha(char a);
static char	lowerit(char);
static void	mkdirs(char const *, bool);
static void	newabbr(const char * abbr);
static zic_t	oadd(zic_t t1, zic_t t2);
static void	outzone(const struct zone * zp, ptrdiff_t ntzones);
static zic_t	rpytime(const struct rule * rp, zic_t wantedy);
static bool	rulesub(struct rule * rp,
			const char * loyearp, const char * hiyearp,
			const char * typep, const char * monthp,
			const char * dayp, const char * timep);
static zic_t	tadd(zic_t t1, zic_t t2);

/* Bound on length of what %z can expand to.  */
enum { PERCENT_Z_LEN_BOUND = sizeof "+995959" - 1 };

static int		charcnt;
static bool		errors;
static bool		warnings;
static int		filenum;
static int		leapcnt;
static bool		leapseen;
static zic_t		leapminyear;
static zic_t		leapmaxyear;
static lineno		linenum;
static int		max_abbrvar_len = PERCENT_Z_LEN_BOUND;
static int		max_format_len;
static zic_t		max_year;
static zic_t		min_year;
static bool		noise;
static int		rfilenum;
static lineno		rlinenum;
static const char *	progname;
static char const *	leapsec;
static char *const *	main_argv;
static ptrdiff_t	timecnt;
static ptrdiff_t	timecnt_alloc;
static int		typecnt;
static int		unspecifiedtype;

/*
** Line codes.
*/

enum {
  LC_RULE,
  LC_ZONE,
  LC_LINK,
  LC_LEAP,
  LC_EXPIRES
};

/*
** Which fields are which on a Zone line.
*/

enum {
  ZF_NAME = 1,
  ZF_STDOFF,
  ZF_RULE,
  ZF_FORMAT,
  ZF_TILYEAR,
  ZF_TILMONTH,
  ZF_TILDAY,
  ZF_TILTIME,
  ZONE_MAXFIELDS,
  ZONE_MINFIELDS = ZF_TILYEAR
};

/*
** Which fields are which on a Zone continuation line.
*/

enum {
  ZFC_STDOFF,
  ZFC_RULE,
  ZFC_FORMAT,
  ZFC_TILYEAR,
  ZFC_TILMONTH,
  ZFC_TILDAY,
  ZFC_TILTIME,
  ZONEC_MAXFIELDS,
  ZONEC_MINFIELDS = ZFC_TILYEAR
};

/*
** Which files are which on a Rule line.
*/

enum {
  RF_NAME = 1,
  RF_LOYEAR,
  RF_HIYEAR,
  RF_COMMAND,
  RF_MONTH,
  RF_DAY,
  RF_TOD,
  RF_SAVE,
  RF_ABBRVAR,
  RULE_FIELDS
};

/*
** Which fields are which on a Link line.
*/

enum {
  LF_TARGET = 1,
  LF_LINKNAME,
  LINK_FIELDS
};

/*
** Which fields are which on a Leap line.
*/

enum {
  LP_YEAR = 1,
  LP_MONTH,
  LP_DAY,
  LP_TIME,
  LP_CORR,
  LP_ROLL,
  LEAP_FIELDS,

  /* Expires lines are like Leap lines, except without CORR and ROLL fields.  */
  EXPIRES_FIELDS = LP_TIME + 1
};

/* The maximum number of fields on any of the above lines.
   (The "+"s pacify gcc -Wenum-compare.)  */
enum {
  MAX_FIELDS = max(max(+RULE_FIELDS, +LINK_FIELDS),
		   max(+LEAP_FIELDS, +EXPIRES_FIELDS))
};

/*
** Year synonyms.
*/

enum {
  YR_MINIMUM, /* "minimum" is for backward compatibility only */
  YR_MAXIMUM,
  YR_ONLY
};

static struct rule *	rules;
static ptrdiff_t	nrules;	/* number of rules */
static ptrdiff_t	nrules_alloc;

static struct zone *	zones;
static ptrdiff_t	nzones;	/* number of zones */
static ptrdiff_t	nzones_alloc;

struct link {
	int		l_filenum;
	lineno		l_linenum;
	const char *	l_target;
	const char *	l_linkname;
};

static struct link *	links;
static ptrdiff_t	nlinks;
static ptrdiff_t	nlinks_alloc;

struct lookup {
	const char *	l_word;
	const int	l_value;
};

static struct lookup const *	byword(const char * string,
					const struct lookup * lp);

static struct lookup const zi_line_codes[] = {
	{ "Rule",	LC_RULE },
	{ "Zone",	LC_ZONE },
	{ "Link",	LC_LINK },
	{ NULL,		0 }
};
static struct lookup const leap_line_codes[] = {
	{ "Leap",	LC_LEAP },
	{ "Expires",	LC_EXPIRES },
	{ NULL,		0}
};

static struct lookup const	mon_names[] = {
	{ "January",	TM_JANUARY },
	{ "February",	TM_FEBRUARY },
	{ "March",	TM_MARCH },
	{ "April",	TM_APRIL },
	{ "May",	TM_MAY },
	{ "June",	TM_JUNE },
	{ "July",	TM_JULY },
	{ "August",	TM_AUGUST },
	{ "September",	TM_SEPTEMBER },
	{ "October",	TM_OCTOBER },
	{ "November",	TM_NOVEMBER },
	{ "December",	TM_DECEMBER },
	{ NULL,		0 }
};

static struct lookup const	wday_names[] = {
	{ "Sunday",	TM_SUNDAY },
	{ "Monday",	TM_MONDAY },
	{ "Tuesday",	TM_TUESDAY },
	{ "Wednesday",	TM_WEDNESDAY },
	{ "Thursday",	TM_THURSDAY },
	{ "Friday",	TM_FRIDAY },
	{ "Saturday",	TM_SATURDAY },
	{ NULL,		0 }
};

static struct lookup const	lasts[] = {
	{ "last-Sunday",	TM_SUNDAY },
	{ "last-Monday",	TM_MONDAY },
	{ "last-Tuesday",	TM_TUESDAY },
	{ "last-Wednesday",	TM_WEDNESDAY },
	{ "last-Thursday",	TM_THURSDAY },
	{ "last-Friday",	TM_FRIDAY },
	{ "last-Saturday",	TM_SATURDAY },
	{ NULL,			0 }
};

static struct lookup const	begin_years[] = {
	{ "minimum",	YR_MINIMUM },
	{ NULL,		0 }
};

static struct lookup const	end_years[] = {
	{ "maximum",	YR_MAXIMUM },
	{ "only",	YR_ONLY },
	{ NULL,		0 }
};

static struct lookup const	leap_types[] = {
	{ "Rolling",	true },
	{ "Stationary",	false },
	{ NULL,		0 }
};

static const int	len_months[2][MONSPERYEAR] = {
	{ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 },
	{ 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }
};

static const int	len_years[2] = {
	DAYSPERNYEAR, DAYSPERLYEAR
};

static struct attype {
	zic_t		at;
	bool		dontmerge;
	unsigned char	type;
} *			attypes;
static zic_t		utoffs[TZ_MAX_TYPES];
static char		isdsts[TZ_MAX_TYPES];
static unsigned char	desigidx[TZ_MAX_TYPES];
static bool		ttisstds[TZ_MAX_TYPES];
static bool		ttisuts[TZ_MAX_TYPES];
static char		chars[TZ_MAX_CHARS];
static zic_t		trans[TZ_MAX_LEAPS];
static zic_t		corr[TZ_MAX_LEAPS];
static char		roll[TZ_MAX_LEAPS];

/*
** Memory allocation.
*/

ATTRIBUTE_NORETURN static void
memory_exhausted(const char *msg)
{
	const char * const details = msg ? msg : "an unknown operation";

	(void)fprintf(stderr, _("%s: Memory exhausted: %s\n"), progname, details);
	exit(EXIT_FAILURE);
}

ATTRIBUTE_NORETURN static void
size_overflow(void)
{
  memory_exhausted(_("size overflow"));
}

ATTRIBUTE_PURE_114833 static ptrdiff_t
size_sum(size_t a, size_t b)
{
#ifdef ckd_add
  ptrdiff_t sum;
  if (!ckd_add(&sum, a, b) && sum <= INDEX_MAX)
    return sum;
#else
  if (a <= INDEX_MAX && b <= INDEX_MAX - a)
    return a + b;
#endif
  size_overflow();
}

ATTRIBUTE_PURE_114833 static ptrdiff_t
size_product(ptrdiff_t nitems, ptrdiff_t itemsize)
{
#ifdef ckd_mul
  ptrdiff_t product;
  if (!ckd_mul(&product, nitems, itemsize) && product <= INDEX_MAX)
    return product;
#else
  ptrdiff_t nitems_max = INDEX_MAX / itemsize;
  if (nitems <= nitems_max)
    return nitems * itemsize;
#endif
  size_overflow();
}

ATTRIBUTE_PURE_114833 static ptrdiff_t
align_to(ptrdiff_t size, ptrdiff_t alignment)
{
  ptrdiff_t lo_bits = alignment - 1, sum = size_sum(size, lo_bits);
  return sum & ~lo_bits;
}

#if !HAVE_STRDUP
static char *
strdup(char const *str)
{
    if (str == NULL)
    {
        return NULL;
    }

    size_t len = strlen(str);
    char *result = malloc(len + 1);

    if (result == NULL)
    {
        return NULL;
    }

    memcpy(result, str, len + 1);
    return result;
}
#endif

static void *
memcheck(void *ptr)
{
	if (ptr == NULL) {
		const int err = HAVE_MALLOC_ERRNO ? errno : ENOMEM;
		memory_exhausted(strerror(err));
	}
	return ptr;
}

#include <stdio.h>
#include <stdlib.h>

static void *
emalloc(size_t size)
{
    void *ptr = malloc(size);
    if (ptr == NULL && size > 0) {
        fprintf(stderr, "Fatal: memory allocation failed.\n");
        exit(EXIT_FAILURE);
    }
    return ptr;
}

static void *
erealloc(void *ptr, size_t size)
{
    if (size == 0) {
        free(ptr);
        return NULL;
    }

    void *new_ptr = realloc(ptr, size);
    return memcheck(new_ptr);
}

static char *
estrdup(char const *str)
{
    if (str == NULL) {
        return NULL;
    }

    char *new_str = strdup(str);
    if (new_str == NULL) {
        perror("strdup failed");
        exit(EXIT_FAILURE);
    }

    return new_str;
}

static ptrdiff_t
grow_nitems_alloc(ptrdiff_t *nitems_alloc, ptrdiff_t itemsize)
{
    ptrdiff_t new_nitems;
    ptrdiff_t new_size_bytes;

    if (itemsize <= 0) {
        goto fail;
    }

    ptrdiff_t current_nitems = *nitems_alloc;
    ptrdiff_t addend = (current_nitems >> 1) + 1;

#if defined ckd_add && defined ckd_mul
    if (ckd_add(&new_nitems, current_nitems, addend)
        || ckd_mul(&new_size_bytes, new_nitems, itemsize)
        || new_size_bytes > INDEX_MAX) {
        goto fail;
    }
#else
    if (current_nitems > PTRDIFF_MAX - addend) {
        goto fail;
    }
    new_nitems = current_nitems + addend;

    if (new_nitems > INDEX_MAX / itemsize) {
        goto fail;
    }
    new_size_bytes = new_nitems * itemsize;
#endif

    *nitems_alloc = new_nitems;
    return new_size_bytes;

fail:
    memory_exhausted(_("integer overflow"));
}

static void *
growalloc(void *ptr, size_t itemsize, size_t nitems,
          size_t *nitems_alloc)
{
    if (nitems < *nitems_alloc)
    {
        return ptr;
    }

    size_t new_capacity = (*nitems_alloc > 0) ? *nitems_alloc : 16;

    while (new_capacity <= nitems)
    {
        if (new_capacity > SIZE_MAX / 2)
        {
            new_capacity = SIZE_MAX;
            break;
        }
        new_capacity *= 2;
    }

    if (itemsize > 0 && new_capacity > SIZE_MAX / itemsize)
    {
        return erealloc(ptr, SIZE_MAX);
    }

    size_t new_size = new_capacity * itemsize;
    void *new_ptr = erealloc(ptr, new_size);

    *nitems_alloc = new_capacity;

    return new_ptr;
}

/*
** Error handling.
*/

/* In most of the code, an input file name is represented by its index
   into the main argument vector, except that LEAPSEC_FILENUM stands
   for leapsec and COMMAND_LINE_FILENUM stands for the command line.  */
enum { LEAPSEC_FILENUM = -2, COMMAND_LINE_FILENUM = -1 };

/* Return the name of the Ith input file, for diagnostics.  */
static char const *
filename(int i)
{
    if (i == COMMAND_LINE_FILENUM)
    {
        return _("command line");
    }

    char const *fname = (i == LEAPSEC_FILENUM) ? leapsec : main_argv[i];

    if (fname && strcmp(fname, "-") == 0)
    {
        return _("standard input");
    }

    return fname;
}

static void update_source_position(int file_index, lineno line_number, int reference_file_index, lineno reference_line_number)
{
	filenum = file_index;
	linenum = line_number;
	rfilenum = reference_file_index;
	rlinenum = reference_line_number;
}

static const int ENTIRE_LINE = -1;
static const int START_COLUMN = 0;

static void
eat(int fnum, lineno num)
{
	eats(fnum, num, START_COLUMN, ENTIRE_LINE);
}

ATTRIBUTE_FORMAT((printf, 1, 0)) static void
verror(const char *const string, va_list args)
{
	/*
	** Match the format of "cc" to allow sh users to
	**	zic ... 2>&1 | error -t "*" -v
	** on BSD systems.
	*/
	if (filenum) {
		if (fprintf(stderr, _("\"%s\", line %"PRIdMAX": "),
			    filename(filenum), linenum) < 0) {
			return;
		}
	}

	if (vfprintf(stderr, string, args) < 0) {
		return;
	}

	if (rfilenum) {
		if (fprintf(stderr, _(" (rule from \"%s\", line %"PRIdMAX")"),
			    filename(rfilenum), rlinenum) < 0) {
			return;
		}
	}

	(void)fputc('\n', stderr);
}

ATTRIBUTE_FORMAT((printf, 1, 2)) #if defined(__GNUC__) || defined(__clang__)
__attribute__((format(printf, 1, 2)))
#endif
static void
error(const char *const string, ...)
{
	va_list args;

	va_start(args, string);
	verror(string, args);
	va_end(args);

	errors = true;
}

ATTRIBUTE_FORMAT((printf, 1, 2)) static void
warning(const char *const string, ...)
{
	va_list args;

	if (!string) {
		return;
	}

	if (fprintf(stderr, _("warning: ")) < 0) {
		return;
	}

	va_start(args, string);
	verror(string, args);
	va_end(args);

	warnings = true;
}

/* Close STREAM.  If it had an I/O error, report it against DIR/NAME,
   remove TEMPNAME if nonnull, and then exit.  */
static void
close_file(FILE *stream, char const *dir, char const *name,
           char const *tempname)
{
  const char *error_message = NULL;

  if (ferror(stream)) {
    error_message = _("I/O error");
  }

  if (fclose(stream) != 0) {
    if (!error_message) {
      error_message = strerror(errno);
    }
  }

  if (error_message) {
    fprintf(stderr, "%s: ", progname);
    if (dir) {
      fprintf(stderr, "%s/", dir);
    }
    if (name) {
      fprintf(stderr, "%s: ", name);
    }
    fprintf(stderr, "%s\n", error_message);

    if (tempname) {
      remove(tempname);
    }
    exit(EXIT_FAILURE);
  }
}

ATTRIBUTE_NORETURN static void
usage(FILE *stream, int status)
{
	fprintf(stream,
		_("%s: usage is %s [ --version ] [ --help ] [ -v ]\n"
		  "\t[ -b {slim|fat} ] [ -d directory ] [ -l localtime ]"
		  " [ -L leapseconds ]\n"
		  "\t[ -p posixrules ] [ -r '[@lo][/@hi]' ] [ -R '@hi' ]\n"
		  "\t[ -t localtime-link ]\n"
		  "\t[ filename ... ]\n\n"
		  "Report bugs to %s.\n"),
		progname, progname, REPORT_BUGS_TO);

	close_file(stream, NULL, NULL, NULL);
	exit(status);
}

/* Change the working directory to DIR, possibly creating DIR and its
   ancestors.  After this is done, all files are accessed with names
   relative to DIR.  */
static void
change_directory(char const *dir)
{
    if (chdir(dir) == 0) {
        return;
    }

    int chdir_errno = errno;

    if (chdir_errno == ENOENT) {
        mkdirs(dir, false);
        if (chdir(dir) == 0) {
            return;
        }
        chdir_errno = errno;
    }

    fprintf(stderr, _("%s: Can't chdir to %s: %s\n"),
            progname, dir, strerror(chdir_errno));
    exit(EXIT_FAILURE);
}

/* Compare the two links A and B, for a stable sort by link name.  */
static int
qsort_linkcmp(void const *a, void const *b)
{
  struct link const *l = a;
  struct link const *m = b;

  int cmp = strcmp(l->l_linkname, m->l_linkname);
  if (cmp) {
    return cmp;
  }

  cmp = (l->l_filenum > m->l_filenum) - (l->l_filenum < m->l_filenum);
  if (cmp) {
    return cmp;
  }

  return (l->l_linenum > m->l_linenum) - (l->l_linenum < m->l_linenum);
}

/* Compare the string KEY to the link B, for bsearch.  */
static int
bsearch_linkcmp(void const *key, void const *element)
{
    struct link const *link_element = element;
    return strcmp((char const *)key, link_element->l_linkname);
}

/* Make the links specified by the Link lines.  */
static void
make_links(void)
{
  if (nlinks > 1)
  {
    qsort(links, nlinks, sizeof *links, qsort_linkcmp);
  }

  if (nlinks > 0)
  {
    ptrdiff_t write_idx = 0;
    for (ptrdiff_t read_idx = 0; read_idx < nlinks; read_idx++)
    {
      while (read_idx + 1 < nlinks && strcmp(links[read_idx].l_linkname, links[read_idx + 1].l_linkname) == 0)
      {
        read_idx++;
      }
      links[write_idx++] = links[read_idx];
    }
    nlinks = write_idx;
  }

  ptrdiff_t total_links = nlinks;
  ptrdiff_t pass_end_idx = nlinks;
  ptrdiff_t links_in_pass = nlinks;

  for (ptrdiff_t current_idx = 0; current_idx < total_links; current_idx++)
  {
    struct link *current_link = &links[current_idx];
    eat(current_link->l_filenum, current_link->l_linenum);

    if (current_idx == pass_end_idx)
    {
      if (total_links - current_idx == links_in_pass)
      {
        error(_("\"Link %s %s\" is part of a link cycle"),
              current_link->l_target, current_link->l_linkname);
        break;
      }
      pass_end_idx = total_links;
      links_in_pass = total_links - current_idx;
    }

    if (strcmp(current_link->l_target, current_link->l_linkname) == 0)
    {
      error(_("link %s targets itself"), current_link->l_target);
      continue;
    }

    struct link *target_link = NULL;
    ptrdiff_t remaining_in_pass = pass_end_idx - (current_idx + 1);
    if (remaining_in_pass > 0)
    {
      target_link = bsearch(current_link->l_target, &links[current_idx + 1],
                            remaining_in_pass, sizeof *links, bsearch_linkcmp);
    }
    if (!target_link)
    {
      ptrdiff_t deferred_count = total_links - pass_end_idx;
      if (deferred_count > 0)
      {
        target_link = bsearch(current_link->l_target, &links[pass_end_idx],
                              deferred_count, sizeof *links, bsearch_linkcmp);
      }
    }

    if (target_link)
    {
      links = growalloc(links, sizeof *links, total_links, &nlinks_alloc);
      links[total_links++] = *current_link;
    }
    else
    {
      dolink(current_link->l_target, current_link->l_linkname, false);
    }

    if (noise && current_idx < nlinks)
    {
      if (target_link)
      {
        warning(_("link %s targeting link %s mishandled by pre-2023 zic"),
                current_link->l_linkname, current_link->l_target);
      }
      else if (bsearch(current_link->l_target, links, nlinks,
                       sizeof *links, bsearch_linkcmp))
      {
        warning(_("link %s targeting link %s"),
                current_link->l_linkname, current_link->l_target);
      }
    }
  }
}

/* Simple signal handling: just set a flag that is checked
   periodically outside critical sections.  To set up the handler,
   prefer sigaction if available to close a signal race.  */

static sig_atomic_t got_signal;

static void
signal_handler(int sig)
{
    got_signal = sig;
}

/* Arrange for SIGINT etc. to be caught by the handler.  */
static void
catch_signals(void)
{
  static const int signals[] = {
#ifdef SIGHUP
    SIGHUP,
#endif
    SIGINT,
#ifdef SIGPIPE
    SIGPIPE,
#endif
    SIGTERM
  };
  const size_t num_signals = sizeof(signals) / sizeof(signals[0]);

  for (size_t i = 0; i < num_signals; i++) {
    const int signum = signals[i];

#ifdef SA_SIGINFO
    struct sigaction new_action;
    new_action.sa_handler = signal_handler;
    sigemptyset(&new_action.sa_mask);
    new_action.sa_flags = 0;

    struct sigaction old_action;
    if (sigaction(signum, &new_action, &old_action) == 0) {
      const bool was_ignored = !(old_action.sa_flags & SA_SIGINFO) &&
                               (old_action.sa_handler == SIG_IGN);
      if (was_ignored) {
        sigaction(signum, &old_action, NULL);
        got_signal = 0;
      }
    }
#else
    void (*old_handler)(int);
    old_handler = signal(signum, signal_handler);

    if (old_handler == SIG_IGN) {
      signal(signum, SIG_IGN);
      got_signal = 0;
    }
#endif
  }
}

/* If a signal has arrived, terminate zic with appropriate status.  */
static void
check_for_signal(void)
{
  const int sig = got_signal;
  if (sig) {
    struct sigaction sa = {0};

    sa.sa_handler = SIG_DFL;
    sigfillset(&sa.sa_mask);
    sa.sa_flags = 0;

    if (sigaction(sig, &sa, NULL) != 0) {
      abort();
    }

    raise(sig);
    abort();
  }
}

enum { TIME_T_BITS_IN_FILE = 64 };

/* The minimum and maximum values representable in a TZif file.  */
static zic_t const min_time = MINVAL(zic_t, TIME_T_BITS_IN_FILE);
static zic_t const max_time = MAXVAL(zic_t, TIME_T_BITS_IN_FILE);

/* The minimum, and one less than the maximum, values specified by
   the -r option.  These default to MIN_TIME and MAX_TIME.  */
static zic_t lo_time = MINVAL(zic_t, TIME_T_BITS_IN_FILE);
static zic_t hi_time = MAXVAL(zic_t, TIME_T_BITS_IN_FILE);

/* The time specified by the -R option, defaulting to MIN_TIME;
   or lo_time, whichever is greater.  */
static zic_t redundant_time = MINVAL(zic_t, TIME_T_BITS_IN_FILE);

/* The time specified by an Expires line, or negative if no such line.  */
static zic_t leapexpires = -1;

/* Set the time range of the output to TIMERANGE.
   Return true if successful.  */
static bool
timerange_option(char *timerange)
{
    intmax_t lo = min_time;
    intmax_t hi = max_time;
    char *current_pos = timerange;

    if (*current_pos == '@') {
        char *end_ptr;
        const char *start_ptr = current_pos + 1;

        errno = 0;
        lo = strtoimax(start_ptr, &end_ptr, 10);

        if (end_ptr == start_ptr || (errno == ERANGE && lo == INTMAX_MAX)) {
            return false;
        }
        current_pos = end_ptr;
    }

    char *final_pos = current_pos;

    if (current_pos[0] == '/' && current_pos[1] == '@') {
        char *end_ptr;
        const char *start_ptr = current_pos + 2;

        errno = 0;
        hi = strtoimax(start_ptr, &end_ptr, 10);

        if (end_ptr == start_ptr || (errno == ERANGE && hi == INTMAX_MIN)) {
            return false;
        }

        bool overflowed = (errno == ERANGE && hi == INTMAX_MAX);
        if (!overflowed) {
            hi--;
        }
        final_pos = end_ptr;
    }

    if (*final_pos != '\0' || hi < lo || max_time < lo || hi < min_time) {
        return false;
    }

    lo_time = max(lo, min_time);
    hi_time = min(hi, max_time);

    return true;
}

/* Generate redundant time stamps up to OPT.  Return true if successful.  */
static bool
redundant_time_option(char *opt)
{
    if (*opt != '@') {
        return false;
    }

    char *number_str = opt + 1;
    if (*number_str == '\0') {
        return false;
    }

    char *end_ptr;
    errno = 0;
    intmax_t parsed_timestamp = strtoimax(number_str, &end_ptr, 10);

    if (errno == ERANGE || *end_ptr != '\0' || end_ptr == number_str) {
        return false;
    }

    redundant_time = max(redundant_time, parsed_timestamp);
    return true;
}

static const char *	psxrules;
static const char *	lcltime;
static const char *	directory;
static const char *	tzdefault;

/* -1 if the TZif output file should be slim, 0 if default, 1 if the
   output should be fat for backward compatibility.  ZIC_BLOAT_DEFAULT
   determines the default.  */
static int bloat;

static bool
want_bloat(void)
{
    return bloat >= 0;
}

#ifndef ZIC_BLOAT_DEFAULT
# define ZIC_BLOAT_DEFAULT "slim"
#endif

static void initialize_system(void)
{
#ifdef S_IWGRP
	umask(umask(S_IWGRP | S_IWOTH) | (S_IWGRP | S_IWOTH));
#endif
#if HAVE_GETTEXT
	setlocale(LC_ALL, "");
# ifdef TZ_DOMAINDIR
	bindtextdomain(TZ_DOMAIN, TZ_DOMAINDIR);
# endif /* defined TEXTDOMAINDIR */
	textdomain(TZ_DOMAIN);
#endif /* HAVE_GETTEXT */
}

static void handle_special_arguments(int argc, char **argv)
{
	for (int k = 1; k < argc; k++) {
		if (strcmp(argv[k], "--version") == 0) {
			printf("zic %s%s\n", PKGVERSION, TZVERSION);
			close_file(stdout, NULL, NULL, NULL);
			exit(EXIT_SUCCESS);
		}
		if (strcmp(argv[k], "--help") == 0) {
			usage(stdout, EXIT_SUCCESS);
		}
	}
}

static bool set_once(char **option_storage, char *value, char option_char)
{
	if (*option_storage) {
		fprintf(stderr, _("%s: More than one -%c option specified\n"), progname, option_char);
		return false;
	}
	*option_storage = value;
	return true;
}

static bool handle_b_option(const char *arg)
{
	if (strcmp(arg, "slim") == 0) {
		if (0 < bloat) {
			error(_("incompatible -b options"));
			return false;
		}
		bloat = -1;
	} else if (strcmp(arg, "fat") == 0) {
		if (bloat < 0) {
			error(_("incompatible -b options"));
			return false;
		}
		bloat = 1;
	} else {
		error(_("invalid option: -b '%s'"), arg);
		return false;
	}
	return true;
}

static bool handle_r_option(const char *arg)
{
	static bool timerange_given = false;
	if (timerange_given) {
		fprintf(stderr, _("%s: More than one -r option specified\n"), progname);
		return false;
	}
	if (!timerange_option(arg)) {
		fprintf(stderr, _("%s: invalid time range: %s\n"), progname, arg);
		return false;
	}
	timerange_given = true;
	return true;
}

static bool handle_R_option(const char *arg)
{
	if (!redundant_time_option(arg)) {
		fprintf(stderr, _("%s: invalid time: %s\n"), progname, arg);
		return false;
	}
	return true;
}

static bool parse_command_line_options(int argc, char **argv)
{
	int c;
	while ((c = getopt(argc, argv, "b:d:l:L:p:r:R:st:vy:")) != -1) {
		switch (c) {
		case 'b':
			if (!handle_b_option(optarg)) return false;
			break;
		case 'd':
			if (!set_once(&directory, optarg, 'd')) return false;
			break;
		case 'l':
			if (!set_once(&lcltime, optarg, 'l')) return false;
			break;
		case 'L':
			if (!set_once(&leapsec, optarg, 'L')) return false;
			break;
		case 'p':
			if (!set_once(&psxrules, optarg, 'p')) return false;
			break;
		case 'r':
			if (!handle_r_option(optarg)) return false;
			break;
		case 'R':
			if (!handle_R_option(optarg)) return false;
			break;
		case 's':
		case 'y':
			warning(_("-%c ignored"), (char)c);
			break;
		case 't':
			if (!set_once(&tzdefault, optarg, 't')) return false;
			break;
		case 'v':
			noise = true;
			break;
		default:
			usage(stderr, EXIT_FAILURE);
		}
	}
	return true;
}

static bool validate_time_ranges(void)
{
	if (hi_time + (hi_time < ZIC_MAX) < redundant_time) {
		fprintf(stderr, _("%s: -R time exceeds -r cutoff\n"), progname);
		return false;
	}
	if (redundant_time < lo_time) {
		redundant_time = lo_time;
	}
	return true;
}

static void set_default_options(void)
{
	if (bloat == 0) {
		static char const bloat_default[] = ZIC_BLOAT_DEFAULT;
		if (strcmp(bloat_default, "slim") == 0) {
			bloat = -1;
		} else if (strcmp(bloat_default, "fat") == 0) {
			bloat = 1;
		} else {
			abort(); /* Configuration error. */
		}
	}
	if (directory == NULL) {
		directory = TZDIR;
	}
	if (tzdefault == NULL) {
		tzdefault = TZDEFAULT;
	}
}

static void process_input_files(int argc, char **argv)
{
	if (optind < argc && leapsec != NULL) {
		infile(LEAPSEC_FILENUM, leapsec);
		adjleap();
	}
	for (int k = optind; k < argc; k++) {
		infile(k, argv[k]);
	}
}

static void process_zones(void)
{
	for (ptrdiff_t i = 0; i < nzones; ) {
		ptrdiff_t j;
		/* Find the next non-continuation zone entry. */
		for (j = i + 1; j < nzones && zones[j].z_name == NULL; ++j) {
			continue;
		}
		outzone(&zones[i], j - i);
		i = j;
	}
}

static void create_special_links(void)
{
	if (lcltime != NULL) {
		eat(COMMAND_LINE_FILENUM, 1);
		dolink(lcltime, tzdefault, true);
	}
	if (psxrules != NULL) {
		eat(COMMAND_LINE_FILENUM, 1);
		dolink(psxrules, TZDEFRULES, true);
	}
}

int main(int argc, char **argv)
{
	initialize_system();
	main_argv = argv;
	progname = argv[0] ? argv[0] : "zic";

	if (TYPE_BIT(zic_t) < 64) {
		fprintf(stderr, "%s: %s\n", progname, _("wild compilation-time specification of zic_t"));
		return EXIT_FAILURE;
	}

	handle_special_arguments(argc, argv);

	if (!parse_command_line_options(argc, argv)) {
		return EXIT_FAILURE;
	}

	if (optind == argc - 1 && strcmp(argv[optind], "=") == 0) {
		usage(stderr, EXIT_FAILURE);
	}

	if (!validate_time_ranges()) {
		return EXIT_FAILURE;
	}

	set_default_options();

	process_input_files(argc, argv);
	if (errors) {
		return EXIT_FAILURE;
	}

	associate();
	change_directory(directory);
	catch_signals();

	process_zones();
	make_links();
	create_special_links();

	if (warnings && (ferror(stderr) || fclose(stderr) != 0)) {
		return EXIT_FAILURE;
	}

	return errors ? EXIT_FAILURE : EXIT_SUCCESS;
}

static bool
componentcheck(char const *name, char const *component,
	       char const *component_end)
{
	enum { COMPONENT_LEN_MAX = 14 };
	ptrdiff_t component_len = component_end - component;

	if (component_len <= 0) {
		if (component_len < 0) {
			error(_("component length is negative"));
			return false;
		}
		if (!*name) {
			error(_("empty file name"));
		} else {
			const char *message;
			if (component == name) {
				message = _("file name '%s' begins with '/'");
			} else if (*component_end) {
				message = _("file name '%s' contains '//'");
			} else {
				message = _("file name '%s' ends with '/'");
			}
			error(message, name);
		}
		return false;
	}

	bool is_dot = (component_len == 1 && component[0] == '.');
	bool is_dotdot = (component_len == 2 && component[0] == '.'
			  && component[1] == '.');

	if (is_dot || is_dotdot) {
		error(_("file name '%s' contains '%.*s' component"),
		      name, (int)component_len, component);
		return false;
	}

	if (noise) {
		if (component[0] == '-') {
			warning(
				_("file name '%s' component contains leading '-'"),
				name);
		}
		if (component_len > COMPONENT_LEN_MAX) {
			warning(
				_("file name '%s' contains overlength component '%.*s...'"),
				name, COMPONENT_LEN_MAX, component);
		}
	}

	return true;
}

static bool
namecheck(const char *name)
{
	/* Benign characters in a portable file name.  */
	static const char benign[] =
	  "-/_"
	  "abcdefghijklmnopqrstuvwxyz"
	  "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

	/* Non-control chars in the POSIX portable character set,
	   excluding the benign characters.  */
	static const char printable_and_not_benign[] =
	  " !\"#$%&'()*+,.0123456789:;<=>?@[\\]^`{|}~";

	const char *component_start = name;
	for (const char *current = name; ; current++)
	{
		const unsigned char c = *current;

		if (noise && c != '\0' && !strchr(benign, c))
		{
			if (strchr(printable_and_not_benign, c))
			{
				warning(_("file name '%s' contains byte '%c'"), name, c);
			}
			else
			{
				warning(_("file name '%s' contains byte '\\%o'"), name, c);
			}
		}

		if (c == '/' || c == '\0')
		{
			if (!componentcheck(name, component_start, current))
			{
				return false;
			}
			if (c == '\0')
			{
				return true;
			}
			component_start = current + 1;
		}
	}
}

/* Return a random uint_fast64_t.  */
static uint_fast64_t
get_rand_u64(void)
{
#if HAVE_GETRANDOM
    static bool getrandom_failed = false;
    if (!getrandom_failed) {
        enum {
            BUFFER_WORDS = (256 / sizeof(uint_fast64_t) > 0)
                               ? (256 / sizeof(uint_fast64_t))
                               : 1
        };
        static uint_fast64_t entropy_buffer[BUFFER_WORDS];
        static size_t words_left = 0;

        if (words_left == 0) {
            ssize_t bytes_read;
            do {
                bytes_read =
                    getrandom(entropy_buffer, sizeof(entropy_buffer), 0);
            } while (bytes_read < 0 && errno == EINTR);

            if (bytes_read > 0) {
                words_left = bytes_read / sizeof(entropy_buffer[0]);
            } else {
                getrandom_failed = true;
            }
        }

        if (words_left > 0) {
            return entropy_buffer[--words_left];
        }
    }
#endif

    static bool initialized = false;
    if (!initialized) {
        srand(time(NULL));
        initialized = true;
    }

    uint_fast64_t r = 0;
    for (int i = 0; i < 64; i += 15) {
        r = (r << 15) | (rand() & 0x7FFF);
    }
    return r;
}

/* Generate a randomish name in the same directory as *NAME.  If
   *NAMEALLOC, put the name into *NAMEALLOC which is assumed to be
   that returned by a previous call and is thus already almost set up
   and equal to *NAME; otherwise, allocate a new name and put its
   address into both *NAMEALLOC and *NAME.  */
static void
random_dirent(char const **name, char **namealloc)
{
    static char const prefix[] = ".zic";
    static char const alphabet[] =
        "abcdefghijklmnopqrstuvwxyz"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "0123456789";
    enum {
        PREFIX_LEN = sizeof prefix - 1,
        ALPHABET_LEN = sizeof alphabet - 1,
        SUFFIX_LEN = 6
    };

    char const *src = *name;
    char const *last_slash = strrchr(src, '/');
    ptrdiff_t dir_len = last_slash ? (last_slash - src + 1) : 0;

    char *dst = *namealloc;
    if (!dst) {
        dst = emalloc(size_sum(dir_len, PREFIX_LEN + SUFFIX_LEN + 1));
        memcpy(dst, src, dir_len);
        memcpy(dst + dir_len, prefix, PREFIX_LEN);
        dst[dir_len + PREFIX_LEN + SUFFIX_LEN] = '\0';
        *name = *namealloc = dst;
    }

    uint_fast64_t const alphabet_size = ALPHABET_LEN;
    uint_fast64_t num_combinations = alphabet_size * alphabet_size * alphabet_size
                                   * alphabet_size * alphabet_size * alphabet_size;

    uint_fast64_t rejection_threshold = (UINTMAX_MAX / num_combinations) * num_combinations;

    uint_fast64_t random_val;
    do {
        random_val = get_rand_u64();
    } while (random_val >= rejection_threshold);

    char *suffix_ptr = dst + dir_len + PREFIX_LEN;
    for (int i = 0; i < SUFFIX_LEN; i++) {
        suffix_ptr[i] = alphabet[random_val % alphabet_size];
        random_val /= alphabet_size;
    }
}

/* Prepare to write to the file *OUTNAME, using *TEMPNAME to store the
   name of the temporary file that will eventually be renamed to
   *OUTNAME.  Assign the temporary file's name to both *OUTNAME and
   *TEMPNAME.  If *TEMPNAME is null, allocate the name of any such
   temporary file; otherwise, reuse *TEMPNAME's storage, which is
   already set up and only needs its trailing suffix updated.  */
static FILE *
open_outfile(char const **outname, char **tempname)
{
#if __STDC_VERSION__ < 201112
  static char const fopen_mode[] = "wb";
#else
  static char const fopen_mode[] = "wbx";
#endif

  if (!*tempname)
    random_dirent(outname, tempname);

  bool dirs_made = false;
  for (;;)
  {
    FILE *fp = fopen(*outname, fopen_mode);
    if (fp)
      return fp;

    int const fopen_errno = errno;
    if (fopen_errno == ENOENT && !dirs_made)
    {
      mkdirs(*outname, true);
      dirs_made = true;
      continue;
    }

    if (fopen_errno == EEXIST)
    {
      random_dirent(outname, tempname);
      continue;
    }

    fprintf(stderr, _("%s: Can't create %s/%s: %s\n"),
            progname, directory, *outname, strerror(fopen_errno));
    exit(EXIT_FAILURE);
  }
}

/* If TEMPNAME, the result is in the temporary file TEMPNAME even
   though the user wanted it in NAME, so rename TEMPNAME to NAME.
   Report an error and exit if there is trouble.  Also, free TEMPNAME.  */
static void
rename_dest(char *tempname, char const *name)
{
  if (!tempname)
    {
      return;
    }

  int rename_status = rename(tempname, name);
  if (rename_status != 0)
    {
      int const rename_errno = errno;
      remove(tempname);
      fprintf(stderr, _("%s: rename to %s/%s: %s\n"),
              progname, directory, name, strerror(rename_errno));
    }

  free(tempname);

  if (rename_status != 0)
    {
      exit(EXIT_FAILURE);
    }
}

/* Create symlink contents suitable for symlinking TARGET to LINKNAME, as a
   freshly allocated string.  TARGET should be a relative file name, and
   is relative to the global variable DIRECTORY.  LINKNAME can be either
   relative or absolute.  */
static char *
relname(char const *target, char const *linkname)
{
    char *absolute_target = NULL;
    char const *target_path = target;
    ptrdiff_t absolute_target_buf_size = INDEX_MAX;

    if (*linkname == '/')
    {
        size_t dir_len = strlen(directory);
        size_t needs_slash = (dir_len > 0 && directory[dir_len - 1] != '/');
        size_t dir_path_len = dir_len + needs_slash;
        size_t target_len = strlen(target);

        absolute_target_buf_size = size_sum(dir_path_len, target_len + 1);
        absolute_target = emalloc(absolute_target_buf_size);

        memcpy(absolute_target, directory, dir_len);
        if (needs_slash)
        {
            absolute_target[dir_len] = '/';
        }
        memcpy(absolute_target + dir_path_len, target, target_len + 1);
        target_path = absolute_target;
    }

    size_t common_prefix_len = 0;
    size_t common_dir_len = 0;
    while (target_path[common_prefix_len] &&
           target_path[common_prefix_len] == linkname[common_prefix_len])
    {
        if (target_path[common_prefix_len] == '/')
        {
            common_dir_len = common_prefix_len + 1;
        }
        common_prefix_len++;
    }

    size_t dotdots_count = 0;
    for (size_t i = common_prefix_len; linkname[i]; i++)
    {
        if (linkname[i] == '/' && (i == 0 || linkname[i - 1] != '/'))
        {
            dotdots_count++;
        }
    }

    size_t target_tail_len = strlen(target_path + common_dir_len);
    ptrdiff_t relative_path_size =
        size_sum(size_product(dotdots_count, 3), target_tail_len + 1);

    if (relative_path_size > absolute_target_buf_size)
    {
        return absolute_target;
    }

    char *result = emalloc(relative_path_size);
    char *p = result;
    for (size_t i = 0; i < dotdots_count; i++)
    {
        memcpy(p, "../", 3);
        p += 3;
    }
    memcpy(p, target_path + common_dir_len, target_tail_len + 1);

    free(absolute_target);
    return result;
}

/* Return true if A and B must have the same parent dir if A and B exist.
   Return false if this is not necessarily true (though it might be true).
   Keep it simple, and do not inspect the file system.  */
ATTRIBUTE_PURE_114833 static bool
same_parent_dirs(char const *a, char const *b)
{
  for (; *a == *b; a++, b++)
    if (!*a)
      return true;
  return ! (strchr(a, '/') || strchr(b, '/'));
}

static void
report_fatal_link_error(const char *target, const char *outname, int err_num)
{
	fprintf(stderr, _("%s: Can't link %s/%s to %s/%s: %s\n"),
		progname, directory, target, directory, outname,
		strerror(err_num));
	exit(EXIT_FAILURE);
}

static void
handle_remove_link(const char *linkname)
{
	if (remove(linkname) != 0 && errno != ENOENT && errno != ENOTDIR) {
		fprintf(stderr, _("%s: Can't remove %s/%s: %s\n"),
			progname, directory, linkname, strerror(errno));
		exit(EXIT_FAILURE);
	}
}

static int
perform_hard_link(const char *target, const char *outname)
{
	if (linkat(AT_FDCWD, target, AT_FDCWD, outname, AT_SYMLINK_FOLLOW) == 0)
		return 0;

	int link_errno = errno;
	if (link_errno == EINVAL)
		link_errno = ENOTSUP;

#if HAVE_LINK
	if (link_errno == ENOTSUP) {
		int targetissym = -2;
		if (same_parent_dirs(target, outname)
		    || 0 <= itssymlink(target, &targetissym)) {
			if (link(target, outname) == 0)
				return 0;
			return errno;
		}
	}
#endif
	return link_errno;
}

static int
create_symlink_with_retry(const char *target, const char *linkname,
			  const char **outname_ptr, char **tempname_ptr,
			  bool * linkdirs_made_ptr)
{
	char *linkalloc = NULL;
	const char *contents = target;
	if (*target != '/') {
		linkalloc = relname(target, linkname);
		if (!linkalloc) {
			fprintf(stderr, _("%s: memory exhausted\n"), progname);
			exit(EXIT_FAILURE);
		}
		contents = linkalloc;
	}

	int symlink_errno;
	while (true) {
		if (symlink(contents, *outname_ptr) == 0) {
			symlink_errno = 0;
			break;
		}
		symlink_errno = errno;

		if (symlink_errno == EEXIST) {
			random_dirent(outname_ptr, tempname_ptr);
		} else if (symlink_errno == ENOENT && !*linkdirs_made_ptr) {
			mkdirs(linkname, true);
			*linkdirs_made_ptr = true;
		} else {
			break;
		}
	}

	free(linkalloc);
	return symlink_errno;
}

static void
perform_copy(const char *target, const char *linkname,
	     const char **outname_ptr, char **tempname_ptr)
{
	FILE *fp = fopen(target, "rb");
	if (!fp) {
		fprintf(stderr, _("%s: Can't read %s/%s: %s\n"),
			progname, directory, target, strerror(errno));
		exit(EXIT_FAILURE);
	}

	FILE *tp = open_outfile(outname_ptr, tempname_ptr);
	int c;
	while ((c = getc(fp)) != EOF)
		putc(c, tp);

	close_file(tp, directory, linkname, *tempname_ptr);
	close_file(fp, directory, target, NULL);
}

static void
dolink(char const *target, char const *linkname, bool staysymlink)
{
	if (strcmp(target, "-") == 0) {
		handle_remove_link(linkname);
		return;
	}

	check_for_signal();

	char *tempname = NULL;
	const char *outname = linkname;
	bool linkdirs_made = false;
	int link_errno;

	while (true) {
		link_errno = perform_hard_link(target, outname);
		if (link_errno == 0) {
			rename_dest(tempname, linkname);
			return;
		}

		if (link_errno == EXDEV || link_errno == ENOTSUP)
			break;

		if (link_errno == EEXIST) {
			if (staysymlink && !tempname) {
				int linknameissym = -2;
				if (itssymlink(linkname, &linknameissym))
					break;
			}
			random_dirent(&outname, &tempname);
		} else if (link_errno == ENOENT && !linkdirs_made) {
			mkdirs(linkname, true);
			linkdirs_made = true;
		} else {
			report_fatal_link_error(target, outname, link_errno);
		}
	}

	const int hard_link_errno = link_errno;
	int symlink_errno =
	    create_symlink_with_retry(target, linkname, &outname, &tempname,
				      &linkdirs_made);

	if (symlink_errno == 0) {
		if (hard_link_errno != ENOTSUP && hard_link_errno != EEXIST)
			warning(
			    _("symbolic link used because hard link failed: %s"),
			    strerror(hard_link_errno));
	} else {
		perform_copy(target, linkname, &outname, &tempname);
		if (hard_link_errno != ENOTSUP)
			warning(_("copy used because hard link failed: %s"),
				strerror(hard_link_errno));
		else if (symlink_errno != ENOTSUP)
			warning(_("copy used because symbolic link failed: %s"),
				strerror(symlink_errno));
	}

	rename_dest(tempname, linkname);
}

/* Return 1 if NAME is an absolute symbolic link, -1 if it is relative,
   0 if it is not a symbolic link.  If *CACHE is not -2, it is the
   cached result of a previous call to this function with the same NAME.  */
#include <unistd.h>

static int
itssymlink(char const *name, int *cache)
{
  if (*cache == -2) {
    char first_char;
    ssize_t ret = readlink(name, &first_char, 1);

    if (ret < 0) {
      *cache = 0;
    } else {
      *cache = (first_char == '/') ? 1 : -1;
    }
  }
  return *cache;
}

/*
** Associate sets of rules with zones.
*/

/*
** Sort by rule name.
*/

static int
rcomp(const void *cp1, const void *cp2)
{
    if (cp1 == cp2) {
        return 0;
    }
    if (!cp1) {
        return -1;
    }
    if (!cp2) {
        return 1;
    }

    const struct rule *r1 = (const struct rule *)cp1;
    const struct rule *r2 = (const struct rule *)cp2;

    const char *name1 = r1->r_name;
    const char *name2 = r2->r_name;

    if (name1 == name2) {
        return 0;
    }
    if (!name1) {
        return -1;
    }
    if (!name2) {
        return 1;
    }

    return strcmp(name1, name2);
}

static void
associate(void)
{
	ptrdiff_t i, j, k, base, out;
	int has_multiple_files;
	int first_filenum;

	if (nrules > 1) {
		qsort(rules, nrules, sizeof *rules, rcomp);
		for (i = 0; i < nrules; i = j) {
			j = i + 1;
			while (j < nrules && strcmp(rules[i].r_name, rules[j].r_name) == 0) {
				j++;
			}

			if (j > i + 1) {
				first_filenum = rules[i].r_filenum;
				has_multiple_files = 0;
				for (k = i + 1; k < j; k++) {
					if (rules[k].r_filenum != first_filenum) {
						has_multiple_files = 1;
						break;
					}
				}

				if (has_multiple_files) {
					for (k = i; k < j; k++) {
						eat(rules[k].r_filenum, rules[k].r_linenum);
						warning(_("same rule name in multiple files"));
					}
				}
			}
		}
	}

	for (i = 0; i < nzones; ++i) {
		zones[i].z_rules = NULL;
		zones[i].z_nrules = 0;
	}

	for (base = 0; base < nrules; base = out) {
		out = base + 1;
		while (out < nrules && strcmp(rules[base].r_name, rules[out].r_name) == 0) {
			out++;
		}

		for (i = 0; i < nzones; ++i) {
			if (strcmp(zones[i].z_rule, rules[base].r_name) == 0) {
				zones[i].z_rules = &rules[base];
				zones[i].z_nrules = out - base;
			}
		}
	}

	for (i = 0; i < nzones; ++i) {
		if (zones[i].z_nrules == 0) {
			eat(zones[i].z_filenum, zones[i].z_linenum);
			zones[i].z_save = getsave(zones[i].z_rule, &zones[i].z_isdst);
			if (zones[i].z_format_specifier == 's') {
				error("%s", _("%s in ruleless zone"));
			}
		}
	}

	if (errors) {
		exit(EXIT_FAILURE);
	}
}

/* Read a text line from FP into BUF, which is of size BUFSIZE.
   Terminate it with a NUL byte instead of a newline.
   Return true if successful, false if EOF.
   On error, report the error and exit.  */
static bool
inputline(FILE *fp, char *buf, ptrdiff_t bufsize)
{
    ptrdiff_t linelen = 0;

    for (;;) {
        int ch = getc(fp);

        if (ch == '\n') {
            break;
        }

        if (ch == EOF) {
            if (ferror(fp)) {
                error(_("input error"));
                exit(EXIT_FAILURE);
            }
            if (linelen == 0) {
                return false;
            }
            error(_("unterminated line"));
            exit(EXIT_FAILURE);
        }

        if (ch == '\0') {
            error(_("NUL input byte"));
            exit(EXIT_FAILURE);
        }

        if (linelen >= bufsize - 1) {
            error(_("line too long"));
            exit(EXIT_FAILURE);
        }

        buf[linelen++] = (char)ch;
    }

    buf[linelen] = '\0';
    return true;
}

static void
infile(int fnum, char const *name)
{
	FILE *fp;
	if (strcmp(name, "-") == 0) {
		fp = stdin;
	} else {
		fp = fopen(name, "r");
		if (fp == NULL) {
			const char *e = strerror(errno);
			fprintf(stderr, _("%s: Can't open %s: %s\n"),
				progname, name, e);
			exit(EXIT_FAILURE);
		}
	}

	bool wantcont = false;
	for (lineno num = 1; ; ++num) {
		enum { bufsize_bound
		  = (min(INT_MAX, INDEX_MAX) / FORMAT_LEN_GROWTH_BOUND) };
		char buf[min(_POSIX2_LINE_MAX, bufsize_bound)];

		eat(fnum, num);
		if (!inputline(fp, buf, sizeof buf)) {
			break;
		}

		char *fields[MAX_FIELDS];
		const int nfields = getfields(buf, fields,
					      sizeof fields / sizeof *fields);
		if (nfields == 0) {
			continue;
		}

		if (wantcont) {
			wantcont = inzcont(fields, nfields);
			continue;
		}

		const struct lookup *line_codes
		  = (fnum < 0) ? leap_line_codes : zi_line_codes;
		const struct lookup *lp = byword(fields[0], line_codes);

		if (lp == NULL) {
			error(_("input line of unknown type"));
			continue;
		}

		bool next_wantcont = false;
		switch (lp->l_value) {
		case LC_RULE:
			inrule(fields, nfields);
			break;
		case LC_ZONE:
			next_wantcont = inzone(fields, nfields);
			break;
		case LC_LINK:
			inlink(fields, nfields);
			break;
		case LC_LEAP:
			inleap(fields, nfields);
			break;
		case LC_EXPIRES:
			inexpires(fields, nfields);
			break;
		default:
			unreachable();
		}
		wantcont = next_wantcont;
	}

	close_file(fp, NULL, filename(fnum), NULL);
	if (wantcont) {
		error(_("expected continuation line not found"));
	}
}

/*
** Convert a string of one of the forms
**	h	-h	hh:mm	-hh:mm	hh:mm:ss	-hh:mm:ss
** into a number of seconds.
** A null string maps to zero.
** Call error with errstring and return zero on errors.
*/

static zic_t
gethms(char const *string, char const *errstring)
{
	if (string == NULL || *string == '\0')
		return 0;

	int sign = 1;
	if (*string == '-') {
		sign = -1;
		string++;
	}

	if (!isdigit((unsigned char)*string)) {
		error("%s", errstring);
		return 0;
	}

	char *endp;
	unsigned long long h_ull = strtoull(string, &endp, 10);
	string = endp;

	unsigned int mm = 0;
	unsigned int ss = 0;

	if (*string == ':') {
		string++;
		unsigned long val = strtoul(string, &endp, 10);
		if (endp == string || val >= MINSPERHOUR) {
			error("%s", errstring);
			return 0;
		}
		mm = val;
		string = endp;

		if (*string == ':') {
			string++;
			val = strtoul(string, &endp, 10);
			if (endp == string || val > SECSPERMIN) {
				error("%s", errstring);
				return 0;
			}
			ss = val;
			string = endp;
		}
	}

	if (*string == '.') {
		string++;
		if (!isdigit((unsigned char)*string)) {
			error("%s", errstring);
			return 0;
		}

		if (noise) {
			warning(_("fractional seconds rejected by"
				  " pre-2018 versions of zic"));
		}

		int tenths = *string - '0';
		string++;

		bool remainder_is_nonzero = false;
		const char *frac_ptr = string;
		while (isdigit((unsigned char)*frac_ptr)) {
			if (*frac_ptr != '0') {
				remainder_is_nonzero = true;
			}
			frac_ptr++;
		}
		string = frac_ptr;

		if (tenths > 5 || (tenths == 5 && (remainder_is_nonzero || (ss % 2 != 0)))) {
			ss++;
		}
	}

	if (*string != '\0') {
		error("%s", errstring);
		return 0;
	}

	if (h_ull > ZIC_MAX) {
		error(_("time overflow"));
		return 0;
	}
	zic_t hh = h_ull;

	if (ZIC_MAX / SECSPERHOUR < hh) {
		error(_("time overflow"));
		return 0;
	}

	if (noise && (hh > HOURSPERDAY ||
		(hh == HOURSPERDAY && (mm != 0 || ss != 0))))
		warning(_("values over 24 hours not handled by pre-2007 versions of zic"));

	return oadd(sign * hh * SECSPERHOUR,
		    sign * ((zic_t)mm * SECSPERMIN + ss));
}

static zic_t
getsave(char *field, bool *isdst)
{
    int dst_suffix = -1;
    size_t fieldlen = strlen(field);

    if (fieldlen > 0) {
        char *end = field + fieldlen - 1;
        switch (*end) {
            case 'd':
                dst_suffix = 1;
                *end = '\0';
                break;
            case 's':
                dst_suffix = 0;
                *end = '\0';
                break;
        }
    }

    zic_t save = gethms(field, _("invalid saved time"));

    if (dst_suffix != -1) {
        *isdst = (dst_suffix != 0);
    } else {
        *isdst = (save != 0);
    }

    return save;
}

static void
inrule(char **fields, int nfields)
{
	struct rule r;
	const char *name;
	const unsigned char first_char;

	if (nfields != RULE_FIELDS) {
		error(_("wrong number of fields on Rule line"));
		return;
	}

	name = fields[RF_NAME];
	first_char = *name;

	if (!first_char || isspace(first_char) || isdigit(first_char) ||
	    first_char == '+' || first_char == '-') {
		error(_("Invalid rule name \"%s\""), name);
		return;
	}

	r.r_filenum = filenum;
	r.r_linenum = linenum;
	r.r_save = getsave(fields[RF_SAVE], &r.r_isdst);

	if (!rulesub(&r, fields[RF_LOYEAR], fields[RF_HIYEAR],
		     fields[RF_COMMAND], fields[RF_MONTH], fields[RF_DAY],
		     fields[RF_TOD])) {
		return;
	}

	r.r_name = estrdup(name);
	r.r_abbrvar = estrdup(fields[RF_ABBRVAR]);

	size_t abbrvar_len = strlen(r.r_abbrvar);
	if (max_abbrvar_len < abbrvar_len) {
		max_abbrvar_len = abbrvar_len;
	}

	rules = growalloc(rules, sizeof *rules, nrules, &nrules_alloc);
	rules[nrules++] = r;
}

static bool
inzone(char **fields, int nfields)
{
	if (nfields < ZONE_MINFIELDS || nfields > ZONE_MAXFIELDS) {
		error(_("wrong number of fields on Zone line"));
		return false;
	}

	const char *const zone_name = fields[ZF_NAME];

	if (lcltime != NULL && strcmp(zone_name, tzdefault) == 0) {
		error(_("\"Zone %s\" line and -l option are mutually exclusive"),
		      tzdefault);
		return false;
	}
	if (strcmp(zone_name, TZDEFRULES) == 0 && psxrules != NULL) {
		error(_("\"Zone %s\" line and -p option are mutually exclusive"),
		      TZDEFRULES);
		return false;
	}

	for (ptrdiff_t i = 0; i < nzones; ++i) {
		if (zones[i].z_name != NULL &&
		    strcmp(zones[i].z_name, zone_name) == 0) {
			error(_("duplicate zone name %s (file \"%s\", line %" PRIdMAX ")"),
			      zone_name,
			      filename(zones[i].z_filenum),
			      zones[i].z_linenum);
			return false;
		}
	}

	return inzsub(fields, nfields, false);
}

static bool
inzcont(char **fields, int nfields)
{
	const bool is_field_count_in_range = (nfields >= ZONEC_MINFIELDS &&
					      nfields <= ZONEC_MAXFIELDS);

	if (!is_field_count_in_range) {
		error(_("wrong number of fields on Zone continuation line"));
		return false;
	}

	return inzsub(fields, nfields, true);
}

static bool
inzsub(char **fields, int nfields, bool iscont)
{
	const int i_stdoff = iscont ? ZFC_STDOFF : ZF_STDOFF;
	const int i_rule = iscont ? ZFC_RULE : ZF_RULE;
	const int i_format = iscont ? ZFC_FORMAT : ZF_FORMAT;
	const int i_untilyear = iscont ? ZFC_TILYEAR : ZF_TILYEAR;
	const int i_untilmonth = iscont ? ZFC_TILMONTH : ZF_TILMONTH;
	const int i_untilday = iscont ? ZFC_TILDAY : ZF_TILDAY;
	const int i_untiltime = iscont ? ZFC_TILTIME : ZF_TILTIME;

	if (!iscont && !namecheck(fields[ZF_NAME]))
		return false;

	struct zone z;
	z.z_filenum = filenum;
	z.z_linenum = linenum;
	z.z_stdoff = gethms(fields[i_stdoff], _("invalid UT offset"));

	char *format_field = fields[i_format];
	char *percent_ptr = strchr(format_field, '%');
	z.z_format_specifier = '\0';

	if (percent_ptr) {
		char specifier = percent_ptr[1];
		if ((specifier != 's' && specifier != 'z') ||
		    strchr(percent_ptr + 1, '%') || strchr(format_field, '/')) {
			error(_("invalid abbreviation format"));
			return false;
		}
		z.z_format_specifier = specifier;
	}

	int format_len = strlen(format_field);
	if (max_format_len < format_len)
		max_format_len = format_len;

	const bool hasuntil = nfields > i_untilyear;
	if (hasuntil) {
		z.z_untilrule.r_filenum = filenum;
		z.z_untilrule.r_linenum = linenum;

		const char *month = (nfields > i_untilmonth) ? fields[i_untilmonth] : "Jan";
		const char *day = (nfields > i_untilday) ? fields[i_untilday] : "1";
		const char *time = (nfields > i_untiltime) ? fields[i_untiltime] : "0";

		if (!rulesub(&z.z_untilrule, fields[i_untilyear], "only", "", month, day, time))
			return false;

		z.z_untiltime = rpytime(&z.z_untilrule, z.z_untilrule.r_loyear);

		if (iscont && nzones > 0) {
			const struct zone *prev_zone = &zones[nzones - 1];
			bool current_until_valid = (z.z_untiltime > min_time && z.z_untiltime < max_time);
			bool prev_until_valid = (prev_zone->z_untiltime > min_time && prev_zone->z_untiltime < max_time);

			if (current_until_valid && prev_until_valid && prev_zone->z_untiltime >= z.z_untiltime) {
				error(_("Zone continuation line end time is not after end time of previous line"));
				return false;
			}
		}
	}

	z.z_name = iscont ? NULL : estrdup(fields[ZF_NAME]);
	z.z_rule = estrdup(fields[i_rule]);
	z.z_format = estrdup(format_field);

	if (z.z_format_specifier == 'z') {
		ptrdiff_t percent_offset = percent_ptr - format_field;
		z.z_format[percent_offset + 1] = 's';
		if (noise)
			warning(_("format '%s' not handled by pre-2015 versions of zic"), format_field);
	}

	zones = growalloc(zones, sizeof *zones, nzones, &nzones_alloc);
	zones[nzones++] = z;

	return hasuntil;
}

static zic_t
getleapdatetime(char **fields, bool expire_line)
{
	zic_t year;
	char xs;
	if (sscanf(fields[LP_YEAR], "%" SCNdZIC "%c", &year, &xs) != 1) {
		error(_("invalid leaping year"));
		return -1;
	}

	if (!expire_line) {
		if (!leapseen) {
			leapminyear = year;
			leapmaxyear = year;
			leapseen = true;
		} else if (year < leapminyear) {
			leapminyear = year;
		} else if (year > leapmaxyear) {
			leapmaxyear = year;
		}
	}

	zic_t dayoff = 0;
	if (year > EPOCH_YEAR) {
		for (zic_t y = EPOCH_YEAR; y < year; ++y) {
			dayoff = oadd(dayoff, len_years[isleap(y)]);
		}
	} else {
		for (zic_t y = year; y < EPOCH_YEAR; ++y) {
			dayoff = oadd(dayoff, -len_years[isleap(y)]);
		}
	}

	const struct lookup *lp = byword(fields[LP_MONTH], mon_names);
	if (lp == NULL) {
		error(_("invalid month name"));
		return -1;
	}
	int month = lp->l_value;

	for (int m = TM_JANUARY; m < month; ++m) {
		dayoff = oadd(dayoff, len_months[isleap(year)][m]);
	}

	int day;
	if (sscanf(fields[LP_DAY], "%d%c", &day, &xs) != 1 ||
	    day <= 0 || day > len_months[isleap(year)][month]) {
		error(_("invalid day of month"));
		return -1;
	}
	dayoff = oadd(dayoff, day - 1);

	if (dayoff < min_time / SECSPERDAY) {
		error(_("time too small"));
		return -1;
	}
	if (dayoff > max_time / SECSPERDAY) {
		error(_("time too large"));
		return -1;
	}

	zic_t tod = gethms(fields[LP_TIME], _("invalid time of day"));
	if (tod < 0) {
		return -1;
	}

	zic_t t = dayoff * SECSPERDAY;
	t = tadd(t, tod);
	if (t < 0) {
		error(_("leap second precedes Epoch"));
	}

	return t;
}

static void
inleap(char **fields, int nfields)
{
    if (nfields != LEAP_FIELDS) {
        error(_("wrong number of fields on Leap line"));
        return;
    }

    zic_t t = getleapdatetime(fields, false);
    if (t < 0) {
        return;
    }

    struct lookup const *lp = byword(fields[LP_ROLL], leap_types);
    if (!lp) {
        error(_("invalid Rolling/Stationary field on Leap line"));
        return;
    }

    int correction;
    if (!fields[LP_CORR][0]) {
        correction = -1;
    } else if (strcmp(fields[LP_CORR], "+") == 0) {
        correction = 1;
    } else {
        error(_("invalid CORRECTION field on Leap line"));
        return;
    }

    leapadd(t, correction, lp->l_value);
}

static void
inexpires(char **fields, int nfields)
{
  if (nfields != EXPIRES_FIELDS) {
    error(_("wrong number of fields on Expires line"));
    return;
  }

  if (leapexpires >= 0) {
    error(_("multiple Expires lines"));
    return;
  }

  leapexpires = getleapdatetime(fields, true);
}

static void
inlink(char **fields, int nfields)
{
	if (nfields != LINK_FIELDS) {
		error(_("wrong number of fields on Link line"));
		return;
	}
	if (fields[LF_TARGET][0] == '\0') {
		error(_("blank TARGET field on Link line"));
		return;
	}
	if (!namecheck(fields[LF_LINKNAME])) {
		return;
	}

	links = growalloc(links, sizeof(*links), nlinks, &nlinks_alloc);

	links[nlinks] = (struct link){
		.l_filenum = filenum,
		.l_linenum = linenum,
		.l_target = estrdup(fields[LF_TARGET]),
		.l_linkname = estrdup(fields[LF_LINKNAME]),
	};

	nlinks++;
}

static bool
parse_month(struct rule *rp, const char *monthp)
{
	const struct lookup *lp = byword(monthp, mon_names);
	if (!lp) {
		error(_("invalid month name"));
		return false;
	}
	rp->r_month = lp->l_value;
	return true;
}

static void
parse_time_of_day(struct rule *rp, const char *timep)
{
	char *dp = estrdup(timep);
	rp->r_todisstd = false;
	rp->r_todisut = false;

	if (*dp != '\0') {
		char *ep = dp + strlen(dp) - 1;
		switch (lowerit(*ep)) {
		case 's':	/* Standard */
			rp->r_todisstd = true;
			*ep = '\0';
			break;
		case 'w':	/* Wall */
			/* This is the default, do nothing. */
			*ep = '\0';
			break;
		case 'g':	/* Greenwich */
		case 'u':	/* Universal */
		case 'z':	/* Zulu */
			rp->r_todisstd = true;
			rp->r_todisut = true;
			*ep = '\0';
			break;
		}
	}

	rp->r_tod = gethms(dp, _("invalid time of day"));
	free(dp);
}

static bool
parse_loyear(struct rule *rp, const char *loyearp)
{
	const struct lookup *lp = byword(loyearp, begin_years);
	if (lp) {
		if (lp->l_value == YR_MINIMUM) {
			warning(_("FROM year \"%s\" is obsolete;"
				  " treated as %d"),
				loyearp, YEAR_32BIT_MIN - 1);
			rp->r_loyear = YEAR_32BIT_MIN - 1;
		} else {
			unreachable();
		}
	} else {
		char xs;
		if (sscanf(loyearp, "%" SCNdZIC "%c", &rp->r_loyear, &xs) != 1) {
			error(_("invalid starting year"));
			return false;
		}
	}
	return true;
}

static bool
parse_hiyear(struct rule *rp, const char *hiyearp)
{
	const struct lookup *lp = byword(hiyearp, end_years);
	rp->r_hiwasnum = (lp == NULL);

	if (rp->r_hiwasnum) {
		char xs;
		if (sscanf(hiyearp, "%" SCNdZIC "%c", &rp->r_hiyear, &xs) != 1) {
			error(_("invalid ending year"));
			return false;
		}
	} else {
		switch (lp->l_value) {
		case YR_MAXIMUM:
			rp->r_hiyear = ZIC_MAX;
			break;
		case YR_ONLY:
			rp->r_hiyear = rp->r_loyear;
			break;
		default:
			unreachable();
		}
	}
	return true;
}

static bool
parse_day_number(struct rule *rp, const char *num_part)
{
	char xs;
	int day;

	if (sscanf(num_part, "%d%c", &day, &xs) != 1 ||
	    day <= 0 || day > len_months[1][rp->r_month]) {
		error(_("invalid day of month"));
		return false;
	}
	rp->r_dayofmonth = day;
	return true;
}

static bool
parse_day_spec(struct rule *rp, const char *dayp)
{
	char *dp = estrdup(dayp);
	const struct lookup *lp;
	bool ok = false;

	lp = byword(dp, lasts);
	if (lp) {
		rp->r_dycode = DC_DOWLEQ;
		rp->r_wday = lp->l_value;
		rp->r_dayofmonth = len_months[1][rp->r_month];
		ok = true;
	} else {
		char *ep = strpbrk(dp, "<>");
		if (!ep) {
			rp->r_dycode = DC_DOM;
			ok = parse_day_number(rp, dp);
		} else {
			rp->r_dycode = (*ep == '<') ? DC_DOWLEQ : DC_DOWGEQ;
			*ep++ = '\0';
			if (*ep++ != '=') {
				error(_("invalid day of month"));
			} else {
				lp = byword(dp, wday_names);
				if (!lp) {
					error(_("invalid weekday name"));
				} else {
					rp->r_wday = lp->l_value;
					ok = parse_day_number(rp, ep);
				}
			}
		}
	}

	free(dp);
	return ok;
}

static bool
rulesub(struct rule *rp, const char *loyearp, const char *hiyearp,
	const char *typep, const char *monthp, const char *dayp,
	const char *timep)
{
	if (!parse_month(rp, monthp))
		return false;

	parse_time_of_day(rp, timep);

	if (!parse_loyear(rp, loyearp) || !parse_hiyear(rp, hiyearp))
		return false;

	if (rp->r_loyear > rp->r_hiyear) {
		error(_("starting year greater than ending year"));
		return false;
	}

	if (*typep != '\0') {
		error(_("year type \"%s\" is unsupported; use \"-\" instead"),
		      typep);
		return false;
	}

	if (!parse_day_spec(rp, dayp))
		return false;

	return true;
}

static void
convert(uint_fast32_t val, char *buf)
{
	if (buf == NULL) {
		return;
	}

	unsigned char *const b = (unsigned char *) buf;

	b[0] = (unsigned char)((val >> 24) & 0xFF);
	b[1] = (unsigned char)((val >> 16) & 0xFF);
	b[2] = (unsigned char)((val >> 8) & 0xFF);
	b[3] = (unsigned char)(val & 0xFF);
}

static void
convert64(uint_fast64_t val, unsigned char *buf)
{
	if (!buf) {
		return;
	}

	for (int i = 7; i >= 0; --i) {
		buf[i] = val & 0xff;
		val >>= 8;
	}
}

static void
puttzcode(zic_t val, FILE *fp)
{
	char buf[4];

	convert(val, buf);
	if (fwrite(buf, sizeof buf, 1, fp) != 1) {
		perror("fwrite");
		exit(EXIT_FAILURE);
	}
}

static void
puttzcodepass(zic_t val, FILE *fp, int pass)
{
  if (pass == 1) {
    puttzcode(val, fp);
  } else {
    char buf[sizeof(int64_t)];
    convert64(val, buf);
    (void)fwrite(buf, sizeof buf, 1, fp);
  }

  if (ferror(fp)) {
    perror("Error writing to output file");
    exit(EXIT_FAILURE);
  }
}

static int
atcomp(const void *avp, const void *bvp)
{
  const struct attype *ap = avp;
  const struct attype *bp = bvp;

  if (ap->at < bp->at) {
    return -1;
  }
  if (ap->at > bp->at) {
    return 1;
  }
  return 0;
}

struct timerange {
  int defaulttype;
  ptrdiff_t base, count;
  int leapbase, leapcount;
  bool leapexpiry;
};

static struct timerange
limitrange(struct timerange r, zic_t lo, zic_t hi,
	   zic_t const *ats, unsigned char const *types,
	   zic_t const *trans, zic_t const *corr,
	   zic_t leapexpires, zic_t max_time)
{
  while (r.count > 0 && ats[r.base] < lo) {
    r.defaulttype = types[r.base];
    r.count--;
    r.base++;
  }

  while (r.leapcount > 1 && trans[r.leapbase + 1] <= lo) {
    r.leapcount--;
    r.leapbase++;
  }

  while (r.leapbase > 0) {
    bool const is_positive_leap = corr[r.leapbase - 1] < corr[r.leapbase];
    bool const is_positive_correction = corr[r.leapbase] > 0;
    if (is_positive_leap == is_positive_correction) {
      break;
    }
    r.leapcount++;
    r.leapbase--;
  }

  if (hi < max_time) {
    while (r.count > 0 && ats[r.base + r.count - 1] > hi + 1) {
      r.count--;
    }
    while (r.leapcount > 0 && trans[r.leapbase + r.leapcount - 1] > hi + 1) {
      r.leapcount--;
    }
  }

  r.leapexpiry = leapexpires >= 0 && leapexpires - 1 <= hi;

  return r;
}

static void
optimize_transitions(void)
{
	ptrdiff_t toi = 0;
	for (ptrdiff_t fromi = 0; fromi < timecnt; ++fromi) {
		if (toi > 0) {
			zic_t prev_at = attypes[toi - 1].at;
			int prev_type = attypes[toi - 1].type;
			int prev_prev_type = (toi == 1) ? 0 : attypes[toi - 2].type;
			zic_t prev_transition_end = tadd(prev_at, utoffs[prev_prev_type]);
			zic_t current_transition_end = tadd(attypes[fromi].at, utoffs[prev_type]);

			if (current_transition_end <= prev_transition_end) {
				attypes[toi - 1].type = attypes[fromi].type;
				continue;
			}
		}

		bool types_are_different = (toi == 0)
			|| (utoffs[attypes[toi - 1].type] != utoffs[attypes[fromi].type])
			|| (isdsts[attypes[toi - 1].type] != isdsts[attypes[fromi].type])
			|| (desigidx[attypes[toi - 1].type] != desigidx[attypes[fromi].type]);

		if (types_are_different || attypes[fromi].dontmerge) {
			attypes[toi++] = attypes[fromi];
		}
	}
	timecnt = toi;
}

static void
apply_leap_corrections(zic_t *ats, ptrdiff_t count)
{
	for (ptrdiff_t i = 0; i < count; ++i) {
		for (ptrdiff_t j = leapcnt - 1; j >= 0; --j) {
			if (ats[i] > trans[j] - corr[j]) {
				ats[i] = tadd(ats[i], corr[j]);
				break;
			}
		}
	}
}

static char
determine_tzif_version(const char *name, char version,
		       const struct timerange *r32, const struct timerange *r64)
{
	for (int pass = 1; pass <= 2; pass++) {
		const struct timerange *r = (pass == 1) ? r32 : r64;
		if (pass == 1 && !want_bloat()) {
			continue;
		}
		if (r->leapexpiry) {
			if (noise) {
				warning(_("%s: pre-2021b clients may mishandle leap second expiry"), name);
			}
			return '4';
		}
		if (r->leapcount > 0 && corr[r->leapbase] != 1 && corr[r->leapbase] != -1) {
			if (noise) {
				warning(_("%s: pre-2021b clients may mishandle leap second table truncation"), name);
			}
			return '4';
		}
	}
	return version;
}

static void
write_minimal_time_block(FILE *fp, char version)
{
	struct tzhead tzh;
	memset(&tzh, 0, sizeof tzh);
	memcpy(tzh.tzh_magic, TZ_MAGIC, sizeof tzh.tzh_magic);
	tzh.tzh_version[0] = version;
	convert(1, tzh.tzh_typecnt);
	convert(1, tzh.tzh_charcnt);

	fwrite(&tzh, sizeof tzh, 1, fp);
	puttzcode(0, fp);
	putc(0, fp);
	putc(0, fp);
	putc('\0', fp);
}

static void
write_pass_data(FILE *fp, int pass, char version, const zic_t *ats,
		const unsigned char *types, const struct timerange *range32,
		const struct timerange *range64)
{
	ptrdiff_t thistimei, thistimecnt;
	int thisleapi, thisleapcnt;
	bool thisleapexpiry;
	int thisdefaulttype;
	zic_t thismin, thismax;

	if (pass == 1) {
		thistimei = range32->base;
		thistimecnt = range32->count;
		thisleapi = range32->leapbase;
		thisleapcnt = range32->leapcount;
		thisleapexpiry = range32->leapexpiry;
		thisdefaulttype = range32->defaulttype;
		thismin = ZIC32_MIN;
		thismax = ZIC32_MAX;
	} else {
		thistimei = range64->base;
		thistimecnt = range64->count;
		thisleapi = range64->leapbase;
		thisleapcnt = range64->leapcount;
		thisleapexpiry = range64->leapexpiry;
		thisdefaulttype = range64->defaulttype;
		thismin = min_time;
		thismax = max_time;
	}

	if ((size_t)thistimecnt > INT_MAX) {
		error(_("too many transition times"));
	}

	bool locut = thismin < lo_time && lo_time <= thismax;
	bool hicut = thismin <= hi_time && hi_time < thismax;
	ptrdiff_t thistimelim = thistimei + thistimecnt;

	if (pass == 1 && lo_time <= thismin) {
		thisdefaulttype = range64->defaulttype;
	}
	if (locut) {
		thisdefaulttype = unspecifiedtype;
	}

	int pretranstype = -1;
	if ((locut || (pass == 1 && thistimei > 0)) &&
	    !(thistimecnt > 0 && ats[thistimei] == lo_time)) {
		pretranstype = thisdefaulttype;
	}

	bool omittype[TZ_MAX_TYPES];
	memset(omittype, true, typecnt);
	if (pretranstype >= 0) {
		omittype[pretranstype] = false;
	}
	omittype[thisdefaulttype] = false;
	for (ptrdiff_t i = thistimei; i < thistimelim; i++) {
		omittype[types[i]] = false;
	}
	if (hicut) {
		omittype[unspecifiedtype] = false;
	}

#ifndef LEAVE_SOME_PRE_2011_SYSTEMS_IN_THE_LURCH
	if (want_bloat()) {
		int mrudst = -1, mrustd = -1;
		if (pretranstype >= 0) {
			if (isdsts[pretranstype]) mrudst = pretranstype; else mrustd = pretranstype;
		}
		for (ptrdiff_t i = thistimei; i < thistimelim; i++) {
			if (isdsts[types[i]]) mrudst = types[i]; else mrustd = types[i];
		}

		int hidst = -1, histd = -1;
		for (int i = 0; i < typecnt; i++) {
			int h = (i == 0) ? thisdefaulttype : (i == thisdefaulttype) ? 0 : i;
			if (!omittype[h]) {
				if (isdsts[h]) hidst = i; else histd = i;
			}
		}

		if (hidst >= 0 && mrudst >= 0 && hidst != mrudst && utoffs[hidst] != utoffs[mrudst]) {
			isdsts[mrudst] = -1;
			int type = addtype(utoffs[mrudst], &chars[desigidx[mrudst]], true, ttisstds[mrudst], ttisuts[mrudst]);
			isdsts[mrudst] = 1;
			omittype[type] = false;
		}
		if (histd >= 0 && mrustd >= 0 && histd != mrustd && utoffs[histd] != utoffs[mrustd]) {
			isdsts[mrustd] = -1;
			int type = addtype(utoffs[mrustd], &chars[desigidx[mrustd]], false, ttisstds[mrustd], ttisuts[mrustd]);
			isdsts[mrustd] = 0;
			omittype[type] = false;
		}
	}
#endif

	int typemap[TZ_MAX_TYPES];
	int thistypecnt = 0;
	for (int i = 0; i < typecnt; i++) {
		if (!omittype[i]) {
			int mapped_idx = (i == 0) ? thisdefaulttype : (i == thisdefaulttype) ? 0 : i;
			typemap[mapped_idx] = thistypecnt++;
		}
	}

	char thischars[TZ_MAX_CHARS];
	int indmap[TZ_MAX_CHARS];
	int thischarcnt = 0, stdcnt = 0, utcnt = 0;
	memset(indmap, -1, sizeof indmap);
	for (int i = 0; i < typecnt; i++) {
		if (omittype[i]) continue;
		if (ttisstds[i]) stdcnt = thistypecnt;
		if (ttisuts[i]) utcnt = thistypecnt;
		if (indmap[desigidx[i]] < 0) {
			const char *thisabbr = &chars[desigidx[i]];
			ptrdiff_t offset = 0;
			while (offset < thischarcnt) {
				if (strcmp(&thischars[offset], thisabbr) == 0) break;
				offset += strlen(&thischars[offset]) + 1;
			}
			if (offset >= thischarcnt) {
				strcpy(&thischars[thischarcnt], thisabbr);
				indmap[desigidx[i]] = thischarcnt;
				thischarcnt += strlen(thisabbr) + 1;
			} else {
				indmap[desigidx[i]] = offset;
			}
		}
	}

	struct tzhead tzh;
	memset(&tzh, 0, sizeof tzh);
	memcpy(tzh.tzh_magic, TZ_MAGIC, sizeof tzh.tzh_magic);
	tzh.tzh_version[0] = version;
	convert(utcnt, tzh.tzh_ttisutcnt);
	convert(stdcnt, tzh.tzh_ttisstdcnt);
	convert(thisleapcnt + thisleapexpiry, tzh.tzh_leapcnt);
	convert((pretranstype >= 0) + thistimecnt + hicut, tzh.tzh_timecnt);
	convert(thistypecnt, tzh.tzh_typecnt);
	convert(thischarcnt, tzh.tzh_charcnt);
	fwrite(&tzh, sizeof tzh, 1, fp);

	zic_t lo = (pass == 1 && lo_time < ZIC32_MIN) ? ZIC32_MIN : lo_time;
	if (pretranstype >= 0) puttzcodepass(lo, fp, pass);
	for (ptrdiff_t i = thistimei; i < thistimelim; ++i) puttzcodepass(ats[i], fp, pass);
	if (hicut) puttzcodepass(hi_time + 1, fp, pass);

	if (pretranstype >= 0) putc(typemap[pretranstype], fp);
	for (ptrdiff_t i = thistimei; i < thistimelim; i++) putc(typemap[types[i]], fp);
	if (hicut) putc(typemap[unspecifiedtype], fp);

	for (int i = 0; i < typecnt; i++) {
		int h = (i == 0) ? thisdefaulttype : (i == thisdefaulttype) ? 0 : i;
		if (!omittype[h]) {
			puttzcode(utoffs[h], fp);
			putc(isdsts[h], fp);
			putc(indmap[desigidx[h]], fp);
		}
	}

	if (thischarcnt > 0) fwrite(thischars, sizeof thischars[0], thischarcnt, fp);

	int thisleaplim = thisleapi + thisleapcnt;
	for (int i = thisleapi; i < thisleaplim; ++i) {
		zic_t todo;
		if (roll[i]) {
			ptrdiff_t j;
			if (timecnt == 0 || trans[i] < ats[0]) {
				j = 0;
				while (isdsts[j]) if (++j >= typecnt) { j = 0; break; }
			} else {
				j = 1;
				while (j < timecnt && trans[i] >= ats[j]) ++j;
				j = types[j - 1];
			}
			todo = tadd(trans[i], -utoffs[j]);
		} else {
			todo = trans[i];
		}
		puttzcodepass(todo, fp, pass);
		puttzcode(corr[i], fp);
	}

	if (thisleapexpiry) {
		puttzcodepass(leapexpires, fp, pass);
		puttzcode(thisleaplim > 0 ? corr[thisleaplim - 1] : 0, fp);
	}

	if (stdcnt != 0) {
		for (int i = 0; i < typecnt; i++) if (!omittype[i]) putc(ttisstds[i], fp);
	}
	if (utcnt != 0) {
		for (int i = 0; i < typecnt; i++) if (!omittype[i]) putc(ttisuts[i], fp);
	}
}

static void
writezone(const char *const name, const char *const string, char version, int defaulttype)
{
	char *tempname = NULL;
	const char *outname = name;

	size_t ats_size = align_to(size_product(timecnt + !timecnt, sizeof(zic_t) + 1), alignof(zic_t));
	zic_t *ats = emalloc(ats_size);
	unsigned char *types = (unsigned char *)(ats + timecnt);

	if (timecnt > 1) {
		qsort(attypes, timecnt, sizeof *attypes, atcomp);
	}
	optimize_transitions();

	if (noise && timecnt > 1200) {
		if (timecnt > TZ_MAX_TIMES) {
			warning(_("reference clients mishandle more than %d transition times"), TZ_MAX_TIMES);
		} else {
			warning(_("pre-2014 clients may mishandle more than 1200 transition times"));
		}
	}

	for (ptrdiff_t i = 0; i < timecnt; ++i) {
		ats[i] = attypes[i].at;
		types[i] = attypes[i].type;
	}

	apply_leap_corrections(ats, timecnt);

	struct timerange rangeall = { .defaulttype = defaulttype, .count = timecnt, .leapcount = leapcnt };
	struct timerange range64 = limitrange(rangeall, lo_time,
		max(hi_time, redundant_time - (ZIC_MIN < redundant_time)), ats, types);
	struct timerange range32 = limitrange(range64, ZIC32_MIN, ZIC32_MAX, ats, types);

	version = determine_tzif_version(name, version, &range32, &range64);

	FILE *fp = open_outfile(&outname, &tempname);

	if (!want_bloat()) {
		write_minimal_time_block(fp, version);
	} else {
		write_pass_data(fp, 1, version, ats, types, &range32, &range64);
	}
	write_pass_data(fp, 2, version, ats, types, &range32, &range64);

	fprintf(fp, "\n%s\n", string);
	close_file(fp, directory, name, tempname);
	rename_dest(tempname, name);
	free(ats);
}

static char const *
abbroffset(char *buf, zic_t offset)
{
  char sign = '+';
  zic_t abs_offset = offset;

  if (offset < 0) {
    abs_offset = -offset;
    sign = '-';
  }

  int const seconds = abs_offset % SECSPERMIN;
  zic_t temp_minutes = abs_offset / SECSPERMIN;
  int const minutes = temp_minutes % MINSPERHOUR;
  zic_t const hours = temp_minutes / MINSPERHOUR;

  if (hours >= 100) {
    error(_("%%z UT offset magnitude exceeds 99:59:59"));
    return "%z";
  }

  if (seconds != 0) {
    sprintf(buf, "%c%02ld%02d%02d", sign, (long)hours, minutes, seconds);
  } else if (minutes != 0) {
    sprintf(buf, "%c%02ld%02d", sign, (long)hours, minutes);
  } else {
    sprintf(buf, "%c%02ld", sign, (long)hours);
  }

  return buf;
}

static char const disable_percent_s[] = "";

static ptrdiff_t
doabbr(char *abbr, struct zone const *zp, char const *letters,
       bool isdst, zic_t save, bool doquotes)
{
	char const * const format = zp->z_format;
	char *slashp = strchr(format, '/');

	if (slashp) {
		if (isdst) {
			strcpy(abbr, slashp + 1);
		} else {
			ptrdiff_t len_prefix = slashp - format;
			memcpy(abbr, format, len_prefix);
			abbr[len_prefix] = '\0';
		}
	} else {
		if (zp->z_format_specifier == 'z') {
			char letterbuf[PERCENT_Z_LEN_BOUND + 1];
			char const *subst = abbroffset(letterbuf, zp->z_stdoff + save);
			sprintf(abbr, format, subst);
		} else if (letters == disable_percent_s) {
			return 0;
		} else if (letters) {
			sprintf(abbr, format, letters);
		} else {
			strcpy(abbr, format);
		}
	}

	ptrdiff_t len = strlen(abbr);
	if (!doquotes) {
		return len;
	}

	char *p = abbr;
	while (is_alpha((unsigned char)*p)) {
		p++;
	}

	if (len > 0 && *p == '\0') {
		return len;
	}

	memmove(abbr + 1, abbr, len);
	abbr[0] = '<';
	abbr[len + 1] = '>';
	abbr[len + 2] = '\0';
	return len + 2;
}

static void
updateminmax(const zic_t year)
{
	if (min_year > year) {
		min_year = year;
	}
	if (max_year < year) {
		max_year = year;
	}
}

static int
stringoffset(char *result, zic_t offset)
{
	unsigned long long abs_offset;
	if (offset < 0) {
		abs_offset = -(unsigned long long)offset;
	} else {
		abs_offset = offset;
	}

	if (abs_offset / (SECSPERMIN * MINSPERHOUR) >= (unsigned long long)(HOURSPERDAY * DAYSPERWEEK)) {
		*result = '\0';
		return 0;
	}

	int hours = abs_offset / (SECSPERMIN * MINSPERHOUR);
	int minutes = (abs_offset / SECSPERMIN) % MINSPERHOUR;
	int seconds = abs_offset % SECSPERMIN;

	int len = sprintf(result, "%s%d", (offset < 0 ? "-" : ""), hours);

	if (minutes == 0 && seconds == 0) {
		return len;
	}

	len += sprintf(result + len, ":%02d", minutes);

	if (seconds != 0) {
		len += sprintf(result + len, ":%02d", seconds);
	}

	return len;
}

static int
stringrule(char *result, struct rule *const rp, zic_t save, zic_t stdoff)
{
	zic_t tod = rp->r_tod;
	int compat = 0;

	switch (rp->r_dycode) {
	case DC_DOM:
	{
		if (rp->r_dayofmonth == 29 && rp->r_month == TM_FEBRUARY) {
			return -1;
		}

		int total = 0;
		for (int month = 0; month < rp->r_month; ++month) {
			total += len_months[0][month];
		}

		if (rp->r_month <= 1) {
			result += sprintf(result, "%d", total + rp->r_dayofmonth - 1);
		} else {
			result += sprintf(result, "J%d", total + rp->r_dayofmonth);
		}
		break;
	}

	case DC_DOWGEQ:
	{
		int wday = rp->r_wday;
		int wdayoff = (rp->r_dayofmonth - 1) % DAYSPERWEEK;
		if (wdayoff != 0) {
			compat = 2013;
		}
		wday -= wdayoff;
		tod += wdayoff * SECSPERDAY;
		int week = 1 + (rp->r_dayofmonth - 1) / DAYSPERWEEK;

		if (wday < 0) {
			wday += DAYSPERWEEK;
		}
		result += sprintf(result, "M%d.%d.%d", rp->r_month + 1, week, wday);
		break;
	}

	case DC_DOWLEQ:
	{
		int wday = rp->r_wday;
		int week;
		if (rp->r_dayofmonth == len_months[1][rp->r_month]) {
			week = 5;
		} else {
			int wdayoff = rp->r_dayofmonth % DAYSPERWEEK;
			if (wdayoff != 0) {
				compat = 2013;
			}
			wday -= wdayoff;
			tod += wdayoff * SECSPERDAY;
			week = rp->r_dayofmonth / DAYSPERWEEK;
		}

		if (wday < 0) {
			wday += DAYSPERWEEK;
		}
		result += sprintf(result, "M%d.%d.%d", rp->r_month + 1, week, wday);
		break;
	}

	default:
		return -1;
	}

	if (rp->r_todisut) {
		tod += stdoff;
	}
	if (rp->r_todisstd && !rp->r_isdst) {
		tod += save;
	}

	const zic_t default_tod = 2 * SECSPERMIN * MINSPERHOUR;
	if (tod != default_tod) {
		*result++ = '/';
		if (!stringoffset(result, tod)) {
			return -1;
		}

		if (tod < 0) {
			if (compat < 2013) {
				compat = 2013;
			}
		} else if (tod >= SECSPERDAY) {
			if (compat < 1994) {
				compat = 1994;
			}
		}
	}

	return compat;
}

static int
rule_cmp(struct rule const *a, struct rule const *b)
{
	if (a == NULL) {
		return (b == NULL) ? 0 : -1;
	}
	if (b == NULL) {
		return 1;
	}

	if (a->r_hiyear != b->r_hiyear) {
		return a->r_hiyear < b->r_hiyear ? -1 : 1;
	}

	if (a->r_hiyear == ZIC_MAX) {
		return 0;
	}

	if (a->r_month != b->r_month) {
		return a->r_month < b->r_month ? -1 : 1;
	}

	if (a->r_dayofmonth != b->r_dayofmonth) {
		return a->r_dayofmonth < b->r_dayofmonth ? -1 : 1;
	}

	return 0;
}

/* Store into RESULT a proleptic TZ string that represent the future
   predictions for the zone ZPFIRST with ZONECOUNT entries.  Return a
   compatibility indicator (a TZDB release year) if successful, a
   negative integer if no such TZ string exists.  */
static int
stringzone(char *result, struct zone const *zpfirst, ptrdiff_t zonecount)
{
	result[0] = '\0';

	/* Internet RFC 8536 section 5.1 says to use an empty TZ string if
	   future timestamps are truncated.  */
	if (hi_time < max_time) {
		return -1;
	}

	const struct zone *zp = zpfirst + zonecount - 1;
	struct rule *lastrp[2] = { NULL, NULL };
	for (ptrdiff_t i = 0; i < zp->z_nrules; ++i) {
		struct rule *rp = &zp->z_rules[i];
		struct rule **last = &lastrp[rp->r_isdst];
		int cmp = rule_cmp(*last, rp);
		if (cmp < 0) {
			*last = rp;
		} else if (cmp == 0) {
			return -1;
		}
	}

	struct rule *stdrp = lastrp[false];
	struct rule *dstrp = lastrp[true];
	const struct zone *stdzp = zp;
	const struct zone *dstzp = zp;
	int dstcmp = zp->z_nrules ? rule_cmp(dstrp, stdrp) : (zp->z_isdst ? 1 : -1);

	struct zone local_zones[2];
	struct rule local_rules[2];

	if (dstcmp < 0) {
		/* Standard time all year.  */
		dstrp = NULL;
	} else if (dstcmp > 0) {
		/* DST all year.  Use an abbreviation like
		   "XXX3EDT4,0/0,J365/23" for EDT (-04) all year.  */
		zic_t save = dstrp ? dstrp->r_save : zp->z_save;
		if (save >= 0) {
			/* Positive DST, the typical case for all-year DST.
			   Fake a timezone with negative DST.  */
			stdzp = &local_zones[0];
			dstzp = &local_zones[1];

			local_zones[0].z_stdoff = zp->z_stdoff + 2 * save;
			local_zones[0].z_format = "XXX"; /* Any 3 letters will do. */
			local_zones[0].z_format_specifier = 0;

			local_zones[1].z_stdoff = local_zones[0].z_stdoff;
			local_zones[1].z_format = zp->z_format;
			local_zones[1].z_format_specifier = zp->z_format_specifier;
		}

		struct rule *new_dstr = &local_rules[0];
		*new_dstr = (struct rule){
			.r_month = TM_JANUARY, .r_dycode = DC_DOM, .r_dayofmonth = 1,
			.r_tod = 0, .r_todisstd = false, .r_todisut = false,
			.r_isdst = true, .r_save = (save < 0 ? save : -save),
			.r_abbrvar = (dstrp ? dstrp->r_abbrvar : NULL)
		};
		dstrp = new_dstr;

		struct rule *new_stdr = &local_rules[1];
		*new_stdr = (struct rule){
			.r_month = TM_DECEMBER, .r_dycode = DC_DOM, .r_dayofmonth = 31,
			.r_tod = SECSPERDAY + dstrp->r_save, .r_todisstd = false,
			.r_todisut = false, .r_isdst = false, .r_save = 0,
			.r_abbrvar = (save < 0 && stdrp ? stdrp->r_abbrvar : NULL)
		};
		stdrp = new_stdr;
	}

	int compat = 0;
	char *p = result;
	ptrdiff_t written;

	p += doabbr(p, stdzp, stdrp ? stdrp->r_abbrvar : NULL, false, 0, true);

	written = stringoffset(p, -stdzp->z_stdoff);
	if (written == 0) {
		goto fail;
	}
	p += written;

	if (dstrp == NULL) {
		*p = '\0';
		return compat;
	}

	p += doabbr(p, dstzp, dstrp->r_abbrvar, dstrp->r_isdst, dstrp->r_save, true);

	if (dstrp->r_save != SECSPERMIN * MINSPERHOUR) {
		written = stringoffset(p, -(dstzp->z_stdoff + dstrp->r_save));
		if (written == 0) {
			goto fail;
		}
		p += written;
	}

	*p++ = ',';
	int c = stringrule(p, dstrp, dstrp->r_save, stdzp->z_stdoff);
	if (c < 0) {
		goto fail;
	}
	if (compat < c) {
		compat = c;
	}
	p += strlen(p);

	*p++ = ',';
	c = stringrule(p, stdrp, dstrp->r_save, stdzp->z_stdoff);
	if (c < 0) {
		goto fail;
	}
	if (compat < c) {
		compat = c;
	}

	return compat;

fail:
	result[0] = '\0';
	return -1;
}

static void
compute_year_range(const struct zone *zpfirst, ptrdiff_t zonecount)
{
	min_year = max_year = EPOCH_YEAR;
	if (leapseen) {
		updateminmax(leapminyear);
		updateminmax(leapmaxyear + (leapmaxyear < ZIC_MAX));
	}
	for (ptrdiff_t i = 0; i < zonecount; ++i) {
		const struct zone *zp = &zpfirst[i];
		if (i < zonecount - 1) {
			updateminmax(zp->z_untilrule.r_loyear);
		}
		for (ptrdiff_t j = 0; j < zp->z_nrules; ++j) {
			const struct rule *rp = &zp->z_rules[j];
			updateminmax(rp->r_loyear);
			if (rp->r_hiwasnum) {
				updateminmax(rp->r_hiyear);
			}
		}
	}
}

static void
adjust_year_range(bool do_extend)
{
	if (do_extend) {
		if (min_year >= ZIC_MIN + years_of_observations)
			min_year -= years_of_observations;
		else
			min_year = ZIC_MIN;
		if (max_year <= ZIC_MAX - years_of_observations)
			max_year += years_of_observations;
		else
			max_year = ZIC_MAX;
	}

	max_year = max(max_year, (redundant_time / (SECSPERDAY * DAYSPERNYEAR)
				  + EPOCH_YEAR + 1));

	if (want_bloat()) {
		if (min_year > YEAR_32BIT_MIN - 1)
			min_year = YEAR_32BIT_MIN - 1;
		if (max_year < YEAR_32BIT_MAX)
			max_year = YEAR_32BIT_MAX;
	}
}

static void
mark_rules_for_year(const struct zone *zp, zic_t year, zic_t max_year0)
{
	const zic_t y2038_boundary = (zic_t)1 << 31;
	for (ptrdiff_t j = 0; j < zp->z_nrules; ++j) {
		struct rule *rp = &zp->z_rules[j];
		eats(zp->z_filenum, zp->z_linenum, rp->r_filenum, rp->r_linenum);
		rp->r_todo = (year >= rp->r_loyear && year <= rp->r_hiyear);
		if (rp->r_todo) {
			rp->r_temp = rpytime(rp, year);
			rp->r_todo = (rp->r_temp < y2038_boundary || year <= max_year0);
		}
	}
}

static ptrdiff_t
find_earliest_transition(const struct zone *zp, zic_t stdoff, zic_t save, zic_t *ktimep)
{
	ptrdiff_t k = -1;
	for (ptrdiff_t j = 0; j < zp->z_nrules; ++j) {
		struct rule *r = &zp->z_rules[j];
		if (!r->r_todo)
			continue;
		eats(zp->z_filenum, zp->z_linenum, r->r_filenum, r->r_linenum);
		zic_t offset = r->r_todisut ? 0 : stdoff;
		if (!r->r_todisstd)
			offset = oadd(offset, save);
		zic_t jtime = r->r_temp;
		if (jtime == min_time || jtime == max_time)
			continue;
		jtime = tadd(jtime, -offset);

		if (k < 0 || jtime < *ktimep) {
			k = j;
			*ktimep = jtime;
		} else if (jtime == *ktimep) {
			const char *dup_rules_msg = _("two rules for same instant");
			eats(zp->z_filenum, zp->z_linenum, r->r_filenum, r->r_linenum);
			warning("%s", dup_rules_msg);
			r = &zp->z_rules[k];
			eats(zp->z_filenum, zp->z_linenum, r->r_filenum, r->r_linenum);
			error("%s", dup_rules_msg);
		}
	}
	return k;
}

static void
trim_deducible_transitions(const char *envvar, zic_t nonTZlimtime, int nonTZlimtype)
{
	zic_t TZstarttime = ZIC_MAX;
	for (ptrdiff_t i = 0; i < timecnt; i++) {
		zic_t at = attypes[i].at;
		if (nonTZlimtime < at && at < TZstarttime)
			TZstarttime = at;
	}

	if (TZstarttime == ZIC_MAX)
		TZstarttime = nonTZlimtime;

	zic_t keep_at_max = max(TZstarttime, redundant_time);
	ptrdiff_t j = 0;
	for (ptrdiff_t i = 0; i < timecnt; i++) {
		if (attypes[i].at <= keep_at_max) {
			attypes[j].at = attypes[i].at;
			attypes[j].dontmerge = (attypes[i].at == TZstarttime
					      && (nonTZlimtype != attypes[i].type
						  || strchr(envvar, ',')));
			attypes[j].type = attypes[i].type;
			j++;
		}
	}
	timecnt = j;
}

static void
ensure_final_transition(int defaulttype)
{
	struct rule xr = {
		.r_month = TM_JANUARY,
		.r_dycode = DC_DOM,
		.r_dayofmonth = 1,
		.r_tod = 0
	};
	struct attype *lastat = attypes;

	for (ptrdiff_t i = 1; i < timecnt; i++)
		if (attypes[i].at > lastat->at)
			lastat = &attypes[i];

	if (timecnt == 0 || lastat->at < rpytime(&xr, max_year - 1)) {
		addtt(rpytime(&xr, max_year + 1), timecnt > 0 ? lastat->type : defaulttype);
		attypes[timecnt - 1].dontmerge = true;
	}
}

static void
outzone(const struct zone *zpfirst, ptrdiff_t zonecount)
{
	check_for_signal();

	const int max_abbr_len = 2 + max_format_len + max_abbrvar_len;
	const int max_envvar_len = 2 * max_abbr_len + 5 * 9;

	char *startbuf = emalloc(max_abbr_len + 1);
	char *ab = emalloc(max_abbr_len + 1);
	char *envvar = emalloc(max_envvar_len + 1);

	timecnt = 0;
	typecnt = 0;
	charcnt = 0;
	
	zic_t starttime = 0;
	bool startttisstd = false;
	bool startttisut = false;
	int defaulttype = -1;
	zic_t nonTZlimtime = ZIC_MIN;
	int nonTZlimtype = -1;

	compute_year_range(zpfirst, zonecount);

	const int compat = stringzone(envvar, zpfirst, zonecount);
	const char version = compat < 2013 ? '2' : '3';
	const bool do_extend = compat < 0;

	if (noise) {
		if (!*envvar)
			warning("%s %s", _("no proleptic TZ string for zone"), zpfirst->z_name);
		else if (compat != 0)
			warning(_("%s: pre-%d clients may mishandle distant timestamps"),
				zpfirst->z_name, compat);
	}

	zic_t max_year0 = max_year;
	adjust_year_range(do_extend);

	if (min_time < lo_time || hi_time < max_time)
		unspecifiedtype = addtype(0, "-00", false, false, false);

	for (ptrdiff_t i = 0; i < zonecount; ++i) {
		const struct zone *zp = &zpfirst[i];
		bool usestart = i > 0 && (zp - 1)->z_untiltime > min_time;
		bool useuntil = i < (zonecount - 1);

		if (useuntil && zp->z_untiltime <= min_time)
			continue;

		eat(zp->z_filenum, zp->z_linenum);
		*startbuf = '\0';
		zic_t save = 0;
		zic_t startoff = zp->z_stdoff;

		if (zp->z_nrules == 0) {
			save = zp->z_save;
			doabbr(startbuf, zp, NULL, zp->z_isdst, save, false);
			int type = addtype(oadd(zp->z_stdoff, save), startbuf, zp->z_isdst, startttisstd, startttisut);
			if (usestart) {
				addtt(starttime, type);
				if (nonTZlimtime < starttime) {
					nonTZlimtime = starttime;
					nonTZlimtype = type;
				}
			} else {
				defaulttype = type;
			}
		} else {
			for (zic_t year = min_year; year <= max_year; ++year) {
				if (useuntil && year > zp->z_untilrule.r_hiyear)
					break;

				mark_rules_for_year(zp, year, max_year0);
				
				for (;;) {
					zic_t ktime;
					ptrdiff_t k = find_earliest_transition(zp, zp->z_stdoff, save, &ktime);
					if (k < 0)
						break;
					
					struct rule *rp = &zp->z_rules[k];
					rp->r_todo = false;

					zic_t untiltime = zp->z_untiltime;
					if (!zp->z_untilrule.r_todisut)
						untiltime = tadd(untiltime, -zp->z_stdoff);
					if (!zp->z_untilrule.r_todisstd)
						untiltime = tadd(untiltime, -save);

					if (useuntil && ktime >= untiltime) {
						if (!*startbuf && (oadd(zp->z_stdoff, rp->r_save) == startoff))
							doabbr(startbuf, zp, rp->r_abbrvar, rp->r_isdst, rp->r_save, false);
						goto next_zone;
					}

					save = rp->r_save;

					if (usestart) {
						if (ktime < starttime) {
							startoff = oadd(zp->z_stdoff, save);
							doabbr(startbuf, zp, rp->r_abbrvar, rp->r_isdst, rp->r_save, false);
							continue;
						}
						if (ktime == starttime) {
							usestart = false;
						} else if (*startbuf == '\0' && startoff == oadd(zp->z_stdoff, save)) {
							doabbr(startbuf, zp, rp->r_abbrvar, rp->r_isdst, rp->r_save, false);
						}
					}
					
					eats(zp->z_filenum, zp->z_linenum, rp->r_filenum, rp->r_linenum);
					doabbr(ab, zp, rp->r_abbrvar, rp->r_isdst, rp->r_save, false);
					zic_t offset = oadd(zp->z_stdoff, rp->r_save);
					int type = addtype(offset, ab, rp->r_isdst, rp->r_todisstd, rp->r_todisut);

					if (defaulttype < 0 && !rp->r_isdst)
						defaulttype = type;
					
					addtt(ktime, type);
					if (nonTZlimtime < ktime && (useuntil || rp->r_hiyear != ZIC_MAX)) {
						nonTZlimtime = ktime;
						nonTZlimtype = type;
					}
				}
			}
		}

next_zone:
		if (usestart) {
			bool isdst = startoff != zp->z_stdoff;
			if (*startbuf == '\0' && zp->z_format)
				doabbr(startbuf, zp, disable_percent_s, isdst, save, false);
			
			eat(zp->z_filenum, zp->z_linenum);
			if (*startbuf == '\0') {
				error(_("can't determine time zone abbreviation to use just after until time"));
			} else {
				int type = addtype(startoff, startbuf, isdst, startttisstd, startttisut);
				if (defaulttype < 0 && !isdst)
					defaulttype = type;
				addtt(starttime, type);
			}
		}

		if (useuntil) {
			startttisstd = zp->z_untilrule.r_todisstd;
			startttisut = zp->z_untilrule.r_todisut;
			starttime = zp->z_untiltime;
			if (!startttisstd)
				starttime = tadd(starttime, -save);
			if (!startttisut)
				starttime = tadd(starttime, -zp->z_stdoff);
		}
	}

	if (defaulttype < 0)
		defaulttype = 0;
	
	if (!do_extend && !want_bloat())
		trim_deducible_transitions(envvar, nonTZlimtime, nonTZlimtype);

	if (do_extend)
		ensure_final_transition(defaulttype);

	writezone(zpfirst->z_name, envvar, version, defaulttype);
	free(startbuf);
	free(ab);
	free(envvar);
}

static void
add_transition_time(zic_t starttime, int type)
{
	void *new_attypes = growalloc(attypes, sizeof *attypes, timecnt, &timecnt_alloc);
	if (new_attypes == NULL)
	{
		perror("memory allocation failed");
		exit(EXIT_FAILURE);
	}
	attypes = new_attypes;

	attypes[timecnt].at = starttime;
	attypes[timecnt].dontmerge = false;
	attypes[timecnt].type = type;
	timecnt++;
}

static int
addtype(zic_t utoff, char const *abbr, bool isdst, bool ttisstd, bool ttisut)
{
	if (utoff < -2147483647L - 1 || utoff > 2147483647L) {
		error(_("UT offset out of range"));
		exit(EXIT_FAILURE);
	}

	if (!want_bloat()) {
		ttisstd = ttisut = false;
	}

	int abbr_idx;
	for (abbr_idx = 0; abbr_idx < charcnt; ++abbr_idx) {
		if (strcmp(&chars[abbr_idx], abbr) == 0) {
			break;
		}
	}

	if (abbr_idx < charcnt) {
		for (int type_idx = 0; type_idx < typecnt; ++type_idx) {
			if (utoffs[type_idx] == utoff &&
			    isdsts[type_idx] == isdst &&
			    desigidx[type_idx] == abbr_idx &&
			    ttisstds[type_idx] == ttisstd &&
			    ttisuts[type_idx] == ttisut) {
				return type_idx;
			}
		}
	} else {
		newabbr(abbr);
	}

	if (typecnt >= TZ_MAX_TYPES) {
		error(_("too many local time types"));
		exit(EXIT_FAILURE);
	}

	const int new_type_idx = typecnt;
	utoffs[new_type_idx] = utoff;
	isdsts[new_type_idx] = isdst;
	ttisstds[new_type_idx] = ttisstd;
	ttisuts[new_type_idx] = ttisut;
	desigidx[new_type_idx] = abbr_idx;
	typecnt++;

	return new_type_idx;
}

static void
leapadd(zic_t t, int correction, int rolling)
{
	if (leapcnt >= TZ_MAX_LEAPS) {
		error(_("too many leap seconds"));
		exit(EXIT_FAILURE);
	}
	if (rolling && (lo_time != min_time || hi_time != max_time)) {
		error(_("Rolling leap seconds not supported with -r"));
		exit(EXIT_FAILURE);
	}

	int i = 0;
	while (i < leapcnt && t > trans[i]) {
		i++;
	}

	const size_t n_to_move = leapcnt - i;
	if (n_to_move > 0) {
		memmove(&trans[i + 1], &trans[i], n_to_move * sizeof(*trans));
		memmove(&corr[i + 1], &corr[i], n_to_move * sizeof(*corr));
		memmove(&roll[i + 1], &roll[i], n_to_move * sizeof(*roll));
	}

	trans[i] = t;
	corr[i] = correction;
	roll[i] = rolling;
	++leapcnt;
}

static void
adjleap(void)
{
	const zic_t min_leap_interval = 28 * SECSPERDAY;
	zic_t cumulative_correction = 0;
	zic_t previous_transition = 0;

	for (int i = 0; i < leapcnt; ++i) {
		if (trans[i] - previous_transition < min_leap_interval) {
			error(_("Leap seconds too close together"));
			exit(EXIT_FAILURE);
		}

		previous_transition = trans[i];
		trans[i] = tadd(trans[i], cumulative_correction);

		cumulative_correction += corr[i];
		corr[i] = cumulative_correction;
	}

	if (leapexpires >= 0) {
		leapexpires = oadd(leapexpires, cumulative_correction);

		if (leapcnt > 0 && trans[leapcnt - 1] >= leapexpires) {
			error(_("last Leap time does not precede Expires time"));
			exit(EXIT_FAILURE);
		}
	}
}

/* Is A a space character in the C locale?  */
static bool
is_space(char a)
{
    return a == ' ' || a == '\f' || a == '\n' || a == '\r' || a == '\t' || a == '\v';
}

/* Is A an alphabetic character in the C locale?  */
static bool
is_alpha(char a)
{
	return (a >= 'a' && a <= 'z') || (a >= 'A' && a <= 'Z');
}

/* If A is an uppercase character in the C locale, return its lowercase
   counterpart.  Otherwise, return A.  */
#include <ctype.h>

static char
lowerit(char a)
{
    return (char)tolower((unsigned char)a);
}

/* case-insensitive equality */
ATTRIBUTE_PURE_114833 static bool
ciequal(register const char *ap, register const char *bp)
{
	while (lowerit(*ap) == lowerit(*bp++))
		if (*ap++ == '\0')
			return true;
	return false;
}

ATTRIBUTE_PURE_114833 static bool
itsabbr(register const char *abbr, register const char *word)
{
	if (lowerit(*abbr) != lowerit(*word))
		return false;
	++word;
	while (*++abbr != '\0')
		do {
			if (*word == '\0')
				return false;
		} while (lowerit(*word++) != lowerit(*abbr));
	return true;
}

/* Return true if ABBR is an initial prefix of WORD, ignoring ASCII case.  */

ATTRIBUTE_PURE_114833 static bool
ciprefix(char const *abbr, char const *word)
{
  do
    if (!*abbr)
      return true;
  while (lowerit(*abbr++) == lowerit(*word++));

  return false;
}

static const struct lookup *
byword(const char *word, const struct lookup *table)
{
	if (word == NULL || table == NULL) {
		return NULL;
	}

	if (table == lasts && ciprefix("last", word) && word[4] != '\0') {
		if (word[4] == '-') {
			warning(_("\"%s\" is undocumented; use \"last%s\" instead"),
				word, word + 5);
		} else {
			word += 4;
			table = wday_names;
		}
	}

	const struct lookup *inexact_match = NULL;
	bool multiple_inexact_matches = false;
	int pre_2017c_match_count = 0;

	for (const struct lookup *lp = table; lp->l_word != NULL; ++lp) {
		if (ciequal(word, lp->l_word)) {
			return lp;
		}

		if (ciprefix(word, lp->l_word)) {
			if (inexact_match == NULL) {
				inexact_match = lp;
			} else {
				multiple_inexact_matches = true;
			}
		}

		if (noise && itsabbr(word, lp->l_word)) {
			pre_2017c_match_count++;
		}
	}

	if (multiple_inexact_matches) {
		return NULL;
	}

	if (inexact_match != NULL && noise && pre_2017c_match_count > 1) {
		warning(_("\"%s\" is ambiguous in pre-2017c zic"), word);
	}

	return inexact_match;
}

static int
getfields(char *cp, char **array, int arrayelts)
{
	int nsubs = 0;

	while (1) {
		while (is_space(*cp)) {
			cp++;
		}

		if (*cp == '\0' || *cp == '#') {
			break;
		}

		if (nsubs >= arrayelts) {
			error(_("Too many input fields"));
			exit(EXIT_FAILURE);
		}

		char *dstart = cp;
		char *dp = cp;

		while (*cp != '\0' && *cp != '#' && !is_space(*cp)) {
			if (*cp != '"') {
				*dp++ = *cp++;
			} else {
				cp++; /* Skip opening quote */
				while (*cp != '"') {
					if (*cp == '\0') {
						error(_("Odd number of quotation marks"));
						exit(EXIT_FAILURE);
					}
					*dp++ = *cp++;
				}
				cp++; /* Skip closing quote */
			}
		}

		if (is_space(*cp)) {
			cp++;
		}
		*dp = '\0';

		if (*dstart == '-' && dp == dstart + 1) {
			array[nsubs++] = dp;
		} else {
			array[nsubs++] = dstart;
		}
	}
	return nsubs;
}

_Noreturn static void time_overflow(void)
{
    error(_("time overflow"));
    exit(EXIT_FAILURE);
}

ATTRIBUTE_PURE_114833 static zic_t
oadd(zic_t t1, zic_t t2)
{
#ifdef ckd_add
  zic_t sum;
  if (!ckd_add(&sum, t1, t2))
    return sum;
#else
  if (t1 < 0 ? ZIC_MIN - t1 <= t2 : t2 <= ZIC_MAX - t1)
    return t1 + t2;
#endif
  time_overflow();
}

ATTRIBUTE_PURE_114833 static zic_t
tadd(zic_t t1, zic_t t2)
{
#ifdef ckd_add
  zic_t sum;
  if (!ckd_add(&sum, t1, t2) && min_time <= sum && sum <= max_time)
    return sum;
#else
  if (t1 < 0 ? min_time - t1 <= t2 : t2 <= max_time - t1)
    return t1 + t2;
#endif
  if (t1 == min_time || t1 == max_time)
    return t1;
  time_overflow();
}

/*
** Given a rule, and a year, compute the date (in seconds since January 1,
** 1970, 00:00 LOCAL time) in that year that the rule refers to.
*/

static zic_t
rpytime(const struct rule *rp, zic_t wantedy)
{
	if (wantedy == ZIC_MIN) {
		return min_time;
	}
	if (wantedy == ZIC_MAX) {
		return max_time;
	}

	zic_t y = EPOCH_YEAR;
	zic_t dayoff = 0;
	zic_t year_diff = wantedy - y;

	if (year_diff != 0) {
		zic_t cycle_diff = year_diff / YEARSPERREPEAT;
		if (year_diff < 0 && (year_diff % YEARSPERREPEAT) != 0) {
			cycle_diff--;
		}
		dayoff = cycle_diff * DAYSPERREPEAT;
		y += cycle_diff * YEARSPERREPEAT;
	}

	while (y < wantedy) {
		dayoff = oadd(dayoff, len_years[isleap(y)]);
		y++;
	}
	while (y > wantedy) {
		y--;
		dayoff = oadd(dayoff, -len_years[isleap(y)]);
	}

	int m = TM_JANUARY;
	while (m != rp->r_month) {
		dayoff = oadd(dayoff, len_months[isleap(y)][m]);
		m++;
	}

	int day_of_month = rp->r_dayofmonth;
	if (m == TM_FEBRUARY && day_of_month == 29 && !isleap(y)) {
		if (rp->r_dycode == DC_DOWLEQ) {
			day_of_month = 28;
		} else {
			error(_("use of 2/29 in non leap-year"));
			exit(EXIT_FAILURE);
		}
	}

	dayoff = oadd(dayoff, day_of_month - 1);

	if (rp->r_dycode == DC_DOWGEQ || rp->r_dycode == DC_DOWLEQ) {
		zic_t wday = (EPOCH_WDAY + dayoff % DAYSPERWEEK + DAYSPERWEEK) % DAYSPERWEEK;
		int day_shift;

		if (rp->r_dycode == DC_DOWGEQ) {
			day_shift = (rp->r_wday - wday + DAYSPERWEEK) % DAYSPERWEEK;
		} else {
			day_shift = -((wday - rp->r_wday + DAYSPERWEEK) % DAYSPERWEEK);
		}

		dayoff = oadd(dayoff, day_shift);

		int adjusted_day_0based = (day_of_month - 1) + day_shift;
		if (adjusted_day_0based < 0 || adjusted_day_0based >= len_months[isleap(y)][m]) {
			if (noise) {
				warning(_("rule goes past start/end of month; "
					  "will not work with pre-2004 versions of zic"));
			}
		}
	}

	if (dayoff < min_time / SECSPERDAY) {
		return min_time;
	}
	if (dayoff > max_time / SECSPERDAY) {
		return max_time;
	}

	zic_t t = dayoff * SECSPERDAY;
	return tadd(t, rp->r_tod);
}

static void
newabbr(const char *string)
{
	if (strcmp(string, GRANDPARENTED) != 0) {
		const char *cp = string;
		while (is_alpha(*cp) || ('0' <= *cp && *cp <= '9')
		       || *cp == '-' || *cp == '+') {
			++cp;
		}

		const char *message = NULL;
		const ptrdiff_t abbr_len = cp - string;

		if (*cp != '\0') {
			message = _("time zone abbreviation differs from POSIX standard");
		} else if (abbr_len > ZIC_MAX_ABBR_LEN_WO_WARN) {
			message = _("time zone abbreviation has too many characters");
		} else if (noise && abbr_len < 3) {
			message = _("time zone abbreviation has fewer than 3 characters");
		}

		if (message != NULL) {
			warning("%s (%s)", message, string);
		}
	}

	const size_t len_with_null = strlen(string) + 1;
	if (charcnt + len_with_null > TZ_MAX_CHARS) {
		error(_("too many, or too long, time zone abbreviations"));
		exit(EXIT_FAILURE);
	}

	memcpy(&chars[charcnt], string, len_with_null);
	charcnt += len_with_null;
}

/* Ensure that the directories of ARGNAME exist, by making any missing
   ones.  If ANCESTORS, do this only for ARGNAME's ancestors; otherwise,
   do it for ARGNAME too.  Exit with failure if there is trouble.
   Do not consider an existing file to be trouble.  */
static void
make_directory(const char *path)
{
	if (mkdir(path, MKDIR_UMASK) != 0) {
		int err = errno;
		switch (err) {
		case ELOOP:
		case ENAMETOOLONG:
		case ENOENT:
		case ENOTDIR:
			error(_("%s: Can't create directory %s: %s"),
			      progname, path, strerror(err));
			exit(EXIT_FAILURE);
		default:
			break;
		}
	}
}

static void
mkdirs(char const *argname, bool ancestors)
{
	char *name = estrdup(argname);
	char *p = name;

	while (*p == '/') {
		p++;
	}

	for (char *slash = strchr(p, '/'); slash; slash = strchr(slash + 1, '/')) {
		*slash = '\0';
		make_directory(name);
		*slash = '/';
	}

	if (!ancestors) {
		make_directory(name);
	}

	free(name);
}
