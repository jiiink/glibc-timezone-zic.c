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
#include <errno.h>
#include <sys/types.h>

static ssize_t readlink(char const *restrict file, char *restrict buf, size_t size)
{
    (void)file;
    (void)buf;
    (void)size;

    errno = ENOTSUP;
    return -1;
}
#include <errno.h>

static int symlink(const char *target, const char *linkname)
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
	const char *program = progname != NULL ? progname : "unknown";
	const char *detail = msg != NULL ? msg : "unknown";

	if (fprintf(stderr, _("%s: Memory exhausted: %s\n"), program, detail) < 0) {
		fputs("Memory exhausted\n", stderr);
	}
	exit(EXIT_FAILURE);
}

#include <stdlib.h>
ATTRIBUTE_NORETURN static void
size_overflow(void)
{
  memory_exhausted(_("size overflow"));
  abort();
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
    if (str == NULL) {
        return NULL;
    }

    size_t len = strlen(str);
    size_t size = len + 1;
    if (size < len) {
        return NULL;
    }

    char *result = malloc(size);
    if (result == NULL) {
        return NULL;
    }

    memcpy(result, str, size);
    return result;
}
#endif

static void *
memcheck(void *ptr)
{
    if (ptr != NULL) {
        return ptr;
    }

    int errcode = HAVE_MALLOC_ERRNO ? errno : ENOMEM;
    const char *msg = strerror(errcode);
    if (msg == NULL) {
        msg = "Out of memory";
    }
    memory_exhausted(msg);
    return NULL;
}

static void *
emalloc(size_t size)
{
  void *ptr = malloc(size);
  return memcheck(ptr);
}

static void *
erealloc(void *ptr, size_t size)
{
    void *new_ptr = realloc(ptr, size);
    return memcheck(new_ptr);
}

static char *
estrdup(char const *str)
{
    size_t len = strlen(str) + 1;
    char *copy = memcheck(malloc(len));
    memcpy(copy, str, len);
    return copy;
}

static ptrdiff_t
grow_nitems_alloc(ptrdiff_t *nitems_alloc, ptrdiff_t itemsize)
{
  ptrdiff_t n = *nitems_alloc;
  ptrdiff_t addend = (n >> 1) + 1;

#if defined ckd_add && defined ckd_mul
  ptrdiff_t new_n, product;
  if (!ckd_add(&new_n, n, addend)
      && !ckd_mul(&product, new_n, itemsize)
      && product <= INDEX_MAX) {
    *nitems_alloc = new_n;
    return product;
  }
#else
  ptrdiff_t limit = (((INDEX_MAX - 1) / 3) * 2) / itemsize;
  if (n <= limit) {
    n += addend;
    *nitems_alloc = n;
    return n * itemsize;
  }
#endif
  memory_exhausted(_("integer overflow"));
  return 0;
}

static void *
growalloc(void *ptr, ptrdiff_t itemsize, ptrdiff_t nitems, ptrdiff_t *nitems_alloc)
{
    if (nitems < *nitems_alloc) {
        return ptr;
    }

    ptrdiff_t new_size = grow_nitems_alloc(nitems_alloc, itemsize);
    return erealloc(ptr, new_size);
}

/*
** Error handling.
*/

/* In most of the code, an input file name is represented by its index
   into the main argument vector, except that LEAPSEC_FILENUM stands
   for leapsec and COMMAND_LINE_FILENUM stands for the command line.  */
enum { LEAPSEC_FILENUM = -2, COMMAND_LINE_FILENUM = -1 };

/* Return the name of the Ith input file, for diagnostics.  */
static const char *
filename(int i)
{
    if (i == COMMAND_LINE_FILENUM) {
        return _("command line");
    }

    const char *fname = (i == LEAPSEC_FILENUM) ? leapsec : (main_argv ? main_argv[i] : NULL);

    if (fname && strcmp(fname, "-") == 0) {
        return _("standard input");
    }

    return fname;
}

static void
eats(int file_num, lineno line_num, int rfile_num, lineno rline_num)
{
	filenum = file_num;
	linenum = line_num;
	rfilenum = rfile_num;
	rlinenum = rline_num;
}

static void eat(int fnum, lineno num)
{
    const int default_flag = 0;
    const int end_marker = -1;
    eats(fnum, num, default_flag, end_marker);
}

ATTRIBUTE_FORMAT((printf, 1, 0)) static void
verror(const char *const string, va_list args)
{
	const char *main_name = NULL;
	const char *rule_name = NULL;

	if (filenum) {
		const char *tmp = filename(filenum);
		main_name = tmp ? tmp : "(unknown)";
		fprintf(stderr, _("\"%s\", line %" PRIdMAX ": "), main_name, linenum);
	}

	if (string) {
		vfprintf(stderr, string, args);
	} else {
		fputs("(null)", stderr);
	}

	if (rfilenum) {
		const char *tmp = filename(rfilenum);
		rule_name = tmp ? tmp : "(unknown)";
		fprintf(stderr, _(" (rule from \"%s\", line %" PRIdMAX ")"), rule_name, rlinenum);
	}

	fputc('\n', stderr);
}

ATTRIBUTE_FORMAT((printf, 1, 2)) static void
error(const char *format, ...)
{
	if (format == NULL) {
		errors = true;
		return;
	}

	va_list args;
	va_start(args, format);
	verror(format, args);
	va_end(args);
	errors = true;
}

ATTRIBUTE_FORMAT((printf, 1, 2)) static void
warning(const char *const string, ...)
{
	va_list args;
	(void)fputs(_("warning: "), stderr);
	va_start(args, string);
	verror(string, args);
	va_end(args);
	warnings = true;
}

/* Close STREAM.  If it had an I/O error, report it against DIR/NAME,
   remove TEMPNAME if nonnull, and then exit.  */
static void
close_file(FILE *stream, char const *dir, char const *name, char const *tempname)
{
  const char *error_msg = NULL;

  if (ferror(stream)) {
    error_msg = _("I/O error");
  } else {
    if (fclose(stream) != 0) {
      int saved_errno = errno;
      error_msg = strerror(saved_errno);
    }
  }

  if (error_msg) {
    const char *dir_str = dir ? dir : "";
    const char *dir_sep = dir ? "/" : "";
    const char *name_str = name ? name : "";
    const char *name_sep = name ? ": " : "";

    fprintf(stderr, "%s: %s%s%s%s%s\n", progname,
            dir_str, dir_sep, name_str, name_sep, error_msg);

    if (tempname)
      remove(tempname);

    exit(EXIT_FAILURE);
  }
}

ATTRIBUTE_NORETURN static void
usage(FILE *stream, int status)
{
  const char *usage_fmt =
    _("%s: usage is %s [ --version ] [ --help ] [ -v ] \\\n"
      "\t[ -b {slim|fat} ] [ -d directory ] [ -l localtime ]"
      " [ -L leapseconds ] \\\n"
      "\t[ -p posixrules ] [ -r '[@lo][/@hi]' ] [ -R '@hi' ] \\\n"
      "\t[ -t localtime-link ] \\\n"
      "\t[ filename ... ]\n\n"
      "Report bugs to %s.\n");
  FILE *out = stream ? stream : (status == EXIT_SUCCESS ? stdout : stderr);

  if (fprintf(out, usage_fmt, progname, progname, REPORT_BUGS_TO) < 0 && out != stderr) {
    fflush(out);
    (void)fprintf(stderr, usage_fmt, progname, progname, REPORT_BUGS_TO);
  }

  if (status == EXIT_SUCCESS)
    close_file(out, NULL, NULL, NULL);
  else
    fflush(out);

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
  const struct link *l = (const struct link *)a;
  const struct link *m = (const struct link *)b;

  if (l == m)
    return 0;

  int cmp = strcmp(l->l_linkname, m->l_linkname);
  if (cmp)
    return cmp;

  if (l->l_filenum < m->l_filenum)
    return -1;
  if (l->l_filenum > m->l_filenum)
    return 1;

  if (l->l_linenum < m->l_linenum)
    return -1;
  if (l->l_linenum > m->l_linenum)
    return 1;

  return 0;
}

/* Compare the string KEY to the link B, for bsearch.  */
static int
bsearch_linkcmp(const void *key, const void *elem)
{
  const struct link *m = elem;
  const char *k = key;
  const char *name = m ? m->l_linkname : NULL;

  if (k == NULL && name == NULL) {
    return 0;
  }
  if (k == NULL) {
    return -1;
  }
  if (name == NULL) {
    return 1;
  }
  return strcmp(k, name);
}

/* Make the links specified by the Link lines.  */
static void
make_links(void)
{
  ptrdiff_t i, j, nalinks, pass_size;

  if (nlinks > 1)
    qsort(links, nlinks, sizeof *links, qsort_linkcmp);

  if (nlinks > 0) {
    ptrdiff_t write = 0;
    ptrdiff_t read = 0;
    while (read < nlinks) {
      ptrdiff_t run_end = read + 1;
      while (run_end < nlinks
             && strcmp(links[read].l_linkname, links[run_end].l_linkname) == 0)
        run_end++;
      links[write++] = links[run_end - 1];
      read = run_end;
    }
    nlinks = write;
    pass_size = write;
  } else {
    pass_size = 0;
  }

  j = nalinks = nlinks;

  for (i = 0; i < nalinks; i++) {
    const char *target = links[i].l_target;
    const char *linkname = links[i].l_linkname;
    struct link *l = NULL;

    eat(links[i].l_filenum, links[i].l_linenum);

    if (i == j) {
      if (nalinks - i == pass_size) {
        error(_("\"Link %s %s\" is part of a link cycle"), target, linkname);
        break;
      }
      j = nalinks;
      pass_size = nalinks - i;
    }

    if (strcmp(target, linkname) == 0) {
      error(_("link %s targets itself"), target);
      continue;
    }

    {
      size_t nmemb1 = (j > i + 1) ? (size_t)(j - (i + 1)) : 0;
      if (nmemb1)
        l = bsearch(target, &links[i + 1], nmemb1, sizeof *links, bsearch_linkcmp);
      if (!l) {
        size_t nmemb2 = (nalinks > j) ? (size_t)(nalinks - j) : 0;
        if (nmemb2)
          l = bsearch(target, &links[j], nmemb2, sizeof *links, bsearch_linkcmp);
      }
    }

    if (!l) {
      dolink(target, linkname, false);
    } else {
      struct link *newlinks = growalloc(links, sizeof *links, nalinks, &nlinks_alloc);
      if (!newlinks) {
        error(_("memory exhausted"));
        return;
      }
      links = newlinks;
      links[nalinks++] = links[i];
    }

    if (noise && i < nlinks) {
      if (l) {
        warning(_("link %s targeting link %s mishandled by pre-2023 zic"),
                linkname, target);
      } else {
        size_t nmemb0 = (size_t)nlinks;
        if (nmemb0 && bsearch(target, links, nmemb0, sizeof *links, bsearch_linkcmp)) {
          warning(_("link %s targeting link %s"), linkname, target);
        }
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
    if (sig <= 0) {
        return;
    }
#if !defined(SA_SIGINFO)
    (void)signal(sig, signal_handler);
#endif
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
  int i;
  int const num_signals = (int)(sizeof signals / sizeof signals[0]);

  for (i = 0; i < num_signals; i++) {
#ifdef SA_SIGINFO
    struct sigaction act = {0};
    struct sigaction oldact = {0};
    act.sa_handler = signal_handler;
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;

    if (sigaction(signals[i], &act, &oldact) == 0) {
      if (!(oldact.sa_flags & SA_SIGINFO) && oldact.sa_handler == SIG_IGN) {
        (void)sigaction(signals[i], &oldact, NULL);
        got_signal = 0;
      }
    }
#else
    void (*prev)(int) = signal(signals[i], signal_handler);
    if (prev == SIG_IGN) {
      (void)signal(signals[i], SIG_IGN);
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
  if (sig <= 0) {
    return;
  }

  struct sigaction sa = {0};
  sa.sa_handler = SIG_DFL;
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = 0;
  (void)sigaction(sig, &sa, NULL);

  raise(sig);
  abort();
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
  if (timerange == NULL)
    return false;

  intmax_t lo = min_time;
  intmax_t hi = max_time;

  char *cursor = timerange;
  char *endptr = cursor;

  if (*cursor == '@') {
    errno = 0;
    lo = strtoimax(cursor + 1, &endptr, 10);
    if (endptr == cursor + 1 || (lo == INTMAX_MAX && errno == ERANGE))
      return false;
    cursor = endptr;
  }

  endptr = cursor;

  if (cursor[0] == '/' && cursor[1] == '@') {
    errno = 0;
    hi = strtoimax(cursor + 2, &endptr, 10);
    if (endptr == cursor + 2 || hi == INTMAX_MIN)
      return false;
    if (!(hi == INTMAX_MAX && errno == ERANGE))
      hi -= 1;
    cursor = endptr;
  }

  if (*cursor != '\0' || hi < lo || max_time < lo || hi < min_time)
    return false;

  lo_time = (lo > min_time) ? lo : min_time;
  hi_time = (hi < max_time) ? hi : max_time;

  return true;
}

/* Generate redundant time stamps up to OPT.  Return true if successful.  */
static bool
redundant_time_option(char *opt)
{
    if (!opt || opt[0] != '@') {
        return false;
    }

    char *end = NULL;
    intmax_t value = strtoimax(opt + 1, &end, 10);

    if (end == opt + 1 || *end != '\0') {
        return false;
    }

    if (redundant_time < value) {
        redundant_time = value;
    }

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

static bool want_bloat(void)
{
    return bloat >= 0;
}

#ifndef ZIC_BLOAT_DEFAULT
# define ZIC_BLOAT_DEFAULT "slim"
#endif

int
main(int argc, char **argv)
{
	int c, k;
	ptrdiff_t i, j;
	bool timerange_given = false;

#ifdef S_IWGRP
	umask(umask(S_IWGRP | S_IWOTH) | (S_IWGRP | S_IWOTH));
#endif
#if HAVE_GETTEXT
	setlocale(LC_ALL, "");
# ifdef TZ_DOMAINDIR
	bindtextdomain(TZ_DOMAIN, TZ_DOMAINDIR);
# endif
	textdomain(TZ_DOMAIN);
#endif
	main_argv = argv;
	progname = (argv && argv[0]) ? argv[0] : "zic";
	if (TYPE_BIT(zic_t) < 64) {
		fprintf(stderr, "%s: %s\n", progname,
			_("wild compilation-time specification of zic_t"));
		return EXIT_FAILURE;
	}
	for (k = 1; k < argc; k++) {
		if (strcmp(argv[k], "--version") == 0) {
			printf("zic %s%s\n", PKGVERSION, TZVERSION);
			close_file(stdout, NULL, NULL, NULL);
			return EXIT_SUCCESS;
		} else if (strcmp(argv[k], "--help") == 0) {
			usage(stdout, EXIT_SUCCESS);
		}
	}
	while ((c = getopt(argc, argv, "b:d:l:L:p:r:R:st:vy:")) != -1) {
		switch (c) {
			default:
				usage(stderr, EXIT_FAILURE);
				break;
			case 'b':
				if (strcmp(optarg, "slim") == 0) {
					if (0 < bloat)
						error(_("incompatible -b options"));
					bloat = -1;
				} else if (strcmp(optarg, "fat") == 0) {
					if (bloat < 0)
						error(_("incompatible -b options"));
					bloat = 1;
				} else {
					error(_("invalid option: -b '%s'"), optarg);
				}
				break;
			case 'd':
				if (directory == NULL) {
					directory = optarg;
				} else {
					fprintf(stderr,
						_("%s: More than one -d option specified\n"),
						progname);
					return EXIT_FAILURE;
				}
				break;
			case 'l':
				if (lcltime == NULL) {
					lcltime = optarg;
				} else {
					fprintf(stderr,
						_("%s: More than one -l option specified\n"),
						progname);
					return EXIT_FAILURE;
				}
				break;
			case 'p':
				if (psxrules == NULL) {
					psxrules = optarg;
				} else {
					fprintf(stderr,
						_("%s: More than one -p option specified\n"),
						progname);
					return EXIT_FAILURE;
				}
				break;
			case 't':
				if (tzdefault != NULL) {
					fprintf(stderr,
						_("%s: More than one -t option specified\n"),
						progname);
					return EXIT_FAILURE;
				}
				tzdefault = optarg;
				break;
			case 'y':
				warning(_("-y ignored"));
				break;
			case 'L':
				if (leapsec == NULL) {
					leapsec = optarg;
				} else {
					fprintf(stderr,
						_("%s: More than one -L option specified\n"),
						progname);
					return EXIT_FAILURE;
				}
				break;
			case 'v':
				noise = true;
				break;
			case 'r':
				if (timerange_given) {
					fprintf(stderr,
						_("%s: More than one -r option specified\n"),
						progname);
					return EXIT_FAILURE;
				}
				if (!timerange_option(optarg)) {
					fprintf(stderr,
						_("%s: invalid time range: %s\n"),
						progname, optarg);
					return EXIT_FAILURE;
				}
				timerange_given = true;
				break;
			case 'R':
				if (!redundant_time_option(optarg)) {
					fprintf(stderr, _("%s: invalid time: %s\n"),
						progname, optarg);
					return EXIT_FAILURE;
				}
				break;
			case 's':
				warning(_("-s ignored"));
				break;
		}
	}
	if (optind == argc - 1 && strcmp(argv[optind], "=") == 0)
		usage(stderr, EXIT_FAILURE);
	if (hi_time + (hi_time < ZIC_MAX) < redundant_time) {
		fprintf(stderr, _("%s: -R time exceeds -r cutoff\n"), progname);
		return EXIT_FAILURE;
	}
	if (redundant_time < lo_time)
		redundant_time = lo_time;
	if (bloat == 0) {
		static char const bloat_default[] = ZIC_BLOAT_DEFAULT;
		if (strcmp(bloat_default, "slim") == 0)
			bloat = -1;
		else if (strcmp(bloat_default, "fat") == 0)
			bloat = 1;
		else
			abort();
	}
	if (directory == NULL)
		directory = TZDIR;
	if (tzdefault == NULL)
		tzdefault = TZDEFAULT;

	if (optind < argc && leapsec != NULL) {
		infile(LEAPSEC_FILENUM, leapsec);
		adjleap();
	}

	for (k = optind; k < argc; k++)
		infile(k, argv[k]);
	if (errors)
		return EXIT_FAILURE;
	associate();
	change_directory(directory);
	catch_signals();
	for (i = 0; i < nzones; i = j) {
		for (j = i + 1; j < nzones && zones[j].z_name == NULL; ++j) {
		}
		outzone(&zones[i], j - i);
	}
	make_links();
	if (lcltime != NULL) {
		eat(COMMAND_LINE_FILENUM, 1);
		dolink(lcltime, tzdefault, true);
	}
	if (psxrules != NULL) {
		eat(COMMAND_LINE_FILENUM, 1);
		dolink(psxrules, TZDEFRULES, true);
	}
	if (warnings && (ferror(stderr) || fclose(stderr) != 0))
		return EXIT_FAILURE;
	return errors ? EXIT_FAILURE : EXIT_SUCCESS;
}

static bool
componentcheck(char const *name, char const *component, char const *component_end)
{
	enum { component_len_max = 14 };

	if (!name || !component || !component_end) {
		error(_("invalid file name component"));
		return false;
	}

	ptrdiff_t diff = component_end - component;
	if (diff < 0) {
		error(_("invalid file name component"));
		return false;
	}
	size_t component_len = (size_t)diff;

	if (component_len == 0) {
		if (!*name) {
			error(_("empty file name"));
		} else {
			char const *msg;
			if (component == name)
				msg = _("file name '%s' begins with '/'");
			else if (*component_end)
				msg = _("file name '%s' contains '//'");
			else
				msg = _("file name '%s' ends with '/'");
			error(msg, name);
		}
		return false;
	}

	if (component_len <= 2 && component[0] == '.' && component[component_len - 1] == '.') {
		int len = (int)component_len;
		error(_("file name '%s' contains '%.*s' component"), name, len, component);
		return false;
	}

	if (noise) {
		if (component[0] == '-')
			warning(_("file name '%s' component contains leading '-'"), name);
		if (component_len > component_len_max)
			warning(_("file name '%s' contains overlength component '%.*s...'"),
			        name, component_len_max, component);
	}

	return true;
}

static bool
namecheck(const char *name)
{
    static const char benign[] =
        "-/_"
        "abcdefghijklmnopqrstuvwxyz"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

    static const char printable_and_not_benign[] =
        " !\"#$%&'()*+,.0123456789:;<=>?@[\\]^`{|}~";

    if (name == NULL) {
        return false;
    }

    const char *component = name;
    const char *cp;

    for (cp = name; *cp != '\0'; cp++) {
        unsigned char c = (unsigned char)*cp;

        if (noise && strchr(benign, c) == NULL) {
            if (strchr(printable_and_not_benign, c) != NULL) {
                warning(_("file name '%s' contains byte '%c'"), name, c);
            } else {
                warning(_("file name '%s' contains byte '\\%o'"), name, (unsigned int)c);
            }
        }

        if (c == '/') {
            if (!componentcheck(name, component, cp)) {
                return false;
            }
            component = cp + 1;
        }
    }
    return componentcheck(name, component, cp);
}

/* Return a random uint_fast64_t.  */
static uint_fast64_t
get_rand_u64(void)
{
#if HAVE_GETRANDOM
  enum { BUF_WORDS = (256 / sizeof(uint_fast64_t)) ? (256 / sizeof(uint_fast64_t)) : 1 };
  static uint_fast64_t entropy_buffer[BUF_WORDS];
  static int nwords = 0;
  if (nwords <= 0) {
    unsigned char *p = (unsigned char *)entropy_buffer;
    size_t to_read = sizeof entropy_buffer;
    while (to_read > 0) {
      ssize_t s;
      do {
        s = getrandom(p, to_read, 0);
      } while (s < 0 && errno == EINTR);
      if (s <= 0)
        break;
      p += (size_t)s;
      to_read -= (size_t)s;
    }
    nwords = (int)((sizeof entropy_buffer - to_read) / sizeof(uint_fast64_t));
    if (nwords == 0)
      nwords = -1;
  }
  if (nwords > 0)
    return entropy_buffer[--nwords];
#endif

  {
    static int initialized = 0;
    if (!initialized) {
      srand((unsigned)time(NULL));
      initialized = 1;
    }
  }

  {
    unsigned int tmp = (unsigned int)RAND_MAX;
    unsigned int width = 0;
    while (tmp) {
      width++;
      tmp >>= 1;
    }
    if (width == 0)
      width = 1;

    uint_fast64_t r = 0;
    unsigned int needed = 64;

    while (needed > 0) {
      unsigned int v = (unsigned int)rand();
      unsigned int take = needed < width ? needed : width;
      uint_fast64_t mask = (take >= 64) ? UINT_FAST64_MAX : ((UINT64_C(1) << take) - UINT64_C(1));
      r = (r << take) | ((uint_fast64_t)v & mask);
      needed -= take;
    }

    return r;
  }
}

/* Generate a randomish name in the same directory as *NAME.  If
   *NAMEALLOC, put the name into *NAMEALLOC which is assumed to be
   that returned by a previous call and is thus already almost set up
   and equal to *NAME; otherwise, allocate a new name and put its
   address into both *NAMEALLOC and *NAME.  */
static void
random_dirent(char const **name, char **namealloc)
{
  char const *src = *name;
  char *dst = *namealloc;
  static char const prefix[] = ".zic";
  static char const alphabet[] =
    "abcdefghijklmnopqrstuvwxyz"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "0123456789";
  const size_t prefixlen = sizeof(prefix) - 1;
  const size_t alphabetlen = sizeof(alphabet) - 1;
  const size_t suffixlen = 6;

  char const *lastslash = strrchr(src, '/');
  size_t dirlen = lastslash ? (size_t)(lastslash - src) + 1 : 0;

  uint_fast64_t base = (uint_fast64_t)alphabetlen;
  uint_fast64_t base_pow = 1;
  for (size_t j = 0; j < suffixlen; j++) {
    base_pow *= base;
  }

  uint_fast64_t unfair_min =
    - (((uint_fast64_t)-1 % base_pow + 1) % base_pow);

  if (!dst) {
    dst = emalloc(size_sum(dirlen, prefixlen + suffixlen + 1));
    memcpy(dst, src, dirlen);
    memcpy(dst + dirlen, prefix, prefixlen);
    dst[dirlen + prefixlen + suffixlen] = '\0';
    *name = *namealloc = dst;
  }

  uint_fast64_t r;
  do {
    r = get_rand_u64();
  } while (r >= unfair_min);

  size_t base_index = dirlen + prefixlen;
  for (size_t i = 0; i < suffixlen; i++) {
    size_t idx = (size_t)(r % (uint_fast64_t)alphabetlen);
    dst[base_index + i] = alphabet[idx];
    r /= (uint_fast64_t)alphabetlen;
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

  if (outname == NULL || tempname == NULL) {
    fprintf(stderr, _("%s: internal error: null pointer passed to open_outfile\n"), progname);
    exit(EXIT_FAILURE);
  }

  FILE *fp = NULL;
  bool dirs_made = false;

  if (*tempname == NULL) {
    random_dirent(outname, tempname);
  }

  if (*outname == NULL) {
    fprintf(stderr, _("%s: internal error: null output filename\n"), progname);
    exit(EXIT_FAILURE);
  }

  for (;;) {
    errno = 0;
    fp = fopen(*outname, fopen_mode);
    if (fp != NULL) {
      break;
    }

    int saved_errno = errno;

    if (saved_errno == ENOENT && !dirs_made) {
      mkdirs(*outname, true);
      dirs_made = true;
      continue;
    }

    if (saved_errno == EEXIST) {
      random_dirent(outname, tempname);
      continue;
    }

    const char *errstr = strerror(saved_errno);
    if (!errstr) {
      errstr = _("Unknown error");
    }

    fprintf(stderr, _("%s: Can't create %s/%s: %s\n"),
            progname, directory, *outname, errstr);
    exit(EXIT_FAILURE);
  }

  return fp;
}

/* If TEMPNAME, the result is in the temporary file TEMPNAME even
   though the user wanted it in NAME, so rename TEMPNAME to NAME.
   Report an error and exit if there is trouble.  Also, free TEMPNAME.  */
static void
rename_dest(char *tempname, char const *name)
{
  if (tempname == NULL) {
    return;
  }

  if (name == NULL) {
    fprintf(stderr, _("%s: rename to %s/%s: %s\n"),
            progname ? progname : "",
            directory ? directory : "",
            "(null)",
            strerror(EINVAL));
    free(tempname);
    exit(EXIT_FAILURE);
  }

  if (rename(tempname, name) != 0) {
    int saved_errno = errno;
    remove(tempname);
    fprintf(stderr, _("%s: rename to %s/%s: %s\n"),
            progname ? progname : "",
            directory ? directory : "",
            name,
            strerror(saved_errno));
    free(tempname);
    exit(EXIT_FAILURE);
  }

  free(tempname);
}

/* Create symlink contents suitable for symlinking TARGET to LINKNAME, as a
   freshly allocated string.  TARGET should be a relative file name, and
   is relative to the global variable DIRECTORY.  LINKNAME can be either
   relative or absolute.  */
static char *
relname(char const *target, char const *linkname)
{
  size_t i = 0, taillen = 0, dir_len = 0, dotdots = 0;
  ptrdiff_t dotdotetcsize = 0, linksize = INDEX_MAX;
  char const *f = target;
  char *result = NULL;

  if (!target || !linkname)
    return NULL;

  if (linkname[0] == '/') {
    size_t len = strlen(directory);
    int need_slash = (len > 0 && directory[len - 1] != '/');
    size_t lenslash = len + (need_slash ? 1 : 0);
    size_t targetsize = strlen(target) + 1;
    linksize = size_sum(lenslash, targetsize);
    result = emalloc(linksize);
    memcpy(result, directory, len);
    if (need_slash)
      result[len] = '/';
    memcpy(result + lenslash, target, targetsize);
    f = result;
  }

  while (f[i] && f[i] == linkname[i]) {
    if (f[i] == '/')
      dir_len = i + 1;
    i++;
  }

  {
    size_t j = i;
    int prev_is_slash = (j == 0) ? 1 : (linkname[j - 1] == '/');
    for (; linkname[j]; j++) {
      if (linkname[j] == '/' && !prev_is_slash) {
        dotdots++;
        prev_is_slash = 1;
      } else {
        prev_is_slash = (linkname[j] == '/');
      }
    }
  }

  taillen = strlen(f + dir_len);
  dotdotetcsize = size_sum(size_product(dotdots, 3), taillen + 1);

  if (dotdotetcsize <= linksize) {
    if (!result)
      result = emalloc(dotdotetcsize);
    for (i = 0; i < dotdots; i++)
      memcpy(result + 3 * i, "../", 3);
    memmove(result + 3 * dotdots, f + dir_len, taillen + 1);
  }

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
dolink(char const *target, char const *linkname, bool staysymlink)
{
	bool linkdirs_made = false;
	int link_errno;
	char *tempname = NULL;
	char const *outname = linkname;
	int targetissym = -2, linknameissym = -2;

	check_for_signal();

	if (strcmp(target, "-") == 0) {
		if (remove(linkname) == 0 || errno == ENOENT || errno == ENOTDIR) {
			return;
		}
		fprintf(stderr, _("%s: Can't remove %s/%s: %s\n"),
		        progname, directory, linkname, strerror(errno));
		exit(EXIT_FAILURE);
	}

	while (true) {
		if (linkat(AT_FDCWD, target, AT_FDCWD, outname, AT_SYMLINK_FOLLOW) == 0) {
			link_errno = 0;
			break;
		}

		link_errno = errno;
		if (link_errno == EINVAL)
			link_errno = ENOTSUP;

#if HAVE_LINK
		if (link_errno == ENOTSUP
		    && (same_parent_dirs(target, outname)
		        || 0 <= itssymlink(target, &targetissym))) {
			if (link(target, outname) == 0) {
				link_errno = 0;
				break;
			}
			link_errno = errno;
		}
#endif

		if (link_errno == EXDEV || link_errno == ENOTSUP) {
			break;
		}

		if (link_errno == EEXIST) {
			staysymlink &= !tempname;
			random_dirent(&outname, &tempname);
			if (staysymlink && itssymlink(linkname, &linknameissym)) {
				break;
			}
			continue;
		}

		if (link_errno == ENOENT && !linkdirs_made) {
			mkdirs(linkname, true);
			linkdirs_made = true;
			continue;
		}

		fprintf(stderr, _("%s: Can't link %s/%s to %s/%s: %s\n"),
		        progname, directory, target, directory, outname, strerror(link_errno));
		exit(EXIT_FAILURE);
	}

	if (link_errno != 0) {
		bool absolute = *target == '/';
		char *linkalloc = absolute ? NULL : relname(target, linkname);
		char const *contents = absolute ? target : linkalloc;
		int symlink_errno;

		while (true) {
			if (symlink(contents, outname) == 0) {
				symlink_errno = 0;
				break;
			}
			symlink_errno = errno;

			if (symlink_errno == EEXIST) {
				random_dirent(&outname, &tempname);
				continue;
			}

			if (symlink_errno == ENOENT && !linkdirs_made) {
				mkdirs(linkname, true);
				linkdirs_made = true;
				continue;
			}

			break;
		}

		free(linkalloc);

		if (symlink_errno == 0) {
			if (link_errno != ENOTSUP && link_errno != EEXIST) {
				warning(_("symbolic link used because hard link failed: %s"),
				        strerror(link_errno));
			}
		} else {
			FILE *fp = fopen(target, "rb");
			FILE *tp;

			if (!fp) {
				fprintf(stderr, _("%s: Can't read %s/%s: %s\n"),
				        progname, directory, target, strerror(errno));
				exit(EXIT_FAILURE);
			}

			tp = open_outfile(&outname, &tempname);

			{
				unsigned char buf[BUFSIZ];
				size_t nread;
				while ((nread = fread(buf, 1, sizeof buf, fp)) > 0) {
					if (fwrite(buf, 1, nread, tp) != nread) {
						break;
					}
				}
			}

			close_file(tp, directory, linkname, tempname);
			close_file(fp, directory, target, NULL);

			if (link_errno != ENOTSUP) {
				warning(_("copy used because hard link failed: %s"),
				        strerror(link_errno));
			} else if (symlink_errno != ENOTSUP) {
				warning(_("copy used because symbolic link failed: %s"),
				        strerror(symlink_errno));
			}
		}
	}

	rename_dest(tempname, linkname);
}

/* Return 1 if NAME is an absolute symbolic link, -1 if it is relative,
   0 if it is not a symbolic link.  If *CACHE is not -2, it is the
   cached result of a previous call to this function with the same NAME.  */
static int
itssymlink(char const *name, int *cache)
{
  if (!cache || !name) {
    return 0;
  }

  if (*cache != -2) {
    return *cache;
  }

  char first = '\0';
  int n = (int)readlink(name, &first, 1);
  if (n < 0) {
    *cache = 0;
  } else {
    *cache = (first == '/') ? 1 : -1;
  }

  return *cache;
}

/*
** Associate sets of rules with zones.
*/

/*
** Sort by rule name.
*/

static int rcomp(const void *cp1, const void *cp2)
{
    const struct rule *r1 = (const struct rule *)cp1;
    const struct rule *r2 = (const struct rule *)cp2;

    if (r1 == r2) {
        return 0;
    }
    if (r1 == NULL) {
        return -1;
    }
    if (r2 == NULL) {
        return 1;
    }

    const char *n1 = r1->r_name;
    const char *n2 = r2->r_name;

    if (n1 == n2) {
        return 0;
    }
    if (n1 == NULL) {
        return -1;
    }
    if (n2 == NULL) {
        return 1;
    }

    return strcmp(n1, n2);
}

static void
associate(void)
{
	struct zone *zp;
	struct rule *rp;
	ptrdiff_t i, j, base, out;

	if (nrules > 1) {
		qsort(rules, nrules, sizeof *rules, rcomp);
		for (i = 0; i + 1 < nrules; ++i) {
			const char *name_i = rules[i].r_name;
			const char *name_ip1 = rules[i + 1].r_name;

			if (strcmp(name_i, name_ip1) != 0) {
				continue;
			}
			if (rules[i].r_filenum == rules[i + 1].r_filenum) {
				continue;
			}

			eat(rules[i].r_filenum, rules[i].r_linenum);
			warning(_("same rule name in multiple files"));
			eat(rules[i + 1].r_filenum, rules[i + 1].r_linenum);
			warning(_("same rule name in multiple files"));

			for (j = i + 2; j < nrules; ++j) {
				if (strcmp(name_i, rules[j].r_name) != 0) {
					break;
				}
				if (rules[j].r_filenum == rules[i].r_filenum) {
					continue;
				}
				if (rules[j].r_filenum == rules[i + 1].r_filenum) {
					continue;
				}
				break;
			}
			i = j - 1;
		}
	}

	for (i = 0; i < nzones; ++i) {
		zones[i].z_rules = NULL;
		zones[i].z_nrules = 0;
	}

	for (base = 0; base < nrules; base = out) {
		rp = &rules[base];
		out = base + 1;
		while (out < nrules && strcmp(rp->r_name, rules[out].r_name) == 0) {
			++out;
		}
		for (i = 0; i < nzones; ++i) {
			zp = &zones[i];
			if (strcmp(zp->z_rule, rp->r_name) == 0) {
				zp->z_rules = rp;
				zp->z_nrules = out - base;
			}
		}
	}

	for (i = 0; i < nzones; ++i) {
		zp = &zones[i];
		if (zp->z_nrules == 0) {
			eat(zp->z_filenum, zp->z_linenum);
			zp->z_save = getsave(zp->z_rule, &zp->z_isdst);
			if (zp->z_format_specifier == 's') {
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
  if (fp == NULL || buf == NULL || bufsize <= 0) {
    error(_("input error"));
    exit(EXIT_FAILURE);
  }

  size_t len = 0;
  size_t capacity = (size_t)bufsize;

  for (;;) {
    int ch = getc(fp);

    if (ch == '\n')
      break;

    if (ch == EOF) {
      if (ferror(fp)) {
        error(_("input error"));
        exit(EXIT_FAILURE);
      }
      if (len == 0)
        return false;
      error(_("unterminated line"));
      exit(EXIT_FAILURE);
    }

    if (ch == 0) {
      error(_("NUL input byte"));
      exit(EXIT_FAILURE);
    }

    if (len >= capacity - 1) {
      error(_("line too long"));
      exit(EXIT_FAILURE);
    }

    buf[len++] = (char)ch;
  }

  buf[len] = '\0';
  return true;
}

static void
infile(int fnum, char const *name)
{
	FILE *fp;
	bool wantcont = false;
	lineno num;

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

	enum { bufsize_bound = (min(INT_MAX, INDEX_MAX) / FORMAT_LEN_GROWTH_BOUND) };
	enum { buflen = min(_POSIX2_LINE_MAX, bufsize_bound) };

	for (num = 1;; ++num) {
		char buf[buflen];
		int nfields;
		char *fields[MAX_FIELDS];

		eat(fnum, num);
		if (!inputline(fp, buf, sizeof buf))
			break;

		nfields = getfields(buf, fields, (int)(sizeof fields / sizeof *fields));
		if (nfields == 0)
			continue;

		if (wantcont) {
			wantcont = inzcont(fields, nfields);
			continue;
		}

		{
			const struct lookup *line_codes = (fnum < 0) ? leap_line_codes : zi_line_codes;
			const struct lookup *lp = byword(fields[0], line_codes);

			if (lp == NULL) {
				error(_("input line of unknown type"));
			} else {
				wantcont = false;
				switch (lp->l_value) {
					case LC_RULE:
						inrule(fields, nfields);
						break;
					case LC_ZONE:
						wantcont = inzone(fields, nfields);
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
			}
		}
	}

	close_file(fp, NULL, filename(fnum), NULL);
	if (wantcont)
		error(_("expected continuation line not found"));
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
	zic_t hh;
	int sign, mm = 0, ss = 0;
	char hhx = '\0', mmx = '\0', ssx = '\0', xr = '0', xs = '\0';
	int tenths = 0;
	bool ok = false;

	if (string == NULL || *string == '\0')
		return 0;

	if (*string == '-') {
		sign = -1;
		++string;
	} else {
		sign = 1;
	}

	int nconv = sscanf(string,
	                   "%" SCNdZIC "%c%d%c%d%c%1d%*[0]%c%*[0123456789]%c",
	                   &hh, &hhx, &mm, &mmx, &ss, &ssx, &tenths, &xr, &xs);

	switch (nconv) {
		case 1:
			ok = true;
			break;
		case 3:
			ok = (hhx == ':');
			break;
		case 5:
			ok = (hhx == ':' && mmx == ':');
			break;
		case 7:
			ok = (hhx == ':' && mmx == ':' && ssx == '.');
			if (ok && noise)
				warning(_("fractional seconds rejected by pre-2018 versions of zic"));
			break;
		case 8:
			ok = (hhx == ':' && mmx == ':' && ssx == '.' && ('0' <= xr && xr <= '9'));
			if (ok && noise)
				warning(_("fractional seconds rejected by pre-2018 versions of zic"));
			break;
		default:
			ok = false;
			break;
	}

	if (!ok) {
		error("%s", errstring);
		return 0;
	}

	if (hh < 0 || mm < 0 || mm >= MINSPERHOUR || ss < 0 || ss > SECSPERMIN) {
		error("%s", errstring);
		return 0;
	}

	if (ZIC_MAX / SECSPERHOUR < hh) {
		error(_("time overflow"));
		return 0;
	}

	{
		int tie = (xr == '0');
		int threshold = 5 + (((ss & 1) == 0) && tie);
		if (tenths >= threshold)
			ss += 1;
	}

	if (noise && (hh > HOURSPERDAY || (hh == HOURSPERDAY && (mm != 0 || ss != 0))))
		warning(_("values over 24 hours not handled by pre-2007 versions of zic"));

	return oadd(sign * hh * SECSPERHOUR, sign * (mm * SECSPERMIN + ss));
}

static zic_t
getsave(char *field, bool *isdst)
{
  if (!field) {
    if (isdst) {
      *isdst = false;
    }
    return 0;
  }

  int dst = -1;
  size_t fieldlen = strlen(field);
  if (fieldlen != 0) {
    char *ep = field + fieldlen - 1;
    char last = *ep;
    if (last == 'd') {
      dst = 1;
      *ep = '\0';
    } else if (last == 's') {
      dst = 0;
      *ep = '\0';
    }
  }

  zic_t save = gethms(field, _("invalid saved time"));
  if (isdst) {
    *isdst = (dst < 0) ? (save != 0) : (dst != 0);
  }
  return save;
}

static void
inrule(char **fields, int nfields)
{
	struct rule r = {0};
	const char *name, *save, *lo, *hi, *cmd, *mon, *day, *tod, *abbrv;
	unsigned char c;
	size_t ablen;

	if (nfields != RULE_FIELDS) {
		error(_("wrong number of fields on Rule line"));
		return;
	}

	name = fields[RF_NAME] ? fields[RF_NAME] : "";
	c = (unsigned char)name[0];
	if (c == '\0' || c == ' ' || c == '\f' || c == '\n' || c == '\r' ||
	    c == '\t' || c == '\v' || c == '+' || c == '-' || (c >= '0' && c <= '9')) {
		error(_("Invalid rule name \"%s\""), name);
		return;
	}

	r.r_filenum = filenum;
	r.r_linenum = linenum;

	save = fields[RF_SAVE] ? fields[RF_SAVE] : "";
	r.r_save = getsave(save, &r.r_isdst);

	lo = fields[RF_LOYEAR] ? fields[RF_LOYEAR] : "";
	hi = fields[RF_HIYEAR] ? fields[RF_HIYEAR] : "";
	cmd = fields[RF_COMMAND] ? fields[RF_COMMAND] : "";
	mon = fields[RF_MONTH] ? fields[RF_MONTH] : "";
	day = fields[RF_DAY] ? fields[RF_DAY] : "";
	tod = fields[RF_TOD] ? fields[RF_TOD] : "";

	if (!rulesub(&r, lo, hi, cmd, mon, day, tod))
		return;

	r.r_name = estrdup(name);

	abbrv = fields[RF_ABBRVAR] ? fields[RF_ABBRVAR] : "";
	r.r_abbrvar = estrdup(abbrv);
	ablen = strlen(r.r_abbrvar);
	if (max_abbrvar_len < ablen)
		max_abbrvar_len = ablen;

	rules = growalloc(rules, sizeof *rules, nrules, &nrules_alloc);
	rules[nrules++] = r;
}

static bool
inzone(char **fields, int nfields)
{
	ptrdiff_t i;

	if (nfields < ZONE_MINFIELDS || nfields > ZONE_MAXFIELDS) {
		error(_("wrong number of fields on Zone line"));
		return false;
	}

	const char *name = fields[ZF_NAME];

	if (lcltime != NULL && strcmp(name, tzdefault) == 0) {
		error(_("\"Zone %s\" line and -l option are mutually exclusive"), tzdefault);
		return false;
	}

	if (strcmp(name, TZDEFRULES) == 0 && psxrules != NULL) {
		error(_("\"Zone %s\" line and -p option are mutually exclusive"), TZDEFRULES);
		return false;
	}

	for (i = 0; i < nzones; ++i) {
		const char *zname = zones[i].z_name;
		if (zname != NULL && strcmp(zname, name) == 0) {
			error(_("duplicate zone name %s (file \"%s\", line %"PRIdMAX")"),
			      name, filename(zones[i].z_filenum), zones[i].z_linenum);
			return false;
		}
	}

	return inzsub(fields, nfields, false);
}

static bool
inzcont(char **fields, int nfields)
{
	if (fields == NULL) {
		error(_("invalid fields pointer on Zone continuation line"));
		return false;
	}
	if (nfields < ZONEC_MINFIELDS || nfields > ZONEC_MAXFIELDS) {
		error(_("wrong number of fields on Zone continuation line"));
		return false;
	}
	return inzsub(fields, nfields, true);
}

static bool
inzsub(char **fields, int nfields, bool iscont)
{
	char *format_copy;
	const char *format_str;
	const char *spec_ptr = NULL;
	struct zone z;
	int format_len;
	int idx_stdoff, idx_rule, idx_format;
	int idx_untilyear, idx_untilmonth, idx_untilday, idx_untiltime;
	bool hasuntil;

	if (iscont) {
		idx_stdoff = ZFC_STDOFF;
		idx_rule = ZFC_RULE;
		idx_format = ZFC_FORMAT;
		idx_untilyear = ZFC_TILYEAR;
		idx_untilmonth = ZFC_TILMONTH;
		idx_untilday = ZFC_TILDAY;
		idx_untiltime = ZFC_TILTIME;
	} else {
		if (!namecheck(fields[ZF_NAME]))
			return false;
		idx_stdoff = ZF_STDOFF;
		idx_rule = ZF_RULE;
		idx_format = ZF_FORMAT;
		idx_untilyear = ZF_TILYEAR;
		idx_untilmonth = ZF_TILMONTH;
		idx_untilday = ZF_TILDAY;
		idx_untiltime = ZF_TILTIME;
	}

	{
		int max_required = idx_stdoff;
		if (idx_rule > max_required) max_required = idx_rule;
		if (idx_format > max_required) max_required = idx_format;
		if (nfields <= max_required) {
			error(_("too few fields"));
			return false;
		}
		if (!fields[idx_stdoff] || !fields[idx_rule] || !fields[idx_format]) {
			error(_("missing required field"));
			return false;
		}
	}

	z.z_filenum = filenum;
	z.z_linenum = linenum;
	z.z_stdoff = gethms(fields[idx_stdoff], _("invalid UT offset"));

	format_str = fields[idx_format];
	{
		const char *percent = strchr(format_str, '%');
		if (percent) {
			char next = percent[1];
			if ((next != 's' && next != 'z') ||
			    strchr(percent + 1, '%') ||
			    strchr(format_str, '/')) {
				error(_("invalid abbreviation format"));
				return false;
			}
			spec_ptr = percent + 1;
		}
	}
	z.z_format_specifier = spec_ptr ? *spec_ptr : '\0';

	format_len = (int) strlen(format_str);
	if (max_format_len < format_len)
		max_format_len = format_len;

	hasuntil = nfields > idx_untilyear;
	if (hasuntil) {
		const char *umonth = (nfields > idx_untilmonth) ? fields[idx_untilmonth] : "Jan";
		const char *uday = (nfields > idx_untilday) ? fields[idx_untilday] : "1";
		const char *utime = (nfields > idx_untiltime) ? fields[idx_untiltime] : "0";

		z.z_untilrule.r_filenum = filenum;
		z.z_untilrule.r_linenum = linenum;

		if (!rulesub(&z.z_untilrule, fields[idx_untilyear], "only", "",
			     umonth, uday, utime))
			return false;

		z.z_untiltime = rpytime(&z.z_untilrule, z.z_untilrule.r_loyear);

		if (iscont && nzones > 0 &&
		    z.z_untiltime > min_time &&
		    z.z_untiltime < max_time &&
		    zones[nzones - 1].z_untiltime > min_time &&
		    zones[nzones - 1].z_untiltime < max_time &&
		    zones[nzones - 1].z_untiltime >= z.z_untiltime) {
			error(_("Zone continuation line end time is not after end time of previous line"));
			return false;
		}
	}

	z.z_name = iscont ? NULL : estrdup(fields[ZF_NAME]);
	z.z_rule = estrdup(fields[idx_rule]);
	z.z_format = format_copy = estrdup(format_str);

	if (z.z_format_specifier == 'z') {
		size_t spec_index = (size_t)(spec_ptr - format_str);
		format_copy[spec_index] = 's';
		if (noise)
			warning(_("format '%s' not handled by pre-2015 versions of zic"), format_str);
	}

	zones = growalloc(zones, sizeof *zones, nzones, &nzones_alloc);
	zones[nzones++] = z;

	return hasuntil;
}

static zic_t
getleapdatetime(char **fields, bool expire_line)
{
	const char *year_str;
	const char *month_str;
	const char *day_str;
	const char *time_str;
	const struct lookup *lp;
	zic_t year;
	int month, day;
	zic_t dayoff = 0;
	zic_t tod;
	zic_t t;
	char xs;
	zic_t y;

	year_str = fields[LP_YEAR];
	if (!year_str || sscanf(year_str, "%" SCNdZIC "%c", &year, &xs) != 1) {
		error(_("invalid leaping year"));
		return -1;
	}

	if (!expire_line) {
		if (!leapseen || leapmaxyear < year)
			leapmaxyear = year;
		if (!leapseen || leapminyear > year)
			leapminyear = year;
		leapseen = true;
	}

	y = EPOCH_YEAR;
	if (year > y) {
		for (; y < year; y++)
			dayoff = oadd(dayoff, len_years[isleap(y)]);
	} else if (year < y) {
		for (; y > year; ) {
			y--;
			dayoff = oadd(dayoff, -len_years[isleap(y)]);
		}
	}

	month_str = fields[LP_MONTH];
	if (!month_str || (lp = byword(month_str, mon_names)) == NULL) {
		error(_("invalid month name"));
		return -1;
	}
	month = lp->l_value;

	for (int m = TM_JANUARY; m < month; m++)
		dayoff = oadd(dayoff, len_months[isleap(year)][m]);

	day_str = fields[LP_DAY];
	if (!day_str || sscanf(day_str, "%d%c", &day, &xs) != 1 ||
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

	t = dayoff * SECSPERDAY;

	time_str = fields[LP_TIME];
	if (!time_str) {
		error(_("invalid time of day"));
		return -1;
	}
	tod = gethms(time_str, _("invalid time of day"));
	t = tadd(t, tod);

	if (t < 0)
		error(_("leap second precedes Epoch"));

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
  if (t < 0)
    return;

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

static void inexpires(char **fields, int nfields)
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
	struct link l;
	char *target;
	char *linkname;

	if (fields == NULL) {
		error(_("invalid fields pointer on Link line"));
		return;
	}
	if (nfields != LINK_FIELDS) {
		error(_("wrong number of fields on Link line"));
		return;
	}
	if (LF_TARGET >= nfields || LF_LINKNAME >= nfields) {
		error(_("field index out of range on Link line"));
		return;
	}

	target = fields[LF_TARGET];
	linkname = fields[LF_LINKNAME];

	if (target == NULL) {
		error(_("missing TARGET field on Link line"));
		return;
	}
	if (*target == '\0') {
		error(_("blank TARGET field on Link line"));
		return;
	}
	if (linkname == NULL) {
		error(_("missing LINKNAME field on Link line"));
		return;
	}
	if (!namecheck(linkname))
		return;

	l.l_filenum = filenum;
	l.l_linenum = linenum;
	l.l_target = estrdup(target);
	if (!l.l_target) {
		error(_("out of memory duplicating TARGET field on Link line"));
		return;
	}
	l.l_linkname = estrdup(linkname);
	if (!l.l_linkname) {
		free(l.l_target);
		error(_("out of memory duplicating LINKNAME field on Link line"));
		return;
	}

	links = growalloc(links, sizeof *links, nlinks, &nlinks_alloc);
	if (!links) {
		free(l.l_target);
		free(l.l_linkname);
		error(_("out of memory growing link array"));
		return;
	}

	links[nlinks++] = l;
}

static void
set_time_flags_from_suffix(char *timebuf, struct rule *rp)
{
	size_t len = strlen(timebuf);
	if (len == 0)
		return;

	char *ep = timebuf + len - 1;
	switch (lowerit(*ep)) {
		case 's':
			rp->r_todisstd = true;
			rp->r_todisut = false;
			*ep = '\0';
			break;
		case 'w':
			rp->r_todisstd = false;
			rp->r_todisut = false;
			*ep = '\0';
			break;
		case 'g':
		case 'u':
		case 'z':
			rp->r_todisstd = true;
			rp->r_todisut = true;
			*ep = '\0';
			break;
		default:
			break;
	}
}

static bool
parse_loyear(struct rule *rp, const char *cp)
{
	const struct lookup *lp = byword(cp, begin_years);
	if (lp) {
		switch (lp->l_value) {
			case YR_MINIMUM:
				warning(_("FROM year \"%s\" is obsolete; treated as %d"),
					cp, YEAR_32BIT_MIN - 1);
				rp->r_loyear = YEAR_32BIT_MIN - 1;
				break;
			default:
				unreachable();
		}
		return true;
	}

	char xs;
	if (sscanf(cp, "%"SCNdZIC"%c", &rp->r_loyear, &xs) != 1) {
		error(_("invalid starting year"));
		return false;
	}
	return true;
}

static bool
parse_hiyear(struct rule *rp, const char *cp)
{
	const struct lookup *lp = byword(cp, end_years);
	rp->r_hiwasnum = lp == NULL;
	if (!rp->r_hiwasnum) {
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
		return true;
	}

	char xs;
	if (sscanf(cp, "%"SCNdZIC"%c", &rp->r_hiyear, &xs) != 1) {
		error(_("invalid ending year"));
		return false;
	}
	return true;
}

static bool
parse_day(struct rule *rp, const char *dayp)
{
	char *dp = estrdup(dayp);
	const struct lookup *lp;
	char *ep;

	if ((lp = byword(dp, lasts)) != NULL) {
		rp->r_dycode = DC_DOWLEQ;
		rp->r_wday = lp->l_value;
		rp->r_dayofmonth = len_months[1][rp->r_month];
		free(dp);
		return true;
	}

	if ((ep = strchr(dp, '<')) != NULL)
		rp->r_dycode = DC_DOWLEQ;
	else if ((ep = strchr(dp, '>')) != NULL)
		rp->r_dycode = DC_DOWGEQ;
	else {
		ep = dp;
		rp->r_dycode = DC_DOM;
	}

	if (rp->r_dycode != DC_DOM) {
		*ep++ = '\0';
		if (*ep++ != '=') {
			error(_("invalid day of month"));
			free(dp);
			return false;
		}
		if ((lp = byword(dp, wday_names)) == NULL) {
			error(_("invalid weekday name"));
			free(dp);
			return false;
		}
		rp->r_wday = lp->l_value;
	}

	char xs;
	if (sscanf(ep, "%d%c", &rp->r_dayofmonth, &xs) != 1 ||
	    rp->r_dayofmonth <= 0 ||
	    rp->r_dayofmonth > len_months[1][rp->r_month]) {
		error(_("invalid day of month"));
		free(dp);
		return false;
	}

	free(dp);
	return true;
}

static bool
rulesub(struct rule *rp, const char *loyearp, const char *hiyearp,
	const char *typep, const char *monthp, const char *dayp,
	const char *timep)
{
	const struct lookup *lp = byword(monthp, mon_names);
	if (lp == NULL) {
		error(_("invalid month name"));
		return false;
	}
	rp->r_month = lp->l_value;

	rp->r_todisstd = false;
	rp->r_todisut = false;

	char *tbuf = estrdup(timep);
	if (*tbuf != '\0')
		set_time_flags_from_suffix(tbuf, rp);
	rp->r_tod = gethms(tbuf, _("invalid time of day"));
	free(tbuf);

	if (!parse_loyear(rp, loyearp))
		return false;

	if (!parse_hiyear(rp, hiyearp))
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

	if (!parse_day(rp, dayp))
		return false;

	return true;
}

static void
convert(uint_fast32_t val, char *buf)
{
    if (buf == NULL) {
        return;
    }

    unsigned char *b = (unsigned char *)buf;
    uint32_t tmp = (uint32_t)val;

    b[0] = (unsigned char)((tmp >> 24) & 0xFFu);
    b[1] = (unsigned char)((tmp >> 16) & 0xFFu);
    b[2] = (unsigned char)((tmp >> 8) & 0xFFu);
    b[3] = (unsigned char)(tmp & 0xFFu);
}

static void
convert64(uint_fast64_t val, char *buf)
{
    if (buf == NULL) {
        return;
    }

    unsigned char *b = (unsigned char *)buf;

    for (unsigned int i = 0U; i < 8U; ++i) {
        unsigned int shift = 56U - (i * 8U);
        b[i] = (unsigned char)((val >> shift) & 0xFFu);
    }
}

static void
puttzcode(zic_t value, FILE *stream)
{
    unsigned char buf[4];
    size_t total = 0;

    if (stream == NULL)
        return;

    convert(value, (char *)buf);

    while (total < sizeof buf) {
        size_t written = fwrite(buf + total, 1u, sizeof buf - total, stream);
        if (written == 0) {
            break;
        }
        total += written;
    }
}

static void puttzcodepass(zic_t val, FILE *fp, int pass)
{
    if (fp == NULL) {
        return;
    }

    if (pass == 1) {
        puttzcode(val, fp);
        return;
    }

    char buf[8];
    convert64(val, buf);
    if (fwrite(buf, sizeof buf, 1, fp) != 1) {
        return;
    }
}

static int atcomp(const void *avp, const void *bvp)
{
    if (avp == bvp) {
        return 0;
    }
    if (avp == NULL) {
        return -1;
    }
    if (bvp == NULL) {
        return 1;
    }

    const struct attype *ap = (const struct attype *)avp;
    const struct attype *bp = (const struct attype *)bvp;

    zic_t a = ap->at;
    zic_t b = bp->at;

    if (a < b) {
        return -1;
    }
    if (a > b) {
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
           zic_t const *ats, unsigned char const *types)
{
  if (!ats || !types)
    return r;

  while (r.count > 0 && ats[r.base] < lo) {
    r.defaulttype = types[r.base];
    r.base++;
    r.count--;
  }

  while (r.leapcount > 1 && trans[r.leapbase + 1] <= lo) {
    r.leapbase++;
    r.leapcount--;
  }

  while (r.leapbase > 0
         && ((corr[r.leapbase - 1] < corr[r.leapbase])
             != (corr[r.leapbase] > 0))) {
    r.leapbase--;
    r.leapcount++;
  }

  if (hi < max_time) {
    zic_t const cutoff = hi + 1;
    while (r.count > 0 && cutoff < ats[r.base + r.count - 1])
      r.count--;
    while (r.leapcount > 0 && cutoff < trans[r.leapbase + r.leapcount - 1])
      r.leapcount--;
  }

  r.leapexpiry = (leapexpires >= 0 && leapexpires - 1 <= hi);

  return r;
}

static void
writezone(const char *const name, const char *const string, char version,
          int defaulttype)
{
    FILE *fp;
    ptrdiff_t i, j;
    int pass;
    char *tempname = NULL;
    char const *outname = name;

    zic_t *ats = emalloc(align_to(size_product(timecnt + !timecnt,
                                               sizeof *ats + 1),
                                  alignof(zic_t)));
    void *typesptr = ats + timecnt;
    unsigned char *types = typesptr;
    struct timerange rangeall = (struct timerange){0}, range32, range64;

    if (timecnt > 1)
        qsort(attypes, timecnt, sizeof *attypes, atcomp);

    {
        ptrdiff_t fromi, toi;
        toi = 0;
        fromi = 0;
        for (; fromi < timecnt; ++fromi) {
            if (toi != 0
                && ((attypes[fromi].at
                     + utoffs[attypes[toi - 1].type])
                    <= (attypes[toi - 1].at
                        + utoffs[toi == 1 ? 0
                                 : attypes[toi - 2].type]))) {
                attypes[toi - 1].type =
                    attypes[fromi].type;
                continue;
            }
            if (toi == 0
                || attypes[fromi].dontmerge
                || (utoffs[attypes[toi - 1].type]
                    != utoffs[attypes[fromi].type])
                || (isdsts[attypes[toi - 1].type]
                    != isdsts[attypes[fromi].type])
                || (desigidx[attypes[toi - 1].type]
                    != desigidx[attypes[fromi].type]))
                attypes[toi++] = attypes[fromi];
        }
        timecnt = toi;
    }

    if (noise && timecnt > 1200) {
      if (timecnt > TZ_MAX_TIMES)
        warning(_("reference clients mishandle"
                  " more than %d transition times"),
                TZ_MAX_TIMES);
      else
        warning(_("pre-2014 clients may mishandle"
                  " more than 1200 transition times"));
    }

    for (i = 0; i < timecnt; ++i) {
        ats[i] = attypes[i].at;
        types[i] = attypes[i].type;
    }

    for (i = 0; i < timecnt; ++i) {
        j = leapcnt;
        while (--j >= 0)
            if (ats[i] > trans[j] - corr[j]) {
                ats[i] = tadd(ats[i], corr[j]);
                break;
            }
    }

    rangeall.defaulttype = defaulttype;
    rangeall.count = timecnt;
    rangeall.leapcount = leapcnt;
    range64 = limitrange(rangeall, lo_time,
                         max(hi_time,
                             redundant_time - (ZIC_MIN < redundant_time)),
                         ats, types);
    range32 = limitrange(range64, ZIC32_MIN, ZIC32_MAX, ats, types);

    for (pass = 1; pass <= 2; pass++) {
      struct timerange const *r = pass == 1 ? &range32 : &range64;
      if (pass == 1 && !want_bloat())
        continue;
      if (r->leapexpiry) {
        if (noise)
          warning(_("%s: pre-2021b clients may mishandle"
                    " leap second expiry"),
                  name);
        version = '4';
      }
      if (0 < r->leapcount
          && corr[r->leapbase] != 1 && corr[r->leapbase] != -1) {
        if (noise)
          warning(_("%s: pre-2021b clients may mishandle"
                    " leap second table truncation"),
                  name);
        version = '4';
      }
      if (version == '4')
        break;
    }

    fp = open_outfile(&outname, &tempname);
    if (!fp) {
        error(_("failed to open output file"));
        free(ats);
        return;
    }

    for (pass = 1; pass <= 2; ++pass) {
        ptrdiff_t thistimei, thistimecnt, thistimelim;
        int thisleapi, thisleapcnt, thisleaplim;
        struct tzhead tzh;
        int pretranstype = -1, thisdefaulttype;
        bool locut, hicut, thisleapexpiry;
        zic_t lo, thismin, thismax;
        int old0;
        char omittype[TZ_MAX_TYPES];
        int typemap[TZ_MAX_TYPES];
        int thistypecnt, stdcnt, utcnt;
        char thischars[TZ_MAX_CHARS];
        int thischarcnt;
        bool toomanytimes;
        int indmap[TZ_MAX_CHARS];

        if (pass == 1) {
            thisdefaulttype = range32.defaulttype;
            thistimei = range32.base;
            thistimecnt = range32.count;
            toomanytimes = thistimecnt >> 31 >> 1 != 0;
            thisleapi = range32.leapbase;
            thisleapcnt = range32.leapcount;
            thisleapexpiry = range32.leapexpiry;
            thismin = ZIC32_MIN;
            thismax = ZIC32_MAX;
        } else {
            thisdefaulttype = range64.defaulttype;
            thistimei = range64.base;
            thistimecnt = range64.count;
            toomanytimes = thistimecnt >> 31 >> 31 >> 2 != 0;
            thisleapi = range64.leapbase;
            thisleapcnt = range64.leapcount;
            thisleapexpiry = range64.leapexpiry;
            thismin = min_time;
            thismax = max_time;
        }
        if (toomanytimes) {
          error(_("too many transition times"));
          goto cleanup;
        }

        locut = thismin < lo_time && lo_time <= thismax;
        hicut = thismin <= hi_time && hi_time < thismax;
        thistimelim = thistimei + thistimecnt;

        for (i = 0; i < (ptrdiff_t)TZ_MAX_TYPES; ++i)
            typemap[i] = -1;

        memset(omittype, 1, sizeof omittype);

        if ((locut || (pass == 1 && thistimei))
            && ! (thistimecnt && ats[thistimei] == lo_time)) {
          pretranstype = thisdefaulttype;
          omittype[pretranstype] = false;
        }

        if (pass == 1 && lo_time <= thismin)
          thisdefaulttype = range64.defaulttype;

        if (locut)
          thisdefaulttype = unspecifiedtype;
        omittype[thisdefaulttype] = false;
        for (i = thistimei; i < thistimelim; i++)
          omittype[types[i]] = false;
        if (hicut)
          omittype[unspecifiedtype] = false;

        old0 = 0;

        thistypecnt = 0;
        for (i = old0; i < typecnt; i++)
          if (!omittype[i])
            typemap[i == old0 ? thisdefaulttype
                    : i == thisdefaulttype ? old0 : i]
              = thistypecnt++;

        for (i = 0; i < (ptrdiff_t)(sizeof indmap / sizeof indmap[0]); ++i)
            indmap[i] = -1;
        thischarcnt = stdcnt = utcnt = 0;
        for (i = old0; i < typecnt; i++) {
            char *thisabbr;

            if (omittype[i])
                continue;
            if (ttisstds[i])
              stdcnt = thistypecnt;
            if (ttisuts[i])
              utcnt = thistypecnt;
            if (indmap[desigidx[i]] >= 0)
                continue;
            thisabbr = &chars[desigidx[i]];
            for (j = 0; j < thischarcnt; ++j)
                if (strcmp(&thischars[j], thisabbr) == 0)
                    break;
            if (j == thischarcnt) {
                size_t ablen = strlen(thisabbr) + 1;
                if ((size_t)thischarcnt + ablen > sizeof thischars) {
                    error(_("too many or too-long abbreviations"));
                    goto cleanup;
                }
                memcpy(&thischars[thischarcnt], thisabbr, ablen);
                thischarcnt += (int)ablen;
            }
            indmap[desigidx[i]] = (int)j;
        }
        if (pass == 1 && !want_bloat()) {
          hicut = thisleapexpiry = false;
          pretranstype = -1;
          thistimecnt = thisleapcnt = 0;
          thistypecnt = thischarcnt = 1;
        }
#define DO_WRITE(field) do { if (fwrite(tzh.field, sizeof tzh.field, 1, fp) != 1) goto write_error; } while (0)
        memset(&tzh, 0, sizeof tzh);
        memcpy(tzh.tzh_magic, TZ_MAGIC, sizeof tzh.tzh_magic);
        tzh.tzh_version[0] = version;
        convert(utcnt, tzh.tzh_ttisutcnt);
        convert(stdcnt, tzh.tzh_ttisstdcnt);
        convert(thisleapcnt + thisleapexpiry, tzh.tzh_leapcnt);
        convert((0 <= pretranstype) + thistimecnt + hicut,
                tzh.tzh_timecnt);
        convert(thistypecnt, tzh.tzh_typecnt);
        convert(thischarcnt, tzh.tzh_charcnt);
        DO_WRITE(tzh_magic);
        DO_WRITE(tzh_version);
        DO_WRITE(tzh_reserved);
        DO_WRITE(tzh_ttisutcnt);
        DO_WRITE(tzh_ttisstdcnt);
        DO_WRITE(tzh_leapcnt);
        DO_WRITE(tzh_timecnt);
        DO_WRITE(tzh_typecnt);
        DO_WRITE(tzh_charcnt);
#undef DO_WRITE
        if (pass == 1 && !want_bloat()) {
          if (puttzcode(0, fp) < 0) goto write_error;
          if (putc(0, fp) == EOF) goto write_error;
          if (putc(0, fp) == EOF) goto write_error;
          if (putc(0, fp) == EOF) goto write_error;
          continue;
        }

        lo = pass == 1 && lo_time < ZIC32_MIN ? ZIC32_MIN : lo_time;

        if (0 <= pretranstype)
          puttzcodepass(lo, fp, pass);
        for (i = thistimei; i < thistimelim; ++i) {
          puttzcodepass(ats[i], fp, pass);
        }
        if (hicut)
          puttzcodepass(hi_time + 1, fp, pass);
        if (0 <= pretranstype)
          if (putc(typemap[pretranstype], fp) == EOF) goto write_error;
        for (i = thistimei; i < thistimelim; i++)
          if (putc(typemap[types[i]], fp) == EOF) goto write_error;
        if (hicut)
          if (putc(typemap[unspecifiedtype], fp) == EOF) goto write_error;

        for (i = old0; i < typecnt; i++) {
          int h = (i == old0 ? thisdefaulttype
                   : i == thisdefaulttype ? old0 : i);
          if (!omittype[h]) {
            if (puttzcode(utoffs[h], fp) < 0) goto write_error;
            if (putc(isdsts[h], fp) == EOF) goto write_error;
            if (putc(indmap[desigidx[h]], fp) == EOF) goto write_error;
          }
        }
        if (thischarcnt != 0) {
            if (fwrite(thischars, sizeof thischars[0],
                       (size_t)thischarcnt, fp) != (size_t)thischarcnt)
                goto write_error;
        }
        thisleaplim = thisleapi + thisleapcnt;
        for (i = thisleapi; i < thisleaplim; ++i) {
            zic_t todo;

            if (roll[i]) {
                if (timecnt == 0 || trans[i] < ats[0]) {
                    j = 0;
                    while (isdsts[j])
                        if (++j >= typecnt) {
                            j = 0;
                            break;
                        }
                } else {
                    j = 1;
                    while (j < timecnt &&
                        trans[i] >= ats[j])
                            ++j;
                    j = types[j - 1];
                }
                todo = tadd(trans[i], -utoffs[j]);
            } else  todo = trans[i];
            puttzcodepass(todo, fp, pass);
            if (puttzcode(corr[i], fp) < 0) goto write_error;
        }
        if (thisleapexpiry) {
          puttzcodepass(leapexpires, fp, pass);
          if (puttzcode(thisleaplim ? corr[thisleaplim - 1] : 0, fp) < 0)
              goto write_error;
        }
        if (stdcnt != 0)
          for (i = old0; i < typecnt; i++)
            if (!omittype[i])
                if (putc(ttisstds[i], fp) == EOF) goto write_error;
        if (utcnt != 0)
          for (i = old0; i < typecnt; i++)
            if (!omittype[i])
                if (putc(ttisuts[i], fp) == EOF) goto write_error;

        if (ferror(fp)) goto write_error;
    }
    if (fprintf(fp, "\n%s\n", string) < 0) goto write_error;

    close_file(fp, directory, name, tempname);
    rename_dest(tempname, name);
    free(ats);
    return;

write_error:
    error(_("write error"));
cleanup:
    if (fp)
        close_file(fp, directory, name, tempname);
    free(ats);
}

static char const *
abbroffset(char *buf, zic_t offset)
{
  char sign = '+';
  int seconds;
  int minutes;
  zic_t tmp;

  if (!buf) {
    error(_("abbreviation buffer is null"));
    return "%z";
  }

  if (offset < 0) {
    offset = -offset;
    sign = '-';
  }

  tmp = offset;
  seconds = (int)(tmp % SECSPERMIN);
  tmp /= SECSPERMIN;
  minutes = (int)(tmp % MINSPERHOUR);
  tmp /= MINSPERHOUR;

  if (tmp >= 100) {
    error(_("%%z UT offset magnitude exceeds 99:59:59"));
    return "%z";
  }

  {
    char *p = buf;
    int hours = (int)tmp;

    *p++ = sign;
    *p++ = (char)('0' + hours / 10);
    *p++ = (char)('0' + hours % 10);

    if (minutes != 0 || seconds != 0) {
      *p++ = (char)('0' + minutes / 10);
      *p++ = (char)('0' + minutes % 10);
      if (seconds != 0) {
        *p++ = (char)('0' + seconds / 10);
        *p++ = (char)('0' + seconds % 10);
      }
    }

    *p = '\0';
    return buf;
  }
}

static char const disable_percent_s[] = "";

static ptrdiff_t
doabbr(char *abbr, struct zone const *zp, char const *letters,
       bool isdst, zic_t save, bool doquotes)
{
	if (!abbr || !zp || !zp->z_format)
		return 0;

	const char *format = zp->z_format;
	const char *slashp = strchr(format, '/');

	if (!slashp) {
		char letterbuf[PERCENT_Z_LEN_BOUND + 1];
		const char *letters_to_use = letters;

		if (zp->z_format_specifier == 'z') {
			letters_to_use = abbroffset(letterbuf, zp->z_stdoff + save);
		} else if (!letters_to_use) {
			letters_to_use = "%s";
		} else if (letters_to_use == disable_percent_s) {
			return 0;
		}
		sprintf(abbr, format, letters_to_use);
	} else if (isdst) {
		strcpy(abbr, slashp + 1);
	} else {
		size_t prelen = (size_t)(slashp - format);
		memcpy(abbr, format, prelen);
		abbr[prelen] = '\0';
	}

	size_t len = strlen(abbr);
	if (!doquotes)
		return (ptrdiff_t)len;

	char *cp = abbr;
	while (is_alpha((unsigned char)*cp))
		cp++;

	if (len > 0 && *cp == '\0')
		return (ptrdiff_t)len;

	memmove(abbr + 1, abbr, len);
	abbr[0] = '<';
	abbr[len + 1] = '>';
	abbr[len + 2] = '\0';
	return (ptrdiff_t)(len + 2);
}

static void updateminmax(const zic_t x)
{
    if (x < min_year) {
        min_year = x;
    }
    if (x > max_year) {
        max_year = x;
    }
}

static int write_uint_dec(char *dst, unsigned value)
{
	char buf[10];
	int i = 0;
	do {
		buf[i++] = (char)('0' + (value % 10u));
		value /= 10u;
	} while (value != 0u);
	int n = i;
	while (i-- > 0)
		*dst++ = buf[i];
	return n;
}

static int write_two_digits(char *dst, unsigned value)
{
	dst[0] = (char)('0' + (value / 10u));
	dst[1] = (char)('0' + (value % 10u));
	return 2;
}

static int
stringoffset(char *result, zic_t offset)
{
	if (!result)
		return 0;

	bool negative = offset < 0;
	uintmax_t mag = negative ? (uintmax_t)0 - (uintmax_t)offset : (uintmax_t)offset;

	unsigned seconds = (unsigned)(mag % SECSPERMIN);
	mag /= SECSPERMIN;
	unsigned minutes = (unsigned)(mag % MINSPERHOUR);
	mag /= MINSPERHOUR;
	uintmax_t hours_u = mag;

	uintmax_t limit = (uintmax_t)HOURSPERDAY * (uintmax_t)DAYSPERWEEK;
	if (hours_u >= limit) {
		result[0] = '\0';
		return 0;
	}

	int len = 0;
	if (negative)
		result[len++] = '-';

	len += write_uint_dec(result + len, (unsigned)hours_u);

	if (minutes != 0u || seconds != 0u) {
		result[len++] = ':';
		len += write_two_digits(result + len, minutes);
		if (seconds != 0u) {
			result[len++] = ':';
			len += write_two_digits(result + len, seconds);
		}
	}
	result[len] = '\0';
	return len;
}

static int
stringrule(char *result, struct rule *const rp, zic_t save, zic_t stdoff)
{
	if (!result || !rp)
		return -1;

	zic_t tod = rp->r_tod;
	int compat = 0;

	if (rp->r_dycode == DC_DOM) {
		int month, total;

		if (rp->r_dayofmonth == 29 && rp->r_month == TM_FEBRUARY)
			return -1;

		total = 0;
		for (month = 0; month < rp->r_month; ++month)
			total += len_months[0][month];

		if (rp->r_month <= 1) {
			int n = sprintf(result, "%d", total + rp->r_dayofmonth - 1);
			if (n < 0)
				return -1;
			result += n;
		} else {
			int n = sprintf(result, "J%d", total + rp->r_dayofmonth);
			if (n < 0)
				return -1;
			result += n;
		}
	} else {
		int week;
		int wday = rp->r_wday;
		int wdayoff;

		if (rp->r_dycode == DC_DOWGEQ) {
			wdayoff = (rp->r_dayofmonth - 1) % DAYSPERWEEK;
			if (wdayoff)
				compat = 2013;
			wday -= wdayoff;
			tod += (zic_t)wdayoff * SECSPERDAY;
			week = 1 + (rp->r_dayofmonth - 1) / DAYSPERWEEK;
		} else if (rp->r_dycode == DC_DOWLEQ) {
			if (rp->r_dayofmonth == len_months[1][rp->r_month]) {
				week = 5;
			} else {
				wdayoff = rp->r_dayofmonth % DAYSPERWEEK;
				if (wdayoff)
					compat = 2013;
				wday -= wdayoff;
				tod += (zic_t)wdayoff * SECSPERDAY;
				week = rp->r_dayofmonth / DAYSPERWEEK;
			}
		} else {
			return -1;
		}

		if (wday < 0)
			wday += DAYSPERWEEK;

		{
			int n = sprintf(result, "M%d.%d.%d", rp->r_month + 1, week, wday);
			if (n < 0)
				return -1;
			result += n;
		}
	}

	if (rp->r_todisut)
		tod += stdoff;
	if (rp->r_todisstd && !rp->r_isdst)
		tod += save;

	{
		zic_t midday = 2;
		midday *= SECSPERMIN;
		midday *= MINSPERHOUR;

		if (tod != midday) {
			*result++ = '/';
			if (!stringoffset(result, tod))
				return -1;
			if (tod < 0) {
				if (compat < 2013)
					compat = 2013;
			} else if (SECSPERDAY <= tod) {
				if (compat < 1994)
					compat = 1994;
			}
		}
	}
	return compat;
}

static int
rule_cmp(const struct rule *a, const struct rule *b)
{
	if (a == NULL && b == NULL) {
		return 0;
	}
	if (a == NULL) {
		return -1;
	}
	if (b == NULL) {
		return 1;
	}

	if (a->r_hiyear != b->r_hiyear) {
		return (a->r_hiyear < b->r_hiyear) ? -1 : 1;
	}
	if (a->r_hiyear == ZIC_MAX) {
		return 0;
	}

	{
		int mdiff = a->r_month - b->r_month;
		if (mdiff != 0) {
			return mdiff;
		}
	}

	return a->r_dayofmonth - b->r_dayofmonth;
}

/* Store into RESULT a proleptic TZ string that represent the future
   predictions for the zone ZPFIRST with ZONECOUNT entries.  Return a
   compatibility indicator (a TZDB release year) if successful, a
   negative integer if no such TZ string exists.  */
static int
stringzone(char *result, struct zone const *zpfirst, ptrdiff_t zonecount)
{
    if (!result)
        return -1;

    result[0] = '\0';

    if (!zpfirst || zonecount <= 0)
        return -1;

    if (hi_time < max_time)
        return -1;

    const struct zone *zp = zpfirst + zonecount - 1;

    struct rule *lastrp[2] = { NULL, NULL };
    for (ptrdiff_t i = 0; i < zp->z_nrules; ++i) {
        struct rule *rp = &zp->z_rules[i];
        struct rule **last = &lastrp[rp->r_isdst ? 1 : 0];
        int cmp = rule_cmp(*last, rp);
        if (cmp < 0) {
            *last = rp;
        } else if (cmp == 0) {
            return -1;
        }
    }

    struct rule *stdrp = lastrp[0];
    struct rule *dstrp = lastrp[1];
    int compat = 0;

    int dstcmp = zp->z_nrules ? rule_cmp(dstrp, stdrp) : (zp->z_isdst ? 1 : -1);
    const struct zone *stdzp = zp;
    const struct zone *dstzp = zp;

    struct zone zstr[2];
    struct rule stdr, dstr;

    if (dstcmp < 0) {
        dstrp = NULL;
    } else if (dstcmp > 0) {
        zic_t save = dstrp ? dstrp->r_save : zp->z_save;
        if (save >= 0) {
            zstr[0].z_stdoff = zp->z_stdoff + 2 * save;
            zstr[0].z_format = "XXX";
            zstr[0].z_format_specifier = 0;
            zstr[1].z_stdoff = zstr[0].z_stdoff;
            zstr[1].z_format = zp->z_format;
            zstr[1].z_format_specifier = zp->z_format_specifier;
            stdzp = &zstr[0];
            dstzp = &zstr[1];
        }
        dstr.r_month = TM_JANUARY;
        dstr.r_dycode = DC_DOM;
        dstr.r_dayofmonth = 1;
        dstr.r_tod = 0;
        dstr.r_todisstd = false;
        dstr.r_todisut = false;
        dstr.r_isdst = true;
        dstr.r_save = save < 0 ? save : -save;
        dstr.r_abbrvar = dstrp ? dstrp->r_abbrvar : NULL;

        stdr.r_month = TM_DECEMBER;
        stdr.r_dycode = DC_DOM;
        stdr.r_dayofmonth = 31;
        stdr.r_tod = SECSPERDAY + dstr.r_save;
        stdr.r_todisstd = false;
        stdr.r_todisut = false;
        stdr.r_isdst = false;
        stdr.r_save = 0;
        stdr.r_abbrvar = (save < 0 && stdrp) ? stdrp->r_abbrvar : NULL;

        dstrp = &dstr;
        stdrp = &stdr;
    }

    ptrdiff_t len = doabbr(result, stdzp, stdrp ? stdrp->r_abbrvar : NULL, false, 0, true);
    int offsetlen = stringoffset(result + len, -stdzp->z_stdoff);
    if (!offsetlen) {
        result[0] = '\0';
        return -1;
    }
    len += offsetlen;

    if (dstrp == NULL)
        return compat;

    len += doabbr(result + len, dstzp, dstrp->r_abbrvar, dstrp->r_isdst, dstrp->r_save, true);

    if (dstrp->r_save != SECSPERMIN * MINSPERHOUR) {
        offsetlen = stringoffset(result + len, -(dstzp->z_stdoff + dstrp->r_save));
        if (!offsetlen) {
            result[0] = '\0';
            return -1;
        }
        len += offsetlen;
    }

    result[len++] = ',';

    int c = stringrule(result + len, dstrp, dstrp->r_save, stdzp->z_stdoff);
    if (c < 0) {
        result[0] = '\0';
        return -1;
    }
    if (compat < c)
        compat = c;

    len += strlen(result + len);
    result[len++] = ',';

    c = stringrule(result + len, stdrp, dstrp->r_save, stdzp->z_stdoff);
    if (c < 0) {
        result[0] = '\0';
        return -1;
    }
    if (compat < c)
        compat = c;

    return compat;
}

static void
outzone(const struct zone *zpfirst, ptrdiff_t zonecount)
{
	ptrdiff_t i, j;
	zic_t starttime = 0, untiltime = 0;
	bool startttisstd = false;
	bool startttisut = false;
	char *startbuf;
	char *ab;
	char *envvar;
	int max_abbr_len;
	int max_envvar_len;
	int compat;
	bool do_extend;
	char version;
	zic_t nonTZlimtime = ZIC_MIN;
	int nonTZlimtype = -1;
	zic_t max_year0;
	int defaulttype = -1;

	check_for_signal();

	max_abbr_len = 2 + max_format_len + max_abbrvar_len;
	max_envvar_len = 2 * max_abbr_len + 5 * 9;

	startbuf = emalloc(max_abbr_len + 1);
	ab = emalloc(max_abbr_len + 1);
	envvar = emalloc(max_envvar_len + 1);

	timecnt = 0;
	typecnt = 0;
	charcnt = 0;

	min_year = max_year = EPOCH_YEAR;
	if (leapseen) {
		updateminmax(leapminyear);
		updateminmax(leapmaxyear + (leapmaxyear < ZIC_MAX));
	}
	for (i = 0; i < zonecount; ++i) {
		struct zone const *zp = &zpfirst[i];
		if (i < zonecount - 1)
			updateminmax(zp->z_untilrule.r_loyear);
		for (j = 0; j < zp->z_nrules; ++j) {
			struct rule *rp = &zp->z_rules[j];
			updateminmax(rp->r_loyear);
			if (rp->r_hiwasnum)
				updateminmax(rp->r_hiyear);
		}
	}

	compat = stringzone(envvar, zpfirst, zonecount);
	version = compat < 2013 ? '2' : '3';
	do_extend = compat < 0;
	if (noise) {
		if (!*envvar)
			warning("%s %s", _("no proleptic TZ string for zone"), zpfirst->z_name);
		else if (compat != 0)
			warning(_("%s: pre-%d clients may mishandle distant timestamps"),
				zpfirst->z_name, compat);
	}
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
	max_year = max(max_year, (redundant_time / (SECSPERDAY * DAYSPERNYEAR) + EPOCH_YEAR + 1));
	max_year0 = max_year;
	if (want_bloat()) {
		if (min_year > YEAR_32BIT_MIN - 1)
			min_year = YEAR_32BIT_MIN - 1;
		if (max_year < YEAR_32BIT_MAX)
			max_year = YEAR_32BIT_MAX;
	}

	if (min_time < lo_time || hi_time < max_time)
		unspecifiedtype = addtype(0, "-00", false, false, false);

	for (i = 0; i < zonecount; ++i) {
		zic_t save = 0;
		struct zone const *zp = &zpfirst[i];
		bool usestart = i > 0 && (zp - 1)->z_untiltime > min_time;
		bool useuntil = i < (zonecount - 1);
		zic_t stdoff = zp->z_stdoff;
		zic_t startoff = stdoff;

		if (useuntil && zp->z_untiltime <= min_time)
			continue;

		eat(zp->z_filenum, zp->z_linenum);
		*startbuf = '\0';

		if (zp->z_nrules == 0) {
			int type;
			save = zp->z_save;
			doabbr(startbuf, zp, NULL, zp->z_isdst, save, false);
			type = addtype(oadd(zp->z_stdoff, save), startbuf, zp->z_isdst, startttisstd, startttisut);
			if (usestart) {
				addtt(starttime, type);
				if (useuntil && nonTZlimtime < starttime) {
					nonTZlimtime = starttime;
					nonTZlimtype = type;
				}
				usestart = false;
			} else {
				defaulttype = type;
			}
		} else {
			zic_t year;
			for (year = min_year; year <= max_year; ++year) {
				if (useuntil && year > zp->z_untilrule.r_hiyear)
					break;

				{
					zic_t y2038_boundary = ((zic_t)1) << 31;
					for (j = 0; j < zp->z_nrules; ++j) {
						struct rule *rp = &zp->z_rules[j];
						eats(zp->z_filenum, zp->z_linenum, rp->r_filenum, rp->r_linenum);
						rp->r_todo = (year >= rp->r_loyear && year <= rp->r_hiyear);
						if (rp->r_todo) {
							rp->r_temp = rpytime(rp, year);
							rp->r_todo = (rp->r_temp < y2038_boundary || year <= max_year0);
						}
					}
				}

				for (;;) {
					ptrdiff_t k = -1;
					zic_t jtime = 0, ktime = 0, offset = 0;
					struct rule *rp;
					int type;

					if (useuntil) {
						untiltime = zp->z_untiltime;
						if (!zp->z_untilrule.r_todisut)
							untiltime = tadd(untiltime, -stdoff);
						if (!zp->z_untilrule.r_todisstd)
							untiltime = tadd(untiltime, -save);
					}

					for (j = 0; j < zp->z_nrules; ++j) {
						struct rule *r = &zp->z_rules[j];
						if (!r->r_todo)
							continue;
						eats(zp->z_filenum, zp->z_linenum, r->r_filenum, r->r_linenum);
						offset = r->r_todisut ? 0 : stdoff;
						if (!r->r_todisstd)
							offset = oadd(offset, save);
						jtime = r->r_temp;
						if (jtime == min_time || jtime == max_time)
							continue;
						jtime = tadd(jtime, -offset);
						if (k < 0 || jtime < ktime) {
							k = j;
							ktime = jtime;
						} else if (jtime == ktime) {
							char const *dup_rules_msg = _("two rules for same instant");
							eats(zp->z_filenum, zp->z_linenum, r->r_filenum, r->r_linenum);
							warning("%s", dup_rules_msg);
							r = &zp->z_rules[k];
							eats(zp->z_filenum, zp->z_linenum, r->r_filenum, r->r_linenum);
							error("%s", dup_rules_msg);
						}
					}

					if (k < 0)
						break;

					rp = &zp->z_rules[k];
					rp->r_todo = false;

					if (useuntil && ktime >= untiltime) {
						if (!*startbuf && (oadd(zp->z_stdoff, rp->r_save) == startoff))
							doabbr(startbuf, zp, rp->r_abbrvar, rp->r_isdst, rp->r_save, false);
						break;
					}

					save = rp->r_save;

					if (usestart && ktime == starttime)
						usestart = false;

					if (usestart) {
						if (ktime < starttime) {
							startoff = oadd(zp->z_stdoff, save);
							doabbr(startbuf, zp, rp->r_abbrvar, rp->r_isdst, rp->r_save, false);
							continue;
						}
						if (*startbuf == '\0' && startoff == oadd(zp->z_stdoff, save))
							doabbr(startbuf, zp, rp->r_abbrvar, rp->r_isdst, rp->r_save, false);
					}

					eats(zp->z_filenum, zp->z_linenum, rp->r_filenum, rp->r_linenum);
					doabbr(ab, zp, rp->r_abbrvar, rp->r_isdst, rp->r_save, false);
					offset = oadd(zp->z_stdoff, rp->r_save);
					type = addtype(offset, ab, rp->r_isdst, rp->r_todisstd, rp->r_todisut);
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
				starttime = tadd(starttime, -stdoff);
		}
	}

	if (defaulttype < 0)
		defaulttype = 0;

	if (!do_extend && !want_bloat()) {
		zic_t keep_at_max;
		zic_t TZstarttime = ZIC_MAX;

		for (i = 0; i < timecnt; i++) {
			zic_t at = attypes[i].at;
			if (nonTZlimtime < at && at < TZstarttime)
				TZstarttime = at;
		}
		if (TZstarttime == ZIC_MAX)
			TZstarttime = nonTZlimtime;

		keep_at_max = max(TZstarttime, redundant_time);
		for (i = 0, j = 0; i < timecnt; i++) {
			if (attypes[i].at <= keep_at_max) {
				attypes[j].at = attypes[i].at;
				attypes[j].dontmerge = (attypes[i].at == TZstarttime &&
					(nonTZlimtype != attypes[i].type || strchr(envvar, ',')));
				attypes[j].type = attypes[i].type;
				j++;
			}
		}
		timecnt = j;
	}

	if (do_extend) {
		struct rule xr;
		struct attype *lastat = NULL;

		xr.r_month = TM_JANUARY;
		xr.r_dycode = DC_DOM;
		xr.r_dayofmonth = 1;
		xr.r_tod = 0;

		for (lastat = attypes, i = 1; i < timecnt; i++)
			if (attypes[i].at > lastat->at)
				lastat = &attypes[i];

		if (!lastat || lastat->at < rpytime(&xr, max_year - 1)) {
			addtt(rpytime(&xr, max_year + 1), lastat ? lastat->type : defaulttype);
			attypes[timecnt - 1].dontmerge = true;
		}
	}

	writezone(zpfirst->z_name, envvar, version, defaulttype);

	free(startbuf);
	free(ab);
	free(envvar);
}

static void
addtt(zic_t starttime, int type)
{
	const int idx = timecnt;
	attypes = growalloc(attypes, sizeof *attypes, idx, &timecnt_alloc);
	attypes[idx].at = starttime;
	attypes[idx].dontmerge = false;
	attypes[idx].type = type;
	timecnt = idx + 1;
}

static int
addtype(zic_t utoff, char const *abbr, bool isdst, bool ttisstd, bool ttisut)
{
	int type_index;
	int abbr_index;

	if (! (-1L - 2147483647L <= utoff && utoff <= 2147483647L)) {
		error(_("UT offset out of range"));
		exit(EXIT_FAILURE);
	}

	if (!want_bloat())
		ttisstd = ttisut = false;

	if (abbr == NULL) {
		error(_("null abbreviation"));
		exit(EXIT_FAILURE);
	}

	abbr_index = 0;
	while (abbr_index < charcnt) {
		if (strcmp(&chars[abbr_index], abbr) == 0)
			break;
		abbr_index += (int)strlen(&chars[abbr_index]) + 1;
	}
	if (abbr_index == charcnt) {
		newabbr(abbr);
	} else {
		for (type_index = 0; type_index < typecnt; type_index++) {
			if (utoff == utoffs[type_index]
			    && isdst == isdsts[type_index]
			    && abbr_index == desigidx[type_index]
			    && ttisstd == ttisstds[type_index]
			    && ttisut == ttisuts[type_index]) {
				return type_index;
			}
		}
	}

	if (typecnt >= TZ_MAX_TYPES) {
		error(_("too many local time types"));
		exit(EXIT_FAILURE);
	}

	type_index = typecnt++;
	utoffs[type_index] = utoff;
	isdsts[type_index] = isdst;
	ttisstds[type_index] = ttisstd;
	ttisuts[type_index] = ttisut;
	desigidx[type_index] = abbr_index;
	return type_index;
}

static void
leapadd(zic_t t, int correction, int rolling)
{
	int i;

	if (leapcnt >= TZ_MAX_LEAPS) {
		error(_("too many leap seconds"));
		exit(EXIT_FAILURE);
	}
	if (rolling && (lo_time != min_time || hi_time != max_time)) {
		error(_("Rolling leap seconds not supported with -r"));
		exit(EXIT_FAILURE);
	}

	for (i = 0; i < leapcnt && t > trans[i]; ++i)
		;

	if (leapcnt > i) {
		size_t move_count = (size_t)(leapcnt - i);
		memmove(&trans[i + 1], &trans[i], move_count * sizeof trans[0]);
		memmove(&corr[i + 1], &corr[i], move_count * sizeof corr[0]);
		memmove(&roll[i + 1], &roll[i], move_count * sizeof roll[0]);
	}

	trans[i] = t;
	corr[i] = correction;
	roll[i] = rolling;
	++leapcnt;
}

static void adjleap(void)
{
	int i;
	zic_t last_correction = 0;
	zic_t prev_transition = 0;
	const zic_t min_gap = 28 * SECSPERDAY;

	for (i = 0; i < leapcnt; ++i) {
		zic_t current = trans[i];
		zic_t gap = current - prev_transition;

		if (gap < min_gap) {
			error(_("Leap seconds too close together"));
			exit(EXIT_FAILURE);
		}

		prev_transition = current;
		trans[i] = tadd(current, last_correction);
		last_correction = (corr[i] += last_correction);
	}

	if (leapexpires >= 0) {
		leapexpires = oadd(leapexpires, last_correction);
		if (leapcnt != 0 && trans[leapcnt - 1] >= leapexpires) {
			error(_("last Leap time does not precede Expires time"));
			exit(EXIT_FAILURE);
		}
	}
}

/* Is A a space character in the C locale?  */
static bool is_space(char c)
{
    unsigned char uc = (unsigned char)c;
    return uc == ' ' || uc == '\f' || uc == '\n' || uc == '\r' || uc == '\t' || uc == '\v';
}

/* Is A an alphabetic character in the C locale?  */
static bool
is_alpha(char a)
{
    return (a >= 'A' && a <= 'Z') || (a >= 'a' && a <= 'z');
}

/* If A is an uppercase character in the C locale, return its lowercase
   counterpart.  Otherwise, return A.  */
#include <ctype.h>

static char lowerit(char a)
{
    unsigned char ua = (unsigned char)a;
    int r = tolower(ua);
    return (char)r;
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
	if (word == NULL || table == NULL)
		return NULL;

	const char *key = word;
	const struct lookup *tab = table;

	if (tab == lasts && ciprefix("last", key) && key[4]) {
		if (key[4] == '-')
			warning(_("\"%s\" is undocumented; use \"last%s\" instead"), key, key + 5);
		else {
			key += 4;
			tab = wday_names;
		}
	}

	const struct lookup *found = NULL;
	for (const struct lookup *p = tab; p->l_word != NULL; ++p) {
		if (ciequal(key, p->l_word))
			return p;
		if (ciprefix(key, p->l_word)) {
			if (found == NULL)
				found = p;
			else
				return NULL;
		}
	}

	if (found && noise) {
		bool pre_2017c_match = false;
		for (const struct lookup *p = tab; p->l_word; p++) {
			if (itsabbr(key, p->l_word)) {
				if (pre_2017c_match) {
					warning(_("\"%s\" is ambiguous in pre-2017c zic"), key);
					break;
				}
				pre_2017c_match = true;
			}
		}
	}

	return found;
}

static int
getfields(char *cp, char **array, int arrayelts)
{
    int nsubs = 0;

    if (cp == NULL || array == NULL || arrayelts < 0) {
        error(_("Invalid arguments"));
        exit(EXIT_FAILURE);
    }

    for (;;) {
        char *dstart;
        char *dp;

        while (is_space(*cp)) {
            ++cp;
        }

        if (*cp == '\0' || *cp == '#') {
            break;
        }

        dstart = dp = cp;

        while (*cp && *cp != '#' && !is_space(*cp)) {
            if (*cp == '"') {
                ++cp;
                for (;;) {
                    char c = *cp++;
                    if (c == '"') {
                        break;
                    }
                    if (c == '\0') {
                        error(_("Odd number of quotation marks"));
                        exit(EXIT_FAILURE);
                    }
                    *dp++ = c;
                }
            } else {
                *dp++ = *cp++;
            }
        }

        if (is_space(*cp)) {
            ++cp;
        }

        *dp = '\0';

        if (nsubs == arrayelts) {
            error(_("Too many input fields"));
            exit(EXIT_FAILURE);
        }

        array[nsubs++] = dstart + ((*dstart == '-') && (dp == dstart + 1));
    }

    return nsubs;
}

ATTRIBUTE_NORETURN static void
time_overflow(void)
{
  const char *msg = _("time overflow");
  if (msg == NULL)
    msg = "time overflow";
  error(msg);
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
	int m;
	int i;
	zic_t dayoff;
	zic_t y;
	int yrem;

	if (!rp) {
		error(_("invalid rule"));
		exit(EXIT_FAILURE);
	}

	if (wantedy == ZIC_MIN)
		return min_time;
	if (wantedy == ZIC_MAX)
		return max_time;

	m = TM_JANUARY;
	y = EPOCH_YEAR;

	yrem = wantedy % YEARSPERREPEAT - y % YEARSPERREPEAT;
	dayoff = ((wantedy / YEARSPERREPEAT - y / YEARSPERREPEAT
	           + yrem / YEARSPERREPEAT - (yrem % YEARSPERREPEAT < 0))
	          * DAYSPERREPEAT);
	wantedy = y + (yrem + 2 * YEARSPERREPEAT) % YEARSPERREPEAT;

	while (wantedy != y) {
		int yearlen = len_years[isleap(y)];
		dayoff = oadd(dayoff, yearlen);
		y++;
	}

	while (m != rp->r_month) {
		int monthlen = len_months[isleap(y)][m];
		dayoff = oadd(dayoff, monthlen);
		++m;
	}

	i = rp->r_dayofmonth;
	if (m == TM_FEBRUARY && i == 29 && !isleap(y)) {
		if (rp->r_dycode == DC_DOWLEQ)
			--i;
		else {
			error(_("use of 2/29 in non leap-year"));
			exit(EXIT_FAILURE);
		}
	}

	--i;
	dayoff = oadd(dayoff, i);

	if (rp->r_dycode == DC_DOWGEQ || rp->r_dycode == DC_DOWLEQ) {
		zic_t wday = ((EPOCH_WDAY + dayoff % DAYSPERWEEK + DAYSPERWEEK) % DAYSPERWEEK);
		if (rp->r_dycode == DC_DOWGEQ) {
			int delta = (rp->r_wday - (int)wday + DAYSPERWEEK) % DAYSPERWEEK;
			dayoff = oadd(dayoff, delta);
			i += delta;
		} else {
			int delta = ((int)wday - rp->r_wday + DAYSPERWEEK) % DAYSPERWEEK;
			dayoff = oadd(dayoff, -delta);
			i -= delta;
		}
		if (i < 0 || i >= len_months[isleap(y)][m]) {
			if (noise)
				warning(_("rule goes past start/end of month; will not work with pre-2004 versions of zic"));
		}
	}

	if (dayoff < min_time / SECSPERDAY)
		return min_time;
	if (dayoff > max_time / SECSPERDAY)
		return max_time;

	return tadd((zic_t) dayoff * SECSPERDAY, rp->r_tod);
}

static void
newabbr(const char *string)
{
	if (string == NULL) {
		error(_("null time zone abbreviation"));
		exit(EXIT_FAILURE);
	}

	if (strcmp(string, GRANDPARENTED) != 0) {
		const char *cp = string;
		const char *mp = NULL;

		while (is_alpha(*cp) || ('0' <= *cp && *cp <= '9') || *cp == '-' || *cp == '+')
			++cp;

		size_t abbr_len = (size_t)(cp - string);

		if (noise && abbr_len < 3)
			mp = _("time zone abbreviation has fewer than 3 characters");
		if (abbr_len > (size_t)ZIC_MAX_ABBR_LEN_WO_WARN)
			mp = _("time zone abbreviation has too many characters");
		if (*cp != '\0')
			mp = _("time zone abbreviation differs from POSIX standard");
		if (mp != NULL)
			warning("%s (%s)", mp, string);
	}

	size_t len = strlen(string) + 1;
	if ((size_t)charcnt + len > (size_t)TZ_MAX_CHARS) {
		error(_("too many, or too long, time zone abbreviations"));
		exit(EXIT_FAILURE);
	}

	memcpy(&chars[charcnt], string, len);
	charcnt += (int)len;
}

/* Ensure that the directories of ARGNAME exist, by making any missing
   ones.  If ANCESTORS, do this only for ARGNAME's ancestors; otherwise,
   do it for ARGNAME too.  Exit with failure if there is trouble.
   Do not consider an existing file to be trouble.  */
static void
mkdirs(char const *argname, bool ancestors)
{
    char *name = estrdup(argname);
    char *start = name;

    while (*start == '/')
        start++;

    if (ancestors) {
        char *s = start;
        for (;;) {
            char *slash = strchr(s, '/');
            if (!slash)
                break;

            *slash = '\0';
            if (mkdir(name, MKDIR_UMASK) != 0) {
                int err = errno;
                if (err == ELOOP || err == ENAMETOOLONG
                    || err == ENOENT || err == ENOTDIR) {
                    error(_("%s: Can't create directory %s: %s"),
                          progname, name, strerror(err));
                    free(name);
                    exit(EXIT_FAILURE);
                }
            }
            *slash = '/';
            s = slash + 1;
        }
    } else {
        if (mkdir(name, MKDIR_UMASK) != 0) {
            int err = errno;
            if (err == ELOOP || err == ENAMETOOLONG
                || err == ENOENT || err == ENOTDIR) {
                error(_("%s: Can't create directory %s: %s"),
                      progname, name, strerror(err));
                free(name);
                exit(EXIT_FAILURE);
            }
        }
    }

    free(name);
}
