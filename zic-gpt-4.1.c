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
#include <stddef.h>
#include <sys/types.h>

ssize_t readlink(const char *restrict file, char *restrict buf, size_t size) {
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

ATTRIBUTE_NORETURN static void memory_exhausted(const char *msg)
{
	if (msg == NULL) {
		msg = "Unknown error";
	}
	if (progname == NULL) {
		progname = "Program";
	}
	fprintf(stderr, "%s: Memory exhausted: %s\n", progname, msg);
	exit(EXIT_FAILURE);
}

ATTRIBUTE_NORETURN static void size_overflow(void)
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
char *safe_strdup(const char *str)
{
    if (str == NULL) {
        return NULL;
    }
    size_t len = strlen(str) + 1;
    char *result = malloc(len);
    if (result == NULL) {
        return NULL;
    }
    memcpy(result, str, len);
    return result;
}
#endif

static void *memcheck(void *ptr) {
	if (ptr != NULL) {
		return ptr;
	}
	const char *err_msg = HAVE_MALLOC_ERRNO ? strerror(errno) : strerror(ENOMEM);
	memory_exhausted(err_msg);
	return NULL;
}

static void *emalloc(size_t size)
{
    void *ptr = malloc(size);
    if (!ptr) {
        // Handle allocation failure appropriately, e.g., abort or return NULL.
        // Here we assume memcheck handles error, but for better reliability:
        // Optionally log or handle error here before calling memcheck.
    }
    return memcheck(ptr);
}

static void *erealloc(void *ptr, size_t size) {
    void *new_ptr = realloc(ptr, size);
    if (!new_ptr && size != 0) {
        // Handle allocation failure (can log error, clean up, or abort as needed)
        abort();
    }
    return memcheck(new_ptr);
}

static char *
estrdup(const char *str)
{
    if (str == NULL) {
        return NULL;
    }
    char *dup = strdup(str);
    if (dup == NULL) {
        // Handle memory allocation failure appropriately for your program
        // Here, we simply return NULL, or you might want to exit or log the error
        return NULL;
    }
    return dup;
}

static ptrdiff_t grow_nitems_alloc(ptrdiff_t *nitems_alloc, ptrdiff_t itemsize) {
    if (nitems_alloc == NULL || itemsize <= 0) {
        memory_exhausted(_("invalid argument"));
    }

    ptrdiff_t original = *nitems_alloc;
    ptrdiff_t addend = (original >> 1) + 1;

#if defined ckd_add && defined ckd_mul
    ptrdiff_t new_nitems;
    ptrdiff_t product;
    if (ckd_add(&new_nitems, original, addend) == 0 &&
        ckd_mul(&product, new_nitems, itemsize) == 0 &&
        product <= INDEX_MAX) {
        *nitems_alloc = new_nitems;
        return product;
    }
#else
    ptrdiff_t max_nitems = ((INDEX_MAX - 1) / 3 * 2) / itemsize;
    if (original <= max_nitems) {
        *nitems_alloc = original + addend;
        return *nitems_alloc * itemsize;
    }
#endif

    memory_exhausted(_("integer overflow"));
    return -1;
}

static void *growalloc(void *ptr, ptrdiff_t itemsize, ptrdiff_t nitems, ptrdiff_t *nitems_alloc) {
    if (nitems < *nitems_alloc) {
        return ptr;
    }
    ptrdiff_t new_alloc = grow_nitems_alloc(nitems_alloc, itemsize);
    if (new_alloc <= 0) {
        return NULL;
    }
    void *new_ptr = erealloc(ptr, new_alloc);
    if (!new_ptr) {
        return NULL;
    }
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
static const char *filename(int i)
{
    if (i == COMMAND_LINE_FILENUM) {
        return _("command line");
    }

    const char *fname = (i == LEAPSEC_FILENUM) ? leapsec : main_argv[i];
    if (fname == NULL) {
        return _("unknown file");
    }

    return (strcmp(fname, "-") == 0) ? _("standard input") : fname;
}

static void eats(int fnum, lineno num, int rfnum, lineno rnum) {
    filenum = fnum;
    linenum = num;
    rfilenum = rfnum;
    rlinenum = rnum;
}

static void eat(int fnum, lineno num) {
	eats(fnum, num, 0, -1);
}

ATTRIBUTE_FORMAT((printf, 1, 0)) static void verror(const char *const string, va_list args)
{
	if (filenum != 0) {
		const char *fname = filename(filenum);
		if (fname)
			fprintf(stderr, "\"%s\", line %" PRIdMAX ": ", fname, linenum);
		else
			fprintf(stderr, "Unknown file, line %" PRIdMAX ": ", linenum);
	}
	vfprintf(stderr, string, args);
	if (rfilenum != 0) {
		const char *rfname = filename(rfilenum);
		if (rfname)
			fprintf(stderr, " (rule from \"%s\", line %" PRIdMAX ")", rfname, rlinenum);
		else
			fprintf(stderr, " (rule from unknown file, line %" PRIdMAX ")", rlinenum);
	}
	fprintf(stderr, "\n");
}

ATTRIBUTE_FORMAT((printf, 1, 2)) static void error(const char *string, ...)
{
    va_list args;
    if (!string) {
        return;
    }
    va_start(args, string);
    verror(string, args);
    va_end(args);
    errors = true;
}

ATTRIBUTE_FORMAT((printf, 1, 2)) static void warning(const char *string, ...)
{
    va_list args;
    if (!string) {
        fprintf(stderr, "warning: (null)\n");
        warnings = true;
        return;
    }
    fprintf(stderr, "%s", _("warning: "));
    va_start(args, string);
    verror(string, args);
    va_end(args);
    warnings = true;
}

/* Close STREAM.  If it had an I/O error, report it against DIR/NAME,
   remove TEMPNAME if nonnull, and then exit.  */
static void close_file(FILE *stream, const char *dir, const char *name, const char *tempname)
{
    const char *error_msg = NULL;

    if (ferror(stream)) {
        error_msg = _("I/O error");
    } else if (fclose(stream) != 0) {
        error_msg = strerror(errno);
    }

    if (error_msg) {
        fprintf(stderr, "%s: ", progname);
        if (dir) {
            fprintf(stderr, "%s/", dir);
        }
        if (name) {
            fprintf(stderr, "%s: ", name);
        }
        fprintf(stderr, "%s\n", error_msg);

        if (tempname) {
            remove(tempname);
        }
        exit(EXIT_FAILURE);
    }
}

ATTRIBUTE_NORETURN static void
usage(FILE *stream, int status)
{
  if (!stream || !progname || !REPORT_BUGS_TO) {
    exit(EXIT_FAILURE);
  }

  fprintf(stream,
    _("%s: usage is %s [ --version ] [ --help ] [ -v ] \\\n"
      "\t[ -b {slim|fat} ] [ -d directory ] [ -l localtime ]"
      " [ -L leapseconds ] \\\n"
      "\t[ -p posixrules ] [ -r '[@lo][/@hi]' ] [ -R '@hi' ] \\\n"
      "\t[ -t localtime-link ] \\\n"
      "\t[ filename ... ]\n\n"
      "Report bugs to %s.\n"),
    progname, progname, REPORT_BUGS_TO);

  if (status == EXIT_SUCCESS) {
    if (close_file(stream, NULL, NULL, NULL) != 0) {
      exit(EXIT_FAILURE);
    }
  }
  exit(status);
}

/* Change the working directory to DIR, possibly creating DIR and its
   ancestors.  After this is done, all files are accessed with names
   relative to DIR.  */
static void change_directory(const char *dir) {
    if (dir == NULL) {
        fprintf(stderr, "%s: Directory path is NULL\n", progname);
        exit(EXIT_FAILURE);
    }

    if (chdir(dir) == 0) {
        return;
    }

    int chdir_errno = errno;
    if (chdir_errno == ENOENT) {
        if (mkdirs(dir, false) != 0) {
            fprintf(stderr, "%s: Failed to create directory %s\n", progname, dir);
            exit(EXIT_FAILURE);
        }
        if (chdir(dir) == 0) {
            return;
        }
        chdir_errno = errno;
    }

    fprintf(stderr, "%s: Can't chdir to %s: %s\n", progname, dir, strerror(chdir_errno));
    exit(EXIT_FAILURE);
}

/* Compare the two links A and B, for a stable sort by link name.  */
static int qsort_linkcmp(const void *a, const void *b) {
    const struct link *l = (const struct link *)a;
    const struct link *m = (const struct link *)b;

    int cmp = strcmp(l->l_linkname, m->l_linkname);
    if (cmp != 0) {
        return cmp;
    }

    if (l->l_filenum != m->l_filenum) {
        return (l->l_filenum > m->l_filenum) - (l->l_filenum < m->l_filenum);
    }

    return (l->l_linenum > m->l_linenum) - (l->l_linenum < m->l_linenum);
}

/* Compare the string KEY to the link B, for bsearch.  */
static int bsearch_linkcmp(const void *key, const void *b) {
    const struct link *m = (const struct link *)b;
    const char *linkname = m ? m->l_linkname : NULL;
    const char *search_key = (const char *)key;
    if (!search_key || !linkname) {
        return (search_key == linkname) ? 0 : (search_key ? 1 : -1);
    }
    return strcmp(search_key, linkname);
}

/* Make the links specified by the Link lines.  */
static void make_links(void) {
    ptrdiff_t i, j, nalinks, pass_size;

    if (nlinks > 1)
        qsort(links, nlinks, sizeof *links, qsort_linkcmp);

    j = 0;
    for (i = 0; i < nlinks; ) {
        ptrdiff_t next = i + 1;
        while (next < nlinks && strcmp(links[i].l_linkname, links[next].l_linkname) == 0)
            next++;
        links[j++] = links[i];
        i = next;
    }
    nlinks = pass_size = j;

    j = nalinks = nlinks;

    for (i = 0; i < nalinks; i++) {
        struct link *l;

        eat(links[i].l_filenum, links[i].l_linenum);

        if (i == j) {
            if ((nalinks - i) == pass_size) {
                error(_("\"Link %s %s\" is part of a link cycle"),
                      links[i].l_target, links[i].l_linkname);
                break;
            }
            j = nalinks;
            pass_size = nalinks - i;
        }

        if (strcmp(links[i].l_target, links[i].l_linkname) == 0) {
            error(_("link %s targets itself"), links[i].l_target);
            continue;
        }

        l = bsearch(links[i].l_target, &links[i + 1], j - (i + 1), sizeof *links, bsearch_linkcmp);
        if (!l)
            l = bsearch(links[i].l_target, &links[j], nalinks - j, sizeof *links, bsearch_linkcmp);

        if (!l) {
            dolink(links[i].l_target, links[i].l_linkname, false);
        } else {
            struct link *temp = growalloc(links, sizeof *links, nalinks, &nlinks_alloc);
            if (!temp) {
                error(_("memory allocation failed for links"));
                return;
            }
            links = temp;
            links[nalinks++] = links[i];
        }

        if (noise && i < nlinks) {
            if (l) {
                warning(_("link %s targeting link %s mishandled by pre-2023 zic"),
                        links[i].l_linkname, links[i].l_target);
            } else if (bsearch(links[i].l_target, links, nlinks, sizeof *links, bsearch_linkcmp)) {
                warning(_("link %s targeting link %s"),
                        links[i].l_linkname, links[i].l_target);
            }
        }
    }
}

/* Simple signal handling: just set a flag that is checked
   periodically outside critical sections.  To set up the handler,
   prefer sigaction if available to close a signal race.  */

static sig_atomic_t got_signal;

static void signal_handler(int sig)
{
#ifdef SA_SIGINFO
    (void)sig;
#else
    signal(sig, signal_handler);
    got_signal = sig;
#endif
}

/* Arrange for SIGINT etc. to be caught by the handler.  */
static void catch_signals(void)
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
    size_t i, num_signals = sizeof(signals) / sizeof(signals[0]);
    for (i = 0; i < num_signals; i++) {
#ifdef SA_SIGINFO
        struct sigaction act = {0}, old_act = {0};
        act.sa_handler = signal_handler;
        if (sigemptyset(&act.sa_mask) != 0)
            continue;
        act.sa_flags = 0;
        if (sigaction(signals[i], &act, &old_act) == 0) {
            if (!(old_act.sa_flags & SA_SIGINFO) && old_act.sa_handler == SIG_IGN) {
                sigaction(signals[i], &old_act, NULL);
                got_signal = 0;
            }
        }
#else
        void (*prev_handler)(int) = signal(signals[i], signal_handler);
        if (prev_handler == SIG_IGN) {
            signal(signals[i], SIG_IGN);
            got_signal = 0;
        }
#endif
    }
}

/* If a signal has arrived, terminate zic with appropriate status.  */
static void check_for_signal(void)
{
    int sig = got_signal;
    if (sig != 0) {
        if (signal(sig, SIG_DFL) == SIG_ERR) {
            abort();
        }
        if (raise(sig) != 0) {
            abort();
        }
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
static bool timerange_option(const char *timerange)
{
    intmax_t lo = min_time, hi = max_time;
    char *endptr;
    if (timerange == NULL)
        return false;

    if (*timerange == '@') {
        errno = 0;
        lo = strtoimax(timerange + 1, &endptr, 10);
        if (endptr == timerange + 1 || errno == ERANGE)
            return false;
        timerange = endptr;
    }

    if (timerange[0] == '/' && timerange[1] == '@') {
        errno = 0;
        hi = strtoimax(timerange + 2, &endptr, 10);
        if (endptr == timerange + 2 || errno == ERANGE)
            return false;
        timerange = endptr;
    }

    if (*timerange != '\0' || hi < lo || max_time < lo || hi < min_time)
        return false;

    lo_time = lo < min_time ? min_time : lo;
    hi_time = hi > max_time ? max_time : hi;
    return true;
}

/* Generate redundant time stamps up to OPT.  Return true if successful.  */
static bool redundant_time_option(const char *opt)
{
    if (opt == NULL || opt[0] != '@') {
        return false;
    }

    char *opt_end = NULL;
    int error_saved = 0;
    errno = 0;
    intmax_t redundant = strtoimax(opt + 1, &opt_end, 10);
    error_saved = errno;

    if (opt_end == opt + 1 || *opt_end != '\0' || error_saved != 0) {
        return false;
    }

    redundant_time = (redundant_time > redundant) ? redundant_time : redundant;
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

static bool want_bloat(void) {
    return bloat >= 0;
}

#ifndef ZIC_BLOAT_DEFAULT
# define ZIC_BLOAT_DEFAULT "slim"
#endif

int main(int argc, char **argv)
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
    progname = argv[0] ? argv[0] : "zic";
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
        }
        if (strcmp(argv[k], "--help") == 0) {
            usage(stdout, EXIT_SUCCESS);
        }
    }

    while ((c = getopt(argc, argv, "b:d:l:L:p:r:R:st:vy:")) != EOF && c != -1) {
        switch (c) {
            case 'b':
                if (strcmp(optarg, "slim") == 0) {
                    if (bloat > 0)
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
                    fprintf(stderr, _("%s: More than one -d option specified\n"), progname);
                    return EXIT_FAILURE;
                }
                break;
            case 'l':
                if (lcltime == NULL) {
                    lcltime = optarg;
                } else {
                    fprintf(stderr, _("%s: More than one -l option specified\n"), progname);
                    return EXIT_FAILURE;
                }
                break;
            case 'p':
                if (psxrules == NULL) {
                    psxrules = optarg;
                } else {
                    fprintf(stderr, _("%s: More than one -p option specified\n"), progname);
                    return EXIT_FAILURE;
                }
                break;
            case 't':
                if (tzdefault != NULL) {
                    fprintf(stderr, _("%s: More than one -t option specified\n"), progname);
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
                    fprintf(stderr, _("%s: More than one -L option specified\n"), progname);
                    return EXIT_FAILURE;
                }
                break;
            case 'v':
                noise = true;
                break;
            case 'r':
                if (timerange_given) {
                    fprintf(stderr, _("%s: More than one -r option specified\n"), progname);
                    return EXIT_FAILURE;
                }
                if (!timerange_option(optarg)) {
                    fprintf(stderr, _("%s: invalid time range: %s\n"), progname, optarg);
                    return EXIT_FAILURE;
                }
                timerange_given = true;
                break;
            case 'R':
                if (!redundant_time_option(optarg)) {
                    fprintf(stderr, _("%s: invalid time: %s\n"), progname, optarg);
                    return EXIT_FAILURE;
                }
                break;
            case 's':
                warning(_("-s ignored"));
                break;
            default:
                usage(stderr, EXIT_FAILURE);
        }
    }

    if (optind == argc - 1 && strcmp(argv[optind], "=") == 0) {
        usage(stderr, EXIT_FAILURE);
    }

    if (hi_time + (hi_time < ZIC_MAX) < redundant_time) {
        fprintf(stderr, _("%s: -R time exceeds -r cutoff\n"), progname);
        return EXIT_FAILURE;
    }
    if (redundant_time < lo_time)
        redundant_time = lo_time;
    if (bloat == 0) {
        static const char bloat_default[] = ZIC_BLOAT_DEFAULT;
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

    for (k = optind; k < argc; k++) {
        infile(k, argv[k]);
    }
    if (errors)
        return EXIT_FAILURE;

    associate();
    change_directory(directory);
    catch_signals();

    for (i = 0; i < nzones; i = j) {
        for (j = i + 1; j < nzones && zones[j].z_name == NULL; ++j)
            ;
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


static bool componentcheck(const char *name, const char *component, const char *component_end) {
    enum { component_len_max = 14 };
    ptrdiff_t component_len = component_end - component;

    if (component_len == 0) {
        if (!*name) {
            error(_("empty file name"));
        } else {
            if (component == name) {
                error(_("file name '%s' begins with '/'"), name);
            } else if (*component_end) {
                error(_("file name '%s' contains '//'"), name);
            } else {
                error(_("file name '%s' ends with '/'"), name);
            }
        }
        return false;
    }

    if (component_len > 0 && component_len <= 2 &&
        component[0] == '.' && component_end[-1] == '.') {
        error(_("file name '%s' contains '%.*s' component"),
              name, (int)component_len, component);
        return false;
    }

    if (noise) {
        if (component_len > 0 && component[0] == '-') {
            warning(_("file name '%s' component contains leading '-'"), name);
        }
        if (component_len > component_len_max) {
            warning(_("file name '%s' contains overlength component '%.*s...'"),
                    name, component_len_max, component);
        }
    }

    return true;
}

static bool namecheck(const char *name)
{
    static const char benign[] =
        "-/_"
        "abcdefghijklmnopqrstuvwxyz"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    static const char printable_and_not_benign[] =
        " !\"#$%&'()*+,.0123456789:;<=>?@[\\]^`{|}~";

    const char *component = name;
    const char *cp = name;
    if (!name)
        return false;

    while (*cp) {
        unsigned char c = (unsigned char)*cp;

        if (noise && !strchr(benign, c)) {
            const char *msg = strchr(printable_and_not_benign, c)
                ? _("file name '%s' contains byte '%c'")
                : _("file name '%s' contains byte '\\%o'");
            warning(msg, name, c);
        }
        if (c == '/') {
            if (!componentcheck(name, component, cp))
                return false;
            component = cp + 1;
        }
        cp++;
    }
    return componentcheck(name, component, cp);
}

/* Return a random uint_fast64_t.  */
static uint_fast64_t get_rand_u64(void)
{
#if HAVE_GETRANDOM
    static uint_fast64_t entropy_buffer[256 / sizeof(uint_fast64_t) > 1 ? 256 / sizeof(uint_fast64_t) : 1];
    static int nwords = 0;
    if (nwords == 0) {
        ssize_t s;
        do {
            s = getrandom(entropy_buffer, sizeof entropy_buffer, 0);
        } while (s < 0 && errno == EINTR);

        if (s < 0) {
            nwords = -1;
        } else {
            nwords = (int)(s / (ssize_t)sizeof *entropy_buffer);
        }
    }
    if (nwords > 0) {
        return entropy_buffer[--nwords];
    }
#endif

    {
        static bool initialized = false;
        if (!initialized) {
            srand((unsigned int)time(NULL));
            initialized = true;
        }
    }

    {
        uint_fast64_t rand_max = (uint_fast64_t)RAND_MAX;
        uint_fast64_t nrand = rand_max < UINT_FAST64_MAX ? rand_max + 1 : 0;
        uint_fast64_t rmod = INT_MAX < UINT_FAST64_MAX ? 0 : UINT_FAST64_MAX / nrand + 1;
        uint_fast64_t r = 0;
        uint_fast64_t rmax = 0;

        do {
            uint_fast64_t rmax1 = rmax;
            if (rmod) {
                rmax1 %= rmod;
                r %= rmod;
            }
            rmax1 = nrand * rmax1 + rand_max;
            r = nrand * r + (uint_fast64_t)rand();
            rmax = rmax1 < UINT_FAST64_MAX ? rmax1 : UINT_FAST64_MAX;
        } while (rmax < UINT_FAST64_MAX);

        return r;
    }
}

/* Generate a randomish name in the same directory as *NAME.  If
   *NAMEALLOC, put the name into *NAMEALLOC which is assumed to be
   that returned by a previous call and is thus already almost set up
   and equal to *NAME; otherwise, allocate a new name and put its
   address into both *NAMEALLOC and *NAME.  */
static void random_dirent(const char **name, char **namealloc)
{
    const char *src = *name;
    char *dst = *namealloc;
    static const char prefix[] = ".zic";
    static const char alphabet[] =
        "abcdefghijklmnopqrstuvwxyz"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "0123456789";
    enum { prefixlen = sizeof(prefix) - 1, alphabetlen = sizeof(alphabet) - 1 };
    const int suffixlen = 6;
    const char *lastslash = strrchr(src, '/');
    size_t dirlen = lastslash ? (size_t)(lastslash + 1 - src) : 0;

    uint_fast64_t base = alphabetlen;
    uint_fast64_t base__6 = 1;
    for (int j = 0; j < suffixlen; ++j)
        base__6 *= base;

    uint_fast64_t unfair_min = 0;
    if (base__6)
        unfair_min = (uint_fast64_t)((~(uintmax_t)0) - ((~(uintmax_t)0) % base__6));

    if (!dst) {
        size_t total_len = dirlen + prefixlen + suffixlen + 1;
        dst = emalloc(total_len);
        if (!dst)
            xalloc_die();
        if (dirlen)
            memcpy(dst, src, dirlen);
        memcpy(dst + dirlen, prefix, prefixlen);
        dst[dirlen + prefixlen + suffixlen] = '\0';
        *name = *namealloc = dst;
    }

    uint_fast64_t r;
    do {
        r = get_rand_u64();
    } while (r >= unfair_min);

    for (int i = 0; i < suffixlen; i++) {
        dst[dirlen + prefixlen + i] = alphabet[r % alphabetlen];
        r /= alphabetlen;
    }
}

/* Prepare to write to the file *OUTNAME, using *TEMPNAME to store the
   name of the temporary file that will eventually be renamed to
   *OUTNAME.  Assign the temporary file's name to both *OUTNAME and
   *TEMPNAME.  If *TEMPNAME is null, allocate the name of any such
   temporary file; otherwise, reuse *TEMPNAME's storage, which is
   already set up and only needs its trailing suffix updated.  */
static FILE *
open_outfile(const char **outname, char **tempname)
{
#if __STDC_VERSION__ < 201112
    static const char fopen_mode[] = "wb";
#else
    static const char fopen_mode[] = "wbx";
#endif

    FILE *fp = NULL;
    bool dirs_made = false;

    if (!*tempname) {
        random_dirent(outname, tempname);
    }

    while (1) {
        fp = fopen(*outname, fopen_mode);
        if (fp) {
            break;
        }

        int fopen_errno = errno;
        if (fopen_errno == ENOENT && !dirs_made) {
            mkdirs(*outname, true);
            dirs_made = true;
        } else if (fopen_errno == EEXIST) {
            random_dirent(outname, tempname);
        } else {
            fprintf(stderr, _("%s: Can't create %s/%s: %s\n"),
                    progname, directory, *outname, strerror(fopen_errno));
            exit(EXIT_FAILURE);
        }
    }

    return fp;
}


/* If TEMPNAME, the result is in the temporary file TEMPNAME even
   though the user wanted it in NAME, so rename TEMPNAME to NAME.
   Report an error and exit if there is trouble.  Also, free TEMPNAME.  */
static void rename_dest(char *tempname, const char *name) {
    if (!tempname) {
        return;
    }

    if (rename(tempname, name) != 0) {
        int rename_errno = errno;
        (void)remove(tempname);
        fprintf(stderr, "%s: rename to %s/%s: %s\n",
                progname, directory, name, strerror(rename_errno));
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
relname(const char *target, const char *linkname)
{
    size_t dir_len = 0, dotdots = 0, i = 0, taillen;
    char *result = NULL;
    const char *f = target;
    size_t linksize = 0;
    size_t lenslash = 0;
    ptrdiff_t dotdotetcsize;

    if (!target || !linkname)
        return NULL;

    if (*linkname == '/') {
        size_t dir_len_local = strlen(directory);
        lenslash = dir_len_local + ((dir_len_local && directory[dir_len_local - 1] != '/') ? 1 : 0);
        size_t target_len = strlen(target) + 1;
        linksize = size_sum(lenslash, target_len);
        result = emalloc(linksize);
        if (!result)
            return NULL;
        memcpy(result, directory, dir_len_local);
        if (lenslash > dir_len_local)
            result[dir_len_local] = '/';
        memcpy(result + lenslash, target, target_len);
        f = result;
    }

    for (i = 0; f[i] && f[i] == linkname[i]; i++) {
        if (f[i] == '/')
            dir_len = i + 1;
    }

    for (; linkname[i]; i++) {
        if (linkname[i] == '/' && linkname[i - 1] != '/')
            dotdots++;
    }

    taillen = strlen(f + dir_len);
    dotdotetcsize = size_sum(size_product(dotdots, 3), taillen + 1);

    if (dotdotetcsize < 1)
        return result;

    if ((linksize && (size_t)dotdotetcsize > linksize) || (!linksize && (result = emalloc(dotdotetcsize)) == NULL))
        return NULL;
    if (!result)
        result = emalloc(dotdotetcsize);
    if (!result)
        return NULL;

    for (i = 0; i < dotdots; i++)
        memcpy(result + 3 * i, "../", 3);

    memmove(result + 3 * dotdots, f + dir_len, taillen + 1);

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

static void dolink(const char *target, const char *linkname, bool staysymlink) {
    bool linkdirs_made = false;
    int link_errno = 0;
    char *tempname = NULL;
    const char *outname = linkname;
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

    while (1) {
        if (linkat(AT_FDCWD, target, AT_FDCWD, outname, AT_SYMLINK_FOLLOW) == 0) {
            break;
        }
        link_errno = errno;
        if (link_errno == EINVAL)
            link_errno = ENOTSUP;
#if HAVE_LINK
        if (link_errno == ENOTSUP &&
            (same_parent_dirs(target, outname) ||
             0 <= itssymlink(target, &targetissym))) {
            if (link(target, outname) == 0) {
                link_errno = 0;
                break;
            }
            link_errno = errno;
        }
#endif
        if (link_errno == EXDEV || link_errno == ENOTSUP)
            break;

        if (link_errno == EEXIST) {
            staysymlink &= !tempname;
            random_dirent(&outname, &tempname);
            if (staysymlink && itssymlink(linkname, &linknameissym))
                break;
        } else if (link_errno == ENOENT && !linkdirs_made) {
            mkdirs(linkname, true);
            linkdirs_made = true;
        } else {
            fprintf(stderr, _("%s: Can't link %s/%s to %s/%s: %s\n"),
                    progname, directory, target, directory, outname,
                    strerror(link_errno));
            exit(EXIT_FAILURE);
        }
    }

    if (link_errno != 0) {
        bool absolute = *target == '/';
        char *linkalloc = NULL;
        const char *contents = target;
        if (!absolute) {
            linkalloc = relname(target, linkname);
            if (!linkalloc) {
                fprintf(stderr, _("%s: Error allocating memory for symlink target\n"), progname);
                exit(EXIT_FAILURE);
            }
            contents = linkalloc;
        }

        int symlink_errno = 0;
        while (1) {
            if (symlink(contents, outname) == 0) {
                break;
            }
            symlink_errno = errno;
            if (symlink_errno == EEXIST) {
                random_dirent(&outname, &tempname);
            } else if (symlink_errno == ENOENT && !linkdirs_made) {
                mkdirs(linkname, true);
                linkdirs_made = true;
            } else {
                break;
            }
        }
        free(linkalloc);

        if (symlink_errno == 0) {
            if (link_errno != ENOTSUP && link_errno != EEXIST)
                warning(_("symbolic link used because hard link failed: %s"),
                        strerror(link_errno));
        } else {
            FILE *fp = fopen(target, "rb");
            if (!fp) {
                fprintf(stderr, _("%s: Can't read %s/%s: %s\n"),
                        progname, directory, target, strerror(errno));
                exit(EXIT_FAILURE);
            }
            FILE *tp = open_outfile(&outname, &tempname);
            int c;
            while ((c = getc(fp)) != EOF) {
                if (putc(c, tp) == EOF) {
                    close_file(tp, directory, linkname, tempname);
                    close_file(fp, directory, target, NULL);
                    fprintf(stderr, _("%s: Write error while copying %s to %s\n"),
                            progname, target, linkname);
                    exit(EXIT_FAILURE);
                }
            }
            if (ferror(fp)) {
                close_file(tp, directory, linkname, tempname);
                close_file(fp, directory, target, NULL);
                fprintf(stderr, _("%s: Read error while copying %s\n"),
                        progname, target);
                exit(EXIT_FAILURE);
            }
            close_file(tp, directory, linkname, tempname);
            close_file(fp, directory, target, NULL);
            if (link_errno != ENOTSUP)
                warning(_("copy used because hard link failed: %s"),
                        strerror(link_errno));
            else if (symlink_errno != ENOTSUP)
                warning(_("copy used because symbolic link failed: %s"),
                        strerror(symlink_errno));
        }
    }
    rename_dest(tempname, linkname);
}


/* Return 1 if NAME is an absolute symbolic link, -1 if it is relative,
   0 if it is not a symbolic link.  If *CACHE is not -2, it is the
   cached result of a previous call to this function with the same NAME.  */
static int itssymlink(const char *name, int *cache) {
    if (*cache != -2) {
        return *cache;
    }

    char c = '\0';
    ssize_t len = readlink(name, &c, 1);
    if (len < 0) {
        *cache = 0;
    } else if (c == '/') {
        *cache = 1;
    } else {
        *cache = -1;
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
    if (cp1 == NULL || cp2 == NULL) {
        return 0;
    }
    const struct rule *r1 = (const struct rule *)cp1;
    const struct rule *r2 = (const struct rule *)cp2;

    if (r1->r_name == NULL && r2->r_name == NULL) {
        return 0;
    }
    if (r1->r_name == NULL) {
        return -1;
    }
    if (r2->r_name == NULL) {
        return 1;
    }
    return strcmp(r1->r_name, r2->r_name);
}

static void associate(void)
{
    ptrdiff_t i, j, base, out;
    if (nrules > 1) {
        qsort(rules, nrules, sizeof *rules, rcomp);
        for (i = 0; i < nrules - 1;) {
            int duplicate_found = 0;
            if (strcmp(rules[i].r_name, rules[i + 1].r_name) == 0 &&
                rules[i].r_filenum != rules[i + 1].r_filenum) {
                eat(rules[i].r_filenum, rules[i].r_linenum);
                warning(_("same rule name in multiple files"));
                eat(rules[i + 1].r_filenum, rules[i + 1].r_linenum);
                warning(_("same rule name in multiple files"));
                for (j = i + 2; j < nrules; ++j) {
                    if (strcmp(rules[i].r_name, rules[j].r_name) != 0)
                        break;
                    if (rules[i].r_filenum == rules[j].r_filenum)
                        continue;
                    if (rules[i + 1].r_filenum == rules[j].r_filenum)
                        continue;
                    break;
                }
                i = j;
                duplicate_found = 1;
            }
            if (!duplicate_found)
                ++i;
        }
    }
    for (i = 0; i < nzones; ++i) {
        struct zone *zp = &zones[i];
        zp->z_rules = NULL;
        zp->z_nrules = 0;
    }
    for (base = 0; base < nrules; base = out) {
        struct rule *rp = &rules[base];
        for (out = base + 1; out < nrules; ++out) {
            if (strcmp(rp->r_name, rules[out].r_name) != 0)
                break;
        }
        for (i = 0; i < nzones; ++i) {
            struct zone *zp = &zones[i];
            if (strcmp(zp->z_rule, rp->r_name) != 0)
                continue;
            zp->z_rules = rp;
            zp->z_nrules = out - base;
        }
    }
    for (i = 0; i < nzones; ++i) {
        struct zone *zp = &zones[i];
        if (zp->z_nrules == 0) {
            eat(zp->z_filenum, zp->z_linenum);
            zp->z_save = getsave(zp->z_rule, &zp->z_isdst);
            if (zp->z_format_specifier == 's')
                error("%s", _("%s in ruleless zone"));
        }
    }
    if (errors)
        exit(EXIT_FAILURE);
}

/* Read a text line from FP into BUF, which is of size BUFSIZE.
   Terminate it with a NUL byte instead of a newline.
   Return true if successful, false if EOF.
   On error, report the error and exit.  */
static bool inputline(FILE *fp, char *buf, ptrdiff_t bufsize)
{
    ptrdiff_t linelen = 0;
    int ch;

    if (!fp || !buf || bufsize <= 1) {
        error(_("invalid arguments to inputline"));
        exit(EXIT_FAILURE);
    }

    while (1) {
        ch = getc(fp);

        if (ch == EOF) {
            if (ferror(fp)) {
                error(_("input error"));
                exit(EXIT_FAILURE);
            }
            if (linelen == 0)
                return false;
            error(_("unterminated line"));
            exit(EXIT_FAILURE);
        }

        if (ch == '\n')
            break;

        if (ch == 0) {
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

static void infile(int fnum, const char *name) {
    FILE *fp = NULL;
    const struct lookup *lp = NULL;
    bool wantcont = false;
    lineno num;

    if (strcmp(name, "-") == 0) {
        fp = stdin;
    } else {
        fp = fopen(name, "r");
        if (fp == NULL) {
            fprintf(stderr, _("%s: Can't open %s: %s\n"), progname, name, strerror(errno));
            exit(EXIT_FAILURE);
        }
    }

    wantcont = false;
    for (num = 1; ; ++num) {
        enum { bufsize_bound = (min(INT_MAX, INDEX_MAX) / FORMAT_LEN_GROWTH_BOUND) };
        char buf[min(_POSIX2_LINE_MAX, bufsize_bound)];
        char *fields[MAX_FIELDS];
        int nfields;

        eat(fnum, num);

        if (!inputline(fp, buf, sizeof(buf))) break;

        nfields = getfields(buf, fields, (int)(sizeof(fields) / sizeof(fields[0])));

        if (nfields == 0) {
            continue;
        }

        if (wantcont) {
            wantcont = inzcont(fields, nfields);
            continue;
        }

        const struct lookup *line_codes = (fnum < 0) ? leap_line_codes : zi_line_codes;
        lp = byword(fields[0], line_codes);
        if (lp == NULL) {
            error(_("input line of unknown type"));
            continue;
        }

        switch(lp->l_value) {
            case LC_RULE:
                inrule(fields, nfields);
                wantcont = false;
                break;
            case LC_ZONE:
                wantcont = inzone(fields, nfields);
                break;
            case LC_LINK:
                inlink(fields, nfields);
                wantcont = false;
                break;
            case LC_LEAP:
                inleap(fields, nfields);
                wantcont = false;
                break;
            case LC_EXPIRES:
                inexpires(fields, nfields);
                wantcont = false;
                break;
            default:
                unreachable();
        }
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

static zic_t gethms(const char *string, const char *errstring) {
    zic_t hh = 0;
    int sign = 1, mm = 0, ss = 0;
    char hhx = '\0', mmx = '\0', ssx = '\0', xr = '0', xs = '\0';
    int tenths = 0;
    bool ok = true;

    if (!string || !*string)
        return 0;

    if (*string == '-') {
        sign = -1;
        ++string;
    }

    int n = sscanf(string,
        "%" SCNdZIC "%c%d%c%d%c%1d%*[0]%c%*[0123456789]%c",
        &hh, &hhx, &mm, &mmx, &ss, &ssx, &tenths, &xr, &xs);

    switch (n) {
        default:
            ok = false;
            break;
        case 8:
            ok = ('0' <= xr && xr <= '9');
            // fall through
        case 7:
            ok = ok && (ssx == '.');
            if (ok && noise)
                warning(_("fractional seconds rejected by pre-2018 versions of zic"));
            // fall through
        case 5:
            ok = ok && (mmx == ':');
            // fall through
        case 3:
            ok = ok && (hhx == ':');
            // fall through
        case 1:
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

    if ((5 + ((ss ^ 1) & (xr == '0'))) <= tenths)
        ss++;
    if (noise && (hh > HOURSPERDAY ||
        (hh == HOURSPERDAY && (mm != 0 || ss != 0))))
        warning(_("values over 24 hours not handled by pre-2007 versions of zic"));

    zic_t value = (zic_t)sign * ((hh * SECSPERHOUR) + (mm * SECSPERMIN + ss));
    return oadd(0, value);
}

static zic_t
getsave(char *field, bool *isdst)
{
    int dst = -1;
    zic_t save;
    size_t fieldlen;

    if (!field || !isdst)
        return 0;

    fieldlen = strlen(field);
    if (fieldlen > 0) {
        char last_char = field[fieldlen - 1];
        if (last_char == 'd' || last_char == 's') {
            dst = (last_char == 'd') ? 1 : 0;
            field[fieldlen - 1] = '\0';
        }
    }

    save = gethms(field, _("invalid saved time"));
    *isdst = (dst < 0) ? (save != 0) : (dst != 0);

    return save;
}

static void inrule(char **fields, int nfields)
{
    struct rule r;
    size_t abbrvar_len;

    if (nfields != RULE_FIELDS) {
        error(_("wrong number of fields on Rule line"));
        return;
    }

    if (!fields[RF_NAME] || fields[RF_NAME][0] == '\0' ||
        strchr(" \f\n\r\t\v+-0123456789", fields[RF_NAME][0])) {
        error(_("Invalid rule name \"%s\""), fields[RF_NAME]);
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

    r.r_name = estrdup(fields[RF_NAME]);
    r.r_abbrvar = estrdup(fields[RF_ABBRVAR]);
    abbrvar_len = strlen(r.r_abbrvar);
    if (abbrvar_len > max_abbrvar_len)
        max_abbrvar_len = abbrvar_len;

    rules = growalloc(rules, sizeof *rules, nrules, &nrules_alloc);
    if (!rules) {
        error(_("Memory allocation failed for rules"));
        free(r.r_name);
        free(r.r_abbrvar);
        return;
    }

    rules[nrules++] = r;
}

static bool inzone(char **fields, int nfields) {
    if (nfields < ZONE_MINFIELDS || nfields > ZONE_MAXFIELDS) {
        error(_("wrong number of fields on Zone line"));
        return false;
    }
    if (lcltime && strcmp(fields[ZF_NAME], tzdefault) == 0) {
        error(_("\"Zone %s\" line and -l option are mutually exclusive"), tzdefault);
        return false;
    }
    if (strcmp(fields[ZF_NAME], TZDEFRULES) == 0 && psxrules) {
        error(_("\"Zone %s\" line and -p option are mutually exclusive"), TZDEFRULES);
        return false;
    }
    for (ptrdiff_t i = 0; i < nzones; ++i) {
        if (zones[i].z_name && strcmp(zones[i].z_name, fields[ZF_NAME]) == 0) {
            error(_("duplicate zone name %s (file \"%s\", line %"PRIdMAX")"),
                  fields[ZF_NAME], filename(zones[i].z_filenum), zones[i].z_linenum);
            return false;
        }
    }
    return inzsub(fields, nfields, false);
}

static bool inzcont(char **fields, int nfields)
{
	if (nfields < ZONEC_MINFIELDS || nfields > ZONEC_MAXFIELDS) {
		error(_("wrong number of fields on Zone continuation line"));
		return false;
	}
	return inzsub(fields, nfields, true);
}

static bool inzsub(char **fields, int nfields, bool iscont) {
    char *cp;
    char *cp1;
    struct zone z;
    int format_len;
    int i_stdoff, i_rule, i_format;
    int i_untilyear, i_untilmonth, i_untilday, i_untiltime;
    bool hasuntil;

    if (iscont) {
        i_stdoff = ZFC_STDOFF;
        i_rule = ZFC_RULE;
        i_format = ZFC_FORMAT;
        i_untilyear = ZFC_TILYEAR;
        i_untilmonth = ZFC_TILMONTH;
        i_untilday = ZFC_TILDAY;
        i_untiltime = ZFC_TILTIME;
    } else {
        if (!namecheck(fields[ZF_NAME]))
            return false;
        i_stdoff = ZF_STDOFF;
        i_rule = ZF_RULE;
        i_format = ZF_FORMAT;
        i_untilyear = ZF_TILYEAR;
        i_untilmonth = ZF_TILMONTH;
        i_untilday = ZF_TILDAY;
        i_untiltime = ZF_TILTIME;
    }

    z.z_filenum = filenum;
    z.z_linenum = linenum;

    z.z_stdoff = gethms(fields[i_stdoff], _("invalid UT offset"));

    cp = strchr(fields[i_format], '%');
    if (cp) {
        cp++;
        if ((*cp != 's' && *cp != 'z') || strchr(cp, '%') || strchr(fields[i_format], '/')) {
            error(_("invalid abbreviation format"));
            return false;
        }
    }
    z.z_format_specifier = cp ? *cp : '\0';

    format_len = strlen(fields[i_format]);
    if (max_format_len < format_len)
        max_format_len = format_len;

    hasuntil = nfields > i_untilyear;
    if (hasuntil) {
        z.z_untilrule.r_filenum = filenum;
        z.z_untilrule.r_linenum = linenum;
        if (!rulesub(
                &z.z_untilrule,
                fields[i_untilyear],
                "only",
                "",
                (nfields > i_untilmonth) ? fields[i_untilmonth] : "Jan",
                (nfields > i_untilday) ? fields[i_untilday] : "1",
                (nfields > i_untiltime) ? fields[i_untiltime] : "0"))
            return false;
        z.z_untiltime = rpytime(&z.z_untilrule, z.z_untilrule.r_loyear);
        if (iscont && nzones > 0 &&
            z.z_untiltime > min_time && z.z_untiltime < max_time &&
            zones[nzones - 1].z_untiltime > min_time && zones[nzones - 1].z_untiltime < max_time &&
            zones[nzones - 1].z_untiltime >= z.z_untiltime) {
            error(_("Zone continuation line end time is not after end time of previous line"));
            return false;
        }
    }

    z.z_name = iscont ? NULL : estrdup(fields[ZF_NAME]);
    z.z_rule = estrdup(fields[i_rule]);
    z.z_format = cp1 = estrdup(fields[i_format]);
    if (z.z_format_specifier == 'z') {
        size_t spec_pos = (size_t)(cp - fields[i_format]);
        cp1[spec_pos] = 's';
        if (noise)
            warning(_("format '%s' not handled by pre-2015 versions of zic"), fields[i_format]);
    }

    zones = growalloc(zones, sizeof *zones, nzones, &nzones_alloc);
    zones[nzones++] = z;

    return hasuntil;
}


static zic_t getleapdatetime(char **fields, bool expire_line) {
    const char *cp;
    const struct lookup *lp;
    zic_t year;
    int month, day;
    zic_t dayoff = 0, tod;
    zic_t t;
    char xs;

    cp = fields[LP_YEAR];
    if (sscanf(cp, "%"SCNdZIC"%c", &year, &xs) != 1) {
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

    for (zic_t j = EPOCH_YEAR; j != year;) {
        if (year > j) {
            dayoff = oadd(dayoff, len_years[isleap(j)]);
            ++j;
        } else {
            --j;
            dayoff = oadd(dayoff, -len_years[isleap(j)]);
        }
    }

    lp = byword(fields[LP_MONTH], mon_names);
    if (!lp) {
        error(_("invalid month name"));
        return -1;
    }
    month = lp->l_value;

    for (int jm = TM_JANUARY; jm != month; ++jm)
        dayoff = oadd(dayoff, len_months[isleap(year)][jm]);

    cp = fields[LP_DAY];
    if (sscanf(cp, "%d%c", &day, &xs) != 1 || day <= 0 || day > len_months[isleap(year)][month]) {
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
    tod = gethms(fields[LP_TIME], _("invalid time of day"));
    t = tadd(t, tod);
    if (t < 0) {
        error(_("leap second precedes Epoch"));
        return -1;
    }
    return t;
}

static void inleap(char **fields, int nfields) {
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

    int correction = 0;
    if (!fields[LP_CORR][0]) {
        correction = -1;
    } else if (strcmp(fields[LP_CORR], "+") == 0) {
        correction = 1;
    } else {
        error(_("invalid CORRECTION field on Leap line"));
        return;
    }

    if (correction) {
        leapadd(t, correction, lp->l_value);
    }
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

static void inlink(char **fields, int nfields) {
    struct link l;

    if (nfields != LINK_FIELDS) {
        error(_("wrong number of fields on Link line"));
        return;
    }

    if (!fields[LF_TARGET] || fields[LF_TARGET][0] == '\0') {
        error(_("blank TARGET field on Link line"));
        return;
    }

    if (!fields[LF_LINKNAME] || !namecheck(fields[LF_LINKNAME])) {
        return;
    }

    l.l_filenum = filenum;
    l.l_linenum = linenum;

    l.l_target = estrdup(fields[LF_TARGET]);
    if (!l.l_target) {
        error(_("memory allocation failed for TARGET"));
        return;
    }

    l.l_linkname = estrdup(fields[LF_LINKNAME]);
    if (!l.l_linkname) {
        free(l.l_target);
        error(_("memory allocation failed for LINKNAME"));
        return;
    }

    struct link *new_links = growalloc(links, sizeof *links, nlinks, &nlinks_alloc);
    if (!new_links) {
        free(l.l_target);
        free(l.l_linkname);
        error(_("memory allocation failed for links"));
        return;
    }

    links = new_links;
    links[nlinks++] = l;
}

static bool rulesub(struct rule *rp, const char *loyearp, const char *hiyearp,
	const char *typep, const char *monthp, const char *dayp,
	const char *timep)
{
	const struct lookup *lp;
	const char *cp;
	char *dp;
	char *ep;
	char xs;

	lp = byword(monthp, mon_names);
	if (lp == NULL) {
		error(_("invalid month name"));
		return false;
	}
	rp->r_month = lp->l_value;
	rp->r_todisstd = false;
	rp->r_todisut = false;

	dp = estrdup(timep);
	if (!dp) {
		error(_("memory allocation failed"));
		return false;
	}

	if (*dp != '\0') {
		ep = dp + strlen(dp) - 1;
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
		}
	}

	rp->r_tod = gethms(dp, _("invalid time of day"));
	free(dp);

	cp = loyearp;
	lp = byword(cp, begin_years);
	if (lp && lp->l_value == YR_MINIMUM) {
		warning(_("FROM year \"%s\" is obsolete; treated as %d"),
			cp, YEAR_32BIT_MIN - 1);
		rp->r_loyear = YEAR_32BIT_MIN - 1;
	} else if (!lp) {
		if (sscanf(cp, "%"SCNdZIC"%c", &rp->r_loyear, &xs) != 1) {
			error(_("invalid starting year"));
			return false;
		}
	} else {
		error(_("unexpected FROM year value"));
		return false;
	}

	cp = hiyearp;
	lp = byword(cp, end_years);
	rp->r_hiwasnum = lp == NULL;

	if (!rp->r_hiwasnum) {
		if (lp->l_value == YR_MAXIMUM) {
			rp->r_hiyear = ZIC_MAX;
		} else if (lp->l_value == YR_ONLY) {
			rp->r_hiyear = rp->r_loyear;
		} else {
			error(_("unexpected TO year value"));
			return false;
		}
	} else {
		if (sscanf(cp, "%"SCNdZIC"%c", &rp->r_hiyear, &xs) != 1) {
			error(_("invalid ending year"));
			return false;
		}
	}

	if (rp->r_loyear > rp->r_hiyear) {
		error(_("starting year greater than ending year"));
		return false;
	}

	if (*typep != '\0') {
		error(_("year type \"%s\" is unsupported; use \"-\" instead"), typep);
		return false;
	}

	dp = estrdup(dayp);
	if (!dp) {
		error(_("memory allocation failed"));
		return false;
	}

	lp = byword(dp, lasts);
	if (lp) {
		rp->r_dycode = DC_DOWLEQ;
		rp->r_wday = lp->l_value;
		rp->r_dayofmonth = len_months[1][rp->r_month];
	} else {
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
			lp = byword(dp, wday_names);
			if (!lp) {
				error(_("invalid weekday name"));
				free(dp);
				return false;
			}
			rp->r_wday = lp->l_value;
		}
		if (sscanf(ep, "%d%c", &rp->r_dayofmonth, &xs) != 1 ||
		    rp->r_dayofmonth <= 0 ||
		    rp->r_dayofmonth > len_months[1][rp->r_month]) {
			error(_("invalid day of month"));
			free(dp);
			return false;
		}
	}

	free(dp);
	return true;
}

static void convert(uint_fast32_t val, char *buf) {
    if (buf == NULL) {
        return;
    }
    unsigned char *b = (unsigned char *)buf;
    for (int i = 0; i < 4; ++i) {
        int shift = 24 - (i * 8);
        b[i] = (unsigned char)((val >> shift) & 0xFF);
    }
}

static void convert64(uint_fast64_t val, char *buf) {
    if (buf == NULL) {
        return;
    }
    for (int i = 0; i < 8; ++i) {
        buf[i] = (char)((val >> (56 - 8 * i)) & 0xFF);
    }
}

static void puttzcode(zic_t val, FILE *fp)
{
    char buf[4];
    if (!fp) {
        return;
    }
    convert(val, buf);
    if (fwrite(buf, sizeof buf, 1, fp) != 1) {
        // Handle error if needed
    }
}

static void puttzcodepass(zic_t val, FILE *fp, int pass) {
    if (pass == 1) {
        puttzcode(val, fp);
        return;
    }
    char buf[8];
    convert64(val, buf);
    if (fwrite(buf, sizeof buf, 1, fp) != 1) {
        /* Handle error as appropriate for your project, e.g., abort or log */
    }
}

static int atcomp(const void *avp, const void *bvp)
{
    const struct attype *ap = (const struct attype *)avp;
    const struct attype *bp = (const struct attype *)bvp;
    if (ap->at < bp->at)
        return -1;
    else if (ap->at > bp->at)
        return 1;
    return 0;
}

struct timerange {
  int defaulttype;
  ptrdiff_t base, count;
  int leapbase, leapcount;
  bool leapexpiry;
};

static struct timerange limitrange(struct timerange r, zic_t lo, zic_t hi, zic_t const *ats, const unsigned char *types) {
    /* Remove transitions before lo */
    while (r.count > 0 && ats[r.base] < lo) {
        r.defaulttype = types[r.base];
        r.count--;
        r.base++;
    }

    /* Handle leap seconds */
    while (r.leapcount > 1 && trans[r.leapbase + 1] <= lo) {
        r.leapcount--;
        r.leapbase++;
    }
    while (r.leapbase > 0 &&
           ((corr[r.leapbase - 1] < corr[r.leapbase]) != (corr[r.leapbase] > 0))) {
        r.leapcount++;
        r.leapbase--;
    }

    /* Remove transitions after hi + 1 */
    if (hi < max_time) {
        while (r.count > 0 && ats[r.base + r.count - 1] > hi + 1)
            r.count--;
        while (r.leapcount > 0 && trans[r.leapbase + r.leapcount - 1] > hi + 1)
            r.leapcount--;
    }

    /* Set leap expiry */
    r.leapexpiry = (leapexpires >= 0 && leapexpires - 1 <= hi);

    return r;
}

static void writezone(const char *const name, const char *const string, char version, int defaulttype) {
    FILE *fp = NULL;
    ptrdiff_t i, j;
    int pass;
    char *tempname = NULL;
    const char *outname = name;

    size_t ats_types_size = align_to(size_product(timecnt + !timecnt, sizeof(zic_t) + 1), alignof(zic_t));
    zic_t *ats = emalloc(ats_types_size);
    if (!ats) {
        error(_("memory allocation failure"));
        return;
    }
    unsigned char *types = (unsigned char *)(ats + timecnt);

    struct timerange rangeall = {0}, range32, range64;

    if (timecnt > 1)
        qsort(attypes, timecnt, sizeof *attypes, atcomp);

    {   // Optimize transitions
        ptrdiff_t fromi, toi = 0;
        for (fromi = 0; fromi < timecnt; ++fromi) {
            if (toi != 0 &&
                (attypes[fromi].at + utoffs[attypes[toi - 1].type]) <= 
                (attypes[toi - 1].at + utoffs[(toi == 1 ? 0 : attypes[toi - 2].type)])) {
                attypes[toi - 1].type = attypes[fromi].type;
                continue;
            }
            if (toi == 0 ||
                attypes[fromi].dontmerge ||
                (utoffs[attypes[toi - 1].type] != utoffs[attypes[fromi].type]) ||
                (isdsts[attypes[toi - 1].type] != isdsts[attypes[fromi].type]) ||
                (desigidx[attypes[toi - 1].type] != desigidx[attypes[fromi].type])) {
                attypes[toi++] = attypes[fromi];
            }
        }
        timecnt = toi;
    }

    if (noise && timecnt > 1200) {
        if (timecnt > TZ_MAX_TIMES)
            warning(_("reference clients mishandle more than %d transition times"), TZ_MAX_TIMES);
        else
            warning(_("pre-2014 clients may mishandle more than 1200 transition times"));
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
    range64 = limitrange(rangeall, lo_time, max(hi_time, redundant_time - (ZIC_MIN < redundant_time)), ats, types);
    range32 = limitrange(range64, ZIC32_MIN, ZIC32_MAX, ats, types);

    for (pass = 1; pass <= 2; pass++) {
        struct timerange const *r = pass == 1 ? &range32 : &range64;
        if (pass == 1 && !want_bloat()) continue;
        if (r->leapexpiry) {
            if (noise)
                warning(_("%s: pre-2021b clients may mishandle leap second expiry"), name);
            version = '4';
        }
        if (0 < r->leapcount && corr[r->leapbase] != 1 && corr[r->leapbase] != -1) {
            if (noise)
                warning(_("%s: pre-2021b clients may mishandle leap second table truncation"), name);
            version = '4';
        }
        if (version == '4')
            break;
    }

    fp = open_outfile(&outname, &tempname);
    if (!fp) {
        free(ats);
        return;
    }

    for (pass = 1; pass <= 2; ++pass) {
        ptrdiff_t thistimei, thistimecnt, thistimelim;
        int thisleapi, thisleapcnt, thisleaplim;
        struct tzhead tzh;
        int pretranstype = -1, thisdefaulttype, old0;
        bool locut, hicut, thisleapexpiry;
        zic_t lo, thismin, thismax;
        char omittype[TZ_MAX_TYPES] = {0};
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
            close_file(fp, directory, name, tempname);
            rename_dest(tempname, name);
            free(ats);
            return;
        }

        locut = thismin < lo_time && lo_time <= thismax;
        hicut = thismin <= hi_time && hi_time < thismax;
        thistimelim = thistimei + thistimecnt;

        memset(omittype, 1, typecnt);

        if ((locut || (pass == 1 && thistimei)) && !(thistimecnt && ats[thistimei] == lo_time)) {
            pretranstype = thisdefaulttype;
            omittype[pretranstype] = 0;
        }

        if (pass == 1 && lo_time <= thismin)
            thisdefaulttype = range64.defaulttype;

        if (locut)
            thisdefaulttype = unspecifiedtype;
        omittype[thisdefaulttype] = 0;
        for (i = thistimei; i < thistimelim; i++)
            omittype[types[i]] = 0;
        if (hicut)
            omittype[unspecifiedtype] = 0;

        old0 = strlen(omittype);

#ifndef LEAVE_SOME_PRE_2011_SYSTEMS_IN_THE_LURCH
        if (want_bloat()) {
            int mrudst = -1, mrustd = -1, hidst = -1, histd = -1, type;
            if (0 <= pretranstype) {
                if (isdsts[pretranstype]) mrudst = pretranstype;
                else mrustd = pretranstype;
            }
            for (i = thistimei; i < thistimelim; i++) {
                if (isdsts[types[i]]) mrudst = types[i];
                else mrustd = types[i];
            }
            for (i = old0; i < typecnt; i++) {
                int h = (i == old0 ? thisdefaulttype : i == thisdefaulttype ? old0 : i);
                if (!omittype[h]) {
                    if (isdsts[h]) hidst = i;
                    else histd = i;
                }
            }
            if (hidst >= 0 && mrudst >= 0 && hidst != mrudst && utoffs[hidst] != utoffs[mrudst]) {
                isdsts[mrudst] = -1;
                type = addtype(utoffs[mrudst], &chars[desigidx[mrudst]], true, ttisstds[mrudst], ttisuts[mrudst]);
                isdsts[mrudst] = 1;
                omittype[type] = 0;
            }
            if (histd >= 0 && mrustd >= 0 && histd != mrustd && utoffs[histd] != utoffs[mrustd]) {
                isdsts[mrustd] = -1;
                type = addtype(utoffs[mrustd], &chars[desigidx[mrustd]], false, ttisstds[mrustd], ttisuts[mrustd]);
                isdsts[mrustd] = 0;
                omittype[type] = 0;
            }
        }
#endif
        thistypecnt = 0;
        for (i = old0; i < typecnt; i++)
            if (!omittype[i])
                typemap[i == old0 ? thisdefaulttype : i == thisdefaulttype ? old0 : i] = thistypecnt++;

        for (i = 0; i < (int)(sizeof indmap / sizeof indmap[0]); ++i)
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
                strcpy(&thischars[thischarcnt], thisabbr);
                thischarcnt += strlen(thisabbr) + 1;
            }
            indmap[desigidx[i]] = j;
        }
        if (pass == 1 && !want_bloat()) {
            hicut = thisleapexpiry = false;
            pretranstype = -1;
            thistimecnt = thisleapcnt = 0;
            thistypecnt = thischarcnt = 1;
        }
        #define DO(field) fwrite(tzh.field, sizeof tzh.field, 1, fp)
        memset(&tzh, 0, sizeof tzh);
        memcpy(tzh.tzh_magic, TZ_MAGIC, sizeof tzh.tzh_magic);
        tzh.tzh_version[0] = version;
        convert(utcnt, tzh.tzh_ttisutcnt);
        convert(stdcnt, tzh.tzh_ttisstdcnt);
        convert(thisleapcnt + thisleapexpiry, tzh.tzh_leapcnt);
        convert((0 <= pretranstype) + thistimecnt + hicut, tzh.tzh_timecnt);
        convert(thistypecnt, tzh.tzh_typecnt);
        convert(thischarcnt, tzh.tzh_charcnt);
        DO(tzh_magic); DO(tzh_version); DO(tzh_reserved);
        DO(tzh_ttisutcnt); DO(tzh_ttisstdcnt); DO(tzh_leapcnt);
        DO(tzh_timecnt); DO(tzh_typecnt); DO(tzh_charcnt);
        #undef DO

        if (pass == 1 && !want_bloat()) {
            puttzcode(0, fp);
            putc(0, fp);
            putc(0, fp);
            putc(0, fp);
            continue;
        }

        lo = pass == 1 && lo_time < ZIC32_MIN ? ZIC32_MIN : lo_time;

        if (0 <= pretranstype)
            puttzcodepass(lo, fp, pass);
        for (i = thistimei; i < thistimelim; ++i)
            puttzcodepass(ats[i], fp, pass);
        if (hicut)
            puttzcodepass(hi_time + 1, fp, pass);
        if (0 <= pretranstype)
            putc(typemap[pretranstype], fp);
        for (i = thistimei; i < thistimelim; i++)
            putc(typemap[types[i]], fp);
        if (hicut)
            putc(typemap[unspecifiedtype], fp);

        for (i = old0; i < typecnt; i++) {
            int h = (i == old0 ? thisdefaulttype : i == thisdefaulttype ? old0 : i);
            if (!omittype[h]) {
                puttzcode(utoffs[h], fp);
                putc(isdsts[h], fp);
                putc(indmap[desigidx[h]], fp);
            }
        }
        if (thischarcnt != 0)
            fwrite(thischars, sizeof thischars[0], thischarcnt, fp);

        thisleaplim = thisleapi + thisleapcnt;
        for (i = thisleapi; i < thisleaplim; ++i) {
            zic_t todo;
            if (roll[i]) {
                if (timecnt == 0 || trans[i] < ats[0]) {
                    j = 0;
                    while (isdsts[j])
                        if (++j >= typecnt) { j = 0; break; }
                } else {
                    j = 1;
                    while (j < timecnt && trans[i] >= ats[j])
                        ++j;
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
            puttzcode(thisleaplim ? corr[thisleaplim - 1] : 0, fp);
        }
        if (stdcnt != 0)
            for (i = old0; i < typecnt; i++)
                if (!omittype[i])
                    putc(ttisstds[i], fp);
        if (utcnt != 0)
            for (i = old0; i < typecnt; i++)
                if (!omittype[i])
                    putc(ttisuts[i], fp);
    }

    fprintf(fp, "\n%s\n", string);
    close_file(fp, directory, name, tempname);
    rename_dest(tempname, name);
    free(ats);
}

static const char *abbroffset(char *buf, zic_t offset)
{
    char sign = '+';
    int hours, minutes, seconds;
    char *p = buf;

    if (offset < 0) {
        if (offset == ((zic_t)1 << (sizeof(zic_t) * 8 - 1))) {
            error(_("%%z UT offset magnitude is too large"));
            return "%z";
        }
        offset = -offset;
        sign = '-';
    }

    seconds = offset % SECSPERMIN;
    offset /= SECSPERMIN;
    minutes = offset % MINSPERHOUR;
    hours = offset / MINSPERHOUR;

    if (hours >= 100) {
        error(_("%%z UT offset magnitude exceeds 99:59:59"));
        return "%z";
    }

    *p++ = sign;
    *p++ = '0' + hours / 10;
    *p++ = '0' + hours % 10;
    if (minutes || seconds) {
        *p++ = '0' + minutes / 10;
        *p++ = '0' + minutes % 10;
        if (seconds) {
            *p++ = '0' + seconds / 10;
            *p++ = '0' + seconds % 10;
        }
    }
    *p = '\0';
    return buf;
}

static char const disable_percent_s[] = "";

static ptrdiff_t
doabbr(char *abbr, const struct zone *zp, const char *letters,
       bool isdst, zic_t save, bool doquotes)
{
    const char *format = zp->z_format;
    char *slashp = strchr(format, '/');
    ptrdiff_t len;

    if (!format || !abbr) {
        return 0;
    }

    if (!slashp) {
        char letterbuf[PERCENT_Z_LEN_BOUND + 1];
        const char *result_letters = letters;

        if (zp->z_format_specifier == 'z') {
            result_letters = abbroffset(letterbuf, zp->z_stdoff + save);
        } else if (!letters) {
            result_letters = "%s";
        } else if (letters == disable_percent_s) {
            return 0;
        }

        if (snprintf(abbr, PERCENT_Z_LEN_BOUND + strlen(format) + 2, format, result_letters) < 0) {
            abbr[0] = '\0';
            return 0;
        }
    } else if (isdst) {
        strncpy(abbr, slashp + 1, PERCENT_Z_LEN_BOUND);
        abbr[PERCENT_Z_LEN_BOUND] = '\0';
    } else {
        size_t lead = (size_t)(slashp - format);
        if (lead > PERCENT_Z_LEN_BOUND) {
            lead = PERCENT_Z_LEN_BOUND;
        }
        memcpy(abbr, format, lead);
        abbr[lead] = '\0';
    }

    len = (ptrdiff_t)strlen(abbr);

    if (!doquotes) {
        return len;
    }

    char *cp = abbr;
    while (*cp && is_alpha((unsigned char)*cp)) {
        cp++;
    }
    if (len > 0 && *cp == '\0') {
        return len;
    }

    if (len + 2 > PERCENT_Z_LEN_BOUND) {
        abbr[0] = '\0';
        return 0;
    }
    memmove(abbr + 1, abbr, len);
    abbr[0] = '<';
    abbr[len + 1] = '>';
    abbr[len + 2] = '\0';

    return len + 2;
}

static void updateminmax(zic_t x)
{
    if (x < min_year) {
        min_year = x;
    }
    if (x > max_year) {
        max_year = x;
    }
}

static int stringoffset(char *result, zic_t offset) {
    int hours, minutes, seconds;
    bool negative = offset < 0;
    int len = 0;

    if (!result) return 0;

    if (negative) {
        offset = -offset;
        result[len++] = '-';
    }

    seconds = (int)(offset % SECSPERMIN);
    offset /= SECSPERMIN;
    minutes = (int)(offset % MINSPERHOUR);
    offset /= MINSPERHOUR;
    hours = (int)offset;

    if (hours >= HOURSPERDAY * DAYSPERWEEK) {
        if (len > 0) result[0] = '\0';
        else result[0] = '\0';
        return 0;
    }

    len += sprintf(result + len, "%d", hours);

    if (minutes != 0 || seconds != 0) {
        len += sprintf(result + len, ":%02d", minutes);
        if (seconds != 0) {
            len += sprintf(result + len, ":%02d", seconds);
        }
    }
    return len;
}

static int stringrule(char *result, struct rule *const rp, zic_t save, zic_t stdoff) {
    zic_t tod = rp->r_tod;
    int compat = 0;

    if (rp->r_dycode == DC_DOM) {
        int month, total = 0;
        if (rp->r_dayofmonth == 29 && rp->r_month == TM_FEBRUARY)
            return -1;
        for (month = 0; month < rp->r_month; ++month)
            total += len_months[0][month];
        if (rp->r_month <= 1)
            result += sprintf(result, "%d", total + rp->r_dayofmonth - 1);
        else
            result += sprintf(result, "J%d", total + rp->r_dayofmonth);
    } else {
        int week = 0;
        int wday = rp->r_wday;
        int wdayoff = 0;

        if (rp->r_dycode == DC_DOWGEQ) {
            wdayoff = (rp->r_dayofmonth - 1) % DAYSPERWEEK;
            if (wdayoff)
                compat = 2013;
            wday -= wdayoff;
            tod += wdayoff * SECSPERDAY;
            week = 1 + (rp->r_dayofmonth - 1) / DAYSPERWEEK;
        } else if (rp->r_dycode == DC_DOWLEQ) {
            if (rp->r_dayofmonth == len_months[1][rp->r_month])
                week = 5;
            else {
                wdayoff = rp->r_dayofmonth % DAYSPERWEEK;
                if (wdayoff)
                    compat = 2013;
                wday -= wdayoff;
                tod += wdayoff * SECSPERDAY;
                week = rp->r_dayofmonth / DAYSPERWEEK;
            }
        } else {
            return -1;
        }

        if (wday < 0)
            wday += DAYSPERWEEK;
        result += sprintf(result, "M%d.%d.%d", rp->r_month + 1, week, wday);
    }

    if (rp->r_todisut)
        tod += stdoff;
    if (rp->r_todisstd && !rp->r_isdst)
        tod += save;

    if (tod != 2 * SECSPERMIN * MINSPERHOUR) {
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

    return compat;
}

static int rule_cmp(const struct rule *a, const struct rule *b)
{
    if (a == b)
        return 0;
    if (!a)
        return -1;
    if (!b)
        return 1;
    if (a->r_hiyear != b->r_hiyear)
        return (a->r_hiyear > b->r_hiyear) ? 1 : -1;
    if (a->r_hiyear == ZIC_MAX)
        return 0;
    if (a->r_month != b->r_month)
        return (a->r_month > b->r_month) ? 1 : -1;
    if (a->r_dayofmonth != b->r_dayofmonth)
        return (a->r_dayofmonth > b->r_dayofmonth) ? 1 : -1;
    return 0;
}

/* Store into RESULT a proleptic TZ string that represent the future
   predictions for the zone ZPFIRST with ZONECOUNT entries.  Return a
   compatibility indicator (a TZDB release year) if successful, a
   negative integer if no such TZ string exists.  */
static int stringzone(char *result, const struct zone *zpfirst, ptrdiff_t zonecount)
{
    const struct zone *zp;
    struct rule *stdrp = NULL;
    struct rule *dstrp = NULL;
    int compat = 0;
    int c;
    int offsetlen;
    struct rule stdr, dstr;
    ptrdiff_t len;
    int dstcmp;
    struct rule *lastrp[2] = {NULL, NULL};
    struct zone zstr[2];
    const struct zone *stdzp;
    const struct zone *dstzp;

    result[0] = '\0';

    if (hi_time < max_time)
        return -1;

    zp = zpfirst + zonecount - 1;
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
    stdrp = lastrp[0];
    dstrp = lastrp[1];
    dstcmp = zp->z_nrules ? rule_cmp(dstrp, stdrp) : (zp->z_isdst ? 1 : -1);
    stdzp = dstzp = zp;

    if (dstcmp < 0) {
        dstrp = NULL;
    } else if (dstcmp > 0) {
        zic_t save = dstrp ? dstrp->r_save : zp->z_save;
        if (save >= 0) {
            stdzp = &zstr[0];
            dstzp = &zstr[1];
            zstr[0].z_stdoff = zp->z_stdoff + 2 * save;
            zstr[0].z_format = "XXX";
            zstr[0].z_format_specifier = 0;
            zstr[1].z_stdoff = zstr[0].z_stdoff;
            zstr[1].z_format = zp->z_format;
            zstr[1].z_format_specifier = zp->z_format_specifier;
        }
        dstr.r_month = TM_JANUARY;
        dstr.r_dycode = DC_DOM;
        dstr.r_dayofmonth = 1;
        dstr.r_tod = 0;
        dstr.r_todisstd = dstr.r_todisut = false;
        dstr.r_isdst = true;
        dstr.r_save = save < 0 ? save : -save;
        dstr.r_abbrvar = dstrp ? dstrp->r_abbrvar : NULL;
        stdr.r_month = TM_DECEMBER;
        stdr.r_dycode = DC_DOM;
        stdr.r_dayofmonth = 31;
        stdr.r_tod = SECSPERDAY + dstr.r_save;
        stdr.r_todisstd = stdr.r_todisut = false;
        stdr.r_isdst = false;
        stdr.r_save = 0;
        stdr.r_abbrvar = (save < 0 && stdrp) ? stdrp->r_abbrvar : NULL;
        dstrp = &dstr;
        stdrp = &stdr;
    }

    len = doabbr(result, stdzp, stdrp ? stdrp->r_abbrvar : NULL, false, 0, true);
    offsetlen = stringoffset(result + len, -stdzp->z_stdoff);
    if (!offsetlen) {
        result[0] = '\0';
        return -1;
    }
    len += offsetlen;

    if (!dstrp)
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
    c = stringrule(result + len, dstrp, dstrp->r_save, stdzp->z_stdoff);
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
	zic_t starttime = 0, untiltime = 0, nonTZlimtime = ZIC_MIN, max_year0;
	bool startttisstd = false;
	bool startttisut = false;
	char *startbuf = NULL, *ab = NULL, *envvar = NULL;
	int max_abbr_len, max_envvar_len;
	int compat, defaulttype = -1;
	bool do_extend;
	char version;
	int nonTZlimtype = -1;
	int max_year, min_year;
	int charcnt = 0, typecnt = 0, timecnt = 0;

	check_for_signal();

	max_abbr_len = 2 + max_format_len + max_abbrvar_len;
	max_envvar_len = 2 * max_abbr_len + 5 * 9;

	startbuf = emalloc(max_abbr_len + 1);
	ab = emalloc(max_abbr_len + 1);
	envvar = emalloc(max_envvar_len + 1);

	INITIALIZE(untiltime);
	INITIALIZE(starttime);

	timecnt = 0;
	typecnt = 0;
	charcnt = 0;

	min_year = max_year = EPOCH_YEAR;
	if (leapseen) {
		updateminmax(leapminyear);
		updateminmax(leapmaxyear + (leapmaxyear < ZIC_MAX));
	}
	for (i = 0; i < zonecount; ++i) {
		const struct zone *zp = &zpfirst[i];
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
		if (!*envvar) {
			warning("%s %s", _("no proleptic TZ string for zone"), zpfirst->z_name);
		} else if (compat != 0) {
			warning(_("%s: pre-%d clients may mishandle distant timestamps"), zpfirst->z_name, compat);
		}
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
		const struct zone *zp = &zpfirst[i];
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
			type = addtype(oadd(zp->z_stdoff, save),
				startbuf, zp->z_isdst, startttisstd, startttisut);
			if (usestart) {
				addtt(starttime, type);
				if (useuntil && nonTZlimtime < starttime) {
					nonTZlimtime = starttime;
					nonTZlimtype = type;
				}
				usestart = false;
			} else
				defaulttype = type;
		} else {
			zic_t year;
			for (year = min_year; year <= max_year; ++year) {
				if (useuntil && year > zp->z_untilrule.r_hiyear)
					break;

				for (j = 0; j < zp->z_nrules; ++j) {
					zic_t one = 1;
					zic_t y2038_boundary = one << 31;
					struct rule *rp = &zp->z_rules[j];
					eats(zp->z_filenum, zp->z_linenum, rp->r_filenum, rp->r_linenum);
					rp->r_todo = year >= rp->r_loyear && year <= rp->r_hiyear;
					if (rp->r_todo) {
						rp->r_temp = rpytime(rp, year);
						rp->r_todo = (rp->r_temp < y2038_boundary || year <= max_year0);
					}
				}
				while (1) {
					ptrdiff_t k = -1;
					zic_t ktime = 0, offset = 0, jtime = 0;
					struct rule *rp = NULL;
					int type;

					INITIALIZE(ktime);
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
							const char *dup_rules_msg = _("two rules for same instant");
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
						if (*startbuf == '\0' && startoff == oadd(zp->z_stdoff, save)) {
							doabbr(startbuf, zp, rp->r_abbrvar, rp->r_isdst, rp->r_save, false);
						}
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
			if (*startbuf == '\0')
				error(_("can't determine time zone abbreviation to use just after until time"));
			else {
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
		for (i = j = 0; i < timecnt; i++) {
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


static int addtt(zic_t starttime, int type) {
	void *tmp = growalloc(attypes, sizeof *attypes, timecnt, &timecnt_alloc);
	if (!tmp) {
		return -1;
	}
	attypes = tmp;
	attypes[timecnt].at = starttime;
	attypes[timecnt].dontmerge = false;
	attypes[timecnt].type = type;
	++timecnt;
	return 0;
}

static int addtype(zic_t utoff, const char *abbr, bool isdst, bool ttisstd, bool ttisut)
{
    int i, j;

    if (utoff < (-2147483647L - 1L) || utoff > 2147483647L) {
        error(_("UT offset out of range"));
        exit(EXIT_FAILURE);
    }
    if (!want_bloat()) {
        ttisstd = false;
        ttisut = false;
    }

    for (j = 0; j < charcnt; ++j) {
        if (strcmp(&chars[j], abbr) == 0) {
            break;
        }
    }

    if (j < charcnt) {
        for (i = 0; i < typecnt; i++) {
            if (utoff == utoffs[i] &&
                isdst == isdsts[i] &&
                j == desigidx[i] &&
                ttisstd == ttisstds[i] &&
                ttisut == ttisuts[i])
                return i;
        }
    } else {
        newabbr(abbr);
    }

    if (typecnt >= TZ_MAX_TYPES) {
        error(_("too many local time types"));
        exit(EXIT_FAILURE);
    }

    i = typecnt++;
    utoffs[i] = utoff;
    isdsts[i] = isdst;
    ttisstds[i] = ttisstd;
    ttisuts[i] = ttisut;
    desigidx[i] = j;
    return i;
}

static void leapadd(zic_t t, int correction, int rolling) {
    int i;

    if (TZ_MAX_LEAPS <= leapcnt) {
        error(_("too many leap seconds"));
        exit(EXIT_FAILURE);
    }

    if (rolling && (lo_time != min_time || hi_time != max_time)) {
        error(_("Rolling leap seconds not supported with -r"));
        exit(EXIT_FAILURE);
    }

    for (i = 0; i < leapcnt; ++i) {
        if (t <= trans[i]) {
            break;
        }
    }

    if (leapcnt > i) {
        memmove(&trans[i + 1], &trans[i], (leapcnt - i) * sizeof(*trans));
        memmove(&corr[i + 1], &corr[i], (leapcnt - i) * sizeof(*corr));
        memmove(&roll[i + 1], &roll[i], (leapcnt - i) * sizeof(*roll));
    }

    trans[i] = t;
    corr[i] = correction;
    roll[i] = rolling;
    leapcnt++;
}

static void adjleap(void)
{
    int i;
    zic_t last = 0;
    zic_t prevtrans = 0;

    for (i = 0; i < leapcnt; ++i) {
        if (trans[i] - prevtrans < 28 * SECSPERDAY) {
            error(_("Leap seconds too close together"));
            exit(EXIT_FAILURE);
        }
        prevtrans = trans[i];
        trans[i] = tadd(trans[i], last);
        last = corr[i] += last;
    }

    if (leapexpires >= 0) {
        leapexpires = oadd(leapexpires, last);
        if (leapcnt > 0 && !(trans[leapcnt - 1] < leapexpires)) {
            error(_("last Leap time does not precede Expires time"));
            exit(EXIT_FAILURE);
        }
    }
}

/* Is A a space character in the C locale?  */
static bool is_space(char a) {
    return a == ' ' || a == '\f' || a == '\n' || a == '\r' || a == '\t' || a == '\v';
}

/* Is A an alphabetic character in the C locale?  */
static bool is_alpha(char a)
{
    return ((a >= 'A' && a <= 'Z') || (a >= 'a' && a <= 'z'));
}

/* If A is an uppercase character in the C locale, return its lowercase
   counterpart.  Otherwise, return A.  */
static char lowerit(char a) {
    if (a >= 'A' && a <= 'Z') {
        return (char)(a + ('a' - 'A'));
    }
    return a;
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
    if (!word || !table)
        return NULL;

    /* Handle special "last" prefix case */
    if (table == lasts && ciprefix("last", word) && word[4]) {
        if (word[4] == '-') {
            warning(_("\"%s\" is undocumented; use \"last%s\" instead"), word, word + 5);
        } else {
            word += 4;
            table = wday_names;
        }
    }

    /* Exact match */
    for (const struct lookup *lp = table; lp->l_word; ++lp) {
        if (ciequal(word, lp->l_word))
            return lp;
    }

    /* Inexact match */
    const struct lookup *foundlp = NULL;
    for (const struct lookup *lp = table; lp->l_word; ++lp) {
        if (ciprefix(word, lp->l_word)) {
            if (foundlp)
                return NULL; /* multiple inexact matches */
            foundlp = lp;
        }
    }

    if (foundlp && noise) {
        bool pre_2017c_match = false;
        for (const struct lookup *lp = table; lp->l_word; ++lp) {
            if (itsabbr(word, lp->l_word)) {
                if (pre_2017c_match) {
                    warning(_("\"%s\" is ambiguous in pre-2017c zic"), word);
                    break;
                }
                pre_2017c_match = true;
            }
        }
    }

    return foundlp;
}

static int getfields(char *cp, char **array, int arrayelts) {
    int nsubs = 0;
    while (cp && *cp) {
        while (is_space(*cp))
            ++cp;
        if (*cp == '\0' || *cp == '#')
            break;

        char *start = cp;
        char *dp = cp;
        int quoted = 0;

        if (*cp == '"') {
            quoted = 1;
            ++cp;
            while (*cp && *cp != '"') {
                *dp++ = *cp++;
            }
            if (*cp != '"') {
                error(_("Odd number of quotation marks"));
                exit(EXIT_FAILURE);
            }
            ++cp;
        } else {
            while (*cp && *cp != '#' && !is_space(*cp) && *cp != '"') {
                *dp++ = *cp++;
            }
        }
        *dp = '\0';

        while (is_space(*cp))
            ++cp;

        if (nsubs >= arrayelts) {
            error(_("Too many input fields"));
            exit(EXIT_FAILURE);
        }
        array[nsubs++] = start + (*start == '-' && (dp == start + 1));
    }
    return nsubs;
}

ATTRIBUTE_NORETURN static void
time_overflow(void)
{
  error(_("time overflow"));
  abort();
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

static zic_t rpytime(const struct rule *rp, zic_t wantedy) {
    int m, i;
    zic_t dayoff, t, y;
    int yrem;

    if (wantedy == ZIC_MIN)
        return min_time;
    if (wantedy == ZIC_MAX)
        return max_time;

    m = TM_JANUARY;
    y = EPOCH_YEAR;

    yrem = wantedy % YEARSPERREPEAT - y % YEARSPERREPEAT;
    int year_adj = wantedy / YEARSPERREPEAT - y / YEARSPERREPEAT;
    int mod_correction = yrem / YEARSPERREPEAT - (yrem % YEARSPERREPEAT < 0);
    dayoff = (zic_t)(year_adj + mod_correction) * DAYSPERREPEAT;
    wantedy = y + (yrem + 2 * YEARSPERREPEAT) % YEARSPERREPEAT;

    for (; y < wantedy; y++)
        dayoff = oadd(dayoff, len_years[isleap(y)]);
    for (; m < rp->r_month; m++)
        dayoff = oadd(dayoff, len_months[isleap(y)][m]);

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
        zic_t wday = (EPOCH_WDAY + ((dayoff % DAYSPERWEEK) + DAYSPERWEEK) % DAYSPERWEEK) % DAYSPERWEEK;
        while (wday != rp->r_wday) {
            if (rp->r_dycode == DC_DOWGEQ) {
                dayoff = oadd(dayoff, 1);
                wday = (wday + 1) % DAYSPERWEEK;
                ++i;
            } else {
                dayoff = oadd(dayoff, -1);
                wday = (wday == 0) ? DAYSPERWEEK - 1 : wday - 1;
                --i;
            }
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

    t = (zic_t)dayoff * SECSPERDAY;
    return tadd(t, rp->r_tod);
}

static void newabbr(const char *string) {
    int i;

    if (strcmp(string, GRANDPARENTED) != 0) {
        const char *cp = string;
        const char *msg = NULL;

        while (*cp && (is_alpha(*cp) || ('0' <= *cp && *cp <= '9') || *cp == '-' || *cp == '+')) {
            cp++;
        }
        if (noise) {
            if (cp - string < 3) {
                msg = _("time zone abbreviation has fewer than 3 characters");
            } else if (cp - string > ZIC_MAX_ABBR_LEN_WO_WARN) {
                msg = _("time zone abbreviation has too many characters");
            }
        }
        if (*cp != '\0') {
            msg = _("time zone abbreviation differs from POSIX standard");
        }
        if (msg != NULL) {
            warning("%s (%s)", msg, string);
        }
    }

    i = (int)strlen(string) + 1;
    if ((charcnt + i) > TZ_MAX_CHARS) {
        error(_("too many, or too long, time zone abbreviations"));
        exit(EXIT_FAILURE);
    }
    memcpy(&chars[charcnt], string, i);
    charcnt += i;
}


/* Ensure that the directories of ARGNAME exist, by making any missing
   ones.  If ANCESTORS, do this only for ARGNAME's ancestors; otherwise,
   do it for ARGNAME too.  Exit with failure if there is trouble.
   Do not consider an existing file to be trouble.  */
static void mkdirs(const char *argname, bool ancestors) {
    char *name = estrdup(argname);
    char *cp = name;

    while (*cp == '/')
        cp++;

    while (1) {
        char *sep = strchr(cp, '/');
        if (sep) {
            *sep = '\0';
        } else if (ancestors) {
            break;
        }

        if (mkdir(name, MKDIR_UMASK) != 0) {
            int err = errno;
            if (err == ELOOP || err == ENAMETOOLONG || err == ENOENT || err == ENOTDIR) {
                error(_("%s: Can't create directory %s: %s"), progname, name, strerror(err));
                free(name);
                exit(EXIT_FAILURE);
            }
        }

        if (!sep)
            break;
        *sep = '/';
        cp = sep + 1;
        while (*cp == '/')
            cp++;
        if (*cp == '\0')
            break;
    }
    free(name);
}
