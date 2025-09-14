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
#include <unistd.h>

static ssize_t safe_readlink(const char *file, char *buf, size_t size) {
    ssize_t result = readlink(file, buf, size);
    if (result == -1) {
        if (errno == ENOTSUP) {
            // Handle unsupported feature
        }
        // Handle other potential errors
    }
    return result;
}
#include <errno.h>

static int create_symlink(const char *target, const char *linkname) {
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

#include <stdio.h>
#include <stdlib.h>

#define ATTRIBUTE_NORETURN __attribute__((noreturn))

ATTRIBUTE_NORETURN static void memory_exhausted(const char *msg) {
    if (!msg) {
        fprintf(stderr, "Memory exhausted: Unknown error\n");
    } else {
        fprintf(stderr, "Memory exhausted: %s\n", msg);
    }
    exit(EXIT_FAILURE);
}

ATTRIBUTE_NORETURN static void size_overflow(void) {
    if (!memory_exhausted(_("size overflow"))) {
        // Handle the error appropriately or log it.
        // This point will typically not be reached unless memory_exhausted function does not terminate.
    }
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
#include <stdlib.h>
#include <string.h>

static char *strdup(const char *str) {
    if (!str) return NULL;

    size_t len = strlen(str) + 1;
    char *result = malloc(len);
    if (result == NULL) return NULL;

    memcpy(result, str, len);
    return result;
}
#endif

#include <errno.h>
#include <string.h>

static void *memcheck(void *ptr) {
    if (ptr == NULL) {
        const char *error_message = HAVE_MALLOC_ERRNO ? strerror(errno) : strerror(ENOMEM);
        memory_exhausted(error_message);
    }
    return ptr;
}

#include <stdlib.h>
#include <stdio.h>

static void *emalloc(size_t size) {
    void *ptr = malloc(size);
    if (!ptr) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(EXIT_FAILURE);
    }
    return ptr;
}

#include <stdlib.h>
#include <errno.h>

static void *memcheck(void *ptr) {
    if (!ptr) {
        perror("Memory allocation failed");
        exit(EXIT_FAILURE);
    }
    return ptr;
}

static void *erealloc(void *ptr, size_t size) {
    void *new_ptr = realloc(ptr, size);
    return memcheck(new_ptr);
}

#include <string.h>
#include <errno.h>
#include <stdlib.h>

static char *checked_strdup(const char *str)
{
    if (str == NULL) {
        return NULL;
    }

    char *dup_str = strdup(str);
    if (dup_str == NULL) {
        // Handle memory allocation error
        perror("strdup failed");
        return NULL;
    }

    return dup_str;
}

#include <limits.h>
#include <stddef.h>

#define CHECKED_ADD_OVERFLOW(a, b, result) (__builtin_add_overflow(a, b, result))
#define CHECKED_MUL_OVERFLOW(a, b, result) (__builtin_mul_overflow(a, b, result))

static ptrdiff_t grow_nitems_alloc(ptrdiff_t *nitems_alloc, ptrdiff_t itemsize) {
    if (!nitems_alloc || itemsize <= 0 || *nitems_alloc < 0) {
        memory_exhausted(_("invalid parameters"));
    }

    ptrdiff_t addend = ((*nitems_alloc > (INDEX_MAX / 3 - 1)) ? 0 : (*nitems_alloc >> 1) + 1);
    if (addend > 0) {
        ptrdiff_t new_size;
        if (!CHECKED_ADD_OVERFLOW(*nitems_alloc, addend, &new_size)) {
            ptrdiff_t product;
            if (!CHECKED_MUL_OVERFLOW(new_size, itemsize, &product) && product <= INDEX_MAX) {
                *nitems_alloc = new_size;
                return product;
            }
        }
    }
    
    memory_exhausted(_("integer overflow"));
}

static void *growalloc(void *ptr, size_t itemsize, size_t nitems, size_t *nitems_alloc) {
    if (nitems >= *nitems_alloc) {
        ptr = erealloc(ptr, grow_nitems_alloc(nitems_alloc, itemsize));
        if (ptr == NULL) {
            // Handle allocation failure, possibly by logging or returning an error code as appropriate
            return NULL;
        }
    }
    return ptr;
}

/*
** Error handling.
*/

/* In most of the code, an input file name is represented by its index
   into the main argument vector, except that LEAPSEC_FILENUM stands
   for leapsec and COMMAND_LINE_FILENUM stands for the command line.  */
enum { LEAPSEC_FILENUM = -2, COMMAND_LINE_FILENUM = -1 };

/* Return the name of the Ith input file, for diagnostics.  */
static const char *filename(int i) {
    if (i == COMMAND_LINE_FILENUM) {
        return _("command line");
    }
    const char *fname = (i == LEAPSEC_FILENUM) ? leapsec : main_argv[i];
    return (strcmp(fname, "-") == 0) ? _("standard input") : fname;
}

void setFileAndLineNumbers(int fileNum, int lineNum, int refFileNum, int refLineNum) {
    filenum = fileNum;
    linenum = lineNum;
    rfilenum = refFileNum;
    rlinenum = refLineNum;
}

static void eat(int fnum, lineno num) {
    const int defaultEatsFlag = 0;
    const int defaultEatsNumber = -1;
    eats(fnum, num, defaultEatsFlag, defaultEatsNumber);
}

ATTRIBUTE_FORMAT((printf, 1, 0)) #include <stdio.h>
#include <stdarg.h>
#include <inttypes.h>

static void verror(const char *string, va_list args) {
    if (filenum) {
        fprintf(stderr, "\"%s\", line %" PRIdMAX ": ", filename(filenum), linenum);
    }
    
    vfprintf(stderr, string, args);
    
    if (rfilenum) {
        fprintf(stderr, " (rule from \"%s\", line %" PRIdMAX ")", filename(rfilenum), rlinenum);
    }
    
    fputc('\n', stderr);
}

ATTRIBUTE_FORMAT((printf, 1, 2)) #include <stdarg.h>
#include <stdbool.h>

static bool errors = false;

static void error(const char *string, ...) {
    if (!string) return;
    va_list args;
    va_start(args, string);
    verror(string, args);
    va_end(args);
    errors = true;
}

ATTRIBUTE_FORMAT((printf, 1, 2)) #include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>

static bool warnings = false;

static void verror(const char *const format, va_list args) {
    vfprintf(stderr, format, args);
    fprintf(stderr, "\n");
}

static void warning(const char *const string, ...) {
    if (string == NULL) {
        return;
    }
    va_list args;
    fprintf(stderr, "warning: ");
    va_start(args, string);
    verror(string, args);
    va_end(args);
    warnings = true;
}

/* Close STREAM.  If it had an I/O error, report it against DIR/NAME,
   remove TEMPNAME if nonnull, and then exit.  */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

static void close_file(FILE *stream, const char *dir, const char *name, const char *tempname) {
    if (ferror(stream) || fclose(stream) != 0) {
        const char *error_message = ferror(stream) ? "I/O error" : strerror(errno);
        fprintf(stderr, "%s: %s%s%s%s%s\n", progname,
                dir ? dir : "", dir ? "/" : "",
                name ? name : "", name ? ": " : "",
                error_message);
        if (tempname) {
            remove(tempname);
        }
        exit(EXIT_FAILURE);
    }
}

ATTRIBUTE_NORETURN static void usage(FILE *stream, int status) {
    const char *usage_format =
        "%s: usage is %s [ --version ] [ --help ] [ -v ] \\\n"
        "\t[ -b {slim|fat} ] [ -d directory ] [ -l localtime ]"
        " [ -L leapseconds ] \\\n"
        "\t[ -p posixrules ] [ -r '[@lo][/@hi]' ] [ -R '@hi' ] \\\n"
        "\t[ -t localtime-link ] \\\n"
        "\t[ filename ... ]\n\n"
        "Report bugs to %s.\n";

    fprintf(stream, usage_format, progname, progname, REPORT_BUGS_TO);

    if (status == EXIT_SUCCESS) {
        if (file_close(stream) != 0) {
            perror("Error closing file");
            exit(EXIT_FAILURE);
        }
    }
    exit(status);
}

/* Change the working directory to DIR, possibly creating DIR and its
   ancestors.  After this is done, all files are accessed with names
   relative to DIR.  */
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>

static void change_directory(const char *dir) {
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

    fprintf(stderr, _("%s: Can't chdir to %s: %s\n"), progname, dir, strerror(chdir_errno));
    exit(EXIT_FAILURE);
}

/* Compare the two links A and B, for a stable sort by link name.  */
static int qsort_linkcmp(const void *a, const void *b) {
    const struct link *l = a;
    const struct link *m = b;

    int cmp = strcmp(l->l_linkname, m->l_linkname);
    if (cmp != 0) {
        return cmp;
    }

    if (l->l_filenum != m->l_filenum) {
        return (l->l_filenum > m->l_filenum) ? 1 : -1;
    }

    if (l->l_linenum != m->l_linenum) {
        return (l->l_linenum > m->l_linenum) ? 1 : -1;
    }

    return 0;
}

/* Compare the string KEY to the link B, for bsearch.  */
#include <string.h>

static int bsearch_linkcmp(const void *key, const void *b) {
  const struct link *m = (const struct link *)b;
  if (key == NULL || m == NULL || m->l_linkname == NULL) {
    return -1;
  }
  return strcmp((const char *)key, m->l_linkname);
}

/* Make the links specified by the Link lines.  */
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

static void handle_error(const char* msg, const char* arg1, const char* arg2) {
  if (arg2) {
    printf(msg, arg1, arg2);
  } else {
    printf(msg, arg1);
  }
}

static void handle_warning(const char* msg, const char* arg1, const char* arg2) {
  if (arg2) {
    printf(msg, arg1, arg2);
  } else {
    printf(msg, arg1);
  }
}

static void
make_links(void)
{
  ptrdiff_t i, j, nalinks, pass_size;
  if (nlinks > 1)
    qsort(links, nlinks, sizeof *links, qsort_linkcmp);

  j = 0;
  for (i = 0; i < nlinks; i++) {
    while (i + 1 < nlinks && strcmp(links[i].l_linkname, links[i + 1].l_linkname) == 0)
      i++;
    links[j++] = links[i];
  }
  nlinks = pass_size = j;
  j = nalinks = nlinks;

  for (i = 0; i < nalinks; i++) {
    eat(links[i].l_filenum, links[i].l_linenum);

    if (i == j) {
      if (nalinks - i == pass_size) {
        handle_error("\"Link %s %s\" is part of a link cycle", links[i].l_target, links[i].l_linkname);
        break;
      }
      j = nalinks;
      pass_size = nalinks - i;
    }

    if (strcmp(links[i].l_target, links[i].l_linkname) == 0) {
      handle_error("link %s targets itself", links[i].l_target, NULL);
      continue;
    }

    struct link *l = bsearch(links[i].l_target, &links[i + 1], j - (i + 1), sizeof *links, bsearch_linkcmp);
    if (!l)
      l = bsearch(links[i].l_target, &links[j], nalinks - j, sizeof *links, bsearch_linkcmp);

    if (!l)
      dolink(links[i].l_target, links[i].l_linkname, false);
    else {
      links = growalloc(links, sizeof *links, nalinks, &nlinks_alloc);
      links[nalinks++] = links[i];
    }

    if (noise && i < nlinks) {
      if (l)
        handle_warning("link %s targeting link %s mishandled by pre-2023 zic", links[i].l_linkname, links[i].l_target);
      else if (bsearch(links[i].l_target, links, nlinks, sizeof *links, bsearch_linkcmp))
        handle_warning("link %s targeting link %s", links[i].l_linkname, links[i].l_target);
    }
  }
}

/* Simple signal handling: just set a flag that is checked
   periodically outside critical sections.  To set up the handler,
   prefer sigaction if available to close a signal race.  */

static sig_atomic_t got_signal;

#include <signal.h>

static volatile sig_atomic_t got_signal = 0;

static void signal_handler(int sig) {
  struct sigaction sa;
  got_signal = sig;
  
  sa.sa_handler = signal_handler;
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = 0;

  if (sigaction(sig, &sa, NULL) == -1) {
    // Handle error (e.g., log, exit, etc.)
  }
}

/* Arrange for SIGINT etc. to be caught by the handler.  */
#include <signal.h>

static void catch_signals(void) {
    static int const signals[] = {
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
    for (i = 0; i < sizeof(signals) / sizeof(signals[0]); i++) {
#ifdef SA_SIGINFO
        struct sigaction act, old_act;

        act.sa_handler = signal_handler;
        sigemptyset(&act.sa_mask);
        act.sa_flags = 0;

        if (sigaction(signals[i], &act, &old_act) == -1) {
            continue; // Handle error if needed
        }

        if (!(old_act.sa_flags & SA_SIGINFO) && old_act.sa_handler == SIG_IGN) {
            sigaction(signals[i], &old_act, NULL);
            got_signal = 0;
        }
#else
        if (signal(signals[i], signal_handler) == SIG_IGN) {
            signal(signals[i], SIG_IGN);
            got_signal = 0;
        }
#endif
    }
}

/* If a signal has arrived, terminate zic with appropriate status.  */
#include <signal.h>
#include <stdlib.h>

static void handle_signal(void) {
    if (got_signal) {
        int sig = got_signal;
        got_signal = 0;
        if (signal(sig, SIG_DFL) != SIG_ERR) {
            raise(sig);
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
#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

static bool timerange_option(char *timerange) {
    extern intmax_t min_time, max_time, lo_time, hi_time; // Assuming these are defined elsewhere
    intmax_t lo = min_time, hi = max_time;
    char *end_ptr;

    errno = 0;
    if (*timerange == '@') {
        lo = strtoimax(timerange + 1, &end_ptr, 10);
        if (end_ptr == timerange + 1 || errno == ERANGE || lo == INTMAX_MAX || lo < min_time || lo > max_time)
            return false;
    } else {
        end_ptr = timerange;
    }

    if (*end_ptr == '/' && *(end_ptr + 1) == '@') {
        hi = strtoimax(end_ptr + 2, &end_ptr, 10);
        if (end_ptr == end_ptr + 2 || errno == ERANGE || hi == INTMAX_MIN || lo > hi || hi < min_time || hi > max_time)
            return false;
    }

    lo_time = lo;
    hi_time = hi;
    return true;
}

/* Generate redundant time stamps up to OPT.  Return true if successful.  */
#include <errno.h>
#include <inttypes.h>
#include <stdbool.h>
#include <limits.h>

static bool redundant_time_option(char *opt) {
    if (opt == NULL || *opt != '@') {
        return false;
    }

    errno = 0;
    char *opt_end;
    intmax_t redundant = strtoimax(opt + 1, &opt_end, 10);

    if (errno != 0 || opt_end == opt + 1 || *opt_end != '\0' || redundant > INTMAX_MAX || redundant < INTMAX_MIN) {
        return false;
    }

    redundant_time = max(redundant_time, redundant);
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
    return (bloat >= 0);
}

#ifndef ZIC_BLOAT_DEFAULT
# define ZIC_BLOAT_DEFAULT "slim"
#endif

#include <stdio.h>
#include <locale.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

int main(int argc, char **argv) {
    int c;
    bool timerange_given = false;
    char *directory = NULL;
    char *lcltime = NULL;
    char *psxrules = NULL;
    char *tzdefault = NULL;
    char *leapsec = NULL;
    extern int optind;
    extern char *optarg;
    int bloat = 0;
    bool noise = false;
    int errors = 0;
    int bloat_value;

#ifdef S_IWGRP
    umask(umask(S_IWGRP | S_IWOTH) | (S_IWGRP | S_IWOTH));
#endif

#if HAVE_GETTEXT
    setlocale(LC_ALL, "");
    #ifdef TZ_DOMAINDIR
    bindtextdomain(TZ_DOMAIN, TZ_DOMAINDIR);
    #endif
    textdomain(TZ_DOMAIN);
#endif

    if (strcmp(argv[0], "--version") == 0) {
        printf("zic %s%s\n", PKGVERSION, TZVERSION);
        close_file(stdout, NULL, NULL, NULL);
        return EXIT_SUCCESS;
    } 
    if (strcmp(argv[0], "--help") == 0) {
        usage(stdout, EXIT_SUCCESS);
    }

    while ((c = getopt(argc, argv, "b:d:l:L:p:r:R:st:vy:")) != -1) {
        switch (c) {
            case 'b':
                if ((bloat_value = strcmp(optarg, "slim") == 0 ? -1 : strcmp(optarg, "fat") == 0 ? 1 : 0) == 0) {
                    error("invalid option: -b '%s'", optarg);
                } else if ((bloat < 0 && bloat_value > 0) || (bloat > 0 && bloat_value < 0)) {
                    error("incompatible -b options");
                }
                bloat = bloat_value;
                break;
            case 'd':
                if (directory != NULL) {
                    fprintf(stderr, "%s: More than one -d option specified\n", progname);
                    return EXIT_FAILURE;
                }
                directory = optarg;
                break;
            case 'l':
                if (lcltime != NULL) {
                    fprintf(stderr, "%s: More than one -l option specified\n", progname);
                    return EXIT_FAILURE;
                }
                lcltime = optarg;
                break;
            case 'p':
                if (psxrules != NULL) {
                    fprintf(stderr, "%s: More than one -p option specified\n", progname);
                    return EXIT_FAILURE;
                }
                psxrules = optarg;
                break;
            case 't':
                if (tzdefault != NULL) {
                    fprintf(stderr, "%s: More than one -t option specified\n", progname);
                    return EXIT_FAILURE;
                }
                tzdefault = optarg;
                break;
            case 'y': 
            case 's':
                warning("ignored");
                break;
            case 'L':
                if (leapsec != NULL) {
                    fprintf(stderr, "%s: More than one -L option specified\n", progname);
                    return EXIT_FAILURE;
                }
                leapsec = optarg;
                break;
            case 'v':
                noise = true;
                break;
            case 'r':
                if (timerange_given) {
                    fprintf(stderr, "%s: More than one -r option specified\n", progname);
                    return EXIT_FAILURE;
                }
                if (!timerange_option(optarg)) {
                    fprintf(stderr, "%s: invalid time range: %s\n", progname, optarg);
                    return EXIT_FAILURE;
                }
                timerange_given = true;
                break;
            case 'R':
                if (!redundant_time_option(optarg)) {
                    fprintf(stderr, "%s: invalid time: %s\n", progname, optarg);
                    return EXIT_FAILURE;
                }
                break;
            default:
                usage(stderr, EXIT_FAILURE);
        }
    }

    if (optind == argc - 1 && strcmp(argv[optind], "=") == 0) {
        usage(stderr, EXIT_FAILURE);
    }

    if (bloat == 0) {
        if (strcmp(ZIC_BLOAT_DEFAULT, "slim") == 0) {
            bloat = -1;
        } else if (strcmp(ZIC_BLOAT_DEFAULT, "fat") == 0) {
            bloat = 1;
        } else {
            abort();
        }
    }

    directory = directory ? directory : TZDIR;
    tzdefault = tzdefault ? tzdefault : TZDEFAULT;

    if (optind < argc && leapsec != NULL) {
        infile(LEAPSEC_FILENUM, leapsec);
        adjleap();
    }
    
    for (int k = optind; k < argc; k++) {
        infile(k, argv[k]);
    }

    if (errors) {
        return EXIT_FAILURE;
    }

    associate();
    change_directory(directory);
    catch_signals();
    
    for (ptrdiff_t i = 0, j; i < nzones; i = j) {
        for (j = i + 1; j < nzones && zones[j].z_name == NULL; ++j);
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

    if (warnings && (ferror(stderr) || fclose(stderr) != 0)) {
        return EXIT_FAILURE;
    }

    return errors ? EXIT_FAILURE : EXIT_SUCCESS;
}

#include <stdbool.h>
#include <stdio.h>
#include <string.h>

static void report_error(const char *format, const char *name, const char *component, ptrdiff_t len) {
    fprintf(stderr, format, name, (int)len, component);
}

static bool componentcheck(const char *name, const char *component, const char *component_end) {
    static const size_t component_len_max = 14;
    ptrdiff_t component_len = component_end - component;

    if (component_len == 0) {
        const char *error_message = !*name ? "empty file name" 
                                : (component == name) ? "file name '%s' begins with '/'"
                                : (*component_end) ? "file name '%s' contains '//'"
                                : "file name '%s' ends with '/'";
        report_error(error_message, name, NULL, 0);
        return false;
    }

    if (component_len > 0 && component_len <= 2 && component[0] == '.' && component_end[-1] == '.') {
        report_error("file name '%s' contains '%.*s' component", name, component, component_len);
        return false;
    }

    if (noise) {
        if (component_len > 0 && component[0] == '-') {
            fprintf(stderr, "file name '%s' component contains leading '-'", name);
        }
        if (component_len > component_len_max) {
            fprintf(stderr, "file name '%s' contains overlength component '%.*s...'", name, (int)component_len_max, component);
        }
    }

    return true;
}

#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

static bool noise = true; // Assuming this is set elsewhere as appropriate 

static const char* benign = "-/_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
static const char* printable_and_not_benign = " !\"#$%&'()*+,.0123456789:;<=>?@[\\]^`{|}~";

static void warning(const char* fmt, const char* name, char c) {
    if (isprint(c)) {
        printf(fmt, name, c);
    } else {
        printf(fmt, name, c);
    }
}

static bool componentcheck(const char *name, const char *component_start, const char *component_end) {
    // Begin implementation of the component check - assumed to be defined elsewhere
    return true;
}

static bool namecheck(const char *name) {
    const char *component = name;
    const char *cp = name;
    
    while (*cp) {
        unsigned char c = *cp;
        
        if (noise && !strchr(benign, c)) {
            const char *msg = strchr(printable_and_not_benign, c) 
                                ? "file name '%s' contains byte '%c'"
                                : "file name '%s' contains byte '\\%o'";
            warning(msg, name, c);
        }
        
        if (c == '/') {
            if (!componentcheck(name, component, cp)) {
                return false;
            }
            component = cp + 1;
        }
        cp++;
    }
    
    return componentcheck(name, component, cp);
}

/* Return a random uint_fast64_t.  */
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>
#include <errno.h>
#include <unistd.h>
#include <sys/random.h>

static uint_fast64_t get_rand_u64(void) {
#if HAVE_GETRANDOM
    static uint_fast64_t entropy_buffer[32];
    static int nwords;
    if (nwords == 0) {
        ssize_t s;
        do {
            s = getrandom(entropy_buffer, sizeof(entropy_buffer), 0);
        } while (s < 0 && errno == EINTR);

        nwords = (s < 0) ? -1 : (s / sizeof(*entropy_buffer));
    }
    if (nwords > 0) {
        return entropy_buffer[--nwords];
    }
#endif

    static bool initialized;
    if (!initialized) {
        srand((unsigned int)time(NULL));
        initialized = true;
    }

    uint_fast64_t r = 0;
    uint_fast64_t rmax = 0;
    uint_fast64_t rand_max = RAND_MAX;
    uint_fast64_t nrand = (rand_max < UINT_FAST64_MAX) ? rand_max + 1 : 0;
    uint_fast64_t rmod = (INT_MAX < UINT_FAST64_MAX) ? 0 : (UINT_FAST64_MAX / nrand + 1);

    do {
        if (rmod) {
            r %= rmod;
        }
        rmax = nrand * rmax + rand_max;
        r = nrand * r + rand();
    } while (rmax < UINT_FAST64_MAX);

    return r;
}

/* Generate a randomish name in the same directory as *NAME.  If
   *NAMEALLOC, put the name into *NAMEALLOC which is assumed to be
   that returned by a previous call and is thus already almost set up
   and equal to *NAME; otherwise, allocate a new name and put its
   address into both *NAMEALLOC and *NAME.  */
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

static void random_dirent(char const **name, char **namealloc) {
    static char const prefix[] = ".zic";
    static char const alphabet[] =
        "abcdefghijklmnopqrstuvwxyz"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "0123456789";
    enum { prefixlen = sizeof(prefix) - 1, alphabetlen = sizeof(alphabet) - 1 };
    const size_t suffixlen = 6;
    const char *src = *name;
    char *dst = *namealloc;
    const char *lastslash = strrchr(src, '/');
    ptrdiff_t dirlen = lastslash ? lastslash + 1 - src : 0;
    uint_fast64_t r;
    
    const uint_fast64_t base = alphabetlen;
    const uint_fast64_t base__6 = 1;
    for (int i = 0; i < suffixlen; ++i)
        base__6 *= base;

    const uint_fast64_t unfair_min = ~(uint_fast64_t)0 - ((~(uint_fast64_t)0 % base__6 + 1) % base__6);
    
    if (!dst) {
        dst = malloc(dirlen + prefixlen + suffixlen + 1);
        if (!dst) abort();
        memcpy(dst, src, dirlen);
        memcpy(dst + dirlen, prefix, prefixlen);
        dst[dirlen + prefixlen + suffixlen] = '\0';
        *name = *namealloc = dst;
    }

    do {
        r = get_rand_u64();
    } while (unfair_min <= r);

    for (size_t i = 0; i < suffixlen; ++i) {
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
#include <stdio.h>
#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

static FILE *open_outfile(const char **outname, char **tempname) {
    #if __STDC_VERSION__ < 201112
        static const char fopen_mode[] = "wb";
    #else
        static const char fopen_mode[] = "wbx";
    #endif

    bool dirs_made = false;

    if (!*tempname) {
        random_dirent(outname, tempname);
    }

    FILE *fp = NULL;
    while (!(fp = fopen(*outname, fopen_mode))) {
        switch (errno) {
            case ENOENT:
                if (!dirs_made) {
                    mkdirs(*outname, true);
                    dirs_made = true;
                }
                break;
            case EEXIST:
                random_dirent(outname, tempname);
                break;
            default:
                fprintf(stderr, _("%s: Can't create %s/%s: %s\n"), progname, directory, *outname, strerror(errno));
                exit(EXIT_FAILURE);
        }
    }

    return fp;
}

/* If TEMPNAME, the result is in the temporary file TEMPNAME even
   though the user wanted it in NAME, so rename TEMPNAME to NAME.
   Report an error and exit if there is trouble.  Also, free TEMPNAME.  */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

static void handle_rename_error(const char *tempname, const char *name) {
    int rename_errno = errno;
    remove(tempname);
    fprintf(stderr, "%s: rename to %s/%s: %s\n", progname, directory, name, strerror(rename_errno));
    exit(EXIT_FAILURE);
}

static void rename_dest(char *tempname, const char *name) {
    if (!tempname) {
        return;
    }

    if (rename(tempname, name) != 0) {
        handle_rename_error(tempname, name);
    }

    free(tempname);
}

/* Create symlink contents suitable for symlinking TARGET to LINKNAME, as a
   freshly allocated string.  TARGET should be a relative file name, and
   is relative to the global variable DIRECTORY.  LINKNAME can be either
   relative or absolute.  */
#include <stddef.h>
#include <string.h>
#include <limits.h>
#include <stdlib.h>

#define INDEX_MAX ((ptrdiff_t)PTRDIFF_MAX)

static char *emalloc(size_t size) {
    char *ptr = malloc(size);
    if (!ptr) {
        exit(EXIT_FAILURE); // Proper error handling
    }
    return ptr;
}

static ptrdiff_t size_sum(size_t a, size_t b) {
    return a + b <= PTRDIFF_MAX ? (ptrdiff_t)(a + b) : INDEX_MAX;
}

static ptrdiff_t size_product(size_t a, size_t b) {
    return a && b <= PTRDIFF_MAX / a ? (ptrdiff_t)(a * b) : INDEX_MAX;
}

static char *relname(char const *target, char const *linkname) {
    size_t dir_len = 0, dotdots = 0;
    ptrdiff_t dotdotetcsize, linksize = 0;
    char const *f = target;
    char *result = NULL;

    if (target == NULL || linkname == NULL) {
        return NULL; // Proper error handling
    }

    if (*linkname == '/') {
        size_t len = strlen(directory);
        size_t lenslash = len + (len && directory[len - 1] != '/');
        size_t targetsize = strlen(target) + 1;
        linksize = size_sum(lenslash, targetsize);
        f = result = emalloc(linksize);
        memcpy(result, directory, len);
        result[len] = '/';
        memcpy(result + lenslash, target, targetsize);
    }

    for (size_t i = 0; f[i] && f[i] == linkname[i]; i++) {
        if (f[i] == '/') {
            dir_len = i + 1;
        }
    }
    
    for (size_t i = dir_len; linkname[i]; i++) {
        if (linkname[i] == '/' && linkname[i-1] != '/') {
            dotdots++;
        }
    }

    size_t taillen = strlen(f + dir_len);
    dotdotetcsize = size_sum(size_product(dotdots, 3), taillen + 1);

    if (dotdotetcsize <= linksize || !result) {
        if (!result) {
            result = emalloc(dotdotetcsize);
        }
        for (size_t i = 0; i < dotdots; i++) {
            memcpy(result + 3 * i, "../", 3);
        }
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

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>

static void handle_error(const char *message, const char *path1, const char *path2) {
    fprintf(stderr, "%s: %s/%s: %s\n", message, path1, path2, strerror(errno));
    exit(EXIT_FAILURE);
}

static FILE* validate_file_open(const char *filepath, const char *directory, const char *target) {
    FILE *file = fopen(filepath, "rb");
    if (!file) handle_error("Can't read", directory, target);
    return file;
}

static void process_link_creation(const char *target, const char *outname, bool *linkdirs_made, bool absolute, char **linkalloc, char const **contents) {
    if (absolute) *linkalloc = NULL;
    else *linkalloc = relname(target, outname);
    *contents = absolute ? target : *linkalloc;
}

static void handle_link_errors(const char *directory, const char *target, const char *linkname, FILE *fp, FILE *tp, const char *tempname) {
    int c;
    while ((c = getc(fp)) != EOF) putc(c, tp);
    close_file(tp, directory, linkname, tempname);
    close_file(fp, directory, target, NULL);
}

static bool attempt_linkat(const char *target, const char *outname) {
    return linkat(AT_FDCWD, target, AT_FDCWD, outname, AT_SYMLINK_FOLLOW) == 0;
}

static int execute_symlink_creation(const char *target, const char *directory, const char *outname, bool absolute, char const *contents, int link_errno) {
    int symlink_errno;
    while (true) {
        if (symlink(contents, outname) == 0) return 0;
        symlink_errno = errno;
        if (symlink_errno == EEXIST) random_dirent(&outname, NULL);
        else if (symlink_errno == ENOENT && !absolute) mkdirs(outname, true);
        else break;
    }
    if (symlink_errno == 0) warning("symbolic link used because hard link failed: %s", strerror(link_errno));
    return symlink_errno;
}

static void dolink(const char *target, const char *linkname, bool staysymlink) {
    bool linkdirs_made = false;
    char *tempname = NULL;
    char const *outname = linkname;
    int link_errno = 0, symlink_errno;
    bool link_created = false;
    int targetissym, linknameissym;

    check_for_signal();

    if (strcmp(target, "-") == 0) {
        if (remove(linkname) != 0 && errno != ENOENT && errno != ENOTDIR)
            handle_error("Can't remove", linkname, linkname);
        return;
    }

    while (true) {
        if (attempt_linkat(target, outname) || link_errno == 0) {
            link_errno = 0;
            break;
        }      
        link_errno = errno;

#if HAVE_LINK
        if (link_errno == ENOTSUP && (same_parent_dirs(target, outname) || itssymlink(target, &targetissym) >= 0)) {
            if (link(target, outname) == 0) {
                link_errno = 0;
                break;
            }
            link_errno = errno;
        }
#endif
        if (link_errno == EXDEV || link_errno == ENOTSUP) break;

        if (link_errno == EEXIST) {
            staysymlink &= !tempname;
            random_dirent(&outname, &tempname);
            if (staysymlink && itssymlink(linkname, &linknameissym)) break;
        } else if (link_errno == ENOENT && !linkdirs_made) {
            mkdirs(linkname, true);
            linkdirs_made = true;
        } else {
            handle_error("Can't link", directory, target);
        }
    }

    if (link_errno != 0) {
        bool absolute = *target == '/';
        char *linkalloc;
        char const *contents;
        process_link_creation(target, outname, &linkdirs_made, absolute, &linkalloc, &contents);
        symlink_errno = execute_symlink_creation(target, directory, outname, absolute, contents, link_errno);
        free(linkalloc);
        
        if (symlink_errno != 0) {
            FILE *fp = validate_file_open(target, directory, target);
            FILE *tp = open_outfile(&outname, &tempname);
            handle_link_errors(directory, target, linkname, fp, tp, tempname);
            if (link_errno == ENOTSUP) warning("copy used because hard link failed: %s", strerror(link_errno));
            else if (symlink_errno != ENOTSUP) warning("copy used because symbolic link failed: %s", strerror(symlink_errno));
        }
    }

    rename_dest(tempname, linkname);
}

/* Return 1 if NAME is an absolute symbolic link, -1 if it is relative,
   0 if it is not a symbolic link.  If *CACHE is not -2, it is the
   cached result of a previous call to this function with the same NAME.  */
static int itssymlink(const char *name, int *cache) {
  if (*cache == -2) {
    char c = '\0';
    ssize_t len = readlink(name, &c, 1);
    if (len < 0) {
      *cache = 0;
    } else {
      *cache = (c == '/') ? 1 : -1;
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

#include <string.h>

static int rcomp(const void *cp1, const void *cp2) {
    const struct rule *r1 = (const struct rule *)cp1;
    const struct rule *r2 = (const struct rule *)cp2;

    if (r1 == NULL || r2 == NULL) {
        return 0;
    }

    if (r1->r_name == NULL || r2->r_name == NULL) {
        return 0;
    }

    return strcmp(r1->r_name, r2->r_name);
}

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

static void associate(void) {
    struct zone *zp;
    struct rule *rp;
    ptrdiff_t i, j, base, out;

    if (nrules > 1) {
        qsort(rules, nrules, sizeof(*rules), rcomp);
        for (i = 0; i < nrules - 1; ++i) {
            if (strcmp(rules[i].r_name, rules[i + 1].r_name) == 0 &&
                rules[i].r_filenum != rules[i + 1].r_filenum) {
                eat(rules[i].r_filenum, rules[i].r_linenum);
                warning("same rule name in multiple files");
                eat(rules[i + 1].r_filenum, rules[i + 1].r_linenum);
                warning("same rule name in multiple files");
                for (j = i + 2; j < nrules; ++j) {
                    if (strcmp(rules[i].r_name, rules[j].r_name) != 0 ||
                        rules[i].r_filenum == rules[j].r_filenum ||
                        rules[i + 1].r_filenum == rules[j].r_filenum) {
                        break;
                    }
                }
                i = j - 1;
            }
        }
    }

    for (i = 0; i < nzones; ++i) {
        zp = &zones[i];
        zp->z_rules = NULL;
        zp->z_nrules = 0;
    }

    for (base = 0; base < nrules; base = out) {
        rp = &rules[base];
        for (out = base + 1; out < nrules; ++out) {
            if (strcmp(rp->r_name, rules[out].r_name) != 0) break;
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
                error("%s in ruleless zone", "%s");
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
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

static bool inputline(FILE *fp, char *buf, ptrdiff_t bufsize) {
    int ch;
    ptrdiff_t linelen = 0;
    
    while ((ch = getc(fp)) != EOF) {
        if (ch == '\n') {
            break;
        }
        
        if (ch == 0) {
            fprintf(stderr, "NUL input byte\n");
            return false;
        }
        
        if (linelen >= bufsize - 1) {
            fprintf(stderr, "Line too long\n");
            return false;
        }

        buf[linelen++] = ch;
    }
    
    if (ferror(fp)) {
        fprintf(stderr, "Input error\n");
        return false;
    }
    
    if (ch == EOF && linelen == 0) {
        return false;
    }
    
    if (ch != '\n') {
        fprintf(stderr, "Unterminated line\n");
        return false;
    }
    
    buf[linelen] = '\0';
    return true;
}

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <limits.h>
#include <stdbool.h>

static void process_line(int fnum, int num, char *buf) {
    int nfields;
    char *fields[MAX_FIELDS];

    nfields = getfields(buf, fields, MAX_FIELDS);
    if (nfields == 0) return;

    static bool wantcont = false;
    if (wantcont) {
        wantcont = inzcont(fields, nfields);
    } else {
        struct lookup const *line_codes = fnum < 0 ? leap_line_codes : zi_line_codes;
        const struct lookup *lp = byword(fields[0], line_codes);

        if (lp == NULL) {
            error(_("input line of unknown type"));
        } else {
            switch (lp->l_value) {
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
    }
}

static void infile(int fnum, const char *name) {
    FILE *fp = (strcmp(name, "-") == 0) ? stdin : fopen(name, "r");
    if (!fp) {
        fprintf(stderr, _("%s: Can't open %s: %s\n"), progname, name, strerror(errno));
        exit(EXIT_FAILURE);
    }

    int bufsize_bound = min(INT_MAX, INDEX_MAX) / FORMAT_LEN_GROWTH_BOUND;
    char buf[min(_POSIX2_LINE_MAX, bufsize_bound)];
    for (int num = 1; inputline(fp, buf, sizeof buf); ++num) {
        process_line(fnum, num, buf);
        eat(fnum, num);
    }

    close_file(fp, NULL, filename(fnum), NULL);
    if (wantcont) error(_("expected continuation line not found"));
}

/*
** Convert a string of one of the forms
**	h	-h	hh:mm	-hh:mm	hh:mm:ss	-hh:mm:ss
** into a number of seconds.
** A null string maps to zero.
** Call error with errstring and return zero on errors.
*/

#include <stdbool.h>
#include <stdio.h>
#include <inttypes.h>

static zic_t
gethms(char const *string, char const *errstring)
{
	zic_t hh;
	int sign = 1, mm = 0, ss = 0, tenths = 0;
	char hhx = '\0', mmx = '\0', ssx = '\0', xr = '0';
	bool ok = true;

	if (!string || !*string) return 0;

	if (*string == '-') {
		sign = -1;
		string++;
	}

	int num_matches = sscanf(string, "%"SCNdZIC"%c%d%c%d%c%1d%*[0]%c%*[0123456789]", &hh, &hhx, &mm, &mmx, &ss, &ssx, &tenths, &xr);

	switch (num_matches) {
		case 8: ok = ('0' <= xr && xr <= '9');
		case 7: ok = (ssx == '.');
		        if (!ok) break;
		case 5: ok = (mmx == ':');
		case 3: ok = (hhx == ':');
		case 1: break;
		default: ok = false;
	}

	if (!ok || hh < 0 || mm < 0 || mm >= MINSPERHOUR || ss < 0 || ss > SECSPERMIN || ZIC_MAX / SECSPERHOUR < hh) {
		error("%s", errstring);
		return 0;
	}

	ss += (tenths >= 5) ? 1 : 0; // Round to nearest.

	if (noise && (hh > HOURSPERDAY || (hh == HOURSPERDAY && (mm != 0 || ss != 0)))) {
		warning(_("values over 24 hours not handled by pre-2007 versions of zic"));
	}

	return oadd(sign * hh * SECSPERHOUR, sign * (mm * SECSPERMIN + ss));
}

#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stddef.h>

static zic_t gethms(char *field, const char *err_msg);

static zic_t getsave(const char *field, bool *isdst) {
    zic_t save;
    bool dst = false;
    size_t fieldlen = strlen(field);
    char *field_copy = strdup(field);
    if (!field_copy) {
        // Handle memory allocation failure (error handling)
        *isdst = false;
        return 0;
    }

    if (fieldlen > 0) {
        switch (field_copy[fieldlen - 1]) {
            case 'd': dst = true; field_copy[fieldlen - 1] = '\0'; break;
            case 's': dst = false; field_copy[fieldlen - 1] = '\0'; break;
            default: break;
        }
    }

    save = gethms(field_copy, "invalid saved time");
    *isdst = dst ? (save != 0) : false;

    free(field_copy);
    return save;
}

static void
inrule(char **fields, int nfields)
{
    if (nfields != RULE_FIELDS) {
        error(_("wrong number of fields on Rule line"));
        return;
    }

    char first_char = *fields[RF_NAME];
    if (first_char == '\0' || isspace((unsigned char)first_char) || 
        first_char == '+' || first_char == '-' || isdigit((unsigned char)first_char)) {
        error(_("Invalid rule name \"%s\""), fields[RF_NAME]);
        return;
    }

    struct rule r = {
        .r_filenum = filenum,
        .r_linenum = linenum,
        .r_save = getsave(fields[RF_SAVE], &r.r_isdst)
    };

    if (!rulesub(&r, fields[RF_LOYEAR], fields[RF_HIYEAR],
                 fields[RF_COMMAND], fields[RF_MONTH], fields[RF_DAY],
                 fields[RF_TOD])) {
        return;
    }

    r.r_name = estrdup(fields[RF_NAME]);
    r.r_abbrvar = estrdup(fields[RF_ABBRVAR]);

    size_t abbrvar_len = strlen(r.r_abbrvar);
    if (max_abbrvar_len < abbrvar_len) {
        max_abbrvar_len = abbrvar_len;
    }

    rules = growalloc(rules, sizeof *rules, nrules, &nrules_alloc);
    rules[nrules++] = r;
}

#include <stdbool.h>
#include <string.h>
#include <stddef.h>
#include <stdio.h>

#define ZONE_MINFIELDS 3
#define ZONE_MAXFIELDS 5
#define ZF_NAME 0

extern char *tzdefault;
extern char *TZDEFRULES;
extern void *lcltime;
extern void *psxrules;
extern int nzones;
extern struct zone {
    char *z_name;
    int z_filenum;
    intmax_t z_linenum;
} zones[];
extern bool inzsub(char **fields, int nfields, bool flag);
extern void error(const char *format, ...);
extern const char *filename(int filenum);

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
            error(_("duplicate zone name %s (file \"%s\", line %" PRIdMAX ")"), fields[ZF_NAME], filename(zones[i].z_filenum), zones[i].z_linenum);
            return false;
        }
    }
    return inzsub(fields, nfields, false);
}

#include <stdbool.h>

static bool validateFieldCount(int nfields) {
    return (nfields >= ZONEC_MINFIELDS && nfields <= ZONEC_MAXFIELDS);
}

static bool inzcont(char **fields, int nfields) {
    if (!validateFieldCount(nfields)) {
        error(_("wrong number of fields on Zone continuation line"));
        return false;
    }
    return inzsub(fields, nfields, true);
}

static bool inzsub(char **fields, int nfields, bool iscont) {
    char *cp;
    struct zone z;
    int format_len;
    int i_stdoff, i_rule, i_format, i_untilyear, i_untilmonth, i_untilday, i_untiltime;
    bool hasuntil;

    i_stdoff = iscont ? ZFC_STDOFF : ZF_STDOFF;
    i_rule = iscont ? ZFC_RULE : ZF_RULE;
    i_format = iscont ? ZFC_FORMAT : ZF_FORMAT;
    i_untilyear = iscont ? ZFC_TILYEAR : ZF_TILYEAR;
    i_untilmonth = iscont ? ZFC_TILMONTH : ZF_TILMONTH;
    i_untilday = iscont ? ZFC_TILDAY : ZF_TILDAY;
    i_untiltime = iscont ? ZFC_TILTIME : ZF_TILTIME;

    if (!iscont && !namecheck(fields[ZF_NAME])) return false;

    z.z_filenum = filenum;
    z.z_linenum = linenum;
    z.z_stdoff = gethms(fields[i_stdoff], _("invalid UT offset"));

    cp = strchr(fields[i_format], '%');
    if (cp && ((*++cp != 's' && *cp != 'z') || strchr(cp, '%') || strchr(fields[i_format], '/'))) {
        error(_("invalid abbreviation format"));
        return false;
    }

    z.z_format_specifier = cp ? *cp : '\0';

    format_len = strlen(fields[i_format]);
    if (max_format_len < format_len) max_format_len = format_len;

    hasuntil = nfields > i_untilyear;
    if (hasuntil) {
        z.z_untilrule.r_filenum = filenum;
        z.z_untilrule.r_linenum = linenum;
        if (!rulesub(&z.z_untilrule, fields[i_untilyear], "only", "", 
            (nfields > i_untilmonth) ? fields[i_untilmonth] : "Jan",
            (nfields > i_untilday) ? fields[i_untilday] : "1",
            (nfields > i_untiltime) ? fields[i_untiltime] : "0")) 
            return false;
        
        z.z_untiltime = rpytime(&z.z_untilrule, z.z_untilrule.r_loyear);
        
        if (iscont && nzones > 0 && z.z_untiltime > min_time && z.z_untiltime < max_time &&
            zones[nzones - 1].z_untiltime > min_time && zones[nzones - 1].z_untiltime < max_time &&
            zones[nzones - 1].z_untiltime >= z.z_untiltime) {
            error(_("Zone continuation line end time is not after end time of previous line"));
            return false;
        }
    }

    z.z_name = iscont ? NULL : estrdup(fields[ZF_NAME]);
    z.z_rule = estrdup(fields[i_rule]);
    z.z_format = estrdup(fields[i_format]);
    
    if (z.z_format_specifier == 'z') {
        z.z_format[cp - fields[i_format]] = 's';
        if (noise) warning(_("format '%s' not handled by pre-2015 versions of zic"), fields[i_format]);
    }

    zones = growalloc(zones, sizeof *zones, nzones, &nzones_alloc);
    zones[nzones++] = z;

    return hasuntil;
}

#include <stdbool.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdlib.h>

typedef int64_t zic_t;
struct lookup {
    const char *name;
    int l_value;
};
extern int min_time;
extern int max_time;
extern bool leapseen;
extern zic_t leapmaxyear;
extern zic_t leapminyear;
extern const zic_t EPOCH_YEAR;
extern const int TM_JANUARY;
extern const zic_t SECSPERDAY;
extern const zic_t len_years[2];
extern const int len_months[2][12];
extern const struct lookup mon_names[];

extern void error(const char *msg);
extern zic_t oadd(zic_t t1, zic_t t2);
extern zic_t tadd(zic_t t1, zic_t t2);
extern const struct lookup *byword(const char *word, const struct lookup table[]);
extern zic_t gethms(const char *str, const char *errmsg);
extern bool isleap(zic_t year);

static zic_t getleapdatetime(char **fields, bool expire_line) {
    const char *cp;
    const struct lookup *lp;
    zic_t year, dayoff, tod, t;
    int month, day;

    cp = fields[LP_YEAR];
    if (sscanf(cp, "%" SCNdZIC, &year) != 1) {
        error("invalid leaping year");
        return -1;
    }

    if (!expire_line) {
        if (!leapseen || leapmaxyear < year) leapmaxyear = year;
        if (!leapseen || leapminyear > year) leapminyear = year;
        leapseen = true;
    }

    dayoff = 0;
    for (zic_t j = EPOCH_YEAR; j != year; j += (year > j ? 1 : -1)) {
        dayoff = oadd(dayoff, year > j ? len_years[isleap(j)] : -len_years[isleap(j - 1)]);
    }

    lp = byword(fields[LP_MONTH], mon_names);
    if (lp == NULL) {
        error("invalid month name");
        return -1;
    }

    month = lp->l_value;
    for (int j = TM_JANUARY; j != month; ++j) {
        dayoff = oadd(dayoff, len_months[isleap(year)][j]);
    }

    cp = fields[LP_DAY];
    if (sscanf(cp, "%d", &day) != 1 || day <= 0 || day > len_months[isleap(year)][month]) {
        error("invalid day of month");
        return -1;
    }

    dayoff = oadd(dayoff, day - 1);
    if (dayoff < min_time / SECSPERDAY || dayoff > max_time / SECSPERDAY) {
        error(dayoff < min_time / SECSPERDAY ? "time too small" : "time too large");
        return -1;
    }

    t = tadd(dayoff * SECSPERDAY, gethms(fields[LP_TIME], "invalid time of day"));
    if (t < 0) error("leap second precedes Epoch");

    return t;
}

#include <stdbool.h>
#include <stdio.h>
#include <string.h>

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
  if (strcmp(fields[LP_CORR], "+") == 0) {
    correction = 1;
  } else if (strcmp(fields[LP_CORR], "-") == 0) {
    correction = -1;
  } else {
    error(_("invalid CORRECTION field on Leap line"));
    return;
  }

  if (correction) {
    leapadd(t, correction, lp->l_value);
  }
}

static void inexpires(char **fields, int nfields) {
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

    struct link new_link = {
        .l_filenum = filenum,
        .l_linenum = linenum,
        .l_target = estrdup(fields[LF_TARGET]),
        .l_linkname = estrdup(fields[LF_LINKNAME])
    };

    links = growalloc(links, sizeof *links, nlinks, &nlinks_alloc);
    if (!links) {
        error(_("memory allocation failed"));
        return;
    }

    links[nlinks++] = new_link;
}

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

struct rule {
	int r_month;
	bool r_todisstd;
	bool r_todisut;
	int r_tod;
	int r_loyear;
	int r_hiyear;
	bool r_hiwasnum;
	int r_dycode;
	int r_wday;
	int r_dayofmonth;
};

struct lookup {
	const char *l_name;
	int l_value;
};

enum { YR_MINIMUM, YR_MAXIMUM, YR_ONLY, ZIC_MAX, YEAR_32BIT_MIN };
enum { DC_DOM, DC_DOWLEQ, DC_DOWGEQ };

static const struct lookup mon_names[] = { /* Placeholder for month names */ };
static const struct lookup begin_years[] = { /* Placeholder for years */ };
static const struct lookup end_years[] = { /* Placeholder for years */ };
static const struct lookup lasts[] = { /* Placeholder for lasts */ };
static const struct lookup wday_names[] = { /* Placeholder for weekdays */ };
static int len_months[2][12] = { /* Placeholder for the lengths of months */ };

static void error(const char *msg) {
	fprintf(stderr, "%s\n", msg);
}

static void warning(const char *format, const char *arg, int val) {
	fprintf(stderr, format, arg, val);
	fprintf(stderr, "\n");
}

static char *estrdup(const char *s) {
	char *d = strdup(s);
	if (!d) {
		error("Memory allocation error");
		exit(EXIT_FAILURE);
	}
	return d;
}

static int gethms(const char *str, const char *errmsg) {
	// Placeholder for the actual implementation
	return 0;
}

static const struct lookup *byword(const char *word, const struct lookup *table) {
	// Placeholder for the actual implementation
	return NULL;
}

static char lowerit(char c) {
	return (char)tolower((unsigned char)c);
}

static bool rulesub(struct rule *rp, const char *loyearp, const char *hiyearp,
	const char *typep, const char *monthp, const char *dayp,
	const char *timep)
{
	const struct lookup *lp;
	char *dp;
	char xs;

	if ((lp = byword(monthp, mon_names)) == NULL) {
		error("invalid month name");
		return false;
	}
	rp->r_month = lp->l_value;
	rp->r_todisstd = rp->r_todisut = false;

	dp = estrdup(timep);
	if (*dp) {
		char *ep = dp + strlen(dp) - 1;
		switch (tolower((unsigned char)*ep)) {
			case 's':
				rp->r_todisstd = true;
				*ep = '\0';
				break;
			case 'g': case 'u': case 'z':
				rp->r_todisstd = rp->r_todisut = true;
				*ep = '\0';
				break;
			default:
				break;
		}
	}
	rp->r_tod = gethms(dp, "invalid time of day");
	free(dp);

	if (!(lp = byword(loyearp, begin_years))) {
		if (sscanf(loyearp, "%d%c", &rp->r_loyear, &xs) != 1) {
			error("invalid starting year");
			return false;
		}
	} else {
		switch (lp->l_value) {
			case YR_MINIMUM:
				warning("FROM year \"%s\" is obsolete; treated as %d", loyearp, YEAR_32BIT_MIN - 1);
				rp->r_loyear = YEAR_32BIT_MIN - 1;
				break;
			default:
				break;
		}
	}

	if (!(lp = byword(hiyearp, end_years))) {
		rp->r_hiwasnum = true;
		if (sscanf(hiyearp, "%d%c", &rp->r_hiyear, &xs) != 1) {
			error("invalid ending year");
			return false;
		}
	} else {
		rp->r_hiwasnum = false;
		switch (lp->l_value) {
			case YR_MAXIMUM:
				rp->r_hiyear = ZIC_MAX;
				break;
			case YR_ONLY:
				rp->r_hiyear = rp->r_loyear;
				break;
			default:
				break;
		}
	}

	if (rp->r_loyear > rp->r_hiyear) {
		error("starting year greater than ending year");
		return false;
	}

	if (*typep) {
		error("year type is unsupported; use \"-\" instead");
		return false;
	}

	dp = estrdup(dayp);
	if ((lp = byword(dp, lasts)) != NULL) {
		rp->r_dycode = DC_DOWLEQ;
		rp->r_wday = lp->l_value;
		rp->r_dayofmonth = len_months[1][rp->r_month];
	} else {
		char *ep = strchr(dp, '<') ? : strchr(dp, '>');
		if (ep) {
			rp->r_dycode = strchr(dp, '<') ? DC_DOWLEQ : DC_DOWGEQ;
			*ep++ = 0;
			if (*ep++ != '=') {
				error("invalid day of month");
				free(dp);
				return false;
			}
			if ((lp = byword(dp, wday_names)) == NULL) {
				error("invalid weekday name");
				free(dp);
				return false;
			}
			rp->r_wday = lp->l_value;
		} else {
			ep = dp;
			rp->r_dycode = DC_DOM;
		}
		if (sscanf(ep, "%d", &rp->r_dayofmonth) != 1 || rp->r_dayofmonth <= 0 ||
			rp->r_dayofmonth > len_months[1][rp->r_month]) {
				error("invalid day of month");
				free(dp);
				return false;
		}
	}
	free(dp);
	return true;
}

#include <stdint.h>

static void convert(uint32_t val, char *buf) {
    for (int i = 0; i < 4; ++i) {
        buf[i] = (val >> (24 - i * 8)) & 0xff;
    }
}

#include <stdint.h>

static void convert64(uint_fast64_t val, char *buf) {
    for (int i = 0; i < 8; ++i) {
        buf[i] = (val >> (56 - i * 8)) & 0xff;
    }
}

#include <errno.h>
#include <stdio.h>
#include <stdint.h>

static void 
convert(zic_t val, char *buf);

static int
puttzcode(zic_t val, FILE *fp)
{
    char buf[4];
    convert(val, buf);
    if (fwrite(buf, sizeof(buf), 1, fp) != 1) {
        return -1; // Error writing to file
    }
    return 0; // Success
}

#include <stdio.h>
#include <string.h>

static void safe_fwrite(const void *ptr, size_t size, size_t count, FILE *fp) {
    if (fwrite(ptr, size, count, fp) != count) {
        perror("Error writing to file");
        exit(EXIT_FAILURE);
    }
}

static void puttzcodepass(zic_t val, FILE *fp, int pass) {
    char buf[8];
    if (pass == 1) {
        puttzcode(val, fp);
    } else {
        convert64(val, buf);
        safe_fwrite(buf, sizeof buf, 1, fp);
    }
}

static int atcomp(const void *avp, const void *bvp) {
    const struct attype *ap = (const struct attype *)avp;
    const struct attype *bp = (const struct attype *)bvp;
    if (ap->at < bp->at) return -1;
    if (ap->at > bp->at) return 1;
    return 0;
}

struct timerange {
  int defaulttype;
  ptrdiff_t base, count;
  int leapbase, leapcount;
  bool leapexpiry;
};

static struct timerange limitrange(struct timerange r, zic_t lo, zic_t hi, zic_t const *ats, unsigned char const *types) {
    while (r.count > 0 && ats[r.base] < lo) {
        r.defaulttype = types[r.base];
        r.count--;
        r.base++;
    }

    while (r.leapcount > 1 && trans[r.leapbase + 1] <= lo) {
        r.leapcount--;
        r.leapbase++;
    }

    while (r.leapbase > 0 && ((corr[r.leapbase - 1] < corr[r.leapbase]) != (corr[r.leapbase] > 0))) {
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

    r.leapexpiry = (leapexpires >= 0 && leapexpires - 1 <= hi);

    return r;
}

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stddef.h>

#define ZIC32_MIN (-2147483648)
#define ZIC32_MAX 2147483647
#define TZ_MAX_TIMES 1200
#define TZ_MAX_TYPES 256
#define TZ_MAX_CHARS 50 // Adjusted as needed
#define ZIC_MIN -1

typedef long zic_t;

struct timerange {
	int defaulttype;
	ptrdiff_t count;
	int leapcount;
	bool leapexpiry;
	ptrdiff_t base;
	ptrdiff_t leapbase;
};

static void writezone(const char *const name, const char *const string, char version, int defaulttype) {
	FILE *fp;
	ptrdiff_t i;
	char *tempname = NULL;
	const char *outname = name;
	zic_t *ats = malloc((timecnt + !timecnt) * sizeof *ats + 1);
	unsigned char *types = (unsigned char *)(ats + timecnt);
	struct timerange rangeall = {0}, range32, range64;

	if (timecnt > 1) qsort(attypes, timecnt, sizeof *attypes, atcomp);

	ptrdiff_t toi = 0;
	for (ptrdiff_t fromi = 0; fromi < timecnt; ++fromi) {
		if (toi != 0 && ((attypes[fromi].at + utoffs[attypes[toi - 1].type]) <= (attypes[toi - 1].at + utoffs[toi == 1 ? 0 : attypes[toi - 2].type]))) {
			attypes[toi - 1].type = attypes[fromi].type;
			continue;
		}
		if (toi == 0 || attypes[fromi].dontmerge || (utoffs[attypes[toi - 1].type] != utoffs[attypes[fromi].type]) || (isdsts[attypes[toi - 1].type] != isdsts[attypes[fromi].type]) || (desigidx[attypes[toi - 1].type] != desigidx[attypes[fromi].type])) {
			attypes[toi++] = attypes[fromi];
		}
	}
	timecnt = toi;

	for (i = 0; i < timecnt; ++i) {
		ats[i] = attypes[i].at;
		types[i] = attypes[i].type;
	}

	for (i = 0; i < timecnt; ++i) {
		ptrdiff_t j = leapcnt;
		while (--j >= 0) {
			if (ats[i] > trans[j] - corr[j]) {
				ats[i] = tadd(ats[i], corr[j]);
				break;
			}
		}
	}

	rangeall.defaulttype = defaulttype;
	rangeall.count = timecnt;
	rangeall.leapcount = leapcnt;
	range64 = limitrange(rangeall, lo_time, max(hi_time, redundant_time - (ZIC_MIN < redundant_time)), ats, types);
	range32 = limitrange(range64, ZIC32_MIN, ZIC32_MAX, ats, types);

	for (int pass = 1; pass <= 2; pass++) {
		struct timerange const *r = pass == 1 ? &range32 : &range64;
		if (pass == 1 && !want_bloat()) continue;
		if (r->leapexpiry || (0 < r->leapcount && (corr[r->leapbase] != 1 && corr[r->leapbase] != -1))) {
			version = '4';
			break;
		}
	}

	fp = open_outfile(&outname, &tempname);

	for (int pass = 1; pass <= 2; ++pass) {
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

		if (toomanytimes) error(_("too many transition times"));

		locut = thismin < lo_time && lo_time <= thismax;
		hicut = thismin <= hi_time && hi_time < thismax;
		thistimelim = thistimei + thistimecnt;
		memset(omittype, true, typecnt);

		if ((locut || (pass == 1 && thistimei)) && !(thistimecnt && ats[thistimei] == lo_time)) {
			pretranstype = thisdefaulttype;
			omittype[pretranstype] = false;
		}

		if (pass == 1 && lo_time <= thismin) thisdefaulttype = range64.defaulttype;

		if (locut) thisdefaulttype = unspecifiedtype;
		omittype[thisdefaulttype] = false;
		for (i = thistimei; i < thistimelim; i++) omittype[types[i]] = false;
		if (hicut) omittype[unspecifiedtype] = false;

		old0 = strlen(omittype);

		thistypecnt = 0;
		for (i = old0; i < typecnt; i++) {
			if (!omittype[i]) {
				typemap[i == old0 ? thisdefaulttype : i == thisdefaulttype ? old0 : i] = thistypecnt++;
			}
		}

		for (i = 0; i < sizeof indmap / sizeof indmap[0]; ++i) indmap[i] = -1;
		thischarcnt = stdcnt = utcnt = 0;

		for (i = old0; i < typecnt; i++) {
			if (omittype[i]) continue;
			if (ttisstds[i]) stdcnt = thistypecnt;
			if (ttisuts[i]) utcnt = thistypecnt;
			if (indmap[desigidx[i]] >= 0) continue;
			char *thisabbr = &chars[desigidx[i]];
			j = 0;
			while (j < thischarcnt) {
				if (strcmp(&thischars[j], thisabbr) == 0) break;
				j++;
			}
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

		memset(&tzh, 0, sizeof tzh);
		memcpy(tzh.tzh_magic, TZ_MAGIC, sizeof tzh.tzh_magic);
		tzh.tzh_version[0] = version;
		convert(utcnt, tzh.tzh_ttisutcnt);
		convert(stdcnt, tzh.tzh_ttisstdcnt);
		convert(thisleapcnt + thisleapexpiry, tzh.tzh_leapcnt);
		convert((0 <= pretranstype) + thistimecnt + hicut, tzh.tzh_timecnt);
		convert(thistypecnt, tzh.tzh_typecnt);
		convert(thischarcnt, tzh.tzh_charcnt);
		fwrite(tzh.tzh_magic, sizeof tzh.tzh_magic, 1, fp);
		fwrite(tzh.tzh_version, sizeof tzh.tzh_version, 1, fp);
		fwrite(tzh.tzh_reserved, sizeof tzh.tzh_reserved, 1, fp);
		fwrite(tzh.tzh_ttisutcnt, sizeof tzh.tzh_ttisutcnt, 1, fp);
		fwrite(tzh.tzh_ttisstdcnt, sizeof tzh.tzh_ttisstdcnt, 1, fp);
		fwrite(tzh.tzh_leapcnt, sizeof tzh.tzh_leapcnt, 1, fp);
		fwrite(tzh.tzh_timecnt, sizeof tzh.tzh_timecnt, 1, fp);
		fwrite(tzh.tzh_typecnt, sizeof tzh.tzh_typecnt, 1, fp);
		fwrite(tzh.tzh_charcnt, sizeof tzh.tzh_charcnt, 1, fp);

		if (pass == 1 && !want_bloat()) {
			puttzcode(0, fp);
			putc(0, fp);
			putc(0, fp);
			putc(0, fp);
			continue;
		}

		lo = pass == 1 && lo_time < ZIC32_MIN ? ZIC32_MIN : lo_time;

		if (0 <= pretranstype) puttzcodepass(lo, fp, pass);
		for (i = thistimei; i < thistimelim; ++i) {
			puttzcodepass(ats[i], fp, pass);
		}
		if (hicut) puttzcodepass(hi_time + 1, fp, pass);
		if (0 <= pretranstype) putc(typemap[pretranstype], fp);
		for (i = thistimei; i < thistimelim; i++) putc(typemap[types[i]], fp);
		if (hicut) putc(typemap[unspecifiedtype], fp);

		for (i = old0; i < typecnt; i++) {
			int h = (i == old0 ? thisdefaulttype : i == thisdefaulttype ? old0 : i);
			if (!omittype[h]) {
				puttzcode(utoffs[h], fp);
				putc(isdsts[h], fp);
				putc(indmap[desigidx[h]], fp);
			}
		}
		if (thischarcnt != 0) fwrite(thischars, sizeof thischars[0], thischarcnt, fp);
		thisleaplim = thisleapi + thisleapcnt;
		for (i = thisleapi; i < thisleaplim; ++i) {
			zic_t todo;
			if (roll[i]) {
				if (timecnt == 0 || trans[i] < ats[0]) {
					j = 0;
					while (isdsts[j]) if (++j >= typecnt) {
						j = 0;
						break;
					}
				} else {
					j = 1;
					while (j < timecnt && trans[i] >= ats[j]) ++j;
					j = types[j - 1];
				}
				todo = tadd(trans[i], -utoffs[j]);
			} else todo = trans[i];
			puttzcodepass(todo, fp, pass);
			puttzcode(corr[i], fp);
		}
		if (thisleapexpiry) {
			puttzcodepass(leapexpires, fp, pass);
			puttzcode(thisleaplim ? corr[thisleaplim - 1] : 0, fp);
		}
		if (stdcnt != 0) for (i = old0; i < typecnt; i++) if (!omittype[i]) putc(ttisstds[i], fp);
		if (utcnt != 0) for (i = old0; i < typecnt; i++) if (!omittype[i]) putc(ttisuts[i], fp);
	}
	fprintf(fp, "\n%s\n", string);
	close_file(fp, directory, name, tempname);
	rename_dest(tempname, name);
	free(ats);
}

#include <limits.h>
#include <stdbool.h>

#define SECSPERMIN 60
#define MINSPERHOUR 60

static const char *abbroffset(char *buf, zic_t offset) {
    if (!buf) return "%z"; // Handle null buffer error

    if (offset < -99 * MINSPERHOUR * SECSPERMIN || offset > 99 * MINSPERHOUR * SECSPERMIN) {
        error(_("%%z UT offset magnitude exceeds 99:59:59"));
        return "%z";
    }

    bool is_negative = offset < 0;
    if (is_negative) {
        offset = -offset;
    }

    int seconds = offset % SECSPERMIN;
    offset /= SECSPERMIN;
    int minutes = offset % MINSPERHOUR;
    offset /= MINSPERHOUR;

    char *p = buf;
    *p++ = is_negative ? '-' : '+';
    snprintf(p, INT_MAX, "%02d", (int)offset); // Safely handle buffer overflow checks

    if (minutes || seconds) {
        p += 2; // Move pointer to next position
        snprintf(p, INT_MAX, "%02d", minutes);
        if (seconds) {
            p += 2; // Move pointer to next position
            snprintf(p, INT_MAX, "%02d", seconds);
        }
    }

    return buf;
}

static char const disable_percent_s[] = "";

#include <stddef.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include <ctype.h>

#define PERCENT_Z_LEN_BOUND 10

typedef ptrdiff_t zic_t;

struct zone {
    char const *z_format;
    char z_format_specifier;
    zic_t z_stdoff;
};

static bool is_alpha(char c) {
    return isalpha((unsigned char)c);
}

static char* abbroffset(char *buf, zic_t offset) {
    /* Dummy implementation for demonstration purposes */
    snprintf(buf, PERCENT_Z_LEN_BOUND + 1, "UTC%+ld", (long)offset);
    return buf;
}

static ptrdiff_t doabbr(char *abbr, const struct zone *zp, const char *letters,
                        bool isdst, zic_t save, bool doquotes) {
    ptrdiff_t len;
    const char *format = zp->z_format;
    
    const char *slashp = strchr(format, '/');
    if (!slashp) {
        char letterbuf[PERCENT_Z_LEN_BOUND + 1];

        if (zp->z_format_specifier == 'z') {
            letters = abbroffset(letterbuf, zp->z_stdoff + save);
        } else if (!letters) {
            letters = "%s";
        } else if (letters == NULL) { // assuming disable_percent_s logic
            return 0;
        }
        snprintf(abbr, PERCENT_Z_LEN_BOUND + 1, format, letters); // fix buffer length
    } else {
        if (isdst) {
            strcpy(abbr, slashp + 1);
        } else {
            memcpy(abbr, format, slashp - format);
            abbr[slashp - format] = '\0';
        }
    }
    len = strlen(abbr);
    if (!doquotes) return len;

    for (const char *cp = abbr; *cp && is_alpha(*cp); cp++);
    if (*abbr && is_alpha(abbr[0]) && abbr[len] == '\0') return len;

    snprintf(abbr, len + 3, "<%s>", abbr);
    return len + 2;
}

static void updateminmax(const zic_t x) {
    if (x < min_year) {
        min_year = x;
    } 
    if (x > max_year) {
        max_year = x;
    }
}

#include <stdio.h>
#include <stdbool.h>

#define SECSPERMIN 60
#define MINSPERHOUR 60
#define HOURSPERDAY 24
#define DAYSPERWEEK 7

static int stringoffset(char *result, zic_t offset) {
    if (!result) return 0;

    bool negative = offset < 0;
    if (negative) offset = -offset;

    int hours = offset / (SECSPERMIN * MINSPERHOUR);
    int minutes = (offset / SECSPERMIN) % MINSPERHOUR;
    int seconds = offset % SECSPERMIN;

    if (hours >= HOURSPERDAY * DAYSPERWEEK) {
        *result = '\0';
        return 0;
    }

    int len = sprintf(result, "%s%d", negative ? "-" : "", hours);

    if (minutes > 0 || seconds > 0) {
        len += sprintf(result + len, ":%02d", minutes);
        if (seconds > 0) {
            len += sprintf(result + len, ":%02d", seconds);
        }
    }
    return len;
}

static int stringrule(char *result, struct rule *const rp, zic_t save, zic_t stdoff) {
    zic_t tod = rp->r_tod;
    int compat = 0;
    int month, total, wday, wdayoff, week;

    switch (rp->r_dycode) {
        case DC_DOM:
            if (rp->r_dayofmonth == 29 && rp->r_month == TM_FEBRUARY) {
                return -1;
            }
            total = 0;
            for (month = 0; month < rp->r_month; ++month) {
                total += len_months[0][month];
            }
            sprintf(result, (rp->r_month <= 1) ? "%d" : "J%d", total + rp->r_dayofmonth - 1);
            break;

        case DC_DOWGEQ:
        case DC_DOWLEQ:
            week = (rp->r_dycode == DC_DOWGEQ) ? 1 + (rp->r_dayofmonth - 1) / DAYSPERWEEK : 
                                                  rp->r_dayofmonth / DAYSPERWEEK;
            wdayoff = rp->r_dayofmonth % DAYSPERWEEK;
            if (wdayoff > 0) {
                compat = 2013;
            }
            wday = (rp->r_dycode == DC_DOWGEQ ? rp->r_wday - wdayoff : rp->r_wday);
            tod += wdayoff * SECSPERDAY;
            wday = (wday + DAYSPERWEEK) % DAYSPERWEEK;

            if (rp->r_dycode == DC_DOWLEQ && rp->r_dayofmonth == len_months[1][rp->r_month]) {
                week = 5;
            }
            sprintf(result, "M%d.%d.%d", rp->r_month + 1, week, wday);
            break;

        default:
            return -1; // should not happen
    }

    if (rp->r_todisut) {
        tod += stdoff;
    }
    if (rp->r_todisstd && !rp->r_isdst) {
        tod += save;
    }
    if (tod != 2 * SECSPERMIN * MINSPERHOUR) {
        *result++ = '/';
        if (!stringoffset(result, tod)) {
            return -1;
        }
        if ((tod < 0 && compat < 2013) || (SECSPERDAY <= tod && compat < 1994)) {
            compat = (tod < 0) ? 2013 : 1994;
        }
    }
    return compat;
}

static int rule_cmp(const struct rule *a, const struct rule *b) {
    if (a == NULL && b == NULL) return 0;
    if (a == NULL) return -1;
    if (b == NULL) return 1;

    if (a->r_hiyear != b->r_hiyear) {
        return (a->r_hiyear < b->r_hiyear) ? -1 : 1;
    }
    
    if (a->r_hiyear == ZIC_MAX) return 0;

    if (a->r_month != b->r_month) {
        return a->r_month - b->r_month;
    }
    
    return a->r_dayofmonth - b->r_dayofmonth;
}

/* Store into RESULT a proleptic TZ string that represent the future
   predictions for the zone ZPFIRST with ZONECOUNT entries.  Return a
   compatibility indicator (a TZDB release year) if successful, a
   negative integer if no such TZ string exists.  */
#include <stddef.h>
#include <string.h>

static int stringzone(char *result, const struct zone *zpfirst, ptrdiff_t zonecount) {
    const struct zone *zp;
    struct rule *stdrp = NULL, *dstrp = NULL;
    struct zone zstr[2];
    const struct zone *stdzp;
    const struct zone *dstzp;

    result[0] = '\0'; 

    if (hi_time < max_time) {
        return -1;
    }

    zp = zpfirst + zonecount - 1;
    for (ptrdiff_t i = 0; i < zp->z_nrules; ++i) {
        struct rule *rp = &zp->z_rules[i];
        if (!rp->r_isdst) {
            if (!stdrp || rule_cmp(stdrp, rp) < 0) {
                stdrp = rp;
            }
        } else {
            if (!dstrp || rule_cmp(dstrp, rp) < 0) {
                dstrp = rp;
            }
        }
    }
    
    int dstcmp = (zp->z_nrules && dstrp && stdrp) ? rule_cmp(dstrp, stdrp) : (zp->z_isdst ? 1 : -1);
    stdzp = dstzp = zp;

    if (dstcmp < 0) {
        dstrp = NULL;
    } else if (dstcmp > 0) {
        zic_t save = dstrp ? dstrp->r_save : zp->z_save;
        if (save >= 0) {
            stdzp = &zstr[0];
            dstzp = &zstr[1];
            zstr[0].z_stdoff = zp->z_stdoff + 2 * save;
            zstr[1].z_stdoff = zstr[0].z_stdoff;
            zstr[1].z_format = zp->z_format;
            zstr[1].z_format_specifier = zp->z_format_specifier;
        }
        struct rule dstr = {.r_month = TM_JANUARY, .r_dycode = DC_DOM, .r_dayofmonth = 1,
                            .r_tod = 0, .r_isdst = true, .r_save = (save < 0) ? save : -save,
                            .r_abbrvar = dstrp ? dstrp->r_abbrvar : NULL};
        struct rule stdr = {.r_month = TM_DECEMBER, .r_dycode = DC_DOM, .r_dayofmonth = 31,
                            .r_tod = SECSPERDAY + dstr.r_save, .r_isdst = false, .r_save = 0,
                            .r_abbrvar = (save < 0 && stdrp) ? stdrp->r_abbrvar : NULL};
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

    if (!dstrp) {
        return 0;
    }

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
    len += strlen(result + len);
    
    result[len++] = ',';
    c = stringrule(result + len, stdrp, dstrp->r_save, stdzp->z_stdoff);
    if (c < 0) {
        result[0] = '\0';
        return -1;
    }
    return c;
}

#include <stddef.h>
#include <stdbool.h>
#include <stdlib.h>

static void outzone(const struct zone *zpfirst, ptrdiff_t zonecount) {
    zic_t starttime = ZIC_MIN, untiltime = ZIC_MIN, max_year0;
    zic_t nonTZlimtime = ZIC_MIN, min_year, max_year;
    int nonTZlimtype = -1, defaulttype = -1, compat;
    bool startttisstd, startttisut, do_extend, usestart, useuntil;
    char *startbuf, *ab, *envvar, version;
    int max_abbr_len, max_envvar_len, stdoff, save, startoff;

    check_for_signal();

    max_abbr_len = 2 + max_format_len + max_abbrvar_len;
    max_envvar_len = 2 * max_abbr_len + 45;

    startbuf = emalloc(max_abbr_len + 1);
    ab = emalloc(max_abbr_len + 1);
    envvar = emalloc(max_envvar_len + 1);

    typecnt = charcnt = timecnt = 0;
    
    startttisstd = startttisut = false;
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
            struct rule *rp = &zp->z_rules[j];
            updateminmax(rp->r_loyear);
            if (rp->r_hiwasnum) {
                updateminmax(rp->r_hiyear);
            }
        }
    }

    compat = stringzone(envvar, zpfirst, zonecount);
    version = compat < 2013 ? '2' : '3';
    do_extend = compat < 0;

    if (do_extend) {
        adjust_year_bounds(&min_year, &max_year, ZIC_MIN, ZIC_MAX, years_of_observations);
    }

    max_year = max(max_year, (redundant_time / SECSPERDAY / DAYSPERNYEAR + EPOCH_YEAR + 1));
    max_year0 = max_year;
    adjust_for_32bit(&min_year, &max_year);

    if (min_time < lo_time || hi_time < max_time) {
        unspecifiedtype = addtype(0, "-00", false, false, false);
    }

    for (ptrdiff_t i = 0; i < zonecount; ++i) {
        const struct zone *zp = &zpfirst[i];
        useuntil = i < (zonecount - 1);
        stdoff = zp->z_stdoff;
        startoff = stdoff;
        usestart = i > 0 && (zp - 1)->z_untiltime > min_time;

        if (zp->z_nrules == 0) {
            handle_no_rules(zp, &usestart, &defaulttype, startbuf, stdoff, save, startttisstd, startttisut);
        } else {
            handle_rules(zp, min_year, max_year, useuntil, &stdoff, &usestart, startbuf, &nonTZlimtime, &nonTZlimtype);
        }

        if (usestart) {
            handle_undefined_start(zp, startbuf, &defaulttype, stdoff, save);
        }

        set_next_start_time(zp, useuntil, &starttime, &startttisstd, &startttisut, stdoff, save);
    }

    finalize_types(&defaulttype, &do_extend, max_year0, nonTZlimtime, redundant_time, envvar);
    writezone(zpfirst->z_name, envvar, version, defaulttype);

    free(startbuf);
    free(ab);
    free(envvar);
}

void adjust_year_bounds(zic_t *min_year, zic_t *max_year, zic_t lower_bound, zic_t upper_bound, int years_offset) {
    if (*min_year >= lower_bound + years_offset) {
        *min_year -= years_offset;
    } else {
        *min_year = lower_bound;
    }

    if (*max_year <= upper_bound - years_offset) {
        *max_year += years_offset;
    } else {
        *max_year = upper_bound;
    }
}

void adjust_for_32bit(zic_t *min_year, zic_t *max_year) {
    if (want_bloat()) {
        if (*min_year > YEAR_32BIT_MIN - 1) {
            *min_year = YEAR_32BIT_MIN - 1;
        }
        if (*max_year < YEAR_32BIT_MAX) {
            *max_year = YEAR_32BIT_MAX;
        }
    }
}

void handle_no_rules(const struct zone *zp, bool *usestart, int *defaulttype, char *startbuf, zic_t stdoff, zic_t save, bool startttisstd, bool startttisut) {
    int type;
    save = zp->z_save;
    doabbr(startbuf, zp, NULL, zp->z_isdst, save, false);
    type = addtype(oadd(zp->z_stdoff, save), startbuf, zp->z_isdst, startttisstd, startttisut);
    if (*usestart) {
        addtt(starttime, type);
        *usestart = false;
    } else {
        *defaulttype = type;
    }
}

void handle_rules(const struct zone *zp, zic_t min_year, zic_t max_year, bool useuntil, zic_t *stdoff,
                  bool *usestart, char *startbuf, zic_t *nonTZlimtime, int *nonTZlimtype) {
    zic_t save = 0;
    for (zic_t year = min_year; year <= max_year; ++year) {
        if (useuntil && year > zp->z_untilrule.r_hiyear) break;

        process_rules_in_year(zp, year, useuntil, stdoff, usestart, startbuf, nonTZlimtime, nonTZlimtype, &save);
    }
}

void process_rules_in_year(const struct zone *zp, zic_t year, bool useuntil, zic_t *stdoff,
                           bool *usestart, char *startbuf, zic_t *nonTZlimtime, int *nonTZlimtype, zic_t *save) {
    for (ptrdiff_t j = 0; j < zp->z_nrules; ++j) {
        struct rule *rp = &zp->z_rules[j];
        set_todo_for_rule(rp, year);
    }

    zic_t untiltime = evaluate_transition_time(zp, useuntil, *stdoff, *save);

    for (;;) {
        process_earliest_rule(zp, year, &untiltime, useuntil, stdoff,
                              usestart, startbuf, nonTZlimtime, nonTZlimtype, save);
    }
}

void process_earliest_rule(const struct zone *zp, zic_t year, zic_t *untiltime, bool useuntil, zic_t *stdoff,
                           bool *usestart, char *startbuf, zic_t *nonTZlimtime, int *nonTZlimtype, zic_t *save) {
    int k;
    zic_t ktime = ZIC_MIN, offset;
    k = find_earliest_rule_index(zp, &ktime);

    if (k < 0) return; 

    struct rule *rp = &zp->z_rules[k];
    rp->r_todo = false;

    if (useuntil && ktime >= *untiltime) {
        if (!*startbuf && oadd(zp->z_stdoff, rp->r_save) == *save) {
            doabbr(startbuf, zp, rp->r_abbrvar, rp->r_isdst, rp->r_save, false);
        }
        return;
    }

    *save = rp->r_save;
    handle_matching_early_transition(zp, rp, *stdoff, usestart, &ktime, startbuf, *save);

    handle_non_matching_early_transition(zp, rp, stdoff, usestart, &ktime, startbuf, nonTZlimtime, nonTZlimtype, *save);
}

void handle_matching_early_transition(const struct zone *zp, struct rule *rp, zic_t stdoff,
                                      bool *usestart, zic_t *ktime, char *startbuf, zic_t save) {
    if (*usestart && *ktime == starttime) {
        *usestart = false;
    }

    if (*usestart && *ktime < starttime) {
        if (zp->z_format) {
            doabbr(startbuf, zp, rp->r_abbrvar, rp->r_isdst, rp->r_save, false);
        }
    }
}

void handle_non_matching_early_transition(const struct zone *zp, struct rule *rp, zic_t *stdoff,
                                          bool *usestart, zic_t *ktime, char *startbuf, zic_t *nonTZlimtime, int *nonTZlimtype, zic_t save) {
    char ab[max_abbr_len + 1];
    make_type_and_add_transition(zp, rp, *stdoff, ab, *ktime);

    if (*nonTZlimtime < *ktime) {
        *nonTZlimtime = *ktime;
        *nonTZlimtype = addtype(stdoff, ab, rp->r_isdst, rp->r_todisstd, rp->r_todisut);
    }
}

void set_todo_for_rule(struct rule *rp, zic_t year) {
    rp->r_todo = (year >= rp->r_loyear && year <= rp->r_hiyear);
    if (rp->r_todo) {
        rp->r_temp = rpytime(rp, year);
        rp->r_todo = (rp->r_temp < (1ULL << 31) || year <= max_year0);
    }
}

zic_t evaluate_transition_time(const struct zone *zp, bool useuntil, zic_t stdoff, zic_t save) {
    zic_t untiltime = zp->z_untiltime;
    if (!zp->z_untilrule.r_todisut) {
        untiltime = tadd(untiltime, -stdoff);
    }
    if (!zp->z_untilrule.r_todisstd) {
        untiltime = tadd(untiltime, -save);
    }
    return untiltime;
}

int find_earliest_rule_index(const struct zone *zp, zic_t *ktime) {
    int k = -1;
    for (ptrdiff_t j = 0; j < zp->z_nrules; ++j) {
        struct rule *rp = &zp->z_rules[j];
        if (!rp->r_todo) continue;

        zic_t jtime = rp->r_temp;
        if (jtime == min_time || jtime == max_time) continue;

        jtime = tadd(jtime, -evaluate_offset(rp, stdoff, save));

        if (k < 0 || jtime < *ktime) {
            k = j;
            *ktime = jtime;
        }
    }
    return k;
}

zic_t evaluate_offset(struct rule *rp, zic_t stdoff, zic_t save) {
    zic_t offset = rp->r_todisut ? 0 : stdoff;
    if (!rp->r_todisstd) {
        offset = oadd(offset, save);
    }
    return offset;
}

void make_type_and_add_transition(const struct zone *zp, struct rule *rp, zic_t stdoff, char *ab, zic_t ktime) {
    doabbr(ab, zp, rp->r_abbrvar, rp->r_isdst, rp->r_save, false);
    int type = addtype(oadd(stdoff, rp->r_save), ab, rp->r_isdst, rp->r_todisstd, rp->r_todisut);
    addtt(ktime, type);
}

void handle_undefined_start(const struct zone *zp, char *startbuf, int *defaulttype, zic_t stdoff, zic_t save) {
    bool isdst = startoff != stdoff;
    if (zp->z_format) {
        doabbr(startbuf, zp, disable_percent_s, isdst, save, false);
    }

    if (*startbuf) {
        add_transition_given_startbuf(startbuf, isdst, defaulttype);
    }
}

void add_transition_given_startbuf(char *startbuf, bool isdst, int *defaulttype) {
    int type = addtype(startoff, startbuf, isdst, false, false);
    if (*defaulttype < 0 && !isdst) {
        *defaulttype = type;
    }
    addtt(starttime, type);
}

void set_next_start_time(const struct zone *zp, bool useuntil, zic_t *starttime, bool *startttisstd, bool *startttisut, zic_t stdoff, zic_t save) {
    if (useuntil) {
        *startttisstd = zp->z_untilrule.r_todisstd;
        *startttisut = zp->z_untilrule.r_todisut;
        *starttime = zp->z_untiltime;

        if (!*startttisstd) {
            *starttime = tadd(*starttime, -save);
        }

        if (!*startttisut) {
            *starttime = tadd(*starttime, -stdoff);
        }
    }
}

void finalize_types(int *defaulttype, bool *do_extend, zic_t max_year0, zic_t nonTZlimtime, zic_t redundant_time, char *envvar) {
    if (*defaulttype < 0) {
        *defaulttype = 0;
    }

    finalize_do_extend_case(do_extend, nonTZlimtime, redundant_time);

    for (ptrdiff_t i = 0, j = 0; i < timecnt; i++) {
        finalize_timecnt_element(&i, &j, nonTZlimtime, _MAX_TYPE_INDEX);
    }
}

void finalize_do_extend_case(bool *do_extend, zic_t nonTZlimtime, zic_t redundant_time) {
    if (!*do_extend && !want_bloat()) {

        zic_t keep_at_max;
        zic_t TZstarttime = find_TZstarttime(nonTZlimtime);

        if (TZstarttime == ZIC_MAX) {
            TZstarttime = nonTZlimtime;
        }

        keep_at_max = max(TZstarttime, redundant_time);

        for (ptrdiff_t i = 0, j = 0; i < timecnt; i++) {
            write_zone_if_within_bounds(&i, &j, keep_at_max, TZstarttime, nonTZlimtype, envvar);
        }

        timecnt = j;
    }

    if (*do_extend) {
        handle_do_extend_case();
    }
}

zic_t find_TZstarttime(zic_t nonTZlimtime) {
    zic_t TZstarttime = ZIC_MAX;
    for (ptrdiff_t i = 0; i < timecnt; i++) {
        zic_t at = attypes[i].at;
        if (nonTZlimtime < at && at < TZstarttime) {
            TZstarttime = at;
        }
    }
    return TZstarttime;
}

void write_zone_if_within_bounds(ptrdiff_t *i, ptrdiff_t *j, zic_t keep_at_max, zic_t TZstarttime, int nonTZlimtype, char *envvar) {
    if (attypes[*i].at <= keep_at_max) {
        attypes[*j].at = attypes[*i].at;
        attypes[*j].dontmerge = (attypes[*i].at == TZstarttime
                                 && (nonTZlimtype != attypes[*i].type || strchr(envvar, ',')));
        attypes[*j].type = attypes[*i].type;
        (*j)++;
    }
}

void handle_do_extend_case() {
    struct rule xr;
    struct attype *lastat;
    xr.r_month = TM_JANUARY;
    xr.r_dycode = DC_DOM;
    xr.r_dayofmonth = 1;
    xr.r_tod = 0;

    for (lastat = attypes, ptrdiff_t i = 1; i < timecnt; i++) {
        if (attypes[i].at > lastat->at) {
            lastat = &attypes[i];
        }
    }

    if (!lastat || lastat->at < rpytime(&xr, max_year - 1)) {
        addtt(rpytime(&xr, max_year + 1), lastat ? lastat->type : defaulttype);
        attypes[timecnt - 1].dontmerge = true;
    }
}

static int
addtt(zic_t starttime, int type)
{
	if (!(attypes = growalloc(attypes, sizeof *attypes, timecnt, &timecnt_alloc))) {
		return -1;
	}
	attypes[timecnt].at = starttime;
	attypes[timecnt].dontmerge = false;
	attypes[timecnt].type = type;
	timecnt++;
	return 0;
}

static int addtype(zic_t utoff, const char *abbr, bool isdst, bool ttisstd, bool ttisut) {
    if (utoff < INT32_MIN || utoff > INT32_MAX) {
        error(_("UT offset out of range"));
        exit(EXIT_FAILURE);
    }

    if (!want_bloat()) {
        ttisstd = false;
        ttisut = false;
    }

    for (int j = 0; j < charcnt; ++j) {
        if (strcmp(&chars[j], abbr) == 0) {
            for (int i = 0; i < typecnt; i++) {
                if (utoff == utoffs[i] && isdst == isdsts[i] && j == desigidx[i] &&
                    ttisstd == ttisstds[i] && ttisut == ttisuts[i]) {
                    return i;
                }
            }
            break;
        }
    }

    if (charcnt < strlen(abbr)) {
        newabbr(abbr);
    }

    if (typecnt >= TZ_MAX_TYPES) {
        error(_("too many local time types"));
        exit(EXIT_FAILURE);
    }

    int i = typecnt++;
    utoffs[i] = utoff;
    isdsts[i] = isdst;
    ttisstds[i] = ttisstd;
    ttisuts[i] = ttisut;
    desigidx[i] = charcnt;

    return i;
}

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void leapadd(zic_t t, int correction, int rolling) {
    if (leapcnt >= TZ_MAX_LEAPS) {
        fprintf(stderr, "Error: too many leap seconds\n");
        exit(EXIT_FAILURE);
    }
    if (rolling && (lo_time != min_time || hi_time != max_time)) {
        fprintf(stderr, "Error: Rolling leap seconds not supported with -r\n");
        exit(EXIT_FAILURE);
    }

    int i = 0;
    while (i < leapcnt && t > trans[i]) {
        i++;
    }

    if (i < leapcnt) {
        memmove(&trans[i+1], &trans[i], (leapcnt - i) * sizeof *trans);
        memmove(&corr[i+1], &corr[i], (leapcnt - i) * sizeof *corr);
        memmove(&roll[i+1], &roll[i], (leapcnt - i) * sizeof *roll);
    }
    
    trans[i] = t;
    corr[i] = correction;
    roll[i] = rolling;
    leapcnt++;
}

#include <stdio.h>
#include <stdlib.h>

static void adjleap(void) {
    zic_t last = 0;
    zic_t prevtrans = 0;

    for (int i = 0; i < leapcnt; ++i) {
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
        if (leapcnt != 0 && !(trans[leapcnt - 1] < leapexpires)) {
            error(_("last Leap time does not precede Expires time"));
            exit(EXIT_FAILURE);
        }
    }
}

/* Is A a space character in the C locale?  */
#include <stdbool.h>

static bool is_space(char a) {
    return (a == ' ' || a == '\f' || a == '\n' || a == '\r' || a == '\t' || a == '\v');
}

/* Is A an alphabetic character in the C locale?  */
#include <ctype.h>

static bool is_alpha(char a) {
    return isalpha((unsigned char)a) != 0;
}

/* If A is an uppercase character in the C locale, return its lowercase
   counterpart.  Otherwise, return A.  */
#include <ctype.h>

static char lowerit(char a) {
    return isupper((unsigned char)a) ? (char)tolower((unsigned char)a) : a;
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

static const struct lookup *byword(const char *word, const struct lookup *table) {
    if (!word || !table) {
        return NULL;
    }

    if (table == lasts && ciprefix("last", word) && word[4]) {
        if (word[4] == '-') {
            warning(_("\"%s\" is undocumented; use \"last%s\" instead"), word, word + 5);
        } else {
            word += 4;
            table = wday_names;
        }
    }

    const struct lookup *foundlp = NULL;
    for (const struct lookup *lp = table; lp->l_word != NULL; ++lp) {
        if (ciequal(word, lp->l_word)) {
            return lp;
        }

        if (ciprefix(word, lp->l_word)) {
            if (foundlp != NULL) {
                return NULL;
            }
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

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

static void handle_error(const char *message) {
    fprintf(stderr, "%s\n", message);
    exit(EXIT_FAILURE);
}

static bool is_space(char c) {
    return c == ' ' || c == '\t' || c == '\n';
}

static int getfields(char *cp, char **array, int arrayelts) {
    int nsubs = 0;
    while (*cp) {
        while (is_space(*cp)) {
            ++cp;
        }

        if (*cp == '\0' || *cp == '#') {
            break;
        }

        char *dstart = cp;
        bool in_quotes = false;
        while (*cp && (in_quotes || (*cp != '#' && !is_space(*cp)))) {
            if (*cp == '"') {
                in_quotes = !in_quotes;
            } else {
                *cp = *cp;
            }
            cp++;
        }

        if (in_quotes) {
            handle_error("Odd number of quotation marks");
        }

        if (*cp) {
            *cp++ = '\0';
        }

        if (nsubs >= arrayelts) {
            handle_error("Too many input fields");
        }

        array[nsubs++] = dstart + (*dstart == '-' && *(dstart + 1) == '\0');
    }

    return nsubs;
}

ATTRIBUTE_NORETURN static void time_overflow(void) {
    if (error(_("time overflow")) != 0) {
        exit(EXIT_FAILURE);
    }
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
    zic_t dayoff = 0;
    zic_t t, y;
    int m, i;
    int yrem;

    if (wantedy == ZIC_MIN) return min_time;
    if (wantedy == ZIC_MAX) return max_time;

    m = TM_JANUARY;
    y = EPOCH_YEAR;

    yrem = wantedy % YEARSPERREPEAT - y % YEARSPERREPEAT;
    dayoff = ((wantedy / YEARSPERREPEAT - y / YEARSPERREPEAT + yrem / YEARSPERREPEAT - (yrem % YEARSPERREPEAT < 0)) * DAYSPERREPEAT);
    wantedy = y + (yrem + 2 * YEARSPERREPEAT) % YEARSPERREPEAT;

    while (wantedy != y) {
        i = len_years[isleap(y)];
        dayoff = oadd(dayoff, i);
        y++;
    }
    while (m != rp->r_month) {
        i = len_months[isleap(y)][m];
        dayoff = oadd(dayoff, i);
        ++m;
    }
    
    i = rp->r_dayofmonth - 1;
    if (m == TM_FEBRUARY && i == 28 && !isleap(y) && rp->r_dycode == DC_DOWLEQ) {
        --i;
    } else if (m == TM_FEBRUARY && i == 28 && !isleap(y)) {
        error(_("use of 2/29 in non leap-year"));
        exit(EXIT_FAILURE);
    }

    dayoff = oadd(dayoff, i);

    if (rp->r_dycode == DC_DOWGEQ || rp->r_dycode == DC_DOWLEQ) {
        zic_t wday = (EPOCH_WDAY + dayoff % DAYSPERWEEK + DAYSPERWEEK) % DAYSPERWEEK;
        while (wday != rp->r_wday) {
            if (rp->r_dycode == DC_DOWGEQ) {
                dayoff = oadd(dayoff, 1);
                wday = (wday + 1) % DAYSPERWEEK;
                ++i;
            } else {
                dayoff = oadd(dayoff, -1);
                wday = (wday - 1 + DAYSPERWEEK) % DAYSPERWEEK;
                --i;
            }
        }
        if (i < 0 || i >= len_months[isleap(y)][m]) {
            if (noise)
                warning(_("rule goes past start/end of month; will not work with pre-2004 versions of zic"));
        }
    }

    if (dayoff < min_time / SECSPERDAY) return min_time;
    if (dayoff > max_time / SECSPERDAY) return max_time;
    t = (zic_t) dayoff * SECSPERDAY;
    return tadd(t, rp->r_tod);
}

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define GRANDPARENTED "Grandparented"
#define ZIC_MAX_ABBR_LEN_WO_WARN 3
#define TZ_MAX_CHARS 128
#define _(x) x

static int charcnt = 0;
static char chars[TZ_MAX_CHARS];
static int noise = 1;

void warning(const char *format, const char *msg) {
    fprintf(stderr, format, msg);
}

void error(const char *msg) {
    fprintf(stderr, "%s\n", msg);
}

static void newabbr(const char *string) {
    if (strcmp(string, GRANDPARENTED) == 0) {
        return;
    }

    const char *cp = string;
    while (isalpha(*cp) || isdigit(*cp) || *cp == '-' || *cp == '+') {
        ++cp;
    }

    const char *warningMsg = NULL;
    size_t length = cp - string;

    if (noise && length < 3) {
        warningMsg = "time zone abbreviation has fewer than 3 characters";
    } else if (length > ZIC_MAX_ABBR_LEN_WO_WARN) {
        warningMsg = "time zone abbreviation has too many characters";
    } else if (*cp != '\0') {
        warningMsg = "time zone abbreviation differs from POSIX standard";
    }

    if (warningMsg) {
        warning("%s (%s)\n", warningMsg, string);
    }

    size_t totalLength = strlen(string) + 1;
    if (charcnt + totalLength > TZ_MAX_CHARS) {
        error("too many, or too long, time zone abbreviations");
        exit(EXIT_FAILURE);
    }

    strcpy(&chars[charcnt], string);
    charcnt += totalLength;
}

/* Ensure that the directories of ARGNAME exist, by making any missing
   ones.  If ANCESTORS, do this only for ARGNAME's ancestors; otherwise,
   do it for ARGNAME too.  Exit with failure if there is trouble.
   Do not consider an existing file to be trouble.  */
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#define MKDIR_UMASK 0755

static void mkdirs(const char *argname, bool ancestors) {
    char *name = estrdup(argname);
    char *cp = name;

    while (*cp == '/') {
        cp++;
    }

    while (true) {
        cp = strchr(cp, '/');
        if (cp) {
            *cp = '\0';
        }

        if (mkdir(name, MKDIR_UMASK) != 0) {
            int err = errno;
            if (!(err == EEXIST || err == ENOSYS || err == EACCES)) {
                if (err == ELOOP || err == ENAMETOOLONG || err == ENOENT || err == ENOTDIR) {
                    error(_("%s: Can't create directory %s: %s"), progname, name, strerror(err));
                    exit(EXIT_FAILURE);
                }
            }
        }

        if (!cp || !ancestors) {
            break;
        }

        *cp++ = '/';
    }

    free(name);
}
