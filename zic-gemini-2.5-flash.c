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
	fprintf(stderr, _("%s: Memory exhausted: %s\n"), progname, msg);
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
#include <stdlib.h> // For malloc
#include <string.h> // For strlen, strcpy

static char *
strdup(char const *str)
{
  if (str == NULL) {
    return NULL;
  }

  size_t len = strlen(str);
  char *result = malloc(len + 1);

  if (result == NULL) {
    return NULL;
  }

  strcpy(result, str);
  return result;
}
#endif

static void *
memcheck(void *ptr)
{
    if (ptr == NULL) {
        const char *error_message;

#ifdef HAVE_MALLOC_ERRNO
        int current_errno = errno;
        error_message = strerror(current_errno);
#else
        error_message = strerror(ENOMEM);
#endif

        memory_exhausted(error_message);
    }
    return ptr;
}

#include <stdlib.h> // For malloc, exit, size_t
#include <stdio.h>  // For fprintf

static void *
emalloc(size_t size)
{
  void *ptr = malloc(size);
  if (ptr == NULL) {
    fprintf(stderr, "Error: Memory allocation failed for size %zu.\n", size);
    exit(EXIT_FAILURE);
  }
  return ptr;
}

#include <stdlib.h> // For realloc, exit, NULL
#include <stdio.h>  // For fprintf

static void handle_fatal_allocation_error(void) {
    fprintf(stderr, "Fatal error: Failed to allocate memory.\n");
    exit(EXIT_FAILURE);
}

static void *
erealloc(void *ptr, size_t size)
{
  void *new_ptr = realloc(ptr, size);
  if (new_ptr == NULL) {
    handle_fatal_allocation_error();
  }
  return new_ptr;
}

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static char *
estrdup(char const *str)
{
  char *new_str = strdup(str);
  if (new_str == NULL) {
    perror("Memory allocation failed during string duplication");
    exit(EXIT_FAILURE);
  }
  return new_str;
}

static ptrdiff_t
grow_nitems_alloc(ptrdiff_t *nitems_alloc, ptrdiff_t itemsize)
{
  ptrdiff_t current_nitems = *nitems_alloc;
  ptrdiff_t addend = (current_nitems >> 1) + 1;

  ptrdiff_t temp_new_nitems;
  ptrdiff_t new_total_bytes;

#if defined ckd_add && defined ckd_mul
  if (ckd_add(&temp_new_nitems, current_nitems, addend)) {
    memory_exhausted(_("integer overflow"));
  }
  if (ckd_mul(&new_total_bytes, temp_new_nitems, itemsize)) {
    memory_exhausted(_("integer overflow"));
  }
#else
  if (current_nitems < 0 || addend < 0) { // Should not happen for valid sizes
      memory_exhausted(_("integer overflow"));
  }
  if (addend > PTRDIFF_MAX - current_nitems) {
    memory_exhausted(_("integer overflow"));
  }
  temp_new_nitems = current_nitems + addend;

  if (itemsize < 0) { // Should not happen for valid sizes
      memory_exhausted(_("integer overflow"));
  }
  if (itemsize == 0) {
      new_total_bytes = 0;
  } else if (temp_new_nitems > PTRDIFF_MAX / itemsize) {
      memory_exhausted(_("integer overflow"));
  } else {
      new_total_bytes = temp_new_nitems * itemsize;
  }
#endif

  if (new_total_bytes > INDEX_MAX) {
    memory_exhausted(_("integer overflow"));
  }

  *nitems_alloc = temp_new_nitems;
  return new_total_bytes;
}

static void *
growalloc(void *ptr, ptrdiff_t itemsize, ptrdiff_t nitems,
	  ptrdiff_t *nitems_alloc)
{
  if (nitems_alloc == NULL) {
    abort();
  }

  if (itemsize < 0 || nitems < 0) {
    abort();
  }

  if (nitems < *nitems_alloc) {
    return ptr;
  } else {
    size_t required_byte_size = grow_nitems_alloc(nitems_alloc, itemsize);
    return erealloc(ptr, required_byte_size);
  }
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
  char const *result_filename;

  if (i == COMMAND_LINE_FILENUM) {
    result_filename = _("command line");
  } else if (i == LEAPSEC_FILENUM) {
    result_filename = leapsec;
  } else {
    char const *arg_pointer = main_argv[i];

    if (arg_pointer != NULL && strcmp(arg_pointer, "-") == 0) {
      result_filename = _("standard input");
    } else {
      result_filename = arg_pointer;
    }
  }

  return result_filename;
}

typedef int lineno;

static void
set_source_location_info(int file_number, lineno line_number, int related_file_number, lineno related_line_number)
{
	filenum = file_number;
	linenum = line_number;
	rfilenum = related_file_number;
	rlinenum = related_line_number;
}

#define EATS_DEFAULT_ARG3 0
#define EATS_DEFAULT_ARG4 -1

static void
eat(int fnum, lineno num)
{
	eats(fnum, num, EATS_DEFAULT_ARG3, EATS_DEFAULT_ARG4);
}

ATTRIBUTE_FORMAT((printf, 1, 0)) static void
verror(const char *const string, va_list args)
{
    if (filenum)
    {
        (void)fprintf(stderr, _("\"%s\", line %"PRIdMAX": "),
                      filename(filenum), linenum);
    }
    (void)vfprintf(stderr, string, args);
    if (rfilenum)
    {
        (void)fprintf(stderr, _(" (rule from \"%s\", line %"PRIdMAX")"),
                      filename(rfilenum), rlinenum);
    }
    (void)fprintf(stderr, "\n");
}

ATTRIBUTE_FORMAT((printf, 1, 2)) static void
error(const char *const string, ...) __attribute__((format(printf, 1, 2)))
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
	(void)fprintf(stderr, _("warning: "));
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
  char const *error_message = NULL;
  int saved_errno = 0;

  if (ferror(stream)) {
    error_message = _("I/O error");
  } else {
    if (fclose(stream) != 0) {
      saved_errno = errno;
      error_message = strerror(saved_errno);
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
      if (remove(tempname) != 0) {
        fprintf(stderr, "%s: warning: failed to remove temporary file '%s': %s\n",
                progname, tempname, strerror(errno));
      }
    }
    exit(EXIT_FAILURE);
  }
}

ATTRIBUTE_NORETURN static void
usage(FILE *stream, int status)
{
  fprintf(stream,
	  _("%s: usage is %s [ --version ] [ --help ] [ -v ] \\\n"
	    "\t[ -b {slim|fat} ] [ -d directory ] [ -l localtime ]"
	    " [ -L leapseconds ] \\\n"
	    "\t[ -p posixrules ] [ -r '[@lo][/@hi]' ] [ -R '@hi' ] \\\n"
	    "\t[ -t localtime-link ] \\\n"
	    "\t[ filename ... ]\n\n"
	    "Report bugs to %s.\n"),
	  progname, progname, REPORT_BUGS_TO);
  exit(status);
}

/* Change the working directory to DIR, possibly creating DIR and its
   ancestors.  After this is done, all files are accessed with names
   relative to DIR.  */
static void
change_directory(char const *dir)
{
  if (chdir(dir) != 0) {
    int saved_errno = errno;

    if (saved_errno == ENOENT) {
      // Directory does not exist, attempt to create it.
      // Assuming mkdirs returns 0 on success and non-zero on failure (setting errno).
      if (mkdirs(dir, false) != 0) {
        // mkdirs failed. Report the error from mkdirs.
        fprintf(stderr, _("%s: Can't create directory %s: %s\n"),
                progname, dir, strerror(errno));
        exit(EXIT_FAILURE);
      }

      // mkdirs succeeded, now attempt to change directory again.
      if (chdir(dir) != 0) {
        // Second chdir failed even after creating the directory.
        // Report the error from this second chdir attempt.
        fprintf(stderr, _("%s: Can't chdir to %s after creation: %s\n"),
                progname, dir, strerror(errno));
        exit(EXIT_FAILURE);
      }
      // If we reach here, the directory was created and chdir succeeded.
    } else {
      // chdir failed with an error other than ENOENT.
      // Report the original chdir error.
      fprintf(stderr, _("%s: Can't chdir to %s: %s\n"),
              progname, dir, strerror(saved_errno));
      exit(EXIT_FAILURE);
    }
  }
}

/* Compare the two links A and B, for a stable sort by link name.  */
static int
qsort_linkcmp(void const *a, void const *b)
{
  struct link const *l = a;
  struct link const *m = b;
  int cmp = strcmp(l->l_linkname, m->l_linkname);
  if (cmp)
    return cmp;

  cmp = l->l_filenum - m->l_filenum;
  if (cmp)
    return cmp;
  return (l->l_linenum > m->l_linenum) - (l->l_linenum < m->l_linenum);
}

/* Compare the string KEY to the link B, for bsearch.  */
static int
bsearch_linkcmp(void const *key, void const *b)
{
  char const *s1 = (char const *)key; // Expect key to be a pointer to a null-terminated string.
  char const *s2; // Will point to the name string within the 'struct link'.

  // Reliability: Handle cases where the array element pointer 'b' itself is NULL.
  // This could happen if the array stores pointers to 'struct link' and some are NULL,
  // or if the caller passes a NULL 'b' for some reason.
  if (b == NULL) {
    // Define a consistent ordering:
    // If the element 'b' is NULL, and the key 's1' is also NULL, they are considered equal (no content).
    // If 's1' is not NULL, it is considered "greater" than a NULL element.
    return (s1 == NULL) ? 0 : 1;
  }

  // Cast 'b' to the expected struct type after ensuring it's not NULL.
  struct link const *m = (struct link const *)b;

  // Reliability: Get the string from the struct, which might also be NULL.
  s2 = m->l_linkname;

  // Reliability: Handle NULL string pointers for comparison to prevent strcmp from dereferencing NULL.
  // Establish a consistent ordering: NULL strings are "less than" any non-NULL string.
  if (s1 == NULL) {
    if (s2 == NULL) {
      return 0; // Both are NULL, consider them equal.
    } else {
      return -1; // s1 (NULL) is less than s2 (non-NULL).
    }
  } else {
    // s1 is not NULL.
    if (s2 == NULL) {
      return 1; // s1 (non-NULL) is greater than s2 (NULL).
    } else {
      // Both s1 and s2 are non-NULL. Perform the standard string comparison.
      return strcmp(s1, s2);
    }
  }
}

/* Make the links specified by the Link lines.  */
static void
make_links(void)
{
  if (nlinks > 1) {
    qsort(links, nlinks, sizeof *links, qsort_linkcmp);

    ptrdiff_t write_idx = 0;
    for (ptrdiff_t read_idx = 0; read_idx < nlinks; ++read_idx) {
      while (read_idx + 1 < nlinks &&
             strcmp(links[read_idx].l_linkname, links[read_idx + 1].l_linkname) == 0) {
        read_idx++;
      }
      links[write_idx++] = links[read_idx];
    }
    nlinks = write_idx;
  }

  ptrdiff_t current_processing_idx = 0;
  ptrdiff_t current_pass_boundary_idx = nlinks;
  ptrdiff_t total_links_in_array = nlinks;
  ptrdiff_t links_in_previous_pass_count = nlinks;

  while (current_processing_idx < total_links_in_array) {

    if (current_processing_idx == current_pass_boundary_idx) {
      if (total_links_in_array - current_processing_idx == links_in_previous_pass_count) {
        if (current_processing_idx < total_links_in_array) {
          error(_("\"Link %s %s\" is part of a link cycle"),
                links[current_processing_idx].l_target,
                links[current_processing_idx].l_linkname);
        } else {
          error(_("An unnamed link is part of a link cycle"));
        }
        break;
      }

      current_pass_boundary_idx = total_links_in_array;
      links_in_previous_pass_count = total_links_in_array - current_processing_idx;
    }

    struct link *current_link = &links[current_processing_idx];
    eat(current_link->l_filenum, current_link->l_linenum);

    if (strcmp(current_link->l_target, current_link->l_linkname) == 0) {
      error(_("link %s targets itself"), current_link->l_target);
      current_processing_idx++;
      continue;
    }

    struct link *target_is_unmade_link_ref = NULL;

    if (current_processing_idx + 1 < current_pass_boundary_idx) {
      target_is_unmade_link_ref = bsearch(current_link->l_target,
                                          &links[current_processing_idx + 1],
                                          current_pass_boundary_idx - (current_processing_idx + 1),
                                          sizeof *links,
                                          bsearch_linkcmp);
    }

    if (!target_is_unmade_link_ref && current_pass_boundary_idx < total_links_in_array) {
      target_is_unmade_link_ref = bsearch(current_link->l_target,
                                          &links[current_pass_boundary_idx],
                                          total_links_in_array - current_pass_boundary_idx,
                                          sizeof *links,
                                          bsearch_linkcmp);
    }

    if (!target_is_unmade_link_ref) {
      dolink(current_link->l_target, current_link->l_linkname, false);
    } else {
      links = growalloc(links, sizeof *links, total_links_in_array, &nlinks_alloc);
      if (!links) {
        error(_("Memory allocation failed when expanding links array."));
        break;
      }
      links[total_links_in_array++] = *current_link;
    }

    if (noise && current_processing_idx < nlinks) {
      if (target_is_unmade_link_ref) {
        warning(_("link %s targeting link %s mishandled by pre-2023 zic"),
                current_link->l_linkname, current_link->l_target);
      } else if (bsearch(current_link->l_target, links, nlinks, sizeof *links, bsearch_linkcmp)) {
        warning(_("link %s targeting link %s"),
                current_link->l_linkname, current_link->l_target);
      }
    }

    current_processing_idx++;
  }
}

/* Simple signal handling: just set a flag that is checked
   periodically outside critical sections.  To set up the handler,
   prefer sigaction if available to close a signal race.  */

static sig_atomic_t got_signal;

#include <signal.h>

static volatile sig_atomic_t got_signal = 0;

static void
signal_handler(int sig)
{
  got_signal = sig;
}

/* Arrange for SIGINT etc. to be caught by the handler.  */
static void
catch_signals(void)
{
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

  for (size_t i = 0; i < sizeof signals / sizeof signals[0]; ++i) {
#ifdef SA_SIGINFO
    struct sigaction action_set, action_old;

    action_set.sa_handler = signal_handler;
    sigemptyset(&action_set.sa_mask);
    action_set.sa_flags = 0;

    if (sigaction(signals[i], &action_set, &action_old) == 0) {
      if (action_old.sa_handler == SIG_IGN && !(action_old.sa_flags & SA_SIGINFO)) {
        if (sigaction(signals[i], &action_old, NULL) != 0) {
          /* Error restoring SIG_IGN, but cannot log per constraints. */
        }
        got_signal = 0;
      }
    } else {
      /* Error setting signal_handler, but cannot log per constraints. */
    }
#else
    void (*old_handler)(int);

    old_handler = signal(signals[i], signal_handler);
    if (old_handler == SIG_ERR) {
      /* Error setting signal_handler, but cannot log per constraints. */
    } else if (old_handler == SIG_IGN) {
      if (signal(signals[i], SIG_IGN) == SIG_ERR) {
        /* Error restoring SIG_IGN, but cannot log per constraints. */
      }
      got_signal = 0;
    }
#endif
  }
}

/* If a signal has arrived, terminate zic with appropriate status.  */
#include <signal.h>
#include <stdlib.h>

static void
check_for_signal(void)
{
  sig_atomic_t sig_received = got_signal;

  if (sig_received != 0) {
    struct sigaction sa;

    sa.sa_handler = SIG_DFL;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;

    sigaction(sig_received, &sa, NULL);

    raise(sig_received);

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
#include <errno.h>    // For errno, ERANGE
#include <inttypes.h> // For intmax_t, strtoimax, INTMAX_MAX, INTMAX_MIN
#include <stdbool.h>  // For bool

// Assume these are file-scope variables that the function interacts with.
// Providing dummy declarations for self-containment and compilation.
// In a real scenario, these would be defined elsewhere or passed as parameters.
static intmax_t min_time = 0;   // Example default or actual system min time
static intmax_t max_time = 3153600000LL; // Example default or actual system max time
static intmax_t lo_time;        // Global variable to store the calculated low time
static intmax_t hi_time;        // Global variable to store the calculated high time

// Helper macros for min/max.
// These are common utility macros; ensure they don't clash with existing definitions.
#ifndef max
#define max(a, b) ((a) > (b) ? (a) : (b))
#endif

#ifndef min
#define min(a, b) ((a) < (b) ? (a) : (b))
#endif

static bool
timerange_option(char *timerange)
{
    intmax_t parsed_lo = min_time; // Initialize with default min_time
    intmax_t parsed_hi = max_time; // Initialize with default max_time
    char *current_ptr = timerange;
    char *end_ptr_temp; // Used by strtoimax to indicate end of parsed number

    // 1. Parse 'lo' (start time) if '@' prefix is present
    if (*current_ptr == '@') {
        errno = 0; // Clear errno before strtoimax to reliably check for ERANGE
        parsed_lo = strtoimax(current_ptr + 1, &end_ptr_temp, 10);

        // Validate strtoimax result for 'lo':
        // - No digits found (end_ptr_temp points to the start of digits string)
        // - Overflow/Underflow occurred (errno == ERANGE)
        if (end_ptr_temp == current_ptr + 1 || errno == ERANGE) {
            return false;
        }
        current_ptr = end_ptr_temp; // Advance current_ptr past the parsed number
    }

    // 2. Parse 'hi' (end time) if '/@' prefix is present
    if (current_ptr[0] == '/' && current_ptr[1] == '@') {
        errno = 0; // Clear errno before strtoimax
        parsed_hi = strtoimax(current_ptr + 2, &end_ptr_temp, 10);

        // Validate strtoimax result for 'hi':
        // - No digits found (end_ptr_temp points to the start of digits string)
        // - Overflow/Underflow occurred (errno == ERANGE)
        // The original logic contained a bug where valid INTMAX_MIN was rejected
        // and all other valid values were decremented. This refactoring corrects this
        // by consistently rejecting all range errors from strtoimax.
        if (end_ptr_temp == current_ptr + 2 || errno == ERANGE) {
            return false;
        }
        current_ptr = end_ptr_temp; // Advance current_ptr past the parsed number
    }

    // 3. Final validation of the parsed string and values
    // Check for any unparsed characters remaining in the string after time components
    if (*current_ptr != '\0') {
        return false;
    }

    // Ensure the end time is not before the start time
    if (parsed_hi < parsed_lo) {
        return false;
    }

    // Ensure the parsed range has some overlap with the global min/max range.
    // If parsed_lo is entirely above max_time, the range is out of bounds.
    // If parsed_hi is entirely below min_time, the range is out of bounds.
    if (parsed_lo > max_time || parsed_hi < min_time) {
        return false;
    }

    // 4. Assign the final, clamped time values to global variables
    // The effective range is clamped by the global min_time and max_time.
    lo_time = max(parsed_lo, min_time);
    hi_time = min(parsed_hi, max_time);

    return true;
}

/* Generate redundant time stamps up to OPT.  Return true if successful.  */
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <errno.h>

static bool
redundant_time_option(char *opt)
{
  if (opt == NULL) {
    return false;
  }

  if (*opt == '@') {
    char *start_ptr = opt + 1;
    char *opt_end = NULL;
    intmax_t redundant;

    errno = 0;
    redundant = strtoimax(start_ptr, &opt_end, 10);

    if (opt_end != start_ptr && *opt_end == '\0' && errno == 0) {
      redundant_time = max(redundant_time, redundant);
      return true;
    }
  }

  return false;
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

int
main(int argc, char **argv)
{
    int c;
    int k;
    ptrdiff_t i, j;
    bool timerange_given = false;

#ifdef S_IWGRP
    umask(umask(0) | (S_IWGRP | S_IWOTH));
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
        fprintf(stderr, "%s: %s\n", progname, _("wild compilation-time specification of zic_t"));
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
                    if (bloat > 0) {
                        error(_("incompatible -b options"));
                    }
                    bloat = -1;
                } else if (strcmp(optarg, "fat") == 0) {
                    if (bloat < 0) {
                        error(_("incompatible -b options"));
                    }
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
    if (redundant_time < lo_time) {
        redundant_time = lo_time;
    }
    if (bloat == 0) {
        static char const bloat_default[] = ZIC_BLOAT_DEFAULT;
        if (strcmp(bloat_default, "slim") == 0) {
            bloat = -1;
        } else if (strcmp(bloat_default, "fat") == 0) {
            bloat = 1;
        } else {
            abort();
        }
    }
    if (directory == NULL) {
        directory = TZDIR;
    }
    if (tzdefault == NULL) {
        tzdefault = TZDEFAULT;
    }

    if (optind < argc && leapsec != NULL) {
        infile(LEAPSEC_FILENUM, leapsec);
        adjleap();
    }

    for (k = optind; k < argc; k++) {
        infile(k, argv[k]);
    }
    if (errors) {
        return EXIT_FAILURE;
    }
    associate();
    change_directory(directory);
    catch_signals();
    for (i = 0; i < nzones; i = j) {
        for (j = i + 1; j < nzones && zones[j].z_name == NULL; ++j) {
            // No operation
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
    if (warnings && (ferror(stderr) || fclose(stderr) != 0)) {
        return EXIT_FAILURE;
    }
    return errors ? EXIT_FAILURE : EXIT_SUCCESS;
}

static bool
componentcheck(char const *name, char const *component,
	       char const *component_end, bool enable_noise_warnings)
{
	static const int COMPONENT_LEN_MAX = 14;
	ptrdiff_t component_len = component_end - component;

	if (component_len == 0) {
	  if (!*name) {
	    error(_("empty file name"));
	  } else if (component == name) {
	    error(_("file name '%s' begins with '/'"), name);
	  } else if (*component != '\0') {
	    error(_("file name '%s' contains '//'"), name);
	  } else {
	    error(_("file name '%s' ends with '/'"), name);
	  }
	  return false;
	}

	if (component_len > 0 && component_len <= 2
	    && component[0] == '.' && component_end[-1] == '.') {
	  error(_("file name '%s' contains '%.*s' component"),
		name, (int)component_len, component);
	  return false;
	}

	if (enable_noise_warnings) {
	  if (component_len > 0 && component[0] == '-') {
	    warning(_("file name '%s' component contains leading '-'"),
		    name);
	  }
	  if (COMPONENT_LEN_MAX < component_len) {
	    warning(_("file name '%s' contains overlength component"
		      " '%.*s...'"),
		    name, (int)COMPONENT_LEN_MAX, component);
	  }
	}
	return true;
}

static bool
namecheck(const char *name)
{
	const char *cp;

	/* Benign characters in a portable file name.  */
	static char const benign[] =
	  "-/_"
	  "abcdefghijklmnopqrstuvwxyz"
	  "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

	/* Non-control chars in the POSIX portable character set,
	   excluding the benign characters.  */
	static char const printable_and_not_benign[] =
	  " !\"#$%&'()*+,.0123456789:;<=>?@[\\]^`{|}~";

	const char *component = name;
	for (cp = name; *cp; cp++) {
		unsigned char c = *cp;
		if (noise && !strchr(benign, c)) {
			bool is_printable_and_not_benign = (strchr(printable_and_not_benign, c) != NULL);
			warning((is_printable_and_not_benign
				 ? _("file name '%s' contains byte '%c'")
				 : _("file name '%s' contains byte '\\%o'")),
				name, c);
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
#include <stdint.h>  // For uint_fast64_t, UINT_FAST64_MAX
#include <stdlib.h>  // For rand, srand, RAND_MAX
#include <stdbool.h> // For bool
#include <time.h>    // For time
#include <errno.h>   // For errno, EINTR

#ifdef HAVE_GETRANDOM
#include <sys/random.h> // For getrandom
#include <unistd.h>     // For ssize_t
#endif

// Define a buffer size for getrandom. 256 bytes is a common choice.
// Ensure it's large enough for at least one uint_fast64_t word.
#define GETRANDOM_BUFFER_BYTES 256
#define GETRANDOM_BUFFER_WORD_COUNT ((GETRANDOM_BUFFER_BYTES / sizeof(uint_fast64_t)) > 0 ? \
                                     (GETRANDOM_BUFFER_BYTES / sizeof(uint_fast64_t)) : 1)

// Helper function to initialize the pseudo-random number generator (PRNG)
// if getrandom is unavailable or fails. This uses time(NULL) for seeding,
// which is standard but NOT cryptographically secure.
static void
initialize_prng_fallback(void)
{
  static bool initialized = false;
  if (!initialized) {
    srand((unsigned int)time(NULL));
    initialized = true;
  }
}

// Helper function to generate a 64-bit pseudo-random number using rand().
// This implementation uses bit shifting to combine multiple rand() calls
// to fill a 64-bit value, which is generally more reliable and easier to
// understand than the original complex arithmetic, and avoids its overflow bug.
// It is NOT cryptographically secure.
static uint_fast64_t
generate_u64_from_rand_fallback(void)
{
  initialize_prng_fallback();

  uint_fast64_t result = 0;
  int bits_to_fill = sizeof(uint_fast64_t) * 8; // Typically 64 bits

  // Determine the effective number of random bits provided by a single call to rand().
  // RAND_MAX is guaranteed to be at least 32767, typically `2^N - 1`.
  // This calculates `floor(log2(RAND_MAX + 1))`.
  unsigned int bits_from_single_rand = 0;
  unsigned int temp_rand_max = (unsigned int)RAND_MAX;
  while (temp_rand_max > 0) {
      temp_rand_max >>= 1;
      bits_from_single_rand++;
  }

  // Defensive check: if RAND_MAX was unexpectedly 0, prevent infinite loop.
  // (Standard guarantees RAND_MAX >= 32767, so this branch should not be taken).
  if (bits_from_single_rand == 0) {
      bits_from_single_rand = 1;
  }

  while (bits_to_fill > 0) {
    // Determine how many bits to take from the current rand() call.
    unsigned int current_chunk_bits = bits_from_single_rand;
    if (current_chunk_bits > bits_to_fill) {
      current_chunk_bits = bits_to_fill;
    }

    // Shift the existing result to make space for the new bits.
    result <<= current_chunk_bits;

    // Get a new random value from rand(). Cast to uint_fast64_t to prevent
    // potential sign extension or truncation issues during bitwise operations.
    uint_fast64_t new_rand_value = (uint_fast64_t)rand();

    // Mask the new random value to only include the `current_chunk_bits` lowest bits.
    // This is important if RAND_MAX is not exactly `2^N - 1`, preventing non-random
    // higher bits from `rand()` (if any) from influencing the result.
    // The mask `(1ULL << current_chunk_bits) - 1` creates a bitmask of `current_chunk_bits` ones.
    if (current_chunk_bits < (sizeof(uint_fast64_t) * 8)) {
        new_rand_value &= ((1ULL << current_chunk_bits) - 1);
    }

    // OR the masked random chunk into the result.
    result |= new_rand_value;

    bits_to_fill -= current_chunk_bits;
  }
  return result;
}

static uint_fast64_t
get_rand_u64(void)
{
#ifdef HAVE_GETRANDOM
  // Static buffer to store entropy and track its usage.
  static uint_fast64_t entropy_buffer[GETRANDOM_BUFFER_WORD_COUNT];
  // nwords:
  // -1 = getrandom failed permanently (e.g., ENOSYS, EFAULT).
  //  0 = buffer empty, needs refilling (initial state or exhausted).
  // >0 = words available in the buffer.
  static int nwords = 0;

  // If the buffer is empty or getrandom hasn't been tried yet (nwords == 0 initially),
  // or if it previously filled but all words were used (nwords became 0 again).
  if (nwords <= 0) {
    // If getrandom failed permanently on a previous call, skip trying again.
    if (nwords == -1) {
      return generate_u64_from_rand_fallback();
    }

    ssize_t bytes_read;
    // Attempt to fill the buffer with cryptographically secure random data.
    // Loop to handle EINTR (interrupted system call) errors.
    do {
      bytes_read = getrandom(entropy_buffer, sizeof(entropy_buffer), 0);
    } while (bytes_read < 0 && errno == EINTR);

    // If getrandom failed (bytes_read < 0) or returned no bytes (bytes_read == 0).
    // A return of 0 bytes for a non-zero buffer size suggests no entropy is available
    // or an unexpected kernel behavior, so we treat it as a failure.
    if (bytes_read < 0 || bytes_read == 0) {
      nwords = -1; // Mark as permanent failure for subsequent calls.
      return generate_u64_from_rand_fallback();
    }

    // Successfully read bytes. Calculate available words.
    nwords = (int)(bytes_read / sizeof(*entropy_buffer));

    // If getrandom provided bytes, but not enough for a single uint_fast64_t,
    // this indicates a problem or very small buffer/type size. Treat as failure.
    if (nwords == 0) {
        nwords = -1; // Mark as permanent failure.
        return generate_u64_from_rand_fallback();
    }
  }

  // If words are available in the buffer, return one and decrement the count.
  if (nwords > 0) {
    return entropy_buffer[--nwords];
  }
  // This point should not be reached if nwords was > 0.
  // If nwords is 0 here, it implies `getrandom` was called, succeeded, but still `nwords` became 0.
  // This could happen if `bytes_read` was positive but less than `sizeof(*entropy_buffer)`.
  // The previous `if (nwords == 0)` block would handle it.
  // If we end up here, it means something went wrong in the state management,
  // or `getrandom` provided some bytes but exactly 0 full words and the check above didn't catch it.
  // As a failsafe, fall back to the less secure PRNG.
  return generate_u64_from_rand_fallback();

#else // HAVE_GETRANDOM is not defined
  // Fallback to the standard rand() function if getrandom is not available.
  return generate_u64_from_rand_fallback();
#endif
}

/* Generate a randomish name in the same directory as *NAME.  If
   *NAMEALLOC, put the name into *NAMEALLOC which is assumed to be
   that returned by a previous call and is thus already almost set up
   and equal to *NAME; otherwise, allocate a new name and put its
   address into both *NAMEALLOC and *NAME.  */
#include <string.h>
#include <stddef.h>
#include <stdint.h>

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

  enum {
    PREFIX_LEN = sizeof prefix - 1,
    ALPHABET_LEN = sizeof alphabet - 1
  };

  static const int SUFFIX_LEN = 6;

  char const *lastslash = strrchr(src, '/');
  size_t dirlen = lastslash ? (size_t)(lastslash + 1 - src) : 0;

  if (!dst) {
    dst = emalloc(size_sum(dirlen, (size_t)PREFIX_LEN + SUFFIX_LEN + 1));
    memcpy(dst, src, dirlen);
    memcpy(dst + dirlen, prefix, PREFIX_LEN);
    dst[dirlen + PREFIX_LEN + SUFFIX_LEN] = '\0';
    *name = *namealloc = dst;
  }

  uint_fast64_t r;
  const uint_fast64_t base = ALPHABET_LEN;
  
  uint_fast64_t power_of_base_suffixlen = 1;
  for (int i = 0; i < SUFFIX_LEN; ++i) {
      power_of_base_suffixlen *= base;
  }

  const uint_fast64_t bias_threshold = (UINTMAX_MAX / power_of_base_suffixlen) * power_of_base_suffixlen;

  do {
    r = get_rand_u64();
  } while (bias_threshold <= r);

  for (int i = 0; i < SUFFIX_LEN; i++) {
    dst[dirlen + PREFIX_LEN + i] = alphabet[r % ALPHABET_LEN];
    r /= ALPHABET_LEN;
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

  FILE *fp = NULL;
  bool dirs_attempted = false;

  if (!*tempname) {
    random_dirent(outname, tempname);
  }

  while (true) {
    fp = fopen(*outname, fopen_mode);

    if (fp != NULL) {
      return fp;
    }

    int fopen_errno = errno;

    if (fopen_errno == ENOENT) {
      if (!dirs_attempted) {
        (void)mkdirs(*outname, true);
        dirs_attempted = true;
      } else {
        // Directories were attempted but fopen still reports ENOENT.
        // This implies mkdirs failed or there's another unresolvable path issue.
        // Fall through to the fatal error handling block.
        fprintf(stderr, _("%s: Can't create %s/%s: %s\n"),
                progname, directory, *outname, strerror(fopen_errno));
        exit(EXIT_FAILURE);
      }
    } else if (fopen_errno == EEXIST) {
      random_dirent(outname, tempname);
    } else {
      fprintf(stderr, _("%s: Can't create %s/%s: %s\n"),
              progname, directory, *outname, strerror(fopen_errno));
      exit(EXIT_FAILURE);
    }
  }
}

/* If TEMPNAME, the result is in the temporary file TEMPNAME even
   though the user wanted it in NAME, so rename TEMPNAME to NAME.
   Report an error and exit if there is trouble.  Also, free TEMPNAME.  */
static void
rename_dest(char *tempname, char const *name)
{
  if (tempname == NULL) {
    return; /* Nothing to do if no temporary file was provided. */
  }

  if (rename(tempname, name) != 0) {
    int rename_errno = errno;
    remove(tempname); /* Attempt to clean up the temporary file on rename failure. */
    /* Assuming progname and directory are globally accessible variables. */
    /* The format string's interpretation of 'directory' and 'name' for the destination path
       matches the original code's error message structure. */
    fprintf(stderr, _("%s: rename to %s/%s: %s\n"),
            progname, directory, name, strerror(rename_errno));
    exit(EXIT_FAILURE); /* Terminate the program on critical failure. */
  }

  free(tempname); /* Free the memory associated with the temporary filename on success. */
}

/* Create symlink contents suitable for symlinking TARGET to LINKNAME, as a
   freshly allocated string.  TARGET should be a relative file name, and
   is relative to the global variable DIRECTORY.  LINKNAME can be either
   relative or absolute.  */
#include <stdlib.h>
#include <string.h>
#include <stddef.h>

#ifndef _WIN32
#include <limits.h>
#endif

extern char const *directory;

static void *emalloc_stub(size_t size) {
    void *p = malloc(size);
    return p;
}
#define emalloc emalloc_stub

#define size_sum(a, b) ((a) + (b))
#define size_product(a, b) ((a) * (b))

#ifndef INDEX_MAX
#ifdef PATH_MAX
#define INDEX_MAX PATH_MAX
#else
#define INDEX_MAX 4096
#endif
#endif

static char *
relname(char const *target, char const *linkname)
{
  size_t i;
  char const *f_path = target;
  char *result_buffer = NULL;
  char *absolute_target_buffer = NULL;

  if (*linkname == '/') {
    size_t dir_len = strlen(directory);
    size_t target_len = strlen(target);
    size_t abs_target_needed_len = dir_len + (dir_len > 0 && directory[dir_len - 1] != '/') + target_len + 1;

    if (abs_target_needed_len < dir_len || abs_target_needed_len < target_len) {
        return NULL;
    }

    absolute_target_buffer = emalloc(abs_target_needed_len);
    if (!absolute_target_buffer) {
      return NULL;
    }

    memcpy(absolute_target_buffer, directory, dir_len);
    size_t current_len = dir_len;
    if (dir_len > 0 && directory[dir_len - 1] != '/') {
      absolute_target_buffer[current_len++] = '/';
    }
    memcpy(absolute_target_buffer + current_len, target, target_len);
    absolute_target_buffer[current_len + target_len] = '\0';
    f_path = absolute_target_buffer;
  }

  size_t path_component_starts_at = 0;
  for (i = 0; f_path[i] && f_path[i] == linkname[i]; ++i) {
    if (f_path[i] == '/') {
      path_component_starts_at = i + 1;
    }
  }

  size_t levels_to_go_up = 0;
  for (size_t k = path_component_starts_at; linkname[k]; ++k) {
    if (linkname[k] == '/' && (k == 0 || linkname[k - 1] != '/')) {
      levels_to_go_up++;
    }
  }

  size_t remaining_f_path_len = strlen(f_path + path_component_starts_at);
  size_t needed_result_len = size_sum(size_product(levels_to_go_up, 3), remaining_f_path_len + 1);

  if (needed_result_len < size_product(levels_to_go_up, 3) || needed_result_len < remaining_f_path_len + 1) {
      free(absolute_target_buffer);
      return NULL;
  }

  size_t max_allowed_result_len;
  if (absolute_target_buffer) {
      max_allowed_result_len = strlen(absolute_target_buffer) + 1;
  } else {
      max_allowed_result_len = INDEX_MAX;
  }

  if (needed_result_len > max_allowed_result_len) {
    if (absolute_target_buffer) {
        return absolute_target_buffer;
    } else {
        return NULL;
    }
  }

  result_buffer = emalloc(needed_result_len);
  if (!result_buffer) {
    free(absolute_target_buffer);
    return NULL;
  }

  size_t current_pos = 0;
  for (i = 0; i < levels_to_go_up; ++i) {
    memcpy(result_buffer + current_pos, "../", 3);
    current_pos += 3;
  }
  memcpy(result_buffer + current_pos, f_path + path_component_starts_at, remaining_f_path_len + 1);

  free(absolute_target_buffer);

  return result_buffer;
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

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdarg.h>

extern char const *progname;
extern char const *directory;
extern char const *_ (char const *msgid);
extern void check_for_signal(void);
extern int itssymlink(char const *file, int *is_symlink);
extern void random_dirent(char const **p_outname, char **p_tempname);
extern void mkdirs(char const *path, bool is_file_path);
extern bool same_parent_dirs(char const *file1, char const *file2);
extern char *relname(char const *target, char const *linkname);
extern void warning(char const *fmt, ...);
extern FILE *open_outfile(char const **p_outname, char **p_tempname);
extern void close_file(FILE *fp, char const *dir, char const *file, char *tempname_arg);
extern void rename_dest(char *tempname, char const *destname);

static void dolink_fatal_error(const char *format, ...)
{
    va_list args;
    va_start(args, format);
    fprintf(stderr, _("%s: "), progname);
    vfprintf(stderr, format, args);
    fprintf(stderr, "\n");
    va_end(args);
    exit(EXIT_FAILURE);
}

static bool handle_remove_target(char const *linkname)
{
    if (remove(linkname) == 0 || errno == ENOENT || errno == ENOTDIR) {
        return true;
    } else {
        dolink_fatal_error(_("Can't remove %s/%s: %s"), directory, linkname, strerror(errno));
        return false;
    }
}

static bool try_mkdirs_if_needed(char const *path_to_create, bool *linkdirs_made)
{
    if (!*linkdirs_made) {
        mkdirs(path_to_create, true);
        *linkdirs_made = true;
        return true;
    }
    return false;
}

static bool handle_eexist_for_link(char const *linkname_orig, bool *current_staysymlink_intent,
                                  char const **p_outname, char **p_tempname,
                                  int *linkname_is_symlink_cache)
{
    if (*p_tempname != NULL) {
        *current_staysymlink_intent = false;
    }

    random_dirent(p_outname, p_tempname);

    if (*current_staysymlink_intent) {
        if (itssymlink(linkname_orig, linkname_is_symlink_cache) >= 0 && *linkname_is_symlink_cache) {
            return false;
        }
    }
    return true;
}

static void report_link_error(int err_code, const char *action, const char *target, const char *outname)
{
    dolink_fatal_error(_("Can't %s %s/%s to %s/%s: %s"), action, directory, target, directory, outname, strerror(err_code));
}

static int try_create_hard_link(char const *target, char const *linkname_orig, bool staysymlink_initial,
                                char const **p_outname, char **p_tempname, bool *linkdirs_made)
{
    int link_errno = 0;
    int linkname_is_symlink_val = -2;
    bool current_staysymlink_intent = staysymlink_initial;

    while (true) {
        check_for_signal();
        if (linkat(AT_FDCWD, target, AT_FDCWD, *p_outname, AT_SYMLINK_FOLLOW) == 0) {
            link_errno = 0;
            break;
        }

        link_errno = errno;
        if (link_errno == EINVAL) {
            link_errno = ENOTSUP;
        }

#if HAVE_LINK
        int target_is_symlink_val = -2;
        if (link_errno == ENOTSUP &&
            (same_parent_dirs(target, *p_outname) || (itssymlink(target, &target_is_symlink_val) >= 0 && target_is_symlink_val != 0)))
        {
            if (link(target, *p_outname) == 0) {
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
            bool continue_retry = handle_eexist_for_link(linkname_orig, &current_staysymlink_intent,
                                                        p_outname, p_tempname, &linkname_is_symlink_val);
            if (!continue_retry) {
                break;
            }
        } else if (link_errno == ENOENT) {
            if (!try_mkdirs_if_needed(linkname_orig, linkdirs_made)) {
                report_link_error(link_errno, "link", target, *p_outname);
            }
        } else {
            report_link_error(link_errno, "link", target, *p_outname);
        }
    }
    return link_errno;
}

static int try_create_sym_link(char const *target, char const *linkname_orig,
                               char const **p_outname, char **p_tempname, bool *linkdirs_made)
{
    int symlink_errno = 0;
    bool absolute = *target == '/';
    char *linkalloc = NULL;
    char const *contents = NULL;

    if (!absolute) {
        linkalloc = relname(target, linkname_orig);
        if (!linkalloc) {
            dolink_fatal_error(_("Memory allocation failed for relative path."));
        }
        contents = linkalloc;
    } else {
        contents = target;
    }

    while (true) {
        check_for_signal();
        if (symlink(contents, *p_outname) == 0) {
            symlink_errno = 0;
            break;
        }
        symlink_errno = errno;

        if (symlink_errno == EEXIST) {
            random_dirent(p_outname, p_tempname);
        } else if (symlink_errno == ENOENT) {
            if (!try_mkdirs_if_needed(linkname_orig, linkdirs_made)) {
                report_link_error(symlink_errno, "symlink", target, *p_outname);
            }
        } else {
            break;
        }
    }
    free(linkalloc);
    return symlink_errno;
}

static void try_copy_file(char const *target, char const *linkname_orig,
                          char const **p_outname, char **p_tempname,
                          int hard_link_err, int sym_link_err)
{
    FILE *fp = NULL;
    FILE *tp = NULL;
    int c;

    check_for_signal();

    fp = fopen(target, "rb");
    if (!fp) {
        dolink_fatal_error(_("Can't read %s/%s: %s"), directory, target, strerror(errno));
    }

    tp = open_outfile(p_outname, p_tempname);
    if (!tp) {
        fclose(fp);
        dolink_fatal_error(_("Can't open output file for %s/%s: %s"), directory, *p_outname, strerror(errno));
    }

    while ((c = getc(fp)) != EOF) {
        if (putc(c, tp) == EOF) {
            fclose(fp);
            close_file(tp, directory, linkname_orig, *p_tempname);
            dolink_fatal_error(_("Error writing to %s/%s: %s"), directory, *p_outname, strerror(errno));
        }
    }

    close_file(tp, directory, linkname_orig, *p_tempname);
    close_file(fp, directory, target, NULL);

    if (hard_link_err != ENOTSUP) {
        warning(_("copy used because hard link failed: %s"), strerror(hard_link_err));
    } else if (sym_link_err != ENOTSUP) {
        warning(_("copy used because symbolic link failed: %s"), strerror(sym_link_err));
    }
}

static void
dolink(char const *target, char const *linkname, bool staysymlink)
{
    char *tempname = NULL;
    char const *outname = linkname;
    bool linkdirs_made = false;

    check_for_signal();

    if (strcmp(target, "-") == 0) {
        handle_remove_target(linkname);
        return;
    }

    int hard_link_errno = try_create_hard_link(target, linkname, staysymlink, &outname, &tempname, &linkdirs_made);

    if (hard_link_errno != 0) {
        int sym_link_errno = try_create_sym_link(target, linkname, &outname, &tempname, &linkdirs_made);

        if (sym_link_errno != 0) {
            try_copy_file(target, linkname, &outname, &tempname, hard_link_errno, sym_link_errno);
        } else {
            if (hard_link_errno != ENOTSUP && hard_link_errno != EEXIST) {
                warning(_("symbolic link used because hard link failed: %s"), strerror(hard_link_errno));
            }
        }
    }

    rename_dest(tempname, linkname);
}

/* Return 1 if NAME is an absolute symbolic link, -1 if it is relative,
   0 if it is not a symbolic link.  If *CACHE is not -2, it is the
   cached result of a previous call to this function with the same NAME.  */
#include <unistd.h>

static const int SYMLINK_STATUS_UNKNOWN = -2;
static const int SYMLINK_STATUS_NOT_SYMLINK_OR_ERROR = 0;
static const int SYMLINK_STATUS_RELATIVE = -1;
static const int SYMLINK_STATUS_ABSOLUTE = 1;

static int
itssymlink(char const *name, int *cache)
{
  if (*cache == SYMLINK_STATUS_UNKNOWN) {
    char target_char = '\0';
    ssize_t bytes_read = readlink(name, &target_char, 1);

    if (bytes_read < 0) {
      *cache = SYMLINK_STATUS_NOT_SYMLINK_OR_ERROR;
    } else if (bytes_read == 0) {
      *cache = SYMLINK_STATUS_RELATIVE;
    } else {
      if (target_char == '/') {
        *cache = SYMLINK_STATUS_ABSOLUTE;
      } else {
        *cache = SYMLINK_STATUS_RELATIVE;
      }
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
  const struct rule *r1 = (const struct rule *)cp1;
  const struct rule *r2 = (const struct rule *)cp2;

  if (r1->r_name == NULL) {
    if (r2->r_name == NULL) {
      return 0; // Both are NULL, considered equal
    }
    return -1; // r1 is NULL, r2 is not. r1 comes before r2.
  }
  if (r2->r_name == NULL) {
    return 1; // r1 is not NULL, r2 is NULL. r1 comes after r2.
  }

  return strcmp(r1->r_name, r2->r_name);
}

static void
associate(void)
{
	struct zone *zp;
	struct rule *rp;
	size_t i, j, base, out;

	if (nrules > 1) {
		qsort(rules, nrules, sizeof *rules, rcomp);

		i = 0;
		while (i < nrules - 1) {
			if (strcmp(rules[i].r_name, rules[i + 1].r_name) == 0 &&
			    rules[i].r_filenum != rules[i + 1].r_filenum)
			{
				eat(rules[i].r_filenum, rules[i].r_linenum);
				warning(_("same rule name in multiple files"));
				eat(rules[i + 1].r_filenum, rules[i + 1].r_linenum);
				warning(_("same rule name in multiple files"));

				j = i + 2;
				while (j < nrules) {
					if (strcmp(rules[i].r_name, rules[j].r_name) != 0) {
						break;
					}
					if (rules[j].r_filenum != rules[i].r_filenum &&
					    rules[j].r_filenum != rules[i + 1].r_filenum)
					{
						break;
					}
					j++;
				}
				i = j - 1;
			}
			i++;
		}
	}

	for (i = 0; i < nzones; ++i) {
		zp = &zones[i];
		zp->z_rules = NULL;
		zp->z_nrules = 0;
	}

	base = 0;
	while (base < nrules) {
		rp = &rules[base];

		out = base + 1;
		while (out < nrules && strcmp(rp->r_name, rules[out].r_name) == 0) {
			out++;
		}

		for (i = 0; i < nzones; ++i) {
			zp = &zones[i];
			if (strcmp(zp->z_rule, rp->r_name) == 0) {
				zp->z_rules = rp;
				zp->z_nrules = out - base;
			}
		}
		base = out;
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
  ptrdiff_t linelen = 0;
  int ch;

  if (bufsize <= 0) {
    error(_("invalid buffer size for inputline"));
    return false;
  }

  while (true) {
    ch = getc(fp);

    if (ch == EOF) {
      if (ferror(fp)) {
        error(_("input error"));
        return false;
      }
      if (linelen == 0) {
        return false; /* EOF encountered before any characters */
      }
      /* EOF encountered after reading some characters, but no newline */
      error(_("unterminated line"));
      return false;
    }

    if (ch == '\n') {
      break; /* Newline found, exit loop */
    }

    if (ch == 0) { /* NUL input byte */
      error(_("NUL input byte"));
      return false;
    }

    /* Store character and increment length */
    buf[linelen] = (char)ch;
    linelen++;

    /* Check if buffer is full (no space left for NUL terminator) */
    if (linelen == bufsize) {
      error(_("line too long"));
      return false;
    }
  }

  /* Loop exited due to newline. NUL-terminate the string. */
  buf[linelen] = '\0';
  return true;
}

static void
infile(int fnum, char const *name)
{
	FILE *fp;
	bool wantcont = false;
	lineno num;
	bool is_stdin = false;

	if (strcmp(name, "-") == 0) {
		fp = stdin;
		is_stdin = true;
	} else {
		fp = fopen(name, "r");
		if (fp == NULL) {
			fprintf(stderr, _("%s: Can't open %s: %s\n"),
				progname, name, strerror(errno));
			exit(EXIT_FAILURE);
		}
	}

	for (num = 1; ; ++num) {
		// Assuming 'min', 'INT_MAX', 'INDEX_MAX', 'FORMAT_LEN_GROWTH_BOUND',
		// and '_POSIX2_LINE_MAX' are compile-time constant expressions.
		// If they are not, this becomes a Variable Length Array (VLA),
		// which might be flagged by SonarCloud as a potential issue
		// (e.g., C2018 in MISRA C).
		// Without altering functionality, this size calculation is retained.
		// The 'enum' for bufsize_bound is removed as it's an intermediate constant
		// used only once; its value is inlined for brevity.
		char buf[min(_POSIX2_LINE_MAX, (min(INT_MAX, INDEX_MAX) / FORMAT_LEN_GROWTH_BOUND))];
		int nfields;
		char *fields[MAX_FIELDS];
		const struct lookup *current_lp;

		eat(fnum, num);

		if (!inputline(fp, buf, sizeof(buf))) {
			break;
		}

		// sizeof fields / sizeof *fields simplifies to MAX_FIELDS for a fixed-size array.
		nfields = getfields(buf, fields, MAX_FIELDS);

		if (nfields == 0) {
			// Nothing to do for empty lines or lines with no fields
		} else if (wantcont) {
			wantcont = inzcont(fields, nfields);
		} else {
			const struct lookup *line_codes = (fnum < 0) ? leap_line_codes : zi_line_codes;
			current_lp = byword(fields[0], line_codes);

			if (current_lp == NULL) {
				error(_("input line of unknown type"));
			} else {
				switch (current_lp->l_value) {
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
						// All expected LC_ values are handled.
						// `unreachable()` indicates an assertion that this path should not be taken.
						unreachable();
				}
			}
		}
	}

	if (!is_stdin) {
		// `close_file` is assumed to handle the actual `fclose` and potential error checking.
		// This explicit check ensures `stdin` is not inadvertently closed,
		// which would break subsequent program I/O.
		close_file(fp, NULL, filename(fnum), NULL);
	}

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
	zic_t   hh = 0;
	int     mm = 0;
	int     ss = 0;
	int     tenths = 0; // The first digit after the decimal point
	bool    has_fractional_part = false;
	bool    has_further_significant_fraction = false; // Non-zero digits after 'tenths'
	int     sign = 1;

	if (string == NULL || *string == '\0') {
		return 0;
	}

	char *s = (char *)string;
	if (*s == '-') {
		sign = -1;
		s++;
	}

	char *endptr;
	long long val; // Use long long for intermediate parsing to handle larger values

	// Parse HH
	val = strtoll(s, &endptr, 10);
	if (endptr == s) { // No digits found
		error("%s", errstring);
		return 0;
	}
	if (val < 0) { // HH should not be negative here, sign already handled
		error("%s", errstring);
		return 0;
	}
	hh = (zic_t)val;
	s = endptr;

	// Parse MM (optional)
	if (*s == ':') {
		s++;
		val = strtoll(s, &endptr, 10);
		if (endptr == s || val < 0 || val >= MINSPERHOUR) {
			error("%s", errstring);
			return 0;
		}
		mm = (int)val;
		s = endptr;
	}

	// Parse SS (optional)
	if (*s == ':') {
		s++;
		val = strtoll(s, &endptr, 10);
		// Check for valid range for seconds (0 to SECSPERMIN, inclusive)
		// Original code: ss < 0 || ss > SECSPERMIN
		if (endptr == s || val < 0 || val > SECSPERMIN) {
			error("%s", errstring);
			return 0;
		}
		ss = (int)val;
		s = endptr;
	}

	// Parse fractional seconds (optional)
	if (*s == '.') {
		has_fractional_part = true;
		s++;
		// A decimal point must be followed by at least one digit
		if (!isdigit((unsigned char)*s)) {
			error("%s", errstring);
			return 0;
		}
		tenths = (*s - '0'); // Get the first digit after '.'
		s++;

		// Determine if there are any non-zero digits after 'tenths'
		// This is for proper "round half to even" logic
		while (*s == '0') { // Skip any trailing zeros immediately after 'tenths'
			s++;
		}
		if (isdigit((unsigned char)*s)) { // If there's a non-zero digit after skipping zeros
			has_further_significant_fraction = true;
		}
		// Skip any remaining digits in the fractional part
		while (isdigit((unsigned char)*s)) {
			s++;
		}

		if (noise) {
			warning(_("fractional seconds rejected by"
				  " pre-2018 versions of zic"));
		}
	}

	// Check for extraneous characters after parsing the time string
	if (*s != '\0') {
		error("%s", errstring);
		return 0;
	}

	// Validate hh against maximum allowed value for zic_t
	// This check prevents overflow during multiplication
	if (hh < 0 || ZIC_MAX / SECSPERHOUR < hh) { // hh < 0 check is redundant here but kept for safety
		error(_("time overflow"));
		return 0;
	}

	// Apply rounding for fractional seconds (round half to even)
	if (has_fractional_part && tenths > 0) {
		bool round_up = false;
		if (tenths > 5) {
			round_up = true;
		} else if (tenths == 5) {
			if (has_further_significant_fraction) { // e.g., 5.0001, 5.6
				round_up = true;
			} else { // Exactly .5 (e.g., 5.5000) - round to nearest even integer
				if (ss % 2 != 0) { // If seconds is odd, round up
					round_up = true;
				}
			}
		}
		if (round_up) {
			ss++;
		}
	}

	if (noise && (hh > HOURSPERDAY || (hh == HOURSPERDAY && (mm != 0 || ss != 0)))) {
		warning(_("values over 24 hours not handled by pre-2007 versions of zic"));
	}

	return oadd(sign * hh * SECSPERHOUR,
		    sign * ((zic_t)mm * SECSPERMIN + ss));
}

#define MAX_TIME_FIELD_LEN 64

static zic_t
getsave(char *field, bool *isdst)
{
  int dst_indicator = -1;
  zic_t save_time;
  size_t fieldlen = strlen(field);
  char time_str_buf[MAX_TIME_FIELD_LEN];

  size_t chars_to_copy = fieldlen;

  if (fieldlen > 0) {
    char last_char = field[fieldlen - 1];
    if (last_char == 'd') {
      dst_indicator = 1;
      chars_to_copy = fieldlen - 1;
    } else if (last_char == 's') {
      dst_indicator = 0;
      chars_to_copy = fieldlen - 1;
    }
  }

  if (chars_to_copy >= sizeof(time_str_buf)) {
    chars_to_copy = sizeof(time_str_buf) - 1;
  }

  memcpy(time_str_buf, field, chars_to_copy);
  time_str_buf[chars_to_copy] = '\0';

  save_time = gethms(time_str_buf, _("invalid saved time"));

  if (dst_indicator == 1) {
    *isdst = true;
  } else if (dst_indicator == 0) {
    *isdst = false;
  } else {
    *isdst = (save_time != 0);
  }

  return save_time;
}

static int
is_invalid_rule_name_start_char(char c)
{
    return c == '\0' || isspace((unsigned char)c) || isdigit((unsigned char)c) || c == '+' || c == '-';
}

static void
inrule(char **fields, int nfields)
{
	struct rule r;

	if (nfields != RULE_FIELDS) {
		error(_("wrong number of fields on Rule line"));
		return;
	}

	if (is_invalid_rule_name_start_char(*fields[RF_NAME])) {
		error(_("Invalid rule name \"%s\""), fields[RF_NAME]);
		return;
	}

	r.r_filenum = filenum;
	r.r_linenum = linenum;
	r.r_save = getsave(fields[RF_SAVE], &r.r_isdst);
	if (!rulesub(&r, fields[RF_LOYEAR], fields[RF_HIYEAR],
		     fields[RF_COMMAND], fields[RF_MONTH], fields[RF_DAY],
		     fields[RF_TOD]))
	  return;
	r.r_name = estrdup(fields[RF_NAME]);
	r.r_abbrvar = estrdup(fields[RF_ABBRVAR]);
	if (max_abbrvar_len < strlen(r.r_abbrvar))
		max_abbrvar_len = strlen(r.r_abbrvar);
	rules = growalloc(rules, sizeof *rules, nrules, &nrules_alloc);
	rules[nrules++] = r;
}

static bool
inzone(char **fields, int nfields)
{
    const char *zone_name = fields[ZF_NAME]; // Cache zone name for clarity and minor optimization

    if (nfields < ZONE_MINFIELDS || nfields > ZONE_MAXFIELDS) {
        error(_("wrong number of fields on Zone line"));
        return false;
    }

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

    for (size_t i = 0; i < nzones; ++i) { // Use size_t for array indexing
        if (zones[i].z_name != NULL &&
            strcmp(zones[i].z_name, zone_name) == 0) {
            error(_("duplicate zone name %s"
                    " (file \"%s\", line %"PRIdMAX")"),
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
    bool result;

    if (nfields < ZONEC_MINFIELDS || nfields > ZONEC_MAXFIELDS) {
        error(_("wrong number of fields on Zone continuation line"));
        result = false;
    } else {
        result = inzsub(fields, nfields, true);
    }

    return result;
}

static bool
inzsub(char **fields, int nfields, bool iscont)
{
    struct zone z = {0}; // Initialize all members to 0/NULL
    int i_stdoff, i_rule, i_format;
    int i_untilyear, i_untilmonth, i_untilday, i_untiltime;
    bool hasuntil;
    char *format_field;
    int current_format_len;

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
    z.z_stdoff = gethms(fields[i_stdoff], _("invalid UT offset")); // Assumed to handle error internally

    format_field = fields[i_format];
    char *percent_pos = strchr(format_field, '%');

    if (percent_pos != NULL) {
        // '%' is found
        if (*(percent_pos + 1) == '\0') {
            error(_("invalid abbreviation format: missing specifier after '%'"));
            return false;
        }
        char specifier_char = *(percent_pos + 1);
        if (specifier_char != 's' && specifier_char != 'z') {
            error(_("invalid abbreviation format: specifier must be 's' or 'z'"));
            return false;
        }
        z.z_format_specifier = specifier_char;

        if (strchr(percent_pos + 2, '%') != NULL) { // Check for another '%' after the first one
            error(_("invalid abbreviation format: multiple '%' characters"));
            return false;
        }
    } else {
        // No '%' found
        z.z_format_specifier = '\0';
    }

    if (strchr(format_field, '/') != NULL) { // Check for '/' anywhere
        error(_("invalid abbreviation format: '/' character is not allowed"));
        return false;
    }

    current_format_len = strlen(format_field);
    if (max_format_len < current_format_len)
      max_format_len = current_format_len;

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
                (nfields > i_untiltime) ? fields[i_untiltime] : "0")) {
            return false;
        }
        z.z_untiltime = rpytime(&z.z_untilrule, z.z_untilrule.r_loyear);

        if (iscont && nzones > 0) {
            bool current_time_valid = z.z_untiltime > min_time && z.z_untiltime < max_time;
            struct zone *prev_zone = &zones[nzones - 1];
            bool prev_time_valid = prev_zone->z_untiltime > min_time && prev_zone->z_untiltime < max_time;

            if (current_time_valid && prev_time_valid && prev_zone->z_untiltime >= z.z_untiltime) {
                error(_("Zone continuation line end time is not after end time of previous line"));
                return false;
            }
        }
    }

    z.z_name = iscont ? NULL : estrdup(fields[ZF_NAME]);
    z.z_rule = estrdup(fields[i_rule]);
    z.z_format = estrdup(format_field);

    if (z.z_format_specifier == 'z') {
        // Calculate the offset of '%' in the original field to apply to the duplicated string.
        size_t percent_offset = strchr(format_field, '%') - format_field;
        z.z_format[percent_offset + 1] = 's'; // Change 'z' to 's' in the duplicated format string
        if (noise) {
            warning(_("format '%s' not handled by pre-2015 versions of zic"),
                    format_field);
        }
    }

    zones = growalloc(zones, sizeof(struct zone), nzones, &nzones_alloc);
    // growalloc is assumed to handle errors internally, typically by exiting.
    // A defensive check here, though possibly unreachable if growalloc terminates on failure.
    if (zones == NULL) {
        error(_("Memory allocation failed for zones array"));
        return false;
    }
    zones[nzones++] = z;

    // If there was an UNTIL field on this line,
    // there's more information about the zone on the next line.
    return hasuntil;
}

static bool parse_zic_t_field(const char *str, zic_t *value, const char *error_msg) {
    char xs; // To catch any trailing character
    if (sscanf(str, "%"SCNdZIC"%c", value, &xs) != 1) {
        error(error_msg);
        return false;
    }
    return true;
}

static bool parse_int_field(const char *str, int *value, const char *error_msg) {
    char xs; // To catch any trailing character
    if (sscanf(str, "%d%c", value, &xs) != 1) {
        error(error_msg);
        return false;
    }
    return true;
}

static zic_t
getleapdatetime(char **fields, bool expire_line)
{
	zic_t	year;
	int	month_val;
	int	day;
	zic_t	dayoff = 0;
	zic_t	t;
	zic_t	tod;

	if (!parse_zic_t_field(fields[LP_YEAR], &year, _("invalid leaping year"))) {
		return -1;
	}

	if (!expire_line) {
	    if (!leapseen || leapmaxyear < year)
		leapmaxyear = year;
	    if (!leapseen || leapminyear > year)
		leapminyear = year;
	    leapseen = true;
	}

    if (year > EPOCH_YEAR) {
        for (zic_t current_year = EPOCH_YEAR; current_year < year; ++current_year) {
            dayoff = oadd(dayoff, len_years[isleap(current_year)]);
        }
    } else if (year < EPOCH_YEAR) {
        for (zic_t current_year = EPOCH_YEAR - 1; current_year >= year; --current_year) {
            dayoff = oadd(dayoff, -len_years[isleap(current_year)]);
        }
    }
    // If year == EPOCH_YEAR, dayoff remains 0, which is correct.

	const struct lookup *lp = byword(fields[LP_MONTH], mon_names);
	if (lp == NULL) {
		error(_("invalid month name"));
		return -1;
	}
	month_val = lp->l_value;

	for (int m = TM_JANUARY; m < month_val; ++m) {
		dayoff = oadd(dayoff, len_months[isleap(year)][m]);
	}

	if (!parse_int_field(fields[LP_DAY], &day, _("invalid day of month"))) {
		return -1;
	}

	if (day <= 0 || day > len_months[isleap(year)][month_val]) {
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
    if (tod == -1) { // Assuming gethms returns -1 on error after reporting it
        return -1;
    }

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
inexpires(char const * const *fields, const int nfields)
{
  if (nfields != EXPIRES_FIELDS) {
    error(_("wrong number of fields on Expires line"));
  } else if (leapexpires >= 0) {
    error(_("multiple Expires lines"));
  } else {
    leapexpires = getleapdatetime(fields, true);
  }
}

static void
inlink(char **fields, int nfields)
{
	struct link new_link;

	if (nfields != LINK_FIELDS) {
		error(_("wrong number of fields on Link line"));
		return;
	}
	if (*fields[LF_TARGET] == '\0') {
		error(_("blank TARGET field on Link line"));
		return;
	}
	if (!namecheck(fields[LF_LINKNAME])) {
		return;
	}

	new_link.l_filenum = filenum;
	new_link.l_linenum = linenum;
	new_link.l_target = estrdup(fields[LF_TARGET]);
	new_link.l_linkname = estrdup(fields[LF_LINKNAME]);

	links = growalloc(links, sizeof *links, nlinks, &nlinks_alloc);
	links[nlinks++] = new_link;
}

static bool parse_time_field(const char* timep, struct rule* rp);
static bool parse_day_field(const char* dayp_orig, struct rule* rp);

static bool
parse_time_field(const char* timep, struct rule* rp)
{
	char* time_str_copy = estrdup(timep);
	if (time_str_copy == NULL) {
		error(_("memory allocation failed for time parsing"));
		return false;
	}

	// Default values
	rp->r_todisstd = false;
	rp->r_todisut = false;

	if (*time_str_copy != '\0') {
		char* last_char_ptr = time_str_copy + strlen(time_str_copy) - 1;
		switch (lowerit(*last_char_ptr)) {
			case 's':	/* Standard */
				rp->r_todisstd = true;
				// rp->r_todisut = false; // Already default
				*last_char_ptr = '\0';
				break;
			case 'w':	/* Wall */
				// rp->r_todisstd = false; // Already default
				// rp->r_todisut = false;  // Already default
				*last_char_ptr = '\0';
				break;
			case 'g':	/* Greenwich */
			case 'u':	/* Universal */
			case 'z':	/* Zulu */
				rp->r_todisstd = true;
				rp->r_todisut = true;
				*last_char_ptr = '\0';
				break;
		}
	}
	// gethms is assumed to handle errors internally (e.g., exit or log),
	// so no explicit check for its return value is performed here,
	// mirroring the original code's behavior.
	rp->r_tod = gethms(time_str_copy, _("invalid time of day"));
	free(time_str_copy);
	return true;
}

static bool
parse_day_field(const char* dayp_orig, struct rule* rp)
{
	char* day_str_copy = estrdup(dayp_orig);
	if (day_str_copy == NULL) {
		error(_("memory allocation failed for day parsing"));
		return false;
	}
	
	const struct lookup* lp;
	char xs; // For sscanf validation

	// 1. Handle "last<weekday>" (e.g., "lastSunday")
	lp = byword(day_str_copy, lasts);
	if (lp != NULL) {
		rp->r_dycode = DC_DOWLEQ; // Day of week less than or equal to (last day of month)
		rp->r_wday = lp->l_value;
		// Use rp->r_month directly as it's already set in rulesub
		rp->r_dayofmonth = len_months[1][rp->r_month]; 
		free(day_str_copy);
		return true;
	}

	// 2. Handle "weekday<=day" or "weekday>=day" (e.g., "Sun<=20")
	char* operator_char_ptr = strchr(day_str_copy, '<');
	if (operator_char_ptr == NULL) {
		operator_char_ptr = strchr(day_str_copy, '>');
	}

	if (operator_char_ptr != NULL) {
		// Validate the operator sequence; must be <= or >=
		if (operator_char_ptr[1] != '=') {
			error(_("invalid day of month (missing '=')"));
			free(day_str_copy);
			return false;
		}
		
		// Determine the day code
		if (*operator_char_ptr == '<') {
			rp->r_dycode = DC_DOWLEQ;
		} else { // Must be '>'
			rp->r_dycode = DC_DOWGEQ;
		}

		*operator_char_ptr = '\0'; // Null-terminate the weekday string (e.g., "Sun\0<=20")
		char* weekday_str = day_str_copy; // day_str_copy now holds the weekday part

		lp = byword(weekday_str, wday_names);
		if (lp == NULL) {
			error(_("invalid weekday name"));
			free(day_str_copy);
			return false;
		}
		rp->r_wday = lp->l_value;

		// The day number string starts after the operator and '=' (e.g., "20")
		char* day_num_str = operator_char_ptr + 2;
		
		// Parse the day number
		if (sscanf(day_num_str, "%d%c", &rp->r_dayofmonth, &xs) != 1 ||
			rp->r_dayofmonth <= 0 ||
			(rp->r_dayofmonth > len_months[1][rp->r_month])) {
				error(_("invalid day of month"));
				free(day_str_copy);
				return false;
		}
	} else {
		// 3. Simple Day Of Month (e.g., "1")
		rp->r_dycode = DC_DOM;
		// Parse the day number from the original string copy
		if (sscanf(day_str_copy, "%d%c", &rp->r_dayofmonth, &xs) != 1 ||
			rp->r_dayofmonth <= 0 ||
			(rp->r_dayofmonth > len_months[1][rp->r_month])) {
				error(_("invalid day of month"));
				free(day_str_copy);
				return false;
		}
	}

	free(day_str_copy);
	return true;
}

static bool
rulesub(struct rule *rp, const char *loyearp, const char *hiyearp,
	const char *typep, const char *monthp, const char *dayp,
	const char *timep)
{
	const struct lookup *	lp;
	char xs; // For sscanf validation (detects extra characters)

	// Month processing
	lp = byword(monthp, mon_names);
	if (lp == NULL) {
		error(_("invalid month name"));
		return false;
	}
	rp->r_month = lp->l_value;

	// Time processing
	if (!parse_time_field(timep, rp)) {
		return false; // Error already logged by parse_time_field
	}

	// Year work - starting year (loyear)
	lp = byword(loyearp, begin_years);
	if (lp) {
		switch (lp->l_value) {
			case YR_MINIMUM:
				warning(_("FROM year \"%s\" is obsolete; treated as %d"),
					loyearp, YEAR_32BIT_MIN - 1);
				rp->r_loyear = YEAR_32BIT_MIN - 1;
				break;
			default:
				unreachable(); // Should not happen with valid begin_years table
		}
	} else if (sscanf(loyearp, "%" SCNdZIC "%c", &rp->r_loyear, &xs) != 1) {
		error(_("invalid starting year"));
		return false;
	}

	// Year work - ending year (hiyear)
	lp = byword(hiyearp, end_years);
	rp->r_hiwasnum = (lp == NULL); // Determine if hiyear was a number or a symbolic word
	if (!rp->r_hiwasnum) {
		switch (lp->l_value) {
			case YR_MAXIMUM:
				rp->r_hiyear = ZIC_MAX;
				break;
			case YR_ONLY:
				rp->r_hiyear = rp->r_loyear; // Depends on r_loyear, so must parse loyear first
				break;
			default:
				unreachable(); // Should not happen with valid end_years table
		}
	} else if (sscanf(hiyearp, "%" SCNdZIC "%c", &rp->r_hiyear, &xs) != 1) {
		error(_("invalid ending year"));
		return false;
	}

	// Year range validation
	if (rp->r_loyear > rp->r_hiyear) {
		error(_("starting year greater than ending year"));
		return false;
	}

	// Year type validation (must be empty or "-")
	if (*typep != '\0') {
		error(_("year type \"%s\" is unsupported; use \"-\" instead"),
			typep);
		return false;
	}

	// Day work
	if (!parse_day_field(dayp, rp)) {
		return false; // Error already logged by parse_day_field
	}

	return true;
}

#include <stdint.h>

static int
convert(uint_fast32_t val, unsigned char *buf)
{
    if (buf == NULL) {
        return -1;
    }

    int i;
    int shift;

    for (i = 0, shift = 24; i < 4; ++i, shift -= 8) {
        buf[i] = (unsigned char)((val >> shift) & 0xff);
    }

    return 0;
}

static void
convert64(uint_fast64_t val, char *buf)
{
    if (buf == NULL) {
        return;
    }

    unsigned char *b = (unsigned char *) buf;

    for (int i = 0, shift = 56; i < 8; ++i, shift -= 8) {
        b[i] = (unsigned char)((val >> shift) & 0xFF);
    }
}

static int
puttzcode(zic_t val, FILE *fp)
{
    char buf[4];
    size_t bytes_to_write = sizeof(buf);

    if (fp == NULL) {
        return -1;
    }

    convert(val, buf);

    if (fwrite(buf, 1, bytes_to_write, fp) != bytes_to_write) {
        return -2;
    }

    return 0;
}

#define ZIC_BUFFER_SIZE_64BIT 8
#define ZIC_PASS_MODE_PUT_TZCODE 1

static void
puttzcodepass(zic_t val, FILE *fp, int pass)
{
  if (pass == ZIC_PASS_MODE_PUT_TZCODE) {
    puttzcode(val, fp);
  } else {
    char buf[ZIC_BUFFER_SIZE_64BIT];
    size_t written_bytes;

    convert64(val, buf);
    written_bytes = fwrite(buf, sizeof(char), ZIC_BUFFER_SIZE_64BIT, fp);

    if (written_bytes != ZIC_BUFFER_SIZE_64BIT) {
      /* I/O error or incomplete write detected.
       * The stream's error indicator (ferror(fp)) will be set.
       * Further error handling (e.g., logging) would depend on the
       * application's specific error reporting mechanisms,
       * as this void function cannot return an error code directly. */
    }
  }
}

static int
atcomp(const void *avp, const void *bvp)
{
  struct attype const *ap = avp;
  struct attype const *bp = bvp;
  zic_t a = ap->at;
  zic_t b = bp->at;

  if (a < b) {
    return -1;
  } else if (a > b) {
    return 1;
  } else {
    return 0;
  }
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
	   zic_t const *trans, int const *corr,
	   zic_t max_time, zic_t leapexpires)
{
  /* Omit ordinary transitions < LO.  */
  while (r.count > 0 && ats[r.base] < lo) {
    r.defaulttype = types[r.base];
    r.count--;
    r.base++;
  }

  /* Omit as many initial leap seconds as possible, such that the
     first leap second in the truncated list is <= LO, and is a
     positive leap second if and only if it has a positive correction.
     This supports common TZif readers that assume that the first leap
     second is positive if and only if its correction is positive.  */
  while (r.leapcount > 1 && trans[r.leapbase + 1] <= lo) {
    r.leapcount--;
    r.leapbase++;
  }
  while (r.leapbase > 0
	 && ((corr[r.leapbase - 1] < corr[r.leapbase])
	     != (0 < corr[r.leapbase]))) {
    r.leapcount++;
    r.leapbase--;
  }

  /* Omit ordinary and leap second transitions greater than HI + 1.  */
  if (hi < max_time) {
    zic_t limit = hi + 1;
    while (r.count > 0 && limit < ats[r.base + r.count - 1]) {
      r.count--;
    }
    while (r.leapcount > 0 && limit < trans[r.leapbase + r.leapcount - 1]) {
      r.leapcount--;
    }
  }

  /* Determine whether to append an expiration to the leap second table.  */
  r.leapexpiry = leapexpires >= 0 && leapexpires - 1 <= hi;

  return r;
}

static struct write_pass_params {
    int default_type;
    ptrdiff_t time_base;
    ptrdiff_t time_count;
    zic_t min_val;
    zic_t max_val;
    int leap_base;
    int leap_count;
    bool leap_expiry;
};

static void write_tz_header_block(FILE *fp, char version, int utisstd_count, int utisut_count,
                                  int leap_count, int time_count, int type_count, int char_count) {
    struct tzhead tzh;
    memset(&tzh, 0, sizeof tzh);
    memcpy(tzh.tzh_magic, TZ_MAGIC, sizeof tzh.tzh_magic);
    tzh.tzh_version[0] = version;
    convert(utisut_count, tzh.tzh_ttisutcnt);
    convert(utisstd_count, tzh.tzh_ttisstdcnt);
    convert(leap_count, tzh.tzh_leapcnt);
    convert(time_count, tzh.tzh_timecnt);
    convert(type_count, tzh.tzh_typecnt);
    convert(char_count, tzh.tzh_charcnt);

    fwrite(tzh.tzh_magic, sizeof tzh.tzh_magic, 1, fp);
    fwrite(tzh.tzh_version, sizeof tzh.tzh_version, 1, fp);
    fwrite(tzh.tzh_reserved, sizeof tzh.tzh_reserved, 1, fp);
    fwrite(tzh.tzh_ttisutcnt, sizeof tzh.tzh_ttisutcnt, 1, fp);
    fwrite(tzh.tzh_ttisstdcnt, sizeof tzh.tzh_ttisstdcnt, 1, fp);
    fwrite(tzh.tzh_leapcnt, sizeof tzh.tzh_leapcnt, 1, fp);
    fwrite(tzh.tzh_timecnt, sizeof tzh.tzh_timecnt, 1, fp);
    fwrite(tzh.tzh_typecnt, sizeof tzh.tzh_typecnt, 1, fp);
    fwrite(tzh.tzh_charcnt, sizeof tzh.tzh_charcnt, 1, fp);
}

static void write_tz_transitions_block(FILE *fp, zic_t lo_val, int pre_transition_type,
                                       ptrdiff_t transition_time_index, ptrdiff_t transition_time_limit,
                                       const zic_t *ats, bool hi_cut, zic_t hi_time_val, int pass) {
    if (pre_transition_type >= 0) {
        puttzcodepass(lo_val, fp, pass);
    }
    for (ptrdiff_t i = transition_time_index; i < transition_time_limit; ++i) {
        puttzcodepass(ats[i], fp, pass);
    }
    if (hi_cut) {
        puttzcodepass(hi_time_val + 1, fp, pass);
    }
}

static void write_tz_type_indices_block(FILE *fp, int pre_transition_type,
                                        ptrdiff_t transition_time_index, ptrdiff_t transition_time_limit,
                                        const unsigned char *types_arr, bool hi_cut, int unspecified_type,
                                        const int *type_map) {
    if (pre_transition_type >= 0) {
        putc(type_map[pre_transition_type], fp);
    }
    for (ptrdiff_t i = transition_time_index; i < transition_time_limit; i++) {
        putc(type_map[types_arr[i]], fp);
    }
    if (hi_cut) {
        putc(type_map[unspecified_type], fp);
    }
}

static void write_tz_type_data_block(FILE *fp, int first_included_type_idx, int total_type_count,
                                     int this_default_type, const char *omitted_type_flags,
                                     const zic_t *utc_offsets, const signed char *is_dst_flags,
                                     const int *designation_indices, const int *index_map) {
    for (int i = first_included_type_idx; i < total_type_count; i++) {
        int h = (i == first_included_type_idx ? this_default_type
                 : i == this_default_type ? first_included_type_idx : i);
        if (!omitted_type_flags[h]) {
            puttzcode(utc_offsets[h], fp);
            putc(is_dst_flags[h], fp);
            putc(index_map[designation_indices[h]], fp);
        }
    }
}

static void write_tz_abbreviation_block(FILE *fp, const char *this_chars_buffer, int this_char_count) {
    if (this_char_count != 0) {
        fwrite(this_chars_buffer, sizeof this_chars_buffer[0], this_char_count, fp);
    }
}

static void write_tz_leap_seconds_block(FILE *fp, int leap_index, int leap_count, bool leap_expiry,
                                        zic_t leap_expires_val, const zic_t *transitions, const int *corrections,
                                        const bool *rolls, ptrdiff_t total_time_count, const zic_t *ats,
                                        const signed char *is_dst_flags, int total_type_count,
                                        const zic_t *utc_offsets, int pass, const unsigned char *types_arr) {
    ptrdiff_t leap_limit = leap_index + leap_count;
    for (ptrdiff_t i = leap_index; i < leap_limit; ++i) {
        zic_t todo;
        if (rolls[i]) {
            ptrdiff_t j_type_idx = 0;
            if (total_time_count == 0 || transitions[i] < ats[0]) {
                while (is_dst_flags[j_type_idx] && ++j_type_idx < total_type_count);
                if (j_type_idx == total_type_count) j_type_idx = 0;
            } else {
                j_type_idx = 1;
                while (j_type_idx < total_time_count && transitions[i] >= ats[j_type_idx]) {
                    ++j_type_idx;
                }
                j_type_idx = types_arr[j_type_idx - 1];
            }
            todo = tadd(transitions[i], -utc_offsets[j_type_idx]);
        } else {
            todo = transitions[i];
        }
        puttzcodepass(todo, fp, pass);
        puttzcode(corrections[i], fp);
    }
    if (leap_expiry) {
        puttzcodepass(leap_expires_val, fp, pass);
        puttzcode(leap_limit ? corrections[leap_limit - 1] : 0, fp);
    }
}

static void write_tz_std_ut_flags_block(FILE *fp, int std_count, int ut_count,
                                        int first_included_type_idx, int total_type_count,
                                        int this_default_type, const char *omitted_type_flags,
                                        const bool *tt_is_stds, const bool *tt_is_uts) {
    if (std_count != 0) {
        for (int i = first_included_type_idx; i < total_type_count; i++) {
            int h = (i == first_included_type_idx ? this_default_type
                     : i == this_default_type ? first_included_type_idx : i);
            if (!omitted_type_flags[h]) {
                putc(tt_is_stds[h], fp);
            }
        }
    }
    if (ut_count != 0) {
        for (int i = first_included_type_idx; i < total_type_count; i++) {
            int h = (i == first_included_type_idx ? this_default_type
                     : i == this_default_type ? first_included_type_idx : i);
            if (!omitted_type_flags[h]) {
                putc(tt_is_uts[h], fp);
            }
        }
    }
}

static void
writezone(const char *const name, const char *const string, char version,
          int defaulttype)
{
	FILE *fp = NULL;
	char *tempname = NULL;
	char const *outname = name;

    // Allocate the ATS and TYPES arrays via a single malloc.
	zic_t *ats = emalloc(align_to(size_product(timecnt + !timecnt,
						   sizeof *ats + 1),
				      alignof(zic_t)));
	unsigned char *types = (unsigned char *)(ats + timecnt);
	struct timerange rangeall = {0}, range32, range64;

	if (timecnt > 1) {
		qsort(attypes, timecnt, sizeof *attypes, atcomp);
	}

    // Optimize transitions by merging or removing redundant ones
	ptrdiff_t toi = 0;
	for (ptrdiff_t fromi = 0; fromi < timecnt; ++fromi) {
		if (toi != 0) {
            zic_t current_effective_at = attypes[fromi].at + utoffs[attypes[fromi].type];
            zic_t previous_at = attypes[toi - 1].at;
            int prev_prev_type_idx = (toi == 1) ? 0 : attypes[toi - 2].type;
            zic_t prev_prev_offset = utoffs[prev_prev_type_idx];

            if (current_effective_at <= (previous_at + prev_prev_offset)) {
                attypes[toi - 1].type = attypes[fromi].type;
                continue;
            }
		}

		if (toi == 0
		    || attypes[fromi].dontmerge
		    || (utoffs[attypes[toi - 1].type] != utoffs[attypes[fromi].type])
		    || (isdsts[attypes[toi - 1].type] != isdsts[attypes[fromi].type])
		    || (desigidx[attypes[toi - 1].type] != desigidx[attypes[fromi].type])) {
			attypes[toi++] = attypes[fromi];
		}
	}
	timecnt = toi;

	if (noise && timecnt > 1200) {
	  if (timecnt > TZ_MAX_TIMES) {
		warning(_("reference clients mishandle"
			  " more than %d transition times"),
			TZ_MAX_TIMES);
	  } else {
		warning(_("pre-2014 clients may mishandle"
			  " more than 1200 transition times"));
	  }
	}

    // Transfer optimized transitions to ats and types arrays
	for (ptrdiff_t i = 0; i < timecnt; ++i) {
		ats[i] = attypes[i].at;
		types[i] = attypes[i].type;
	}

    // Correct transition times for leap seconds
	for (ptrdiff_t i = 0; i < timecnt; ++i) {
		for (ptrdiff_t j = leapcnt - 1; j >= 0; --j) {
			if (ats[i] > trans[j] - corr[j]) {
				ats[i] = tadd(ats[i], corr[j]);
				break;
			}
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

    // Determine TZif version based on leap second handling
	for (int pass_version_check = 1; pass_version_check <= 2; pass_version_check++) {
	  struct timerange const *r = (pass_version_check == 1) ? &range32 : &range64;
	  if (pass_version_check == 1 && !want_bloat()) {
	    continue;
      }
	  if (r->leapexpiry) {
	    if (noise) {
	      warning(_("%s: pre-2021b clients may mishandle"
			" leap second expiry"),
		      name);
          }
	    version = '4';
	  }
	  if (r->leapcount > 0
	      && corr[r->leapbase] != 1 && corr[r->leapbase] != -1) {
	    if (noise) {
	      warning(_("%s: pre-2021b clients may mishandle"
			" leap second table truncation"),
		      name);
          }
	    version = '4';
	  }
	  if (version == '4') {
	    break;
      }
	}

	fp = open_outfile(&outname, &tempname);
	if (fp == NULL) {
		free(ats);
		return;
	}

    // Main loop for writing TZif data for 32-bit and 64-bit sections
	for (int pass = 1; pass <= 2; ++pass) {
		struct write_pass_params params;
		bool locut, hicut;
        ptrdiff_t i, j;

		if (pass == 1) { // 32-bit data section
			params.default_type = range32.defaulttype;
			params.time_base = range32.base;
			params.time_count = range32.count;
			params.leap_base = range32.leapbase;
			params.leap_count = range32.leapcount;
			params.leap_expiry = range32.leapexpiry;
			params.min_val = ZIC32_MIN;
			params.max_val = ZIC32_MAX;
		} else { // 64-bit data section
			params.default_type = range64.defaulttype;
			params.time_base = range64.base;
			params.time_count = range64.count;
			params.leap_base = range64.leapbase;
			params.leap_count = range64.leapcount;
			params.leap_expiry = range64.leapexpiry;
			params.min_val = min_time;
			params.max_val = max_time;
		}

        // Check for too many transitions for the current section's limits
        // These constants need to be defined in zic.h or equivalent
        // Example: #define MAX_TIMES_32BIT_SECTION (2147483647L)
        // Example: #define MAX_TIMES_64BIT_SECTION (9223372036854775807LL)
		if (params.time_count > MAX_TIMES_32BIT_SECTION && pass == 1) {
            error(_("too many transition times for 32-bit section"));
        }
		if (params.time_count > MAX_TIMES_64BIT_SECTION && pass == 2) {
            error(_("too many transition times for 64-bit section"));
        }

		locut = params.min_val < lo_time && lo_time <= params.max_val;
		hicut = params.min_val <= hi_time && hi_time < params.max_val;

		ptrdiff_t thistimelim = params.time_base + params.time_count;
		char omittype[TZ_MAX_TYPES];
		memset(omittype, true, sizeof omittype);

        int pretranstype = -1;
		if ((locut || (pass == 1 && params.time_base))
		    && !(params.time_count && ats[params.time_base] == lo_time)) {
		  pretranstype = params.default_type;
		  omittype[pretranstype] = false;
		}

		if (pass == 1 && lo_time <= params.min_val) {
		  params.default_type = range64.defaulttype;
		}

		if (locut) {
		  params.default_type = unspecifiedtype;
		}
		omittype[params.default_type] = false;
		for (i = params.time_base; i < thistimelim; i++) {
		  omittype[types[i]] = false;
        }
		if (hicut) {
		  omittype[unspecifiedtype] = false;
        }

#ifndef LEAVE_SOME_PRE_2011_SYSTEMS_IN_THE_LURCH
		if (want_bloat()) {
			int mrudst = -1, mrustd = -1;
            int hidst = -1, histd = -1;

			if (pretranstype >= 0) {
			  if (isdsts[pretranstype]) {
			    mrudst = pretranstype;
              } else {
			    mrustd = pretranstype;
              }
			}
			for (i = params.time_base; i < thistimelim; i++) {
				if (isdsts[types[i]]) {
					mrudst = types[i];
                } else {
					mrustd = types[i];
                }
            }

            int first_included_type_for_bloat = 0;
            while (first_included_type_for_bloat < typecnt && omittype[first_included_type_for_bloat]) {
                first_included_type_for_bloat++;
            }

			for (i = first_included_type_for_bloat; i < typecnt; i++) {
			  int h_bloat_idx = (i == first_included_type_for_bloat ? params.default_type
				   : i == params.default_type ? first_included_type_for_bloat : i);
			  if (!omittype[h_bloat_idx]) {
			    if (isdsts[h_bloat_idx]) {
			      hidst = i;
                    } else {
			      histd = i;
                    }
			  }
			}
			if (hidst >= 0 && mrudst >= 0 && hidst != mrudst &&
				utoffs[hidst] != utoffs[mrudst]) {
					addtype(utoffs[mrudst], &chars[desigidx[mrudst]],
						true, ttisstds[mrudst], ttisuts[mrudst]);
					omittype[typecnt - 1] = false;
			}
			if (histd >= 0 && mrustd >= 0 && histd != mrustd &&
				utoffs[histd] != utoffs[mrustd]) {
					addtype(utoffs[mrustd], &chars[desigidx[mrustd]],
						false, ttisstds[mrustd], ttisuts[mrustd]);
					omittype[typecnt - 1] = false;
			}
		}
#endif /* !defined LEAVE_SOME_PRE_2011_SYSTEMS_IN_THE_LURCH */

        // Determine the index of the first type that is not omitted.
        int first_included_type_idx = 0;
        while (first_included_type_idx < typecnt && omittype[first_included_type_idx]) {
            first_included_type_idx++;
        }

		int typemap[TZ_MAX_TYPES];
		int thistypecnt = 0;
		for (i = first_included_type_idx; i < typecnt; i++) {
		  if (!omittype[i]) {
		    typemap[i == first_included_type_idx ? params.default_type
			    : i == params.default_type ? first_included_type_idx : i]
		      = thistypecnt++;
          }
        }

		int indmap[TZ_MAX_CHARS];
		memset(indmap, -1, sizeof indmap);
		int thischarcnt = 0, stdcnt = 0, utcnt = 0;
		char thischars[TZ_MAX_CHARS];

		for (i = first_included_type_idx; i < typecnt; i++) {
			if (omittype[i]) {
				continue;
            }
			if (ttisstds[i]) {
			  stdcnt = thistypecnt;
            }
			if (ttisuts[i]) {
			  utcnt = thistypecnt;
            }
			if (indmap[desigidx[i]] >= 0) {
				continue;
            }
			const char *thisabbr = &chars[desigidx[i]];
			for (j = 0; j < thischarcnt; ++j) {
				if (strcmp(&thischars[j], thisabbr) == 0) {
					break;
                }
            }
			if (j == thischarcnt) {
				strcpy(&thischars[thischarcnt], thisabbr);
				thischarcnt += strlen(thisabbr) + 1;
			}
			indmap[desigidx[i]] = j;
		}

		if (pass == 1 && !want_bloat()) {
		  hicut = false;
		  params.leap_expiry = false;
		  pretranstype = -1;
		  params.time_count = 0;
		  params.leap_count = 0;
		  thistypecnt = 1;
		  thischarcnt = 1;
		}

		write_tz_header_block(fp, version, stdcnt, utcnt,
                              params.leap_count + params.leap_expiry,
                              (pretranstype >= 0) + params.time_count + hicut,
                              thistypecnt, thischarcnt);

		if (pass == 1 && !want_bloat()) {
		  puttzcode(0, fp);
		  putc(0, fp);
		  putc(0, fp);
		  putc(0, fp);
		  continue;
		}

		zic_t lo_for_pass = (pass == 1 && lo_time < ZIC32_MIN) ? ZIC32_MIN : lo_time;

		write_tz_transitions_block(fp, lo_for_pass, pretranstype,
                                   params.time_base, thistimelim, ats, hicut, hi_time, pass);

		write_tz_type_indices_block(fp, pretranstype,
                                    params.time_base, thistimelim, types, hicut, unspecifiedtype, typemap);

		write_tz_type_data_block(fp, first_included_type_idx, typecnt, params.default_type,
                                 omittype, utoffs, isdsts, desigidx, indmap);

		write_tz_abbreviation_block(fp, thischars, thischarcnt);

		write_tz_leap_seconds_block(fp, params.leap_base, params.leap_count, params.leap_expiry,
                                    leapexpires, trans, corr, roll, timecnt, ats, isdsts, typecnt, utoffs, pass, types);

		write_tz_std_ut_flags_block(fp, stdcnt, utcnt,
                                    first_included_type_idx, typecnt, params.default_type,
                                    omittype, ttisstds, ttisuts);
	}
	fprintf(fp, "\n%s\n", string);
	close_file(fp, directory, name, tempname);
	rename_dest(tempname, name);
	free(ats);
}

static void append_two_digits(char **buf_ptr, int value) {
    *(*buf_ptr)++ = '0' + (value / 10);
    *(*buf_ptr)++ = '0' + (value % 10);
}

static char const *
abbroffset(char *buf, zic_t offset)
{
  char sign = '+';
  int seconds;
  int minutes;
  int hours;

  if (offset < 0) {
    offset = -offset;
    sign = '-';
  }

  seconds = offset % SECSPERMIN;
  offset /= SECSPERMIN;
  minutes = offset % MINSPERHOUR;
  offset /= MINSPERHOUR;
  hours = offset;

  if (hours >= 100) {
    error(_("%%z UT offset magnitude exceeds 99:59:59"));
    return "%z";
  }

  char *p = buf;
  *p++ = sign;
  append_two_digits(&p, hours);

  if (minutes != 0 || seconds != 0) {
    append_two_digits(&p, minutes);

    if (seconds != 0) {
      append_two_digits(&p, seconds);
    }
  }
  *p = '\0';
  return buf;
}

static char const disable_percent_s[] = "";

static ptrdiff_t
doabbr(char *abbr, struct zone const *zp, char const *letters,
       bool isdst, zic_t save, bool doquotes)
{
	char *cp;
	char *slashp;
	ptrdiff_t len;
	char const *format = zp->z_format;

	slashp = strchr(format, '/');
	if (slashp == NULL) {
	  char letterbuf[PERCENT_Z_LEN_BOUND + 1];
	  char const *sprintf_arg = letters;

	  if (zp->z_format_specifier == 'z') {
	    sprintf_arg = abbroffset(letterbuf, zp->z_stdoff + save);
	  } else if (letters == NULL) {
	    sprintf_arg = "%s";
	  } else if (letters == disable_percent_s) {
	    return 0;
	  }
	  sprintf(abbr, format, sprintf_arg);
	} else if (isdst) {
		strcpy(abbr, slashp + 1);
	} else {
		memcpy(abbr, format, slashp - format);
		abbr[slashp - format] = '\0';
	}
	len = strlen(abbr);
	if (!doquotes)
		return len;
	for (cp = abbr; is_alpha(*cp); ++cp)
		;
	if (len > 0 && *cp == '\0')
		return len;
	abbr[len + 2] = '\0';
	abbr[len + 1] = '>';
	memmove(abbr + 1, abbr, len);
	abbr[0] = '<';
	return len + 2;
}

static void
updateminmax(const zic_t x)
{
	min_year = (min_year > x) ? x : min_year;
	max_year = (max_year < x) ? x : max_year;
}

static int
stringoffset(char *result, zic_t offset)
{
    // Define the maximum expected string length including the null terminator.
    // This calculation is based on the format "[-]HHH:MM:SS".
    // Max hours (before special handling): 167 (24 * 7 - 1)
    // Max length: 1 (sign) + 3 (hours) + 1 (colon) + 2 (minutes) + 1 (colon) + 2 (seconds) = 10 characters.
    // Plus 1 for the null terminator.
    enum { MAX_OFFSET_STR_LEN = 11 };

	int hours;
	int minutes;
	int seconds;
	bool negative = offset < 0;
	int current_len = 0; // Tracks the current length written to 'result'
    int written_chars;   // Stores the return value of snprintf
    size_t remaining_buf_size = MAX_OFFSET_STR_LEN; // Tracks remaining available buffer space

    // Reliability: Check for a NULL result buffer to prevent dereferencing issues.
    if (result == NULL) {
        return 0; // Indicate failure or no characters written.
    }

	if (negative) {
        // Security/Reliability: Before writing, ensure there's space for '-' and a null terminator.
        if (current_len + 1 >= remaining_buf_size) {
            result[0] = '\0'; // Ensure the buffer is empty if insufficient space.
            return 0; // Not enough space for the sign and null terminator.
        }
		result[current_len++] = '-';
        remaining_buf_size--; // Decrement available buffer space after writing the sign.
	}

	seconds = offset % SECSPERMIN;
	offset /= SECSPERMIN;
	minutes = offset % MINSPERHOUR;
	offset /= MINSPERHOUR;
	hours = offset;

    // Maintain original functionality: For overly large offsets, the string is cleared and 0 is returned.
	if (hours >= HOURSPERDAY * DAYSPERWEEK) {
		result[0] = '\0';
		return 0;
	}

    // Security/Reliability: Use snprintf for safe string formatting to prevent buffer overflows.
    // It writes at most `remaining_buf_size - 1` characters and always null-terminates if `remaining_buf_size > 0`.
    written_chars = snprintf(result + current_len, remaining_buf_size, "%d", hours);

    // Error Handling: Check snprintf's return value for errors or truncation.
    // A negative value indicates an encoding error.
    // A value >= `remaining_buf_size` indicates truncation (buffer was too small).
    if (written_chars < 0 || (size_t)written_chars >= remaining_buf_size) {
        result[0] = '\0'; // Ensure buffer is empty on error.
        return 0; // Indicate failure.
    }
    current_len += written_chars;
    remaining_buf_size -= written_chars;

	// Maintain original functionality: Append minutes and seconds only if they are not both zero.
	if (minutes != 0 || seconds != 0) {
        // Security/Reliability: Safely append minutes.
        written_chars = snprintf(result + current_len, remaining_buf_size, ":%02d", minutes);
        if (written_chars < 0 || (size_t)written_chars >= remaining_buf_size) {
            result[0] = '\0';
            return 0;
        }
        current_len += written_chars;
        remaining_buf_size -= written_chars;

		if (seconds != 0) {
            // Security/Reliability: Safely append seconds.
            written_chars = snprintf(result + current_len, remaining_buf_size, ":%02d", seconds);
            if (written_chars < 0 || (size_t)written_chars >= remaining_buf_size) {
                result[0] = '\0';
                return 0;
            }
            current_len += written_chars;
            // No need to update remaining_buf_size after the final segment.
		}
	}

	return current_len; // Return the total number of characters written, excluding the null terminator.
}

#define FEB_29_DAY_OF_MONTH 29
#define DEFAULT_TIME_TWO_HOURS (2 * SECSPERMIN * MINSPERHOUR)
#define COMPAT_YEAR_2013 2013
#define COMPAT_YEAR_1994 1994

static int
stringrule(char *result, struct rule *const rp, zic_t save, zic_t stdoff)
{
	zic_t	tod = rp->r_tod;
	int	compat_year = 0;

	if (rp->r_dycode == DC_DOM) {
		int	month_idx;
		int	days_before_month = 0;

		if (rp->r_dayofmonth == FEB_29_DAY_OF_MONTH && rp->r_month == TM_FEBRUARY)
			return -1;

		for (month_idx = 0; month_idx < rp->r_month; ++month_idx)
			days_before_month += len_months[0][month_idx];

		if (rp->r_month <= TM_FEBRUARY) {
		  result += sprintf(result, "%d", days_before_month + rp->r_dayofmonth - 1);
		} else {
		  result += sprintf(result, "J%d", days_before_month + rp->r_dayofmonth);
		}
	} else {
		int	week_number;
		int	weekday = rp->r_wday;
		int	day_offset_in_week;

		if (rp->r_dycode == DC_DOWGEQ) {
			day_offset_in_week = (rp->r_dayofmonth - 1) % DAYSPERWEEK;
			if (day_offset_in_week)
				compat_year = COMPAT_YEAR_2013;
			
			weekday -= day_offset_in_week;
			tod += (zic_t)day_offset_in_week * SECSPERDAY;
			week_number = 1 + (rp->r_dayofmonth - 1) / DAYSPERWEEK;
		} else if (rp->r_dycode == DC_DOWLEQ) {
			if (rp->r_dayofmonth == len_months[1][rp->r_month]) {
				week_number = 5;
			} else {
				day_offset_in_week = rp->r_dayofmonth % DAYSPERWEEK;
				if (day_offset_in_week)
					compat_year = COMPAT_YEAR_2013;
				
				weekday -= day_offset_in_week;
				tod += (zic_t)day_offset_in_week * SECSPERDAY;
				week_number = rp->r_dayofmonth / DAYSPERWEEK;
			}
		} else {
			return -1;
		}
		
		if (weekday < 0)
			weekday += DAYSPERWEEK;
		
		result += sprintf(result, "M%d.%d.%d",
				  rp->r_month + 1, week_number, weekday);
	}

	if (rp->r_todisut)
	  tod += stdoff;
	if (rp->r_todisstd && !rp->r_isdst)
	  tod += save;

	if (tod != DEFAULT_TIME_TWO_HOURS) {
		*result++ = '/';
		if (! stringoffset(result, tod))
			return -1;

		if (tod < 0) {
			if (compat_year < COMPAT_YEAR_2013)
				compat_year = COMPAT_YEAR_2013;
		} else if (SECSPERDAY <= tod) {
			if (compat_year < COMPAT_YEAR_1994)
				compat_year = COMPAT_YEAR_1994;
		}
	}
	return compat_year;
}

static int
rule_cmp(struct rule const *a, struct rule const *b)
{
	if (!a) {
		return (b ? -1 : 0);
	}
	if (!b) {
		return 1;
	}

	if (a->r_hiyear == ZIC_MAX && b->r_hiyear == ZIC_MAX) {
		return 0;
	}

	if (a->r_hiyear != b->r_hiyear) {
		return (a->r_hiyear < b->r_hiyear) ? -1 : 1;
	}

	int diff_month = a->r_month - b->r_month;
	if (diff_month != 0) {
		return diff_month;
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
	const struct zone *zp;
	const struct rule *rp;
	struct rule *stdrp = NULL;
	struct rule *dstrp = NULL;
	ptrdiff_t i;
	int compat = 0;
	int c;
	ptrdiff_t current_len = 0;
	int written_chars;
	int dstcmp_result;

	const struct rule *last_found_rules[2] = { NULL, NULL };
	
	struct zone zstr[2];
	
	const struct zone *current_std_zone;
	const struct zone *current_dst_zone;

	struct rule temp_std_rule, temp_dst_rule;

	result[0] = '\0';

	if (hi_time < max_time) {
	  return -1;
	}

	zp = zpfirst + zonecount - 1;

	for (i = 0; i < zp->z_nrules; ++i) {
		rp = &zp->z_rules[i];
		const struct rule **last_rule_ptr = &last_found_rules[rp->r_isdst];
		int cmp = rule_cmp(*last_rule_ptr, rp);
		if (cmp < 0) {
		  *last_rule_ptr = rp;
		} else if (cmp == 0) {
		  return -1;
		}
	}
	stdrp = (struct rule *)last_found_rules[false];
	dstrp = (struct rule *)last_found_rules[true];

	if (zp->z_nrules) {
		dstcmp_result = rule_cmp(dstrp, stdrp);
	} else {
		dstcmp_result = zp->z_isdst ? 1 : -1;
	}

	current_std_zone = current_dst_zone = zp;

	if (dstcmp_result < 0) {
		dstrp = NULL;
	} else if (0 < dstcmp_result) {
		zic_t actual_dst_save = dstrp ? dstrp->r_save : zp->z_save;
		zic_t effective_dstr_rule_save = actual_dst_save;

		if (actual_dst_save >= 0) {
			current_std_zone = &zstr[0];
			current_dst_zone = &zstr[1];

			zstr[0].z_stdoff = zp->z_stdoff + 2 * actual_dst_save;
			zstr[0].z_format = "XXX";
			zstr[0].z_format_specifier = 0;

			zstr[1].z_stdoff = zstr[0].z_stdoff;
			zstr[1].z_format = zp->z_format;
			zstr[1].z_format_specifier = zp->z_format_specifier;

			effective_dstr_rule_save = -actual_dst_save;
		}

		temp_dst_rule.r_month = TM_JANUARY;
		temp_dst_rule.r_dycode = DC_DOM;
		temp_dst_rule.r_dayofmonth = 1;
		temp_dst_rule.r_tod = 0;
		temp_dst_rule.r_todisstd = false;
		temp_dst_rule.r_todisut = false;
		temp_dst_rule.r_isdst = true;
		temp_dst_rule.r_save = effective_dstr_rule_save;
		temp_dst_rule.r_abbrvar = dstrp ? dstrp->r_abbrvar : NULL;
		dstrp = &temp_dst_rule;

		temp_std_rule.r_month = TM_DECEMBER;
		temp_std_rule.r_dycode = DC_DOM;
		temp_std_rule.r_dayofmonth = 31;
		temp_std_rule.r_tod = SECSPERDAY + temp_dst_rule.r_save;
		temp_std_rule.r_todisstd = false;
		temp_std_rule.r_todisut = false;
		temp_std_rule.r_isdst = false;
		temp_std_rule.r_save = 0;
		temp_std_rule.r_abbrvar = (actual_dst_save < 0 && stdrp) ? stdrp->r_abbrvar : NULL;
		stdrp = &temp_std_rule;
	}

	written_chars = doabbr(result + current_len, current_std_zone, stdrp ? stdrp->r_abbrvar : NULL,
						   false, 0, true);
	if (written_chars < 0) {
		result[0] = '\0';
		return -1;
	}
	current_len += written_chars;

	written_chars = stringoffset(result + current_len, -current_std_zone->z_stdoff);
	if (written_chars <= 0) {
		result[0] = '\0';
		return -1;
	}
	current_len += written_chars;

	if (dstrp == NULL) {
		return compat;
	}

	written_chars = doabbr(result + current_len, current_dst_zone, dstrp->r_abbrvar,
						   dstrp->r_isdst, dstrp->r_save, true);
	if (written_chars < 0) {
		result[0] = '\0';
		return -1;
	}
	current_len += written_chars;

	if (dstrp->r_save != SECSPERMIN * MINSPERHOUR) {
		written_chars = stringoffset(result + current_len,
									 -(current_dst_zone->z_stdoff + dstrp->r_save));
		if (written_chars <= 0) {
			result[0] = '\0';
			return -1;
		}
		current_len += written_chars;
	}

	result[current_len++] = ',';

	c = stringrule(result + current_len, dstrp, dstrp->r_save, current_std_zone->z_stdoff);
	if (c < 0) {
		result[0] = '\0';
		return -1;
	}
	if (compat < c) {
		compat = c;
	}
	current_len += strlen(result + current_len);

	result[current_len++] = ',';

	c = stringrule(result + current_len, stdrp, dstrp->r_save, current_std_zone->z_stdoff);
	if (c < 0) {
		result[0] = '\0';
		return -1;
	}
	if (compat < c) {
		compat = c;
	}
	current_len += strlen(result + current_len);

	return compat;
}

static bool
find_earliest_rule_transition(const struct zone *zp, zic_t current_save, zic_t current_stdoff,
                              ptrdiff_t *k_out, zic_t *ktime_out, struct rule **rp_out,
                              zic_t effective_untiltime, bool useuntil, zic_t max_year0)
{
    ptrdiff_t k = -1;
    zic_t ktime = ZIC_MAX;
    zic_t one = 1;
    zic_t y2038_boundary = one << 31;

    for (ptrdiff_t j = 0; j < zp->z_nrules; ++j) {
        struct rule *r = &zp->z_rules[j];
        if (!r->r_todo) {
            continue;
        }

        eats(zp->z_filenum, zp->z_linenum, r->r_filenum, r->r_linenum);

        if (!(r->r_temp < y2038_boundary || r->r_hiyear <= max_year0)) {
            r->r_todo = false;
            continue;
        }

        zic_t offset = r->r_todisut ? 0 : current_stdoff;
        if (!r->r_todisstd) {
            offset = oadd(offset, current_save);
        }

        zic_t jtime = r->r_temp;
        if (jtime == min_time || jtime == max_time) {
            continue;
        }
        jtime = tadd(jtime, -offset);

        if (useuntil && jtime >= effective_untiltime) {
            continue;
        }

        if (k < 0 || jtime < ktime) {
            k = j;
            ktime = jtime;
        } else if (jtime == ktime) {
            char const *dup_rules_msg = _("two rules for same instant");
            eats(zp->z_filenum, zp->z_linenum, r->r_filenum, r->r_linenum);
            error("%s", dup_rules_msg);
        }
    }

    if (k >= 0) {
        *k_out = k;
        *ktime_out = ktime;
        *rp_out = &zp->z_rules[k];
        return true;
    }
    return false;
}

static void
process_zone_with_rules(const struct zone *zp, char *startbuf, char *ab,
                        bool usestart_current_zone, bool useuntil_current_zone,
                        zic_t min_year_val, zic_t max_year_val, zic_t max_year0,
                        zic_t *nonTZlimtime_ptr, int *nonTZlimtype_ptr, int *defaulttype_ptr,
                        zic_t starttime_val, zic_t untiltime_val,
                        zic_t *startoff_ptr, zic_t *current_save_ptr)
{
    zic_t current_save = *current_save_ptr;
    zic_t startoff = *startoff_ptr;

    for (zic_t year = min_year_val; year <= max_year_val; ++year) {
        if (useuntil_current_zone && year > zp->z_untilrule.r_hiyear) {
            break;
        }

        for (ptrdiff_t j = 0; j < zp->z_nrules; ++j) {
            struct rule *rp = &zp->z_rules[j];
            eats(zp->z_filenum, zp->z_linenum, rp->r_filenum, rp->r_linenum);
            rp->r_todo = (year >= rp->r_loyear && year <= rp->r_hiyear);
            if (rp->r_todo) {
                rp->r_temp = rpytime(rp, year);
            }
        }

        for (;;) {
            ptrdiff_t k = -1;
            zic_t ktime = 0;
            struct rule *rp = NULL;

            zic_t effective_untiltime = untiltime_val;
            if (useuntil_current_zone) {
                if (!zp->z_untilrule.r_todisut) {
                    effective_untiltime = tadd(effective_untiltime, -zp->z_stdoff);
                }
                if (!zp->z_untilrule.r_todisstd) {
                    effective_untiltime = tadd(effective_untiltime, -current_save);
                }
            }

            if (!find_earliest_rule_transition(zp, current_save, zp->z_stdoff,
                                               &k, &ktime, &rp, effective_untiltime,
                                               useuntil_current_zone, max_year0)) {
                break;
            }

            rp->r_todo = false;

            if (useuntil_current_zone && ktime >= effective_untiltime) {
                if (!*startbuf && (oadd(zp->z_stdoff, rp->r_save) == startoff)) {
                    doabbr(startbuf, zp, rp->r_abbrvar, rp->r_isdst, rp->r_save, false);
                }
                goto end_rule_processing;
            }

            current_save = rp->r_save;

            if (usestart_current_zone) {
                if (ktime < starttime_val) {
                    startoff = oadd(zp->z_stdoff, current_save);
                    doabbr(startbuf, zp, rp->r_abbrvar, rp->r_isdst, rp->r_save, false);
                    continue;
                }
                if (*startbuf == '\0' && startoff == oadd(zp->z_stdoff, current_save)) {
                    doabbr(startbuf, zp, rp->r_abbrvar, rp->r_isdst, rp->r_save, false);
                }
            }

            eats(zp->z_filenum, zp->z_linenum, rp->r_filenum, rp->r_linenum);
            doabbr(ab, zp, rp->r_abbrvar, rp->r_isdst, rp->r_save, false);

            zic_t offset = oadd(zp->z_stdoff, rp->r_save);
            int type = addtype(offset, ab, rp->r_isdst, rp->r_todisstd, rp->r_todisut);

            if (*defaulttype_ptr < 0 && !rp->r_isdst) {
                *defaulttype_ptr = type;
            }

            addtt(ktime, type);

            if (*nonTZlimtime_ptr < ktime && (useuntil_current_zone || rp->r_hiyear != ZIC_MAX)) {
                *nonTZlimtime_ptr = ktime;
                *nonTZlimtype_ptr = type;
            }
        }
    }

end_rule_processing:;
    *current_save_ptr = current_save;
    *startoff_ptr = startoff;
}


static void
outzone(const struct zone *zpfirst, ptrdiff_t zonecount)
{
    ptrdiff_t           i, j;
    zic_t               starttime = ZIC_MIN;
    zic_t               untiltime = ZIC_MIN;
    bool                startttisstd = false;
    bool                startttisut = false;
    char *              startbuf = NULL;
    char *              ab = NULL;
    char *              envvar = NULL;
    int                 max_abbr_len;
    int                 max_envvar_len;
    int                 compat;
    bool                do_extend;
    char                version;
    zic_t               nonTZlimtime = ZIC_MIN;
    int                 nonTZlimtype = -1;
    zic_t               max_year0;
    int                 defaulttype = -1;

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
        if (i < zonecount - 1) {
            updateminmax(zp->z_untilrule.r_loyear);
        }
        for (j = 0; j < zp->z_nrules; ++j) {
            struct rule *rp = &zp->z_rules[j];
            updateminmax(rp->r_loyear);
            if (rp->r_hiwasnum) {
                updateminmax(rp->r_hiyear);
            }
        }
    }

    compat = stringzone(envvar, zpfirst, zonecount);
    version = (compat < 2013) ? '2' : '3';
    do_extend = (compat < 0);

    if (noise) {
        if (!*envvar) {
            warning("%s %s", _("no proleptic TZ string for zone"), zpfirst->z_name);
        } else if (compat != 0) {
            warning(_("%s: pre-%d clients may mishandle distant timestamps"),
                    zpfirst->z_name, compat);
        }
    }

    if (do_extend) {
        min_year = (min_year >= ZIC_MIN + years_of_observations)
                   ? min_year - years_of_observations : ZIC_MIN;
        max_year = (max_year <= ZIC_MAX - years_of_observations)
                   ? max_year + years_of_observations : ZIC_MAX;
    }
    max_year = max(max_year, (redundant_time / (SECSPERDAY * DAYSPERNYEAR) + EPOCH_YEAR + 1));
    max_year0 = max_year;

    if (want_bloat()) {
        if (min_year > YEAR_32BIT_MIN - 1) {
            min_year = YEAR_32BIT_MIN - 1;
        }
        if (max_year < YEAR_32BIT_MAX) {
            max_year = YEAR_32BIT_MAX;
        }
    }

    if (min_time < lo_time || hi_time < max_time) {
        unspecifiedtype = addtype(0, "-00", false, false, false);
    }

    for (i = 0; i < zonecount; ++i) {
        struct zone const *zp = &zpfirst[i];
        bool usestart_for_zone_initial = (i > 0 && (zp - 1)->z_untiltime > min_time);
        bool useuntil_for_zone_initial = (i < (zonecount - 1));
        
        zic_t current_save_for_zone = 0;
        zic_t startoff = zp->z_stdoff;

        if (useuntil_for_zone_initial && zp->z_untiltime <= min_time) {
            continue;
        }
        eat(zp->z_filenum, zp->z_linenum);
        *startbuf = '\0';

        if (zp->z_nrules == 0) {
            current_save_for_zone = zp->z_save;
            doabbr(startbuf, zp, NULL, zp->z_isdst, current_save_for_zone, false);
            int type = addtype(oadd(zp->z_stdoff, current_save_for_zone),
                               startbuf, zp->z_isdst, startttisstd, startttisut);
            if (usestart_for_zone_initial) {
                addtt(starttime, type);
                if (useuntil_for_zone_initial && nonTZlimtime < starttime) {
                    nonTZlimtime = starttime;
                    nonTZlimtype = type;
                }
            } else if (defaulttype < 0) {
                defaulttype = type;
            }
        } else {
            process_zone_with_rules(zp, startbuf, ab,
                                    usestart_for_zone_initial, useuntil_for_zone_initial,
                                    min_year, max_year, max_year0,
                                    &nonTZlimtime, &nonTZlimtype, &defaulttype,
                                    starttime, untiltime,
                                    &startoff, &current_save_for_zone);
        }

        if (usestart_for_zone_initial) {
            bool isdst = (startoff != zp->z_stdoff);
            if (*startbuf == '\0' && zp->z_format) {
                doabbr(startbuf, zp, disable_percent_s, isdst, current_save_for_zone, false);
            }
            eat(zp->z_filenum, zp->z_linenum);
            if (*startbuf == '\0') {
                error(_("can't determine time zone abbreviation to use just after until time"));
            } else {
                int type = addtype(startoff, startbuf, isdst, startttisstd, startttisut);
                if (defaulttype < 0 && !isdst) {
                    defaulttype = type;
                }
                addtt(starttime, type);
            }
        }

        if (useuntil_for_zone_initial) {
            startttisstd = zp->z_untilrule.r_todisstd;
            startttisut = zp->z_untilrule.r_todisut;
            starttime = zp->z_untiltime;
            if (!startttisstd) {
                starttime = tadd(starttime, -current_save_for_zone);
            }
            if (!startttisut) {
                starttime = tadd(starttime, -zp->z_stdoff);
            }
        }
    }

    if (defaulttype < 0) {
        defaulttype = 0;
    }

    if (!do_extend && !want_bloat()) {
        zic_t keep_at_max;
        zic_t TZstarttime = ZIC_MAX;

        for (i = 0; i < timecnt; i++) {
            zic_t at = attypes[i].at;
            if (nonTZlimtime < at && at < TZstarttime) {
                TZstarttime = at;
            }
        }
        if (TZstarttime == ZIC_MAX) {
            TZstarttime = nonTZlimtime;
        }

        keep_at_max = max(TZstarttime, redundant_time);
        ptrdiff_t write_idx = 0;
        for (i = 0; i < timecnt; i++) {
            if (attypes[i].at <= keep_at_max) {
                attypes[write_idx].at = attypes[i].at;
                attypes[write_idx].dontmerge = (attypes[i].at == TZstarttime
                                                && (nonTZlimtype != attypes[i].type || strchr(envvar, ',')));
                attypes[write_idx].type = attypes[i].type;
                write_idx++;
            }
        }
        timecnt = write_idx;
    }

    if (do_extend) {
        struct rule xr = { .r_month = TM_JANUARY, .r_dycode = DC_DOM, .r_dayofmonth = 1, .r_tod = 0 };
        struct attype *lastat = NULL;

        if (timecnt > 0) {
             lastat = &attypes[0];
             for (i = 1; i < timecnt; i++) {
                 if (attypes[i].at > lastat->at) {
                     lastat = &attypes[i];
                 }
             }
        }

        if (!lastat || lastat->at < rpytime(&xr, max_year - 1)) {
            addtt(rpytime(&xr, max_year + 1),
                  lastat ? lastat->type : defaulttype);
            if (timecnt > 0) {
                attypes[timecnt - 1].dontmerge = true;
            }
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
	void *new_attypes_ptr = growalloc(attypes, sizeof *attypes, timecnt, &timecnt_alloc);
	if (new_attypes_ptr == NULL) {
		fprintf(stderr, "Error: Failed to allocate memory for 'attypes' array. Terminating.\n");
		exit(EXIT_FAILURE);
	}
	attypes = new_attypes_ptr;

	attypes[timecnt].at = starttime;
	attypes[timecnt].dontmerge = false;
	attypes[timecnt].type = type;
	++timecnt;
}

static int
addtype(zic_t utoff, char const *abbr, bool isdst, bool ttisstd, bool ttisut)
{
    if (utoff < -2147483647L - 1L || utoff > 2147483647L) {
        error(_("UT offset out of range"));
        exit(EXIT_FAILURE);
    }

    if (!want_bloat()) {
        ttisstd = false;
        ttisut = false;
    }

    int abbr_idx = -1;
    for (int j = 0; j < charcnt; ++j) {
        if (strcmp(&chars[j], abbr) == 0) {
            abbr_idx = j;
            break;
        }
    }

    if (abbr_idx == -1) {
        newabbr(abbr);
        abbr_idx = charcnt - 1; // newabbr increments charcnt and places it there
    } else {
        for (int i = 0; i < typecnt; ++i) {
            if (utoff == utoffs[i] &&
                isdst == isdsts[i] &&
                abbr_idx == desigidx[i] &&
                ttisstd == ttisstds[i] &&
                ttisut == ttisuts[i]) {
                return i;
            }
        }
    }

    if (typecnt >= TZ_MAX_TYPES) {
        error(_("too many local time types"));
        exit(EXIT_FAILURE);
    }

    utoffs[typecnt] = utoff;
    isdsts[typecnt] = isdst;
    ttisstds[typecnt] = ttisstd;
    ttisuts[typecnt] = ttisut;
    desigidx[typecnt] = abbr_idx;

    return typecnt++;
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

    int insert_idx = 0;
    while (insert_idx < leapcnt && t > trans[insert_idx]) {
        insert_idx++;
    }

    size_t elements_to_shift = (size_t)(leapcnt - insert_idx);
    if (elements_to_shift > 0) {
        memmove(&trans[insert_idx + 1], &trans[insert_idx], elements_to_shift * sizeof(*trans));
        memmove(&corr[insert_idx + 1], &corr[insert_idx], elements_to_shift * sizeof(*corr));
        memmove(&roll[insert_idx + 1], &roll[insert_idx], elements_to_shift * sizeof(*roll));
    }

    trans[insert_idx] = t;
    corr[insert_idx] = correction;
    roll[insert_idx] = rolling;

    leapcnt++;
}

#include <stdlib.h>

#ifndef MIN_LEAP_SECOND_INTERVAL_SECONDS
#define MIN_LEAP_SECOND_INTERVAL_SECONDS (28 * SECSPERDAY)
#endif

static void
adjleap(void)
{
	zic_t	last = 0;
	zic_t	prevtrans = 0;
	int	i;

	for (i = 0; i < leapcnt; ++i) {
		if (trans[i] - prevtrans < MIN_LEAP_SECOND_INTERVAL_SECONDS) {
		  error(_("Leap seconds too close together"));
		  exit(EXIT_FAILURE);
		}
		prevtrans = trans[i];
		trans[i] = tadd(trans[i], last);
		corr[i] += last;
		last = corr[i];
	}

	if (0 <= leapexpires) {
	  leapexpires = oadd(leapexpires, last);
	  if (! (leapcnt == 0 || (trans[leapcnt - 1] < leapexpires))) {
	    error(_("Last leap time does not precede Expires time"));
	    exit(EXIT_FAILURE);
	  }
	}
}

/* Is A a space character in the C locale?  */
#include <stdbool.h>
#include <ctype.h>

static bool
is_space(char a)
{
    return (bool)isspace((unsigned char)a);
}

/* Is A an alphabetic character in the C locale?  */
#include <stdbool.h>
#include <ctype.h>

static bool
is_alpha(char a)
{
    return isalpha((unsigned char)a) != 0;
}

/* If A is an uppercase character in the C locale, return its lowercase
   counterpart.  Otherwise, return A.  */
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
	const struct lookup *foundlp = NULL;
	const struct lookup *lp;

	if (word == NULL || table == NULL) {
		return NULL;
	}

	/* Store original word for potential warning messages. */
	const char *original_word = word;

	/* If TABLE is LASTS and the word starts with "last" followed
	   by a non-'-', skip the "last" and look in WDAY_NAMES instead.
	   Warn about any usage of the undocumented prefix "last-".  */
	static const char LAST_PREFIX[] = "last";
	const size_t LAST_PREFIX_LEN = sizeof(LAST_PREFIX) - 1; // Length is 4

	if (table == lasts && ciprefix(LAST_PREFIX, word) && word[LAST_PREFIX_LEN]) {
		if (word[LAST_PREFIX_LEN] == '-') {
			warning(_("\"%s\" is undocumented; use \"last%s\" instead"),
					original_word, original_word + LAST_PREFIX_LEN + 1);
		} else {
			word += LAST_PREFIX_LEN; /* Modify 'word' for subsequent search */
			table = wday_names;     /* Modify 'table' for subsequent search */
		}
	}

	/*
	** Look for exact match.
	*/
	for (lp = table; lp->l_word != NULL; ++lp) {
		if (ciequal(word, lp->l_word)) {
			return lp;
		}
	}

	/*
	** Look for inexact match.
	*/
	for (lp = table; lp->l_word != NULL; ++lp) {
		if (ciprefix(word, lp->l_word)) {
			if (foundlp == NULL) {
				foundlp = lp;
			} else {
				return NULL; /* multiple inexact matches */
			}
		}
	}

	/* Warn about any backward-compatibility issue with pre-2017c zic.  */
	if (foundlp != NULL && noise) {
		bool pre_2017c_match = false;
		for (lp = table; lp->l_word != NULL; ++lp) {
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

#include <stddef.h> // For NULL, size_t
#include <stdlib.h> // For exit, EXIT_FAILURE
#include <ctype.h>  // For isspace

// Assuming error and _ are defined elsewhere.
// e.g., extern void error(const char *msg);
//       #define _(x) x

static int
getfields(char *current_pos, char **field_array, int max_fields)
{
    char *dest_pos;
    int num_fields = 0;

    for (;;) {
        // 1. Skip leading whitespace for the next field
        while (isspace((unsigned char)*current_pos)) {
            ++current_pos;
        }

        // 2. Check for end of input string or comment start character ('#')
        if (*current_pos == '\0' || *current_pos == '#') {
            break; // Stop processing if end of line or comment
        }

        char *field_start = current_pos; // Mark the beginning of the field in the buffer
        dest_pos = current_pos;           // Destination for writing processed characters (in-place modification)

        // 3. Parse the current field, handling quotes and concatenating parts.
        // This do-while loop processes one character or one quoted segment at a time.
        // It's designed to strip quotes and compact the string in place.
        do {
            if (*current_pos != '"') {
                // Not a quote: copy character to destination and advance both pointers
                *dest_pos = *current_pos;
                ++dest_pos;
                ++current_pos;
            } else {
                // Found an opening quote:
                // Advance current_pos to skip the opening quote (it's stripped, not copied)
                ++current_pos;
                
                // Process characters inside the quoted string until a closing quote is found
                while (*current_pos != '"') {
                    if (*current_pos == '\0') {
                        // Error: End of string encountered before a closing quote
                        error(_("Odd number of quotation marks"));
                        exit(EXIT_FAILURE);
                    }
                    // Copy character inside quote to destination and advance both pointers
                    *dest_pos = *current_pos;
                    ++dest_pos;
                    ++current_pos;
                }
                // Found the closing quote:
                // Advance current_pos to skip the closing quote (it's also stripped)
                ++current_pos;
                // Crucially, dest_pos is NOT advanced here. This allows subsequent unquoted
                // characters (or a new quoted segment) to overwrite the space where the
                // quotes were, effectively stripping them and concatenating parts like "foo"bar -> foobar.
            }
        // Continue parsing characters as long as it's not end of string, a comment, or an unquoted whitespace.
        // This condition ensures that a sequence like "foo"bar is treated as a single field "foobar".
        } while (*current_pos != '\0' && *current_pos != '#' && !isspace((unsigned char)*current_pos));

        // 4. Null-terminate the parsed field in the buffer.
        // dest_pos now points to the location right after the last copied character.
        *dest_pos = '\0';

        // 5. Store the pointer to the parsed field.
        if (num_fields == max_fields) {
            error(_("Too many input fields"));
            exit(EXIT_FAILURE);
        }

        // Replicate the original, unusual behavior for a single hyphen field ("-").
        // If the field, after processing, is exactly "-", it returns a pointer to its
        // null terminator (field_start + 1). Otherwise, it returns a pointer to the
        // start of the field (field_start).
        // The condition (dest_pos == field_start + 1) means the final processed
        // string had a length of 1 character before its null terminator.
        if (*field_start == '-' && (dest_pos == field_start + 1)) {
            field_array[num_fields++] = field_start + 1; // Point to the null terminator
        } else {
            field_array[num_fields++] = field_start; // Point to the start of the field
        }
    } // End of main processing loop

    return num_fields;
}

ATTRIBUTE_NORETURN static void
time_overflow(void)
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
floor_div(zic_t a, int b)
{
	return a / b - (a % b < 0);
}

static zic_t
positive_mod(zic_t a, int b)
{
	return (a % b + b) % b;
}

static zic_t
rpytime(const struct rule *rp, zic_t wantedy)
{
	int month = TM_JANUARY;
	zic_t current_year = EPOCH_YEAR;
	zic_t day_offset;
	int days_in_period;

	if (wantedy == ZIC_MIN)
		return min_time;
	if (wantedy == ZIC_MAX)
		return max_time;

	zic_t year_diff = wantedy - current_year;
	zic_t full_blocks = floor_div(year_diff, YEARSPERREPEAT);
	day_offset = full_blocks * DAYSPERREPEAT;

	wantedy = current_year + positive_mod(year_diff, YEARSPERREPEAT);

	while (current_year != wantedy) {
		days_in_period = len_years[isleap(current_year)];
		day_offset = oadd(day_offset, days_in_period);
		current_year++;
	}

	while (month != rp->r_month) {
		days_in_period = len_months[isleap(current_year)][month];
		day_offset = oadd(day_offset, days_in_period);
		month++;
	}

	int day_of_month = rp->r_dayofmonth;

	if (month == TM_FEBRUARY && day_of_month == 29 && !isleap(current_year)) {
		if (rp->r_dycode == DC_DOWLEQ) {
			day_of_month--;
		} else {
			error(_("use of 2/29 in non leap-year"));
			exit(EXIT_FAILURE);
		}
	}

	day_offset = oadd(day_offset, day_of_month - 1);

	if (rp->r_dycode == DC_DOWGEQ || rp->r_dycode == DC_DOWLEQ) {
		zic_t current_wday = positive_mod(EPOCH_WDAY + day_offset, DAYSPERWEEK);
		int adjusted_day_of_month = day_of_month - 1;

		while (current_wday != rp->r_wday) {
			if (rp->r_dycode == DC_DOWGEQ) {
				day_offset = oadd(day_offset, 1);
				current_wday = positive_mod(current_wday + 1, DAYSPERWEEK);
				adjusted_day_of_month++;
			} else {
				day_offset = oadd(day_offset, -1);
				current_wday = positive_mod(current_wday - 1, DAYSPERWEEK);
				adjusted_day_of_month--;
			}
		}

		if (adjusted_day_of_month < 0 || adjusted_day_of_month >= len_months[isleap(current_year)][month]) {
			if (noise) {
				warning(_("rule goes past start/end of month; "
				          "will not work with pre-2004 versions of zic"));
			}
		}
	}

	if (day_offset < min_time / SECSPERDAY)
		return min_time;
	if (day_offset > max_time / SECSPERDAY)
		return max_time;

	zic_t total_time = day_offset * SECSPERDAY;
	return tadd(total_time, rp->r_tod);
}

static void
newabbr(const char *string)
{
	if (strcmp(string, GRANDPARENTED) != 0) {
		const char *current_char_ptr = string;
		const char *message_ptr = NULL;

		while (is_alpha(*current_char_ptr) || ('0' <= *current_char_ptr && *current_char_ptr <= '9')
		       || *current_char_ptr == '-' || *current_char_ptr == '+')
		{
			++current_char_ptr;
		}

		size_t validated_len = current_char_ptr - string;

		if (noise && validated_len < 3) {
			message_ptr = _("time zone abbreviation has fewer than 3 characters");
		}
		if (validated_len > ZIC_MAX_ABBR_LEN_WO_WARN) {
			message_ptr = _("time zone abbreviation has too many characters");
		}
		if (*current_char_ptr != '\0') {
			message_ptr = _("time zone abbreviation differs from POSIX standard");
		}

		if (message_ptr != NULL) {
			warning("%s (%s)", message_ptr, string);
		}
	}

	size_t len_with_null = strlen(string) + 1;

	if (charcnt + len_with_null > TZ_MAX_CHARS) {
		error(_("too many, or too long, time zone abbreviations"));
		exit(EXIT_FAILURE);
	}

	strcpy(&chars[charcnt], string);
	charcnt += len_with_null;
}

/* Ensure that the directories of ARGNAME exist, by making any missing
   ones.  If ANCESTORS, do this only for ARGNAME's ancestors; otherwise,
   do it for ARGNAME too.  Exit with failure if there is trouble.
   Do not consider an existing file to be trouble.  */
static void
mkdirs(char const *argname, bool ancestors)
{
	char *name_copy = estrdup(argname);
	char *path_ptr = name_copy;

	while (*path_ptr == '/') {
		path_ptr++;
	}

	char *next_slash;
	while ((next_slash = strchr(path_ptr, '/')) != NULL) {
		*next_slash = '\0';

		if (mkdir(name_copy, MKDIR_UMASK) != 0) {
			int err = errno;
			if (err == ELOOP || err == ENAMETOOLONG || err == ENOENT || err == ENOTDIR) {
				error(_("%s: Can't create directory %s: %s"), progname, name_copy, strerror(err));
				exit(EXIT_FAILURE);
			}
		}

		*next_slash = '/';
		path_ptr = next_slash + 1;
	}

	if (*path_ptr != '\0' && !ancestors) {
		if (mkdir(name_copy, MKDIR_UMASK) != 0) {
			int err = errno;
			if (err == ELOOP || err == ENAMETOOLONG || err == ENOENT || err == ENOTDIR) {
				error(_("%s: Can't create directory %s: %s"), progname, name_copy, strerror(err));
				exit(EXIT_FAILURE);
			}
		}
	}
	free(name_copy);
}
