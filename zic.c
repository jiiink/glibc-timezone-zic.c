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
static ssize_t readlink(char const *restrict file, char *restrict buf, size_t size)
{
  errno = ENOTSUP;
  return -1;
}
static int
symlink(char const *target, char const *linkname)
{
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
static char *strdup(char const *str)
{
    if (str == NULL) {
        return NULL;
    }
    
    size_t len = strlen(str) + 1;
    char *result = malloc(len);
    
    if (result == NULL) {
        return NULL;
    }
    
    strcpy(result, str);
    return result;
}
#endif

static void *memcheck(void *ptr)
{
	if (ptr == NULL) {
		const char *error_msg = strerror(HAVE_MALLOC_ERRNO ? errno : ENOMEM);
		memory_exhausted(error_msg);
	}
	return ptr;
}

static void *
emalloc(size_t size)
{
  return memcheck(malloc(size));
}

static void *erealloc(void *ptr, size_t size)
{
    void *new_ptr = realloc(ptr, size);
    return memcheck(new_ptr);
}

static char *estrdup(char const *str)
{
    char *result = strdup(str);
    return memcheck(result);
}

static ptrdiff_t calculate_addend(ptrdiff_t nitems_alloc)
{
    return (nitems_alloc >> 1) + 1;
}

static bool is_overflow_safe_add(ptrdiff_t *result, ptrdiff_t a, ptrdiff_t b)
{
#if defined ckd_add
    return !ckd_add(result, a, b);
#else
    if (a > INDEX_MAX - b)
        return false;
    *result = a + b;
    return true;
#endif
}

static bool is_overflow_safe_mul(ptrdiff_t *result, ptrdiff_t a, ptrdiff_t b)
{
#if defined ckd_mul
    return !ckd_mul(result, a, b);
#else
    if (b != 0 && a > INDEX_MAX / b)
        return false;
    *result = a * b;
    return true;
#endif
}

static bool can_grow_allocation(ptrdiff_t *new_nitems, ptrdiff_t *new_size, 
                                ptrdiff_t current_nitems, ptrdiff_t itemsize)
{
    ptrdiff_t addend = calculate_addend(current_nitems);
    
    if (!is_overflow_safe_add(new_nitems, current_nitems, addend))
        return false;
    
    if (!is_overflow_safe_mul(new_size, *new_nitems, itemsize))
        return false;
    
    return *new_size <= INDEX_MAX;
}

static ptrdiff_t grow_nitems_alloc(ptrdiff_t *nitems_alloc, ptrdiff_t itemsize)
{
    ptrdiff_t new_nitems, new_size;
    
    if (can_grow_allocation(&new_nitems, &new_size, *nitems_alloc, itemsize)) {
        *nitems_alloc = new_nitems;
        return new_size;
    }
    
    memory_exhausted(_("integer overflow"));
}

static void *
growalloc(void *ptr, ptrdiff_t itemsize, ptrdiff_t nitems,
	  ptrdiff_t *nitems_alloc)
{
  if (nitems < *nitems_alloc) {
    return ptr;
  }
  return erealloc(ptr, grow_nitems_alloc(nitems_alloc, itemsize));
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
    return _("command line");
  
  if (i == LEAPSEC_FILENUM)
    return strcmp(leapsec, "-") == 0 ? _("standard input") : leapsec;
  
  char const *fname = main_argv[i];
  return strcmp(fname, "-") == 0 ? _("standard input") : fname;
}

static void
eats(int fnum, lineno num, int rfnum, lineno rnum)
{
	filenum = fnum;
	linenum = num;
	rfilenum = rfnum;
	rlinenum = rnum;
}

static void
eat(int fnum, lineno num)
{
	eats(fnum, num, 0, -1);
}

ATTRIBUTE_FORMAT((printf, 1, 0)) static void print_file_location(FILE *stream, const char *prefix, int filenum, intmax_t linenum)
{
    if (filenum) {
        fprintf(stream, prefix, filename(filenum), linenum);
    }
}

static void print_rule_location(FILE *stream, int rfilenum, intmax_t rlinenum)
{
    if (rfilenum) {
        fprintf(stream, _(" (rule from \"%s\", line %"PRIdMAX")"), 
                filename(rfilenum), rlinenum);
    }
}

static void verror(const char *const string, va_list args)
{
    print_file_location(stderr, _("\"%s\", line %"PRIdMAX": "), filenum, linenum);
    vfprintf(stderr, string, args);
    print_rule_location(stderr, rfilenum, rlinenum);
    fprintf(stderr, "\n");
}

ATTRIBUTE_FORMAT((printf, 1, 2)) static void error(const char *const string, ...)
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
	fprintf(stderr, _("warning: "));
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
  char const *e = get_error_message(stream);
  if (e) {
    print_error_message(dir, name, e);
    if (tempname)
      remove(tempname);
    exit(EXIT_FAILURE);
  }
}

static char const *
get_error_message(FILE *stream)
{
  if (ferror(stream))
    return _("I/O error");
  if (fclose(stream) != 0)
    return strerror(errno);
  return NULL;
}

static void
print_error_message(char const *dir, char const *name, char const *error)
{
  fprintf(stderr, "%s: %s%s%s%s%s\n", progname,
          dir ? dir : "", 
          dir ? "/" : "",
          name ? name : "", 
          name ? ": " : "",
          error);
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
  if (status == EXIT_SUCCESS)
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
static int compare_link_names(struct link const *l, struct link const *m)
{
    return strcmp(l->l_linkname, m->l_linkname);
}

static int compare_file_numbers(struct link const *l, struct link const *m)
{
    return l->l_filenum - m->l_filenum;
}

static int compare_line_numbers(struct link const *l, struct link const *m)
{
    return (l->l_linenum > m->l_linenum) - (l->l_linenum < m->l_linenum);
}

static int
qsort_linkcmp(void const *a, void const *b)
{
    struct link const *l = a;
    struct link const *m = b;
    
    int cmp = compare_link_names(l, m);
    if (cmp)
        return cmp;
    
    cmp = compare_file_numbers(l, m);
    if (cmp)
        return cmp;
    
    return compare_line_numbers(l, m);
}

/* Compare the string KEY to the link B, for bsearch.  */
static int bsearch_linkcmp(void const *key, void const *b)
{
  struct link const *m = b;
  return strcmp(key, m->l_linkname);
}

/* Make the links specified by the Link lines.  */
static void sort_links_if_needed(void)
{
    if (1 < nlinks)
        qsort(links, nlinks, sizeof *links, qsort_linkcmp);
}

static ptrdiff_t remove_duplicate_links(void)
{
    ptrdiff_t i, j = 0;
    for (i = 0; i < nlinks; i++) {
        while (i + 1 < nlinks
               && strcmp(links[i].l_linkname, links[i + 1].l_linkname) == 0)
            i++;
        links[j++] = links[i];
    }
    return j;
}

static int is_self_link(struct link *link)
{
    return strcmp(link->l_target, link->l_linkname) == 0;
}

static void report_self_link_error(struct link *link)
{
    error(_("link %s targets itself"), link->l_target);
}

static void report_cycle_error(struct link *link)
{
    error(_("\"Link %s %s\" is part of a link cycle"),
          link->l_target, link->l_linkname);
}

static struct link* find_link_target(struct link *target_name, ptrdiff_t i, ptrdiff_t j, ptrdiff_t nalinks)
{
    struct link *l = bsearch(target_name->l_target, &links[i + 1], j - (i + 1),
                            sizeof *links, bsearch_linkcmp);
    if (!l)
        l = bsearch(target_name->l_target, &links[j], nalinks - j,
                   sizeof *links, bsearch_linkcmp);
    return l;
}

static void append_link_copy(ptrdiff_t i, ptrdiff_t *nalinks)
{
    links = growalloc(links, sizeof *links, *nalinks, &nlinks_alloc);
    links[(*nalinks)++] = links[i];
}

static void process_link(ptrdiff_t i, ptrdiff_t j, ptrdiff_t *nalinks)
{
    struct link *l = find_link_target(&links[i], i, j, *nalinks);
    if (!l)
        dolink(links[i].l_target, links[i].l_linkname, false);
    else
        append_link_copy(i, nalinks);
}

static void emit_link_warnings(ptrdiff_t i, struct link *l)
{
    if (!noise || i >= nlinks)
        return;
    
    if (l)
        warning(_("link %s targeting link %s mishandled by pre-2023 zic"),
                links[i].l_linkname, links[i].l_target);
    else if (bsearch(links[i].l_target, links, nlinks, sizeof *links,
                     bsearch_linkcmp))
        warning(_("link %s targeting link %s"),
                links[i].l_linkname, links[i].l_target);
}

static int check_pass_completion(ptrdiff_t i, ptrdiff_t *j, ptrdiff_t nalinks, ptrdiff_t *pass_size)
{
    if (i != *j)
        return 0;
    
    if (nalinks - i == *pass_size) {
        report_cycle_error(&links[i]);
        return 1;
    }
    *j = nalinks;
    *pass_size = nalinks - i;
    return 0;
}

static void make_links(void)
{
    ptrdiff_t i, j, nalinks, pass_size;
    
    sort_links_if_needed();
    nlinks = pass_size = remove_duplicate_links();
    j = nalinks = nlinks;

    for (i = 0; i < nalinks; i++) {
        eat(links[i].l_filenum, links[i].l_linenum);

        if (check_pass_completion(i, &j, nalinks, &pass_size))
            break;

        if (is_self_link(&links[i])) {
            report_self_link_error(&links[i]);
            continue;
        }

        struct link *l = find_link_target(&links[i], i, j, nalinks);
        if (!l)
            dolink(links[i].l_target, links[i].l_linkname, false);
        else
            append_link_copy(i, &nalinks);

        emit_link_warnings(i, l);
    }
}

/* Simple signal handling: just set a flag that is checked
   periodically outside critical sections.  To set up the handler,
   prefer sigaction if available to close a signal race.  */

static sig_atomic_t got_signal;

static void signal_handler(int sig)
{
#ifndef SA_SIGINFO
    signal(sig, signal_handler);
#endif
    got_signal = sig;
}

/* Arrange for SIGINT etc. to be caught by the handler.  */
static void setup_signal_action(int signal)
{
#ifdef SA_SIGINFO
    struct sigaction act0, act;
    act.sa_handler = signal_handler;
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    if (sigaction(signal, &act, &act0) == 0
        && !(act0.sa_flags & SA_SIGINFO) && act0.sa_handler == SIG_IGN) {
        sigaction(signal, &act0, NULL);
        got_signal = 0;
    }
#else
    if (signal(signal, signal_handler) == SIG_IGN) {
        signal(signal, SIG_IGN);
        got_signal = 0;
    }
#endif
}

static void catch_signals(void)
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
    
    const int signal_count = sizeof signals / sizeof signals[0];
    int i;
    
    for (i = 0; i < signal_count; i++) {
        setup_signal_action(signals[i]);
    }
}

/* If a signal has arrived, terminate zic with appropriate status.  */
static void check_for_signal(void)
{
    int sig = got_signal;
    if (!sig) {
        return;
    }
    
    signal(sig, SIG_DFL);
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
static bool parse_timestamp(const char *str, intmax_t *result)
{
    char *end;
    errno = 0;
    *result = strtoimax(str, &end, 10);
    return end != str && !(*result == INTMAX_MAX && errno == ERANGE);
}

static bool parse_lower_bound(char *timerange, intmax_t *lo, char **end)
{
    if (*timerange != '@') {
        *lo = min_time;
        *end = timerange;
        return true;
    }
    
    if (!parse_timestamp(timerange + 1, lo)) {
        return false;
    }
    
    *end = timerange + 1;
    while (**end && **end != '/') {
        (*end)++;
    }
    return true;
}

static bool parse_upper_bound(char *lo_end, intmax_t *hi, char **end)
{
    *end = lo_end;
    
    if (lo_end[0] != '/' || lo_end[1] != '@') {
        *hi = max_time;
        return true;
    }
    
    char *parse_start = lo_end + 2;
    char *temp_end;
    errno = 0;
    *hi = strtoimax(parse_start, &temp_end, 10);
    
    if (temp_end == parse_start || *hi == INTMAX_MIN) {
        return false;
    }
    
    if (!(*hi == INTMAX_MAX && errno == ERANGE)) {
        (*hi)--;
    }
    
    *end = temp_end;
    return true;
}

static bool validate_range(intmax_t lo, intmax_t hi, char *hi_end)
{
    return !*hi_end && 
           hi >= lo && 
           lo <= max_time && 
           hi >= min_time;
}

static bool timerange_option(char *timerange)
{
    intmax_t lo, hi;
    char *lo_end, *hi_end;
    
    if (!parse_lower_bound(timerange, &lo, &lo_end)) {
        return false;
    }
    
    if (!parse_upper_bound(lo_end, &hi, &hi_end)) {
        return false;
    }
    
    if (!validate_range(lo, hi, hi_end)) {
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
  
  intmax_t redundant;
  char *opt_end;
  redundant = strtoimax(opt + 1, &opt_end, 10);
  
  if (opt_end == opt + 1 || *opt_end) {
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

static bool want_bloat(void)
{
    return bloat >= 0;
}

#ifndef ZIC_BLOAT_DEFAULT
# define ZIC_BLOAT_DEFAULT "slim"
#endif

int handle_version_help(int argc, char **argv)
{
	for (int k = 1; k < argc; k++) {
		if (strcmp(argv[k], "--version") == 0) {
			printf("zic %s%s\n", PKGVERSION, TZVERSION);
			close_file(stdout, NULL, NULL, NULL);
			return 1;
		}
		if (strcmp(argv[k], "--help") == 0) {
			usage(stdout, EXIT_SUCCESS);
			return 1;
		}
	}
	return 0;
}

int validate_option_not_duplicate(const char *current, const char *option_name)
{
	if (current != NULL) {
		fprintf(stderr, _("%s: More than one %s option specified\n"),
			progname, option_name);
		return EXIT_FAILURE;
	}
	return 0;
}

int handle_b_option(const char *optarg)
{
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
	return 0;
}

int handle_r_option(const char *optarg, bool *timerange_given)
{
	if (*timerange_given) {
		fprintf(stderr, _("%s: More than one -r option specified\n"),
			progname);
		return EXIT_FAILURE;
	}
	if (!timerange_option(optarg)) {
		fprintf(stderr, _("%s: invalid time range: %s\n"),
			progname, optarg);
		return EXIT_FAILURE;
	}
	*timerange_given = true;
	return 0;
}

int handle_R_option(const char *optarg)
{
	if (!redundant_time_option(optarg)) {
		fprintf(stderr, _("%s: invalid time: %s\n"),
			progname, optarg);
		return EXIT_FAILURE;
	}
	return 0;
}

int process_command_options(int argc, char **argv, bool *timerange_given)
{
	int c;
	while ((c = getopt(argc, argv, "b:d:l:L:p:r:R:st:vy:")) != EOF && c != -1) {
		switch (c) {
		default:
			usage(stderr, EXIT_FAILURE);
		case 'b':
			handle_b_option(optarg);
			break;
		case 'd':
			if (validate_option_not_duplicate(directory, "-d") != 0)
				return EXIT_FAILURE;
			directory = optarg;
			break;
		case 'l':
			if (validate_option_not_duplicate(lcltime, "-l") != 0)
				return EXIT_FAILURE;
			lcltime = optarg;
			break;
		case 'p':
			if (validate_option_not_duplicate(psxrules, "-p") != 0)
				return EXIT_FAILURE;
			psxrules = optarg;
			break;
		case 't':
			if (validate_option_not_duplicate(tzdefault, "-t") != 0)
				return EXIT_FAILURE;
			tzdefault = optarg;
			break;
		case 'y':
			warning(_("-y ignored"));
			break;
		case 'L':
			if (validate_option_not_duplicate(leapsec, "-L") != 0)
				return EXIT_FAILURE;
			leapsec = optarg;
			break;
		case 'v':
			noise = true;
			break;
		case 'r':
			if (handle_r_option(optarg, timerange_given) != 0)
				return EXIT_FAILURE;
			break;
		case 'R':
			if (handle_R_option(optarg) != 0)
				return EXIT_FAILURE;
			break;
		case 's':
			warning(_("-s ignored"));
			break;
		}
	}
	return 0;
}

void initialize_defaults(void)
{
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
}

void process_input_files(int argc, char **argv)
{
	if (optind < argc && leapsec != NULL) {
		infile(LEAPSEC_FILENUM, leapsec);
		adjleap();
	}
	for (int k = optind; k < argc; k++)
		infile(k, argv[k]);
}

void generate_output_zones(void)
{
	ptrdiff_t i, j;
	for (i = 0; i < nzones; i = j) {
		for (j = i + 1; j < nzones && zones[j].z_name == NULL; ++j)
			continue;
		outzone(&zones[i], j - i);
	}
}

void create_symlinks(void)
{
	make_links();
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
	bool timerange_given = false;

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
	main_argv = argv;
	progname = argv[0] ? argv[0] : "zic";
	if (TYPE_BIT(zic_t) < 64) {
		fprintf(stderr, "%s: %s\n", progname,
			_("wild compilation-time specification of zic_t"));
		return EXIT_FAILURE;
	}

	if (handle_version_help(argc, argv))
		return EXIT_SUCCESS;

	if (process_command_options(argc, argv, &timerange_given) != 0)
		return EXIT_FAILURE;

	if (optind == argc - 1 && strcmp(argv[optind], "=") == 0)
		usage(stderr, EXIT_FAILURE);

	if (hi_time + (hi_time < ZIC_MAX) < redundant_time) {
		fprintf(stderr, _("%s: -R time exceeds -r cutoff\n"), progname);
		return EXIT_FAILURE;
	}
	if (redundant_time < lo_time)
		redundant_time = lo_time;

	initialize_defaults();
	process_input_files(argc, argv);

	if (errors)
		return EXIT_FAILURE;

	associate();
	change_directory(directory);
	catch_signals();
	generate_output_zones();
	create_symlinks();

	if (warnings && (ferror(stderr) || fclose(stderr) != 0))
		return EXIT_FAILURE;

	return errors ? EXIT_FAILURE : EXIT_SUCCESS;
}

static bool is_empty_component(char const *component, char const *component_end)
{
    return component_end - component == 0;
}

static bool is_dot_component(char const *component, char const *component_end)
{
    ptrdiff_t len = component_end - component;
    return len > 0 && len <= 2 && component[0] == '.' && component_end[-1] == '.';
}

static void report_empty_component_error(char const *name, char const *component, char const *component_end)
{
    if (!*name)
        error(_("empty file name"));
    else if (component == name)
        error(_("file name '%s' begins with '/'"), name);
    else if (*component_end)
        error(_("file name '%s' contains '///'"), name);
    else
        error(_("file name '%s' ends with '/'"), name);
}

static void report_dot_component_error(char const *name, char const *component, ptrdiff_t component_len)
{
    int len = component_len;
    error(_("file name '%s' contains '%.*s' component"), name, len, component);
}

static void check_component_warnings(char const *name, char const *component, ptrdiff_t component_len)
{
    enum { component_len_max = 14 };
    
    if (!noise)
        return;
    
    if (component_len > 0 && component[0] == '-')
        warning(_("file name '%s' component contains leading '-'"), name);
    
    if (component_len > component_len_max)
        warning(_("file name '%s' contains overlength component '%.*s...'"),
                name, component_len_max, component);
}

static bool componentcheck(char const *name, char const *component, char const *component_end)
{
    ptrdiff_t component_len = component_end - component;
    
    if (is_empty_component(component, component_end)) {
        report_empty_component_error(name, component, component_end);
        return false;
    }
    
    if (is_dot_component(component, component_end)) {
        report_dot_component_error(name, component, component_len);
        return false;
    }
    
    check_component_warnings(name, component, component_len);
    
    return true;
}

static bool
namecheck(const char *name)
{
	static char const benign[] =
	  "-/_"
	  "abcdefghijklmnopqrstuvwxyz"
	  "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

	static char const printable_and_not_benign[] =
	  " !\"#$%&'()*+,.0123456789:;<=>?@[\\]^`{|}~";

	register char const *component = name;
	register char const *cp;
	
	for (cp = name; *cp; cp++) {
		unsigned char c = *cp;
		
		if (noise && !strchr(benign, c)) {
			const char *format = strchr(printable_and_not_benign, c)
				? _("file name '%s' contains byte '%c'")
				: _("file name '%s' contains byte '\\%o'");
			warning(format, name, c);
		}
		
		if (c == '/') {
			if (!componentcheck(name, component, cp))
				return false;
			component = cp + 1;
		}
	}
	
	return componentcheck(name, component, cp);
}

/* Return a random uint_fast64_t.  */
static uint_fast64_t try_getrandom(void)
{
#if HAVE_GETRANDOM
  static uint_fast64_t entropy_buffer[max(1, 256 / sizeof(uint_fast64_t))];
  static int nwords;
  
  if (!nwords) {
    ssize_t s;
    do
      s = getrandom(entropy_buffer, sizeof entropy_buffer, 0);
    while (s < 0 && errno == EINTR);
    
    nwords = s < 0 ? -1 : s / sizeof *entropy_buffer;
  }
  
  if (0 < nwords)
    return entropy_buffer[--nwords];
#endif
  return 0;
}

static void ensure_rand_initialized(void)
{
  static bool initialized;
  if (!initialized) {
    srand(time(NULL));
    initialized = true;
  }
}

static uint_fast64_t calculate_nrand(uint_fast64_t rand_max)
{
  return rand_max < UINT_FAST64_MAX ? rand_max + 1 : 0;
}

static uint_fast64_t calculate_rmod(uint_fast64_t nrand)
{
  return INT_MAX < UINT_FAST64_MAX ? 0 : UINT_FAST64_MAX / nrand + 1;
}

static void adjust_for_rmod(uint_fast64_t *value, uint_fast64_t rmod)
{
  if (rmod)
    *value %= rmod;
}

static uint_fast64_t update_rmax(uint_fast64_t rmax, uint_fast64_t nrand, uint_fast64_t rand_max, uint_fast64_t rmod)
{
  uint_fast64_t rmax1 = rmax;
  adjust_for_rmod(&rmax1, rmod);
  rmax1 = nrand * rmax1 + rand_max;
  return rmax < rmax1 ? rmax1 : UINT_FAST64_MAX;
}

static uint_fast64_t generate_fallback_random(void)
{
  ensure_rand_initialized();
  
  uint_fast64_t rand_max = RAND_MAX;
  uint_fast64_t nrand = calculate_nrand(rand_max);
  uint_fast64_t rmod = calculate_rmod(nrand);
  uint_fast64_t r = 0;
  uint_fast64_t rmax = 0;
  
  do {
    rmax = update_rmax(rmax, nrand, rand_max, rmod);
    adjust_for_rmod(&r, rmod);
    r = nrand * r + rand();
  } while (rmax < UINT_FAST64_MAX);
  
  return r;
}

static uint_fast64_t get_rand_u64(void)
{
#if HAVE_GETRANDOM
  uint_fast64_t result = try_getrandom();
  if (result != 0 || errno != ENOSYS)
    return result;
#endif
  
  return generate_fallback_random();
}

/* Generate a randomish name in the same directory as *NAME.  If
   *NAMEALLOC, put the name into *NAMEALLOC which is assumed to be
   that returned by a previous call and is thus already almost set up
   and equal to *NAME; otherwise, allocate a new name and put its
   address into both *NAMEALLOC and *NAME.  */
static void
random_dirent(char const **name, char **namealloc)
{
  enum { PREFIX_LEN = 4, SUFFIX_LEN = 6, ALPHABET_LEN = 62 };
  static char const prefix[] = ".zic";
  static char const alphabet[] =
    "abcdefghijklmnopqrstuvwxyz"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "0123456789";

  char const *src = *name;
  char *dst = *namealloc;
  
  if (!dst) {
    dst = allocate_name_buffer(src, name, namealloc);
  }

  uint_fast64_t r = get_unbiased_random();
  generate_random_suffix(dst, get_dir_length(src), r);
}

static ptrdiff_t get_dir_length(char const *path)
{
  char const *lastslash = strrchr(path, '/');
  return lastslash ? lastslash + 1 - path : 0;
}

static char *allocate_name_buffer(char const *src, char const **name, char **namealloc)
{
  enum { PREFIX_LEN = 4, SUFFIX_LEN = 6 };
  static char const prefix[] = ".zic";
  
  ptrdiff_t dirlen = get_dir_length(src);
  char *dst = emalloc(size_sum(dirlen, PREFIX_LEN + SUFFIX_LEN + 1));
  
  memcpy(dst, src, dirlen);
  memcpy(dst + dirlen, prefix, PREFIX_LEN);
  dst[dirlen + PREFIX_LEN + SUFFIX_LEN] = '\0';
  
  *name = *namealloc = dst;
  return dst;
}

static uint_fast64_t get_unbiased_random(void)
{
  enum { ALPHABET_LEN = 62, SUFFIX_LEN = 6 };
  uint_fast64_t base = ALPHABET_LEN;
  uint_fast64_t base__6 = base * base * base * base * base * base;
  uint_fast64_t unfair_min = - ((UINTMAX_MAX % base__6 + 1) % base__6);
  
  uint_fast64_t r;
  do {
    r = get_rand_u64();
  } while (unfair_min <= r);
  
  return r;
}

static void generate_random_suffix(char *dst, ptrdiff_t dirlen, uint_fast64_t r)
{
  enum { PREFIX_LEN = 4, SUFFIX_LEN = 6, ALPHABET_LEN = 62 };
  static char const alphabet[] =
    "abcdefghijklmnopqrstuvwxyz"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "0123456789";
  
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

  if (!*tempname)
    random_dirent(outname, tempname);

  return try_open_with_retry(outname, tempname, fopen_mode);
}

static FILE *
try_open_with_retry(char const **outname, char **tempname, char const *mode)
{
  FILE *fp;
  bool dirs_made = false;

  while (!(fp = fopen(*outname, mode))) {
    if (!handle_open_error(errno, outname, tempname, &dirs_made))
      exit_with_error(*outname, errno);
  }

  return fp;
}

static bool
handle_open_error(int error_code, char const **outname, char **tempname, bool *dirs_made)
{
  if (error_code == ENOENT && !*dirs_made) {
    mkdirs(*outname, true);
    *dirs_made = true;
    return true;
  }
  
  if (error_code == EEXIST) {
    random_dirent(outname, tempname);
    return true;
  }
  
  return false;
}

static void
exit_with_error(char const *outname, int error_code)
{
  fprintf(stderr, _("%s: Can't create %s/%s: %s\n"),
          progname, directory, outname, strerror(error_code));
  exit(EXIT_FAILURE);
}

/* If TEMPNAME, the result is in the temporary file TEMPNAME even
   though the user wanted it in NAME, so rename TEMPNAME to NAME.
   Report an error and exit if there is trouble.  Also, free TEMPNAME.  */
static void
rename_dest(char *tempname, char const *name)
{
  if (!tempname) {
    return;
  }
  
  if (rename(tempname, name) == 0) {
    free(tempname);
    return;
  }
  
  int rename_errno = errno;
  remove(tempname);
  fprintf(stderr, _("%s: rename to %s/%s: %s\n"),
          progname, directory, name, strerror(rename_errno));
  exit(EXIT_FAILURE);
}

/* Create symlink contents suitable for symlinking TARGET to LINKNAME, as a
   freshly allocated string.  TARGET should be a relative file name, and
   is relative to the global variable DIRECTORY.  LINKNAME can be either
   relative or absolute.  */
static char *make_absolute_path(char const *target)
{
  size_t len = strlen(directory);
  size_t lenslash = len + (len && directory[len - 1] != '/');
  size_t targetsize = strlen(target) + 1;
  ptrdiff_t linksize = size_sum(lenslash, targetsize);
  char *result = emalloc(linksize);
  memcpy(result, directory, len);
  result[len] = '/';
  memcpy(result + lenslash, target, targetsize);
  return result;
}

static size_t find_common_prefix_length(char const *f, char const *linkname)
{
  size_t i, dir_len = 0;
  for (i = 0; f[i] && f[i] == linkname[i]; i++) {
    if (f[i] == '/')
      dir_len = i + 1;
  }
  return dir_len;
}

static size_t count_parent_dirs(char const *linkname, size_t start_pos)
{
  size_t i, dotdots = 0;
  for (i = start_pos; linkname[i]; i++) {
    dotdots += linkname[i] == '/' && linkname[i - 1] != '/';
  }
  return dotdots;
}

static void add_parent_dir_prefixes(char *result, size_t dotdots)
{
  size_t i;
  for (i = 0; i < dotdots; i++) {
    memcpy(result + 3 * i, "../", 3);
  }
}

static char *build_relative_path(char const *f, size_t dir_len, size_t dotdots, ptrdiff_t linksize)
{
  size_t taillen = strlen(f + dir_len);
  ptrdiff_t dotdotetcsize = size_sum(size_product(dotdots, 3), taillen + 1);
  
  if (dotdotetcsize > linksize)
    return NULL;
  
  char *result = emalloc(dotdotetcsize);
  add_parent_dir_prefixes(result, dotdots);
  memmove(result + 3 * dotdots, f + dir_len, taillen + 1);
  return result;
}

static char *relname(char const *target, char const *linkname)
{
  ptrdiff_t linksize = INDEX_MAX;
  char const *f = target;
  char *result = NULL;
  
  if (*linkname == '/') {
    result = make_absolute_path(target);
    f = result;
    linksize = strlen(result) + 1;
  }
  
  size_t dir_len = find_common_prefix_length(f, linkname);
  size_t dotdots = count_parent_dirs(linkname, dir_len);
  
  if (!result) {
    result = build_relative_path(f, dir_len, dotdots, linksize);
  } else {
    char *relative = build_relative_path(f, dir_len, dotdots, linksize);
    if (relative) {
      free(result);
      result = relative;
    }
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

static void handle_remove_target(const char *linkname) {
    if (remove(linkname) == 0 || errno == ENOENT || errno == ENOTDIR)
        return;
    
    const char *e = strerror(errno);
    fprintf(stderr, _("%s: Can't remove %s/%s: %s\n"),
            progname, directory, linkname, e);
    exit(EXIT_FAILURE);
}

static int try_linkat(const char *target, const char *outname) {
    if (linkat(AT_FDCWD, target, AT_FDCWD, outname, AT_SYMLINK_FOLLOW) == 0)
        return 0;
    
    int link_errno = errno;
    if (link_errno == EINVAL)
        link_errno = ENOTSUP;
    return link_errno;
}

static int try_link_fallback(const char *target, const char *outname, int *targetissym) {
#if HAVE_LINK
    if (same_parent_dirs(target, outname) || 0 <= itssymlink(target, targetissym)) {
        if (link(target, outname) == 0)
            return 0;
        return errno;
    }
#endif
    return ENOTSUP;
}

static int attempt_hard_link(const char *target, char **outname, char **tempname,
                            bool *staysymlink, bool *linkdirs_made, 
                            int *targetissym, int *linknameissym, const char *linkname) {
    int link_errno = try_linkat(target, *outname);
    
    if (link_errno == 0)
        return 0;
    
    if (link_errno == ENOTSUP) {
        link_errno = try_link_fallback(target, *outname, targetissym);
        if (link_errno == 0)
            return 0;
    }
    
    if (link_errno == EXDEV || link_errno == ENOTSUP)
        return link_errno;
    
    if (link_errno == EEXIST) {
        *staysymlink &= !*tempname;
        random_dirent(outname, tempname);
        if (*staysymlink && itssymlink(linkname, linknameissym))
            return link_errno;
        return -1;
    }
    
    if (link_errno == ENOENT && !*linkdirs_made) {
        mkdirs(linkname, true);
        *linkdirs_made = true;
        return -1;
    }
    
    fprintf(stderr, _("%s: Can't link %s/%s to %s/%s: %s\n"),
            progname, directory, target, directory, *outname,
            strerror(link_errno));
    exit(EXIT_FAILURE);
}

static int create_symlink(const char *contents, char **outname, char **tempname,
                         bool *linkdirs_made, const char *linkname) {
    while (true) {
        if (symlink(contents, *outname) == 0)
            return 0;
        
        int symlink_errno = errno;
        
        if (symlink_errno == EEXIST) {
            random_dirent(outname, tempname);
            continue;
        }
        
        if (symlink_errno == ENOENT && !*linkdirs_made) {
            mkdirs(linkname, true);
            *linkdirs_made = true;
            continue;
        }
        
        return symlink_errno;
    }
}

static void copy_file_contents(const char *target, char **outname, char **tempname,
                               const char *linkname) {
    FILE *fp = fopen(target, "rb");
    if (!fp) {
        const char *e = strerror(errno);
        fprintf(stderr, _("%s: Can't read %s/%s: %s\n"),
                progname, directory, target, e);
        exit(EXIT_FAILURE);
    }
    
    FILE *tp = open_outfile(outname, tempname);
    int c;
    while ((c = getc(fp)) != EOF)
        putc(c, tp);
    
    close_file(tp, directory, linkname, *tempname);
    close_file(fp, directory, target, NULL);
}

static void handle_link_failure(const char *target, const char *linkname,
                               char **outname, char **tempname,
                               bool linkdirs_made, int link_errno) {
    bool absolute = *target == '/';
    char *linkalloc = absolute ? NULL : relname(target, linkname);
    const char *contents = absolute ? target : linkalloc;
    
    int symlink_errno = create_symlink(contents, outname, tempname, 
                                       &linkdirs_made, linkname);
    free(linkalloc);
    
    if (symlink_errno == 0) {
        if (link_errno != ENOTSUP && link_errno != EEXIST)
            warning(_("symbolic link used because hard link failed: %s"),
                   strerror(link_errno));
    } else {
        copy_file_contents(target, outname, tempname, linkname);
        
        if (link_errno != ENOTSUP)
            warning(_("copy used because hard link failed: %s"),
                   strerror(link_errno));
        else if (symlink_errno != ENOTSUP)
            warning(_("copy used because symbolic link failed: %s"),
                   strerror(symlink_errno));
    }
}

static void dolink(const char *target, const char *linkname, bool staysymlink) {
    bool linkdirs_made = false;
    char *tempname = NULL;
    const char *outname = linkname;
    int targetissym = -2, linknameissym = -2;
    
    check_for_signal();
    
    if (strcmp(target, "-") == 0) {
        handle_remove_target(linkname);
        return;
    }
    
    int link_errno;
    while (true) {
        link_errno = attempt_hard_link(target, (char**)&outname, &tempname, 
                                       &staysymlink, &linkdirs_made,
                                       &targetissym, &linknameissym, linkname);
        if (link_errno >= 0)
            break;
    }
    
    if (link_errno != 0) {
        handle_link_failure(target, linkname, (char**)&outname, &tempname,
                           linkdirs_made, link_errno);
    }
    
    rename_dest(tempname, linkname);
}

/* Return 1 if NAME is an absolute symbolic link, -1 if it is relative,
   0 if it is not a symbolic link.  If *CACHE is not -2, it is the
   cached result of a previous call to this function with the same NAME.  */
static int
itssymlink(char const *name, int *cache)
{
  #define CACHE_UNINITIALIZED -2
  #define NOT_SYMLINK 0
  #define ABSOLUTE_SYMLINK 1
  #define RELATIVE_SYMLINK -1
  
  if (*cache == CACHE_UNINITIALIZED) {
    char c = '\0';
    int read_result = readlink(name, &c, 1);
    
    if (read_result < 0) {
      *cache = NOT_SYMLINK;
    } else if (c == '/') {
      *cache = ABSOLUTE_SYMLINK;
    } else {
      *cache = RELATIVE_SYMLINK;
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
  const struct rule *r1 = cp1;
  const struct rule *r2 = cp2;
  return strcmp(r1->r_name, r2->r_name);
}

static void sort_rules_if_needed(void)
{
    if (1 < nrules) {
        qsort(rules, nrules, sizeof *rules, rcomp);
    }
}

static int rules_have_same_name(ptrdiff_t i, ptrdiff_t j)
{
    return strcmp(rules[i].r_name, rules[j].r_name) == 0;
}

static int rules_have_same_filenum(ptrdiff_t i, ptrdiff_t j)
{
    return rules[i].r_filenum == rules[j].r_filenum;
}

static void warn_duplicate_rule(ptrdiff_t idx)
{
    eat(rules[idx].r_filenum, rules[idx].r_linenum);
    warning(_("same rule name in multiple files"));
}

static ptrdiff_t find_last_duplicate_rule(ptrdiff_t start)
{
    ptrdiff_t j;
    for (j = start + 2; j < nrules; ++j) {
        if (!rules_have_same_name(start, j))
            break;
        if (rules_have_same_filenum(start, j))
            continue;
        if (rules_have_same_filenum(start + 1, j))
            continue;
        break;
    }
    return j - 1;
}

static void check_duplicate_rules(void)
{
    ptrdiff_t i;
    if (nrules <= 1)
        return;
    
    for (i = 0; i < nrules - 1; ++i) {
        if (!rules_have_same_name(i, i + 1))
            continue;
        if (rules_have_same_filenum(i, i + 1))
            continue;
        
        warn_duplicate_rule(i);
        warn_duplicate_rule(i + 1);
        i = find_last_duplicate_rule(i);
    }
}

static void initialize_zones(void)
{
    ptrdiff_t i;
    for (i = 0; i < nzones; ++i) {
        zones[i].z_rules = NULL;
        zones[i].z_nrules = 0;
    }
}

static ptrdiff_t find_rule_group_end(ptrdiff_t base)
{
    ptrdiff_t out;
    for (out = base + 1; out < nrules; ++out) {
        if (strcmp(rules[base].r_name, rules[out].r_name) != 0)
            break;
    }
    return out;
}

static void assign_rules_to_zone(struct zone *zp, struct rule *rp, ptrdiff_t base, ptrdiff_t out)
{
    if (strcmp(zp->z_rule, rp->r_name) == 0) {
        zp->z_rules = rp;
        zp->z_nrules = out - base;
    }
}

static void assign_rules_to_zones(void)
{
    ptrdiff_t base, out, i;
    
    for (base = 0; base < nrules; base = out) {
        out = find_rule_group_end(base);
        for (i = 0; i < nzones; ++i) {
            assign_rules_to_zone(&zones[i], &rules[base], base, out);
        }
    }
}

static void process_ruleless_zone(struct zone *zp)
{
    eat(zp->z_filenum, zp->z_linenum);
    zp->z_save = getsave(zp->z_rule, &zp->z_isdst);
    if (zp->z_format_specifier == 's')
        error("%s", _("%s in ruleless zone"));
}

static void process_ruleless_zones(void)
{
    ptrdiff_t i;
    for (i = 0; i < nzones; ++i) {
        if (zones[i].z_nrules == 0) {
            process_ruleless_zone(&zones[i]);
        }
    }
}

static void associate(void)
{
    sort_rules_if_needed();
    check_duplicate_rules();
    initialize_zones();
    assign_rules_to_zones();
    process_ruleless_zones();
    
    if (errors)
        exit(EXIT_FAILURE);
}

/* Read a text line from FP into BUF, which is of size BUFSIZE.
   Terminate it with a NUL byte instead of a newline.
   Return true if successful, false if EOF.
   On error, report the error and exit.  */
static void handle_input_error(void)
{
    error(_("input error"));
    exit(EXIT_FAILURE);
}

static void handle_unterminated_line(void)
{
    error(_("unterminated line"));
    exit(EXIT_FAILURE);
}

static void handle_nul_byte(void)
{
    error(_("NUL input byte"));
    exit(EXIT_FAILURE);
}

static void handle_line_too_long(void)
{
    error(_("line too long"));
    exit(EXIT_FAILURE);
}

static bool is_end_of_file(int ch, ptrdiff_t linelen)
{
    if (ch >= 0)
        return false;
    
    if (ferror(stdin))
        handle_input_error();
    
    if (linelen == 0)
        return true;
    
    handle_unterminated_line();
    return false;
}

static void validate_character(int ch)
{
    if (!ch)
        handle_nul_byte();
}

static void add_character_to_buffer(char *buf, ptrdiff_t *linelen, int ch, ptrdiff_t bufsize)
{
    buf[(*linelen)++] = ch;
    if (*linelen == bufsize)
        handle_line_too_long();
}

static bool
inputline(FILE *fp, char *buf, ptrdiff_t bufsize)
{
    ptrdiff_t linelen = 0;
    int ch;
    
    while ((ch = getc(fp)) != '\n') {
        if (is_end_of_file(ch, linelen))
            return false;
        
        validate_character(ch);
        add_character_to_buffer(buf, &linelen, ch, bufsize);
    }
    
    buf[linelen] = '\0';
    return true;
}

static FILE* open_input_file(const char *name)
{
	FILE *fp;
	
	if (strcmp(name, "-") == 0) {
		return stdin;
	}
	
	fp = fopen(name, "r");
	if (fp == NULL) {
		const char *e = strerror(errno);
		fprintf(stderr, _("%s: Can't open %s: %s\n"),
			progname, name, e);
		exit(EXIT_FAILURE);
	}
	
	return fp;
}

static struct lookup const* get_line_codes(int fnum)
{
	return fnum < 0 ? leap_line_codes : zi_line_codes;
}

static bool process_line_type(const struct lookup *lp, char *fields[], int nfields)
{
	bool wantcont = false;
	
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
	
	return wantcont;
}

static bool process_input_line(char *fields[], int nfields, bool wantcont, int fnum)
{
	if (nfields == 0) {
		return wantcont;
	}
	
	if (wantcont) {
		return inzcont(fields, nfields);
	}
	
	struct lookup const *line_codes = get_line_codes(fnum);
	const struct lookup *lp = byword(fields[0], line_codes);
	
	if (lp == NULL) {
		error(_("input line of unknown type"));
		return false;
	}
	
	return process_line_type(lp, fields, nfields);
}

#define BUFSIZE_BOUND (min(INT_MAX, INDEX_MAX) / FORMAT_LEN_GROWTH_BOUND)
#define BUF_SIZE min(_POSIX2_LINE_MAX, BUFSIZE_BOUND)

static void
infile(int fnum, char const *name)
{
	register FILE *fp;
	register bool wantcont;
	register lineno num;
	
	fp = open_input_file(name);
	wantcont = false;
	
	for (num = 1; ; ++num) {
		char buf[BUF_SIZE];
		int nfields;
		char *fields[MAX_FIELDS];
		
		eat(fnum, num);
		
		if (!inputline(fp, buf, sizeof buf))
			break;
		
		nfields = getfields(buf, fields,
				    sizeof fields / sizeof *fields);
		
		wantcont = process_input_line(fields, nfields, wantcont, fnum);
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

static bool is_empty_string(char const *string)
{
	return string == NULL || *string == '\0';
}

static int extract_sign(char const **string)
{
	if (**string == '-') {
		++(*string);
		return -1;
	}
	return 1;
}

static bool validate_time_components(zic_t hh, int mm, int ss, char const *errstring)
{
	if (hh < 0 || mm < 0 || mm >= MINSPERHOUR || ss < 0 || ss > SECSPERMIN) {
		error("%s", errstring);
		return false;
	}
	if (ZIC_MAX / SECSPERHOUR < hh) {
		error(_("time overflow"));
		return false;
	}
	return true;
}

static bool parse_time_string(char const *string, zic_t *hh, int *mm, int *ss, int *tenths, char *xr)
{
	char hhx, mmx, ssx, xs;
	bool ok = true;
	
	int count = sscanf(string,
		"%"SCNdZIC"%c%d%c%d%c%1d%*[0]%c%*[0123456789]%c",
		hh, &hhx, mm, &mmx, ss, &ssx, tenths, xr, &xs);
	
	switch (count) {
	case 8:
		ok = '0' <= *xr && *xr <= '9';
		ATTRIBUTE_FALLTHROUGH;
	case 7:
		ok &= ssx == '.';
		if (ok && noise)
			warning(_("fractional seconds rejected by pre-2018 versions of zic"));
		ATTRIBUTE_FALLTHROUGH;
	case 5:
		ok &= mmx == ':';
		ATTRIBUTE_FALLTHROUGH;
	case 3:
		ok &= hhx == ':';
		ATTRIBUTE_FALLTHROUGH;
	case 1:
		break;
	default:
		ok = false;
		break;
	}
	
	return ok;
}

static void warn_if_over_24_hours(zic_t hh, int mm, int ss)
{
	if (noise && (hh > HOURSPERDAY || (hh == HOURSPERDAY && (mm != 0 || ss != 0))))
		warning(_("values over 24 hours not handled by pre-2007 versions of zic"));
}

static int round_seconds_to_even(int ss, int tenths, char xr)
{
	return ss + (5 + ((ss ^ 1) & (xr == '0')) <= tenths);
}

static zic_t
gethms(char const *string, char const *errstring)
{
	zic_t hh;
	int mm = 0, ss = 0, tenths = 0;
	char xr = '0';
	
	if (is_empty_string(string))
		return 0;
	
	int sign = extract_sign(&string);
	
	if (!parse_time_string(string, &hh, &mm, &ss, &tenths, &xr)) {
		error("%s", errstring);
		return 0;
	}
	
	if (!validate_time_components(hh, mm, ss, errstring))
		return 0;
	
	ss = round_seconds_to_even(ss, tenths, xr);
	
	warn_if_over_24_hours(hh, mm, ss);
	
	return oadd(sign * hh * SECSPERHOUR, sign * (mm * SECSPERMIN + ss));
}

static zic_t
getsave(char *field, bool *isdst)
{
  int dst = -1;
  zic_t save;
  ptrdiff_t fieldlen = strlen(field);
  
  if (fieldlen != 0) {
    dst = extract_dst_suffix(field, fieldlen);
  }
  
  save = gethms(field, _("invalid saved time"));
  *isdst = determine_isdst(dst, save);
  return save;
}

static int
extract_dst_suffix(char *field, ptrdiff_t fieldlen)
{
  char *ep = field + fieldlen - 1;
  int dst = -1;
  
  if (*ep == 'd') {
    dst = 1;
    *ep = '\0';
  } else if (*ep == 's') {
    dst = 0;
    *ep = '\0';
  }
  
  return dst;
}

static bool
determine_isdst(int dst, zic_t save)
{
  return dst < 0 ? save != 0 : dst;
}

static int is_invalid_first_char(char c)
{
	if (c == '\0' || c == ' ' || c == '\f' || c == '\n' || 
	    c == '\r' || c == '\t' || c == '\v' || c == '+' || 
	    c == '-') {
		return 1;
	}
	if (c >= '0' && c <= '9') {
		return 1;
	}
	return 0;
}

static void update_max_abbrvar_len(const char *abbrvar)
{
	size_t len = strlen(abbrvar);
	if (max_abbrvar_len < len) {
		max_abbrvar_len = len;
	}
}

static int validate_rule_fields(char **fields, int nfields)
{
	if (nfields != RULE_FIELDS) {
		error(_("wrong number of fields on Rule line"));
		return 0;
	}
	
	if (is_invalid_first_char(*fields[RF_NAME])) {
		error(_("Invalid rule name \"%s\""), fields[RF_NAME]);
		return 0;
	}
	
	return 1;
}

static int populate_rule(struct rule *r, char **fields)
{
	r->r_filenum = filenum;
	r->r_linenum = linenum;
	r->r_save = getsave(fields[RF_SAVE], &r->r_isdst);
	
	if (!rulesub(r, fields[RF_LOYEAR], fields[RF_HIYEAR],
		     fields[RF_COMMAND], fields[RF_MONTH], fields[RF_DAY],
		     fields[RF_TOD])) {
		return 0;
	}
	
	r->r_name = estrdup(fields[RF_NAME]);
	r->r_abbrvar = estrdup(fields[RF_ABBRVAR]);
	update_max_abbrvar_len(r->r_abbrvar);
	
	return 1;
}

static void
inrule(char **fields, int nfields)
{
	struct rule r;
	
	if (!validate_rule_fields(fields, nfields)) {
		return;
	}
	
	if (!populate_rule(&r, fields)) {
		return;
	}
	
	rules = growalloc(rules, sizeof *rules, nrules, &nrules_alloc);
	rules[nrules++] = r;
}

static bool is_valid_field_count(int nfields)
{
	if (nfields < ZONE_MINFIELDS || nfields > ZONE_MAXFIELDS) {
		error(_("wrong number of fields on Zone line"));
		return false;
	}
	return true;
}

static bool check_lcltime_conflict(const char *zone_name)
{
	if (lcltime != NULL && strcmp(zone_name, tzdefault) == 0) {
		error(_("\"Zone %s\" line and -l option are mutually exclusive"),
			tzdefault);
		return false;
	}
	return true;
}

static bool check_psxrules_conflict(const char *zone_name)
{
	if (strcmp(zone_name, TZDEFRULES) == 0 && psxrules != NULL) {
		error(_("\"Zone %s\" line and -p option are mutually exclusive"),
			TZDEFRULES);
		return false;
	}
	return true;
}

static bool is_duplicate_zone(const char *zone_name)
{
	register ptrdiff_t i;
	
	for (i = 0; i < nzones; ++i) {
		if (zones[i].z_name == NULL)
			continue;
			
		if (strcmp(zones[i].z_name, zone_name) == 0) {
			error(_("duplicate zone name %s"
				" (file \"%s\", line %"PRIdMAX")"),
				zone_name,
				filename(zones[i].z_filenum),
				zones[i].z_linenum);
			return true;
		}
	}
	return false;
}

static bool
inzone(char **fields, int nfields)
{
	if (!is_valid_field_count(nfields))
		return false;
		
	if (!check_lcltime_conflict(fields[ZF_NAME]))
		return false;
		
	if (!check_psxrules_conflict(fields[ZF_NAME]))
		return false;
		
	if (is_duplicate_zone(fields[ZF_NAME]))
		return false;
		
	return inzsub(fields, nfields, false);
}

static bool
inzcont(char **fields, int nfields)
{
	if (nfields < ZONEC_MINFIELDS || nfields > ZONEC_MAXFIELDS) {
		error(_("wrong number of fields on Zone continuation line"));
		return false;
	}
	return inzsub(fields, nfields, true);
}

static int get_field_indices(bool iscont, int *i_stdoff, int *i_rule, int *i_format,
                             int *i_untilyear, int *i_untilmonth, int *i_untilday, int *i_untiltime)
{
    if (iscont) {
        *i_stdoff = ZFC_STDOFF;
        *i_rule = ZFC_RULE;
        *i_format = ZFC_FORMAT;
        *i_untilyear = ZFC_TILYEAR;
        *i_untilmonth = ZFC_TILMONTH;
        *i_untilday = ZFC_TILDAY;
        *i_untiltime = ZFC_TILTIME;
    } else {
        *i_stdoff = ZF_STDOFF;
        *i_rule = ZF_RULE;
        *i_format = ZF_FORMAT;
        *i_untilyear = ZF_TILYEAR;
        *i_untilmonth = ZF_TILMONTH;
        *i_untilday = ZF_TILDAY;
        *i_untiltime = ZF_TILTIME;
    }
    return 1;
}

static bool validate_format_string(const char *format_field, char *format_specifier)
{
    char *cp = strchr(format_field, '%');
    if (cp != NULL) {
        cp++;
        if ((*cp != 's' && *cp != 'z') || strchr(cp, '%') || strchr(format_field, '/')) {
            error(_("invalid abbreviation format"));
            return false;
        }
    }
    *format_specifier = cp ? *cp : '\0';
    return true;
}

static bool process_until_fields(struct zone *z, char **fields, int nfields, 
                                 int i_untilyear, int i_untilmonth, int i_untilday, int i_untiltime)
{
    z->z_untilrule.r_filenum = filenum;
    z->z_untilrule.r_linenum = linenum;
    
    const char *month = (nfields > i_untilmonth) ? fields[i_untilmonth] : "Jan";
    const char *day = (nfields > i_untilday) ? fields[i_untilday] : "1";
    const char *time = (nfields > i_untiltime) ? fields[i_untiltime] : "0";
    
    if (!rulesub(&z->z_untilrule, fields[i_untilyear], "only", "", month, day, time))
        return false;
    
    z->z_untiltime = rpytime(&z->z_untilrule, z->z_untilrule.r_loyear);
    return true;
}

static bool validate_continuation_time(struct zone *z, bool iscont)
{
    if (!iscont || nzones == 0)
        return true;
        
    if (z->z_untiltime <= min_time || z->z_untiltime >= max_time)
        return true;
        
    if (zones[nzones - 1].z_untiltime <= min_time || zones[nzones - 1].z_untiltime >= max_time)
        return true;
        
    if (zones[nzones - 1].z_untiltime >= z->z_untiltime) {
        error(_("Zone continuation line end time is not after end time of previous line"));
        return false;
    }
    return true;
}

static void update_max_format_len(const char *format_field)
{
    int format_len = strlen(format_field);
    if (max_format_len < format_len)
        max_format_len = format_len;
}

static void setup_zone_format(struct zone *z, const char *format_field)
{
    char *cp1 = estrdup(format_field);
    z->z_format = cp1;
    
    if (z->z_format_specifier == 'z') {
        char *cp = strchr(format_field, '%');
        if (cp != NULL) {
            cp1[cp - format_field] = 's';
            if (noise)
                warning(_("format '%s' not handled by pre-2015 versions of zic"), format_field);
        }
    }
}

static bool
inzsub(char **fields, int nfields, bool iscont)
{
    struct zone z;
    int i_stdoff, i_rule, i_format;
    int i_untilyear, i_untilmonth, i_untilday, i_untiltime;
    bool hasuntil;

    if (!iscont && !namecheck(fields[ZF_NAME]))
        return false;

    get_field_indices(iscont, &i_stdoff, &i_rule, &i_format,
                     &i_untilyear, &i_untilmonth, &i_untilday, &i_untiltime);

    z.z_filenum = filenum;
    z.z_linenum = linenum;
    z.z_stdoff = gethms(fields[i_stdoff], _("invalid UT offset"));

    if (!validate_format_string(fields[i_format], &z.z_format_specifier))
        return false;

    update_max_format_len(fields[i_format]);

    hasuntil = nfields > i_untilyear;
    if (hasuntil) {
        if (!process_until_fields(&z, fields, nfields, i_untilyear, i_untilmonth, i_untilday, i_untiltime))
            return false;
        if (!validate_continuation_time(&z, iscont))
            return false;
    }

    z.z_name = iscont ? NULL : estrdup(fields[ZF_NAME]);
    z.z_rule = estrdup(fields[i_rule]);
    setup_zone_format(&z, fields[i_format]);

    zones = growalloc(zones, sizeof *zones, nzones, &nzones_alloc);
    zones[nzones++] = z;

    return hasuntil;
}

static zic_t parse_year(const char *cp)
{
	zic_t year;
	char xs;
	
	if (sscanf(cp, "%"SCNdZIC"%c", &year, &xs) != 1) {
		error(_("invalid leaping year"));
		return -1;
	}
	return year;
}

static void update_leap_bounds(zic_t year, bool expire_line)
{
	if (expire_line)
		return;
		
	if (!leapseen || leapmaxyear < year)
		leapmaxyear = year;
	if (!leapseen || leapminyear > year)
		leapminyear = year;
	leapseen = true;
}

static zic_t calculate_year_offset(zic_t year)
{
	zic_t dayoff = 0;
	zic_t j = EPOCH_YEAR;
	zic_t i;
	
	while (j != year) {
		if (year > j) {
			i = len_years[isleap(j)];
			++j;
		} else {
			--j;
			i = -len_years[isleap(j)];
		}
		dayoff = oadd(dayoff, i);
	}
	return dayoff;
}

static int parse_month(const char *field)
{
	const struct lookup *lp = byword(field, mon_names);
	
	if (lp == NULL) {
		error(_("invalid month name"));
		return -1;
	}
	return lp->l_value;
}

static zic_t calculate_month_offset(zic_t year, int month)
{
	zic_t dayoff = 0;
	int j = TM_JANUARY;
	
	while (j != month) {
		dayoff = oadd(dayoff, len_months[isleap(year)][j]);
		++j;
	}
	return dayoff;
}

static int parse_day(const char *cp, zic_t year, int month)
{
	int day;
	char xs;
	
	if (sscanf(cp, "%d%c", &day, &xs) != 1 ||
	    day <= 0 || day > len_months[isleap(year)][month]) {
		error(_("invalid day of month"));
		return -1;
	}
	return day;
}

static bool validate_dayoff(zic_t dayoff)
{
	if (dayoff < min_time / SECSPERDAY) {
		error(_("time too small"));
		return false;
	}
	if (dayoff > max_time / SECSPERDAY) {
		error(_("time too large"));
		return false;
	}
	return true;
}

static zic_t
getleapdatetime(char **fields, bool expire_line)
{
	zic_t year = parse_year(fields[LP_YEAR]);
	if (year == -1)
		return -1;
	
	update_leap_bounds(year, expire_line);
	
	zic_t dayoff = calculate_year_offset(year);
	
	int month = parse_month(fields[LP_MONTH]);
	if (month == -1)
		return -1;
	
	dayoff = oadd(dayoff, calculate_month_offset(year, month));
	
	int day = parse_day(fields[LP_DAY], year, month);
	if (day == -1)
		return -1;
	
	dayoff = oadd(dayoff, day - 1);
	
	if (!validate_dayoff(dayoff))
		return -1;
	
	zic_t t = dayoff * SECSPERDAY;
	zic_t tod = gethms(fields[LP_TIME], _("invalid time of day"));
	t = tadd(t, tod);
	
	if (t < 0)
		error(_("leap second precedes Epoch"));
	
	return t;
}

static void
validate_leap_fields(int nfields)
{
    if (nfields != LEAP_FIELDS) {
        error(_("wrong number of fields on Leap line"));
    }
}

static int
parse_leap_correction(const char *field)
{
    if (!field[0]) {
        return -1;
    }
    if (strcmp(field, "+") == 0) {
        return 1;
    }
    error(_("invalid CORRECTION field on Leap line"));
    return 0;
}

static void
process_leap_data(char **fields)
{
    zic_t t = getleapdatetime(fields, false);
    if (t < 0) {
        return;
    }
    
    struct lookup const *lp = byword(fields[LP_ROLL], leap_types);
    if (!lp) {
        error(_("invalid Rolling/Stationary field on Leap line"));
        return;
    }
    
    int correction = parse_leap_correction(fields[LP_CORR]);
    if (correction) {
        leapadd(t, correction, lp->l_value);
    }
}

static void
inleap(char **fields, int nfields)
{
    validate_leap_fields(nfields);
    if (nfields == LEAP_FIELDS) {
        process_leap_data(fields);
    }
}

static void
inexpires(char **fields, int nfields)
{
  #define EXPIRES_FIELDS_COUNT EXPIRES_FIELDS
  
  if (nfields != EXPIRES_FIELDS_COUNT) {
    error(_("wrong number of fields on Expires line"));
    return;
  }
  
  if (0 <= leapexpires) {
    error(_("multiple Expires lines"));
    return;
  }
  
  leapexpires = getleapdatetime(fields, true);
}

static void
inlink(char **fields, int nfields)
{
	if (!validate_link_fields(fields, nfields))
		return;
	
	if (!namecheck(fields[LF_LINKNAME]))
		return;
	
	add_link_entry(fields);
}

static int
validate_link_fields(char **fields, int nfields)
{
	if (nfields != LINK_FIELDS) {
		error(_("wrong number of fields on Link line"));
		return 0;
	}
	if (*fields[LF_TARGET] == '\0') {
		error(_("blank TARGET field on Link line"));
		return 0;
	}
	return 1;
}

static void
add_link_entry(char **fields)
{
	struct link l;
	
	l.l_filenum = filenum;
	l.l_linenum = linenum;
	l.l_target = estrdup(fields[LF_TARGET]);
	l.l_linkname = estrdup(fields[LF_LINKNAME]);
	links = growalloc(links, sizeof *links, nlinks, &nlinks_alloc);
	links[nlinks++] = l;
}

static bool parse_month(struct rule *rp, const char *monthp)
{
	const struct lookup *lp = byword(monthp, mon_names);
	if (lp == NULL) {
		error(_("invalid month name"));
		return false;
	}
	rp->r_month = lp->l_value;
	return true;
}

static void parse_time_suffix(struct rule *rp, char *ep)
{
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

static void parse_time(struct rule *rp, const char *timep)
{
	rp->r_todisstd = false;
	rp->r_todisut = false;
	char *dp = estrdup(timep);
	
	if (*dp != '\0') {
		char *ep = dp + strlen(dp) - 1;
		parse_time_suffix(rp, ep);
	}
	
	rp->r_tod = gethms(dp, _("invalid time of day"));
	free(dp);
}

static bool parse_low_year(struct rule *rp, const char *loyearp)
{
	const struct lookup *lp = byword(loyearp, begin_years);
	char xs;
	
	if (lp) {
		if (lp->l_value == YR_MINIMUM) {
			warning(_("FROM year \"%s\" is obsolete; treated as %d"),
				loyearp, YEAR_32BIT_MIN - 1);
			rp->r_loyear = YEAR_32BIT_MIN - 1;
			return true;
		}
		unreachable();
	}
	
	if (sscanf(loyearp, "%"SCNdZIC"%c", &rp->r_loyear, &xs) != 1) {
		error(_("invalid starting year"));
		return false;
	}
	return true;
}

static bool parse_high_year(struct rule *rp, const char *hiyearp)
{
	const struct lookup *lp = byword(hiyearp, end_years);
	char xs;
	
	rp->r_hiwasnum = (lp == NULL);
	
	if (!rp->r_hiwasnum) {
		if (lp->l_value == YR_MAXIMUM) {
			rp->r_hiyear = ZIC_MAX;
			return true;
		}
		if (lp->l_value == YR_ONLY) {
			rp->r_hiyear = rp->r_loyear;
			return true;
		}
		unreachable();
	}
	
	if (sscanf(hiyearp, "%"SCNdZIC"%c", &rp->r_hiyear, &xs) != 1) {
		error(_("invalid ending year"));
		return false;
	}
	return true;
}

static bool validate_year_range(struct rule *rp)
{
	if (rp->r_loyear > rp->r_hiyear) {
		error(_("starting year greater than ending year"));
		return false;
	}
	return true;
}

static bool validate_year_type(const char *typep)
{
	if (*typep != '\0') {
		error(_("year type \"%s\" is unsupported; use \"-\" instead"), typep);
		return false;
	}
	return true;
}

static bool parse_last_day(struct rule *rp, const struct lookup *lp)
{
	rp->r_dycode = DC_DOWLEQ;
	rp->r_wday = lp->l_value;
	rp->r_dayofmonth = len_months[1][rp->r_month];
	return true;
}

static bool parse_relative_day(struct rule *rp, char *dp, char *ep)
{
	*ep++ = 0;
	if (*ep++ != '=') {
		error(_("invalid day of month"));
		return false;
	}
	
	const struct lookup *lp = byword(dp, wday_names);
	if (lp == NULL) {
		error(_("invalid weekday name"));
		return false;
	}
	rp->r_wday = lp->l_value;
	return true;
}

static bool parse_day_of_month(struct rule *rp, const char *ep)
{
	char xs;
	if (sscanf(ep, "%d%c", &rp->r_dayofmonth, &xs) != 1 ||
		rp->r_dayofmonth <= 0 ||
		rp->r_dayofmonth > len_months[1][rp->r_month]) {
		error(_("invalid day of month"));
		return false;
	}
	return true;
}

static bool parse_day(struct rule *rp, const char *dayp)
{
	char *dp = estrdup(dayp);
	const struct lookup *lp = byword(dp, lasts);
	
	if (lp != NULL) {
		bool result = parse_last_day(rp, lp);
		free(dp);
		return result;
	}
	
	char *ep = strchr(dp, '<');
	if (ep != NULL) {
		rp->r_dycode = DC_DOWLEQ;
	} else {
		ep = strchr(dp, '>');
		if (ep != NULL) {
			rp->r_dycode = DC_DOWGEQ;
		} else {
			ep = dp;
			rp->r_dycode = DC_DOM;
		}
	}
	
	if (rp->r_dycode != DC_DOM) {
		if (!parse_relative_day(rp, dp, ep)) {
			free(dp);
			return false;
		}
	}
	
	bool result = parse_day_of_month(rp, ep);
	free(dp);
	return result;
}

static bool
rulesub(struct rule *rp, const char *loyearp, const char *hiyearp,
	const char *typep, const char *monthp, const char *dayp,
	const char *timep)
{
	if (!parse_month(rp, monthp))
		return false;
	
	parse_time(rp, timep);
	
	if (!parse_low_year(rp, loyearp))
		return false;
	
	if (!parse_high_year(rp, hiyearp))
		return false;
	
	if (!validate_year_range(rp))
		return false;
	
	if (!validate_year_type(typep))
		return false;
	
	if (!parse_day(rp, dayp))
		return false;
	
	return true;
}

static void
convert(uint_fast32_t val, char *buf)
{
	const int BYTE_COUNT = 4;
	const int BITS_PER_BYTE = 8;
	const int INITIAL_SHIFT = 24;
	const unsigned char BYTE_MASK = 0xff;
	
	unsigned char *const b = (unsigned char *) buf;

	for (int i = 0; i < BYTE_COUNT; ++i)
	{
		int shift = INITIAL_SHIFT - (i * BITS_PER_BYTE);
		b[i] = (val >> shift) & BYTE_MASK;
	}
}

static void convert64(uint_fast64_t val, char *buf)
{
    const int BYTE_COUNT = 8;
    const int BITS_PER_BYTE = 8;
    const int INITIAL_SHIFT = 56;
    const unsigned char BYTE_MASK = 0xff;
    
    unsigned char *const b = (unsigned char *) buf;
    
    for (int i = 0; i < BYTE_COUNT; ++i) {
        int shift = INITIAL_SHIFT - (i * BITS_PER_BYTE);
        b[i] = (val >> shift) & BYTE_MASK;
    }
}

static void
puttzcode(zic_t val, FILE *fp)
{
	char	buf[4];

	convert(val, buf);
	fwrite(buf, sizeof buf, 1, fp);
}

static void
puttzcodepass(zic_t val, FILE *fp, int pass)
{
  if (pass == 1) {
    puttzcode(val, fp);
    return;
  }
  
  char buf[8];
  convert64(val, buf);
  fwrite(buf, sizeof buf, 1, fp);
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

static void omit_transitions_before_lo(struct timerange *r, zic_t lo,
                                       zic_t const *ats, unsigned char const *types)
{
    while (0 < r->count && ats[r->base] < lo) {
        r->defaulttype = types[r->base];
        r->count--;
        r->base++;
    }
}

static void omit_initial_leap_seconds(struct timerange *r, zic_t lo)
{
    while (1 < r->leapcount && trans[r->leapbase + 1] <= lo) {
        r->leapcount--;
        r->leapbase++;
    }
}

static void adjust_leap_second_base(struct timerange *r)
{
    while (0 < r->leapbase) {
        int prev_corr_less = corr[r->leapbase - 1] < corr[r->leapbase];
        int current_corr_positive = 0 < corr[r->leapbase];
        
        if (prev_corr_less == current_corr_positive) {
            break;
        }
        
        r->leapcount++;
        r->leapbase--;
    }
}

static void omit_transitions_after_hi(struct timerange *r, zic_t hi,
                                      zic_t const *ats)
{
    if (hi >= max_time) {
        return;
    }
    
    zic_t hi_plus_one = hi + 1;
    
    while (0 < r->count && hi_plus_one < ats[r->base + r->count - 1]) {
        r->count--;
    }
    
    while (0 < r->leapcount && hi_plus_one < trans[r->leapbase + r->leapcount - 1]) {
        r->leapcount--;
    }
}

static struct timerange
limitrange(struct timerange r, zic_t lo, zic_t hi,
          zic_t const *ats, unsigned char const *types)
{
    omit_transitions_before_lo(&r, lo, ats, types);
    omit_initial_leap_seconds(&r, lo);
    adjust_leap_second_base(&r);
    omit_transitions_after_hi(&r, hi, ats);
    
    r.leapexpiry = 0 <= leapexpires && leapexpires - 1 <= hi;
    
    return r;
}

static void sort_transitions(void) {
    if (timecnt > 1)
        qsort(attypes, timecnt, sizeof *attypes, atcomp);
}

static int should_merge_transitions(ptrdiff_t toi, ptrdiff_t fromi) {
    if (toi == 0 || attypes[fromi].dontmerge)
        return 0;
    
    if (utoffs[attypes[toi - 1].type] != utoffs[attypes[fromi].type])
        return 0;
    
    if (isdsts[attypes[toi - 1].type] != isdsts[attypes[fromi].type])
        return 0;
    
    if (desigidx[attypes[toi - 1].type] != desigidx[attypes[fromi].type])
        return 0;
    
    return 1;
}

static int can_merge_adjacent_transitions(ptrdiff_t toi, ptrdiff_t fromi) {
    if (toi == 0)
        return 0;
    
    zic_t curr_trans = attypes[fromi].at + utoffs[attypes[toi - 1].type];
    zic_t prev_trans = attypes[toi - 1].at + utoffs[toi == 1 ? 0 : attypes[toi - 2].type];
    
    return curr_trans <= prev_trans;
}

static void optimize_transitions(void) {
    ptrdiff_t fromi, toi = 0;
    
    for (fromi = 0; fromi < timecnt; ++fromi) {
        if (toi != 0 && can_merge_adjacent_transitions(toi, fromi)) {
            attypes[toi - 1].type = attypes[fromi].type;
            continue;
        }
        
        if (!should_merge_transitions(toi, fromi)) {
            attypes[toi++] = attypes[fromi];
        }
    }
    timecnt = toi;
}

static void check_transition_count(const char *const name) {
    if (!noise || timecnt <= 1200)
        return;
    
    if (timecnt > TZ_MAX_TIMES) {
        warning(_("reference clients mishandle more than %d transition times"), TZ_MAX_TIMES);
    } else {
        warning(_("pre-2014 clients may mishandle more than 1200 transition times"));
    }
}

static void transfer_transitions(zic_t *ats, unsigned char *types) {
    register ptrdiff_t i;
    for (i = 0; i < timecnt; ++i) {
        ats[i] = attypes[i].at;
        types[i] = attypes[i].type;
    }
}

static void correct_for_leap_seconds(zic_t *ats) {
    register ptrdiff_t i, j;
    
    for (i = 0; i < timecnt; ++i) {
        j = leapcnt;
        while (--j >= 0) {
            if (ats[i] > trans[j] - corr[j]) {
                ats[i] = tadd(ats[i], corr[j]);
                break;
            }
        }
    }
}

static void check_leap_second_version(struct timerange const *r, const char *const name, char *version) {
    if (r->leapexpiry) {
        if (noise)
            warning(_("%s: pre-2021b clients may mishandle leap second expiry"), name);
        *version = '4';
    }
    
    if (0 < r->leapcount && corr[r->leapbase] != 1 && corr[r->leapbase] != -1) {
        if (noise)
            warning(_("%s: pre-2021b clients may mishandle leap second table truncation"), name);
        *version = '4';
    }
}

static void determine_version(struct timerange const *range32, struct timerange const *range64, 
                              const char *const name, char *version) {
    for (int pass = 1; pass <= 2; pass++) {
        struct timerange const *r = pass == 1 ? range32 : range64;
        if (pass == 1 && !want_bloat())
            continue;
        
        check_leap_second_version(r, name, version);
        if (*version == '4')
            break;
    }
}

static void setup_pass_parameters(int pass, struct timerange const *range32, struct timerange const *range64,
                                  int *thisdefaulttype, ptrdiff_t *thistimei, ptrdiff_t *thistimecnt,
                                  int *thisleapi, int *thisleapcnt, bool *thisleapexpiry,
                                  zic_t *thismin, zic_t *thismax, bool *toomanytimes) {
    if (pass == 1) {
        *thisdefaulttype = range32->defaulttype;
        *thistimei = range32->base;
        *thistimecnt = range32->count;
        *toomanytimes = (*thistimecnt >> 31 >> 1) != 0;
        *thisleapi = range32->leapbase;
        *thisleapcnt = range32->leapcount;
        *thisleapexpiry = range32->leapexpiry;
        *thismin = ZIC32_MIN;
        *thismax = ZIC32_MAX;
    } else {
        *thisdefaulttype = range64->defaulttype;
        *thistimei = range64->base;
        *thistimecnt = range64->count;
        *toomanytimes = (*thistimecnt >> 31 >> 31 >> 2) != 0;
        *thisleapi = range64->leapbase;
        *thisleapcnt = range64->leapcount;
        *thisleapexpiry = range64->leapexpiry;
        *thismin = min_time;
        *thismax = max_time;
    }
}

static void determine_pretranstype(bool locut, int pass, ptrdiff_t thistimei, ptrdiff_t thistimecnt,
                                   zic_t *ats, int thisdefaulttype, int *pretranstype, char *omittype) {
    if ((locut || (pass == 1 && thistimei)) && !(thistimecnt && ats[thistimei] == lo_time)) {
        *pretranstype = thisdefaulttype;
        omittype[*pretranstype] = false;
    }
}

static void mark_used_types(ptrdiff_t thistimei, ptrdiff_t thistimelim, unsigned char *types,
                            int thisdefaulttype, bool locut, bool hicut, char *omittype) {
    register ptrdiff_t i;
    
    memset(omittype, true, typecnt);
    
    if (locut)
        thisdefaulttype = unspecifiedtype;
    
    omittype[thisdefaulttype] = false;
    
    for (i = thistimei; i < thistimelim; i++)
        omittype[types[i]] = false;
    
    if (hicut)
        omittype[unspecifiedtype] = false;
}

static void add_bloat_type(int mrudst, int mrustd, int hidst, int histd, char *omittype) {
    int type;
    
    if (hidst >= 0 && mrudst >= 0 && hidst != mrudst && utoffs[hidst] != utoffs[mrudst]) {
        isdsts[mrudst] = -1;
        type = addtype(utoffs[mrudst], &chars[desigidx[mrudst]], true,
                      ttisstds[mrudst], ttisuts[mrudst]);
        isdsts[mrudst] = 1;
        omittype[type] = false;
    }
    
    if (histd >= 0 && mrustd >= 0 && histd != mrustd && utoffs[histd] != utoffs[mrustd]) {
        isdsts[mrustd] = -1;
        type = addtype(utoffs[mrustd], &chars[desigidx[mrustd]], false,
                      ttisstds[mrustd], ttisuts[mrustd]);
        isdsts[mrustd] = 0;
        omittype[type] = false;
    }
}

static void handle_bloat_types(int pretranstype, ptrdiff_t thistimei, ptrdiff_t thistimelim,
                               unsigned char *types, int old0, int thisdefaulttype, char *omittype) {
#ifndef LEAVE_SOME_PRE_2011_SYSTEMS_IN_THE_LURCH
    if (!want_bloat())
        return;
    
    register ptrdiff_t i;
    int mrudst = -1, mrustd = -1, hidst = -1, histd = -1;
    
    if (0 <= pretranstype) {
        if (isdsts[pretranstype])
            mrudst = pretranstype;
        else
            mrustd = pretranstype;
    }
    
    for (i = thistimei; i < thistimelim; i++) {
        if (isdsts[types[i]])
            mrudst = types[i];
        else
            mrustd = types[i];
    }
    
    for (i = old0; i < typecnt; i++) {
        int h = (i == old0 ? thisdefaulttype : i == thisdefaulttype ? old0 : i);
        if (!omittype[h]) {
            if (isdsts[h])
                hidst = i;
            else
                histd = i;
        }
    }
    
    add_bloat_type(mrudst, mrustd, hidst, histd, omittype);
#endif
}

static int build_typemap(int old0, int thisdefaulttype, char *omittype, int *typemap) {
    int thistypecnt = 0;
    register ptrdiff_t i;
    
    for (i = old0; i < typecnt; i++) {
        if (!omittype[i]) {
            int idx = (i == old0 ? thisdefaulttype : i == thisdefaulttype ? old0 : i);
            typemap[idx] = thistypecnt++;
        }
    }
    
    return thistypecnt;
}

static int build_char_data(int old0, char *omittype, int *indmap, char *thischars,
                          int *stdcnt, int *utcnt, int thistypecnt) {
    register ptrdiff_t i, j;
    int thischarcnt = 0;
    
    for (i = 0; i < TZ_MAX_CHARS; ++i)
        indmap[i] = -1;
    
    *stdcnt = *utcnt = 0;
    
    for (i = old0; i < typecnt; i++) {
        if (omittype[i])
            continue;
        
        if (ttisstds[i])
            *stdcnt = thistypecnt;
        if (ttisuts[i])
            *utcnt = thistypecnt;
        
        if (indmap[desigidx[i]] >= 0)
            continue;
        
        char *thisabbr = &chars[desigidx[i]];
        for (j = 0; j < thischarcnt; ++j) {
            if (strcmp(&thischars[j], thisabbr) == 0)
                break;
        }
        
        if (j == thischarcnt) {
            strcpy(&thischars[thischarcnt], thisabbr);
            thischarcnt += strlen(thisabbr) + 1;
        }
        indmap[desigidx[i]] = j;
    }
    
    return thischarcnt;
}

static void write_header(FILE *fp, char version, int utcnt, int stdcnt, int thisleapcnt,
                        bool thisleapexpiry, int pretranstype, ptrdiff_t thistimecnt,
                        bool hicut, int thistypecnt, int thischarcnt) {
    struct tzhead tzh;
    
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
    
    DO(tzh_magic);
    DO(tzh_version);
    DO(tzh_reserved);
    DO(tzh_ttisutcnt);
    DO(tzh_ttisstdcnt);
    DO(tzh_leapcnt);
    DO(tzh_timecnt);
    DO(tzh_typecnt);
    DO(tzh_charcnt);
#undef DO
}

static void write_minimal_data(FILE *fp) {
    puttzcode(0, fp);
    putc(0, fp);
    putc(0, fp);
    putc(0, fp);
}

static void write_transitions(FILE *fp, int pass, int pretranstype, ptrdiff_t thistimei,
                              ptrdiff_t thistimelim, zic_t *ats, unsigned char *types,
                              bool hicut, int *typemap) {
    register ptrdiff_t i;
    zic_t lo = pass == 1 && lo_time < ZIC32_MIN ? ZIC32_MIN : lo_time;
    
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
}

static void write_type_data(FILE *fp, int old0, int thisdefaulttype, char *omittype, int *indmap) {
    register ptrdiff_t i;
    
    for (i = old0; i < typecnt; i++) {
        int h = (i == old0 ? thisdefaulttype : i == thisdefaulttype ? old0 : i);
        if (!omittype[h]) {
            puttzcode(utoffs[h], fp);
            putc(isdsts[h], fp);
            putc(indmap[desigidx[h]], fp);
        }
    }
}

static void write_leap_data(FILE *fp, int pass, int thisleapi, int thisleapcnt,
                           bool thisleapexpiry, zic_t *ats, unsigned char *types) {
    register ptrdiff_t i, j;
    int thisleaplim = thisleapi + thisleapcnt;
    
    for (i = thisleapi; i < thisleaplim; ++i) {
        zic_t todo;
        
        if (roll[i]) {
            if (timecnt == 0 || trans[i] < ats[0]) {
                j = 0;
                while (isdsts[j]) {
                    if (++j >= typecnt) {
                        j = 0;
                        break;
                    }
                }
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
}

static void write_std_utc_indicators(FILE *fp, int old0, char *omittype, int stdcnt, int utcnt) {
    register ptrdiff_t i;
    
    if (stdcnt != 0) {
        for (i = old0; i < typecnt; i++) {
            if (!omittype[i])
                putc(ttisstds[i], fp);
        }
    }
    
    if (utcnt != 0) {
        for (i = old0; i < typecnt; i++) {
            if (!omittype[i])
                putc(ttisuts[i], fp);
        }
    }
}

static void write_pass_data(FILE *fp, int pass, struct timerange const *range32,
                           struct timerange const *range64, zic_t *ats, unsigned char *types,
                           char version) {
    ptrdiff_t thistimei, thistimecnt, thistimelim;
    int thisleapi, thisleapcnt, thisdefaulttype;
    int pretranstype = -1;
    bool locut, hicut, thisleapexpiry, toomanytimes;
    zic_t thismin, thismax;
    int old0;
    char omittype[TZ_MAX_TYPES];
    int typemap[TZ_MAX_TYPES];
    int thistypecnt, stdcnt, utcnt, thischarcnt;
    char thischars[TZ_MAX_CHARS];
    int indmap[TZ_MAX_CHARS];
    
    setup_pass_parameters(pass, range32, range64, &thisdefaulttype, &thistimei, &thistimecnt,
                         &thisleapi, &thisleapcnt, &thisleapexpiry, &thismin, &thismax, &toomanytimes);
    
    if (toomanytimes)
        error(_("too many transition times"));
    
    locut = thismin < lo_time && lo_time <= thismax;
    hicut = thismin <= hi_time && hi_time < thismax;
    thistimelim = thistimei + thistimecnt;
    
    mark_used_types(thistimei, thistimelim, types, thisdefaulttype, locut, hicut, omittype);
    
    determine_pretranstype(locut, pass, thistimei, thistimecnt, ats, thisdefaulttype,
                          &pretranstype, omittype);
    
    if (pass == 1 && lo_time <= thismin)
        thisdefaulttype = range64->defaulttype;
    
    if (locut)
        thisdefaulttype = unspecifiedtype;
    
    omittype[thisdefaulttype] = false;
    
    old0 = strlen(omittype);
    
    handle_bloat_types(pretranstype, thistimei, thistimelim, types, old0, thisdefaulttype, omittype);
    
    thistypecnt = build_typemap(old0, thisdefaulttype, omittype, typemap);
    thischarcnt = build_char_data(old0, omittype, indmap, thischars, &stdcnt, &utcnt, thistypecnt);
    
    if (pass == 1 && !want_bloat()) {
        hicut = thisleapexpiry = false;
        pretranstype = -1;
        thistimecnt = thisleapcnt = 0;
        thistypecnt = thischarcnt = 1;
    }
    
    write_header(fp, version, utcnt, stdcnt, thisleapcnt, thisleapexpiry, pretranstype,
                thistimecnt, hicut, thistypecnt, thischarcnt);
    
    if (pass == 1 && !want_bloat()) {
        write_minimal_data(fp);
        return;
    }
    
    write_transitions(fp, pass, pretranstype, thistimei, thistimelim, ats, types, hicut, typemap);
    write_type_data(fp, old0, thisdefaulttype, omittype, indmap);
    
    if (thischarcnt != 0)
        fwrite(thischars, sizeof thischars[0], thischarcnt, fp);
    
    write_leap_data(fp, pass, thisleapi, thisleapcnt, thisleapexpiry, ats, types);
    write_std_utc_indicators(fp, old0, omittype, stdcnt, utcnt);
}

static void writezone(const char *const name, const char *const string, char version, int defaulttype) {
    register FILE *fp;
    register int pass;
    char *tempname = NULL;
    char const *outname = name;
    struct timerange rangeall = {0}, range32, range64;
    
    zic_t *ats = emalloc(align_to(size_product(timecnt + !timecnt, sizeof *ats + 1), alignof(zic_t)));
    void *typesptr = ats + timecnt;
    unsigned char *types = typesptr;
    
    sort_transitions();
    optimize_transitions();
    check_transition_count(name);
    transfer_transitions(ats, types);
    correct_for_leap_seconds(ats);
    
    rangeall.defaulttype = defaulttype;
    rangeall.count = timecnt;
    rangeall.leapcount = leapcnt;
    range64 = limitrange(rangeall, lo_time, max(hi_time, redundant_time - (ZIC_MIN < redundant_time)), ats, types);
    range32 = limitrange(range64, ZIC32_MIN, ZIC32_MAX, ats, types);
    
    determine_version(&range32, &range64, name, &version);
    
    fp = open_outfile(&outname, &tempname);
    
    for (pass = 1; pass <= 2; ++pass) {
        write_pass_data(fp, pass, &range32, &range64, ats, types, version);
    }
    
    fprintf(fp, "\n%s\n", string);
    close_file(fp, directory, name, tempname);
    rename_dest(tempname, name);
    free(ats);
}

static char const *
abbroffset(char *buf, zic_t offset)
{
  char sign = '+';
  int seconds, minutes, hours;

  if (offset < 0) {
    offset = -offset;
    sign = '-';
  }

  seconds = offset % SECSPERMIN;
  offset /= SECSPERMIN;
  minutes = offset % MINSPERHOUR;
  hours = offset / MINSPERHOUR;
  
  if (100 <= hours) {
    error(_("%%z UT offset magnitude exceeds 99:59:59"));
    return "%z";
  }
  
  format_offset(buf, sign, hours, minutes, seconds);
  return buf;
}

static void format_offset(char *buf, char sign, int hours, int minutes, int seconds)
{
  char *p = buf;
  *p++ = sign;
  p = append_two_digits(p, hours);
  
  if (minutes | seconds) {
    p = append_two_digits(p, minutes);
    if (seconds) {
      p = append_two_digits(p, seconds);
    }
  }
  *p = '\0';
}

static char* append_two_digits(char *p, int value)
{
  *p++ = '0' + value / 10;
  *p++ = '0' + value % 10;
  return p;
}

static char const disable_percent_s[] = "";

static ptrdiff_t
handle_no_slash_format(char *abbr, struct zone const *zp, char const *letters, zic_t save)
{
	char letterbuf[PERCENT_Z_LEN_BOUND + 1];
	char const *format = zp->z_format;
	
	if (zp->z_format_specifier == 'z') {
		letters = abbroffset(letterbuf, zp->z_stdoff + save);
	} else if (!letters) {
		letters = "%s";
	} else if (letters == disable_percent_s) {
		return 0;
	}
	
	sprintf(abbr, format, letters);
	return strlen(abbr);
}

static ptrdiff_t
handle_slash_format(char *abbr, struct zone const *zp, bool isdst)
{
	char const *format = zp->z_format;
	char *slashp = strchr(format, '/');
	
	if (isdst) {
		strcpy(abbr, slashp + 1);
	} else {
		ptrdiff_t prefix_len = slashp - format;
		memcpy(abbr, format, prefix_len);
		abbr[prefix_len] = '\0';
	}
	
	return strlen(abbr);
}

static bool
needs_quotes(char const *abbr, ptrdiff_t len)
{
	char const *cp;
	
	if (len == 0) {
		return false;
	}
	
	for (cp = abbr; is_alpha(*cp); cp++) {
		continue;
	}
	
	return *cp != '\0';
}

static ptrdiff_t
add_quotes(char *abbr, ptrdiff_t len)
{
	abbr[len + 2] = '\0';
	abbr[len + 1] = '>';
	memmove(abbr + 1, abbr, len);
	abbr[0] = '<';
	return len + 2;
}

static ptrdiff_t
doabbr(char *abbr, struct zone const *zp, char const *letters,
       bool isdst, zic_t save, bool doquotes)
{
	ptrdiff_t len;
	char const *format = zp->z_format;
	char *slashp = strchr(format, '/');
	
	if (slashp == NULL) {
		len = handle_no_slash_format(abbr, zp, letters, save);
		if (len == 0) {
			return 0;
		}
	} else {
		len = handle_slash_format(abbr, zp, isdst);
	}
	
	if (!doquotes) {
		return len;
	}
	
	if (needs_quotes(abbr, len)) {
		return add_quotes(abbr, len);
	}
	
	return len;
}

static void
updateminmax(const zic_t x)
{
	if (min_year > x)
		min_year = x;
	if (max_year < x)
		max_year = x;
}

static int format_offset_sign(char *result, zic_t *offset)
{
    if (*offset < 0) {
        *offset = -(*offset);
        result[0] = '-';
        return 1;
    }
    return 0;
}

static void extract_time_components(zic_t offset, int *hours, int *minutes, int *seconds)
{
    *seconds = offset % SECSPERMIN;
    offset /= SECSPERMIN;
    *minutes = offset % MINSPERHOUR;
    offset /= MINSPERHOUR;
    *hours = offset;
}

static int validate_hours(int hours)
{
    const int MAX_HOURS = HOURSPERDAY * DAYSPERWEEK;
    return hours < MAX_HOURS;
}

static int format_time_components(char *result, int len, int hours, int minutes, int seconds)
{
    len += sprintf(result + len, "%d", hours);
    
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
stringoffset(char *result, zic_t offset)
{
    int hours;
    int minutes;
    int seconds;
    int len;
    
    len = format_offset_sign(result, &offset);
    extract_time_components(offset, &hours, &minutes, &seconds);
    
    if (!validate_hours(hours)) {
        result[0] = '\0';
        return 0;
    }
    
    return format_time_components(result, len, hours, minutes, seconds);
}

static int calculate_total_days(int month)
{
    int total = 0;
    for (int i = 0; i < month; ++i)
        total += len_months[0][i];
    return total;
}

static int format_dom_rule(char *result, int month, int dayofmonth)
{
    int total = calculate_total_days(month);
    
    if (month <= 1)
        return sprintf(result, "%d", total + dayofmonth - 1);
    else
        return sprintf(result, "J%d", total + dayofmonth);
}

static int calculate_dowgeq_week(int dayofmonth, int *wday, zic_t *tod)
{
    int wdayoff = (dayofmonth - 1) % DAYSPERWEEK;
    int compat = 0;
    
    if (wdayoff) {
        compat = 2013;
        *wday -= wdayoff;
        *tod += wdayoff * SECSPERDAY;
    }
    
    return compat;
}

static int calculate_dowleq_week(int dayofmonth, int month, int *wday, zic_t *tod, int *week)
{
    if (dayofmonth == len_months[1][month]) {
        *week = 5;
        return 0;
    }
    
    int wdayoff = dayofmonth % DAYSPERWEEK;
    int compat = 0;
    
    if (wdayoff) {
        compat = 2013;
        *wday -= wdayoff;
        *tod += wdayoff * SECSPERDAY;
    }
    
    *week = dayofmonth / DAYSPERWEEK;
    return compat;
}

static int handle_dow_rule(char *result, struct rule *const rp, zic_t *tod)
{
    int week;
    int wday = rp->r_wday;
    int compat = 0;
    
    if (rp->r_dycode == DC_DOWGEQ) {
        compat = calculate_dowgeq_week(rp->r_dayofmonth, &wday, tod);
        week = 1 + (rp->r_dayofmonth - 1) / DAYSPERWEEK;
    } else if (rp->r_dycode == DC_DOWLEQ) {
        compat = calculate_dowleq_week(rp->r_dayofmonth, rp->r_month, &wday, tod, &week);
    } else {
        return -1;
    }
    
    if (wday < 0)
        wday += DAYSPERWEEK;
        
    sprintf(result, "M%d.%d.%d", rp->r_month + 1, week, wday);
    return compat;
}

static void adjust_tod(zic_t *tod, struct rule *const rp, zic_t save, zic_t stdoff)
{
    if (rp->r_todisut)
        *tod += stdoff;
    if (rp->r_todisstd && !rp->r_isdst)
        *tod += save;
}

static int append_time_offset(char **result, zic_t tod, int compat)
{
    const zic_t STANDARD_TOD = 2 * SECSPERMIN * MINSPERHOUR;
    
    if (tod == STANDARD_TOD)
        return compat;
        
    *(*result)++ = '/';
    if (!stringoffset(*result, tod))
        return -1;
        
    if (tod < 0) {
        if (compat < 2013)
            compat = 2013;
    } else if (SECSPERDAY <= tod) {
        if (compat < 1994)
            compat = 1994;
    }
    
    return compat;
}

static int
stringrule(char *result, struct rule *const rp, zic_t save, zic_t stdoff)
{
    zic_t tod = rp->r_tod;
    int compat = 0;
    
    if (rp->r_dycode == DC_DOM) {
        if (rp->r_dayofmonth == 29 && rp->r_month == TM_FEBRUARY)
            return -1;
        result += format_dom_rule(result, rp->r_month, rp->r_dayofmonth);
    } else {
        int bytes_written = 0;
        compat = handle_dow_rule(result + bytes_written, rp, &tod);
        if (compat == -1)
            return -1;
        result += strlen(result);
    }
    
    adjust_tod(&tod, rp, save, stdoff);
    compat = append_time_offset(&result, tod, compat);
    
    return compat;
}

static int
rule_cmp(struct rule const *a, struct rule const *b)
{
	if (!a)
		return -!!b;
	if (!b)
		return 1;
	
	if (a->r_hiyear != b->r_hiyear)
		return a->r_hiyear < b->r_hiyear ? -1 : 1;
	
	if (a->r_hiyear == ZIC_MAX)
		return 0;
	
	if (a->r_month != b->r_month)
		return a->r_month - b->r_month;
	
	return a->r_dayofmonth - b->r_dayofmonth;
}

/* Store into RESULT a proleptic TZ string that represent the future
   predictions for the zone ZPFIRST with ZONECOUNT entries.  Return a
   compatibility indicator (a TZDB release year) if successful, a
   negative integer if no such TZ string exists.  */
static int
find_last_rules(struct zone const *zp, struct rule **lastrp)
{
	ptrdiff_t i;
	for (i = 0; i < zp->z_nrules; ++i) {
		struct rule *rp = &zp->z_rules[i];
		struct rule **last = &lastrp[rp->r_isdst];
		int cmp = rule_cmp(*last, rp);
		if (cmp < 0)
			*last = rp;
		else if (cmp == 0)
			return -1;
	}
	return 0;
}

static void
setup_all_year_dst_rules(struct rule *dstr, struct rule *stdr, 
                         zic_t save, struct rule *dstrp, struct rule *stdrp)
{
	dstr->r_month = TM_JANUARY;
	dstr->r_dycode = DC_DOM;
	dstr->r_dayofmonth = 1;
	dstr->r_tod = 0;
	dstr->r_todisstd = false;
	dstr->r_todisut = false;
	dstr->r_isdst = true;
	dstr->r_save = save < 0 ? save : -save;
	dstr->r_abbrvar = dstrp ? dstrp->r_abbrvar : NULL;
	
	stdr->r_month = TM_DECEMBER;
	stdr->r_dycode = DC_DOM;
	stdr->r_dayofmonth = 31;
	stdr->r_tod = SECSPERDAY + dstr->r_save;
	stdr->r_todisstd = false;
	stdr->r_todisut = false;
	stdr->r_isdst = false;
	stdr->r_save = 0;
	stdr->r_abbrvar = save < 0 && stdrp ? stdrp->r_abbrvar : NULL;
}

static void
setup_fake_zones(struct zone *zstr, struct zone const *zp, zic_t save)
{
	zstr[0].z_stdoff = zp->z_stdoff + 2 * save;
	zstr[0].z_format = "XXX";
	zstr[0].z_format_specifier = 0;
	zstr[1].z_stdoff = zstr[0].z_stdoff;
	zstr[1].z_format = zp->z_format;
	zstr[1].z_format_specifier = zp->z_format_specifier;
}

static int
append_standard_time(char *result, struct zone const *stdzp, struct rule *stdrp)
{
	ptrdiff_t len = doabbr(result, stdzp, stdrp ? stdrp->r_abbrvar : NULL,
	                       false, 0, true);
	int offsetlen = stringoffset(result + len, - stdzp->z_stdoff);
	if (!offsetlen)
		return -1;
	return len + offsetlen;
}

static int
append_dst_time(char *result, ptrdiff_t len, struct zone const *dstzp, 
               struct rule *dstrp)
{
	len += doabbr(result + len, dstzp, dstrp->r_abbrvar,
	             dstrp->r_isdst, dstrp->r_save, true);
	
	if (dstrp->r_save != SECSPERMIN * MINSPERHOUR) {
		int offsetlen = stringoffset(result + len,
		                            - (dstzp->z_stdoff + dstrp->r_save));
		if (!offsetlen)
			return -1;
		len += offsetlen;
	}
	return len;
}

static int
append_rule(char *result, ptrdiff_t *len, struct rule *rp, 
           zic_t save, zic_t stdoff, int *compat)
{
	result[(*len)++] = ',';
	int c = stringrule(result + *len, rp, save, stdoff);
	if (c < 0)
		return -1;
	if (*compat < c)
		*compat = c;
	*len += strlen(result + *len);
	return 0;
}

static int
stringzone(char *result, struct zone const *zpfirst, ptrdiff_t zonecount)
{
	struct rule *stdrp;
	struct rule *dstrp;
	int compat = 0;
	struct rule stdr, dstr;
	ptrdiff_t len;
	int dstcmp;
	struct rule *lastrp[2] = { NULL, NULL };
	struct zone zstr[2];
	struct zone const *stdzp;
	struct zone const *dstzp;
	struct zone const *zp;

	result[0] = '\0';

	if (hi_time < max_time)
		return -1;

	zp = zpfirst + zonecount - 1;
	
	if (find_last_rules(zp, lastrp) < 0)
		return -1;
	
	stdrp = lastrp[false];
	dstrp = lastrp[true];
	dstcmp = zp->z_nrules ? rule_cmp(dstrp, stdrp) : zp->z_isdst ? 1 : -1;
	stdzp = dstzp = zp;

	if (dstcmp < 0) {
		dstrp = NULL;
	} else if (0 < dstcmp) {
		zic_t save = dstrp ? dstrp->r_save : zp->z_save;
		if (0 <= save) {
			stdzp = &zstr[0];
			dstzp = &zstr[1];
			setup_fake_zones(zstr, zp, save);
		}
		setup_all_year_dst_rules(&dstr, &stdr, save, dstrp, stdrp);
		dstrp = &dstr;
		stdrp = &stdr;
	}
	
	len = append_standard_time(result, stdzp, stdrp);
	if (len < 0) {
		result[0] = '\0';
		return -1;
	}
	
	if (dstrp == NULL)
		return compat;
	
	len = append_dst_time(result, len, dstzp, dstrp);
	if (len < 0) {
		result[0] = '\0';
		return -1;
	}
	
	if (append_rule(result, &len, dstrp, dstrp->r_save, stdzp->z_stdoff, &compat) < 0) {
		result[0] = '\0';
		return -1;
	}
	
	if (append_rule(result, &len, stdrp, dstrp->r_save, stdzp->z_stdoff, &compat) < 0) {
		result[0] = '\0';
		return -1;
	}
	
	return compat;
}

static void initialize_variables(zic_t *nonTZlimtime, int *nonTZlimtype, int *defaulttype,
                                 bool *startttisstd, bool *startttisut)
{
    *nonTZlimtime = ZIC_MIN;
    *nonTZlimtype = -1;
    *defaulttype = -1;
    *startttisstd = false;
    *startttisut = false;
    timecnt = 0;
    typecnt = 0;
    charcnt = 0;
}

static void update_min_max_years(const struct zone *zpfirst, ptrdiff_t zonecount)
{
    ptrdiff_t i, j;
    
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
}

static void adjust_year_range(bool do_extend, zic_t *max_year0)
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
    max_year = max(max_year, (redundant_time / (SECSPERDAY * DAYSPERNYEAR) + EPOCH_YEAR + 1));
    *max_year0 = max_year;
    
    if (want_bloat()) {
        if (min_year > YEAR_32BIT_MIN - 1)
            min_year = YEAR_32BIT_MIN - 1;
        if (max_year < YEAR_32BIT_MAX)
            max_year = YEAR_32BIT_MAX;
    }
}

static void mark_rules_todo(struct zone const *zp, zic_t year, zic_t max_year0)
{
    ptrdiff_t j;
    zic_t one = 1;
    zic_t y2038_boundary = one << 31;
    
    for (j = 0; j < zp->z_nrules; ++j) {
        struct rule *rp = &zp->z_rules[j];
        eats(zp->z_filenum, zp->z_linenum, rp->r_filenum, rp->r_linenum);
        rp->r_todo = year >= rp->r_loyear && year <= rp->r_hiyear;
        if (rp->r_todo) {
            rp->r_temp = rpytime(rp, year);
            rp->r_todo = (rp->r_temp < y2038_boundary || year <= max_year0);
        }
    }
}

static ptrdiff_t find_earliest_rule(struct zone const *zp, zic_t stdoff, zic_t save)
{
    ptrdiff_t j, k = -1;
    zic_t ktime = 0;
    
    INITIALIZE(ktime);
    
    for (j = 0; j < zp->z_nrules; ++j) {
        struct rule *r = &zp->z_rules[j];
        zic_t jtime, offset;
        
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
    
    return k;
}

static zic_t calculate_untiltime(struct zone const *zp, zic_t stdoff, zic_t save)
{
    zic_t untiltime = zp->z_untiltime;
    if (!zp->z_untilrule.r_todisut)
        untiltime = tadd(untiltime, -stdoff);
    if (!zp->z_untilrule.r_todisstd)
        untiltime = tadd(untiltime, -save);
    return untiltime;
}

static void process_no_rules(struct zone const *zp, char *startbuf, zic_t starttime,
                            bool usestart, bool useuntil, bool startttisstd, bool startttisut,
                            zic_t *nonTZlimtime, int *nonTZlimtype, int *defaulttype)
{
    int type;
    zic_t save = zp->z_save;
    
    doabbr(startbuf, zp, NULL, zp->z_isdst, save, false);
    type = addtype(oadd(zp->z_stdoff, save), startbuf, zp->z_isdst, startttisstd, startttisut);
    
    if (usestart) {
        addtt(starttime, type);
        if (useuntil && *nonTZlimtime < starttime) {
            *nonTZlimtime = starttime;
            *nonTZlimtype = type;
        }
    } else {
        *defaulttype = type;
    }
}

static void handle_start_transition(struct zone const *zp, char *startbuf, zic_t starttime,
                                   zic_t startoff, zic_t save, bool startttisstd, bool startttisut,
                                   int *defaulttype)
{
    bool isdst = startoff != zp->z_stdoff;
    
    if (*startbuf == '\0' && zp->z_format)
        doabbr(startbuf, zp, disable_percent_s, isdst, save, false);
    
    eat(zp->z_filenum, zp->z_linenum);
    
    if (*startbuf == '\0')
        error(_("can't determine time zone abbreviation to use just after until time"));
    else {
        int type = addtype(startoff, startbuf, isdst, startttisstd, startttisut);
        if (*defaulttype < 0 && !isdst)
            *defaulttype = type;
        addtt(starttime, type);
    }
}

static void add_extension_transition(zic_t max_year, int defaulttype)
{
    struct rule xr;
    struct attype *lastat;
    ptrdiff_t i;
    
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

static void trim_trailing_transitions(zic_t nonTZlimtime, int nonTZlimtype, char *envvar)
{
    zic_t TZstarttime = ZIC_MAX;
    zic_t keep_at_max;
    ptrdiff_t i, j;
    
    for (i = 0; i < timecnt; i++) {
        zic_t at = attypes[i].at;
        if (nonTZlimtime < at && at < TZstarttime)
            TZstarttime = at;
    }
    
    if (TZstarttime == ZIC_MAX)
        TZstarttime = nonTZlimtime;
    
    keep_at_max = max(TZstarttime, redundant_time);
    
    for (i = j = 0; i < timecnt; i++)
        if (attypes[i].at <= keep_at_max) {
            attypes[j].at = attypes[i].at;
            attypes[j].dontmerge = (attypes[i].at == TZstarttime &&
                                   (nonTZlimtype != attypes[i].type || strchr(envvar, ',')));
            attypes[j].type = attypes[i].type;
            j++;
        }
    timecnt = j;
}

static void
outzone(const struct zone *zpfirst, ptrdiff_t zonecount)
{
    register ptrdiff_t i, j;
    register zic_t starttime, untiltime;
    register bool startttisstd;
    register bool startttisut;
    register char *startbuf;
    register char *ab;
    register char *envvar;
    register int max_abbr_len;
    register int max_envvar_len;
    register int compat;
    register bool do_extend;
    register char version;
    zic_t nonTZlimtime;
    int nonTZlimtype;
    zic_t max_year0;
    int defaulttype;

    check_for_signal();

    max_abbr_len = 2 + max_format_len + max_abbrvar_len;
    max_envvar_len = 2 * max_abbr_len + 5 * 9;

    startbuf = emalloc(max_abbr_len + 1);
    ab = emalloc(max_abbr_len + 1);
    envvar = emalloc(max_envvar_len + 1);
    INITIALIZE(untiltime);
    INITIALIZE(starttime);
    
    initialize_variables(&nonTZlimtime, &nonTZlimtype, &defaulttype, &startttisstd, &startttisut);
    update_min_max_years(zpfirst, zonecount);
    
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
    
    adjust_year_range(do_extend, &max_year0);
    
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
            process_no_rules(zp, startbuf, starttime, usestart, useuntil, startttisstd,
                           startttisut, &nonTZlimtime, &nonTZlimtype, &defaulttype);
        } else {
            zic_t year;
            for (year = min_year; year <= max_year; ++year) {
                if (useuntil && year > zp->z_untilrule.r_hiyear)
                    break;
                
                mark_rules_todo(zp, year, max_year0);
                
                for (;;) {
                    register ptrdiff_t k;
                    register zic_t jtime, ktime;
                    register zic_t offset;
                    struct rule *rp;
                    int type;
                    
                    INITIALIZE(ktime);
                    
                    if (useuntil)
                        untiltime = calculate_untiltime(zp, stdoff, save);
                    
                    k = find_earliest_rule(zp, stdoff, save);
                    
                    if (k < 0)
                        break;
                    
                    rp = &zp->z_rules[k];
                    rp->r_todo = false;
                    ktime = tadd(rp->r_temp, -(rp->r_todisut ? 0 : stdoff));
                    if (!rp->r_todisstd)
                        ktime = tadd(ktime, -save);
                    
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
        
        if (usestart)
            handle_start_transition(zp, startbuf, starttime, startoff, save,
                                   startttisstd, startttisut, &defaulttype);
        
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
    
    if (!do_extend && !want_bloat())
        trim_trailing_transitions(nonTZlimtime, nonTZlimtype, envvar);
    
    if (do_extend)
        add_extension_transition(max_year0, defaulttype);
    
    writezone(zpfirst->z_name, envvar, version, defaulttype);
    free(startbuf);
    free(ab);
    free(envvar);
}

static void
addtt(zic_t starttime, int type)
{
	attypes = growalloc(attypes, sizeof *attypes, timecnt, &timecnt_alloc);
	attypes[timecnt].at = starttime;
	attypes[timecnt].dontmerge = false;
	attypes[timecnt].type = type;
	++timecnt;
}

static int find_abbreviation_index(char const *abbr)
{
    register int j;
    for (j = 0; j < charcnt; ++j)
        if (strcmp(&chars[j], abbr) == 0)
            return j;
    return -1;
}

static int find_existing_type(zic_t utoff, bool isdst, int abbr_idx, bool ttisstd, bool ttisut)
{
    register int i;
    for (i = 0; i < typecnt; i++)
        if (utoff == utoffs[i] && isdst == isdsts[i] && abbr_idx == desigidx[i]
            && ttisstd == ttisstds[i] && ttisut == ttisuts[i])
            return i;
    return -1;
}

static void validate_utoff(zic_t utoff)
{
    if (! (-1L - 2147483647L <= utoff && utoff <= 2147483647L)) {
        error(_("UT offset out of range"));
        exit(EXIT_FAILURE);
    }
}

static void check_type_limit(void)
{
    if (typecnt >= TZ_MAX_TYPES) {
        error(_("too many local time types"));
        exit(EXIT_FAILURE);
    }
}

static int create_new_type(zic_t utoff, bool isdst, bool ttisstd, bool ttisut, int abbr_idx)
{
    int i = typecnt++;
    utoffs[i] = utoff;
    isdsts[i] = isdst;
    ttisstds[i] = ttisstd;
    ttisuts[i] = ttisut;
    desigidx[i] = abbr_idx;
    return i;
}

static int
addtype(zic_t utoff, char const *abbr, bool isdst, bool ttisstd, bool ttisut)
{
    int abbr_idx, existing_type;
    
    validate_utoff(utoff);
    
    if (!want_bloat())
        ttisstd = ttisut = false;
    
    abbr_idx = find_abbreviation_index(abbr);
    if (abbr_idx == -1) {
        newabbr(abbr);
        abbr_idx = find_abbreviation_index(abbr);
    } else {
        existing_type = find_existing_type(utoff, isdst, abbr_idx, ttisstd, ttisut);
        if (existing_type >= 0)
            return existing_type;
    }
    
    check_type_limit();
    return create_new_type(utoff, isdst, ttisstd, ttisut, abbr_idx);
}

static void
validate_leap_capacity(void)
{
	if (TZ_MAX_LEAPS <= leapcnt) {
		error(_("too many leap seconds"));
		exit(EXIT_FAILURE);
	}
}

static void
validate_rolling_leap(int rolling)
{
	if (rolling && (lo_time != min_time || hi_time != max_time)) {
		error(_("Rolling leap seconds not supported with -r"));
		exit(EXIT_FAILURE);
	}
}

static int
find_insertion_point(zic_t t)
{
	register int i;
	for (i = 0; i < leapcnt; ++i)
		if (t <= trans[i])
			break;
	return i;
}

static void
shift_leap_arrays(int position)
{
	size_t elements_to_move = (leapcnt - position) * sizeof *trans;
	memmove(&trans[position + 1], &trans[position], elements_to_move);
	memmove(&corr[position + 1], &corr[position], (leapcnt - position) * sizeof *corr);
	memmove(&roll[position + 1], &roll[position], (leapcnt - position) * sizeof *roll);
}

static void
insert_leap_at_position(int position, zic_t t, int correction, int rolling)
{
	trans[position] = t;
	corr[position] = correction;
	roll[position] = rolling;
	++leapcnt;
}

static void
leapadd(zic_t t, int correction, int rolling)
{
	validate_leap_capacity();
	validate_rolling_leap(rolling);
	
	int insertion_point = find_insertion_point(t);
	shift_leap_arrays(insertion_point);
	insert_leap_at_position(insertion_point, t, correction, rolling);
}

static void check_leap_spacing(int index, zic_t prevtrans)
{
    const zic_t MIN_LEAP_INTERVAL = 28 * SECSPERDAY;
    
    if (trans[index] - prevtrans < MIN_LEAP_INTERVAL) {
        error(_("Leap seconds too close together"));
        exit(EXIT_FAILURE);
    }
}

static void check_leap_expiry(void)
{
    if (leapcnt > 0 && trans[leapcnt - 1] >= leapexpires) {
        error(_("last Leap time does not precede Expires time"));
        exit(EXIT_FAILURE);
    }
}

static zic_t propagate_leap_second(int index, zic_t accumulated)
{
    trans[index] = tadd(trans[index], accumulated);
    corr[index] += accumulated;
    return corr[index];
}

static void adjleap(void)
{
    zic_t last = 0;
    zic_t prevtrans = 0;

    for (int i = 0; i < leapcnt; ++i) {
        check_leap_spacing(i, prevtrans);
        prevtrans = trans[i];
        last = propagate_leap_second(i, last);
    }

    if (leapexpires >= 0) {
        leapexpires = oadd(leapexpires, last);
        if (leapcnt > 0) {
            check_leap_expiry();
        }
    }
}

/* Is A a space character in the C locale?  */
static bool is_space(char a)
{
    return a == ' ' || a == '\f' || a == '\n' || 
           a == '\r' || a == '\t' || a == '\v';
}

/* Is A an alphabetic character in the C locale?  */
static bool
is_alpha(char a)
{
	return (a >= 'A' && a <= 'Z') || (a >= 'a' && a <= 'z');
}

/* If A is an uppercase character in the C locale, return its lowercase
   counterpart.  Otherwise, return A.  */
static char
lowerit(char a)
{
	if (a >= 'A' && a <= 'Z') {
		return a + ('a' - 'A');
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
handle_last_prefix(const char **word, const struct lookup **table)
{
	if (*table != lasts || !ciprefix("last", *word) || !(*word)[4])
		return NULL;
	
	if ((*word)[4] == '-') {
		warning(_("\"%s\" is undocumented; use \"last%s\" instead"),
			*word, *word + 5);
		return NULL;
	}
	
	*word += 4;
	*table = wday_names;
	return NULL;
}

static const struct lookup *
find_exact_match(const char *word, const struct lookup *table)
{
	const struct lookup *lp;
	
	for (lp = table; lp->l_word != NULL; ++lp)
		if (ciequal(word, lp->l_word))
			return lp;
	
	return NULL;
}

static const struct lookup *
find_inexact_match(const char *word, const struct lookup *table)
{
	const struct lookup *foundlp = NULL;
	const struct lookup *lp;
	
	for (lp = table; lp->l_word != NULL; ++lp) {
		if (ciprefix(word, lp->l_word)) {
			if (foundlp == NULL)
				foundlp = lp;
			else
				return NULL;
		}
	}
	
	return foundlp;
}

static void
check_pre_2017c_ambiguity(const char *word, const struct lookup *table)
{
	bool pre_2017c_match = false;
	const struct lookup *lp;
	
	for (lp = table; lp->l_word; lp++) {
		if (itsabbr(word, lp->l_word)) {
			if (pre_2017c_match) {
				warning(_("\"%s\" is ambiguous in pre-2017c zic"), word);
				break;
			}
			pre_2017c_match = true;
		}
	}
}

static const struct lookup *
byword(const char *word, const struct lookup *table)
{
	const struct lookup *foundlp;
	
	if (word == NULL || table == NULL)
		return NULL;
	
	handle_last_prefix(&word, &table);
	
	foundlp = find_exact_match(word, table);
	if (foundlp != NULL)
		return foundlp;
	
	foundlp = find_inexact_match(word, table);
	
	if (foundlp && noise)
		check_pre_2017c_ambiguity(word, table);
	
	return foundlp;
}

static void handle_quote_error(void)
{
    error(_("Odd number of quotation marks"));
    exit(EXIT_FAILURE);
}

static void handle_too_many_fields_error(void)
{
    error(_("Too many input fields"));
    exit(EXIT_FAILURE);
}

static void skip_spaces(char **cp)
{
    while (is_space(**cp))
        ++(*cp);
}

static int is_end_of_input(char c)
{
    return c == '\0' || c == '#';
}

static int is_field_separator(char c)
{
    return c == '\0' || c == '#' || is_space(c);
}

static char* process_quoted_content(char *dp, char **cp)
{
    while ((*dp = *(*cp)++) != '"') {
        if (*dp != '\0')
            ++dp;
        else
            handle_quote_error();
    }
    return dp;
}

static char* process_field_character(char *dp, char **cp)
{
    if ((*dp = *(*cp)++) != '"')
        ++dp;
    else
        dp = process_quoted_content(dp, cp);
    return dp;
}

static char* extract_field(char *cp, char **next_cp)
{
    char *dstart = cp;
    char *dp = cp;
    
    while (!is_field_separator(*cp)) {
        dp = process_field_character(dp, &cp);
    }
    
    if (is_space(*cp))
        ++cp;
    
    *dp = '\0';
    *next_cp = cp;
    
    return dstart + (*dstart == '-' && dp == dstart + 1);
}

static int getfields(char *cp, char **array, int arrayelts)
{
    int nsubs = 0;
    
    while (1) {
        skip_spaces(&cp);
        
        if (is_end_of_input(*cp))
            break;
        
        if (nsubs == arrayelts)
            handle_too_many_fields_error();
        
        array[nsubs++] = extract_field(cp, &cp);
    }
    
    return nsubs;
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

static zic_t handle_boundary_years(zic_t wantedy)
{
	if (wantedy == ZIC_MIN)
		return min_time;
	if (wantedy == ZIC_MAX)
		return max_time;
	return 0;
}

static zic_t calculate_dayoff_base(zic_t wantedy, zic_t y)
{
	int yrem = wantedy % YEARSPERREPEAT - y % YEARSPERREPEAT;
	return ((wantedy / YEARSPERREPEAT - y / YEARSPERREPEAT
		 + yrem / YEARSPERREPEAT - (yrem % YEARSPERREPEAT < 0))
		* DAYSPERREPEAT);
}

static zic_t normalize_wanted_year(zic_t wantedy, zic_t y)
{
	int yrem = wantedy % YEARSPERREPEAT - y % YEARSPERREPEAT;
	return y + (yrem + 2 * YEARSPERREPEAT) % YEARSPERREPEAT;
}

static zic_t add_year_days(zic_t dayoff, zic_t y, zic_t wantedy)
{
	while (wantedy != y) {
		int i = len_years[isleap(y)];
		dayoff = oadd(dayoff, i);
		y++;
	}
	return dayoff;
}

static zic_t add_month_days(zic_t dayoff, int *m, int target_month, zic_t y)
{
	while (*m != target_month) {
		int i = len_months[isleap(y)][*m];
		dayoff = oadd(dayoff, i);
		++(*m);
	}
	return dayoff;
}

static int validate_february_29(int day, int month, zic_t year, int dycode)
{
	if (month == TM_FEBRUARY && day == 29 && !isleap(year)) {
		if (dycode == DC_DOWLEQ)
			return 28;
		error(_("use of 2/29 in non leap-year"));
		exit(EXIT_FAILURE);
	}
	return day;
}

static zic_t calculate_weekday(zic_t dayoff)
{
	return ((EPOCH_WDAY + dayoff % DAYSPERWEEK + DAYSPERWEEK) % DAYSPERWEEK);
}

static void adjust_forward(zic_t *dayoff, zic_t *wday, int *i)
{
	*dayoff = oadd(*dayoff, 1);
	if (++(*wday) >= DAYSPERWEEK)
		*wday = 0;
	++(*i);
}

static void adjust_backward(zic_t *dayoff, zic_t *wday, int *i)
{
	*dayoff = oadd(*dayoff, -1);
	if (--(*wday) < 0)
		*wday = DAYSPERWEEK - 1;
	--(*i);
}

static zic_t adjust_for_weekday(zic_t dayoff, const struct rule *rp, int *i, zic_t y, int m)
{
	if (rp->r_dycode != DC_DOWGEQ && rp->r_dycode != DC_DOWLEQ)
		return dayoff;
	
	zic_t wday = calculate_weekday(dayoff);
	
	while (wday != rp->r_wday) {
		if (rp->r_dycode == DC_DOWGEQ)
			adjust_forward(&dayoff, &wday, i);
		else
			adjust_backward(&dayoff, &wday, i);
	}
	
	if (*i < 0 || *i >= len_months[isleap(y)][m]) {
		if (noise)
			warning(_("rule goes past start/end of month; \
will not work with pre-2004 versions of zic"));
	}
	
	return dayoff;
}

static zic_t validate_dayoff_bounds(zic_t dayoff)
{
	if (dayoff < min_time / SECSPERDAY)
		return min_time;
	if (dayoff > max_time / SECSPERDAY)
		return max_time;
	return 0;
}

static zic_t
rpytime(const struct rule *rp, zic_t wantedy)
{
	register int m, i;
	register zic_t dayoff;
	register zic_t t, y;
	
	zic_t boundary = handle_boundary_years(wantedy);
	if (boundary != 0)
		return boundary;
	
	m = TM_JANUARY;
	y = EPOCH_YEAR;
	
	dayoff = calculate_dayoff_base(wantedy, y);
	wantedy = normalize_wanted_year(wantedy, y);
	
	dayoff = add_year_days(dayoff, y, wantedy);
	dayoff = add_month_days(dayoff, &m, rp->r_month, wantedy);
	
	i = validate_february_29(rp->r_dayofmonth, m, wantedy, rp->r_dycode);
	--i;
	dayoff = oadd(dayoff, i);
	
	dayoff = adjust_for_weekday(dayoff, rp, &i, wantedy, m);
	
	zic_t bounds = validate_dayoff_bounds(dayoff);
	if (bounds != 0)
		return bounds;
	
	t = (zic_t) dayoff * SECSPERDAY;
	return tadd(t, rp->r_tod);
}

static int is_valid_abbr_char(char c)
{
    return is_alpha(c) || ('0' <= c && c <= '9') || c == '-' || c == '+';
}

static const char* validate_abbreviation_length(const char *string, int length)
{
    if (noise && length < 3)
        return _("time zone abbreviation has fewer than 3 characters");
    if (length > ZIC_MAX_ABBR_LEN_WO_WARN)
        return _("time zone abbreviation has too many characters");
    return NULL;
}

static int count_valid_prefix_length(const char *string)
{
    const char *cp = string;
    while (is_valid_abbr_char(*cp))
        ++cp;
    return cp - string;
}

static void validate_abbreviation(const char *string)
{
    if (strcmp(string, GRANDPARENTED) == 0)
        return;
    
    int valid_length = count_valid_prefix_length(string);
    const char *mp = validate_abbreviation_length(string, valid_length);
    
    if (mp == NULL && string[valid_length] != '\0')
        mp = _("time zone abbreviation differs from POSIX standard");
    
    if (mp != NULL)
        warning("%s (%s)", mp, string);
}

static void check_buffer_capacity(int required_size)
{
    if (charcnt + required_size > TZ_MAX_CHARS) {
        error(_("too many, or too long, time zone abbreviations"));
        exit(EXIT_FAILURE);
    }
}

static void append_to_buffer(const char *string, int size)
{
    strcpy(&chars[charcnt], string);
    charcnt += size;
}

static void newabbr(const char *string)
{
    validate_abbreviation(string);
    
    int size = strlen(string) + 1;
    check_buffer_capacity(size);
    append_to_buffer(string, size);
}

/* Ensure that the directories of ARGNAME exist, by making any missing
   ones.  If ANCESTORS, do this only for ARGNAME's ancestors; otherwise,
   do it for ARGNAME too.  Exit with failure if there is trouble.
   Do not consider an existing file to be trouble.  */
static void
mkdirs(char const *argname, bool ancestors)
{
	char *name = estrdup(argname);
	char *cp = skip_root_slashes(name);
	
	while (cp && ((cp = strchr(cp, '/')) || !ancestors)) {
		if (cp)
			*cp = '\0';
		
		create_directory(name);
		
		if (cp)
			*cp++ = '/';
	}
	free(name);
}

static char *
skip_root_slashes(char *path)
{
	while (*path == '/')
		path++;
	return path;
}

static void
create_directory(const char *name)
{
	if (mkdir(name, MKDIR_UMASK) != 0) {
		check_mkdir_error(name, errno);
	}
}

static void
check_mkdir_error(const char *name, int err)
{
	if (is_fatal_mkdir_error(err)) {
		error(_("%s: Can't create directory %s: %s"),
		      progname, name, strerror(err));
		exit(EXIT_FAILURE);
	}
}

static bool
is_fatal_mkdir_error(int err)
{
	return err == ELOOP || err == ENAMETOOLONG ||
	       err == ENOENT || err == ENOTDIR;
}
