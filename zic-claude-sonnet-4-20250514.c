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
  if (!file || !buf) {
    errno = EINVAL;
    return -1;
  }
  
  if (size == 0) {
    errno = EINVAL;
    return -1;
  }
  
  errno = ENOTSUP;
  return -1;
}
static int
symlink(const char *target, const char *linkname)
{
  if (!target || !linkname) {
    errno = EINVAL;
    return -1;
  }
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
    if (msg == NULL) {
        msg = "unknown error";
    }
    fprintf(stderr, _("%s: Memory exhausted: %s\n"), progname, msg);
    exit(EXIT_FAILURE);
}

ATTRIBUTE_NORETURN static void
size_overflow(void)
{
  memory_exhausted("size overflow");
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
  if (!str) {
    return NULL;
  }
  
  size_t len = strlen(str) + 1;
  char *result = malloc(len);
  if (!result) {
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
        int error_code = ENOMEM;
#ifdef HAVE_MALLOC_ERRNO
        error_code = errno;
#endif
        memory_exhausted(strerror(error_code));
    }
    return ptr;
}

static void *
emalloc(size_t size)
{
  if (size == 0) {
    return NULL;
  }
  return memcheck(malloc(size));
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
  return memcheck(strdup(str));
}

static ptrdiff_t
grow_nitems_alloc(ptrdiff_t *nitems_alloc, ptrdiff_t itemsize)
{
  if (!nitems_alloc || itemsize <= 0) {
    memory_exhausted(_("invalid parameters"));
  }
  
  ptrdiff_t current_alloc = *nitems_alloc;
  ptrdiff_t addend = (current_alloc >> 1) + 1;
  
#if defined ckd_add && defined ckd_mul
  ptrdiff_t new_alloc;
  ptrdiff_t product;
  if (ckd_add(&new_alloc, current_alloc, addend) ||
      ckd_mul(&product, new_alloc, itemsize) ||
      product > INDEX_MAX) {
    memory_exhausted(_("integer overflow"));
  }
  *nitems_alloc = new_alloc;
  return product;
#else
  ptrdiff_t max_safe_alloc = ((INDEX_MAX - 1) / 3 * 2) / itemsize;
  if (current_alloc > max_safe_alloc) {
    memory_exhausted(_("integer overflow"));
  }
  
  ptrdiff_t new_alloc = current_alloc + addend;
  *nitems_alloc = new_alloc;
  return new_alloc * itemsize;
#endif
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
  if (i == COMMAND_LINE_FILENUM) {
    return _("command line");
  }
  
  if (i == LEAPSEC_FILENUM) {
    return leapsec ? (strcmp(leapsec, "-") == 0 ? _("standard input") : leapsec) : NULL;
  }
  
  if (i < 0 || !main_argv || !main_argv[i]) {
    return NULL;
  }
  
  return strcmp(main_argv[i], "-") == 0 ? _("standard input") : main_argv[i];
}

static void
eats(int fnum, lineno num, int rfnum, lineno rnum)
{
    if (fnum >= 0 && rfnum >= 0) {
        filenum = fnum;
        linenum = num;
        rfilenum = rfnum;
        rlinenum = rnum;
    }
}

static void
eat(int fnum, lineno num)
{
    eats(fnum, num, 0, -1);
}

ATTRIBUTE_FORMAT((printf, 1, 0)) static void
verror(const char *const string, va_list args)
{
	if (filenum != 0) {
		fprintf(stderr, _("\"%s\", line %"PRIdMAX": "),
			filename(filenum), linenum);
	}
	vfprintf(stderr, string, args);
	if (rfilenum != 0) {
		fprintf(stderr, _(" (rule from \"%s\", line %"PRIdMAX")"),
			filename(rfilenum), rlinenum);
	}
	fprintf(stderr, "\n");
}

ATTRIBUTE_FORMAT((printf, 1, 2)) static void
error(const char *const string, ...)
{
    va_list args;
    
    if (string == NULL) {
        return;
    }
    
    va_start(args, string);
    verror(string, args);
    va_end(args);
    errors = true;
}

ATTRIBUTE_FORMAT((printf, 1, 2)) static void
warning(const char *const string, ...)
{
	if (string == NULL) {
		return;
	}
	
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
  char const *error_msg = NULL;
  
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
  if (!stream) {
    exit(EXIT_FAILURE);
  }
  
  if (fprintf(stream,
              _("%s: usage is %s [ --version ] [ --help ] [ -v ] \\\n"
                "\t[ -b {slim|fat} ] [ -d directory ] [ -l localtime ]"
                " [ -L leapseconds ] \\\n"
                "\t[ -p posixrules ] [ -r '[@lo][/@hi]' ] [ -R '@hi' ] \\\n"
                "\t[ -t localtime-link ] \\\n"
                "\t[ filename ... ]\n\n"
                "Report bugs to %s.\n"),
              progname, progname, REPORT_BUGS_TO) < 0) {
    exit(EXIT_FAILURE);
  }
  
  if (status == EXIT_SUCCESS) {
    close_file(stream, NULL, NULL, NULL);
  }
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
  if (!a || !b) {
    return (!a && !b) ? 0 : (!a ? -1 : 1);
  }

  struct link const *l = (struct link const *)a;
  struct link const *m = (struct link const *)b;
  
  if (!l->l_linkname || !m->l_linkname) {
    return (!l->l_linkname && !m->l_linkname) ? 0 : (!l->l_linkname ? -1 : 1);
  }

  int cmp = strcmp(l->l_linkname, m->l_linkname);
  if (cmp != 0) {
    return cmp;
  }

  cmp = l->l_filenum - m->l_filenum;
  if (cmp != 0) {
    return cmp;
  }
  
  if (l->l_linenum < m->l_linenum) {
    return -1;
  }
  if (l->l_linenum > m->l_linenum) {
    return 1;
  }
  return 0;
}

/* Compare the string KEY to the link B, for bsearch.  */
static int
bsearch_linkcmp(const void *key, const void *b)
{
  const struct link *m = (const struct link *)b;
  const char *key_str = (const char *)key;
  
  if (!key_str || !m || !m->l_linkname) {
    return key_str ? 1 : (m && m->l_linkname ? -1 : 0);
  }
  
  return strcmp(key_str, m->l_linkname);
}

/* Make the links specified by the Link lines.  */
static void
make_links(void)
{
    ptrdiff_t i, j, nalinks, pass_size;
    
    if (1 < nlinks) {
        qsort(links, nlinks, sizeof *links, qsort_linkcmp);
    }

    j = 0;
    for (i = 0; i < nlinks; i++) {
        while (i + 1 < nlinks && strcmp(links[i].l_linkname, links[i + 1].l_linkname) == 0) {
            i++;
        }
        links[j++] = links[i];
    }
    nlinks = pass_size = j;

    j = nalinks = nlinks;

    for (i = 0; i < nalinks; i++) {
        struct link *l;

        eat(links[i].l_filenum, links[i].l_linenum);

        if (i == j) {
            if (nalinks - i == pass_size) {
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

        l = bsearch(links[i].l_target, &links[i + 1], j - (i + 1),
                    sizeof *links, bsearch_linkcmp);
        if (!l) {
            l = bsearch(links[i].l_target, &links[j], nalinks - j,
                        sizeof *links, bsearch_linkcmp);
        }
        
        if (!l) {
            dolink(links[i].l_target, links[i].l_linkname, false);
        } else {
            links = growalloc(links, sizeof *links, nalinks, &nlinks_alloc);
            links[nalinks++] = links[i];
        }

        if (noise && i < nlinks) {
            if (l) {
                warning(_("link %s targeting link %s mishandled by pre-2023 zic"),
                        links[i].l_linkname, links[i].l_target);
            } else if (bsearch(links[i].l_target, links, nlinks, sizeof *links,
                               bsearch_linkcmp)) {
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

static void
signal_handler(int sig)
{
#ifndef SA_SIGINFO
    if (signal(sig, signal_handler) == SIG_ERR) {
        return;
    }
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
  
  const size_t signal_count = sizeof(signals) / sizeof(signals[0]);
  
  for (size_t i = 0; i < signal_count; i++) {
#ifdef SA_SIGINFO
    struct sigaction new_action;
    struct sigaction old_action;
    
    new_action.sa_handler = signal_handler;
    sigemptyset(&new_action.sa_mask);
    new_action.sa_flags = 0;
    
    if (sigaction(signals[i], &new_action, &old_action) == 0) {
      if (!(old_action.sa_flags & SA_SIGINFO) && old_action.sa_handler == SIG_IGN) {
        sigaction(signals[i], &old_action, NULL);
        got_signal = 0;
      }
    }
#else
    void (*old_handler)(int) = signal(signals[i], signal_handler);
    if (old_handler == SIG_IGN) {
      signal(signals[i], SIG_IGN);
      got_signal = 0;
    }
#endif
  }
}

/* If a signal has arrived, terminate zic with appropriate status.  */
static void
check_for_signal(void)
{
    int sig = got_signal;
    if (sig != 0) {
        if (signal(sig, SIG_DFL) == SIG_ERR) {
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
  intmax_t lo = min_time, hi = max_time;
  char *lo_end = timerange, *hi_end;
  
  if (*timerange == '@') {
    errno = 0;
    lo = strtoimax(timerange + 1, &lo_end, 10);
    if (lo_end == timerange + 1 || (lo == INTMAX_MAX && errno == ERANGE)) {
      return false;
    }
  }
  
  hi_end = lo_end;
  if (lo_end[0] == '/' && lo_end[1] == '@') {
    errno = 0;
    hi = strtoimax(lo_end + 2, &hi_end, 10);
    if (hi_end == lo_end + 2 || hi == INTMAX_MIN) {
      return false;
    }
    if (hi == INTMAX_MAX && errno == ERANGE) {
      hi--;
    }
  }
  
  if (*hi_end != '\0') {
    return false;
  }
  
  if (hi < lo || max_time < lo || hi < min_time) {
    return false;
  }
  
  lo_time = (lo > min_time) ? lo : min_time;
  hi_time = (hi < max_time) ? hi : max_time;
  
  return true;
}

/* Generate redundant time stamps up to OPT.  Return true if successful.  */
static bool
redundant_time_option(char *opt)
{
  if (!opt || *opt != '@') {
    return false;
  }
  
  char *opt_start = opt + 1;
  if (*opt_start == '\0') {
    return false;
  }
  
  char *opt_end;
  errno = 0;
  intmax_t redundant = strtoimax(opt_start, &opt_end, 10);
  
  if (errno != 0 || opt_end == opt_start || *opt_end != '\0') {
    return false;
  }
  
  if (redundant > redundant_time) {
    redundant_time = redundant;
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
	ptrdiff_t k, i, j;
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
		static const char bloat_default[] = ZIC_BLOAT_DEFAULT;
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
		infile((int)k, argv[k]);
	}
	
	if (errors) {
		return EXIT_FAILURE;
	}
	
	associate();
	change_directory(directory);
	catch_signals();
	
	for (i = 0; i < nzones; i = j) {
		for (j = i + 1; j < nzones && zones[j].z_name == NULL; ++j) {
			continue;
		}
		outzone(&zones[i], (int)(j - i));
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
	       char const *component_end)
{
	enum { component_len_max = 14 };
	ptrdiff_t component_len = component_end - component;
	
	if (component_len == 0) {
		if (!*name) {
			error(_("empty file name"));
		} else if (component == name) {
			error(_("file name '%s' begins with '/'"), name);
		} else if (*component_end) {
			error(_("file name '%s' contains '//'"), name);
		} else {
			error(_("file name '%s' ends with '/'"), name);
		}
		return false;
	}
	
	if (component_len > 0 && component_len <= 2 && 
	    component[0] == '.' && component_end[-1] == '.') {
		int len = (int)component_len;
		error(_("file name '%s' contains '%.*s' component"),
		      name, len, component);
		return false;
	}
	
	if (noise) {
		if (component_len > 0 && component[0] == '-') {
			warning(_("file name '%s' component contains leading '-'"),
			        name);
		}
		if (component_len > component_len_max) {
			warning(_("file name '%s' contains overlength component"
			          " '%.*s...'"),
			        name, component_len_max, component);
		}
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

    if (!name) {
        return false;
    }

    const char *component = name;
    const char *cp;

    for (cp = name; *cp; cp++) {
        unsigned char c = *cp;
        if (noise && !strchr(benign, c)) {
            if (strchr(printable_and_not_benign, c)) {
                warning(_("file name '%s' contains byte '%c'"), name, c);
            } else {
                warning(_("file name '%s' contains byte '\\%o'"), name, c);
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
#include <errno.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <time.h>
#include <limits.h>

#if HAVE_GETRANDOM
#include <sys/random.h>
#endif

#define ENTROPY_BUFFER_SIZE 32

static uint_fast64_t get_getrandom_value(void);
static uint_fast64_t get_fallback_value(void);
static void initialize_fallback_generator(void);

static uint_fast64_t
get_rand_u64(void)
{
#if HAVE_GETRANDOM
    uint_fast64_t result = get_getrandom_value();
    if (result != 0 || errno != ENOSYS) {
        return result;
    }
#endif
    return get_fallback_value();
}

#if HAVE_GETRANDOM
static uint_fast64_t
get_getrandom_value(void)
{
    static uint_fast64_t entropy_buffer[ENTROPY_BUFFER_SIZE];
    static int nwords = 0;
    
    if (nwords <= 0) {
        ssize_t bytes_read;
        do {
            bytes_read = getrandom(entropy_buffer, sizeof(entropy_buffer), 0);
        } while (bytes_read < 0 && errno == EINTR);
        
        if (bytes_read < 0) {
            nwords = -1;
            return 0;
        }
        nwords = (int)(bytes_read / sizeof(uint_fast64_t));
    }
    
    if (nwords > 0) {
        return entropy_buffer[--nwords];
    }
    
    return 0;
}
#endif

static uint_fast64_t
get_fallback_value(void)
{
    initialize_fallback_generator();
    
    uint_fast64_t rand_max = RAND_MAX;
    uint_fast64_t nrand = (rand_max < UINT_FAST64_MAX) ? rand_max + 1 : 0;
    uint_fast64_t rmod = (INT_MAX < UINT_FAST64_MAX) ? 0 : UINT_FAST64_MAX / nrand + 1;
    uint_fast64_t r = 0;
    uint_fast64_t rmax = 0;
    
    do {
        uint_fast64_t rmax1 = rmax;
        if (rmod != 0) {
            rmax1 %= rmod;
            r %= rmod;
        }
        rmax1 = nrand * rmax1 + rand_max;
        r = nrand * r + (uint_fast64_t)rand();
        rmax = (rmax < rmax1) ? rmax1 : UINT_FAST64_MAX;
    } while (rmax < UINT_FAST64_MAX);
    
    return r;
}

static void
initialize_fallback_generator(void)
{
    static bool initialized = false;
    if (!initialized) {
        srand((unsigned int)time(NULL));
        initialized = true;
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
  static const int prefixlen = 4;
  static const int alphabetlen = 62;
  static const int suffixlen = 6;
  
  char const *lastslash = strrchr(src, '/');
  ptrdiff_t dirlen = lastslash ? lastslash + 1 - src : 0;
  
  uint_fast64_t base__6 = 56800235584ULL;
  uint_fast64_t unfair_min = - ((UINTMAX_MAX % base__6 + 1) % base__6);

  if (!dst) {
    dst = emalloc(size_sum(dirlen, prefixlen + suffixlen + 1));
    memcpy(dst, src, dirlen);
    memcpy(dst + dirlen, prefix, prefixlen);
    dst[dirlen + prefixlen + suffixlen] = '\0';
    *name = *namealloc = dst;
  }

  uint_fast64_t r;
  do
    r = get_rand_u64();
  while (unfair_min <= r);

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
open_outfile(char const **outname, char **tempname)
{
#if __STDC_VERSION__ < 201112
  static char const fopen_mode[] = "wb";
#else
  static char const fopen_mode[] = "wbx";
#endif

  FILE *fp;
  bool dirs_made = false;
  
  if (!*tempname) {
    random_dirent(outname, tempname);
  }

  while (!(fp = fopen(*outname, fopen_mode))) {
    int fopen_errno = errno;
    
    if (fopen_errno == ENOENT && !dirs_made) {
      mkdirs(*outname, true);
      dirs_made = true;
      continue;
    }
    
    if (fopen_errno == EEXIST) {
      random_dirent(outname, tempname);
      continue;
    }
    
    fprintf(stderr, _("%s: Can't create %s/%s: %s\n"),
            progname, directory, *outname, strerror(fopen_errno));
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
  if (!tempname) {
    return;
  }
  
  if (rename(tempname, name) != 0) {
    int rename_errno = errno;
    remove(tempname);
    fprintf(stderr, _("%s: rename to %s/%s: %s\n"),
            progname, directory, name, strerror(rename_errno));
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
  if (!target || !linkname) {
    return NULL;
  }

  size_t dir_len = 0;
  size_t dotdots = 0;
  char const *f = target;
  char *result = NULL;
  ptrdiff_t linksize = INDEX_MAX;

  if (*linkname == '/') {
    size_t len = strlen(directory);
    if (len == 0) {
      return NULL;
    }
    
    size_t lenslash = len + (directory[len - 1] != '/' ? 1 : 0);
    size_t targetsize = strlen(target) + 1;
    linksize = size_sum(lenslash, targetsize);
    
    if (linksize <= 0) {
      return NULL;
    }
    
    result = emalloc(linksize);
    if (!result) {
      return NULL;
    }
    
    memcpy(result, directory, len);
    if (directory[len - 1] != '/') {
      result[len] = '/';
    }
    memcpy(result + lenslash, target, targetsize);
    f = result;
  }

  size_t i = 0;
  while (f[i] && f[i] == linkname[i]) {
    if (f[i] == '/') {
      dir_len = i + 1;
    }
    i++;
  }

  while (linkname[i]) {
    if (linkname[i] == '/' && (i == 0 || linkname[i - 1] != '/')) {
      dotdots++;
    }
    i++;
  }

  size_t taillen = strlen(f + dir_len);
  ptrdiff_t dotdotetcsize = size_sum(size_product(dotdots, 3), taillen + 1);
  
  if (dotdotetcsize <= 0) {
    free(result);
    return NULL;
  }

  if (dotdotetcsize <= linksize) {
    if (!result) {
      result = emalloc(dotdotetcsize);
      if (!result) {
        return NULL;
      }
    }
    
    for (i = 0; i < dotdots; i++) {
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

static void
dolink(char const *target, char const *linkname, bool staysymlink)
{
	bool linkdirs_made = false;
	int link_errno;
	char *tempname = NULL;
	char const *outname = linkname;
	int targetissym = -2, linknameissym = -2;

	check_for_signal();

	if (handle_remove_target(target, linkname)) {
		return;
	}

	link_errno = attempt_hard_link(target, &outname, &tempname, &linkdirs_made, 
									linkname, staysymlink, &targetissym, &linknameissym);

	if (link_errno != 0) {
		handle_link_fallback(target, linkname, &outname, &tempname, 
							 &linkdirs_made, link_errno);
	}

	rename_dest(tempname, linkname);
}

static bool
handle_remove_target(char const *target, char const *linkname)
{
	if (strcmp(target, "-") != 0) {
		return false;
	}

	if (remove(linkname) == 0 || errno == ENOENT || errno == ENOTDIR) {
		return true;
	}

	char const *e = strerror(errno);
	fprintf(stderr, _("%s: Can't remove %s/%s: %s\n"),
			progname, directory, linkname, e);
	exit(EXIT_FAILURE);
}

static int
attempt_hard_link(char const *target, char const **outname, char **tempname,
				 bool *linkdirs_made, char const *linkname, bool staysymlink,
				 int *targetissym, int *linknameissym)
{
	int link_errno;

	while (true) {
		if (linkat(AT_FDCWD, target, AT_FDCWD, *outname, AT_SYMLINK_FOLLOW) == 0) {
			return 0;
		}
		link_errno = errno;
		
		if (link_errno == EINVAL) {
			link_errno = ENOTSUP;
		}

#if HAVE_LINK
		if (should_try_link_fallback(link_errno, target, *outname, targetissym)) {
			if (link(target, *outname) == 0) {
				return 0;
			}
			link_errno = errno;
		}
#endif

		if (link_errno == EXDEV || link_errno == ENOTSUP) {
			break;
		}

		if (!handle_link_error(link_errno, outname, tempname, linkdirs_made,
							  linkname, &staysymlink, linknameissym)) {
			fprintf(stderr, _("%s: Can't link %s/%s to %s/%s: %s\n"),
					progname, directory, target, directory, *outname,
					strerror(link_errno));
			exit(EXIT_FAILURE);
		}
	}
	return link_errno;
}

#if HAVE_LINK
static bool
should_try_link_fallback(int link_errno, char const *target, char const *outname,
						int *targetissym)
{
	return (link_errno == ENOTSUP &&
			(same_parent_dirs(target, outname) ||
			 0 <= itssymlink(target, targetissym)));
}
#endif

static bool
handle_link_error(int link_errno, char const **outname, char **tempname,
				 bool *linkdirs_made, char const *linkname,
				 bool *staysymlink, int *linknameissym)
{
	if (link_errno == EEXIST) {
		*staysymlink &= !*tempname;
		random_dirent(outname, tempname);
		if (*staysymlink && itssymlink(linkname, linknameissym)) {
			return false;
		}
		return true;
	}
	
	if (link_errno == ENOENT && !*linkdirs_made) {
		mkdirs(linkname, true);
		*linkdirs_made = true;
		return true;
	}
	
	return false;
}

static void
handle_link_fallback(char const *target, char const *linkname,
					 char const **outname, char **tempname,
					 bool *linkdirs_made, int link_errno)
{
	int symlink_errno = attempt_symlink(target, linkname, outname, tempname, linkdirs_made);

	if (symlink_errno == 0) {
		if (link_errno != ENOTSUP && link_errno != EEXIST) {
			warning(_("symbolic link used because hard link failed: %s"),
					strerror(link_errno));
		}
		return;
	}

	copy_file(target, linkname, outname, tempname);
	emit_copy_warning(link_errno, symlink_errno);
}

static int
attempt_symlink(char const *target, char const *linkname,
			   char const **outname, char **tempname, bool *linkdirs_made)
{
	bool absolute = (*target == '/');
	char *linkalloc = absolute ? NULL : relname(target, linkname);
	char const *contents = absolute ? target : linkalloc;
	int symlink_errno;

	while (true) {
		if (symlink(contents, *outname) == 0) {
			symlink_errno = 0;
			break;
		}
		symlink_errno = errno;
		
		if (symlink_errno == EEXIST) {
			random_dirent(outname, tempname);
		} else if (symlink_errno == ENOENT && !*linkdirs_made) {
			mkdirs(linkname, true);
			*linkdirs_made = true;
		} else {
			break;
		}
	}
	
	free(linkalloc);
	return symlink_errno;
}

static void
copy_file(char const *target, char const *linkname,
		 char const **outname, char **tempname)
{
	FILE *fp = fopen(target, "rb");
	if (!fp) {
		char const *e = strerror(errno);
		fprintf(stderr, _("%s: Can't read %s/%s: %s\n"),
				progname, directory, target, e);
		exit(EXIT_FAILURE);
	}
	
	FILE *tp = open_outfile(outname, tempname);
	int c;
	while ((c = getc(fp)) != EOF) {
		putc(c, tp);
	}
	
	close_file(tp, directory, linkname, *tempname);
	close_file(fp, directory, target, NULL);
}

static void
emit_copy_warning(int link_errno, int symlink_errno)
{
	if (link_errno != ENOTSUP) {
		warning(_("copy used because hard link failed: %s"),
				strerror(link_errno));
	} else if (symlink_errno != ENOTSUP) {
		warning(_("copy used because symbolic link failed: %s"),
				strerror(symlink_errno));
	}
}

/* Return 1 if NAME is an absolute symbolic link, -1 if it is relative,
   0 if it is not a symbolic link.  If *CACHE is not -2, it is the
   cached result of a previous call to this function with the same NAME.  */
static int
itssymlink(char const *name, int *cache)
{
  char c = '\0';
  ssize_t result;
  
  if (*cache != -2) {
    return *cache;
  }
  
  if (name == NULL) {
    *cache = 0;
    return *cache;
  }
  
  result = readlink(name, &c, 1);
  if (result < 0) {
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

static int
rcomp(const void *cp1, const void *cp2)
{
  const struct rule *r1 = cp1;
  const struct rule *r2 = cp2;
  
  if (!r1 || !r2) {
    return (r1 == r2) ? 0 : (r1 ? 1 : -1);
  }
  
  if (!r1->r_name || !r2->r_name) {
    return (r1->r_name == r2->r_name) ? 0 : (r1->r_name ? 1 : -1);
  }
  
  return strcmp(r1->r_name, r2->r_name);
}

static void
associate(void)
{
	register struct zone *	zp;
	register struct rule *	rp;
	register ptrdiff_t i, j, base, out;

	if (nrules > 1) {
		qsort(rules, nrules, sizeof *rules, rcomp);
		for (i = 0; i < nrules - 1; ++i) {
			if (strcmp(rules[i].r_name, rules[i + 1].r_name) != 0)
				continue;
			if (rules[i].r_filenum == rules[i + 1].r_filenum)
				continue;
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
			i = j - 1;
		}
	}
	
	for (i = 0; i < nzones; ++i) {
		zp = &zones[i];
		zp->z_rules = NULL;
		zp->z_nrules = 0;
	}
	
	for (base = 0; base < nrules; base = out) {
		rp = &rules[base];
		for (out = base + 1; out < nrules; ++out)
			if (strcmp(rp->r_name, rules[out].r_name) != 0)
				break;
		for (i = 0; i < nzones; ++i) {
			zp = &zones[i];
			if (strcmp(zp->z_rule, rp->r_name) != 0)
				continue;
			zp->z_rules = rp;
			zp->z_nrules = out - base;
		}
	}
	
	for (i = 0; i < nzones; ++i) {
		zp = &zones[i];
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
static bool
inputline(FILE *fp, char *buf, ptrdiff_t bufsize)
{
  if (!fp || !buf || bufsize <= 0) {
    error(_("invalid parameters"));
    exit(EXIT_FAILURE);
  }

  ptrdiff_t linelen = 0;
  int ch;
  
  while ((ch = getc(fp)) != '\n') {
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

static void
infile(int fnum, char const *name)
{
	FILE *fp;
	const struct lookup *lp;
	bool wantcont;
	lineno num;

	if (strcmp(name, "-") == 0) {
		fp = stdin;
	} else if ((fp = fopen(name, "r")) == NULL) {
		const char *e = strerror(errno);
		fprintf(stderr, _("%s: Can't open %s: %s\n"),
			progname, name, e);
		exit(EXIT_FAILURE);
	}
	
	wantcont = false;
	for (num = 1; ; ++num) {
		enum { bufsize_bound
		  = (min(INT_MAX, INDEX_MAX) / FORMAT_LEN_GROWTH_BOUND) };
		char buf[min(_POSIX2_LINE_MAX, bufsize_bound)];
		int nfields;
		char *fields[MAX_FIELDS];
		
		eat(fnum, num);
		if (!inputline(fp, buf, sizeof buf))
			break;
		
		nfields = getfields(buf, fields,
				    sizeof fields / sizeof *fields);
		
		if (nfields == 0) {
			continue;
		}
		
		if (wantcont) {
			wantcont = inzcont(fields, nfields);
		} else {
			struct lookup const *line_codes
			  = fnum < 0 ? leap_line_codes : zi_line_codes;
			lp = byword(fields[0], line_codes);
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
    char hhx, mmx, ssx, xr = '0', xs;
    int tenths = 0;
    bool ok = true;
    int scan_result;

    if (string == NULL || *string == '\0')
        return 0;

    if (*string == '-') {
        sign = -1;
        ++string;
    } else {
        sign = 1;
    }

    scan_result = sscanf(string,
                        "%"SCNdZIC"%c%d%c%d%c%1d%*[0]%c%*[0123456789]%c",
                        &hh, &hhx, &mm, &mmx, &ss, &ssx, &tenths, &xr, &xs);

    switch (scan_result) {
        case 8:
            ok = ('0' <= xr && xr <= '9');
            /* FALLTHROUGH */
        case 7:
            ok = ok && (ssx == '.');
            if (ok && noise)
                warning(_("fractional seconds rejected by pre-2018 versions of zic"));
            /* FALLTHROUGH */
        case 5:
            ok = ok && (mmx == ':');
            /* FALLTHROUGH */
        case 3:
            ok = ok && (hhx == ':');
            /* FALLTHROUGH */
        case 1:
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

    ss += (5 + ((ss ^ 1) & (xr == '0'))) <= tenths;

    if (noise && (hh > HOURSPERDAY || (hh == HOURSPERDAY && (mm != 0 || ss != 0))))
        warning(_("values over 24 hours not handled by pre-2007 versions of zic"));

    return oadd(sign * hh * SECSPERHOUR, sign * (mm * SECSPERMIN + ss));
}

static zic_t
getsave(char *field, bool *isdst)
{
  int dst = -1;
  zic_t save;
  size_t fieldlen = strlen(field);
  
  if (fieldlen > 0) {
    char *ep = field + fieldlen - 1;
    if (*ep == 'd') {
      dst = 1;
      *ep = '\0';
    } else if (*ep == 's') {
      dst = 0;
      *ep = '\0';
    }
  }
  
  save = gethms(field, _("invalid saved time"));
  *isdst = (dst < 0) ? (save != 0) : (dst != 0);
  return save;
}

static void
inrule(char **fields, int nfields)
{
	struct rule r;
	size_t abbrvar_len;

	if (nfields != RULE_FIELDS) {
		error(_("wrong number of fields on Rule line"));
		return;
	}
	
	if (fields[RF_NAME] == NULL || !is_valid_rule_name(fields[RF_NAME])) {
		error(_("Invalid rule name \"%s\""), fields[RF_NAME] ? fields[RF_NAME] : "(null)");
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
	if (max_abbrvar_len < abbrvar_len) {
		max_abbrvar_len = abbrvar_len;
	}
	
	rules = growalloc(rules, sizeof *rules, nrules, &nrules_alloc);
	rules[nrules++] = r;
}

static int
is_valid_rule_name(const char *name)
{
	char c = *name;
	
	if (c == '\0' || c == ' ' || c == '\f' || c == '\n' || 
	    c == '\r' || c == '\t' || c == '\v' || c == '+' || 
	    c == '-' || (c >= '0' && c <= '9')) {
		return 0;
	}
	return 1;
}

static bool
inzone(char **fields, int nfields)
{
	ptrdiff_t i;

	if (nfields < ZONE_MINFIELDS || nfields > ZONE_MAXFIELDS) {
		error(_("wrong number of fields on Zone line"));
		return false;
	}
	
	if (lcltime != NULL && strcmp(fields[ZF_NAME], tzdefault) == 0) {
		error(_("\"Zone %s\" line and -l option are mutually exclusive"),
			tzdefault);
		return false;
	}
	
	if (strcmp(fields[ZF_NAME], TZDEFRULES) == 0 && psxrules != NULL) {
		error(_("\"Zone %s\" line and -p option are mutually exclusive"),
			TZDEFRULES);
		return false;
	}
	
	for (i = 0; i < nzones; ++i) {
		if (zones[i].z_name != NULL &&
		    strcmp(zones[i].z_name, fields[ZF_NAME]) == 0) {
			error(_("duplicate zone name %s"
				" (file \"%s\", line %"PRIdMAX")"),
			      fields[ZF_NAME],
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
	if (nfields < ZONEC_MINFIELDS || nfields > ZONEC_MAXFIELDS) {
		error(_("wrong number of fields on Zone continuation line"));
		return false;
	}
	return inzsub(fields, nfields, true);
}

static bool
inzsub(char **fields, int nfields, bool iscont)
{
    char *cp;
    char *cp1;
    struct zone z;
    int format_len;
    int i_stdoff, i_rule, i_format;
    int i_untilyear, i_untilmonth;
    int i_untilday, i_untiltime;
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
    if (cp != NULL) {
        ++cp;
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
    z.z_rule = estrdup(fields[i_rule]);
    z.z_format = cp1 = estrdup(fields[i_format]);
    
    if (z.z_format_specifier == 'z') {
        cp1[cp - fields[i_format]] = 's';
        if (noise)
            warning(_("format '%s' not handled by pre-2015 versions of zic"), fields[i_format]);
    }
    
    zones = growalloc(zones, sizeof *zones, nzones, &nzones_alloc);
    zones[nzones++] = z;
    
    return hasuntil;
}

static zic_t
getleapdatetime(char **fields, bool expire_line)
{
	const char *cp;
	const struct lookup *lp;
	zic_t i, j;
	zic_t year;
	int month, day;
	zic_t dayoff, tod;
	zic_t t;
	char xs;

	if (!fields || !fields[LP_YEAR] || !fields[LP_MONTH] || 
	    !fields[LP_DAY] || !fields[LP_TIME]) {
		error(_("missing required fields"));
		return -1;
	}

	dayoff = 0;
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

	j = EPOCH_YEAR;
	while (j != year) {
		if (year > j) {
			i = len_years[isleap(j)];
			++j;
		} else {
			--j;
			i = -len_years[isleap(j)];
		}
		dayoff = oadd(dayoff, i);
		if (dayoff == -1) {
			error(_("day offset overflow"));
			return -1;
		}
	}

	lp = byword(fields[LP_MONTH], mon_names);
	if (lp == NULL) {
		error(_("invalid month name"));
		return -1;
	}
	month = lp->l_value;

	j = TM_JANUARY;
	while (j != month) {
		i = len_months[isleap(year)][j];
		dayoff = oadd(dayoff, i);
		if (dayoff == -1) {
			error(_("day offset overflow"));
			return -1;
		}
		++j;
	}

	cp = fields[LP_DAY];
	if (sscanf(cp, "%d%c", &day, &xs) != 1 ||
		day <= 0 || day > len_months[isleap(year)][month]) {
		error(_("invalid day of month"));
		return -1;
	}

	dayoff = oadd(dayoff, day - 1);
	if (dayoff == -1) {
		error(_("day offset overflow"));
		return -1;
	}

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
	if (tod == -1) {
		return -1;
	}

	t = tadd(t, tod);
	if (t == -1) {
		error(_("time addition overflow"));
		return -1;
	}

	if (t < 0) {
		error(_("leap second precedes Epoch"));
		return -1;
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

    int correction = 0;
    if (!fields[LP_CORR][0]) {
        correction = -1;
    } else if (strcmp(fields[LP_CORR], "+") == 0) {
        correction = 1;
    } else {
        error(_("invalid CORRECTION field on Leap line"));
        return;
    }

    if (correction != 0) {
        leapadd(t, correction, lp->l_value);
    }
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
	struct link	l;

	if (nfields != LINK_FIELDS) {
		error(_("wrong number of fields on Link line"));
		return;
	}
	if (fields[LF_TARGET] == NULL || *fields[LF_TARGET] == '\0') {
		error(_("blank TARGET field on Link line"));
		return;
	}
	if (fields[LF_LINKNAME] == NULL || !namecheck(fields[LF_LINKNAME])) {
		return;
	}
	l.l_filenum = filenum;
	l.l_linenum = linenum;
	l.l_target = estrdup(fields[LF_TARGET]);
	if (l.l_target == NULL) {
		return;
	}
	l.l_linkname = estrdup(fields[LF_LINKNAME]);
	if (l.l_linkname == NULL) {
		free(l.l_target);
		return;
	}
	links = growalloc(links, sizeof *links, nlinks, &nlinks_alloc);
	if (links == NULL) {
		free(l.l_target);
		free(l.l_linkname);
		return;
	}
	links[nlinks++] = l;
}

static bool
rulesub(struct rule *rp, const char *loyearp, const char *hiyearp,
	const char *typep, const char *monthp, const char *dayp,
	const char *timep)
{
	const struct lookup *lp;
	const char *cp;
	char *dp;
	char *ep;
	char xs;

	if (!rp || !loyearp || !hiyearp || !typep || !monthp || !dayp || !timep) {
		error(_("invalid parameters"));
		return false;
	}

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
	if (lp) {
		if (lp->l_value == YR_MINIMUM) {
			warning(_("FROM year \"%s\" is obsolete; treated as %d"),
				cp, YEAR_32BIT_MIN - 1);
			rp->r_loyear = YEAR_32BIT_MIN - 1;
		}
	} else if (sscanf(cp, "%"SCNdZIC"%c", &rp->r_loyear, &xs) != 1) {
		error(_("invalid starting year"));
		return false;
	}

	cp = hiyearp;
	lp = byword(cp, end_years);
	rp->r_hiwasnum = (lp == NULL);
	if (!rp->r_hiwasnum) {
		if (lp->l_value == YR_MAXIMUM) {
			rp->r_hiyear = ZIC_MAX;
		} else if (lp->l_value == YR_ONLY) {
			rp->r_hiyear = rp->r_loyear;
		}
	} else if (sscanf(cp, "%"SCNdZIC"%c", &rp->r_hiyear, &xs) != 1) {
		error(_("invalid ending year"));
		return false;
	}

	if (rp->r_loyear > rp->r_hiyear) {
		error(_("starting year greater than ending year"));
		return false;
	}

	if (*typep != '\0') {
		error(_("year type \"%s\" is unsupported; use \"-\" instead"),
			typep);
		return false;
	}

	dp = estrdup(dayp);
	if (!dp) {
		error(_("memory allocation failed"));
		return false;
	}

	lp = byword(dp, lasts);
	if (lp != NULL) {
		rp->r_dycode = DC_DOWLEQ;
		rp->r_wday = lp->l_value;
		rp->r_dayofmonth = len_months[1][rp->r_month];
	} else {
		ep = strchr(dp, '<');
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
			*ep++ = 0;
			if (*ep++ != '=') {
				error(_("invalid day of month"));
				free(dp);
				return false;
			}
			lp = byword(dp, wday_names);
			if (lp == NULL) {
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

static void
convert(uint_fast32_t val, char *buf)
{
	int i;
	int shift;
	unsigned char *const b = (unsigned char *) buf;

	if (buf == NULL) {
		return;
	}

	for (i = 0, shift = 24; i < 4; ++i, shift -= 8) {
		b[i] = (val >> shift) & 0xff;
	}
}

static void
convert64(uint_fast64_t val, char *buf)
{
	unsigned char *const b = (unsigned char *) buf;

	for (int i = 0; i < 8; ++i) {
		b[i] = (val >> (56 - (i * 8))) & 0xff;
	}
}

static void
puttzcode(zic_t val, FILE *fp)
{
	char buf[4];

	if (!fp) {
		return;
	}

	convert(val, buf);
	
	if (fwrite(buf, sizeof buf, 1, fp) != 1) {
		return;
	}
}

static void
puttzcodepass(zic_t val, FILE *fp, int pass)
{
    if (pass == 1) {
        puttzcode(val, fp);
    } else {
        char buf[8];
        convert64(val, buf);
        if (fwrite(buf, sizeof buf, 1, fp) != 1) {
            return;
        }
    }
}

static int
atcomp(const void *avp, const void *bvp)
{
  const struct attype *ap = (const struct attype *)avp;
  const struct attype *bp = (const struct attype *)bvp;
  zic_t a = ap->at;
  zic_t b = bp->at;
  
  if (a < b) return -1;
  if (a > b) return 1;
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
    if (!ats || !types) {
        return r;
    }

    while (r.count > 0 && ats[r.base] < lo) {
        r.defaulttype = types[r.base];
        r.count--;
        r.base++;
    }

    while (r.leapcount > 1 && trans[r.leapbase + 1] <= lo) {
        r.leapcount--;
        r.leapbase++;
    }

    while (r.leapbase > 0 &&
           ((corr[r.leapbase - 1] < corr[r.leapbase]) != (corr[r.leapbase] > 0))) {
        r.leapcount++;
        r.leapbase--;
    }

    if (hi < max_time) {
        while (r.count > 0 && hi + 1 < ats[r.base + r.count - 1]) {
            r.count--;
        }
        while (r.leapcount > 0 && hi + 1 < trans[r.leapbase + r.leapcount - 1]) {
            r.leapcount--;
        }
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
	struct timerange rangeall = {0}, range32, range64;

	if (timecnt > 1)
		qsort(attypes, timecnt, sizeof *attypes, atcomp);

	optimize_transitions();

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

	correct_for_leap_seconds(ats, timecnt);

	rangeall.defaulttype = defaulttype;
	rangeall.count = timecnt;
	rangeall.leapcount = leapcnt;
	range64 = limitrange(rangeall, lo_time,
			     max(hi_time,
				 redundant_time - (ZIC_MIN < redundant_time)),
			     ats, types);
	range32 = limitrange(range64, ZIC32_MIN, ZIC32_MAX, ats, types);

	version = determine_version(version, &range32, &range64, name);

	fp = open_outfile(&outname, &tempname);

	for (pass = 1; pass <= 2; ++pass) {
		write_pass_data(fp, pass, &range32, &range64, ats, types, version);
	}
	fprintf(fp, "\n%s\n", string);
	close_file(fp, directory, name, tempname);
	rename_dest(tempname, name);
	free(ats);
}

static void optimize_transitions(void)
{
	ptrdiff_t fromi, toi;

	toi = 0;
	for (fromi = 0; fromi < timecnt; ++fromi) {
		if (should_merge_transition(fromi, toi)) {
			attypes[toi - 1].type = attypes[fromi].type;
			continue;
		}
		if (should_keep_transition(fromi, toi)) {
			attypes[toi++] = attypes[fromi];
		}
	}
	timecnt = toi;
}

static bool should_merge_transition(ptrdiff_t fromi, ptrdiff_t toi)
{
	if (toi == 0) return false;
	
	zic_t at_adjusted = attypes[fromi].at + utoffs[attypes[toi - 1].type];
	zic_t prev_at_adjusted = attypes[toi - 1].at + 
		utoffs[toi == 1 ? 0 : attypes[toi - 2].type];
	
	return at_adjusted <= prev_at_adjusted;
}

static bool should_keep_transition(ptrdiff_t fromi, ptrdiff_t toi)
{
	if (toi == 0 || attypes[fromi].dontmerge) return true;
	
	int curr_type = attypes[fromi].type;
	int prev_type = attypes[toi - 1].type;
	
	return (utoffs[prev_type] != utoffs[curr_type]) ||
		   (isdsts[prev_type] != isdsts[curr_type]) ||
		   (desigidx[prev_type] != desigidx[curr_type]);
}

static void correct_for_leap_seconds(zic_t *ats, ptrdiff_t count)
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

static char determine_version(char version, const struct timerange *range32,
							  const struct timerange *range64, const char *name)
{
	for (int pass = 1; pass <= 2; pass++) {
		const struct timerange *r = pass == 1 ? range32 : range64;
		
		if (pass == 1 && !want_bloat()) continue;
		
		if (r->leapexpiry) {
			if (noise) {
				warning(_("%s: pre-2021b clients may mishandle"
						  " leap second expiry"), name);
			}
			version = '4';
		}
		
		if (0 < r->leapcount &&
			corr[r->leapbase] != 1 && corr[r->leapbase] != -1) {
			if (noise) {
				warning(_("%s: pre-2021b clients may mishandle"
						  " leap second table truncation"), name);
			}
			version = '4';
		}
		
		if (version == '4') break;
	}
	return version;
}

static void write_pass_data(FILE *fp, int pass, 
						   const struct timerange *range32,
						   const struct timerange *range64,
						   const zic_t *ats, const unsigned char *types,
						   char version)
{
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
		thisdefaulttype = range32->defaulttype;
		thistimei = range32->base;
		thistimecnt = range32->count;
		toomanytimes = thistimecnt >> 31 >> 1 != 0;
		thisleapi = range32->leapbase;
		thisleapcnt = range32->leapcount;
		thisleapexpiry = range32->leapexpiry;
		thismin = ZIC32_MIN;
		thismax = ZIC32_MAX;
	} else {
		thisdefaulttype = range64->defaulttype;
		thistimei = range64->base;
		thistimecnt = range64->count;
		toomanytimes = thistimecnt >> 31 >> 31 >> 2 != 0;
		thisleapi = range64->leapbase;
		thisleapcnt = range64->leapcount;
		thisleapexpiry = range64->leapexpiry;
		thismin = min_time;
		thismax = max_time;
	}
	
	if (toomanytimes) {
		error(_("too many transition times"));
	}

	locut = thismin < lo_time && lo_time <= thismax;
	hicut = thismin <= hi_time && hi_time < thismax;
	thistimelim = thistimei + thistimecnt;
	memset(omittype, true, typecnt);

	setup_pretranstype(&pretranstype, thisdefaulttype, locut, pass,
					   thistimei, thistimecnt, ats, omittype);

	setup_default_type(&thisdefaulttype, pass, range64->defaulttype);

	if (locut) {
		thisdefaulttype = unspecifiedtype;
	}
	
	mark_used_types(thistimei, thistimelim, thisdefaulttype, hicut,
					omittype, types);

	old0 = strlen(omittype);
	setup_type_mapping(old0, thisdefaulttype, &thistypecnt, omittype, typemap);
	setup_character_data(old0, &thischarcnt, &stdcnt, &utcnt,
						 thistypecnt, omittype, thischars, indmap);

	if (pass == 1 && !want_bloat()) {
		hicut = thisleapexpiry = false;
		pretranstype = -1;
		thistimecnt = thisleapcnt = 0;
		thistypecnt = thischarcnt = 1;
	}

	write_header(fp, version, utcnt, stdcnt, thisleapcnt, thisleapexpiry,
				 pretranstype, thistimecnt, hicut, thistypecnt, thischarcnt);

	if (pass == 1 && !want_bloat()) {
		write_minimal_data(fp);
		return;
	}

	write_transition_times(fp, pass, pretranstype, thistimei, thistimelim,
						   hicut, ats);
	write_transition_types(fp, pretranstype, thistimei, thistimelim,
						   hicut, typemap, types);
	write_type_info(fp, old0, thisdefaulttype, omittype, indmap);
	write_character_data(fp, thischars, thischarcnt);
	write_leap_seconds(fp, pass, thisleapi, thisleapcnt, thisleapexpiry);
	write_standard_wall_indicators(fp, stdcnt, utcnt, old0, omittype);
}

static void setup_pretranstype(int *pretranstype, int thisdefaulttype,
								bool locut, int pass, ptrdiff_t thistimei,
								ptrdiff_t thistimecnt, const zic_t *ats,
								char *omittype)
{
	if ((locut || (pass == 1 && thistimei)) &&
		!(thistimecnt && ats[thistimei] == lo_time)) {
		*pretranstype = thisdefaulttype;
		omittype[*pretranstype] = false;
	}
}

static void setup_default_type(int *thisdefaulttype, int pass,
								int range64_defaulttype)
{
	if (pass == 1 && lo_time <= ZIC32_MIN) {
		*thisdefaulttype = range64_defaulttype;
	}
}

static void mark_used_types(ptrdiff_t thistimei, ptrdiff_t thistimelim,
							int thisdefaulttype, bool hicut,
							char *omittype, const unsigned char *types)
{
	omittype[thisdefaulttype] = false;
	
	for (ptrdiff_t i = thistimei; i < thistimelim; i++) {
		omittype[types[i]] = false;
	}
	
	if (hicut) {
		omittype[unspecifiedtype] = false;
	}
}

static void setup_type_mapping(int old0, int thisdefaulttype,
							   int *thistypecnt, const char *omittype,
							   int *typemap)
{
	*thistypecnt = 0;
	for (int i = old0; i < typecnt; i++) {
		if (!omittype[i]) {
			int mapped_i = (i == old0) ? thisdefaulttype :
						   (i == thisdefaulttype) ? old0 : i;
			typemap[mapped_i] = (*thistypecnt)++;
		}
	}
}

static void setup_character_data(int old0, int *thischarcnt, int *stdcnt,
								 int *utcnt, int thistypecnt,
								 const char *omittype, char *thischars,
								 int *indmap)
{
	for (int i = 0; i < TZ_MAX_CHARS; ++i) {
		indmap[i] = -1;
	}
	
	*thischarcnt = *stdcnt = *utcnt = 0;
	
	for (int i = old0; i < typecnt; i++) {
		if (omittype[i]) continue;
		
		if (ttisstds[i]) *stdcnt = thistypecnt;
		if (ttisuts[i]) *utcnt = thistypecnt;
		if (indmap[desigidx[i]] >= 0) continue;
		
		char *thisabbr = &chars[desigidx[i]];
		int j;
		for (j = 0; j < *thischarcnt; ++j) {
			if (strcmp(&thischars[j], thisabbr) == 0) break;
		}
		
		if (j == *thischarcnt) {
			strcpy(&thischars[*thischarcnt], thisabbr);
			*thischarcnt += strlen(thisabbr) + 1;
		}
		indmap[desigidx[i]] = j;
	}
}

static void write_header(FILE *fp, char version, int utcnt, int stdcnt,
						 int thisleapcnt, bool thisleapexpiry,
						 int pretranstype, ptrdiff_t thistimecnt,
						 bool hicut, int thistypecnt, int thischarcnt)
{
	struct tzhead tzh;
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
}

static void write_minimal_data(FILE *fp)
{
	puttzcode(0, fp);
	putc(0, fp);
	putc(0, fp);
	putc(0, fp);
}

static void write_transition_times(FILE *fp, int pass, int pretranstype,
								   ptrdiff_t thistimei, ptrdiff_t thistimelim,
								   bool hicut, const zic_t *ats)
{
	zic_t lo = (pass == 1 && lo_time < ZIC32_MIN) ? ZIC32_MIN : lo_time;

	if (0 <= pretranstype) {
		puttzcodepass(lo, fp, pass);
	}
	
	for (ptrdiff_t i = thistimei; i < thistimelim; ++i) {
		puttzcodepass(ats[i], fp, pass);
	}
	
	if (hicut) {
		puttzcodepass(hi_time + 1, fp, pass);
	}
}

static void write_transition_types(FILE *fp, int pretranstype,
								   ptrdiff_t thistimei, ptrdiff_t thistimelim,
								   bool hicut, const int *typemap,
								   const unsigned char *types)
{
	if (0 <= pretranstype) {
		putc(typemap[pretranstype], fp);
	}
	
	for (ptrdiff_t i = thistimei; i < thistimelim; i++) {
		putc(typemap[types[i]], fp);
	}
	
	if (hicut) {
		putc(typemap[unspecifiedtype], fp);
	}
}

static void write_type_info(FILE *fp, int old0, int thisdefaulttype,
							const char *omittype, const int *indmap)
{
	for (int i = old0; i < typecnt; i++) {
		int h = (i == old0) ? thisdefaulttype :
				(i == thisdefaulttype) ? old0 : i;
		
		if (!omittype[h]) {
			puttzcode(utoffs[h], fp);
			putc(isdsts[h], fp);
			putc(indmap[desigidx[h]], fp);
		}
	}
}

static void write_character_data(FILE *fp, const char *thischars, int thischarcnt)
{
	if (thischarcnt != 0) {
		fwrite(thischars, sizeof(char), thischarcnt, fp);
	}
}

static void write_leap_seconds(FILE *fp, int pass, int thisleapi,
							   int thisleapcnt, bool thisleapexpiry)
{
	int thisleaplim = thisleapi + thisleapcnt;
	
	for (int i = thisleapi; i < thisleaplim; ++i) {
		zic_t todo;
		
		if (roll[i]) {
			int j = find_transition_type(i);
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

static int find_transition_type(int leap_index)
{
	int j;
	
	if (timecnt == 0 || trans[leap_index] < ats[0]) {
		j = 0;
		while (isdsts[j]) {
			if (++j >= typecnt) {
				j = 0;
				break;
			}
		}
	} else {
		j = 1;
		while (j < timecnt && trans[leap_index] >= ats[j]) {
			++j;
		}
		j = types[j - 1];
	}
	
	return j;
}

static void write_standard_wall_indicators(FILE *fp, int stdcnt, int utcnt,
										   int old0, const char *omittype)
{
	if (stdcnt != 0) {
		for (int i = old0; i < typecnt; i++) {
			if (!omittype[i]) {
				putc(ttisstds[i], fp);
			}
		}
	}
	
	if (utcnt != 0) {
		for (int i = old0; i < typecnt; i++) {
			if (!omittype[i]) {
				putc(ttisuts[i], fp);
			}
		}
	}
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
  
  if (hours >= 100) {
    error(_("%%z UT offset magnitude exceeds 99:59:59"));
    return "%z";
  }

  char *p = buf;
  *p++ = sign;
  *p++ = '0' + hours / 10;
  *p++ = '0' + hours % 10;
  
  if (minutes != 0 || seconds != 0) {
    *p++ = '0' + minutes / 10;
    *p++ = '0' + minutes % 10;
    
    if (seconds != 0) {
      *p++ = '0' + seconds / 10;
      *p++ = '0' + seconds % 10;
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
	char const *format;

	if (!abbr || !zp) {
		return 0;
	}

	format = zp->z_format;
	slashp = strchr(format, '/');

	if (slashp == NULL) {
		char letterbuf[PERCENT_Z_LEN_BOUND + 1];
		if (zp->z_format_specifier == 'z') {
			letters = abbroffset(letterbuf, zp->z_stdoff + save);
		} else if (!letters) {
			letters = "%s";
		} else if (letters == disable_percent_s) {
			return 0;
		}
		sprintf(abbr, format, letters);
	} else if (isdst) {
		strcpy(abbr, slashp + 1);
	} else {
		ptrdiff_t prefix_len = slashp - format;
		memcpy(abbr, format, prefix_len);
		abbr[prefix_len] = '\0';
	}

	len = strlen(abbr);
	if (!doquotes) {
		return len;
	}

	for (cp = abbr; is_alpha(*cp); cp++) {
		continue;
	}

	if (len > 0 && *cp == '\0') {
		return len;
	}

	abbr[len + 2] = '\0';
	abbr[len + 1] = '>';
	memmove(abbr + 1, abbr, len);
	abbr[0] = '<';

	return len + 2;
}

static void
updateminmax(const zic_t x)
{
    if (x < min_year)
        min_year = x;
    if (x > max_year)
        max_year = x;
}

static int
stringoffset(char *result, zic_t offset)
{
	int hours;
	int minutes;
	int seconds;
	bool negative = offset < 0;
	int len = 0;

	if (negative) {
		offset = -offset;
		result[0] = '-';
		len = 1;
	}
	
	seconds = offset % SECSPERMIN;
	offset /= SECSPERMIN;
	minutes = offset % MINSPERHOUR;
	offset /= MINSPERHOUR;
	hours = offset;
	
	if (hours >= HOURSPERDAY * DAYSPERWEEK) {
		result[0] = '\0';
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

static int
stringrule(char *result, struct rule *const rp, zic_t save, zic_t stdoff)
{
	zic_t tod = rp->r_tod;
	int compat = 0;

	if (rp->r_dycode == DC_DOM) {
		int month, total;

		if (rp->r_dayofmonth == 29 && rp->r_month == TM_FEBRUARY)
			return -1;
		
		total = 0;
		for (month = 0; month < rp->r_month; ++month)
			total += len_months[0][month];
		
		if (rp->r_month <= 1)
			result += sprintf(result, "%d", total + rp->r_dayofmonth - 1);
		else
			result += sprintf(result, "J%d", total + rp->r_dayofmonth);
	} else if (rp->r_dycode == DC_DOWGEQ || rp->r_dycode == DC_DOWLEQ) {
		int week;
		int wday = rp->r_wday;
		int wdayoff;

		if (rp->r_dycode == DC_DOWGEQ) {
			wdayoff = (rp->r_dayofmonth - 1) % DAYSPERWEEK;
			if (wdayoff)
				compat = 2013;
			wday -= wdayoff;
			tod += wdayoff * SECSPERDAY;
			week = 1 + (rp->r_dayofmonth - 1) / DAYSPERWEEK;
		} else {
			if (rp->r_dayofmonth == len_months[1][rp->r_month]) {
				week = 5;
			} else {
				wdayoff = rp->r_dayofmonth % DAYSPERWEEK;
				if (wdayoff)
					compat = 2013;
				wday -= wdayoff;
				tod += wdayoff * SECSPERDAY;
				week = rp->r_dayofmonth / DAYSPERWEEK;
			}
		}
		
		if (wday < 0)
			wday += DAYSPERWEEK;
		result += sprintf(result, "M%d.%d.%d", rp->r_month + 1, week, wday);
	} else {
		return -1;
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

static int
rule_cmp(struct rule const *a, struct rule const *b)
{
	if (a == NULL && b == NULL)
		return 0;
	if (a == NULL)
		return -1;
	if (b == NULL)
		return 1;
	
	if (a->r_hiyear < b->r_hiyear)
		return -1;
	if (a->r_hiyear > b->r_hiyear)
		return 1;
	
	if (a->r_hiyear == ZIC_MAX)
		return 0;
	
	if (a->r_month < b->r_month)
		return -1;
	if (a->r_month > b->r_month)
		return 1;
	
	if (a->r_dayofmonth < b->r_dayofmonth)
		return -1;
	if (a->r_dayofmonth > b->r_dayofmonth)
		return 1;
	
	return 0;
}

/* Store into RESULT a proleptic TZ string that represent the future
   predictions for the zone ZPFIRST with ZONECOUNT entries.  Return a
   compatibility indicator (a TZDB release year) if successful, a
   negative integer if no such TZ string exists.  */
static int
stringzone(char *result, struct zone const *zpfirst, ptrdiff_t zonecount)
{
	const struct zone *zp;
	struct rule *rp;
	struct rule *stdrp;
	struct rule *dstrp;
	ptrdiff_t i;
	int compat = 0;
	int c;
	int offsetlen;
	struct rule stdr, dstr;
	ptrdiff_t len;
	int dstcmp;
	struct rule *lastrp[2] = { NULL, NULL };
	struct zone zstr[2];
	const struct zone *stdzp;
	const struct zone *dstzp;

	if (!result || !zpfirst || zonecount <= 0) {
		if (result) result[0] = '\0';
		return -1;
	}

	result[0] = '\0';

	if (hi_time < max_time)
		return -1;

	zp = zpfirst + zonecount - 1;
	for (i = 0; i < zp->z_nrules; ++i) {
		struct rule **last;
		int cmp;
		rp = &zp->z_rules[i];
		last = &lastrp[rp->r_isdst];
		cmp = rule_cmp(*last, rp);
		if (cmp < 0)
			*last = rp;
		else if (cmp == 0)
			return -1;
	}
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
		stdr.r_abbrvar = save < 0 && stdrp ? stdrp->r_abbrvar : NULL;
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
	zic_t starttime, untiltime;
	bool startttisstd;
	bool startttisut;
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

	if (!zpfirst || zonecount <= 0) {
		return;
	}

	check_for_signal();

	max_abbr_len = 2 + max_format_len + max_abbrvar_len;
	max_envvar_len = 2 * max_abbr_len + 5 * 9;

	startbuf = emalloc(max_abbr_len + 1);
	if (!startbuf) return;
	ab = emalloc(max_abbr_len + 1);
	if (!ab) {
		free(startbuf);
		return;
	}
	envvar = emalloc(max_envvar_len + 1);
	if (!envvar) {
		free(startbuf);
		free(ab);
		return;
	}

	INITIALIZE(untiltime);
	INITIALIZE(starttime);

	timecnt = 0;
	typecnt = 0;
	charcnt = 0;
	startttisstd = false;
	startttisut = false;
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
			warning(_("%s: pre-%d clients may mishandle distant timestamps"),
				zpfirst->z_name, compat);
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
				
				while (true) {
					ptrdiff_t k;
					zic_t jtime, ktime;
					zic_t offset;
					struct rule *rp;
					int type;

					INITIALIZE(ktime);
					
					if (useuntil) {
						untiltime = zp->z_untiltime;
						if (!zp->z_untilrule.r_todisut)
							untiltime = tadd(untiltime, -stdoff);
						if (!zp->z_untilrule.r_todisstd)
							untiltime = tadd(untiltime, -save);
					}
					
					k = -1;
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
						if (!*startbuf && (oadd(zp->z_stdoff, rp->r_save) == startoff)) {
							doabbr(startbuf, zp, rp->r_abbrvar, rp->r_isdst, rp->r_save, false);
						}
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
		
		for (i = j = 0; i < timecnt; i++) {
			if (attypes[i].at <= keep_at_max) {
				attypes[j].at = attypes[i].at;
				attypes[j].dontmerge = (attypes[i].at == TZstarttime &&
						       (nonTZlimtype != attypes[i].type ||
							strchr(envvar, ',')));
				attypes[j].type = attypes[i].type;
				j++;
			}
		}
		timecnt = j;
	}
	
	if (do_extend) {
		struct rule xr;
		struct attype *lastat;
		
		xr.r_month = TM_JANUARY;
		xr.r_dycode = DC_DOM;
		xr.r_dayofmonth = 1;
		xr.r_tod = 0;
		
		for (lastat = attypes, i = 1; i < timecnt; i++) {
			if (attypes[i].at > lastat->at)
				lastat = &attypes[i];
		}
		
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
	if (!attypes) {
		timecnt_alloc = 0;
	}
	
	attypes = growalloc(attypes, sizeof(*attypes), timecnt, &timecnt_alloc);
	if (!attypes) {
		return;
	}
	
	attypes[timecnt].at = starttime;
	attypes[timecnt].dontmerge = false;
	attypes[timecnt].type = type;
	timecnt++;
}

static int
addtype(zic_t utoff, char const *abbr, bool isdst, bool ttisstd, bool ttisut)
{
    int i, j;

    if (utoff < -2147483647L - 1L || utoff > 2147483647L) {
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
    
    if (j == charcnt) {
        newabbr(abbr);
    } else {
        for (i = 0; i < typecnt; i++) {
            if (utoff == utoffs[i] && isdst == isdsts[i] && j == desigidx[i] &&
                ttisstd == ttisstds[i] && ttisut == ttisuts[i]) {
                return i;
            }
        }
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

static void
leapadd(zic_t t, int correction, int rolling)
{
	int i;

	if (TZ_MAX_LEAPS <= leapcnt) {
		error(_("too many leap seconds"));
		exit(EXIT_FAILURE);
	}
	if (rolling && (lo_time != min_time || hi_time != max_time)) {
		error(_("Rolling leap seconds not supported with -r"));
		exit(EXIT_FAILURE);
	}
	for (i = 0; i < leapcnt; i++) {
		if (t <= trans[i]) {
			break;
		}
	}
	
	int elements_to_move = leapcnt - i;
	if (elements_to_move > 0) {
		memmove(&trans[i + 1], &trans[i], elements_to_move * sizeof(trans[0]));
		memmove(&corr[i + 1], &corr[i], elements_to_move * sizeof(corr[0]));
		memmove(&roll[i + 1], &roll[i], elements_to_move * sizeof(roll[0]));
	}
	
	trans[i] = t;
	corr[i] = correction;
	roll[i] = rolling;
	leapcnt++;
}

static void
adjleap(void)
{
	register int	i;
	register zic_t	last = 0;
	register zic_t	prevtrans = 0;
	const zic_t	min_leap_interval = 28 * SECSPERDAY;

	for (i = 0; i < leapcnt; ++i) {
		if (trans[i] - prevtrans < min_leap_interval) {
			error(_("Leap seconds too close together"));
			exit(EXIT_FAILURE);
		}
		prevtrans = trans[i];
		trans[i] = tadd(trans[i], last);
		last = corr[i] += last;
	}

	if (leapexpires >= 0) {
		leapexpires = oadd(leapexpires, last);
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
    return (a == ' ' || a == '\f' || a == '\n' || a == '\r' || a == '\t' || a == '\v');
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
byword(const char *word, const struct lookup *table)
{
	const struct lookup *foundlp;
	const struct lookup *lp;

	if (word == NULL || table == NULL)
		return NULL;

	if (table == lasts && ciprefix("last", word) && word[4]) {
		if (word[4] == '-') {
			warning(_("\"%s\" is undocumented; use \"last%s\" instead"),
				word, word + 5);
		} else {
			word += 4;
			table = wday_names;
		}
	}

	for (lp = table; lp->l_word != NULL; ++lp) {
		if (ciequal(word, lp->l_word))
			return lp;
	}

	foundlp = NULL;
	for (lp = table; lp->l_word != NULL; ++lp) {
		if (ciprefix(word, lp->l_word)) {
			if (foundlp == NULL) {
				foundlp = lp;
			} else {
				return NULL;
			}
		}
	}

	if (foundlp && noise) {
		bool pre_2017c_match = false;
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

	return foundlp;
}

static int
getfields(char *cp, char **array, int arrayelts)
{
	char *dp;
	int nsubs;

	if (!cp || !array || arrayelts <= 0)
		return 0;

	nsubs = 0;
	while (*cp) {
		char *dstart;
		
		while (is_space(*cp))
			++cp;
		if (*cp == '\0' || *cp == '#')
			break;
			
		dstart = dp = cp;
		
		while (*cp && *cp != '#' && !is_space(*cp)) {
			if (*cp != '"') {
				*dp++ = *cp++;
			} else {
				++cp;
				while (*cp && *cp != '"') {
					*dp++ = *cp++;
				}
				if (*cp == '"') {
					++cp;
				} else {
					error(_("Odd number of quotation marks"));
					exit(EXIT_FAILURE);
				}
			}
		}
		
		while (is_space(*cp))
			++cp;
		*dp = '\0';
		
		if (nsubs >= arrayelts) {
			error(_("Too many input fields"));
			exit(EXIT_FAILURE);
		}
		
		if (*dstart == '-' && *(dstart + 1) == '\0') {
			array[nsubs++] = dstart + 1;
		} else {
			array[nsubs++] = dstart;
		}
	}
	return nsubs;
}

ATTRIBUTE_NORETURN static void
time_overflow(void)
{
    (void)error(_("time overflow"));
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
	int m, i;
	zic_t dayoff;
	zic_t t, y;
	int yrem;

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
		i = len_years[isleap(y)];
		dayoff = oadd(dayoff, i);
		y++;
	}

	while (m != rp->r_month) {
		i = len_months[isleap(y)][m];
		dayoff = oadd(dayoff, i);
		++m;
	}

	i = rp->r_dayofmonth;
	if (m == TM_FEBRUARY && i == 29 && !isleap(y)) {
		if (rp->r_dycode == DC_DOWLEQ) {
			--i;
		} else {
			error(_("use of 2/29 in non leap-year"));
			exit(EXIT_FAILURE);
		}
	}

	--i;
	dayoff = oadd(dayoff, i);

	if (rp->r_dycode == DC_DOWGEQ || rp->r_dycode == DC_DOWLEQ) {
		zic_t wday = ((EPOCH_WDAY + dayoff % DAYSPERWEEK + DAYSPERWEEK)
			      % DAYSPERWEEK);
		while (wday != rp->r_wday) {
			if (rp->r_dycode == DC_DOWGEQ) {
				dayoff = oadd(dayoff, 1);
				if (++wday >= DAYSPERWEEK)
					wday = 0;
				++i;
			} else {
				dayoff = oadd(dayoff, -1);
				if (--wday < 0)
					wday = DAYSPERWEEK - 1;
				--i;
			}
		}
		if (i < 0 || i >= len_months[isleap(y)][m]) {
			if (noise)
				warning(_("rule goes past start/end of month; \
will not work with pre-2004 versions of zic"));
		}
	}

	if (dayoff < min_time / SECSPERDAY)
		return min_time;
	if (dayoff > max_time / SECSPERDAY)
		return max_time;

	t = (zic_t) dayoff * SECSPERDAY;
	return tadd(t, rp->r_tod);
}

static void
newabbr(const char *string)
{
	int i;
	
	if (strcmp(string, GRANDPARENTED) != 0) {
		const char *cp = string;
		const char *mp = NULL;
		
		while (is_alpha(*cp) || (*cp >= '0' && *cp <= '9') || *cp == '-' || *cp == '+') {
			++cp;
		}
		
		int len = cp - string;
		if (noise && len < 3) {
			mp = _("time zone abbreviation has fewer than 3 characters");
		}
		if (len > ZIC_MAX_ABBR_LEN_WO_WARN) {
			mp = _("time zone abbreviation has too many characters");
		}
		if (*cp != '\0') {
			mp = _("time zone abbreviation differs from POSIX standard");
		}
		if (mp != NULL) {
			warning("%s (%s)", mp, string);
		}
	}
	
	i = strlen(string) + 1;
	if (charcnt > TZ_MAX_CHARS - i) {
		error(_("too many, or too long, time zone abbreviations"));
		exit(EXIT_FAILURE);
	}
	strcpy(&chars[charcnt], string);
	charcnt += i;
}

/* Ensure that the directories of ARGNAME exist, by making any missing
   ones.  If ANCESTORS, do this only for ARGNAME's ancestors; otherwise,
   do it for ARGNAME too.  Exit with failure if there is trouble.
   Do not consider an existing file to be trouble.  */
static void
mkdirs(char const *argname, bool ancestors)
{
	if (!argname) {
		return;
	}

	char *name = estrdup(argname);
	if (!name) {
		return;
	}

	char *cp = name;

	while (*cp == '/') {
		cp++;
	}

	while (cp && ((cp = strchr(cp, '/')) || !ancestors)) {
		if (cp) {
			*cp = '\0';
		}

		if (mkdir(name, MKDIR_UMASK) != 0) {
			int err = errno;
			if (err == ELOOP || err == ENAMETOOLONG || err == ENOENT || err == ENOTDIR) {
				error(_("%s: Can't create directory %s: %s"), progname, name, strerror(err));
				free(name);
				exit(EXIT_FAILURE);
			}
		}

		if (cp) {
			*cp++ = '/';
		}
	}

	free(name);
}
