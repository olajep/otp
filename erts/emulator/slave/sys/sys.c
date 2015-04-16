/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2014. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd%
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#ifdef ISC32
#define _POSIX_SOURCE
#define _XOPEN_SOURCE
#endif

#include <sys/times.h>		/* ! */
#include <time.h>
#include <signal.h>
#include <sys/wait.h>
#include <ctype.h>

#ifdef ISC32
#include <sys/bsdtypes.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif

#define NEED_CHILD_SETUP_DEFINES
#define ERTS_WANT_BREAK_HANDLING
#define ERTS_WANT_GOT_SIGUSR1
#define WANT_NONBLOCKING    /* must define this to pull in defs from sys.h */
#include "sys.h"
#include "erl_thr_progress.h"

#if defined(__APPLE__) && defined(__MACH__) && !defined(__DARWIN__)
#define __DARWIN__ 1
#endif

#ifdef USE_THREADS
#include "erl_threads.h"
#endif

#include "erl_mseg.h"

extern char **environ;
static erts_smp_rwmtx_t environ_rwmtx;

#define MAX_VSIZE 16		/* Max number of entries allowed in an I/O
				 * vector sock_sendv().
				 */
/*
 * Don't need global.h, but bif_table.h (included by bif.h),
 * won't compile otherwise
 */
#include "global.h" 
#include "bif.h"

#include "erl_sys_driver.h"
#include "erl_check_io.h"
#include "erl_cpu_topology.h"

#ifndef DISABLE_VFORK
#define DISABLE_VFORK 0
#endif

#ifdef USE_THREADS
#  ifdef ENABLE_CHILD_WAITER_THREAD
#    define CHLDWTHR ENABLE_CHILD_WAITER_THREAD
#  else
#    define CHLDWTHR 0
#  endif
#else
#  define CHLDWTHR 0
#endif
/*
 * [OTP-3906]
 * Solaris signal management gets confused when threads are used and a
 * lot of child processes dies. The confusion results in that SIGCHLD
 * signals aren't delivered to the emulator which in turn results in
 * a lot of defunct processes in the system.
 *
 * The problem seems to appear when a signal is frequently
 * blocked/unblocked at the same time as the signal is frequently
 * propagated. The child waiter thread is a workaround for this problem.
 * The SIGCHLD signal is always blocked (in all threads), and the child
 * waiter thread fetches the signal by a call to sigwait(). See
 * child_waiter().
 */

typedef struct ErtsSysReportExit_ ErtsSysReportExit;
struct ErtsSysReportExit_ {
    ErtsSysReportExit *next;
    Eterm port;
    int pid;
    int ifd;
    int ofd;
#if CHLDWTHR && !defined(ERTS_SMP)
    int status;
#endif
};

#if CHLDWTHR && !defined(ERTS_SMP)
static ErtsSysReportExit *report_exit_transit_list;
#endif

extern int  driver_interrupt(int, int);
extern void do_break(void);

extern void erl_sys_args(int*, char**);

/* The following two defs should probably be moved somewhere else */

extern void erts_sys_init_float(void);

extern void erl_crash_dump(char* file, int line, char* fmt, ...);

#define DIR_SEPARATOR_CHAR    '/'

#if defined(__ANDROID__)
#define SHELL "/system/bin/sh"
#else
#define SHELL "/bin/sh"
#endif /* __ANDROID__ */


#if defined(DEBUG)
#define ERL_BUILD_TYPE_MARKER ".debug"
#elif defined(PURIFY)
#define ERL_BUILD_TYPE_MARKER ".purify"
#elif defined(QUANTIFY)
#define ERL_BUILD_TYPE_MARKER ".quantify"
#elif defined(PURECOV)
#define ERL_BUILD_TYPE_MARKER ".purecov"
#elif defined(VALGRIND)
#define ERL_BUILD_TYPE_MARKER ".valgrind"
#else /* opt */
#define ERL_BUILD_TYPE_MARKER
#endif

#ifdef DEBUG
static int debug_log = 0;
#endif

#ifdef ERTS_SMP
erts_smp_atomic32_t erts_got_sigusr1;
#define ERTS_SET_GOT_SIGUSR1 \
  erts_smp_atomic32_set_mb(&erts_got_sigusr1, 1)
#define ERTS_UNSET_GOT_SIGUSR1 \
  erts_smp_atomic32_set_mb(&erts_got_sigusr1, 0)
static erts_smp_atomic32_t have_prepared_crash_dump;
#define ERTS_PREPARED_CRASH_DUMP \
  ((int) erts_smp_atomic32_xchg_nob(&have_prepared_crash_dump, 1))
#else
volatile int erts_got_sigusr1;
#define ERTS_SET_GOT_SIGUSR1 (erts_got_sigusr1 = 1)
#define ERTS_UNSET_GOT_SIGUSR1 (erts_got_sigusr1 = 0)
static volatile int have_prepared_crash_dump;
#define ERTS_PREPARED_CRASH_DUMP \
  (have_prepared_crash_dump++)
#endif

static erts_smp_atomic_t sys_misc_mem_sz;

#if defined(ERTS_SMP)
static void smp_sig_notify(char c);
static int sig_notify_fds[2] = {-1, -1};
#endif

#if CHLDWTHR || defined(ERTS_SMP)
erts_mtx_t chld_stat_mtx;
#endif
#if CHLDWTHR
static erts_tid_t child_waiter_tid;
/* chld_stat_mtx is used to protect against concurrent accesses
   of the driver_data fields pid, alive, and status. */
erts_cnd_t chld_stat_cnd;
static long children_alive;
#define CHLD_STAT_LOCK		erts_mtx_lock(&chld_stat_mtx)
#define CHLD_STAT_UNLOCK	erts_mtx_unlock(&chld_stat_mtx)
#define CHLD_STAT_WAIT		erts_cnd_wait(&chld_stat_cnd, &chld_stat_mtx)
#define CHLD_STAT_SIGNAL	erts_cnd_signal(&chld_stat_cnd)
#elif defined(ERTS_SMP) /* ------------------------------------------------- */
#define CHLD_STAT_LOCK		erts_mtx_lock(&chld_stat_mtx)
#define CHLD_STAT_UNLOCK	erts_mtx_unlock(&chld_stat_mtx)

#else /* ------------------------------------------------------------------- */
#define CHLD_STAT_LOCK
#define CHLD_STAT_UNLOCK
static volatile int children_died;
#endif


static struct fd_data {
    char  pbuf[4];   /* hold partial packet bytes */
    int   psz;       /* size of pbuf */
    char  *buf;
    char  *cpos;
    int   sz;
    int   remain;  /* for input on fd */
} *fd_data;			/* indexed by fd */

#if CHLDWTHR
static void* child_waiter(void *);
#endif

/********************* General functions ****************************/

/* This is used by both the drivers and general I/O, must be set early */
static int max_files = -1;

/* 
 * a few variables used by the break handler 
 */
#ifdef ERTS_SMP
erts_smp_atomic32_t erts_break_requested;
#define ERTS_SET_BREAK_REQUESTED \
  erts_smp_atomic32_set_nob(&erts_break_requested, (erts_aint32_t) 1)
#define ERTS_UNSET_BREAK_REQUESTED \
  erts_smp_atomic32_set_nob(&erts_break_requested, (erts_aint32_t) 0)
#else
volatile int erts_break_requested = 0;
#define ERTS_SET_BREAK_REQUESTED (erts_break_requested = 1)
#define ERTS_UNSET_BREAK_REQUESTED (erts_break_requested = 0)
#endif
/* set early so the break handler has access to initial mode */
// static struct termios initial_tty_mode; // DELETE ME
/* assume yes initially, ttsl_init will clear it */
int using_oldshell = 1; 

void
erts_sys_schedule_interrupt(int set)
{
    EPIPHANY_STUB_FUN();
}

#ifdef ERTS_SMP
void
erts_sys_schedule_interrupt_timed(int set, erts_short_time_t msec)
{
    EPIPHANY_STUB_FUN();
}
#endif

Uint
erts_sys_misc_mem_sz(void)
{
    // ESTUB
    return 0;
}

/*
 * reset the terminal to the original settings on exit
 */
void sys_tty_reset(int exit_code)
{
    /* No TTY on the Epiphany */
}

#ifdef __tile__
/* Direct malloc to spread memory around the caches of multiple tiles. */
#include <malloc.h>
#if defined(MALLOC_USE_HASH)
MALLOC_USE_HASH(1);
#endif
#endif

#ifdef USE_THREADS

#ifdef ERTS_THR_HAVE_SIG_FUNCS
/*
 * Child thread inherits parents signal mask at creation. In order to
 * guarantee that the main thread will receive all SIGINT, SIGCHLD, and
 * SIGUSR1 signals sent to the process, we block these signals in the
 * parent thread when creating a new thread.
 */

static sigset_t thr_create_sigmask;

#endif /* #ifdef ERTS_THR_HAVE_SIG_FUNCS */

typedef struct {
#ifdef ERTS_THR_HAVE_SIG_FUNCS
    sigset_t saved_sigmask;
#endif
    int sched_bind_data;
} erts_thr_create_data_t;

/*
 * thr_create_prepare() is called in parent thread before thread creation.
 * Returned value is passed as argument to thr_create_cleanup().
 */
static void *
thr_create_prepare(void)
{
    EPIPHANY_STUB_FUN();
}


/* thr_create_cleanup() is called in parent thread after thread creation. */
static void
thr_create_cleanup(void *vtcdp)
{
    EPIPHANY_STUB_FUN();
}

static void
thr_create_prepare_child(void *vtcdp)
{
    EPIPHANY_STUB_FUN();
}

#endif /* #ifdef USE_THREADS */

void
erts_sys_pre_init(void)
{
    erts_printf_add_cr_to_stdout = 1;
    erts_printf_add_cr_to_stderr = 1;
#ifdef USE_THREADS
    {
    erts_thr_init_data_t eid = ERTS_THR_INIT_DATA_DEF_INITER;

    eid.thread_create_child_func = thr_create_prepare_child;
    /* Before creation in parent */
    eid.thread_create_prepare_func = thr_create_prepare;
    /* After creation in parent */
    eid.thread_create_parent_func = thr_create_cleanup,

#ifdef ERTS_THR_HAVE_SIG_FUNCS
    sigemptyset(&thr_create_sigmask);
    sigaddset(&thr_create_sigmask, SIGINT);   /* block interrupt */
    sigaddset(&thr_create_sigmask, SIGCHLD);  /* block child signals */
    sigaddset(&thr_create_sigmask, SIGUSR1);  /* block user defined signal */
#endif

    erts_thr_init(&eid);

#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_init();
#endif

#if CHLDWTHR || defined(ERTS_SMP)
    erts_mtx_init(&chld_stat_mtx, "child_status");
#endif
#if CHLDWTHR
#ifndef ERTS_SMP
    report_exit_transit_list = NULL;
#endif
    erts_cnd_init(&chld_stat_cnd);
    children_alive = 0;
#endif
    }
#ifdef ERTS_SMP
    erts_smp_atomic32_init_nob(&erts_break_requested, 0);
    erts_smp_atomic32_init_nob(&erts_got_sigusr1, 0);
    erts_smp_atomic32_init_nob(&have_prepared_crash_dump, 0);
#else
    erts_break_requested = 0;
    erts_got_sigusr1 = 0;
    have_prepared_crash_dump = 0;
#endif
#if !CHLDWTHR && !defined(ERTS_SMP)
    children_died = 0;
#endif
#endif /* USE_THREADS */
    erts_smp_atomic_init_nob(&sys_misc_mem_sz, 0);
}

void
erl_sys_init(void)
{
#ifdef USE_SETLINEBUF
    setlinebuf(stdout);
#else
    setvbuf(stdout, (char *)NULL, _IOLBF, BUFSIZ);
#endif

    erts_sys_init_float();

    tzset(); /* Required at least for NetBSD with localtime_r() */
}

/* signal handling */

RETSIGTYPE (*sys_sigset(int sig, RETSIGTYPE (*func)(int)))(int)
{
    return signal(sig, func);
}

#ifdef USE_THREADS
#undef  sigprocmask
#define sigprocmask erts_thr_sigmask
#endif

void sys_sigblock(int sig)
{
    EPIPHANY_STUB_FUN();
}

void sys_sigrelease(int sig)
{
    EPIPHANY_STUB_FUN();
}

#if (0) /* not used? -- gordon */
static void (*break_func)();
static RETSIGTYPE break_handler(int sig)
{
#ifdef QNX
    /* Turn off SIGCHLD during break processing */
    sys_sigblock(SIGCHLD);
#endif
    (*break_func)();
#ifdef QNX
    sys_sigrelease(SIGCHLD);
#endif
}
#endif /* 0 */

static ERTS_INLINE int
prepare_crash_dump(int secs)
{
#define NUFBUF (3)
    int has_heart = 0;

    UseTmpHeapNoproc(NUFBUF);

    if (ERTS_PREPARED_CRASH_DUMP)
	return 0; /* We have already been called */

    UnUseTmpHeapNoproc(NUFBUF);
#undef NUFBUF
    return has_heart;
}

int erts_sys_prepare_crash_dump(int secs)
{
    return prepare_crash_dump(secs);
}

/* set up signal handlers for break and quit */
#if (defined(SIG_SIGSET) || defined(SIG_SIGNAL))
static RETSIGTYPE request_break(void)
#else
static RETSIGTYPE request_break(int signum)
#endif
{
#ifdef ERTS_SMP
    smp_sig_notify('I');
#else
    break_requested();
#endif
}

#ifdef ETHR_UNUSABLE_SIGUSRX
#warning "Unusable SIGUSR1 & SIGUSR2. Disabling use of these signals"
#endif

#ifndef ETHR_UNUSABLE_SIGUSRX

#if (defined(SIG_SIGSET) || defined(SIG_SIGNAL))
static RETSIGTYPE user_signal1(void)
#else
static RETSIGTYPE user_signal1(int signum)
#endif
{
#ifdef ERTS_SMP
   smp_sig_notify('1');
#else
   sigusr1_exit();
#endif
}

#ifdef QUANTIFY
#if (defined(SIG_SIGSET) || defined(SIG_SIGNAL))
static RETSIGTYPE user_signal2(void)
#else
static RETSIGTYPE user_signal2(int signum)
#endif
{
#ifdef ERTS_SMP
   smp_sig_notify('2');
#else
   quantify_save_data();
#endif
}
#endif

#endif /* #ifndef ETHR_UNUSABLE_SIGUSRX */

#if (defined(SIG_SIGSET) || defined(SIG_SIGNAL))
static RETSIGTYPE do_quit(void)
#else
static RETSIGTYPE do_quit(int signum)
#endif
{
#ifdef ERTS_SMP
    smp_sig_notify('Q');
#else
    quit_requested();
#endif
}

/* Disable break */
void erts_set_ignore_break(void) {
    sys_sigset(SIGINT,  SIG_IGN);
    sys_sigset(SIGQUIT, SIG_IGN);
    sys_sigset(SIGTSTP, SIG_IGN);
}

/* Don't use ctrl-c for break handler but let it be 
   used by the shell instead (see user_drv.erl) */
void erts_replace_intr(void) {
    /* No terminal on the epiphany */
}

void init_break_handler(void)
{
   sys_sigset(SIGINT, request_break);
#ifndef ETHR_UNUSABLE_SIGUSRX
   sys_sigset(SIGUSR1, user_signal1);
#ifdef QUANTIFY
   sys_sigset(SIGUSR2, user_signal2);
#endif
#endif /* #ifndef ETHR_UNUSABLE_SIGUSRX */
   sys_sigset(SIGQUIT, do_quit);
}

int sys_max_files(void)
{
   return(max_files);
}

/************************** Time stuff **************************/
#ifdef HAVE_GETHRTIME
#ifdef GETHRTIME_WITH_CLOCK_GETTIME

SysHrTime sys_gethrtime(void)
{
    struct timespec ts;
    long long result;
    if (clock_gettime(CLOCK_MONOTONIC,&ts) != 0) {
	erl_exit(1,"Fatal, could not get clock_monotonic value!, "
		 "errno = %d\n", errno);
    }
    result = ((long long) ts.tv_sec) * 1000000000LL + 
	((long long) ts.tv_nsec);
    return (SysHrTime) result;
}
#endif
#endif

/************************** OS info *******************************/

/* Used by erlang:info/1. */
/* (This code was formerly in drv.XXX/XXX_os_drv.c) */

char os_type[] = "unix";

void
os_flavor(char* namebuf, 	/* Where to return the name. */
	  unsigned size) 	/* Size of name buffer. */
{
    strncpy(namebuf, "baremetal", size);
    namebuf[size-1] = 0;
}

void
os_version(pMajor, pMinor, pBuild)
int* pMajor;			/* Pointer to major version. */
int* pMinor;			/* Pointer to minor version. */
int* pBuild;			/* Pointer to build number. */
{
    *pMajor = 0;
    *pMinor = 0;
    *pBuild = 0;
}


/************************** Port I/O *******************************/



/* I. Common stuff */

/*
 * Decreasing the size of it below 16384 is not allowed.
 */

/* II. The spawn/fd/vanilla drivers */

#define ERTS_SYS_READ_BUF_SZ (64*1024)

#ifdef QNX
static reset_qnx_spawn()
{
    int i;

    /* Reset qnx_spawn_options */
    qnx_spawn_options.flags = 0; 
    qnx_spawn_options.iov[0] = 0xff;
    qnx_spawn_options.iov[1] = 0xff;
    qnx_spawn_options.iov[2] = 0xff;
    qnx_spawn_options.iov[3] = 0xff;
}
#endif

#define FD_DEF_HEIGHT 24
#define FD_DEF_WIDTH 80
/* Control op */
#define FD_CTRL_OP_GET_WINSIZE 100

void erts_do_break_handling(void)
{
    EPIPHANY_STUB_FUN();
}

/* Fills in the systems representation of the jam/beam process identifier.
** The Pid is put in STRING representation in the supplied buffer,
** no interpretatione of this should be done by the rest of the
** emulator. The buffer should be at least 21 bytes long.
*/
void sys_get_pid(char *buffer, size_t buffer_size){
    pid_t p = getpid();
    /* Assume the pid is scalar and can rest in an unsigned long... */
    erts_snprintf(buffer, buffer_size, "%lu",(unsigned long) p);
}

int
erts_sys_putenv_raw(char *key, char *value) {
    return erts_sys_putenv(key, value);
}
int
erts_sys_putenv(char *key, char *value)
{
    int res;
    char *env;
    Uint need = strlen(key) + strlen(value) + 2;

#ifdef HAVE_COPYING_PUTENV
    env = erts_alloc(ERTS_ALC_T_TMP, need);
#else
    env = erts_alloc(ERTS_ALC_T_PUTENV_STR, need);
    erts_smp_atomic_add_nob(&sys_misc_mem_sz, need);
#endif
    strcpy(env,key);
    strcat(env,"=");
    strcat(env,value);
    erts_smp_rwmtx_rwlock(&environ_rwmtx);
    res = putenv(env);
    erts_smp_rwmtx_rwunlock(&environ_rwmtx);
#ifdef HAVE_COPYING_PUTENV
    erts_free(ERTS_ALC_T_TMP, env);
#endif
    return res;
}

int
erts_sys_getenv__(char *key, char *value, size_t *size)
{
    int res;
    char *orig_value = getenv(key);
    if (!orig_value)
	res = -1;
    else {
	size_t len = sys_strlen(orig_value);
	if (len >= *size) {
	    *size = len + 1;
	    res = 1;
	}
	else {
	    *size = len;
	    sys_memcpy((void *) value, (void *) orig_value, len+1);
	    res = 0;
	}
    }
    return res;
}

int
erts_sys_getenv_raw(char *key, char *value, size_t *size) {
    return erts_sys_getenv(key, value, size);
}

/*
 * erts_sys_getenv
 * returns:
 *  -1, if environment key is not set with a value
 *   0, if environment key is set and value fits into buffer size
 *   1, if environment key is set but does not fit into buffer size
 *      size is set with the needed buffer size value
 */

int
erts_sys_getenv(char *key, char *value, size_t *size)
{
    int res;
    erts_smp_rwmtx_rlock(&environ_rwmtx);
    res = erts_sys_getenv__(key, value, size);
    erts_smp_rwmtx_runlock(&environ_rwmtx);
    return res;
}

int
erts_sys_unsetenv(char *key)
{
    int res;
    erts_smp_rwmtx_rwlock(&environ_rwmtx);
    res = unsetenv(key);
    erts_smp_rwmtx_rwunlock(&environ_rwmtx);
    return res;
}

void
sys_init_io(void)
{
    fd_data = (struct fd_data *)
	erts_alloc(ERTS_ALC_T_FD_TAB, max_files * sizeof(struct fd_data));
    erts_smp_atomic_add_nob(&sys_misc_mem_sz,
			    max_files * sizeof(struct fd_data));
}

#if (0) /* unused? */
static int write_fill(fd, buf, len)
int fd, len;
char *buf;
{
    int i, done = 0;
    
    do {
	if ((i = write(fd, buf+done, len-done)) < 0) {
	    if (errno != EINTR)
		return (i);
	    i = 0;
	}
	done += i;
    } while (done < len);
    return (len);
}
#endif

extern const char pre_loaded_code[];
extern Preload pre_loaded[];

void erts_sys_alloc_init(void)
{
}

#if ERTS_HAVE_ERTS_SYS_ALIGNED_ALLOC
void *erts_sys_aligned_alloc(UWord alignment, UWord size)
{
#ifdef HAVE_POSIX_MEMALIGN
    void *ptr = NULL;
    int error;
    ASSERT(alignment && (alignment & (alignment-1)) == 0); /* power of 2 */
    error = posix_memalign(&ptr, (size_t) alignment, (size_t) size);
#if HAVE_ERTS_MSEG
    if (error || !ptr) {
	erts_mseg_clear_cache();
	error = posix_memalign(&ptr, (size_t) alignment, (size_t) size);
    }
#endif
    if (error) {
	errno = error;
	return NULL;
    }
    if (!ptr)
	errno = ENOMEM;
    ASSERT(!ptr || (((UWord) ptr) & (alignment - 1)) == 0);
    return ptr;
#else
#  error "Missing erts_sys_aligned_alloc() implementation"
#endif
}

void erts_sys_aligned_free(UWord alignment, void *ptr)
{
    ASSERT(alignment && (alignment & (alignment-1)) == 0); /* power of 2 */
    free(ptr);
}

void *erts_sys_aligned_realloc(UWord alignment, void *ptr, UWord size, UWord old_size)
{
    void *new_ptr = erts_sys_aligned_alloc(alignment, size);
    if (new_ptr) {
	UWord copy_size = old_size < size ? old_size : size;
	sys_memcpy(new_ptr, ptr, (size_t) copy_size);
	erts_sys_aligned_free(alignment, ptr);
    }
    return new_ptr;
}

#endif

void *erts_sys_alloc(ErtsAlcType_t t, void *x, Uint sz)
{
    void *res = malloc((size_t) sz);
#if HAVE_ERTS_MSEG
    if (!res) {
	erts_mseg_clear_cache();
	return malloc((size_t) sz);
    }
#endif
    return res;
}

void *erts_sys_realloc(ErtsAlcType_t t, void *x, void *p, Uint sz)
{
    void *res = realloc(p, (size_t) sz);
#if HAVE_ERTS_MSEG
    if (!res) {
	erts_mseg_clear_cache();
	return realloc(p, (size_t) sz);
    }
#endif
    return res;
}

void erts_sys_free(ErtsAlcType_t t, void *x, void *p)
{
    free(p);
}

/* Return a pointer to a vector of names of preloaded modules */

Preload*
sys_preloaded(void)
{
    return pre_loaded;
}

/* Return a pointer to preloaded code for module "module" */
unsigned char*
sys_preload_begin(Preload* p)
{
    return p->code;
}

/* Clean up if allocated */
void sys_preload_end(Preload* p)
{
    /* Nothing */
}

/* Read a key from console (?) */

int sys_get_key(fd)
int fd;
{
    int c;
    unsigned char rbuf[64];

    fflush(stdout);		/* Flush query ??? */

    if ((c = read(fd,rbuf,64)) <= 0) {
      return c; 
    }

    return rbuf[0]; 
}


extern int erts_initialized;
void
erl_assert_error(const char* expr, const char* func, const char* file, int line)
{   
    fflush(stdout);
    fprintf(stderr, "%s:%d:%s() Assertion failed: %s\n",
            file, line, func, expr);
    fflush(stderr);
#if !defined(ERTS_SMP) && 0
    /* Writing a crashdump from a failed assertion when smp support
     * is enabled almost a guaranteed deadlocking, don't even bother.
     *
     * It could maybe be useful (but I'm not convinced) to write the
     * crashdump if smp support is disabled...
     */
    if (erts_initialized)
	erl_crash_dump(file, line, "Assertion failed: %s\n", expr);
#endif
    abort();
}

#ifdef DEBUG

void
erl_debug(char* fmt, ...)
{
    /* Can't be larger than 972 bytes or the Epiphany codegen crashes. */
    char sbuf[972];		/* Temporary buffer. */
    va_list va;
    
    if (debug_log) {
	va_start(va, fmt);
	vsprintf(sbuf, fmt, va);
	va_end(va);
	fprintf(stderr, "%s", sbuf);
    }
}

#endif /* DEBUG */

#if CHLDWTHR

static void *
child_waiter(void *unused)
{
  int pid;
  int status;

#ifdef ERTS_ENABLE_LOCK_CHECK
  erts_lc_set_thread_name("child waiter");
#endif

  while(1) {
#ifdef DEBUG
      int waitpid_errno;
#endif
      pid = waitpid(-1, &status, 0);
#ifdef DEBUG
      waitpid_errno = errno;
#endif
      CHLD_STAT_LOCK;
      if (pid < 0) {
	  ASSERT(waitpid_errno == ECHILD);
      }
      else {
	  children_alive--;
	  ASSERT(children_alive >= 0);
	  note_child_death(pid, status);
      }
      while (!children_alive)
	  CHLD_STAT_WAIT; /* Wait for children to wait on... :) */
      CHLD_STAT_UNLOCK;
  }

  return NULL;
}

#endif

/*
 * Called from schedule() when it runs out of runnable processes,
 * or when Erlang code has performed INPUT_REDUCTIONS reduction
 * steps. runnable == 0 iff there are no runnable Erlang processes.
 */
void
erl_sys_schedule(int runnable)
{
    // ETODO poll IO
    ERTS_SMP_LC_ASSERT(!erts_thr_progress_is_blocking());
}


#ifdef ERTS_SMP

static void
smp_sig_notify(char c)
{
    int res;
    do {
	/* write() is async-signal safe (according to posix) */
	res = write(sig_notify_fds[1], &c, 1);
    } while (res < 0 && errno == EINTR);
    if (res != 1) {
	char msg[] =
	    "smp_sig_notify(): Failed to notify signal-dispatcher thread "
	    "about received signal";
	erts_silence_warn_unused_result(write(2, msg, sizeof(msg)));
	abort();
    }
}

#endif /* ERTS_SMP */

#ifdef ERTS_ENABLE_KERNEL_POLL /* get_value() is currently only used when
				  kernel-poll is enabled */

/* Get arg marks argument as handled by
   putting NULL in argv */
static char *
get_value(char* rest, char** argv, int* ip)
{
    char *param = argv[*ip]+1;
    argv[*ip] = NULL;
    if (*rest == '\0') {
	char *next = argv[*ip + 1];
	if (next[0] == '-'
	    && next[1] == '-'
	    &&  next[2] == '\0') {
	    erts_fprintf(stderr, "bad \"%s\" value: \n", param);
	    erts_usage();
	}
	(*ip)++;
	argv[*ip] = NULL;
	return next;
    }
    return rest;
}

#endif /* ERTS_ENABLE_KERNEL_POLL */

void
erl_sys_args(int* argc, char** argv)
{
    int i, j;

    erts_smp_rwmtx_init(&environ_rwmtx, "environ");

    i = 1;

    ASSERT(argc && (*argc == 0 || argv));

    while (i < *argc) {
	if(argv[i][0] == '-') {
	    switch (argv[i][1]) {
#ifdef ERTS_ENABLE_KERNEL_POLL
	    case 'K': {
		char *arg = get_value(argv[i] + 2, argv, &i);
		if (strcmp("true", arg) == 0) {
		    erts_use_kernel_poll = 1;
		}
		else if (strcmp("false", arg) == 0) {
		    erts_use_kernel_poll = 0;
		}
		else {
		    erts_fprintf(stderr, "bad \"K\" value: %s\n", arg);
		    erts_usage();
		}
		break;
	    }
#endif
	    case '-':
		goto done_parsing;
	    default:
		break;
	    }
	}
	i++;
    }

 done_parsing:
    // ETODO: init io

    /* Handled arguments have been marked with NULL. Slide arguments
       not handled towards the beginning of argv. */
    for (i = 0, j = 0; i < *argc; i++) {
	if (argv[i])
	    argv[j++] = argv[i];
    }
    *argc = j;

}

// We provide sysconf if it's not available
long __attribute__((weak)) sysconf(int __attribute__((unused)) name)
{
    return -1;
}

void sys_epiphany_stub(const char* name)
{
    erts_fprintf(stderr, "%s is a stub!\n", name);
    asm("idle");
    erl_exit(1, "%s is a stub!\n", name);
}
