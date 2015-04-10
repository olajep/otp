/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1997-2013. All Rights Reserved.
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

#include "sys.h"
#include <ctype.h>
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "erl_version.h"
#include "erl_db.h"
#include "beam_bp.h"
#include "erl_bits.h"
#include "erl_binary.h"
#include "dist.h"
#include "erl_mseg.h"
#include "erl_threads.h"
#include "erl_bif_timer.h"
#include "erl_instrument.h"
#include "erl_printf_term.h"
#include "erl_misc_utils.h"
#include "packet_parser.h"
#include "erl_cpu_topology.h"
#include "erl_thr_progress.h"
#include "erl_thr_queue.h"
#include "erl_async.h"
#include "erl_ptab.h"

#ifdef HIPE
#include "hipe_mode_switch.h"	/* for hipe_mode_switch_init() */
#include "hipe_signal.h"	/* for hipe_signal_init() */
#endif

#ifdef HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
#endif

#define ERTS_DEFAULT_NO_ASYNC_THREADS	10

/*
 * The variables below (prefixed with etp_) are for erts/etc/unix/etp-commands
 * only. Do not remove even though they aren't used elsewhere in the emulator!
 */
#ifdef ERTS_SMP
const int etp_smp_compiled = 1;
#else
const int etp_smp_compiled = 0;
#endif
#ifdef USE_THREADS
const int etp_thread_compiled = 1;
#else
const int etp_thread_compiled = 0;
#endif
const char etp_erts_version[] = ERLANG_VERSION;
const char etp_otp_release[] = ERLANG_OTP_RELEASE;
const char etp_compile_date[] = ERLANG_COMPILE_DATE;
const char etp_arch[] = ERLANG_ARCHITECTURE;
#ifdef ERTS_ENABLE_KERNEL_POLL
const int etp_kernel_poll_support = 1;
#else
const int etp_kernel_poll_support = 0;
#endif
#if defined(ARCH_64)
const int etp_arch_bits = 64;
#elif defined(ARCH_32)
const int etp_arch_bits = 32;
#else
# error "Not 64-bit, nor 32-bit arch"
#endif
#if HALFWORD_HEAP
const int etp_halfword = 1;
#else
const int etp_halfword = 0;
#endif
#ifdef HIPE
const int etp_hipe = 1;
#else
const int etp_hipe = 0;
#endif
#ifdef DEBUG
const int etp_debug_compiled = 1;
#else
const int etp_debug_compiled = 0;
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
const int etp_lock_count = 1;
#else
const int etp_lock_count = 0;
#endif
#ifdef ERTS_ENABLE_LOCK_CHECK
const int etp_lock_check = 1;
#else
const int etp_lock_check = 0;
#endif
#ifdef WORDS_BIGENDIAN
const int etp_big_endian = 1;
#else
const int etp_big_endian = 0;
#endif
/*
 * Note about VxWorks: All variables must be initialized by executable code,
 * not by an initializer. Otherwise a new instance of the emulator will
 * inherit previous values.
 */

extern void erl_crash_dump_v(char *, int, char *, va_list);
#ifdef __WIN32__
extern void ConNormalExit(void);
extern void ConWaitForExit(void);
#endif

static void erl_init(int ncpu,
		     int proc_tab_sz,
		     int legacy_proc_tab,
		     int port_tab_sz,
		     int port_tab_sz_ignore_files,
		     int legacy_port_tab);

static erts_atomic_t exiting;

#ifdef ERTS_SMP
erts_smp_atomic32_t erts_writing_erl_crash_dump;
erts_tsd_key_t erts_is_crash_dumping_key;
#else
volatile int erts_writing_erl_crash_dump = 0;
#endif
int erts_initialized = 0;

#if defined(USE_THREADS) && !defined(ERTS_SMP)
static erts_tid_t main_thread;
#endif

int erts_use_sender_punish;

/*
 * Configurable parameters.
 */

Uint display_items;	    	/* no of items to display in traces etc */
int H_MIN_SIZE;			/* The minimum heap grain */
int BIN_VH_MIN_SIZE;		/* The minimum binary virtual*/

Uint32 erts_debug_flags;	/* Debug flags. */
#ifdef ERTS_OPCODE_COUNTER_SUPPORT
int count_instructions;
#endif
int erts_backtrace_depth;	/* How many functions to show in a backtrace
				 * in error codes.
				 */

erts_smp_atomic32_t erts_max_gen_gcs;

Eterm erts_error_logger_warnings; /* What to map warning logs to, am_error, 
				     am_info or am_warning, am_error is 
				     the default for BC */

int erts_compat_rel;

static int no_schedulers;
static int no_schedulers_online;
#ifdef ERTS_DIRTY_SCHEDULERS
static int no_dirty_cpu_schedulers;
static int no_dirty_cpu_schedulers_online;
static int no_dirty_io_schedulers;
#endif

#ifdef DEBUG
Uint32 verbose;             /* See erl_debug.h for information about verbose */
#endif

int erts_disable_tolerant_timeofday; /* Time correction can be disabled it is
				      * not and/or it is too slow.
				      */

int erts_atom_table_size = ATOM_LIMIT;	/* Maximum number of atoms */

int erts_modified_timing_level;

int erts_no_crash_dump = 0;	/* Use -d to suppress crash dump. */

int erts_no_line_info = 0;	/* -L: Don't load line information */

/*
 * Other global variables.
 */

ErtsModifiedTimings erts_modified_timings[] = {
    /* 0 */	{make_small(0), CONTEXT_REDS, INPUT_REDUCTIONS},
    /* 1 */	{make_small(0), 2*CONTEXT_REDS, 2*INPUT_REDUCTIONS},
    /* 2 */	{make_small(0), CONTEXT_REDS/2, INPUT_REDUCTIONS/2},
    /* 3 */	{make_small(0), 3*CONTEXT_REDS, 3*INPUT_REDUCTIONS},
    /* 4 */	{make_small(0), CONTEXT_REDS/3, 3*INPUT_REDUCTIONS},
    /* 5 */	{make_small(0), 4*CONTEXT_REDS, INPUT_REDUCTIONS/2},
    /* 6 */	{make_small(1), CONTEXT_REDS/4, 2*INPUT_REDUCTIONS},
    /* 7 */	{make_small(1), 5*CONTEXT_REDS, INPUT_REDUCTIONS/3},
    /* 8 */	{make_small(10), CONTEXT_REDS/5, 3*INPUT_REDUCTIONS},
    /* 9 */	{make_small(10), 6*CONTEXT_REDS, INPUT_REDUCTIONS/4}
};

#define ERTS_MODIFIED_TIMING_LEVELS \
  (sizeof(erts_modified_timings)/sizeof(ErtsModifiedTimings))

Export *erts_delay_trap = NULL;

int ignore_break;
int replace_intr;

static int
this_rel_num(void)
{
    static int this_rel = -1;

    if (this_rel < 1) {
	int i;
	char this_rel_str[] = ERLANG_OTP_RELEASE;
	    
	i = 0;
	while (this_rel_str[i] && !isdigit((int) this_rel_str[i]))
	    i++;
	this_rel = atoi(&this_rel_str[i]); 
	if (this_rel < 1)
	    erl_exit(-1, "Unexpected ERLANG_OTP_RELEASE format\n");
    }
    return this_rel;
}

/*
 * Common error printout function, all error messages
 * that don't go to the error logger go through here.
 */

void erl_error(char *fmt, va_list args)
{
    erts_vfprintf(stderr, fmt, args);
}

static int early_init(int *argc, char **argv);

void
erts_short_init(void)
{
    int ncpu = early_init(NULL, NULL);
    erl_init(ncpu,
	     ERTS_DEFAULT_MAX_PROCESSES,
	     0,
	     ERTS_DEFAULT_MAX_PORTS,
	     0,
	     0);
    erts_initialized = 1;
}

static void
erl_init(int ncpu,
	 int proc_tab_sz,
	 int legacy_proc_tab,
	 int port_tab_sz,
	 int port_tab_sz_ignore_files,
	 int legacy_port_tab)
{
    // ESTUB: Make sure we init what we need in the slave
    
/*     init_benchmarking(); */

/*     erts_init_monitors(); */
/*     erts_init_time(); */
/*     erts_init_sys_common_misc(); */
/*     erts_init_process(ncpu, proc_tab_sz, legacy_proc_tab); */
/*     erts_init_scheduling(no_schedulers, */
/* 			 no_schedulers_online */
/* #ifdef ERTS_DIRTY_SCHEDULERS */
/* 			 , no_dirty_cpu_schedulers, */
/* 			 no_dirty_cpu_schedulers_online, */
/* 			 no_dirty_io_schedulers */
/* #endif */
/* 			 ); */
    /* erts_init_cpu_topology(); /\* Must be after init_scheduling *\/ */
    /* erts_init_gc(); /\* Must be after init_scheduling *\/ */
    // erts_alloc_late_init();

    // ESTUB: erl_gc
    // H_MIN_SIZE      = erts_next_heap_size(H_MIN_SIZE, 0);
    // BIN_VH_MIN_SIZE = erts_next_heap_size(BIN_VH_MIN_SIZE, 0);

    // erts_init_trace();
    // erts_init_bits();
    // erts_code_ix_init();
    // erts_init_fun_table(); // ESTUB: erl_fun
    // init_atom_table();
    // init_export_table(); // ESTUB: export
    // init_module_table(); // ESTUB: module
    // init_register_table(); // ESTUB: register
    // init_message(); // ESTUB: erl_message
    // erts_bif_info_init(); // ESTUB: erl_bif_info
    // erts_ddll_init();
    init_emulator();
    // erts_ptab_init(); /* Must be after init_emulator() */
    // erts_init_binary(); /* Must be after init_emulator() */
    // erts_bp_init();
    // init_db(); /* Must be after init_emulator */
    // erts_bif_timer_init();
    // erts_init_node_tables();
    // init_dist();
    // erl_drv_thr_init();
    // erts_init_async();
    // erts_init_io(port_tab_sz, port_tab_sz_ignore_files, legacy_port_tab);
    // init_load();
    // erts_init_bif(); // ESTUB: bif
    // erts_init_bif_chksum();
    // erts_init_bif_binary();
    // erts_init_bif_re();
    // erts_init_unicode(); /* after RE to get access to PCRE unicode */
/*     erts_init_external(); */
/*     erts_delay_trap = erts_export_put(am_erlang, am_delay_trap, 2); */
/*     erts_late_init_process(); */
/* #if HAVE_ERTS_MSEG */
/*     erts_mseg_late_init(); /\* Must be after timer (erts_init_time()) and thread */
/* 			      initializations *\/ */
/* #endif */
/* #ifdef HIPE */
/*     hipe_mode_switch_init(); /\* Must be after init_load/beam_catches/init *\/ */
/* #endif */
/*     packet_parser_init(); */
/*     erl_nif_init(); */
}

/* Eterm */
/* erts_preloaded(Process* p) */
/* { */
/*     Eterm previous; */
/*     int j; */
/*     int need; */
/*     Eterm mod; */
/*     Eterm* hp; */
/*     char* name; */
/*     const Preload *preload = sys_preloaded(); */

/*     j = 0; */
/*     while (preload[j].name != NULL) { */
/* 	j++; */
/*     } */
/*     previous = NIL; */
/*     need = 2*j; */
/*     hp = HAlloc(p, need); */
/*     j = 0; */
/*     while ((name = preload[j].name) != NULL)  { */
/* 	mod = am_atom_put(name, sys_strlen(name)); */
/* 	previous = CONS(hp, mod, previous); */
/* 	hp += 2; */
/* 	j++; */
/*     } */
/*     return previous; */
/* } */


/* static variables that must not change (use same values at restart) */
static char* program;
static int    boot_argc;
static char** boot_argv;

static void 
load_preloaded(void)
{
    // ESTUB: beam_load, atom
    // int i;
    // Eterm res;
    // Preload* preload_p;
    // Eterm module_name;
    // byte* code;
    // char* name;
    // int length;
    // 
    // if ((preload_p = sys_preloaded()) == NULL) {
    //     return;
    // }
    // i = 0;
    // while ((name = preload_p[i].name) != NULL) {
    //     length = preload_p[i].size;
    //     module_name = erts_atom_put((byte *) name, sys_strlen(name), ERTS_ATOM_ENC_LATIN1, 1);
    //     if ((code = sys_preload_begin(&preload_p[i])) == 0)
    //         erl_exit(1, "Failed to find preloaded code for module %s\n", 
    //     	     name);
    //     res = erts_preload_module(NULL, 0, NIL, &module_name, code, length);
    //     sys_preload_end(&preload_p[i]);
    //     if (res != NIL)
    //         erl_exit(1,"Failed loading preloaded module %s (%T)\n",
    //     	     name, res);
    //     i++;
    // }
}

/* be helpful (or maybe downright rude:-) */
void erts_usage(void)
{
    erts_fprintf(stderr, "Slave emulator usage requested (?)\n");
    erl_exit(-1, "");
}

#ifdef USE_THREADS
/*
 * allocators for thread lib
 */

static void *ethr_std_alloc(size_t size)
{
    return erts_alloc_fnf(ERTS_ALC_T_ETHR_STD, (Uint) size);
}
static void *ethr_std_realloc(void *ptr, size_t size)
{
    return erts_realloc_fnf(ERTS_ALC_T_ETHR_STD, ptr, (Uint) size);
}
static void ethr_std_free(void *ptr)
{
    erts_free(ERTS_ALC_T_ETHR_STD, ptr);
}
static void *ethr_sl_alloc(size_t size)
{
    return erts_alloc_fnf(ERTS_ALC_T_ETHR_SL, (Uint) size);
}
static void *ethr_sl_realloc(void *ptr, size_t size)
{
    return erts_realloc_fnf(ERTS_ALC_T_ETHR_SL, ptr, (Uint) size);
}
static void ethr_sl_free(void *ptr)
{
    erts_free(ERTS_ALC_T_ETHR_SL, ptr);
}
static void *ethr_ll_alloc(size_t size)
{
    return erts_alloc_fnf(ERTS_ALC_T_ETHR_LL, (Uint) size);
}
static void *ethr_ll_realloc(void *ptr, size_t size)
{
    return erts_realloc_fnf(ERTS_ALC_T_ETHR_LL, ptr, (Uint) size);
}
static void ethr_ll_free(void *ptr)
{
    erts_free(ERTS_ALC_T_ETHR_LL, ptr);
}

#endif

static int
early_init(int *argc, char **argv) /*
				   * Only put things here which are
				   * really important initialize
				   * early!
				   */
{
    // ErtsAllocInitOpts alloc_opts = ERTS_ALLOC_INIT_DEF_OPTS_INITER;
    int ncpu;
    int ncpuonln;
    int ncpuavail;
#ifdef ERTS_DIRTY_SCHEDULERS
    int dirty_cpu_scheds;
    int dirty_cpu_scheds_online;
    int dirty_cpu_scheds_pctg = 100;
    int dirty_cpu_scheds_onln_pctg = 100;
    int dirty_io_scheds;
#endif

    erts_save_emu_args(*argc, argv);

    // ESTUB
    // erts_sched_compact_load = 1;
    erts_printf_eterm_func = erts_printf_term;
    erts_disable_tolerant_timeofday = 0;
    display_items = 200;
    erts_backtrace_depth = DEFAULT_BACKTRACE_SIZE;
    // erts_async_max_threads = ERTS_DEFAULT_NO_ASYNC_THREADS;
    // erts_async_thread_suggested_stack_size = ERTS_ASYNC_THREAD_MIN_STACK_SIZE;
    H_MIN_SIZE = H_DEFAULT_SIZE;
    BIN_VH_MIN_SIZE = VH_DEFAULT_SIZE;

    erts_initialized = 0;

    erts_use_sender_punish = 1;

    //erts_pre_early_init_cpu_topology(&max_reader_groups,
    //  			     &ncpu,
    //  			     &ncpuonln,
    //  			     &ncpuavail);
#ifndef ERTS_SMP
    ncpu = 1;
    ncpuonln = 1;
    ncpuavail = 1;
#endif

    ignore_break = 0;
    replace_intr = 0;
    program = argv[0];

    erts_modified_timing_level = -1;

    erts_compat_rel = this_rel_num();

    erts_sys_pre_init();
    erts_atomic_init_nob(&exiting, 0);
#ifdef ERTS_SMP
    erts_thr_progress_pre_init();
#endif

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_init();
#endif
#ifdef ERTS_SMP
    erts_smp_atomic32_init_nob(&erts_writing_erl_crash_dump, 0L);
    erts_tsd_key_create(&erts_is_crash_dumping_key,"erts_is_crash_dumping_key");
#else
    erts_writing_erl_crash_dump = 0;
#endif

    erts_smp_atomic32_init_nob(&erts_max_gen_gcs,
			       (erts_aint32_t) ((Uint16) -1));

    // erts_pre_init_process();
#if defined(USE_THREADS) && !defined(ERTS_SMP)
    main_thread = erts_thr_self();
#endif

    /*
     * We need to know the number of schedulers to use before we
     * can initialize the allocators.
     */
    no_schedulers = (Uint) (ncpu > 0 ? ncpu : 1);
    no_schedulers_online = (ncpuavail > 0
			    ? ncpuavail
			    : (ncpuonln > 0 ? ncpuonln : no_schedulers));

#ifdef ERTS_DIRTY_SCHEDULERS
    dirty_cpu_scheds = no_schedulers;
    dirty_cpu_scheds_online = no_schedulers_online;
    dirty_io_scheds = 10;
#endif

    /* erts_sys_getenv(_raw)() not initialized yet; need erts_sys_getenv__() */
    // if (erts_sys_getenv__("ERL_THREAD_POOL_SIZE", envbuf, &envbufsz) == 0)
    //     erts_async_max_threads = atoi(envbuf);
    // else
    //     erts_async_max_threads = ERTS_DEFAULT_NO_ASYNC_THREADS;
    // if (erts_async_max_threads > ERTS_MAX_NO_OF_ASYNC_THREADS)
    //     erts_async_max_threads = ERTS_MAX_NO_OF_ASYNC_THREADS;

    // ESTUB: argv parsing

#ifndef USE_THREADS
    // erts_async_max_threads = 0;
#endif

#ifdef ERTS_SMP
    no_schedulers = schdlrs;
    no_schedulers_online = schdlrs_onln;

    erts_no_schedulers = (Uint) no_schedulers;
#endif
#ifdef ERTS_DIRTY_SCHEDULERS
    erts_no_dirty_cpu_schedulers = no_dirty_cpu_schedulers = dirty_cpu_scheds;
    no_dirty_cpu_schedulers_online = dirty_cpu_scheds_online;
    erts_no_dirty_io_schedulers = no_dirty_io_schedulers = dirty_io_scheds;
#endif
    // erts_early_init_scheduling(no_schedulers);

    // alloc_opts.ncpu = ncpu;
    // erts_alloc_init(argc, argv, &alloc_opts); /* Handles (and removes)
    // 					 -M flags. */

    /* Require allocators */
#ifdef ERTS_SMP
    /*
     * Thread progress management:
     *
     * * Managed threads:
     * ** Scheduler threads (see erl_process.c)
     * ** Aux thread (see erl_process.c)
     * ** Sys message dispatcher thread (see erl_trace.c)
     *
     * * Unmanaged threads that need to register:
     * ** Async threads (see erl_async.c)
     * ** Dirty scheduler threads
     */
    erts_thr_progress_init(no_schedulers,
			   no_schedulers+2,
#ifndef ERTS_DIRTY_SCHEDULERS
			   erts_async_max_threads
#else
			   erts_async_max_threads +
			   erts_no_dirty_cpu_schedulers +
			   erts_no_dirty_io_schedulers
#endif
			   );
#endif
    // erts_thr_q_init();
    erts_init_utils();
    // erts_early_init_cpu_topology(no_schedulers,
    //                              &max_main_threads,
    //                              max_reader_groups,
    //                              &reader_groups);

#ifdef USE_THREADS
    {
	erts_thr_late_init_data_t elid = ERTS_THR_LATE_INIT_DATA_DEF_INITER;
	elid.mem.std.alloc = ethr_std_alloc;
	elid.mem.std.realloc = ethr_std_realloc;
	elid.mem.std.free = ethr_std_free;
	elid.mem.sl.alloc = ethr_sl_alloc;
	elid.mem.sl.realloc = ethr_sl_realloc;
	elid.mem.sl.free = ethr_sl_free;
	elid.mem.ll.alloc = ethr_ll_alloc;
	elid.mem.ll.realloc = ethr_ll_realloc;
	elid.mem.ll.free = ethr_ll_free;
	elid.main_threads = max_main_threads;
	elid.reader_groups = reader_groups;

	erts_thr_late_init(&elid);
    }
#endif

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_late_init();
#endif
    
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_late_init();
#endif

#if defined(HIPE)
    hipe_signal_init();	/* must be done very early */
#endif

    erl_sys_args(argc, argv);

    /* Creates threads on Windows that depend on the arguments, so has to be after erl_sys_args */
    erl_sys_init();

    // erts_ets_realloc_always_moves = 0; // ESTUB: erl_db
    // erts_ets_always_compress = 0; // ESTUB: erl_db
    // erts_dist_buf_busy_limit = ERTS_DE_BUSY_LIMIT; // ESTUB: dist

    return ncpu;
}

#ifndef ERTS_SMP
static void set_main_stack_size(void)
{
    if (erts_sched_thread_suggested_stack_size > 0) {
# if HAVE_DECL_GETRLIMIT && HAVE_DECL_SETRLIMIT && HAVE_DECL_RLIMIT_STACK
	struct rlimit rl;
	int bytes = erts_sched_thread_suggested_stack_size * sizeof(Uint) * 1024;
	if (getrlimit(RLIMIT_STACK, &rl) != 0 ||
	    (rl.rlim_cur = bytes, setrlimit(RLIMIT_STACK, &rl) != 0)) {
	    erts_fprintf(stderr, "failed to set stack size for scheduler "
				 "thread to %d bytes\n", bytes);
	    erts_usage();
	}	    
# else
	erts_fprintf(stderr, "no OS support for dynamic stack size limit\n");
	erts_usage();    
# endif
    }
}
#endif

void
erl_start(int argc, char **argv)
{
    int i = 1;
    int have_break_handler = 1;
    int ncpu = early_init(&argc, argv);
    int proc_tab_sz = ERTS_DEFAULT_MAX_PROCESSES;
    int port_tab_sz = ERTS_DEFAULT_MAX_PORTS;
    int port_tab_sz_ignore_files = 0;
    int legacy_proc_tab = 0;
    int legacy_port_tab = 0;


    /* envbufsz = sizeof(envbuf); */
    /* if (erts_sys_getenv_raw(ERL_MAX_ETS_TABLES_ENV, envbuf, &envbufsz) == 0) */
    /*     user_requested_db_max_tabs = atoi(envbuf); */
    /* else */
    /*     user_requested_db_max_tabs = 0; */

    /* envbufsz = sizeof(envbuf); */
    /* if (erts_sys_getenv_raw("ERL_FULLSWEEP_AFTER", envbuf, &envbufsz) == 0) { */
    /*     Uint16 max_gen_gcs = atoi(envbuf); */
    /*     erts_smp_atomic32_set_nob(&erts_max_gen_gcs, */
    /*     			  (erts_aint32_t) max_gen_gcs); */
    /* } */

    /* envbufsz = sizeof(envbuf); */
    /* if (erts_sys_getenv_raw("ERL_MAX_PORTS", envbuf, &envbufsz) == 0) { */
    /*     port_tab_sz = atoi(envbuf); */
    /*     port_tab_sz_ignore_files = 1; */
    /* } */

#if (defined(__APPLE__) && defined(__MACH__)) || defined(__DARWIN__)
    /*
     * The default stack size on MacOS X is too small for pcre.
     */
    erts_sched_thread_suggested_stack_size = 256;
#endif

#ifdef DEBUG
    verbose = DEBUG_DEFAULT;
#endif

    erts_error_logger_warnings = am_error;

    // ESTUB: argv parsing

/* Output format on windows for sprintf defaults to three exponents.
 * We use two-exponent to mimic normal sprintf behaviour.
 */

#if defined(__WIN32__) && defined(_TWO_DIGIT_EXPONENT)
    _set_output_format(_TWO_DIGIT_EXPONENT);
#endif

   /* Restart will not reinstall the break handler */
#ifdef __WIN32__
    if (ignore_break)
	erts_set_ignore_break();
    else if (replace_intr)
	erts_replace_intr();
    else
	init_break_handler();
#else
    if (ignore_break)
	erts_set_ignore_break();
    else if (have_break_handler)
	init_break_handler();
    if (replace_intr)
	erts_replace_intr();
#endif

    boot_argc = argc - i;  /* Number of arguments to init */
    boot_argv = &argv[i];

    erl_init(ncpu,
	     proc_tab_sz,
	     legacy_proc_tab,
	     port_tab_sz,
	     port_tab_sz_ignore_files,
	     legacy_port_tab);

    load_preloaded();
    // erts_end_staging_code_ix();
    // erts_commit_staging_code_ix();

    erts_initialized = 1;

    // erl_first_process_otp("otp_ring0", NULL, 0, boot_argc, boot_argv);

#ifdef ERTS_SMP
    erts_start_schedulers();
    /* Let system specific code decide what to do with the main thread... */

    erts_sys_main_thread(); /* May or may not return! */
#else
    erts_thr_set_main_status(1, 1);
#if ERTS_USE_ASYNC_READY_Q
    erts_get_scheduler_data()->aux_work_data.async_ready.queue
	= erts_get_async_ready_queue(1);
#endif
    set_main_stack_size();
    process_main();
#endif
}


#ifdef USE_THREADS

__decl_noreturn void erts_thr_fatal_error(int err, char *what)
{
    char *errstr = err ? strerror(err) : NULL;
    erts_fprintf(stderr,
		 "Failed to %s: %s%s(%d)\n",
		 what,
		 errstr ? errstr : "",
		 errstr ? " " : "",
		 err);
    abort();
}

#endif

static void
system_cleanup(int flush_async)
{
    /*
     * Make sure only one thread exits the runtime system.
     */
    if (erts_atomic_inc_read_nob(&exiting) != 1) {
	/*
	 * Another thread is currently exiting the system;
	 * wait for it to do its job.
	 */
#ifdef ERTS_SMP
	if (erts_thr_progress_is_managed_thread()) {
	    /*
	     * The exiting thread might be waiting for
	     * us to block; need to update status...
	     */
	    erts_thr_progress_active(NULL, 0);
	    erts_thr_progress_prepare_wait(NULL);
	}
#endif
	/* Wait forever... */
	while (1)
	    erts_milli_sleep(10000000);
    }

    // ESTUB async disabled
}

static __decl_noreturn void __noreturn
erl_exit_vv(int n, int flush_async, char *fmt, va_list args1, va_list args2)
{
    unsigned int an;

    system_cleanup(flush_async);

    // save_statistics(); // ESTUB
    if (n < 0)
        an = -(unsigned int)n;
    else
        an = n;

    // if (erts_mtrace_enabled)
    //     erts_mtrace_exit((Uint32) an);

    // /* Produce an Erlang core dump if error */
    // if (((n > 0 && erts_no_crash_dump == 0) || n == ERTS_DUMP_EXIT)
    //     && erts_initialized) {
    //     erl_crash_dump_v((char*) NULL, 0, fmt, args1);
    // }

    if (fmt != NULL && *fmt != '\0')
	  erl_error(fmt, args2);	/* Print error message. */
    sys_tty_reset(n);

    if (n == ERTS_INTR_EXIT)
	exit(0);
    else if (n == ERTS_DUMP_EXIT)
	ERTS_EXIT_AFTER_DUMP(1);
    else if (n > 0 || n == ERTS_ABORT_EXIT)
        abort();
    exit(an);
}

/* Exit without flushing async threads */
__decl_noreturn void __noreturn erl_exit(int n, char *fmt, ...)
{
    va_list args1, args2;
    va_start(args1, fmt);
    va_start(args2, fmt);
    erl_exit_vv(n, 0, fmt, args1, args2);
    va_end(args2);
    va_end(args1);
}

/* Exit after flushing async threads */
__decl_noreturn void __noreturn erl_exit_flush_async(int n, char *fmt, ...)
{
    va_list args1, args2;
    va_start(args1, fmt);
    va_start(args2, fmt);
    erl_exit_vv(n, 1, fmt, args1, args2);
    va_end(args2);
    va_end(args1);
}
