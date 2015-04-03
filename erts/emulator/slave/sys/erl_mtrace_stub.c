/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2003-2013. All Rights Reserved.
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

/*
 * Description:	Memory allocation trace. This is a stub implementation.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "global.h"
//#include "erl_sock.h"
#include "erl_threads.h"
#include "erl_memory_trace_protocol.h"
#include "erl_mtrace.h"

#ifdef DEBUG
void
check_alloc_entry(byte *sp, byte *ep,
		  byte tag,
		  Uint16 ct_no, int ct_no_n,
		  Uint16 type, int type_n,
		  UWord res, int res_n,
		  Uint size, int size_n,
		  Uint32 ti,int ti_n);
void
check_realloc_entry(byte *sp, byte *ep,
		    byte tag,
		    Uint16 ct_no, int ct_no_n,
		    Uint16 type, int type_n,
		    UWord res, int res_n,
		    UWord ptr, int ptr_n,
		    Uint size, int size_n,
		    Uint32 ti,int ti_n);
void
check_free_entry(byte *sp, byte *ep,
		 byte tag,
		 Uint16 ct_no, int ct_no_n,
		 Uint16 t_no, int t_no_n,
		 UWord ptr, int ptr_n,
		 Uint32 ti,int ti_n);
void
check_time_inc_entry(byte *sp, byte *ep,
		     Uint32 secs, int secs_n,
		     Uint32 usecs, int usecs_n);
#endif



int erts_mtrace_enabled;

char* erl_errno_id(int error);

void erts_mtrace_pre_init(void)
{
}

void erts_mtrace_init(char *receiver, char *nodename)
{
    /* stubbed out */
    fprintf(stderr, "Trying to use a stubbed out mtrace\n");
}

void
erts_mtrace_install_wrapper_functions(void)
{
    /* stubbed out */
}

void
erts_mtrace_stop(void)
{
    /* stubbed out */
}

void
erts_mtrace_exit(Uint32 exit_value)
{
    /* stubbed out */
}

void
erts_mtrace_crr_alloc(void *res, ErtsAlcType_t n, ErtsAlcType_t m, Uint size)
{
    /* stubbed out */
}

void
erts_mtrace_crr_realloc(void *res, ErtsAlcType_t n, ErtsAlcType_t m, void *ptr,
			Uint size)
{
    /* stubbed out */
}

void
erts_mtrace_crr_free(ErtsAlcType_t n, ErtsAlcType_t m, void *ptr)
{
    /* stubbed out */
}


#if TRACE_PRINTOUTS
static void
print_trace_entry(byte tag,
		  Uint16 t_no, int t_no_n,
		  Uint16 ct_no, int ct_no_n,
		  Uint res, int res_n,
		  Uint ptr, int ptr_n,
		  Uint size, int size_n,
		  Uint32 ti,int ti_n)
{
    /* stubbed out */
}

#endif /* #if TRACE_PRINTOUTS */

#ifdef DEBUG

#define GET_UI16(P) ((P) += UI16_SZ, \
		     (((Uint16) (*((P) - 2) << 8)) | ((Uint16) (*((P) - 1)))))

static void
check_ui(Uint16 *hdrp, byte **pp, Uint ui, int msb,
	 Uint16 f_mask, Uint16 f_size)
{
    Uint x;
    int n;

    ASSERT((msb & ~f_mask) == 0);

    n = (int) (*hdrp & f_mask);

    ASSERT(n == msb);

    *hdrp >>= f_size;

    x = 0;
    switch (n) {
#ifdef ARCH_64
    case 7: x |= *((*pp)++); x <<= 8;
    case 6: x |= *((*pp)++); x <<= 8;
    case 5: x |= *((*pp)++); x <<= 8;
    case 4: x |= *((*pp)++); x <<= 8;
#endif
    case 3: x |= *((*pp)++); x <<= 8;
    case 2: x |= *((*pp)++); x <<= 8;
    case 1: x |= *((*pp)++); x <<= 8;
    case 0: x |= *((*pp)++); break;
    default: ASSERT(0);
    }

    ASSERT(x == ui);
}


void
check_alloc_entry(byte *sp, byte *ep,
		  byte tag,
		  Uint16 ct_no, int ct_no_n,
		  Uint16 t_no, int t_no_n,
		  UWord res, int res_n,
		  Uint size, int size_n,
		  Uint32 ti,int ti_n)
{
    byte *p = sp;
    Uint16 hdr;

    ASSERT(*p == tag);
    p++;

    hdr = GET_UI16(p);

    if (tag == ERTS_MT_CRR_ALLOC_BDY_TAG)
	check_ui(&hdr, &p, ct_no, ct_no_n, UI16_MSB_EHF_MSK, UI16_MSB_EHF_SZ);
    check_ui(&hdr, &p, t_no, t_no_n, UI16_MSB_EHF_MSK, UI16_MSB_EHF_SZ);
    check_ui(&hdr, &p, res,  res_n,  UI_MSB_EHF_MSK,   UI_MSB_EHF_SZ);
    check_ui(&hdr, &p, size, size_n, UI_MSB_EHF_MSK,   UI_MSB_EHF_SZ);
    check_ui(&hdr, &p, ti,   ti_n,   UI32_MSB_EHF_MSK, UI32_MSB_EHF_SZ);

    ASSERT(hdr == 0);
    ASSERT(p == ep);
}

void
check_realloc_entry(byte *sp, byte *ep,
		    byte tag,
		    Uint16 ct_no, int ct_no_n,
		    Uint16 t_no, int t_no_n,
		    UWord res, int res_n,
		    UWord ptr, int ptr_n,
		    Uint size, int size_n,
		    Uint32 ti,int ti_n)
{
    byte *p = sp;
    Uint16 hdr;

    ASSERT(*p == tag);
    p++;

    hdr = GET_UI16(p);

    if (tag == ERTS_MT_CRR_REALLOC_BDY_TAG)
	check_ui(&hdr, &p, ct_no, ct_no_n, UI16_MSB_EHF_MSK, UI16_MSB_EHF_SZ);
    check_ui(&hdr, &p, t_no, t_no_n, UI16_MSB_EHF_MSK, UI16_MSB_EHF_SZ);
    check_ui(&hdr, &p, res,  res_n,  UI_MSB_EHF_MSK,   UI_MSB_EHF_SZ);
    check_ui(&hdr, &p, ptr,  ptr_n,  UI_MSB_EHF_MSK,   UI_MSB_EHF_SZ);
    check_ui(&hdr, &p, size, size_n, UI_MSB_EHF_MSK,   UI_MSB_EHF_SZ);
    check_ui(&hdr, &p, ti,   ti_n,   UI32_MSB_EHF_MSK, UI32_MSB_EHF_SZ);

    ASSERT(hdr == 0);
    ASSERT(p == ep);
}

void
check_free_entry(byte *sp, byte *ep,
		 byte tag,
		 Uint16 ct_no, int ct_no_n,
		 Uint16 t_no, int t_no_n,
		 UWord ptr, int ptr_n,
		 Uint32 ti,int ti_n)
{
    byte *p = sp;
    Uint16 hdr;

    ASSERT(*p == tag);
    p++;

    hdr = GET_UI16(p);

    if (tag == ERTS_MT_CRR_FREE_BDY_TAG)
	check_ui(&hdr, &p, ct_no, ct_no_n, UI16_MSB_EHF_MSK, UI16_MSB_EHF_SZ);
    check_ui(&hdr, &p, t_no, t_no_n, UI16_MSB_EHF_MSK, UI16_MSB_EHF_SZ);
    check_ui(&hdr, &p, ptr,  ptr_n,  UI_MSB_EHF_MSK,   UI_MSB_EHF_SZ);
    check_ui(&hdr, &p, ti,   ti_n,   UI32_MSB_EHF_MSK, UI32_MSB_EHF_SZ);

    ASSERT(hdr == 0);
    ASSERT(p == ep);

}

void
check_time_inc_entry(byte *sp, byte *ep,
		     Uint32 secs, int secs_n,
		     Uint32 usecs, int usecs_n)
{
    byte *p = sp;
    Uint16 hdr;

    ASSERT(*p == ERTS_MT_TIME_INC_BDY_TAG);
    p++;

    hdr = GET_UI16(p);

    check_ui(&hdr, &p, secs,  secs_n,  UI32_MSB_EHF_MSK, UI32_MSB_EHF_SZ);
    check_ui(&hdr, &p, usecs, usecs_n, UI32_MSB_EHF_MSK, UI32_MSB_EHF_SZ);

    ASSERT(hdr == 0);
    ASSERT(p == ep);

}

#endif /* #ifdef DEBUG */
