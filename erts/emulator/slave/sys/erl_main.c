/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2000-2015. All Rights Reserved.
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
#define EPIPHANY_NO_WRITE_REDIRECT
#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_printf_format.h"
#include "slave_io.h"
#include "epiphany.h"
#include <e-lib.h>

static int is_leader(void);
static void grab_barrier(void);
EPIPHANY_SRAM_FUNC void __attribute__((interrupt)) handl(void);

static int is_leader() {
    if (epiphany_in_emulator()) {
	/* Hardcoded coordinates 32,8 */
        return e_get_coreid() == 04010;
    } else {
        return e_group_config.core_row == 0
            && e_group_config.core_col == 0;
    }
}

#ifdef ERTS_SMP
static volatile int slave_flag = 0;
#endif

/*
 * The barrier is non-static as it it used by erl_slave_command in the master to
 * await slave initialisation.
 */
volatile int start_barrier __attribute__((section(".data_bank0")));
static void grab_barrier() {
    int row, col;
    const int rows = e_group_config.group_rows;
    const int cols = e_group_config.group_cols;
    if (epiphany_in_emulator()) {
	/* The barrier does not fix the problem in the emulator, and we don't
	 * know the workgroup size anyway (the fields above are always 0).
	 */
	return;
    }
    start_barrier = 1;
    for (row = 0; row < rows; row++) {
	for (col = 0; col < cols; col++) {
	    volatile int *other
		= e_get_global_address(row, col, (void*)&start_barrier);
	    while (*other == 0);
	}
    }
}

int
main(int argc, char **argv)
{
    /* Dram behaves strangely until all cores have started */
    grab_barrier();

    erts_init_slave_io();

    e_irq_attach(E_SYNC, handl);
    e_irq_attach(E_SW_EXCEPTION, handl);
    e_irq_attach(E_MEM_FAULT, handl);
    e_irq_attach(E_TIMER0_INT, handl);
    e_irq_attach(E_TIMER1_INT, handl);
    e_irq_attach(E_DMA0_INT, handl);
    e_irq_attach(E_DMA1_INT, handl);
    e_irq_attach(E_USER_INT, handl);

    /* Cleanup any old interrupts before unmasking them */
    if (e_reg_read(E_REG_ILAT))
	erts_printf("Ignoring ILAT=%x\n", e_reg_read(E_REG_ILAT));
    e_reg_write(E_REG_ILAT, 0);
    e_reg_write(E_REG_IMASK, 0);

    /* Bits 2 and 3 protects the lower and upper half of data_bank1 from writing */
    e_reg_write(E_REG_MEMPROTECT,
		e_reg_read(E_REG_MEMPROTECT)
		| (1 << 2)
		| (1 << 3));

    if (is_leader()) {
        erl_start(argc, argv);
    } else {
#ifdef ERTS_SMP
	unsigned sched_no = epiphany_coreno();
        while (slave_flag == 0);
	enter_scheduler(sched_no);
#endif
    }

    erts_printf("Terminating normally\n");
    return 0;
}

#ifdef ERTS_SMP
void erts_sys_main_thread() {
    /* We want to run BEAM on the main thread */
    enter_scheduler(0);
}

void erts_start_schedulers() {
    slave_flag = 1;
}
#endif

EPIPHANY_SRAM_FUNC void __attribute__((interrupt))
handl(void) {
    unsigned lr;
    asm("mov %0, lr" : "=r"(lr) : );
    erts_printf("Interrupted! IPEND=%x, IRET=%#x, lr=%#x\n",
		e_reg_read(E_REG_IPEND),
		e_reg_read(E_REG_IRET),
		lr);
};
