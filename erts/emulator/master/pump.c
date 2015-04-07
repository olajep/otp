#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <assert.h>
#include <sys/mman.h>
#include <time.h>
#include <e-hal.h>
#include <e-loader.h>
#include "slave_syms.h"

#define ASSERT assert

#define OUTBUF_SZ 1024
static char * const outbuf = (char*) SLAVE_SYM_outbuf;
static char * volatile * const out_start = (char * volatile *)SLAVE_SYM_out_start;
static char * volatile * const out_end = (char * volatile *)SLAVE_SYM_out_end;

void cleanup();

/* Copies data from the log output ringbuffer to stdout.
 * Returns the number of bytes copied, or -1 if an error occurred.
 */
static int pump_output() {
    int pumped = 0;
    ssize_t written;

    char *captured_start = *out_start;
    char *captured_end = *out_end;
    ASSERT(outbuf <= captured_start && captured_start < outbuf + OUTBUF_SZ);
    ASSERT(outbuf <= captured_end && captured_end < outbuf + OUTBUF_SZ);

    if (captured_start > captured_end) {
        written = write(STDOUT_FILENO, captured_start, OUTBUF_SZ - (outbuf - captured_start));
        if (written == -1) goto write_error;
        pumped += written;
        captured_start += written;
        if (captured_start == outbuf + OUTBUF_SZ) captured_start = outbuf;
    }
    written = write(STDOUT_FILENO, captured_start, captured_end - captured_start);
    if (written == -1) goto write_error;
    pumped += written;
    *out_start = captured_start + written;
    return pumped;

 write_error:
    perror("write");
    return -1;
}

static int memfd = 0;

/* Maps the memory area shared with the epiphany chip to the same address as the
 * chip sees it at.
 * Returns 0 on success and -1 on failure.
 */
static int map_shm() {
    // We need the emem vector, which is not exposed by e_get_platform_info
    extern const e_platform_t e_platform;
    
    // The old way, for devices without the Epiphany kernel driver
    memfd = open("/dev/mem", O_RDWR | O_SYNC);
    if (memfd == 0) {
        perror("open: /dev/mem");
        return -1;
    }
    printf("Mapping 0x%x+0x%x to 0x%x\n",
           (unsigned)e_platform.emem[0].phy_base, 
           (unsigned)e_platform.emem[0].size,
           (unsigned)e_platform.emem[0].ephy_base);
    void *ret;
    // We avoid using MAP_FIXED because we'd rather know if there is another
    // mapping in the way than overwrite it.
    ret = mmap((void*)e_platform.emem[0].ephy_base, e_platform.emem[0].size,
               PROT_READ|PROT_WRITE, MAP_SHARED,
               memfd, e_platform.emem[0].phy_base);
    if (ret == MAP_FAILED) {
        perror("mmap");
        return -1;
    }
    if (ret != (void*)e_platform.emem[0].ephy_base) {
        fprintf(stderr, "mmap: wanted 0x%x, got 0x%x\n",
                (unsigned)e_platform.emem[0].ephy_base, (unsigned)ret);
        return -1;
    }
    return 0;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Usage: %s [binary]\n", argv[0]);
        return 1;
    }
    int ret;
    ret = e_init(NULL);
    if (ret != E_OK) { return 1; }

    // We make things easy for ourselves by mapping in the shared memory area at
    // *the same* address as it is observed by the Epiphany chip.
    if (map_shm()) { cleanup(); return 1; }

    ret = e_reset_system();
    if (ret != E_OK) { perror("e_reset_system"); cleanup(); return 1; }

#define ROWS 1
#define COLS 1
    e_epiphany_t workgroup;
    ret = e_open(&workgroup, 0, 0, ROWS, COLS);
    if (ret != E_OK) { perror("e_open"); cleanup(); return 1; }

    printf("Loading and starting program\n");
    ret = e_load_group(argv[1], &workgroup, 0, 0, ROWS, COLS, E_TRUE);
    if (ret != E_OK) { perror("e_load"); cleanup(); return 1; }

    printf("Yay! Beginning pumping...\n");

    while(1) {
        if (pump_output() == 0) {
            struct timespec ms = {0, 1000};
            nanosleep(&ms, NULL);
        }
    }

    return 0;
}

void cleanup() {
    int ret;
    if (memfd != 0) {
        ret = close(memfd);
        if (ret != 0) perror("free: memfd");
    }
    
    ret = e_finalize();
    if (ret != E_OK) perror("e_finalize");
}
