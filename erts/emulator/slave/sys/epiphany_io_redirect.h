#ifndef _EPIPHANY_IO_REDIRECT_H
#define _EPIPHANY_IO_REDIRECT_H

#ifndef EPIPHANY_NO_WRITE_REDIRECT
#include <stdio.h>
// ETODO: Do we need this?
ssize_t write(int fildes, const void *buf, size_t nbyte) asm(__USER_LABEL_PREFIX__ "internal_write");
int erts_printf(const char*, ...);
int erts_fprintf(FILE*, const char*, ...);
#define printf erts_printf
#define fprintf erts_fprintf
#endif

#endif /* defined(_EPIPHANY_IO_REDIRECT_H) */
