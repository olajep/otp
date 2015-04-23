#ifndef _EPIPHANY_ALLOC_REDIRECT_H
#define _EPIPHANY_ALLOC_REDIRECT_H

#include <stdlib.h>

void *internal_malloc(size_t size);
void internal_free(void *ptr);
void *internal_calloc(size_t nmemb, size_t size);
void *internal_realloc(void *ptr, size_t size);

/* ETODO: Do we need this? */
#ifndef EPIPHANY_NO_ALLOC_REDIRECT
void *malloc(size_t size) asm("_internal_malloc");
void free(void *ptr) asm("_internal_free");
void *calloc(size_t nmemb, size_t size) asm("_internal_calloc");
void *realloc(void *ptr, size_t size) asm("_internal_realloc");
#endif

/*
 * I don't trust the header files to not inline something that calls malloc; we
 * will undefine these macros in the implementation instead.
 */
#define malloc  internal_malloc
#define free    internal_free
#define calloc  internal_calloc
#define realloc internal_realloc

#endif /* defined(_EPIPHANY_ALLOC_REDIRECT_H) */
