#ifndef _EPIPHANY_ALLOC_REDIRECT_H
#define _EPIPHANY_ALLOC_REDIRECT_H

#include <stdlib.h>

void *internal_malloc(size_t size);
void internal_free(void *ptr);
void *internal_calloc(size_t nmemb, size_t size);
void *internal_realloc(void *ptr, size_t size);

#ifndef EPIPHANY_NO_ALLOC_REDIRECT
/* ETODO: Do we need this? */
void *malloc(size_t size) asm(__USER_LABEL_PREFIX__ "internal_malloc");
void free(void *ptr) asm(__USER_LABEL_PREFIX__ "internal_free");
void *calloc(size_t nmemb, size_t size) asm(__USER_LABEL_PREFIX__ "internal_calloc");
void *realloc(void *ptr, size_t size) asm(__USER_LABEL_PREFIX__ "internal_realloc");

/*
 * I don't trust the header files to not inline something that calls malloc; we
 * will undefine these macros in the implementation instead.
 */
#define malloc  internal_malloc
#define free    internal_free
#define calloc  internal_calloc
#define realloc internal_realloc
#endif

#endif /* defined(_EPIPHANY_ALLOC_REDIRECT_H) */
