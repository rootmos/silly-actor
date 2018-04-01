#pragma once

#ifndef TEST
#include <gc.h>
#define my_malloc(n) GC_malloc(n)
#define my_calloc(m,n) GC_malloc((m)*(n))
#else
#define my_malloc(n) malloc(n)
#define my_calloc(m,n) calloc(m,n)
#endif
