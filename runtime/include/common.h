#pragma once

#include <stdio.h>

#define failwith(...) __failwith(__extension__ __FUNCTION__, __extension__ __FILE__, __extension__ __LINE__, __VA_ARGS__)

#define not_implemented() failwith("not_implemented\n")

void __failwith(const char* caller,
                const char* file,
                int line,
                const char* fmt, ...)
    __attribute__ ((noreturn));

#define info(...) do {                                          \
    fprintf(stderr, "%s:%s:%d: ", __extension__ __FUNCTION__,  __extension__ __FILE__, __extension__ __LINE__);         \
    fprintf(stderr, __VA_ARGS__);                               \
    fprintf(stderr, "\n");                                      \
} while (0)

#define warn(...) do {                                          \
    fprintf(stderr, "%s:%d: ",                                  \
            __extension__ __FUNCTION__,__extension__ __LINE__); \
    fprintf(stderr, __VA_ARGS__);                               \
} while (0)

#define debug(...) do {                                          \
    fprintf(stderr, "%s:%s:%d: ", __extension__ __FUNCTION__,  __extension__ __FILE__, __extension__ __LINE__);         \
    fprintf(stderr, __VA_ARGS__);                               \
} while (0)
