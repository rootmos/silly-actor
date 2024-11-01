#pragma once

#include <stdio.h>

#define failwith(...) __failwith(__extension__ __FUNCTION__, __extension__ __FILE__, __extension__ __LINE__, __VA_ARGS__)

#define not_implemented() failwith("not_implemented\n")

void __failwith(const char* caller,
                const char* file,
                int line,
                const char* fmt, ...)
    __attribute__ ((noreturn));

#define log(l, ...) do { \
    if(log_level >= l) { \
        fprintf(stderr, "%s:%s:%d: ", \
                __extension__ __FUNCTION__, \
                __extension__ __FILE__, \
                __extension__ __LINE__); \
        fprintf(stderr, __VA_ARGS__); \
        fprintf(stderr, "\n"); \
    } \
} while (0)

#define warn(...) log(WARN, __VA_ARGS__)
#define info(...) log(INFO, __VA_ARGS__)
#define debug(...) log(DEBUG, __VA_ARGS__)
#define trace(...) log(TRACE, __VA_ARGS__)

enum log_level {
    STDERR=0,
    WARN,
    INFO,
    DEBUG,
    TRACE
};

extern enum log_level log_level;

void set_log_level();
