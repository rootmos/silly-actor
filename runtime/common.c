#include "common.h"
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

void __failwith(
        const char* caller,
        const char* file,
        int line,
        const char* fmt, ...)
{
    va_list vl;
    va_start(vl, fmt);
    fprintf(stderr, "%s:%s:%d: ", caller, file, line);
    vfprintf(stderr, fmt, vl);
    va_end(vl);

    abort();
}

enum log_level log_level;

void set_log_level()
{
    const char* s;
    if ((s = getenv("TRACE")) != NULL && strcmp(s, "1") == 0) {
        log_level = TRACE;
    } else if ((s = getenv("DEBUG")) != NULL && strcmp(s, "1") == 0) {
        log_level = DEBUG;
    } else if ((s = getenv("INFO")) != NULL && strcmp(s, "1") == 0) {
        log_level = INFO;
    } else if ((s = getenv("WARN")) != NULL && strcmp(s, "1") == 0) {
        log_level = WARN;
    } else {
        log_level = STDERR;
    }
}
