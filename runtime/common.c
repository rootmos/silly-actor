#include "common.h"
#include <stdarg.h>
#include <stdlib.h>

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
