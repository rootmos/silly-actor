#include <tests.h>

#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <limits.h>

void fill_with_random(void* p, size_t n)
{
    assert(n < SSIZE_MAX);
    int fd = open("/dev/urandom", 0); assert(fd >= 0);
    assert(read(fd, p, n) == (ssize_t)n);
    assert(close(fd) == 0);
}

void* random_blob(size_t n)
{
    void* blob = malloc(n); assert(blob);
    fill_with_random(blob, n);
    return blob;
}

int main() {
    extern void TEST_SUITE(void);
    TEST_SUITE();
    return 0;
}
