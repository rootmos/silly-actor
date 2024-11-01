#pragma once

#include <assert.h>
#include <stdio.h>

#define test_done() do { \
    assert(ok); printf("%s - ok\n", __extension__ __FUNCTION__); \
} while (0)

#define test_suite(n) void n##_test_suite(void) {
#define test_suite_end() exit(0); }

#define test_case(n) void n##_test_case() { \
    volatile bool ok = true; \
    printf("%s - starting\n", __extension__ __FUNCTION__);
#define test_case_end(n) \
    assert(ok); printf("%s - ok\n", __extension__ __FUNCTION__); } \
    n##_test_case();

void fill_with_random(void* p, size_t n);
void* random_blob(size_t n);

#define fresh(t, v) t v; do { fill_with_random(&v, sizeof(v)); } while (0)
