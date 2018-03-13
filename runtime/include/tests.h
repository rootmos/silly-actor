#pragma once

#include <assert.h>
#include <stdio.h>

#define test_done() do { \
    assert(ok); printf("%s - ok\n", __extension__ __FUNCTION__); \
} while (0)

#define test_suite(n) void n##_test_suite() {
#define test_suite_end() exit(0); }

#define test_case(n) void n##_test_case() { \
    volatile bool ok = false; \
    printf("%s - starting\n", __extension__ __FUNCTION__);
#define test_case_end(n) \
    assert(ok); printf("%s - ok\n", __extension__ __FUNCTION__); } \
    n##_test_case();
