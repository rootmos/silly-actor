#include <stack.h>
#include <common.h>

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

struct slot {
    struct stack_data data;
    uint16_t fork_count;
};

struct block {
    bool claimed;
    size_t N;
    size_t msp; /* TODO: maybe set this as a *size_t to the owners sp? */
    struct slot slots[];
};

struct stack {
    struct block* block;
    size_t sp;
    bool fork;
};

#define STACK_NO_INITIAL_SLOTS 100

struct stack* stack_fresh()
{
    struct stack* st = (struct stack*)malloc(sizeof(*st));
    st->sp = -1;
    st->fork = false;

    size_t block_size =
        sizeof(struct block) +
        STACK_NO_INITIAL_SLOTS * sizeof(struct slot);
    struct block* b = st->block = (struct block*)calloc(block_size, 1);
    b->claimed = true;
    b->msp = st->sp;
    b->N = STACK_NO_INITIAL_SLOTS;
    return st;
}

void stack_push(struct stack* st, struct stack_data data)
{
    size_t sp0 = st->sp;
    size_t sp1 = st->sp += 1;
    struct block* block = st->block;

    if (st->fork) {
        block->slots[sp0].fork_count -= 1;

        if (sp0 != block->msp || block->claimed) {
            size_t block_size =
                sizeof(struct block) + block->N * sizeof(struct slot);
            st->block = (struct block*)calloc(block_size, 1);
            st->block->msp = sp0;
            st->block->N = block->N;
            memcpy(st->block->slots, block->slots,
                   (1 + sp0) * sizeof(struct slot));
            block = st->block;
        }
        st->fork = false;
        block->claimed = true;
        block->msp += 1;
        block->slots[sp1].data = data;
    } else {
        block->msp += 1;
        block->slots[sp1].data = data;
    }
}

void stack_pop(struct stack* st)
{
    size_t sp = st->sp;
    st->sp -= 1;
    struct block* block = st->block;

    if (st->fork) {
        block->slots[sp].fork_count -= 1;
        block->slots[st->sp].fork_count += 1;

        if (sp == block->msp
           && block->slots[sp].fork_count == 0
           && !block->claimed) {
           block->msp -= 1;
        }
    } else {
        if (block->slots[sp].fork_count == 0) {
            block->msp -= 1;
        } else {
            st->fork = true;
            st->block->claimed = false;
            block->slots[st->sp].fork_count += 1;
        }
    }
}

struct stack_data stack_nth(const struct stack* st, size_t n)
{
    if(n >= st->block->N)
        failwith("stack overflow (implement growing blocks)");

    return st->block->slots[st->sp - n].data;
}

struct stack* stack_fork(const struct stack* st)
{
    if (st->sp == -1) return stack_fresh();

    struct stack* tt = (struct stack*)malloc(sizeof(*tt));
    tt->block = st->block;
    tt->sp = st->sp;
    tt->fork = true;
    tt->block->slots[tt->sp].fork_count += 1;
    return tt;
}

size_t stack_height(const struct stack* st)
{
    return st->sp;
}

#ifdef TEST
#include <tests.h>

test_suite(stack)

bool cmp_data(struct stack_data d0, struct stack_data d1)
{
    return 0 == memcmp(&d0, &d1, sizeof(struct stack_data));
}

struct stack_data zero = {0};

test_case(stack_fresh)
{
    struct stack* st = stack_fresh();
    assert(st != NULL);
    assert(!st->fork);
    assert(st->sp == -1);
    assert(st->block->claimed);
    assert(st->block->N == STACK_NO_INITIAL_SLOTS);
    assert(st->block->msp == -1);
    for (size_t i = 0; i < st->block->N; i++) {
        assert(cmp_data(st->block->slots[i].data, zero));
        assert(st->block->slots[i].fork_count == 0);
    }
}
test_case_end(stack_fresh);

test_case(stack_push)
{
    struct stack* st = stack_fresh();
    fresh(struct stack_data, p0); stack_push(st, p0);
    assert(st->sp == 0);
    assert(st->block->msp == 0);
    assert(cmp_data(st->block->slots[0].data, p0));

    fresh(struct stack_data, p1); stack_push(st, p1);
    assert(st->sp == 1);
    assert(st->block->msp == 1);
    assert(cmp_data(st->block->slots[1].data, p1));
}
test_case_end(stack_push);

test_case(stack_nth)
{
    struct stack* st = stack_fresh();
    fresh(struct stack_data, p0); stack_push(st, p0);
    assert(cmp_data(stack_nth(st, 0), p0));

    fresh(struct stack_data, p1); stack_push(st, p1);
    assert(cmp_data(stack_nth(st, 0), p1));
    assert(cmp_data(stack_nth(st, 1), p0));
}
test_case_end(stack_nth);

test_case(stack_pop)
{
    struct stack* st = stack_fresh();
    fresh(struct stack_data, p0); stack_push(st, p0);
    fresh(struct stack_data, p1); stack_push(st, p1);

    stack_pop(st);
    assert(cmp_data(stack_nth(st, 0), p0));
    assert(st->sp == 0);
    assert(st->block->msp == 0);

    stack_pop(st);
    assert(st->sp == -1);
    assert(st->block->msp == -1);
}
test_case_end(stack_pop);

test_case(fork_empty_stack)
{
    struct stack* st = stack_fresh();
    struct stack* f = stack_fork(st);
    assert(st != f);
    assert(st->block != f->block);

    assert(!st->fork);
    assert(st->sp == -1);
    assert(st->block->N == STACK_NO_INITIAL_SLOTS);
    assert(st->block->claimed);
    assert(st->block->msp == -1);
    for (size_t i = 0; i < st->block->N; i++) {
        assert(cmp_data(st->block->slots[i].data, zero));
        assert(st->block->slots[i].fork_count == 0);
    }

    assert(!f->fork);
    assert(f->sp == -1);
    assert(f->block->N == STACK_NO_INITIAL_SLOTS);
    assert(f->block->claimed);
    assert(f->block->msp == -1);
    for (size_t i = 0; i < f->block->N; i++) {
        assert(cmp_data(f->block->slots[i].data, zero));
        assert(f->block->slots[i].fork_count == 0);
    }
}
test_case_end(fork_empty_stack)

test_case(fork_non_empty_stack)
{
    struct stack* st = stack_fresh();
    fresh(struct stack_data, p0); stack_push(st, p0);
    assert(st->block->slots[0].fork_count == 0);

    struct stack* f = stack_fork(st);
    assert(f != st);
    assert(f->fork);
    assert(!st->fork);
    assert(f->sp == st->sp);
    assert(f->block == st->block);
    assert(f->block->claimed);
    assert(f->block->slots[0].fork_count == 1);
    assert(cmp_data(stack_nth(st, 0), p0));
    assert(cmp_data(stack_nth(f, 0), p0));
}
test_case_end(fork_non_empty_stack)

test_case(push_fork_pop)
{
    struct stack* st = stack_fresh();
    fresh(struct stack_data, p0); stack_push(st, p0);
    fresh(struct stack_data, p1); stack_push(st, p1);

    struct stack* f = stack_fork(st);
    stack_pop(st);
    assert(cmp_data(stack_nth(f, 0), p1));
    assert(cmp_data(stack_nth(st, 0), p0));
    assert(f->block->slots[0].fork_count == 1);
    assert(f->block->slots[1].fork_count == 1);
    assert(f->block->msp == 1);
    assert(!st->block->claimed);
    assert(st->fork);
    assert(f->fork);
}
test_case_end(push_fork_pop)

test_case(push_fork_pop_fork)
{
    struct stack* st = stack_fresh();
    fresh(struct stack_data, p0); stack_push(st, p0);
    fresh(struct stack_data, p1); stack_push(st, p1);

    struct stack* f = stack_fork(st);
    stack_pop(f);
    assert(cmp_data(stack_nth(f, 0), p0));
    assert(cmp_data(stack_nth(st, 0), p1));
    assert(f->block->slots[1].fork_count == 0);
    assert(f->block->slots[0].fork_count == 1);
    assert(f->block->msp == 1);
    assert(st->block->claimed);
    assert(!st->fork);
}
test_case_end(push_fork_pop_fork)

test_case(push_fork_pop_pop_fork)
{
    struct stack* st = stack_fresh();
    fresh(struct stack_data, p0); stack_push(st, p0);
    fresh(struct stack_data, p1); stack_push(st, p1);

    struct stack* f = stack_fork(st);
    stack_pop(st);
    assert(f->block->slots[0].fork_count == 1);
    assert(f->block->slots[1].fork_count == 1);
    assert(f->block->msp == 1);

    stack_pop(f);
    assert(cmp_data(stack_nth(f, 0), p0));
    assert(cmp_data(stack_nth(st, 0), p0));
    assert(f->block->slots[0].fork_count == 2);
    assert(f->block->msp == 0);
    assert(st->fork);
    assert(f->fork);
}
test_case_end(push_fork_pop_pop_fork)

test_case(push_fork_claim)
{
    struct stack* st = stack_fresh();
    fresh(struct stack_data, p0); stack_push(st, p0);
    fresh(struct stack_data, p1); stack_push(st, p1);

    struct stack* f = stack_fork(st);
    stack_pop(st);
    assert(!st->block->claimed);
    assert(f->fork);
    assert(st->fork);

    fresh(struct stack_data, p2); stack_push(f, p2);
    assert(st->block == f->block);
    assert(f->block->msp == 2);
    assert(f->block->slots[0].fork_count == 1);
    assert(f->block->slots[1].fork_count == 0);
    assert(f->block->slots[2].fork_count == 0);
    assert(f->block->claimed);
    assert(!f->fork);
    assert(st->fork);
}
test_case_end(push_fork_claim)

test_case(push_fork_clone)
{
    struct stack* st = stack_fresh();
    fresh(struct stack_data, p0); stack_push(st, p0);
    fresh(struct stack_data, p1); stack_push(st, p1);

    struct stack* f = stack_fork(st);
    assert(f->fork);
    assert(!st->fork);
    assert(f->block->slots[1].fork_count == 1);

    fresh(struct stack_data, p2); stack_push(f, p2);
    assert(st->block != f->block);

    assert(!f->fork);
    assert(cmp_data(stack_nth(f, 0), p2));
    assert(cmp_data(stack_nth(f, 1), p1));
    assert(cmp_data(stack_nth(f, 2), p0));
    assert(f->block->msp == 2);
    assert(f->block->claimed);
    assert(f->block->slots[0].fork_count == 0);
    assert(f->block->slots[1].fork_count == 0);
    assert(f->block->slots[2].fork_count == 0);

    assert(!st->fork);
    assert(cmp_data(stack_nth(st, 0), p1));
    assert(cmp_data(stack_nth(st, 1), p0));
    assert(st->block->msp == 1);
    assert(st->block->claimed);
    assert(st->block->slots[0].fork_count == 0);
    assert(st->block->slots[1].fork_count == 0);
}
test_case_end(push_fork_clone)

test_suite_end()

#endif
