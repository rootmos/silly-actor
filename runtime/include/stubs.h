#pragma once

#include <stack.h>
#include <value.h>

void setup_system(struct stack*);
void atoms_lookup(word_t w, const char** p, size_t* n);

struct value ATOM_TRUE;
struct value ATOM_FALSE;
