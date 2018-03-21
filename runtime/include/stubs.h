#pragma once

#include <stack.h>
#include <value.h>

void setup_system(struct stack*);
void atoms_lookup(word_t w, const char** p, size_t* n);
