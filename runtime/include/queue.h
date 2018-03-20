#pragma once

#include <runtime.h>

struct queue;

struct queue* fresh_queue();
void enqueue(struct queue*, struct msg*);
bool dequeue(struct queue*, struct msg**);
