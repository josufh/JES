#ifndef INSTRUCTION_H
#define INSTRUCTION_H

#include <stdint.h>

typedef struct {
    char *name;
    uint8_t(*operation)(void);
    uint8_t(*addrmode )(void);
    uint8_t cycles;
} Instruction;

#endif // INSTRUCTION_H
