#include "cpu6502.h"
#include <stddef.h>
#include "bus.h"
#include "instruction.h"
#include "addr_modes.h"

Instruction instructions[] = {
    {"ADD", NULL, IMM, 1},
    {"SUB", NULL, ZP0, 2}
};

typedef enum {
    C = 1, B = 16,
    Z = 2, U = 32,
    I = 4, V = 64,
    D = 8, N = 128
} CPUFLAGS;

static uint8_t  AC = 0x00,
                X  = 0x00,
                Y  = 0x00,
                SR = 0x00,
                SP = 0x00;

static uint16_t PC = 0x0000,
                addr_abs = 0x0000,
                addr_rel = 0x0000;

static uint8_t data = 0x00,
               opcode = 0x00,
               cycles = 0;

void clock() {
    if (!cycles) {
        opcode = read_data(PC);
        PC++;

        cycles = instructions[opcode].cycles;
        
    }
}

void reset();
void irq();
void nmi();
void get_data();

void cpu_init() {
    bus_init();
}

void set_flag(CPUFLAGS f, char c) {
    if (c) SR |=  f;
    else   SR &= ~f;
}

uint8_t get_flag(CPUFLAGS f) {
    return SR & f;
}

uint8_t toggle_flag(CPUFLAGS f) {
    SR ^= f;
}

void write_data(uint16_t addr, uint8_t data) {
   bus_write(addr, data); 
}

uint8_t read_data(uint16_t addr) {
    return bus_read(addr);
}

uint8_t IMM() { }
uint8_t ZP0() { }
