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

        cycles = instructions[opcode].cycles + (instructions[opcode].operation() & instructions[opcode].addrmode()); 
    }
    cycles--;
}

void reset();
void irq();
void nmi();

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

uint8_t IMP() {
    data = AC;
    return 0x00;
}

uint8_t IMM() {
	addr_abs = PC++;
    return 0x00;
}

uint8_t ZP0() {
	addr_abs = read_data(PC++) & 0x00FF;
    return 0;
}

uint8_t ZPX() {
	addr_abs = (read_data(PC++) + X) & 0x00FF;
    return 0;

}

uint8_t ZPY() {
	addr_abs = (read_data(PC++) + Y) & 0x00FF;
    return 0;
}

uint8_t REL() {
	addr_rel = read_data(PC++);
    if (addr_rel & 0x80) addr_rel |= 0xFF00;
    return 0;
}

uint8_t ABS() {
	uint16_t lo = read_data(PC++);
    uint16_t hi = read_data(PC++);
    addr_abs = (hi << 8) | lo;
    return 0;
}

uint8_t ABX() {
	uint16_t lo = read_data(PC++);
    uint16_t hi = read_data(PC++);
    addr_abs = (hi << 8) | lo;
    addr_abs += X;
    if ((addr_abs & 0xFF00) != (hi << 8)) return 1;
    else return 0;
}

uint8_t ABY() {
	uint16_t lo = read_data(PC++);
    uint16_t hi = read_data(PC++);
    addr_abs = (hi << 8) | lo;
    addr_abs += Y;
    if ((addr_abs & 0xFF00) != (hi << 8)) return 1;
    else return 0;

}

uint8_t IND() {
    uint16_t lo = read_data(PC++);
    uint16_t hi = read_data(PC++);
    uint16_t ptr = (hi << 8) | lo;
    if (lo == 0x00FF) {
        addr_abs = (read_data(ptr & 0xFF00) << 8) | read_data(ptr);
    } else {
        addr_abs = (read_data(ptr+1) << 8) | read_data(ptr);
    }
    return 0;
}

uint8_t IZX() {
	uint16_t t = read_data(PC++);
    uint16_t lo = read_data((uint16_t)(t + (uint16_t)X + 0) & 0x00FF);
    uint16_t hi = read_data((uint16_t)(t + (uint16_t)X + 1) & 0x00FF);
    addr_abs = (hi << 8) | lo;
    return 0;
}

uint8_t IZY() {
	uint16_t t = read_data(PC++);
    uint16_t lo = read_data((t + 0) & 0x00FF);
    uint16_t hi = read_data((t + 1) & 0x00FF);

    addr_abs = (hi << 8) | lo;
    addr_abs += Y;
    
    if ((addr_abs & 0xFF00) != (hi << 8)) return 1;
    else return 0;
}

uint8_t get_data() {
    if (instructions[opcode].addrmode != IMP)
        data = read_data(addr_abs);
    return data;
}

uint8_t ADC() {
    get_data();
    uint16_t temp = (uint16_t)AC + (uint16_t)data + (uint16_t)get_flag(C);
    set_flag(C, temp > 255);
    set_flag(Z, !(temp & 0x00FF));
    set_flag(N, temp & 0x00);
    set_flag(V, (~((uint16_t)AC ^ (uint16_t)data) & ((uint16_t)AC ^ (uint16_t)temp)) & 0x0080);
    AC = temp & 0x00FF;
    return 1;
}

uint8_t AND() {
    AC &= get_data();
    set_flag(Z, AC == 0x00);
    set_flag(N, AC & 0x80);
    return 1;
}

uint8_t ASL() {
	
}

uint8_t BCC() {
	if (!get_flag(C)) {
        cycles++;
        addr_abs = PC + addr_rel;

        if (addr_abs & 0xFF00 != PC & 0xFF00)
            cycles++;

        PC = addr_abs;
    }
    return 0;

}

uint8_t BCS() {
	if (get_flag(C)) {
        cycles++;
        addr_abs = PC + addr_rel;

        if (addr_abs & 0xFF00 != PC & 0xFF00)
            cycles++;

        PC = addr_abs;
    }
    return 0;
}

uint8_t BEQ() {
	if (get_flag(Z)) {
        cycles++;
        addr_abs = PC + addr_rel;

        if (addr_abs & 0xFF00 != PC & 0xFF00)
            cycles++;

        PC = addr_abs;
    }
    return 0;
}

uint8_t BIT() {
	
}

uint8_t BMI() {
	if (get_flag(N)) {
        cycles++;
        addr_abs = PC + addr_rel;

        if (addr_abs & 0xFF00 != PC & 0xFF00)
            cycles++;

        PC = addr_abs;
    }
    return 0;
}

uint8_t BNE() {
	if (!get_flag(Z)) {
        cycles++;
        addr_abs = PC + addr_rel;

        if (addr_abs & 0xFF00 != PC & 0xFF00)
            cycles++;

        PC = addr_abs;
    }
    return 0;
}

uint8_t BPL() {
	if (!get_flag(N)) {
        cycles++;
        addr_abs = PC + addr_rel;

        if (addr_abs & 0xFF00 != PC & 0xFF00)
            cycles++;

        PC = addr_abs;
    }
    return 0;
}

uint8_t BRK() {
	
}

uint8_t BVC() {
	if (!get_flag(V)) {
        cycles++;
        addr_abs = PC + addr_rel;

        if (addr_abs & 0xFF00 != PC & 0xFF00)
            cycles++;

        PC = addr_abs;
    }
    return 0;
}

uint8_t BVS() {
	if (get_flag(V)) {
        cycles++;
        addr_abs = PC + addr_rel;

        if (addr_abs & 0xFF00 != PC & 0xFF00)
            cycles++;

        PC = addr_abs;
    }
    return 0;
}

uint8_t CLC() {
	set_flag(C, 0);
    return 0;
}

uint8_t CLD() {
    set_flag(D, 0);
    return 0;
}

uint8_t CLI() {
	
}

uint8_t CLV() {
	
}

uint8_t CMP() {
	
}

uint8_t CPX() {
	
}

uint8_t CPY() {
	
}

uint8_t DEC() {
	
}

uint8_t DEX() {
	
}

uint8_t DEY() {
	
}

uint8_t EOR() {
	
}

uint8_t INC() {
	
}

uint8_t INX() {
	
}

uint8_t INY() {
	
}

uint8_t JMP() {
	
}

uint8_t JSR() {
	
}

uint8_t LDA() {
	
}

uint8_t LDX() {
	
}

uint8_t LDY() {
	
}

uint8_t LSR() {
	
}

uint8_t NOP() {
	
}

uint8_t ORA() {
	
}

uint8_t PHA() {
	
}

uint8_t PHP() {
	
}

uint8_t PLA() {
	
}

uint8_t PLP() {
	
}

uint8_t ROL() {
	
}

uint8_t ROR() {
	
}

uint8_t RTI() {
	
}

uint8_t RTS() {
	
}

uint8_t SBC() {
	
}

uint8_t SEC() {
	
}

uint8_t SED() {
	
}

uint8_t SEI() {
	
}

uint8_t STA() {
	
}

uint8_t STX() {
	
}

uint8_t STY() {
	
}

uint8_t TAX() {
	
}

uint8_t TAY() {
	
}

uint8_t TSX() {
	
}

uint8_t TXA() {
	
}

uint8_t TXS() {
	
}

uint8_t TYA() {
	
}

