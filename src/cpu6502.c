#include "cpu6502.h"
#include <stddef.h>
#include "bus.h"
#include "instruction.h"
#include "addr_modes.h"

static Instruction instructions[] = {
    { "BRK", BRK, IMM, 7 },{ "ORA", ORA, IZX, 6 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "NOP", NOP, IMP, 3 },{ "ORA", ORA, ZP0, 3 },{ "ASL", ASL, ZP0, 5 },{ "???", XXX, IMP, 5 },{ "PHP", PHP, IMP, 3 },{ "ORA", ORA, IMM, 2 },{ "ASL", ASL, IMP, 2 },{ "???", XXX, IMP, 2 },{ "???", NOP, IMP, 4 },{ "ORA", ORA, ABS, 4 },{ "ASL", ASL, ABS, 6 },{ "???", XXX, IMP, 6 },
    { "BPL", BPL, REL, 2 },{ "ORA", ORA, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "NOP", NOP, IMP, 4 },{ "ORA", ORA, ZPX, 4 },{ "ASL", ASL, ZPX, 6 },{ "???", XXX, IMP, 6 },{ "CLC", CLC, IMP, 2 },{ "ORA", ORA, ABY, 4 },{ "NOP", NOP, IMP, 2 },{ "???", XXX, IMP, 7 },{ "???", NOP, IMP, 4 },{ "ORA", ORA, ABX, 4 },{ "ASL", ASL, ABX, 7 },{ "???", XXX, IMP, 7 },
	{ "JSR", JSR, ABS, 6 },{ "AND", AND, IZX, 6 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "BIT", BIT, ZP0, 3 },{ "AND", AND, ZP0, 3 },{ "ROL", ROL, ZP0, 5 },{ "???", XXX, IMP, 5 },{ "PLP", PLP, IMP, 4 },{ "AND", AND, IMM, 2 },{ "ROL", ROL, IMP, 2 },{ "???", XXX, IMP, 2 },{ "BIT", BIT, ABS, 4 },{ "AND", AND, ABS, 4 },{ "ROL", ROL, ABS, 6 },{ "???", XXX, IMP, 6 },
	{ "BMI", BMI, REL, 2 },{ "AND", AND, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "NOP", NOP, IMP, 4 },{ "AND", AND, ZPX, 4 },{ "ROL", ROL, ZPX, 6 },{ "???", XXX, IMP, 6 },{ "SEC", SEC, IMP, 2 },{ "AND", AND, ABY, 4 },{ "NOP", NOP, IMP, 2 },{ "???", XXX, IMP, 7 },{ "???", NOP, IMP, 4 },{ "AND", AND, ABX, 4 },{ "ROL", ROL, ABX, 7 },{ "???", XXX, IMP, 7 },
	{ "RTI", RTI, IMP, 6 },{ "EOR", EOR, IZX, 6 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "NOP", NOP, IMP, 3 },{ "EOR", EOR, ZP0, 3 },{ "LSR", LSR, ZP0, 5 },{ "???", XXX, IMP, 5 },{ "PHA", PHA, IMP, 3 },{ "EOR", EOR, IMM, 2 },{ "LSR", LSR, IMP, 2 },{ "???", XXX, IMP, 2 },{ "JMP", JMP, ABS, 3 },{ "EOR", EOR, ABS, 4 },{ "LSR", LSR, ABS, 6 },{ "???", XXX, IMP, 6 },
	{ "BVC", BVC, REL, 2 },{ "EOR", EOR, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "NOP", NOP, IMP, 4 },{ "EOR", EOR, ZPX, 4 },{ "LSR", LSR, ZPX, 6 },{ "???", XXX, IMP, 6 },{ "CLI", CLI, IMP, 2 },{ "EOR", EOR, ABY, 4 },{ "NOP", NOP, IMP, 2 },{ "???", XXX, IMP, 7 },{ "???", NOP, IMP, 4 },{ "EOR", EOR, ABX, 4 },{ "LSR", LSR, ABX, 7 },{ "???", XXX, IMP, 7 },
	{ "RTS", RTS, IMP, 6 },{ "ADC", ADC, IZX, 6 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "NOP", NOP, IMP, 3 },{ "ADC", ADC, ZP0, 3 },{ "ROR", ROR, ZP0, 5 },{ "???", XXX, IMP, 5 },{ "PLA", PLA, IMP, 4 },{ "ADC", ADC, IMM, 2 },{ "ROR", ROR, IMP, 2 },{ "???", XXX, IMP, 2 },{ "JMP", JMP, IND, 5 },{ "ADC", ADC, ABS, 4 },{ "ROR", ROR, ABS, 6 },{ "???", XXX, IMP, 6 },
	{ "BVS", BVS, REL, 2 },{ "ADC", ADC, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "NOP", NOP, IMP, 4 },{ "ADC", ADC, ZPX, 4 },{ "ROR", ROR, ZPX, 6 },{ "???", XXX, IMP, 6 },{ "SEI", SEI, IMP, 2 },{ "ADC", ADC, ABY, 4 },{ "NOP", NOP, IMP, 2 },{ "???", XXX, IMP, 7 },{ "???", NOP, IMP, 4 },{ "ADC", ADC, ABX, 4 },{ "ROR", ROR, ABX, 7 },{ "???", XXX, IMP, 7 },
	{ "NOP", NOP, IMP, 2 },{ "STA", STA, IZX, 6 },{ "NOP", NOP, IMP, 2 },{ "???", XXX, IMP, 6 },{ "STY", STY, ZP0, 3 },{ "STA", STA, ZP0, 3 },{ "STX", STX, ZP0, 3 },{ "???", XXX, IMP, 3 },{ "DEY", DEY, IMP, 2 },{ "NOP", NOP, IMP, 2 },{ "TXA", TXA, IMP, 2 },{ "???", XXX, IMP, 2 },{ "STY", STY, ABS, 4 },{ "STA", STA, ABS, 4 },{ "STX", STX, ABS, 4 },{ "???", XXX, IMP, 4 },
	{ "BCC", BCC, REL, 2 },{ "STA", STA, IZY, 6 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 6 },{ "STY", STY, ZPX, 4 },{ "STA", STA, ZPX, 4 },{ "STX", STX, ZPY, 4 },{ "???", XXX, IMP, 4 },{ "TYA", TYA, IMP, 2 },{ "STA", STA, ABY, 5 },{ "TXS", TXS, IMP, 2 },{ "???", XXX, IMP, 5 },{ "???", NOP, IMP, 5 },{ "STA", STA, ABX, 5 },{ "???", XXX, IMP, 5 },{ "???", XXX, IMP, 5 },
	{ "LDY", LDY, IMM, 2 },{ "LDA", LDA, IZX, 6 },{ "LDX", LDX, IMM, 2 },{ "???", XXX, IMP, 6 },{ "LDY", LDY, ZP0, 3 },{ "LDA", LDA, ZP0, 3 },{ "LDX", LDX, ZP0, 3 },{ "???", XXX, IMP, 3 },{ "TAY", TAY, IMP, 2 },{ "LDA", LDA, IMM, 2 },{ "TAX", TAX, IMP, 2 },{ "???", XXX, IMP, 2 },{ "LDY", LDY, ABS, 4 },{ "LDA", LDA, ABS, 4 },{ "LDX", LDX, ABS, 4 },{ "???", XXX, IMP, 4 },
	{ "BCS", BCS, REL, 2 },{ "LDA", LDA, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 5 },{ "LDY", LDY, ZPX, 4 },{ "LDA", LDA, ZPX, 4 },{ "LDX", LDX, ZPY, 4 },{ "???", XXX, IMP, 4 },{ "CLV", CLV, IMP, 2 },{ "LDA", LDA, ABY, 4 },{ "TSX", TSX, IMP, 2 },{ "???", XXX, IMP, 4 },{ "LDY", LDY, ABX, 4 },{ "LDA", LDA, ABX, 4 },{ "LDX", LDX, ABY, 4 },{ "???", XXX, IMP, 4 },
	{ "CPY", CPY, IMM, 2 },{ "CMP", CMP, IZX, 6 },{ "NOP", NOP, IMP, 2 },{ "???", XXX, IMP, 8 },{ "CPY", CPY, ZP0, 3 },{ "CMP", CMP, ZP0, 3 },{ "DEC", DEC, ZP0, 5 },{ "???", XXX, IMP, 5 },{ "INY", INY, IMP, 2 },{ "CMP", CMP, IMM, 2 },{ "DEX", DEX, IMP, 2 },{ "???", XXX, IMP, 2 },{ "CPY", CPY, ABS, 4 },{ "CMP", CMP, ABS, 4 },{ "DEC", DEC, ABS, 6 },{ "???", XXX, IMP, 6 },
	{ "BNE", BNE, REL, 2 },{ "CMP", CMP, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "NOP", NOP, IMP, 4 },{ "CMP", CMP, ZPX, 4 },{ "DEC", DEC, ZPX, 6 },{ "???", XXX, IMP, 6 },{ "CLD", CLD, IMP, 2 },{ "CMP", CMP, ABY, 4 },{ "NOP", NOP, IMP, 2 },{ "???", XXX, IMP, 7 },{ "???", NOP, IMP, 4 },{ "CMP", CMP, ABX, 4 },{ "DEC", DEC, ABX, 7 },{ "???", XXX, IMP, 7 },
	{ "CPX", CPX, IMM, 2 },{ "SBC", SBC, IZX, 6 },{ "NOP", NOP, IMP, 2 },{ "???", XXX, IMP, 8 },{ "CPX", CPX, ZP0, 3 },{ "SBC", SBC, ZP0, 3 },{ "INC", INC, ZP0, 5 },{ "???", XXX, IMP, 5 },{ "INX", INX, IMP, 2 },{ "SBC", SBC, IMM, 2 },{ "NOP", NOP, IMP, 2 },{ "???", SBC, IMP, 2 },{ "CPX", CPX, ABS, 4 },{ "SBC", SBC, ABS, 4 },{ "INC", INC, ABS, 6 },{ "???", XXX, IMP, 6 },
	{ "BEQ", BEQ, REL, 2 },{ "SBC", SBC, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "NOP", NOP, IMP, 4 },{ "SBC", SBC, ZPX, 4 },{ "INC", INC, ZPX, 6 },{ "???", XXX, IMP, 6 },{ "SED", SED, IMP, 2 },{ "SBC", SBC, ABY, 4 },{ "NOP", NOP, IMP, 2 },{ "???", XXX, IMP, 7 },{ "???", NOP, IMP, 4 },{ "SBC", SBC, ABX, 4 },{ "INC", INC, ABX, 7 },{ "???", XXX, IMP, 7 }
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
        opcode = read_data(PC++);
        uint8_t add_cycle1 = instructions[opcode].addrmode();
        uint8_t add_cycle2 = instructions[opcode].operation(); 
        cycles = instructions[opcode].cycles + (add_cycle1 & add_cycle2); 
    }
    cycles--;
}

void set_flag(CPUFLAGS f, char c) {
    if (c) SR |=  f;
    else   SR &= ~f;
}

uint8_t get_flag(CPUFLAGS f) {
    return SR & f;
}

void reset() {
    AC = 0x00;
    X = 0x00;
    Y = 0x00;
    SP = 0xFD;
    SR = 0x00 | U;

    addr_abs = 0xFFFC;
    uint16_t lo = read_data(addr_abs + 0);
    uint16_t hi = read_data(addr_abs + 1);
    PC = (hi << 8) | lo;

    addr_abs = 0x0000;
    addr_rel = 0x0000;
    data = 0x00;

    cycles = 8;
}

void irq() {
    if (!get_flag(I)) {
        write_data(0x0100 + SP--, (PC >> 8) & 0x00FF);
        write_data(0x0100 + SP--, PC & 0x00FF);

        set_flag(B, 0);
        set_flag(U, 1);
        set_flag(I, 1);
        write_data(0x0100 + SP--, SR);

        addr_abs = 0xFFFE;
        uint16_t lo = read_data(addr_abs + 0);
        uint16_t hi = read_data(addr_abs + 1);
        PC = (hi << 8) | lo;

        cycles = 7;
    }
}

void nmi() {
    write_data(0x0100 + SP--, (PC >> 8) & 0x00FF);
    write_data(0x0100 + SP--, PC & 0x00FF);

    set_flag(B, 0);
    set_flag(U, 1);
    set_flag(I, 1);
    write_data(0x0100 + SP--, SR);

    addr_abs = 0xFFFA;
    uint16_t lo = read_data(addr_abs + 0);
    uint16_t hi = read_data(addr_abs + 1);
    PC = (hi << 8) | lo;

    cycles = 8;   
}

void cpu_init() {
    bus_init();
    reset();
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

// ADDRESSING MODES

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

// INSTRUCTIONS

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
    set_flag(V, (~((uint16_t)AC ^ (uint16_t)data) &
                ((uint16_t)AC ^ (uint16_t)temp)) & 0x0080);
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
    uint16_t temp = (uint16_t)get_data() << 1;
    set_flag(C, (temp & 0xFF00) > 0);
    set_flag(Z, !(temp & 0x00FF));
    set_flag(N, temp & 0x0080);
    if (instructions[opcode].addrmode == IMP)
        AC = temp & 0x00FF;
    else
        write_data(addr_abs, temp & 0x00FF);
    return 0;
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
    uint8_t temp = AC & get_data();	
    set_flag(Z, !(temp & 0x00FF));
    set_flag(N, data & (1 << 7));
    set_flag(V, data & (1 << 6));
    return 0;
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
    PC++;

    set_flag(I, 1);
    write_data(0x0100 + SP--, (PC >> 8) & 0x00FF);
    write_data(0x0100 + SP--, PC & 0x00FF);

    set_flag(B, 1);
    write_data(0x0100 + SP--, SR);
    set_flag(B, 0);

    PC = (uint16_t)read_data(0xFFFE) | ((uint16_t)read_data(0xFFFF) << 8);
    return 0;
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
    set_flag(I, 0);
    return 0;
}

uint8_t CLV() {
	set_flag(V, 0);
    return 0;
}

uint8_t CMP() {
    uint16_t temp = (uint16_t)AC - (uint16_t)get_data();
    set_flag(C, AC >= data);
    set_flag(Z, !(temp & 0x00FF));
    set_flag(N, temp & 0x0080);
    return 1;
}

uint8_t CPX() {
	uint16_t temp = (uint16_t)X - (uint16_t)get_data();
    set_flag(C, X >= data);
    set_flag(Z, !(temp & 0x00FF));
    set_flag(N, temp & 0x0080);
    return 0;

}

uint8_t CPY() {
	uint16_t temp = (uint16_t)Y - (uint16_t)get_data();
    set_flag(C, Y >= data);
    set_flag(Z, !(temp & 0x00FF));
    set_flag(N, temp & 0x0080);
    return 0;

}

uint8_t DEC() {
    uint8_t temp = get_data() - 1;
    write_data(addr_abs, temp);
    set_flag(Z, !temp);
    set_flag(N, temp & 0x80);
    return 0;
}

uint8_t DEX() {
	X--;
    set_flag(Z, !X);
    set_flag(N, X & 0x80);
    return 0;
}

uint8_t DEY() {
	Y--;
    set_flag(Z, !Y);
    set_flag(N, Y & 0x80);
    return 0;
}

uint8_t EOR() {
	AC = AC ^ get_data();
    set_flag(Z, !AC);
    set_flag(N, AC & 0x80);
    return 1;
}

uint8_t INC() {
	uint8_t temp = get_data() + 1;
    write_data(addr_abs, temp);
    set_flag(Z, !temp);
    set_flag(N, temp & 0x80);
    return 0;
}

uint8_t INX() {
	X++;
    set_flag(Z, !X);
    set_flag(N, X & 0x80);
    return 0;
}

uint8_t INY() {
	Y++;
    set_flag(Z, !Y);
    set_flag(N, Y & 0x80);
    return 0;
}

uint8_t JMP() {
	PC = addr_abs;
    return 0;
}

uint8_t JSR() {
	PC--;
    write_data(0x0100 + SP--, (PC >> 8) & 0x00FF);
    write_data(0x0100 + SP--, PC & 0x00FF);

    PC = addr_abs;
    return 0;
}

uint8_t LDA() {
	AC = get_data();
    set_flag(Z, !AC);
    set_flag(N, AC & 0x80);
    return 1;
}

uint8_t LDX() {
	X = get_data();
    set_flag(Z, !X);
    set_flag(N, X & 0x80);
    return 1;
}

uint8_t LDY() {
	Y = get_data();
    set_flag(Z, !Y);
    set_flag(N, Y & 0x80);
    return 1;
}

uint8_t LSR() {
    get_data();
    set_flag(C, data & 0x01);

    uint8_t temp = data >> 1;
    set_flag(Z, !temp);
    set_flag(N, temp & 0x80);
    if (instructions[opcode].addrmode == IMP)
        AC = temp;
    else
        write_data(addr_abs, temp);
    return 0;
}

uint8_t NOP() {
	switch (opcode) {
        case 0x1C:
        case 0x3C:
        case 0x5C:
        case 0x7C:
        case 0xDC:
        case 0xFC:
            return 1;
    }
    return 0;
}

uint8_t ORA() {
	AC = AC | get_data();
    set_flag(Z, !AC);
    set_flag(N, AC & 0x80);
    return 1;
}

uint8_t PHA() {
    write_data(0x0100 + SP--, AC);
    return 0;
}

uint8_t PHP() {
	write_data(0x0100 + SP--, SR | B);
    set_flag(B, 0);
    return 0;
}

uint8_t PLA() {
	AC = read_data(0x0100 + SP++);
    set_flag(Z, !AC);
    set_flag(N, AC & 0x80);
    return 0;
}

uint8_t PLP() {
	SR = read_data(0x0100 + ++SP);
    return 0;
}

uint8_t ROL() {
	uint16_t temp = (uint16_t)(get_data() << 1) | get_flag(C);
    set_flag(C, temp & 0xFF00 > 0);
    set_flag(Z, !(temp & 0x00FF));
    set_flag(N, temp & 0x0080);
    if (instructions[opcode].addrmode == IMP)
        AC = temp & 0x00FF;
    else
        write_data(addr_abs, temp & 0x00FF);
    return 0;
}

uint8_t ROR() {
	uint8_t temp = (get_flag(C) << 7) | (get_data() >> 1);
    set_flag(C, data & 0x01);
    set_flag(Z, !temp);
    set_flag(N, temp & 0x80);
    if (instructions[opcode].addrmode == IMP)
        AC = temp;
    else
        write_data(addr_abs, temp);
    return 0;
}

uint8_t RTI() {
    SR = read_data(0x0100 + ++SP);
    toggle_flag(B);
    toggle_flag(U);

    PC = (uint16_t)read_data(0x0100 + ++SP);
    PC |= (uint16_t)read_data(0x0100 + ++SP) << 8;
    return 0;
}

uint8_t RTS() {
    PC = (uint16_t)read_data(0x0100 + ++SP);
    PC |= (uint16_t)read_data(0x0100 + ++SP) << 8;
    PC++;
    return 0;
}

uint8_t SBC() {
    get_data();
    uint16_t not_data = ((uint16_t)data) ^ 0x00FF;
    uint16_t temp = (uint16_t)AC + not_data + (uint16_t)get_flag(C);
    set_flag(C, temp > 255);
    set_flag(Z, !(temp & 0x00FF));
    set_flag(V, (temp ^ (uint16_t)AC)  & (temp ^ not_data) & 0x0080);
    set_flag(N, temp & 0x0080);
    AC = temp & 0x00FF;
    return 1;
}

uint8_t SEC() {
    set_flag(C, 1);
    return 0;
}

uint8_t SED() {
	set_flag(D, 1);
    return 0;
}

uint8_t SEI() {
	set_flag(I, 1);
}

uint8_t STA() {
	write_data(addr_abs, AC);
    return 0;
}

uint8_t STX() {
	write_data(addr_abs, X);
    return 0;
}

uint8_t STY() {
	write_data(addr_abs, Y);
    return 0;
}

uint8_t TAX() {
	X = AC;
    set_flag(Z, !X);
    set_flag(N, X & 0x80);
    return 0;
}

uint8_t TAY() {
	Y = AC;
    set_flag(Z, !Y);
    set_flag(N, Y & 0x80);
    return 0;
}

uint8_t TSX() {
	X = SP;
    set_flag(Z, !X);
    set_flag(N, X & 0x80);
    return 0;
}

uint8_t TXA() {
	AC = X;
    set_flag(Z, !AC);
    set_flag(N, AC & 0x80);
    return 0;
}

uint8_t TXS() {
	SP = X;
    return 0;
}

uint8_t TYA() {
	AC = Y;
    set_flag(Z, !AC);
    set_flag(N, AC & 0x80);
    return 0;
}

uint8_t XXX() { // ILLEGAL
    return 0;
}

