#include "bus.h"
#include <string.h>

#define RAM_SIZE 64*1024 // 64kB RAM
static uint8_t ram[RAM_SIZE];

void bus_init() {
    memset(ram, 0, RAM_SIZE);
}

void bus_write(uint16_t addr, uint8_t data) {
    if (addr >= 0x0000 && addr <= 0xFFFF) {
        ram[addr] = data;
    }
}

uint8_t bus_read(uint16_t addr) {
    if (addr >= 0x0000 && addr <= 0xFFFF) {
        return ram[addr];
    }
    return 0x00;
}
