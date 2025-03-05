#ifndef CPU6502_H
#define CPU6502_H

#include <stdint.h>

void     cpu_init();
void   write_data(uint16_t addr, uint8_t data);
uint8_t read_data(uint16_t addr);

#endif // CPU6502_H
