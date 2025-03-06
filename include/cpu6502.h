#ifndef CPU6502_H
#define CPU6502_H

#include <stdint.h>

void cpu_init();
void reset();
void clock();
void write_data(uint16_t, uint8_t);
uint8_t read_data(uint16_t);

uint8_t  get_AC();
uint8_t  get_X() ;
uint8_t  get_Y() ;
uint8_t  get_SR();
uint8_t  get_SP();
uint16_t get_PC();


#endif // CPU6502_H
