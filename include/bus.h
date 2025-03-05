#ifndef BUS_H
#define BUS_H

#include <stdint.h>
#include <stdbool.h>

void bus_init();
void bus_write(uint16_t addr, uint8_t data);
uint8_t bus_read(uint16_t addr);

#endif // BUS_H
