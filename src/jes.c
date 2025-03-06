#include <stdio.h>
#include "cpu6502.h"

int main() {
    printf("Hello JES!\n\n");
    cpu_init();

    write_data(0xFFFC, 0x00);
    write_data(0xFFFD, 0xFF);
    reset();

    write_data(0xFF00, 0xA9);
    write_data(0xFF01, 42);
    write_data(0xFF02, 0xA2);
    write_data(0xFF03, 69);

    while (get_X() != 69) {
        clock();
    }

    printf("PC: 0x%04X\n", get_PC());
    printf("AC: 0x%02X (%d)\n", get_AC(), get_AC());
    printf(" X: 0x%02X (%d)\n", get_X(), get_X());

	return 0;
}
