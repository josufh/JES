#include <stdio.h>
#include "cpu6502.h"

int main() {
    printf("Hello JES!\n");

    cpu_init();
    write_data(0x1234, 42);
    uint8_t data = read_data(0x1234);
    printf("Data: %d\n", data);
    
	return 0;
}
