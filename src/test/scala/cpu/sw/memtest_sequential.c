#include <stdint.h>
#include <stddef.h>

extern void __attribute__((naked)) __attribute__((section(".isr_vector"))) isr_vector(void)
{
    asm volatile ("j _start");
    asm volatile ("j _start");
}

void __attribute__((noreturn)) main(void);

extern void __attribute__((naked)) _start(void)
{
    asm volatile ("la sp, stack_top");
    main();
}

static volatile uint32_t* const REG_GPIO_OUT = (volatile uint32_t*)0x40000000;
#define PSRAM_LENGTH_BYTES (128*4)
static uint32_t* const PSRAM_START = (uint32_t*)0xa0000000;                         // PSRAMの先頭アドレス

void __attribute__((noreturn)) main(void)
{
    *REG_GPIO_OUT = 0;

    // Fill
    uint32_t* p = PSRAM_START;
    for(uint32_t i = 0; i < PSRAM_LENGTH_BYTES/4; i++, p++ ) {
        uint32_t v = ((i+1) << 16) | (i << 16);
        *p = v;
    }
    
    *REG_GPIO_OUT = 2;
    // Sequential read
    for(uint32_t i = 0; i < PSRAM_LENGTH_BYTES/4; i++, p++ ) {
        uint32_t expected = ((i+1) << 16) | (i << 16);
        uint32_t actual = *(PSRAM_START + i);
        if( expected != actual ) {
            *REG_GPIO_OUT = 0;  // successフラグをクリア.
            break;
        }
    }
    *REG_GPIO_OUT |= 1; // exit
    for(;;);
}
