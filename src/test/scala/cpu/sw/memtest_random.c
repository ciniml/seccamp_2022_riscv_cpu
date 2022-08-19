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

static uint32_t read_random_csr()
{
    uint32_t result;
    asm volatile ("csrr %0, 0x7d0" : "=r" (result));
    return result;
} 

static volatile uint32_t* const REG_GPIO_OUT = (volatile uint32_t*)0x40000000;
#define PSRAM_LENGTH_BYTES (128*4)
static uint32_t* const PSRAM_START = (uint32_t*)0xa0000000;                         // PSRAMの先頭アドレス
static uint32_t mem[PSRAM_LENGTH_BYTES/4];

static uint32_t generate_psram_offset() {
    uint32_t offset = read_random_csr();
    return ((offset >> 16) ^ (offset)) & (PSRAM_LENGTH_BYTES/4 - 1);
}

void __attribute__((noreturn)) main(void)
{
    *REG_GPIO_OUT = 0;

    {
        uint32_t random0 = read_random_csr();
        uint32_t random1 = read_random_csr();
        uint32_t random2 = read_random_csr();
        if( random0 == random1 && random1 == random2 ) {
            // RANDOM CSR does not seem working or implemented.
            *REG_GPIO_OUT |= 1; // exit
            for(;;);
        }
    }

    uint32_t* p = PSRAM_START;
    // Fill
    for(uint32_t i = 0; i < PSRAM_LENGTH_BYTES/4; i++, p++ ) {
        uint32_t v = ((i+1) << 16) | (i << 16);
        *p = v;
        *(mem + i) = v;
    }
    // Random update
    for(uint32_t i = 0; i < PSRAM_LENGTH_BYTES/4; i++, p++ ) {
        uint32_t v = read_random_csr();
        uint32_t offset = generate_psram_offset();
        *(PSRAM_START + offset) = v;
        *(mem + offset) = v;
    }
    
    *REG_GPIO_OUT = 2;
    // Random read
    for(uint32_t i = 0; i < PSRAM_LENGTH_BYTES/4; i++, p++ ) {
        uint32_t offset = generate_psram_offset();
        uint32_t expected = *(mem + offset);
        uint32_t actual = *(PSRAM_START + offset);
        if( expected != actual ) {
            *REG_GPIO_OUT = 0;  // successフラグをクリア.
            break;
        }
    }
    *REG_GPIO_OUT |= 1; // exit
    for(;;);
}
