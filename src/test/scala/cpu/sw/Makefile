.PHONY: all clean

CC = riscv64-unknown-elf-gcc
OBJDUMP = riscv64-unknown-elf-objdump
OBJCOPY = riscv64-unknown-elf-objcopy
CFLAGS = -mabi=ilp32 -march=rv32i -Os -g -std=gnu11
LDFLAGS = -Wl,-Tlink.ld -nostartfiles 

TARGETS := memtest_random memtest_sequential

memtest_random_OBJS := memtest_random.o
memtest_sequential_OBJS := memtest_sequential.o

BUILD_TARGETS := $(addsuffix .elf,$(TARGETS)) \
				 $(addsuffix .bin,$(TARGETS)) \
				 $(addsuffix .hex,$(TARGETS)) \
				 $(addsuffix _0.hex,$(TARGETS)) \
				 $(addsuffix _1.hex,$(TARGETS)) \
				 $(addsuffix _2.hex,$(TARGETS)) \
				 $(addsuffix _3.hex,$(TARGETS)) \
				 $(addsuffix .dump,$(TARGETS))

all: $(BUILD_TARGETS)

memtest_random.elf: $(memtest_random_OBJS)
memtest_sequential.elf: $(memtest_sequential_OBJS)

%.elf: link.ld
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $($(subst .elf,,$@)_OBJS)

%.o: %.c
	$(CC) -c -o $@ $(CFLAGS) $<

%.bin: %.elf
	$(OBJCOPY) -O binary $< $@

%.hex: %.bin
	od -An -tx4 -w4 -v $< > $@
%.dump: %.elf
	$(OBJDUMP) -dSC $< > $@

%_0.hex: %.hex
	awk '{print substr($$1,7,2)}' $< > $@
%_1.hex: %.hex
	awk '{print substr($$1,5,2)}' $< > $@
%_2.hex: %.hex
	awk '{print substr($$1,3,2)}' $< > $@
%_3.hex: %.hex
	awk '{print substr($$1,1,2)}' $< > $@

clean:
	-@$(RM) *.o *.elf *.bin *.hex *.dump