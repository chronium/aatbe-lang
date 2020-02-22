HOST = i686-elf

AS = $(HOST)-as
CC = $(HOST)-gcc

AATBOOT = target/debug/aatboot
AATC = clang-6.0

OBJs:=\
	boot.o \
	kern.o

all: kern.bin

kern.bin: $(OBJs)
	$(CC) -T link.ld -o $@ -ffreestanding -O2 -nostdlib $(OBJs) -lgcc

%.o: %.S
	$(AS) $< -o $@

%.ll: %.aat
	$(AATBOOT) $< --emit-llvm $@

%.o: %.ll
	$(AATC) $< -ffreestanding -c -o $@ -target i386-unknown-none-elf

run: all
	qemu-system-i386 -kernel kern.bin
