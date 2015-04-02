
CC=gcc
LD=gcc
OPT_FLAGS=-Og -flto
CFLAGS=-g $(OPT_FLAGS) -I$(EPIPHANY_HOME)/tools/host/include -Wall
LDFLAGS=$(OPT_FLAGS) -fwhole-program -L$(EPIPHANY_HOME)/tools/host/lib -le-hal
PUMP_OBJS=pump.o

all: pump

pump: $(PUMP_OBJS)
	$(LD) -o $@ $< $(LDFLAGS)

%.o: %.c slave_syms.h
	$(CC) $(CFLAGS) -c -o $@ $<

slave_syms.h: ~/otp/bin/epiphany-unknown-elf/beam
	~/otp/erts/emulator/utils/make_slave_syms

clean:
	rm -f pump $(PUMP_OBJS) slave_syms.h
