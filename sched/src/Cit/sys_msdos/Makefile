SRCS = $(wildcard *.for)
OBJS = $(SRCS:.for=.o)

RM = /bin/rm -f

all: $(OBJS)

clean:
	$(RM) $(OBJS)

.SUFFIXES: .for

.for.o:
	$(FC) $(FFLAGS) -c $<
