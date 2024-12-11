SRCS = $(wildcard *.f)
OBJS = $(SRCS:.f=.o)

RDCAT_SRCS = $(shell grep -l rdcat.inc *.f)
RDCAT_OBJS = $(RDCAT_SRCS:.f=.o)

RM = /bin/rm -f

all: $(OBJS)

$(RDCAT_OBJS): rdcat.inc

clean:
	$(RM) $(OBJS)
