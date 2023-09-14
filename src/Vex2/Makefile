SRCS = $(wildcard *.f)
OBJS = $(SRCS:.f=.o)

VXLINK_SRCS = $(shell grep -l vxlink2.inc *.f)
VXLINK_OBJS = $(VXLINK_SRCS:.f=.o)

SCHED_SRCS = $(shell grep -l sched2.inc *.f)
SCHED_OBJS = $(SCHED_SRCS:.f=.o)

SCHSET_SRCS = $(shell grep -l schset2.inc *.f)
SCHSET_OBJS = $(SCHSET_SRCS:.f=.o)

RM = /bin/rm -f

all: $(OBJS)

$(VXLINK_OBJS): vxlink2.inc
$(SCHED_OBJS): sched2.inc
$(SCHSET_OBJS): schset2.inc

clean:
	$(RM) $(OBJS)
