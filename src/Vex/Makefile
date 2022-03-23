SRCS = $(wildcard *.f)
OBJS = $(SRCS:.f=.o)

VXLINK_SRCS = $(shell grep -l vxlink.inc *.f)
VXLINK_OBJS = $(VXLINK_SRCS:.f=.o)

SCHED_SRCS = $(shell grep -l sched.inc *.f)
SCHED_OBJS = $(SCHED_SRCS:.f=.o)

SCHSET_SRCS = $(shell grep -l schset.inc *.f)
SCHSET_OBJS = $(SCHSET_SRCS:.f=.o)

RM = /bin/rm -f

all: $(OBJS)

$(VXLINK_OBJS): vxlink.inc
$(SCHED_OBJS): sched.inc
$(SCHSET_OBJS): schset.inc

clean:
	$(RM) $(OBJS)
