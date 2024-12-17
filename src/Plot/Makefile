SRCS = $(wildcard *.f)
OBJS = $(SRCS:.f=.o)

RDCAT_SRCS = $(shell grep -l rdcat.inc *.f)
RDCAT_OBJS = $(RDCAT_SRCS:.f=.o)

SCHED_SRCS = $(shell grep -l sched.inc *.f)
SCHED_OBJS = $(SCHED_SRCS:.f=.o)

SCHSET_SRCS = $(shell grep -l schset.inc *.f)
SCHSET_OBJS = $(SCHSET_SRCS:.f=.o)

SCHFREQ_SRCS = $(shell grep -l schfreq.inc *.f)
SCHFREQ_OBJS = $(SCHFREQ_SRCS:.f=.o)

SCHSRL_SRCS = $(shell grep -l srlist.inc *.f)
SCHSRL_OBJS = $(SCHSRL_SRCS:.f=.o)

PLOT_SRCS = $(shell grep -l plot.inc *.f)
PLOT_OBJS = $(PLOT_SRCS:.f=.o)

BEAM_SRCS = $(shell grep -l beam.inc *.f)
BEAM_OBJS = $(BEAM_SRCS:.f=.o)

RM = /bin/rm -f

all: $(OBJS)

$(RDCAT_OBJS): rdcat.inc
$(SCHED_OBJS): sched.inc
$(SCHSET_OBJS): schset.inc
$(SCHFREQ_OBJS): schfreq.inc
$(SCHSRL_OBJS): srlist.inc
$(PLOT_OBJS): plot.inc
$(BEAM_OBJS): beam.inc

clean:
	$(RM) $(OBJS)
