#  Makefile for makesta.  Use gmake.
#  This is a very slightly modified version of makesetup

SRCS = $(wildcard *.f)
OBJS = $(SRCS:.f=.o)

MAKESET_SRCS = $(shell grep -l makeset.inc *.f)
MAKESET_OBJS = $(MAKESET_SRCS:.f=.o)

RM = /bin/rm -f

FC= f77 -C -g -u
 
all: makesta
 
makesta:  $(OBJS)
	$(FC) -o makesta $(OBJS) \
        ../sublib/getline.o ../sublib/nxtwrd.o \
        ../sched/src/Cit/len1.o ../sched/src/Cit/upcase.o \
          -lX11

.f.o:
	$(FC) -c $<

$(MAKESET_OBJS): makeset.inc

clean:
	$(RM) $(OBJS)
