#  Makefile for makesetup.  Use gmake.

LIBDIR = /home/eolus/cwalker/code/Lib_linux
SCHDIR = /home/eolus/cwalker/code/sched/src/

SRCS = $(wildcard *.f)
OBJS = $(SRCS:.f=.o)

MAKESET_SRCS = $(shell grep -l makeset.inc *.f)
MAKESET_OBJS = $(MAKESET_SRCS:.f=.o)

RM = /bin/rm -f

FC = g77 -Wall -Wimplicit -fno-backslash
 
all: makesetup
 
makesetup:  makesetup.o $(OBJS)
	$(FC) -o makesetup $(OBJS) \
        $(LIBDIR)/sublib/getline.o \
        $(LIBDIR)/sublib/nxtwrd.o \
        $(SCHDIR)/Cit/len1.o $(SCHDIR)Cit/upcase.o


.f.o:
	$(FC) -c $<

$(MAKESET_OBJS): makeset.inc

clean:
	$(RM) $(OBJS)
