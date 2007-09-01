#
#    makefile for inserttex
#
SRCS = $(wildcard *.f)
OBJS = $(SRCS:.f=.o)

FC = g77 -Wall -Wimplicit -fno-backslash

inserttex:  inserttex.o $(OBJS)
	$(FC) -o inserttex $(OBJS) \
           /home/eolus/cwalker/code/Lib_linux/sublib/libcit.a

.f.o:
	$(FC) -c $<

