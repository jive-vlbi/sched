#  Makefile for newsrc

F77 = g77 -Wall -Wimplicit -fno-backslash

# RCWOBJS = julda.o len1.o getnum.o getline.o nxtwrd.o
# OBJS    = rdvla.o rdgeo.o adjvla.o wrtloc.o wrtdat.o getnv.o 
OBJS = newsrc.o newsrc.o getuin.o getold.o getnew.o fixfile.o

newsrc:  $(OBJS)
	$(F77) -o newsrc newsrc.o getuin.o getold.o getnew.o fixfile.o \
        /home/noatak/cwalker/code/lib/LINUX64/libcit.a \
        /home/noatak/cwalker/code/lib/LINUX64/librcw.a

.f.o:
	$(F77) -c -Wimplicit $<

$(OBJS):  rdcat.inc  newsrc.inc

newsrc.o:  rdcat.inc  newsrc.inc
