#  Makefile for vla2itrf
#  This one assumes that the required subroutines are local.  I'm
#  doing it on my Mac at the airport.

F77 = g77 -Wall -Wimplicit -fno-backslash

RCWOBJS = julda.o len1.o getnum.o getline.o nxtwrd.o
OBJS    = rdvla.o rdgeo.o adjvla.o wrtloc.o wrtdat.o getnv.o 

newloc:  newloc.o $(RCWOBJS) $(OBJS) 
	$(F77) -o newloc newloc.o $(RCWOBJS) $(OBJS) \
        /home/noatak/cwalker/code/lib/LINUX64/libcit.a \
        /home/noatak/cwalker/code/lib/LINUX64/slalib.a

.f.o:
	$(F77) -c -Wimplicit $<

$(OBJS):  rdcat.inc  newloc.inc

newloc.o:  rdcat.inc  newloc.inc
