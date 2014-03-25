#  Makefile for SearchSrc

F77 = g77 -Wall -Wimplicit -fno-backslash

# RCWOBJS = julda.o len1.o getnum.o getline.o nxtwrd.o
# OBJS    = rdvla.o rdgeo.o adjvla.o wrtloc.o wrtdat.o getnv.o 

newsrc:  searchsrc.o
	$(F77) -o searchsrc searchsrc.o  \
        /home/noatak/cwalker/code/lib/LINUX64/libcit.a \
        /home/noatak/cwalker/code/lib/LINUX64/librcw.a \
        /home/noatak/cwalker/code/lib/LINUX64/slalib.a

.f.o:
	$(F77) -c -Wimplicit $<

$(OBJS):  rdcat.inc  searchsrc.inc

searchsrc.o:  rdcat.inc  searchsrc.inc
