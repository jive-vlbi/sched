#
#    makefile for testgeo
#
#    will not pay attention to the currency of the cit routines
#    or the routines in the sublib.
#
#unsetenv LD_LIBRARY_PATH
OBJ=  sla_geoc.o geoxyz.o

f77=     f77 -Bstatic -C -g -u

testgeo:  testgeo.o $(OBJ)
	$(f77)  -o testgeo testgeo.o $(OBJ) \
           /home/pingora/cwalker/code/sublib/cit/citsubs.o \
           /home/pingora/cwalker/code/sublib/cit/citunix.o \
           /home/pingora/cwalker/code/sublib/cit/citsun.o

.f.o:
	$(f77) -u -c $<

