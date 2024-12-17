C*PROGNM -- return file name of current program [VAX/VMS]
C+
      SUBROUTINE PROGNM (PROG)
      CHARACTER*(*) PROG
C
C Return the file name of the currently executing program.
C
C Argument:
C  PROG   (output) : receives the file name.
C
C Subroutines required:
C  LIB$GETJPI (VMS)
C
C History:
C  1988 Jun 9 - TJP.
C-----------------------------------------------------------------------
      INCLUDE '($JPIDEF)'
      INTEGER  IER, L
      INTEGER  LIB$GETJPI
C
      IER = LIB$GETJPI(JPI$_IMAGNAME, , , , PROG, L)
      I = INDEX(PROG,']')
      PROG = PROG(I+1:)
      I = INDEX(PROG,'.')
      PROG = PROG(:I-1)
C
      END
