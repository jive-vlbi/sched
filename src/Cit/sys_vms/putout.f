C*PUTOUT -- write line on standard output [VAX/VMS]
C+
      SUBROUTINE PUTOUT(TEXT)
      CHARACTER*(*) TEXT
C
C This subroutine writes one line on the standard output; the text to be
C written is supplied as a character-string argument.
C
C Argument:
C  TEXT   (input)  : character string for output.
C
C Subroutines required:
C  LIB$PUT_OUTPUT  [VMS]
C
C History:
C  1987 Nov 11 - TJP
C
C-----------------------------------------------------------------------
      INTEGER  LIB$PUT_OUTPUT, JUNK, LEN1, L1
C
      L1 = LEN1(TEXT)
      JUNK = LIB$PUT_OUTPUT(TEXT(1:L1))
      END
