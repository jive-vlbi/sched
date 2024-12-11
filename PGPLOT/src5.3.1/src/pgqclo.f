C*PGQCLO -- inquire contour label orientation
C%void cpgqclo(int *type);
C+
      SUBROUTINE PGQCLO (TYPE)
      INTEGER  TYPE
C
C This routine returns the current value of the contour label
C orientation parameter set by routine PGSCLO.
C
C Argument:
C  TYPE   (output) : 0: uphill alignment
C                    1: downhill alignment
C                    2: upright alignment
C--
C 13-Mar-2003 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGQCLO')) THEN
         TYPE = 0
      ELSE
         TYPE = PGCLAN
      END IF
      END
