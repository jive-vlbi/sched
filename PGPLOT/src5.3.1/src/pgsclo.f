C*PGSCLO -- set contour label orientation
C%void cpgsclo(int type);
C+
      SUBROUTINE PGSCLO (TYPE)
      INTEGER  TYPE
C
C This routine controls the orientation of labels drawn by PGCONL.
C There are three options: 0 - label is written aligned with the
C uphill direction (default); 1 - labels are written aligned with the 
C downhill direction; 2 - orientation is chosen to align the
C labels as closely as possible with the vertical on the page. 
C
C Argument:
C  TYPE   (input)  : 0: uphill alignment
C                    1: downhill alignment
C                    2: upright alignment
C--
C 13-Mar-2003 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGSCLO')) RETURN
      PGCLAN = TYPE
      END
