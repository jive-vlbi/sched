C*PGCONL -- label contour map of a 2D data array
C%void cpgconl(const float *a, int idim, int jdim, int i1, int i2, \
C% int j1, int j2, float c, const float *tr, const char *label, \
C% int intval, int minint);
C+
      SUBROUTINE PGCONL (A, IDIM, JDIM, I1, I2, J1, J2, C, TR,
     1                   LABEL, INTVAL, MININT)
      INTEGER IDIM, JDIM, I1, J1, I2, J2, INTVAL, MININT
      REAL A(IDIM,JDIM), C, TR(6)
      CHARACTER*(*) LABEL
C
C Label a contour map drawn with routine PGCONT. Routine PGCONT should
C be called first to draw the contour lines, then this routine should be
C called to add the labels. Labels are written at intervals along the
C contour lines, centered on the contour lines. Labels are opaque, so a
C part of the underlying contour line is obscured by the label. Labels
C use the current attributes (character height, line width, color index,
C character font).
C
C By default, labels will be drawn aligned with the uphill direction.
C Use routine PGSCLO to select alternate alignments, e.g.,
C CALL PGSCLO(2) to request that the labels be closer to upright
C on the page.
C
C The first 7 arguments, and the 9th argument (TR), are the same as
C the corresponding arguments to PGCONT, and they should normally be
C identical to those used with PGCONT. Note that the argument C is not
C an array (unlike PGCONT): only one contour level can be specified;
C to label more contours, call PGCONL for each level.
C
C The label is supplied as a character string in argument LABEL.
C
C The spacing of labels along the contour is specified by parameters
C INTVAL and MININT. The routine follows the contour through the
C array, counting the number of cells that the contour crosses. The
C first label will be written in the MININT'th cell, and additional
C labels will be written every INTVAL cells thereafter. A contour
C that crosses less than MININT cells will not be labelled. Some
C experimentation may be needed to get satisfactory results; a good
C place to start is INTVAL=20, MININT=10.
C
C Arguments:
C  A      (input) : data array.
C  IDIM   (input) : first dimension of A.
C  JDIM   (input) : second dimension of A.
C  I1, I2 (input) : range of first index to be contoured (inclusive).
C  J1, J2 (input) : range of second index to be contoured (inclusive).
C  C      (input) : the level of the contour to be labelled (one of the
C                   values given to PGCONT).
C  TR     (input) : array defining a transformation between the I,J
C                   grid of the array and the world coordinates.
C                   The world coordinates of the array point A(I,J)
C                   are given by:
C                     X = TR(1) + TR(2)*I + TR(3)*J
C                     Y = TR(4) + TR(5)*I + TR(6)*J
C                   Usually TR(3) and TR(5) are zero - unless the
C                   coordinate transformation involves a rotation or
C                   shear.
C  LABEL  (input) : character strings to be used to label the specified
C                   contour. Leading and trailing blank spaces are
C                   ignored.
C  INTVAL (input) : spacing along the contour between labels, in
C                   grid cells.
C  MININT (input) : contours that cross less than MININT cells
C                   will not be labelled.
C--
C  5-May-1994 - New routine; this routine is virtually identical to
C               PGCONT, but calls PGCONX with a different external
C               routine [TJP].
C  4-Feb-1997 - PGCONX requires an array argument, not scalar [TJP].
C 22-Sep-2001 - correct error in header comments [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      INTEGER  I
      LOGICAL  PGNOTO
      REAL     CL(1)
      EXTERNAL PGCL
C
      IF (PGNOTO('PGCONL')) RETURN
C
C Save TRANS matrix and other parameters.
C
      DO 10 I=1,6
          TRANS(I) = TR(I)
   10 CONTINUE
      PGCINT = INTVAL
      PGCMIN = MININT
      PGCLAB = LABEL
C
C Use PGCONX with external function PGCL.
C
      CL(1) = C
      CALL PGCONX (A, IDIM, JDIM, I1, I2, J1, J2, CL, 1, PGCL)
C
      END
