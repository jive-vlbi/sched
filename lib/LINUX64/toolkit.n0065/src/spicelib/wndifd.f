C$Procedure      WNDIFD ( Difference two DP windows )
 
      SUBROUTINE WNDIFD ( A, B, C )
 
C$ Abstract
C
C      Place the difference of two double precision windows into
C      a third window.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C      WINDOWS
C
C$ Keywords
C
C      WINDOWS
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      DOUBLE PRECISION      A        ( LBCELL:* )
      DOUBLE PRECISION      B        ( LBCELL:* )
      DOUBLE PRECISION      C        ( LBCELL:* )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      A,
C      B          I   Input windows.
C      C          I   Difference of A and B.
C
C$ Detailed_Input
C
C      A,
C      B           are windows, each of which contains zero or more
C                  intervals.
C
C$ Detailed_Output
C
C      C           is the output window, containing the difference
C                  of A and B---every point contained in A, but not
C                  contained in B.
C
C                  C must be distinct from both A and B.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C      1. If the difference of the two windows results in an excess of
C         elements, the error SPICE(WINDOWEXCESS) is signalled.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      Mathematically, the difference of two windows contains every
C      point contained in the first window but not contained in the
C      second window.
C
C      Fortran offers no satisfactory floating point representation
C      of open intervals. Thus, for floating point windows we must
C      return the closure of the set theoretical difference: that is,
C      the difference plus the endpoints of the first window that are
C      contained in the second window.
C
C$ Examples
C
C      Let A contain the intervals
C
C            [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
C
C      and B contain the intervals
C
C            [ 2, 4 ]  [ 8, 10 ]  [ 16, 18 ]
C
C      Then the difference of A and B contains the intervals
C
C            [ 1, 2 ]  [ 7, 8 ]  [ 10, 11 ]  [ 23, 27 ]
C
C$ Restrictions
C
C      None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      H.A. Neilan     (JPL)
C      W.L. Taber      (JPL)
C      I.M. Underwood  (JPL)
C
C$ Version
C
C-     SPICELIB Version 2.0.0, 16-SEP-1998 (WLT)
C
C         The previous version did not work when removing
C         singletons.  This has been corrected.
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     difference two d.p. windows
C
C-&
 
 
 
C$ Revisions
C
C-     Beta Version 1.1.0, 27-FEB-1989  (HAN)
C
C         Due to the calling sequence and functionality changes
C         in the routine EXCESS, the method of signalling an
C         excess of elements needed to be changed.
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      INTEGER               CARDD
      INTEGER               SIZED
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               ACARD
      INTEGER               BCARD
      INTEGER               CSIZE
 
      INTEGER               APB
      INTEGER               APE
      INTEGER               BPB
      INTEGER               BPE
      INTEGER               PUT
      DOUBLE PRECISION      F
      DOUBLE PRECISION      L
      INTEGER               NEEDED
 
      INTEGER               OVER
 
      LOGICAL               KEEP
      LOGICAL               UNRSLV
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'WNDIFD' )
 
C
C     Find the cardinality of the input windows, and the allowed size
C     of the output window. Also, save the size of the second window.
C
      ACARD = CARDD ( A )
      BCARD = CARDD ( B )
      CSIZE = SIZED ( C )
      OVER  = 0
 
C
C     Empty out the output window.
C
      CALL SSIZED ( CSIZE, C )
 
C
C     Let's handle the pathological cases first.
C
      IF ( BCARD .EQ. 0 ) THEN
         CALL COPYD ( A, C )
         CALL CHKOUT ( 'WNDIFD' )
         RETURN
      ELSE IF ( ACARD .EQ. 0 ) THEN
         CALL CHKOUT ( 'WNDIFD' )
         RETURN
      END IF
 
C
C     Now get pointers to the first intervals of A and B.
C
      APB = 1
      APE = 2
      BPB = 1
      BPE = 2
      PUT = 1
C
C     As long as the endpointer for A is less than the cardinality
C     of A we need to examine intervals and decide how much of
C     them to keep in C.
C
      DO WHILE ( APE .LE. ACARD )
C
C        We will work with the interval [F,L] which starts out
C        as the next interval of A.  We modify it below as required
C        when subtracting out intervals of B.
C
         F      = A(APB)
         L      = A(APE)
C
C        Right now we have not resolved whether to keep the interval
C        [F,L], but until we know better we assume it is a keeper.
C
         UNRSLV =  BPE .LE. BCARD
         KEEP   = .TRUE.
 
         DO WHILE ( UNRSLV )
 
            IF ( L .LT. B(BPB) ) THEN
C
C              The interval [F,L] is before the next interval of B, we
C              have resolved what to do with this one.   It is a
C              keeper.
C
               UNRSLV = .FALSE.
 
            ELSE IF ( F .GT. B(BPE) ) THEN
C
C              [F,L] is after the end of the current interval in B,
C              we need to look at the next interval of B
C
               BPB    = BPB + 2
               BPE    = BPE + 2
               UNRSLV = BPE .LE. BCARD
 
            ELSE
C
C              There is some overlap between the current interval
C              of B and the current interval of A. There are
C              several possibilities
C
C              1) The current interval of A is contained in the
C                 current interval of B (This includes singleton
C                 intervals in A). We just mark [F,L] so that it
C                 won't be kept.  We have fully resolved what to
C                 do with [F,L].
C
C              2) The interval from B overlaps at the beginning
C                 of the interval of A
C
C                 B interval [......]
C                 A interval     [............]
C                 result of A-B     [.........]
C
C                 In this case we need to shrink the interval [F,L]
C                 but we have not resolved how much of the result
C                 to keep.
C
C              3) The interval from B falls inside the current
C                 interval [F,L]
C
C                 B interval        [......]
C                 A interval     [............]
C                 result of A-B  [..]      [..]
C
C                 If the interval from B is not a singleton, we store
C                 the first part of [F,L] in C and then set [F,L] to
C                 be the right interval which is still not resolved.
C
C                 If the B interval is a singleton we can ignore ignore
C                 it.  But we have not resolved what to do about
C                 [F,L], we need to look at the next interval of B.
C
 
C
C              4) The interval from B overlaps at the ending
C                 of the interval of A
C
C                 B interval          [......]
C                 A interval     [......]
C                 result of A-B  [....]
C
C                 We need to shrink [F,L]. In this case we know we can
C                 keep all of what's left because all other intervals
C                 of B are to the right of [F,L]
C
               IF (       B(BPB) .LE. F
     .              .AND. L      .LE. B(BPE) ) THEN
C
C                 Case 1 above
C
                  KEEP   = .FALSE.
                  UNRSLV = .FALSE.
 
               ELSE IF ( B(BPB) .LE. F ) THEN
C
C                 Case 2 above
C
                  F      = B(BPE)
                  BPB    = BPB+2
                  BPE    = BPE+2
                  UNRSLV = BPE .LE. BCARD
 
               ELSE IF (      F      .LE. B(BPB)
     .                  .AND. L      .GE. B(BPE)
     .                  .AND. B(BPB) .LT. B(BPE) ) THEN
C
C                 Case 3 above (non-singleton interval of B).
C
 
                  IF ( PUT .LT. CSIZE ) THEN
                     C(PUT)   = F
                     C(PUT+1) = B(BPB)
 
                     CALL SCARDD ( PUT+1, C )
 
                     PUT      = PUT + 2
                  ELSE
                     OVER = OVER + 2
                  END IF
 
                  F      = B(BPE)
C
C                 If the interval from B contained L, we will not
C                 want to be keeping the singleton [F,L].
C
                  IF ( F .EQ. L ) THEN
                     KEEP   = .FALSE.
                     UNRSLV = .FALSE.
                  END IF
 
                  BPB    = BPB + 2
                  BPE    = BPE + 2
                  UNRSLV = UNRSLV .AND. BPE .LE. BCARD
 
               ELSE IF (      F      .LE. B(BPB)
     .                  .AND. L      .GE. B(BPE)
     .                  .AND. B(BPB) .EQ. B(BPE) ) THEN
C
C                 Case 3 above (singleton interval of B).
C
                  BPB    = BPB + 2
                  BPE    = BPE + 2
                  UNRSLV = BPE .LE. BCARD
 
               ELSE
C
C                 Case 4 above
C
                  L      =  B(BPB)
                  UNRSLV = .FALSE.
 
               END IF
 
            END IF
 
         END DO
 
C
C        If there is anything to keep in C, put it there.
C
         IF ( KEEP ) THEN
C
C           Make sure there is sufficient room to do the putting.
C
            IF ( PUT .LT. CSIZE ) THEN
               C(PUT)   = F
               C(PUT+1) = L
               CALL SCARDD ( PUT+1, C )
               PUT      = PUT + 2
            ELSE
               OVER = OVER + 2
            END IF
 
         END IF
C
C        Move the pointers in A to the next interval.
C
         APB = APB + 2
         APE = APE + 2
 
      END DO
 
C
C     We've examined all of the intervals of A and B, but if we
C     didn't actually store all of the difference, signal an error.
C
      IF ( OVER .GT. 0 ) THEN
 
         NEEDED = OVER + CSIZE
 
         CALL SETMSG ( 'The output window did not have '
     .   //            'sufficient room to contain the result of '
     .   //            'the window difference.  It has room for '
     .   //            '# endpoints, but # were needed to '
     .   //            'describe the difference. ' )
 
         CALL ERRINT ( '#', CSIZE  )
         CALL ERRINT ( '#', NEEDED )
         CALL SIGERR ( 'SPICE(WINDOWEXCESS)'  )
 
      END IF
 
      CALL CHKOUT ( 'WNDIFD' )
 
      RETURN
      END
