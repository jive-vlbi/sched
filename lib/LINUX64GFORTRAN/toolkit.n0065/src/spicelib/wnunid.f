C$Procedure      WNUNID ( Union two DP windows )
 
      SUBROUTINE WNUNID ( A, B, C )
 
C$ Abstract
C
C      Place the union of two double precision windows into a third
C      window.
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
C      C          I   Union of A and B.
C
C$ Detailed_Input
C
C      A,
C      B           are windows, each of which contains zero or more
C                  intervals.
C
C$ Detailed_Output
C
C      C           is the output window, containing the union of
C                  A and B---every point contained in A, or in B,
C                  or in both.
C
C                  C must be distinct from both A and B.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      The union of two windows contains every point contained in the
C      first window, or the second window, or both.
C
C$ Examples
C
C      Let A contain the intervals
C
C            [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
C
C      and B contain the intervals
C
C            [ 2, 6 ]  [ 8, 10 ]  [ 16, 18 ]
C
C      Then the union of A and B contains the intervals
C
C            [ 1, 6 ]  [ 7, 11 ]  [ 16, 18 ]  [ 23, 27 ]
C
C$ Exceptions
C
C     1. If the union of the two windows results in an excess of
C        elements, the error SPICE(WINDOWEXCESS) is signalled.
C
C$ Files
C
C      None.
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
C-     SPICELIB Version 1.1.0, 8-FEB-1999 (WLT)
C
C         The variable END was not initialized in the previous
C         edition.  It is now initialized to be the minimum of
C         A(1) and B(1).
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
C     union two d.p. windows
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
 
      INTEGER               AP
      INTEGER               BP
      INTEGER               CP
      DOUBLE PRECISION      END
      CHARACTER*1           USE
      INTEGER               OVER
 
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'WNUNID' )
      END IF
 
 
C
C     Find the cardinality of the input windows, and the allowed size
C     of the output window.
C
      ACARD = CARDD ( A )
      BCARD = CARDD ( B )
      CSIZE = SIZED ( C )
 
C
C     Begin with the input pointers at the first elements of the
C     input windows. The initial cardinality of the output window
C     is zero. And there is no overflow so far.
C
C     (Note that AP and BP point to the LEFT endpoints of intervals
C     in A and B, while CP points to the RIGHT endpoint of the latest
C     interval in C.)
C
      AP   = 1
      BP   = 1
      CP   = 0
      END  = MIN( A(1), B(1) )
      OVER = 0
 
C
C     When the ends of both input windows are reached, we're done.
C
      DO WHILE ( AP .LT. ACARD  .OR.  BP .LT. BCARD )
 
C
C        If the end of one window has been reached, copy (or merge)
C        the next interval from the other window.
C
         IF ( AP .GT. ACARD ) THEN
            USE = 'B'
 
         ELSE IF ( BP .GT. BCARD ) THEN
            USE = 'A'
 
C
C        Otherwise, let's see what we can do with the earlier of
C        the next intervals from A and B.
C
         ELSE IF ( A(AP) .LT. B(BP) ) THEN
            USE = 'A'
 
         ELSE IF ( B(BP) .LE. A(AP) ) THEN
            USE = 'B'
         END IF
 
C
C        If there is still space in the output window, fill it
C        as necessary. Otherwise, stop filling the array, but continue
C        to count the number of elements in excess of the size of the
C        output window.
C
C        The general idea is this: if the next interval overlaps the
C        latest output interval, merge the two (extending the output
C        interval to the right). Otherwise, insert the next interval
C        intact.
C
         IF ( USE .EQ. 'A' ) THEN
 
            IF ( CP .LT. CSIZE ) THEN
 
               IF ( A(AP) .LE. END .AND. CP .GT. 0 ) THEN
                  C(CP) = MAX ( C(CP), A(AP+1) )
               ELSE
                  CP      = CP + 2
                  C(CP-1) = A(AP  )
                  C(CP  ) = A(AP+1)
               END IF
 
               END = C(CP)
 
            ELSE
 
               IF ( A(AP) .LE. END ) THEN
                  END  = MAX ( END, A(AP+1) )
               ELSE
                  OVER = OVER + 2
                  END  = A(AP+1)
               END IF
 
            END IF
 
            AP = AP + 2
 
C
C        This is the same as the last clause, with B replacing A.
C
         ELSE IF ( USE .EQ. 'B' ) THEN
 
            IF ( CP .LT. CSIZE ) THEN
 
               IF ( B(BP) .LE. END .AND. CP .GT. 0 ) THEN
                  C(CP)   = MAX ( C(CP), B(BP+1) )
               ELSE
                  CP      = CP + 2
                  C(CP-1) = B(BP  )
                  C(CP  ) = B(BP+1)
               END IF
 
               END = C(CP)
 
            ELSE
 
               IF ( B(BP) .LE. END ) THEN
                  END  = MAX ( END, B(BP+1) )
               ELSE
                  OVER = OVER + 2
                  END  = B(BP+1)
               END IF
            END IF
 
            BP = BP + 2
 
         END IF
 
      END DO
 
C
C     Set the cardinality of the output window.
C
      CALL SCARDD ( CP, C )
 
C
C     If there is an excess of elements, signal an error and check out
C     as usual.
C
      IF ( OVER .GT. 0 ) THEN
         CALL EXCESS ( OVER, 'window' )
         CALL SIGERR ( 'SPICE(WINDOWEXCESS)' )
      END IF
 
 
      CALL CHKOUT ( 'WNUNID' )
 
      RETURN
      END
