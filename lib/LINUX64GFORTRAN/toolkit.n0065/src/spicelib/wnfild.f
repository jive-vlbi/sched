C$Procedure      WNFILD ( Fill small gaps in a DP window )
 
      SUBROUTINE WNFILD ( SMALL, WINDOW )
 
C$ Abstract
C
C     Fill small gaps between adjacent intervals of a double precision
C     window.
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
C     WINDOWS
C
C$ Keywords
C
C     WINDOWS
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      DOUBLE PRECISION      SMALL
      DOUBLE PRECISION      WINDOW  ( LBCELL:* )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      SMALL      I   Limiting measure of small gaps.
C      WINDOW    I,O  Window to be filled.
C
C$ Detailed_Input
C
C      SMALL       is the limiting measure of the small gaps to be
C                  filled. Adjacent intervals separated by gaps of
C                  measure less than or equal to SMALL are merged.
C
C      WINDOW      on input, is a window containing zero or more
C                  intervals.
C
C$ Detailed_Output
C
C      WINDOW      on output, is the original window, after adjacent
C                  intervals separated by small gaps have been merged.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      This routine removes small gaps between adjacent intervals
C      by merging intervals separated by gaps of measure less than
C      or equal to the limiting measure (SMALL).
C
C$ Examples
C
C      Let WINDOW contain the intervals
C
C            [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]  [ 29, 29 ]
C
C      Then the following series of calls
C
C            CALL WNFILD (  1, WINDOW )                       (1)
C            CALL WNFILD (  2, WINDOW )                       (2)
C            CALL WNFILD (  3, WINDOW )                       (3)
C            CALL WNFILD ( 12, WINDOW )                       (4)
C
C      produces the following series of windows
C
C            [ 1,  3 ]  [ 7, 11 ]  [ 23, 27 ]  [ 29, 29 ]     (1)
C            [ 1,  3 ]  [ 7, 11 ]  [ 23, 29 ]                 (2)
C            [ 1,  3 ]  [ 7, 11 ]  [ 23, 29 ]                 (3)
C            [ 1, 29 ]                                        (4)
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
C      N.J. Bachman    (JPL)
C      H.A. Neilan     (JPL)
C      W.L. Taber      (JPL)
C      I.M. Underwood  (JPL)
C
C$ Version
C
C-     SPICELIB Version 1.0.3, 29-JUL-2007 (NJB)
C
C         Corrected typo in the previous Version line date string,
C         "29-JUL-20022" to "29-JUL-2002."
C
C-     SPICELIB Version 1.0.2, 29-JUL-2002 (NJB)
C
C         Changed gap size from 10 to 12 to correct erroneous example.
C         
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
C     fill small gaps in a d.p. window
C
C-&
 
 
 
C$ Revisions
C
C-     Beta Version 1.2.0, 24-FEB-1989  (HAN)
C
C         Added calls to CHKIN and CHKOUT.
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      INTEGER               CARDD
      LOGICAL               RETURN
 
 
C
C     Local variables
C
      INTEGER               CARD
      INTEGER               I
      INTEGER               J
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'WNFILD' )
      END IF
 
C
C     Get the cardinality of the window. (The size is not important;
C     this routine can't create any new intervals.)
C
      CARD = CARDD ( WINDOW )
 
C
C     Step through the window, looking for the next right endpoint
C     more than SMALL away from the following left endpoint. This marks
C     the end of the new first interval, and the beginning of the new
C     second interval. Keep this up until the last right endpoint has
C     been reached. This remains the last right endpoint.
C
      IF ( CARD .GT. 0 ) THEN
 
         I = 2
         J = 2
 
         DO WHILE ( J .LT. CARD )
 
            IF ( ( WINDOW(J) + SMALL )  .LT. WINDOW(J+1) ) THEN
               WINDOW(I  ) = WINDOW(J  )
               WINDOW(I+1) = WINDOW(J+1)
               I           = I + 2
            END IF
 
            J = J + 2
 
         END DO
 
         WINDOW(I) = WINDOW(J)
         CALL SCARDD ( I, WINDOW )
 
      END IF
 
      CALL CHKOUT ( 'WNFILD' )
      RETURN
      END
