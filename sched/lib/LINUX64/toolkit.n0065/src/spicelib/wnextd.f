C$Procedure      WNEXTD ( Extract the endpoints from a DP window )
 
      SUBROUTINE WNEXTD ( SIDE, WINDOW )
 
C$ Abstract
C
C     Extract the left or right endpoints from a double precision
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
 
      CHARACTER*1           SIDE
      DOUBLE PRECISION      WINDOW  ( LBCELL:* )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      SIDE       I   Extract left ('L') or right ('R') endpoints.
C      WINDOW    I,O  Window to be extracted.
C
C$ Detailed_Input
C
C      SIDE        indicates whether the left or right endpoints of
C                  the intervals in the window are to be extracted.
C
C                        'L', 'l'       Left endpoints.
C                        'R', 'r'       Right endpoints.
C
C                  If SIDE is not recognized, the input window is
C                  not changed.
C
C      WINDOW      on input, is a window containing zero or more
C                  intervals.
C
C$ Detailed_Output
C
C      WINDOW      on output, is the collection of singleton intervals
C                  containing either the left or the right endpoints
C                  of the intervals in the original window.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      This routine replaces every interval in the input window with
C      the singleton interval containing one of the endpoints of the
C      interval.
C
C$ Examples
C
C      Let WINDOW contain the intervals
C
C            [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]  [ 29, 29 ]
C
C      Then the call
C
C            CALL WNEXTD (  'L', WINDOW )
C
C      produces the window
C
C            [ 1, 1 ]  [ 7, 7 ]  [ 23, 23 ]  [ 29, 29 ]
C
C      And the call
C
C            CALL WNEXTD ( 'R', WINDOW )
C
C      produces the window
C
C            [ 3, 3 ]  [ 11, 11 ]  [ 27, 27 ]  [ 29, 29 ]
C
C$ Exceptions
C
C     1) If the endpoint specification, SIDE, is not recognized, the
C        error SPICE(INVALIDENDPNTSPEC) is signalled.
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
C     extract the endpoints from a d.p. window
C
C-&
 
 
 
 
C$ Revisions
C
C-     Beta Version 1.2.0, 24-FEB-1989  (HAN)
C
C         Added calls to CHKIN and CHKOUT. Error handling was added to
C         detect invalid endpoint specification. The previous version
C         did not signal an error if SIDE was not 'R', 'r', 'L', or 'l'.
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
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'WNEXTD' )
      END IF
 
C
C     Get the cardinality of the window. (The size is not important;
C     this routine can't create any new intervals.)
C
      CARD = CARDD ( WINDOW )
 
C
C     Step through the window, keeping one endpoint from each interval.
C     For the sake of efficiency, we have separate loops for the two
C     possible values of SIDE.
C
      IF ( SIDE .EQ. 'L'  .OR.  SIDE .EQ. 'l' ) THEN
 
         DO I = 1, CARD, 2
            WINDOW(I+1) = WINDOW(I)
         END DO
 
      ELSE IF ( SIDE .EQ. 'R'  .OR.  SIDE .EQ. 'r' ) THEN
 
         DO I = 1, CARD, 2
            WINDOW(I) = WINDOW(I+1)
         END DO
 
      ELSE
 
         CALL SETMSG ( 'SIDE was *.' )
         CALL ERRCH  ( '*', SIDE     )
         CALL SIGERR ( 'SPICE(INVALIDENDPNTSPEC)' )
 
      END IF
 
      CALL CHKOUT ( 'WNEXTD' )
      RETURN
      END
