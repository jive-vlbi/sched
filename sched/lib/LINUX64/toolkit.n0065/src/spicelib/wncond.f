C$Procedure      WNCOND ( Contract the intervals of a DP window )
 
      SUBROUTINE WNCOND ( LEFT, RIGHT, WINDOW )
 
C$ Abstract
C
C     Contract each of the intervals of a double precision window.
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
 
      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      RIGHT
      DOUBLE PRECISION      WINDOW  ( LBCELL:* )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      LEFT       I   Amount added to each left endpoint.
C      RIGHT      I   Amount subtracted from each right endpoint.
C      WINDOW    I,O  Window to be contracted.
C
C$ Detailed_Input
C
C      LEFT        is the amount to be added to the left endpoint of
C                  each interval in the input window.
C
C      RIGHT       is the amount to be subtracted from the right
C                  endpoint of each interval in the window.
C
C      WINDOW      on input, is a window containing zero or more
C                  intervals.
C
C$ Detailed_Output
C
C      WINDOW      on output, is the original window with each of its
C                  intervals contracted by LEFT units on the left and
C                  RIGHT units on the right.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      This routine contracts (shortens) each of the intervals in
C      the input window. The adjustments are not necessarily symmetric.
C      That is, LEFT units are added to the left endpoint of each
C      interval, and RIGHT units are subtracted from the right endpoint
C      of each interval, where LEFT and RIGHT may be different.
C
C      Intervals are dropped when they are contracted by amounts
C      greater than their measures.
C
C$ Examples
C
C      Let WINDOW contain the intervals
C
C            [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]  [ 29, 29 ]
C
C      Then the following series of calls
C
C            CALL WNCOND (  2,  1, WINDOW )              (1)
C            CALL WNCOND ( -2,  2, WINDOW )              (2)
C            CALL WNCOND ( -2, -1, WINDOW )              (3)
C
C      produces the following series of windows
C
C            [ 9, 10 ]  [ 25, 26 ]                       (1)
C            [ 7,  8 ]  [ 23, 24 ]                       (2)
C            [ 5,  9 ]  [ 21, 25 ]                       (3)
C
C      Note that intervals may be "contracted" by negative amounts.
C      In the example above, the second call shifts each interval to
C      the left, while the third call undoes the effect of the first
C      call (without restoring the destroyed intervals).
C
C      Note also that the third call is exactly equivalent to the
C      call
C
C            CALL WNEXPD ( 2, 1, WINDOW )
C
C
C$ Exceptions
C
C     None.
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
C     contract the intervals of a d.p. window
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
C     Spicelib functions
C
      LOGICAL               RETURN
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'WNCOND' )
      END IF
 
 
C
C     This is just negative expansion.
C
      CALL WNEXPD ( -LEFT, -RIGHT, WINDOW )
 
 
      CALL CHKOUT ( 'WNCOND' )
      RETURN
      END
