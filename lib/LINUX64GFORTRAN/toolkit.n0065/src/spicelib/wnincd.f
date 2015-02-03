C$Procedure            WNINCD ( Included in a double precision window )
 
      LOGICAL FUNCTION WNINCD ( LEFT, RIGHT, WINDOW )
 
C$ Abstract
C
C     Determine whether an interval is included in a double precision
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
C      WINDOWS
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
      DOUBLE PRECISION      WINDOW      ( LBCELL:* )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      LEFT,
C      RIGHT      I   Input interval.
C      WINDOW     I   Input window.
C
C      The function returns TRUE if POINT is an element of WINDOW.
C
C$ Detailed_Input
C
C      LEFT,
C      RIGHT       are the endpoints of an interval, which may or
C                  may not be contained in one of the intervals in
C                  WINDOW.
C
C      WINDOW      is a window containing zero or more intervals.
C
C$ Detailed_Output
C
C      The function returns TRUE if the input interval is included
C      in the input window---that is, if
C
C            a(i)  <  LEFT  <  RIGHT  <  b(i)
C                              -        -         -
C
C       for some interval [ a(i), b(i) ] in WINDOW---and
C       is false otherwise.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      None.
C
C$ Examples
C
C      Let A contain the intervals
C
C            [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
C
C      Then the following expressions are true
C
C            WNINCD ( 1.D0,  3.D0, WINDOW )
C            WNINCD ( 9.D0, 10.D0, WINDOW )
C
C      and the following expressions are false.
C
C            WNINCD (  0,  2, WINDOW )
C            WNINCD ( 13, 15, WINDOW )
C            WNINCD ( 29, 30, WINDOW )
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
C-     SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
C
C        If the value of the function RETURN is TRUE upon execution of
C        this module, this function is assigned a default value of
C        either 0, 0.0D0, .FALSE., or blank depending on the type of
C        the function.
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
C     included in a d.p. window
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
 
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         WNINCD = .FALSE.
         RETURN
      ELSE
         CALL CHKIN ( 'WNINCD' )
      END IF
 
C
C     How many endpoints in the window?
C
      CARD = CARDD ( WINDOW )
 
C
C     Check this interval agains every interval in the window.
C     Inefficient, but foolproof.
C
      DO I = 1, CARD, 2
 
         IF ( LEFT .GE. WINDOW(I)  .AND.  RIGHT .LE. WINDOW(I+1) ) THEN
            WNINCD = .TRUE.
            CALL CHKOUT ( 'WNINCD' )
            RETURN
         END IF
 
      END DO
 
      WNINCD = .FALSE.
 
      CALL CHKOUT ( 'WNINCD' )
      RETURN
      END
