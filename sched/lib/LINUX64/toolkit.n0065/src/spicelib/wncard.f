C$Procedure WNCARD ( Cardinality of a double precision window )
 
      INTEGER FUNCTION WNCARD ( WINDOW )
 
C$ Abstract
C
C     Return the cardinality (number of intervals) of a double
C     precision window.
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
 
      DOUBLE PRECISION      WINDOW  ( LBCELL:* )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     WINDOW     I   Input window.
C
C     The function returns the cardinality of the input window.
C
C$ Detailed_Input
C
C      WINDOW      is a window containing zero or more intervals.
C
C$ Detailed_Output
C
C     The function returns the cardinality of (number of intervals in)
C     the input window.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If the number of elements in WINDOW is not even
C        the error SPICE(INVALIDSIZE) signals.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The window cardinality (WNCARD) function simply wraps a CARD call
C     then divides the result by 2. A common error when using the SPICE
C     windows function is to use the CARDD value as the number of 
C     window intervals rather than the CARDD/2 value.
C
C$ Examples
C
C      INTEGER               LBCELL
C      PARAMETER           ( LBCELL = -5 )
C
C      INTEGER               WNSIZE
C      PARAMETER           ( WNSIZE = 10 )
C 
C      DOUBLE PRECISION      WINDOW      ( LBCELL:WNSIZE )
C      DOUBLE PRECISION      LEFT
C      DOUBLE PRECISION      RIGHT
C
C      INTEGER               WNCARD
C      INTEGER               I      
C
C      Validate the window with size WNSIZE and zero elements.
C
C      CALL WNVALD( WNSIZE, 0, WINDOW )
C
C      Insert the intervals
C
C         [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
C
C      into WINDOW.
C
C      CALL WNINSD(  1.D0,  3.D0, WINDOW )
C      CALL WNINSD(  7.D0, 11.D0, WINDOW )
C      CALL WNINSD( 23.D0, 27.D0, WINDOW )
C
C      Loop over the number of intervals in WINDOW, output
C      the LEFT and RIGHT endpoints for each interval.
C
C      DO I=1, WNCARD(WINDOW)
C
C         CALL WNFETD( WINDOW, I, LEFT, RIGHT )
C
C         WRITE(*,*) 'Interval', I, ' [', LEFT, RIGHT, ']'
C
C      END DO
C
C   The code outputs:
C
C      Interval 1 [  1.    3.]
C      Interval 2 [  7.   11.]
C      Interval 3 [  23.  27.]
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 24-APR-2010 (EDW)
C
C       Minor edit to code comments eliminating typo.
C
C-    SPICELIB Version 1.0.0, 10-AUG-2007 (EDW)
C
C-&
 
C$ Index_Entries
C
C     cardinality of a d.p. window
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               EVEN
      
      INTEGER               CARDD
 
 
      IF ( RETURN() ) THEN
         WNCARD = 0
         RETURN
      ELSE
         CALL CHKIN ( 'WNCARD' )
      END IF

      WNCARD = CARDD(WINDOW)
      
C
C     Confirm the cardinality as an even integer.
C
      IF( .NOT. EVEN(WNCARD) ) THEN

         CALL SETMSG ( 'Invalid window size, a window should '
     .        // 'have an even number of elements. The size was #.')
         CALL ERRINT ( '#', WNCARD )
         CALL SIGERR ( 'SPICE(INVALIDSIZE)' )
         CALL CHKOUT ( 'WNCARD' )
         WNCARD = 0
         RETURN

      END IF

C
C     Set return value. Cardinality in a SPICE window sense 
C     means the number of intervals, half the cell
C     cardinality value.
C
      WNCARD = WNCARD/2
 
      CALL CHKOUT ( 'WNCARD' )
      RETURN
      END
      
      

