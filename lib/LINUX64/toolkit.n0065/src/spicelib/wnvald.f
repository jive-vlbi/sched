C$Procedure      WNVALD ( Validate a DP window )
 
      SUBROUTINE WNVALD ( SIZE, N, A )
 
C$ Abstract
C
C     Form a valid double precision window from the contents
C     of a window array.
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
 
      INTEGER               SIZE
      INTEGER               N
      DOUBLE PRECISION      A        ( LBCELL:* )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     SIZE       I   Size of window.
C     N          I   Original number of endpoints.
C     A         I,O  Input, output window.
C
C$ Detailed_Input
C
C     SIZE        is the size of the window to be validated. This
C                 is the maximum number of endpoints that the cell
C                 used to implement the window is capable of holding
C                 at any one time.
C
C     N           is the original number of endpoints in the input
C                 cell.
C
C     A           on input, is a (possibly uninitialized) cell array
C                 SIZE containing N endpoints of (possibly unordered
C                 and non-disjoint) intervals.
C
C$ Detailed_Output
C
C     A           on output, is a window containing the union of the
C                 intervals in the input cell.
C
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C     This routine takes as input a cell array containing pairs of
C     endpoints and validates it to form a window.
C
C     On input, A is a cell of size SIZE containing N endpoints.
C     During validation, the intervals are ordered, and overlapping
C     intervals are merged. On output, the cardinality of A is
C     the number of endpoints remaining, and it is ready for use with
C     any of the window routines.
C
C     Because validation is done in place, there is no chance of
C     overflow.
C
C     Validation is primarily useful for ordering and merging
C     intervals read from input files or initialized in DATA
C     statements.
C
C$ Examples
C
C     The following small program
C
C            INTEGER               CARDD
C            INTEGER               SIZED
C
C            DOUBLE PRECISION      WINDOW  ( LBCELL:20 )
C
C            DATA                  WINDOW  /  0,  0,
C           .                                10, 12,
C                                             2,  7,
C                                            13, 15,
C                                             1,  5,
C                                            23, 29,   8*0 /
C
C            CALL WNVALD ( 20, 10, WINDOW )
C
C            WRITE (6,*) 'Current intervals: ', CARDD ( WINDOW ) / 2
C            WRITE (6,*) 'Maximum intervals: ', SIZED ( WINDOW ) / 2
C            WRITE (6,*)
C            WRITE (6,*) 'Intervals:'
C            WRITE (6,*)
C
C            DO I = 1, CARDD ( WINDOW ), 2
C               WRITE (6,*) WINDOW(I), WINDOW(I+1)
C            END DO
C
C            END
C
C     produces the following output (possibly formatted differently).
C
C            Current intervals:        5
C            Maximum intervals:       10
C
C            Intervals:
C
C             0.000000000000000     0.000000000000000
C             1.000000000000000     7.000000000000000
C             10.00000000000000     12.00000000000000
C             13.00000000000000     15.00000000000000
C             23.00000000000000     29.00000000000000
C
C$ Exceptions
C
C     1. If the number of endpoints N is odd, the error
C        SPICE(UNMATCHENDPTS) is signalled.
C
C     2. If the number of end points of the window exceeds its size, the
C        error SPICE(WINDOWTOOSMALL) is signalled.
C
C     3. If the left endpoint is greater than the right endpoint, the
C        error SPICE(BADENDPOINTS) is signalled.
C
C$ Files
C
C     None.
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
C     N.J. Bachman    (JPL)
C     H.A. Neilan     (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 30-JUL-2002 (NJB)
C
C        Fixed bugs in example program.
C
C-    SPICELIB Version 1.1.0, 14-AUG-1995 (HAN)
C
C        Fixed a character string that continued over two lines. 
C        The "//" characters were missing. The Alpha/OpenVMS compiler 
C        issued a warning regarding this incorrect statement syntax.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     validate a d.p. window
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 14-AUG-1995 (HAN)
C
C        Fixed a character string that continued over two lines. 
C        The "//" characters were missing. The Alpha/OpenVMS compiler 
C        issued a warning regarding this incorrect statement syntax.
C
C-    Beta Version 1.1.0, 17-FEB-1989 (HAN) (NJB)
C
C        Contents of the Required_Reading section was
C        changed from "None." to "WINDOWS".  Also, the
C        declaration of the unused function FAILED was
C        removed.
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               ODD
      LOGICAL               RETURN
 
C
C     Local variables
C
      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      RIGHT
      INTEGER               I

C
C     Setting up error processing.
C
      IF ( RETURN () ) RETURN
      CALL CHKIN ( 'WNVALD' )
 
C
C     First, some error checks. The number of endpoints must be even,
C     and smaller than the reported size of the window.
C
      IF ( ODD ( N ) ) THEN
 
         CALL SETMSG ( 'WNVALD: Unmatched endpoints' )
         CALL SIGERR ( 'SPICE(UNMATCHENDPTS)' )
         CALL CHKOUT ( 'WNVALD' )
         RETURN
 
      ELSE IF ( N .GT. SIZE ) THEN
 
         CALL SETMSG ( 'WNVALD: Inconsistent value for SIZE.' )
         CALL SIGERR ( 'SPICE(WINDOWTOOSMALL)' )
         CALL CHKOUT ( 'WNVALD' )
         RETURN
 
      END IF
 
C
C     Taking the easy way out, we will simply insert each new interval
C     as we happen upon it. We can do this safely in place. The output
C     window can't possibly contain more intervals than the input array.
C
C     What can go wrong is this: a left endpoint might be greater than
C     the corresponding left endpoint. This is a boo-boo, and should be
C     reported.
C
      CALL SSIZED ( SIZE, A )
      CALL SCARDD (    0, A )
 
      I = 1
 
      DO WHILE  ( I .LT. N )
 
         LEFT  = A(I)
         RIGHT = A(I+1)
 
         IF ( LEFT .GT. RIGHT ) THEN
           CALL SETMSG ( 'WNVALD: Left endpoint may not exceed ' //
     .                   'right endpoint.'                        )
           CALL SIGERR ( 'SPICE(BADENDPOINTS)'                    )
           CALL CHKOUT ( 'WNVALD'                                 )
           RETURN
         END IF
 
         CALL WNINSD ( LEFT, RIGHT, A )
         I = I + 2
 
      END DO
 
      CALL CHKOUT ( 'WNVALD' )
 
      RETURN
      END
