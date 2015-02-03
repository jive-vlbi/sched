C$Procedure            SIZEI ( Size of an integer cell )
 
      INTEGER FUNCTION SIZEI ( CELL )
 
C$ Abstract
C
C     Return the size (maximum cardinality) of an integer cell.
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
C     CELLS
C
C$ Keywords
C
C     CELLS
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               CELL  ( LBCELL: * )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     CELL       I   Input cell.
C
C     The function returns the size of the input cell.
C
C$ Detailed_Input
C
C
C      CELL        is a cell.
C
C
C$ Detailed_Output
C
C     The function returns the size of (maximum number of elements in)
C     the input cell.
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     The size (SIZE) functions are typically used in conjunction
C     with the cardinality functions to predict (and subsequently
C     avoid) overflows when manipulating cells. In the following
C     example, SIZEI is used to determine whether the integer cell
C     ORIGINAL can be safely copied into the integer cell SAVE before
C     actually attempting the operation. (If ORIGINAL contains more
C     elements than SAVE is capable of holding, then the operation
C     will fail.)
C
C           IF ( CARDI ( ORIGINAL ) .LE. SIZEI ( SAVE ) ) THEN
C              CALL COPYI ( ORIGINAL, SAVE, ERROR )
C
C           ELSE
C            .
C            .
C           END DO
C
C$ Restrictions
C
C     None.
C
C$ Exceptions
C
C     1)  If the input array has invalid cardinality, the error
C         SPICE(INVALIDCARDINALITY) is signalled.  SIZEI returns
C         an unspecified value in this case.
C
C     2)  If the input array has invalid size, the error
C         SPICE(INVALIDSIZE) is signalled.  SIZEI returns
C         an unspecified value in this case.
C
C$ Files
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
C     C.A. Curzon     (JPL)
C     H.A. Neilan     (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
C
C       If the value of the function RETURN is TRUE upon execution of
C       this module, this function is assigned a default value of
C       either 0, 0.0D0, .FALSE., or blank depending on the type of the
C       function.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     size of an integer cell
C
C-&
 
 
C$ Revisions
C
C-    Beta Version 2.0.0, 13-MAR-1989 (NJB)
C
C        Check for valid input cell added.  The input cell must
C        have valid size and cardinality values.
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
 
 
      IF ( RETURN() ) THEN
         SIZEI = 0
         RETURN
      ELSE
         CALL CHKIN ( 'SIZEI' )
      END IF
 
C
C     Set return value, regardless of validity.
C
      SIZEI = CELL(-1)
 
C
C     Squeal if something is awry.
C
 
      IF (  CELL(-1) .LT. 0  )  THEN
 
         CALL SETMSG ( 'Invalid cell size.  The size was #.')
         CALL ERRINT ( '#', CELL(-1)  )
         CALL SIGERR ( 'SPICE(INVALIDSIZE)' )
         CALL CHKOUT ( 'SIZEI' )
         RETURN
 
      ELSE IF ( CELL(0) .LT. 0  ) THEN
 
         CALL SETMSG ( 'Invalid cell cardinality.  The cardinality '  //
     .                 'was #.'      )
         CALL ERRINT ( '#', CELL(0)  )
         CALL SIGERR ( 'SPICE(INVALIDCARDINALITY)' )
         CALL CHKOUT ( 'SIZEI' )
         RETURN
 
      ELSE IF ( CELL(0) .GT. CELL(-1)  ) THEN
 
         CALL SETMSG ( 'Invalid cell cardinality; cardinality exceeds'//
     .                 ' cell size.  The cardinality was #.  The size'//
     .                 ' was #.'  )
         CALL ERRINT ( '#', CELL( 0)  )
         CALL ERRINT ( '#', CELL(-1)  )
         CALL SIGERR ( 'SPICE(INVALIDCARDINALITY)' )
         CALL CHKOUT ( 'SIZEI' )
         RETURN
 
      END IF
 
 
      CALL CHKOUT ( 'SIZEI' )
      RETURN
      END
