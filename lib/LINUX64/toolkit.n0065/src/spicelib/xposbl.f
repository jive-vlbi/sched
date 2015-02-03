C$Procedure      XPOSBL ( Transpose a matrix by blocks    )
 
      SUBROUTINE XPOSBL ( BMAT, NROW, NCOL, BSIZE, BTMAT )
 
C$ Abstract
C
C     Transpose the square blocks within a matrix. 
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
C     None.
C
C$ Keywords
C
C     MATH
C     MATRIX
C     TRANSFORMATION
C
C$ Declarations
 
      INTEGER               NROW
      INTEGER               NCOL
      INTEGER               BSIZE
      DOUBLE PRECISION      BMAT  ( NROW, NCOL )
      DOUBLE PRECISION      BTMAT ( NROW, NCOL )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     BMAT       I   a matrix composed of square block submatrices
C     NROW       I   the number of rows in the matrix BMAT
C     NCOL       I   the number of columns in the matrix BMAT
C     BSIZE      I   the size of the square blocks in BMAT
C     BTMAT      O   the result of transposing the blocks of BMAT
C
C$ Detailed_Input
C
C     BMAT       is a block structured matrix.  In other words
C                it looks like:
C
C
C                   -                                -
C                  |      :      :       :     :      |
C                  |  B   :  B   :   B   :     :  B   |
C                  |   11 :   12 :    13 : ... :   1C |
C                  |......:......:.......:     :......|
C                  |      :      :       :     :      |
C                  |  B   :  B   :   B   :     :  B   |
C                  |   21 :   22 :    23 : ... :   2C |
C                  |......:......:.......:     :......|
C                  |      .                           |
C                  |      .                           |
C                  |      .                           |
C                  |......................     .......|
C                  |      :      :       :     :      |
C                  |  B   :  B   :   B   :     :  B   |
C                  |   R1 :   R2 :    R3 : ... :   RC |
C                  |......:......:.......:     :......|
C                   -                                -
C
C                where each B   is a square matrix of BSIZE rows and
C                            ij
C                and columns.
C
C     NROW       is the number of rows in the input matrix.
C
C     NCOL       is the number of columns in the input matrix.
C
C     BSIZE      is the number of rows and columns in each block
C                of the input matrix.
C
C$ Detailed_Output
C
C     BTMAT     is the matrix obtained from BMAT when each of its
C               blocks is transposed.  Given the description of
C               BMAT above, BTMAT looks like:
C
C
C                   -                                -
C                  |   t  :   t  :    t  :     :   t  |
C                  |  B   :  B   :   B   :     :  B   |
C                  |   11 :   12 :    13 : ... :   1C |
C                  |......:......:.......:     :......|
C                  |      :      :       :     :      |
C                  |   t  :   t  :    t  :     :   t  |
C                  |  B   :  B   :   B   :     :  B   |
C                  |   21 :   22 :    23 : ... :   2C |
C                  |......:......:.......:     :......|
C                  |      .                           |
C                  |      .                           |
C                  |      .                           |
C                  |......................     .......|
C                  |      :      :       :     :      |
C                  |   t  :   t  :    t  :     :   t  |
C                  |  B   :  B   :   B   :     :  B   |
C                  |   R1 :   R2 :    R3 : ... :   RC |
C                  |......:......:.......:     :......|
C                   -                                -
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the number of rows input is not positive, your program
C        will probably experience a run-time error.  However, in the
C        event that your system does not catch this error, this routine
C        will diagnose it and signal the error 'SPICE(BADROWCOUNT)'.
C
C     1) If the number of columns input is not positive, your program
C        will probably experience a run-time error.  However, in the
C        event that your system does not catch this error, this routine
C        will diagnose it and signal the error 'SPICE(BADCOLUMNCOUNT)'.
C
C     3) If the block size input is not positive, the error
C        'SPICE(BADBLOCKSIZE)' is signalled.
C
C     4) If BMAT cannot be partitioned into an integer number of
C        blocks, the error 'SPICE(BLOCKSNOTEVEN)' is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine transposes the blocks of a block structured
C     matrix.  This operation is valuable, as it is a means
C     for computing the inverse of a state transformation matrix
C     (see the example below).
C
C$ Examples
C
C     The following code fragment illustrates how you would convert
C     a state relative to earth-fixed coordinates to J2000 coordinates.
C
C     C
C     C     We want to state earthfixed coordinates (399) to J2000
C     C     coordinates
C     C
C           BODY = 399
C           REF  = 'J2000'
C
C     C
C     C     Get the 6 by 6 state transformation matrix from J2000
C     C     coordinates to earthfixed coordinates.
C     C
C           CALL TISBOD ( REF, BODY, ET, TISPM )
C
C     C
C     C     The inverse of TISPM can be obtained by transposing the
C     C     3 by 3 blocks of the 6 by 6 matrix TISPM.
C     C
C           CALL XPOSBL ( TISPM, 6, 6, 3, TSPMI )
C
C
C     C
C     C     Now transform the earthfixed state (ESTATE) to the
C     C     inertial state (ISTATE).
C     C
C           CALL MXVG   ( TSPMI, ESTATE, 6, 6, ISTATE )
C
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
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 22-APR-2010 (NJB)
C
C        Header correction: assertions that the output
C        can overwrite the input have been removed.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 5-NOV-1990 (WLT)
C
C-&
 
C$ Index_Entries
C
C     transpose a matrix by blocks
C
C-&
 
 
 
 
C
C     Local variables
C
      DOUBLE PRECISION      TEMP
 
      INTEGER               I
      INTEGER               J
      INTEGER               CB
      INTEGER               RB
 
 
C
C     Ok. Here's what's going to happen.
C
C     The matrix has the form:
C
C       -                                -
C      |      :      :       :     :      |
C      |  B   :  B   :   B   :     :  B   |
C      |   11 :   12 :    13 : ... :   1C |
C      |......:......:.......:     :......|
C      |      :      :       :     :      |
C      |  B   :  B   :   B   :     :  B   |
C      |   21 :   22 :    23 : ... :   2C |
C      |......:......:.......:     :......|
C      |      .                           |
C      |      .                           |
C      |      .                           |
C      |......................     .......|
C      |      :      :       :     :      |
C      |  B   :  B   :   B   :     :  B   |
C      |   R1 :   R2 :    R3 : ... :   RC |
C      |......:......:.......:     :......|
C
C      Where each block B   is a square matrix.
C                        ij
C
C      All we really need to do is figure out how to transpose any
C      of the blocks.  Once that is done we can just cycle over
C      all of the blocks in the matrix.
C
C      So what does the ij block look like? Well, this is it.
C
C
C             a              a               ...    a
C              RB+1 CB+1      RB+1 CB+2              RB+1 CB+BSIZE
C
C             a              a               ...    a
C              RB+2 CB+1      RB+2 CB+2              RB+2 CB+BSIZE
C
C             a              a               ...    a
C              RB+3 CB+1      RB+3 CB+2              RB+3 CB+BSIZE
C
C                                 .
C                                 .
C                                 .
C
C              a              a                 ... a
C               RB+BSIZE CB+1  RB+BSIZE CB+2         RB+BSIZE CB+BSIZE
C
C
C     where RB = (i-1)*BSIZE, and CB = (j-1)*BSIZE.  But inspection of
C     this block shows that to transpose it we simply need to swap
C     the entries
C
C                a           and  a
C                 RB+m CB+n        RB+n CB+m
C
C     where m and n range over all integers from 1 to BSIZE.
C
 
C
C     Let's first check to make sure that the requested operation
C     makes sense.  Are all of the integers positive?
C
      IF (  BSIZE .LT. 1 ) THEN
         CALL CHKIN  ( 'XPOSBL'                                       )
         CALL SETMSG ( 'The block size is not positive. '             //
     .                 'The block size is #.'                         )
         CALL ERRINT ( '#', BSIZE                                     )
         CALL SIGERR ( 'SPICE(BADBLOCKSIZE)'                          )
         CALL CHKOUT ( 'XPOSBL'                                       )
         RETURN
      END IF
 
      IF (  NROW .LT. 1 ) THEN
         CALL CHKIN  ( 'XPOSBL'                                       )
         CALL SETMSG ( 'The number of rows in the matrix is not '     //
     .                 'positive. The number of rows is #.'           )
         CALL ERRINT ( '#', NROW                                      )
         CALL SIGERR ( 'SPICE(BADROWCOUNT)'                           )
         CALL CHKOUT ( 'XPOSBL'                                       )
         RETURN
      END IF
 
      IF (  NCOL .LT. 1 ) THEN
         CALL CHKIN  ( 'XPOSBL'                                       )
         CALL SETMSG ( 'The number of columns in the matrix is not '  //
     .                 'positive. The number of columns is #.'        )
         CALL ERRINT ( '#', NCOL                                      )
         CALL SIGERR ( 'SPICE(BADCOLUMNCOUNT)'                        )
         CALL CHKOUT ( 'XPOSBL'                                       )
         RETURN
      END IF
 
 
 
C
C     Is there a whole number of blocks in the matrix.
C
      IF (      ( MOD( NCOL, BSIZE ) .NE. 0 )
     .     .OR. ( MOD( NROW, BSIZE ) .NE. 0 ) ) THEN
 
         CALL CHKIN  ( 'XPOSBL'                                       )
         CALL SETMSG ( 'The block size does not evenly divide both '  //
     .                 'the number of rows and the number of '        //
     .                 'columns. The '                                //
     .                 'block size is #; the number of rows is #; '   //
     .                 'the number of columns is #. '                 )
 
         CALL ERRINT ( '#', BSIZE                                     )
         CALL ERRINT ( '#', NROW                                      )
         CALL ERRINT ( '#', NCOL                                      )
         CALL SIGERR ( 'SPICE(BLOCKSNOTEVEN)'                         )
         CALL CHKOUT ( 'XPOSBL'                                       )
         RETURN
      END IF
 
C
C     If we get to this point we are ready to do the transposes.
C     Cycle over all of the blocks in the matrix.
C
      DO CB = 0, NCOL-1, BSIZE
         DO RB = 0, NROW-1, BSIZE
 
C
C           OK. Transpose block ( RB, CB ).
C
            DO I = 1, BSIZE
               DO J = 1, I
 
                  IF ( I .EQ. J ) THEN
                     BTMAT ( RB+I, CB+J ) = BMAT ( RB+I, CB+J )
                  ELSE
                     TEMP                 = BMAT ( RB+I, CB+J )
                     BTMAT ( RB+I, CB+J ) = BMAT ( RB+J, CB+I )
                     BTMAT ( RB+J, CB+I ) = TEMP
                  END IF
 
               END DO
            END DO
 
         END DO
      END DO
 
 
      RETURN
      END
