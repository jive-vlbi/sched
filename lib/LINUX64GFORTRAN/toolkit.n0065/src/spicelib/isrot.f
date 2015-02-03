C$Procedure     ISROT ( Indicate whether a matrix is a rotation matrix )
 
      LOGICAL FUNCTION ISROT ( M, NTOL, DTOL )
 
C$ Abstract
C
C     Indicate whether a 3x3 matrix is a rotation matrix.
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
C     ROTATION
C
C$ Keywords
C
C     ERROR
C     MATRIX
C     ROTATION
C
C$ Declarations
 
      DOUBLE PRECISION      M    ( 3, 3 )
      DOUBLE PRECISION      NTOL
      DOUBLE PRECISION      DTOL
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     M          I   A matrix to be tested.
C     NTOL       I   Tolerance for the norms of the columns of M.
C     DTOL       I   Tolerance for the determinant of a matrix whose
C                    columns are the unitized columns of M.
C
C     The function returns the value .TRUE. if and only if M is
C     a rotation matrix.
C
C$ Detailed_Input
C
C     M              is a 3x3 matrix to be tested.
C
C     NTOL           is the tolerance for the norms of the columns
C                    of M.
C
C     DTOL           is the tolerance for the determinant of a matrix
C                    whose columns are the unitized columns of M.
C
C$ Detailed_Output
C
C     The function returns the value .TRUE. if and only if M is found
C     to be a rotation matrix.  The criteria that M must meet are:
C
C
C        1) The norm of each column of M must satisfy the relation
C
C              1.D0 - NTOL  <   || column ||   <  1.D0 + NTOL.
C                           -                  -
C
C        2) The determinant of the matrix whose columns are the
C           unitized columns of M must satisfy
C
C              1.D0 - DTOL  <   determinant   <  1.D0 + DTOL.
C                           -                 -
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If either of NTOL or DTOL is negative, the error
C         SPICE(VALUEOUTOFRANGE) is signalled.  ISROT returns the
C         value .FALSE. in this case.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is an error checking `filter'; its purpose is to
C     detect gross errors, such as uninitialized matrices.  Matrices
C     that do not pass the tests used by this routine hardly qualify as
C     rotation matrices.  The test criteria can be adjusted by varying
C     the parameters NTOL and DTOL.
C
C     A property of rotation matrices is that their columns form a
C     right-handed, orthonormal basis in 3-dimensional space.  The
C     converse is true:  all 3x3 matrices with this property are
C     rotation matrices.
C
C     An ordered set of three vectors V1, V2, V3 forms a right-handed,
C     orthonormal basis if and only if
C
C        1)   || V1 ||  =  || V2 ||  =  || V3 ||  =  1
C
C        2)   V3 = V1 x V2.  Since V1, V2, and V3 are unit vectors,
C             we also have
C
C             < V3, V1 x V2 > = 1.
C
C             This quantity is the determinant of the matrix whose
C             colums are V1, V2 and V3.
C
C     When finite precision numbers are used, rotation matrices will
C     usually fail to satisfy these criteria exactly.  We must use
C     criteria that indicate approximate conformance to the criteria
C     listed above.  We choose
C
C        1)   |   || Vi ||  -  1   |   <   NTOL,  i = 1, 2, 3.
C                                      -
C
C        2)   Let
C
C                       Vi
C                Ui = ------ ,   i = 1, 2, 3.
C                     ||Vi||
C
C             Then we require
C
C                | < U3, U1 x U2 > - 1 |  <  DTOL;
C                                         -
C
C             equivalently, letting U be the matrix whose columns
C             are U1, U2, and U3, we insist on
C
C                | det(U) - 1 |  <  DTOL.
C                                _
C$ Examples
C
C     1)  We have obtained an instrument pointing matrix C from a
C         C-kernel, and we wish to test whether it is in fact a
C         rotation matrix.  We can use ISROT to check this:
C
C            C
C            C    Obtain pointing matrix:
C            C
C                 CALL CKGP ( INST, TIMEIN, TOL, REF, C, TIMOUT, FOUND )
C
C            C
C            C    Verify that C is a rotation:
C            C
C                 IF ( .NOT. ISROT ( C )  ) THEN
C
C                    [ perform exception handling ]
C
C                 ELSE
C
C                    [ code for the normal case goes here ]
C
C                 END IF
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
C     N.J. Bachman   (JPL)
C     H.A. Neilan    (JPL)
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
C-    SPICELIB Version 1.0.0, 06-SEP-1990  (NJB)
C
C-&
 
C$ Index_Entries
C
C     indicate whether a matrix is a rotation matrix
C
C-&
 
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      BRCKTD
      DOUBLE PRECISION      DET
 
      LOGICAL               RETURN
 
C
C     Local variables
C
      DOUBLE PRECISION      D
      DOUBLE PRECISION      N1
      DOUBLE PRECISION      N2
      DOUBLE PRECISION      N3
      DOUBLE PRECISION      UNIT ( 3, 3 )
 
      LOGICAL               DETOK
      LOGICAL               NORMOK
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         ISROT = .FALSE.
         RETURN
      ELSE
         CALL CHKIN ( 'ISROT' )
      END IF
 
C
C     Tolerances must be non-negative.
C
      IF ( NTOL .LT. 0.D0 ) THEN
 
         ISROT = .FALSE.
 
         CALL SETMSG ( 'NTOL should be non-negaitve; it is #.' )
         CALL ERRDP  ( '#',  NTOL                              )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                )
         CALL CHKOUT ( 'ISROT'                                 )
         RETURN
 
      ELSE IF ( DTOL .LT. 0.D0 ) THEN
 
         ISROT = .FALSE.
 
         CALL SETMSG ( 'DTOL should be non-negaitve; it is #.' )
         CALL ERRDP  ( '#',  DTOL                              )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)' )
         CALL CHKOUT ( 'ISROT'                  )
         RETURN
 
      END IF
 
C
C     The columns of M must resemble unit vectors.  If the norms are
C     outside of the allowed range, M is not a rotation matrix.
C
C     Also, the columns of M are required to be pretty nearly
C     orthogonal.  The discrepancy is gauged by taking the determinant
C     of the matrix UNIT, computed below, whose columns are the
C     unitized columns of M.
C
      CALL UNORM (  M(1,1),  UNIT(1,1),  N1  )
      CALL UNORM (  M(1,2),  UNIT(1,2),  N2  )
      CALL UNORM (  M(1,3),  UNIT(1,3),  N3  )
 
      D      =  DET ( UNIT )
 
 
      NORMOK =       ( N1 .EQ. BRCKTD ( N1, 1.D0 - NTOL, 1.D0 + NTOL ) )
     .         .AND. ( N2 .EQ. BRCKTD ( N2, 1.D0 - NTOL, 1.D0 + NTOL ) )
     .         .AND. ( N3 .EQ. BRCKTD ( N3, 1.D0 - NTOL, 1.D0 + NTOL ) )
 
      DETOK  =         D  .EQ. BRCKTD ( D,  1.D0 - DTOL, 1.D0 + DTOL )
 
 
      IF ( NORMOK .AND. DETOK ) THEN
         ISROT = .TRUE.
      ELSE
         ISROT = .FALSE.
      END IF
 
      CALL CHKOUT ( 'ISROT' )
      RETURN
      END
