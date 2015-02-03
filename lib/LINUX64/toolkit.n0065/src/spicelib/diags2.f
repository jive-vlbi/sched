C$Procedure  DIAGS2   ( Diagonalize symmetric 2x2 matrix )
 
      SUBROUTINE DIAGS2 ( SYMMAT, DIAG, ROTATE )
 
C$ Abstract
C
C     Diagonalize a symmetric 2x2 matrix.
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
C     ELLIPSE
C     MATRIX
C     ROTATION
C     TRANSFORMATION
C
C$ Declarations
 
      DOUBLE PRECISION      SYMMAT ( 2, 2 )
      DOUBLE PRECISION      DIAG   ( 2, 2 )
      DOUBLE PRECISION      ROTATE ( 2, 2 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C
C     SYMMAT     I   A symmetric 2x2 matrix.
C     DIAG       O   A diagonal matrix similar to SYMMAT.
C     ROTATE     O   A rotation used as the similarity transformation.
C
C$ Detailed_Input
C
C     SYMMAT         A symmetric 2x2 matrix.  That is, SYMMAT has the
C                    form
C
C                       +-        -+
C                       |  A    B  |
C                       |          |.
C                       |  B    C  |
C                       +-        -+
C
C                    This routine uses only the upper-triangular
C                    elements of SYMMAT, that is, the elements
C
C                       SYMMAT(1,1)
C                       SYMMAT(1,2)
C                       SYMMAT(2,2)
C
C                    to determine the outputs DIAG and ROTATE.
C
C$ Detailed_Output
C
C     DIAG,
C     ROTATE         are, respectively, a diagonal matrix and a 2x2
C                    rotation matrix that satisfy the equation
C
C                                        T
C                       DIAG   =   ROTATE   *  SYMMAT  *  ROTATE.
C
C                    In other words, DIAG is similar to SYMMAT, and
C                    ROTATE is a change-of-basis matrix that
C                    diagonalizes SYMMAT.  DIAGS2 chooses ROTATE so
C                    that its angle of rotation has the smallest
C                    possible magnitude.  If there are two rotations
C                    that meet these criteria (they will be inverses of
C                    one another), either rotation may be chosen.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1)  The matrix element SYMMAT(2,1) is not used in this routine's
C         computations, so the condition
C
C            SYMMAT(1,2)  .NE.  SYMMAT(2,1)
C
C         has no effect on this routine's outputs.
C
C$ Particulars
C
C     The capability of diagonalizing a 2x2 symmetric matrix is
C     especially useful in a number of geometric applications
C     involving quadratic curves such as ellipses.  Such curves are
C     described by expressions of the form
C
C           2                    2
C        A x   +   B xy   +   C y   +   D x    +    E y   +   F   =   0.
C
C     Diagonalization of the matrix
C
C        +-         -+
C        | A     B/2 |
C        |           |
C        | B/2     C |
C        +-         -+
C
C     allows us to perform a coordinate transformation (a rotation,
C     specifically) such that the equation of the curve becomes
C
C           2         2
C        P u   +   Q v   +   R u    +    S v   +   T   =   0
C
C     in the transformed coordinates.  This form is much easier to
C     handle.  If the quadratic curve in question is an ellipse,
C     we can easily find its center, semi-major axis, and semi-minor
C     axis from the second equation.
C
C     Ellipses turn up frequently in navigation geometry problems;
C     for example, the limb and terminator (if we treat the Sun as a
C     point source) of a body modelled as a tri-axial ellipsoid are
C     ellipses.
C
C     A mathematical note:  because SYMMAT is symmetric, we can ALWAYS
C     find an orthogonal similarity transformation that diagonalizes
C     SYMMAT, and we can choose the similarity transformation to be a
C     rotation matrix.  By `orthogonal' we mean that if the ROTATE is
C     the matrix in question, then
C
C              T                         T
C        ROTATE  ROTATE  =  ROTATE ROTATE  =  I.
C
C     The reasons this routine handles only the 2x2 case are:  first,
C     the 2x2 case is much simpler than the general case, in which
C     iterative diagonalization methods must be used, and second, the
C     2x2 case is adequate for solving problems involving ellipses in
C     3 dimensional space.  Finally, this routine can be used to
C     support a routine that solves the general-dimension
C     diagonalization problem for symmetric matrices.
C
C     Another feature of the routine that might provoke curiosity is
C     its insistence on choosing the diagonalization matrix that
C     rotates the original basis vectors by the smallest amount.  The
C     rotation angle of ROTATE is of no concern for most applications,
C     but can be important if this routine is used as part of an
C     iterative diagonalization method for higher-dimensional matrices.
C     In that case, it is most undesirable to interchange diagonal
C     matrix elements willy-nilly; the matrix to be diagonalized could
C     get ever closer to being diagonal without converging.  Choosing
C     the smallest rotation angle precludes this possibility.
C
C$ Examples
C
C     1)  A case that can be verified by hand computation:
C         Suppose SYMMAT is
C
C            +-                -+
C            |  1.0D0    4.0D0  |
C            |                  |
C            |  4.0D0   -5.0D0  |
C            +-                -+
C
C         Then SYMMAT is similar to the diagonal matrix
C
C            +-                -+
C            |  3.0D0    0.0D0  |
C            |                  |
C            |  0.0D0   -7.0D0  |
C            +-                -+
C
C         so
C
C            DIAG(1,1) =  3.D0
C            DIAG(2,1) =  0.D0
C            DIAG(1,2) =  0.D0
C            DIAG(2,2) = -7.D0
C
C         and ROTATE is
C
C            +-                                   -+
C            |   0.894427191          -0.447213595 |
C            |                                     |
C            |   0.447213595           0.894427191 |
C            +-                                   -+
C
C        which is an approximation to
C
C            +-                                   -+
C            |  0.4 * 5**(1/2)     -0.2 * 5**(1/2) |
C            |                                     |
C            |  0.2 * 5**(1/2)      0.4 * 5**(1/2) |
C            +-                                   -+
C
C
C     2)  Suppose we want to find the semi-axes of the ellipse defined
C         by
C                2                 2
C            27 x  +  10 xy  +  3 y   =  1.
C
C         We can write the above equation as the matrix equation
C
C            +-     -+  +-         -+  +- -+
C            | x   y |  | 27     5  |  | x |    =   1;
C            +-     -+  |           |  |   |
C                       |  5     3  |  | y |
C                       +-         -+  +- -+
C
C         let SYMMAT be the symmetric matrix on the left.  The code
C         fragment
C
C            SYMMAT(1,1)  =  27.D0
C            SYMMAT(2,1)  =   5.D0
C            SYMMAT(1,2)  =   5.D0
C            SYMMAT(2,2)  =   3.D0
C
C            CALL DIAGS2 ( SYMMAT, DIAG, ROTATE )
C
C         will return DIAG, an array containing the eigenvalues of
C         SYMMAT, and ROTATE, the coordinate transformation required
C         to diagonalize SYMMAT.  In this case,
C
C            DIAG(1,1)   =  28.D0
C            DIAG(2,1)   =  0.D0
C            DIAG(1,2)   =  0.D0
C            DIAG(2,2)   =  2.D0  
C
C          and
C
C            ROTATE(1,1) =  0.980580676D0
C            ROTATE(2,1) =  0.196116135D0
C            ROTATE(1,2) = -0.196116135D0
C            ROTATE(2,2) =  0.980580676D0
C
C         The columns of ROTATE give the ellipse's axes, after scaling
C         them by
C
C                   1                            1
C            ----------------     and     ---------------
C              ____________                 ____________
C            \/  DIAG(1,1)                \/  DIAG(2,2)
C
C         respectively.
C
C         If SMAJOR and SMINOR are semi-major and semi-minor axes,
C         we can find them as shown below.  For brevity, we omit the
C         check for zero or negative eigenvalues.  Negative or zero
C         eigenvalues will occur only as a result of round-off error;
C         mathematically, the eigenvalues of the matrix SYMMAT are
C         guaranteed to be positive, since they are the reciprocals of
C         the squares of the lengths of the ellipse's semi-axes.
C
C            DO I = 1, 2
C               SMAJOR(I) = ROTATE(I,1)  /  DSQRT( DIAG(1,1) )
C               SMINOR(I) = ROTATE(I,2)  /  DSQRT( DIAG(2,2) )
C            END DO
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     [1]  Calculus, Vol. II.  Tom Apostol.  John Wiley & Sons, 1969.
C          See Chapter 5, `Eigenvalues of Operators Acting on Euclidean
C          Spaces'.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 06-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VHATG and SWAPD calls.
C
C-    SPICELIB Version 1.1.0, 24-JAN-2002 (EDW)
C
C        Edited incorrect examples in the header. The example 
C        outputs did not correspond to the actual function
C        of the routine.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 04-NOV-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     diagonalize symmetric 2x2_matrix
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.2.0, 06-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VHATG and SWAPD calls.
C
C-& 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      EIGVEC ( 2 )
      DOUBLE PRECISION      IDENT  ( 2, 2 )
      DOUBLE PRECISION      ROOT1  ( 2 )
      DOUBLE PRECISION      ROOT2  ( 2 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      TMPD
      DOUBLE PRECISION      TMPV   ( 2 )

C
C     Saved variables
C
      SAVE                  IDENT
 
C
C     Initial values
C
      DATA IDENT /  1.D0, 0.D0,
     .              0.D0, 1.D0  /
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DIAGS2' )
      END IF
 
C
C     We check for the case of a diagonal input matrix, since
C     eigenvector determination is simplified by ruling out this
C     case.
 
      IF  ( SYMMAT(1,2) .EQ. 0.D0 ) THEN
 
         CALL MOVED  ( IDENT,  4, ROTATE )
         CALL MOVED  ( SYMMAT, 4, DIAG   )
C
C        Explicity zero out the (2,1) entry of DIAG, since DIAG is
C        guaranteed to be diagonal.
C
         DIAG(2,1) = 0.D0
 
         CALL CHKOUT ( 'DIAGS2' )
         RETURN
 
      END IF
 
C
C     Getting here means there's some actual work to do.  We start out
C     by scaling our matrix, in order to reduce the chance of overflow.
C     We divide everything by the largest magnitude of any element of
C     SYMMAT.  We're guaranteed that SCALE is non-zero, since the 0
C     matrix is diagonal.
C
      SCALE  =  MAX (  DABS( SYMMAT(1,1) ),
     .                 DABS( SYMMAT(1,2) ),
     .                 DABS( SYMMAT(2,2) )   )
 
      A      =  SYMMAT(1,1) / SCALE
      B      =  SYMMAT(1,2) / SCALE
      C      =  SYMMAT(2,2) / SCALE
 
C
C     Compute the eigenvalues of the scaled version of SYMMAT.  The
C     eigenvalues are roots of the equation
C
C          DET (  (1 / SCALE) * SYMMAT  -  x * IDENTITY  ) = 0,
C
C     or equivalently,
C
C           2                             2
C          x   -  ( A + C ) x  +  ( AC - B )  =   0.
C
C
      CALL RQUAD (  1.D0,  -(A + C),   A*C - B**2,   ROOT1,   ROOT2   )
 
C
C     ROOT1 is the root corresponding to the positive discriminant term;
C     this is guaranteed by RQUAD.
C
      DIAG(1,1) = ROOT1(1)
      DIAG(2,1) = 0.D0
      DIAG(1,2) = 0.D0
      DIAG(2,2) = ROOT2(1)
 
C
C     Our next job is to find an eigenvector corresponding to the
C     eigenvalue of smaller magnitude.  We can unitize it and choose
C     an orthogonal unit vector so as to create the desired rotation
C     matrix.
C
C        If our original matrix is
C
C           +-        -+
C           |  A    B  |
C           |          |,
C           |  B    C  |
C           +-        -+
C
C        then the matrix
C
C           +-                                -+
C           |  A - DIAG(x,x)    B              |
C           |                                  |
C           |  B                C - DIAG(x,x)  |
C           +-                                -+
C
C        maps to zero all elements of the eigenspace corresponding to
C        DIAG(x,x), where x is either 1 or 2.
C
C        So
C
C           +-               -+           +-               -+
C           |  B              |           |  DIAG(x,x) - C  |
C           |                 |   and     |                 |
C           |  DIAG(x,x) - A  |           |  B              |
C           +-               -+           +-               -+
C
C        are candidates for eigenvectors for DIAG(x,x).  To minimize
C        loss of accuracy in our eigenvector due to subtraction of
C        nearly equal quantities, we choose the vector in which the
C        term involving the eigenvalue has the larger magnitude.  The
C        rigorous justification of this choice would literally take
C        pages of explanation, and we are not going to go through it
C        here.  In most cases, either choice is satisfactory, and in
C        the case where cancellation is a problem, our choice is
C        preferable.
C
C        Note that there is nothing to be gained as far as accuracy is
C        concerned by working with one eigenvalue as opposed to the
C        other:  the magnitudes of the quantities DIAG(x,x) - A and
C        DIAG(x,x) - C would be interchanged by taking x = '2' instead
C        of x = '1'.
C
 
      IF (  DABS( DIAG(1,1) - A ) .GE. DABS( DIAG(1,1) - C )  ) THEN
C
C        In this case, the second eigenvector component EIGVEC(2)
C        should be larger than |B|; we explain why in detail below.
C        We use the MAX function below to guard against reversal of the
C        inequality due to round-off error.
C
         EIGVEC(1)  =  B
         EIGVEC(2)  =  MAX (   DIAG(1,1) - A,   ABS(B)   )
 
C
C        Recall that DIAG(1,1) is an eigenvalue of the scaled version
C        of SYMMAT
C
C           +-      -+
C           | A    B |
C           |        |.
C           | B    C |
C           +-      -+
C
C        DIAG(1,1) is the positive-discriminant root of this matrix's
C        characteristic equation.  EIGVEC's y-component
C
C           DIAG(1,1) - A
C
C        is positive and of magnitude at least as large as that of B,
C        since it is the larger of
C                                                 ______________________
C                                                /         2
C                             C - A             / ( A - C )           2
C           DIAG(1,1) - A  =  -----   +     \  /  ----------    +    B
C                               2            \/       4
C
C        and
C                                                 ______________________
C                                                /         2
C                             A - C             / ( A - C )           2
C           DIAG(1,1) - C  =  -----   +     \  /  ----------    +    B
C                               2            \/       4
C
C        Equality between these expressions can occur only when A is
C        equal to C, in which case both expressions are equal (except
C        for round-off error) to |B|.
C
 
C
C        So the argument of EIGVEC is in the interval [pi/4, 3*pi/4].
C        The second eigenvector is EIGVEC, and the first
C        eigenvector is found by rotating EIGVEC by -pi/2.  Since
C        DIAG(1,1) is the eigenvalue for the SECOND eigenvector, we
C        must swap the eigenvalues.
C
 
C
C        Unitize the eigenvector.
C
         CALL VHATG ( EIGVEC, 2, TMPV   )
         CALL MOVED ( TMPV,   2, EIGVEC )
 
         ROTATE(1,1)  =  EIGVEC(2)
         ROTATE(2,1)  = -EIGVEC(1)
         ROTATE(1,2)  =  EIGVEC(1)
         ROTATE(2,2)  =  EIGVEC(2)
 
C
C        Swap DIAG(1,1) and DIAG(2,2).
C
         TMPD         =  DIAG(2,2)
         DIAG(2,2)    =  DIAG(1,1)
         DIAG(1,1)    =  TMPD 
         
      ELSE
 
         EIGVEC(1)  =  MAX (  DIAG(1,1) - C,   ABS(B)  )
         EIGVEC(2)  =  B
 
C
C        The x-component of EIGVEC is positive and has magnitude
C        greater than or equal to that of the y-component of EIGVEC.
C        The argument of EIGVEC is in [-pi/4, pi/4], and the second
C        eigenvector is found by rotating EIGVEC by pi/2.
C
 
C
C        Unitize the eigenvector.
C
         CALL VHATG ( EIGVEC, 2, TMPV   )
         CALL MOVED ( TMPV,   2, EIGVEC )
 
         ROTATE(1,1)  =  EIGVEC(1)
         ROTATE(2,1)  =  EIGVEC(2)
         ROTATE(1,2)  = -EIGVEC(2)
         ROTATE(2,2)  =  EIGVEC(1)
 
      END IF
 
C
C     We must scale the eigenvalues.
C
      DIAG(1,1)  =  DIAG(1,1)  *  SCALE
      DIAG(2,2)  =  DIAG(2,2)  *  SCALE
 
      CALL CHKOUT ( 'DIAGS2' )
      RETURN
      END
