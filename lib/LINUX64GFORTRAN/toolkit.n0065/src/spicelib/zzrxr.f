C$Procedure      ZZRXR ( Multiply sequence of 3x3 matrices )
 
 
      SUBROUTINE ZZRXR ( MATRIX, N, OUTPUT )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This routine multiplies together a sequence of state
C     transformation matrices.
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
C     PRIVATE
C
C$ Declarations
 
      IMPLICIT NONE
      DOUBLE PRECISION      MATRIX ( 3, 3, * )
      INTEGER               N
      DOUBLE PRECISION      OUTPUT ( 3, 3    )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     MATRIX     I   A sequence of state transformation matrices
C     N          I   The number of 3x3  matrices
C     OUTPUT     O   The product of the 3x3 matrices.
C
C$ Detailed_Input
C
C     MATRIX      is an array of 3x3  matrices.
C
C     N           is an integer giving the number of matrices in the
C                 sequence.
C
C
C$ Detailed_Output
C
C     OUTPUT      is the product of the matrices stored in MATRIX.
C                 Specifically, it is the result of the product
C
C                       M_N * M_(N-1) * ... * M_2 * M_1
C
C                 where the K'th matrix M_K is define by the
C                 relationship
C
C                    M_K( I, J )  = MATRIX ( I, J, K )
C
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
C     Error free.
C
C     1) If N is 0 or smaller OUTPUT will be returned as the
C        3x36 identity matrix.
C
C     2) IF N is 1 OUTPUT will be returned as M_1 where M_1 is
C        the matrix defined above in the description of OUTPUT.
C
C$ Particulars
C
C     This is a private SPICE routine that computes the product
C     of a sequence of 3x3  matrices.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 03-MAR-1999 (WLT)
C
C
C-&
 
      INTEGER               I
      INTEGER               J
      INTEGER               K
 
      INTEGER               GET
      INTEGER               PUT
      INTEGER               INCR
 
      DOUBLE PRECISION      TEMP ( 3, 3, 2 )
 
C
C    If we have more than 2 matrices to deal with we will need to
C    set up the PUT location
C
      PUT = 1
 
C
C     We perform tests in the order they seem most likely to
C     occur.
C
      IF ( N .EQ. 2 ) THEN
C
C        If there are exactly two inputs, then the output takes
C        only a single matrix multiply.
C
         DO J = 1,3
            DO K = 1,3
 
               OUTPUT(J,K) = MATRIX(J,1,2)*MATRIX(1,K,1)
     .                     + MATRIX(J,2,2)*MATRIX(2,K,1)
     .                     + MATRIX(J,3,2)*MATRIX(3,K,1)
            END DO
         END DO
 
 
      ELSE IF ( N .GT. 2 ) THEN
 
C
C        We need to compute the product
C
C           MATRIX( , ,N) * MATRIX( , ,N-1) * ... * MATRIX( , , 1 )
C
C        Compute the first product.  MATRIX( , ,2) * MATRIX( , ,1)
C
C
C        First compute the upper left hand 3x3 portion of the product...
C
         DO J = 1,3
            DO K = 1,3
 
               TEMP(J,K,PUT) =  MATRIX(J,1,2)*MATRIX(1,K,1)
     .                        + MATRIX(J,2,2)*MATRIX(2,K,1)
     .                        + MATRIX(J,3,2)*MATRIX(3,K,1)
            END DO
         END DO
 
C
C        Now continue building the product.  Note we will toggle
C        back and forth from TEMP(,,1) to TEMP(,,2) for storing
C        (PUTting) the results of our computations.  This way we
C        don't have to spend time moving any of the our computation
C        results to get ready for the next product.  See the end
C        of the loop below (keeping mind the next three values) to
C        see the little trick that's used to toggle back and forth.
C
         INCR = -1
         PUT  =  2
         GET  =  1
 
         DO I = 3, N-1
C
C           First the uppper left hand portion of the product.
C
            DO J = 1,3
               DO K = 1,3
 
 
                  TEMP(J,K,PUT) =  MATRIX(J,1,I)*TEMP(1,K,GET)
     .                           + MATRIX(J,2,I)*TEMP(2,K,GET)
     .                           + MATRIX(J,3,I)*TEMP(3,K,GET)
 
               END DO
            END DO
 
C
C           And as before, we don't need to compute the upper right
C           or lower right hand 3x3 portions of the matrix. So
C           we just skip them.  Toggle GET and PUT so we will
C           be ready for the next pass.
C
            GET  =  PUT
            PUT  =  PUT + INCR
            INCR =      - INCR
 
         END DO
 
 
C
C        Finally compute the last product.  First the upper
C        left hand portion of the product.
C
         DO J = 1,3
            DO K = 1,3
 
               OUTPUT(J,K) = MATRIX(J,1,N)*TEMP(1,K,GET)
     .                     + MATRIX(J,2,N)*TEMP(2,K,GET)
     .                     + MATRIX(J,3,N)*TEMP(3,K,GET)
            END DO
         END DO
 
 
      ELSE IF ( N .EQ. 1 ) THEN
C
C        If there is only one matrix in the list the output is
C        simply the input.
C
         DO I = 1, 3
            DO J = 1, 3
               OUTPUT(J,I) = MATRIX(J,I,1)
            END DO
         END DO
 
 
      ELSE IF ( N .LE. 0 ) THEN
 
         CALL IDENT ( OUTPUT )
 
      END IF
 
      RETURN
      END
