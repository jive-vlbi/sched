C$Procedure      ZZSIZEOK ( Determine if the size of a segment is ok )
 
      SUBROUTINE ZZSIZEOK ( SIZE, PSIZE, DSIZE, OFFSET, OK, N )
 
C$ Abstract
C
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This routine exists to determine whether or not the type of
C     a DAF segment is compatible with the sizes allowed for SPK
C     type 01 segments or CK type 02 segments.  However, more generally
C     it determines whether or not the integer equation:
C
C         SIZE = PSIZE*N + (N-OFFSET)/DSIZE
C
C     can be satisfied for some value of N.  Moreover, if such
C     an N exists (there can be only one) it returns that value.
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
C     NUMERIC
C
C$ Declarations
 
      IMPLICIT NONE
      INTEGER               SIZE
      INTEGER               PSIZE
      INTEGER               DSIZE
      INTEGER               OFFSET
      LOGICAL               OK
      INTEGER               N
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     SIZE       I   Left hand side of the equation in the abstract
C     PSIZE      I   Coefficient of N (packet size).
C     DSIZE      I   Divisor of N-OFFSET (directory size).
C     OFFSET     I   Offset used in computation of number of directories
C     OK         O   TRUE if a solution for N exists.
C     N          O   Value of N if there is a solution, 0 otherwise.
C
C$ Detailed_Input
C
C     SIZE       Constant terms in the equation given in the abstract.
C     PSIZE
C     DSIZE
C
C     OFFSET     Constant term in the equation above.  It should be
C                1 or 0.
C
C$ Detailed_Output
C
C     OK         is TRUE if an integer solution for N exists.  Otherwise
C                it is returned FALSE.
C
C     N          is the solution to the equation in the abstract
C                if such a solution exists.  Otherwise it is returned
C                with the value zero.
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
C     1)  If SIZE, PSIZE, or DSIZE is less than 1, OK is set to FALSE
C         N is set to zero and no attempt is made at finding a
C         solution.
C
C$ Particulars
C
C     This routine determines whether or not the integer arithmetic
C     equation
C
C         SIZE = PSIZE*N + (N-1)/DSIZE
C
C     has a solution for N and if so returns the value of N.
C
C     The routine is intended for checking the sizes of segments
C     for SPK type 01 and CK type 02.  For SPK type 01,
C
C        SIZE   = segment size - 1
C        PSIZE  = 72
C        DSIZE  = 100
C        OFFSET = 0
C
C
C     for CK type 02,
C
C        SIZE   = segment size
C        PSIZE  = 10
C        DSIZE  = 100
C        OFFSET = 1
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
C-    SPICELIB Version 1.0.0, 02-DEC-1999 (WLT)
C
C
C-&
 
C
C     Here's the scoop.
C
C     Suppose N is a solution to SIZE = PSIZE*N + (N-OFFSET)/DSIZE
C     N can be represented uniquely as
C
C         N = q*DSIZE + r
C
C     where OFFSET <= r <= DSIZE+OFFSET-1.  Therefore there must
C     be values q and r such that
C
C        SIZE = PSIZE*(q*DSIZE + r ) + ( q*DSIZE + r - 1 ) / DSIZE
C
C             = PSIZE*DSIZE*q + q + PSIZE*r
C
C             = (PSIZE*DSIZE+1)*q + PSIZE*r
C
C     But SIZE can be represented uniquely as
C
C           SIZE = (PSIZE*DSIZE+1)*k + a
C
C     where  0 <= a < (PSIZE*DSIZE+1).
C
C     But   PSIZE*OFFSET < PSIZE*r < (PSIZE*DSIZE+OFFSET-1),
C    therefore  it must be that
C
C              SIZE mod(PSIZE*DSIZE+1) = PSIZE*r
C     and                            q = k
C
C     Hence, there is a solution to our equation if and only if
C
C          PSIZE divides SIZE mod(PSIZE*DSIZE+1)
C     and  OFFSET*PSIZE <= SIZE mod(PSIZE*DSIZE+1)
C
 
      INTEGER               Q
      INTEGER               PD1
      INTEGER               R
      INTEGER               A
 
 
C
C     Handle the exceptional case first.
C
      IF (       SIZE .LE. 0
     .     .OR. DSIZE .LE. 0
     .     .OR. PSIZE .LE. 0 ) THEN
         N  = 0
         OK = .FALSE.
         RETURN
      END IF
 
      PD1 = PSIZE*DSIZE + 1
 
      CALL RMAINI ( SIZE, PD1, Q, A )
 
      IF ( OFFSET*PSIZE .GT. A ) THEN
         N  = 0
         OK = .FALSE.
         RETURN
      END IF
 
      IF ( A .EQ. (A/PSIZE)*PSIZE ) THEN
 
         R  = A/PSIZE
         N  = DSIZE*Q + R
         OK = .TRUE.
 
      ELSE
 
         OK = .FALSE.
         N  =  0
 
      END IF
 
      RETURN
      END
