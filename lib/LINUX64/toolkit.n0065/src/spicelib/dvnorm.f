C$Procedure DVNORM ( Derivative of vector norm )

       DOUBLE PRECISION FUNCTION DVNORM( STATE )

C$ Abstract
C
C     Function to calculate the derivative of the norm of a 3-vector.
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
C     DERIVATIVE
C     MATH
C     VECTOR
C
C$ Declarations

      DOUBLE PRECISION      STATE ( 6 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STATE      I   A 6-vector composed of three coordinates and their
C                    derivatives.
C
C$ Detailed_Input
C
C     STATE      A double precision 6-vector, the second three
C                components being the derivatives of the first three
C                with respect to some scalar.
C                
C                   STATE =  ( x, dx )
C                                 --
C                                 ds
C
C                A common form for STATE would contain position and 
C                velocity.
C
C$ Detailed_Output
C
C     DVNORM     The value of d||x|| corresponding to STATE.
C                             ------
C                               ds
C
C                                   1/2         2    2    2  1/2
C              where ||x|| = < x, x >    =  ( x1 + x2 + x3 )
C
C
C                        v = ( dx1, dx2, dx3 )
C                              ---  ---  ---
C                              ds   ds   ds
C
C                   d||x||   < x, v >  
C                   ------ =  ------     =  < xhat, v >
C                     ds            1/2
C                            < x, x > 
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C   A common use for this routine is to calculate the time derivative 
C   of the radius corresponding to a state vector.
C
C$ Examples
C
C   Any numerical results shown for this example may differ between
C   platforms as the results depend on the SPICE kernels used as input
C   and the machine specific arithmetic implementation.
C
C
C           PROGRAM DVNORM_T
C           IMPLICIT NONE
C
C           DOUBLE PRECISION      X     (3)
C           DOUBLE PRECISION      MAG   (3)
C           DOUBLE PRECISION      DVMAG (3)
C           DOUBLE PRECISION      Y     (6)
C
C           DOUBLE PRECISION      DVNORM
C     C
C     C     Create several 6-vectors (6x1 arrays) with the structure
C     C
C     C        s = |  x  |
C     C            |     |
C     C            |  dx |
C     C            |  -- |
C     C            |  ds |
C     C
C     C      where 'x' is a 3-vector (3x1 array).
C     C
C    
C     C
C     C      Create 's' with 'x' of varying magnitudes. Use 'x'
C     C      and '-x' to define the derivative as parallel and
C     C      anti-parallel.
C     C
C           MAG(1) =  -4.D0
C           MAG(2) =   4.D0
C           MAG(3) =  12.D0
C     
C           X(1)   = 1.D0
C           X(2)   = DSQRT( 2.D0 )
C           X(3)   = DSQRT( 3.D0 )
C     
C     C
C     C     Parallel...
C     C
C           Y(1)   = X(1) * 10.D0**MAG(1)
C           Y(2)   = X(2) * 10.D0**MAG(1)
C           Y(3)   = X(3) * 10.D0**MAG(1)
C           Y(4)   = X(1) 
C           Y(5)   = X(2) 
C           Y(6)   = X(3) 
C     
C           WRITE(*,*) 'Parallel x, dx/ds         : ', DVNORM( Y )
C     
C     C
C     C     ... anti-parallel...
C     C
C           Y(1)   = X(1) * 10.D0**MAG(2)
C           Y(2)   = X(2) * 10.D0**MAG(2)
C           Y(3)   = X(3) * 10.D0**MAG(2)
C           Y(4)   = -X(1) 
C           Y(5)   = -X(2) 
C           Y(6)   = -X(3) 
C     
C           WRITE(*,*) 'Anti-parallel x, dx/ds    : ', DVNORM( Y )
C     
C     C
C     C     ... 'x' zero vector
C     C
C           Y(1)   = 0.D0
C           Y(2)   = 0.D0
C           Y(3)   = 0.D0
C           Y(4)   = X(1) * 10.D0**MAG(3)
C           Y(5)   = X(2) * 10.D0**MAG(3)
C           Y(6)   = X(3) * 10.D0**MAG(3)
C     
C           WRITE(*,*) 'Zero vector x, large dx/ds: ', DVNORM( Y )
C           END
C
C   The program outputs:
C
C      Parallel x, dx/ds         :   2.44948974
C      Anti-parallel x, dx/ds    :  -2.44948974
C      Zero vector x, large dx/ds:   0.
C
C$ Restrictions
C
C     Error free.
C
C     1) If the first three components of STATE ("x") describes the 
C        origin (zero vector) the routine returns zero as the 
C        derivative of the vector norm.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     Ed Wright     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 03-MAY-2010 (EDW)
C
C-&


C$ Index_Entries
C
C   derivative of 3-vector norm
C
C-&

C
C     SPICELIB functions.
C
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

C
C     Local Variables.
C
      DOUBLE PRECISION      XHAT ( 3 )

C
C     If "x" describes the zero vector, return zero as the derivative
C     of the vector norm.
C
      IF ( VNORM( STATE(1) ) .EQ. 0.D0 ) THEN

         DVNORM = 0.D0
         RETURN

      END IF

C
C     Construct a unit vector from the x vector data
C     in STATE.
C
      CALL VHAT( STATE(1), XHAT )

C
C     Project the velocity components onto the XHAT vector.
C
C      d ||x||          x
C      -------  = v . -----
C        ds           ||x||
C

      DVNORM = VDOT( STATE(4), XHAT )

      RETURN
      END
