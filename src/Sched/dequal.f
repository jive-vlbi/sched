      LOGICAL FUNCTION DEQUAL( A, B )
C
C     Function to use to test the "equality" of two double 
C     precision variables making allowance for the fact that they
C     could well differ in the lowest order bits.  Require
C     equality at the level of a part in 1.D-14
C
      DOUBLE PRECISION   A, B
C ------------------------------------------------------------
      DEQUAL = DABS( A - B ) .LT. 1.0D-14 * ( A + B )
      RETURN
      END
