      SUBROUTINE PLSOSL( MODE )
C
C     Scroll Left or Right the SOURCES area
C     If MODE equal 1 Clear the Area
C
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      CHARACTER     STRING*14, ANUM*3
      INTEGER       MODE, I, J, K, N, REV 
      REAL          XC1, XC2, YC1, YC2, XL, CSIZ
C ----------------------------------------------------------------------
C
C     Set Font dimension, Select Buttons Policy
C     and Label offset.
C
      CSIZ  = PPNDIM(3)
      XL    = 0.02
C
C     Save PGPLOT calling attribute
C
      CALL PGSAVE
C
C     Clear Stations Area
C
      N = PSOPLT / 2 
      IF( MODE .EQ. 1 ) THEN
         XC1 = PPSDIM(1)   - 0.01
         XC2 = PPSDIM(2)   + 0.01
         YC1 = PSOBXP(3,N) - 0.01
         YC2 = PSOBXP(4,1) + 0.01
         CALL PGSCI( 0 )
         CALL PGRECT( XC1, XC2, YC1, YC2 )
      END IF
C
C     Draw the sources box
C     If sources greater than 10 set the max sources button to 10
C     else to NSRC. The name index is set relative to the scroll index
C
      J = PSOCNT * N
      K = NSRC - J
      IF( K .GT. PSOPLT ) K = PSOPLT
C
      DO 20 I=1,K
         J = J + 1
         REV = 0
         IF ( PSOBCK(J) .GT. 0 ) REV = 1
         CALL PLBUTS( .TRUE., PSOBXP(1,I), PSOBXP(2,I), PSOBXP(3,I),
     1                PSOBXP(4,I), XL, CSIZ, SRCNAME(J), 0, REV)
 20   CONTINUE
C
C     Make and plot a Scroll Bar with Label
C
      IF( PSOSCR ) THEN
         J = PSOCNT * ( PSOPLT / 2 )
         K = NSRC - J
         IF( K .GT. PSOPLT ) THEN
            K = J + PSOPLT
         ELSE
            K = NSRC
         END IF
C
         J = J + 1
         N = 1
         CALL PGNUMB( J, 0, 1, ANUM, I )
         STRING = ANUM(1:I)//'-'
         N = N + I + 1
         CALL PGNUMB( K, 0, 1, ANUM, I )
         STRING(N:) = ANUM(1:I)//' of '
         N = N + I + 4
         CALL PGNUMB( NSRC, 0, 1, ANUM, I )
         STRING(N:) = ANUM(1:I)
C
         CALL PLSBAR( PSOSBR(1), PSOSBR(2), PSOSBR(3),
     1                PSOSBR(4), 2, 1, PSOBXM )
C
         CALL PGSLW( 5 )
         CALL PGSCF( 1 )
         CALL PGSCH( 1.0 )
         CALL PGSCI( 15 )
         YC1 = PSOSBR(3) + 0.01
         XC1 = PSOSBR(1) + 0.05
         CALL PGPTXT(XC1, YC1, 0.0, 0.0, STRING )
      END IF
C      
C     Restore PGPLOT calling attributes
C
      CALL PGUNSA
C
      RETURN
      END
