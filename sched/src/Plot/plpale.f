      SUBROUTINE PLPALE( PMIN, PMAX, PTYPE, BEMIN, GEMIN )
C
C     Routine for sched that set a palette for plot beam.
C
      INTEGER       PMIN, PMAX, PTYPE, J, I, K, ISC
      REAL          BEMIN, GEMIN, CR, CG, CB, CINT
C ----------------------------------------------------------------------
C
C     Set Palette
C
      IF( PTYPE .EQ. 0 ) THEN
         CINT = 1.0 / ( PMAX - PMIN + 1)
C
C        Gray Color Palette
C
         DO 10 I = PMIN, PMAX
            CG = CINT * ( I - PMIN )
            CALL PGSCR( I, CG, CG, CG )
 10      CONTINUE
C
      ELSE IF( PTYPE .EQ. 1 ) THEN
         ISC  = ( PMAX - PMIN + 1 ) / 3
         CINT = 1.0 / ISC
C
C        Thermal Color Palette
C
         DO 20 I = PMIN, PMAX
            K = I - PMIN + 1
            IF( K .LT. ISC ) THEN
                CG = 0.0
                CB = 0.0
                CR = CINT * K
            ELSE IF( K .LT. ( ISC * 2 ) ) THEN
                    CR = 1.0
                    CG = ( CINT / 2 ) * ( K - ISC )
                    CB = 0.0
            ELSE
                        CR = 1.0
                        CG = ( CINT / 2 ) * ( K - ISC ) 
                        CB = ( CINT ) * ( K - ( ISC * 2 ) )
            END IF
            CALL PGSCR( I, CR, CG, CB )
 20      CONTINUE
C
      ELSE
         ISC  = ( PMAX - PMIN + 1 ) / 3
         CINT = 1.0 / ISC
C
C        RGB
C
         DO 30 I = PMIN, PMAX
            K = I - PMIN + 1
            IF( K .LT. ISC ) THEN
                CR = 0.4 + ( CINT / 2 * K )
                CG = 0.0
                CB = 0.0
            ELSE IF( K .LT. ( ISC * 2 ) ) THEN
                CR = 0.0
                CG = 0.4 + ( CINT / 2  * ( K - ISC ) )
                CB = 0.0
            ELSE
                J = K - ( ISC * 2 )
                CR = CINT / 2 * J
                CG = CINT / 2 * J
                CB = 0.5 + ( CINT / 3 * J)
            END IF
            CALL PGSCR( I, CR, CG, CB )
 30      CONTINUE
      END IF
C
      CALL PGSCR( PMIN, 0.0, 0.0, 0.0 )
      IF( BEMIN .EQ. GEMIN ) THEN
         CALL PGSCR( PMAX, 1.0, 1.0, 1.0 )
      END IF
C
      RETURN
      END
