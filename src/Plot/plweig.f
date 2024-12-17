      SUBROUTINE PLWEIG( IVZ, U, V, W, NPTS,
     1                   KLDIMU, KLDIMV, UINT, VINT ) 
C
C     Routine for sched that calculate the Weights array to
C     give Dirty Beam.
C
C     PLWEIG is an alias of subroutine WEIGHT that is part of
C     the program "INVERT" ( AUG. 23,1982 )
C
      INCLUDE 'beam.inc'
C
      INTEGER          NPTS(1), NPNTS
      INTEGER          ENORM, ER0, I, J, Q, IU, IV, KLO, KHI
      INTEGER          JLO, JHI, K, M, N
      INTEGER          KLDIMU, KLDIMV, KLDTU, KLDTV
      DOUBLE PRECISION U(NPBAS,NBASE), V(NPBAS,NBASE)
      DOUBLE PRECISION UINT, VINT
      DOUBLE PRECISION IVZ(KXSIZE*2,KYSIZE*2), W(NPBAS,NBASE)
      DOUBLE PRECISION UU, VV
C -------------------------------------------------------------------
C
      KLDTU = 2 * KLDIMU
      KLDTV = 2 * KLDIMV
      ENORM = 0
      ER0   = 1
C
C     Zero Weight array
C
      DO 5 J = 1, KLDTV
         DO 5 I = 1, KLDTU
            IVZ(I,J) = 0.0
 5    CONTINUE
C
      DO 106 I = 1, NBASE
         NPNTS = NPTS(I)
C
         DO 65 Q = 1, NPNTS
            ENORM = ENORM + 1
            UU  = U(Q,I)
            VV  = V(Q,I)
            IU  = UU / UINT + KLDIMU + 1
            IV  = VV / VINT + KLDIMV + 1
            KLO = MAX0(IU-1, 1)
            KHI = MIN0(IU+1, KLDTU)
            JLO = MAX0(IV-1, 1)
            JHI = MIN0(IV+1, KLDTV)
C
            DO 63 J = JLO, JHI
               DO 63 K = KLO, KHI
                  IVZ(K,J) = IVZ(K,J) + 1 / ER0
                  M        = KLDTU - K + 1
                  N        = KLDTV - J + 1
                  IVZ(M,N) = IVZ(M,N) + 1 / ER0
 63         CONTINUE
 65      CONTINUE
 106  CONTINUE
C
C     Calculate Weights
C
      DO 131 I = 1, NBASE
         NPNTS = NPTS(I)
         DO 130 Q = 1, NPNTS
            UU     = U(Q,I)
            VV     = V(Q,I)
            IU     = UU / UINT + KLDIMU + 1
            IV     = VV / VINT + KLDIMV + 1
            W(Q,I) = 1.0 / IVZ(IU,IV)
 130     CONTINUE
 131  CONTINUE
C
      DO 20 J = 1, KLDTV
         DO 20 I = 1, KLDTU
            IVZ(I,J) = IVZ(I,J) / ENORM
 20   CONTINUE
C
      RETURN
      END
