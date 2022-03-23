      SUBROUTINE PLTRAN( VZ, TT, CSU, CSV, IDIMU, IDIMV,
     1                   KL, KH, LL, LH, ML, MH, NL, NH, FNM )
C
C     PLTRAN is a subroutine for fourier transforming in
C     two dimensions an array of data. The actual transform
C     is done by the Fast Fourier Transform subroutine PLFFT
C
C     PLTRAN is an alias of subroutine TRANS that is part of
C     the program "INVERT" ( AUG. 23,1982 )
C
      INCLUDE 'beam.inc'
C
      INTEGER           IDIMU, IDIMV, KL, KH, LL, LH
      INTEGER           ML, MH, NL, NH, L, I, K, M, N, II, JJ
      DOUBLE PRECISION  VZ, FNM, RI(2), TT(KXSIZE*4)
      COMPLEX*16        CSU(KXSIZE*2), CSV(KXSIZE*2), TP
      DIMENSION         VZ(KXSIZE*2,KYSIZE*2)
      EQUIVALENCE       ( RI(1), TP )
C -------------------------------------------------------------------
C
C     Start row loop
C
      DO 40 L = LL, LH
C
C        Clear trans array
C
         DO 10 I = 1, IDIMU
            II       = 2 * I - 1
            TT(II)   = 0.0
            TT(II+1) = 0.0
 10      CONTINUE
C
C        Load into trans array
C
         DO 20 K = KL, KH
            TP       = CMPLX( VZ(K,L), -VZ(K,L) ) * CSU(K)
            JJ       = 2 * K - 1
            TT(JJ)   = RI(1)
            TT(JJ+1) = RI(2)
 20      CONTINUE
C
C        Transform
C
         CALL PLFFT( TT, TT(2), IDIMU, IDIMU, IDIMU, -2 )
C
C        Return to row
C
         DO 30 M = ML, MH
            JJ = 2 * M - 1
            VZ(M,L) = DBLE( CMPLX( TT(JJ), TT(JJ+1) ) * CSU(M) ) * FNM
 30      CONTINUE
C
 40   CONTINUE
C
C     Start column loop
C
      DO 80 M = ML, MH
C
C        Clear trans array
C
         DO 50 I = 1, IDIMV
            II       = 2 * I - 1
            TT(II)   = 0.0
            TT(II+1) = 0.0
 50      CONTINUE
C
C        Load into trans array
C
         DO 60 L = LL, LH
            TP       = CMPLX( VZ(M,L), -VZ(M,L) ) * CSV(L)
            JJ       = 2 * L - 1
            TT(JJ)   = RI(1)
            TT(JJ+1) = RI(2)
 60      CONTINUE
C
C        Transform
C
         CALL PLFFT( TT, TT(2), IDIMV, IDIMV, IDIMV, -2 )
C
C        Return to column
C
         DO 70 N = NL, NH
            JJ      = 2 * N - 1
            VZ(M,N) = DBLE( CMPLX( TT(JJ), TT(JJ+1) ) * CSV(N) ) * FNM
 70      CONTINUE
C
 80   CONTINUE
C
      RETURN
      END
