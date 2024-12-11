      SUBROUTINE PLFFT( A, B, NTOT, N, NSPAN, ISN )
C
C     Multivariate Complex Fourier Transform, computed in place
C     using mixed-radix Fast Fourier Transform algorithm.
C     By R. C. SINGLETON, Stanford Research Institute, Oct. 1968.
C
C     Arrays A and B originally hold the real and imaginary
C     components of the data, and return the real and
C     imaginary components of the resulting fourier coefficients.
C
C     Multivariate data is indexed according to the fortran
C     array element successor function, without limit
C     on the number of implied multiple subscripts.
C     The subroutine is called once for each variate.
C     The calls for a multivariate transform may be in any order.
C
C     NTOT is the total number of complex data values.
C
C     N is the dimension of the current variable.
C
C     NSPAN/N is the spacing of consecutive values while indexing
C     the current variable.
C
C     The sign of ISN determines the sign of the complex
C     exponential, and the magnitude of ISN is normally one.
C
C     A tri-variate transform with A(N1,N2,N3), B(N1,N2,N3)
C     is computed by
C       CALL FFT(A,B,N1*N2*N3,N1,N1,1)
C       CALL FFT(A,B,N1*N2*N3,N2,N1*N2,1)
C       CALL FFT(A,B,N1*N2*N3,N3,N1*N2*N3,1)
C
C     For a single-variate transform,
C     NTOT = N = NSPAN = (number of complex data values), F.G.
C       CALL FFT(A,B,N,N,N,1)
C
C     The data may alternatively be stored in a single complex
C     array A, then the magnitude of ISN is changed to two to
C     give the correct indexing increment and A(2) is used to
C     pass the initial address for the sequence of imaginary
C     values, E.G.
C       CALL FFT(A,A(2),NTOT,N,NSPAN,2)
C
C     Arrays AT(MAXF), CK(MAXF), BT(MAXF), SK(MAXF), and NP(MAXP)
C     are used for temporary storage.  If the available storage
C     is insufficient, the program is terminated by a stop.
C     MAXF must be .GE. the maximum prime factor of N.
C     MAXP must be .GT. the number of prime factors of N.
C     In addition, if the square-free portion K of N has two or
C     more prime factors, then MAXP must be .GE. K-1.
C
C     Array storage in NFAC for a maximum of 11 factors of N.
C     If N has more than one square-free factor, the product of the
C     square-free factors must be .LE. 210
C
C     Array storage for maximum prime factor of 23
C
C     PLFFT is an alias of subroutine FFT that is part of
C     the program "INVERT" ( AUG. 23,1982 )
C
      INCLUDE 'beam.inc'
C
      INTEGER           NTOT, N, NSPAN, ISN
      INTEGER           I, M, K, J, JJ, II, NP, NT, NN, JC, JF, KS
      INTEGER           KT, KK, K1, K2, K3, K4
      INTEGER           INC, NFAC, MAXF, MAXP, KSPAN, KSPNN
      DOUBLE PRECISION  A, B
      DOUBLE PRECISION  AT, CK, BT, SK, AA, AJ, AK, BB, BJ, BK
      DOUBLE PRECISION  CD, C1, C2, C3, SD, S1, S2, S3, AKP, AKM
      DOUBLE PRECISION  AJP, AJM, BKP, BKM, BJP, BJM, RAD, RADF
      DOUBLE PRECISION  S72, C72, S120
      DIMENSION         A(KXSIZE*4), B(KXSIZE*4-1)
      DIMENSION         NFAC(11), NP(209)
      DIMENSION         AT(23), CK(23), BT(23), SK(23)
      EQUIVALENCE       (I, II)
C -------------------------------------------------------------------
C
C     The following two constants should agree with the array
C     dimensions.
C
      MAXF = 23
      MAXP = 209
      IF( N .LT. 2 ) RETURN
C
      INC  = ISN
      RAD  = 8.0 * ATAN( 1.0 )
      S72  = RAD / 5.0
      C72  = COS( S72 )
      S72  = SIN( S72 )
      S120 = SQRT( 0.75 )
C
      IF( ISN .LT. 0 ) THEN
         S72  = -S72
         S120 = -S120
         RAD  = -RAD
         INC  = -INC
      END IF
C
      NT    = INC * NTOT
      KS    = INC * NSPAN
      KSPAN = KS
      NN    = NT - INC
      JC    = KS / N
      RADF  = RAD * FLOAT( JC ) * 0.5
      I     = 0
      JF    = 0
C
C     Determine the factors of N
C
      M = 0
      K = N
      GOTO 20
C
 15   M       = M + 1
      NFAC(M) = 4
      K       = K / 16
C
 20   IF( K - ( K / 16 ) * 16 .EQ. 0 ) GOTO 15
      J  = 3
      JJ = 9
      GOTO 30
C
 25   M       = M + 1
      NFAC(M) = J
      K       = K / JJ
C
 30   IF( MOD( K, JJ ) .EQ. 0 ) GOTO 25
      J  = J + 2
      JJ = J**2
      IF( JJ .LE. K ) GOTO 30
      IF( K  .GT. 4 ) GOTO 40
      KT        = M
      NFAC(M+1) = K
      IF( K .NE. 1 ) M = M + 1
      GOTO 80
C
 40   IF( K - ( K / 4 ) * 4 .NE. 0 ) GOTO 50
      M       = M + 1
      NFAC(M) = 2
      K       = K / 4
C
 50   KT = M
      J  = 2
C
 60   IF( MOD( K, J ) .NE. 0 ) GOTO 70
      M=M+1
      NFAC(M)=J
      K=K/J
   70 J=((J+1)/2)*2+1
      IF(J .LE. K) GO TO 60
   80 IF(KT .EQ. 0) GO TO 100
      J=KT
   90 M=M+1
      NFAC(M)=NFAC(J)
      J=J-1
      IF(J .NE. 0) GO TO 90
C
C     Compute Fourier Transform
C
  100 SD=RADF/FLOAT(KSPAN)
      CD=2.0*SIN(SD)**2
      SD=SIN(SD+SD)
      KK=1
      I=I+1
      IF(NFAC(I) .NE. 2) GO TO 400
C
C     Transform for factor of 2 (including rotation factor)
C
      KSPAN=KSPAN/2
      K1=KSPAN+2
  210 K2=KK+KSPAN
      AK=A(K2)
      BK=B(K2)
      A(K2)=A(KK)-AK
      B(K2)=B(KK)-BK
      A(KK)=A(KK)+AK
      B(KK)=B(KK)+BK
      KK=K2+KSPAN
      IF(KK .LE. NN) GO TO 210
      KK=KK-NN
      IF(KK .LE. JC) GO TO 210
      IF(KK .GT.KSPAN) GO TO 800
  220 C1=1.0-CD
      S1=SD
  230 K2=KK+KSPAN
      AK=A(KK)-A(K2)
      BK=B(KK)-B(K2)
      A(KK)=A(KK)+A(K2)
      B(KK)=B(KK)+B(K2)
      A(K2)=C1*AK-S1*BK
      B(K2)=S1*AK+C1*BK
      KK=K2+KSPAN
      IF(KK .LT. NT) GO TO 230
      K2=KK-NT
      C1=-C1
      KK=K1-K2
      IF(KK .GT. K2) GO TO 230
      AK=C1-(CD*C1+SD*S1)
      S1=(SD*C1-CD*S1)+S1
C
C     The following three statements compensate for truncation
C     error.  If rounded arithmetic is used, substitute
C     C1=AK
C
C      C1=0.5/(AK**2+S1**2)+0.5
C      S1=C1*S1
C      C1=C1*AK
      C1=AK
C
      KK=KK+JC
      IF(KK .LT. K2) GO TO 230
      K1=K1+INC+INC
      KK=(K1-KSPAN)/2+JC
      IF(KK .LE. JC+JC) GO TO 220
      GO TO 100
C
C     Transform for factor of 3 (optional code)
C
  320 K1=KK+KSPAN
      K2=K1+KSPAN
      AK=A(KK)
      BK=B(KK)
      AJ=A(K1)+A(K2)
      BJ=B(K1)+B(K2)
      A(KK)=AK+AJ
      B(KK)=BK+BJ
      AK=-0.5*AJ+AK
      BK=-0.5*BJ+BK
      AJ=(A(K1)-A(K2))*S120
      BJ=(B(K1)-B(K2))*S120
      A(K1)=AK-BJ
      B(K1)=BK+AJ
      A(K2)=AK+BJ
      B(K2)=BK-AJ
      KK=K2+KSPAN
      IF(KK .LT. NN) GO TO 320
      KK=KK-NN
      IF(KK .LE. KSPAN) GO TO 320
      GO TO 700
C
C     Transform for factor of 4
C
  400 IF(NFAC(I) .NE. 4) GO TO 600
      KSPNN=KSPAN
      KSPAN=KSPAN/4
  410 C1=1.0
      S1=0
  420 K1=KK+KSPAN
      K2=K1+KSPAN
      K3=K2+KSPAN
      AKP=A(KK)+A(K2)
      AKM=A(KK)-A(K2)
      AJP=A(K1)+A(K3)
      AJM=A(K1)-A(K3)
      A(KK)=AKP+AJP
      AJP=AKP-AJP
      BKP=B(KK)+B(K2)
      BKM=B(KK)-B(K2)
      BJP=B(K1)+B(K3)
      BJM=B(K1)-B(K3)
      B(KK)=BKP+BJP
      BJP=BKP-BJP
      IF(ISN.LT.0) GO TO 450
      AKP=AKM-BJM
      BKP=BKM+AJM
      BKM=BKM-AJM
      IF(S1.EQ.0.0) GO TO 460
  430 A(K1)=AKP*C1-BKP*S1
      B(K1)=AKP*S1+BKP*C1
      A(K2)=AJP*C2-BJP*S2
      B(K2)=AJP*S2+BJP*C2
      A(K3)=AKM*C3-BKM*S3
      B(K3)=AKM*S3+BKM*C3
      KK=K3+KSPAN
      IF(KK.LE.NT) GO TO 420
  440 C2=C1-(CD*C1+SD*S1)
      S1=(SD*C1-CD*S1)+S1
C
C     The following three statements compensate for truncation
C     error. If rounded arithmetic is used, substitute
C     C1=C2
C
C      C1=0.5/(C2**2+S1**2)+0.5
C      S1=C1*S1
C      C1=C1*C2
      C1=C2
C
      C2=C1**2-S1**2
      S2=2.0*C1*S1
      C3=C2*C1-S2*S1
      S3=C2*S1+S2*C1
      KK=KK-NT+JC
      IF(KK.LE.KSPAN) GO TO 420
      KK=KK-KSPAN+INC
      IF(KK.LE.JC) GO TO 410
      IF(KSPAN.EQ.JC) GO TO 800
      GO TO 100
  450 AKP=AKM+BJM
      AKM=AKM-BJM
      BKP=BKM-AJM
      BKM=BKM+AJM
      IF(S1.NE.0.0) GO TO 430
  460 A(K1)=AKP
      B(K1)=BKP
      A(K2)=AJP
      B(K2)=BJP
      A(K3)=AKM
      B(K3)=BKM
      KK=K3+KSPAN
      IF(KK.LE.NT) GO TO 420
      GO TO 440
C
C     Transform for factor of 5 (optional code)
C
  510 C2=C72**2-S72**2
      S2=2.0*C72*S72
  520 K1=KK+KSPAN
      K2=K1+KSPAN
      K3=K2+KSPAN
      K4=K3+KSPAN
      AKP=A(K1)+A(K4)
      AKM=A(K1)-A(K4)
      BKP=B(K1)+B(K4)
      BKM=B(K1)-B(K4)
      AJP=A(K2)+A(K3)
      AJM=A(K2)-A(K3)
      BJP=B(K2)+B(K3)
      BJM=B(K2)-B(K3)
      AA=A(KK)
      BB=B(KK)
      A(KK)=AA+AKP+AJP
      B(KK)=BB+BKP+BJP
      AK=AKP*C72+AJP*C2+AA
      BK=BKP*C72+BJP*C2+BB
      AJ=AKM*S72+AJM*S2
      BJ=BKM*S72+BJM*S2
      A(K1)=AK-BJ
      A(K4)=AK+BJ
      B(K1)=BK+AJ
      B(K4)=BK-AJ
      AK=AKP*C2+AJP*C72+AA
      BK=BKP*C2+BJP*C72+BB
      AJ=AKM*S2-AJM*S72
      BJ=BKM*S2-BJM*S72
      A(K2)=AK-BJ
      A(K3) = AK+BJ
      B(K2)=BK+AJ
      B(K3)=BK-AJ
      KK = K4+KSPAN
      IF(KK.LT.NN) GO TO 520
      KK=KK-NN
      IF(KK.LE.KSPAN) GO TO 520
      GO TO 700
C
C     Transform for odd factors
C
  600 K=NFAC(I)
      KSPNN=KSPAN
      KSPAN=KSPAN/K
      IF(K.EQ.3) GOTO 320
      IF(K.EQ.5) GOTO 510
      IF(K.EQ.JF) GOTO 640
      JF = K
      S1=RAD/FLOAT(K)
      C1=COS(S1)
      S1=SIN(S1)
C
C     Check for Insufficient Array Storage
C
      IF( JF .GT. MAXF ) THEN
         ISN = 0
         RETURN
      END IF
C
      CK(JF)=1.0
      SK(JF)=0.0
      J=1
  630 CK(J)=CK(K)*C1+SK(K)*S1
      SK(J)=CK(K)*S1-SK(K)*C1
      K=K-1
      CK(K)=CK(J)
      SK(K)=-SK(J)
      J = J + 1
      IF(J.LT.K) GOTO 630
  640 K1=KK
      K2=KK+KSPNN
      AA=A(KK)
      BB=B(KK)
      AK=AA
      BK=BB
      J=1
      K1=K1+KSPAN
  650 K2=K2-KSPAN
      J=J+1
      AT(J)=A(K1)+A(K2)
      AK=AT(J)+AK
      BT(J)=B(K1)+B(K2)
      BK=BT(J)+BK
      J=J+1
      AT(J)=A(K1)-A(K2)
      BT(J)=B(K1)-B(K2)
      K1=K1+KSPAN
      IF(K1.LT.K2) GOTO 650
      A(KK)=AK
      B(KK)=BK
      K1=KK
      K2=KK+KSPNN
      J=1
  660 K1=K1+KSPAN
      K2=K2-KSPAN
      JJ=J
      AK=AA
      BK=BB
      AJ=0.0
      BJ=0.0
      K=1
  670 K = K+1
      AK=AT(K)*CK(JJ)+AK
      BK=BT(K)*CK(JJ)+BK
      K=K+1
      AJ=AT(K)*SK(JJ)+AJ
      BJ=BT(K)*SK(JJ)+BJ
      JJ=JJ+J
      IF(JJ.GT.JF) JJ=JJ-JF
      IF(K.LT.JF) GOTO 670
      K=JF-J
      A(K1)=AK-BJ
      B(K1)=BK+AJ
      A(K2)=AK+BJ
      B(K2)=BK-AJ
      J=J+1
      IF(J.LT. K) GOTO 660
      KK=KK+KSPNN
      IF(KK.LE.NN) GOTO 640
      KK=KK-NN
      IF(KK.LE.KSPAN) GOTO 640
C
C     Multiply by rotation factor (except for factors of 2 and 4)
C
  700 IF(I.EQ.M) GOTO 800
      KK=JC+1
  710 C2=1.0-CD
      S1=SD
  720 C1=C2
      S2=S1
      KK=KK+KSPAN
  730 AK=A(KK)
      A(KK)=C2*AK-S2*B(KK)
      B(KK)=S2*AK+C2*B(KK)
      KK=KK+KSPNN
      IF(KK.LE.NT) GOTO 730
      AK=S1*S2
      S2=S1*C2+C1*S2
      C2=C1*C2-AK
      KK=KK-NT+KSPAN
      IF(KK.LE.KSPNN) GOTO 730
      C2=C1-(CD*C1+SD*S1)
      S1=S1+(SD*C1-CD*S1)
C
C    the following three statements compensate for truncation
C    error. If rounded arithmetic is used, they may
C    be deleted.
C
C      C1=0.5/(C2**2+S1**2)+0.5
C      S1=C1*S1
C      C2=C1*C2
      KK=KK-KSPNN+JC
      IF(KK.LE.KSPAN) GOTO 720
      KK=KK-KSPAN+JC+INC
      IF(KK.LE.JC+JC) GOTO 710
      GOTO 100
C
C     Permute the results to normal order---done in two stages
C     permutation for square factors of N
C
  800 NP(1)=KS
      IF (KT.EQ. 0) GOTO 890
      K=KT+KT+1
      IF(M.LT.K) K=K-1
      J=1
      NP(K+1)=JC
  810 NP(J+1)=NP(J)/NFAC(J)
      NP(K)=NP(K+1)*NFAC(J)
      J=J+1
      K=K-1
      IF(J.LT.K) GOTO 810
      K3=NP(K+1)
      KSPAN=NP(2)
      KK=JC+1
      K2=KSPAN+1
      J=1
      IF(N.NE.NTOT) GOTO 850
C
C     Permutation for single-variate transform (optional code)
C
  820 AK=A(KK)
      A(KK)=A(K2)
      A(K2)=AK
      BK=B(KK)
      B(KK)=B(K2)
      B(K2)=BK
      KK=KK+INC
      K2=KSPAN+K2
      IF(K2.LT.KS) GOTO 820
  830 K2=K2-NP(J)
      J=J+1
      K2=NP(J+1)+K2
      IF(K2.GT.NP(J)) GOTO 830
      J=1
  840 IF(KK.LT.K2) GOTO 820
      KK=KK+INC
      K2=KSPAN+K2
      IF(K2.LT.KS ) GOTO 840
      IF(KK .LT. KS) GOTO 830
      JC = K3
      GOTO 890
C
C     Permutation for multi-variate transform
C
  850 K=KK+JC
  860 AK=A(KK)
      A(KK)=A(K2)
      A(K2)=AK
      BK=B(KK)
      B(KK)=B(K2)
      B(K2)=BK
      KK=KK+INC
      K2=K2+INC
      IF(KK.LT.K) GOTO 860
      KK=KK+KS-JC
      K2=K2+KS-JC
      IF(KK.LT.NT) GOTO 850
      K2=K2-NT+KSPAN
      KK=KK-NT+JC
      IF(K2.LT.KS) GOTO 850
  870 K2=K2-NP(J)
      J=J+1
      K2=NP(J+1)+K2
      IF(K2.GT.NP(J)) GOTO 870
      J=1
  880 IF(KK.LT.K2) GOTO 850
      KK=KK+JC
      K2=KSPAN+K2
      IF(K2.LT.KS) GOTO 880
      IF(KK.LT.KS) GOTO 870
      JC=K3
  890 IF(2*KT+1 .GE. M) RETURN
      KSPNN=NP(KT+1)
C
C     Permutation for square-free factors of N
C
      J=M-KT
      NFAC(J+1)=1
  900 NFAC(J)=NFAC(J)*NFAC(J+1)
      J=J-1
      IF(J.NE.KT) GOTO 900
      KT=KT+1
      NN=NFAC(KT)-1
C
C     Check for Insufficient Array Storage
C
      IF( NN .GT. MAXP ) THEN
         ISN = 0
         RETURN
      END IF
C
      CK(JF)=1.0
      JJ=0
      J=0
      GOTO 906
  902 JJ=JJ-K2
      K2=KK
      K=K+1
      KK=NFAC(K)
  904 JJ=KK+JJ
      IF(JJ.GE.K2) GOTO 902
      NP(J) = JJ
  906 K2=NFAC(KT)
      K=KT+1
      KK=NFAC(K)
      J=J+1
      IF(J.LE.NN) GOTO 904
C
C     Determine the permutation cycles of length greater than 1
C
      J=0
      GOTO 914
  910 K=KK
      KK=NP(K)
      NP(K)=-KK
      IF(KK.NE.J) GOTO 910
      K3=KK
  914 J=J+1
      KK=NP(J)
      IF(KK.LT.0) GOTO 914
      IF(KK.NE.J) GOTO 910
      NP(J)=-J
      IF(J.NE.NN) GOTO 914
      MAXF=INC*MAXF
C
C     Reorder A and B, following the permutation cycles
C
      GO TO 950
  924 J=J-1
      IF(NP(J) .LT. 0) GO TO 924
      JJ=JC
  926 KSPAN=JJ
      IF(JJ .GT. MAXF) KSPAN=MAXF
      JJ=JJ-KSPAN
      K=NP(J)
      KK=JC*K+II+JJ
      K1=KK+KSPAN
      K2=0
  928 K2=K2+1
      AT(K2)=A(K1)
      BT(K2)=B(K1)
      K1=K1-INC
      IF(K1 .NE. KK) GO TO 928
  932 K1=KK+KSPAN
      K2=K1-JC*(K+NP(K))
      K=-NP(K)
  936 A(K1)=A(K2)
      B(K1)=B(K2)
      K1=K1-INC
      K2=K2-INC
      IF(K1 .NE. KK) GO TO 936
      KK=K2
      IF(K .NE. J) GO TO 932
      K1=KK+KSPAN
      K2=0
  940 K2=K2+1
      A(K1)=AT(K2)
      B(K1)=BT(K2)
      K1=K1-INC
      IF(K1 .NE. KK) GO TO 940
      IF(JJ .NE. 0) GO TO 926
      IF(J .NE. 1) GO TO 924
  950 J=K3+1
      NT=NT-KSPNN
      II=NT-INC+1
      IF(NT .GE. 0) GO TO 924
      RETURN
      END
