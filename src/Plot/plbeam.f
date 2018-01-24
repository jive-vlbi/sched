      SUBROUTINE PLBEAM( BEAM, U, V, IUVMAX, UVMAX, PBMWGT,
     1                   PBMCEL, XYINT )
C
C     Routine for sched that regrid the two vectors of U and V,
C     for the stations selected, and give Dirty Beam.
C
C     PLBEAM is an alias of program "INVERT" ( AUG. 23,1982 ) 
C     
C     Basis of "INVERT" is by D.Rogstad, modifications by S.Kent,
C     A.Readhead, P.Wilkinson, C.Fanti and F.Tinarelli
C     Reads data baseline by baseline, grids with a gaussian convolution 
C     function and inverts to give dirty beam.
C     Inverse weighting of points by density of coverage in U,V plane is
C     used.
C
C     The program use the Singleton FFT routine, i.e. multiple of only
C     2,3,4,5.
C
      INCLUDE 'beam.inc'
C
      INTEGER           NPTS(NBASE), IUVMAX, PBMCEL
      INTEGER           KLDIMU, KLDIMV, PBMWGT
      INTEGER           IDIMU, IUU, IUUM
      INTEGER           IDIMV, IVV, IVVM
      INTEGER           KLMAXU, KLMAXV
      INTEGER           KLLOU, KLHIU, KLLOV, KLHIV, I, J, M, L, LL, K
      INTEGER           KK, JJ, II, MSLO, NSLO, NSHI, MSHI
      INTEGER           INDX, IMAX, IU, IV, IUP, IVP, KLO
      INTEGER           KHI, LLO, LHI, IUO, IVO, KO, LO, NN, NPLO
      INTEGER           N, MM, MPLO, NCOL, IRI, IRF, JCL, JCR, JQ, IQ
      LOGICAL           BFLG
      REAL              BEAM(KXSIZE,KYSIZE)
Cf2py intent(out) BEAM
      DOUBLE PRECISION  U(NPBAS,NBASE), V(NPBAS,NBASE), UVMAX 
      DOUBLE PRECISION  XMAX, YMAX, XYINT
Cf2py intent(out) XYINT
      DOUBLE PRECISION  VZ(KXSIZE*2,KYSIZE*2), CONV(60,60)
      DOUBLE PRECISION  W(NPBAS,NBASE)
      DOUBLE PRECISION  RADSEC, PI, OFST, ARG
      DOUBLE PRECISION  CA, SA, UINT, VINT, UUT, VVT
      DOUBLE PRECISION  SIGMAU, SIGMAV, SIGSQU, SIGSQV, TEMP
      DOUBLE PRECISION  TMPA, SUM, FACT, WSUM, UP, VP
      DOUBLE PRECISION  DATA, CDATA, SDATA, AMAX, SCALE, UNIT, VMX
      DOUBLE PRECISION  VAB, FNORMS, FNORM, SSQ, VVZ, CORR
      COMPLEX*16        CSU(KXSIZE*2), CSV(KXSIZE*2), TT(KXSIZE*2)
C -------------------------------------------------------------------
C
      RADSEC = 4.848137E-6
      PI     = 3.1415926536
      BFLG   = .TRUE.
      KLDIMU = KXSIZE
      KLDIMV = KYSIZE
C
C     Set up array parameters
C
      IDIMU = 2 * KLDIMU
      IUU   = IDIMU * 10
      IUUM  = IUU / 2
      IDIMV = 2 * KLDIMV
      IVV   = IDIMV * 10
      IVVM  = IVV / 2
C
C     Fill cosine-sine array
C
      OFST = PI * KLDIMU / 2.0
C
      DO 5 I = 1, IDIMU
         ARG    = PI * ( I - 1 ) - OFST
         CA     = DCOS( ARG )
         SA     = DSIN( ARG )
         CSU(I) = CMPLX( CA, SA )
  5   CONTINUE
C
      OFST = PI * KLDIMV / 2.0
C
      DO 6 I = 1, IDIMV
         ARG    = PI * ( I - 1 ) - OFST
         CA     = DCOS( ARG )
         SA     = DSIN( ARG )
         CSV(I) = CMPLX( CA, SA )
  6   CONTINUE
C
      UVMAX   = UVMAX * 0.0001
      NPTS(1) = IUVMAX
      UNIT    = 3.0
C
C     Now compute x,y and u,v sampling.
C     Factor 1.2 is to avoid truncation in the gaussian convolving
C     function.
C
      XMAX  = KLDIMU / (2.0 * PBMCEL * KLDIMU * UVMAX * 1.2 * RADSEC)
      YMAX  = KLDIMV / (2.0 * PBMCEL * KLDIMV * UVMAX * 1.2 * RADSEC)
      XYINT = MIN( XMAX, YMAX )
      UINT  = 1.0 / ( 2.0 * KLDIMU * XYINT * RADSEC )
      VINT  = 1.0 / ( 2.0 * KLDIMV * XYINT * RADSEC )
C
C     Compute Uniform or Natural Weights
C
      IF( PBMWGT .EQ. 1 ) THEN
         CALL PLWEIG( VZ, U, V, W, NPTS, KLDIMU,
     1                KLDIMV, UINT, VINT )
      ELSE
         DO 4 INDX = 1, NBASE
            IMAX = NPTS(INDX)
            DO 4 I= 1, IMAX
               W(I,INDX) = 1.0
 4       CONTINUE
      END IF
C
      UUT    = UINT / 10.0
      VVT    = VINT / 10.0
      KLMAXU = 1.2 * UVMAX * PBMCEL / UINT + 0.55
      KLMAXV = 1.2 * UVMAX * PBMCEL / VINT + 0.55
      KLLOU  = KLDIMU - KLMAXU + 1
      KLHIU  = KLDIMU + KLMAXU
      KLLOV  = KLDIMV - KLMAXV + 1
      KLHIV  = KLDIMV + KLMAXV
      SIGMAU = UINT
      SIGMAV = VINT
      SIGSQU = 0.6931 * ( UUT / SIGMAU )**2
      SIGSQV = 0.6931 * ( VVT / SIGMAV )**2
C
C     Fill Convolution array and normalize
C
      DO 10 J = 1, 60
         TEMP = SIGSQV * ( J - 30 )**2
         DO 10 I = 1 , 60
            TMPA = TEMP + SIGSQU * ( I - 30 )**2
            CONV(I,J) = EXP( -TMPA )
 10   CONTINUE 
C
      DO 30 L = 1, 10
         LL = L - 1
         DO 30 K = 1, 10
            KK  = K - 1
            SUM = 0.0
            DO 20 J = 1, 6
               JJ = J * 10 - LL
               DO 20 I = 1, 6
                  II  = I * 10 - KK
                  SUM = SUM + CONV(II,JJ)
 20         CONTINUE
            FACT = 1.0 / SUM
            DO 25 J = 1 , 6
               JJ = J * 10 - LL
               DO 25 I = 1, 6
                  II  = I * 10 - KK
                  CONV(II,JJ) = FACT * CONV(II,JJ)
 25         CONTINUE
 30   CONTINUE
C
C     Form Dirty Beam
C
      MSLO = KLDIMU
      NSLO = 1
      NSHI = IDIMV
      MSHI = IDIMU
C
C     Clear Visibility array
C
      DO 40 L = 1, IDIMV
         DO 40 K = 1, IDIMU
            VZ(K,L) = 0.0
 40   CONTINUE
      WSUM = 0.0
C
      DO 110 INDX = 1, NBASE
         IMAX = NPTS(INDX)
C
C        Set V=-V because DHR and MHC use different definitions of V
C
         DO 58 I= 1, IMAX
            V(I,INDX) = -V(I,INDX)
 58      CONTINUE
C
C        ADD TO ARRAY
C
         DO 110 I = 1, IMAX
            UP    = U(I,INDX)
            VP    = V(I,INDX)
            IU    = UP / UUT + 0.5 * UP / ABS( UP )
            IV    = VP / VVT + 0.5 * VP / ABS( VP )
            IU    = IUUM + IU
            IV    = IVVM - IV
            WSUM  = WSUM + W(I,INDX)
            CDATA = W(I,INDX) * 100.0
            SDATA = 0.0
C
            DO 100 J = 1, 4
               GO TO( 60, 65, 70, 75 ), J
C
C                Add Imag Part
C
 60              IF( BFLG ) GO TO 100
                 DATA = SDATA
                 GO TO 80
C
C                Add Real Part
C
 65              IV = IVV - IV
                 DATA = CDATA
                 GO TO 80
C
C                Add -Imag Part (reflected)
C
 70              IU = IUU - IU
                 IF( BFLG ) GO TO 100
                 DATA = -SDATA
                 GO TO 80
C
C                add Real Part (reflected)
C
 75              IV = IVV - IV
                 DATA = CDATA
C
 80            IUP = IU / 10
               IVP = IV / 10
               KLO = MAX0(IUP-1,KLLOU)
               KHI = MIN0(IUP+4,KLHIU)
               LLO = MAX0(IVP-1,KLLOV)
               LHI = MIN0(IVP+4,KLHIV)
               IUO = IU - 10 * IUP
               IVO = IV - 10 * IVP
               KO  = KLO - 1
               LO  = LLO - 1
C
               DO 90 L = LLO, LHI
                  LL = ( L - LO ) * 10 - IVO
                  DO 90 K = KLO, KHI
                     KK = ( K - KO ) * 10 - IUO
                     VZ(K,L) = VZ(K,L) + DATA * CONV(KK,LL)
 90            CONTINUE
 100        CONTINUE
 110  CONTINUE
C
C     Rescale Visibility Array
C
      AMAX  = 2.0E + 4
      SCALE = 10.0**( UNIT - 2.0 ) / ( 2.0 * WSUM )
      VMX   = 0.0
      DO 125 L = KLLOV, KLHIV
         DO 125 K = KLLOU, KLHIU
            VAB = ABS( VZ(K,L) )
            IF( VAB .GT. VMX ) VMX = VAB
 125  CONTINUE
C
      VMX = VMX * SCALE
      FNORMS = MIN( AMAX / VMX,( AMAX / ( VMX *2.0 * KLMAXU ) )**2,
     1            ( AMAX / ( VMX * 2.0 * KLMAXV ) )**2 )
      FNORM  = 1.0 / DSQRT( FNORMS )
      SCALE  = SCALE * FNORMS
C
      DO 130 L = KLLOV, KLHIV
         DO 130 K = KLLOU, KLHIU
            VZ(K,L) = VZ(K,L) * SCALE
 130  CONTINUE
C
      SCALE = FNORMS * 10**UNIT
      MPLO  = MSLO - KLDIMU + 1
      NPLO  = NSLO
C
C     Fourier Transform DV
C
      CALL PLTRAN( VZ, TT, CSU, CSV, IDIMU, IDIMV,
     1             KLLOU, KLHIU, KLLOV, KLHIV, MSLO, MSHI,
     2             NSLO, NSHI, FNORM)
C
C     Correct BEAM for convolution taper 
C
      SIGSQU = ( XYINT * RADSEC * SIGMAU * PI )**2 / 0.6931
      SIGSQV = ( XYINT * RADSEC * SIGMAV * PI )**2 / 0.6931
      NN     = NPLO - 1
      DO 155 N = NSLO, NSHI
         NN  = NN + 1
         MM  = MPLO - 1
         SSQ = SIGSQV * ( KLDIMV - N )**2
         DO 155 M = MSLO, MSHI
            MM      = MM + 1
            VVZ     = VZ(M,N)
            CORR    = EXP( SIGSQU * ( KLDIMU - M )**2 + SSQ )
            VZ(M,N) = VVZ * CORR
 155     CONTINUE
C
C     Fill the Centre of Dirty Beam to Plot
C   
      NCOL = KXSIZE / 2
C
      IRI  = KLDIMU + 1
      IRF  = KLDIMU + NCOL
      JCL  = KLDIMV - NCOL + 1
      JCR  = KLDIMV + NCOL
      JQ   = 0
      DO J = JCL, JCR
         IQ  = NCOL - 1
         JQ  = JQ + 1
         DO I = IRI, IRF
            IQ  = IQ + 1
            BEAM(IQ,JQ) = VZ(I,J)
         END DO
      END DO
C
      IRI  = KLDIMU + 2
      IRF  = KLDIMU + NCOL
      JCL  = KLDIMV - NCOL + 2
      JCR  = KLDIMV + NCOL + 1
      JQ   = NCOL * 2 + 1
      DO J = JCL, JCR
         IQ  = NCOL
         JQ  = JQ - 1
         DO I = IRI, IRF
            IQ  = IQ - 1
            BEAM(IQ,JQ) = VZ(I,J)
         END DO
      END DO
C
      RETURN
      END
