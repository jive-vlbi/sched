      SUBROUTINE LSQFIT ( IDEBUG,print_solution,
     +                    PRINT_COR,MAXDAT,NUMXES,
     +                    N,VNAME,X,ID,S,C,E,XHAT2,ERROR,IERR )

C --------------------------------------------------------------------
C     Extracted from Mark Reid's make_geodetic_blocks_ut_8.f that, in
C     my directory, had a date of March 11, 2010 but is probably 
C     considerably older.  I'm not sure of the original source of 
C     the code.  It doesn't match my ancient Numerical Recipes code.
C     I have retyped a some of the code to follow normal SCHED coding
C     practice, have corrected some comments and fixed and have altered
C     some code that I think followed bad practice (mainly getting 
C     rid of the implicit declaration and following the SCHED style
C     of requiring everything be declared - real good at catching typos). 
C     I removed the tabs - I HATE TABS!  You can't see them and they
C     have consequences.  To my knowledge, I have not changed the
C     actual actions of the code.
C     In Mark's code, it was called "LEAST_SQUARES_FIT"
C     Added IERR and more info about failures.
C     RCW  July 2, 2010.
C --------------------------------------------------------------------

C     SUBROUTINE FOR LINEAR LEAST SQUARE FITTING...
C     IT REQUIRES INPUT OF THE FOLLOWING:
C        IDEBUG    = DEBUGGING PARAMETER
C                    0 = SUPRESS PRINT OUT BEYOND SOLUTION AND CORRELATI
C                    3 = PRINT OUT ALL DATA AND MATRICES
C        print_solution = logical flag: print out least-squares solution
C       PRINT_COR = LOGICAL FLAG: PRINT CORRELATION MATRIX
C       MAXDAT    = MAX NUM DATA POINTS.  DIMENSION LIMITS IN MAIN PROG.
C        NUMXES    = NUMBER OF PARAMETERS IN MODEL (WHETHER SOLVED FOR O
C                    Maximum is 40 (guess)
C        N         = NUMBER OF EQUATIONS
C        VNAME     = ALPHANUMERIC 'NAME' FOR THE PARAMETERS
C        X         = 1-D ARRAY OF INITIAL PARAMETER VALUES
C        ID        = 1-D ARRAY OF 'SOLVE FOR' CODES
C                  0 = HOLD PARAMETER CONSTANT
C                  1 = SOLVE FOR PARAMETER
C        S         = 1-D ARRAY OF DATA (E.G. VLB DIFFERENCED PHASES)
C        C         = 2-D ARRAY OF PARTIAL DERIVATIVES OF MODEL
C                    First dimension is maximum number of data points.
C                    That has a maximum of 20000
C        E         = 1-D ARRAY OF ERRORS ON DATA POINTS
C        IERR      = 0 - Don't print some of error messages.  1 - Do
C
C     THE OUTPUT INCLUDES:
C        XHAT2     = 1-D ARRAY OF FINAL LEAST SQUARE VALUES FOR ALL 
C                    PARAMETERS
C        ERROR     = 1-D ARRAY Errors on the parameters.
C        IERR      = Return code.  0=> ok.  1=> inversion failed.

C        THE SOLUTION AND CORRELATIONS ARE AUTOMATICALLY PRINTED OUT

C      IMPLICIT REAL*8 (A-H,O-Z)   

      INTEGER           IDEBUG, MAXDAT, N, NUMXES, IERR
      DOUBLE PRECISION  C(MAXDAT,*)
      DOUBLE PRECISION  S(*),E(*),X(*),XHAT2(*), ERROR(*)
      INTEGER           ID(*)
      character*8       VNAME(*),QUEST(2)
C
      INTEGER           I, J, L, M, IN, JNEW, ITEST2, INOTE
      INTEGER           PRTERR
      DOUBLE PRECISION  B(40,40),BSTORE(40,40),XIDENT(40,40),
     +                  COR(40,40),STDEV(40),PR(40),
     +                  XHAT(40),SNEW(20000),PRODCT(20000)
      DOUBLE PRECISION  BMIN10, BMAX10, B1, BADJST, DETERM
C
      INTEGER           LL(40),MM(40), MSIZE
      DATA              MSIZE /40/

C
      LOGICAL PRINT_COR, print_solution

      DATA QUEST/'YES     ','NO      '/

C ---------------------------------------------------------------------
      PRTERR = IERR
      IERR = 0
 9999 FORMAT (1X,10(1PD13.5))
 9998 FORMAT (1X,20F6.2)
 9417 FORMAT (/1X)
C
      IF (IDEBUG.ge.3) then

         WRITE (6,5522)
 5522    FORMAT (////50X,' C MATRIX')
         do I=1,N
            WRITE (6,9999) (C(I,J),J=1,NUMXES)
         enddo
         WRITE (6,5523)
 5523    FORMAT (////40X,'        S(I)           E(I)')
         do I=1,N
            WRITE (6,9999) S(I),E(I)
         enddo

      endif

C     Count number of solve-for parameters (M)...
      M=0
      DO I=1,NUMXES
         M=M+ID(I)
      enddo

C     Check number of data .ge. number of parameters...
      IF ( N.lt.M )  then
         WRITE (6,2094)  N,M
 2094    FORMAT (////' LEAST_SQUARES_FIT: # DATA POINTS (',I4,
     +                  ') < # PARAMETERS (',I2,').  STOP' )
         STOP
      endif

c     Compress partial derivative matrix to retain only those
c     associated with solve-for parameters...
      JNEW=0
      do J=1,NUMXES
         if ( ID(J) .ne. 0 ) then
            JNEW=JNEW+1
            do I=1,N
               C(I,JNEW)=C(I,J)
            enddo
         endif
      enddo

C     Weight equations by dividing each by their error: E(I)
      do I=1,N
         SNEW(I)=S(I)/E(I)
         do J=1,M
            C(I,J)=C(I,J)/E(I)
         enddo
      enddo

C     Debug printout...
      IF (IDEBUG.ge.3) then
         WRITE (6,2006)
 2006    FORMAT ('1'/////50X,' CSTAR MATRIX')
         do I=1,N
            WRITE (6,9999) (C(I,J),J=1,M)
         enddo
         WRITE (6,2009)
 2009    FORMAT (/////50X,'     SSTAR')
         do I=1,N
            WRITE (6,9999) SNEW(I)
         enddo
      endif

C     Square up partial deravitive matrix...
      ITEST2=0
      BMIN10=0.D0
      BMAX10=0.D0
      DO I=1,M
         DO J=1,M
            B(I,J)=0.D0
         enddo
      enddo

      do I=1,M
         do J=1,M
            do L=1,N
               B(I,J)=B(I,J) + C(L,I)*C(L,J)
            enddo
c           Test for big range of exponents...
            if (B(I,J).ne.0.D0) then
               B1=DLOG10( DABS( B(I,J) ) )
               IF (BMIN10.GT.B1) BMIN10=B1
               IF (BMAX10.LT.B1) BMAX10=B1
            endif
            BSTORE(I,J)=B(I,J)
         enddo
      enddo

c     More debug printout...
      if (IDEBUG.ge.3) then
         WRITE (6,2010)
 2010    FORMAT ('1'/////50X,' C*TRANSPOSE C')
         do I=1,M
            WRITE (6,9999) (B(I,J),J=1,M)
         enddo
      endif


c     Warn user if the matrix has a large range of exponents...
      IF (DABS(BMAX10-BMIN10).ge.16.D0) then
         WRITE (6,9622) BMIN10,BMAX10
 9622    FORMAT (///1X,'********   BMIN10 = ',F6.1,
     +          'BMAX10 = ',F6.1,'   *************',///1X)
      endif

c     "Center" exponents about zero...
      BADJST=10.D0**( (BMAX10+BMIN10)/2.D0 )
      do I=1,M
         do J=1,M
            B(I,J)=B(I,J)/BADJST
         enddo
      enddo

c     About to invert the matrix...
C        THE SUBROUTINE 'MINV' IS A DOUBLE PRECISION MATRIX INVERSION
C        IT INVERTS THE MATRIX 'BB' AND STORES IT AS 'BB' (I.E. THE ORIGIN
C        MATIRX IS DESTROYED

      CALL MINV(B,M,DETERM,LL,MM,MSIZE)

c     Re-scale inverted matrix for "Centering"
      do I=1,M
         do J=1,M
            B(I,J)=B(I,J)/BADJST
         enddo
      enddo

c     More debug printout...
      IF (IDEBUG.ge.3) then
         WRITE (6,2011) DETERM
 2011       FORMAT ('1'////'  THE DETERMINANT IS',1PD13.5)
         WRITE (6,2022)
 2022       FORMAT (////45X,' (C*TRANSPOSE C*) INVERSE')
         do I=1,M
            WRITE (6,9999) (B(I,J),J=1,M)
         enddo
      endif

c     Prepare to check matrix inversion by multiplying inverse matrix
c     by the original martix to obtain (hopefully) the indentity matrix...
      do I=1,M
         do J=1,M
            XIDENT(I,J)=0.D0
         enddo
      enddo

      do I=1,M
         do J=1,M
            do L=1,M
               XIDENT(I,J)=XIDENT(I,J) + B(I,L)*BSTORE(L,J)
            enddo
            if (I.ne.J) then
c              Off-diagonal elements should be near zero...
               IF (DABS(XIDENT(I,J)).GT.1.D-06) ITEST2=1
            else
c              Diagonal elements should be near unity...
               IF (DABS(XIDENT(I,I)-1.D0).GT.1.D-06) ITEST2=-1
            endif
         enddo
      enddo

      if (ITEST2.eq.0) then

c        Matrix seemed to invert.
c        Calculate corrections (XHAT's) to parameters...
         do I=1,M
            XHAT(I)=0.D0
            do J=1,N
               PRODCT(J)=0.D0
               do L=1,M
                  PRODCT(J)=PRODCT(J) + B(I,L)*C(J,L)
               enddo
               XHAT(I)=XHAT(I)+PRODCT(J)*SNEW(J)
            enddo
         enddo

C        XHAT'S ARE (IF ADDED TO THE X'S) THE UNCONSTRAINED LEAST SQUARE
C        SOLUTION OF THE 'SOLVE FOR' PARAMETERS ONLY.
C        XHAT2(J) = THE LEAST SQUARES SOLTIONS (INCLUDING NON SOLVE FOR
C        PARAMETERS)
         IN=0
         do J=1,NUMXES
            if (ID(J).ne.0) then
               IN=IN+1
               XHAT2(J)=XHAT(IN) + X(J)
            else
               XHAT2(J)=X(J)
            endif
         enddo

c        Calculate correlation coefficients...
         do I=1,M
            do J=1,M
               COR(I,J)=-BSTORE(I,J)/DSQRT(BSTORE(I,I)*BSTORE(J,J))
            enddo
         enddo

c        Check for unphysical negative variances...
         INOTE=0
         do I=1,M
            IF (B(I,I).le.0.D0) then
               B(I,I)=-B(I,I)
               INOTE = INOTE + 1
            endif
         enddo
         if (INOTE.gt.0) then
            WRITE (6,2071) INOTE
 2071         FORMAT (///' ***** THERE ARE ',I2,' NEGATIVE VARIANCES; ',
     +            'SOLUTION IS UNPHYSICAL')
         endif

c        Convert variances to standard deviations...
         do J=1,M
            STDEV(J)=DSQRT(B(J,J))
         enddo


C        REARRANGE CALCULATED 1 SIGMA ERRORS TO APPLY TO THE SOLVED 
C        FOR PARAMETERS
         IN=0
         do J=1,NUMXES
            if (ID(J).ne.0) then
               IN=IN+1
               ERROR(J)=STDEV(IN)
            else
               ERROR(J)=0.D0
            endif
         enddo

         if ( print_solution ) then
C           OUTPUT FOR THE UNCONSTRAINED LEAST SQUARES SOLUTION
            WRITE (6,2040)
 2040       FORMAT (/1X,'PARAMETER    ORIGINAL VALUE   LEAST SQUARE',
     +               ' VALUE  1 SIGMA ERRORS   SOLVED FOR?')
            do J=1,NUMXES
               L=1
               IF (ID(J).EQ.0) L=2

               if ( L.eq.1 ) then
                  WRITE (6,2041) VNAME(J),X(J),XHAT2(J),ERROR(J),
     1                QUEST(L)
 2041               FORMAT (2X,A8,5X,3(F13.6,5X),4X,A8)
               endif

            enddo
         endif

c        Print correlations?
         if ( PRINT_COR )  then
C           CALCULATE THE CORRELATION COEFFICIENTS (COR)... 
            do I=1,M
               do J=1,M
                  COR(I,J)=B(I,J)/DSQRT(B(I,I)*B(J,J))
               enddo
            enddo

c           WRITE (6,2056)
c 2056         FORMAT(/10X,' THE CORRELATION COEFFICIENTS')
c           do I=1,M
c              WRITE (6,9998) (COR(I,J),J=1,M)
c           enddo

C           THE MULTIPLE CORRELATION COEFFICIENTS (PR) ARE CALCULATED
            do I=1,M
               PR(I)= 1.D0 - (1.d0/(BSTORE(I,I)*B(I,I)))
            enddo
c           WRITE (6,2060)
c 2060      FORMAT (///10X,'THE MULTIPLE CORRELATION COEFFICIENTS'/)
            I = 0
            do J=1,NUMXES
               if ( ID(J).ne.0 ) then
                  I = I + 1
C                 Not sure if the true PR is the sqrt(PR)??
c                 WRITE (6,2061) VNAME(J),PR(I)
c 2061               FORMAT (10X,A8,2X,F10.5)
               endif
            enddo

         endif

       else

c         Print out warning... and set error flag.

          IF( ITEST2 .EQ. 1 .AND. PRTERR .GE. 1 ) 
     1      WRITE( 6, '( A, I2, A )' ) 
     2          ' LSQFIT * ITEST2 =', ITEST2,
     3          '  MATRIX INVERSION FAILED. Off-diagonal elements '//
     4          'after multiplication by inverse too large. '
          IF( ITEST2 .EQ. -1 .AND. PRTERR .GE. 1  )
     1      WRITE( 6, '( A, I2, A )' ) 
     2          ' LSQFIT * ITEST2 =', ITEST2,
     3          '  MATRIX INVERSION FAILED. Diagonal elements not '//
     4          'near unity after multiplication by inverse. '
          IERR = 1

      endif

      RETURN
      END


      SUBROUTINE MINV(A,N,D,L,M,MSIZE)

C      IMPLICIT REAL*8 (A-H,O-Z)    ! RCW: See explicit declarations below.
C
C     IBM SCIENTIFIC SUBROUTINE PACAKAGE PAGE 118
C     ******************************************************************
C
C     PURPOSE  INVERT A MATRIX
C
C     DESCRIPTION OF PARAMETERS
C        A  INPUT MATRIX, DESTROYED IN COMPUTATION AND REPLACED BY INVER
C        N  ORDER OF MATRIX A
C        D  RESULTANT DETERMINANT
C        L  WORK VECTOR OF LENGTH N
C        M  WORK VECTOR OF LENGTH N
C     MSIZE ORDER OF TWO DIMENSIONAL MATRIX A IN MAIN PROGRAM
C
C     METHOD
C        THE STANDARD GAUSS-JORDAN METHOD IS USED. THE DETERMINANT IS A
C        CALCULATED. A DETERMINANT OF ZERO INDICATES THAT THE MATRIX IS
C        SINGULAR.
C
C     ******************************************************************
C
C     Explicit declarations for SCHED.  SCHED is compiled with a switch
C     that requires these.
C
      DOUBLE PRECISION A(*), D
      INTEGER          N, L(*),M(*), MSIZE
C
      INTEGER          NI, IJ, NM, K, LL
      INTEGER          IK, NK, KK, KI, JI, J, I, JK, KJ, IZ, JP
      DOUBLE PRECISION BIGA, HOLD
      INTEGER          JQ, JR
C -----------------------------------------------------------------------
C
C     STORE MATRIX A IN VECTOR FORM (COLUMN BY COLUMN)
C     SEE SSP PROGRAM ARRAY  P.98
C
      IF(MSIZE.EQ.N) GO TO 2
      NI=MSIZE-N
      IJ=0
      NM=0
      DO 230 K=1,N
      DO 225 LL=1,N
      IJ=IJ+1
      NM=NM+1
  225 A(IJ)=A(NM)
  230 NM=NM+NI
    2 CONTINUE
C
C     SEARCH FOR LARGEST ELEMENT
C
      D=1.0D0
      NK=-N
      DO 80 K=1,N
      NK=NK+N
      L(K)=K
      M(K)=K
      KK=NK+K
      BIGA=A(KK)
      DO 20 J=K,N
      IZ=N*(J-1)
      DO 20 I=K,N
      IJ=IZ+I
   10 IF(ABS(BIGA)-ABS(A(IJ))) 15,20,20
   15 BIGA=A(IJ)
      L(K)=I
      M(K)=J
   20 CONTINUE
C
C     INTERCHANGE ROWS
C
      J=L(K)
      IF(J-K) 35,35,25
   25 KI=K-N
      DO 30 I=1,N
      KI=KI+N
      HOLD=-A(KI)
      JI=KI-K+J
      A(KI)=A(JI)
   30 A(JI)=HOLD
C
C     INTERCHANGE COLUMNS
C
   35 I=M(K)
      IF(I-K) 45,45,38
   38 JP=N*(I-1)
      DO 40 J=1,N
      JK=NK+J
      JI=JP+J
      HOLD=-A(JK)
      A(JK)=A(JI)
   40 A(JI)=HOLD
C
C     DIVIDE COLUMN BY MINUS PIVOT (VALUE OF PIVOT ELEMENT IS CONTAINED
C     BIGA)
C
   45 IF(BIGA) 48,46,48
   46 D=0.0D0
      RETURN
   48 DO 55 I=1,N
      IF(I-K) 50,55,50
   50 IK=NK+I
      A(IK)=A(IK)/(-BIGA)
   55 CONTINUE
C
C     REDUCE MATRIX
C
      DO 65 I=1,N
      IK=NK+I
      HOLD=A(IK)
      IJ=I-N
      DO 65 J=1,N
      IJ=IJ+N
      IF(I-K) 60,65,60
   60 IF(J-K) 62,65,62
   62 KJ=IJ-I+K
      A(IJ)=HOLD*A(KJ)+A(IJ)
   65 CONTINUE
C
C     DIVIDE ROW BY PIVOT
C
      KJ=K-N
      DO 75 J=1,N
      KJ=KJ+N
      IF(J-K) 70,75,70
   70 A(KJ)=A(KJ)/BIGA
   75 CONTINUE
C
C     PRODUCT OF PIVOTS
C
      D=D*BIGA
C
C     REPLACE PIVOT BY RECIPROCAL
C
      A(KK)=1.0D0/BIGA
   80 CONTINUE
C
C     FINAL ROW AND COLUMN INTERCHANGE
C
      K=N
  100 K=K-1
      IF(K) 150,150,105
  105 I=L(K)
      IF(I-K) 120,120,108
  108 JQ=N*(K-1)
      JR=N*(I-1)
      DO 110 J=1,N
      JK=JQ+J
      HOLD=A(JK)
      JI=JR+J
      A(JK)=-A(JI)
  110 A(JI)=HOLD
  120 J=M(K)
      IF(J-K) 100,100,125
  125 KI=K-N
      DO 130 I=1,N
      KI=KI+N
      HOLD=A(KI)
      JI=KI-K+J
      A(KI)=-A(JI)
  130 A(JI)=HOLD
      GO TO 100
  150 CONTINUE
C
C     PUT MATRIX BACK INTO SQUARE FORM
C
      IF(MSIZE.EQ.N) GO TO 4
      IJ=N*N+1
      NM=N*MSIZE+1
      DO 210 K=1,N
      NM=NM-NI
      DO 210 LL=1,N
      IJ=IJ-1
      NM=NM-1
  210 A(NM)=A(IJ)
    4 CONTINUE
      RETURN
      END
