      LOGICAL FUNCTION SAMESET( KS, JS )
C
C     Compare setup group KS with group JS to determine if they 
C     are the same.
C
C     This will be used, for example, to help prevent a lot of 
C     repetition in the summary file when different stations have
C     the same setup (like VLBA stations).
C
C     In the comparison, don't compare:
C       Station name
C       Frequency catalog info
C
C     Provide several spots to skip tests if SA becomes false - just
C     jump to the end.
C 
      INTEGER   KS, JS, I, ICH, IIF, ITP, IPC
      INTEGER   LEN1
      LOGICAL   SA, LOCDBG
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      PARAMETER  (LOCDBG=.FALSE.)
C ---------------------------------------------------------------------
      IF( SDEBUG .OR. LOCDBG ) THEN
         WRITE( MSGTXT, '( A, I4, A, I4 )' )
     1      'SAMESET: Starting on setups ', KS, ' and ', JS
            CALL WLOG( 0, MSGTXT(1:LEN1(MSGTXT)) )
            MSGTXT = ' '
      END IF
C
C     First gross tests of setup file and number of channels.
C     Having channels equal allows simpler loops later.
C
      SA = .TRUE.
      SA = SA .AND. ISETNUM(KS) .EQ. ISETNUM(JS) .AND.
     1     NCHAN(KS) .EQ. NCHAN(JS)
      IF ( .NOT. SA ) THEN
         IF( LOCDBG ) CALL WLOG( 0, 'SAMESET: setnum or nchan' )
         GO TO 999 
      END IF
C
      DO ICH = 1, NCHAN(KS)
         SA = SA .AND.
     1        SFCHAN(ICH,KS)  .EQ. SFCHAN(ICH,JS) .AND.
     2        SGCHAN(ICH,KS)  .EQ. SGCHAN(ICH,JS) .AND.
     3        FREQREF(ICH,KS) .EQ. FREQREF(ICH,JS) .AND.
     4        FIRSTLO(ICH,KS) .EQ. FIRSTLO(ICH,JS) .AND.
     5        FIFMIN(ICH,KS)  .EQ. FIFMIN(ICH,JS) .AND.
     6        FIFMAX(ICH,KS)  .EQ. FIFMAX(ICH,JS)
      END DO
C
C     Don't worry about the pointing offsets since they don't appear
C     in the outputs dependent on SAMESET.  Leave the commented
C     code here in case it matters some day.
C
C     SA = SA .AND.
C    1     AZCOLIM(KS) .EQ. AZCOLIM(JS) .AND.
C    2     ELCOLIM(KS) .EQ. ELCOLIM(JS) .AND.
C    3     PTINCR(KS)  .EQ. PTINCR(JS)  .AND.
C    4     PTOFF(KS)   .EQ. PTOFF(JS)
C
      DO ICH = 1, NCHAN(KS)
         SA = SA .AND.
     1        BBSYN(ICH,KS)  .EQ. BBSYN(ICH,JS)  .AND.
     2        BBSYN2(ICH,KS) .EQ. BBSYN2(ICH,JS) .AND.
     3        BBFILT(ICH,KS) .EQ. BBFILT(ICH,JS)
      END DO
      IF ( .NOT. SA ) THEN
         IF( LOCDBG ) CALL WLOG( 0, 'SAMESET: before bbfilt' )
         GO TO 999 
      END IF
C
      SA = SA .AND.
     1     SAMPRATE(KS) .EQ. SAMPRATE(JS) .AND.
     2     SYNTH(1,KS)  .EQ. SYNTH(1,JS)  .AND.
     3     SYNTH(2,KS)  .EQ. SYNTH(2,JS)  .AND.
     4     SYNTH(3,KS)  .EQ. SYNTH(3,JS)  .AND.
     5     SPEEDH(KS)   .EQ. SPEEDH(JS)   .AND.
     6     SPEEDL(KS)   .EQ. SPEEDL(JS)   .AND.
     7     FANOUT(KS)   .EQ. FANOUT(JS)
C
      SA = SA .AND.
     1     SPEEDUP(KS)  .EQ. SPEEDUP(JS)  .AND.
     2     BESTOVER(KS) .EQ. BESTOVER(JS) .AND.
     3     TOTBW(KS)    .EQ. TOTBW(JS)
      IF ( .NOT. SA ) THEN
         IF( LOCDBG ) CALL WLOG( 0, 'SAMESET: before totbw' )
         GO TO 999 
      END IF
C
      DO ICH = 1, NCHAN(KS)
         DO ITP = 1, TPMODE(KS)
            SA = SA .AND.
     1          TRACK(ICH,ITP,KS) .EQ. TRACK(ICH,ITP,JS)
         END DO
         SA = SA .AND.
     1     BITS(ICH,KS) .EQ. BITS(ICH,JS) .AND.
     2     BBC(ICH,KS)  .EQ. BBC(ICH,JS)
      END DO
      SA = SA .AND.
     1     PERIOD(KS)   .EQ. PERIOD(JS)   .AND.
     2     LEVEL(KS)    .EQ. LEVEL(JS)    .AND.
     3     TAPEMODE(KS) .EQ. TAPEMODE(JS) .AND.
     4     TPMODE(KS)   .EQ. TPMODE(JS)   .AND.
     5     SWTCHDUR(KS) .EQ. SWTCHDUR(JS)
      IF ( .NOT. SA ) THEN
         IF( LOCDBG ) CALL WLOG( 0, 'SAMESET: before swtchdur' )
         GO TO 999 
      END IF
C
      DO IPC = 1, MAXPC
         SA = SA .AND.
     1        PCALFR1(IPC,KS) .EQ. PCALFR1(IPC,JS) .AND.
     2        PCALFR2(IPC,KS) .EQ. PCALFR2(IPC,JS) .AND.
     3        PCALX1(IPC,KS)  .EQ. PCALX1(IPC,JS)  .AND.
     4        PCALX2(IPC,KS)  .EQ. PCALX2(IPC,JS)
      END DO
      IF ( .NOT. SA ) THEN
         IF( LOCDBG ) CALL WLOG( 0, 'SAMESET: before pcalx2' )
         GO TO 999 
      END IF
C
C     Don't test quite all of these.
C     I thought I had found g77 compiler bug, but I am not used to
C     .EQV. and it turns out the .AND.s get evaluated first, producing
C     very confusing results without parentheses.
C
      SA = SA .AND.
     1     ( DUALPOL(KS) .EQV. DUALPOL(JS) ) .AND.
     2     ( FRSWITCH(KS) .EQV. FRSWITCH(JS) ) .AND.
     3     ( DUALX(KS)    .EQV. DUALX(JS) ) .AND.
     4     ( MODETEST(KS) .EQV. MODETEST(JS) ) .AND.
     5     ( BAND(KS) .EQ. BAND(JS) )
C     5     ( USED(KS)     .EQV. USED(JS) )     .AND. 
      IF ( .NOT. SA ) THEN
         IF( LOCDBG ) CALL WLOG( 1, 'SAMESET: before band' )
         GO TO 999 
      END IF
C
      DO IIF = 1, 4
         SA = SA .AND.
     1        IFDIST(IIF,KS) .EQ. IFDIST(IIF,JS) .AND.
     2        NOISE(IIF,KS)  .EQ. NOISE(IIF,JS)  .AND.
     3        FE(IIF,KS)     .EQ. FE(IIF,JS) 
      END DO
      IF ( .NOT. SA ) THEN
         IF( LOCDBG ) CALL WLOG( 0, 'SAMESET: before fe' )
         GO TO 999 
      END IF
C
      DO ICH = 1, NCHAN(KS)
         SA = SA .AND.
     1        SIDEBD(ICH,KS)  .EQ. SIDEBD(ICH,JS)  .AND.
     2        POL(ICH,KS)     .EQ. POL(ICH,JS)     .AND.
     3        IFCHAN(ICH,KS)  .EQ. IFCHAN(ICH,JS)  .AND.
     4        ALTIFC(ICH,KS)  .EQ. ALTIFC(ICH,JS)  .AND.
     5        NETSIDE(ICH,KS) .EQ. NETSIDE(ICH,JS) .AND.
     6        SIDE1(ICH,KS)   .EQ. SIDE1(ICH,JS) 
      END DO
C
      DO I = 1, 4
         SA = SA .AND.
     1   STRING(4,KS) .EQ. STRING(4,JS)
      END DO
      IF ( .NOT. SA ) THEN
         IF( LOCDBG ) CALL WLOG( 0, 'SAMESET: before string' )
         GO TO 999 
      END IF
C
      SA = SA .AND.
     1  NOISEFRQ(KS) .EQ. NOISEFRQ(JS) .AND.
     2  LOGGING(KS)  .EQ. LOGGING(JS)  .AND.
     3  FORMAT(KS)   .EQ. FORMAT(JS)   .AND.
     4  SPCAL(KS)    .EQ. SPCAL(JS)    .AND.
     5  RCHAN(KS)    .EQ. RCHAN(JS)    .AND.
     6  LCHAN(KS)    .EQ. LCHAN(JS)    .AND.
     7  LCP50CM(KS)  .EQ. LCP50CM(JS)  .AND.
     8  RCP50CM(KS)  .EQ. RCP50CM(JS)  .AND.
     9  BARREL(KS)   .EQ. BARREL(JS) 
      IF ( .NOT. SA .AND. LOCDBG ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, A, A, A )' ) '     BARREL (KS), (JS) ',
     1       BARREL(KS), '   ', BARREL(JS)
         CALL WLOG( 0, MSGTXT )
         CALL WLOG( 0, 'SAMESET: before BARREL' )
         GO TO 999 
      END IF
C
      SA = SA .AND.
     1  MINTRAK(KS)  .EQ. MINTRAK(JS)  .AND.
     2  MAXTRAK(KS)  .EQ. MAXTRAK(JS)  .AND.
     3  MINTBPS(KS)  .EQ. MINTBPS(JS)  .AND.
     4  MAXTBPS(KS)  .EQ. MAXTBPS(JS)  .AND.
     5  TBPS(KS)     .EQ. TBPS(JS)     .AND.
     6  TOTBPS(KS)   .EQ. TOTBPS(JS)   .AND.
     7  ( VLBAMKIV(KS) .EQV. VLBAMKIV(JS) )
      IF ( .NOT. SA .AND. LOCDBG ) THEN
         CALL WLOG( 0, 'SAMESET: before vlbamkiv' )
         GO TO 999 
      END IF
C
C     Only check the VLA parameters if both antennas are VLA.
C     Also don't allow them to pretend to be the same if one
C     is a VLA antenna and the other is not (will need VLA
C     parameters in the VLA listing).
C
      SA = SA .AND. 
     1     ( SETSTA(1,KS)(1:3) .EQ. 'VLA' .AND. 
     2       SETSTA(1,JS)(1:3) .EQ. 'VLA' ) .OR.
     3     ( SETSTA(1,KS)(1:3) .NE. 'VLA' .AND. 
     4       SETSTA(1,JS)(1:3) .NE. 'VLA' )
C
      IF( SETSTA(1,KS)(1:3) .EQ. 'VLA' .AND. 
     1    SETSTA(1,JS)(1:3) .EQ. 'VLA' ) THEN
C
         SA = SA .AND.
     1        FLUKESET(KS) .EQ. FLUKESET(JS) .AND.
     2        FLUKEA(KS)   .EQ. FLUKEA(JS)   .AND.
     3        FLUKEB(KS)   .EQ. FLUKEB(JS)   .AND.
     4        VLAFEAB(KS)  .EQ. VLAFEAB(JS)  .AND.
     5        VLAFECD(KS)  .EQ. VLAFECD(JS)  .AND.
     6        VLASYNA(KS)  .EQ. VLASYNA(JS)  .AND.
     7        VLASYNB(KS)  .EQ. VLASYNB(JS)  .AND.
     8        FEFILTER(KS) .EQ. FEFILTER(JS) .AND.
     9        VLAIF(KS)    .EQ. VLAIF(JS) 
C
         SA = SA .AND.
     1        VLAROT(KS)  .EQ. VLAROT(JS)  .AND.
     2        VLABAND(KS) .EQ. VLABAND(JS) .AND.
     3        VLABW(KS)   .EQ. VLABW(JS)   .AND.
     4        ( VLALOFI(KS) .EQV. VLALOFI(JS) ) .AND.
     5        ( VLAVA(KS)   .EQV. VLAVA(JS) )  .AND.
     6        ( VLAVB(KS)   .EQV. VLAVB(JS) )   .AND.
     7        ( VLAVR(KS)   .EQV. VLAVR(JS) )   .AND.
     8        ( VLAVL(KS)   .EQV. VLAVL(JS) ) 
      END IF
      IF ( .NOT. SA ) THEN
         IF( LOCDBG ) CALL WLOG( 0, 'SAMESET: before vlavl' )
         GO TO 999 
      END IF

	   
C     Don't forget to deal with frequency sets and pcal sets
C     separately.  They are not compared here.  Think about what
C     to do.

  999 CONTINUE
      SAMESET = SA
      RETURN
      END

