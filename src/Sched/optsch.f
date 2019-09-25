
      SUBROUTINE OPTSCH( ISCN )
Cf2py intent(in) ISCN
C
C     Routine for optimization mode that writes the keyin input for 
C     the optimized schedule.
C
C     ISCN is the sched scan number.
C     LINE is a line-on-page counter.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      DOUBLE PRECISION  START, STOP
      INTEGER     ISCN, ISTA, LEN1, I, ISCH, LSCH, ISTN, IDUR
      INTEGER     IOERR, VLBOPE, YEAR, DAY1, DAY2, KSCH
      INTEGER     LSET, PTCHAR, LASTDAY, LASTDUR
      INTEGER     I1, I2, IEL, MISCH, MISTN
      DOUBLE PRECISION  LASTBW(MAXCHN)
      CHARACTER   LINESC*256, SCHFILE*80, LINEPT*132, LASTLINE*256
      CHARACTER   TFORM*8, OPTEXT*256, OPSTAT*4, USENAME*8, CEL*3
      LOGICAL     NEWBW, FIRSTPT, EXISTS, NEEDPAR
      LOGICAL     LDOPCAL, LNOREC
      DOUBLE PRECISION  LSTDAY, PTLONG, GAST, PTLST, SLA_GMST
      SAVE        FIRSTPT, LASTLINE, LSCH, LSET, LASTDUR
      SAVE        LDOPCAL, LNOREC, LASTDAY, LASTBW, MISCH
      DATA        FIRSTPT / .TRUE. /
C --------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'OPTSCH starting' )
C
C     Some initializations.
C
      IF( FIRSTPT ) THEN
         LASTLINE = ' '
         LSCH = 0
         LSET = 0
         LASTDUR = -1.0
         LDOPCAL = .FALSE.
         LNOREC  = .FALSE.
         LASTDAY = -1
         DO I = 1, MAXCHN
            LASTBW(MAXCHN) = 0.0D0
         END DO
         MISCH = 0
C
C        Open output SCH file - contains the new schedule.
C
         SCHFILE = EXPCODE(1:LEN1(EXPCODE))//'.sch'
         CALL DWCASE( SCHFILE )
         INQUIRE( FILE=SCHFILE, EXIST=EXISTS )
         IF( EXISTS .AND. OVERWRIT ) THEN
            OPSTAT = 'OLD'
         ELSE IF( EXISTS ) THEN
            CALL WLOG( 1, 'OPTSCH: '//SCHFILE//' already exists.' )
            CALL ERRLOG( 'OPTSCH: You need to delete old output files'
     1                // ' or set OVERWRIT.' )
         ELSE
            OPSTAT = 'NEW'
         END IF
         IOERR = VLBOPE( IUOPT, SCHFILE, 'TEXT', OPSTAT, OPTEXT )
         IF( IOERR .NE. 1 ) CALL ERRLOG( OPTEXT )
C
C        If the original schedule was LST, make the output LST.
C
         IF( LST ) THEN
C
C           Write introductory lines
C
            WRITE( IUOPT, '( A )' ) '!  LST schedule '
            WRITE( IUOPT, '( A, A )' ) ' LST = ', STATION(LSTSTA)
         ELSE
            WRITE( IUOPT, '( A )' ) '!  PTLST given for start times.'//
     1          '  Those are Pie Town LST.'
            WRITE( IUOPT, '( A )' ) '!  PTLST is not understood by '//
     1          'SCHED so it will need to be edited.'
         END IF
C
      END IF
      LINEPT = ' '
      LINESC = ' '
C
C     Get the time information for the scan.
C     Worry about roundoff for IDUR.
C
C     If the input schedule was in LST, make the output in LST too.
C     Later, the variables we want are DAY2 and START.
C
      IF( LST ) THEN
         CALL SIDTIM( STARTJ(ISCN), LONG(STANUM(LSTSTA)), TWOPI, 
     1        DAY1, START, LSTDAY )
         CALL SIDTIM( STOPJ(ISCN), LONG(STANUM(LSTSTA)), TWOPI, 
     1        DAY2, STOP, LSTDAY )
      ELSE
         CALL TIMEJ( STARTJ(ISCN), YEAR, DAY1, START )
         CALL TIMEJ( STOPJ(ISCN), YEAR, DAY2, STOP )
      END IF
      IDUR = ( STOPJ(ISCN) - STARTJ(ISCN) ) * 3600.D0 * 24.D0 + 0.05D0
C
C     Write the new schedule output lines.
C
      LINEPT = ' '
      PTCHAR = 0
C
C     First deal with setup files.
C
      IF( FIRSTPT .OR. LSET .NE. SETNUM(ISCN) ) THEN
         LSET = SETNUM(ISCN)
         PTCHAR = LEN1( SETNAME(LSET) )
         LINEPT(1:PTCHAR+9) = ' setup='''//SETNAME(LSET)(1:PTCHAR)//''''
         PTCHAR = PTCHAR + 9
      END IF
C
C     Doppler calculation request.
C
      IF( DOPCAL(ISCN) .AND. .NOT. LDOPCAL ) THEN
         LINEPT(PTCHAR+4:PTCHAR+10) = 'dopcal'
         PTCHAR = PTCHAR + 10
      ELSE IF( LDOPCAL .AND. .NOT. DOPCAL(ISCN) ) THEN
         LINEPT(PTCHAR+4:PTCHAR+13) = 'dopcal=-1'
         PTCHAR = PTCHAR + 13
      END IF
      LDOPCAL = DOPCAL(ISCN)
C
C     record/norecord.
C
      IF( NOREC(ISCN) .AND. .NOT. LNOREC ) THEN
         LINEPT(PTCHAR+4:PTCHAR+12) = 'record=-1'
         PTCHAR = PTCHAR + 12
      ELSE IF( LNOREC .AND. .NOT. NOREC(ISCN) ) THEN
         LINEPT(PTCHAR+4:PTCHAR+9) = 'record'
         PTCHAR = PTCHAR + 9
      END IF
      LNOREC = NOREC(ISCN)
C
C     Bandwidth.
C
      NEWBW = .FALSE.
      DO I = 1, MSCHN(SETNUM(ISCN))
         IF( LASTBW(I) .NE. BW(I,ISCN) ) NEWBW = .TRUE.
         LASTBW(I) = BW(I,ISCN)
      END DO
      IF( NEWBW ) THEN
         IF( MSCHN(SETNUM(ISCN)) .GT. 4 ) THEN
             WRITE( IUOPT, '(A)' ) LINEPT(1:PTCHAR)
             PTCHAR = 0
         END IF
         WRITE( LINEPT(PTCHAR+1:132),
     1       '( A, 16( F7.3, :, '','' ), / , 16( F7.3, :, '','' ) )' ) 
     2       '  bw=', (BW(I,ISCN),I=1,MSCHN(SETNUM(ISCN)))
         PTCHAR = PTCHAR + 5 + MSCHN(SETNUM(ISCN)) * 8 - 1
      END IF
C
      IF( IDUR .NE. LASTDUR ) THEN
         WRITE( LINEPT(PTCHAR+4:PTCHAR+12), '( A, I4 )' ) ' DUR=', IDUR
         LASTDUR = IDUR
         PTCHAR = PTCHAR + 12
      END IF
C
C     Adjust day number if needed.
C
      IF( DAY2 .NE. LASTDAY ) THEN
         WRITE( LINEPT(PTCHAR+4:PTCHAR+13), '( A, I5 )' ) ' DAY=', DAY2
         LASTDAY = DAY2
         PTCHAR = PTCHAR + 14
      END IF
C
C     Write out the line.
C
      IF( PTCHAR .NE. 0 ) THEN
         WRITE( IUOPT, '(A)' ) LINEPT(1:PTCHAR)
      END IF
C
C
C
C     Write out scan information line:
C
C     The source name.
C
      WRITE( LINESC(1:22), '(3A)' ) ' SOURCE=''',
     1       SCNSRC(ISCN)(1:LEN1(SCNSRC(ISCN))),''''
C
C     Write out scan time.  Based on TIMEJ calls earlier.
C     Use the LST at Pie Town for the time.  But don't assume
C     that Pie town is present.
C
      PTLONG = 108.11919D0 * RADDEG
      GAST = SLA_GMST( STARTJ(ISCN) )
      PTLST = DMOD( GAST - PTLONG, TWOPI )
      IF( PTLST .LT. 0.0D0 ) PTLST = PTLST + TWOPI
C
C
C
C      LINESC(25:38) = 'START=' // TFORM( START, 'T', 0, 2, 2, '::@' )
      LINESC(25:38) = 'PTLST=' // TFORM( PTLST, 'T', 0, 2, 2, '::@' )
C
C     Set up stations list.
C     Use codes for VLBA.
C     Also keep track of length if all stations are present.
C     The initial value for MISTN is for the 'STATIONS='
C
      ISCH = 42
      KSCH = ISCH
      MISTN = 9 + ISCH
      NEEDPAR = .TRUE.
      DO ISTA = 1, NSTA
         IF( STATION(STANUM(ISTA))(1:4) .EQ. 'VLBA' ) THEN
            USENAME = STCODE(STANUM(ISTA))
         ELSE
            USENAME = STATION(STANUM(ISTA))
         END IF
         ISTN = LEN1( USENAME )
         MISTN = MISTN + ISTN + 1
         IF( STASCN(ISCN,ISTA) ) THEN
            IF( NEEDPAR ) THEN
               WRITE( LINESC(ISCH:ISCH+8), '(A)' ) 'STATIONS='
               ISCH = ISCH + 9
               NEEDPAR = .FALSE.
            END IF
            WRITE( LINESC(ISCH:ISCH+ISTN+1), '(2A)' )
     1          USENAME(1:ISTN),','
            ISCH = ISCH + ISTN + 1
         END IF
      END DO
C
C     Write the stations line.  
C     Write one every line in case we break into the middle.
C     Old code to limit the writes is commented.
C
C      IF( LSCH .NE. ISCH .OR. 
C     1    LASTLINE(66:ISCH-2) .NE. LINESC(66:ISCH-2) ) THEN
C         WRITE( LINESC(57:65), '(A)' ) 'STATIONS='
C         LSCH = ISCH
C      ELSE
C         ISCH = 57
C      END IF
C
C     Add slash on the end.  And save the line to this point.
C
      WRITE( LINESC(ISCH-1:ISCH), '(A)' ) ' /'
      LASTLINE = LINESC
      ISCH = ISCH + 2
      MISCH = MAX( ISCH, MISCH )
      MISTN = MISTN + 2
      IF( MISCH .GT. MISTN ) THEN
         CALL WLOG( 1, 'OPTSCH:  Programming error - MISTN wrong ' )
      END IF
C
C     Add a list of elevations to the end of the line.
C
      I1 = MISTN
      DO ISTA = 1, NSTA
         I1 = I1 + 3
         I2 = I1 + 2
         IEL = ( EL1(ISCN, ISTA) + EL2(ISCN,ISTA) ) / 2.0
         WRITE( CEL, '(I3)' ) IEL         
         IF( STASCN(ISCN,ISTA) ) THEN
            LINESC(I1:I2) = CEL
         ELSE
            LINESC(I1:I2) = ' **'
         END IF
         ISCH = I2
      END DO
C     Actually write out the line with a slash on the end.
C
      WRITE( IUOPT, '(A)' ) LINESC(1:ISCH)
C
      FIRSTPT = .FALSE.
C
      RETURN
      END






