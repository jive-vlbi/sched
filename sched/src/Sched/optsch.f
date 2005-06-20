      SUBROUTINE OPTSCH( ISCN )
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
      INTEGER     IOERR, VLBOPE, YEAR, DAY1, DAY2
      INTEGER     LSET, PTCHAR, LASTDAY, LASTDUR
      REAL        LASTBW(MAXCHN)
      CHARACTER   LINESC*256, SCHFILE*80, LINEPT*132, LASTLINE*256
      CHARACTER   TFORM*8, OPTEXT*256, OPSTAT*4
      LOGICAL     NEWBW, FIRSTPT, EXISTS
      LOGICAL     LDOPCAL, LNOREC
      SAVE        FIRSTPT, LASTLINE, LSCH, LSET, LASTDUR
      SAVE        LDOPCAL, LNOREC, LASTDAY
      DATA        FIRSTPT / .TRUE. /
C --------------------------------------------------------------------
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
      END IF
      LINEPT = ' '
      LINESC = ' '
C
C     Write the next scan into the output schedule file.
C     Worry about roundoff.
C
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
C     Write out the line.
C
      IF( PTCHAR .NE. 0 ) THEN
         WRITE( IUOPT, '(A)' ) LINEPT(1:PTCHAR)
      END IF
C
C     Write out scan info.
C
      WRITE( LINESC(1:22), '(3A)' ) ' SOURCE=''',
     1       SCNSRC(ISCN)(1:LEN1(SCNSRC(ISCN))),''''
C
C     Write out scan times.
C
      CALL TIMEJ( STARTJ(ISCN), YEAR, DAY1, START )
      CALL TIMEJ( STOPJ(ISCN), YEAR, DAY2, STOP )
      LINESC(25:38) = 'START=' // TFORM( START, 'T', 0, 2, 2, '::@' )
      IF( IDUR .NE. LASTDUR ) THEN
         WRITE( LINESC(47:55), '( A, I4 )' ) ' DUR=', IDUR
         LASTDUR = IDUR
      END IF
C
C     Adjust day number if needed.
C
      IF( DAY2 .NE. LASTDAY ) THEN
         WRITE( LINESC(39:46), '( A, I3 )' ) ' DAY=', DAY2
         LASTDAY = DAY2
      END IF
C
C     Set up stations list.
C
      ISCH = 66
      DO ISTA = 1, NSTA
         IF( STASCN(ISCN,ISTA) ) THEN
            ISTN = LEN1(STATION(STANUM(ISTA)))
            WRITE( LINESC(ISCH:ISCH+ISTN+1), '(2A)' )
     1          STATION(STANUM(ISTA))(1:ISTN),','
            ISCH = ISCH + ISTN + 1
         END IF
      END DO
C
      IF( LSCH .NE. ISCH .OR. 
     1    LASTLINE(66:ISCH-2) .NE. LINESC(66:ISCH-2) ) THEN
         WRITE( LINESC(57:65), '(A)' ) 'STATIONS='
         LSCH = ISCH
      ELSE
         ISCH = 57
      END IF
C
C     Actually write out the line with a slash on the end.
C
      WRITE( IUOPT, '(A)' ) LINESC(1:ISCH-2)//' /'
      LASTLINE = LINESC
C
      FIRSTPT = .FALSE.
C
      RETURN
      END






