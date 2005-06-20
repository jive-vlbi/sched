      SUBROUTINE CHK4DAR( KS, SNBBC, ERRS )
C
C     Routine for SCHED called by CHKSET that checks items related
C     to the MARK IV DAR's.  It will only be called if the DAR is
C     of the MARK IV type.
C     By H.J. van Langevelde, JIVE, 281196 
C     Currently only supports astronomical patching irresp High or Low
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER        SNBBC, KS, ICH, I, LNAME, LEN1, JCH
      LOGICAL        ERRS, ALDONE, IFISLO, IFISHI, SPLPATCH
      LOGICAL        USENOR, PATERR
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 1, 'CHK4DAR: starting.' )
C
C
C
      PATERR = .FALSE.
C
      LNAME = LEN1( SETNAME(KS) )
C
C     Check FORMAT.
C
      IF( VLBITP .AND. FORMAT(KS) .NE. 'NONE' ) THEN
         IF( FORMAT(KS) .NE. 'MARKIII' .AND. 
     1       FORMAT(KS) .NE. 'MKIV1:1' .AND. 
     2       FORMAT(KS) .NE. 'MKIV1:2' .AND.
     3       FORMAT(KS) .NE. 'MKIV1:4' .AND. 
     4       FORMAT(KS) .NE. 'MKIV2:1' .AND.
     5       FORMAT(KS) .NE. 'MKIV4:1' .AND.
     4       FORMAT(KS) .NE. 'S2' ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, A)' )
     1          'CHK4DAR: Invalid FORMAT for wideband observing '//
     2          '(see OBSTYPE): ', FORMAT(KS)
            CALL WLOG( 1, MSGTXT )
            ERRS = .TRUE.
         END IF
      END IF
C
C     For MkIV IFDIST should not be used (currently)
C
      DO I = 1, 4
         IF( IFDIST(I,KS) .NE. '0'  ) THEN
            MSGTXT = 'CHK4DAR: IFDIST specified: '//IFDIST(I,KS)// 
     1             ' not allowed in MkIV mode '
            CALL WLOG ( 1, MSGTXT )
            ERRS = .TRUE.
         END IF
      END DO
C
C     Check items that depend on observation. Huib can't find
C     documentation must be same as VLBA
C
      IF( VLBITP ) THEN 
         IF( SAMPRATE(KS) .NE. 0.25 .AND. SAMPRATE(KS) .NE. 0.5 .AND. 
     1       SAMPRATE(KS) .NE. 1.0  .AND. SAMPRATE(KS) .NE. 2.0 .AND. 
     2       SAMPRATE(KS) .NE. 4.0  .AND. SAMPRATE(KS) .NE. 8.0 .AND.  
     3       SAMPRATE(KS) .NE. 16.  .AND. SAMPRATE(KS) .NE. 32. ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, F8.3 )' )
     1          'CHK4DAR: Invalid SAMPRATE specified: ', SAMPRATE(KS)
            CALL WLOG ( 1, MSGTXT )
            ERRS = .TRUE.
         END IF
C
         IF( BITS(1,KS) .NE. 1 .AND. BITS(1,KS) .NE. 2 ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I5 )' ) 'CHK4DAR: BITS must be 1 or 2'
     1           // ' - Requested: ', BITS(1,KS)
            CALL WLOG ( 1, MSGTXT )
            ERRS = .TRUE.
         END IF
      END IF
C
C     Check channel parameters, here is the fun for MkIV
C
      USENOR = .FALSE.
      IFISLO = .FALSE.
      IFISHI = .FALSE.
      SPLPATCH = .FALSE.
      DO ICH = 1, NCHAN(KS)
C
C        IF channel assignment. Remember this maps as follow
C
         IF( IFCHAN(ICH,KS) .NE. '1N' .AND. 
     1       IFCHAN(ICH,KS) .NE. '1A' .AND.
     2       IFCHAN(ICH,KS) .NE. '2N' .AND. 
     3       IFCHAN(ICH,KS) .NE. '2A' .AND.
     4       IFCHAN(ICH,KS) .NE. '3' ) THEN
            CALL WLOG ( 1, 'CHK4DAR: IFCHAN ''' // IFCHAN(ICH,KS) //
     1          ''' not 1N,1A,2A, or 2N' )
            ERRS = .TRUE.
            IF( IFCHAN(ICH,KS) .EQ. '3' ) 
     1          CALL WLOG ( 1, 'CHK4DAR: Not many astronomical ' //
     2          'stations support IF=3, do I?' )
         END IF
C        
C        Make sure that at least one of the inputs is NOR. If NOR is not
C        used at all, then there is no signal from the telescope.
         IF( IFCHAN(ICH,KS) .EQ. '1N' .OR. 
     1       IFCHAN(ICH,KS) .EQ. '2N') THEN
            USENOR = .TRUE.
         END IF
C
C        1N should not occur with 1A, 2N not with 2A
C
         ALDONE = .FALSE.
         IF( ICH .GT. 1 ) THEN
            DO JCH = 1, ICH-1
C
C              looks like I did this not optimal, but is historical:
C
               IF( ( IFCHAN(ICH,KS) .EQ. '1N' .AND.
     1             IFCHAN(JCH,KS) .EQ. '1A' ) .OR. 
     2             ( IFCHAN(ICH,KS) .EQ. '2A' .AND. 
     3             IFCHAN(JCH,KS) .EQ. '2N' )  .OR.
     2             ( IFCHAN(ICH,KS) .EQ. '1A' .AND. 
     3             IFCHAN(JCH,KS) .EQ. '1N' )  .OR.
     2             ( IFCHAN(ICH,KS) .EQ. '2N' .AND. 
     3             IFCHAN(JCH,KS) .EQ. '2A' )) THEN
                  IF( .NOT. ALDONE ) THEN 
                     MSGTXT = ' '
                     WRITE( MSGTXT, '( A  )' )
     1                   'CHK4DAR: IFCHAN setup assigns IFbox '//
     2                   'to two inputs simultaneously '
                     CALL WLOG( 1, MSGTXT )
                     ERRS = .TRUE.
                     ALDONE = .TRUE.
                  END IF
               END IF
            END DO
         END IF
C
C        BBC assignment, check total number
C
         IF( BBC(ICH,KS) .LT. 1 ) THEN
            CALL WLOG( 1, 'CHK4DAR: BBC number less than 1! ')
            ERRS = .TRUE.
         END IF
         IF( BBC(ICH,KS) .GT. SNBBC ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I3, A, I3, 2A )' )
     1         'CHK4DAR: Channel', ICH, ' uses a BBC (', BBC(ICH,KS),
     2         ') that does not exist.'
            CALL WLOG( 1, MSGTXT )
            ERRS = .TRUE.
         END IF
C
C        Now check patching, for now allow astronomical only:
C
         IF( MOD(BBC(ICH,KS),2) .EQ. 0 ) THEN
            IF( IFCHAN(ICH,KS) .NE. '2A' 
     1          .AND. IFCHAN(ICH,KS) .NE. '2N' ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, I2, A, A3, A, A )' )
     1             'CHK4DAR: MkIV astronomical patching '//
     2             'has even numbered BBCs on IF2 not: BBC= ',
     3             BBC(ICH,KS), ' with IF= ',
     4             IFCHAN(ICH,KS), ' in setup ',
     5             SETNAME(KS)(1:LEN1(SETNAME(KS)))
               CALL WLOG( 1, MSGTXT )
               ERRS = .TRUE.
               PATERR = .TRUE.
            END IF
         ELSE
            IF( IFCHAN(ICH,KS) .NE. '1N' 
     1          .AND. IFCHAN(ICH,KS) .NE. '1A' ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, I2, A, A3, A, A )' )
     1             'CHK4DAR: MkIV astronomical patching '//
     2             'has odd numbered BBCs on IF1 not: BBC= ',
     3             BBC(ICH,KS), ' with IF= ',
     4             IFCHAN(ICH,KS), ' in setup ',
     5             SETNAME(KS)(1:LEN1(SETNAME(KS)))
               CALL WLOG( 1, MSGTXT )
               ERRS = .TRUE.
               PATERR = .TRUE.
            END IF
         END IF
C
C        BBC synthesizer setting, must be between 100 and 500
C
         IF( BBSYN(ICH,KS) .LT. 100.0 .OR.
     1       BBSYN(ICH,KS) .GT. 500.0 ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, F8.2 )' )
     1         'CHK4DAR: BBSYN not between 100 and 500: ', 
     2         BBSYN(ICH,KS)
            CALL WLOG( 1, MSGTXT )
            WRITE( MSGTXT, '( A, F9.2, 2A )' )
     1         '        FIRSTLO and first sideband are:', 
     2         FIRSTLO(ICH,KS), '  ', SIDE1(ICH,KS)
            CALL WLOG( 1, MSGTXT )
            ERRS = .TRUE.
C
C        We can also check whether High/Low IF output is used
C        consistently.  It cannot be checked whether it is conform the
C        stations preferred patching. But usually they should all be on
C        low or all on high. I am assuming Drudge picks low below 220
C        and high above Formally low can do up to 224 and high down to
C        216
C
         ELSE IF( BBSYN(ICH,KS) .GT. 96 .AND. 
     1       BBSYN(ICH,KS) .LT. 220 ) THEN 
C     
C           must be low
C
            IFISLO = .TRUE.
            IF( IFISHI ) THEN
C              must issue error
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, I2.2, A )' )
     1             'CHK4DAR: Inconsistent patching VC#',
     2             BBC(ICH,KS), ' is not on High '
               CALL WLOG( 1, MSGTXT )
               SPLPATCH = .TRUE.
            END IF
         ELSE IF( BBSYN(ICH,KS) .GT. 220 .AND. 
     1          BBSYN(ICH,KS) .LT. 504 ) THEN 
C
C           must be high
C
            IFISHI = .TRUE.
            IF( IFISLO ) THEN
C              must issue error
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, I2.2, A )' )
     1             'CHK4DAR: Inconsistent patching VC#',
     2             BBC(ICH,KS), ' is not on Low '
               CALL WLOG( 1, MSGTXT )
               SPLPATCH = .TRUE.
            END IF
C
C           current version should not reach grey area below, but keep logic
C
         ELSE IF( BBSYN(ICH,KS) .GT. 216 .AND. 
     1          BBSYN(ICH,KS) .LT. 224 ) THEN 
C              must issue warning
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I2.2, A )' )
     1          'CHK4DAR: Dangerous patching VC#',
     2          BBC(ICH,KS), ' could possibly go either way Hi/Lo '
            CALL WLOG( 1, MSGTXT )
         ELSE 
            CALL ERRLOG(' CHK4DAR: This Error is impossible ')
         END IF
C
C        BBC filter bandwidth, the following set exists
C
         IF( BBFILT(ICH,KS).NE.0.0625 .AND. BBFILT(ICH,KS).NE.0.125
     1       .AND. BBFILT(ICH,KS).NE.0.250 .AND. BBFILT(ICH,KS).NE.0.5 
     2       .AND. BBFILT(ICH,KS).NE.1.0   .AND. BBFILT(ICH,KS).NE.2.0 
     3       .AND. BBFILT(ICH,KS).NE.4.0   .AND. BBFILT(ICH,KS).NE.8.0
     4       .AND. BBFILT(ICH,KS).NE.16. ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, F8.2 )' )
     1          'CHK4DAR: Invalid BBFILTER specified: ',
     2          BBFILT(ICH,KS)
            CALL WLOG( 1, MSGTXT )
            ERRS = .TRUE.
C
C           but some of these are not standard in the rack:
C
         ELSE IF( BBFILT(ICH,KS) .EQ. 0.250 
     1          .OR. BBFILT(ICH,KS).EQ.1.000 ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, F6.2, A )' )
     1          'CHK4DAR: The ', BBFILT(ICH,KS),
     2          'MHz filter only available as plug-in '//
     3          'at most MkIV stations, contact friend of VLBI'
            CALL WLOG( 1, MSGTXT )
C
C              not an error, available as plug in...
C
         ELSE IF( BBFILT(ICH,KS) .EQ. 0.500 
     1          .OR. BBFILT(ICH,KS).EQ. 0.125 ) THEN
C
C              and the narrow ones could be restricted to 1st 8 USB
C
            IF( BBC(ICH,KS) .GT. 8 ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, A, A, I2.2, A )' )
     1             'CHK4DAR: ', SETNAME(KS)(1:LEN1(SETNAME(KS))),
     2             ' uses narrow filter in BBC#', BBC(ICH,KS),
     3             ', verify with station! '
               CALL WLOG( 1, MSGTXT )
            ENDIF
            IF (SIDEBD(ICH,KS) .EQ. 'L' ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, A, A )' ) 
     1             'CHK4DAR: ', SETNAME(KS)(1:LEN1(SETNAME(KS))),
     2             'uses narrow filters in LSB, verify with station! '
               CALL WLOG( 1, MSGTXT )
            END IF
         END IF
C
C        copy this bit on frequency switching, already disallowed
C
         IF( FRSWITCH(KS) .AND. (BBSYN2(ICH,KS) .LT. 100.0 .OR.
     1       BBSYN2(ICH,KS) .GT. 500.0 ) ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, F8.2 )' )
     1         'CHK4DAR: BBSYN2 not between 100 and 500: ', 
     2          BBSYN2(ICH,KS)
            CALL WLOG( 1, MSGTXT )
            ERRS = .TRUE.
         END IF
C
C        stop use of VC's above 8 in 1:4 fan-out, Graham 22/1/01
C
         IF( BBC(ICH,KS) .GT. 8 .AND. FANOUT(KS) .EQ. 4.0 ) THEN 
            WRITE( MSGTXT, '( A, A )' ) 
     1          'CHK4DAR: VC9 and above cannot be used in 1:4',
     2          ' fan-out choose another setup '
            CALL WLOG( 1, MSGTXT )
            ERRS = .TRUE.
         END IF
C
      END DO
C
      IF( PATERR ) THEN
        CALL WRTMSG( 'CHK4DAR', 'astropatch' )
      END IF
C
      IF( .NOT. USENOR ) THEN
         WRITE(MSGTXT, '( A, A, A)' )
     1       'CHk4DAR: Neither IFCHAN 1N nor 2N have been assigned ',
     2       ' in setup ',SETNAME(KS)(1:LEN1(SETNAME(KS)) )
         CALL WLOG( 1, MSGTXT )
         WRITE(MSGTXT, '(A, A)' )
     1      'This is not a valid setup for a MkIV data acquisition ',
     2      'rack. Please check your setting of IFCHAN'
         CALL WLOG( 1, MSGTXT )
         ERRS = .TRUE.
      END IF
        
C
      IF (SPLPATCH) THEN 
         WRITE(MSGTXT, '( A, A, A, A)' )
     1       'CHk4DAR: Requested mixed patching for ',SETSTA(1,KS),
     2       ' in setup ',SETNAME(KS)(1:LEN1(SETNAME(KS)) )
         CALL WLOG( 1, MSGTXT )
         WRITE(MSGTXT, '( A, A )' )
     1       '         Decrease frequency coverage or change LO, ', 
     2       'or verify with station that this is OK!'
         CALL WLOG( 1, MSGTXT )
C
      END IF 
      RETURN
      END
C


