      SUBROUTINE CHK4GEO( KS, SNBBC, ERRS )
C
C     Routine for SCHED called by CHKSET that checks items related
C     to the MARK IV DAR's.  It will only be called if the DAR is
C     of the MARK IV type.
C
C     By H.J. van Langevelde, JIVE, 281196 
C
C     Modified version of CHK4DAR that will pass schedules that use
C     geodetic patching.  The patching checks are minimal so far.
C     Will be called when M4PATCH='GEO1'
C     Some day, this should be merged back into chk4dar.f.  The
C     changes are just in the patching area, I think.
C     I'm basing the checks on information from Brian Corey send by
C     email on June 11, 2007 while preparing for RD0705 - Alessandra
C     Bertarini's thesis observations of polarization on geo stations.
C     Here is the table for the normal geodetic setup.  That is what
C     we will assume:
C        VCs 1-2    IF1 low  = 8180-8300 MHz
C         "  3-4    IF1 high = 8300-8580 MHz
C         "  5-8    IF3      = 8680-8980 or 8280-8580 MHz
C         "  9-10   IF2 low  = 2120-2240 MHz
C         "  11-14  IF2 high = 2240-2520 MHz
C
C     RCW  June 20, 2007.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER        SNBBC, KS, ICH, I, LNAME, LEN1, JCH
      LOGICAL        ERRS, ALDONE, IFISLO, IFISHI, SPLPATCH
      LOGICAL        USENOR, PATERR
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 1, 'CHK4GEO: starting.' )
C
      CALL WLOG( 1, ' Warning: Using Geodetic patching. ' )
      CALL WLOG( 1, '          Not well understood by SCHED.' )
      CALL WLOG( 1, '          CHECK VERY CAREFULLY!' )
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
     1          'CHK4GEO: Invalid FORMAT for wideband observing '//
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
            MSGTXT = 'CHK4GEO: IFDIST specified: '//IFDIST(I,KS)// 
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
     1          'CHK4GEO: Invalid SAMPRATE specified: ', SAMPRATE(KS)
            CALL WLOG ( 1, MSGTXT )
            ERRS = .TRUE.
         END IF
C
         IF( BITS(1,KS) .NE. 1 .AND. BITS(1,KS) .NE. 2 ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I5 )' ) 'CHK4GEO: BITS must be 1 or 2'
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
     4       IFCHAN(ICH,KS) .NE. '3N' .AND.
     5       IFCHAN(ICH,KS) .NE. '3O' ) THEN
            CALL WLOG ( 1, 'CHK4GEO: IFCHAN ''' // IFCHAN(ICH,KS) //
     1          ''' not 1N, 2N, 3N, or 3O' )
            ERRS = .TRUE.
            IF( IFCHAN(ICH,KS) .EQ. '3' ) 
     1          CALL WLOG ( 1, 'CHK4GEO: Not many astronomical ' //
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
     1                   'CHK4GEO: IFCHAN setup assigns IFbox '//
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
            CALL WLOG( 1, 'CHK4GEO: BBC number less than 1! ')
            ERRS = .TRUE.
         END IF
         IF( BBC(ICH,KS) .GT. SNBBC ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I3, A, I3, 2A )' )
     1         'CHK4GEO: Channel', ICH, ' uses a BBC (', BBC(ICH,KS),
     2         ') that does not exist.'
            CALL WLOG( 1, MSGTXT )
            ERRS = .TRUE.
         END IF
C
C        Now check patching.  Do for the geodetic case.
C        This where the code from chk4dar and this routine could
C        be in separate sections of an IF statement.  But I did not
C        want to alter Cormac's code.
C
         IF( BBC(ICH,KS) .LE. 4 .AND. 
     1       IFCHAN(ICH,KS) .NE. '1N' ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I2, A, A3, A, A )' )
     1          'CHK4GEO: MkIV geodetic patching '//
     2          'has BBCs 1-4 on IF 1N, not BBC = ',
     3          BBC(ICH,KS), ' with IF= ',
     4          IFCHAN(ICH,KS), ' in setup ',
     5          SETNAME(KS)(1:LEN1(SETNAME(KS)))
            CALL WLOG( 1, MSGTXT )
            ERRS = .TRUE.
            PATERR = .TRUE.
         ELSE IF( ( BBC(ICH,KS) .GE. 5 .AND. BBC(ICH,KS) .LE. 8 ) .AND.
     1            ( IFCHAN(ICH,KS) .NE. '3N' .AND. 
     2              IFCHAN(ICH,KS) .NE. '3O' ) ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I2, A, A3, A, A )' )
     1          'CHK4GEO: MkIV geodetic patching '//
     2          'has BBCs 5-8 on IF 3N or 3O, not BBC = ',
     3          BBC(ICH,KS), ' with IF= ',
     4          IFCHAN(ICH,KS), ' in setup ',
     5          SETNAME(KS)(1:LEN1(SETNAME(KS)))
            CALL WLOG( 1, MSGTXT )
            ERRS = .TRUE.
            PATERR = .TRUE.
         ELSE IF( BBC(ICH,KS) .GE. 9 .AND. 
     1            IFCHAN(ICH,KS) .NE. '2N' ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I2, A, A3, A, A )' )
     1          'CHK4GEO: MkIV geodetic patching '//
     2          'has BBCs 9-14 on IF 2, not BBC = ',
     3          BBC(ICH,KS), ' with IF= ',
     4          IFCHAN(ICH,KS), ' in setup ',
     5          SETNAME(KS)(1:LEN1(SETNAME(KS)))
            CALL WLOG( 1, MSGTXT )
            ERRS = .TRUE.
            PATERR = .TRUE.
         END IF
C
C        BBC synthesizer setting, must be between 100 and 500
C
         IF( BBSYN(ICH,KS) .LT. 100.0 .OR.
     1       BBSYN(ICH,KS) .GT. 500.0 ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, F8.2 )' )
     1         'CHK4GEO: BBSYN not between 100 and 500: ', 
     2         BBSYN(ICH,KS)
            CALL WLOG( 1, MSGTXT )
            WRITE( MSGTXT, '( A, F9.2, 2A )' )
     1         '        FIRSTLO and first sideband are:', 
     2         FIRSTLO(ICH,KS), '  ', SIDE1(ICH,KS)
            CALL WLOG( 1, MSGTXT )
            ERRS = .TRUE.
C
C        The high/low patching is another confusing issue.  We will
C        force conformance with Brian Corey's table, although will
C        assume that IF3 can be either, as per the email.
C
         ELSE IF( ( BBC(ICH,KS) .LE. 2 .OR.
     1       ( BBC(ICH,KS) .GE. 9 .AND. BBC(ICH,KS) .LE. 10 ) ) .AND.
     2       .NOT. ( BBSYN(ICH,KS) .GT. 96 .AND. 
     3       BBSYN(ICH,KS) .LT. 220 ) )  THEN 
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I2.2, A )' )
     1        'CHK4GEO: Improper patching VC#',
     2         BBC(ICH,KS), ' is not on Low '
               CALL WLOG( 1, MSGTXT )
            SPLPATCH = .TRUE.
         ELSE IF( ( ( BBC(ICH,KS) .GE. 3 .AND. BBC(ICH,KS) .LE. 4 ) .OR.
     1       ( BBC(ICH,KS) .GE. 11 .AND. BBC(ICH,KS) .LE. 14 ) ) .AND.
     2       .NOT. ( BBSYN(ICH,KS) .GT. 220 .AND. 
     3       BBSYN(ICH,KS) .LT. 504 ) )  THEN 
C
C           must be high
C
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I2.2, A )' )
     1          'CHK4GEO: Inconsistent patching VC#',
     2          BBC(ICH,KS), ' is not on High '
            CALL WLOG( 1, MSGTXT )
            SPLPATCH = .TRUE.
         ELSE 
C            Will get here for IF 3 where eigher is possible.
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
     1          'CHK4GEO: Invalid BBFILTER specified: ',
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
     1          'CHK4GEO: The ', BBFILT(ICH,KS),
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
     1             'CHK4GEO: ', SETNAME(KS)(1:LEN1(SETNAME(KS))),
     2             ' uses narrow filter in BBC#', BBC(ICH,KS),
     3             ', verify with station! '
               CALL WLOG( 1, MSGTXT )
            ENDIF
            IF (SIDEBD(ICH,KS) .EQ. 'L' ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, A, A )' ) 
     1             'CHK4GEO: ', SETNAME(KS)(1:LEN1(SETNAME(KS))),
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
     1         'CHK4GEO: BBSYN2 not between 100 and 500: ', 
     2          BBSYN2(ICH,KS)
            CALL WLOG( 1, MSGTXT )
            ERRS = .TRUE.
         END IF
C
C        stop use of VC's above 8 in 1:4 fan-out, Graham 22/1/01
C
         IF( BBC(ICH,KS) .GT. 8 .AND. FANOUT(KS) .EQ. 4.0 ) THEN 
            WRITE( MSGTXT, '( A, A )' ) 
     1          'CHK4GEO: VC9 and above cannot be used in 1:4',
     2          ' fan-out choose another setup '
            CALL WLOG( 1, MSGTXT )
            ERRS = .TRUE.
         END IF
C
      END DO
C
      IF( PATERR ) THEN
        CALL WRTMSG( 'CHK4GEO', 'astropatch' )
      END IF
C
      IF( .NOT. USENOR ) THEN
         WRITE(MSGTXT, '( A, A, A)' )
     1       'CHK4GEO: Neither IFCHAN 1N nor 2N have been assigned ',
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
     1       'CHK4GEO: Requested mixed patching for ',SETSTA(1,KS),
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


