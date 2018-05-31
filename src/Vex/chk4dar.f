      SUBROUTINE CHK4DAR( KS, SNBBC, ERRS )
Cf2py intent(in, out) ERRS
C
C     Routine for SCHED called by CHKSET that checks items related
C     to the MARK IV DAR's.  It will only be called if the DAR is
C     of the MARK IV type.
C     By H.J. van Langevelde, JIVE, 281196 
C
C     2007-10-18 Modified by Cormac to know a little bit about GEO
C     patching. Previously could only handle ASTRO patching. Also added
C     a 'FREE' patch to allow blackbelts to do crazy things.
C
C     Rules for astro patching: Odd numbered VCs go on IF1N (and 2A),
C     even numbered on IF2N (and 1A). At least one of IF1N or IF2N must
C     be used. High/low patch should be the same for all VCs if
C     possible.
C
C     Rules for geo patching:
C        VCs 1-2    IF1N low
C        VC  3      IF1N high/low
C        VC  4      IF1N high
C        VCs 5-8    IF1N high or IF3N high or IF3O high
C        VC  9      IF2N low
C        VC  10     IF2N high/low
C        VCs 11-14  IF2N high
C     Geo patching doesn't typically have anything on the alt input.
C
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER        SNBBC, KS, ICH, I, LNAME, LEN1, JCH
      LOGICAL        ERRS, ALDONE, IFISLO, IFISHI, SPLPATCH
      LOGICAL        USENOR, PATERR, GEOPATER, GEOSPLPA
      REAL           LOWEDGE, HIEDGE
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 1, 'CHK4DAR: starting.' )
C
C
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
     6       FORMAT(KS) .NE. 'MARK5B' .AND.
     7       FORMAT(KS) .NE. 'S2' ) THEN
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
      PATERR = .FALSE.
      GEOSPLPA = .FALSE.
      GEOPATER = .FALSE.
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
            CALL WLOG ( 1, 'CHK4DAR: IFCHAN ''' // IFCHAN(ICH,KS) //
     1          ''' not 1N,1A,2A,2N,3N or 3O' )
            ERRS = .TRUE.
         END IF
         IF( IFCHAN(ICH,KS)(1:1) .EQ. '3' ) THEN
                CALL WLOG ( 1, 'CHK4DAR: Not many astronomical ' //
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
C        Now check patching
C
         IF( M4PATCH(KS) .EQ. 'FREE' ) THEN
C           This switches off all patching checks. Should only be used by 
C           blackbelts
            WRITE( MSGTXT, '(A, A, A)' )
     1         'You have specified FREE patching in setup ',
     2         SETNAME(KS)(1:LEN1(SETNAME(KS))),
     3         '. This schedule will almost certainly *not* work. Be'//
     4         ' very sure that you know what you are doing!'
            CALL WLOG( 1, MSGTXT )
         ELSE IF( M4PATCH(KS) .EQ. 'GEO1' ) THEN
C           Check for consistency with standard geodetic patching
            IF( BBC(ICH,KS) .LE. 4 .AND. 
     1           IFCHAN(ICH,KS) .NE. '1N' ) THEN

                MSGTXT = ' '
                WRITE( MSGTXT, '( A, I2, A, A3, A, A )' )
     1              'CHK4DAR: MkIV geodetic patching '//
     2              'has BBCs 1-4 on IF 1N, not BBC = ',
     3              BBC(ICH,KS), ' with IF= ',
     4              IFCHAN(ICH,KS), ' in setup ',
     5              SETNAME(KS)(1:LEN1(SETNAME(KS)))
                CALL WLOG( 1, MSGTXT )
                ERRS = .TRUE.
                GEOPATER = .TRUE.
            ELSE IF( ( BBC(ICH,KS) .GE. 5 .AND. BBC(ICH,KS) .LE. 8 ) 
     1          .AND. ( IFCHAN(ICH,KS) .NE. '3N' .AND. 
     2                  IFCHAN(ICH,KS) .NE. '3O' .AND.
     3                  IFCHAN(ICH,KS) .NE. '1N' ) ) THEN
                MSGTXT = ' '
                WRITE( MSGTXT, '( A, I2, A, A3, A, A )' )
     1              'CHK4DAR: MkIV geodetic patching '//
     2              'has BBCs 5-8 on IF 3N or 3O or 1N, not BBC = ',
     3              BBC(ICH,KS), ' with IF= ',
     4              IFCHAN(ICH,KS), ' in setup ',
     5              SETNAME(KS)(1:LEN1(SETNAME(KS)))
                CALL WLOG( 1, MSGTXT )
                ERRS = .TRUE.
                GEOPATER = .TRUE.
            ELSE IF( BBC(ICH,KS) .GE. 9 .AND. 
     1                IFCHAN(ICH,KS) .NE. '2N' ) THEN
                MSGTXT = ' '
                WRITE( MSGTXT, '( A, I2, A, A3, A, A )' )
     1              'CHK4DAR: MkIV geodetic patching '//
     2              'has BBCs 9-14 on IF 2, not BBC = ',
     3              BBC(ICH,KS), ' with IF= ',
     4              IFCHAN(ICH,KS), ' in setup ',
     5              SETNAME(KS)(1:LEN1(SETNAME(KS)))
                CALL WLOG( 1, MSGTXT )
                ERRS = .TRUE.
                GEOPATER = .TRUE.
            END IF
         ELSE IF( M4PATCH(KS) .EQ. 'ASTRO' ) THEN
C           This means we have astro patching
            IF( MOD(BBC(ICH,KS),2) .EQ. 0 ) THEN
               IF( IFCHAN(ICH,KS) .NE. '2A' 
     1             .AND. IFCHAN(ICH,KS) .NE. '2N' ) THEN
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, I2, A, A3, A, A )' )
     1                'CHK4DAR: MkIV astronomical patching '//
     2                'has even numbered BBCs on IF2 not: BBC= ',
     3                BBC(ICH,KS), ' with IF= ',
     4                IFCHAN(ICH,KS), ' in setup ',
     5                SETNAME(KS)(1:LEN1(SETNAME(KS)))
                  CALL WLOG( 1, MSGTXT )
                  ERRS = .TRUE.
                  PATERR = .TRUE.
               END IF
            ELSE
               IF( IFCHAN(ICH,KS) .NE. '1N' 
     1             .AND. IFCHAN(ICH,KS) .NE. '1A' ) THEN
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, I2, A, A3, A, A )' )
     1                'CHK4DAR: MkIV astronomical patching '//
     2                'has odd numbered BBCs on IF1 not: BBC= ',
     3                BBC(ICH,KS), ' with IF= ',
     4                IFCHAN(ICH,KS), ' in setup ',
     5                SETNAME(KS)(1:LEN1(SETNAME(KS)))
                  CALL WLOG( 1, MSGTXT )
                  ERRS = .TRUE.
                  PATERR = .TRUE.
               END IF
            END IF
         ELSE
            ERRS = .TRUE.
            MSGTXT = ' Unrecognized M4PATCH: ' // M4PATCH(KS)
            CALL WLOG( 1, MSGTXT )
         END IF

C        Get the edge of the baseband filters, because this is what
C        Drudg uses to determine whether to patch to high or low.
         LOWEDGE = BBSYN(ICH, KS)
         HIEDGE = BBSYN(ICH, KS)
         IF (NETSIDE(ICH,KS) .EQ. 'L') THEN
            LOWEDGE = LOWEDGE - BBFILT(ICH,KS)
         ELSE IF (NETSIDE(ICH,KS) .EQ. 'U') THEN
            HIEDGE = HIEDGE + BBFILT(ICH,KS)
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
C           We can also check whether High/Low IF output used conforms
C           with standards. 
C
C           For ASTRO patching, usually prefer that VCs are all on low
C           or all on high. There is some overlap between the high and
C           low patches and Drudg will try to keep all VCs on the same
C           patch if possible, so ignore VCs that lie in this range
C           (email from Himwich). 
C
C           For GEO patching the preference for high or low patch
C           depends on the VC number, and again Drudg will try to put
C           VCs in the overlap region on the right patch.
C
         ELSE IF( M4PATCH(KS) .EQ. 'GEO1' ) THEN
            IF( ( BBC(ICH,KS) .LE. 2 .OR.
     1            BBC(ICH,KS) .EQ. 9 ) 
     2         .AND. ( HIEDGE .GT. 230 ) )  THEN
C
C              These VCs should be on low
C
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, I2.2, A )' )
     1           'CHK4DAR: Improper patching BBC#',
     2            BBC(ICH,KS), 
     3            ' is not on Low.'
                  CALL WLOG( 1, MSGTXT )
               GEOSPLPA = .TRUE.
            ELSE IF( ( ( BBC(ICH,KS) .GE. 4 .AND. BBC(ICH,KS) .LE. 8 ) 
     1        .OR. ( BBC(ICH,KS) .GE. 11 .AND. BBC(ICH,KS) .LE. 14 ) ) 
     2        .AND. ( LOWEDGE .LT. 210 ) )  THEN
C
C              These VCs should be on high
C
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, I2.2, A )' )
     1             'CHK4DAR: Inconsistent patching BBC#',
     2             BBC(ICH,KS), 
     3             ' is not on High.'
               CALL WLOG( 1, MSGTXT )
               GEOSPLPA = .TRUE.
            ELSE
C              Will get here for VCs 3 and 10 where either patch is o.k.
            END IF

         ELSE IF( M4PATCH(KS) .EQ. 'ASTRO' ) THEN
C
C           This must be astro patching. Patches can be high or low, but
C           should be the same for all VCs if possible.
C            
            IF( LOWEDGE .LT. 210 ) THEN 
C     
C              must be low
C
               IFISLO = .TRUE.
               IF( IFISHI ) THEN
C                 must issue warning
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, I2.2, A )' )
     1                'CHK4DAR: Inconsistent patching BBC#',
     2                BBC(ICH,KS), ' is not on High '
                  CALL WLOG( 1, MSGTXT )
                  SPLPATCH = .TRUE.
               END IF
            ELSE IF( HIEDGE .GT. 230 ) THEN 
C
C              must be high
C
               IFISHI = .TRUE.
               IF( IFISLO ) THEN
C                 must issue warning
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, I2.2, A )' )
     1                'CHK4DAR: Inconsistent patching BBC#',
     2                BBC(ICH,KS), ' is not on Low '
                  CALL WLOG( 1, MSGTXT )
                  SPLPATCH = .TRUE.
               END IF
C
            END IF
         ELSE IF( M4PATCH(KS) .EQ. 'FREE') THEN
C           Anything goes. This is the 'blackbelt' mode for people like
C           Dave Graham.
         ELSE
            ERRS = .TRUE.
            MSGTXT = ' Unrecognized M4PATCH: ' // M4PATCH(KS)
            CALL WLOG( 1, MSGTXT )
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
     1          'CHK4DAR: BBC9 and above cannot be used in 1:4',
     2          ' fan-out choose another setup '
            CALL WLOG( 1, MSGTXT )
            ERRS = .TRUE.
         END IF
C
      END DO
C
      IF( PATERR ) THEN
        CALL WRTMSG(0, 'CHK4DAR', 'astropatch' )
      END IF
C
      IF( GEOPATER .OR. GEOSPLPA ) THEN
        CALL WRTMSG(0, 'CHK4DAR', 'geopatch' )
      END IF
C
      IF( M4PATCH(KS) .EQ. 'FREE' ) THEN
        CALL WRTMSG(0, 'CHK4DAR', 'freepatch' )
      ELSE IF( M4PATCH(KS) .EQ. 'GEO1' ) THEN
         WRITE( MSGTXT, '(A)' )
     1      'You have specified geodetic patching (M4PATCH=GEO1).'//
     2      ' This is experimental - check results carefully!'
         CALL WLOG( 1, MSGTXT )
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
C
      IF (GEOSPLPA) THEN
         WRITE(MSGTXT, '( A, A, A, A)' )
     1       'CHk4DAR: Requested non-standard geodetic patching for ',
     2       SETSTA(1,KS), ' in setup ',
     3       SETNAME(KS)(1:LEN1(SETNAME(KS)) )
         CALL WLOG( 1, MSGTXT )
         WRITE(MSGTXT, '( A, A )' )
     1       '         Change frequency or LO, ', 
     2       'or verify with station that this is OK! ' //
     3       'See sched.runlog for more details.'
         CALL WLOG( 1, MSGTXT )
C
      END IF 

      RETURN
      END
C


