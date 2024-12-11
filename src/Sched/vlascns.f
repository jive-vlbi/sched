      SUBROUTINE VLASCNS
C
C     Routine for SCHED that checks and/or sets some items
C     for the VLA.
C
C     Started Oct. 27, 2012 mainly to be sure phasing modes
C     specified with VLAMODE are reflected in the INTENTS.
C
      INCLUDE   'sched.inc'
C
      INTEGER   ISCN, ISTA
      INTEGER   IIVA, IIVX, IIVO, IIVS, II
      INTEGER   IIDE, IIAP, IIAD, IIOF, LEN1
      LOGICAL   GOTPHINT, GOTVLAMD
      LOGICAL   GOTPTINT, VLADOPK
C -------------------------------------------------------------
C     See if the VLA is used, setting VLASTA if so.
C
      VLASTA = 0
      DO ISTA = 1, NSTA
         IF( STANAME(ISTA)(1:3) .EQ. 'VLA' ) THEN
            VLASTA = ISTA
         END IF
      END DO
C
C     Don't do the rest of this routine if the VLA is not present.
C
      IF( VLASTA .EQ. 0 ) RETURN
C
C     Allow only one method of specifying VLA phasing - specific INTENTS
C     or VLAMODE.  Detect which a user is using.  If both, stop.
C
C     First determine if any VLA phasing INTENTs were specified, and, if so,
C     which they were.
C     While at it, get the intent numbers for the VLA AUTOPHASE commands 
C     if any were issued by the user.
C     Note that the intent for single dish is not actually
C     supported yet.
C
C     Ignore similar commands meant for another site (presence of ":"
C     but not "VLA"), but accept a version that is not qualified by site.
C
      IIVA = 0
      IIVX = 0
      IIVO = 0
      IIVS = 0
      IF( NINTENT .GE. 1 ) THEN
         DO II = 1, NINTENT
            IF( INDEX( INTENT(II), ':' ) .EQ. 0  .OR. 
     1          INDEX( INTENT(II), 'VLA' ) .NE. 0 ) THEN
C
               IF( INDEX( INTENT(II), 'AUTOPHASE_DETERMINE' ) .NE. 0 ) 
     1              THEN 
                  IIVA = II
               ELSE IF( INDEX( INTENT(II), 'AUTOPHASE_APPLY' ) .NE. 0 ) 
     1              THEN
                  IIVX = II
               ELSE IF( INDEX( INTENT(II), 'AUTOPHASE_OFF' ) .NE. 0 ) 
     1              THEN
                  IIVO = II
               ELSE IF( INDEX( INTENT(II), 'SINGLE_DISH' ) .NE. 0 ) 
     1              THEN
                  IIVS = II
               ELSE IF( INDEX( INTENT(II), 'AUTOPHASE' ) .NE. 0 ) THEN
                  CALL WLOG( 1, ' ' )
                  CALL WLOG( 1, '   ************************  ' )
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( 3A )' )
     1                'VLASCNS: Unrecognized AUTOPHASE INTENT used: ',
     2                INTENT(II)(1:LEN1(INTENT(II))), 
     3                '.  Please fix.'
                  CALL WLOG( 1, MSGTXT )
                  CALL WLOG( 1, '         Recognized options are '//
     1                 '(see the manual):' )
                  CALL WLOG( 1, 
     1                          '         AUTOPHASE_DETERIMINE, '//
     2                 'AUTOPHASE_APPLY, AUTOPHASE_OFF, and '//
     3                 'SINGLE_DISH' )
                  CALL WLOG( 1, '         For the VLA, it is better '//
     2                 'to use VLAMODE.' )
                  CALL WLOG( 1, '   ************************  ' )
                  CALL WLOG( 1, ' ' )
               END IF
C
C              Attempt to detect an unrecognized AUTOPHASE command.
C

            END IF
         END DO
      END IF
      GOTPHINT =  IIVA .GT. 0 .OR. IIVX .GT. 0 .OR. 
     1            IIVO .GT. 0 .OR. IIVS .GT. 0 
C
C     Now detect the use of VLAMODE.  Note that the default is ZZ, not
C     one of the allowed modes, but test against the allowed ones.
C
      GOTVLAMD = .FALSE.
      DO ISCN = SCAN1, SCANL
         IF( VLAMODE(ISCN) .EQ. '  ' .OR. VLAMODE(ISCN) .EQ. 'VA' .OR.
     1       VLAMODE(ISCN) .EQ. 'VX' .OR. VLAMODE(ISCN) .EQ. 'VS' ) THEN
            GOTVLAMD = .TRUE.
         END IF
      END DO
C
C     Don't allow both methods in one project.
C
      IF( GOTPHINT .AND. GOTVLAMD ) THEN
         CALL WLOG( 1, 'VLASCNS:  Both VLA phasing INTENTS and '//
     1       'VLAMODE used in the same schedule.' )
         CALL WLOG( 1, '          This confuses SCHED so do not '//
     1       'do it.  VLAMODE is preferred.' )
         CALL ERRLOG( 
     1       '          Use only VLAMODE -or- phasing INTENTS ' )
      END IF
C
C     Now add the INTENTS and set their use if VLAMODE is being used.
C
      IF( GOTVLAMD ) THEN
         IF( NINTENT + 4 .GT. MINTENT ) CALL ERRLOG( 
     1      'VLASCNS: Too many intents for SCHED arrays after adding '//
     2      'phasing commands.' )
         INTENT(NINTENT+1) = 'VLA:AUTOPHASE_DETERMINE'
         IIVA = NINTENT + 1
         INTENT(NINTENT+2) = 'VLA:AUTOPHASE_APPLY'
         IIVX = NINTENT +2
         INTENT(NINTENT+3) = 'VLA:AUTOPHASE_OFF'
         IIVO = NINTENT + 3
         INTENT(NINTENT+4) = 'VLA:SINGLE_DISH'
         IIVS = NINTENT + 4
         NINTENT = NINTENT + 4      
C
C        Loop over scans to set the required phasing intents if they are
C        to be based on VLAMODE.
C
         DO ISCN = SCAN1, SCANL
C
C           Only worry about the scan if the VLA is in it.
C           Note that SCHOPT has not yet been called, so the current
C           scans may not be the final ones and the VLA might be
C           dropped from the list later.  But set the phasing intents now.
C           They might show up later on non-VLA scans, but that is not
C           really a problem.
C
            IF( STASCN(ISCN,VLASTA) ) THEN
               NSCINT(ISCN) = NSCINT(ISCN) + 1
               IF( VLAMODE(ISCN) .EQ. ' ' ) THEN
                   ISCINT(NSCINT(ISCN),ISCN) = IIVO
               ELSE IF( VLAMODE(ISCN) .EQ. 'VA' ) THEN
                   ISCINT(NSCINT(ISCN),ISCN) = IIVA
               ELSE IF( VLAMODE(ISCN) .EQ. 'VX' ) THEN
                   ISCINT(NSCINT(ISCN),ISCN) = IIVX
               ELSE IF( VLAMODE(ISCN) .EQ. 'VS' ) THEN
                   ISCINT(NSCINT(ISCN),ISCN) = IIVS
               ELSE
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, A, A, I5 )' )
     1               'VLASCNS: Invalid VLAMODE=', VLAMODE(ISCN),
     2               ' on scan ', ISCN
                  CALL WLOG( 1, MSGTXT )
                  CALL WLOG( 1, '         VLAMODE must be '' '', '//
     1               '''VA'', ''VX'', or ''VS''' )
                  CALL ERRLOG( '         A valid VLA phasing mode '//
     1               'via VLAMODE or INTENT is required.' )
               END IF
C
            END IF
         END DO
C
      END IF
C
C     -----------------
C
C     Now do a rather similar sequence of steps to deal with reference
C     pointing.
C
C     First find out if VLAPEAK is ever invoked for anything but OFF
C     during this observation.  Set VLADOPK.  Check for invalid VLAPEAK
C     commands while at it.
C
      VLADOPK = .FALSE.
      DO ISCN = SCAN1, SCANL
         IF( VLAPEAK(ISCN) .EQ. 'DETERMINE' .OR. 
     1       VLAPEAK(ISCN) .EQ. 'ADJUST' .OR.
     2       VLAPEAK(ISCN) .EQ. 'APPLY' ) THEN
            VLADOPK = .TRUE.
         ELSE IF( VLAPEAK(ISCN) .NE. 'OFF' ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, A, A, I5 )' )
     1          'VLASCNS:  Invalid VLAPEAK ', VLAPEAK(ISCN), 
     2          ' found on scan ', ISCN 
            CALL WLOG( 1, MSGTXT ) 
            CALL WLOG( 1, 
     1         '          It must be DETERMINE, ADJUST, APPLY, or OFF' )
            CALL ERRLOG( ' Fix VLAPEAK ' )
         END IF
      END DO
C
C     Detect if any VLA reference pointing INTENTS were given by the
C     observer.  If so, set the pointers.  These would be INTENTS that
C     either have no station qualifier (no ":") or are for the VLA.
C
      IIDE = 0
      IIAP = 0
      IIAD = 0
      IIOF = 0
      IF( NINTENT .GE. 1 ) THEN
         DO II = 1, NINTENT
            IF( INDEX( INTENT(II), ':' ) .EQ. 0  .OR. 
     1          INDEX( INTENT(II), 'VLA' ) .NE. 0 ) THEN
               IF( INDEX( INTENT(II), 'REFERENCE_POINTING_DETERMINE' ) 
     1              .NE. 0 ) IIDE = II
               IF( INDEX( INTENT(II), 'REFERENCE_POINTING_APPLY' ) 
     1              .NE. 0 ) IIAP = II
               IF( INDEX( INTENT(II), 'REFERENCE_POINTING_ADJUST' ) 
     1              .NE. 0 ) IIAD = II
               IF( INDEX( INTENT(II), 'REFERENCE_POINTING_OFF' ) 
     1              .NE. 0 ) IIOF = II
            END IF
         END DO
      END IF
      GOTPTINT = IIDE .NE. 0 .OR. IIAP .NE. 0 .OR. IIAD .NE. 0 .OR.
     1           IIOF .NE. 0
C        
C     Don't allow both INTENTS and VLAPEAKs.
C        
      IF( VLADOPK .AND. GOTPTINT ) THEN
         CALL WLOG( 1, 'VLASCNS:  Both VLA reference pointing '//
     1       'INTENTS and VLAPEAK are used in the same schedule.' )
         CALL WLOG( 1, '          This confuses SCHED so do not '//
     1       'do it.  VLAPEAK is preferred.' )
         CALL ERRLOG( 
     1      '           Use only VLAPEAK -or- pointing INTENTS ' )
      END IF
C
C     If they are using INTENTS, let them hang themselves.  Put if, as
C     recommended, they use VLAPEAK, set up the appropriate intents.
C
      IF( VLADOPK ) THEN
         IF( NINTENT + 4 .GT. MINTENT ) CALL ERRLOG( 
     1      'VLASCNS: Too many intents for SCHED arrays after adding '//
     2      'VLA pointing commands.' )
         IIDE = NINTENT + 1
         INTENT(NINTENT+1) = 'VLA:REFERENCE_POINTING_DETERMINE'
         IIAP = NINTENT + 2
         INTENT(NINTENT+2) = 'VLA:REFERENCE_POINTING_APPLY'
         IIAD = NINTENT + 3
         INTENT(NINTENT+3) = 'VLA:REFERENCE_POINTING_ADJUST'
         IIOF = NINTENT + 4
         INTENT(NINTENT+4) = 'VLA:REFERENCE_POINTING_OFF'
         NINTENT = NINTENT + 4
C         
C        Loop over scans and set the INTENTS as required.
C         
         DO ISCN = SCAN1, SCANL
C         
C           Only worry about the scan if the VLA is in it.
C           Note that SCHOPT has not yet been called, so the current
C           scans may not be the final ones and the VLA might be
C           dropped from the list later.  But set the phasing intents now.
C           They might show up later on non-VLA scans, but that is not
C           really a problem.
C         
            IF( STASCN(ISCN,VLASTA) ) THEN
C         
C              Set the INTENT according to VLAPEAK.
C         
               NSCINT(ISCN) = NSCINT(ISCN) + 1
               IF( VLAPEAK(ISCN) .EQ. 'DETERMINE' ) THEN
                  ISCINT(NSCINT(ISCN),ISCN) = IIDE
               ELSE IF( VLAPEAK(ISCN) .EQ. 'APPLY' ) THEN
                  ISCINT(NSCINT(ISCN),ISCN) = IIAP
               ELSE IF( VLAPEAK(ISCN) .EQ. 'ADJUST' ) THEN
                  ISCINT(NSCINT(ISCN),ISCN) = IIAD
               ELSE IF( VLAPEAK(ISCN) .EQ. 'OFF' ) THEN
                  ISCINT(NSCINT(ISCN),ISCN) = IIOF
               ELSE
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, A, A, I5 )' )
     1               'VLASCNS: Invalid VLAPEAK=', VLAPEAK(ISCN),
     2               ' on scan ', ISCN
                  CALL WLOG( 1, MSGTXT )
                  CALL WLOG( 1, '         VLAPEAK must be '//
     1               'OFF, DETERMINE, ADJUST, or APPLY' )
                  CALL ERRLOG( '         Correct VLAPEAK ' )
               END IF
C         
            END IF
C         
         END DO
      END IF
C
C   
      RETURN
      END
