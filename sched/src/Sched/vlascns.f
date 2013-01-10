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
      INTEGER   IIDE, IIAP, IIAD, IIOF
      LOGICAL   GOTINT, GOTPTINT, VLASCN, GOTVLAMD

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
     1              IIVA = II
               IF( INDEX( INTENT(II), 'AUTOPHASE_APPLY' ) .NE. 0 ) 
     1              IIVX = II
               IF( INDEX( INTENT(II), 'AUTOPHASE_OFF' ) .NE. 0 ) 
     1              IIVO = II
               IF( INDEX( INTENT(II), 'SINGLE_DISH' ) .NE. 0 ) 
     1              IIVS = II
C
            END IF
         END DO
      END IF
      GOTPTINT =  IIVA .GT. 0 .OR. IIVX .GT. 0 .OR. 
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
      IF( GOTPTINT .AND. GOTVLAMD ) THEN
         CALL WLOG( 1, 'VLASCNS:  Both VLA phasing INTENTS and '//
     1       'VLAMODE used in the same schedule.' )
         CALL WLOG( 1, '          This confuses SCHED so do not '//
     1       'do it.'
         CALL ERRLOG( ' Use only VLAMODE -or- pointing INTENTS ' )
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
     1             ISCINT(NSCINT(ISCN),ISCN) = IIVO
               ELSE IF( VLAMODE(ISCN) .EQ. 'VA' )
     1             ISCINT(NSCINT(ISCN),ISCN) = IIVA
               ELSE IF( VLAMODE(ISCN) .EQ. 'VX' ) THEN
     1             ISCINT(NSCINT(ISCN),ISCN) = IIVX
               ELSE IF( VLAMODE(ISCN) .EQ. 'VS' ) THEN
     1             ISCINT(NSCINT(ISCN),ISCN) = IIVS
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
         END IF
C
      END DO
C
C     -----------------
C

  *****************  still need to do the mods to this section where I
don't allow VLAPEAK and explicit INTENTS in the same schedule.



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
C     Now add the pointing intents if they are needed.
C
      IF( VLADOPK ) THEN
C
C        Get the intent numbers for the VLA AUTOPHASE commands.
C        Note that the intent for single dish is not actually
C        defined yet.
C        Allow for the case of some other interferometer being told
C        to autophase.  Ignore if so.  That is done with a station
C        name in the intent.
C         
         IIDE = 0
         IIAP = 0
         IIAD = 0
         IIOF = 0
         IF( NINTENT .GE. 1 ) THEN
            DO II = 1, NINTENT
               IF( INDEX( INTENT(II), ':' ) .EQ. 0  .OR. 
     1             INDEX( INTENT(II), 'VLA' ) .NE. 0 ) THEN
                  IF( INDEX( INTENT(II), 'REFERENCE_POINTING_DETERMINE' ) 
     1                 .NE. 0 ) IIDE = II
                  IF( INDEX( INTENT(II), 'REFERENCE_POINTING_APPLY' ) 
     1                 .NE. 0 ) IIAP = II
                  IF( INDEX( INTENT(II), 'REFERENCE_POINTING_ADJUST' ) 
     1                 .NE. 0 ) IIAD = II
                  IF( INDEX( INTENT(II), 'REFERENCE_POINTING_OFF' ) 
     1                 .NE. 0 ) IIOF = II
               END IF
            END DO
         END IF
C        
C        Add the phasing intents to the list if they are not there yet.
C        
         IF( IIDE .EQ. 0 ) THEN
            NINTENT = NINTENT + 1
            IF( NINTENT .GT. MINTENT ) CALL ERRLOG( 
     1         'VLASCNS: Too many intents when adding reference '//
     2         'pointing commands.' )
            IIDE = NINTENT
            INTENT(NINTENT) = 'VLA:REFERENCE_POINTING_DETERMINE'
         END IF
         IF( IIAP .EQ. 0 ) THEN
            NINTENT = NINTENT + 1
            IF( NINTENT .GT. MINTENT ) CALL ERRLOG( 
     1         'VLASCNS: Too many intents when adding reference '//
     2         'pointing commands.' )
            IIAP = NINTENT
            INTENT(NINTENT) = 'VLA:REFERENCE_POINTING_APPLY'
         END IF
         IF( IIAD .EQ. 0 ) THEN
            NINTENT = NINTENT + 1
            IF( NINTENT .GT. MINTENT ) CALL ERRLOG( 
     1         'VLASCNS: Too many intents when adding reference '//
     2         'pointing commands.' )
            IIAD = NINTENT
            INTENT(NINTENT) = 'VLA:REFERENCE_POINTING_ADJUST'
         END IF
         IF( IIOF .EQ. 0 ) THEN
            NINTENT = NINTENT + 1
            IF( NINTENT .GT. MINTENT ) CALL ERRLOG( 
     1         'VLASCNS: Too many intents when adding reference '//
     2         'pointing commands.' )
            IIOF = NINTENT
            INTENT(NINTENT) = 'VLA:REFERENCE_POINTING_OFF'
         END IF
C         
C        Loop over scans to make the check.
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
C              Check if the intent is already set.
C         
               GOTINT = .FALSE.
               DO II = 1, NSCINT(ISCN)
                  IF( ISCINT(II,ISCN) .EQ. IIDE .OR. 
     1                ISCINT(II,ISCN) .EQ. IIAP .OR. 
     2                ISCINT(II,ISCN) .EQ. IIAD .OR. 
     3                ISCINT(II,ISCN) .EQ. IIOF ) THEN
                     GOTINT = .TRUE.
                  END IF
               END DO
C         
C              If VLAMODE was set and the corresponding INTENT was not,
C              set the INTENT.  Remove the old VB, VR, and VL options.
C         
               IF( .NOT. GOTINT ) THEN
                  NSCINT(ISCN) = NSCINT(ISCN) + 1
                  IF( VLAPEAK(ISCN) .EQ. 'DETERMINE' ) THEN
                     ISCINT(NSCINT(ISCN),ISCN) = IIDE
                     GOTINT = .TRUE.
                  ELSE IF( VLAPEAK(ISCN) .EQ. 'APPLY' ) THEN
                     NSCINT(ISCN) = NSCINT(ISCN) + 1
                     ISCINT(NSCINT(ISCN),ISCN) = IIAP
                     GOTINT = .TRUE.
                  ELSE IF( VLAPEAK(ISCN) .EQ. 'ADJUST' ) THEN
                     NSCINT(ISCN) = NSCINT(ISCN) + 1
                     ISCINT(NSCINT(ISCN),ISCN) = IIAD
                     GOTINT = .TRUE.
                  ELSE IF( VLAPEAK(ISCN) .EQ. 'OFF' ) THEN
                     NSCINT(ISCN) = NSCINT(ISCN) + 1
                     ISCINT(NSCINT(ISCN),ISCN) = IIOF
                     GOTINT = .TRUE.
                  ELSE
                     MSGTXT = ' '
                     WRITE( MSGTXT, '( A, A, A, I5 )' )
     1                  'VLASCNS: Invalid VLAPEAK=', VLAPEAK(ISCN),
     2                  ' on scan ', ISCN
                     CALL WLOG( 1, MSGTXT )
                     CALL WLOG( 1, '         VLAPEAK must be '//
     1                  'OFF, DETERMINE, ADJUST, or APPLY'
                     CALL ERRLOG( '         Correct VLAPEAK ' )
                  END IF
  trap for next compile to pay attention to this routine.


               END IF
C         
            END IF
C         
         END DO
      END IF
C
C   
      END DO
      RETURN
      END
