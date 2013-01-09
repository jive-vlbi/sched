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
      LOGICAL   GOTINT, VLASCN

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
C     Get the intent numbers for the VLA AUTOPHASE commands if any
C     were issued by the user.
C     Note that the intent for single dish is not actually
C     supported yet.
C     Allow for the case of some other interferometer being told
C     to autophase by only paying attention to intents specific
C     for the VLA (start with VLA:).  Someday this may be a problem
C     if one wishes to use the generic version of the command for
C     more than one interferometer.
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
C
C     Add the phasing intents to the list if they are not there yet.
C     The user must provide the phasing status, so if the intents were
C     not set explicitly, they will need to be added based on VLAMODE.
C     So go ahead and add the VLA specific forms to the INTENTS list.
C
      IF( IIVA .EQ. 0 ) THEN
         NINTENT = NINTENT + 1
         IF( NINTENT .GT. MINTENT ) CALL ERRLOG( 
     1      'VLASCNS: Too many intents when adding phasing commands.' )
         IIVA = NINTENT
         INTENT(NINTENT) = 'VLA:AUTOPHASE_DETERMINE'
      END IF
      IF( IIVX .EQ. 0 ) THEN
         NINTENT = NINTENT + 1
         IF( NINTENT .GT. MINTENT ) CALL ERRLOG( 
     1      'VLASCNS: Too many intents when adding phasing commands.' )
         IIVX = NINTENT
         INTENT(NINTENT) = 'VLA:AUTOPHASE_APPLY'
      END IF
      IF( IIVO .EQ. 0 ) THEN
         NINTENT = NINTENT + 1
         IF( NINTENT .GT. MINTENT ) CALL ERRLOG( 
     1      'VLASCNS: Too many intents when adding phasing commands.' )
         IIVO = NINTENT
         INTENT(NINTENT) = 'VLA:AUTOPHASE_OFF'
      END IF
      IF( IIVS .EQ. 0 ) THEN
         NINTENT = NINTENT + 1
         IF( NINTENT .GT. MINTENT ) CALL ERRLOG( 
     1      'VLASCNS: Too many intents when adding phasing commands.' )
         IIVS = NINTENT
         INTENT(NINTENT) = 'VLA:SINGLE_DISH'
      END IF
C
C     Loop over scans to set the required phasing intents if they are
C     to be based on VLAMODE.
C
      DO ISCN = SCAN1, SCANL
C
C        Only worry about the scan if the VLA is in it.
C        Note that SCHOPT has not yet been called, so the current
C        scans may not be the final ones and the VLA might be
C        dropped from the list later.  But set the phasing intents now.
C        They might show up later on non-VLA scans, but that is not
C        really a problem.
C
         IF( STASCN(ISCN,VLASTA) ) THEN
C
C           Check if the intent is already set.
C
            GOTINT = .FALSE.
            DO II = 1, NSCINT(ISCN)
               IF( ISCINT(II,ISCN) .EQ. IIVA .OR. 
     1             ISCINT(II,ISCN) .EQ. IIVX .OR. 
     2             ISCINT(II,ISCN) .EQ. IIVS ) THEN
                  GOTINT = .TRUE.
               END IF
            END DO
C
C           If VLAMODE was set and the corresponding INTENT was not,
C           set the INTENT.  Remove the old VB, VR, and VL options.
C           Require that a VLA phasing mode of some sort is provided.
C
            IF( .NOT. GOTINT ) THEN
C
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
