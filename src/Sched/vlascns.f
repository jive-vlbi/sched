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
      INTEGER   IIVA, IIVX, IIVS, II
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
      IF( VLASTA .EQ. 0 ) RETURN
C
C     Get the intent numbers for the VLA AUTOPHASE commands.
C     Note that the intent for single dish is not actually
C     defined yet.
C     Allow for the case of some other interferometer being told
C     to autophase.  Ignore if so.  That is done with a station
C     name in the intent.
C
      IIVA = 0
      IIVX = 0
      IIVS = 0
      IF( NINTENT .GE. 1 ) THEN
         DO II = 1, NINTENT
            IF( INDEX( INTENT(II), ':' ) .EQ. 0  .OR. 
     1          INDEX( INTENT(II), 'VLA' ) .NE. 0 ) THEN
               IF( INDEX( INTENT(II), 'DETERMINE_AUTOPHASE' ) .NE. 0 ) 
     1              IIVA = II
               IF( INDEX( INTENT(II), 'APPLY_AUTOPHASE' ) .NE. 0 ) 
     1              IIVX = II
               IF( INDEX( INTENT(II), 'NO_AUTOPHASE' ) .NE. 0 ) 
     1              IIVS = II
            END IF
         END DO
      END IF
C
C     Add the phasing intents to the list if they are not there yet.
C
      IF( IIVA .EQ. 0 ) THEN
         NINTENT = NINTENT + 1
         IF( NINTENT .GT. MINTENT ) CALL ERRLOG( 
     1      'VLASCNS: Too many intents when adding phasing commands.' )
         IIVA = NINTENT
         INTENT(NINTENT) = 'DETERMINE_AUTOPHASE'
      END IF
      IF( IIVX .EQ. 0 ) THEN
         NINTENT = NINTENT + 1
         IF( NINTENT .GT. MINTENT ) CALL ERRLOG( 
     1      'VLASCNS: Too many intents when adding phasing commands.' )
         IIVX = NINTENT
         INTENT(NINTENT) = 'APPLY_AUTOPHASE'
      END IF
      IF( IIVS .EQ. 0 ) THEN
         NINTENT = NINTENT + 1
         IF( NINTENT .GT. MINTENT ) CALL ERRLOG( 
     1      'VLASCNS: Too many intents when adding phasing commands.' )
         IIVS = NINTENT
         INTENT(NINTENT) = 'NO_AUTOPHASE'
      END IF
C
C     Loop over scans to make the check.
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
C           set the INTENT.
C
            IF( .NOT. GOTINT .AND. (
     1          VLAMODE(ISCN) .EQ. 'VA' .OR.
     2          VLAMODE(ISCN) .EQ. 'VB' .OR.
     3          VLAMODE(ISCN) .EQ. 'VR' .OR.
     4          VLAMODE(ISCN) .EQ. 'VL' ) ) THEN
               NSCINT(ISCN) = NSCINT(ISCN) + 1
               ISCINT(NSCINT(ISCN),ISCN) = IIVA
               GOTINT = .TRUE.
            END IF
            IF( .NOT. GOTINT .AND. VLAMODE(ISCN) .EQ. 'VX' ) THEN
               NSCINT(ISCN) = NSCINT(ISCN) + 1
               ISCINT(NSCINT(ISCN),ISCN) = IIVX
               GOTINT = .TRUE.
            END IF
            IF( .NOT. GOTINT .AND. VLAMODE(ISCN) .EQ. 'VS' ) THEN
               NSCINT(ISCN) = NSCINT(ISCN) + 1
               ISCINT(NSCINT(ISCN),ISCN) = IIVS
               GOTINT = .TRUE.
            END IF
C
C           Now check that the VLA has been put in a valid mode.
C           Note that there used to be many other valid VLAMODES, but
C           we won't allow them for now, except that VB, VR, and VL
C           are treated as equivalent to VA (All channels are phased
C           in the new system.
C
            IF( .NOT. GOTINT ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, I4 )' )
     1            'VLASCNS: No VLA phasing INTENT or VLAMODE given ',
     2            'for input scan: ', ISCN
               CALL ERRLOG( MSGTXT )
            END IF
C
         END IF
C
      END DO
C
      RETURN
      END
