      SUBROUTINE PHINT
C
C     Routine for SCHED called by chkscn that deals with INTENTs for
C     reference pointing for stations other than the VLA (the VLA is
C     handled in VLASCNS).
C
C     The main indicator that peaking has been requested is that the
C     PEAK command was set which is in the variable DOPEAK.  That
C     command is not station specific, so for now don't try to be 
C     station specific.  
C
      INCLUDE  'sched.inc'
C
      INTEGER         ISCN, ISTA, II, IIDE, IIAP, IIAD, IIOF
      LOGICAL         GOTPTINT, GOTDOPK, GOTPK1, JUSTVLA
C  ----------------------------------------------------------------------
C
C     First see if the user has added their own peaking INTENTS.  If
C     so, don't try to do more.
C
      IIDE = 0
      IIAP = 0
      IIAD = 0
      IIOF = 0
      IF( NINTENT .GE. 1 ) THEN
         DO II = 1, NINTENT
            IF( INDEX( INTENT(II), ':' ) .EQ. 0  .OR. 
     1          INDEX( INTENT(II), 'VLA:' ) .EQ. 0 ) THEN
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
C     Now process scans if we don't already have INTENTS.
C
      IF( .NOT. GOTPTINT ) THEN
C
C        First see if DOPEAK was ever set for a non-VLA station.  If 
C        not, don't do anyting.
C
         GOTDOPK = .FALSE.
         DO ISCN = SCAN1, SCANL
            IF( DOPEAK(ISCN) .GT. 0 ) THEN
               DO ISTA = 1, NSTA
                  IF( STASCN(ISCN,ISTA) ) THEN
                     IF( INDEX( STANAME(ISTA), 'VLA' ) .EQ. 0 ) THEN
                        GOTDOPK = .TRUE.
                        GO TO 100
                     END IF
                  END IF
               END DO
             END IF
          END DO
  100     CONTINUE
C
C         Now, if the INTENTS weren't set by the user and peaking is 
C         used, set them.
C
          IF( GOTDOPK ) THEN
C
C            First add the intents to the list.
C            I don't think the available inputs allow the use of ADJUST.
C
             IF( NINTENT + 3 .GT. MINTENT ) THEN
                CALL WLOG( 1, 'PHINT:  Too many INTENTS when adding '//
     1              'ones for non-VLA reference pointing.' )
                CALL ERRLOG( 
     1               'Report SCHED problem and use fewer INTENTS' )
             ENDIF
C
             INTENT(NINTENT+1) = 'REFERENCE_POINTING_DETERMINE'
             INTENT(NINTENT+2) = 'REFERENCE_POINTING_APPLY'
             INTENT(NINTENT+3) = 'REFERENCE_POINTING_OFF'
             IIDE = NINTENT + 1
             IIAP = NINTENT + 2
             IIOF = NINTENT + 3
             NINTENT = NINTENT + 3
C
C            Now go through the scans.  For now, issue OFF until the first
C            DOPEAK ge 1 is found.  Then do DETERMINE and APPLY.  The user
C            doesn't really have control of when the pointing is really 
C            applied so don't worry about that.  If there is a long slew,
C            or a long time, the on-line system or operators may stop 
C            applying on its own.
C
C            Don't do anything on a scan that only includes the VLA 
C            (probably a phasing or VLA pointing scan).
C
             GOTPK1 = .FALSE.
             DO ISCN = SCAN1, SCANL
C
                JUSTVLA = .TRUE.
                DO ISTA = 1, NSTA
                  IF( STASCN(ISCN,ISTA) .AND. 
     1                INDEX( STANAME(ISTA), 'VLA' ) .EQ. 0 ) THEN
                     JUSTVLA = .FALSE.
                     GO TO 200
                  END IF
                END DO
  200           CONTINUE
                IF( .NOT. JUSTVLA ) THEN
                   NSCINT(ISCN) = NSCINT(ISCN) + 1
                   IF( DOPEAK(ISCN) .GE. 1 ) THEN
                      ISCINT(NSCINT(ISCN),ISCN) = IIDE
                      GOTPK1 = .TRUE.
                   ELSE
                      IF( GOTPK1 ) THEN
                         ISCINT(NSCINT(ISCN),ISCN) = IIAP
                      ELSE
                         ISCINT(NSCINT(ISCN),ISCN) = IIOF
                      END IF
                   END IF
                END IF
             END DO
          END IF
      END IF
C
      RETURN
      END
      
