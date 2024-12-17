      SUBROUTINE SBPAIR
C
C     This routine for SCHED, called by GETSET, determines if any
C     channels are from the same BBC at any station in a setup
C     file.  If so, it sets up arrays that will be used to cause
C     Doppler calculations to set the channels to the same LO
C     frequency.  Also it will balk if any station is being asked
C     in the setup file to set different frequencies into the same 
C     BBC.  The situations of concern arise when upper-lower 
C     sideband pairs are being used.
C
C     The Doppler calculations will not be allowed if channels
C     don't match between stations.  Exact matches are not required,
C     but each channel must correspond to one of the "logical"
C     channels for the setup file as determined by SFINFO.  Some
C     of the tests here are skipped if matches don't exist (OKXC 
C     false).
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER  KS, ICHN, KCHN, LEN1, ISETF, ILC, KLC
      LOGICAL  ERRS, WARN
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SBPAIR: starting' )
C
      ERRS = .FALSE.
      WARN = .FALSE.
      IF( .NOT. VLAONLY ) THEN
C
C        Initialize.  These are the "logical channels".
C
         DO ISETF = 1, NSETF
            DO ICHN = 1, MAXCHN
               SAMEBBC(ICHN,ISETF) = 0
            END DO
         END DO
C
C        Now loop through the setup groups.
C
         DO KS = 1, NSET
            ISETF = ISETNUM(KS)
            IF( NCHAN(KS) .GT. 1 ) THEN
C
C              Look at pairs of channels.
C
               DO ICHN = 2, NCHAN(KS)
                  DO KCHN = 1, ICHN - 1
C
C                    Select those that use the same BBC.
C
                     IF( BBC(ICHN,KS) .EQ. BBC(KCHN,KS) .AND. 
     1                   DAR(ISETSTA(KS)) .NE. 'NONE' ) THEN
C
C                       Balk at setting them to different frequencies.
C                       (LBA is different)
C
                        IF( BBSYN(ICHN,KS) .NE. BBSYN(KCHN,KS) .AND.
     1                      DAR(ISETSTA(KS)) .NE. 'LBA' ) THEN
                           WRITE( MSGTXT, '( A, I3, A, A, 2I3 )' )
     1                         'SBPAIR: Attempting to set BBC ',
     2                         BBC(ICHN,KS), ' to two frequencies,',
     3                         ' channels: ', ICHN, KCHN
                           CALL WLOG( 1, MSGTXT )
                           ERRS = .TRUE.
C                           
C                         For LBA just make sure they are selected from the
C                         same 64 MHz band (there are more constraints,
C                         but for now...)
C
                        ELSE IF( ABS(BBSYN(ICHN,KS) - BBSYN(KCHN,KS)) 
     1                           .GT. 64 .AND.
     2                           DAR(ISETSTA(KS)) .EQ. 'LBA' ) THEN
                           WRITE( MSGTXT, '( A, I3, A, A, 2I3 )' )
     1                         'SBPAIR: Attempting to set LBA BBC ',
     2                         BBC(ICHN,KS), 
     3                      ' to frequencies more than 64 MHz apart,',
     4                         ' channels: ', ICHN, KCHN
                           CALL WLOG( 1, MSGTXT )
                           ERRS = .TRUE.
                        END IF
C
C                       Cross check that corresponding logical
C                       channels share BBCs at different
C                       stations.
C
                        IF( OKXC(ISETF) ) THEN 
                           ILC = SFCHAN(ICHN,KS)
                           KLC = SFCHAN(KCHN,KS)
                           IF( SAMEBBC(ILC,ISETF) .NE. 0 .AND.
     1                         SAMEBBC(ILC,ISETF) .NE. KLC ) THEN
                              MSGTXT = ' '
                              WRITE( MSGTXT, '( 3A )' )
     1                         'SBPAIR: Assignment of BBCs to pairs',
     2                         ' of channels don''t seem consistent',
     3                         ' between stations.'
                              CALL WLOG( 1, MSGTXT )
                              MSGTXT = ' '
                              WRITE( MSGTXT, '( A, A, A, I3, A, I3 )' )
     1                          '    For ', SETSTA(1,KS), ', Channel',
     2                            ICHN, ' paired with channel ', KCHN
                              CALL WLOG( 1, MSGTXT )
                              MSGTXT = ' '
                              WRITE( MSGTXT, '( A, I3, A, I3 )' )
     1                          '    These are logical channels: ',
     2                          ILC, ' and ', KLC
                              CALL WLOG( 1, MSGTXT )
                              MSGTXT = ' '
                              WRITE( MSGTXT, '( A, I3, A, I3 )' )
     1                         '     For an earlier station, ', ILC, 
     2                         ' was paired with logical channel ', 
     3                         SAMEBBC(ILC,ISETF)
                              WARN = .TRUE.
                           END IF
                           SAMEBBC(ILC,ISETF) = KLC
                        END IF
                     END IF
                  END DO
               END DO
            END IF
C
C           Issue the warning.
C
            IF( ERRS .OR. WARN ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, A )' ) 
     1            'SBPAIR: The problem occured in ',
     2            SETNAME(KS)(1:LEN1(SETNAME(KS)))
               CALL WLOG( 1, MSGTXT )
               IF( ERRS ) CALL ERRLOG( 
     1            'SBPAIR: Fix the problem and try again' )
            END IF
C
         END DO
C
      END IF
C
C
      RETURN
      END
