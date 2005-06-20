      SUBROUTINE VLBACHG( ISCN, ISTA, FIRSTS, DOTAPE, POSTPASS,
     1              LASTDY, LSTOP, TPCDRIV, PPOK )
C
C     Routine for SCHED, called by VLBA, that deals with tape changes.
C     On the first scan, it will just be sure that any unused drives are
C     stopped.  On tape changes, it may UNLOAD or POSTPASS a drive.
C     Note that this routine will not be called if VLBITP is not .TRUE.
C     and AUTOALOC is not .FALSE.
C
C     At a tape change at a single drive station, the drive in use 
C     will be unloaded or postpassed.
C
C     PPOK indicates if an UNLOAD without POSTPASS is safe if the 
C     schedule went according to plan.
C
C     For stations with multiple drives, at a tape change, the drive
C     with the full tape will be stopped.  If there is time, an
C     unload or postpass will be initiated.  If there is inadequate
C     time, a request will be made to routine VLBA to issue the 
C     appropriate command at the beginning of the next scan (usually
C     the main scan after a short setup scan).
C
C     Whether or not to postpass is something under discussion an
C     the opinion changes occasionally.  Therefore variable POSTPASS
C     is set in VLBA according to the current wisdom.  This may get
C     more complicated later if we decide to do something different
C     with single drive stations.
C
      INCLUDE    'sched.inc'
C
      INTEGER           MTP, ISCN, ISTA
      PARAMETER         (MTP=2)
      LOGICAL           FIRSTS, POSTPASS
      INTEGER           LASTDY, KSTA, NDR, DOY, TPCDRIV, LTPCDRIV
      INTEGER           LEN1
      CHARACTER         HUMAND*16, VLBAD*9, TFORM*15, TSTOP*9
      CHARACTER         ACTION(MTP)*8
      LOGICAL           DOTAPE, GOTONE, PPOK
      DOUBLE PRECISION  FRACD, LSTOP
      SAVE              LTPCDRIV
C ---------------------------------------------------------------------
      IF( DEBUG .AND. ISCN .LE. 3 ) CALL WLOG( 0, 'VLBACHG: Starting.' )
C
C     Get the useful shortcut.
C
      KSTA = STANUM(ISTA)
C
C     First deal with turning off the unused drives on the first scan.
C
      IF( FIRSTS .AND. VLBITP ) THEN
         DO NDR = 1, STNDRIV(KSTA)
            IF( NDR .NE. TPCDRIV .AND. .NOT. 
     1        ( TWOHEAD .AND. NDR - 1 .EQ. TPCDRIV ) ) THEN
               WRITE( IUVBA, '( A, I1, A, I1, A )' )
     1           'tape=(', NDR, ',STOP)    write=(', NDR, ',off) '
            END IF
         END DO
C
C        Keep track of the drive in in use.
C
         LTPCDRIV = TPCDRIV
C
C     Now deal with tape changes.  Note that a "change tape" message
C     went out from routine VLBA.
C     
      ELSE IF( DOTAPE ) THEN
C
C        Decide what to do with each drive.  Note that postpasses are
C        not needed for thick tape or when the last pass was continuous.
C        LTPCDRIV is the last drive that was in use (the program 
C        structure allows for this to be converted to an array.
C
         GOTONE = .FALSE.
         DO NDR = 1, STNDRIV(KSTA)
            IF( NDR .EQ. LTPCDRIV ) THEN
C
C              Unload thick tape.
C
               IF( TPLENG(ISTA) .LT. 12000 ) THEN
                  ACTION(NDR) = 'UNLOAD'
C
C              Decide whether to unload or postpass thin tape.
C
               ELSE
C
C                 Separate this case for variation depending
C                 on whether the previous pass was scheduled to be 
C                 continuous.  Warn when doing an avoidable postpass.
C
                  IF( PPOK .OR. .NOT. POSTPASS ) THEN
                     ACTION(NDR) = 'UNLOAD'
                  ELSE
                     ACTION(NDR) = 'POSTPASS'
C
C                    Warn the user that he/she could save time.
C
                     MSGTXT = ' '
                     IF( STNDRIV(KSTA) .EQ. 1 ) THEN
                        WRITE( MSGTXT, '( 3A )' ) 'VLBACHG: **** ',
     1                      'Doing postpass at single drive site: ',
     2                      STATION(KSTA)
                     ELSE IF( TWOHEAD ) THEN
                        WRITE( MSGTXT, '( 3A )' ) 'VLBACHG: **** ',
     1                      'Doing postpass during dual tape expt.: ',
     2                   STATION(KSTA)
                     END IF
                     IF( MSGTXT .NE. ' ' ) THEN
                        CALL WLOG( 1, MSGTXT )
                        MSGTXT = ' '
                        WRITE( MSGTXT, '( 3A )' ) '         **** ',
     1                   'Takes up to 22 minutes in addition to tape ',
     2                   'change time. '
                        CALL WLOG( 1, MSGTXT )
                        MSGTXT = ' '
                        WRITE( MSGTXT, '( 3A )' ) '         **** ',
     1                   'Can be avoided by not ',
     2                   'stopping tape in preceeding pass.'
                        CALL WLOG( 1, MSGTXT )
                     END IF
                  END IF
               END IF
               GOTONE = .TRUE.
C
            ELSE
C
C              This tape is not one that was just filled up.
C              Note that, in TWOHEAD observations, something might
C              actually be done.
C
               ACTION(NDR) = 'SKIP'
C
            END IF
         END DO
C
C        Complain if didn't find the change.  This should not happen.
C
         IF( .NOT. GOTONE ) CALL ERRLOG( 'VLBACHG: Programming '//
     1       'problem with tape change.' )
C
C        Now loop through the drives stopping the tape.
C
         DO NDR = 1, STNDRIV(KSTA)
            IF( ACTION(NDR) .NE. 'SKIP' ) THEN
C
C              Warn operator of action being taken.
C
               WRITE(IUVBA,'( 3A, I1, A )') ' !* ======== ',
     1             ACTION(NDR)(1:LEN1(ACTION(NDR))), 'ING DRIVE ', NDR, 
     2             ' ========= *! '
               IF( TWOHEAD .AND. NDR + 1 .LE. STNDRIV(KSTA) ) THEN
                  WRITE(IUVBA,'( 3A, I1, A )') ' !* ======== ',
     1               ACTION(NDR)(1:LEN1(ACTION(NDR))), 'ING DRIVE ', 
     2               NDR + 1, ' ========= *! '
               END IF
C
C              Now stop the tape.  The scan stop time was set above.
C           
               WRITE( IUVBA, '( A, I1, A, I1, A )' )
     1              'tape=(', NDR, ',STOP)    write=(', NDR, 
     2              ',off) '
               IF( TWOHEAD .AND. NDR + 1 .LE. STNDRIV(KSTA) ) THEN
                  WRITE( IUVBA, '( A, I1, A, I1, A )' )
     1                 'tape=(', NDR + 1, ',STOP)    write=(', NDR + 1, 
     2                 ',off) '
               END IF
            END IF
         END DO
C
C        Finish the tape stopping scan.
C        A stop time will be needed.
C        Use the last stop time plus two seconds (could
C        be one second, but two makes is very unlikely that roundoff
C        will give two identical stop times.
C
         LSTOP = LSTOP + 2.D0 / 86400.D0
         CALL SCHDAY( LSTOP, VLBAD, HUMAND, DOY, FRACD )
         TSTOP  = TFORM( FRACD*TWOPI, 'T', 0, 2, 2, 'hms' )
         IF( DOY .NE. LASTDY ) THEN
            WRITE( IUVBA, '(''  date = '', A9 )' ) VLBAD
            LASTDY = DOY
         END IF
         WRITE( IUVBA, '( 3A )' ) 'dur=0  stop=', TSTOP, '  !NEXT! '
C
C        Now POSTPASS or UNLOAD.  This will be part of the next
C        scan created by VLBA - either the setup or main scan.
C        NOTE:  tape=(1,POSTPASS) is a "command".  tape=(1,STOP) is
C        a condition.  Both can be issued legitimately within one
C        scan!  This is according to Barry Clark, 16 July 1997.
C        However, 2 POSTPASS commands cannot be issued in one scan.
C        A scan should be at least 2 seconds long to be sure that
C        the POSTPASS is initiated.
C        A POSTPASS currently (16July1997) will be stopped and 
C        recording begun if a scan with tape=(1,RUN) is encountered
C        before the POSTPASS is done.  Barry promised to change this
C        to force a POSTPASS to go to completion.
C
         DO NDR = 1, STNDRIV(KSTA)
            IF( ACTION(NDR) .NE. 'SKIP' ) THEN
               WRITE( IUVBA, '( A, I1, 3A )' ) 'tape=(', NDR, ',', 
     1                ACTION(NDR)(1:LEN1(ACTION(NDR))), ')'
               IF( TWOHEAD .AND. NDR + 1 .LE. STNDRIV(KSTA) ) THEN
                  LSTOP = LSTOP + 2.D0 / 86400.D0
                  CALL SCHDAY( LSTOP, VLBAD, HUMAND, DOY, FRACD )
                  TSTOP  = TFORM( FRACD*TWOPI, 'T', 0, 2, 2, 'hms' )
                  IF( DOY .NE. LASTDY ) THEN
                     WRITE( IUVBA, '(''  date = '', A9 )' ) VLBAD
                     LASTDY = DOY
                  END IF
                  WRITE( IUVBA, '( 3A )' ) 'dur=0  stop=', TSTOP, 
     1                 '  !NEXT! '
                  WRITE( IUVBA, '( A, I1, 3A )' ) 'tape=(', NDR + 1, 
     1                ',', ACTION(NDR)(1:LEN1(ACTION(NDR))), ')'
               END IF
            END IF
         END DO
C
C        Maintain the history of what drives were used.
C
         LTPCDRIV = TPCDRIV
C
      END IF
C
      RETURN
      END
