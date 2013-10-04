      SUBROUTINE STAFILES
C
C     Subroutine for SCHED, called by the main routine, that loops
C     through the stations creating the individual station files.
C     Global files such as the VEX and VSOP (DRUDG) output are 
C     handled separately. This routine creates the crd, sch, and 
C     obs files.
C
C     For each station, keep track of the setups used and then 
C     write them at the end of the operator schedule file.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER      ISTA, ISCN, SETUSED(MSET), NSUSED, I, KS
      LOGICAL      FIRSTS, OVBA, OLOC, GOT
C  --------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'STAFILES: Station loop starting.' )
      DO ISTA = 1, NSTA
C
C        Open the various files with antenna specific info.
C
         CALL FILEOPEN( ISTA, OVBA, OLOC )
C
C        Initialize bookkeeping of setup files.
C
         NSUSED = 0
C
C        Now loop through the scans.
C
         FIRSTS = .TRUE.
         DO  ISCN = SCAN1, SCANL
            IF( STASCN(ISCN,ISTA) ) THEN
               IF( DEBUG .AND. ( ISCN .LE. 3 .OR. ISCN .GE. SCANL - 2 )
     1             ) THEN
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, I5 )' ) 
     1                'STAFILES: Starting loop for scan ', ISCN
                  CALL WLOG( 0, MSGTXT )
               END IF
C
C              Get the setup group number for this scan/station
C              and record whether it was used.
C
               KS = NSETUP( ISCN, ISTA )
               GOT = .FALSE.
               IF( NSUSED .NE. 0 ) THEN
                  DO I = 1, NSUSED
                     IF( SETUSED(I) .EQ. KS ) GOT = .TRUE.
                  END DO
               END IF
               IF( .NOT. GOT ) THEN
                  NSUSED = NSUSED + 1
                  SETUSED(NSUSED) = KS
               END IF
C
C              Write operator schedule.
C
               CALL PRTSCH( ISCN, ISTA, FIRSTS )
C
C              Write out telescope control files.
C
               CALL CRDWRT( ISCN, ISTA, FIRSTS )
C
               FIRSTS = .FALSE.
            END IF
         END DO
C
C        IF( DEBUG ) CALL WLOG( 0, 'STAFILES: Done with scan loop.' )
C
C        Put final lines in telescope control file and CLOSE it.
C        Last call is flagged by scan number -999.
C
         CALL CRDWRT( -999, ISTA, .FALSE. )
         IF( OVBA ) CLOSE( UNIT=IUVBA )
         IF( OLOC ) CLOSE( UNIT=IULOC )
         IF( DEBUG ) CALL WLOG( 0, 'STAFILES: Files closed.' )
C
C        Put setup information in the operator schedule.
C
         WRITE( IPRT, '( 1X, /, 1X, /, A, /, 2A )' ) 
     1       'SETUP FILE INFORMATION:',
     2       '   NOTE: If DOPPLER, FREQ, or BW were used, see the ',
     3       'individual scans for the final BBC settings.'
C
         DO I = 1, NSUSED
            CALL PRTSET( SETUSED(I), IPRT )
         END DO
C
C        Put source list in operator schedule and close it.
C
         CALL SRCLST( IPRT, 1 )
         CLOSE( UNIT=IPRT )
C
         IF( DEBUG ) CALL WLOG( 0, 'STAFILES: Done with station.' )
C
      END DO   ! Station loop
C
      IF( DEBUG ) CALL WLOG( 0, 'STAFILES: Ending.' )
      RETURN
      END
