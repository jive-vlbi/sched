      SUBROUTINE TPMFIX
C
C     Deal with experiments that, at this stage, have setups with
C     different TAPEMODEs.  These can get into big trouble with tape
C     management and so we have not allowed this situation.  Here
C     is where we enforce it.  For each station separately, go 
C     through all the setups looking at those with VLBA and MKIV 
C     formats.  Get the minimum TAPEMODE.  Then go back and set 
C     the TAPEMODE for all these setups to the minimum.  I tried to
C     deal with this entirely in TPSCH, but that had trouble with
C     track assignments.  A TAPEMODE=4 setup would only get 2 passes
C     per head position, as expected, but they would use only the
C     even tracks, hence writing over half the tracks used by an
C     earlier TAPEMODE=2 scan.
C
C     Doing this for each station separately allows for different
C     bandwidths at different stations, but significantly 
C     complicates the logic.
C
      INCLUDE   'sched.inc'
      INCLUDE   'schset.inc'
C
      INTEGER    ISET, ISTA, KSTA, ISCN, MINTPM, MAXTPM
      INTEGER    TPM(MAXSTA,MSET), GOTN, LEN1
      LOGICAL    GOTMULTI, CHANGED, ANYCHG, ALERT
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'TPMFIX starting' )
C
C     First go through the setups and see if there is any chance
C     of a problem.  For most observations, this is all that
C     will be needed.  Only check VLBA, MarkIII and MKIV 
C     format setups (skips NONE, along with S2, MKII etc.)
C
      MINTPM = 100
      MAXTPM = 0
      DO ISET = 1, NSET
         IF( FORMAT(ISET)(1:4) .EQ. 'VLBA' .OR. 
     1       FORMAT(ISET)(1:4) .EQ. 'MARK' .OR.
     2       FORMAT(ISET)(1:4) .EQ. 'MKIV' ) THEN
            MINTPM =  MIN( MINTPM, TAPEMODE(ISET) )
            MAXTPM =  MAX( MAXTPM, TAPEMODE(ISET) )
         END IF
      END DO
C
C     If all setups have the same TAPEMODE, we can quit.
C
      IF( MINTPM .NE. MAXTPM .AND. MINTPM .NE. 100 ) THEN
C
C        So now we need to try to set TAPEMODES the same for
C        all setup groups used by a station.  The complication
C        is that some setup groups (eg those that specify VLBA)
C        are used for more than one station.  This makes 
C        getting all the dependencies a mess.  
C
C        Set up a matrix TPM that gives the TAPEMODE for each
C        station/setup group.  We will then manipulate that
C        matrix to get what to change the TAPEMODES to.
C        First initialize the matrix.
C
         DO ISTA = 1, NSTA
            DO ISET = 1, NSET
               TPM(ISTA,ISET) = 0
            END DO
         END DO
C
C        Now set the elements that are actually used to the 
C        starting TAPEMODE.
C
         DO ISTA = 1, NSTA
            KSTA = STANUM(ISTA)
C
C           Only do anything if this station is using a VLBA or MK??
C           data aquisition rack.
C
            IF( DAR(KSTA)(1:4) .EQ. 'VLBA' .OR. 
     1          DAR(KSTA)(1:2) .EQ. 'MK' ) THEN
C
C              Loop over scans finding the used setup groups.
C
               DO ISCN = 1, NSCANS
C
C                 Only consider recording scans that use this station.
C
                  IF( STASCN(ISCN,ISTA) .AND. .NOT. NOREC(ISCN) ) THEN
C
C                    Get the setup group number for this scan/station.
C
                     ISET = NSETUP(ISCN,ISTA)
C
C                    Only worry about it if this is a recording type
C                    format.
C
                     IF( FORMAT(ISET)(1:4) .EQ. 'VLBA' .OR. 
     1                   FORMAT(ISET)(1:4) .EQ. 'MARK' .OR.
     2                   FORMAT(ISET)(1:4) .EQ. 'MKIV' ) THEN
C
C                       Ok, now set TPM
C
                        TPM(ISTA,ISET) = TAPEMODE(ISET)
C
                     END IF
                  END IF
               END DO
            END IF
         END DO
C
C        Now inspect TPM to see if any setups are used for multiple 
C        stations.
C
         GOTMULTI = .FALSE.
         DO ISET = 1, NSET
            GOTN = 0
            DO ISTA = 1, NSTA
               IF( TPM(ISTA,ISET) .GT. 0 ) GOTN = GOTN + 1
            END DO
            GOTMULTI = GOTMULTI .OR. GOTN .GE. 2
         END DO
C
C        For each station, set all the TPM cells to the minimum value.
C
         ANYCHG = .FALSE.
         DO ISTA = 1, NSTA
            MINTPM = 100
            DO ISET = 1, NSET
               IF ( TPM(ISTA,ISET) .NE. 0 ) THEN
                  MINTPM = MIN( TPM(ISTA,ISET), MINTPM )
               END IF
            END DO
            IF( MINTPM .LT. 100 ) THEN
               DO ISET = 1, NSET
                  IF ( TPM(ISTA,ISET) .NE. 0 ) THEN
                     IF( TPM(ISTA,ISET) .NE. MINTPM ) 
     1                    ANYCHG = .TRUE.
                     TPM(ISTA,ISET) = MINTPM
                  END IF
               END DO
            END IF
         END DO
C
C        Now deal with the multi station setups.
C
         IF( GOTMULTI ) THEN
C
C           Do a few passes through a loop of setting equalizing
C           TPM across a setup and then setting it to the minimum
C           for a station.  Do until no changes are made.  I don't
C           see an easy way out of this iterative proceedure.
C
  100       CONTINUE
            CHANGED = .FALSE.
C
C           For each setup, set TPM to the minimum value.
C
            DO ISET = 1, NSET
               MINTPM = 100
               DO ISTA = 1, NSTA
                  IF ( TPM(ISTA,ISET) .NE. 0 ) THEN
                     MINTPM = MIN( TPM(ISTA,ISET), MINTPM )
                  END IF
               END DO
               IF( MINTPM .LT. 100 ) THEN
                  DO ISTA = 1, NSTA
                     IF ( TPM(ISTA,ISET) .NE. 0 ) THEN
                        CHANGED = TPM(ISTA,ISET) .NE. MINTPM
                        TPM(ISTA,ISET) = MINTPM
                     END IF
                  END DO
               END IF
            END DO
C
C           Now redo the setting to the minimum across setups for
C           a station.
C
            DO ISTA = 1, NSTA
               MINTPM = 100
               DO ISET = 1, NSET
                  IF ( TPM(ISTA,ISET) .NE. 0 ) THEN
                     MINTPM = MIN( TPM(ISTA,ISET), MINTPM )
                  END IF
               END DO
               IF( MINTPM .LT. 100 ) THEN
                  DO ISET = 1, NSET
                     IF ( TPM(ISTA,ISET) .NE. 0 ) THEN
                        CHANGED = TPM(ISTA,ISET) .NE. MINTPM
                        TPM(ISTA,ISET) = MINTPM
                     END IF
                  END DO
               END IF
            END DO
C
C           If anything was changed, do it again.
C
            IF( CHANGED ) ANYCHG = .TRUE.
            IF( CHANGED ) GO TO 100
C
C           If get this far, are done with the multi case.
C
         END IF
C
C        If changes are needed, deal with them.
C
         IF( ANYCHG ) THEN
C
C           Tell the user something about what we are doing.
C
            MSGTXT = ' '
            WRITE( MSGTXT, '( 2A )' )
     1        'TPMFIX:   Forcing equal numbers of passes ',
     2        'per head position for all scans at each station'
            CALL WLOG( 1, MSGTXT )
C
C           Now reset the actual TAPEMODE values.
C
            DO ISET = 1, NSET
               ALERT = .TRUE.
               DO ISTA = 1, NSTA
                  IF( TPM(ISTA,ISET) .NE. 0 .AND. 
     1                TPM(ISTA,ISET) .NE. TAPEMODE(ISET) ) THEN
C
C                    Tell the user something was done to this setup.
C
                     IF( ALERT ) THEN
                        MSGTXT = ' '
                        WRITE( MSGTXT, '( A, I2, A, I2, 4A )' )
     1                     '          Changing TPMODE from ', 
     2                     TAPEMODE(ISET), ' to ', TPM(ISTA,ISET),
     3                     ' in setup: ',
     4                     SETNAME(ISET)(1:LEN1(SETNAME(ISET))),
     5                     ', station: ', 
     6                     SETSTA(1,ISET)(1:LEN1(SETSTA(1,ISET)))
                        CALL WLOG( 1, MSGTXT )
                        MSGTXT = ' '
                        ALERT = .FALSE.
                     END IF
C
C                    Actually do it.
C
                     TAPEMODE(ISET) = TPM(ISTA,ISET) 
C
                  END IF
               END DO
            END DO
         END IF
C
C        I think this atrocity is done.
C
      END IF
C
      RETURN
      END
