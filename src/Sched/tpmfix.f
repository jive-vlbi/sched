      SUBROUTINE TPMFIX
C
C     Deal with cases were tape stations have varying TAPEMODE
C     during an observation.  While varying TAPEMODE  might be 
C     possible, it would introduce very serious complications 
C     in the allocation of tape resources and possible inefficient 
C     use of tape.  Don't allow it.  Note that different stations
C     are allowed to use different tapemodes.
C
C     Note that TAPEMODE is really a statement of how many tracks
C     are being used at once.  We don't want this to vary for tape.
C
C     For disk, variable numbers of tracks are not an issue.  Also
C     there is no head position concept.  We just want to keep
C     the same heads all the time - the ones that a tape would have
C     used in the first pass.  So just set TAPEMODE=1.  Note that
C     this is for MARK5A.  For MARK5B, the concept of tracks goes
C     away and we are only setting channels.  This routine will
C     probably be skipped, but I have to think about it.
C
C     The logic used to be a mess when one setup could control 
C     several stations (like VLBA stations).  But SCHED was modified
C     to avoid this situation so the logic is being simplified
C     on June 28, 2005.
C
C     Note that this routine comes between setting the basic 
C     formatter parameters and setting track assignments.  This is
C     not where we attempt to adjust the fan outs etc to equalize
C     the number of tracks.  This was already attempted in SETFORM
C     in setting default formats.  But that might not have 
C     succeeded, or the user might have forced a situation with
C     variable TAPEMODEs.  
C
C     To enforce constant TAPEMODE, for each station separately, go 
C     through all the setups looking at those with VLBA and MKIV 
C     formats.  Get the minimum TAPEMODE.  Then go back and set 
C     the TAPEMODE for all these setups to the minimum.  I tried to
C     deal with this entirely in TPSCH, but that had trouble with
C     track assignments.  A TAPEMODE=4 setup would only get 2 passes
C     per head position, as expected, but they would use only the
C     even tracks, hence writing over half the tracks used by an
C     earlier TAPEMODE=2 scan.
C
C     Modified June 27, 2005 for the new regime with a separate setup
C     for every station.  See the archive of obsolete code for the
C     version that understands multiple stations per setup.  The
C     changes were an immense simplification.  RCW.
C
      INCLUDE   'sched.inc'
      INCLUDE   'schset.inc'
C
      INTEGER    KS, ISTA, MINTPM
      INTEGER    LEN1
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'TPMFIX starting' )
C
C     Since we are not changing track rates here, only deciding
C     whether to waste some tracks, we can do each station 
C     independently.
C
      DO ISTA = 1, NSTA
         IF( USEDISK(ISTA) ) THEN
            DO KS = 1, NSET
               IF( ISCHSTA(ISETSTA(KS)) .EQ. ISTA .AND.
     1             RECUSED(KS) .AND. (
     2             FORMAT(KS)(1:4) .EQ. 'VLBA' .OR. 
     3             FORMAT(KS)(1:4) .EQ. 'MARK' .OR.
     4             FORMAT(KS)(1:4) .EQ. 'MKIV' ) ) THEN
                  TAPEMODE(KS) = 1
               END IF
            END DO
         ELSE IF( USETAPE(ISTA) ) THEN
C
C           Find the minumum TAPEMODE for this station.
C
            MINTPM = 100
            DO KS = 1, NSET
               IF( ISCHSTA(ISETSTA(KS)) .EQ. ISTA .AND.
     1             RECUSED(KS) .AND. (
     2             FORMAT(KS)(1:4) .EQ. 'VLBA' .OR. 
     3             FORMAT(KS)(1:4) .EQ. 'MARK' .OR.
     4             FORMAT(KS)(1:4) .EQ. 'MKIV' ) ) THEN
                  MINTPM =  MIN( MINTPM, TAPEMODE(KS) )
               END IF
            END DO
C
C           Now set all setups for this station to MINTPM.
C
            DO KS = 1, NSET
               IF( ISCHSTA(ISETSTA(KS)) .EQ. ISTA .AND.
     1             RECUSED(KS) .AND. (
     2             FORMAT(KS)(1:4) .EQ. 'VLBA' .OR. 
     3             FORMAT(KS)(1:4) .EQ. 'MARK' .OR.
     4             FORMAT(KS)(1:4) .EQ. 'MKIV' ) ) THEN
C
                  IF( TAPEMODE(KS) .NE. MINTPM ) THEN
C
                     WRITE( MSGTXT, '( A, I2, A, I2, 4A )' )
     1                  '          Changing TPMODE from ', 
     2                  TAPEMODE(KS), ' to ', MINTPM,
     3                  ' in setup: ',
     4                  SETNAME(KS)(1:LEN1(SETNAME(KS))),
     5                  ', station: ', 
     6                  SETSTA(1,KS)(1:LEN1(SETSTA(1,KS)))
                     CALL WLOG( 1, MSGTXT )
                     MSGTXT = ' '
                     CALL WLOG( 1, 'to match other setups.'//
     1                 '  This will waste some tape space.' )
C
C                    Do it.
C
                     TAPEMODE(KS) =  MINTPM
                  END IF
               END IF
            END DO
         END IF
      END DO
C
C     We're done.  If you look at the archive version of this
C     routine, you'll see just how much easier this was!
C
      RETURN
      END
