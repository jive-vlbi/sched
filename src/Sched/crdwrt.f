      SUBROUTINE CRDWRT( ISCN, ISTA, FIRSTS )
C
C     Subroutine for SCHED.  Calls appropriate routine to write
C     machine readable output.  Global files like VEX will have
C     been written earlier.
C
C     This used to include many different file types.  In fact, the
C     original purpose of SCHED was to write the distinct file type
C     required for each station.  The world is now much more 
C     standardized on Vex.  Now, the only thing left here is the 
C     VLBA crd files and this routine looks superfluous, but keep
C     for now in case it's original need every returns.  The crd 
C     files will go away too when the VME computers at the VLBA 
C     sites are abandoned..  RCW May 2012 and Sep 2013.
C
      INCLUDE 'sched.inc'
C
      INTEGER     ISCN, ISTA
      LOGICAL     FIRSTS
C-----------------------------------------------------------------
      IF( DEBUG .AND. ( FIRSTS .OR. ISCN .EQ. -999 ) ) THEN
         CALL WLOG( 0, 'CRDWRT: Making ' //
     1      CONTROL(STANUM(ISTA))//' control file for '//STANAME(ISTA) )
      END IF
C
      IF( ( CONTROL(STANUM(ISTA)) .EQ. 'VLBA' .OR. 
     1      VLBADAR(STANUM(ISTA)) ) .AND. .NOT. VLAONLY ) THEN
         CALL VLBA( ISCN, ISTA, FIRSTS )
      END IF
C
      RETURN
      END
