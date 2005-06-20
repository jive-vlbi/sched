      SUBROUTINE CRDWRT( ISCN, ISTA, FIRSTS )
C
C     Subroutine for SCHED.  Calls appropriate routine to write
C     machine readable output.  Global files like VEX will have
C     been written earlier.
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
C     Call appropriate subroutine for each card type.  CRDVLA (observe
C     deck) should be called after subroutine VLBA to be sure that the 
C     SETUP file information for the VLA is available - it is read 
C     in VLBA.
C
C     Note the CONTOL = 'NONE' will just flush through without 
C     writing anything.
C
      IF( CONTROL(STANUM(ISTA)) .EQ. 'SNAP' .OR. 
     1    CONTROL(STANUM(ISTA)) .EQ. 'SN50' ) THEN
         CALL SNAP( ISCN, ISTA, FIRSTS )
      END IF
C
      IF( ( CONTROL(STANUM(ISTA)) .EQ. 'VLBA' .OR. 
     1      VLBADAR(STANUM(ISTA)) ) .AND. .NOT. VLAONLY ) THEN
         CALL VLBA( ISCN, ISTA, FIRSTS )
      END IF
C
      IF(CONTROL(STANUM(ISTA))(1:3) .EQ. 'VLA' ) THEN
         CALL CRDVLA ( ISCN, ISTA, FIRSTS )
      END IF
C
      IF( CONTROL(STANUM(ISTA)) .EQ. 'NRAO' ) THEN
         CALL CRDNRAO( ISCN, ISTA, FIRSTS )
      END IF
C
      RETURN
      END
