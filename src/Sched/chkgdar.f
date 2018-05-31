      SUBROUTINE CHKGDAR( KS, ERRS )
Cf2py intent(in, out) ERRS
C
C     Routine for SCHED called by CHKSET that checks a few specific
C     restrictions on the "geodetic" VLBA racks.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER    KS, ICH, IIF, IBBC
      LOGICAL    ERRS, OK, SOMEBAD, SOMETWO
C
      INTEGER    MAXBBC, MAXIF, I
      PARAMETER  (MAXBBC=14)
      PARAMETER  (MAXIF=4)
      INTEGER    IFBBC(MAXBBC,MAXIF)
      CHARACTER  IFNAM(MAXIF)*2
      DATA  (IFBBC(I,1),I=1,MAXBBC) / 1,1,1,1,1,1,1,1,0,0,0,0,0,0 /
      DATA  (IFBBC(I,2),I=1,MAXBBC) / 1,1,0,0,0,0,0,0,1,1,1,1,1,1 /
      DATA  (IFBBC(I,3),I=1,MAXBBC) / 1,1,1,1,1,1,1,1,0,0,0,0,0,0 /
      DATA  (IFBBC(I,4),I=1,MAXBBC) / 1,1,0,0,0,0,0,0,1,1,1,1,1,1 /
      DATA  (IFNAM(I),I=1,MAXIF) / 'A', 'B', 'C', 'D' /
C
C     Note that the above data statement is also in BBCGEO.  Changes
C     should be made in both places.
C ---------------------------------------------------------------------
      IF( SDEBUG ) CALL WLOG( 0, 'CHKGDAR: Starting.' )
C
C     Loop through the channels checking IF assignments.
C
      SOMEBAD = .FALSE.
      DO ICH = 1, NCHAN(KS)
C
C        See if this channel uses an allowed IF given the wiring
C        constraints embodied in the IFBBC array.
C
         OK = .FALSE.
         IBBC = BBC(ICH,KS)
         DO IIF = 1, MAXIF
            IF( IFNAM(IIF) .EQ. IFCHAN(ICH,KS) .AND. 
     1          IFBBC(IBBC,IIF) .EQ. 1 ) OK = .TRUE.
         END DO
C
C        Give warnings if it was not allowed.
C
         IF( .NOT. OK ) THEN
            SOMEBAD = .TRUE.
            SETMSG = ' '
            WRITE( SETMSG, '( 3A, I4 )' )
     1          'CHKGDAR: Illegal IF input ', IFCHAN(ICH,KS),
     2          ' for geodetic type VLBA rack, channel ', ICH
            CALL WLOG( 1, SETMSG )
            ERRS = .TRUE.
         END IF
C
      END DO
      IF( SOMEBAD ) THEN
         SETMSG = ' '
         WRITE( SETMSG, '( 2A )' )
     1       '         Be careful of special wiring restrictions ',
     2       'for these DARs.' 
         CALL WLOG( 0, SETMSG )
      END IF
C
C     Now check that no more than 8 BBCs are being requested with
C     two bit sampling.
C
      SOMEBAD = .FALSE.
      SOMETWO = .FALSE.
      DO ICH = 1, NCHAN(KS)
         IF( BITS(ICH,KS) .NE. 1 ) SOMETWO = .TRUE.
      END DO
      DO ICH = 1, NCHAN(KS)
         IF( SOMETWO .AND. BBC(ICH,KS) .GT. 8 ) SOMEBAD = .TRUE.
      END DO
C
      IF( SOMEBAD ) THEN
         SETMSG = ' '
         WRITE( SETMSG, '( 2A )' )
     1        'CHKGDAR:  Geodetic type VLBA systems cannot use 2 bit ',
     2        'samples from more than the first 8 BBCs.'
         CALL WLOG( 1, SETMSG )
         ERRS = .TRUE.
      END IF
C
      RETURN
      END






