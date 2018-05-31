      SUBROUTINE CHKV4DAR( KS, ERRS )
Cf2py intent(in, out) ERRS
C
C     Routine for SCHED called by CHKSET that checks a few specific
C     restrictions on the VLBA4 racks, original VLBAG racks with a MkIV
C     formatter.
C     By H.J. van Langevelde, JIVE, 151099 
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER    KS, ICH, IIF, IBBC
      LOGICAL    ERRS, OK, SOMEBAD
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
      IF( SDEBUG ) CALL WLOG( 1, 'CHKV4DAR: Starting.' )
C
C
C
      IF( VLBITP .AND. FORMAT(KS) .NE. 'NONE' ) THEN
         IF( FORMAT(KS) .NE. 'MARKIII' .AND. 
     1       FORMAT(KS) .NE. 'MKIV1:1' .AND. 
     2       FORMAT(KS) .NE. 'MKIV1:2' .AND.
     3       FORMAT(KS) .NE. 'MKIV1:4' .AND. 
     4       FORMAT(KS) .NE. 'MKIV2:1' .AND.
     5       FORMAT(KS) .NE. 'MKIV4:1' .AND.
     6       FORMAT(KS) .NE. 'MARK5B' .AND.
     7       FORMAT(KS) .NE. 'S2' ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, A)' )
     1          'CHKV4DAR: Invalid FORMAT for wideband observing '//
     2          '(see OBSTYPE): ', FORMAT(KS)
            CALL WLOG( 1, MSGTXT )
            ERRS = .TRUE.
         END IF
      END IF
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
     1          'CHKV4DAR: Illegal IF input ', IFCHAN(ICH,KS),
     2          ' for VLBA4 geodetic type VLBA rack, channel ', ICH
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
         CALL WLOG( 1, SETMSG )
      END IF
C
C     For VLBA 4 no need to check number of bits is restricted to first 8
C
      RETURN
      END






