      SUBROUTINE OPTUPT( LASTISCN, KSCN, ISCN, ADJUST, KEEP, DONE )
Cf2py intent(in, out) LASTISCN
Cf2py intent(in) KSCN, ISCN
Cf2py intent(out) ADJUST, KEEP, DONE
C
C     Routine for SCHED that creates a schedule useful for planning.
C     Each scan (source) is expanded to a series of scans, each of
C     length DUR, spanning the time from START(1) to START(1) + OPTDUR
C
C     This routine, along with new plotting capabilities, should make
C     it possible to stop supporting UPTIME.
C
C     Reset LASTISCN when changing source to prevent unrealistic slew
C     worries.
C
      INCLUDE  'sched.inc'
C
      INTEGER           ISCN, INSCN, KSCN, JSCN, LSCN, LASTISCN(*)
      INTEGER           ISTA
      LOGICAL           KEEP, ADJUST, DONE, DUP
      DOUBLE PRECISION  LASTTIME, T_AVAIL
      SAVE              INSCN
C ---------------------------------------------------------------------
      DONE = .FALSE.
      KEEP = .TRUE.
      ADJUST = .FALSE.
C
C     Some initializations on first scan.
C
      IF( KSCN .EQ. 1 ) THEN
C
C        Warn that station files will not be written.
C
         CALL WLOG( 0, 'OPTUPT:  OPTMODE=UPTIME was specified.  Station'
     1       // ' files (sch, crd, ' )
         CALL WLOG( 0, '         vex, and drudg) will not be written.' )
C
C        Start the set of new scans beyond NSCANS and use first start
C        time.  TAPPROX is the approximate start time of the next scan.
C        Don't allow this time to be adjusted.
C
         INSCN    = 1
         STARTJ(ISCN) = STARTJ(1)
C
C        Make sure that OPDUR is available.
C
         IF( OPDUR .LE. 0.D0 ) THEN
            CALL ERRLOG( 
     1       ' OPTCUPT: For OPTMODE=UPTIME, OPDUR must be given.' )
         END IF
C
C        Check that each scan is a different source.
C
         WRITE( MSGTXT, '( A, I5, A )' ) '         There are ', NSCANS,
     1        ' input scans with sources: '
         CALL WLOG( 0, MSGTXT )
         DUP = .FALSE.
         DO JSCN = 1, NSCANS
            MSGTXT = ' '
            WRITE( MSGTXT, '( 5X, I5, 2A )' ) JSCN, ': ', SCNSRC(JSCN)
            CALL WLOG( 0, MSGTXT )
            IF( JSCN .GT. 1 ) THEN
               DO LSCN = 1, JSCN - 1
                  IF( SCNSRC(JSCN) .EQ. SCNSRC(LSCN) ) DUP = .TRUE.
               END DO
            END IF
         END DO
         IF( DUP ) THEN
            CALL WLOG( 1, '       There were duplicate sources. ' )
            CALL WLOG( 1, '       This is probably not what you ' // 
     1                 'wanted for OPTYPE=UPTIME.' )
         END IF
C
      ELSE
C
C        Set the new start time.
C
         STARTJ(ISCN) = STOPJ(ISCN-1) + GAP(INSCN)
C
      END IF
C
C     Set the stop time of the scan.
C
      STOPJ(ISCN) = STARTJ(ISCN) + DUR(INSCN)
C
C     See if it is time to start the next source.
C     If it is, start new sequence and reset LASTISCN
C
      IF( STOPJ(ISCN) .GT. STARTJ(1) + OPDUR ) THEN
C
         IF( INSCN .GE. NSCANS ) THEN
            DONE = .TRUE.
            KEEP = .FALSE.
         ELSE
            INSCN = INSCN + 1
            STARTJ(ISCN) = STARTJ(1)
            STOPJ(ISCN) = STARTJ(ISCN) + DUR(INSCN)
            DO ISTA = 1, NSTA
               LASTISCN(ISTA) = 0
            END DO
         END IF
C
      END IF
C
      IF( .NOT. DONE ) THEN
C
C        Fill in the required information for the scan.
C
         CALL SCNDUP( ISCN, INSCN, .FALSE., 'OPTUPT' )
C
C        Only keep stations that are up.
C
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) ) THEN
               CALL STAGEO( ISCN, ISTA, STARTJ(ISCN), 
     1             0, LASTTIME, T_AVAIL, 'OPTUPT' )
               STASCN(ISCN,ISTA) = UP1(ISCN,ISTA) .EQ. ' ' .AND.
     1             UP2(ISCN,ISTA) .EQ. ' ' .AND.
     2             ( EL1(ISCN,ISTA) + EL2(ISCN,ISTA) ) / 2.0 .GT. 
     3              OPMINEL(ISCN)               
C
            END IF
         END DO
C
      END IF
C
      RETURN
      END
