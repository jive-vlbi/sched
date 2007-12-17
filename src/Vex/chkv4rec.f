      SUBROUTINE CHKV4REC( KS, ERRS, SRECORD )
C
C     Routine for SCHED called by CHKSET that checks items related
C     to VLBA4 RECORDERS's. It also deals with VLBA recorders which
C     already have a VLBA4 DAR but not upgraded yet for 2-head/320ips
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      CHARACTER     SRECORD*5
      INTEGER       KS, ICH, ITRK, IPAS, KSTA, ISTA, TSTA
      INTEGER       ONCHN, TRK1, TRK2
      REAL          TRKCHN, TRKUSED(100)
      LOGICAL       ERRS
C ----------------------------------------------------------------------
C     Check the format request.
C
      IF( FORMAT(KS) .NE. 'MKIV1:1' .AND. 
     1    FORMAT(KS) .NE. 'MKIV1:2' .AND.
     2    FORMAT(KS) .NE. 'MKIV1:4' .AND. 
     3    FORMAT(KS) .NE. 'MKIV2:1' .AND.
     4    FORMAT(KS) .NE. 'MKIV4:1' .AND. 
     5    FORMAT(KS) .NE. 'MARKIII' ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, A)' )
     1       'CHKV4REC: Invalid FORMAT for VLBA4 recording '//
     2       '(see OBSTYPE): ', FORMAT(KS)
         CALL WLOG( 1, MSGTXT )
         ERRS = .TRUE.
      END IF
C
C     copied Track numbering from VLBA recorder section
C
      DO ITRK = 2, 100
         TRKUSED(ITRK) = 0
      END DO
C
C     Check that each channel for each pass uses heads in the
C     allowed range 2 to 33.  If so, flag the fraction of the
C     track that was used.
C     Recall that a channel uses every other head because of 
C     the way heads are numbered.
C
      DO IPAS = 1, TAPEMODE(KS)
         DO ICH = 1, NCHAN(KS)
C
C           Check that each channel for each pass uses heads in the
C           allowed ranges of  2 to 33 (and 66 to 97 for TWOHEAD).
C           If so, flag the fraction of the track that was used 
C           (preparing for fan-in modes which may never be used).
C           Recall that a channel uses every other head because of 
C           the way heads are numbered.
C
C           Get the number of tracks per channel.  Recall FANOUT can
C           be less than one for fan in modes (0.25 or 0.5).  By
C           adding 0.8 before intergeriziing, we get minimum of
C           one track.
C
            TRKCHN = FANOUT(KS) * BITS(ICH,KS)
            ONCHN  = TRKCHN + 0.8
            TRK1 = TRACK(ICH,IPAS,KS)
            TRK2 = TRK1 - 2 + 2 * ONCHN
            IF( ( ( TRK1 .GE.  2 .AND. TRK1 .LE. 33 ) .OR. ( TWOHEAD
     1           .AND. ( TRK1 .GE. 66 .AND. TRK1 .LE. 97 ) ) ) .AND.
     2          ( ( TRK2 .GE.  2 .AND. TRK2 .LE. 33 ) .OR. ( TWOHEAD
     3           .AND.  ( TRK2 .GE. 66 .AND. TRK2 .LE. 97 ) ) ) ) THEN
C
               DO ITRK = TRK1, TRK2, 2
                  TRKUSED(ITRK) = TRKUSED(ITRK) + MIN( TRKCHN, 1.0 )
               END DO
            ELSE
               IF( TWOHEAD ) THEN
                  WRITE( MSGTXT, '( A, 2I4, A )' ) 
     1             'CHKV4REC:Requested tracks ', TRK1, TRK2, 
     2             ' are not in the ranges 2 to 33 or 66 to 97.'
               ELSE
                  WRITE( MSGTXT, '( A, 2I4, A )' ) 
     1             'CHKV4REC:Requested tracks ', TRK1, TRK2, 
     2             ' are not in the ranges 2 to 33.'
               END IF
               CALL WLOG( 1, MSGTXT )
               ERRS = .TRUE.
            END IF
         END DO
      END DO
C
C     Check the flags to be sure that not too many channels
C     were written on each track.
C
      DO ITRK = 2, 33
         IF( TRKUSED(ITRK) .GT. 1.0 ) THEN
            CALL WLOG( 1, 'CHKV4REC: The setup file requests that '//
     1        'a track be overwritten.' )
            CALL WLOG( 1, 'CHKV4REC: Check assignments and room for '//
     1                 'fan out and 2 bits in' )
            CALL WLOG( 1, '       ' // SETNAME(KS) )
            ERRS = .TRUE.
         END IF
         IF( TRKUSED(ITRK) .NE. 0.0 .AND. 
     1       TRKUSED(ITRK) .LT. 1.0 ) THEN
            CALL WLOG( 1, 'CHKV4REC: Insufficient data routed to a'
     1            // ' track while using fan-in.' )
            ERRS = .TRUE.
         END IF
      END DO
C
C     Set allowed passes per head position (TAPEMODE).  I don't
C     have the track tables for more than 8.
C
      IF( TAPEMODE(KS) .NE. 1 .AND. TAPEMODE(KS) .NE. 2 .AND.
     1    TAPEMODE(KS) .NE. 4 .AND. TAPEMODE(KS) .NE. 8 ) THEN
         CALL WLOG( 1, 'CHKV4REC: tpmode must be 1, 2, 4, or 8' )
         ERRS = .TRUE.
      END IF
C
C      CR (25 Oct 2004): barrel-roll now possible (but not for JIVE, see
C                        chkjive.f)
C     Barrel-rolling currently not enabled for VLBA4, waits firmware upgrade
C
C      IF( BARREL(KS) .NE. 'roll_off' .AND.
C     $    .NOT. MODETEST(KS) ) THEN
C         CALL WLOG( 1, 'CHKV4REC:Barrel-rolling currently not in MkIV')
C         CALL WLOG( 1, '         Can be tested by setting MODETEST')
C         ERRS = .TRUE.
C      END IF
C      IF( BARREL(KS) .NE. 'roll_off' .AND.
C     $    .NOT. MODETEST(KS) ) THEN
CC         CALL WLOG( 1, 'CHK4REC: Barrel-rolling currently not in MkIV')
CC         CALL WLOG( 1, '         Can be tested by setting MODETEST')
C         CALL WRTMSG( 0, 'CHKV4REC', 'barrelroll')
C         IF ( CORREL(1:4) .EQ. 'JIVE') THEN
C           WRITE ( MSGTXT, '(A, A, A)')
C     1          'CHKV4REC: You have requested barrel-rolling ', 
C     2          'for the JIVE correlator. This is not currently ',
C     3          'available. Testers *only* may specify MODETEST.'
C           CALL WLOG ( 1, MSGTXT )
C           ERRS = .TRUE.
C         END IF
C      END IF
      IF( FANOUT(KS) .LT. 0.9 ) THEN
         CALL WLOG( 1, 'CHKV4REC:Fan-in currently not supported VLBA4')
         ERRS = .TRUE.
      END IF
C
C     Check tape length vs density.  This might be a TPINI item,
C     but the right answers can be system dependent so it will be
C     put here.  A little hoop jumping is required to relate the
C     station of the setup with the station number in the schedule,
C     from which the tape initialization stuff can be found.
C
      TSTA = 0
      KSTA = ISETSTA(KS)
      DO ISTA = 1, NSTA
         IF( STANUM(ISTA) .EQ. ISETSTA(KS) ) THEN
            TSTA = ISTA
         END IF
      END DO
C
C     Make sure we got the tape initialization station.
C
      IF( TSTA .EQ. 0 ) THEN
         WRITE(*,*) 'CHKV4REC: Could not relate setup station and ',
     1         'tape inialization stations.'
         CALL ERRLOG( '         Possible programming problem.' )
      ELSE
C
C        Test for appropriate length vs density.
C
         IF( TPLENG(TSTA) .GT. 12000 .AND. DENSITY(TSTA) .NE. 'H' ) THEN
            WRITE(*, '( 2A )') ' CHKV4REC: ** WARNING - high density ',
     1          'not specified for long tape.  Was this intended?'
         END IF
         IF( TPLENG(TSTA) .LE. 12000 .AND. DENSITY(TSTA) .NE. 'L' ) THEN
            WRITE(*, '( 2A )') ' CHKV4REC: ** WARNING - low density ',
     1          'not specified for short tape.  Was this intended?'
         END IF
      END IF
C
C     There should be a test here eventually to guard the VLBA drives
C     against two head recording and 320 ips, which MkIV/VLBA4 can do.
C
      RETURN
      END
