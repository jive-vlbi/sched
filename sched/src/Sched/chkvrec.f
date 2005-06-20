      SUBROUTINE CHKVREC( KS, ERRS )
C
C     Routine for SCHED called by CHKSET that checks items related
C     to the VLBA RECORDERS's.  
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER       KS, ICH, ITRK, IPAS, IGRP, ISTA, KSTA
      INTEGER       ONCHN, TRK1, TRK2
      REAL          TRKCHN, TRKUSED(100)
      LOGICAL       ERRS
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 1, 'CHKVREC starting.' )
C
C     Check the format request.
C
      IF( FORMAT(KS) .EQ. 'NONE' ) THEN
C
C        Don't attempt to check various items when FORMAT='NONE'.
C
         RETURN
C
      ELSE IF( FORMAT(KS) .NE. 'VLBA1:1' .AND. 
     1         FORMAT(KS) .NE. 'VLBA1:2' .AND.
     2         FORMAT(KS) .NE. 'VLBA1:4' .AND. 
     3         FORMAT(KS) .NE. 'VLBA2:1' .AND.
     4         FORMAT(KS) .NE. 'VLBA4:1' .AND. 
     5         FORMAT(KS) .NE. 'MARKIII' ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, A)' )
     1       'CHKVREC: Invalid FORMAT for VLBA recording '//
     2       '(see OBSTYPE): ', FORMAT(KS)
         CALL WLOG( 1, MSGTXT )
         ERRS = .TRUE.
      END IF
C
C
C     Check that no request has been made to overwrite a track:
C     Also check use of reasonable track numbers.
C
      DO ITRK = 2, 33
         TRKUSED(ITRK) = 0
      END DO
C
C
      DO IPAS = 1, TAPEMODE(KS)
         DO ICH = 1, NCHAN(KS)
            TRKCHN = FANOUT(KS) * BITS(ICH,KS)
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
     1             'CHKVREC: Requested tracks ', TRK1, TRK2, 
     2             ' are not in the ranges 2 to 33 or 66 to 97.'
               ELSE
                  WRITE( MSGTXT, '( A, 2I4, A )' ) 
     1             'CHKVREC: Requested tracks ', TRK1, TRK2, 
     2             ' are not in the ranges 2 to 33.'
               END IF
               CALL WLOG( 1, MSGTXT )
               ERRS = .TRUE.
            END IF
C
C           Check that the channel assignments are to reasonable heads
C           within roll groups.  IGRP is the channel within the group.
C
            IGRP = INT( (TRACK(ICH,IPAS,KS) - 2 ) / 2 )
            IGRP = MOD( IGRP, 8 )
            IF( .NOT. ERRS .AND. FANOUT(KS) .GE. 1.0  .AND. 
     1          MOD( IGRP, NINT( TRKCHN ) ) .NE. 0 ) THEN
               CALL WLOG( 1, 'CHKVREC: Track assignment does not ' //
     1              'allow an even number of channels per head group.' )
               CALL WLOG( 1, '         It might even cause a channel '//
     1              'to use heads in two groups.' )
               CALL WLOG( 1, '         Adjust track assignments or '//
     1              'let SCHED chose them.' )
               ERRS = .TRUE.
            END IF
C
         END DO
      END DO
C
C     Check the flags to be sure that not too many channels
C     were written on each track.
C
      DO ITRK = 2, 33
         IF( TRKUSED(ITRK) .GT. 1.0 ) THEN
            CALL WLOG( 1, 'CHKVREC: The setup file requests that '//
     1        'a track be overwritten.' )
            CALL WLOG( 1, 'CHKVREC: Check assignments and room for '//
     1                 'fan out and 2 bits in' )
            CALL WLOG( 1, '       ' // SETNAME(KS) )
            ERRS = .TRUE.
         END IF
         IF( TRKUSED(ITRK) .NE. 0.0 .AND. 
     1       TRKUSED(ITRK) .LT. 1.0 ) THEN
            CALL WLOG( 1, 'CHKVREC: Insufficient data routed to a'
     1            // ' track while using fan-in.' )
            ERRS = .TRUE.
         END IF
      END DO
C
C     Check the barrel roll request.
C
      IF( BARREL(KS) .NE. 'roll_off' .AND. 
     1    BARREL(KS) .NE. 'roll_8' .AND.
     2    BARREL(KS) .NE. 'roll_16' .AND.
     3    BARREL(KS) .NE. 'roll_auto' ) THEN
         CALL WLOG( 1, 'CHKVREC: Bad barrel roll specification: ' //
     1       BARREL(KS) )
         ERRS = .TRUE.
      END IF
C
C     Set allowed passes per head position (TAPEMODE).  I don't
C     have the track tables for more than 8.
C
      IF( TAPEMODE(KS) .NE. 1 .AND. TAPEMODE(KS) .NE. 2 .AND.
     1    TAPEMODE(KS) .NE. 4 .AND. TAPEMODE(KS) .NE. 8 ) THEN
         CALL WLOG( 1, 'CHKVREC: tpmode must be 1, 2, 4, or 8' )
         ERRS = .TRUE.
      END IF
C
C     Check tape length vs density.  This might be a TPINI item,
C     but the right answers can be system dependent so it will be
C     put here.  A little hoop jumping is required to relate the
C     station of the setup with the station number in the schedule,
C     from which the tape initialization stuff can be found.
C
      KSTA = ISETSTA(KS)
      ISTA = ISCHSTA(ISETSTA(KS))
C
C     Make sure we got the tape initialization station.
C
      IF( ISTA .EQ. 0 ) THEN
         CALL WLOG( 1, 'CHKVREC: ISCHSTA = 0. ' )
         CALL ERRLOG( '         Possible programming problem.' )
      ELSE
C
C        Test for appropriate length vs density.
C
         IF( TPLENG(ISTA) .GT. 12000 .AND. DENSITY(ISTA) .NE. 'H' ) THEN
            CALL WLOG( 1, ' CHKVREC: ** WARNING - high density ' //
     1          'not specified for long tape.  Was this intended?' )
         END IF
         IF( TPLENG(ISTA) .LE. 12000 .AND. DENSITY(ISTA) .NE. 'L' ) THEN
            CALL WLOG( 1, ' CHKVREC: ** WARNING - low density ' //
     1          'not specified for short tape.  Was this intended?' )
         END IF
      END IF
C
C     Note possible return from earlier if FORMAT=NONE.
C
      RETURN
      END

