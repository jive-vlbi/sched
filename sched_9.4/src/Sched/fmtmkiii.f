      SUBROUTINE FMTMKIII( KS, OK )
C
C     Routine for SCHED called by SETFMT that sets the format 
C     for Mark III for setup group KS.  It will attempt to 
C     set FORMAT, SPEEDUP, FANOUT, and TAPEMODE.  OK will 
C     return .TRUE. if it succeeds.  It will return .FALSE. 
C     if not, which for MarkIII should not happen.
C
      INCLUDE        'sched.inc'
      INCLUDE        'schset.inc'
C
      INTEGER        KS
      REAL           TBPR
      LOGICAL        OK
C ---------------------------------------------------------------------
C   
C     FORMAT is already set or we wouldn't be in this routine.
C     There are no choices for this format.
C
      IF( FANOUT(KS) .NE. 0.0 ) GO TO 999
C
C     Get the total bit rate.
C
      TBPR = NCHAN(KS) * SAMPRATE(KS) * BITS(1,KS)
C
C     First see if the user has set TPMODE (TAPEMODE) which is
C     the number of passes per head position.  For Mark III, this
C     is the only way the user has of affecting the fanout and
C     speedup.
C
      IF( TAPEMODE(KS) .GT. 0 ) THEN
C
C        Do an incomplete consistency check.
C
         IF( ( NCHAN(KS) .GT. 16 .AND. TAPEMODE(KS) .GT. 1 ) .OR.
     1       ( NCHAN(KS) .GT. 8  .AND. TAPEMODE(KS) .GT. 2 ) ) THEN
            CALL WLOG( 1, 
     1         'FMTMKIII: Impossible TPMODE for this format in ' //
     2         'MarkIII format' )
            CALL ERRSET( KS )
         END IF
      ELSE 
         IF( NCHAN(KS) .GT. 16 ) THEN
            TAPEMODE(KS) = 1
         ELSE IF( NCHAN(KS) .GT. 8 ) THEN
            TAPEMODE(KS) = 2
         ELSE 
            TAPEMODE(KS) = 4
         END IF
      END IF
C
      SPEEDUP(KS) = 8.0 / SAMPRATE(KS)
C
      IF( SPEEDUP(KS) .LT. 1.0 ) THEN
         CALL WLOG( 1, 'FMTMKIII: Mark III can''t do sample rates '//
     1           'over 8 Msamp/s' )
         CALL ERRSET( KS )
      END IF
C
C     Fill in some items that are mainly useful for VLBA/MKIV, but 
C     could be useful in mixed mode observations.
C
      FANOUT(KS) = 1.0
      TBPS(KS)   = SAMPRATE(KS)
      MINTRAK(KS) = NCHAN(KS)
      MAXTRAK(KS) = NCHAN(KS)
      MINTBPS(KS) = SAMPRATE(KS)
      MAXTBPS(KS) = SAMPRATE(KS)
C
  999 CONTINUE
      OK = .TRUE.
      RETURN
      END








