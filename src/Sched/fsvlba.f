      SUBROUTINE FSVLBA( TSTOP, KS )
C
C     Routine for SCHED that writes the extra lines in a VLBA file for a
C     frequency switching loop.
C
C     This capability should probably be eliminated.  It was useful in the
C     MarkII days, but I doubt that it has been used since. There are several
C     input parameters associated with this.  Note added Sept. 23, 2013 RCW.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      CHARACTER  TSTOP*9
C
C     Common for communication with VLBASU.
C
      INTEGER  MBBCFREQ, KS
      DOUBLE PRECISION  LBBCFREQ(MCHAN)
      COMMON  / CVLBA /  LBBCFREQ, MBBCFREQ
      SAVE    /CVLBA/
C ----------------------------------------------------------------------
      WRITE( IUVBA, '( 3A, I4, A )' )
     1   'stop=', TSTOP, '  dur=', SWTCHDUR(KS), 
     2   's  !BEGIN LOOP!  !NEXT!'
C
      CALL VLBAREAL( 'bbsynth', 7, NCHAN(KS), BBSYN2(1,KS), LBBCFREQ,
     1         MBBCFREQ, '(F6.2)', 6, .FALSE., IUVBA )
C
      WRITE( IUVBA, '( A )' )
     1   'qual=2     !LOOP BACK!  !NEXT!'
C
      RETURN
      END
