      SUBROUTINE FSVLBA( TSTOP )
C
C     Routine for SCHED that writes the extra lines in a VLBA file for a
C     frequency switching loop.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      CHARACTER  TSTOP*9
C
C     Common for communication with VLBASU.
C
      INTEGER  MBBCFREQ
      REAL     LBBCFREQ(MCHAN)
      COMMON  / CVLBA /  LBBCFREQ, MBBCFREQ
      SAVE    /CVLBA/
C ----------------------------------------------------------------------
      WRITE( IUVBA, '( 3A, I4, A )' )
     1   'stop=', TSTOP, '  dur=', SWTCHDUR(LS), 
     2   's  !BEGIN LOOP!  !NEXT!'
C
      CALL VLBAREAL( 'bbsynth', 7, NCHAN(LS), BBSYN2(1,LS), LBBCFREQ,
     1         MBBCFREQ, '(F6.2)', 6, .FALSE., IUVBA )
C
      WRITE( IUVBA, '( A )' )
     1   'qual=2     !LOOP BACK!  !NEXT!'
C
      RETURN
      END
