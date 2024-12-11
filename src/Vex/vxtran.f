      SUBROUTINE VXTRAN ( TRANLEN, ISCN, ISTA, GAPERR )
C
C     Routine for the VEX extension of SCHED.
C     Calculates how long an ftp scan will take to copy from the Mark5
C     to the linux disk. If this is longer than the gap at the end of
C     the scan, issue a warning.
C     C. Reynolds JIVE, 30 Sep 2004
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
C     variables
C     
      INTEGER   TRANLEN, ISCN, ISTA
      REAL BITRATE, TRANTIME, SCNGAP
      LOGICAL GAPERR
C 
C ----------------------------------------------------------------------
C
      IF (DEBUG) CALL WLOG( 1,'VXTRAN: Check transfer time for ftp'//
     1          ' scans')
      GAPERR = .FALSE.
C
C     get the data rate for the current scan. First need the setup for
C     current scan
C
      BITRATE = TOTBPS( NSETUP(ISCN, ISTA) )
C
C     estimate how long it will take to transfer the data based on an
C     assumed baud rate and including some latency
C
      TRANTIME = 5. + REAL (TRANLEN) * BITRATE /110.
C
C     Calculate the gap before the next scan and warn if too short
C
      SCNGAP = REAL( ( STARTJ(ISCN+1) - STOPJ (ISCN) ) * 86400.d0)
      IF ( SCNGAP .LT. TRANTIME ) THEN
        WRITE ( MSGTXT, '(A, A, A, I6, A)' )     
     1      'VXTRAN: You have scheduled an ftp (GRABTO) scan but you ',
     2      'have not left a long enough gap to transfer the data ',
     3      'before the next scan starts. Try inserting a gap of',
     4      NINT (TRANTIME) + 1, ' seconds after the ftp scan:'
        CALL WLOG ( 1, MSGTXT )
        CALL SCANID ( 1, ISCN, ISTA )
        GAPERR = .TRUE.
      END IF
C
      RETURN
      END
