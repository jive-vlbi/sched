      SUBROUTINE PLCKRD( XPT, YPT, R, DECM, RAM, VALID )
C
C     Routine for plot that check if the source is inside the
C     Calibrators area
C
      INCLUDE 'sched.inc'
C
      LOGICAL           VALID
      REAL              XPT, YPT, R, DECM, RAM, RPT, XGPT, SDECM
C ----------------------------------------------------------------------
C
      VALID = .TRUE.
C
C     Rescale RA to degree
C
      XGPT = XPT * 15.0 * COS( DECM )
      SDECM = ( DECM / RADDEG ) * 3600.0
C
C     Radius of the source
C
      RPT = SQRT( ( ( XGPT - RAM )**2 ) + ( ( YPT - SDECM )**2 ) )
C
C     Check
C
      IF( RPT .GT. R ) VALID = .FALSE.
C
      RETURN
      END
