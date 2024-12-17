      SUBROUTINE PLINCK
C
C     Programming checks: subroutine for PLOT to check that
C     some parameters that must be the same in different 
C     include files are the same.
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
      INCLUDE 'beam.inc'
C
      INTEGER     I
C ----------------------------------------------------------------------
C
      IF( PSTMAX .LT. MAXSTA ) THEN
         CALL ERROR( 'PLINCK: PSTMAX in plot.inc must be >= MAXSTA'//
     1                ' in sched.inc.' )
      END IF
C
      IF( PSOMAX .LT. MAXSRC ) THEN
         CALL ERROR( 'PLINCK: PSOMAX in plot.inc must be >= MAXSRC'//
     1                ' in sched.inc.' )
      END IF
C
      I = MAXSCN * MAXSTA * 2
      IF( NPBAS .LT. I ) THEN
         WRITE( MSGTXT, '( A, I8, A, I8 )' )
     1        'PLINCK: BPBAS = ', NPBAS, '  MAXSCN*MAXSTA*2 = ', I
         CALL PUTOUT( MSGTXT )
         CALL ERROR( 'PLINCK: NPBAS in beam.inc must be at least'//
     1                ' ( MAXSCN * MAXSTA * 2 ) in sched.inc' )
      END IF
C
C ----------------------------------------------------------------------
C
      RETURN
      END
