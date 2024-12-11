      SUBROUTINE VXWRPR
C 
C     Routine specific for the VEX extension of SCHED. 
C     Writes a specific section of the VEX file 
C     In this case the PR = $PROCEDURES section 
C     By H.J. van Langevelde, JIVE, 300496 
C
C     Dec 2011 RCW.  Write a block even for FORMAT=NONE scans because
C     some stations need to know about such scans for such activities
C     as reference pointing, or single dish observing.
C 
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink.inc' 
C 
      INTEGER   IXX
      INTEGER   LEN1, ISET, VXGTST
C ----------------------------------------------------------------------
C
C     Write timing proc, only STANDARD for now
C
      WRITE( IVEX, '( A, A1 )' ) '$PROCEDURES', SEP
C
      DO IXX = 1, NPRVEX
C
C        Write a comment for format=none.
C           
         ISET = VXGTST( IXX )
         IF( FORMAT(ISET)(1:4) .EQ. 'NONE' ) THEN
            WRITE( IVEX, '( A1 )' ) COM
            WRITE( IVEX, '( A1, A )' ) COM, 
     1          'The following def is used with FORMAT=NONE.'
         END IF
C      
C        Write a prefix (number)
C      
         WRITE( IVEX, '( A1 )' ) COM
         IF (IXX.LT.100) THEN
            WRITE( IVEX, '( A, A, A1 )' ) 'def ', 
     1          PRLINK(IXX)(1:LEN1(PRLINK(IXX))), SEP      
            WRITE( IVEX, '( 5X, A, 1X, A1, I2.2, A1, A1 )' )
     1          'procedure_name_prefix = ', QOT, IXX, QOT, SEP
         ELSE IF (IXX .LT. 1000) THEN
            WRITE( IVEX, '( A, A, A1 )' ) 'def ', 
     1          PRLINK(IXX)(1:LEN1(PRLINK(IXX))), SEP      
            WRITE( IVEX, '( 5X, A, 1X, A1, I2.2, A1, A1 )' )
     1          'procedure_name_prefix = ', QOT, IXX, QOT, SEP
         ELSE
            CALL ERRLOG(' Cannot format prefix statement ')
         END IF
         WRITE( IVEX, '( 5X, A, I4, 1X, A, A1 )' )
     1        'tape_change = ', 420, 'sec', SEP
         WRITE( IVEX, '( 5X, A, I4, 1X, A, A1 )' )
     1        'headstack_motion = ', 6, 'sec', SEP
         WRITE( IVEX, '( 5X, A, I4, 1X, A, A1 )' )
     1        'new_source_command = ', 5, 'sec', SEP
         WRITE( IVEX, '( 5X, A, I4, 1X, A, A1 )' )
     1             'new_tape_setup = ', 20, 'sec', SEP
         WRITE( IVEX, '( 5X, A, A, 1X, A1, I4, 1X, A, A1 )' )
     1        'setup_always = ', 'on', COL, 20, 'sec', SEP
         WRITE( IVEX, '( 5X, A, A, 1X, A1, I4, 1X, A, A1 )' )
     1              'parity_check = ', 'off', COL, 100, 'sec', SEP
         WRITE( IVEX, '( 5X, A, A, 1X, A1, I4, 1X, A, A1 )' )
     1        'tape_prepass = ', 'off', COL, 600, 'sec', SEP
C      
C        Standard Cal procedure for now...
C      
         WRITE( IVEX, '( 5X, A, A, 1X, A1, I4, 1X, A, 1X, A1, 
     1         1X, A, A1 )' )
     2        'preob_cal  = ', 'on', COL, 10, 'sec', COL, 'preob', SEP

         WRITE( IVEX, '( 5X, A, A, 1X, A1, I4, 1X, A, 1X, A1, 
     1        1X, A, A1 )' )
     2        'midob_cal  = ', 'on', COL, 15, 'sec', COL, 'midob', SEP

         WRITE( IVEX, '( 5X, A, A, 1X, A1, I4, 1X, A, 1X, A1, 
     1        1X, A, A1 )' )
     2        'postob_cal = ', 'on', COL, 0, 'sec', COL, 'postob', SEP
         WRITE( IVEX, '( A, A1 )' ) 'enddef',SEP  
C
      END DO
      WRITE( IVEX, '( A )' ) COMLIN      
      RETURN
      END
