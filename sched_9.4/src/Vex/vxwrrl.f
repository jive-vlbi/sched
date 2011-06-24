      SUBROUTINE VXWRRL
      IMPLICIT NONE
C 
C     Routine specific for the VEX extension of SCHED. 
C     Writes a specific section of the VEX file 
C     In this case the RL = $ROLL section 
C     By H.J. van Langevelde, JIVE, 300496 
C     Updated fro the real roll and 2 heads 011200
C 
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink.inc' 
C      
      INTEGER   IRL, KS, J, ISTA, IMODE, IHEAD
      INTEGER   ROLLBY, NHDSTK, VXNHDS
      INTEGER   LEN1
      CHARACTER LINE*132
      LOGICAL   DOWARN
C ----------------------------------------------------------------------
C
      LINE = ' ' 
      DOWARN = .FALSE.
      WRITE( IVEX, '( A, A1 )' ) '$ROLL', SEP   
C
      DO IRL = 1, NRLVEX
         KS = RLISSET(IRL)
         WRITE( IVEX, '( A1 )' ) COM
         WRITE( IVEX, '( A, A, A1 )' ) 'def ',
     1        RLLINK(IRL)(1:LEN1(RLLINK(IRL))), SEP
         CALL VXSTLI( IRL, NSTARL, ISTARL )
C
C        Find the roll, there are four modes, for auto we
C        have to come up with the setting...
C
         ROLLBY = -1
         IF( BARREL(KS) .EQ. 'roll_off' ) ROLLBY = 0
         IF( BARREL(KS) .EQ. 'roll_16' ) ROLLBY = 16
         IF( BARREL(KS) .EQ. 'roll_8' ) ROLLBY = 8
         IF( BARREL(KS) .EQ. 'roll_auto' ) THEN
            IF( FORMAT(KS)(1:7) .EQ. 'MARKIII' ) THEN
               ROLLBY = 0
            ELSE
               IF( TAPEMODE(KS) .EQ. 1 ) ROLLBY = 16
               IF( TAPEMODE(KS) .EQ. 2 ) ROLLBY = 16
               IF( TAPEMODE(KS) .EQ. 4 ) ROLLBY = 8
               IF( TAPEMODE(KS) .EQ. 8 ) ROLLBY = 8
               IF( TAPEMODE(KS) .EQ. 0 ) ROLLBY = 0
            END IF
         END IF
C
C        Also find number of heads
C
         NHDSTK = VXNHDS( KS )
C
C        Write a comment
C
         WRITE( IVEX, '( A1, 4X, A, A, A, I2, A, I1, A )' ) COM, 
     1        'barrel-roll set to ''', BARREL(KS)(1:LEN1(BARREL(KS))), 
     2        ''', so reverse-roll by ', ROLLBY, ' (',NHDSTK,' head)'
         
C
C        Standard rolls all have the same rate
C
         IF( ROLLBY .LT. 0) THEN
            WRITE( MSGTXT, '( A, A )' ) 
     1          'VXWRRL: unknown barrel-roll:', 
     2          BARREL(KS)(1:LEN1(BARREL(KS)))
            CALL ERRLOG( MSGTXT )
         END IF
         IF( ROLLBY .EQ. 0 ) THEN
C
C           roll off now defined in VEX 1.5!
C
            WRITE( IVEX, '( 5X, A, A1 )' )  
     1           'roll = off', SEP 
         ELSE
            WRITE( IVEX, '( 5X, A, A1 )' ) 
     1           'roll = on', SEP 
            WRITE( IVEX, '( 5X, A, A1 )' ) 
     1          'roll_reinit_period = 2 sec', SEP 
            WRITE( IVEX, '( 5X, A, A1 )' ) 
     1          'roll_inc_period = 1', SEP 
C
C           write 2 comment lineS
C
            LINE = ' '
            WRITE( LINE(1:24), '( A1, 15X, A2, 1X, A4, 1X )' ) 
     1           COM, 'inpt'
            WRITE( LINE(25:80), '( A )' )
     1           ' writing inp trck to this output sequence'
            WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
            LINE = ' '
            WRITE( LINE(1:24), '( A1, 15X, A2, 1X, A4, 1X )' )
     1           COM, 'hd','trck'
            DO J = 1, ROLLBY
               WRITE( LINE(25+(J-1)*3:24+J*3), '( I2, 1X )' )  (J-1)
            END DO
            WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
         ENDIF
C     
C        if there is roll, warn about PCFS not implementing it
C     
         IF( ROLLBY .NE. 0 ) THEN
            DO IMODE = 1, NMDVEX 
               DO ISTA = 1, NSTARL(IRL,IMODE)
                  IF( CONTROL(STANUM(ISTARL(ISTA,IRL,IMODE))) 
     1                .EQ. 'VEX' .AND. 
     2                .NOT. MODETEST( KS ) ) DOWARN = .TRUE.
               END DO
            END DO
C
C        At this point we need to know how many heads will roll
C


C
C        first the 8roll, can happen tpmode 4, but also for 2 and 1
C        so I guess we give roll for all 32 tracks
C
            DO IHEAD = 1, NHDSTK
               IF( ROLLBY .EQ. 8 ) THEN
                  CALL VXRL8( IHEAD )
               END IF
C
C        the 16 track roll is a lot more complicated
C
               IF( ROLLBY .EQ. 16 ) THEN
                  CALL VXRL16( IHEAD )
               END IF 
            END DO
         END IF
         WRITE( IVEX, '( A, A1 )' ) 'enddef',SEP
      END DO
      WRITE( IVEX, '( A )' ) COMLIN
C
C     issue a warning for rolling if necessary... 
C
      IF( DOWARN ) 
     1       CALL WLOG( 1,'VXWRRL: WARNING, barrel rolling not yet '//
     2       'permitted for the JIVE correlator')
      IF( DEBUG ) CALL WLOG( 1, 'VXWRRL: Done with setups to VEX.' )
C
      RETURN
      END
