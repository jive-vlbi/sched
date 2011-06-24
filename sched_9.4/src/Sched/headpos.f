      INTEGER FUNCTION HEADPOS( TPINDX, SYSTEM, IHEAD )
C
C     Function for SCHED called by VLBA and perhaps other routines that
C     returns the head position in microns for the head index position.a
C     IHEAD is the number of the head for this call.  For VLBA systems,
C     it is always 1, but put in the hooks for otherwise.  For Mark IV,
C     it can be one or two.
C
C
      INTEGER     TPINDX, HEADA(14), HEADB(6), HEADB2(6), IHEAD
      CHARACTER   SYSTEM*(*), MSG*10
      SAVE  HEADA, HEADB
C
C     Set the head positions for SYSTEM = VLBA14.
C
      DATA  HEADA / -319,   31, -271,  79, -223, 127, -175,
     1               175, -127,  223, -79,  271, -31,  319 /
C
C     Set the head pos for SYSTEM = MKIV2H
C
      DATA  HEADB  / -301,  47, -253,  95, -205, 143 /
      DATA  HEADB2 / -143, 205,  -95, 253,  -47, 301 /
C  ---------------------------------------------------------------------
      IF( SYSTEM .EQ. 'VLBA14' ) THEN
         IF( TPINDX .GE. 1 .AND. TPINDX .LE. 14 ) THEN
            IF( IHEAD .EQ. 1 .OR. IHEAD .EQ. 2 ) THEN
               HEADPOS = HEADA( TPINDX)
            ELSE
               WRITE( MSG, '(I10)' ) IHEAD
               CALL ERRLOG( 'HEADPOS: Bad head number: ' // MSG )
            END IF
         ELSE
            WRITE( MSG, '(I10)' ) TPINDX
            CALL ERRLOG( 'HEADPOS: Bad index position: ' // MSG )
         END IF
      ELSE IF( SYSTEM .EQ. 'MKIV2H' ) THEN
         IF( TPINDX .GE. 1 .AND. TPINDX .LE. 6 ) THEN
            IF( IHEAD .EQ. 1 ) THEN
               HEADPOS = HEADB( TPINDX)
            ELSE IF( IHEAD .EQ. 2 ) THEN
               HEADPOS = HEADB2( TPINDX)
            ELSE
               WRITE( MSG, '(I6)' ) IHEAD
               CALL ERRLOG( 'HEADPOS: Bad head number: '//MSG(1:6) )
            END IF
         ELSE
            WRITE( MSG, '(I6)' ) TPINDX
            CALL ERRLOG( 'HEADPOS: Bad index position: '//MSG(1:6) )
         END IF
      ELSE
         MSG = SYSTEM
         CALL ERRLOG( 'HEADPOS: Unknown head position system: '//MSG )
      END IF
C
      RETURN
      END
