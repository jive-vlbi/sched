      SUBROUTINE VXSTNM( NAMEIN, SERIOUS )
C
C     routine to check NAMEIN is a valid (unquoted) VEX string
C     suitable for VEX def/ref name, alter if necessary
C     no blanks, etc, 32 chars, produces warning if SERIOUS
C     H.J. van Langevelde, JIVE 121200 (from VXSTRN)
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
      CHARACTER NAMEIN*(*)
      LOGICAL SERIOUS
C
      CHARACTER NAMORI*32
      INTEGER LPOS, I, LENGTH, ISTART
      LOGICAL CHRFND, CHANGED
      INTEGER LEN1
C ----------------------------------------------------------------------
C
      CHANGED = .FALSE.
      NAMORI = NAMEIN
      IF( NAMEIN .EQ. ' ' ) NAMEIN = 'Nescio'
      LENGTH = LEN(NAMEIN)
C
C     search for leading blanks
C
      DO LPOS = 1, LENGTH
         IF( NAMEIN(LPOS:LPOS) .NE. BLN ) GOTO 50
      END DO
   50 CONTINUE
      ISTART = LPOS
C
C     remove leading blanks
C
      IF( ISTART .NE. 1 ) THEN
         DO LPOS = 1, LENGTH + 1 - ISTART
            NAMEIN(LPOS:LPOS) = NAMEIN(LPOS+ISTART-1:LPOS+ISTART-1)
         END DO
         DO LPOS = LENGTH - ISTART + 1, LENGTH
            NAMEIN(LPOS:LPOS) = BLN
         END DO
      END IF
C
C     search for first blank, if followed by non blanks change to _
C
      DO LPOS = 1, LENGTH
         IF( NAMEIN(LPOS:LPOS) .EQ. BLN ) THEN
            CHRFND = .FALSE.
            DO I = LPOS+1, LENGTH
               IF( NAMEIN(I:I) .NE. BLN) CHRFND = .TRUE.
            END DO
            IF( CHRFND ) THEN
               NAMEIN(LPOS:LPOS) = '_'
               CHANGED = .TRUE.
            ELSE
C
C              nothing left to do, jump out
C
               GOTO 100         
            END IF
         END IF
      END DO
  100 CONTINUE
C
C     characters &, :, ; $ * are reserved in VEX, I reserve #
C     for numbering, change all occurences to X
C
      DO LPOS = 1, LENGTH
         IF( NAMEIN(LPOS:LPOS) .EQ. COL .OR.
     1       NAMEIN(LPOS:LPOS) .EQ. SEP .OR.
     2       NAMEIN(LPOS:LPOS) .EQ. LNK .OR. 
     3       NAMEIN(LPOS:LPOS) .EQ. '$' .OR.
     4       NAMEIN(LPOS:LPOS) .EQ. COM .OR.
     5       NAMEIN(LPOS:LPOS) .EQ. '#' ) THEN
            CHANGED = .TRUE.
            NAMEIN(LPOS:LPOS) = 'X'
         END IF
      END DO
C
      IF( LENGTH .GT. 32 ) THEN
         DO LPOS = 33, LENGTH
            NAMEIN(LPOS:LPOS) = BLN
         END DO
      END IF
C
      IF( CHANGED .AND. (SERIOUS .OR. DEBUG)) THEN
         WRITE( MSGTXT , '( A, A )' )
     1        'VXSTNM: Changed VEX string: ', NAMORI(1:LEN1(NAMORI))
         CALL WLOG( 1, MSGTXT )
         WRITE( MSGTXT , '( A, A )' )
     1        'VXSTNM:                 to: ', 
     2        NAMEIN(1:LEN1(NAMEIN))
         CALL WLOG( 1, MSGTXT )
      END IF
C     
      RETURN
      END

