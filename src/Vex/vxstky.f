      SUBROUTINE VXSTKY( NAMEIN, SERIOUS )
C
C     function to check NAMEIN is a valid (unquoted) VEX string
C     suitable for a VEX key value, quote it if not.
C     no blanks, etc, 32 chars, produces warning if SERIOUS
C     H.J. van Langevelde, JIVE 121200
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
      CHARACTER NAMEIN*(*)
      LOGICAL SERIOUS
C
      INTEGER MAXSTR
      PARAMETER ( MAXSTR = 32 )
C
      CHARACTER NAMOUT*(MAXSTR)
      INTEGER LPOS, ILAST, ISTART, IOUT
      LOGICAL TOQUOT
      INTEGER LEN1
C ----------------------------------------------------------------------
C
      TOQUOT = .FALSE.
      IF( NAMEIN .EQ. ' ' ) NAMEIN = 'Nescio'
      ILAST = LEN1(NAMEIN)
C
C     search for leading blanks
C
      DO LPOS = 1, ILAST
         IF( NAMEIN(LPOS:LPOS) .NE. BLN ) GOTO 50
      END DO
   50 CONTINUE
      ISTART = LPOS
C
C     characters &, :, ; $ * are reserved in VEX, I reserve #
C     for numbering, Quote occurences
C
      DO LPOS = 1, ILAST
         IF( NAMEIN(LPOS:LPOS) .EQ. COL .OR.
     1       NAMEIN(LPOS:LPOS) .EQ. SEP .OR.
     2       NAMEIN(LPOS:LPOS) .EQ. LNK .OR. 
     2       NAMEIN(LPOS:LPOS) .EQ. BLN .OR. 
     3       NAMEIN(LPOS:LPOS) .EQ. '$' .OR.
     4       NAMEIN(LPOS:LPOS) .EQ. COM .OR.
     5       NAMEIN(LPOS:LPOS) .EQ. '#' ) THEN
            TOQUOT = .TRUE.
         END IF
      END DO
C
C     Copy but only 32 or 30 positions
C
      ILAST = MIN( ILAST, MAXSTR+ISTART )
      IOUT = 1
      IF ( TOQUOT ) THEN
         NAMOUT(1:1)= QOT
         IOUT = 2
         ILAST=MIN( ILAST,MAXSTR-3+ISTART)
      ENDIF

      DO LPOS = ISTART, ILAST
         NAMOUT(IOUT:IOUT) = NAMEIN(LPOS:LPOS)
         IOUT = IOUT + 1
      END DO

      IF ( TOQUOT ) THEN
         NAMOUT(IOUT:IOUT)= QOT
         IOUT = IOUT+1
      ENDIF
C
      DO LPOS = IOUT, MAXSTR
         NAMOUT(LPOS:LPOS) = BLN
      END DO
C
C
      IF( TOQUOT .AND. ( SERIOUS .OR. DEBUG ) ) THEN
         WRITE( MSGTXT , '( A, A, A )' )
     1       'VXSTKY: Quoting VEX string: <', 
     2       NAMOUT(1:LEN1(NAMOUT)),'> '
         CALL WLOG( 1, MSGTXT )
      END IF
C     
      NAMEIN = NAMOUT
C
      RETURN
      END

