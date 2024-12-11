      SUBROUTINE SHORTN
C
C     Get a short version of the name of each source.  Look through
C     the aliases for an option.  Use the one from the schedule, if 
C     it qualifies.  Do only on final call (OPENIT=.TRUE.) so all
C     sources are present.  Don't complain yet if there is none - 
C     only do so when it is needed.  It won't be for most (only 
C     SNAP and special VLA).
C
      INCLUDE  'sched.inc'
C
      INTEGER         ISRC, INAME, NCHAR, LEN1
      LOGICAL         SLWARN
      SAVE            SLWARN
      DATA            SLWARN / .TRUE. /
C --------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SHORTN: Starting.' )
C
C     Loop over catalog sources.
C
      DO ISRC = 1, MSRC
         SOUR8(ISRC) = ' '
         DO INAME = 1, MALIAS
C
C           Use the name used in the schedule for the short name
C           if it is short enough.
C
            NCHAR = LEN1( SOURCE(INAME,ISRC) )
            IF( CSUSED(INAME,ISRC) .EQ. '*' .AND.
     1          NCHAR .LE. 8 .AND. NCHAR .GE. 1 ) THEN
               SOUR8(ISRC) = SOURCE(INAME,ISRC)
            END IF
C
C           While at it, don't allow imbedded blanks in any names.
C
            IF( NCHAR .GT. 0 .AND. 
     1          INDEX( SOURCE(INAME,ISRC)(1:NCHAR), ' ' ) .NE. 0 ) THEN
               CALL ERRLOG( 'SHORTN: Imbedded blanks not '//
     1             'allowed in source names: '//SOURCE(INAME,ISRC) )
            END IF
C
C           Warn about possible problems when using long names.
C           This was confirmed and reworded on Jan. 15, 2012.
C           See email on that day from David Gordon.
C
            IF( CSUSED(INAME,ISRC) .EQ. '*' .AND.
     1          NCHAR .GT. 8 .AND. SLWARN ) THEN
               CALL WLOG( 1, 'SHORTN: WARNING - Source '//
     1            'names longer than 8 characters used. ' )
               CALL WLOG( 1, '        This will be a problem if '//
     1            'geodetic software (SOLV etc) is used.' )
               CALL WLOG( 1, '        This might be a problem on '//
     1            'some correlators.' )
               SLWARN = .FALSE.
            END IF
         END DO
C
C        Now if the schedule source name was long, look for a short
C        alias.
C
         IF( SOUR8(ISRC) .EQ. ' ' ) THEN
            DO INAME = 1, MALIAS
               NCHAR = LEN1( SOURCE(INAME,ISRC) )
               IF( NCHAR .LE. 8 .AND. NCHAR .GE. 1 ) THEN
                  SOUR8(ISRC) = SOURCE(INAME,ISRC)
                  IF(CSUSED(INAME,ISRC) .EQ. ' ' ) 
     1               CSUSED(INAME,ISRC) = '+'
                  GO TO 995
               END IF
            END DO
  995       CONTINUE
         END IF
      END DO
C
      RETURN
      END
