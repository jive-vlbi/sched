      SUBROUTINE WRTMSG( TERM, CALLER, MSGNAM )
C
C     Subroutine for SCHED for writing messages to the log file.
C     The messages are found in the MSGFILE which is kept in 
C     the catalogs area.  Normally it is messages.txt.  It is 
C     a user input only because some machines might not be able
C     to deal with logical variables to get the default right.
C
C     This routine opens the message file and reads through until
C     it finds a line that matches ++MSGNAM.  It then writes 
C     everything it finds to the log file until it finds another 
C     line that starts with ++.
C
C     TERM is the WLOG input of that name.  0=> write to log file
C     only.  1=> Write to log file and terminal.
C
      INCLUDE  'sched.inc'
C
      CHARACTER   CALLER*(*), MSGNAM*(*), INLINE*256, OPTEXT*256
      INTEGER     VLBOPE, IERR, LEN1, NCH, TERM
      LOGICAL     INMSG
C ---------------------------------------------------------------------
      INMSG = .FALSE.
C
C     Put request information
C
      MSGTXT = 'WRTMSG: Special message from routine ' //
     1      CALLER(1:LEN1(CALLER)) // ':'
      CALL WLOG( TERM, ' ' )
      CALL WLOG( TERM, MSGTXT )
C
C     Open the message file:
C
      IERR = VLBOPE( IMSG, MSGFILE, 'TEXT', 'OLD', OPTEXT )
      IF( IERR .NE. 1 ) THEN
         CALL WLOG( 1, 'WRTMSG: Could not open message file.' )
         CALL WLOG( 1, OPTEXT )
      ELSE
C
C        Get the message.
C
  100    CONTINUE
         INLINE = ' '
         READ( IMSG, '( A )', END = 200, ERR = 990 ) INLINE
C
C        Process the message.
C
         IF( .NOT. INMSG ) THEN
C
C           Check if this is the start of our message.
C
            NCH = LEN1( INLINE )
            IF( NCH .GE. 3 ) THEN
               IF( INLINE(1:2) .EQ. '++' .AND. 
     1              INLINE(3:NCH) .EQ. MSGNAM(1:LEN1(MSGNAM)) ) THEN
                  INMSG = .TRUE.
               END IF
            END IF
         ELSE
C
C           Check if done.
C
            IF( INLINE(1:2) .EQ. '++' ) GO TO 999
C
C           Check for comment.
C
            IF( INLINE(1:2) .NE. '--' ) THEN
C
C              If not, write the line to the log file.
C              Set to ask user to read the log file.  This will
C              happen for each line, but that is not a big
C              burden.
C
               CALL WLOG( TERM, INLINE )
               READLOG = .TRUE.
C
            END IF
         END IF
C
C        Jump back for the next line.
C
         GO TO 100
C
      END IF
C
C     Jump here if out of data.  Complain if not reading the 
C     message, which means it wasn't found.
C
  200 CONTINUE
      IF( .NOT. INMSG ) THEN
         MSGTXT = 'WRTMSG: Message ' //
     1     MSGNAM // ' not found!'
         CALL WLOG( 1, MSGTXT )
      END IF
      GO TO 999
C
C     Problem reading the file.
C
  990 CONTINUE
      CALL WLOG( 1, 'WRTMSG:  Error reading the messages file.' )
C
C     End of program.  Close the message file.
C
  999 CONTINUE
      IF( IERR .EQ. 1 ) CLOSE( UNIT=IMSG )
C
      RETURN
      END

