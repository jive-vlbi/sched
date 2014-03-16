      SUBROUTINE FIXFILE
C
C     Read through the new catalog and fix any undesirable
C     characters (mostly in Petrov files).  Then rewind
C     the file to be read again later.  This is a bit odd, but is
C     driven by the use of rdsrc from SCHED on SCHED format files.
C     which still have to be fixed first.
C
C     Note that the user should prepare new file headers, perhaps
C     a copy and paste from the input file headers, that will be
C     copied to the output files external to this program.  In
C     the date specific directories, there is file runsrc that 
C     deals with these headers.
C
      INCLUDE 'rdcat.inc'
      INCLUDE 'newsrc.inc'
C
      INTEGER     LEN1, INLEN, ICH
      CHARACTER   INLINE*256
C -----------------------------------------------------------------
C
  150 CONTINUE
C
C        Read a line of the input file.
C
         READ( 9, '(A)', END=160 ) INLINE
         INLEN = LEN1(INLINE)
C
C        Deal with the -1.00 flag for data not present and the
C        use of "<" which can't be put in a variable.  Use the
C        rather more visible and less likely -9.99 for the flag
C        and a negative to mean less than.
C        Nov 2011 - now he's thrown "<?" at me too.  Set to -9.
C        For the sched file, I think < probably causes the 
C        entry to be interpreted as a character string.  But don't
C        deal with that for now.
C
C        Note that comment lines might also be affected, but this
C        is only the temporary file, so that should not be a
C        problem.
C
         DO ICH = 1, INLEN
            IF( INLINE(ICH:ICH+1) .EQ. '<?' ) THEN
               INLINE(ICH:ICH+1) = '-9'
            END IF
            IF( INLINE(ICH:ICH) .EQ. '<' ) THEN
               INLINE(ICH:ICH) = '-'
            END IF
            IF( INLINE(ICH:ICH+4) .EQ. '-1.00' ) THEN
               INLINE(ICH:ICH+5) = '-9.99'
            END IF
         END DO
C
C        Now write out the line to a temporary file.  Don't write the
C        SRCCAT and ENDCAT lines.
C
         IF( INDEX( INLINE, 'srccat' ) .EQ. 0 .AND. 
     1       INDEX( INLINE, 'endcat' ) .EQ. 0 ) THEN
            WRITE( 10, '( A )' ) INLINE(1:LEN1(INLINE))
         END IF
         GO TO 150
  160 CONTINUE
C
C     Rewind the input file for the proper read.
C
      REWIND( UNIT=10 )
C
      RETURN
      END
