      SUBROUTINE VLBAREAL( LABEL, NCL, NCHAN, ARRAY, OLDARRAY, OLDCHAN,
     1                    FMT, NDIG, FIRSTS, IUVBA ) 
C
C     Routine to put an array of setup parameters into a VLBA style
C     control file.  This one is for real numbers.
C     It turns out that when this routine is wanted, the arrays
C     are now double precision, so just change the declares here rather
C     than creating a VLBADBL.
C
C     Only NCHAN channels will be written.  The current array will be
C     tested against the most recent one and only channels that have
C     changed will be written.  LABEL*NCL is the keyword for the output
C     record (eg BBSYN) and IUVBA is the output unit number.  OLDARRAY
C     should be dimensioned the same as ARRAY.  If the number of 
C     channels increases, write out the new ones.
C
      CHARACTER   LABEL*(*), OUTLINE*80, FMT*6
      DOUBLE PRECISION  ARRAY(*), OLDARRAY(*)
      INTEGER     NCHAN, OLDCHAN, KCHAR, NCHAR, NCL
      INTEGER     I, IUVBA, NDIG
      LOGICAL     FIRSTS, WRTLAB, DEQUAL
C----------------------------------------------------------------------
      NCHAR = 0
      WRTLAB = .TRUE.
      IF( FIRSTS ) OLDCHAN = 0
      DO I = 1, NCHAN
C
C        Does this channel need to be processed?
C
         IF( FIRSTS .OR. I .GT. OLDCHAN .OR.
     1       .NOT. DEQUAL( ARRAY(I), OLDARRAY(I) ) ) THEN
C
C           Get the number of characters for this point.
C
            KCHAR = NDIG + 6
            IF( NCHAR + KCHAR .GT. 80 ) THEN
C
C              Write out the previous line.
C
               WRITE( IUVBA, '(A)' ) OUTLINE(1:NCHAR)
C
               NCHAR = 0
               WRTLAB = .TRUE.
            END IF
C
C           Write the beginning of the line.
C
            IF( WRTLAB ) THEN
               OUTLINE = ' '
               NCHAR = NCL
               OUTLINE(1:NCHAR+1) = LABEL(1:NCL)//'='
               NCHAR = NCHAR + 1
               WRTLAB = .FALSE.
            ELSE
               OUTLINE(NCHAR+1:NCHAR+1) = ','
               NCHAR = NCHAR + 1
            END IF
C
C           Write in the actual data point
C
            OUTLINE(NCHAR+1:NCHAR+1) = '('
            WRITE( OUTLINE(NCHAR+1:NCHAR+4), '(A1, I2, A1)' ) 
     1              '(', I, ','
            NCHAR = NCHAR + 4
            WRITE( OUTLINE(NCHAR+1:NCHAR+NDIG), FMT ) ARRAY(I)
            NCHAR = NCHAR + NDIG + 1
            OUTLINE(NCHAR:NCHAR) = ')'
C
         END IF
C
C        Arrange to sense changes next time.
C
         OLDARRAY(I) = ARRAY(I)
C
      END DO
C
C     Record number of channels done.
C
      OLDCHAN = NCHAN
C
C     Write out the last line.
C
      IF( NCHAR .GT. 0 ) WRITE( IUVBA, '(A)' ) OUTLINE(1:NCHAR)
C
      RETURN
      END
