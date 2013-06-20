      SUBROUTINE VLBAINT( LABEL, NCL, NCHAN, ARRAY, OLDARRAY, OLDCHAN,
     1                    FIRSTS, IUVBA ) 
C
C     Routine to put an array of setup parameters into a VLBA style
C     control file.  This one is for integer numbers.
C
C     Only NCHAN channels will be written.  The current array will be
C     tested against the most recent one and only channels that have
C     changed will be written.  LABEL*NCL is the keyword for the output
C     record (eg LEVEL) and IUVBA is the output unit number.  OLDARRAY
C     should be dimensioned the same as ARRAY.  If the number of 
C     channels increases in the middle of a schedule, the new ones 
C     must be written.
C
C     Some hoop jumping is done to avoid writing blanks.
C
      CHARACTER   LABEL*(*), OUTLINE*80, MSGLINE*80
      CHARACTER   FMTCH*4, FMTDAT*4
      INTEGER     ARRAY(*), OLDARRAY(*)
      INTEGER     NCHAN, OLDCHAN, KCHAR, NCHAR, NCL, NDIG
      INTEGER     I, ICHADD, IDAADD, IUVBA
      LOGICAL     FIRSTS, WRTLAB
C----------------------------------------------------------------------
      NCHAR = 0
      WRTLAB = .TRUE.
      IF( FIRSTS ) OLDCHAN = 0
      DO I = 1, NCHAN
C
C        Does this channel need to be processed?
C
         IF( FIRSTS .OR. ARRAY(I) .NE. OLDARRAY(I) .OR. 
     1       I .GT. OLDCHAN ) THEN
C
C           Get the format and number of digits needed for the 
C           channel number.
C
            IF( I .LE. 9 ) THEN
               FMTCH = '(I1)'
               ICHADD = 1
            ELSE
               FMTCH = '(I2)'
               ICHADD = 2
            END IF
C
C           Get the format and number of digits needed for the array 
C           number.
C
            IF( ARRAY(I) .EQ. 0 ) THEN
               NDIG = 1
            ELSE
               NDIG = AINT( LOG10( ABS( FLOAT( ARRAY(I) ) ) ) ) + 1
               IF( ARRAY(I) .LT. 0 ) NDIG = NDIG + 1
            END IF
            IF( NDIG .GT. 9 ) THEN
               MSGLINE = ' A value in '//LABEL(1:NCL)//
     1             ' is too large for format.'
               CALL ERRLOG( MSGLINE )
            END IF
            WRITE( FMTDAT, '( A, I1, A )' ) '(I', NDIG, ')'
            IDAADD = NDIG
C
C           Get the number of characters for this point.
C
            KCHAR = ICHADD + IDAADD + 4
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
            WRITE( OUTLINE(NCHAR+2:NCHAR+ICHADD+1), FMTCH ) I
            NCHAR = NCHAR + ICHADD + 2
            OUTLINE(NCHAR:NCHAR) = ','
            WRITE( OUTLINE(NCHAR+1:NCHAR+IDAADD), FMTDAT ) ARRAY(I)
            NCHAR = NCHAR + IDAADD + 1
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
