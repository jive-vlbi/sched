      SUBROUTINE VLBACHAR( LABEL, NCL, NCHAN, ARRAY, OLDARRAY, 
     1                    OLDCHAN, FIRSTS, IUVBA ) 
C
C     Routine to put an array of setup parameters into a VLBA style
C     control file.  This one is for character variables. 
C
C     Only NCHAN channels will be written.  The current array will be
C     tested against the most recent one and only channels that have
C     changed will be written.  LABEL*NCL is the keyword for the output
C     record (eg SIDEBAND) and IUVBA is the output unit number.  
C     OLDARRAY should be dimensioned the same as ARRAY.  If the 
C     number of channels changes, write out all information for the 
C     new ones.
C
C     Some hoop jumping is done to avoid writing blanks.
C
      CHARACTER   LABEL*(*), OUTLINE*80, FMT1*4, FMT2*4
      CHARACTER   FMTCH*4, ARRAY(*)*(*), OLDARRAY(*)*(*)
      INTEGER     NCHAN, OLDCHAN, KCHAR, NCHAR, LEN1, NCL, NCA
      INTEGER     I, ICHADD, IUVBA
      LOGICAL     FIRSTS, WRTLAB
      SAVE        FMT1, FMT2
C
      DATA        FMT1, FMT2  / '(I1)', '(I2)' /
C----------------------------------------------------------------------
      NCHAR = 0
      WRTLAB = .TRUE.
      IF( FIRSTS ) OLDCHAN = 0
      DO I = 1, NCHAN
C
C        Does this channel need to be processed?
C
         IF( ( FIRSTS .OR. ARRAY(I) .NE. OLDARRAY(I) .OR. I.GT.OLDCHAN)
     1        .AND. ARRAY(I) .NE. 'omit' ) THEN
C
C           Get the format and number of digits needed for the channel 
C           number.
C
            IF( I .LE. 9 ) THEN
               FMTCH = FMT1
               ICHADD = 1
            ELSE
               FMTCH = FMT2
               ICHADD = 2
            END IF
C
C           Get the number of characters in ARRAY(I)
C           Protect against a blank requesting write to a substring
C           of negative length.
C
            NCA = MAX( LEN1( ARRAY(I) ), 1 )
C
C           Get the number of characters for this point.
C
            KCHAR = ICHADD + NCA + 4
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
            WRITE( OUTLINE(NCHAR+1:NCHAR+NCA), '(A)' ) 
     1             ARRAY(I)(1:NCA)
            NCHAR = NCHAR + NCA + 1
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
C     Record number of channels
C
      OLDCHAN = NCHAN
C
C     Write out the last line.
C
      IF( NCHAR .GT. 0 ) WRITE( IUVBA, '(A)' ) OUTLINE(1:NCHAR)
C
      RETURN
      END
