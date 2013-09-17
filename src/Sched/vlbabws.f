      SUBROUTINE VLBABWS( LABEL, NCL, NCHAN, ARRAY, OLDARRAY, OLDCHAN,
     1                    FIRSTS, IUVBA ) 
C
C     Routine to put an array of setup parameters into a VLBA style
C     control file.  This one is for bandwidth or sample rate 
C     specifications. It converts the values in ARRAY, assumed to 
C     be in MHz, into the correct ASCII format for the on-line systems.
C
C     Only NCHAN channels will be written.  The current array will be
C     tested against the most recent one and only channels that have
C     changed will be written.  LABEL*NCL is the keyword for the output
C     record (eg bandwidth) and IUVBA is the output unit number.  
C     OLDARRAY should be dimensioned the same as ARRAY.  If the number 
C     of channels increases, information for the new ones should be 
C     written.
C
C     Some hoop jumping is done to avoid writing blanks.
C
C     The Feb 1991 version of the on-line system dropped the subscript from
C     the samplerate.  Feb 8, 1991 - RCW added code to write this correctly.
C
C     Set MCHAN very high to avoid problems if the number of channels
C     is changed elsewhere.
C
      INTEGER     MCHAN
      PARAMETER   (MCHAN=128)
      CHARACTER   LABEL*(*), OUTLINE*80, FMT1*4, FMT2*4
      CHARACTER   FMTCH*4, MSGLINE*132
      DOUBLE PRECISION  ARRAY(*), OLDARRAY(*)
      DOUBLE PRECISION  LOG2
      INTEGER     NCHAN, OLDCHAN, KCHAR, NCHAR, LEN1, NCL
      INTEGER     I, ICHADD, IDAADD, IUVBA, NBWS
      LOGICAL     FIRSTS, WRTLAB, DEQUAL
      INTEGER     MBWS
      PARAMETER   ( MBWS = 13 )
      CHARACTER*7   SPECBWS(MBWS), PRTBWS(MCHAN)
C
      SAVE        LOG2, SPECBWS, FMT1, FMT2
C
      DATA          LOG2         / 0.693147181 /
      DATA  SPECBWS / '62.5K', '125K', '250K', '500K',
     1               '1M', '2M', '4M', '8M', '16M', '32M',
     2               '64M', '128M', '256M' /
C
      DATA        FMT1, FMT2  / '(I1)', '(I2)' /
C----------------------------------------------------------------------
      NCHAR = 0
      WRTLAB = .TRUE.
      IF( FIRSTS ) OLDCHAN = 0
      IF( NCHAN .GT. MCHAN ) 
     1      CALL ERRLOG( ' VLBABWS: Too many channels.' )
      DO I = 1, NCHAN
C
C        Does this channel need to be processed?
C
         IF( FIRSTS .OR. .NOT. DEQUAL( ARRAY(I), OLDARRAY(I) ) .OR. 
     1       I .GT. OLDCHAN ) THEN
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
C           Determine the ASCII version of the number to be printed.
C
            NBWS = LOG( (ARRAY(I) + 0.02D0 ) / 0.0625D0 ) / LOG2  +  1
            IF( NBWS .LT. 1 .OR. NBWS .GT. MBWS ) THEN
               CALL WLOG ( 1,
     1          'VLBABWS: Bad bandwidth or sample rate specification ' )
               MSGLINE = ' '
               WRITE( MSGLINE, '( 3A, I3 )' ) 
     1           '         label = ', LABEL,
     2           '         channel = ', I
               CALL WLOG( 1, MSGLINE )
               MSGLINE = ' '
               WRITE( MSGLINE, '( A, F8.3, A, I4, A, I4 )' ) 
     1           '         bandwidth/samprate (MHz) = ', ARRAY(I),
     2           '         index of bandwidth = ', NBWS, '  Max=', MBWS
               CALL WLOG( 1, MSGLINE )
               CALL ERRLOG( '         Fix the problem.' //
     1             '  Possible programming issue.' )
            END IF
            PRTBWS(I) = SPECBWS(NBWS)
C
C           Get the format and number of digits needed for the array 
C           number.
C
            IDAADD = LEN1(PRTBWS(I))
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
C           Write in the actual data point.  Has to be different
C           for samplerate!!!
C
            IF( LABEL(1:3) .NE. 'sam' ) THEN
               OUTLINE(NCHAR+1:NCHAR+1) = '('
               WRITE( OUTLINE(NCHAR+2:NCHAR+ICHADD+1), FMTCH ) I
               NCHAR = NCHAR + ICHADD + 2
               OUTLINE(NCHAR:NCHAR) = ','
               WRITE( OUTLINE(NCHAR+1:NCHAR+IDAADD), '(A)' ) 
     1             PRTBWS(I)(1:IDAADD)
               NCHAR = NCHAR + IDAADD + 1
               OUTLINE(NCHAR:NCHAR) = ')'
            ELSE
               IF( I .GT. 1 ) CALL ERRLOG( ' VLBABWS: Only 1 '//
     1            'samplerate allowed.' )
               WRITE( OUTLINE(NCHAR+1:NCHAR+IDAADD), '(A)' ) 
     1             PRTBWS(I)(1:IDAADD)
               NCHAR = NCHAR + IDAADD
            END IF
C
         END IF
C
C        Arrange to sense changes next time.
C
         OLDARRAY(I) = ARRAY(I)
C
      END DO
C
C     Record number of channels.
C
      OLDCHAN = NCHAN
C
C     Write out the last line.
C
      IF( NCHAR .GT. 0 ) WRITE( IUVBA, '(A)' ) OUTLINE(1:NCHAR)
C
      RETURN
      END
