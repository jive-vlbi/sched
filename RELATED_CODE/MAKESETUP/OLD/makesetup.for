      PROGRAM MAKESETUP
C
C     This file reads a master file of setup information and writes
C     out setup files for use by SCHED.  The intent is to make 
C     maintenance of setup files easier.
C
C     The master file contains two types of information.  They
C     are "mode" specifications delineated by #mode and #end
C     (#mode starting in column1, #end being the last word of a line.) 
C     and "file" information delineated by #file and #end.  
C     The #mode lines and #file lines must
C     have a name beginning in column 7 (1 blank).  The #file
C     name will be used as the file name of output setup file.
C     The #mode name will be used as a reference.
C
C     Anywhere in either a #mode or #file section, a line containing
C     @mode and a name will be given.  That line will be replaced
C     by everything in the corresponding #mode section on output.
C     It is also possible to put a string of @mode designations on
C     the #file line.
C
C     A mode should not reference another mode that references 
C     yet another mode.  The code can only go 2 deep.
C
C     The #mode and #file sections can be in any order.  Everything
C     will be read in before anything is written out so any #mode
C     sections will be available for expansion by the time it is
C     needed.
C
      INTEGER    MF, MM, ML, MW
      PARAMETER  (MF=400)       ! Maximum number of output files.
      PARAMETER  (MM=100)       ! Maximum number of "modes".
      PARAMETER  (ML=50)        ! Maximum number of lines in a section.
      PARAMETER  (MW=30)        ! Maximum number of words in a line.
C
      CHARACTER  INFILE*80, INLINE*80
      CHARACTER  FILE(MF)*40, FILETEXT(ML,MF)*80
      CHARACTER  MODE(MM)*40, MODETEXT(ML,MM)*80
      CHARACTER  WORD(MW)*80
C
      INTEGER    NF, NM, NL, IF, IL1, IL2, IL3, IM1, IM2, IW, ICH
      INTEGER    NWORDS, WLEN(MW)
      INTEGER    FILEL(MF), MODEL(MM), NUMLINE
C
C---------------------------------------------------------------------------
C
C      WRITE(*,'(A)') ' Input master file:'
C      READ(*,'(A)')  INFILE
      INFILE = 'Master_setups'
      OPEN(UNIT=10,FILE=INFILE,FORM='FORMATTED',STATUS='OLD')
C
C     Initialize some things:
C
      NF = 0
      NM = 0
      NUMLINE = 0
C
C     Begin outer loop by reading next line.  This should always
C     be a "#mode" or "#file" line.
C
  100 CONTINUE
         NWORDS = MW
         CALL GETLINE( WORD, WLEN, NWORDS, INLINE, 10, 6 )
         IF( NWORDS .EQ. -1 ) GO TO 500
         NUMLINE = NUMLINE + 1
         IF(INLINE .EQ. ' ' ) GO TO 100
C
C        Get a mode definition.
C
         IF( WORD(1) .EQ. '#mode' ) THEN
            NM = NM + 1
            IF( NM .GT. MM ) THEN
               WRITE(*,*) ' Too many modes.  Fix MAKESETUP.'
               STOP
            END IF
            MODE(NM) = WORD(2)
C
C           Read the mode lines.
C
            NL = 0
  200       CONTINUE
               NL = NL + 1
               IF( NL .GT. ML ) THEN
                  WRITE(*,*) ' Too many lines in mode '//MODE(NM)
                  STOP
               END IF
               MODETEXT(NL,NM) = ' '
               NWORDS = MW
               CALL GETLINE( WORD, WLEN, NWORDS, MODETEXT(NL,NM),10,6)
               IF( NWORDS .EQ. -1 ) THEN
                  WRITE(*,*) 'ERROR: Input ended in middle of mode '//
     1                'definition - stop.'
                  STOP
               END IF
               NUMLINE = NUMLINE + 1
C
C              Look for a mode spec.  If there is one, be sure
C              it is on a line of its own and reformat for what is
C              wanted later.
C
               IF( INDEX( MODETEXT(NL,NM), '@mode' ) .GT. 0 ) THEN
                  IF( NWORDS .NE. 2 ) THEN
                     WRITE(*,*) 'A mode invocation not on the #file ',
     1                   'line must have a line of its own.'
                     STOP
                  END IF
                  MODETEXT(NL,NM) = WORD(1)(1:WLEN(1)) // ' ' //
     1                  WORD(2)(1:WLEN(2))
               END IF
C
C              End of mode if #end is last word of line.
C
               IF( WORD(NWORDS) .NE. '#end' ) THEN
                   IF( WORD(NWORDS)(1:1) .EQ. '#' ) THEN
                      WRITE(*,*) ' Unexpected # found on line ',
     1                    NUMLINE, ' - stop '
                      STOP
                   END IF
                   GO TO 200
               ELSE
                   IF( NWORDS .EQ. 1 ) THEN
                      MODEL(NM) = NL - 1
                   ELSE
                      ICH = INDEX( MODETEXT(NL,NM), '#end' ) - 1
                      MODETEXT(NL,NM) = MODETEXT(NL,NM)(1:ICH) // ' '
                      MODEL(NM) = NL
                   END IF
               END IF
C
C        Get a file definition.
C
         ELSE IF( WORD(1) .EQ. '#file' ) THEN
            NF = NF + 1
            IF( NF .GT. MF ) THEN
               WRITE(*,*) ' Too many files.  Fix MAKESETUP.'
               STOP
            END IF
            FILE(NF) = WORD(2)
            NL = 0
C
C           Pick any mode references off the #file line.  Just make
C           a FILETEXT lines with each reference.  Also look for #end
C
            IF( NWORDS .GE. 4 ) THEN
               DO IW = 3, NWORDS, 2
                  IF( WORD(IW) .EQ. '@mode' ) THEN
	             NL = NL + 1
                     FILETEXT(NL,NF) = WORD(IW)(1:WLEN(IW)) // ' ' //
     1                   WORD(IW+1)(1:WLEN(IW+1))
                  END IF
                  IF( WORD(IW) .EQ. '#end' ) THEN
                     FILEL(NF) = NL
                     GO TO 100
                  END IF
               END DO
            END IF
C
C           Read the file lines.
C
  300       CONTINUE
               NL = NL + 1
               IF( NL .GT. ML ) THEN
                  WRITE(*,*) ' Too many lines in file '//FILE(NF)
                  STOP
               END IF
               NWORDS = MW
               CALL GETLINE( WORD, WLEN, NWORDS, FILETEXT(NL,NF),10,6)
               IF( NWORDS .EQ. -1 ) THEN
                  WRITE(*,*) 'ERROR: Input ended in middle of file '//
     1                'definition - stop.'
                  STOP
               END IF
               NUMLINE = NUMLINE + 1
C
C              Look for a mode spec.  If there is one, be sure
C              it is on a line of its own and reformat for what is
C              wanted later.
C
               IF( INDEX( FILETEXT(NL,NF), '@mode' ) .GT. 0 ) THEN
                  IF( NWORDS .NE. 2 ) THEN
                     WRITE(*,*) 'A mode invocation not on the #file ',
     1                   'line must have a line of its own.'
                     WRITE(*,*) FILETEXT(NL,NF)
     1                          (1:LEN1(FILETEXT(NL,NF)))
                     STOP
                  END IF
                  FILETEXT(NL,NF) = WORD(1)(1:WLEN(1)) // ' ' //
     1                  WORD(2)(1:WLEN(2))
               END IF
C
C              Look for the last line.  If not there, go back for more.
C
               IF( WORD(NWORDS) .NE. '#end' ) THEN
                   IF( WORD(NWORDS)(1:1) .EQ. '#' ) THEN
                      WRITE(*,*) ' Unexpected # found on line ',
     1                    NUMLINE, ' - stop '
                      STOP
                   END IF
                   GO TO 300
C
C              If it is the last line, wrap up.
C
               ELSE
                   IF( NWORDS .EQ. 1 ) THEN
                      FILEL(NF) = NL - 1
                   ELSE
                      ICH = INDEX( FILETEXT(NL,NF), '#end' ) - 1
                      FILETEXT(NL,NF) = FILETEXT(NL,NF)(1:ICH)//' '
                      FILEL(NF) = NL
                   END IF
               END IF
         END IF
         GO TO 100
C
C     Now write out the files.
C
  500 CONTINUE
      WRITE(*,'(A,I3,A,I3,A)' ) ' Read ', NM, ' modes and ', NF, 
     1    ' files.'
C
      DO IF = 1, NF
         OPEN(UNIT=11,FILE=FILE(IF),FORM='FORMATTED',STATUS='UNKNOWN')
C
C        Insert warning about lost modifications.
C
         WRITE( 11, '(A,A)' ) '! ', FILE(IF)
         WRITE( 11, '(A)' ) '!      Setup file produced by MAKESETUP.'
C
         DO IL1 = 1, FILEL(IF)
C
C           Replace a mode reference with the mode.  Otherwise write
C           out the line.
C
            IF( FILETEXT(IL1,IF)(1:5) .EQ. '@mode' ) THEN
               DO IM1 = 1, NM
                  IF( FILETEXT(IL1,IF)(7:46) .EQ. MODE(IM1) ) GO TO 510
               END DO
               WRITE(*,*) ' Mode not found: '//FILETEXT(IL1,IF)
               STOP
C
  510          CONTINUE
               DO IL2 = 1, MODEL(IM1)
C
C                 Deal with a nested mode.
C
                  IF( MODETEXT(IL2,IM1)(1:5) .EQ. '@mode' ) THEN
                     DO IM2 = 1, NM
                        IF( MODETEXT(IL2,IM1)(7:46) .EQ. MODE(IM2) ) 
     1                       GO TO 520
                     END DO
                     WRITE(*,*) ' Mode not found: '//MODETEXT(IL2,IM1)
                     STOP
C
  520                CONTINUE
                     DO IL3 = 1, MODEL(IM2)
                        WRITE(11,'(A)') 
     1                    MODETEXT(IL3,IM2)(1:LEN1(MODETEXT(IL3,IM2)))
                     END DO
                  ELSE
                     WRITE(11,'(A)') 
     1                    MODETEXT(IL2,IM1)(1:LEN1(MODETEXT(IL2,IM1)))
                  END IF
               END DO
            ELSE
               WRITE(11,'(A)') 
     1                    FILETEXT(IL1,IF)(1:LEN1(FILETEXT(IL1,IF)))
            END IF
C
         END DO
         CLOSE( UNIT = 11 )
      END DO
C
  990 CONTINUE
      STOP
      END
      INTEGER FUNCTION LEN1(S)
      CHARACTER*(*) S
C
C Find the length of a character string excluding trailing blanks.
C A blank string returns a value of 0.
C
C Argument:
C  S      (input)  : character string.
C
C Returns:
C  LEN1            : number of characters in S, excluding trailing
C                    blanks, in range 0...LEN(S). A blank string
C                    returns a value of 0.
C
C Subroutines required:
C  None
C
C Fortran 77 extensions:
C  None
C
C History:
C  1987 Nov 12 - TJP.
C-----------------------------------------------------------------------
      INTEGER  I
C
      IF (S.EQ.' ') THEN
          LEN1 = 0
      ELSE
          DO 10 I=LEN(S),1,-1
              LEN1 = I
              IF (S(I:I).NE.' ') GOTO 20
   10     CONTINUE
          LEN1 = 0
   20     CONTINUE
      END IF
      END


