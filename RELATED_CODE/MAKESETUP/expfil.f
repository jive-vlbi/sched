      SUBROUTINE EXPFIL
C
C     Any time a @mode is encountered, replace it with the 
C     appropriate #mode.  Note that the logic is such that any modes
C     imbedded within modes will also be expanded to infinite
C     looping depth (at least until you run out of lines).
C
      INCLUDE  'makeset.inc'
C
      INTEGER     IFL, IL, JL, JM, ICH, JLEN, JCH1, JCHN, LEN1
      CHARACTER   MODENAME*40
C ---------------------------------------------------------------------
C     Loop over the files
C
      DO IFL = 1, NF
C
C        Loop over the lines
C
         IL = 0
  100    CONTINUE
            IL = IL + 1
            ICH = INDEX( FILETEXT(IL,IFL), '@mode' ) 
            jlen = len1(filetext(il,ifl))
            IF( ICH .GT. 0 ) THEN
C
C              This line contains a mode specification.
C              Find the next word after @mode.  That will be the
C              mode name.
C
               CALL NXTWRD( FILETEXT(IL,IFL), ICH + 5, JCH1, JCHN )
               MODENAME = FILETEXT(IL,IFL)(JCH1:JCHN)
C
C              Require that a name is there.
C
               IF( JCH1 .EQ. 0 ) THEN
                  WRITE(*,*) 'EXPFIL: There is an incorrectly '//
     1               'specified mode name in, or nested in,  #file: '
                  WRITE(*,*) FILE(IFL)
                  STOP
               END IF
C
C              Remove the @mode request from the line.
C
               FILETEXT(IL,IFL)(ICH:JCHN) = ' '
C
C              Split the line if there were other things on it.
C              Leave a blank line where the mode expansion will start.
C              This is the same as would happen if the @mode were on
C              a line of its own.  Be careful about a case where we
C              are on the last line of the file.
C
               IF( ICH .GT. 1 ) THEN
                  IF( FILETEXT(IL,IFL)(1:ICH-1) .NE. ' ' ) THEN
                     CALL SHIFT( FILETEXT(1,IFL), MLF, IL+1, 
     1                              FILEL(IFL), 1 )
                     FILETEXT(IL+1,IFL) = FILETEXT(IL,IFL)(JCHN+1:120)
                     FILETEXT(IL,IFL) = FILETEXT(IL,IFL)(1:ICH-1)
                     IL = IL + 1
                  END IF
               END IF
               JLEN = LEN1( FILETEXT(IL,IFL) )
               IF( JLEN .NE. 0 ) THEN
                     CALL SHIFT( FILETEXT(1,IFL), MLF, IL+1, 
     1                           FILEL(IFL), 1 )
                  FILETEXT(IL+1,IFL) = FILETEXT(IL,IFL)
                  FILETEXT(IL,IFL) = ' '
               END IF
C
C              Identify the mode and insert the text, writing over 
C              line IL.
C
               DO JM = 1, NM
                  IF( MODENAME .EQ. MODE(JM) ) THEN
                     IF( MODEL(JM) .GT. 1 ) THEN
                        CALL SHIFT( FILETEXT(1,IFL), MLF, IL+1, 
     1                              FILEL(IFL), MODEL(JM) - 1 )
                     END IF
                     DO JL = 1, MODEL(JM)
                        FILETEXT(JL+IL-1,IFL) = MODETEXT(JL,JM)
                     END DO
C
C                    Set up so that will look at first line of expanded
C                    mode on next pass.
C
                     IL = IL - 1
C
                     GO TO 200
C
                  END IF
               END DO
C
C              Complain about no matching mode.
C
               WRITE(*,'(A,A)') 'EXPFIL:  There is no mode: ', 
     1              MODENAME(1:LEN1(MODENAME))
               WRITE(*,'(A,A)') '         It was requested in file: ',
     1              FILE(IFL)
               STOP
C
  200          CONTINUE
C
            END IF
C
C           Loop back controlled with IF because insertion of modes
C           can change MODEL(IM)
C
            IF( IL .LT. FILEL(IFL) ) GO TO 100
C
      END DO
C
      RETURN
      END



