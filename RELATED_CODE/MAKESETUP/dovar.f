      SUBROUTINE DOVAR
C
C     Deal with variables in file specifications for MAKESETUP.
C     Note that the modes are already expanded so we don't need to
C     worry about "@mode"
C
      INCLUDE   'makeset.inc'
C
      INTEGER     IF, IL, JL, ICH, I1, IN, NC, LEN1, LENVAR, LENNAM
      INTEGER     ICS, ICQ, NFR, KL, ICE, IDUMP, JDUMP, ISEM
      INTEGER     VARLINES, NBK, NVL, IX1, IXN, DLINES
      CHARACTER   VARNAM*40, VARVAL*120, VARVAL2*120
      CHARACTER   FRONT*120, BACK*120
C ---------------------------------------------------------------------
C     Loop over files.
C
      DO IF = 1, NF
C
C        To avoid worrying about how many variables there are, 
C        deal with them one at a time.
C
C        Look for a variable.  If found, get name and value.
C        Use a GOTO loop since FILEL might change.
C
         IL = 0
  100    CONTINUE
            IL = IL + 1
            VARNAM = ' '
            ICH = INDEX( FILETEXT(IL,IF), '#' )
            IF( ICH .NE. 0 ) THEN
C
C              We've got a variable definition.
C              First, break the line at ";" if there is one.
C
               NC = LEN1( FILETEXT(IL,IF) )
               ISEM = INDEX( FILETEXT(IL,IF)(ICH:120), ';' )
               IF( ISEM .GT. 0 ) THEN
                  ISEM = ISEM + ICH - 1
                  CALL SHIFT( FILETEXT(1,IF), MLF, IL+1, FILEL(IF), 1 )
                  FILETEXT(IL+1,IF) = FILETEXT(IL,IF)(ISEM+1:NC)
                  FRONT = FILETEXT(IL,IF)(1:ISEM-1)
                  FILETEXT(IL,IF) = ' '
                  FILETEXT(IL,IF) = FRONT
               END IF
C
C              Now process the variable as if it is at the end of the
C              line.  Get NC again because above may have shortened it.
C
               CALL NXTWRD( FILETEXT(IL,IF), ICH+1, I1, IN )
               VARNAM = FILETEXT(IL,IF)(I1:IN)
               LENNAM = IN - I1 + 1
               NC = LEN1( FILETEXT(IL,IF) )
               VARLINES = 1
               IF( NC .EQ. IN ) THEN
                  VARVAL = ' '
                  LENVAR = 0
               ELSE
                  VARVAL = FILETEXT(IL,IF)(IN+2:NC)
                  LENVAR = NC - IN - 1
C
C                 Check for an "=" sign next - a common error.
C
                  CALL NXTWRD( VARVAL, 1, IX1, IXN )
                  IF( VARVAL(IX1:IX1) .EQ. '=' ) THEN
                     WRITE(*,*) 'DOVAR: Unexpected = sign in variable'
                     WRITE(*,*) '  in: ', FILE(IF)(1:LEN1(FILE(IF)))
                  END IF
C
C                 Deal with a second line.  If there, save it
C                 and get rid of it.
C
                  IF( FILETEXT(IL,IF)(NC:NC) .EQ. ',' .AND. 
     1                FILEL(IF) .GE. IL + 1 ) THEN
                     VARLINES = 2
                     VARVAL2 = FILETEXT(IL+1,IF)
                     CALL SHIFT( FILETEXT(1,IF), MLF, IL+2, 
     1                           FILEL(IF), -1 )
                  END IF
               END IF
C
C              Got a variable. 
C              Blank it out of the current line.
C              Then get rid of the current line if it is now blank.
C
               FILETEXT(IL,IF)(ICH:NC) = ' '
               IF( FILETEXT(IL,IF) .EQ. ' ' ) THEN
                  CALL SHIFT( FILETEXT(1,IF), MLF, IL+1, FILEL(IF), -1 )
                  IL = IL - 1
               END IF
C
C              Now search through the the whole file and make
C              substitutions.  Substitute where $VARNAM is found.
C              Where ?varnam is seen, only include to ?end if the
C              variable is not blank.
C
               JL = 0
 200           CONTINUE
                  JL = JL + 1
                  ICS = INDEX( FILETEXT(JL,IF), '@'//VARNAM(1:LENNAM) )
                  NC = LEN1( FILETEXT(JL,IF) )
C
C                 Do a substitution.
C
                  IF( ICS .NE. 0 ) THEN
                     FRONT = ' '
                     NFR = 0
                     BACK = ' '
                     IF( ICS .GT. 1 ) THEN
                        FRONT = FILETEXT(JL,IF)(1:ICS-1)//' '
                        NFR = ICS - 1
                     END IF
                     IF( ICS + LENNAM  .LT. NC ) THEN
                        BACK = FILETEXT(JL,IF)(ICS+LENNAM+1:NC)
                        NBK = LEN1( BACK )
                     END IF
C
C                    Assemble the line.  Break it if necessary.
C                    Add second variable line if necessary.
C
C
                     FILETEXT(JL,IF) = FRONT
                     NVL = NFR
                     IF( NFR + LENVAR .GT. 120 ) THEN
                        CALL SHIFT( FILETEXT(1,IF), MLF, JL + 1, 
     1                              FILEL(IF), 1 )
                        IF( IL .GE. JL + 1 ) IL = IL + 1
                        JL = JL + 1
                        FILETEXT(JL,IF) = VARVAL
                        NVL = LENVAR
                     ELSE
                        FILETEXT(JL,IF)(1+NFR:120) = VARVAL
                        NVL = NFR + LENVAR
                     END IF
C
C                    Add the second line if needed.
C
                     IF( VARLINES .EQ. 2 ) THEN
                        CALL SHIFT( FILETEXT(1,IF), MLF, JL + 1, 
     1                              FILEL(IF), 1 )
                        IF( IL .GE. JL + 1 ) IL = IL + 1
                        JL = JL + 1
                        FILETEXT(JL,IF) = VARVAL2
                     END IF
C
C                    Add the back of the line.
C
                     IF( NBK .GT. 0 .AND.
     1                   ( VARLINES .EQ. 2 .OR. 
     2                     NVL + NBK .GT. 120 ) ) THEN
                        CALL SHIFT( FILETEXT(1,IF), MLF, JL + 1, 
     1                              FILEL(IF), 1 )
                        IF( IL .GE. JL + 1 ) IL = IL + 1
                        JL = JL + 1
                        FILETEXT(JL,IF) = BACK
                     ELSE IF( NBK .GT. 0 ) THEN
                        FILETEXT(JL,IF)(1+NVL:120) = BACK
                     END IF
C
                  END IF
C
C                 Look for ?varnam and delete some stuff if requested.
C
                  ICQ = INDEX( FILETEXT(JL,IF), '?'//VARNAM(1:LENNAM) )
                  IF( ICQ .NE. 0 ) THEN
                     IF( LENVAR .EQ. 0 ) THEN
C
C                       Need to delete a section.
C                       Look for the end of data to be omitted.
C
                        DO KL = JL, FILEL(IF)
                           ICE = INDEX( FILETEXT(KL,IF), '?end' )
                           IF( ICE .NE. 0 ) GO TO 300
                        END DO
                        WRITE(*,*) 'DOVAR: Omit region not ended.'
                        WRITE(*,*) FILE(IF)
                        STOP
C
  300                   CONTINUE
C
C                       Now delete the required region.
C
                        IF( JL .EQ. KL ) THEN
C
C                          Whole deletion region is on one line.
C
                           IF( ICQ .GT. 1 ) THEN
                              FILETEXT(JL,IF) = FILETEXT(JL,IF)(1:ICQ-1)
     1                          //FILETEXT(JL,IF)(ICE+4:120)
                           ELSE
                              FILETEXT(JL,IF) = 
     1                            FILETEXT(JL,IF)(ICE+4:120)
                           END IF
                        ELSE
C
C                          Several lines are involved.
C                          Keep first line up to the ?varnam.
C
                           IF( ICQ .GT. 1 ) THEN
                              FILETEXT(JL,IF) = FILETEXT(JL,IF)(1:ICQ-1)
                           ELSE
                              FILETEXT(JL,IF) = ' '
                           END IF
C
C                          Determine IDUMP, the first line to delete
C                          completely.
C
                           IDUMP = JL
                           IF( LEN1( FILETEXT(JL,IF) ) .NE. 0 ) 
     1                         IDUMP = JL + 1
C
C                          Remove everything in last line up to the
C                          ?end
C
                           FILETEXT(KL,IF) = FILETEXT(KL,IF)(ICE+4:120)
C
C                          Now see how far to delete whole lines.
C
                           JDUMP = KL
                           IF( LEN1( FILETEXT(KL,IF) ) .NE. 0 ) 
     1                         JDUMP = KL - 1
C
C                          Now actually get rid of the lines.
C
                           IF( JDUMP .GE. IDUMP .AND.
     1                         JDUMP .LT. FILEL(IF) ) THEN
                              DLINES = JDUMP - IDUMP + 1 
                              CALL SHIFT( FILETEXT(1,IF), MLF, JDUMP+1,
     1                            FILEL(IF), -DLINES )
                              IF( IL .GE. JDUMP +1 ) IL = IL - DLINES
                           ELSE
                              FILEL(IF) = IDUMP - 1
                           END IF
                        END IF
C
                     ELSE
C
C                       Get rid of the ? specs if keeping the lines.
C                       Deal with the ?varnam part.
C
                        FILETEXT(JL,IF)(ICQ:ICQ+LENNAM) = ' '
                        IF( LEN1( FILETEXT(JL,IF)) .EQ. 0 ) THEN
                           CALL SHIFT( FILETEXT(1,IF), MLF, JL+1, 
     1                                    FILEL(IF), -1 )
                           IF( IL .GE. JL + 1 ) IL = IL - 1
                        END IF
C
C                       Now deal with the ?end.  First find it.
C
                        DO KL = JL, FILEL(IF)
                           ICE = INDEX( FILETEXT(KL,IF), '?end' )
                           IF( ICE .NE. 0 ) THEN
                              FILETEXT(KL,IF)(ICE:ICE+3) = ' '
                              IF( LEN1( FILETEXT(KL,IF) ) .EQ. 0 ) THEN
                                 CALL SHIFT( FILETEXT(1,IF), MLF, 
     1                               KL+1, FILEL(IF), -1 )
                                 IF( IL .GE. KL + 1 ) IL = IL - 1
                              END IF
                              GO TO 400
                           END IF
                        END DO
                        WRITE(*,*) 'DOVAR: Omit region not ended.'
                        WRITE(*,*) FILE(IF)
                        STOP
C
  400                   CONTINUE
                        
                     END IF
                  END IF
                  IF( JL .LT. FILEL(IF) ) GO TO 200
C
            END IF
C
C           Go back for next line looking for definitions.
C
            IF( IL .LT. FILEL(IF) ) GO TO 100
C
C     Come here when done with this "file".    
C
      END DO
C
      RETURN
      END
      

