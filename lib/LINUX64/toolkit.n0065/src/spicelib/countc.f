C$Procedure COUNTC ( Count characters in a text file )
 
      INTEGER FUNCTION COUNTC ( UNIT, BLINE, ELINE, LINE )
 
C$ Abstract
C
C     Count the characters in a group of lines in a text file.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     CHARACTERS
C     FILES
C
C$ Declarations
 
      INTEGER               UNIT
      INTEGER               BLINE
      INTEGER               ELINE
      CHARACTER*(*)         LINE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     UNIT       I   Logical unit connected to text file.
C     BLINE      I   Beginning line number.
C     ELINE      I   Ending line number.
C     LINE      I,O  Workspace.
C
C     COUNTC returns the number of characters.
C
C$ Detailed_Input
C
C     UNIT        is a logical unit that has been connected to a
C                 text file by the calling program.  Use the routine
C                 TXTOPR to open the file for read access and get its
C                 logical unit.  A text file is a formatted,
C                 sequential file that contains only printable
C                 characters:  ASCII 32-126.
C
C     BLINE,
C     ELINE       are line numbers in the text file.  BLINE is
C                 the line where the count will begin, and ELINE
C                 is the line where the count will end.  The
C                 number of characters in the beginning and ending
C                 lines are included in the total count.
C
C                 By convention, line 1 is the first line of the file.
C
C     LINE        on input, is an arbitrary character string whose
C                 contents are ignored. LINE is used to read lines
C                 from the file connected to UNIT; its function
C                 is to determine the maximum length of the lines
C                 that can be read from the file. Lines longer
C                 than the declared length of LINE are truncated
C                 as they are read.
C
C$ Detailed_Output
C
C      LINE       on output, is undefined.
C
C     The function, COUNTC,  returns the number of characters in the
C     group of lines in the file beginning with BLINE and ending with
C     ELINE.  Trailing blanks on a line are not included in the count.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C      1) If an error occurs while reading from the input file,
C         the error SPICE(FILEREADFAILED) is signalled.
C
C      2) If a non-printing ASCII character is encountered during
C         the count, the error SPICE(INVALIDTEXT) is signalled.
C
C      3) If BLINE is greater than ELINE or if the file does not
C         contain both of this lines, the error SPICE(CANNOTFINDGRP)
C         is signalled.
C
C$ Files
C
C     See argument UNIT.  COUNTC rewinds the text file connected to
C     UNIT and then steps through the file.  The next read statement
C     after calling COUNTC would return the line after ELINE.
C
C$ Particulars
C
C     This routine counts characters in a group of lines in a text
C     file.  Using COUNTC, you can determine in advance how much space
C     is required to store those characters.
C
C$ Examples
C
C     The following code fragment opens an existing text file for
C     read access and counts the characters that it contains in
C     the first five lines.  We'll assume that the longest line
C     in the file is 80 characters.
C
C        INTEGER               COUNTC
C        INTEGER               UNIT
C        INTEGER               N
C        CHARACTER*(80)        LINE
C
C        CALL TXTOPR ( 'DATA.TXT', UNIT )
C
C        N = COUNTC ( UNIT, 1, 5, LINE )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J.E. McLean    (JPL)
C     H.A. Neilan    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
C
C       Set the default function value to either 0, 0.0D0, .FALSE.,
C       or blank depending on the type of the function.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 05-APR-1991 (JEM)
C
C-&
 
C$ Index_Entries
C
C     count characters in a text file
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               LASTNB
 
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               CHARS
      INTEGER               IOSTAT
      INTEGER               LINECT
 
      LOGICAL               DONE
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         COUNTC = 0
         RETURN
      ELSE
         CALL CHKIN ( 'COUNTC' )
         COUNTC = 0
      END IF
 
C
C     First, see if the line numbers make sense.
C
      IF ( ( BLINE .GT. ELINE ) .OR. ( BLINE .LE. 0 ) ) THEN
 
         CALL SETMSG ( 'The line numbers do not make sense:  '        //
     .                 'BLINE = # and  ELINE = #.'                    )
         CALL ERRINT ( '#', BLINE                                     )
         CALL ERRINT ( '#', ELINE                                     )
         CALL SIGERR ( 'SPICE(CANNOTFINDGRP)'                         )
         CALL CHKOUT ( 'COUNTC'                                       )
         RETURN
 
      END IF
 
C
C     Read through the file, line by line, beginning with the first
C     line in the file, checking for I/O errors, and counting
C     characters in the lines between and including BLINE and ELINE.
C
      REWIND ( UNIT )
 
      LINECT = 0
      CHARS  = 0
      DONE   = .FALSE.
 
      DO WHILE ( .NOT. DONE )
 
         READ ( UNIT, FMT='(A)', IOSTAT=IOSTAT ) LINE
C
C        An end-of-file condition is indicated by a negative value
C        for IOSTAT. Any other non-zero value indicates some other
C        error.  If IOSTAT is zero, the read was successful.
C
         IF ( IOSTAT .GT. 0 ) THEN
 
            CALL SETMSG ( 'Error reading text file named FILENAME.'   //
     .                    'The value of IOSTAT is #.'                 )
            CALL ERRINT ( '#', IOSTAT                                 )
            CALL ERRFNM ( 'FILENAME', UNIT                            )
            CALL SIGERR ( 'SPICE(FILEREADFAILED)'                     )
            CALL CHKOUT ( 'COUNTC'                                    )
            RETURN
 
         ELSE IF ( IOSTAT .LT. 0 ) THEN
 
            CALL SETMSG ( 'Reached end of file unexpectedly at line ' //
     .                    '# in file FILE.  BLINE = # and ELINE = #.' )
            CALL ERRINT ( '#', LINECT                                 )
            CALL ERRINT ( '#', BLINE                                  )
            CALL ERRINT ( '#', ELINE                                  )
            CALL ERRFNM ( 'FILE', UNIT                                )
            CALL SIGERR ( 'SPICE(CANNOTFINDGRP)'                      )
            CALL CHKOUT ( 'COUNTC'                                    )
            RETURN
 
         ELSE
C
C           We've read a line successfully, so add it to the line count.
C           If this line is in the group delimited by BLINE and ELINE,
C           count the characters in it, and if this line is ELINE, we're
C           done.
C
            LINECT = LINECT + 1
 
            IF (( LINECT .GE. BLINE ) .AND. ( LINECT .LE. ELINE )) THEN
 
C
C              Add the number of characters in this line to the count.
C              If LINE is blank, LASTNB will return 0 which is just
C              what we want.
C
               CHARS = CHARS + LASTNB ( LINE )
 
C
C              Remove the printable characters from the line.  If
C              any characters remain, signal an error.
C
               CALL ASTRIP ( LINE, CHAR( 32), CHAR(126), LINE )
 
               IF ( LINE .NE. ' ' ) THEN
 
                  CALL SETMSG ( 'Non-printing ASCII characters were ' //
     .                          'found when counting characters on '  //
     .                          'line number # in file FILENAME.'     )
                  CALL ERRINT ( '#', LINECT                           )
                  CALL ERRFNM ( 'FILENAME', UNIT                      )
                  CALL SIGERR ( 'SPICE(INVALIDTEXT)'                  )
                  CALL CHKOUT ( 'COUNTC'                              )
                  RETURN
 
               END IF
 
            END IF
 
            IF ( LINECT .EQ. ELINE ) DONE = .TRUE.
 
         END IF
 
      END DO
 
C
C     Assign the final character count.
C
      COUNTC = CHARS
 
 
      CALL CHKOUT ( 'COUNTC' )
      RETURN
      END
 
 
 
