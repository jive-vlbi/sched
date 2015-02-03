 
C$Procedure LOCLN ( Locate lines in a text file )
 
      SUBROUTINE LOCLN ( UNIT, BMARK, EMARK, LINE, BLINE, ELINE, FOUND )
 
C$ Abstract
C
C     Locate a group of lines in a text file delimited by markers.
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
C     FILES
C     TEXT
C
C$ Declarations
 
      INTEGER               UNIT
      CHARACTER*(*)         BMARK
      CHARACTER*(*)         EMARK
      CHARACTER*(*)         LINE
      INTEGER               BLINE
      INTEGER               ELINE
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     UNIT       I   Logical unit connected to text file.
C     BMARK      I   Begin marker.
C     EMARK      I   End marker.
C     LINE      I,O  Workspace.
C     BLINE      O   Beginning line.
C     ELINE      O   Ending line.
C     FOUND      O   Markers found?
C
C$ Detailed_Input
C
C     UNIT        is a logical unit that has been connected to a
C                 text file by the calling program.  Use the routine
C                 TXTOPR to open the file for read access and get its
C                 logical unit.  The file pointer may be pointing to
C                 any line in the file due to previous read statements,
C                 for example, or due to previous calls to LOCLN.
C
C     BMARK,
C     EMARK       are markers that delimit some group of lines in
C                 the part of the file following the current position
C                 of the file pointer. The group begins with the
C                 first line equivalent to BMARK and ends with the
C                 next line equivalent to EMARK, ignoring leading
C                 and trailing blanks.
C
C                 If BMARK is blank, the group of lines begins with
C                 the first line following the current position of the
C                 file pointer; if EMARK is blank, the group of lines
C                 ends with the last line in the file.
C
C      LINE       on input, is an arbitrary character string whose
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
C      BLINE,
C      ELINE      are the line numbers of the first and last lines
C                 in the group delimited by BMARK and EMARK.
C
C                 By convention, the first line read by the routine
C                 is line 1; the second line is line 2; and so on.
C                 If BMARK is blank, BLINE will be 1.
C
C      FOUND      is true if a group of lines delimited by BMARK and
C                 EMARK is found, and is false otherwise.  ELINE is
C                 the last line read by LOCLN, so if FOUND is true,
C                 the file pointer will be positioned on the line
C                 after ELINE.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C      1) If FOUND is false, the values of BLINE and ELINE are not
C         changed.
C
C      2) If an error occurs while reading from the input file,
C         the error SPICE(FILEREADFAILED) is signalled.
C
C      3) Lines in the file that are longer than the declared length of
C         LINE are truncated as they are read.  If the truncation of
C         line containing a marker causes truncation of that marker,
C         it will not match the input value for that marker, so
C         FOUND will be .FALSE.
C
C$ Files
C
C     See argument UNIT.
C
C$ Particulars
C
C     This routine locates delimited groups of lines in a text file.
C     This allows files to be partitioned into sub-files; it also
C     allows related inputs to be grouped together in a relatively
C     free-format way.
C
C$ Examples
C
C     1) Let FILE.TXT be a text file that contains the following lines.
C        (The lines are numbered for reference, but these numbers do
C        not appear in the file).
C
C           1    BEGIN POEM
C           2       Oh snail,
C           3       Climb Mount Fuji,
C           4       But slowly, slowly!
C           5    END POEM
C           6
C           7    BEGIN PROSE
C           8       Lady, one of us has this book open
C           9       to the wrong page.
C           10   END PROSE
C           11
C           12   BEGIN POEM
C           13      John Keats, John Keats,
C           14      John,
C           15      Put your scarf on.
C           16   END POEM
C           17
C           18   BEGIN QUOTE
C           19      That's not writing. That's typing.
C           20
C           21               (Truman Capote on Jack Kerouac)
C           22   END QUOTE
C           23
C           24   BEGIN POEM
C           25      Twice five syllables
C           26      Plus seven isn't much, but
C           27      That's haiku for you.
C           28   BEGIN POEM
C           29
C           30   BEGIN EQUATION
C           31            2
C           32      e = mc
C           33   END EQUATION
C
C     Then the code fragment
C
C           CALL TXTOPR ( 'FILE.TXT', UNIT )
C
C           BMARK = 'BEGIN POEM'
C           EMARK = 'END POEM'
C
C           CALL LOCLN ( UNIT, BMARK, EMARK, LINE, B, E, FOUND )
C
C           DO WHILE ( FOUND )
C              WRITE (*,*) 'Found poem between lines ', B, ' and ', E
C
C              CALL LOCLN ( UNIT, BMARK, EMARK, LINE, B, E, FOUND )
C           END DO
C
C     produces the following report:
C
C           Found poem between lines   1 and   5
C           Found poem between lines   7 and  11
C           Found poem between lines   8 and  12
C
C     Note that line numbers are returned relative to the position
C     of the file pointer when LOCLN is called. The following code
C     fragment generates the numbers relative to the start of the
C     file.
C
C           REWIND ( UNIT )
C
C           OFFSET = 0
C           CALL LOCLN ( UNIT, BMARK, EMARK, LINE, B, E, FOUND )
C
C           DO WHILE ( FOUND )
C              WRITE (*,*) 'Found poem between lines ',
C          .                OFFSET + B,
C          .                ' and ',
C          .                OFFSET + E
C
C              OFFSET = OFFSET + E
C              CALL LOCLN ( UNIT, BMARK, EMARK, LINE, B, E, FOUND )
C           END DO
C
C           CLOSE ( UNIT )
C
C     The following report is produced:
C
C           Found poem between lines   1 and   5
C           Found poem between lines  12 and  16
C           Found poem between lines  24 and  28
C
C
C     2) Given the same file, the code fragment
C
C           CALL TXTOPR ( 'FILE.TXT', UNIT )
C
C           CALL LOCLN ( UNIT,
C          .             'begin poem',
C          .             'end poem',
C          .             LINE,
C          .             B,
C          .             E,
C          .             FOUND )
C
C           CLOSE ( UNIT )
C
C     finds nothing because case is significant: FOUND is false,
C     and B and E are unchanged.
C
C     3) This code fragment
C
C           CALL TXTOPR ( 'FILE.TXT', UNIT )
C
C           CALL LOCLN ( UNIT,
C          .             ' ',
C          .             'BEGIN PROSE',
C          .             LINE,
C          .             B,
C          .             E,
C          .             FOUND )
C
C           CLOSE ( UNIT )
C
C     when executed on the same file returns B = 1 and E = 7.
C     In effect, a blank begin marker "matches" the first line
C     that is read.
C
C     Similarly, a blank end marker "matches" the last line of
C     the file, the code fragment
C
C           CALL TXTOPR ( 'FILE.TXT', UNIT )
C
C           CALL LOCLN ( UNIT,
C          .             'BEGIN QUOTE',
C          .             ' ',
C          .             LINE,
C          .             B,
C          .             E,
C          .             FOUND )
C
C           CLOSE ( UNIT )
C
C     when executed on the same file returns B = 18 and E = 33.
C     If both markers are blank, LOCLN basically counts the lines
C     in the file.
C
C     4) The code fragment
C
C           CALL TXTOPR ( 'FILE.TXT', UNIT )
C
C           MARK = 'BEGIN POEM'
C
C           CALL LOCLN ( UNIT, MARK, MARK, LINE, FIRST, SECOND, FOUND )
C
C           CLOSE ( UNIT )
C
C     returns FIRST = 1 and SECOND = 12 -- the first two lines that
C     are equivalent to MARK.
C
C     5) Nesting is not supported. That is, if UNIT is connected to
C     a file containing the following lines (ignoring line numbers),
C
C           1   Begin Object
C           2     Begin Object
C           3       Begin Object
C           4         Just kidding!
C           5       End Object
C           6     End Object
C           7   End Object
C
C           REWIND ( UNIT )
C
C           CALL LOCLN ( UNIT,
C          .             'Begin Object'
C          .             'End Object',
C          .             LINE,
C          .             B,
C          .             E,
C          .             FOUND )
C
C     returns B = 1 and E = 5, not E = 7.
C
C     6) Let UNIT be connected to a text file containing the
C     following lines, again ignoring line numbers which are
C     listed for easy reference.
C
C           1    The first case tests the capability of ...
C           2
C           3    NEW CASE
C           4       TARGET = JUPITER
C           5       EPOCH  = 21 JUN 1992 13:04
C           6    END CASE
C           7
C           8    The next case uses a different target and a slightly
C           9    longer exposure time...
C           10
C           11   NEW CASE
C           12      TARGET   = IO
C           13      EPOCH    = 21 JUN 1992 13:04
C           14      EXPOSURE = 2.44 SECONDS
C           15   END CASE
C           16
C           17   The next case changes targets in order to...
C           18
C           19   NEW CASE
C           20      TARGET   = EUROPA
C           21      EPOCH    = 21 JUN 1992 13:04
C           22      EXPOSURE = 2.44 SECONDS
C           23   END CASE
C
C     Then the code fragment
C
C           REWIND ( UNIT )
C
C           BMARK = 'NEW CASE'
C           EMARK = 'END CASE'
C
C           CASES  = 0
C           OFFSET = 0
C           CALL LOCLN ( UNIT, BMARK, EMARK, LINE, B, E, FOUND )
C
C           DO WHILE ( FOUND )
C              CASES      = CASES  + 1
C              BEG(CASES) = OFFSET + B
C              END(CASES) = OFFSET + E
C
C              OFFSET = OFFSET + E
C              CALL LOCLN ( UNIT, BMARK, EMARK, LINE, B, E, FOUND )
C           END DO
C
C     saves the locations of the various input cases (skipping past
C     the intervening commentary) in the arrays BEG and END.  After
C     running the code, CASES, BEG, and END have the following values:
C
C           CASES = 3
C           BEG   = 3,  11,  19
C           END   = 6,  15,  23
C
C     The following code fragment retrieves the i'th case.
C
C           REWIND ( UNIT )
C
C           DO J = 1, BEG(I) - 1
C              READ (UNIT,FMT='(A)') LINE
C           END DO
C
C           DO J = BEG(I), END(I)
C              READ (UNIT,FMT='(A)') LINE
C               .
C               .  Process the line
C               .
C           END DO
C
C     While this isn't an incredibly efficient way to process
C     large files, it can be effective for smaller files.
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
C
C$ Version
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
C     locate lines in a text file
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      INTEGER               LTRIM
 
C
C     Local variables
C
 
      INTEGER               BLTEMP
      INTEGER               ELTEMP
      INTEGER               IOSTAT
 
      LOGICAL               BFOUND
      LOGICAL               EFOUND
      LOGICAL               EOF
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LOCLN' )
      END IF
 
C
C     We'll use temporary variables BLTEMP and ELTEMP for BLINE and
C     ELINE until we know that both markers have been found.  We'll
C     use BFOUND to indicate whether or not BMARK was found, and
C     EFOUND to indicate whether or not EMARK was found.  EOF
C     indicates end of file.
C
      BLTEMP =  0
      BFOUND = .FALSE.
      EFOUND = .FALSE.
      EOF    = .FALSE.
 
C
C     Read through the file, line by line, searching for the first
C     occurrence of BMARK and counting lines as we go.  Once we
C     find BMARK, we'll start searching for EMARK.  After each read
C     we'll check for I/O errors.
C
 
      DO WHILE (  ( .NOT. BFOUND )  .AND.  ( .NOT. EOF )  )
 
         READ ( UNIT, FMT='(A)', IOSTAT=IOSTAT ) LINE
C
C        An end-of-file condition is indicated by a negative value
C        for IOSTAT. Any other non-zero value indicates some other
C        error.
C
         IF ( IOSTAT .GT. 0 ) THEN
 
            CALL SETMSG ( 'While searching for BMARK = #, an attempt '//
     .                    'to read the file named FILENAME failed.  ' //
     .                    'The value of IOSTAT is #.'                 )
            CALL ERRCH  ( '#', BMARK                                  )
            CALL ERRINT ( '#', IOSTAT                                 )
            CALL ERRFNM ( 'FILENAME', UNIT                            )
            CALL SIGERR ( 'SPICE(FILEREADFAILED)'                     )
            CALL CHKOUT ( 'LOCLN'                                     )
            RETURN
 
         ELSE IF ( IOSTAT .LT. 0 ) THEN
 
            EOF = .TRUE.
 
         ELSE
C
C           The read was successful, so count the line then
C           check for a match.
C
            BLTEMP = BLTEMP + 1
 
            CALL LJUST ( LINE, LINE )
 
C
C           By convention, if BMARK is blank, it matches the
C           first line that we read.  If it is not blank, we
C           compare it to the line just read, ignoring leading
C           and trailing blanks.
C
            IF ( BMARK .EQ. ' ' ) THEN
               BFOUND = .TRUE.
 
            ELSE
               IF (  BMARK ( LTRIM(BMARK): ) .EQ. LINE  ) THEN
                  BFOUND = .TRUE.
               END IF
 
            END IF
 
 
         END IF
 
      END DO
 
 
 
C
C     Start the search for EMARK starting from where we left off.
C
      ELTEMP = BLTEMP
 
      DO WHILE (  ( .NOT. EFOUND )  .AND.  ( .NOT. EOF )  )
 
         READ ( UNIT, FMT='(A)', IOSTAT=IOSTAT ) LINE
C
C        An end-of-file condition is indicated by a negative value
C        for IOSTAT. Any other non-zero value indicates some other
C        error.
C
         IF ( IOSTAT .GT. 0 ) THEN
 
            CALL SETMSG ( 'While searching for EMARK = #, an attempt '//
     .                    'to read the file named FILENAME failed.  ' //
     .                    'The value of IOSTAT is #.'                 )
            CALL ERRCH  ( '#', EMARK                                  )
            CALL ERRINT ( '#', IOSTAT                                 )
            CALL ERRFNM ( 'FILENAME', UNIT                            )
            CALL SIGERR ( 'SPICE(FILEREADFAILED)'                     )
            CALL CHKOUT ( 'LOCLN'                                     )
            RETURN
 
         ELSE IF ( IOSTAT .LT. 0 ) THEN
 
            EOF = .TRUE.
C
C           By convention, if EMARK is blank, it matches the
C           last line in the file.
C
            IF ( EMARK .EQ. ' ' ) THEN
               EFOUND = .TRUE.
            END IF
 
         ELSE
C
C           The read was successful, so count the line and check for
C           a match.
C
            ELTEMP = ELTEMP + 1
 
            CALL LJUST ( LINE, LINE )
 
            IF ( EMARK .NE. ' ' ) THEN
               IF ( EMARK ( LTRIM(EMARK): ) .EQ. LINE  ) THEN
                  EFOUND = .TRUE.
               END IF
            END IF
 
 
         END IF
 
      END DO
 
C
C     Assign the line numbers to BLINE and ELINE only if both markers
C     were found.
C
      FOUND = BFOUND .AND. EFOUND
 
      IF ( FOUND ) THEN
 
         BLINE = BLTEMP
         ELINE = ELTEMP
 
      END IF
 
 
      CALL CHKOUT ( 'LOCLN' )
      RETURN
      END
 
 
