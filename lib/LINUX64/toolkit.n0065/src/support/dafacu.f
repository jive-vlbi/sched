C$Procedure DAFACU ( DAF add comments from a logical unit )
 
      SUBROUTINE DAFACU ( COMLUN, BEGMRK, ENDMRK, INSBLN, HANDLE )
 
C$ Abstract
C
C     Add comments to an open binary DAF from an opened text file
C     attached to a Fortran logical unit.
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
C     None.
C
C$ Declarations
 
      INTEGER               COMLUN
      CHARACTER*(*)         BEGMRK
      CHARACTER*(*)         ENDMRK
      LOGICAL               INSBLN
      INTEGER               HANDLE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      COMLUN    I   Logical unit of the open comment text file.
C      BEGMRK    I   The begin comments marker to be used.
C      ENDMRK    I   The end comments marker to be used.
C      INSBLN    I   A flag indicating whether to insert a blank line.
C      HANDLE    I   Handle of a DAF opened with write access.
C
C$ Detailed_Input
C
C     COMLUN   The Fortran logical unit of a previously opened text
C              file which contains comments that are to be added to
C              the comment area of a binary DAF.
C
C     BEGMRK   A marker which identifies the beginning of the comments
C              in the comment text file. This marker must appear on a
C              line by itself and leading and trailing blanks are not
C              significant. The marker is case sensitive.
C
C              The line immediately following this marker is the first
C              comment line to be placed into the comment area of the
C              binary DAF.
C
C              If the begin marker is blank, BEGMRK .EQ. ' ', then the
C              comments are assumed to start at the current location
C              in the comment text file.
C
C     ENDMRK   A marker which identifies the end of the comments in the
C              comment text file. This marker must appear on a line by
C              itself and leading and trailing blanks are not
C              significant. The marker is case sensitive.
C
C              The line immediately preceeding this marker is the last
C              comment line to be placed into the comment area of the
C              binary DAF file.
C
C              If the end marker is blank, ENDMRK .EQ. ' ', then the
C              comments are assumed to stop at the end of the comment
C              text file.
C
C     INSBLN   A logical flag which indicates whether a blank line is
C              to be inserted into the comment area of the binary DAF
C              attached to HANDLE before any comments are added to the
C              comment area of the file. This is to provide a simple
C              mechanism for separating any comments already contained
C              in the comment area of a DAF from those comments that
C              are being added.
C
C              If the comment area of a binary DAF is empty, the value
C              of this flag is not significant, the comments are simply
C              be placed into the comment area.
C
C     HANDLE   The file handle for a binary DAF file that has been
C              opened with write access. The comments from the text
C              file are placed into the comment area of this file.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)   If the input logical unit COMLUN is not positive or there
C          is not an opened file attached to it, the error
C          SPICE(INVALIDARGUMENT) will be signalled.
C
C     2)   If the INQUIRE on the logical unit to see if there is a
C          file attached fails, the error SPICE(INQUIREFAILED) will
C          be signalled.
C
C     3)   If the scratch file for temporarily holding the comments
C          culled from the text file cannot be opened, then the
C          error SPICE(FILEOPENFAILED) will be signalled.
C
C     4)   If a nonprinting ASCII character is encountered in the
C          comments, the error SPICE(ILLEGALCHARACTER) will be
C          signalled.
C
C     5)   If the begin marker cannot be found in the text file, the
C          error SPICE(MARKERNOTFOUND) will be signalled.
C
C     6)   If the end marker cannot be found in the text file, the
C          error SPICE(MARKERNOTFOUND) will be signalled.
C
C$ Files
C
C     1)   See parameters COMLUN and HANDLE in the $ Detailed_Inputs
C          section.
C
C     2)   A scratch file is used to temporarily hold the comments
C          culled from the comment text file. This is so we do not
C          have to find the place where we started searching for
C          comments in the text file.
C
C$ Particulars
C
C     This routine will place all lines between two specified markers,
C     a `begin comments marker' and an `end comments marker,' in a
C     text file into the comment area of a binary DAF attached to
C     HANDLE. If the `begin comments marker' is blank, then the
C     comments are asumed to start at the current location in the
C     comment text file attached to COMLUN. If the `end comments
C     marker' is blank, then the comments are assumed to stop at the
C     end of the comment text file attached to COMLUN.
C
C$ Examples
C
C     We will be using the files `jabber.txt', 'batty.txt', and
C     `wndrland.daf' in the examples which follow.
C
C     `wndrland.daf' is a binary DAF file with an empty comment area
C                    into which we are going to place the entire file
C                    `jabber.txt' and a selected portion of the file
C                    `batty.txt'.
C
C     `jabber.txt'   is a text file that is to be placed into the
C                    comment area of the binary DAF file `wndrland.daf'.
C
C     `batty.txt'    is a text file from which will have a selected
C                    portion of its text placed into the comment area
C                    of the binary DAF file `wndrland.daf'.
C
C     Let -BOF- and -EOF- denote the beginning and end of a file,
C     respectively.
C
C     The file `jabber.txt' contains:
C
C        -BOF-
C                  The Jabberwock
C
C        'Twas brillig, and the slithy toves
C              Did gyre and gimble in the wabe;
C        All mimsy were the borogoves,
C              And the mome raths outgrabe.
C
C        ``Beware the Jabberwock, my son!
C              The jaws that bite, the claws that catch!''
C
C        And as in uffish thought he stood,
C              The Jabberwock, with eyes of flame,
C        Came whiffling through the tulgey wood,
C              And burbled as it came!
C
C        One, two! One, two! And through and through
C              The vorpal blade went snicker-snack!
C        He left it dead, and with its head
C              He went galumphing back.
C
C        ``And hast thou slain the Jabberwock?
C              Come to my arms, my beamish boy!
C        O frabjous day! Callooh! Callay!''
C              He chortled in his joy.
C
C               Through the Looking-Glass
C               Lewis Carroll
C        -EOF-
C
C     The file `batty.txt' contains:
C
C        -BOF-
C        This file contains a brief poem about bats.
C
C        BEGIN bat poem
C        Twinkle, twinkle, little bat!
C        How I wonder what you're at!
C        Up above the world you fly!
C        Like a teatray in the sky.
C
C               Alice's Adventures in Wonderland
C               Lewis Carroll
C        END bat poem
C
C        And that's that for bats.
C        -EOF-
C
C     Let
C
C           JABLUN   be the logical unit for the file `jabber.txt'
C           BATLUN   be the logical unit for the file `batty.txt'
C     and
C           HANDLE   be the DAF handle for the file `wndrland.daf'
C
C     The code fragment
C
C     C
C     C      Open the files.
C     C
C            CALL DAFOPW ( `wndrland.daf', HANDLE )
C            CALL TXTOPR ( `jabber.txt'  , JABLUN )
C            CALL TXTOPR ( `batty.txt'   , BATLUN )
C     C
C     C      Initialize the markers for the file `jabber.txt'. We want
C     C      to include the entire file, so both markers are blank.
C     C
C            BEGMRK = ' '
C            ENDMRK = ' '
C            INSBLN = .TRUE.
C     C
C     C      Add the comments from the file 'jabber.txt'
C     C
C            CALL DAFACU ( JABLUN, BEGMRK, ENDMRK, INSBLN, HANDLE )
C     C
C     C      Initialize the markers for the file `batty.txt'. We want
C     C      to include the bat poem only, so we define the begin and
C     C      end markere accordingly.
C     C
C            BEGMRK = 'BEGIN bat poem'
C            ENDMRK = 'END bat poem'
C            INSBLN = .TRUE.
C     C
C     C      Add the comments from the file 'batty.txt'
C     C
C            CALL DAFACU ( BATLUN, BEGMRK, ENDMRK, INSBLN, HANDLE )
C     C
C     C      Close the files.
C
C            CLOSE       ( JABLUN )
C            CLOSE       ( BATLUN )
C            CALL DAFCLS ( HANDLE )
C
C     will create a comment area in `wndrland.daf' which contains:
C
C        -BOC-
C                  The Jabberwock
C
C        'Twas brillig, and the slithy toves
C              Did gyre and gimble in the wabe;
C        All mimsy were the borogoves,
C              And the mome raths outgrabe.
C
C        ``Beware the Jabberwock, my son!
C              The jaws that bite, the claws that catch!''
C
C        And as in uffish thought he stood,
C              The Jabberwock, with eyes of flame,
C        Came whiffling through the tulgey wood,
C              And burbled as it came!
C
C        One, two! One, two! And through and through
C              The vorpal blade went snicker-snack!
C        He left it dead, and with its head
C              He went galumphing back.
C
C        ``And hast thou slain the Jabberwock?
C              Come to my arms, my beamish boy!
C        O frabjous day! Callooh! Callay!''
C              He chortled in his joy.
C
C               Through the Looking-Glass
C               Lewis Carroll
C
C        Twinkle, twinkle, little bat!
C        How I wonder what you're at!
C        Up above the world you fly!
C        Like a teatray in the sky.
C
C               Alice's Adventures in Wonderland
C               Lewis Carroll
C        -EOC-
C
C     where -BOC- and -EOC- represent the beginning and end of the
C     comments, respectively.
C
C$ Restrictions
C
C     1) The begin comments marker, BEGMRK, and the end comments marker,
C        ENDMRK, must each appear alone on a line in the comment text
C        file, if they are not blank.
C
C     2) The maximum length of a text line in a comment file is
C        specified by the LINLEN parameter defined below. Currently
C        this values is 1000 characters.
C
C     3) The maximum length of a single line comment in the comment
C        area is specified by the parameter LINLEN defined below.
C        Currently this value is 1000 characters.
C
C     4) This routine uses constants that are specific to the ASCII
C        character sequence. The results of using this routine with
C        a different character sequence are unpredictable.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer (JPL)
C
C$ Version
C
C-    Support Version 1.3.0, 01-NOV-2006 (NJB) (EDW)
C
C        Changed storage duration of array COMBUF to "saved" to
C        prevent memory problems on the PC/Windows/Visual C platform.
C
C-    Support Version 1.2.0, 16-NOV-2001 (BVS) (FST)
C
C        Buffer line size (LINLEN) was increased from 255 to 1000
C        characters to make it consistent the line size in SPC
C        routines.
C
C        Removed an unnecesary call to DAFHLU, as this routine
C        does not interact with the DAF attached to HANDLE at
C        the unit level.
C
C-    Beta Version 1.1.1, 23-JAN-1999 (BVS)
C
C        Buffer size (BUFSIZ) was increases from 22 to 2000 lines.
C
C-    Beta Version 1.1.0, 18-JAN-1996 (KRG)
C
C        Added a test and errors for checking to see whether COMLUN
C        was actually attached to an ASCII text file when this routine
C        was called.
C
C-    Beta Version 1.0.0, 4-JAN-1993 (KRG)
C
C-&
 
C$ Index_Entries
C
C      add comments from a logical unit to a daf file
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               LASTNB
 
      LOGICAL               FAILED
      LOGICAL               RETURN
C
C     Local parameters
C
C     Set the value for the maximum length of a text line.
C
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 1000 )
C
C     Set the length of a DAF file internal filename.
C
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )
C
C     Set the size of the comment buffer.
C
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 2000 )
C
C     Maximum and minimum decimal values for the printable ASCII
C     characters.
C
      INTEGER               MAXPCH
      PARAMETER           ( MAXPCH = 126 )
 
      INTEGER               MINPCH
      PARAMETER           ( MINPCH = 32 )
C
C     Local variables
C
      CHARACTER*(LINLEN)    COMBUF(BUFSIZ)
      CHARACTER*(IFNLEN)    IFNAME
      CHARACTER*(LINLEN)    LINE
 
      INTEGER               I
      INTEGER               INTCHR
      INTEGER               IOSTAT
      INTEGER               J
      INTEGER               LENGTH
      INTEGER               NCOMR
      INTEGER               NUMCOM
      INTEGER               SCRLUN
 
      INTEGER               BWARD
      INTEGER               FWARD
      INTEGER               FREE
      INTEGER               ND
      INTEGER               NI
 
      LOGICAL               EOF
      LOGICAL               MORE
      LOGICAL               OPENED

C
C     Saved variables
C
      SAVE                  COMBUF

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFACU' )
      END IF
C
C     Verify that the DAF file attached to HANDLE is opened with write
C     access.
C
      CALL DAFSIH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'DAFACU' )
         RETURN
 
      END IF
C
C     Logical units must be positive. If it is not, signal an error.
C
      IF ( COMLUN .LE. 0 ) THEN
 
         CALL SETMSG ( '# is not a valid logical unit. Logical'
     .   //            ' units must be positive.'               )
         CALL ERRINT ( '#', COMLUN                              )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                 )
         CALL CHKOUT ( 'DAFACU'                                 )
         RETURN
 
      END IF
C
C     Verify that there is an open ASCII text file attached to COMLUN.
C
      INQUIRE ( UNIT=COMLUN, OPENED=OPENED, IOSTAT=IOSTAT )
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL SETMSG ( 'The INQUIRE on logical unit # failed. The'
     .   //            ' value of IOSTAT was #.'                   )
         CALL ERRINT ( '#', COMLUN                                 )
         CALL ERRINT ( '#', IOSTAT                                 )
         CALL SIGERR ( 'SPICE(INQUIREFAILED)'                      )
         CALL CHKOUT ( 'DAFACU'                                    )
         RETURN
 
      END IF
 
      IF ( .NOT. OPENED ) THEN
 
         CALL SETMSG ( 'There is no open file attached to logical'
     .   //            ' unit #, so no comments could be read.'    )
         CALL ERRINT ( '#', COMLUN                                 )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                    )
         CALL CHKOUT ( 'DAFACU'                                    )
         RETURN
 
      END  IF
 
C
C     Read the file record of the DAF attached to HANDLE. We get back
C     some stuff that we do not use.
C
      CALL DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'DAFACU' )
         RETURN
 
      END IF
C
C     Compute the number of comment records.
C
      NCOMR = FWARD - 2
C
C     Get an available logical unit for the comment scratch file.
C
      CALL GETLUN ( SCRLUN )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'DAFACU' )
         RETURN
 
      END IF
C
C     Attempt to open the comment scratch file.
C
      OPEN ( UNIT=SCRLUN, STATUS='SCRATCH', IOSTAT=IOSTAT )
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL SETMSG ( 'Attempt to open a temporary file failed.' //
     .                 ' IOSTAT = #.'                              )
         CALL ERRINT ( '#', IOSTAT                                 )
         CALL SIGERR ( 'SPICE(FILEOPENFAILED)'                     )
         CALL CHKOUT ( 'DAFACU'                                    )
         RETURN
 
      END IF
C
C     Start looking for the begin comment marker. If the begin marker
C     is a blank line, then the comments begin on the first line of the
C     comment file. Otherwise, the comments begin on the line
C     immediately following the line which contains the begin comments
C     marker.
C
      LINE = ' '
      EOF  = .FALSE.
      DO WHILE ( LINE .NE. BEGMRK )
 
         CALL READLN ( COMLUN, LINE, EOF )
         CALL LJUST  ( LINE, LINE        )
 
         IF ( FAILED() ) THEN
 
            CLOSE       ( SCRLUN   )
            CALL CHKOUT ( 'DAFACU' )
            RETURN
 
         END IF
C
C        If we have encountered the end of file  here, we have a
C        problem: We did not find the begin comments marker in the
C        text file. So, set an appropriate error message and signal
C        the error. don't forget to close the scratch file.
C
         IF ( EOF ) THEN
 
            CLOSE       ( SCRLUN                                      )
            CALL SETMSG ( 'The begin comments marker ''#'' was not'  //
     .                    ' found in the comment file ''#''.'         )
            CALL ERRCH  ( '#', BEGMRK                                 )
            CALL ERRFNM ( '#', COMLUN                                 )
            CALL SIGERR ( 'SPICE(MARKERNOTFOUND)'                     )
            CALL CHKOUT ( 'DAFACU'                                    )
            RETURN
 
         END IF
 
      END DO
C
C     Begin reading in the comment lines from the comment file,
C     placing them a buffer at a time into the temporary file.
C     We also scan each line for non printing characters.
C
      LINE = ' '
      IF ( ENDMRK .EQ. ' ' ) THEN
C
C        If the end mark is blank, then we want to go until we hit the
C        end of the comment file.
C
         DO WHILE ( .NOT. EOF )
 
            NUMCOM = 0
            CALL READLA ( COMLUN, BUFSIZ, NUMCOM, COMBUF, EOF )
 
            IF ( FAILED() ) THEN
 
               CLOSE       ( SCRLUN   )
               CALL CHKOUT ( 'DAFACU' )
               RETURN
 
            END IF
C
C           If we got some comments, we need to scan them for non-
C           printing characters.
C
            IF ( NUMCOM .GT. 0 ) THEN
 
               DO I = 1, NUMCOM
 
                  LENGTH = LASTNB ( COMBUF(I) )
C
C                 Scan the comment line for non printinig characters.
C
                  DO J = 1, LENGTH
C
C                    Check to see that the characters in the buffer
C                    are all printing ASCII characters. The bounds
C                    for printing ASCII characters are given by
C                    MAXPCH and MINPCH, which are defined in the
C                    $ Local Parameters section of the header.
C
                     INTCHR = ICHAR ( COMBUF(I)(J:J) )
                     IF ( ( INTCHR .GT. MAXPCH ) .OR.
     .                    ( INTCHR .LT. MINPCH )      ) THEN
 
                           CLOSE       ( SCRLUN                      )
                           CALL SETMSG ( 'A nonprinting character'  //
     .                                   ' was encountered in the'  //
     .                                   ' comments. Value: #'       )
                           CALL ERRINT ( '#', INTCHR                 )
                           CALL SIGERR ( 'SPICE(ILLEGALCHARACTER)'   )
                           CALL CHKOUT ( 'DAFACU'                    )
                           RETURN
 
                     END IF
 
                  END DO
 
               END DO
C
C              Write the comments to the temporary file.
C
               CALL WRITLA( NUMCOM, COMBUF, SCRLUN )
 
            END IF
 
            IF ( FAILED() ) THEN
 
               CLOSE       ( SCRLUN   )
               CALL CHKOUT ( 'DAFACU' )
               RETURN
 
            END IF
 
         END DO
 
      ELSE
C
C        The endmark is non blank, then  we want to go until we find a
C        line in the comment file that matches the end mark that was
C        entered.
C
         MORE = .TRUE.
         DO WHILE ( MORE )
 
            NUMCOM = 0
            CALL READLA ( COMLUN, BUFSIZ, NUMCOM, COMBUF, EOF )
 
            IF ( FAILED() ) THEN
 
               CLOSE       ( SCRLUN   )
               CALL CHKOUT ( 'DAFACU' )
               RETURN
 
            END IF
C
C           Look for ENDMRK in the current buffer, if we got some
C           comments.
C
            IF ( NUMCOM .GT. 0 ) THEN
 
               I = 1
               DO WHILE ( MORE .AND. ( I .LE. NUMCOM ) )
 
                  LINE = COMBUF(I)
                  CALL LJUST ( LINE, LINE )
 
                  IF ( LINE .EQ. ENDMRK ) THEN
 
                     MORE   = .FALSE.
                     NUMCOM = I - 1
 
                  ELSE
 
                     I = I + 1
 
                  END IF
 
               END DO
 
            END IF
C
C           If we still have some comments, we need to scan them for
C           non printing characters.
C
            IF ( NUMCOM .GT. 0 ) THEN
 
               DO I = 1, NUMCOM
 
                  LENGTH = LASTNB ( COMBUF(I) )
C
C                 Scan the comment line for non printinig characters.
C
                  DO J = 1, LENGTH
C
C                    Check to see that the characters in the buffer
C                    are all printing ASCII characters. The bounds
C                    for printing ASCII characters are given by
C                    MAXPCH and MINPCH, which are defined in the
C                    $ Local Parameters section of the header.
C
                     INTCHR = ICHAR ( COMBUF(I)(J:J) )
                     IF ( ( INTCHR .GT. MAXPCH ) .OR.
     .                    ( INTCHR .LT. MINPCH )      ) THEN
 
                           CLOSE       ( SCRLUN                      )
                           CALL SETMSG ( 'A nonprinting character'  //
     .                                   ' was encountered in the'  //
     .                                   ' comment buffer. Value:'  //
     .                                   ' #'                        )
                           CALL ERRINT ( '#', INTCHR                 )
                           CALL SIGERR ( 'SPICE(ILLEGALCHARACTER)'   )
                           CALL CHKOUT ( 'DAFACU'                    )
                           RETURN
 
                     END IF
 
                  END DO
 
               END DO
C
C              Write the comments to the temporary file.
C
               CALL WRITLA( NUMCOM, COMBUF, SCRLUN )
 
            END IF
 
            IF ( FAILED() ) THEN
 
               CLOSE       ( SCRLUN   )
               CALL CHKOUT ( 'DAFACU' )
               RETURN
 
            END IF
C
C           If we have encountered the end of file here, we have a
C           problem: We did not find the end comments marker in the
C           text file. So, set an appropriate error message and
C           signal the error.
C
            IF ( MORE .AND. EOF ) THEN
 
               CLOSE       ( SCRLUN                                )
               CALL SETMSG ( 'The end comments marker ''#'' was'  //
     .                       ' not found in the comment file'     //
     .                       ' ''#''.'                             )
               CALL ERRCH  ( '#', ENDMRK                           )
               CALL ERRFNM ( '#', COMLUN                           )
               CALL SIGERR ( 'SPICE(MARKERNOTFOUND)'               )
               CALL CHKOUT ( 'DAFACU'                              )
               RETURN
 
            END IF
 
         END DO
 
      END IF
C
C     If we made it to here, we have culled all of the comments out of
C     the text file and they were all OK. So we need to add all of the
C     comments to the DAF comment area now.
C
C     If we are supposed to insert a blank line to separate the current
C     addition from any previously stored comments, and there are
C     comments already in the comment area, indicated by NCOMR > 0, then
C     we insert the blank line. Otherwise, just add the comments.
C
      IF ( INSBLN .AND. ( NCOMR .GT. 0 ) ) THEN
 
         CALL DAFAC ( HANDLE, 1, ' ' )
 
         IF ( FAILED() ) THEN
 
            CLOSE       ( SCRLUN   )
            CALL CHKOUT ( 'DAFACU' )
            RETURN
 
         END IF
 
      END IF
C
C     Rewind the scratch file to get ready to put the comments into the
C     comment area.
C
      REWIND ( SCRLUN )
C
C     Begin reading through the scratch file, placing the comment lines
C     into the comment area of the DAF file a buffer at a time
C
      EOF = .FALSE.
      DO WHILE ( .NOT. EOF )
 
         NUMCOM = 0
C
C        Read in a buffer of comment lines.
C
         CALL READLA ( SCRLUN, BUFSIZ, NUMCOM, COMBUF, EOF )
C
C        If we got some, add them to the comment area of the DAF file.
C
         IF ( NUMCOM .GT. 0 ) THEN
 
            CALL DAFAC  ( HANDLE, NUMCOM, COMBUF )
 
         END IF
 
         IF ( FAILED() ) THEN
 
            CLOSE       ( SCRLUN   )
            CALL CHKOUT ( 'DAFACU' )
            RETURN
 
         END IF
 
      END DO
C
C     Close the scratch file before exiting, it's the only one we
C     opened.
C
      CLOSE ( SCRLUN )
 
      CALL CHKOUT ( 'DAFACU' )
      RETURN
      END
