C$Procedure      PARCML ( Parse command line )

      SUBROUTINE PARCML ( LINE, NKEYS, CLKEYS, CLFLAG, CLVALS, FOUND,
     .                    UNPRSD )

C$ Abstract
C
C     Parse a command-line like string in the "key value key value ..."
C     format with keys provided in any order and any letter case
C     (lower, upper, mixed) and return values of requested keys.
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
C     PARSING
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         LINE
      INTEGER               NKEYS
      CHARACTER*(*)         CLKEYS ( * )
      LOGICAL               CLFLAG ( * )
      CHARACTER*(*)         CLVALS ( * )
      LOGICAL               FOUND
      CHARACTER*(*)         UNPRSD

      INTEGER               LLNSIZ
      PARAMETER           ( LLNSIZ = 2048 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LINE      I/O  Input command-line like string.
C     NKEYS      I   Number of keys to look for.
C     CLKEYS     I   Keys to look for.
C     CLFLAG     O   "A particular key found" flags.
C     CLVALS     O   Key values.
C     FOUND      O   "At least one key found" flag.
C     UNPRSD     O   Beginning part of the LINE that was not parsed
C     LLNSIZ     P   Size of longest sub-string that can be processed.
C
C$ Detailed_Input
C
C     LINE        is the input command-line like string in the "key
C                 value key value ..." format. The line should start
C                 with one of the keys provided in CLKEYS as the
C                 routine ignores any words before the first recognized
C                 key.
C
C                 To avoid limiting the size of the input string that
C                 can be processed, this routine uses LINE as the work
C                 buffer; it modifies LINE in the process of execution,
C                 and sets it to blank before return.
C
C     NKEYS       is the number of keys to look for provided in the
C                 CLKEYS array.
C
C     CLKEYS      is an array of keys to look for. Individual keys
C                 must be left-justified string consisting of any 
C                 printable the characters except lower-case letters 
C                 and blanks.
C
C$ Detailed_Output
C
C     LINE        is set to blank on the output.
C
C     CLFLAG      are the "key found" flags; set to TRUE if
C                 corresponding key was found.
C
C     CLVALS      are the key values; if a key wasn't found, its value
C                 set to a blank string.
C
C     FOUND       is set to .TRUE. if at least one key was found.
C                 Otherwise it is set to .FALSE.
C     
C     UNPRSD      is the beginning part of the LINE, preceeding the
C                 first recognized key, that was ignored by this
C                 routine.
C
C$ Parameters
C
C     LLNSIZ      is the size of the internal buffer that holds a
C                 portion of the input string that is being examined.
C                 It limits the maximum total length of a front and
C                 back blank-padded, blank-separated sub-string
C                 containing a key, the value that follows it, and the
C                 next key (e.g. ' key value key ') that this routine
C                 can correctly process.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine modifies the input string. It returns it set to
C     blank.
C
C     The case of the keys in the input string is not significant.
C
C     The order of keys in the input string is not significant.
C
C     If any key appears in the string more than once, only the 
C     last value of that key is returned.
C
C     The part of the line from the start up to the first recognized
C     key is returned in the UNPRSD argument.
C
C$ Examples
C
C     If CLKEYS are
C
C        CLKEYS(1) = '-SETUP'
C        CLKEYS(2) = '-TO'
C        CLKEYS(3) = '-FROM'
C        CLKEYS(4) = '-HELP'
C
C     then:
C
C     line '-setup my.file -FROM utc -TO sclk'
C     will be parsed as
C
C        CLFLAG(1) = .TRUE.       CLVALS(1) = 'my.file'
C        CLFLAG(2) = .TRUE.       CLVALS(2) = 'utc'
C        CLFLAG(3) = .TRUE.       CLVALS(3) = 'sclk'
C        CLFLAG(4) = .FALSE.      CLVALS(4) = ' '
C        UNPRSD    = ' '
C        FOUND = .TRUE.
C
C     line '-bogus -setup my.file -FROM utc -TO sclk'
C     will be parsed as
C
C        CLFLAG(1) = .TRUE.       CLVALS(1) = 'my.file'
C        CLFLAG(2) = .TRUE.       CLVALS(2) = 'utc'
C        CLFLAG(3) = .TRUE.       CLVALS(3) = 'sclk'
C        CLFLAG(4) = .FALSE.      CLVALS(4) = ' '
C        UNPRSD    = '-bogus'
C        FOUND = .TRUE.
C
C     line 'why not -setup my.file -FROM utc -TO sclk'
C     will be parsed as
C
C        CLFLAG(1) = .TRUE.       CLVALS(1) = 'my.file'
C        CLFLAG(2) = .TRUE.       CLVALS(2) = 'utc'
C        CLFLAG(3) = .TRUE.       CLVALS(3) = 'sclk'
C        CLFLAG(4) = .FALSE.      CLVALS(4) = ' '
C        UNPRSD    = 'why not'
C        FOUND = .TRUE.
C
C     line '-SETUP my.file -setup your.file'
C     will be parsed as
C
C        CLFLAG(1) = .TRUE.       CLVALS(1) = 'your.file'
C        CLFLAG(2) = .FALSE.      CLVALS(2) = ' '
C        CLFLAG(3) = .FALSE.      CLVALS(3) = ' '
C        CLFLAG(4) = .FALSE.      CLVALS(4) = ' '
C        UNPRSD    = ' '
C        FOUND = .TRUE.
C
C     line '-setup my.file -SeTuP your.file'
C     will be parsed as
C
C        CLFLAG(1) = .TRUE.       CLVALS(1) = 'your.file'
C        CLFLAG(2) = .FALSE.      CLVALS(2) = ' '
C        CLFLAG(3) = .FALSE.      CLVALS(3) = ' '
C        CLFLAG(4) = .FALSE.      CLVALS(4) = ' '
C        UNPRSD    = ' '
C        FOUND = .TRUE.
C
C     line '-help'
C     will be parsed as
C
C        CLFLAG(1) = .FALSE.      CLVALS(1) = ' '
C        CLFLAG(2) = .FALSE.      CLVALS(2) = ' '
C        CLFLAG(3) = .FALSE.      CLVALS(3) = ' '
C        CLFLAG(4) = .TRUE.       CLVALS(4) = ' '
C        UNPRSD    = ' '
C        FOUND = .TRUE.
C
C     and so on.
C
C$ Restrictions
C
C     This routine cannot process input lines with any ' -key value
C     -key ' sub-string that is longer than LLNSIZ.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SUPPORT Version 1.0.0, 15-FEB-2012 (BVS)
C
C-&

C
C     Local variables.
C
      CHARACTER*(LLNSIZ)    HKEY
      CHARACTER*(LLNSIZ)    HLINE
      CHARACTER*(LLNSIZ)    HLNGWD
      CHARACTER*(LLNSIZ)    LNGWD
      CHARACTER*(LLNSIZ)    ULINE

      INTEGER               BEGPOS
      INTEGER               CLIDX
      INTEGER               ENDPOS
      INTEGER               I
      INTEGER               PCLIDX

C
C     Save everything to prevent potential memory problems in f2c'ed
C     version.
C
      SAVE

C
C     SPICELIB functions.
C
      INTEGER               ISRCHC
      INTEGER               POS
      INTEGER               RTRIM

      LOGICAL               RETURN

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PARCML' )
      END IF

C
C     Set initial values of keys to blanks and flags to .FALSE.
C
      DO I = 1, NKEYS
         CLFLAG( I ) = .FALSE.
         CLVALS( I ) = ' '
      END DO

      FOUND = .FALSE.

C
C     Parsing loop. We will set the sub-string buffer HLINE to as many
C     characters from the input line as it will fit, starting with the
C     initial part of the line on the first iteration and resetting to
C     sub-strings starting at the first character of each value after
C     the previous key-value pair was processed, and will pick at HLINE
C     word by word looking for recognized keys. The loop will
C     continue until we reach the end of the string -- all key-value
C     pairs were processed and the sub-string buffer HLINE was set to
C     blank.
C
      HLINE  = LINE

      PCLIDX = 0
      CLIDX  = 0

      UNPRSD = LINE

      DO WHILE ( HLINE .NE. ' ' )

C
C        Get next word; uppercase it; look for it in the input keys
C        array.
C
         CALL NEXTWD ( HLINE, LNGWD, HLINE )
         CALL UCASE  ( LNGWD, HLNGWD )
         CLIDX = ISRCHC( HLNGWD, NKEYS, CLKEYS )

C
C        Is the token that we found a recognized key?
C
         IF ( CLIDX .NE. 0 ) THEN

C
C           Yes, it is. Is it the first key that we have found?
C
            IF ( PCLIDX .NE. 0 ) THEN

C
C              No it is not. We need to save the value of the previous
C              key.
C
C              Compute the begin and end positions of the sub-string
C              that contains the previous value by looking for the
C              previous and current keys in the upper-cased remainder of
C              the input line.
C
C              The begin position is the position of the previous key
C              plus its length. The end position is the position of the
C              front-n-back blank-padded current key.
C
               CALL UCASE( LINE, ULINE )
               BEGPOS =
     .            POS(ULINE, CLKEYS(PCLIDX)(:RTRIM(CLKEYS(PCLIDX))), 1)
     .            + RTRIM( CLKEYS(PCLIDX) )

               HKEY = ' ' // CLKEYS(CLIDX)(:RTRIM(CLKEYS(CLIDX)))
               ENDPOS = POS(ULINE//' ', HKEY(:RTRIM(HKEY)+1), BEGPOS )

C
C              Extract the value, left-justify it, and RTRIM it. Set
C              "value found" flag to .TRUE.
C
               CLVALS(PCLIDX) = LINE(BEGPOS:ENDPOS)
               CALL LJUST( CLVALS(PCLIDX), CLVALS(PCLIDX) )
               CLVALS(PCLIDX) = CLVALS(PCLIDX)(:RTRIM(CLVALS(PCLIDX)))

               CLFLAG(PCLIDX) = .TRUE.

C
C              Check whether we already parsed the whole line. It will
C              be so if the remainder of the buffer holding the
C              sub-string that we examine word-by-word is a blank
C              string.
C
               IF ( HLINE .NE. ' ' ) THEN

C
C                 No, we did not parse the whole line yet. There is
C                 more stuff to parse and we reset the temporary
C                 sub-string buffer to hold the part of the input string
C                 starting with the first character after the current
C                 key -- the end position plus the length of the
C                 current key.
C                 
C
                  HLINE = LINE(ENDPOS+1+RTRIM(CLKEYS(CLIDX)):)

               END IF

C
C              Now reset the line to its portion starting with the
C              first character of the current key.
C
               LINE  = LINE(ENDPOS+1:)

            ELSE

C
C              This is the first key that we have found. Set UNPRSD
C              to the part of the line from the start to this key.
C
               CALL UCASE( LINE, ULINE )

               HKEY = ' ' // CLKEYS(CLIDX)(:RTRIM(CLKEYS(CLIDX)))
               BEGPOS = POS( ' ' // ULINE, HKEY(:RTRIM(HKEY)+1), 1 )
               IF ( BEGPOS .LE. 1 ) THEN
                  UNPRSD = ' '
               ELSE
                  UNPRSD = LINE(1:BEGPOS-1)
               END IF
               
            END IF

C
C           Save the current key index in as previous.
C
            PCLIDX = CLIDX

         END IF

      END DO

C
C     If we found at least one recognized key, we need to save the last
C     value.
C
      IF ( PCLIDX .NE. 0 ) THEN

C
C        Set "found any" output flag and "found previous key" flags to
C        .TRUE.
C
         FOUND = .TRUE.
         CLFLAG(PCLIDX) = .TRUE.

C
C        Check if there was any value following the last key (there was
C        if the non-blank length of what's left in the line starting 
C        with the last key if greater than the non-blank length of the
C        last key).
C
         IF ( RTRIM(LINE) .GT. RTRIM(CLKEYS(PCLIDX)) ) THEN

C
C           Compute begin position of, extract, left justify and
C           RTRIM the last value.
C
            CALL UCASE( LINE, ULINE )
            BEGPOS =
     .         POS(ULINE,CLKEYS(PCLIDX)(:RTRIM(CLKEYS(PCLIDX))), 1)
     .          + RTRIM( CLKEYS(PCLIDX) )

            CLVALS(PCLIDX) = LINE(BEGPOS:)
            CALL LJUST( CLVALS(PCLIDX), CLVALS(PCLIDX) )
            CLVALS(PCLIDX) = CLVALS(PCLIDX)(:RTRIM(CLVALS(PCLIDX)))

         ELSE

C
C           The key was the last thing on the line. So, it's value is
C           blank.
C
            CLVALS(PCLIDX) = ' '

         END IF

      END IF

      CALL CHKOUT ( 'PARCML' )
      RETURN
      END

