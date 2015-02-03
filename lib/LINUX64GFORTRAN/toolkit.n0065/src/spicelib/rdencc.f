 
C$Procedure  RDENCC  ( Read encoded characters from a text file )
 
      SUBROUTINE RDENCC ( UNIT, N, DATA )
 
C$ Abstract
C
C     Read and decode encoded characters from a text file.
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
C     CONVERSION
C     UTILITY
C
C$ Declarations
 
      INTEGER               UNIT
      INTEGER               N
      CHARACTER*(*)         DATA(*)
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      UNIT      I    Fortran unit number of input text file.
C      N         I    Number of characters to be read and decoded.
C      DATA      O    List of decoded characters to be returned.
C
C$ Detailed_Input
C
C     UNIT     The Fortran unit number for a previously opened text
C              file. All reading will begin at the CURRENT POSITION
C              in the text file.
C
C     N        The number of characters to be read from the text file
C              attached to UNIT.
C
C$ Detailed_Output
C
C     DATA     List of characters which were read from the text file
C              attached to UNIT and decoded.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1)   If N, the number of data items, is not positive, the error
C          SPICE(INVALIDARGUMENT) will be signalled.
C
C     2)   If an error occurs while reading from the text file
C          attached to UNIT, the error SPICE(FILEREADFAILED) will
C          be signalled.
C
C     3)   If an error occurs while decoding a character, the error
C          SPICE(DECODINGERROR) will be signalled.
C
C$ Files
C
C     See the description of UNIT in Detailed_Input.
C
C$ Particulars
C
C     This routine will read quoted character strings of length
C     MAXENC containing encoded characters produced by the routine
C     WRENCC, or some equivalent procedure. The reading begins at
C     the current position in a previously opened text file attached
C     to logical UNIT and continues until N contiguous characters
C     have been successfully decoded and placed in the data buffer
C     DATA or an error occurs. The current position in a file is
C     defined to be the text line immediately following the last text
C     line that was written or read.
C
C     The character strings are quoted so that a Fortran list directed
C     read may be used to read them, rather than a formatted read with
C     the format specifier FMT = '(A)'.
C
C     As the characters are decoded they are placed into the first N
C     contiguous positions in the data buffer DATA, where the first N
C     contiguous positions are determined by moving from the lowest
C     array indices to highest array indices, i.e., moving from ``left''
C     to ``right'' and ``top'' to ``bottom'' in the character array
C     DATA, beginning at the first character position, DATA(1)(1:1). So,
C     logically all of the quoted strings containing encoded data can
C     be thought of as being concatenated together into one long
C     character string.
C
C     This routine is one of a pair of routines which are used to
C     encode and decode ASCII characters:
C
C           WRENCC -- Encode and write ASCII characters to a file.
C           RDENCC -- Read and decode ASCII characters from a file.
C
C     The encoding/decoding of characters is performed to provide
C     a portable means for transferring character data values.
C
C     This routine is for use with the ASCII character set and
C     extensions to it. The supported characters must have decimal
C     values in the range from 0 to 255.
C
C$ Examples
C
C     The following examples demonstrate the use of this routine. In
C     each of the examples, the variable UNIT is the Fortran logical
C     unit of a previously opened text file, and the variable N is
C     an integer which will represent the number of characters to be
C     read and decoded.
C
C     The first example demonstrates a typical correct usage of this
C     routine. The second example demonstrates what would probably be
C     the most common incorrect usage of this routine. These examples
C     are meant to be illustrative, so for the sake of brevity and
C     clarity, the length of the quoted strings expected in the input
C     text file has been shortened.
C
C     The examples use as data correctly and incorrectly encoded
C     versions of the following character string which has a length
C     of exactly 64 characters:
C
C        'Here is some data. What follows is more '//
C        'data. This is more data.                '
C
C     Example 1
C     ---------
C
C        This example demonstrates a typical usage of this routine.
C
C        Let the symbol '-->' denote the file pointer.
C
C        Let the current file pointer position and succeeding data be
C        the following:
C
C           --> 'Here is some data. W'
C               'hat follows is more '
C               'data. This is more d'
C               'ata.                '
C
C        There are exactly N = 64 characters of encoded data.
C
C        Let the character data buffer have the following
C        declaration in the calling program:
C
C           CHARACTER*(40)         DATA(2)
C
C        Then, the subroutine call
C
C           CALL RDENCC( UNIT, N, DATA )
C
C        with N = 64 would produce the following results:
C
C           DATA(1) = 'Here is some data. What follows is more '
C           DATA(2) = 'data. This is more data.'
C
C     Example 2
C     ---------
C
C        This example is meant to demonstrate what would probably be
C        a common misuse of this routine.
C
C        Let the symbol '-->' denote the file pointer.
C
C        Let the current file pointer position and succeeding data be
C        the following:
C
C           --> 'Here is some data.  '
C               'What follows is more'
C               'data. This is more  '
C               'data.               '
C
C        As in example 1, there are exactly N = 64 characters of
C        encoded data, but to make the data more ``readable'' two extra
C        spaces have been added: one at the end of the first line and
C        one at the end of the third line.
C
C        Let the character data buffer have the following
C        declaration in the calling program:
C
C           CHARACTER*(40)         DATA(2)
C
C        Then, the subroutine call
C
C           CALL RDENCC( UNIT, N, DATA )
C
C        with N = 64 would produce the following results:
C
C           DATA(1) = 'Here is some data.  What follows is more'
C           DATA(2) = ' data. This is  more dat'
C
C        This is probably not what was desired. The problem is that
C        the ``significant'' characters in the encoded string do not
C        appear contiguously; an ``extra'' blank appears at the end
C        of the first and third encoded quoted strings.
C
C     Example 3
C     ---------
C
C        This example demonstrates the use of WRENCC and RDENCC for
C        writing and subsequent reading of character data using data
C        buffers that are ``shaped'' differently, i.e., that have
C        different dimensions.
C
C        Let the input and output character data buffers have the
C        following declarations:
C
C           CHARACTER*(25)  OUTBUF(3)
C           CHARACTER*(10)  INPBUF(7)
C
C        Further, let the output buffer contain the following data:
C
C           OUTBUF(1) = 'Today is the first day of'
C           OUTBUF(2) = ' the rest of my life, so '
C           OUTBUF(3) = 'I will enjoy it.'
C
C        There are exactly N = 66 significant characters in the output
C        buffer. The code fragment
C
C           N = 66
C           CALL WRENCC ( UNIT, N, OUTBUF )
C           REWIND ( UNIT )
C           CALL RDENCC ( UNIT, N, INPBUF )
C
C        has the effect of placing the original data into the
C        differently ``shaped'' input buffer with the following
C        results:
C
C           INPBUF(1) = 'Today is t'
C           INPBUF(2) = 'he first d'
C           INPBUF(3) = 'ay of the '
C           INPBUF(4) = 'rest of my'
C           INPBUF(5) = ' life, so '
C           INPBUF(6) = 'I will enj'
C           INPBUF(7) = 'oy it.    '
C
C       No information has been lost, it is simply arranged differently.
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
C     K.R. Gehringer (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 20-OCT-1992 (KRG)
C
C-&
 
C$ Index_Entries
C
C      read encoded characters from a text file
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
C
C     Local parameters
C
      CHARACTER*(1)         ESCCHR
      PARAMETER           ( ESCCHR = '@')
 
      INTEGER               MXESCD
      PARAMETER           ( MXESCD = 2 )
 
      INTEGER               MAXENC
      PARAMETER           ( MAXENC = 64 )
 
      INTEGER               ERRLEN
      PARAMETER           ( ERRLEN = 80 )
C
C     Local variables
C
      CHARACTER*(1)         CH
      CHARACTER*(MAXENC)    ENCCHR
      CHARACTER*(ERRLEN)    ERRMSG
      CHARACTER*(MXESCD)    HEXNUM
 
 
      INTEGER               DTALEN
      INTEGER               DTALIN
      INTEGER               DTAPOS
      INTEGER               ENCPOS
      INTEGER               INTCH
      INTEGER               IOSTAT
      INTEGER               NCHARS
      INTEGER               NESCD
 
      LOGICAL               ERROR
      LOGICAL               ESCAPE
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'RDENCC' )
      END IF
C
C     Check to see if the number of data items is less than or equal
C     to zero. If it is, signal an error.
C
      IF ( N .LT. 1 ) THEN
 
         CALL SETMSG ( 'The number of data items to be read was' //
     .                 ' not positive: #.'                        )
         CALL ERRINT ( '#', N                                     )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                   )
         CALL CHKOUT ( 'RDENCC'                                   )
         RETURN
 
      END IF
C
C     Initialize some stuff here
C
C     Make sure that the encoding character string is empty when we
C     start.
C
      ENCCHR = ' '
C
C     We have not encountered any errors yet, so set the error indicator
C     to .FALSE..
C
      ERROR = .FALSE.
C
C     Get the length of a data ``line'' in the data buffer DATA.
C
      DTALEN = LEN( DATA(1) )
C
C     We are not currently parsing an escaped character, so set the
C     escape indicator to .FALSE. and set the number of escape digits
C     to zero.
C
      ESCAPE = .FALSE.
      NESCD  = 0
C
C     Set the initial line and position for the output data buffer.
C
      DTAPOS = 1
      DTALIN = 1
C
C     Set the initial position in the encoding buffer to be 1 too
C     big so that we read an encoded character string from the file
C     attached to UNIT on the first pass through the loop.
C
      ENCPOS = MAXENC + 1
C
C     Set the number of characters decoded to zero and begin the
C     decoding loop.
C
      NCHARS = 0
      DO WHILE ( NCHARS .LT. N )
C
C        If the last character we processed was the last one in the
C        encoded character string, then we need to read in the next
C        encoded character string from the file. This also accomplishes
C        the task of reading in the first encoded character string.
C
         IF ( ENCPOS .GT. MAXENC ) THEN
 
            READ(UNIT,*,IOSTAT=IOSTAT) ENCCHR
            IF ( IOSTAT .NE. 0 ) THEN
 
               CALL SETMSG ( 'Error reading from logical unit #,' //
     .                       ' IOSTAT = #.'                        )
               CALL ERRINT ( '#', UNIT                             )
               CALL ERRINT ( '#', IOSTAT                           )
               CALL SIGERR ( 'SPICE(FILEREADFAILED)'               )
               CALL CHKOUT ( 'RDENCC'                              )
               RETURN
 
            END IF
C
C           Set the pointer for the encoded character buffer to the
C           beginning of the buffer.
C
            ENCPOS = 1
 
         END IF
 
         CH = ENCCHR(ENCPOS:ENCPOS)
C
C        If we are processing a character which was escaped when it was
C        encoded, we need to do some special stuff.
C
         IF ( ESCAPE ) THEN
 
            NESCD = NESCD + 1
 
            IF ( NESCD .EQ. MXESCD ) THEN
C
C              If we have all of the digits in the encoded character,
C              then decode it.
C
               HEXNUM(NESCD:NESCD) = CH
               CALL HX2INT( HEXNUM, INTCH, ERROR, ERRMSG )
 
               IF ( ERROR ) THEN
 
                  CALL SETMSG ( 'Decoding error occurred while'       //
     .                          ' attempting to decode item #: @#. #'  )
                  CALL ERRINT ( '#', NCHARS+1                          )
                  CALL ERRCH  ( '#', HEXNUM                            )
                  CALL ERRCH  ( '#', ERRMSG                            )
                  CALL SIGERR ( 'SPICE(DECODINGERROR)'                 )
                  CALL CHKOUT ( 'RDENCC'                               )
                  RETURN
 
               END IF
 
               CH = CHAR( INTCH )
C
C              We now have the decoded character. We are no longer
C              processing an escaped character, so set the escape
C              indicator to .FALSE. and continue. The character we
C              just decoded will be placed into the data buffer DATA
C              below.
C
               ESCAPE = .FALSE.
               NESCD  = 0
 
            ELSE IF ( ( NESCD .LT. MXESCD ) .AND.
     .                ( NESCD .GT. 0      ) ) THEN
C
C              Otherwise we are still collecting the digits of the
C              encoded character, so store the current character and
C              move on to the next one.
C
               HEXNUM(NESCD:NESCD) = CH
 
            END IF
 
         ELSE
C
C           Check to see if the current character is the escape
C           character. If it is, we need to set the escape indicator
C           to .TRUE. so that we correctly process the encoded
C           digits.
C
            IF ( CH .EQ. ESCCHR ) THEN
 
               ESCAPE = .TRUE.
 
            END IF
 
         END IF
C
C        At this point one of the following is true:
C
C           (1) CH contains a character to be placed into the data
C               buffer DATA.
C
C           (2) We are currently building an escaped character from
C               its escape sequence, ESCAPE = .TRUE., and CH contains
C               some part of the escape sequence.
C
C        If we are not currently decoding an escaped character, then
C        we need to store the character value that we have in the data
C        buffer, and move on to the next character.
C
         IF ( .NOT. ESCAPE ) THEN
 
            NCHARS = NCHARS + 1
C
C           If the position in the data buffer is greater than the
C           length  of a data line (DTALEN) then we need to increment
C           the current data line (DTALIN) and reset the current data
C           line buffer position (DTAPOS).
C
            IF ( DTAPOS .GT. DTALEN ) THEN
 
               DTALIN = DTALIN + 1
               DTAPOS = 1
 
            END IF
C
C           Store the current character in the data buffer and
C           increment the buffer position.
C
            DATA(DTALIN)(DTAPOS:DTAPOS) = CH
            DTAPOS                      = DTAPOS + 1
 
         END IF
C
C        Increment the encoded character buffer position
C
         ENCPOS = ENCPOS + 1
C
C        At this point, we know the following:
C
C        (1)  1 <= ENCPOS <= MAXENC
C        (2)  1 <= NCHARS <= N
C        (3)  1 <= DTAPOS <= DTALEN
C        (4)  1 <= DTALIN
C        (5)  0 <= NESCD <= MXESCD
C        (6)  ESCAPE is .TRUE. if we are currently decoding an escaped
C             character, otherwise it is .FALSE..
C
      END DO
 
      CALL CHKOUT ( 'RDENCC' )
      RETURN
      END
