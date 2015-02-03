C$Procedure WRENCC ( Write characters to text file encoded )
 
      SUBROUTINE WRENCC ( UNIT, N, DATA )
 
C$ Abstract
C
C     Encode and write characters to a text file.
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
      CHARACTER*(*)         DATA ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      UNIT      I    Fortran unit number of output text file.
C      N         I    Number of characters to encode and write.
C      DATA      I    List of characters to encode and write.
C
C$ Detailed_Input
C
C     UNIT     The Fortran unit number for a previously opened text
C              file. All writing will begin at the CURRENT POSITION
C              in the text file.
C
C     N        The number of data items, characters, to be encoded and
C              written to the text file attached to UNIT.
C
C     DATA     List of characters to be encoded and written to the
C              text file attached to UNIT.
C
C$ Detailed_Output
C
C     See the Particulars section for a description of the effect of
C     this routine.
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
C     2)   If an error occurs while writing to the text file attached
C          to unit UNIT, the error SPICE(FILEWRITEFAILED) will be
C          signalled.
C
C     3)   If the Fortran logical unit UNIT is not defined, the results
C          of this routine are unpredictable.
C
C$ Files
C
C     See the description of UNIT in the Detailed_Input section.
C
C$ Particulars
C
C     This routine will encode and write the first N contiguous
C     characters contained in the data buffer array DATA. The
C     encoded characters will be written to a previously opened
C     text file attached to logical unit UNIT beginning at the
C     current position in the file. The current position in a
C     file is defined to be the text line immediately following
C     the last text line that was written or read.
C
C     The first N contiguous characters in the data buffer array
C     DATA are defined to be those N characters encountered while
C     moving from the lowest array indices to highest array indices,
C     i.e., those characters encountered while moving from ``left''
C     to ``right'' and ``top'' to ``bottom'' in the character array
C     DATA, beginning at the first character position, DATA(1)(1:1).
C     Logically all of the array elements in the data buffer DATA
C     containing characters to be encoded can be thought of as being
C     concatenated together into one long character string.
C
C     On any single call to this routine, the encoded characters
C     will be contiguous when written, and all but possibly the
C     final character string written to the file will contain
C     MAXENC characters. The last, if it does not contain MAXENC
C     characters, will be padded with blanks so that it has a
C     length of MAXENC characters. The encoded character strings
C     are meant to be read and processed in blocks of MAXENC
C     characters.
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
C     The encoded characters are written to the output text file as
C     quoted character strings so that a Fortran list directed read
C     may be used to read the character strings, rather than a Fortran
C     formatted read with format specifier FMT = '(A)'.
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
C     encoded.
C
C     The first example demonstrates a typical correct usage of this
C     routine. The second example demonstrates what would probably
C     be the most common incorrect usage of this routine. The first
C     two examples are attempting to encode the sentence 'This is the
C     data.', which has a length of N = 17 characters. The third
C     example presents ``before'' and ``after'' pictures of the complete
C     ASCII character set.
C
C     Example 1
C     ---------
C
C        This example demonstrates a typical usage of this routine.
C
C        Let the character data buffer have the following declaration
C        in the calling program:
C
C           CHARACTER*(4)         DATA(5)
C
C        We make the following variable assignments:
C
C           DATA(1) = 'This'
C           DATA(2) = ' is '
C           DATA(3) = 'the '
C           DATA(4) = 'data'
C           DATA(5) = '.'
C           N = 17
C
C        The subroutine call
C
C           CALL WRENCC( UNIT, N, DATA )
C
C        will produce a record in the text file attached to the
C        logical unit UNIT which is identical to the following
C        except for the length of the character string written.
C
C           'This is the data.                             '
C
C
C     Example 2
C     ---------
C
C        This example is meant to demonstrate what would probably be
C        a common misuse of this routine.
C
C        Let the character data buffer have the following declaration
C        in the calling program:
C
C           CHARACTER*(10)         DATA(2)
C
C        We make the following variable assignments:
C
C           DATA(1) = 'This is'
C           DATA(2) = ' the data.'
C           N = 17
C
C        The subroutine call
C
C           CALL WRENCC( UNIT, N, DATA )
C
C        will produce a record in the text file attached to the
C        logical unit UNIT which is identical to the following
C        except for the length of the character string written.
C
C           'This is    the da                             '
C
C        This is probably not what was intended. The problem is that
C        all of the characters which were to be encoded did not appear
C        contiguously in the data buffer DATA. The first element of the
C        character string array DATA has three ``extra'' blanks
C        following the 's' in the word 'is'. To correctly encode the
C        data, the following assignments should be made:
C
C           DATA(1) = 'This is th'
C           DATA(2) = 'e data.'
C
C     Example 3
C     ---------
C
C        This example presents the results of applying WRENCC to
C        the complete ASCII character set and an extension with
C        characters having decimal values form 128 to 255.
C
C        Let the character data buffer have the following declaration
C        in the calling program:
C
C           CHARACTER*(1)          DATA(0:255)
C
C        Then, letting
C
C           DATA(I) = CHAR( I ), I = 0, 255
C           N = 256
C
C        the subroutine call
C
C           CALL WRENCC( UNIT, N, DATA )
C
C        would produce
C
C     '@00@01@02@03@04@05@06@07@08@09@0A@0B@0C@0D@0E@0F@10@11@12@13@14@'
C     '15@16@17@18@19@1A@1B@1C@1D@1E@1F !"#$%&@27()*+,-./0123456789:;<='
C     '>?@40ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{'
C     '|}~@7F@80@81@82@83@84@85@86@87@88@89@8A@8B@8C@8D@8E@8F@90@91@92@'
C     '93@94@95@96@97@98@99@9A@9B@9C@9D@9E@9F@A0@A1@A2@A3@A4@A5@A6@A7@A'
C     '8@A9@AA@AB@AC@AD@AE@AF@B0@B1@B2@B3@B4@B5@B6@B7@B8@B9@BA@BB@BC@BD'
C     '@BE@BF@C0@C1@C2@C3@C4@C5@C6@C7@C8@C9@CA@CB@CC@CD@CE@CF@D0@D1@D2@'
C     'D3@D4@D5@D6@D7@D8@D9@DA@DB@DC@DD@DE@DF@E0@E1@E2@E3@E4@E5@E6@E7@E'
C     '8@E9@EA@EB@EC@ED@EE@EF@F0@F1@F2@F3@F4@F5@F6@F7@F8@F9@FA@FB@FC@FD'
C     '@FE@FF                                                          '
C
C     Example 4
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
C     F.S. Turner    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.28.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 1.27.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 1.26.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 1.25.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 1.24.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    SPICELIB Version 1.23.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    SPICELIB Version 1.22.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    SPICELIB Version 1.21.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    SPICELIB Version 1.20.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    SPICELIB Version 1.19.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    SPICELIB Version 1.18.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 1.17.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    SPICELIB Version 1.16.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    SPICELIB Version 1.15.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    SPICELIB Version 1.14.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 1.13.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    SPICELIB Version 1.12.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    SPICELIB Version 1.11.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    SPICELIB Version 1.10.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    SPICELIB Version 1.9.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    SPICELIB Version 1.8.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    SPICELIB Version 1.7.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    SPICELIB Version 1.6.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    SPICELIB Version 1.5.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-    SPICELIB Version 1.4.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    SPICELIB Version 1.3.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 1.3.0, 05-DEC-2001 (FST)
C
C        Replaced ICHAR with the statement function ZZICHR
C        to fix a problem on some PC-LINUX environments.
C
C-    SPICELIB Version 1.2.0, 09-SEP-1993 (KRG)
C
C        The list directed write was changed to a formatted write using
C        the specifier FMT='(A)'. This was done in order to prevent a
C        space from appearing as the first character on each line of the
C        file for certian computer platforms.
C
C-    SPICELIB Version 1.1.0, 08-MAR-1993 (KRG)
C
C        The variables INTESC, INTFPC, INTLPC, INTQUO were not saved
C        when they should have been. This eventually caused some
C        problems, so it was fixed. They are now saved.
C
C-    SPICELIB Version 1.0.0, 20-OCT-1992 (KRG)
C
C-&
 
C$ Index_Entries
C
C      encode and write characters to a text file
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.3.0, 05-DEC-2001 (FST)
C
C        Previous versions of this routine required the range
C        of ICHAR to be [0,255].  This is not the case on some
C        environments, so references to ICHAR were replaced
C        with a ZZICHR statement function that returns values
C        in this range for all supported environments.
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local parameters
C
      CHARACTER*(1)         QUOTE
      PARAMETER           ( QUOTE = '''' )
 
      CHARACTER*(1)         ESCCHR
      PARAMETER           ( ESCCHR = '@')
 
      CHARACTER*(1)         CHRFPC
      PARAMETER           ( CHRFPC = ' ')
 
      CHARACTER*(1)         CHRLPC
      PARAMETER           ( CHRLPC = '~')
 
      INTEGER               MAXENC
      PARAMETER           ( MAXENC = 64 )
 
      INTEGER               HEXBAS
      PARAMETER           ( HEXBAS = 16 )
 
C
C     Local variables
C
      CHARACTER*(1)         CARG
      CHARACTER*(1)         CH
      CHARACTER*(MAXENC)    ENCCHR
      CHARACTER*(1)         HEXDIG(0:15)
      CHARACTER*(2)         LFTOVR
 
      INTEGER               DTALEN
      INTEGER               DTALIN
      INTEGER               DTAPOS
      INTEGER               ENCPOS
      INTEGER               HIBITS
      INTEGER               INTCH
      INTEGER               INTESC
      INTEGER               INTFPC
      INTEGER               INTLPC
      INTEGER               INTQUO
      INTEGER               IOSTAT
      INTEGER               LOBITS
      INTEGER               NCHARS
      INTEGER               NCHOUT
      INTEGER               ROOM
 
      LOGICAL               FIRST
 
C
C     Statement Functions
C
      INTEGER               ZZICHR
 
C
C     Saved variables
C
      SAVE                  HEXDIG
      SAVE                  INTESC
      SAVE                  INTFPC
      SAVE                  INTLPC
      SAVE                  INTQUO
 
      SAVE                  FIRST
 
C
C     Initial values
C
C     Define the hexadecimal digits
C
      DATA                  HEXDIG /
     .                               '0', '1', '2', '3',
     .                               '4', '5', '6', '7',
     .                               '8', '9', 'A', 'B',
     .                               'C', 'D', 'E', 'F'
     .                             /
 
      DATA                  FIRST    / .TRUE. /
 
C
C     Statement Function Definitions
C
C     This function controls the conversion of characters to integers.
C     Some versions of the g77 implement ICHAR with a signed integer.
C     This function computes the value of ICHAR that this code requires
C     on any version of g77 for x86 Linux.
C
      ZZICHR(CARG) = ICHAR(CARG) - MAX( -1, MIN(0,ICHAR(CARG)) )*256
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'WRENCC' )
      END IF
 
      IF ( FIRST ) THEN
C
C        Initialize the integer values for the special characters
C
         FIRST  = .FALSE.
         INTESC = ZZICHR( ESCCHR )
         INTQUO = ZZICHR( QUOTE  )
         INTFPC = ZZICHR( CHRFPC )
         INTLPC = ZZICHR( CHRLPC )
 
      END IF
C
C     Get the length of a data ``line'' in the data buffer DATA.
C
      DTALEN = LEN( DATA(1) )
C
C     Make sure that the encoding character string is empty when we
C     start.
C
      ENCCHR = ' '
C
C     Check to see if the number of data items is less than or equal
C     to zero. If it is, signal an error.
C
      IF ( N .LT. 1 ) THEN
 
         CALL SETMSG ( 'The number of data items to be written was' //
     .                 ' not positive: #.'                           )
         CALL ERRINT ( '#', N                                        )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                      )
         CALL CHKOUT ( 'WRENCC'                                      )
         RETURN
 
      END IF
C
C     We need to begin scanning through the characters and placing them
C     into a temporary buffer that is an appropriate length for output
C     to the text file (see the parameter MAXENC above).
C
C     Initialize all of the counters and pointers used to move through
C     the various character data buffers and count the number of
C     characters processed.
C
C     Initialize the data line and data line position.
C
      DTALIN = 1
      DTAPOS = 1
C
C     Initialize the encoded character buffer position.
C
      ENCPOS = 1
C
C     Set the number of characters encoded to zero, and set the number
C     of characters output to zero. The number of output characters may
C     be larger than the number of characters because characters that
C     are escaped are more than one character in length.
C
      NCHARS = 0
      NCHOUT = 0
 
      DO WHILE ( NCHARS .LT. N )
C
C        At this point, we know the following:
C
C           (1) 1 <= ENCPOS <= MAXENC
C           (2) 1 <= DTAPOS <= DTALEN
C           (3) 1 <= DTALIN
C           (4) 0 <= NCHARS <= N
C           (5) 0 <= NCHOUT
C
         CH    = DATA(DTALIN)(DTAPOS:DTAPOS)
         INTCH = ZZICHR( CH )
C
C        If the character is a special character, then encode it and
C        place it in the encoded character buffer. Otherwise the
C        character is a printing character, so just put it in the
C        encoded character buffer.
C
         IF ( ( INTCH .LT. INTFPC ) .OR.
     .        ( INTCH .GT. INTLPC ) .OR.
     .        ( INTCH .EQ. INTESC ) .OR.
     .        ( INTCH .EQ. INTQUO ) ) THEN
C
C           The character is a nonprinting character, the escape
C           character, or a single quote, and so we need to encode
C           it using the escape character ESCCHR followed by two
C           hexadecimal digits which represent the position of the
C           character in the ASCII character sequence.
C
            HIBITS                = INTCH / HEXBAS
            LOBITS                = INTCH - HIBITS * HEXBAS
            ENCCHR(ENCPOS:ENCPOS) = ESCCHR
C
C           We need to see if there is enough room in the encoded
C           character buffer to place all of the hexadecimal digits
C           in the encoding. If not, we need to put what we can in the
C           encoded character buffer and temporarily store the rest,
C           which will be placed in the encoded character buffer after
C           the filled buffer is written to the file.
C
            ROOM = MAXENC - ENCPOS
 
            IF ( ROOM .GE. 2 ) THEN
 
               ENCCHR(ENCPOS+1:ENCPOS+1) = HEXDIG(HIBITS)
               ENCCHR(ENCPOS+2:ENCPOS+2) = HEXDIG(LOBITS)
 
            ELSE IF ( ROOM .EQ. 1 ) THEN
 
               ENCCHR(ENCPOS+1:ENCPOS+1) = HEXDIG(HIBITS)
               LFTOVR(1:1)               = HEXDIG(LOBITS)
               LFTOVR(2:2)               = ' '
 
            ELSE
 
               LFTOVR(1:1) = HEXDIG(HIBITS)
               LFTOVR(2:2) = HEXDIG(LOBITS)
 
            END IF
C
C           Increment the character buffer pointers, including the
C           pointer for the encoded character (possibly over
C           incrementing, but that's OK).
C
            NCHARS = NCHARS + 1
            DTAPOS = DTAPOS + 1
            ENCPOS = ENCPOS + 3
            NCHOUT = NCHOUT + 3
 
         ELSE
C
C           The character is a printing character, and we encode it
C           as itself and increment the character buffer pointers
C           appropriately.
C
            ENCCHR(ENCPOS:ENCPOS) = CH
            NCHARS                = NCHARS + 1
            DTAPOS                = DTAPOS + 1
            ENCPOS                = ENCPOS + 1
            NCHOUT                = NCHOUT + 1
 
         END IF
C
C        If we have filled the encoded character buffer, we need to
C        write it out to the file and prepare it for reuse.
C
         IF ( ENCPOS .GT. MAXENC ) THEN
C
C           Write out the encoded character buffer placing single
C           quotes around it so that it may be read using a Fortran
C           list directed read statement rather than the format
C           specifier FMT = '(A)'.
C
            WRITE ( UNIT,
     .              FMT='(A)',
     .              IOSTAT=IOSTAT )
     .              QUOTE // ENCCHR // QUOTE
 
            IF ( IOSTAT .NE. 0 ) THEN
 
               CALL SETMSG ( 'Error writing to logical unit #,' //
     .                       ' IOSTAT = #.'                      )
               CALL ERRINT ( '#', UNIT                           )
               CALL ERRINT ( '#', IOSTAT                         )
               CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'            )
               CALL CHKOUT ( 'WRENCC'                            )
               RETURN
 
            END IF
C
C           Get ready to fill up the encoded character buffer again,
C           taking care to place any leftover characters in the buffer
C           first.
C
            NCHOUT = NCHOUT - MAXENC
 
            IF ( NCHOUT .GT. 0 ) THEN
 
               ENCCHR(1:2) = LFTOVR(1:2)
 
            END IF
 
            ENCPOS          = 1 + NCHOUT
            ENCCHR(ENCPOS:) = ' '
            LFTOVR          = ' '
 
         END IF
C
C        If we have reached the end of the current data ``line'' in the
C        data buffer DATA, we need to increment the data line pointer
C        and reset the data position pointer.
C
         IF ( DTAPOS .GT. DTALEN ) THEN
 
            DTALIN = DTALIN + 1
            DTAPOS = 1
 
         END IF
 
      END DO
C
C     If the number of output characters remaining is greater than
C     zero, we need to flush the encoded character buffer before
C     exiting, because we have a partially filled encoded character
C     buffer. Otherwise, we're done.
C
C     This last encoded string that is written will be padded with
C     blanks out to MAXENC character positions, so there is no
C     ``garbage'' written at the end of the data.
C
      IF ( NCHOUT .GT. 0 ) THEN
C
C        Write out the encoded character buffer placing single
C        quotes around it so that it may be read using a Fortran
C        list directed read statement rather than the format
C        specifier FMT = '(A)'.
C
         WRITE ( UNIT,
     .           FMT='(A)',
     .           IOSTAT=IOSTAT)
     .           QUOTE // ENCCHR // QUOTE
 
         IF ( IOSTAT .NE. 0 ) THEN
 
            CALL SETMSG ( 'Error writing to logical unit #,' //
     .                    ' IOSTAT = #.'                      )
            CALL ERRINT ( '#', UNIT                           )
            CALL ERRINT ( '#', IOSTAT                         )
            CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'            )
            CALL CHKOUT ( 'WRENCC'                            )
            RETURN
 
         END IF
 
      END IF
 
      CALL CHKOUT ( 'WRENCC' )
      RETURN
      END
