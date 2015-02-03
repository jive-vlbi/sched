C$Procedure      DPFMT ( Format a double precision number )
 
      SUBROUTINE DPFMT ( X, PICTUR, STR )
      IMPLICIT NONE
 
C$ Abstract
C
C     Using a picture, create a formatted string that represents a
C     double precision number.
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
C     ALPHANUMERIC
C     CONVERSION
C     UTILITY
C
C$ Declarations
 
      DOUBLE PRECISION      X
      CHARACTER*(*)         PICTUR
      CHARACTER*(*)         STR
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     X          I   a double precision number.
C     PICTUR     I   a string describing the appearance of the output
C     STR        O   a string representing X as prescribed by PICTUR
C
C$ Detailed_Input
C
C     X          is any double precision number.
C
C     PICTUR     is a string used to describe the format of the
C                output string.  There are four special characters
C                recognized by DPFMT --- a leading + or -, a leading
C                zero ( '0' ) or a zero that follows a leading + or -,
C                and the first decimal point of the string.
C
C                All other non-blank characters are regarded as
C                equivalent.  The picture ends at the first blank
C                character.  The effects associated with the various
C                characters in a picture are spelled out in the
C                description of the output STRING.
C
C                The following pictures are treated as errors.
C
C                ' ', '+', '-', '.', '+.', '-.'
C
C$ Detailed_Output
C
C     STRING     is a string representing X that matches the input
C                picture.  The format of STRING is governed by PICTUR.
C                It will represent X rounded to the level of precision
C                specified by PICTUR.
C
C                If the first character of the picture is a minus sign,
C                the first character in the output string will be
C                a blank if the number is non-negative, a minus sign
C                if the number is negative.
C
C                If the first character of the picture is a plus sign,
C                the first character of the output string will be a
C                plus if the number is positive, a blank if the number
C                is zero, and a minus sign if the number is negative.
C
C                If the first character of the string is NOT a sign
C                (plus or minus) the first character of the output
C                string will be a minus sign if the number is negative
C                and will be the first character of the integer part
C                of the number otherwise.
C
C                The integer portion of STRING will contain the same
C                number of characters as appear before the decimal
C                point (or last character if there is no decimal
C                point) but after a leading + or -.
C
C                If the picture begins with any of the following
C
C                   '+0', '-0', or '0'
C
C                it is said to have a leading zero.  If a picture has
C                a leading zero and the integer portion is not large
C                enough to fill up the integer space specified by
C                PICTUR, STRING will be zero padded from the sign (if
C                one is required) up to the first character of the
C                integer part of the number.
C
C                If picture does NOT have a leading zero and the
C                integer portion is not large enough to fill up the
C                space specified by PICTUR, STRING will be blank
C                padded on the left between the sign (if one is
C                required) and the first character of the integer part
C                of the number.
C
C                If a decimal point ( '.' ) is present in PICTUR it
C                will be present following the integer portion of
C                STRING. Moreover, the decimal portion of STRING will
C                contain the same number of digits as there are
C                non-blank characters following the decimal point in
C                PICTUR.  However, only the first 14 digits starting
C                with the first non-zero digit are meaningful.
C
C                If the format specified by PICTUR does not provide
C                enough room for the integer portion of X, the routine
C                determines whether or not the number of characters
C                present in the picture is sufficient to create a
C                representation for X using scientific notation.  If
C                so, the output is displayed using scientific notation
C                (leading signs, if they are present in PICTUR, will
C                also appear in STRING).   If the format specified by
C                PICTUR is too short to accommodate scientific
C                notation, the output string is filled with '*' to the
C                same length as the length of PICTUR.  Leading signs
C                are not preserved in this overflow case.
C
C                STRING may overwrite PICTUR.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) A picture that begins with a blank will cause the error
C        'SPICE(NOPICTURE)' to be signalled.
C
C     2) A picture that consists only of '+', '-', '.', '+.' or '-.'
C        are regarded are regarded as errors (there's no significant
C        component to the picture.)  These pictures cause the error
C        'SPICE(BADPICTURE)' to be signalled.
C
C     3) If the length of STR is less than the length of the first
C        non-blank portion of PICTUR, the error 'SPICE(OUTPUTTOOSHORT)'
C        will be signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine provides a mechanism for producing numeric strings
C     formatted according to a user supplied picture. We expect that
C     the string produced by this routine will be used to assist in
C     the construction of a string that can be read by people.
C
C     Note that the process of converting a double precision number
C     to a string, in not precisely invertible even if the string
C     contains all of the significant figures allowed by this
C     routine.  You should not anticipate that the string produced
C     by this routine can be "read" into a double precision number
C     to reproduce the double precision number X. To the level of
C     accuracy implied by the string representation, they will be
C     the same.  But, they are unlikely to have the same internal
C     binary representation.
C
C$ Examples
C
C     Suppose that X has the binary representation of PI. Then the
C     table below illustrates the strings that would be produced
C     by a variety of different pictures.
C
C     PICTUR         |    STRING
C     -------------------------------
C     '0x.xxx'       |  '03.142'
C     'xx.xxx'       |  ' 3.142'
C     '+xxx.yyyy'    |  '+  3.1416'
C     '-.yyyy'       |  '******'
C     'xxxxxxxx'     |  '       3'
C     '00xx'         |  '0003'
C     '-00.0000000'  |  ' 03.1415927'
C     '00'           |  '03'
C     'x.'           |  '3.'
C     '.mynumber'    |  '3.142E+00'
C     'my dog spot'  |  ' 3'
C     'my.dog spot'  |  ' 3.142'
C     '+my.dog,spot' |  '+ 3.14159265'
C
C
C
C     Suppose that X has the binary representation of 2/3. Then the
C     table below illustrates the strings that would be produced
C     by a variety of different pictures.
C
C     PICTUR         |    STRING
C     -------------------------------
C     '+x.xxx'       |  '+0.667'
C     '+xx.xxx'      |  '+ 0.667'
C     'xxx.yyyy'     |  '  0.6667'
C     '.yyyy'        |  '.6667'
C     'xxxxxxxx'     |  '       1'
C     '00xx'         |  '0001'
C     '-0.0000000'   |  ' 0.6666667'
C     '00'           |  '01'
C     'x.'           |  '1.'
C     'mynumber'     |  '       1'
C     'my dog spot'  |  ' 1'
C     'my.dog spot'  |  ' 0.667'
C     'my.dog,spot'  |  ' 0.66666667'
C
C     Suppose that X has the binary representation of -8/9. Then the
C     table below illustrates the strings that would be produced
C     by a variety of different pictures.
C
C
C     PICTUR         |    STRING
C     -------------------------------
C     '+x.xxx'       |  '-0.889'
C     '-00.xxxx'     |  '-00.8889'
C     'xxx.xxx'      |  ' -0.889'
C     '000.000'      |  '-00.889'
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
C     W.L. Taber      (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 31-JAN-2008 (BVS)
C
C        Removed non-standard end-of-declarations marker
C        'C%&END_DECLARATIONS' from comments.
C
C-    Spicelib Version 1.0.1, 22-JUN-1998 (WLT)
C
C        A number of typographical and grammatical errors
C        were corrected in the header.
C
C-    Spicelib Version 1.0.0, 17-SEP-1996 (WLT)
C
C-&
 
C$ Index_Entries
C
C     format a string representing a d.p. number
C     string from a d.p. number and format picture
C
C-&
 
 
C$ Revisions
C
C     None.
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               POS
      INTEGER               NCPOS
 
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
 
      CHARACTER*(1)         FILL
      CHARACTER*(1)         SIGN
      CHARACTER*(WDSIZE)    MYSTR
 
      DOUBLE PRECISION      Y
 
      INTEGER               DECLEN
      INTEGER               DPAT
      INTEGER               EXP
      INTEGER               EXPSIZ
      INTEGER               FRSTCH
      INTEGER               I
      INTEGER               INTLEN
      INTEGER               LASTCH
      INTEGER               FIRSTB
      INTEGER               SGNLEN
      INTEGER               SIGDIG
      INTEGER               SPRSIZ
      INTEGER               START
 
 
      LOGICAL               NEEDSN
      LOGICAL               OVFLOW
      LOGICAL               SHIFT
 
 
 
 
C
C     Initial values
C 
 
C
C     Determine where the picture ends.
C
      FIRSTB  = POS( PICTUR, ' ',  1 )
 
      IF ( FIRSTB .EQ. 0 ) THEN
         LASTCH = LEN ( PICTUR )
      ELSE
         LASTCH = FIRSTB - 1
      END IF
 
C
C     Make sure there is a picture to worry about.
C
      IF ( LASTCH .EQ. 0 ) THEN
 
         CALL CHKIN  ( 'DPFMT'                                      )
         CALL SETMSG ( 'The format picture must begin with a '        //
     .                 'non-blank character.  The picture supplied '  //
     .                 'was began with a blank.'                      )
         CALL SIGERR ( 'SPICE(NOPICTURE)'                             )
         CALL CHKOUT ( 'DPFMT'                                        )
         RETURN
 
      ELSE IF ( LASTCH .EQ. 1 ) THEN
 
         IF (     PICTUR .EQ. '+'
     .       .OR. PICTUR .EQ. '-'
     .       .OR. PICTUR .EQ. '.' ) THEN
 
            CALL CHKIN ( 'DPFMT' )
            CALL SETMSG ( 'Format pictures must have at least '
     .      //            'one significant character. The '
     .      //            'picture provided ''#'' does not. ' )
            CALL ERRCH  ( '#', PICTUR(1:1)                    )
            CALL SIGERR ( 'SPICE(BADPICTURE)'                 )
            CALL CHKOUT ( 'DPFMT'                             )
            RETURN
 
         END IF
 
      ELSE IF ( LASTCH .EQ. 2 ) THEN
 
         IF (     PICTUR .EQ. '+.'
     .       .OR. PICTUR .EQ. '-.' ) THEN
 
            CALL CHKIN ( 'DPFMT' )
            CALL SETMSG ( 'Format pictures must have at least '
     .      //            'one significant character. The '
     .      //            'picture provided ''#'' does not. ' )
            CALL ERRCH  ( '#', PICTUR(1:2)                    )
            CALL SIGERR ( 'SPICE(BADPICTURE)'                 )
            CALL CHKOUT ( 'DPFMT'                             )
            RETURN
         END IF
 
      ELSE IF ( LASTCH .GT. LEN(STR) ) THEN
 
         CALL CHKIN  ( 'DPFMT'                                      )
         CALL SETMSG ( 'The output string is not long enough to '
     .   //            'accommodate a number formatted according '
     .   //            'the the supplied format picture.  The '
     .   //            'output string has length #.  The output '
     .   //            'picture ''#'' requires # characters. ' )
 
         CALL ERRINT ( '#', LEN(STR) )
         CALL ERRCH  ( '#', PICTUR(1:LASTCH) )
         CALL ERRINT ( '#', LASTCH )
         CALL SIGERR ( 'SPICE(OUTPUTTOOSHORT)' )
         CALL CHKOUT ( 'DPFMT'                                      )
         RETURN
 
      END IF
 
C
C     If we get this far, the routine can go ahead and do its business.
C     Determine the sign of X.  Also, determine how many characters
C     are needed to represent the sign if leading sign is suppressed for
C     positive numbers.
C
      IF ( X .GT. 0 ) THEN
         SIGN   = '+'
         SPRSIZ =  0
      ELSE IF ( X .LT. 0 ) THEN
         SIGN   = '-'
         SPRSIZ = 1
      ELSE
         SIGN   = ' '
         SPRSIZ =  0
      END IF
C
C     Look at the picture and see if a leading sign is required and
C     if so whether the sign just determined should use a different
C     character and how many characters are needed for the sign.
C
      IF      ( PICTUR(1:1) .EQ. '+' ) THEN
 
         NEEDSN = .TRUE.
         SGNLEN =  1
 
      ELSE IF ( PICTUR(1:1) .EQ. '-' ) THEN
 
         NEEDSN = .TRUE.
         SGNLEN = 1
 
         IF ( X .GT. 0 ) THEN
            SIGN = ' '
         END IF
 
      ELSE
 
         IF ( X .GT. 0 ) THEN
            SIGN = ' '
         END IF
 
         NEEDSN = .FALSE.
         SGNLEN =  SPRSIZ
 
      END IF
 
C
C     If we need a leading sign. The numeric part of the string
C     will start at character 2.  Otherwise it starts at character 1.
C
      IF ( NEEDSN ) THEN
         START    = 2
      ELSE
         START = 1
      END IF
 
C
C     We can set the sign portion of the string now.
C
      STR = SIGN
 
C
C     Determine what character should be use for leading characters
C     before the first significant character of the output string.
C
      IF ( PICTUR(START:START) .EQ. '0' ) THEN
         FILL   = '0'
      ELSE
         FILL  = ' '
      END IF
 
C
C     See if there is a decimal point.
C
      DPAT   = POS( PICTUR, '.',  1 )
C
C     The integer part is the stuff to the left of the first
C     decimal point and that follows the sign (if there is one
C     that is explicitly required.  The length of the decimal
C     portion is the stuff to the right of the decimal point.
C
      IF ( DPAT .GT. 0 ) THEN
 
         INTLEN = DPAT - START
         DECLEN = LASTCH - DPAT
 
      ELSE
 
         INTLEN =  LASTCH - START + 1
         DECLEN = -1
 
      END IF
C
C     If a sign was not explicitly requested by placing it in
C     the first digit of the picture START will be 1.  If in
C     addition X is less than zero ( SGNLEN will be 1 in this
C     case) we have one fewer digits available for the integer
C     portion of the string than is currently set in INTLEN.
C     Adjust INTLEN to reflect the actual number of digits
C     available.
C
C     Also set the SHIFT flag to .TRUE. so that we know to swap
C     the sign and any blanks that might lie between the sign
C     and the first significant character of the output string.
C
      IF ( START .EQ. 1 .AND. SGNLEN .EQ. 1 ) THEN
         INTLEN = INTLEN - 1
         SHIFT  = .TRUE.
C
C        If INTLEN has become negative (i.e. -1) the picture
C        must be of the form .xxxxx and the input number must
C        be negative. Add 1 back onto the INTLEN but take one
C        away from the decimal length DECLEN.
C
         IF ( INTLEN .EQ. -1 ) THEN
            INTLEN = 0
            DECLEN = DECLEN - 1
 
            IF (       DECLEN .EQ. 0
     .           .AND. INTLEN .EQ. 0 ) THEN
C
C              There is no room for anything other than a
C              decimal point.  We simply fill the output
C              string with the '*' character.
C
               DO I = 1, LASTCH
                  STR(I:I) = '*'
               END DO
               RETURN
 
            END IF
 
         END IF
 
 
 
      ELSE
         SHIFT  = .FALSE.
      END IF
 
C
C     Create the "virtual decimal string" associated with the
C     unsigned part of X.
C
      Y   = DABS(X)
 
      CALL ZZVSTSTR ( Y, FILL, EXP )
 
C
C     The actual number of digits required to print the unsigned integer
C     portion X is EXP + 1 (provided EXP is at least 0.) We have
C     INTLEN slots available.  So if EXP + 1 is more than INTLEN
C     ( which is equivalent to EXP being at least INTLEN) we don't
C     have enough room to print the unsigned integer portion of the
C     number.
C
 
 
      IF ( EXP .GE. INTLEN .AND. Y .NE. 0.0D0 ) THEN
C
C        See if we have room to print an exponential form.
C        First we need the number of characters for the
C        exponent which is always of the form 'E+dd...'
C
         EXPSIZ = 4 + MIN(1, EXP/1000 ) + MIN(1,EXP/100)
C
C        The number of significant digits that can be printed is the
C        size of the picture minus:   the size of the sign
C                                     the size of the exponent
C                                     the size of the decimal point.
C
         SIGDIG = LASTCH - SGNLEN - EXPSIZ - 1
C
C        If we don't have room for at least one significant digit,
C        there's not much we can do.  Fill the string with '*'.
C
 
         IF ( SIGDIG .LT. 1 ) THEN
            DO I = 1, LASTCH
               STR(I:I) = '*'
            END DO
         ELSE
 
            CALL DPSTR ( X, SIGDIG, MYSTR )
            MYSTR(1:1) = SIGN
            CALL LJUST ( MYSTR, STR )
            CALL RJUST ( STR(1:LASTCH), STR(1:LASTCH) )
 
         END IF
 
 
         RETURN
 
      END IF
C
C     One more check.  If -INTLEN is greater than DECLEN, or if
C     both are zero, we don't have room to create an output string.
C
      IF (  (       INTLEN .EQ. 0
     .        .AND. DECLEN .EQ. 0 )
     .        .OR. -INTLEN .GT. DECLEN ) THEN
 
         DO I = 1, LASTCH
            STR(I:I) = '*'
         END DO
 
         RETURN
      END IF
C
C     We have a reasonable chance of successfully constructing
C     the string without overflow.
C
      START = SGNLEN + 1
      CALL ZZVSBSTR ( -INTLEN, DECLEN, .TRUE., STR(START:), OVFLOW )
 
C
C     We might be done at this point.  The IF-THEN block below
C     handles the one snag that could arise.
C
C     If the first digit is a zero as a result of rounding it up
C     OVFLOW will be true.  This means we don't have enough room
C     in the picture for the integer portion of the string.  We try
C     to make an exponential picture.
C
      IF ( OVFLOW ) THEN
C
C        See if we have room to print an exponential form.
C
         EXPSIZ = 4 + MIN(1, EXP/1000 ) + MIN(1,EXP/100)
C
C        The number of significant digits that can be printed is the
C        size of the picture minus:   the size of the sign
C                                     the size of the exponent
C                                     the size of the decimal point.
C
         SIGDIG = LASTCH - SGNLEN - EXPSIZ - 1
 
 
         IF ( SIGDIG .LT. 1 ) THEN
            DO I = 1, LASTCH
               STR(I:I) = '*'
            END DO
         ELSE
 
            CALL DPSTR ( X, SIGDIG, MYSTR )
            MYSTR(1:1) = SIGN
            CALL LJUST ( MYSTR, STR )
            CALL RJUST ( STR(1:LASTCH), STR(1:LASTCH) )
            RETURN
 
         END IF
 
      ELSE IF ( SHIFT ) THEN
C
C        We need to move the sign right until, there are no
C        blanks between it and the next character.
C
         FRSTCH = NCPOS ( STR, ' -', 1 )
 
         IF ( FRSTCH .GT. 2 ) THEN
            STR(FRSTCH-1:FRSTCH-1) = STR(1:1)
            STR(1:1)               = ' '
         END IF
 
      END IF
 
 
 
      RETURN
      END
