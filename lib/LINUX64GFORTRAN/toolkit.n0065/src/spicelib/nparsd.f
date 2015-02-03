C$Procedure      NPARSD ( Double Precision parsing of a string )
 
      SUBROUTINE NPARSD ( STRING, X, ERROR, PTR )
 
C$ Abstract
C
C     Parse a character string that represents a number and return
C     a double precision value.
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
C      None.
C
C$ Keywords
C
C      ALPHANUMERIC
C      CONVERSION
C      PARSING
C
C$ Declarations
 
      CHARACTER*(*)    STRING
      DOUBLE PRECISION X
      CHARACTER*(*)    ERROR
      INTEGER          PTR
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  ---------------------------------------------------
C     STRING     I   Character string representing a numeric value.
C     X          O   Double precision value parsed from STRING.
C     ERROR      O   Message indicating whether errors have occurred.
C     PTR        O   Position in string where an error occurred.
C
C$ Detailed_Input
C
C     STRING     A character string that represents a numeric value.
C                Commas and spaces may be used in this string for
C                ease of reading and writing the number.  They
C                are treated as insignificant but non-error-producing
C                characters.
C
C                For exponential representation the characters
C                'E','D','e','d' may be used.
C
C                The following are legitimate numeric expressions
C
C                 +12.2 e-1
C                 -3. 1415 9276
C                 1e12
C                 E10
C
C                The program also recognizes the following  mnemonics
C                'PI', 'pi', 'Pi', 'pI'
C                '+PI', '+pi', '+Pi', '+pI'
C                '-PI', '-pi', '-Pi', '-pI'
C                and returns the value
C                ( + OR - ) 3.1415 9265 3589 7932 3846 2600 D0 as
C                appropriate.
C
C$ Detailed_Output
C
C     X          Double precision parsed value of input string. If an
C                error is encountered, X is not changed.
C
C     ERROR      is a message indicating that the string could
C                not be parsed due to use of an unexpected or misplaced
C                character or due to a string representing a number
C                too large for double precision.  If the number was
C                successfully parsed, ERROR will be returned as a blank.
C
C                In particular, blank strings, or strings that do not
C                contain either a digit or exponent character will
C                be regarded as errors.
C
C     PTR        This indicates which character was being used when
C                the error occurred.  If no error occurs, PTR is
C                returned as 0.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If the string is non-numeric, PTR indicates the location in
C        the string where the error occurred, and ERROR contains a
C        descriptive error message.
C
C$ Particulars
C
C     This routine parses an input character string that represents a
C     number, checks for overflow, unexpected or misplaced
C     characters.  It returns the double precision number or an error
C     message.
C
C$ Examples
C
C     Let   LINE = 'DELTA_T_A       =   32.184'
C
C     The following code fragment parses the line and obtains the
C     double precision value.
C
C
C        CALL NEXTWD ( LINE,  FIRST,  REST )
C        CALL NEXTWD ( REST, SECOND,  REST )
C        CALL NEXTWD ( REST,  THIRD,  REST )
C
C        CALL NPARSD (  THIRD,  VALUE, ERROR, PTR    )
C
C$ Restrictions
C
C     Due to rounding errors this routine may not be able to parse
C     the decimal character string representation of the largest
C     and smallest double precision numbers.
C
C$ Files
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     K.R. Gehringer  (JPL)
C     H.A. Neilan     (JPL)
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 3.5.0, 15-AUG-2002 (WLT)
C
C        Replaced the call to INSSUB with a call to ZZINSSUB so
C        that this routine can legitimately call itself Error Free
C
C-    SPICELIB Version 3.4.0, 3-DEC-2001
C
C        Added an extra check to make sure that ICHAR of any character
C        of the input string is positive.
C
C-    SPICELIB Version 3.3.0, 29-FEB-1996 (KRG)
C
C        The declaration for the SPICELIB function PI is now
C        preceded by an EXTERNAL statement declaring PI to be an 
C        external function. This removes a conflict with any
C        compilers that have a PI intrinsic function.
C
C        Removed the error message and storage for the unexpected
C        comma error message. This variable was set but never used,
C        and according to the spec for this routine a comma is a valid 
C        delimiter, treated like a space, within numbers.
C
C-    SPICELIB Version 3.2.0, 10-JAN-1995 (WLT)
C
C        Changed error strings from parameters to assignments to
C        compensate for shortcomings of the Absoft FORTRAN compiler
C        on the NeXT.
C
C-    SPICELIB Version 3.1.0, 12-JUL-1994 (WLT)
C
C        The previous version of the routine assumed that the range
C        of values of ICHAR was 0 to 128.  That turns out not to be
C        true on some machines.  If a character whose ICHAR value is
C        outside this range is detected, it is now handled properly
C        as an unexpected character.
C
C-    SPICELIB Version 3.0.0, 24-FEB-1993 (WLT)
C
C        The previous version of the algorithm interpreted P or p as 1.
C        This was not the intent of the routine and was corrected.
C
C-    SPICELIB Version 2.0.0, 28-AUG-1992 (WLT) (KRG)
C
C        The basic algorithm was completely re-written.  As a result
C        the routine now runs an order of magnitude faster than
C        it did before.  In addition, strings that do not contain
C        enough information to assign a value to the string are now
C        regarded as errors.  These include blank strings or strings
C        that contain only a sign characters, blanks and commas.
C
C        In addition the error diagnosis and checking for overflow
C        was greatly enhanced.
C
C        Note: strings may now parse with slightly different values
C        from the previous version of NPARSD.  The current
C        implementation is more accurate in converting strings to
C        double precision numbers.
C
C-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.1.0, 17-APR-1990 (WLT)
C
C        Bug fix.  The subscript used to reference individual characters
C        of the input string could sometimes step out of bounds.  This
C        went unnoticed until NAIF began compiling with the CHECK=BOUNDS
C        option of the DEC Fortran compiler.
C
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
C
C-&
 
C$ Index_Entries
C
C     parse a character_string to a d.p. number
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 3.3.0, 29-FEB-1996 (KRG)
C
C        The declaration for the SPICELIB function PI is now
C        preceded by an EXTERNAL statement declaring PI to be an 
C        external function. This removes a conflict with any
C        compilers that have a PI intrinsic function.
C
C        Removed the error message and storage for the unexpected
C        comma error message. This variable was set but never used,
C        and according to the spec for this routine a comma is a valid 
C        delimiter, treated like a space, within numbers.
C
C-    SPICELIB Version 3.2.0, 10-JAN-1995 (WLT)
C
C        Changed error strings from parameters to assignments to
C        compensate for shortcomings of the Absoft FORTRAN compiler
C        on the NeXT.
C
C-    SPICELIB Version 3.1.0, 12-JUL-1994 (WLT)
C
C        The previous version of the routine assumed that the range
C        of values of ICHAR was 0 to 128.  That turns out not to be
C        true on some machines.  If a character whose ICHAR value is
C        outside this range is detected, it is now handled properly
C        as an unexpected character.
C
C-    SPICELIB Version 3.0.0, 24-FEB-1993 (WLT)
C
C        The previous version of the algorithm interpreted P or p as 1.
C        This was not the intent of the routine and was corrected.
C
C-    SPICELIB Version 2.0.0, 28-AUG-1992 (WLT) (KRG)
C
C        The basic algorithm was completely re-written.  As a result
C        the routine now runs an order of magnitude faster than
C        it did before.  In addition, strings that do not contain
C        enough information to assign a value to the string are now
C        regarded as errors.  These include blank strings or strings
C        that contain only a sign characters, blanks and commas.
C
C        In addition the error diagnosis and checking for overflow
C        was greatly enhanced.
C
C        In general the current algorithm is more robust and much
C        faster than the previous version.
C
C        Note: strings may now parse with slightly different values
C        from the previous version of NPARSD.  The current
C        implementation is more accurate in converting strings to
C        double precision numbers.
C
C-    SPICELIB Version 1.1.0, 17-APR-1990 (WLT)
C
C        Bug fix.  The subscript used to reference individual characters
C        of the input string could sometimes step out of bounds.  This
C        went unnoticed until NAIF began compiling with the CHECK=BOUNDS
C        option of the DEC Fortran compiler.
C
C-    Beta Version 1.1.0, 16-FEB-1989 (HAN) (NJB)
C
C        Contents of the Exceptions section was changed to "error free"
C        to reflect the decision that the module will never participate
C        in error handling.
C
C        An example was added to the header, and the Exceptions section
C        was completed.
C
C        Declaration of unused variables J, K and unused function
C        LASTNB removed.
C
C-&
 
 
C
C     SPICELIB functions.
C
 
      DOUBLE PRECISION      DPMAX
      EXTERNAL              PI
      DOUBLE PRECISION      PI
 
C
C     Local Parameters.
C
 
      DOUBLE PRECISION      BASE
      PARAMETER           ( BASE   = 10.0D0 )
 
      INTEGER               DIGIT
      PARAMETER           ( DIGIT  = 1   )
 
      INTEGER               DPOINT
      PARAMETER           ( DPOINT = 2   )
 
      INTEGER               EXPONT
      PARAMETER           ( EXPONT = 3   )
 
      INTEGER               IGNORE
      PARAMETER           ( IGNORE = 4   )
 
      INTEGER               BPISTR
      PARAMETER           ( BPISTR = 5   )
 
      INTEGER               EPISTR
      PARAMETER           ( EPISTR = 6   )
 
      INTEGER               SIGN
      PARAMETER           ( SIGN   = 7   )
 
      INTEGER               IBASE
      PARAMETER           ( IBASE  = 10  )
 
      INTEGER               NKNOWN
      PARAMETER           ( NKNOWN = 128 )
 
      INTEGER               TEST
      PARAMETER           ( TEST = 16 )
 
      INTEGER               ERRSIZ
      PARAMETER           ( ERRSIZ = 160 )
 
      CHARACTER*(ERRSIZ)    BLNKST
      CHARACTER*(ERRSIZ)    TOOBIG
      CHARACTER*(ERRSIZ)    UNRCST
      CHARACTER*(ERRSIZ)    UNXPCH
      CHARACTER*(ERRSIZ)    UNXPPT
      CHARACTER*(ERRSIZ)    UNXPSN
 
      DOUBLE PRECISION      DECVAL
      DOUBLE PRECISION      DIVISR
      DOUBLE PRECISION      DPSIGN (     2     )
      DOUBLE PRECISION      ECOUNT
      DOUBLE PRECISION      EXPVAL
      DOUBLE PRECISION      FACTOR
      DOUBLE PRECISION      INTBND
      DOUBLE PRECISION      INTVAL
      DOUBLE PRECISION      LOOKUP ( 0 : IBASE )
      DOUBLE PRECISION      MAXEXP
      DOUBLE PRECISION      MINEXP
      DOUBLE PRECISION      NEXT
      DOUBLE PRECISION      SMLBND
      DOUBLE PRECISION      VALUE
      DOUBLE PRECISION      VALUES (     NKNOWN )
 
      INTEGER               CLASS  ( 0 : NKNOWN )
      INTEGER               EXP
      INTEGER               I
      INTEGER               ID
      INTEGER               SIGNDX
 
      INTEGER               B
      INTEGER               BLANK
      INTEGER               L
      INTEGER               M
      INTEGER               NL
      INTEGER               NEXTI
      INTEGER               THISI
 
 
      LOGICAL               DODEC
      LOGICAL               EXPOK
      LOGICAL               DOEXP
      LOGICAL               FIRST
      LOGICAL               DOINT
      LOGICAL               MANTSA
      LOGICAL               BPIOK
      LOGICAL               EPIOK
      LOGICAL               PNTOK
      LOGICAL               ROUNDI
      LOGICAL               ROUNDD
      LOGICAL               SIGCHR
      LOGICAL               SIGNOK
      LOGICAL               ZEROI
C
C     Save everything.  It's easier than tracking down every
C     little variable that might need to be saved.
C
      SAVE
 
      DATA                  LOOKUP  /               1.0D0,
     .                                             10.0D0,
     .                                            100.0D0,
     .                                           1000.0D0,
     .                                          10000.0D0,
     .                                         100000.0D0,
     .                                        1000000.0D0,
     .                                       10000000.0D0,
     .                                      100000000.0D0,
     .                                     1000000000.0D0,
     .                                    10000000000.0D0  /
 
      DATA                  FIRST  / .TRUE. /
 
      DATA                  VALUES  / NKNOWN * 0.0D0 /
 
      DATA                  CLASS   / 0, NKNOWN * 0 /
 
 
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
C
C        Set up the error messages
C
         TOOBIG = 'The number represented by the input string is '
     .   //       'too large to be stored as a double precision '
     .   //       'number. '
 
 
         UNXPCH = 'An unexpected character was found while '
     .   //       'attempting to parse the input string. '
 
 
         UNXPPT = 'An unexpected decimal point was found in the '
     .   //       'input string. '
 
 
         UNXPSN = 'An unexpected sign character was found in the '
     .   //       'input string. '
 
 
         BLNKST = 'The input string is blank. Blank strings are '
     .   //       'not considered to be numbers. '
 
 
         UNRCST = 'The input string could not be recognized as a '
     .   //       'number. '
 
         BLANK = ICHAR ( ' ' )
 
         VALUES( ICHAR ( '0' ) ) =  0.0D0
         VALUES( ICHAR ( '1' ) ) =  1.0D0
         VALUES( ICHAR ( '2' ) ) =  2.0D0
         VALUES( ICHAR ( '3' ) ) =  3.0D0
         VALUES( ICHAR ( '4' ) ) =  4.0D0
         VALUES( ICHAR ( '5' ) ) =  5.0D0
         VALUES( ICHAR ( '6' ) ) =  6.0D0
         VALUES( ICHAR ( '7' ) ) =  7.0D0
         VALUES( ICHAR ( '8' ) ) =  8.0D0
         VALUES( ICHAR ( '9' ) ) =  9.0D0
         VALUES( ICHAR ( '-' ) ) = -1.0D0
         VALUES( ICHAR ( '+' ) ) =  1.0D0
 
         CLASS( ICHAR(' ') ) = IGNORE
         CLASS( ICHAR(',') ) = IGNORE
 
         CLASS( ICHAR('.') ) = DPOINT
 
         CLASS( ICHAR('E') ) = EXPONT
         CLASS( ICHAR('D') ) = EXPONT
         CLASS( ICHAR('e') ) = EXPONT
         CLASS( ICHAR('d') ) = EXPONT
 
         CLASS( ICHAR('+') ) = SIGN
         CLASS( ICHAR('-') ) = SIGN
 
         CLASS( ICHAR('1') ) = DIGIT
         CLASS( ICHAR('2') ) = DIGIT
         CLASS( ICHAR('3') ) = DIGIT
         CLASS( ICHAR('4') ) = DIGIT
         CLASS( ICHAR('5') ) = DIGIT
         CLASS( ICHAR('6') ) = DIGIT
         CLASS( ICHAR('7') ) = DIGIT
         CLASS( ICHAR('8') ) = DIGIT
         CLASS( ICHAR('9') ) = DIGIT
         CLASS( ICHAR('0') ) = DIGIT
 
         CLASS( ICHAR('p') ) = BPISTR
         CLASS( ICHAR('P') ) = BPISTR
         CLASS( ICHAR('i') ) = EPISTR
         CLASS( ICHAR('I') ) = EPISTR
C
C        Finally create the numbers that will be used for checking
C        for floating point overflow.
C
C        NOTE: The value for MINEXP may be too small by one, but it
C              really doesn't make any difference, as you're going to
C              underflow anyway, and dividing zero by a number (BASE)
C              still gives you zero.
C
         MAXEXP = DINT ( DLOG10( DPMAX() ) )
         MINEXP =-( MAXEXP + 1 )
         SMLBND = DPMAX()/LOOKUP(IBASE)
 
         INTBND = BASE
         NEXT   = INTBND + 1.0D0
 
         DO WHILE ( INTBND .NE. NEXT )
 
            INTBND = INTBND *  BASE
            NEXT   = INTBND +  1.0D0
 
         END DO
 
         INTBND = INTBND / BASE
C
C        That takes care of the first pass initializations.
C
      END IF
C
C     Here's what's true right now.
C
C     There are no errors.
C     The error pointer doesn't need to point anywhere.
C     It's ok for the next token to be a decimal point.
C     It's ok for the next token to be a sign character.
C     It's ok for the next token to be an exponent marker.
C     It's ok for the next character to be the start of pi.
C
C     We expect to be constructing the integer part of the
C     numeric string.
C
      ERROR     = ' '
      PTR       = 0
 
      PNTOK     = .TRUE.
      SIGNOK    = .TRUE.
      EXPOK     = .TRUE.
      BPIOK     = .TRUE.
      DOINT     = .TRUE.
      ROUNDD    = .TRUE.
      ROUNDI    = .TRUE.
C
C     Here's some other facts.
C
C     We are not parsing the decimal part of the string.
C     We are not parsing the exponent part of the string.
C     We have not encountered any digits in the mantissa.
C     We have not encountered any significant characters.
C     It's not ok for the next character to be the end of pi (i).
C
      DODEC     = .FALSE.
      DOEXP     = .FALSE.
      MANTSA    = .FALSE.
      SIGCHR    = .FALSE.
      EPIOK     = .FALSE.
C
C     So far there is no integer, decimal or exponent part to this
C     string.
C
      INTVAL    = 0.0D0
      DECVAL    = 0.0D0
      EXPVAL    = 0.0D0
      DIVISR    = 1.0D0
      FACTOR    = 1.0D0
      ECOUNT    = 0.0D0
C
C     Right now if we encounter a sign, it's part of the mantissa.
C     And until we know better the sign of both the mantissa and
C     exponent are +1 (as opposed to -1).
C
      SIGNDX    = 1
      DPSIGN(1) = 1.0D0
      DPSIGN(2) = 1.0D0
C
C     Before doing anything else we determine whether or not
C     the input string is empty.
C
      IF ( STRING .EQ. ' ' ) THEN
 
         ERROR = BLNKST
         PTR   = 1
         RETURN
 
      END IF
 
C
C     We need to find the last non-blank character of the input
C     string.  We shall use the idea of binary searching to locate
C     this character.  At first this may appear to be a bit convoluted
C     when compared to the obvious thing to do (start at the end of
C     the string and step backward until a non-blank character is
C     located).  However, on every machine we've looked at this method
C     locates the last non-blank character much more quickly on average
C     than the obvious method.
C
C     L and B denote the last and beginning characters
C     of the substring we are searching.  NL is the next to last
C     character that we are concerned with and M is the middle of
C     the current search interval ( from B to NL ).
C
      L     =  LEN(STRING)
      B     =  1
      NL    =  L - 1
 
C
C     We want M to be ( B + NL ) / 2   but right now that's L/2
C
      M     =  L / 2
 
      DO WHILE ( L-B .GT. TEST )
C
C        What is true right now?  The string from L+1 on out
C        is blank.  L > B; L-1 = NL >= B;  M = (B + NL) / 2;
C        and M >= B,  B is at least one and if greater than 1
C        there must be a non-blank character between B and the
C        end of the string.
C
         IF ( ICHAR(STRING(L:L)) .NE. BLANK ) THEN
 
            B = L
 
         ELSE IF ( STRING(M:NL) .EQ. ' ' ) THEN
 
C
C           If you got here, the STRING(L:L) is a blank.
C           The string from L+1 on out is blank.
C           The string from M to NL (=L-1) is blank.  Thus the
C           string from M out is blank.
C
C           M is greater than or equal to B.
C           If M  is less than B + 2, then L will become
C           B or less and there will not be a
C           next pass through the loop.  That means that
C           we will never get to this point again and don't
C           have to worry about the reference STRING(M:NL)
C           giving us an access violation.
C
            L  = M - 1
C
C           With the new value of L, we now know that STRING(L+1:)
C           is blank.
C
         ELSE
C
C           If you get to this point all of the string from
C           L out is blank and L is greater than M.
C           There is a non-blank character between M and NL.
C           If L should get within 16 of B, then the loop
C           will not be executed again.  That means again that
C           we don't have to worry about STRING(M:NL) being
C           an ill formed string.
C
            L  =  NL
            B  =  M
C
C           With the new value of L, we now know that STRING(L+1:)
C           is blank.
C
         END IF
 
 
C
C        Finally compute NL,the index of the character that precedes
C        L and the new midpoint of the stuff from B to NL.
C
         NL =  L - 1
         M  = (B + NL) / 2
 
C
C        What's true now?  The string from L+1 on out is blank.
C
      END DO
 
C
C     L is now within 16 characters of the last non-blank character
C     of the input string.  We simply search backward from L to
C     locate this last non-blank.
C
      DO WHILE ( ICHAR(STRING(L:L)) .EQ. BLANK )
         L = L - 1
      END DO
 
 
C
C     Begin to collect the number in its various parts: an integer
C     portion, a fractional portion, and an exponent.
C
      DO I = 1, L
 
 
 
         ID    = ICHAR(STRING(I:I))
 
         IF ( ID .GT. NKNOWN .OR. ID .LT. 0 ) THEN
 
C
C           This is definitely not expected.  Set the error message
C           and return.
C
            NEXTI = I + 1
            THISI = I
 
            CALL ZZINSSUB ( STRING, ']', NEXTI, ERROR )
            CALL ZZINSSUB ( ERROR,  '[', THISI, ERROR )
            CALL PREFIX ( UNXPCH,  1,         ERROR )
 
            PTR   = I
            RETURN
 
C
C        The action taken depends upon the class of the token.
C
         ELSE IF ( CLASS(ID) .EQ. DIGIT ) THEN
C
C           Once a digit has been encountered, we can no longer
C           allow the string 'PI' or a sign until an exponent
C           character is hit and resets the SIGNOK flag.
C
            BPIOK  = .FALSE.
            EPIOK  = .FALSE.
            SIGNOK = .FALSE.
            SIGCHR = .TRUE.
C
C           If we are constructing the integer part ...
C
            IF ( DOINT  ) THEN
 
               MANTSA = .TRUE.
C
C              Check the current value of the integer part to
C              make sure we don't overflow.
C
               IF ( INTVAL .LT. INTBND ) THEN
 
                  INTVAL = INTVAL * BASE + VALUES(ID)
 
               ELSE
C
C                 Once the integer exceeds a given bound,
C                 we add the rest on as fractional part and
C                 keep track of the factor we will need to
C                 multiply the decimal part by to scale things
C                 appropriately.  We also keep track of the number
C                 we will need to add to the exponent part.
C
                  ECOUNT = ECOUNT + 1
                  FACTOR = FACTOR / BASE
 
                  IF ( ROUNDI ) THEN
 
                     ROUNDI = .FALSE.
 
                     IF ( VALUES(ID) .GT. 0.5D0*BASE ) THEN
                        INTVAL = INTVAL + 1.0D0
                     END IF
 
                  END IF
 
               END IF
C
C           ... or the decimal part ...
C
            ELSE IF ( DODEC ) THEN
 
               MANTSA = .TRUE.
C
C              There are two cases to consider.  The case in which
C              the integer portion of the string has value 0...
C
               IF ( ZEROI ) THEN
C
C                 We can just keep accumulating the decimal part
C                 as an integer.  But we keep track of how many
C                 places past the decimal point the first non-zero
C                 digit occurs.  Note that once the decimal part
C                 exceeds the integer bound, we don't need to do
C                 anything.  The remaining digits cannot contribute
C                 to the value of the decimal part.
C
                  IF  ( DECVAL .LT. INTBND ) THEN
 
                     DECVAL = DECVAL * BASE + VALUES(ID)
                     ECOUNT = ECOUNT - 1
 
                  ELSE IF ( ROUNDD ) THEN
 
                     ROUNDD = .FALSE.
 
                     IF ( VALUES(ID) .GE. 0.5D0*BASE ) THEN
                        DECVAL =  DECVAL + 1.0D0
                     END IF
 
                  END IF
C
C              ...and the case in which the integer portion is not
C              zero.
C
               ELSE
C
C                 In this case, we know there is at least _something_
C                 to the integer part of this string.  We can
C                 stop accumulating the decimal part when the divisor
C                 portion exceeds the integer barrier.  After that
C                 the extra digits can't make any contribution to
C                 the double precision value given to the string.
C
                  IF ( DIVISR .LT. INTBND ) THEN
 
                     DECVAL = DECVAL * BASE + VALUES(ID)
                     DIVISR = DIVISR * BASE
 
                  END IF
 
               END IF
C
C           ...or the exponent part of the string.
C
            ELSE IF ( DOEXP  ) THEN
 
               IF ( EXPVAL + ECOUNT .GT. MAXEXP ) THEN
C
C                 This number is too big to put into a double
C                 precision number. The marginal case where
C                 EXPVAL + ECOUNT .EQ. MAXEXP will be dealt
C                 with when the integer and fractional parts
C                 of the double precision number are built
C                 at the end of this routine.
C
                  ERROR = TOOBIG
                  PTR   = I
                  RETURN
 
               ELSE IF ( EXPVAL + ECOUNT .LT. MINEXP ) THEN
C
C                 This number is going to underflow, we can
C                 just stop accumulating exponent. But we don't
C                 stop parsing the string yet. There might be
C                 a bad character lurking somewhere later in the
C                 string.
C
C                 NOTE: It is also possible to underflow when the
C                       value of EXPVAL + ECOUNT is equal to MINEXP,
C                       since an entire 'BASE' scale is not supported
C                       for this particular exponent.
C
               ELSE
C
C                 This is the case we expect.  Just add on the
C                 next part of the exponent.
C
                  EXPVAL = EXPVAL*BASE + DPSIGN(2)*VALUES(ID)
 
               END IF
C
C           Even though this character is a digit, its not expected
C           for some reason.  Set the error flag and return.
C
            ELSE
 
               NEXTI = I + 1
               THISI = I
 
               CALL ZZINSSUB ( STRING, ']', NEXTI, ERROR )
               CALL ZZINSSUB ( ERROR,  '[', THISI, ERROR )
               CALL PREFIX ( UNXPCH,  1,         ERROR )
 
               PTR   = I
               RETURN
 
            END IF
 
         ELSE IF ( CLASS(ID) .EQ. DPOINT ) THEN
 
            IF ( PNTOK ) THEN
 
               BPIOK  = .FALSE.
               EPIOK  = .FALSE.
               PNTOK  = .FALSE.
               SIGNOK = .FALSE.
               DODEC  = .TRUE.
               DOINT  = .FALSE.
               DOEXP  = .FALSE.
               ZEROI  = INTVAL .EQ. 0.0D0
 
            ELSE
 
               NEXTI = I + 1
               THISI = I
 
               CALL ZZINSSUB ( STRING, ']', NEXTI, ERROR )
               CALL ZZINSSUB ( ERROR,  '[', THISI, ERROR )
               CALL PREFIX ( UNXPPT,  1,         ERROR )
 
               PTR    = I
               RETURN
 
            END IF
 
         ELSE IF ( CLASS(ID) .EQ. EXPONT ) THEN
 
            SIGCHR = .TRUE.
 
            IF ( EXPOK ) THEN
 
               BPIOK  = .FALSE.
               EPIOK  = .FALSE.
               EXPOK  = .FALSE.
               PNTOK  = .FALSE.
               DODEC  = .FALSE.
               DOINT  = .FALSE.
               DOEXP  = .TRUE.
               SIGNOK = .TRUE.
               SIGNDX =  2
 
            ELSE
 
               NEXTI = I + 1
               THISI = I
 
               CALL ZZINSSUB ( STRING, ']', NEXTI, ERROR )
               CALL ZZINSSUB ( ERROR,  '[', THISI, ERROR )
               CALL PREFIX ( UNXPCH,  1,         ERROR )
 
               PTR   =  I
               RETURN
 
            END IF
 
         ELSE IF ( CLASS(ID) .EQ. SIGN ) THEN
 
            IF ( SIGNOK ) THEN
 
               DPSIGN(SIGNDX) = VALUES(ID)
               SIGNOK         = .FALSE.
 
            ELSE
 
               NEXTI = I + 1
               THISI = I
 
               CALL ZZINSSUB ( STRING, ']', NEXTI, ERROR )
               CALL ZZINSSUB ( ERROR,  '[', THISI, ERROR )
               CALL PREFIX ( UNXPSN,  1,         ERROR )
 
               PTR   = I
               RETURN
 
            END IF
 
         ELSE IF ( CLASS(ID) .EQ. BPISTR ) THEN
 
            SIGCHR = .TRUE.
 
            IF ( BPIOK ) THEN
 
               DOINT  = .FALSE.
               DODEC  = .FALSE.
               DOEXP  = .FALSE.
               EXPOK  = .FALSE.
               PNTOK  = .FALSE.
               BPIOK  = .FALSE.
               SIGNOK = .FALSE.
               EPIOK  = .TRUE.
 
            ELSE
 
               NEXTI = I + 1
               THISI = I
 
               CALL ZZINSSUB ( STRING, ']', NEXTI, ERROR )
               CALL ZZINSSUB ( ERROR,  '[', THISI, ERROR )
               CALL PREFIX ( UNXPCH,  1,         ERROR )
 
               PTR   = I
               RETURN
 
            END IF
 
         ELSE IF ( CLASS(ID) .EQ. EPISTR ) THEN
 
            IF ( EPIOK ) THEN
 
               DOINT  = .FALSE.
               DODEC  = .FALSE.
               DOEXP  = .FALSE.
               EXPOK  = .FALSE.
               PNTOK  = .FALSE.
               BPIOK  = .FALSE.
               SIGNOK = .FALSE.
               EPIOK  = .FALSE.
               MANTSA = .TRUE.
 
               INTVAL = PI()
 
            ELSE
 
               NEXTI = I + 1
               THISI = I
 
               CALL ZZINSSUB ( STRING, ']', NEXTI, ERROR )
               CALL ZZINSSUB ( ERROR,  '[', THISI, ERROR )
               CALL PREFIX ( UNXPCH,  1,         ERROR )
 
               PTR   = I
               RETURN
 
            END IF
 
         ELSE IF ( CLASS(ID) .EQ. IGNORE ) THEN
 
C
C           We don't do anything.
C
 
         ELSE
 
C
C           This is definitely not expected.  Set the error message
C           and return.
C
            NEXTI = I + 1
            THISI = I
 
            CALL ZZINSSUB ( STRING, ']', NEXTI, ERROR )
            CALL ZZINSSUB ( ERROR,  '[', THISI, ERROR )
            CALL PREFIX ( UNXPCH,  1,         ERROR )
 
            PTR   = I
            RETURN
 
         END IF
 
      END DO
 
 
C
C     If we got through the loop and it's OK to end PI, then we started
C     it but never finished.  This is an error.
C
      IF ( EPIOK ) THEN
         ERROR = UNRCST
         PTR   = L
         RETURN
      END IF
 
C
C     Put together the portion that does not involve an exponent.
C
C     If
C        (1) MANTSA = .TRUE., then we had some explicit part of a
C            number, an  integer part, a fractional part, or both.
C
C        (2) SIGCHR = .TRUE, then we had either:
C
C            (a) MANTSA = .TRUE.
C
C         or
C
C            (b) there was an implicit value associated with the input
C                string. For example, an exponent character followed
C                by an optional exponent would produce a valid number:
C                E+10 --> 1.0d+10. This is due to the fact that this
C                routine emulates an RPN calculator of popular repute,
C                not because it is inherently a good idea.
C
      IF ( MANTSA ) THEN
C
C        We had an integer part of the number, a fractional part, or
C        both, so we need to put them together in an appropriate
C        fashion.
C
         VALUE = INTVAL + ( DECVAL / DIVISR ) * FACTOR
 
      ELSE IF ( SIGCHR ) THEN
C
C        We do not have a mantissa, so we had an  implicit mantissa,
C        see above, so we need to set the value to one.
C
         VALUE = 1.0D0
 
      ELSE
C
C        We have an error. There were no significant characters in the
C        input character string, and hence we could not parse it into
C        a number. An example of such a string would be: '+  ,,.,,'.
C        So, we will set an appropriate error message and return.
C
         ERROR = UNRCST
         PTR   = LEN(STRING) + 1
         RETURN
 
      END IF
C
C     Adjust the entered part of the exponent by the amount
C     we "shifted" the decimal point when we were computing
C     the integer and decimal values.
C
      EXPVAL = EXPVAL + ECOUNT
C
C     Now take care of the exponent contribution to the answer.
C
C     If the exponent is negative ...
C
      IF ( EXPVAL .LT. 0 ) THEN
 
         DO WHILE ( EXPVAL .LT. -BASE )
 
            VALUE  = VALUE  / LOOKUP(IBASE)
            EXPVAL = EXPVAL + BASE
 
         END DO
 
         VALUE  = VALUE / LOOKUP( -INT(EXPVAL) )
C
C     If the exponent is positive ...
C
      ELSE IF ( EXPVAL .GT. 0 ) THEN
 
         DO WHILE ( EXPVAL .GT. BASE )
C
C           Make sure that a multiply isn't going to create
C           a number that overflows.
C
            IF ( VALUE .GE. SMLBND ) THEN
 
               ERROR = TOOBIG
               PTR   = LEN(STRING)+1
               RETURN
 
            ELSE
 
               VALUE  = VALUE  * LOOKUP(IBASE)
               EXPVAL = EXPVAL - BASE
 
            END IF
 
         END DO
 
 
 
         EXP = NINT(EXPVAL)
C
C        Again, make sure that a floating point overflow isn't
C        going to happen.
C
         IF ( VALUE .LT. DPMAX()/LOOKUP(EXP) ) THEN
 
            VALUE = VALUE * LOOKUP(EXP)
 
         ELSE
 
               ERROR = TOOBIG
               PTR   = LEN(STRING)+1
               RETURN
 
         END IF
 
      END IF
 
      X = DPSIGN(1)*VALUE
 
      RETURN
      END
 
