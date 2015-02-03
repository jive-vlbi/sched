C$Procedure       ZZVSTRNG ( Virtual String )
 
      SUBROUTINE ZZVSTRNG( X, FILL, FROM, TO, RND, EXPONT, SUBSTR, DID )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Maintain a virtual decimal string associated with a d.p. number X.
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
C      ALPHANUMERIC, PRIVATE
C
C$ Declarations
 
      IMPLICIT NONE
      DOUBLE PRECISION      X
      CHARACTER*(1)         FILL
      INTEGER               FROM
      INTEGER               TO
      LOGICAL               RND
      INTEGER               EXPONT
      CHARACTER*(*)         SUBSTR
      LOGICAL               DID
 
C$ Brief_I/O
C
C     VARIABLE  I/O  Entry
C     --------  ---  --------------------------------------------------
C     X          I   ZZVSTSTR
C     FILL       I   ZZVSTSTR
C     FROM       I   ZZVSBSTR
C     TO         I   ZZVSBSTR
C     EXPONT     O   ZZVSTSTR
C     SUBSTR     O   ZZVSBSTR
C
C$ Detailed_Input
C
C     X          is a double precision number for which we want to
C                create a virtual decimal string.  This is supplied
C                to the routine ZZVSTSTR which sets up the internal
C                representation of the virtual decimal string.
C
C                X is assumed to be positive.
C
C     FILL       is the character to use for digits that precede the
C                first significant digit in the virtual decimal string.
C                Usually this will be a blank or zero ('0')
C
C     FROM       is the index in the virtual decimal string of the
C                first character that will be returned by ZZVSBSTR.
C
C     TO         is the index in the virtual decimal string of the
C                last character that will be returned by ZZVSBSTR.
C
C     RND        is a logical flag used to indicate that the output
C                string should represent the virtual decimal string
C                that results from rounding to the TO'th decimal
C                location.
C
C$ Detailed_Output
C
C     EXPONT     is the exponent associated with X when represented
C                in scientific notation.  It is returned by ZZVSTSTR.
C
C     SUBSTR     is the substring of the virtual decimal string from
C                index FROM to TO returned by ZZVSBSTR
C
C     DID        is a logical flag that is used to indicate that
C                the left most character returned by ZZVSBSTR became
C                a zero as a result of rounding up from 9. (i.e. there
C                are significant digits to the left of the first
C                character returned in SUBSTR.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     Given a character representation of a number such as
C     '1.234567890123E+3' there is a corresponding "infinite"
C     representation.  In this case
C
C           ...0000001234.56789012300000....
C
C     If we let the "index" of the decimal point be zero and number
C     the other characters from left to right in sequence we can
C     speak of the J'th character in the infinite representation.
C
C     We call the combination of the infinite representation and
C     indexing scheme  the virtual decimal string associated with the
C     input string.
C
C     The internal representation of the virtual decimal string is
C     set using the entry point ZZVSTSTR.  This entry point returns
C     the exponent associated with the string when it is written
C     in scientific notation.
C
C     For any J the entry point ZZVSBSTR returns the J'th character
C     of the virtual decimal string.
C
C     You may request that ZZVSBSTR return a string that is rounded
C     to the right most digit returned.  If return to the example
C     above
C
C           ...0000001234.56789012300000....
C
C     and the substring from -5 to 3 is requested with rounding,
C     the virtual decimal string will be treated as virtual string
C     rounded to the 3rd decimal point.
C
C           ...0000001234.56800000000000....
C
C     As a special convenience, you may specify any character to
C     be used in place of the extra leading zeros in the representation.
C     This leading character is specified via the input FILL in
C     ZZVSTSTR.
C
C$ Examples
C
C     Suppose you would like to create an output string associated
C     with X and you would like to present it in decimal format.
C
C     Moreover, suppose you know that X is positive and less than
C     100000.  The following would create the string and set the
C     leading character to be a blank.
C
C        CALL ZZVSTSTR ( X, ' ', EXP )
C
C     Check the exponent returned.  If it's greater than 5, our basic
C     assumptions were violated.
C
C        IF ( EXP .GT. 5 ) THEN
C           WRITE (*,*) 'The exponent is too big. It is: ', EXP
C        END IF
C
C     Now fill in the string.
C
C        CALL ZZVSBSTR ( -6, 5, RND, SUBSTR, DID )
C
C        WRITE (*,*) 'The value of X was: ', SUBSTR
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 12-SEP-1996 (WLT)
C
C-&
 
 
C
C     Local Variables
C
 
 
      INTEGER               MAXSIG
      PARAMETER           ( MAXSIG = 14 )
 
      INTEGER               ESGN
      PARAMETER           ( ESGN   = MAXSIG + 4 )
 
      INTEGER               EFST
      PARAMETER           ( EFST   = ESGN + 1 )
 
      INTEGER               ESCD
      PARAMETER           ( ESCD   = EFST + 1 )
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = MAXSIG + 16 )
 
      LOGICAL               YES
      PARAMETER           ( YES = .TRUE. )
 
      LOGICAL               NO
      PARAMETER           ( NO  = .FALSE. )
 
 
 
      CHARACTER*(WDSIZE)    STRING
      CHARACTER*(1)         MYFILL
      CHARACTER*(1)         LETTER
 
      INTEGER               BLANK
      INTEGER               CODE
      INTEGER               CODE0
      INTEGER               EXP
      INTEGER               I
      INTEGER               J
      INTEGER               LSUB
      INTEGER               SLOT
      INTEGER               VALUE
 
      LOGICAL               MINUS
      LOGICAL               INCR
 
      SAVE
C
C     Although we don't anticipate ever needing these values
C     we set some initial values for EXP and STRING.
C
      DATA                  STRING / ' 0.0000000000000E+00 '/
      DATA                  EXP    /   0   /
      DATA                  MYFILL / ' '   /
C
C     This routine doesn't do anything.
C
      RETURN
 
 
C$Procedure      ZZVSTSTR ( Set Virtual String)
 
      ENTRY ZZVSTSTR ( X, FILL, EXPONT )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Set up the virtual string associated with X and return the
C     exponent associated with X when represented in scientific
C     notation.
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
C     ALPHANUMERIC, PRIVATE
C
C$ Declarations
C
C     IMPLICIT NONE
C     DOUBLE PRECISION      X
C     CHARACTER*(1)         FILL
C     INTEGER               EXPONT
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     X          I   double precision number to needing a virtual string
C     FILL       I   leading character for virtual string.
C     EXPONT     O   The exponent associated with X.
C
C     The function returns the exponent associated with X.
C
C$ Detailed_Input
C
C     X          is a double precision number that from which
C                a virtual decimal string should be created.
C
C     FILL       is the character to use for the leading character
C                in the virtual decimal string.
C
C$ Detailed_Output
C
C     EXPONT     is the value of the scientific notation
C                exponent associated with X.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This entry point is used to establish a virtual decimal string.
C     The companion entry point ZZVSBSTR is used to retrieve the
C     characters in the virtual string.
C
C$ Examples
C
C     See the main entry point or the routine DPFMT.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 12-SEP-1996 (WLT)
C
C
C-&
 
      MYFILL = FILL
 
      CALL DPSTR ( X, MAXSIG, STRING )
 
C
C     Parse the exponent, string looks like the pattern presented
C     below:
C
C                    MAXSIG + 2
C                    |
C                    v
C     by.xxxxxxxxxxxxxEsxxx
C     1234567890123456789
C                      ^^
C                      ||
C                      |EFST = ESGN + 1
C                      |
C                      ESGN = MAXSIG + 4
C
 
      CODE0 = ICHAR ( '0' )
      BLANK = ICHAR ( ' ' )
 
 
      MINUS = ICHAR( STRING(ESGN:ESGN) ).EQ. ICHAR('-')
      CODE  = ICHAR( STRING(EFST:EFST) )
      EXP   = CODE - CODE0
 
      I     = ESCD
      CODE  = ICHAR( STRING(I:I) )
 
      DO WHILE ( CODE .NE. BLANK )
 
         EXP  = EXP * 10 + ( CODE - CODE0 )
         I    = I + 1
         CODE = ICHAR( STRING(I:I) )
 
      END DO
 
      IF ( MINUS ) THEN
         EXP = -EXP
      END IF
 
      EXPONT = EXP
 
      RETURN
 
 
C$Procedure      ZZVSBSTR ( Virtual String Character )
 
      ENTRY  ZZVSBSTR ( FROM, TO, RND, SUBSTR, DID )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Return the character from the specified SLOT of a virtual
C     decimal string.
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
C     ALPHANUMERIC, PRIVATE
C
C$ Declarations
C
C     IMPLICIT NONE
C     INTEGER               FROM
C     INTEGER               TO
C     LOGICAL               RND
C     CHARACTER*(*)         SUBSTR
C     LOGICAL               DID
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FROM       I   the index of the first character to retrieve
C     TO         I   the index of the last character to retrieve
C     RND        I   treat the virtual string as rounded string.
C     SUBSTR     O   Contents of virtual string from FROM to TO.
C     DID        O   is a leading zero a result of rounding.
C
C$ Detailed_Input
C
C     FROM       is the index in the virtual decimal string of the
C                first character that will be returned in SUBSTR.
C
C     TO         is the index in the virtual decimal string of the
C                last character that will be returned in SUBSTR.
C
C     RND        is a logical flag used to indicate that the output
C                string should represent the virtual decimal string
C                that results from rounding to the TO'th decimal
C                location.
C
C
C$ Detailed_Output
C
C     SUBSTR     is we regard the virtual string as VIRTUL.  Then
C                in FORTRAN notation SUBSTR = VIRTUL(FROM:TO)
C
C     DID        is a logical flag that is used to indicate that
C                the left most character returned by ZZVSBSTR became
C                a zero as a result of rounding up from 9.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This entry point retrieves a specified character from the
C     virtual decimal string that was established by the last
C     call to the entry point ZZVSTSTR.
C
C$ Examples
C
C     See the main entry point or the routine DPFMT.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 12-SEP-1996 (WLT)
C
C
C-&
 
 
C
C     The buffered numeric string has the form:
C
C       by.xxxxxxxxxxxxxEseee...
C       123456789012345678901234
C                1         2
C
C     Ignoring the exponent we can regard this as being the
C     decimal equivalent of the number with the decimal point
C     in the wrong position.  We'll need to remedy this.
C
C       by.xxxxxxxxxxxxx
C       1234567890123456
C                1
C
C      We can think of this decimal representation as being a
C      simplification of the "infinite string" representation
C      below.
C
C                     b    y   .   x   x       x
C        d-4 d-3 d-2 d-1  d00  p  d01 d02 ... d13  0   0   0   0
C        -2  -1   0   1    2   3   4   5       16
C
C
C     From this its clear that i'th digit can be easily computed
C     via following decision block.
C
C
C        if ( i .lt. 0 ) then
C           digit  = '0'
C        else if ( i .eq. 0 ) then
C           digit  = string(2:2)
C        else if ( i .lt. maxsig ) then
C           digit = string(i+3:i+3)
C        else
C           digit = '0'
C        end if
C
C     To have an accurate representation of the number (one that
C     accounts for the exponent) we shift the decimal point ('p')
C     "right" by EXP slots. (If EXP is negative we shift right a
C     negative number of slots).  In the sequence of characters the
C     decimal point will follow d_EXP.
C
C     IF we renumber the slots so that the decimal point is in
C     slot 0 then for S < 0 slot S contains digit d_EXP+1+S
C
C     For S > 0 slot S contains digit d_EXP+S
C
C     Combining these observations we can compute the SLOT'th character
C     of the virtual string as follows.
C
C
C     If the character requested is character zero of the virtual
C     string, we just get the decimal point.
C
C     If the character requested is in a slot whose index is
C     greater than zero it is to the
C     right of the decimal point so it must be D_exp+slot.
C
C     If the character requested is in a slot whose index is negative
C     it is to the left of the decimal point.  Since the slot
C     just to the left of the decimal point contains D_exp it follows
C     by induction that for any negative slot, the decimal is
C     D_exp+slot+1
C
C
C     Since we may need to round the output, we will work from right
C     to left.  First thing we do is get the index of the right most
C     significant portion of SUBSTR that we will manipulate.
C
      J    = TO - FROM + 1
      LSUB = LEN(SUBSTR)
C
C     Blank pad to the right of J (if there's anything to pad).
C
      IF ( J .LT. LSUB ) THEN
         SUBSTR(J+1:) = ' '
      END IF
C
C     If we need to round the output string, locate the first numeric
C     slot after TO.
C
      IF ( RND ) THEN
 
         SLOT = TO + 1
C
C        If this points to the decimal point, move one more to the
C        right.
C
         IF ( SLOT .EQ. 0 ) THEN
            SLOT = SLOT + 1
         END IF
C
C        Determine which digit D_i corresponds to SLOT.
C
         IF ( SLOT .LT. 0 ) THEN
            I = EXP + SLOT + 1
         ELSE
            I = EXP + SLOT
         END IF
C
C        We will need to round in D_i is 5 or more.
C
         IF ( I .LT. 0 ) THEN
            LETTER = '0'
         ELSE IF ( I .EQ. 0 ) THEN
            LETTER = STRING(2:2)
         ELSE IF ( I .LT. MAXSIG ) THEN
            LETTER = STRING(3+I:3+I)
         ELSE
            LETTER = '0'
         END IF
 
         INCR = LGE ( LETTER, '5' )
 
      ELSE
         INCR = NO
      END IF
C
C     Starting at the right most slot, we work left incrementing
C     digits as required.  Note that once we don't round up
C     some value, we are done incrementing.
C
      DO SLOT = TO, FROM, -1
 
         IF ( SLOT .EQ. 0 ) THEN
 
            LETTER = '.'
 
         ELSE
 
C
C           Otherwise we need to first see which digit, d_I, is being
C           requested.
C
            IF ( SLOT .LT. 0 ) THEN
               I = EXP + SLOT + 1
            ELSE
               I = EXP + SLOT
            END IF
 
C
C           Now just look up d_I according to the rule we established
C           earlier.
C
            IF ( I .LT. 0 ) THEN
 
C
C              If the SLOT is prior to the first significant character
C              or the virtual string, we use the fill character.
C              Otherwise we use a zero.
C
 
               IF ( INCR ) THEN
                  LETTER = '1'
                  INCR   = NO
               ELSE
 
                  IF ( SLOT .LT. -1 ) THEN
                     LETTER = MYFILL
                  ELSE
                     LETTER = '0'
                  END IF
 
               END IF
 
            ELSE IF ( I .EQ. 0 ) THEN
 
 
               LETTER = STRING(2:2)
C
C              If necessary, increment LETTER.
C
               IF ( INCR ) THEN
 
                  VALUE = ICHAR(LETTER) - CODE0 + 1
C
C                 If value is 10 or more we will need to
C                 increment the next character too.  If VALUE
C                 is less than 10, we are done incrementing set
C                 INCR to NO.
C
                  IF ( VALUE .EQ. 10 ) THEN
                     LETTER = '0'
                  ELSE
                     LETTER = CHAR ( VALUE + CODE0 )
                     INCR   = NO
                  END IF
 
               END IF
 
 
            ELSE IF ( I .LT. MAXSIG ) THEN
C
C              This case is virtually identical to the previous
C              case, except that we need to pick off a different
C              letter from STRING.
C
               LETTER = STRING(I+3:I+3)
 
               IF ( INCR ) THEN
 
                  VALUE = ICHAR(LETTER) - CODE0 + 1
 
                  IF ( VALUE .EQ. 10 ) THEN
                     LETTER = '0'
                  ELSE
                     LETTER = CHAR ( VALUE + CODE0 )
                     INCR   = NO
                  END IF
               END IF
 
            ELSE
 
               LETTER = '0'
               INCR   = NO
 
            END IF
 
         END IF
 
         IF ( J .LE. LSUB ) THEN
            SUBSTR(J:J) = LETTER
         END IF
 
         J = J - 1
 
      END DO
 
      DID = INCR
 
      RETURN
      END
