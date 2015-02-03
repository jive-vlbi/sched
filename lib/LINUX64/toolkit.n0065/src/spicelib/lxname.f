C$Procedure      LXNAME ( Lex names )
 
      SUBROUTINE LXNAME ( HDCHRS, TLCHRS, STRING, FIRST, LAST, IDSPEC,
     .                    NCHAR                                        )
 
C$ Abstract
C
C     Umbrella routine for name scanning entry points.
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
C     CHARACTER
C     PARSING
C     SCANNING
C     STRING
C     UTILITY
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               MXSPEC
      PARAMETER           ( MXSPEC = 512 )
 
      CHARACTER*(*)         HDCHRS
      CHARACTER*(*)         TLCHRS
      CHARACTER*(*)         STRING
      INTEGER               FIRST
      INTEGER               IDSPEC ( LBCELL : * )
      INTEGER               LAST
      INTEGER               NCHAR
 
C$ Brief_I/O
C
C     Variable  I/O  Entry points
C     --------  ---  --------------------------------------------------
C     HDCHRS     I   LXCSID
C     TLCHRS     I   LXCSID
C     STRING     I   LXIDNT
C     FIRST      I   LXIDNT
C     IDSPEC    I-O  LXDFID, LXCSID, LXIDNT
C     LAST       O   LXIDNT
C     NCHAR      O   LXIDNT
C     MXSPEC     P   LXDFID, LXCSID
C     LBCELL     P   LXIDNT, LXDFID, LXCSID
C
C$ Detailed_Input
C
C     See the entry points for descriptions of their inputs.
C
C$ Detailed_Output
C
C     See the entry points for descriptions of their outputs.
C
C$ Parameters
C
C     See the entry points for descriptions of their parameters.
C
C$ Exceptions
C
C     1) If this routine is called directly, the error
C        SPICE(BOGUSENTRY) is signaled.
C
C     See the entry points for descriptions of the exceptions
C     specific to those entry points.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Many computer languages include tokens that represent names.
C     Examples of names include procedure names and variable names.
C     The term `identifier' is generally used to indicate this type
C     of token.  Rules for constructing identifiers vary from
C     language to language, but identifiers conforming to the
C     following rules are widely recognized:
C
C        1)  The first character of the identifier is a letter.
C
C        2)  The remaining characters are letters or numbers.
C
C        3)  The length of the identifier is less than some specified
C            limit.
C
C     This suite of routines has its own set of default rules for
C     forming identifiers.  These rules are somewhat more liberal
C     than those listed above.  Rule (1) above still holds, but
C     trailing characters may include letters, numbers, and the
C     special characters
C
C        $
C        _  (underscore)
C
C     No mechanism for enforcing rule (3) is provided; this task is
C     left to the caller, since this routine would be unnecessarily
C     complicated by the need to construct diagnostic messages.
C
C     The entry point LXIDNT (Lex identifier) recognizes valid
C     identifier tokens, using either the default character sets
C     for the head and tail of the identifier, or character sets
C     specified in the last call to LXCSID.
C
C     In order to use this suite of routines to scan identifiers that
C     conform to the default rules, a program normally calls the entry
C     point LXDFID (Lex, default identifier specification) once to
C     obtain the default `identifier specification'.  This specification
C     is an integer array in which the allowed head and tail character
C     sets are specified.  This specification is then saved and supplied
C     to the entry point LXIDNT (Lex identifier) whenever LXIDNT is
C     called to scan an identifier.  The entry point LXIDNT  recognizes
C     valid identifier tokens, using an input identifier specification
C     to decide which head and tail characters are allowed in an
C     identifier.
C
C     The scanning code using these routines might have the following
C     structure:
C
C
C              INTEGER               IDSPEC ( LBCELL : MXSPEC )
C                 .
C                 .
C                 .
C        C
C        C     Initialize the identifier specification, using the
C        C     default:
C        C
C              CALL SSIZEI ( MXSPEC, IDSPEC )
C              CALL LXDFID ( IDSPEC )
C                 .
C                 .
C                 .
C        C
C        C     Scan string:
C        C
C              DO WHILE ( <more tokens> )
C                       .
C                       .
C                       .
C                 IF ( <test for identifier> ) THEN
C
C                    CALL LXIDNT ( IDSPEC, STRING, FIRST, LAST, NCHARS )
C
C                    IF ( NCHARS .GT. 0 ) THEN
C
C                       [Identifier was found--process result]
C
C                    ELSE
C
C                       [Token at starting at location FIRST was not
C                        an identifier--handle alternatives]
C
C                    END IF
C
C                 ELSE
C
C                    [ perform tests for other tokens ]
C
C                 END IF
C
C              END DO
C
C
C     It is possible to override the default rules by calling the
C     entry point LXCSID (Lex, custom identifier characters).  This
C     routine allows the caller to specify the precise set of
C     characters allowed as the first character (`head') of the
C     identifier, as well as those allowed in the remainder (`tail')
C     of the identifier.
C
C     If a custom identifier specification is desired, the call to
C     LXDFID in the pseudo code above would be replaced by a call to
C     LXCSID. After setting the strings HDCHRS and TLCHRS to contain,
C     respectively, the allowed head and tail identifier characters, the
C     following call would produce an identifier specification structure
C     IDSPEC representing these set of allowed characters.
C
C        CALL LXCSID ( HDCHRS, TLCHRS, IDSPEC )
C
C     The array IDSPEC obtained from LXCSID would be used as input to
C     LXIDNT, instead of using the array obtained by calling LXDFID.
C
C$ Examples
C
C     1)  The following table illustrates the behavior of the scanning
C         entry point LXIDNT when the default identifier syntax is in
C         effect:
C
C         STRING CONTENTS             FIRST   LAST   NCHAR
C         ==========================================================
C         WHERE A LT B                1       5      5
C         WHERE A LT B                7       7      1
C         WHERE A.LT.B                7       7      1
C         WHERE (A0)LT(B8)            8       9      2
C         WHERE A0$LT_B7              7       14     8
C         WHERE A LT B                12      12     1
C         WHERE A .LT. B              9       8      0
C
C
C     2)  The following table illustrates the behavior of the scanning
C         entry point LXIDNT when a custom identifier syntax is used.
C         The call
C
C            CALL LXCSID ( HDCHRS, TLCHRS, IDSPEC )
C
C         where
C
C            HDCHRS = 'abcdefghijklmnopqrstuvwxyz'
C
C         and
C
C            TLCHRS = 'abcdefghijklmnopqrstuvwxyz012345.'
C
C        will produce an indentifier specification IDSPEC that,
C        when supplied as an input to LXIDNT, will cause LXIDNT
C        to perform in accordance with the table shown below:
C
C
C         STRING CONTENTS             FIRST   LAST   NCHAR
C         ==========================================================
C         WHERE A LT B                1       0      0
C         where a lt b                1       5      5
C         WHERE a LT b                7       7      1
C         WHERE a.LT.b                7       8      2
C         WHERE (a0)LT(b8)            14      14     1
C         WHERE (a0)LT(b5)            14      15     2
C         WHERE a0.lt.b8              7       13     7
C         WHERE a0$lt_b7              7       8      2
C         where a .lt. b              9       12     4
C
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
C     N.J. Bachman       (JPL)
C     B.V. Semenov       (JPL)
C
C$ Version
C
C-    Beta Version 1.0.1, 10-FEB-2014 (BVS)
C
C        Added LBCELL to the Brief_I/O section.
C
C-    Beta Version 1.0.0, 25-OCT-1995 (NJB)
C
C-&
 
C$ Index_Entries
C
C     scan name tokens --- umbrella
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               BSRCHI
      INTEGER               CARDI
      INTEGER               RTRIM
 
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               MAXCHR
      PARAMETER           ( MAXCHR = 255 )
 
      INTEGER               BLANK
      PARAMETER           ( BLANK  = 32  )
 
      INTEGER               MINPRT
      PARAMETER           ( MINPRT = 32 )
 
      INTEGER               MAXPRT
      PARAMETER           ( MAXPRT = 126 )
 
C
C     IDSPEC parameters:
C
      INTEGER               NHPOS
      PARAMETER           ( NHPOS  = 1 )
 
      INTEGER               NTPOS
      PARAMETER           ( NTPOS  = 2 )
 
      INTEGER               HCPOS
      PARAMETER           ( HCPOS  = 3 )
 
 
C
C     Local variables
C
      INTEGER               C
      INTEGER               HEADC  ( LBCELL : MAXCHR )
      INTEGER               HL
      INTEGER               I
      INTEGER               L
      INTEGER               NHEAD
      INTEGER               NTAIL
      INTEGER               TAILC  ( LBCELL : MAXCHR )
      INTEGER               TCPOS
      INTEGER               TL
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LXNAME' )
      END IF
 
 
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
 
 
      CALL CHKOUT ( 'LXNAME' )
      RETURN
 
 
 
 
 
C$Procedure      LXIDNT ( Lex identifer )
 
      ENTRY LXIDNT ( IDSPEC, STRING, FIRST, LAST, NCHAR )
 
C$ Abstract
C
C     Lex (scan) an identifer,  starting from a specified character
C     position.
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
C     CHARACTER
C     PARSING
C     SCANNING
C     STRING
C     UTILITY
C
C$ Declarations
C
C     INTEGER               LBCELL
C     PARAMETER           ( LBCELL = -5 )
C 
C     INTEGER               IDSPEC ( LBCELL : * )
C     CHARACTER*(*)         STRING
C     INTEGER               FIRST
C     INTEGER               LAST
C     INTEGER               NCHAR
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     IDSPEC     I   Identifier character specification.
C     STRING     I   String to be scanned.
C     FIRST      I   Character position at which to start scanning.
C     LAST       O   Character position of end of token.
C     NCHAR      O   Number of characters in token.
C     LBCELL     P   The SPICELIB cell lower bound.
C
C$ Detailed_Input
C
C     IDSPEC         is an integer cell containing a specification of
C                    the head and tail identifier character sets to be
C                    used in scanning the input argument STRING.  IDSPEC
C                    should be obtained by calling LXDFID or LXCSID.
C                    The structure of IDSPEC is not part of the
C                    specification of this routine suite and should not
C                    be relied upon by calling code.
C
C     STRING         is a character string that may contain an
C                    `identifier' starting at the character position
C                    indicated by the input argument FIRST (see
C                    below).  Identifier tokens are sequences of
C                    characters that represent names.  Syntactically, an
C                    identifier is a sequence of characters that begins
C                    with a character belonging to a set of valid `head'
C                    characters and is followed by zero or more
C                    characters belonging to a set of valid `tail'
C                    characters.
C
C     FIRST          is the character position at which the routine
C                    is to start scanning an identifier.  Note
C                    that the character STRING(FIRST:FIRST) must be a
C                    valid head character if an identifier is to
C                    be found; this routine does *not* attempt to locate
C                    the first identifier following the position
C                    FIRST.
C
C$ Detailed_Output
C
C     LAST           is the last character position such that the
C                    substring STRING(FIRST:LAST) is an identifier, if
C                    such a substring exists.  Otherwise, the
C                    returned value of LAST is FIRST-1.
C
C     NCHAR          is the length of the identifier found by this
C                    routine, if such a token exists.  If an identifier
C                    is not found, the returned value of NCHAR is
C                    zero.
C
C$ Parameters
C
C     LBCELL         is the SPICELIB cell lower bound.
C
C$ Exceptions
C
C     Error free.
C
C     1) If the input argument FIRST is less than 1 or greater than
C        LEN(STRING)-1, the returned value of LAST is FIRST-1, and the
C        returned value of NCHAR is zero.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The default syntax rules for valid identifiers are specified in
C     the $Particulars section of the umbrella routine LXNAME.  These
C     rules may be overridden by calling LXCSID.
C
C$ Examples
C
C     See the $Examples section of the umbrella routine LXNAME.
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
C     N.J. Bachman       (JPL)
C     B.V. Semenov       (JPL)
C
C$ Version
C
C-    Beta Version 1.0.1, 10-FEB-2014 (BVS)
C
C        Added LBCELL to the Declarations, Brief_I/O, and Parameters
C        sections.
C
C-    Beta Version 1.0.0, 25-OCT-1995 (NJB)
C
C-&
 
C$ Index_Entries
C
C        scan identifiers
C
C-&
 
C
C     No check-in required; this entry point is error-free.
C
 
 
C
C     Save the length of the non-blank prefix of the input string.
C
      L    =   RTRIM(STRING)
 
C
C     Handle the cases in which we can tell right away that
C     no token can be found.
C
      IF (  ( FIRST .LT. 1 ) .OR. ( FIRST .GT. L )  ) THEN
 
         LAST  = FIRST - 1
         NCHAR = 0
         RETURN
 
      END IF
 
C
C     In order for there to be a match, the character at position
C     FIRST must be in the head character set.
C
      NHEAD =  IDSPEC(NHPOS)
 
      C     =  ICHAR  (  STRING(FIRST:FIRST)       )
      I     =  BSRCHI (  C,  NHEAD,  IDSPEC(HCPOS) )
 
      IF ( I .EQ. 0 ) THEN
 
         LAST  = FIRST - 1
         NCHAR = 0
         RETURN
 
      END IF
 
C
C     We have an identifier.  The remaining question is how long it is.
C     Each subsequent character that is in the tail character set is
C     considered to be part of the identifier.
C
      NCHAR  =  1
      LAST   =  FIRST
      NTAIL  =  IDSPEC(NTPOS)
      TCPOS  =  3 + NHEAD
 
      DO WHILE ( LAST .LT. L )
 
         C  =  ICHAR  (  STRING( LAST+1 : LAST+1 )       )
         I  =  BSRCHI (  C,      NTAIL,   IDSPEC(TCPOS)  )
 
         IF ( I .EQ. 0 ) THEN
            RETURN
         ELSE
            NCHAR  =  NCHAR + 1
            LAST   =  LAST  + 1
         END IF
 
      END DO
 
 
      RETURN
 
 
 
 
 
C$Procedure      LXDFID ( Lex, default identifier characters )
 
      ENTRY LXDFID ( IDSPEC )
 
C$ Abstract
C
C     Return the default specification for the characters that may
C     appear in an identifier.
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
C     CHARACTER
C     PARSING
C     SCANNING
C     STRING
C     UTILITY
C
C$ Declarations
C
C     INTEGER               MXSPEC
C     PARAMETER           ( MXSPEC = 512 )
C
C     INTEGER               LBCELL
C     PARAMETER           ( LBCELL = -5 )
C 
C     INTEGER               IDSPEC ( LBCELL : * )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     IDSPEC    I-O  Identifier character specification.
C     MXSPEC     P   Recommended size for declaration of IDSPEC.
C     LBCELL     P   The SPICELIB cell lower bound.
C
C$ Detailed_Input
C
C     IDSPEC         is an integer cell.  The caller must initialize
C                    IDSPEC as a cell, and should use MXSPEC as the size
C                    of IDSPEC.
C
C$ Detailed_Output
C
C     IDSPEC         is an integer cell containing a specification of
C                    the head and tail identifier character sets to be
C                    used the entry point LXIDNT in scanning strings.
C
C$ Parameters
C
C     MXSPEC         is the recommended size for the declaration of
C                    IDSPEC; the caller should declare IDSPEC as shown:
C
C                       INTEGER       IDSPEC ( LBCELL : MXSPEC )
C
C                    The caller should also initialize IDSPEC as shown:
C
C                       CALL SSIZEI ( MXSPEC, IDSPEC )
C
C     LBCELL         is the SPICELIB cell lower bound.
C
C$ Exceptions
C
C     1) If IDSPEC is not properly initialized on input, or if its
C        size is too small, the error will be diagnosed by routines
C        called by this routine.  IDSPEC is undefined on output in this
C        case.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine allows a calling program to obtain the default set of
C     allowed patterns for identifiers recognized by LXIDNT.
C
C     Normally, this routine should be called once during the calling
C     program's initialization.
C
C$ Examples
C
C     See the $Examples section of the umbrella routine LXNAME.
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
C     N.J. Bachman       (JPL)
C     B.V. Semenov       (JPL)
C
C$ Version
C
C-    Beta Version 1.0.1, 10-FEB-2014 (BVS)
C
C        Added LBCELL to the Declarations, Brief_I/O, and Parameters
C        sections.
C
C-    Beta Version 1.0.0, 25-OCT-1995 (NJB)
C
C-&
 
C$ Index_Entries
C
C     return default allowed identifier characters
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LXDFID' )
      END IF
 
C
C     Initialize our head and tail character sets.
C
      CALL SSIZEI ( MAXCHR, HEADC )
      CALL SSIZEI ( MAXCHR, TAILC )
 
C
C     Fill in the head and tail character arrays with their default
C     values.  User integer codes for the characters.
C
      DO I = 1, 26
 
         HEADC( I    ) = ICHAR( 'A'  ) + I - 1
         HEADC( I+26 ) = ICHAR( 'a'  ) + I - 1
         TAILC( I    ) = HEADC( I    )
         TAILC( I+26 ) = HEADC( I+26 )
 
      END DO
 
      DO I = 1, 10
         TAILC(52+I) = ICHAR('0') + I - 1
      END DO
 
      TAILC(63) = ICHAR( '$' )
      TAILC(64) = ICHAR( '_' )
 
      NHEAD     = 52
      NTAIL     = 64
 
C
C     Turn the arrays into integer sets.
C
      CALL VALIDI ( MAXCHR, NHEAD, HEADC )
      CALL VALIDI ( MAXCHR, NTAIL, TAILC )
 
C
C     Create the output specification IDSPEC.  This is a cell
C     containing, in order,
C
C        - the number of head characters
C        - the number of tail characters
C        - integer codes for the head characters
C        - integer codes for the tail characters
C
C     IDSPEC is assumed to be initialized.
C
C
      CALL SCARDI ( 0,     IDSPEC )
 
      CALL APPNDI ( NHEAD, IDSPEC )
      CALL APPNDI ( NTAIL, IDSPEC )
 
      DO I = 1, NHEAD
         CALL APPNDI ( HEADC(I), IDSPEC )
      END DO
 
      DO I = 1, NTAIL
         CALL APPNDI ( TAILC(I), IDSPEC )
      END DO
 
      CALL CHKOUT ( 'LXDFID' )
      RETURN
 
 
 
 
 
C$Procedure      LXCSID ( Lex, custom identifier characters )
 
      ENTRY LXCSID ( HDCHRS, TLCHRS, IDSPEC )
 
C$ Abstract
C
C     Set the acceptable characters that may appear in an identifier
C     token.
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
C     CHARACTER
C     PARSING
C     SCANNING
C     STRING
C     UTILITY
C
C$ Declarations
C
C     INTEGER               MXSPEC
C     PARAMETER           ( MXSPEC = 512 )
C
C     INTEGER               LBCELL
C     PARAMETER           ( LBCELL = -5 )
C 
C     CHARACTER*(*)         HDCHRS
C     CHARACTER*(*)         TLCHRS
C     INTEGER               IDSPEC ( LBCELL : * )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HDCHRS     I   Allowed head characters for identifiers.
C     TLCHRS     I   Allowed tail characters for identifiers.
C     IDSPEC    I-O  Identifier character specification.
C     MXSPEC     P   Recommended size for declaration of IDSPEC.
C     LBCELL     P   The SPICELIB cell lower bound.
C
C$ Detailed_Input
C
C     HDCHRS         is a string containing the set of characters
C                    allowed as the first (`head') character of an
C                    identifier token.  Case is significant; if both
C                    upper and lower case instances of a letter are
C                    allowed, they must both be listed.  White space is
C                    ignored.  Non-printing characters are not allowed.
C
C     TLCHRS         is a string containing the set of characters
C                    allowed as tail characters (characters following
C                    the head character) of an identifier token.  Case
C                    is significant; white space is ignored.
C                    Non-printing characters are not allowed.
C
C     IDSPEC         is an integer cell.  The caller must initialize
C                    IDSPEC as a cell, and should use MXSPEC as the size
C                    of IDSPEC.
C
C$ Detailed_Output
C
C     IDSPEC         is an integer cell containing a specification of
C                    the head and tail identifier character sets to be
C                    used the entry point LXIDNT in scanning strings.
C                    The caller must initialize IDSPEC as a cell, and
C                    should use MXSPEC as the size of IDSPEC.
C
C$ Parameters
C
C     MXSPEC         is the recommended size for the declaration of
C                    IDSPEC; the caller should declare IDSPEC as shown:
C
C                       INTEGER       IDSPEC ( LBCELL : MXSPEC )
C
C                    The caller should also initialize IDSPEC as shown:
C
C                       CALL SSIZEI ( MXSPEC, IDSPEC )
C
C     LBCELL         is the SPICELIB cell lower bound.
C
C$ Exceptions
C
C     1) If non-printing characters are found in either of the input
C        arguments HDCHRS or TLCHRS, the error SPICE(NONPRINTINGCHARS)
C        is signaled.  The set of allowed identifier characters is not
C        modified.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine allows a calling program to customize the set of
C     allowed patterns for identifiers recognized by LXIDNT.
C
C     Normally, this routine should be called once during the calling
C     program's initialization, if this routine is called at all.
C
C$ Examples
C
C     See the $Examples section of the umbrella routine LXNAME.
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
C     N.J. Bachman       (JPL)
C     B.V. Semenov       (JPL)
C
C$ Version
C
C-    Beta Version 1.0.1, 10-FEB-2014 (BVS)
C
C        Added LBCELL to the Declarations, Brief_I/O, and Parameters
C        sections.
C
C-    Beta Version 1.0.0, 25-OCT-1995 (NJB)
C
C-&
 
C$ Index_Entries
C
C     customize allowed identifier characters for lexing
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LXCSID' )
      END IF
 
C
C     Initialize our head and tail character sets, every time.
C
      CALL SSIZEI ( MAXCHR, HEADC )
      CALL SSIZEI ( MAXCHR, TAILC )
 
C
C     Check the inputs before proceeding.
C
      HL  =  RTRIM(HDCHRS)
      TL  =  RTRIM(TLCHRS)
 
 
      DO I = 1, HL
 
         C  =  ICHAR( HDCHRS(I:I) )
 
         IF (  ( C .LT. MINPRT ) .OR. ( C .GT. MAXPRT )  ) THEN
 
            CALL SETMSG ( 'The character having integer code # in '  //
     .                    'position # of the head character string ' //
     .                    'HDCHRS is a non-printing character.'      )
            CALL ERRINT ( '#',  C                                    )
            CALL ERRINT ( '#',  I                                    )
            CALL SIGERR ( 'SPICE(NONPRINTINGCHARS)'                  )
            CALL CHKOUT ( 'LXCSID'                                   )
            RETURN
 
         END IF
 
      END DO
 
 
      DO I = 1, TL
 
         C  =  ICHAR( TLCHRS(I:I) )
 
         IF (  ( C .LT. MINPRT ) .OR. ( C .GT. MAXPRT )  ) THEN
 
            CALL SETMSG ( 'The character having integer code # in '  //
     .                    'position # of the tail character string ' //
     .                    'TLCHRS is a non-printing character.'      )
            CALL ERRINT ( '#',  C                                    )
            CALL ERRINT ( '#',  I                                    )
            CALL SIGERR ( 'SPICE(NONPRINTINGCHARS)'                  )
            CALL CHKOUT ( 'LXCSID'                                   )
            RETURN
 
         END IF
 
      END DO
 
 
C
C     The characters of HDCHRS become the set of acceptable
C     characters for the head identifier character---all except
C     the blanks.  Same deal goes for the tail characters.
C
      DO I = 1, HL
 
         C  =  ICHAR( HDCHRS(I:I) )
 
         IF ( C .NE. BLANK ) THEN
            CALL INSRTI ( C, HEADC )
         END IF
 
      END DO
 
      NHEAD  =  CARDI ( HEADC )
 
 
 
      DO I = 1, TL
 
         C  =  ICHAR( TLCHRS(I:I) )
 
         IF ( C .NE. BLANK ) THEN
            CALL INSRTI ( C, TAILC )
         END IF
 
      END DO
 
 
      NTAIL  =  CARDI ( TAILC )
 
C
C     Create the output specification IDSPEC.  This is a cell
C     containing, in order,
C
C        - the number of head characters
C        - the number of tail characters
C        - integer codes for the head characters
C        - integer codes for the tail characters
C
C     IDSPEC is assumed to be initialized.
C
C
      CALL SCARDI ( 0,     IDSPEC )
 
      CALL APPNDI ( NHEAD, IDSPEC )
      CALL APPNDI ( NTAIL, IDSPEC )
 
      DO I = 1, NHEAD
         CALL APPNDI ( HEADC(I), IDSPEC )
      END DO
 
      DO I = 1, NTAIL
         CALL APPNDI ( TAILC(I), IDSPEC )
      END DO
 
      CALL CHKOUT ( 'LXCSID' )
      RETURN
      END
