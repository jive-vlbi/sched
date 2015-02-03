C$Procedure ZZTIME ( Private, Time --- time parsing utilities )

      LOGICAL FUNCTION ZZTIME ( STRING, TRANSL, LETTER, ERROR, PIC,
     .                          TVEC,   B,      E,      L2R,   YABBRV )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This is an umbrella routine for a collection of entry points
C     to the time parsing utility functions.
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
C      TIME --- Private
C
C$ Declarations

      IMPLICIT NONE
      CHARACTER*(*)         STRING
      CHARACTER*(*)         TRANSL
      CHARACTER*(1)         LETTER
      CHARACTER*(*)         ERROR
      CHARACTER*(*)         PIC
      DOUBLE PRECISION      TVEC  ( * )
      INTEGER               B
      INTEGER               E
      LOGICAL               L2R
      LOGICAL               YABBRV


C$ Brief_I/O
C
C     VARIABLE  I/O  Entry Points
C     --------  ---  --------------------------------------------------
C     STRING    I/O  ZZUNPCK ZZCMBT ZZGREP ZZISPT  ZZSUBT ZZTOKNS ZZVALT
C     TRANSL     I   ZZUNPCK ZZSUBT
C     LETTER     I   ZZCMBT  ZZIST   ZZNOTE  ZZREMT ZZVALT
C     ERROR      O   ZZUNPCK ZZTOKNS
C     TVEC       O   ZZUNPCK
C     B          O   ZZISPT  ZZNOTE  ZZVALT
C     E          O   ZZISPT  ZZNOTE  ZZUNPCK ZZVALT
C     L2R        I   ZZCMBT  ZZSUBT
C     YABBRV     I   ZZUNPCK
C
C$ Detailed_Input
C
C     See Individual Entry Points.
C
C$ Detailed_Output
C
C     See Individual Entry Points.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If ZZTIME is called directly the error 'SPICE(BOGUSENTRY)'
C         is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine serves as an umbrella for a collection of
C     related entry points that are used to parse time strings.
C
C     Normal usage is to first call ZZTOKNS to create an internal
C     representation for a time string. This internal representations
C     maintains a list of identified substrings from the original
C     input time string. For example the call to ZZTOKNS using
C     the string
C
C       '1996 JAN 25 12:18:19.199'
C        123456789012345678901234
C
C     yields the following internal representation:
C
C       'ibmbibi:i:i.i'
C
C     where the individual tokens correspond to the substrings
C     indicated in the following table:
C
C       Identifier    Substring          meaning
C       ----------    -------------      ----------------
C         i           from 01 to 04      unsigned integer
C         b           from 05 to 05      blanks or tab
C         m           from 06 to 08      month
C         b           from 09 to 09      blanks or tab
C         i           from 10 to 11      unsigned integer
C         b           from 12 to 12      blank or tab
C         i           from 13 to 14      unsigned integer
C         :           from 15 to 15      colon
C         i           from 16 to 17      unsigned integer
C         :           from 18 to 18      colon
C         i           from 19 to 20      unsigned integer
C         .           from 21 to 21      decimal point
C         i           from 22 to 24      unsigned integer
C
C     These substrings may be combined and reidentified, removed
C     or re-identified using the various entry points listed here:
C
C     ZZCMBT   combine several tokens into a single token
C              for example you might scan right to left and replace
C              the token sequence i.i by n (for number). In this
C              case the substring boundaries of n would be from 19
C              to 24.
C
C     ZZGREP   returns the current internal representation
C              in the case above 'ibmbibi:i:i.i'
C
C
C     ZZISPT   returns TRUE if a pair of letters from a list are
C              present in the internal representation. This is
C              used primarily to detect erroneous substrings such
C              as ',,' or ':,'
C
C     ZZIST    Return TRUE if a particular letter is present in the
C              string.
C
C     ZZNOTE   Returns the substring boundaries associated with
C              a letter and removes the letter from the internal
C              representation. This is used primarily for calendar
C              string modifiers such as 'B.C.', 'A.D.' etc.
C
C     ZZREMT   remove a letter from the internal representation.
C              In the input example you might remove all white space
C              markers.
C
C     ZZSUBT   substitute a different letter for one listed in the
C              input one for one. For example after removing blanks
C              you might substitute YmD for imi.
C
C
C     ZZVALT   replace an integer by a new marker if the integer
C              lies withing a particular range. For example
C              you might replace any integer between 1000 and 10000
C              by Y (for year).
C
C     Once all substitutions and removals have been performed that
C     can be made, the entry point ZZUNPCK allows you to extract
C     year(Y), month(m), day or month(D), day of year (y), hours(H),
C     minutes(M) and seconds(S) from the input string
C
C$ Examples
C
C     See TPARTV.
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
C     N.J. Bachman    (JPL)
C     B.V. Semenov    (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.6.0, 05-FEB-2014 (EDW) (BVS)
C
C        BUG FIX: entry point ZZUNPCK: added error check on ITEM value.
C        Failure to perform this check can cause BADSUBSCRIPT error
C        signals from CSPICE code on invalid time strings.
C
C        BUG FIX: entry point ZZTOKNS: added checks for token indexes
C        overflowing the maximum number of tokens and for the character
C        positions in the time picture overflowing the time picture
C        length. Both overflows previously resulted in segmentation
C        faults for invalid input time strings that contained too many
C        recognizable tokens or were too long and required too many
C        characters in the picture representation.
C
C-    SPICELIB Version 1.5.0, 08-MAR-2009 (NJB)
C
C        Bug fix: in entry point ZZTOKNS, changed upper
C        bound used to detect non-printing characters from 128
C        to 126.
C
C        Bug fix: added error handling to this routine. Header
C        already referred to SPICE(BOGUSENTRY) error, but no
C        such error was signaled.
C
C        Changed upper bound of arrays NAMES, F, and L from 128
C        to 126.
C
C        Re-ordered header sections in various entry points.
C
C-    SPICELIB Version 1.4.0, 27-OCT-2006 (BVS)
C
C        Fixed the bug in the ZZTOKNS entry that in the case of a one
C        character long blank input time string caused the TO variable
C        be set to the value greater than the string length, triggering
C        an OUT OF BOUNDS runtime error on HP. Added to ZZTOKNS a
C        separate check for the blank input strings.
C
C-    SPICELIB Version 1.3.0, 13-Nov-2000 (WLT)
C
C        Changed the call to EQSTR to a call to SAMSBI so as to
C        guard against overflowing strings.
C
C-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
C
C        The main routine (which should never be called) now returns
C        the value .FALSE.
C
C-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
C
C        Added a RETURN statement at the end of the main routine.
C        Enhanced error message for the case when the input string
C        to ZZTOKNS has a non-printing character.
C
C-    SPICELIB Version 1.0.0, 8-APR-1996 (WLT)
C
C-&


C
C     Entry points
C
      LOGICAL               ZZCMBT
      LOGICAL               ZZGREP
      LOGICAL               ZZISPT
      LOGICAL               ZZIST
      LOGICAL               ZZNOTE
      LOGICAL               ZZREMT
      LOGICAL               ZZSUBT
      LOGICAL               ZZTOKNS
      LOGICAL               ZZUNPCK
      LOGICAL               ZZVALT

C
C     SPICELIB Functions
C
      INTEGER               CPOS
      INTEGER               POS
      INTEGER               POSR
      INTEGER               RTRIM
      INTEGER               ISRCHC

      LOGICAL               SAMSBI
      LOGICAL               SAMCHI

C
C     Standard Parameters
C
      LOGICAL               YES
      PARAMETER           ( YES = .TRUE.  )

      LOGICAL               NO
      PARAMETER           ( NO  = .FALSE. )



C
C     LOWER
C     UPPER
C     MIXED
C

      INTEGER               LOWER
      PARAMETER           ( LOWER  = 1 )

      INTEGER               UPPER
      PARAMETER           ( UPPER  = LOWER  + 1 )

      INTEGER               MIXED
      PARAMETER           ( MIXED  = UPPER  + 1 )

C
C     FULL
C     SHORT
C

      INTEGER               FULL
      PARAMETER           ( FULL   = 1 )

      INTEGER               SHORT
      PARAMETER           ( SHORT  = FULL   + 1 )

C
C     Maximum number of tokens that a valid time string can contain.
C
      INTEGER               MAXTKN
      PARAMETER           ( MAXTKN = 64 )

C
C     Length of the string buffer containing the time string picture.
C
      INTEGER               PICLEN
      PARAMETER           ( PICLEN = MAXTKN * 5 )

C
C     Representation Variables.
C
      CHARACTER*(MAXTKN)    REP
      INTEGER               SIZE

      INTEGER               BEGS ( MAXTKN )
      INTEGER               ENDS ( MAXTKN )
      INTEGER               PBEGS( MAXTKN )
      INTEGER               PENDS( MAXTKN )

      CHARACTER*(PICLEN)    PICTUR


      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      CHARACTER*(WDSIZE)    NAMES( 32 : 126 )

      CHARACTER*(WDSIZE)    MYERR

      CHARACTER*(PICLEN)    MESSGE
      CHARACTER*(PICLEN)    PICERR
      CHARACTER*(PICLEN)    TKNERR


      CHARACTER*(3)         MONTH
      CHARACTER*(3)         MONTHS ( 12 )

      DOUBLE PRECISION      HMS    ( 3  )

      INTEGER               BLANK
      INTEGER               FROM
      INTEGER               GET
      INTEGER               I
      INTEGER               ITEM
      INTEGER               J
      INTEGER               K
      INTEGER               LAST
      INTEGER               NCHAR
      INTEGER               NEXT
      INTEGER               P1
      INTEGER               P2
      INTEGER               PTR
      INTEGER               PUT
      INTEGER               R
      INTEGER               TO
      INTEGER               W
      INTEGER               NJD

      INTEGER               PFROM
      INTEGER               PTO
      INTEGER               PNEXT
      INTEGER               CASE
      INTEGER               KIND


      INTEGER               NYEAR
      INTEGER               NMON
      INTEGER               NDAY
      INTEGER               NHOUR
      INTEGER               NMIN
      INTEGER               NSEC
      INTEGER               NDOY
      INTEGER               VALUE

      INTEGER               MNSIZE ( SHORT )
      INTEGER               WKSIZE ( SHORT )
      
C
C     Token Recognition Variables.
C
C     At the moment there are 53 recognized substrings, we
C     make room for 70 just so we won't have to increase
C     the parameter NRECOG soon.
C
      INTEGER               NRECOG
      PARAMETER           ( NRECOG = 70 )

      INTEGER               SMWDSZ
      PARAMETER           ( SMWDSZ = 12 )

      INTEGER               WIDTH ( NRECOG )
      CHARACTER*(SMWDSZ)    RECOG ( NRECOG )
      CHARACTER*(SMWDSZ)    SPCIAL

      CHARACTER*(SMWDSZ)    MNMRK ( MIXED, SHORT )
      CHARACTER*(SMWDSZ)    WKDAY ( MIXED, SHORT )

      CHARACTER*(1)         CLASS ( NRECOG )
      CHARACTER*(1)         THIS

      INTEGER               F ( 32 : 126 )
      INTEGER               L ( 32 : 126 )

      LOGICAL               AMPM
      LOGICAL               DID
      LOGICAL               GOT
      LOGICAL               CHECK
      LOGICAL               FIRST

      SAVE

      DATA                  SIZE   /  0     /
      DATA                  FIRST  / .TRUE. /
      DATA                  MONTHS / 'JAN', 'FEB', 'MAR', 'APR',
     .                               'MAY', 'JUN', 'JUL', 'AUG',
     .                               'SEP', 'OCT', 'NOV', 'DEC' /

      ZZTIME = .FALSE.

      CALL CHKIN  ( 'ZZTIME'            )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'ZZTIME'            )
      RETURN




C$Procedure ZZCMBT ( Private, Time --- combine tokens )

      ENTRY ZZCMBT  ( STRING, LETTER, L2R )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Combine several token representatives into a single token.
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
C      TIME --- PRIVATE
C
C
C$ Declarations
C
C     CHARACTER*(*)         STRING
C     CHARACTER*(1)         LETTER
C     LOGICAL               L2R
C
C$ Brief_I/O
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   A sequence of tokens to be combined.
C     LETTER     I   The replacement token for the combination
C     L2R        I   If TRUE scan left to right, else scan right to left
C
C     The function returns TRUE is a combination was performed.
C
C$ Detailed_Input
C
C     STRING     is a sequence of tokens to look for in the
C                stored internal representation.
C
C     LETTER     is the replacement token to insert for STRING.
C
C                If letter is a blank, the combination is simply
C                replaced by a blank.
C
C     L2R        is a logical. If TRUE, the internal representation
C                is scanned left to right. If FALSE, the internal
C                representation is scanned right to left.
C
C$ Detailed_Output
C
C     The function returns TRUE if a combination is performed.
C     Otherwise it returns FALSE.
C
C     Note that the most important action of this function is a
C     side-effect. The internal representation of a time string
C     is modified to reflect the requested token combination.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error Free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This function allows you to alter the internal representation
C     of a time string by combining two or more tokens into a single
C     token.
C
C$ Examples
C
C     See TPARTV
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
C
C$ Version
C
C-    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB)
C
C        Re-ordered header sections.
C
C-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
C
C        The main routine (which should never be called) now returns
C        the value .FALSE.
C
C-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
C
C        Added a RETURN statement at the end of the main routine.
C        Enhanced error message for the case when the input string
C        to ZZTOKNS has a non-printing character.
C
C-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
C
C
C-&
C
C     So far we haven't combined anything.
C
      DID = NO
C
C     Look for the substring either looking from the
C     left (L2R is YES) or from the right (L2R is NO).
C
      IF ( L2R ) THEN
         FROM = POS  ( REP(1:SIZE), STRING, 1    )
      ELSE
         FROM = POSR ( REP(1:SIZE), STRING, SIZE )
      END IF

      TO = FROM + LEN(STRING) - 1

      IF ( FROM .GT. 0 ) THEN

         DID         = YES
         ENDS(FROM)  = ENDS(TO)
         PENDS(FROM) = PENDS(TO)
         PUT         = FROM + 1
         NEXT        = TO   + 1

C
C        Perform the substitution in the representation
C
         CALL ZZREPSUB ( REP, FROM, TO, LETTER, REP )
C
C        Now update the begins and ends of tokens in the original
C        string.
C
         DO GET = NEXT, SIZE

            BEGS (PUT) = BEGS (GET)
            ENDS (PUT) = ENDS (GET)
            PBEGS(PUT) = PBEGS(GET)
            PENDS(PUT) = PENDS(GET)

            PUT       = PUT + 1
         END DO

         SIZE = SIZE - LEN(STRING) + 1

      END IF

      ZZCMBT = DID
      RETURN




C$Procedure ZZGREP ( Private, Time --- get representation )

      ENTRY ZZGREP  ( STRING )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Return the internal representation of the time string.
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
C      TIME --- PRIVATE
C
C
C$ Declarations
C
C     CHARACTER*(*)         STRING
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     O   The current representation of tokenized time
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     STRING     is the current internal tokenized representation of
C                the time string that was last supplied to ZZTIME
C                via the entry point ZZTOKNS.
C
C     The function returns TRUE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This returns the current internal representation of the
C     tokenized time string. The function always returns the
C     value TRUE.
C
C$ Examples
C
C     See TPARTV.
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
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
C
C        The main routine (which should never be called) now returns
C        the value .FALSE.
C
C-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
C
C        Added a RETURN statement at the end of the main routine.
C        Enhanced error message for the case when the input string
C        to ZZTOKNS has a non-printing character.
C
C-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
C
C
C-&

      STRING = REP(1:MAX(1,SIZE))
      ZZGREP = .TRUE.
 
      RETURN




C$Procedure ZZISPT ( Private, Time --- is pair of tokens )

      ENTRY ZZISPT  ( STRING, B, E )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Determine if there is a pair of consecutive tokens from
C     a user specified list of tokens.
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
C      TIME --- PRIVATE
C
C$ Declarations
C
C     CHARACTER*(*)         STRING
C     INTEGER               B
C     INTEGER               E
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   a list of tokens to search for.
C     B          O   the beginning of the first matching token
C     E          O   the ending of the last matching token.
C
C     The function returns TRUE if a pair is found.
C
C$ Detailed_Input
C
C     STRING     is a character string that gives a list of tokens
C                to search for in a string.
C
C$ Detailed_Output
C
C     B          is the location in the original time string supplied
C                to ZZTOKNS of the beginning a pair of consecutive
C                tokens from the list specified by STRING.
C
C     E          is the location in the original time string supplied
C                to ZZTOKENS of the end a pair of consecutive
C                tokens from the list specified by STRING.
C
C     The function returns the TRUE is a consecutive pair of tokens
C     from STRING is located. Otherwise it returns FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error Free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine exists primarily to assist in the diagnosis
C     of consecutive delimiters in a time string.
C
C$ Examples
C
C     See TPARTV
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
C
C$ Version
C
C-    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB)
C
C        Re-ordered header sections.
C
C-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
C
C        The main routine (which should never be called) now returns
C        the value .FALSE.
C
C-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
C
C        Added a RETURN statement at the end of the main routine.
C        Enhanced error message for the case when the input string
C        to ZZTOKNS has a non-printing character.
C
C-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
C
C
C-&

      DID  = NO
      FROM = CPOS ( REP, STRING, 1 )

      DO WHILE ( FROM .GT. 0 )

         IF ( FROM .LT. SIZE ) THEN
            TO   = FROM + 1
            DID  = INDEX( STRING, REP(TO:TO) ) .GT. 0
         ELSE
            B       = 0
            E       = 0
            ZZISPT  = .FALSE.
            RETURN
         END IF

         IF ( DID ) THEN
            B      = BEGS(FROM)
            E      = ENDS(TO)
            ZZISPT = .TRUE.
            RETURN
         END IF

         FROM = CPOS ( REP, STRING, TO )

      END DO

      B      =  0
      E      =  0
      ZZISPT = .FALSE.
 
      RETURN




C$Procedure ZZIST ( Private, Time --- is there a token )

      ENTRY ZZIST   ( LETTER )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Determine if a token is present in the internal representation
C     of a tokenized time string.
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
C      TIME --- PRIVATE
C
C
C$ Declarations
C
C     CHARACTER*(1)         LETTER
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LETTER     I
C
C     The function returns
C
C$ Detailed_Input
C
C     LETTER     is a token to look for in the tokenized representation
C                of a time string.
C
C$ Detailed_Output
C
C     The function returns TRUE is LETTER is present in the internal
C     representation of the last time string passed to ZZTOKNS.
C     Otherwise it returns FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine determines whether or not a particular token
C     is present in a tokenized representation of a time.
C
C$ Examples
C
C     See TPARTV
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
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
C
C        The main routine (which should never be called) now returns
C        the value .FALSE.
C
C-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
C
C        Added a RETURN statement at the end of the main routine.
C        Enhanced error message for the case when the input string
C        to ZZTOKNS has a non-printing character.
C
C-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
C
C
C-&

      ZZIST = INDEX ( REP(1:SIZE), LETTER ) .GT. 0
      RETURN




C$Procedure ZZNOTE ( Private, Time --- note the existence and remove )

      ENTRY ZZNOTE  ( LETTER, B, E )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Return the beginning and ending of a token in a time string
C     and remove the token from the internal representation.
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
C      TIME --- PRIVATE
C
C$ Declarations
C
C     CHARACTER*(1)         LETTER
C     INTEGER               B
C     INTEGER               E
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LETTER     I   a token to look for in the internal representation
C     B          O   is the beginning of the token
C     E          O   is the end of the token.
C
C     The function returns TRUE if the token is located.
C
C$ Detailed_Input
C
C     LETTER     is a token to look for and remove from the
C                current tokenization of a time string.
C
C                If located the token is removed from the string.
C
C                Note that this simply finds the first matching
C                token. If others are present they are not
C                affected.
C
C$ Detailed_Output
C
C     B          is the beginning of the requested token if it
C                was found. Otherwise B is zero.
C
C     E          is the ending of the requested token if it was
C                found. Otherwise E is zero.
C
C     The function returns the value TRUE if the token is located.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Look up and remove a token from the internal representation
C     of a time string. This is useful in removing modifiers
C     from a string (such as the ERA of an epoch, AM/PM of a time
C     etc.)
C
C$ Examples
C
C     See TPARTV
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
C
C$ Version
C
C-    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB)
C
C        Re-ordered header sections.
C
C-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
C
C        The main routine (which should never be called) now returns
C        the value .FALSE.
C
C-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
C
C        Added a RETURN statement at the end of the main routine.
C        Enhanced error message for the case when the input string
C        to ZZTOKNS has a non-printing character.
C
C-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
C
C
C-&


      PUT = INDEX ( REP, LETTER )

      IF ( PUT .GT. 0 ) THEN
         B = BEGS(PUT)
         E = ENDS(PUT)

         NEXT = PUT + 1

         DO GET = NEXT, SIZE

            BEGS (PUT)     = BEGS (GET)
            ENDS (PUT)     = ENDS (GET)
            PBEGS(PUT)     = PBEGS(GET)
            PENDS(PUT)     = PENDS(GET)
            REP  (PUT:PUT) = REP  (GET:GET)

            PUT           = PUT + 1
         END DO

         REP(SIZE:) = ' '
         SIZE       = SIZE - 1
         DID        = YES

      ELSE
         B          = 0
         E          = 0
         DID        = NO
      END IF

      ZZNOTE = DID
      RETURN




C$Procedure ZZREMT ( Private, Time --- remove token )

      ENTRY ZZREMT  ( LETTER )

C$ Abstract
C
C    SPICE Private routine intended solely for the support of SPICE
C    routines. Users should not call this routine directly due
C    to the volatile nature of this routine.
C
C    Remove a specified token from the internal representation
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
C     TIME --- Private
C
C
C$ Declarations
C
C     CHARACTER*(1)         LETTER
C
C$ Brief_I/O
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LETTER     I   token to remove from the internal representation.
C
C     The function returns TRUE if any tokens are removed.
C
C$ Detailed_Input
C
C     LETTER     is a token to be removed from the internal
C                representation of a tokenized time string.
C                All instances of LETTER will be removed from
C                the internal representation.
C
C$ Detailed_Output
C
C     The function returns TRUE if any instance of LETTER is removed
C     from the internal representation of a tokenized time string.
C     If no instances are removed the function returns FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is used to remove various delimiters that
C     appear in a tokenized time string (although it could be
C     used to remove any token from a tokenized time string).
C
C$ Examples
C
C     See TPARTV
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
C
C$ Version
C
C-    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB)
C
C        Re-ordered header sections.
C
C-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
C
C        The main routine (which should never be called) now returns
C        the value .FALSE.
C
C-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
C
C        Added a RETURN statement at the end of the main routine.
C        Enhanced error message for the case when the input string
C        to ZZTOKNS has a non-printing character.
C
C-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
C
C
C-&

      PUT = 0
      DID = NO

      DO I = 1, SIZE

         IF ( ICHAR(REP(I:I) ) .NE. ICHAR(LETTER) ) THEN
            PUT            = PUT + 1
            REP  (PUT:PUT) = REP  (I:I)
            BEGS (PUT)     = BEGS (I)
            ENDS (PUT)     = ENDS (I)
            PBEGS(PUT)     = PBEGS(I)
            PENDS(PUT)     = PENDS(I)
         ELSE
            DID = YES
         END IF

      END DO

      SIZE = PUT

      IF ( PUT .EQ. 0 ) THEN
         REP = ' '
      ELSE IF ( PUT .LT. LEN(REP) ) THEN
         REP(PUT+1:) = ' '
      END IF

      ZZREMT = DID
      RETURN




C$Procedure ZZSUBT ( Private, Time --- substitute tokens )

      ENTRY ZZSUBT  ( STRING, TRANSL, L2R )

C$ Abstract
C
C    SPICE Private routine intended solely for the support of SPICE
C    routines. Users should not call this routine directly due
C    to the volatile nature of this routine.
C
C    Substitute one token for another in the internal representation
C    of a tokenized time string.
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
C     TIME --- Private
C
C
C$ Declarations
C
C     IMPLICIT NONE
C     CHARACTER*(*)         STRING
C     CHARACTER*(*)         TRANSL
C     LOGICAL               L2R
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   token pattern to look for.
C     TRANSL     I   token replacement pattern.
C     L2R        I   direction to scan internal representation.
C
C     The function returns TRUE is a substitution is performed.
C
C$ Detailed_Input
C
C     STRING     is a string of tokens to look for in the internal
C                representation of a tokenized time string.
C
C                Only the first occurrence of STRING will be modified.
C
C                If the first character in STRING is '<', (and string
C                is more than 1 character in length) substitutions
C                will be performed in the4 tokenized string only if
C                STRING exactly matches the tokenized string
C                starting at the left most character.
C
C                If the last character in STRING is '>' (and string
C                is more than 1 character in length) substitutions
C                will be performed in the4 tokenized string only if
C                STRING exactly matches the tokenized string
C                ending at the right most character.
C
C                If first and last character of STRING are '<' and '>'
C                respectively, the first case above is applied and the
C                greater than character ('>') is regarded as just
C                another character.
C
C     TRANSL     is a sequence of replacement tokens to substitute
C                in place of STRING.
C
C     L2R        is a logical flag. If L2R is TRUE, the internal
C                representation is scanned from left to right. If
C                L2R is FALSE, the internal representation is scanned
C                from right to left.
C
C$ Detailed_Output
C
C     The function returns TRUE if a substitution is performed.
C     Otherwise it returns FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine searches for the first instance of a specified
C     pattern in the internal representation of a tokenized
C     time string. If the pattern is found, it is replaced
C     by that value of TRANSL. Only one pattern substitution
C     is performed per call to this function.
C
C$ Examples
C
C     See TPARTV
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
C
C$ Version
C
C-    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB)
C
C        Re-ordered header sections.
C
C-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
C
C        The main routine (which should never be called) now returns
C        the value .FALSE.
C
C-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
C
C        Added a RETURN statement at the end of the main routine.
C        Enhanced error message for the case when the input string
C        to ZZTOKNS has a non-printing character.
C
C-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
C
C
C-&
C
C     So far we haven't combined anything.
C
      DID = NO
      K   = LEN ( STRING )

C
C     We have two special cases to deal with.
C
      IF ( ICHAR(STRING(1:1)) .EQ. ICHAR('<') .AND. K .GT. 1 ) THEN

         TO   = MIN( K-1, SIZE )
         FROM = 1

         IF ( STRING(2:K) .EQ. REP(FROM:TO) ) THEN
            REP(FROM:TO) = TRANSL
            ZZSUBT       = YES
         ELSE
            ZZSUBT       = NO
         END IF

         RETURN

      ELSE IF ( ICHAR(STRING(K:K)) .EQ. ICHAR('>') .AND. K .GT. 1 ) THEN

         FROM = MAX(1,SIZE - K + 2)
         TO   = SIZE

         IF ( STRING(1:K-1) .EQ. REP(FROM:TO) ) THEN
            REP(FROM:TO) =  TRANSL
            ZZSUBT       = YES
         ELSE
            ZZSUBT       = NO
         END IF

         RETURN

      END IF


C
C     Look for the substring either looking from the
C     left (L2R is YES) or from the right (L2R is NO).
C
      IF ( L2R ) THEN
         FROM = POS  ( REP, STRING, 1 )
      ELSE
         FROM = POSR ( REP, STRING, SIZE )
      END IF

      TO   = FROM + LEN(TRANSL) - 1

      IF ( FROM .GT. 0 ) THEN
         DID          = YES
         REP(FROM:TO) = TRANSL
      END IF

      ZZSUBT = DID
      RETURN




C$Procedure ZZTOKNS ( Private, Time --- Time Tokens )

      ENTRY ZZTOKNS ( STRING, ERROR )

C$ Abstract
C
C    SPICE Private routine intended solely for the support of SPICE
C    routines. Users should not call this routine directly due
C    to the volatile nature of this routine.
C
C    Construct an internal tokenized representation of STRING.
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
C     TIME --- PRIVATE
C
C$ Declarations
C
C     IMPLICIT NONE
C     CHARACTER*(*)         STRING
C     CHARACTER*(*)         ERROR
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   A time string to be tokenized and internalized.
C     ERROR      O   A diagnostic message
C
C     The function returns TRUE is STRING can be tokenized.
C
C$ Detailed_Input
C
C     STRING     is a string that is intended to represent some
C                epoch and that needs parsing.
C
C$ Detailed_Output
C
C     ERROR      is a diagnostic message that is returned if a
C                problem occurs while trying to tokenize the
C                input time string. If no problems arise, ERROR
C                will be returned as a blank.
C
C     The function returns TRUE if the input string can be successfully
C     tokenized. If a problem arises, the function returns FALSE
C     and diagnostic is returned in ERROR.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This is the first step in parsing a time string. The
C     string is examined for integers, month, weekdays, time systems
C     time zones, eras, am/pm and various separators. This
C     representation is maintained and manipulated by the
C     companion entry points in ZZTIME.
C
C     The various recognized tokens represented by this routine
C     are:
C
C        '    --- the quote character (year abbreviation)
C        ,    --- a comma  (delimiter)
C        -    --- a dash   (delimiter)
C        .    --- a period (delimiter)
C        /    --- a slash  (delimiter)
C        :    --- a colon  (delimiter)
C        N    --- AM/PM marker
C        O    --- UTC+  marker
C        Z    --- US Time Zone Marker
C        [    --- left parenthesis marker
C        ]    --- right parenthesis marker
C        b    --- stands for blanks, or tabs (delimiter)
C        d    --- day of year marker (delimiter)
C        e    --- era marker
C        j    --- Julian date system marker
C        m    --- month marker
C        o    --- UTC- marker
C        s    --- time system marker
C        t    --- the "T" marker used in ISO formats.
C        w    --- the weekday marker
C        i    --- unsigned integer marker
C
C     Using the other entry points in ZZTIME, these markers are
C     gradually removed and transformed to more meaningful markers.
C
C$ Examples
C
C     See TPARTV
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
C     N.J. Bachman    (JPL)
C     B.V. Semenov    (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.6.0, 05-JAN-2014 (NJB)
C
C        BUG FIX: added checks for token indexes overflowing the
C        maximum number of tokens and for the character positions in
C        the time picture overflowing the time picture length. Both
C        overflows previously resulted in segmentation faults for
C        invalid input time strings that contained too many
C        recognizable tokens or were too long and required too many
C        characters in the picture representation.
C
C-    SPICELIB Version 1.5.0, 08-MAR-2009 (NJB)
C
C        Bug fix: changed upper bound used to detect
C        non-printing characters from 128 to 126.
C
C        Re-ordered header sections.
C
C-    SPICELIB Version 1.3.0, 27-OCT-2006 (BVS)
C
C        Fixed the bug that in the case of a one character long blank
C        input time string caused the TO variable be set to the value
C        greater than the string length, triggering an OUT OF BOUNDS
C        runtime error on HP. Added a separate up-front check for the
C        blank input string.
C
C-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
C
C        The main routine (which should never be called) now returns
C        the value .FALSE.
C
C-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
C
C        Added a RETURN statement at the end of the main routine.
C        Enhanced error message for the case when the input string
C        to ZZTOKNS has a non-printing character.
C
C-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
C
C
C-&

C
C     The first time in this routine we initialize our "tokenizing"
C     table.
C
      ZZTOKNS = NO

      IF ( FIRST ) THEN

         FIRST = NO
         BLANK = ICHAR ( ' ' )

C
C        These are the error message templates for errors generated
C        for input time strings that have too many recognizable tokens
C        or are too long for their pictures to fit in the internal
C        picture buffer.
C
         TKNERR = 'The input time string ''#'' cannot be processed ' //
     .            'because it contains more than @ recognizable ' //
     .            'tokens. The token that could not be processed ' //
     .            'was ''#''.'
         CALL REPMI ( TKNERR, '@', MAXTKN, TKNERR )

         PICERR = 'The input time string ''#'' cannot be processed '//
     .            'because the internal picture describing it ' //
     .            'requires more than @ characters. The token that ' //
     .            'could not be processed was ''#''.'
         CALL REPMI ( PICERR, '@', PICLEN, PICERR )
         
C
C        Below is the list of recognized substrings. The basic
C        pattern here is to find the block of special tokens
C        that begin with a particular character. Insert into
C        that block the lines of code below
C
C        I              =  I + 1
C        F( ICHAR('letter')) =  I
C        RECOG(I)       = 'the full substring that's recognized '
C        WIDTH(I)       =  number of characters required to match
C        CLASS(I)       = 'the classification of this substring'
C        L( ICHAR('b')) =  I
C
C        Note matching is performed from the first string in the
C        group to the last.
C
C

         DO I = 32, 126
            F(I)     =  0
            L(I)     = -1
            NAMES(I) = 'substring'
         END DO


         NAMES( ICHAR('''') ) = '"Year Abbreviation Mark"'
         NAMES( ICHAR(',' ) ) = 'comma'
         NAMES( ICHAR('-' ) ) = 'dash'
         NAMES( ICHAR('.' ) ) = 'period'
         NAMES( ICHAR('/' ) ) = 'slash'
         NAMES( ICHAR(':' ) ) = 'colon'
         NAMES( ICHAR('D' ) ) = 'Day of Month'
         NAMES( ICHAR('H' ) ) = 'Hour'
         NAMES( ICHAR('M' ) ) = 'Minute'
         NAMES( ICHAR('N' ) ) = 'AM/PM indicator'
         NAMES( ICHAR('O' ) ) = 'UTC-Offset indicator'
         NAMES( ICHAR('S' ) ) = 'Second'
         NAMES( ICHAR('Y' ) ) = 'Year'
         NAMES( ICHAR('Z' ) ) = 'Time-Zone indicator'
         NAMES( ICHAR('[' ) ) = 'Left Parenthesis'
         NAMES( ICHAR(']' ) ) = 'Right Parenthesis'
         NAMES( ICHAR('b' ) ) = 'White Space'
         NAMES( ICHAR('d' ) ) = 'Day-of-Year indicator'
         NAMES( ICHAR('e' ) ) = 'Era'
         NAMES( ICHAR('i' ) ) = 'Integer'
         NAMES( ICHAR('j' ) ) = 'Julian Date indicator'
         NAMES( ICHAR('m' ) ) = 'Month'
         NAMES( ICHAR('n' ) ) = 'Decimal Number'
         NAMES( ICHAR('o' ) ) = 'UTC-Offset indicator'
         NAMES( ICHAR('s' ) ) = 'Time System specification'
         NAMES( ICHAR('t' ) ) = 'ISO Time Separator'
         NAMES( ICHAR('w' ) ) = 'Weekday'
         NAMES( ICHAR('y' ) ) = 'Day of Year'

         MNMRK ( LOWER, FULL  ) = 'month'
         MNMRK ( UPPER, FULL  ) = 'MONTH'
         MNMRK ( MIXED, FULL  ) = 'Month'
         MNMRK ( LOWER, SHORT ) = 'mon'
         MNMRK ( UPPER, SHORT ) = 'MON'
         MNMRK ( MIXED, SHORT ) = 'Mon'

         WKDAY ( LOWER, FULL  ) = 'weekday'
         WKDAY ( UPPER, FULL  ) = 'WEEKDAY'
         WKDAY ( MIXED, FULL  ) = 'Weekday'
         WKDAY ( LOWER, SHORT ) = 'wkd'
         WKDAY ( UPPER, SHORT ) = 'WKD'
         WKDAY ( MIXED, SHORT ) = 'Wkd'
C
C        Length of the items Month, Mon, weekday, wkd
C
         WKSIZE ( FULL  ) = 7
         WKSIZE ( SHORT ) = 3
         MNSIZE ( FULL  ) = 5
         MNSIZE ( SHORT ) = 3

         I = 0
C
C        Tokens beginning with ' '
C
         I              =  I + 1
         F( ICHAR(' ')) =  I
         RECOG(I)       = ' '
         WIDTH(I)       =  1
         CLASS(I)       = 'b'
         L( ICHAR(' ')) =  I

C
C        Tokens beginning with '('
C
         I              =  I + 1
         F( ICHAR('(')) =  I
         RECOG(I)       = '('
         WIDTH(I)       =  1
         CLASS(I)       = '['
         L( ICHAR('(')) =  I
C
C        Tokens beginning with ')'
C
         I              =  I + 1
         F( ICHAR(')')) =  I
         RECOG(I)       = ')'
         WIDTH(I)       =  1
         CLASS(I)       = ']'
         L( ICHAR(')')) =  I
C
C        Tokens beginning with ','
C
         I              =  I + 1
         F( ICHAR(',')) =  I
         RECOG(I)       = ','
         WIDTH(I)       =  1
         CLASS(I)       = ','
         L( ICHAR(',')) =  I

C
C        Tokens beginning with '-'
C
         I              =  I + 1
         F( ICHAR('-')) =  I
         RECOG(I)       = '-'
         WIDTH(I)       =  1
         CLASS(I)       = '-'
         L( ICHAR('-')) =  I

C
C        Tokens beginning with '.'
C
         I              =  I + 1
         F( ICHAR('.')) =  I
         RECOG(I)       = '.'
         WIDTH(I)       =  1
         CLASS(I)       = '.'
         L( ICHAR('.')) =  I

C
C        Tokens beginning with '/'
C
         I              =  I + 1
         F( ICHAR('/')) =  I
         RECOG(I)       = '//'
         WIDTH(I)       =  2
         CLASS(I)       = 'd'
         L( ICHAR('/')) =  I

         I              =  I + 1
         RECOG(I)       = '/'
         WIDTH(I)       =  1
         CLASS(I)       = '/'
         L( ICHAR('/')) =  I
C
C        Tokens beginning with ':'
C
         I              =  I + 1
         F( ICHAR(':')) =  I
         RECOG(I)       = '::'
         WIDTH(I)       =  2
         CLASS(I)       = 'd'
         L( ICHAR(':')) =  I

         I              =  I + 1
         RECOG(I)       = ':'
         WIDTH(I)       =  1
         CLASS(I)       = ':'
         L( ICHAR(':')) =  I

C
C        Tokens beginning with 'A'
C
         I              =  I + 1
         F( ICHAR('A')) =  I
         RECOG(I)       = 'A.D.'
         WIDTH(I)       =  4
         CLASS(I)       = 'e'
         L( ICHAR('A')) =  I

         I              =  I + 1
         RECOG(I)       = 'AD'
         WIDTH(I)       =  2
         CLASS(I)       = 'e'
         L( ICHAR('A')) =  I

         I              =  I + 1
         RECOG(I)       = 'A.M.'
         WIDTH(I)       =  4
         CLASS(I)       = 'N'
         L( ICHAR('A')) =  I

         I              =  I + 1
         RECOG(I)       = 'AM'
         WIDTH(I)       =  2
         CLASS(I)       = 'N'
         L( ICHAR('A')) =  I

         I              =  I + 1
         RECOG(I)       = 'APRIL'
         WIDTH(I)       =  3
         CLASS(I)       = 'm'
         L( ICHAR('A')) =  I

         I              =  I + 1
         RECOG(I)       = 'AUGUST'
         WIDTH(I)       =  3
         CLASS(I)       = 'm'
         L( ICHAR('A')) =  I

C
C        Tokens beginning with 'B'
C
         I              =  I + 1
         F( ICHAR('B')) =  I
         RECOG(I)       = 'B.C.'
         WIDTH(I)       =  4
         CLASS(I)       = 'e'
         L( ICHAR('B')) =  I

         I              =  I + 1
         RECOG(I)       = 'BC'
         WIDTH(I)       =  2
         CLASS(I)       = 'e'
         L( ICHAR('B')) =  I

C
C        Tokens beginning with 'C'
C
         I              =  I + 1
         F( ICHAR('C')) =  I
         RECOG(I)       = 'CDT'
         WIDTH(I)       =  3
         CLASS(I)       = 'Z'
         L( ICHAR('C')) =  I

         I              =  I + 1
         RECOG(I)       = 'CST'
         WIDTH(I)       =  3
         CLASS(I)       = 'Z'
         L( ICHAR('C')) =  I

C
C        Tokens beginning with 'D'
C
         I              =  I + 1
         F( ICHAR('D')) =  I
         RECOG(I)       = 'DECEMBER'
         WIDTH(I)       =  3
         CLASS(I)       = 'm'
         L( ICHAR('D')) =  I

         I              =  I + 1
         RECOG(I)       = 'D+'
         WIDTH(I)       =  2
         CLASS(I)       = 'E'
         L( ICHAR('D')) =  I

         I              =  I + 1
         RECOG(I)       = 'D-'
         WIDTH(I)       =  2
         CLASS(I)       = 'E'
         L( ICHAR('D')) =  I

         I              =  I + 1
         RECOG(I)       = 'D'
         WIDTH(I)       =  1
         CLASS(I)       = 'E'
         L( ICHAR('D')) =  I




C
C        Tokens beginning with 'E'
C
         I              =  I + 1
         F( ICHAR('E')) =  I
         RECOG(I)       = 'EDT'
         WIDTH(I)       =  3
         CLASS(I)       = 'Z'
         L( ICHAR('E')) =  I

         I              =  I + 1
         RECOG(I)       = 'EST'
         WIDTH(I)       =  3
         CLASS(I)       = 'Z'
         L( ICHAR('E')) =  I

         I              =  I + 1
         RECOG(I)       = 'E+'
         WIDTH(I)       =  2
         CLASS(I)       = 'E'
         L( ICHAR('E')) =  I

         I              =  I + 1
         RECOG(I)       = 'E-'
         WIDTH(I)       =  2
         CLASS(I)       = 'E'
         L( ICHAR('E')) =  I

         I              =  I + 1
         RECOG(I)       = 'E'
         WIDTH(I)       =  1
         CLASS(I)       = 'E'
         L( ICHAR('E')) =  I


C
C        Tokens beginning with 'F'
C
         I              =  I + 1
         F( ICHAR('F')) =  I
         RECOG(I)       = 'FEBRUARY'
         WIDTH(I)       =  3
         CLASS(I)       = 'm'
         L( ICHAR('F')) =  I

         I              =  I + 1
         RECOG(I)       = 'FRIDAY'
         WIDTH(I)       =  3
         CLASS(I)       = 'w'
         L( ICHAR('F')) =  I

C
C        Tokens beginning with 'J'
C
         I              =  I + 1
         F( ICHAR('J')) =  I
         RECOG(I)       = 'JANUARY'
         WIDTH(I)       =  3
         CLASS(I)       = 'm'
         L( ICHAR('J')) =  I

         I              =  I + 1
         RECOG(I)       = 'JD'
         WIDTH(I)       =  2
         CLASS(I)       = 'j'
         L( ICHAR('J')) =  I

         I              =  I + 1
         RECOG(I)       = 'JULY'
         WIDTH(I)       =  3
         CLASS(I)       = 'm'
         L( ICHAR('J')) =  I

         I              =  I + 1
         RECOG(I)       = 'JUNE'
         WIDTH(I)       =  3
         CLASS(I)       = 'm'
         L( ICHAR('J')) =  I

C
C        Tokens beginning with 'M'
C
         I              =  I + 1
         F( ICHAR('M')) =  I
         RECOG(I)       = 'MARCH'
         WIDTH(I)       =  3
         CLASS(I)       = 'm'
         L( ICHAR('M')) =  I

         I              =  I + 1
         RECOG(I)       = 'MAY'
         WIDTH(I)       =  3
         CLASS(I)       = 'm'
         L( ICHAR('M')) =  I

         I              =  I + 1
         RECOG(I)       = 'MDT'
         WIDTH(I)       =  3
         CLASS(I)       = 'Z'
         L( ICHAR('M')) =  I

         I              =  I + 1
         RECOG(I)       = 'MONDAY'
         WIDTH(I)       =  3
         CLASS(I)       = 'w'
         L( ICHAR('M')) =  I

         I              =  I + 1
         RECOG(I)       = 'MST'
         WIDTH(I)       =  3
         CLASS(I)       = 'Z'
         L( ICHAR('M')) =  I

C
C        Tokens beginning with 'N'
C
         I              =  I + 1
         F( ICHAR('N')) =  I
         RECOG(I)       = 'NOVEMBER'
         WIDTH(I)       =  3
         CLASS(I)       = 'm'
         L( ICHAR('N')) =  I

C
C        Tokens beginning with 'O'
C
         I              =  I + 1
         F( ICHAR('O')) =  I
         RECOG(I)       = 'OCTOBER'
         WIDTH(I)       =  3
         CLASS(I)       = 'm'
         L( ICHAR('O')) =  I

C
C        Tokens beginning with 'P'
C
         I              =  I + 1
         F( ICHAR('P')) =  I
         RECOG(I)       = 'P.M.'
         WIDTH(I)       =  4
         CLASS(I)       = 'N'
         L( ICHAR('P')) =  I

         I              =  I + 1
         RECOG(I)       = 'PDT'
         WIDTH(I)       =  3
         CLASS(I)       = 'Z'
         L( ICHAR('P')) =  I

         I              =  I + 1
         RECOG(I)       = 'PM'
         WIDTH(I)       =  2
         CLASS(I)       = 'N'
         L( ICHAR('P')) =  I

         I              =  I + 1
         RECOG(I)       = 'PST'
         WIDTH(I)       =  3
         CLASS(I)       = 'Z'
         L( ICHAR('P')) =  I

C
C        Tokens beginning with 'S'
C
         I              =  I + 1
         F( ICHAR('S')) =  I
         RECOG(I)       = 'SATURDAY'
         WIDTH(I)       =  3
         CLASS(I)       = 'w'
         L( ICHAR('S')) =  I

         I              =  I + 1
         RECOG(I)       = 'SEPTEMBER'
         WIDTH(I)       =  3
         CLASS(I)       = 'm'
         L( ICHAR('S')) =  I

         I              =  I + 1
         RECOG(I)       = 'SUNDAY'
         WIDTH(I)       =  3
         CLASS(I)       = 'w'
         L( ICHAR('S')) =  I

C
C        Tokens beginning with 'T'
C
         I              =  I + 1
         F( ICHAR('T')) =  I
         RECOG(I)       = 'TDB'
         WIDTH(I)       =  3
         CLASS(I)       = 's'
         L( ICHAR('T')) =  I

         I              =  I + 1
         RECOG(I)       = 'TDT'
         WIDTH(I)       =  3
         CLASS(I)       = 's'
         L( ICHAR('T')) =  I

         I              =  I + 1
         RECOG(I)       = 'THURSDAY'
         WIDTH(I)       =  3
         CLASS(I)       = 'w'
         L( ICHAR('T')) =  I

         I              =  I + 1
         RECOG(I)       = 'TUESDAY'
         WIDTH(I)       =  3
         CLASS(I)       = 'w'
         L( ICHAR('T')) =  I

         I              =  I + 1
         RECOG(I)       = 'T'
         WIDTH(I)       =  1
         CLASS(I)       = 't'
         L( ICHAR('T')) =  I

C
C        Tokens beginning with 'U'
C
         I              =  I + 1
         F( ICHAR('U')) =  I
         RECOG(I)       = 'UTC+'
         WIDTH(I)       =  4
         CLASS(I)       = 'O'
         L( ICHAR('U')) =  I

         I              =  I + 1
         RECOG(I)       = 'UTC-'
         WIDTH(I)       =  4
         CLASS(I)       = 'o'
         L( ICHAR('U')) =  I

         I              =  I + 1
         RECOG(I)       = 'UTC'
         WIDTH(I)       =  3
         CLASS(I)       = 's'
         L( ICHAR('U')) =  I
C
C        Tokens beginning with ''''
C
         I              =  I + 1
         F( ICHAR('''')) =  I
         RECOG(I)       = ''''
         WIDTH(I)       =  1
         CLASS(I)       = ''''
         L( ICHAR('''')) =  I
C
C        Tokens beginning with 'W'
C
         I              =  I + 1
         F( ICHAR('W')) =  I
         RECOG(I)       = 'WEDNESDAY'
         WIDTH(I)       =  3
         CLASS(I)       = 'w'
         L( ICHAR('W')) =  I

      END IF

C
C     If the input string is blank, return with an error message.
C
      IF ( STRING .EQ. ' ' ) THEN
         ERROR = 'The input time string is blank.'
         ZZTOKNS = NO
         RETURN
      END IF

C
C     OK. Initializations are out of the way. We now take
C     apart the string.
C
      DID    = NO
      ERROR  = ' '
      REP    = ' '
      PICTUR = ' '
      SIZE   = 0
      NEXT   = 1
      PNEXT  = 1
      PUT    = 0
      AMPM   = .FALSE.
      LAST   = RTRIM ( STRING )

      DO WHILE ( NEXT .LE. LAST )
C
C        FROM and NEXT point to parts of the string, PFROM and PNEXT
C        point to parts of the picture we will construct.
C
         FROM  = NEXT
         PFROM = PNEXT
         ITEM  = ICHAR( STRING(NEXT:NEXT) )
C
C        First we try to find an unsigned integer in the string.
C
         CALL LX4UNS ( STRING(1:LAST), FROM, TO, NCHAR )

         IF ( NCHAR .GT. 0 ) THEN
C
C           We found an unsigned integer, add a letter to the
C           internal representation, note the begin and end
C           of the token and set NEXT to the first character
C           beyond this token.
C
            PUT               =  PUT + 1

            IF ( PUT .GT. MAXTKN ) THEN
               CALL ZZTKNERR ( TKNERR, STRING, STRING(FROM:TO), 
     .                         ERROR, ZZTOKNS )
               RETURN
            END IF

            REP (PUT:PUT)     = 'i'
            BEGS(PUT)         =  FROM
            ENDS(PUT)         =  TO
            NEXT              =  TO    + 1
            PTO               =  PFROM + NCHAR - 1

            IF ( PTO .GT. PICLEN ) THEN
               CALL ZZTKNERR ( PICERR, STRING, STRING(FROM:TO), 
     .                         ERROR, ZZTOKNS )
               RETURN
            END IF

            PNEXT             =  PTO   + 1
            PICTUR(PFROM:PTO) =  STRING(FROM:TO)
            PBEGS(PUT)        =  PFROM
            PENDS(PUT)        =  PTO

         ELSE IF ( ITEM .EQ. BLANK  ) THEN
C
C           We have a blank. We lump all consecutive
C           blanks together as one big fat blank.
C
            PUT           =  PUT + 1

            IF ( PUT .GT. MAXTKN ) THEN
               CALL ZZTKNERR ( TKNERR, STRING, ' ', ERROR, ZZTOKNS )
               RETURN
            END IF

            TO            =  FROM
            BEGS(PUT)     =  FROM
            REP (PUT:PUT) = 'b'

            DO WHILE ( ITEM .EQ. BLANK .AND. TO .LE. LAST )

               TO   = TO + 1
               IF ( TO .LE. LAST ) THEN
                  ITEM = ICHAR( STRING(TO:TO) )
               END IF

            END DO

            NEXT              = TO
            TO                = TO - 1
            ENDS(PUT)         = TO

            PTO               = PFROM + TO - FROM

            IF ( PTO .GT. PICLEN ) THEN
               CALL ZZTKNERR ( PICERR, STRING, ' ', ERROR, ZZTOKNS )
               RETURN
            END IF

            PNEXT             = PTO   + 1
            PICTUR(PFROM:PTO) = STRING(FROM:TO)
            PBEGS (PUT)       = PFROM
            PENDS (PUT)       = PTO

         ELSE IF ( ITEM .EQ. 9 ) THEN
C
C           We've got a tab character, we treat tabs as
C           blanks.
C
            PUT               = PUT + 1

            IF ( PUT .GT. MAXTKN ) THEN
               CALL ZZTKNERR ( TKNERR, STRING, '<TAB>', 
     .                         ERROR, ZZTOKNS )
               RETURN
            END IF

            REP(PUT:PUT)      = 'b'
            BEGS(PUT)         = FROM
            ENDS(PUT)         = FROM
            NEXT              = NEXT + 1

            PTO               = PFROM

            IF ( PTO .GT. PICLEN ) THEN
               CALL ZZTKNERR ( PICERR, STRING, '<TAB>', 
     .                         ERROR, ZZTOKNS )
               RETURN
            END IF

            PNEXT             = PTO   + 1
            PICTUR(PFROM:PTO) = ' '
            PBEGS (PUT)       = PFROM
            PENDS (PUT)       = PFROM

         ELSE IF (      ITEM .LT. 32
     .             .OR. ITEM .GT. 126 ) THEN
C
C           This is a non-printing character. This is
C           regarded as an error.
C
            ERROR = STRING

            CALL ZZINSSUB ( ERROR, '<', NEXT,   ERROR )

C
C           Overwrite the non-printing character with a
C           closing angle bracket.
C
            IF ( NEXT .LT. LEN(ERROR) ) THEN
               ERROR( NEXT+1 : NEXT+1 ) = '>'
            END IF

            CALL PREFIX ( 'There is a non-printing, non-tab '
     .      //            'character (ASCII #) at position # of the '
     .      //            'time string: ', 1, ERROR )

            CALL REPMI ( ERROR, '#', ITEM, ERROR )
            CALL REPMI ( ERROR, '#', NEXT, ERROR )

            ZZTOKNS = NO
            RETURN


         ELSE
C
C           This has to be one of the known types or we
C           have an unknown component in the string. We've constructed
C           a "parsing" table for handling these special cases.
C           This table uses the first letter of the string
C           to begin a search. We get that code and force it
C           into a suitable range.
C
            CALL     UCASE ( STRING(NEXT:NEXT), THIS )
            ITEM  =  ICHAR ( THIS )
            FROM  =  NEXT
            CHECK =  YES
            I     =  F(ITEM)

            DO WHILE ( CHECK .AND. I .LE. L(ITEM) )

               W    = WIDTH(I)
               TO   = FROM + W - 1

               GOT  = SAMSBI( STRING, FROM, TO, RECOG(I), 1, W )

               IF ( GOT ) THEN
C
C                 We have a match. If it is the match of a month
C                 or day of the week, we keep looking for the
C                 end of the match.
C
                  IF (      CLASS(I) .EQ. 'm'
     .                 .OR. CLASS(I) .EQ. 'w' ) THEN


                     SPCIAL = RECOG(I)
                     R      = RTRIM(SPCIAL)
                     W      = W+1
                     TO     = TO + 1

                     DO WHILE ( SAMCHI ( STRING, TO, SPCIAL(1:R), W ) )
                        W  = W  + 1
                        TO = TO + 1
                     END DO

                     TO        = TO - 1

                     IF ( W .GT. R ) THEN
                        KIND = FULL
                     ELSE
                        KIND = SHORT
                     END IF

                     IF ( THIS .NE. STRING(NEXT:NEXT) ) THEN
                        CASE = LOWER
                     ELSE IF (STRING(NEXT:NEXT+2) .EQ. SPCIAL(1:3))
     .               THEN
                        CASE = UPPER
                     ELSE
                        CASE = MIXED
                     END IF

                     IF ( CLASS(I) .EQ. 'm' ) THEN

                        PTO               = PFROM + MNSIZE(KIND) - 1

                        IF ( PTO .GT. PICLEN ) THEN
                           CALL ZZTKNERR ( PICERR, STRING, 
     .                              STRING(FROM:TO), ERROR, ZZTOKNS )
                           RETURN
                        END IF

                        PNEXT             = PTO   + 1
                        PICTUR(PFROM:PTO) = MNMRK(CASE,KIND)

                     ELSE

                        PTO               = PFROM + WKSIZE(KIND) - 1

                        IF ( PTO .GT. PICLEN ) THEN
                           CALL ZZTKNERR ( PICERR, STRING, 
     .                              STRING(FROM:TO), ERROR, ZZTOKNS )
                           RETURN
                        END IF

                        PNEXT             = PTO   + 1
                        PICTUR(PFROM:PTO) = WKDAY(CASE,KIND)

                     END IF

                  ELSE IF ( CLASS(I) .EQ. 'e' ) THEN

                     PTO               =  PFROM + 2

                     IF ( PTO .GT. PICLEN ) THEN
                        CALL ZZTKNERR ( PICERR, STRING, STRING(FROM:TO),
     .                                                  ERROR, ZZTOKNS )
                        RETURN
                     END IF

                     PNEXT             =  PTO   + 1

                     IF ( STRING(FROM:FROM) .EQ. THIS ) THEN
                        PICTUR(PFROM:PTO) = 'ERA'
                     ELSE
                        PICTUR(PFROM:PTO) = 'era'
                     END IF

                  ELSE IF ( CLASS(I) .EQ. 'N' ) THEN

                     PTO               =  PFROM + 3

                     IF ( PTO .GT. PICLEN ) THEN
                        CALL ZZTKNERR ( PICERR, STRING, STRING(FROM:TO),
     .                                                  ERROR, ZZTOKNS )
                        RETURN
                     END IF

                     PNEXT             =  PTO   + 1
                     IF ( STRING(FROM:FROM) .EQ. THIS ) THEN
                        PICTUR(PFROM:PTO) = 'AMPM'
                     ELSE
                        PICTUR(PFROM:PTO) = 'ampm'
                     END IF

                     AMPM = .TRUE.

                  ELSE

                     PTO               =  PFROM + TO - FROM

                     IF ( PTO .GT. PICLEN ) THEN
                        CALL ZZTKNERR ( PICERR, STRING, STRING(FROM:TO),
     .                                                  ERROR, ZZTOKNS )
                        RETURN
                     END IF

                     PNEXT             =  PTO   + 1
                     PICTUR(PFROM:PTO) =  STRING(FROM:TO )

                  END IF

                  PUT           = PUT + 1

                  IF ( PUT .GT. MAXTKN ) THEN
                     CALL ZZTKNERR ( TKNERR, STRING, STRING(FROM:TO), 
     .                               ERROR, ZZTOKNS )
                     RETURN
                  END IF

                  REP (PUT:PUT) = CLASS(I)(1:1)
                  BEGS(PUT)     = FROM
                  ENDS(PUT)     = TO
                  PBEGS(PUT)    = PFROM
                  PENDS(PUT)    = PTO
                  CHECK         = NO
                  NEXT          = TO + 1


               END IF

               I = I + 1

            END DO
C
C           If we reach the end of the loop and CHECK is still
C           set to TRUE, we have a bit of unrecognizable string.
C
            IF ( CHECK ) THEN

               ERROR = STRING

               CALL ZZINSSUB ( ERROR, '>', FROM + 1, ERROR )
               CALL ZZINSSUB ( ERROR, '<', FROM,     ERROR )

               CALL PREFIX ( 'The input string contains an '
     .         //            'unrecognizable substring beginning '
     .         //            'at the character marked by <#>: "',
     .                            0, ERROR )
               CALL SUFFIX ( '"', 0, ERROR )

               CALL REPMC ( ERROR, '#', STRING(FROM:FROM), ERROR )
               ZZTOKNS = NO
               RETURN

            END IF

         END IF

      END DO


      SIZE    = PUT
      ZZTOKNS = YES
      RETURN




C$Procedure ZZUNPCK ( Private, Time --- Unpack a time string )

      ENTRY ZZUNPCK ( STRING, YABBRV, TVEC,  E, TRANSL, PIC, ERROR )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Unpack the time string and parse its components using the
C     stored internal representation.
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
C      TIME --- PRIVATE
C
C$ Declarations
C
C     IMPLICIT NONE
C     CHARACTER*(*)         STRING
C     LOGICAL               YABBRV
C     DOUBLE PRECISION      TVEC ( * )
C     INTEGER               E
C     CHARACTER*(*)         TRANSL
C     CHARACTER*(*)         ERROR
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   is a time string that has been tokenized.
C     YABBRV     I   has the year been abbreviated.
C     TVEC       O   is a vector of time components
C     E          O   is the actual number of components present
C     TRANSL     O   is the type TVEC ( YMD or YD )
C     PIC        O   is a picture of the format used for the time string
C     ERROR      O   a diagnostic of any problems
C
C     The function returns TRUE if the string was unpacked completely.
C
C$ Detailed_Input
C
C     STRING     is the original string from which the current
C                internal representation was derived.
C
C     YABBRV     is a logical that indicates whether or not an
C                abbreviated year was encountered in the string.
C                YABBRV is TRUE if such an abbreviation was present
C                otherwise it is FALSE.
C
C$ Detailed_Output
C
C     TVEC       is a double precision array of the parsed time
C                components. TVEC will have either 5 or 6 values
C                depending upon whether the string is Year, Month,
C                and Day of Month, or Year and Day of Year.
C
C     E          is the actual number of components that were
C                present in the internal representation.
C
C                If STRING cannot be fully resolved, E is returned
C                as a zero.
C
C     TRANSL     is the type of time vector. The value will be
C                'YD' (day of year) or 'YMD' (Year, Month, Day).
C
C                If STRING cannot be fully resolved, TRANSL is
C                returned as a blank.
C
C     PIC        is a picture of the time format corresponding the
C                the time string in the last call to ZZTOKNS.
C
C                If some part of the input string can't be identified
C                PIC is returned as a blank. Note that there is a
C                distinction between recognizable and parsable.
C                The input string must be unambiguous to be parsable,
C                However, even if a string is ambiguous it may
C                correspond to a legitimate format picture. Since
C                occasionally, that's what you want (an ambiguous
C                format), we allow it in PIC.
C
C     ERROR      is a diagnostic that indicates some problem in
C                resolving STRING. If no problems occur ERROR
C                is returned as a blank.
C
C     The function returns TRUE if STRING was successfully unpacked.
C     That is the string is parsed and is unambiguously recognized.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is the last routine that will normally be
C     called by a time parsing routine. This call should be
C     made after all combinations, replacements and removals
C     that make sense to perform have been made.
C
C$ Examples
C
C     See TPARTV
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
C
C$ Version
C
C-    SPICELIB Version 1.3.0, 29-APR-2013 (EDW)
C
C        Added error check on ITEM value. Failure to perform
C        this check can cause BADSUBSCRIPT error signals
C        from CSPICE code on invalid time strings.
C
C-    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB)
C
C        Re-ordered header sections.
C
C-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
C
C        The main routine (which should never be called) now returns
C        the value .FALSE.
C
C-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
C
C        Added a RETURN statement at the end of the main routine.
C        Enhanced error message for the case when the input string
C        to ZZTOKNS has a non-printing character.
C
C-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
C
C
C-&


      NYEAR  = 0
      NMON   = 0
      NDAY   = 0
      NHOUR  = 0
      NMIN   = 0
      NSEC   = 0
      NDOY   = 0
      NJD    = 0
      E      = 0
      TRANSL = ' '

      HMS(1) = 0.0D0
      HMS(2) = 0.0D0
      HMS(3) = 0.0D0

      DO I = SIZE, 1, -1

         ITEM = ICHAR( REP(I:I) )
         
C
C        Confirm ITEM range [32,126].
C
         IF (      ITEM .LT. 32
     .        .OR. ITEM .GT. 126 ) THEN

C
C           A non-printing character found in REP. This is
C           an error.
C
            ERROR = 'A character at location #1 does not '
     .   //         'have ASCII value [32,126] for REP string.'

            CALL REPMI ( ERROR, '#1', I,   ERROR )

C
C           Error condition, return.
C
            ZZUNPCK = NO
            RETURN

         END IF

         J    = BEGS(I)
         K    = ENDS(I)

         IF      ( ITEM .EQ. ICHAR('Y') ) THEN

            NYEAR = NYEAR + 1
            E     = E     + 1
            CALL NPARSD   ( STRING(J:K), TVEC(1), ERROR, PTR )

            IF ( YABBRV ) THEN
               CALL ZZREPSUB ( PICTUR, PBEGS(I), PENDS(I),
     .                        'YR', PICTUR )
            ELSE
               CALL ZZREPSUB ( PICTUR, PBEGS(I), PENDS(I),
     .                        'YYYY', PICTUR )
            END IF

         ELSE IF ( ITEM .EQ. ICHAR('m') ) THEN

            NMON  = NMON  + 1
            E     = E     + 1

            CALL UCASE ( STRING(J:K), MONTH )
            VALUE = ISRCHC ( MONTH, 12, MONTHS )

            IF ( VALUE .EQ. 0 ) THEN
               CALL NPARSD  ( STRING(J:K), TVEC(2), ERROR, PTR )
               CALL ZZREPSUB( PICTUR, PBEGS(I), PENDS(I), 'MM', PICTUR )
            ELSE
               TVEC(2) = DBLE( VALUE )
            END IF


         ELSE IF ( ITEM .EQ. ICHAR('D') ) THEN

            NDAY  = NDAY  + 1
            E     = E     + 1

            CALL NPARSD ( STRING(J:K), TVEC(3), ERROR, PTR  )

            CALL ZZMKPC ( PICTUR, PBEGS(I), PENDS(I), 'DD',
     .                    STRING(J:K) )

         ELSE IF ( ITEM .EQ. ICHAR('y') ) THEN

            NDOY  = NDOY  + 1
            E     = E     + 1

            CALL NPARSD ( STRING(J:K), TVEC(2), ERROR, PTR  )

            CALL ZZMKPC ( PICTUR, PBEGS(I), PENDS(I), 'DOY',
     .                    STRING(J:K) )

         ELSE IF ( ITEM .EQ. ICHAR('H') ) THEN

            NHOUR = NHOUR + 1
            E     = E     + 1

            CALL NPARSD ( STRING(J:K), HMS(1), ERROR, PTR  )
C
C           We have to handle the hour component based on the
C           presence of the AM/PM mark in the picture. We earlier
C           set up the logical AMPM to indicate its presence.
C
            IF (  AMPM ) THEN

               CALL ZZMKPC ( PICTUR, PBEGS(I), PENDS(I), 'AP',
     .                       STRING(J:K) )

            ELSE

               CALL ZZMKPC ( PICTUR, PBEGS(I), PENDS(I), 'HR',
     .                       STRING(J:K) )

            END IF

         ELSE IF ( ITEM .EQ. ICHAR('M') ) THEN

            NMIN  = NMIN  + 1
            E     = E     + 1

            CALL NPARSD ( STRING(J:K), HMS(2), ERROR, PTR  )

            CALL ZZMKPC ( PICTUR, PBEGS(I), PENDS(I), 'MN',
     .                    STRING(J:K) )

         ELSE IF ( ITEM .EQ. ICHAR('S') ) THEN

            NSEC  = NSEC  + 1
            E     = E     + 1

            CALL NPARSD ( STRING(J:K), HMS(3), ERROR, PTR  )

            CALL ZZMKPC ( PICTUR, PBEGS(I), PENDS(I), 'SC',
     .                    STRING(J:K) )

         ELSE IF ( ITEM .EQ. ICHAR('J') ) THEN

            NJD   = NJD   + 1
            E     = E     + 1

            CALL NPARSD ( STRING(J:K), TVEC(1), ERROR, PTR  )

            CALL ZZMKPC ( PICTUR, PBEGS(I), PENDS(I), 'JULIAND',
     .                    STRING(J:K) )

         ELSE IF ( ITEM .EQ. ICHAR('i') ) THEN

            ERROR = STRING

            CALL ZZINSSUB ( ERROR, '>', K+1,  ERROR )
            CALL ZZINSSUB ( ERROR, '<', J,    ERROR )

            CALL PREFIX ( 'The meaning of the integer <#> could '
     .      //            'not be determined: ''', 1, ERROR )
            CALL SUFFIX ( '''', 0, ERROR )

            CALL REPMC ( ERROR, '#', STRING(J:K), ERROR )

            E       = 0
            PIC     = ' '
            ZZUNPCK = NO
            RETURN

         ELSE IF ( ITEM .EQ. ICHAR('n') ) THEN

            ERROR = STRING

            CALL ZZINSSUB ( ERROR, '>', K+1,  ERROR )
            CALL ZZINSSUB ( ERROR, '<',  J,   ERROR )

            CALL PREFIX ( 'The meaning of the decimal number <#> '
     .      //            'could not be determined: ', 1, ERROR )

            CALL REPMC ( ERROR, '#', STRING(J:K), ERROR )

            E       = 0
            PIC     = ' '
            ZZUNPCK = NO
            RETURN


         ELSE

            ERROR = STRING

            CALL ZZINSSUB ( ERROR, '>', K+1,  ERROR )
            CALL ZZINSSUB ( ERROR, '<',  J,   ERROR )

            CALL PREFIX ( 'An unexpected # ("#") was encountered '
     .      //            'in the time string: ', 1, ERROR )
            CALL REPMC ( ERROR, '#', NAMES(ITEM),    ERROR )
            CALL REPMC ( ERROR, '#', STRING(J:K),    ERROR )

            PIC     = ' '
            E       = 0
            ZZUNPCK = NO
            RETURN

         END IF


      END DO

C
C     Ok. Check the counts of substrings to make sure everything
C     looks ok. If so move the HMS into the appropriate slots
C     in TVEC, set the kind of TVEC, set the function value to YES,
C     and RETURN. Note regardless of the correctness of the parsing
C     we have a legitimate format picture at this point so we keep it.
C
      PIC = PICTUR

      IF (       NYEAR .EQ. 1
     .     .AND. NMON  .EQ. 1
     .     .AND. NDAY  .EQ. 1
     .     .AND. NDOY  .EQ. 0
     .     .AND. NJD   .EQ. 0
     .     .AND. NHOUR .LE. 1
     .     .AND. NMIN  .LE. NHOUR
     .     .AND. NSEC  .LE. NMIN ) THEN

          TVEC(4) = HMS(1)
          TVEC(5) = HMS(2)
          TVEC(6) = HMS(3)

          TRANSL  = 'YMD'
          ZZUNPCK =  YES
          RETURN

      ELSE IF (      NYEAR .EQ. 1
     .         .AND. NMON  .EQ. 0
     .         .AND. NDAY  .EQ. 0
     .         .AND. NJD   .EQ. 0
     .         .AND. NDOY  .EQ. 1
     .         .AND. NHOUR .LE. 1
     .         .AND. NMIN  .LE. NHOUR
     .         .AND. NSEC  .LE. NMIN  ) THEN

          TVEC(3) = HMS(1)
          TVEC(4) = HMS(2)
          TVEC(5) = HMS(3)

          TRANSL  = 'YD'
          ZZUNPCK =  YES
          RETURN

      ELSE IF (      NYEAR .EQ. 0
     .         .AND. NMON  .EQ. 0
     .         .AND. NDAY  .EQ. 0
     .         .AND. NJD   .EQ. 1
     .         .AND. NDOY  .EQ. 0
     .         .AND. NHOUR .LE. 0
     .         .AND. NMIN  .LE. 0
     .         .AND. NSEC  .LE. 0  ) THEN

          TRANSL  = 'JD'
          ZZUNPCK =  YES
          RETURN

      END IF
C
C     If we're still here, there is some kind of an error
C     in the input string. There are a lot of possible
C     problems.
C
      E = 0

      IF (           NYEAR .EQ. 0
     .         .AND. NDAY  .EQ. 0
     .         .AND. NJD   .EQ. 0
     .         .AND. NDOY  .EQ. 0
     .         .AND. NHOUR .EQ. 0
     .         .AND. NMIN  .EQ. 0
     .         .AND. NSEC  .EQ. 0  ) THEN


         ERROR = 'No numeric components were supplied in the '
     .   //      'time string. '


      ELSE IF ( NJD .EQ. 1 ) THEN

         ERROR = 'The string possesses calendar components in '
     .   //      'addition to Julian Date specifier. '

      ELSE IF ( NJD .GT. 1 ) THEN

         ERROR = 'There is more than one Julian Date specified '
     .   //      'in the epoch string. '


      ELSE IF      ( NYEAR .EQ. 0 ) THEN

         ERROR = 'The year associated with the calendar string '
     .   //      '"#" could not be identified. '

         CALL REPMC ( ERROR, '#', STRING, ERROR )

      ELSE IF ( NYEAR .GT. 1 ) THEN

         ERROR  =  STRING
         MESSGE = 'Two substrings indicating a calendar year '
     .   //       'were identified in the input time string <#> '
     .   //       'and <#>: "'


         P1 = POS ( REP, 'Y', 1    )
         P2 = POS ( REP, 'Y', P1+1 )

         J = BEGS(P2)
         K = ENDS(P2)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         J = BEGS(P1)
         K = ENDS(P1)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         CALL PREFIX ( MESSGE, 1, ERROR )
         CALL SUFFIX ( '"',    0, ERROR )

      ELSE IF ( NMON .GT. 0 .AND. NDOY .GT. 0 ) THEN

         ERROR  =  STRING
         MESSGE = 'Both a day of year and month were identified '
     .   //       'in the input string. "'


         P2 = MAX ( POS ( REP, 'm', 1 ),
     .              POS ( REP, 'y', 1 ) )

         P1 = MIN ( POS ( REP, 'm', 1 ),
     .              POS ( REP, 'y', 1 ) )

         J = BEGS(P2)
         K = ENDS(P2)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )

         J = BEGS(P1)
         K = ENDS(P1)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )

         CALL PREFIX ( MESSGE, 1, ERROR )
         CALL SUFFIX ( '"',    0, ERROR )

      ELSE IF ( NMON .GT. 1 ) THEN

         ERROR  =  STRING
         MESSGE = 'Two substrings indicating a calendar month '
     .   //       'were identified in the input time string <#> '
     .   //       'and <#>: "'


         P1 = POS ( REP, 'm', 1    )
         P2 = POS ( REP, 'm', P1+1 )

         J = BEGS(P2)
         K = ENDS(P2)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         J = BEGS(P1)
         K = ENDS(P1)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         CALL PREFIX ( MESSGE, 1, ERROR )
         CALL SUFFIX ( '"',    0, ERROR )

      ELSE IF ( NDOY .GT. 1 ) THEN

         ERROR  =  STRING
         MESSGE = 'Two substrings indicating a day of year '
     .   //       'were identified in the input time string <#> '
     .   //       'and <#>: "'


         P1 = POS ( REP, 'y', 1    )
         P2 = POS ( REP, 'y', P1+1 )

         J = BEGS(P2)
         K = ENDS(P2)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         J = BEGS(P1)
         K = ENDS(P1)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         CALL PREFIX ( MESSGE, 1, ERROR )
         CALL SUFFIX ( '"',    0, ERROR )

      ELSE IF ( NDAY .GT. 1 ) THEN

         ERROR  =  STRING
         MESSGE = 'Two substrings indicating a day of month '
     .   //       'were identified in the input time string <#> '
     .   //       'and <#>: "'


         P1 = POS ( REP, 'D', 1    )
         P2 = POS ( REP, 'D', P1+1 )

         J = BEGS(P2)
         K = ENDS(P2)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         J = BEGS(P1)
         K = ENDS(P1)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         CALL PREFIX ( MESSGE, 1, ERROR )
         CALL SUFFIX ( '"',    0, ERROR )

      ELSE IF ( NHOUR .GT. 1 ) THEN

         ERROR  =  STRING
         MESSGE = 'Two substrings representing an hour of the day '
     .   //       'were identified in the input time string <#> '
     .   //       'and <#>: "'


         P1 = POS ( REP, 'H', 1    )
         P2 = POS ( REP, 'H', P1+1 )

         J = BEGS(P2)
         K = ENDS(P2)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         J = BEGS(P1)
         K = ENDS(P1)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         CALL PREFIX ( MESSGE, 1, ERROR )
         CALL SUFFIX ( '"',    0, ERROR )

      ELSE IF ( NMIN  .GT. 1 ) THEN

         ERROR  =  STRING
         MESSGE = 'Two substrings representing minutes of the hour '
     .   //       'were identified in the input time string <#> '
     .   //       'and <#>: "'


         P1 = POS ( REP, 'M', 1    )
         P2 = POS ( REP, 'M', P1+1 )

         J = BEGS(P2)
         K = ENDS(P2)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         J = BEGS(P1)
         K = ENDS(P1)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         CALL PREFIX ( MESSGE, 1, ERROR )
         CALL SUFFIX ( '"',    0, ERROR )

      ELSE IF ( NSEC  .GT. 1 ) THEN

         ERROR  =  STRING
         MESSGE = 'Two substrings representing seconds '
     .   //       'were identified in the input time string <#> '
     .   //       'and <#>: "'


         P1 = POS ( REP, 'S', 1    )
         P2 = POS ( REP, 'S', P1+1 )

         J = BEGS(P2)
         K = ENDS(P2)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         J = BEGS(P1)
         K = ENDS(P1)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         CALL PREFIX ( MESSGE, 1, ERROR )
         CALL SUFFIX ( '"',    0, ERROR )

      ELSE IF ( NDOY .EQ. 0 .AND. NMON .EQ. 0 ) THEN

         ERROR = 'Neither a month nor day of year could be '
     .   //      'identified in the input time string: "#" '

         CALL REPMC ( ERROR, '#', STRING, ERROR )

      ELSE IF ( NMON .EQ. 1 .AND. NDAY .EQ. 0 ) THEN

         ERROR = 'A month was identified in the time string "#", '
     .   //      'but a day of month could not be identified. '

         CALL REPMC ( ERROR, '#', STRING, ERROR )

      ELSE IF ( NMON .EQ. 0 .AND. NDAY .EQ. 1 ) THEN

         ERROR = 'A day of month was identified in the time '
     .   //      'string "#", but the month it belongs to could '
     .   //      'not be identified. '

         CALL REPMC ( ERROR, '#', STRING, ERROR )

      ELSE IF ( NMIN  .GT. NHOUR ) THEN

         ERROR = 'A minutes components of the time  was '
     .   //      'identified in the time string "#", but the '
     .   //      'hours component could not be identified. '

         CALL REPMC ( ERROR, '#', STRING, ERROR )

      ELSE IF ( NSEC  .GT. NMIN  ) THEN

         ERROR = 'A seconds components of the time was '
     .   //      'identified in the time string "#", but the '
     .   //      'minutes component could not be identified. '

         CALL REPMC ( ERROR, '#', STRING, ERROR )

      END IF


      ZZUNPCK = NO
      RETURN




C$Procedure ZZVALT ( Private, Time --- Value Based Tokens )

      ENTRY ZZVALT  ( STRING, B, E, LETTER )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Examine the value of an integer token and if it is within the
C     range from B to E replace the token with the new token
C     specified by LETTER.
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
C      TIME --- PRIVATE
C
C$ Declarations
C
C     IMPLICIT NONE
C     CHARACTER*(*)         STRING
C     INTEGER               B
C     INTEGER               E
C     CHARACTER*(1)         LETTER
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   Original time string.
C     B          I   Lower bound of value range
C     E          I   Upper bound of value range
C     LETTER     I   New token if integer is within range.
C
C     The function returns TRUE if any substitutions are performed.
C
C$ Detailed_Input
C
C     STRING     is an original time string as last supplied to ZZTOKNS.
C
C     B          is the lower bound of some test range of integers
C
C     E          is the upper bound of some test range of integers
C
C     LETTER     is the new token value to put in place of 'i' if
C                the value of the integer is between B and E
C                (inclusive).
C$ Detailed_Output
C
C     The function returns TRUE if any substitutions are performed..
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This function replaces every occurrence of 'i' in the internal
C     representation by the value LETTER if the numerical value
C     of the token corresponding to 'i' is between B and E.
C
C     This is used primarily to identify YEAR tokens in a time
C     string.
C
C$ Examples
C
C     See TPARTV
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
C
C$ Version
C
C-    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB)
C
C        Re-ordered header sections.
C
C-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
C
C        The main routine (which should never be called) now returns
C        the value .FALSE.
C
C-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
C
C        Added a RETURN statement at the end of the main routine.
C        Enhanced error message for the case when the input string
C        to ZZTOKNS has a non-printing character.
C
C-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
C
C
C-&
C
C     So far no translations have been performed.
C
      DID = NO
C
C     Examine each token to see if it is an integer.
C
      DO I = 1, SIZE

         ITEM = ICHAR( REP(I:I) )

         IF ( ITEM .EQ. ICHAR('i') ) THEN
C
C           We've got an integer. Parse it to see if it
C           is in the specified range.
C
            J = BEGS(I)
            K = ENDS(I)

            CALL NPARSI ( STRING(J:K), VALUE, MYERR, PTR )

            IF (      PTR   .EQ. 0
     .          .AND. VALUE .GE. B
     .          .AND. VALUE .LE. E ) THEN

               REP(I:I) = LETTER
               DID      = YES

            END IF

         END IF

      END DO

      ZZVALT = DID
      RETURN

      END
