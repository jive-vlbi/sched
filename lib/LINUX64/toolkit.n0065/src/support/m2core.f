C$Procedure      M2CORE ( META/2 core syntax checking routines. )
 
      SUBROUTINE M2CORE ( TEMP,   TBEG,   KEYWDS,
     .                    STRING, SBEG,   REASON, CUTOFF,
     .                    M2CODE, SCORE,  CAUSE,  SEND    )
      IMPLICIT NONE
 
 
C$ Abstract
C
C     This routine is the header routine for use by M2MTCH and its
C     associated entry point M2RCVR.  As it takes no action, it should
C     not be called directly.
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
C      The META/2 book.
C
C$ Keywords
C
C     PARSING
C     UTILITY
C     WORD
C
C$ Declarations
 
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      CHARACTER*(*)         TEMP
      INTEGER               TBEG
      CHARACTER*(*)         KEYWDS ( LBCELL: * )
      CHARACTER*(*)         STRING
      INTEGER               SBEG
      LOGICAL               REASON
      INTEGER               CUTOFF
      INTEGER               M2CODE
      INTEGER               SCORE
      CHARACTER*(*)         CAUSE ( 2 )
      INTEGER               SEND
 
C$ Brief_I/O
C
C     See the entry point headers for description of each of the
C     input/output arguements.
C
C$ Detailed_Input
C
C     See individual entry points.
C
C$ Detailed_Output
C
C     See individual entry points.
C
C$ Error_Handling
C
C     See individual entry points.
C
C$ Input_Files
C
C     None.
C
C$ Output_Files
C
C     None.
C
C$ Particulars
C
C     This routine serves as the header routine for entry point M2MTCH
C     and its associated entry M2RCVR.  M2MTCH is the essential syntax
C     checking portion of the META/2 syntax comparison routine.
C
C$ Examples
C
C     To compare two templates call M2MTCH
C
C     To find the position of a mispelled keyword in the input string
C     and the possible spelling corrections call M2RCVR
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
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 9, 1994
C
C
C-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of META/2
C         software as of May 3, 1994
C
C
C     Version B1.0.0, 7-APR-1988 (WLT) (IMU)
C
C-&
C
 
 
 
C
C     SPICELIB functions
C
      INTEGER               CARDC
      INTEGER               CARDI
      INTEGER               ESRCHC
      LOGICAL               M2WMCH
      LOGICAL               M2KEYW
      INTEGER               QLSTNB
      INTEGER               POS
      INTEGER               QRTRIM
 
      CHARACTER*(2)         ANA
 
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 120 )
 
C
C     Local variables
C
      INTEGER               TBEGIN
      INTEGER               SBEGIN
      INTEGER               BEGOUT
      INTEGER               SLEN
 
      CHARACTER*(32)        ROOT
      CHARACTER*(LNSIZE)    PHRASE
      CHARACTER*(2)         ARTCLE
 
      LOGICAL               ERROR
      LOGICAL               CALWRD
      LOGICAL               KEYTBE
 
      INTEGER               MSPELL
 
      INTEGER               TE
      INTEGER               TB
      INTEGER               TC
 
      INTEGER               ORIGNL
 
      INTEGER               LOWER
      INTEGER               UPPER
 
      LOGICAL               USEEND
      LOGICAL               USELST
      LOGICAL               USEKEY
      LOGICAL               ENDOK
 
      INTEGER               ENDCHK
      INTEGER               KB
      INTEGER               KE
 
      INTEGER               NKEY
 
      LOGICAL               ENDIT
      INTEGER               MCOUNT
      INTEGER               COUNT
      INTEGER               DCOUNT
 
      INTEGER               SB
      INTEGER               SE
      INTEGER               DB
      INTEGER               DE
      INTEGER               OVERSB
      INTEGER               LASTSB
      INTEGER               OVERSE
      INTEGER               LASTSE
      INTEGER               TIMEB
      INTEGER               SUFFSB
      INTEGER               SUFFSE
 
      INTEGER               TCODE
 
      LOGICAL               KEYWRD
      LOGICAL               CMATCH
 
      CHARACTER*64          LOWERC
      CHARACTER*64          UPPERC
      CHARACTER*64          COUNTC
 
      INTEGER               MAXBST
      PARAMETER           ( MAXBST = 10 )
 
      INTEGER               SCORES ( LBCELL: MAXBST)
      INTEGER               BEST   ( LBCELL: MAXBST)
 
      INTEGER               BSCORE
 
 
C
C     KNOWN, PBEG, and PEND are storage for the recovery entry point.
C
C     Should a spelling error be detected, the best matching words will
C     be stored in KNOWN and the index of the beginning and ending
C     of the problem word in STRING will be stored in PBEG and PEND
C     respectively.
C
      CHARACTER*32          KNOWN  ( LBCELL: MAXBST)
 
      INTEGER               PBEG
 
      INTEGER               PEND
 
      CHARACTER*420         MSSG
 
      INTEGER               METASC
      PARAMETER           ( METASC = 15 )
 
      INTEGER               KEYSC
      PARAMETER           ( KEYSC  = 100)
 
      LOGICAL               PASS1
 
      INTEGER               I
 
      SAVE
 
C
C     Initial values
C
      DATA                  PASS1   / .TRUE.    /
 
      RETURN
 
C$Procedure  M2MTCH ( Match a string with a simple META/2 template )
 
      ENTRY  M2MTCH ( TEMP,   TBEG,  KEYWDS,
     .                STRING, SBEG,  REASON, CUTOFF,
     .                M2CODE, SCORE, CAUSE    )
 
C$ Abstract
C
C     This entry points compares simple templates with strings and
C     produces scores reflecting the extent of agreement between
C     the template and string.  If requested diagnostics are produced.
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
C     The META/2 book.
C
C$ Keywords
C
C     PARSING
C     UTILITY
C     WORD
C
C$ Declarations
C
C     INTEGER               LBCELL
C     PARAMETER           ( LBCELL = -5 )
C
C     CHARACTER*(*)         TEMP
C     INTEGER               TBEG
C     CHARACTER*(*)         KEYWDS ( LBCELL: * )
C     CHARACTER*(*)         STRING
C     INTEGER               SBEG
C     LOGICAL               REASON
C     INTEGER               CUTOFF
C     INTEGER               M2CODE
C     INTEGER               SCORE
C     CHARACTER*(*)         CAUSE  ( 2 )
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     TEMP       I   A simple language specification sentence .
C     TBEG       I   Position in the template to start match attempt.
C     KEYWDS     I   A cell of possible terminators of a META-KEY string
C     STRING     I   A potential language sentence
C     SBEG      I/O  Marker of the current start of the string
C     REASON     I   Set to .TRUE. to request production of diagnostics
C     CUTOFF     I   Spelling error threshold
C     M2CODE     O   Indicates type of mismatch between TEMP and  STRING
C     SCORE      O   Number giving a measure of how closeness of match.
C     CAUSE      O   If requested, a diagnostic of mismatch.
C
C$ Detailed_Input
C
C     TEMP       A simple language specification sentence.  Such a
C                statement consists of only keyword and META/2 class
C                specifiers.  No groups or @then directives are allowed.
C
C     TBEG       Position in the template to start match attempt.
C
C     KEYWDS     A cell of possible terminators of a META-KEY string.
C                This is primarily usefull for higher level routines
C                that pick simple templates out of more complex META/2
C                specification sentences.
C
C     STRING     A is a collection of words that might make up a valid
C                sentence in a META/2 language.  A substring of STRING
C                beginning with SBEG will be matched against TEMP to
C                see if we have a valid phrase in a META/2 language.
C
C     SBEG       Marker of the current start of the string
C
C     REASON     Set to .TRUE. to request production of diagnostics
C
C     CUTOFF     is a parameter used to determine how close words
C                of STRING must match up with keywords in TEMP
C                in order to be diagnosed as spelling errors.
C                Ranges from 0 to 100 are acceptable.  A "good" range
C                of values is from from 65 to 75.
C
C$ Detailed_Output
C
C     SBEG       if the match is successful, SBEG will be set to the
C                first word of the input string that follows the
C                matched substring. ( Note that words in KEYWDS do
C                not qualify as part of the template, but merely
C                serve to delimit the ends of variable length
C                templates. Thus if one of these words was actually
C                used to delimit the end of TEMP, SBEG will point to
C                the beginning of that word in STRING.)
C
C
C     M2CODE     Indicates type of mismatch between TEMP and  STRING
C
C     M2CODE = 0     Indicates that the template supplied matched the
C                    input string as far as it went.
C
C     M2CODE = 10    Indicates that the keyword that was supposed to
C                    terminate a variable length template was probably
C                    mispelled.
C
C     M2CODE = 11    We were expecting a specific keyword and failed
C                    in our match attempt.  It is likely that the
C                    keyword was simply misspelled.
C
C     M2CODE = 101   Indicates that a variable length template had too
C                    few entries before the keyword was encountered.
C
C     M2CODE = 102   Indicates that a variable length template had too
C                    many entries before the keyword was encountered.
C
C     M2CODE = 103   Indicates that the correct number of entries
C                    for a variable length template were encountered
C                    but the input string terminated without finding
C                    the correct keyword.
C
C     M2CODE = 104   Indicates that the string should have terminated
C                    but instead contained extra characters.
C
C     M2CODE = 105   Indicates that correct number if items were
C                    present in the variable length template but that
C                    it did not end with an expected keyword.
C                    Moreover, it is not thought that the problem is
C                    likely to be a simple spelling error.
C
C     M2CODE = 106   The number of items found in a variable length
C                    template was too small and we did not get
C                    an expected keyword.  A possible explanation
C                    is a mistyping one or more of the letters in
C                    one of the META class words.
C
C     M2CODE = 107   The number of items found in a variable length
C                    template was too large and we did not get
C                    an expected keyword.  A possible explanation
C                    is a a forgotten keyword.
C
C     M2CODE = 108   We ran out of string while in a fixed length
C                    template.
C
C     M2CODE = 109   We failed to match a META class word while within
C                    a fixed length template.
C
C     M2CODE = 110   We were expecting to see a specific keyword and
C                    got something else.  This is not thought to be
C                    the result of a spelling error.
C
C     M2CODE = 111   We were expecting to see a META class word and
C                    failed in our matching attempt.
C
C     M2CODE values from 1001 to 1014 indicate problems that can occur
C     when attempting to match a substring with the @calendar specifier.
C
C     M2CODE = 1001  Too many tokens in a @calendar string.
C
C     M2CODE = 1002  Time indicated is JD but no numeric portion
C                    supplied.
C
C     M2CODE = 1003  The date portion of the Julian date didn't make
C                    it through the number parsing.
C
C     M2CODE = 1004  No date was supplied
C
C     M2CODE = 1005  A year was not supplied in a calendar date
C
C     M2CODE = 1006  Ambiguous date specification.
C
C     M2CODE = 1007  Ambiguous month specification
C
C     M2CODE = 1008  Invalid day specification in a calendar date.
C
C     M2CODE = 1009  Year appears as the second item without a
C                    spelled month.
C
C     M2CODE = 1010  Month is not spelled and is not an integer
C                    between 1 and 12.
C
C     M2CODE = 1011  Month not spelled and day is not between 1 and
C                    366.
C
C     M2CODE = 1012  Hour portion of time is not an integer from 0
C                    to 23.
C
C     M2CODE = 1013  Minutes portio of time is not an integer from
C                    0 to 59.
C
C     M2CODE = 1014  Seconds must be a positive number less than 61
C
C     SCORE      Number giving a measure of how closeness of match, 100
C                points are awarded for matched keywords, 15 points
C                for matched classes, 100 points for matched calendar
C                strings.  Fractions of 100 awarded for words that
C                look like they might be misspelled keyword.  The
C                score is used primarily in thos cases when a substring
C                does not match any of a collection of templates
C                exactly.  In this case the one that has the highest
C                score is regarded as being what the user probably
C                meant.
C
C     CAUSE      If requested, a diagnostic of mismatch.
C
C$ Exceptions
C
C     The following errors are detected by this routine.
C
C           'SPICE(KEYWORDNOTFOUND)'
C
C     Additional errors may be detected by SPICELIB routines called
C     by this routine.
C
C
C$ Input_Files
C
C     None.
C
C$ Output_Files
C
C     None.
C
C$ Particulars
C
C     This routine is the central utility used in META/2 when attempting
C     to match potential sentences with language templates.  It compares
C     simple templates with substrings of a command and produces a score
C     indicating the degree of match.  Moreover, if requested,
C     diagnostics are available that indicate why a string did not
C     match a given template.
C
C$ Examples
C
C      None.
C
C$ Restrictions
C
C      It is assumed that all templates are simple META/2 templates.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 9, 1994
C
C
C-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of META/2
C         software as of May 3, 1994
C
C
C     Version B1.0.0, 7-APR-1988 (WLT) (IMU)
C
C-&
C
 
 
C
C     Take care of the SPICE error handling first.
C
      IF ( PASS1 ) THEN
 
         PASS1  = .FALSE.
 
         CALL SSIZEI ( MAXBST, BEST   )
         CALL SSIZEI ( MAXBST, SCORES )
         CALL SSIZEC ( MAXBST, KNOWN  )
         CALL SCARDC ( 1,      KNOWN  )
 
      END IF
 
      SLEN   = LEN ( STRING )
 
      CAUSE(1)  = ' '
      SCORE     = 0
 
      TBEGIN =  TBEG
      SBEGIN =  SBEG
 
      ERROR  = .FALSE.
 
      M2CODE = 0
      MSPELL = 0
 
      KB     = 0
      KE     = 0
 
      NKEY   = CARDC ( KEYWDS )
 
C
C     Locate the next word of the template.
C
      CALL FNDNWD ( TEMP, TBEGIN, TB,   TE )
 
 
      DO WHILE (       (  TB .NE.  0     )
     .           .AND. (     .NOT. ERROR ) )
 
C
C        Zero out the keyword pointers.
C
         KB    =  0
         KE    =  0
         ENDOK = .FALSE.
 
C
C        Examine the current template word.  Is there a range template
C        attatched?
C
         ORIGNL = TB
 
         CALL M2BEGR ( TEMP, TB,   TE, LOWER, UPPER )
 
C
C        Locate the boundaries of the root of this template word.
C
         CALL M2TRIM ( TEMP(TB:TE),           ROOT  )
 
         TC = QRTRIM(ROOT) - 1 + TB
 
 
 
C
C        If TB changed from its original value there is a range template
C        attached to the word TEMP(TB:TE).  The associated values are in
C        LOWER and UPPER.
C
         KEYTBE = M2KEYW   (  TEMP(TB:TE) )
         CALWRD =             TEMP(TB:TC)  .EQ. '@calendar'
     .            .AND. .NOT. KEYTBE
 
         IF (      ( ORIGNL .LT. TB )
     .        .OR. ( CALWRD         ) ) THEN
 
C
C           Yes.   There is a range template attatched.  Is it of
C           variable length?
C
            IF ( CALWRD ) THEN
               LOWER  = 1
               UPPER  = 40
               TIMEB  = SBEGIN
            END IF
 
            IF ( LOWER .NE. UPPER ) THEN
 
C
C              Yes.  The template has a variable length. Determine
C              what delimiters might signal the end of a matching
C              substring of word from string.
C
C              Possibilities are:  The end of the string     (USEEND)
C                                  One of the listed KEYWDS  (USELST)
C                                  A keyword listed in TEMP. (USEKEY)
C
C              Right now we don't know which of the three cases to use.
C
 
               USEEND = .FALSE.
               USELST = .FALSE.
               USEKEY = .FALSE.
               ENDOK  = .FALSE.
 
               ENDCHK = TE + 1
 
C
C              If the end of the current template word, was not
C              at the end of the template, then there might be
C              a keyword next.  Look for the next word to find out.
C
               CALL FNDNWD ( TEMP, ENDCHK, KB, KE )
 
               IF ( KE .GT. 0 ) THEN
 
C
C                 There is a word in the template that follows
C                 our current template word.  See if it is a keyword.
C
                  IF ( M2KEYW( TEMP(KB:KE) )  )THEN
 
C
C                    If it is a keyword, it will be used as the
C                    delimiter for a sequence of words in STRING.
C                    ( Note we only want to work with the root of this
C                      template word. )
C
                     USEKEY = .TRUE.
                     CALL M2TRIM ( TEMP(KB:KE), ROOT )
                     KE = QRTRIM ( ROOT ) + KB - 1
 
                  ELSE
 
C
C                    Its not a keyword.  Bad, Bad. The user was not
C                    using META/2 properly.
C
 
                     CALL CHKIN  ( 'M2MTCH' )
                     CALL SETMSG ( 'M2MTCH: Any META-KEY that '       //
     .                             'is preceded by a variable length' //
     .                             ' range template '                 //
     .                             'in a specification statement '    //
     .                             'must be followed by a keyword. ' )
 
                     CALL SIGERR ( 'SPICE(KEYWORDNOTFOUND)' )
                     CALL CHKOUT ( 'M2MTCH' )
 
                     RETURN
 
                  END IF
 
               ELSE IF ( KE .LE. 0 ) THEN
 
C
C                 We got to this point because there was nothing
C                 to look at beyond where we were in TEMP.  So we
C                 either use one of the listed keywords or the end
C                 of the string will be our delimiter.
C
                  IF ( NKEY .GT. 0 ) THEN
                     USELST = .TRUE.
                     ENDOK  = ESRCHC ( '@end', NKEY, KEYWDS(1) ) .NE. 0
                  ELSE
                     USEEND = .TRUE.
                  END IF
 
               END IF
 
C
C              Until we have detected one of the keywords
C                 or we have not matched the current class
C                 or we run out of words in the sentence
C
C                   Grab the next word of the sentence
C                   Check it for keyword .
C                   Check it for class .
C
               ENDIT  = .FALSE.
               KEYWRD = .FALSE.
 
               MCOUNT =  0
               SUFFSB =  0
               OVERSB =  0
               OVERSE =  0
               LASTSB =  SBEGIN
               LASTSE =  POS ( STRING, ' ', SBEGIN ) - 1
 
               DO WHILE ( .NOT. ENDIT )
 
C
C                 Fetch the next word of the sentence.
C
                  CALL FNDNWD ( STRING, SBEGIN, SB, SE )
 
C
C                 If there WAS a next word SE will not be zero.
C
                  IF ( SE .EQ. 0 ) THEN
 
                     KEYWRD = USEEND .OR. ENDOK
                     ENDIT  = .TRUE.
 
C
C                    BEGOUT will point past the matched portion of the
C                    string.  If no errors occur, it will be used to
C                    set SBEG on output.
C
                     BEGOUT = SLEN + 1
 
                  ELSE
 
C
C                    is this a delimiting word for a variable length
C                    list?
C
                     IF ( USELST ) THEN
 
                        KEYWRD = ESRCHC ( STRING(SB:SE),
     .                                    NKEY,
     .                                    KEYWDS(1)     )
     .                           .NE. 0
 
                        ENDIT  = KEYWRD
 
                        IF ( KEYWRD ) THEN
 
C
C                          Mark the position just before the beginning
C                          of this word in STRING  so that SBEG will
C                          point to the first word past the end of
C                          the matched portion of STRING.
C
                           BEGOUT = SB - 1
                        END IF
 
                     ELSE IF ( USEKEY ) THEN
 
                        KEYWRD =        M2WMCH ( STRING, SB, SE,
     .                                           TEMP(KB:KE)    )
     .                           .AND.  M2KEYW ( TEMP(KB:KE)    )
                        ENDIT  = KEYWRD
 
C
C                       Mark the position of the "next" character
C                       in the string beyond the end of the current
C                       STRING word.
C
                        BEGOUT = SE + 1
 
                     END IF
 
C
C                    If we didn't bump into a keyword this must
C                    be (or should be) another of the words specified
C                    by the META-KEY TEMP(TB:TE)
C
                     IF ( .NOT. KEYWRD ) THEN
 
                        CMATCH = M2WMCH ( STRING, SB, SE, TEMP(TB:TE) )
 
                        IF ( CMATCH ) THEN
 
                           MCOUNT = MCOUNT + 1
 
C
C                          Mark the position of the first character
C                          beyond the end of the current STRING
C                          word.
C
                           BEGOUT = SE     + 1
 
C
C                          If MCOUNT has gotten too big, record the
C                          begin and end of the "bad" portion of the
C                          substring.
C
                           IF      ( MCOUNT .EQ. LOWER + 1 ) THEN
 
C
C                             Mark the location of the beginning
C                             and end of this word in case we need to
C                             backtrack to here.
C
                              SUFFSB = SB
                              SUFFSE = SE
 
                           ELSE IF ( MCOUNT .LE. UPPER     ) THEN
 
C
C                             Mark the end of this word in case
C                             we need it later.
C
                              SUFFSE = SE
 
                           ELSE IF ( MCOUNT .EQ. UPPER + 1 ) THEN
 
                              OVERSB = SB
                              OVERSE = SE
 
                           ELSE IF ( MCOUNT .GT. UPPER ) THEN
 
                              OVERSE = SE
 
                           END IF
 
                        ELSE
 
                           ENDIT  = .TRUE.
 
                        END IF
 
                     END IF
 
                     LASTSB = SB
                     LASTSE = SE
 
                  END IF
 
C
C                 Set the pointer to the input string to the first
C                 character past the end of the current word.
C
                  SBEGIN = SE     + 1
 
               END DO
 
 
C
C              We're now at the end of the loop matching words of STRING
C              with the class of object that had a variable length
C              template.
C
C              The question now is: 'Did we get out of the loop in
C              a healthy or unhealthy way?'
C
C
C              Did we have the required range of items in the class?
C              Did we hit  the keyword?
C
C              If both questions were answered YES,
C
               IF (        KEYWRD
     .              .AND. (MCOUNT .GE. LOWER )
     .              .AND. (MCOUNT .LE. UPPER ) ) THEN
 
C
C                 Increment the score by METASC times the number of
C                 words found in the variable length template plus
C                 KEYSC for getting the keyword right.
C
 
                  IF ( .NOT. CALWRD ) THEN
 
                     SCORE = SCORE + METASC*MCOUNT  + KEYSC
 
                     IF ( USEKEY ) THEN
 
C
C                       set the end of the last template word used to
C                       be the end of the keyword that we just hit.
C
                        TE = KE
 
                     END IF
 
                  ELSE
 
                     MSSG = ' '
                     CALL M2CAL ( STRING(TIMEB:SUFFSE), MSSG, TCODE )
 
                     IF ( TCODE .EQ. 0 ) THEN
 
                        SCORE = SCORE + 2*KEYSC
 
                     ELSE
 
                        SCORE = SCORE + KEYSC
                        ERROR = .TRUE.
 
                        IF ( M2CODE .EQ. 0 ) THEN
                           M2CODE = 1000 + TCODE
                        END IF
 
                        IF ( REASON ) THEN
 
                           CAUSE(1) = 'I was not able to parse the ' //
     .                                'calendar string "'
 
                           CALL SUFFIX (STRING(TIMEB:SUFFSE), 0, CAUSE)
                           CALL SUFFIX ('". ',                0, CAUSE)
                           CALL SUFFIX (MSSG,                 1, CAUSE)
 
                           CALL M2MARK ( STRING, TIMEB, SUFFSE,  CAUSE)
 
                           CAUSE(2) = CAUSE(1)
 
                        END IF
 
                     END IF
 
                  END IF
 
C
C              If less than the required range but a keyword was found
C              the error was: " not enough values loaded. "
C
               ELSE IF ( KEYWRD .AND. MCOUNT .LT. LOWER ) THEN
 
                  CALL INTTXT ( LOWER,  LOWERC )
                  CALL INTTXT ( MCOUNT, COUNTC )
 
                  CALL LCASE  ( LOWERC, LOWERC )
                  CALL LCASE  ( COUNTC, COUNTC )
 
                  ERROR  = .TRUE.
 
                  IF ( M2CODE .EQ. 0 ) THEN
                     M2CODE = 101
                  END IF
 
C
C                 We grant METASC points for every word of the current
C                 class that was found, but we subtract METASC points
C                 for each item we were short.  That is:
C
C                 MCOUNT + ( LOWER - MCOUNT ) = 2*MCOUNT - LOWER
C
                  SCORE  = SCORE + METASC * MAX ( 0, 2*MCOUNT - LOWER )
 
C
C                 Add on KEYSC points for getting the correct keyword.
C
                  SCORE = SCORE + KEYSC
 
                  IF ( REASON ) THEN
 
                     CAUSE(1) = 'I was expecting to see at least '
     .               //         '# # at this point in the command '
     .               //         'string. '
     .               //         'I counted #. '
 
                     CALL M2CLSS ( TEMP(TB:TC), LOWER,   PHRASE )
                     CALL REPMC  ( CAUSE, '#',  LOWERC,  CAUSE )
                     CALL REPMC  ( CAUSE, '#',  PHRASE,  CAUSE )
                     CALL REPMC  ( CAUSE, '#',  COUNTC,  CAUSE )
 
C
C                    OK. now we want to tack on the string and keep
C                    track of where the current word STRING(SB:SE)
C                    will get put.
C
                     CALL M2MARK ( STRING, LASTSB, LASTSE, CAUSE )
                     CAUSE(2) = CAUSE(1)
 
                  END IF
 
 
C
C              If more than the required range but a keyword was found
C              the error was too many values loaded.
C
               ELSE IF ( KEYWRD .AND. MCOUNT .GT. UPPER ) THEN
 
                  CALL INTTXT ( UPPER,  UPPERC )
                  CALL INTTXT ( MCOUNT, COUNTC )
 
                  CALL LCASE  ( UPPERC, UPPERC )
                  CALL LCASE  ( COUNTC, COUNTC )
 
                  ERROR  = .TRUE.
 
                  IF ( M2CODE .EQ. 0 ) THEN
                     M2CODE = 102
                  END IF
 
C
C                 We grant METASC points for every word of the current
C                 class that was found prior to the cutoff limit.
C                 But we subtract METASC points for each extra item.
C                 That is:
C
C                 UPPER + ( MCOUNT - UPPER ) = 2*UPPER - MCOUNT
C
                  SCORE  = SCORE + METASC * MAX ( 0, 2*UPPER - MCOUNT )
 
C
C                 Add on KEYSC points for getting the correct keyword.
C
                  SCORE = SCORE + KEYSC
 
                  IF ( REASON ) THEN
 
                     CAUSE(1) = 'I was expecting to see at most '
     .               //         '# #. I counted #. '
     .               //         'I''ve marked the location of '
     .               //         'the problem for you. '
 
                     CALL M2CLSS ( TEMP(TB:TC), UPPER,   PHRASE )
                     CALL REPMC  ( CAUSE, '#',  UPPERC,  CAUSE )
                     CALL REPMC  ( CAUSE, '#',  PHRASE,  CAUSE )
                     CALL REPMC  ( CAUSE, '#',  COUNTC,  CAUSE )
 
 
                     CALL M2MARK ( STRING, OVERSB, OVERSE,  CAUSE )
                     CAUSE(2) = CAUSE(1)
 
                  END IF
 
C
C              If required range but no keyword, error could be
C              misspelled keyword ( we estimate this ) or keyword
C              was missing.
C
               ELSE IF (       (MCOUNT .GE. LOWER)
     .                   .AND. (MCOUNT .LE. UPPER)  ) THEN
 
 
C
C                 Add METASC points to the score for each of the
C                 words encountered.
C
 
                  IF ( SE .EQ. 0 ) THEN
 
C
C                    We are going to try to see if we had a spelling
C                    error that caused us to run out of string
C
                     CALL FNDNWD ( STRING, SUFFSB, DB, DE )
 
                     ORIGNL = SUFFSB
                     COUNT  = LOWER + 1
                     BSCORE = 0
                     DCOUNT = 0
 
                     DO WHILE ( COUNT .LE. MCOUNT )
 
                        IF     ( USEKEY ) THEN
 
                           KNOWN(1) = TEMP(KB:KE)
 
C
C                          Compare the last word encountered in the
C                          string with the KEYWORD we were expecting.
C
                           CALL BESTWD(STRING(DB:DE), KNOWN,  CUTOFF,
     .                                 BEST,          SCORES, MSSG  )
 
                        ELSE IF ( USELST ) THEN
 
C
C                          Compare the last word that we hit with one
C                          of the keywords from the list of possible
C                          closing keywords.
C
                           CALL BESTWD(STRING(DB:DE), KEYWDS, CUTOFF,
     .                                 BEST,          SCORES, MSSG  )
 
                        END IF
 
                        IF (       ( CARDI (SCORES) .GT. 0      )
     .                       .AND. ( SCORES(1     ) .GE. CUTOFF ) )
     .                  THEN
 
C
C                          We are going to treat this as a spelling
C                          error.
C
                           IF ( M2CODE .EQ. 0 ) THEN
                              M2CODE = 13
                           END IF
 
C
C                          Save the beginning and ending of the
C                          problem word for use in the recovery
C                          entry point.
C
                           IF ( SCORES(1) .GT. BSCORE ) THEN
 
                              BSCORE = SCORES(1)
                              PBEG   = DB
                              PEND   = DE
 
C
C                             Everything up to this is now regarded
C                             as simply matching the META-KEY.  Store
C                             this number of META-KEYs for use by
C                             diagnostics generation.
C
                              DCOUNT = COUNT - 1
 
                           END IF
 
                        END IF
 
                        SUFFSB = DE + 1
 
C
C                       Look at the next word until we have gone
C                       past UPPER even if we already have a
C                       candidate for misspelling, there might be
C                       a better one.
C
                        CALL FNDNWD ( STRING, SUFFSB, DB, DE )
 
                        COUNT = COUNT + 1
 
                     END DO
 
C
C                    Save the misspelling information associated
C                    with the best match (if there was one).
C
                     IF ( BSCORE .GT. 0 ) THEN
 
                        IF     ( USEKEY ) THEN
 
                           KNOWN(1) = TEMP(KB:KE)
 
C
C                          Compare the last word encountered in the
C                          string with the KEYWORD we were expecting.
C
                           CALL BESTWD( STRING(PBEG:PEND),  KNOWN,
     .                                  CUTOFF,             BEST,
     .                                  SCORES,             MSSG  )
 
                        ELSE IF ( USELST ) THEN
 
C
C                          Compare the last word that we hit with one
C                          of the keywords from the list of possible
C                          closing keywords.
C
                           CALL BESTWD( STRING(PBEG:PEND), KEYWDS,
     .                                  CUTOFF,            BEST,
     .                                  SCORES,            MSSG  )
 
C
C                          Save the best matches for use in the
C                          recovery entry point.
C
                           DO I = 1, CARDI(BEST)
                              KNOWN(I) = KEYWDS( BEST(I) )
                           END DO
 
                           CALL SCARDC ( CARDI(BEST), KNOWN )
 
                        END IF
 
C
C                       This is not regarded as an error worth
C                       stopping for unless our
C                       misspelling total has runs over 100.
C
                        MSPELL = MSPELL + ( 100 - BSCORE )
 
                        IF ( MSPELL .LT. 100 ) THEN
                           SCORE = SCORE + DCOUNT*METASC + BSCORE
 
C
C                          Back the value of SBEGIN back up to the
C                          point of failure, so that we can continue
C                          processing as if nothing had gone wrong.
C
                           SBEGIN = PEND + 1
                        ELSE
                           SCORE = SCORE + DCOUNT*METASC
                           ERROR = .TRUE.
                        END IF
 
                     ELSE
 
C
C                       Restore the initial value of SUFFSB
C
                        SUFFSB = ORIGNL
                        SCORE  = SCORE + MCOUNT * METASC
 
                        IF ( M2CODE .EQ. 0 ) THEN
                           M2CODE = 103
                        END IF
 
 
C
C                       This occurs if we ran out of stuff in STRING
C                       and we were looking to find a keyword instead.
C
                        ERROR  = .TRUE.
 
                     END IF
 
                     IF      ( USEKEY .AND. REASON ) THEN
 
                        CAUSE(1) = 'I was looking for the keyword "'
 
                        CALL SUFFIX ( TEMP(KB:KE),            1, CAUSE )
                        CALL SUFFIX ( '" when I reached the', 1, CAUSE )
                        CALL SUFFIX ( 'end of the input ',    1, CAUSE )
                        CALL SUFFIX ( 'command. ',            1, CAUSE )
 
                     ELSE IF ( USELST .AND. REASON ) THEN
 
                        CAUSE(1) = 'I was looking for one of the '   //
     .                             'keywords that follow when I '    //
     .                             'reached the end of the input '   //
     .                             'command.  Keywords: {'
 
                        DO I = 1, NKEY
 
                           CALL SUFFIX ( KEYWDS(I), 2, CAUSE )
                           CALL SUFFIX ( ',',       0, CAUSE )
 
                        END DO
 
                        CAUSE(1)(QLSTNB(CAUSE(1)):) = ' }.'
 
                     END IF
 
                     IF ( REASON .AND. ( BSCORE .NE. 0 ) ) THEN
 
                        CAUSE(2) = CAUSE(1)
                        CAUSE(1) = ' '
 
                        CALL SUFFIX ( MSSG,           1,      CAUSE   )
                        CALL M2MARK ( STRING, PBEG,   PEND,   CAUSE   )
                        CALL SUFFIX ( MSSG,           1,      CAUSE(2))
                        CALL M2MARK ( STRING, PBEG,   PEND,   CAUSE(2))
 
                     ELSE IF ( REASON ) THEN
 
                        CALL M2MARK ( STRING, LASTSB, LASTSE, CAUSE   )
                        CAUSE(2) = CAUSE(1)
 
                     END IF
 
 
C
C                 Recall that we are examining the case when the number
C                 of word matches is within the expected range, but
C                 no keyword was present.  We have already looked at
C                 what to do if we ran out of string prematurely.
C
                  ELSE IF ( SE .GT. 0 ) THEN
 
 
                     SCORE = SCORE + MCOUNT * METASC
 
C
C                    We ran into something unexepected.  Possibly
C                    a misspelled keyword.  See if any of the
C                    expected keywords are close to what we got.
C
                     IF  ( USEEND ) THEN
 
                        ERROR  = .TRUE.
 
                        IF ( M2CODE .EQ. 0 ) THEN
                           M2CODE = 104
                        END IF
 
                        IF ( REASON ) THEN
 
                           CAUSE(1) = 'The input command contains '   //
     .                                'extra characters that are not '//
     .                                'part of a valid command.  '
 
                           CALL M2MARK ( STRING, SB, SE, CAUSE       )
 
                           CAUSE(2) = CAUSE(1)
 
                        END IF
 
                     ELSE IF ( USEKEY .OR. USELST ) THEN
 
 
                        IF     ( USEKEY ) THEN
 
                           KNOWN(1) = TEMP(KB:KE)
 
C
C                          Compare the last word encountered in the
C                          string with the KEYWORD we were expecting.
C
 
                           CALL BESTWD ( STRING(SB:SE), KNOWN,  CUTOFF,
     .                                   BEST,          SCORES, MSSG   )
 
                        ELSE IF ( USELST ) THEN
 
C
C                          Compare the last word that we hit with one
C                          of the keywords from the list of possible
C                          closing keywords.
C
                           CALL BESTWD ( STRING(SB:SE), KEYWDS, CUTOFF,
     .                                   BEST,          SCORES, MSSG   )
 
C
C                          Save the best matches for use in the recovery
C                          entry point.
C
                           DO I = 1, CARDI(BEST)
                              KNOWN(I) = KEYWDS( BEST(I) )
                           END DO
 
                           CALL SCARDC ( CARDI(BEST), KNOWN )
 
                        END IF
 
C
C                       We are still checking out the case in which we
C                       had a correct range of words for a variable
C                       length template, but ran into
C                       something that was not a terminating keyword
C                       that we were expecting.  Possibly we hit a
C                       mispelled keyword.
C
C                       Well? Was there anything to the rumor of a
C                       spelling error?
C
                        IF (       ( CARDI (SCORES) .GT. 0      )
     .                       .AND. ( SCORES(1     ) .GE. CUTOFF ) ) THEN
 
                           IF ( M2CODE .EQ. 0 ) THEN
                              M2CODE = 10
 
C
C                             Save the beginning and ending of the
C                             problem word for use in the recovery
C                             entry point.
C
                              PBEG  = SB
                              PEND  = SE
 
                           END IF
 
C
C                          This is probably a spelling error.
C                          Point out the error.
C
                           MSPELL = MSPELL + ( 100 - SCORES(1) )
 
                           IF ( MSPELL .LT. 100 ) THEN
                              SCORE  = SCORE + SCORES(1)
                           ELSE
                              ERROR  = .TRUE.
                           END IF
 
                           IF ( REASON ) THEN
 
C
C                             Construct an error message indicating
C                             the spelling diagnostic.
C
                              ERROR  = .TRUE.
 
                           END IF
 
                        ELSE IF (    (CARDI (SCORES) .EQ. 0     )
     .                           .OR.(SCORES(1     ) .LT. CUTOFF)) THEN
 
C
C                          This is not a misspelling.
C                          Set the error flag
C
                           ERROR  = .TRUE.
 
                           IF ( M2CODE .EQ. 0 ) THEN
                              M2CODE = 105
                           END IF
 
                           MSSG = ' '
 
                        END IF
 
                        IF ( REASON .AND. USEKEY ) THEN
 
                           CAUSE(1) = 'I was looking for the '
 
                           CALL SUFFIX ('keyword "',    1, CAUSE)
                           CALL SUFFIX (TEMP(KB:KE),    0, CAUSE)
                           CALL SUFFIX ('" when I ',    0, CAUSE)
                           CALL SUFFIX ('encountered ', 1, CAUSE)
                           CALL SUFFIX ('the word "',   1, CAUSE)
                           CALL SUFFIX (STRING(SB:SE),  0, CAUSE)
                           CALL SUFFIX ('".   ',        0, CAUSE)
                           CALL SUFFIX ( MSSG,          1, CAUSE)
                           CAUSE(2) = CAUSE(1)
                           CAUSE(1) = MSSG
 
                           CALL M2MARK ( STRING, SB,   SE, CAUSE(1))
                           CALL M2MARK ( STRING, SB,   SE, CAUSE(2))
 
                        ELSE IF ( REASON .AND. USELST ) THEN
 
                           CAUSE(1) = 'I was looking for one of ' //
     .                                'the keywords in the list: '//
     .                                '{ '
 
                           DO I = 1, NKEY
                              CALL SUFFIX ( KEYWDS(I),  1, CAUSE)
 
                              IF ( I .NE. NKEY ) THEN
                                 CALL SUFFIX ( ',',     0, CAUSE)
                              END IF
                           END DO
 
                           CALL SUFFIX ('}  when I ',   1, CAUSE)
                           CALL SUFFIX ('encountered ', 1, CAUSE)
                           CALL SUFFIX ('the word "',   1, CAUSE)
                           CALL SUFFIX (STRING(SB:SE),  0, CAUSE)
                           CALL SUFFIX ('".   ',        0, CAUSE)
                           CALL SUFFIX ( MSSG,          1, CAUSE)
 
                           CALL M2MARK ( STRING, SB,   SE, CAUSE)
                           CAUSE(2) = CAUSE(1)
 
                        END IF
 
                     END IF
 
                  END IF
 
 
C
C              If out of range and no keyword then we don't have
C              a good guess as to what went wrong.
C
               ELSE IF ( .NOT. KEYWRD
     .                   .AND. (    ( MCOUNT .LT. LOWER )
     .                          .OR.( MCOUNT .GT. UPPER ) ) ) THEN
 
 
                  IF      ( MCOUNT .LT. LOWER ) THEN
 
                     IF ( M2CODE .EQ. 0 ) THEN
                        M2CODE = 106
                     END IF
 
                     SCORE = SCORE + METASC * MAX (0, 2*MCOUNT - LOWER)
 
                     ERROR  = .TRUE.
 
                  ELSE IF ( MCOUNT .GT. UPPER ) THEN
 
                     IF ( USEKEY .OR. USELST ) THEN
 
C
C                       We are going to try to see if we had a spelling
C                       error that caused us to get too many words.
C
                        CALL FNDNWD ( STRING, SUFFSB, DB, DE )
 
                        COUNT  = LOWER + 1
                        BSCORE = 0
                        DCOUNT = 0
 
                        DO WHILE ( COUNT .LE. UPPER + 1 )
 
                           IF     ( USEKEY ) THEN
 
                              KNOWN(1) = TEMP(KB:KE)
 
C
C                             Compare the last word encountered in the
C                             string with the KEYWORD we were expecting.
C
                              CALL BESTWD(STRING(DB:DE), KNOWN,  CUTOFF,
     .                                    BEST,          SCORES, MSSG  )
 
                           ELSE IF ( USELST ) THEN
 
C
C                             Compare the last word that we hit with one
C                             of the keywords from the list of possible
C                             closing keywords.
C
                              CALL BESTWD(STRING(DB:DE), KEYWDS, CUTOFF,
     .                                    BEST,          SCORES, MSSG  )
 
                           END IF
 
                           IF (       ( CARDI (SCORES) .GT. 0      )
     .                          .AND. ( SCORES(1     ) .GE. CUTOFF ) )
     .                     THEN
 
C
C                             We are going to treat this as a spelling
C                             error.
C
                              IF ( M2CODE .EQ. 0 ) THEN
                                 M2CODE = 12
                              END IF
 
C
C                             Save the beginning and ending of the
C                             problem word for use in the recovery
C                             entry point.
C
                              IF ( SCORES(1) .GT. BSCORE ) THEN
 
                                 BSCORE = SCORES(1)
                                 PBEG   = DB
                                 PEND   = DE
 
C
C                                Everything up to this is now regarded
C                                as simply matching the META-KEY.  Store
C                                this number of META-KEYs for use by
C                                diagnostics generation.
C
                                 DCOUNT = COUNT - 1
 
                              END IF
 
                           END IF
 
                           SUFFSB = DE + 1
 
C
C                          Look at the next word until we have gone
C                          past UPPER even if we already have a
C                          candidate for misspelling, there might be
C                          a better one.
C
                           CALL FNDNWD ( STRING, SUFFSB, DB, DE )
 
                           COUNT = COUNT + 1
 
                        END DO
 
C
C                       Save the misspelling information associated
C                       with the best match (if there was one).
C
                        IF ( BSCORE .GT. 0 ) THEN
 
                           IF     ( USEKEY ) THEN
 
                              KNOWN(1) = TEMP(KB:KE)
 
C
C                             Compare the last word encountered in the
C                             string with the KEYWORD we were expecting.
C
                              CALL BESTWD( STRING(PBEG:PEND),  KNOWN,
     .                                     CUTOFF,             BEST,
     .                                     SCORES,             MSSG  )
 
                           ELSE IF ( USELST ) THEN
 
C
C                             Compare the last word that we hit with one
C                             of the keywords from the list of possible
C                             closing keywords.
C
                              CALL BESTWD( STRING(PBEG:PEND), KEYWDS,
     .                                     CUTOFF,            BEST,
     .                                     SCORES,            MSSG  )
 
C
C                             Save the best matches for use in the
C                             recovery entry point.
C
                              DO I = 1, CARDI(BEST)
                                 KNOWN(I) = KEYWDS( BEST(I) )
                              END DO
 
                              CALL SCARDC ( CARDI(BEST), KNOWN )
 
                           END IF
 
C
C                          This is not regarded as an error worth
C                          stopping for unless our
C                          misspelling total has runs over 100.
C
                           MSPELL = MSPELL + ( 100 - BSCORE )
 
                           IF ( MSPELL .LT. 100 ) THEN
                              SCORE = SCORE + DCOUNT*METASC + BSCORE
 
C
C                             Back the value of SBEGIN back up to the
C                             point of failure, so that we can continue
C                             processing as if nothing had gone wrong.
C
                              SBEGIN = PEND + 1
                           ELSE
                              SCORE = SCORE + DCOUNT*METASC
                              ERROR = .TRUE.
                           END IF
 
                        END IF
 
                     END IF
 
C
C                    We might not have had a good candidate for a
C                    misspelling, if not we don't have a good clue
C                    as to what went wrong.
C
                     IF ( M2CODE .EQ. 0 ) THEN
 
                        M2CODE = 107
                        SCORE  = SCORE
     .                         + METASC * MAX (0, 2*UPPER - MCOUNT)
 
                        ERROR  = .TRUE.
 
                     END IF
 
 
                  END IF
 
C
C                 If there is to be a diagnostic generated, set up
C                 the beginning of it so that everyone else can
C                 share in the same work.
C
                  IF ( REASON ) THEN
 
                     ERROR = .TRUE.
 
                     CAUSE(1) = 'I was expecting to see between '
     .               //         '# and # # '
 
                     CALL M2CLSS ( TEMP(TB:TC),UPPER,  PHRASE )
                     CALL REPMC  ( CAUSE, '#', LOWERC, CAUSE )
                     CALL REPMC  ( CAUSE, '#', UPPERC, CAUSE )
                     CALL REPMC  ( CAUSE, '#', PHRASE, CAUSE )
 
 
 
                     IF      ( USEKEY ) THEN
 
                        CALL SUFFIX ( 'followed by ',      1, CAUSE )
                        CALL SUFFIX ( 'the keyword, ',     1, CAUSE )
                        CALL SUFFIX ( TEMP(KB:KE),         1, CAUSE )
                        CALL SUFFIX ( '.',                 0, CAUSE )
 
                     ELSE IF ( USELST ) THEN
 
                        CALL SUFFIX ( 'followed by ',      1, CAUSE )
                        CALL SUFFIX ( 'one of the ',       1, CAUSE )
                        CALL SUFFIX ( 'keywords from the', 1, CAUSE )
                        CALL SUFFIX ( 'list {',            1, CAUSE )
 
                        DO I = 1, NKEY
 
                           CALL SUFFIX ( KEYWDS(I),        1, CAUSE )
 
                           IF ( I .NE. NKEY ) THEN
                              CALL SUFFIX ( ',',           1, CAUSE )
                           END IF
                        END DO
 
                        CALL SUFFIX ( '}.',                1, CAUSE )
 
                     ELSE IF ( USEEND ) THEN
 
                        CALL SUFFIX ( 'filling out the   ',1, CAUSE )
                        CALL SUFFIX ( 'end of the string.',1, CAUSE )
 
                     END IF
 
C
C                    Use the information stored in M2CODE to determine
C                    how many words we encountered before we figured
C                    out we had an error.
C
                     IF ( M2CODE .GE. 100 ) THEN
 
                        CALL INTTXT ( MCOUNT, COUNTC  )
                        CALL LCASE  ( COUNTC, COUNTC  )
 
                     ELSE
 
                        CALL INTTXT ( DCOUNT, COUNTC )
                        CALL LCASE  ( COUNTC, COUNTC )
 
                     END IF
 
                     CALL SUFFIX ( 'I had counted ',      1, CAUSE )
                     CALL SUFFIX ( COUNTC,                1, CAUSE )
 
                     IF ( MCOUNT .EQ. 1 ) THEN
                        CALL SUFFIX ( 'such word',  1, CAUSE )
                     ELSE
                        CALL SUFFIX ( 'such words', 1, CAUSE )
                     END IF
 
                     CALL SUFFIX ( 'when I encountered',  1, CAUSE )
 
                  END IF
 
C
C                 We are still in the case of a variable length template
C                 for which we did not hit a keyword and did not have
C                 the expected range of items for the current META-KEY.
C
C                 OK. Now tailor the end of the message to reflect
C                 what went wrong in particular.
C
                  IF ( REASON .AND. ( M2CODE .LT. 100 ) ) THEN
 
                     CALL SUFFIX ( 'the word "',          1, CAUSE )
                     CALL SUFFIX ( STRING(PBEG:PEND),     0, CAUSE )
                     CALL SUFFIX ( '" .',                 0, CAUSE )
 
                     CALL SUFFIX ( MSSG,                  1, CAUSE )
 
                     CALL M2MARK ( STRING, PBEG,       PEND, CAUSE )
                     CAUSE(2) = CAUSE(1)
                  ELSE IF ( REASON .AND. ( SE .EQ. 0 ) ) THEN
 
                     CALL SUFFIX ( 'the end of the input',1,   CAUSE )
                     CALL SUFFIX ( 'string.    ',         1,   CAUSE )
                     CALL M2MARK ( STRING, QLSTNB(STRING)+1,
     .                                     QLSTNB(STRING)+1,   CAUSE )
                     CAUSE(2) = CAUSE(1)
 
C
C                  check for a misspell.
C
 
                  ELSE IF (  REASON .AND. ( SE  .NE. 0   )  ) THEN
 
                     CALL SUFFIX ( 'the word "',          1, CAUSE )
                     CALL SUFFIX ( STRING(SB:SE),         0, CAUSE )
                     CALL SUFFIX ( '" .',                 0, CAUSE )
 
C
C                    If misspell likely mention that too.
C
                     IF      ( USEKEY ) THEN
 
                        KNOWN(1) = TEMP(KB:KE)
 
                        CALL BESTWD ( STRING(SB:SE), KNOWN,  CUTOFF,
     .                                BEST,          SCORES, MSSG    )
 
 
                     ELSE IF ( USELST ) THEN
 
                        CALL BESTWD ( STRING(SB:SE), KEYWDS, CUTOFF,
     .                                BEST,          SCORES, MSSG    )
 
                     END IF
 
                     IF (       ( CARDI ( SCORES ) .GT. 0      )
     .                    .AND. ( SCORES(1)        .GT. CUTOFF ) ) THEN
 
                        CALL SUFFIX ( MSSG, 1, CAUSE )
 
 
                     END IF
 
                     CALL M2MARK ( STRING, SB,    SE, CAUSE     )
                     CAUSE(2) = CAUSE(1)
 
                  END IF
 
               END IF
 
 
            ELSE
 
C
C              This "ELSE" is the "NO" response to the question:  "Ok.
C              we have a range template. Is it of variable length?"
C
               ENDIT   = LOWER .EQ. 0
               MCOUNT  = 0
 
               DO WHILE ( .NOT. ENDIT )
 
                  CALL FNDNWD ( STRING, SBEGIN, SB, SE )
 
                  IF      ( SE .EQ. 0 ) THEN
 
                     ENDIT  = .TRUE.
                     ERROR  = .TRUE.
 
                     IF ( M2CODE .EQ. 0 ) THEN
                        M2CODE = 108
                     END IF
 
                     IF ( REASON ) THEN
 
                        CAUSE(1) = 'I was expecting to see # '
     .                  //         '# when I ran out of '
     .                  //         'words in the command string. '
 
 
                        CALL M2CLSS ( TEMP(TB:TC), 1, PHRASE )
                        ARTCLE = ANA( PHRASE, 'L'              )
                        CALL REPMC  ( CAUSE, '#', ARTCLE, CAUSE )
                        CALL REPMC  ( CAUSE, '#', PHRASE, CAUSE )
 
                        CALL M2MARK( STRING, QLSTNB(STRING)+1,
     .                                       QLSTNB(STRING)+1, CAUSE )
 
                        CAUSE(2) = CAUSE(1)
                     END IF
 
                  ELSE IF ( M2WMCH ( STRING, SB, SE, TEMP(TB:TE) ) )
     .            THEN
 
                     MCOUNT  = MCOUNT + 1
                     SCORE  =  SCORE  + METASC
 
                     SBEGIN = SE + 1
 
C
C                    Mark the position of the first character beyond the
C                    current STRING word.
C
                     BEGOUT = SBEGIN
                     ENDIT  = MCOUNT .GE. LOWER
 
                  ELSE
 
                    IF ( M2CODE .EQ. 0 ) THEN
                       M2CODE = 109
                    END IF
 
                     ERROR  = .TRUE.
                     ENDIT  = .TRUE.
                     IF ( REASON ) THEN
 
                        CAUSE(1) = 'I was expecting to see # # '
     .                  //         'when I encounterd the word '
     .                  //         '"#" in the command. '
 
                        CALL M2CLSS ( TEMP(TB:TC), 1, PHRASE )
                        ARTCLE = ANA( PHRASE, 'L'              )
                        CALL REPMC  ( CAUSE, '#', ARTCLE,        CAUSE )
                        CALL REPMC  ( CAUSE, '#', PHRASE,        CAUSE )
                        CALL REPMC  ( CAUSE, '#', STRING(SB:SE), CAUSE )
 
 
                        CALL M2MARK ( STRING, SB,           SE, CAUSE )
                        CAUSE(2) = CAUSE(1)
 
                     END IF
 
                  END IF
 
               END DO
 
            END IF
 
         ELSE
 
            CALL FNDNWD ( STRING, SBEGIN, SB, SE )
 
C
C           This "ELSE" is the "NO" response to the question: "Is a
C           range template present?" that was asked a very long, long
C           time ago.
C
            CMATCH = M2WMCH ( STRING, SB, SE, TEMP(TB:TE) )
 
C
C           Set the string pointer to the first character following
C           the current string word.
C
            SBEGIN = SE + 1
 
C
C           Record SBEGIN in case we have run out of teplate and
C           haven't produced any errors.
C
            BEGOUT = SBEGIN
 
            IF ( CMATCH ) THEN
 
               KEYWRD = M2KEYW ( TEMP(TB:TE) )
 
               IF ( KEYWRD )THEN
                  SCORE  = SCORE  + KEYSC
               ELSE
                  SCORE  = SCORE  + METASC
               END IF
 
 
            ELSE IF ( .NOT. CMATCH ) THEN
 
               KEYWRD = M2KEYW ( TEMP(TB:TE) )
 
C
C              See if we were supposed to get a keyword and if
C              so see if this is just some simple spelling error.
C
               IF ( KEYWRD ) THEN
 
                  KNOWN (1) = TEMP(TB:TC)
                  CALL  SCARDC ( 1, KNOWN )
 
                  IF ( SE .GT. 0 ) THEN
 
                     CALL BESTWD ( STRING(SB:SE), KNOWN,  CUTOFF,
     .                             BEST,          SCORES, MSSG    )
                  END IF
 
                  IF (       ( CARDI(SCORES) .GT. 0      )
     .                 .AND. ( SCORES(1)     .GE. CUTOFF ) ) THEN
 
                     IF ( M2CODE .EQ. 0 ) THEN
                        M2CODE = 11
 
C
C                       Save the beginning and ending of the
C                       problem word for use in the recovery
C                       entry point.
C
                        PBEG  = SB
                        PEND  = SE
 
                     END IF
 
C
C                    We regard this to be a spelling error  of the
C                    keyword. This will be a signal to stop looking at
C                    this keyword if we are asking for diagnostics.
C
 
                     IF ( MSPELL .GT. 100 ) THEN
                        ERROR  = .TRUE.
                     ELSE
                        SCORE  = SCORE  + SCORES(1)
                        MSPELL = MSPELL + ( 100 - SCORES(1) )
                     END IF
 
                     IF ( REASON ) THEN
 
                        ERROR  = .TRUE.
 
                        CAUSE(1) = 'I was expecting to see the '      //
     .                             'keyword "'
 
                        CALL SUFFIX ( TEMP(TB:TC),            0, CAUSE )
                        CALL SUFFIX ( '" when I encountered', 0, CAUSE )
                        CALL SUFFIX ( 'the word "',           1, CAUSE )
                        CALL SUFFIX ( STRING(SB:SE),          0, CAUSE )
                        CALL SUFFIX ( '" in the input ',      0, CAUSE )
                        CALL SUFFIX ( 'string.     ',         1, CAUSE )
                        CALL SUFFIX ( MSSG,                   1, CAUSE )
                        CAUSE(2) = CAUSE(1)
                        CAUSE(1) = MSSG
                        CALL M2MARK ( STRING, SB,    SE,      CAUSE(1) )
                        CALL M2MARK ( STRING, SB,    SE,      CAUSE(2) )
 
                     END IF
 
                  ELSE IF (      ( CARDI(SCORES) .EQ. 0      )
     .                      .OR. ( SCORES(1)     .LT. CUTOFF ) ) THEN
 
                     ERROR  = .TRUE.
 
                     IF ( M2CODE .EQ. 0 ) THEN
                        M2CODE = 110
 
                        IF ( SE .GT. 0 ) THEN
 
                           CALL BESTWD ( STRING(SB:SE), KNOWN,  1,
     .                                   BEST,          SCORES, MSSG )
 
                        END IF
 
                        IF (       ( SB            .NE. 0 )
     .                       .AND. ( CARDI(SCORES) .GT. 0 ) )
     .                  THEN
                           SCORE = SCORE + SCORES(1)
                        END IF
                     END IF
 
 
                     IF (REASON) THEN
 
                        CAUSE(1) = 'I was expecting to see the '      //
     .                             'keyword "'
 
                        CALL SUFFIX ( TEMP(TB:TC),            0, CAUSE )
                        CALL SUFFIX ( '" when I ',            0, CAUSE )
 
                        IF ( SB .EQ. 0 ) THEN
                           CALL SUFFIX ( 'ran out of ',       1, CAUSE )
                           CALL SUFFIX ( 'characters in the', 1, CAUSE )
                           CALL SUFFIX ( 'input string. ',    1, CAUSE )
 
                           SB = QLSTNB ( STRING ) + 1
                           SE = SB
                        ELSE
                           CALL SUFFIX ( 'encountered',       1, CAUSE )
                           CALL SUFFIX ( 'the word "',        1, CAUSE )
                           CALL SUFFIX ( STRING(SB:SE),       0, CAUSE )
                           CALL SUFFIX ( '" in the input ',   0, CAUSE )
                           CALL SUFFIX ( 'string.     ',      1, CAUSE )
                        END IF
 
                        CALL M2MARK ( STRING, SB,    SE,         CAUSE )
                        CAUSE(2) = CAUSE(1)
 
                     END IF
 
                  END IF
 
               ELSE IF ( .NOT. M2KEYW(TEMP(TB:TE)) ) THEN
 
                  ERROR  = .TRUE.
 
                  IF ( M2CODE .EQ. 0 ) THEN
                     M2CODE = 111
                  END IF
 
                  IF ( REASON ) THEN
 
                     CAUSE(1) = 'I was expecting to see # # '
     .               //         'when I '
 
 
                     CALL M2CLSS ( TEMP(TB:TC), 1, PHRASE )
                     ARTCLE = ANA( PHRASE, 'L'              )
                     CALL  REPMC ( CAUSE, '#', ARTCLE,        CAUSE )
                     CALL  REPMC ( CAUSE, '#', PHRASE,        CAUSE )
 
 
                     IF ( SB .EQ. 0 ) THEN
                        CALL SUFFIX ( 'ran out of characters', 1, CAUSE)
                        CALL SUFFIX ( 'in the input string. ', 1, CAUSE)
 
                        SB = QLSTNB ( STRING ) + 1
                        SE = SB
                     ELSE
                        CALL SUFFIX ( 'encountered the word "', 1,CAUSE)
                        CALL SUFFIX ( STRING(SB:SE),            0,CAUSE)
                        CALL SUFFIX ( '" in the input string.', 0,CAUSE)
                     END IF
 
                     CALL M2MARK (STRING, SB, SE, CAUSE )
                     CAUSE(2) = CAUSE(1)
 
                  END IF
 
               END IF
 
            END IF
 
         END IF
 
         TBEGIN = MAX(KE,TE) + 1
 
C
C        Locate the next word of the template and continue unless
C        we get a second error detected.
C
         CALL FNDNWD ( TEMP, TBEGIN, TB, TE )
      END DO
 
 
C
C     If we got out of the template without an error, set SBEG to
C     BEGOUT---the first character after the matched portion of the
C     STRING and before the first word of whatever is left.
C
      IF ( M2CODE .EQ. 0 ) THEN
         SBEG = BEGOUT
      END IF
 
      RETURN
 
 
C
C$Prodedure M2RCVR ( Recover from a spelling error )
C
      ENTRY M2RCVR ( SBEG, SEND, KEYWDS )
 
C
C$ Abstract
C
C     Fetch the indices of the beginning and end of a "misspelled"
C     keyword along with the list of corrections.
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
C     The META/2 book.
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
C
C     INTEGER               SBEG
C     INTEGER               SEND
C     CHARACTER*(*)         KEYWDS ( LBCELL: * )
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     SBEG       O   Beginning of "misspelled" word in STRING
C     SEND       O   Ending of "misspelled" word in STRING
C     KEYWDS     O   Cell of possible correct spellings of keyword.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     SBEG       Beginning of "misspelled" word in STRING
C
C     SEND       Ending of "misspelled" word in STRING
C
C     KEYWDS     Cell of possible correct spellings of keyword.
C
C$ Error_Handling
C
C     No errors are detected by this entry point.
C
C$ Input_Files
C
C     None.
C
C$ Output_Files
C
C     None.
C
C$ Particulars
C
C
C$ Examples
C
C
C$ Restrictions
C
C     One must call M2MTCH before calling this routine if correct
C     results are desired.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 9, 1994
C
C
C-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of META/2
C         software as of May 3, 1994
C
C
C     Version B1.0.0, 7-APR-1988 (WLT) (IMU)
C
C-&
C
 
 
 
      SBEG = PBEG
      SEND = PEND
 
      CALL COPYC ( KNOWN, KEYWDS )
 
 
      RETURN
 
      END
