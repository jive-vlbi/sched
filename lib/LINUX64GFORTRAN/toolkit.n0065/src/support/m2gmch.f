 
C$Procedure      M2GMCH ( Match a META/2 template including groups )
 
      SUBROUTINE M2GMCH ( TEMP,    THNWDS,
     .                    STRING,  SBEG,   REASON, CUTOFF,
     .                    PSSTHN,  M2CODE, SCORE,  CAUSE   )
      IMPLICIT NONE
 
C$ Abstract
C
C      This routine will match a META/2 template that contains no
C      qualified @then directives.
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
C     STRING
C     UTILITY
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      CHARACTER*(*)         TEMP
      CHARACTER*(*)         THNWDS  ( LBCELL : * )
      CHARACTER*(*)         STRING
      INTEGER               SBEG
      LOGICAL               REASON
      INTEGER               CUTOFF
      LOGICAL               PSSTHN
      INTEGER               M2CODE
      INTEGER               SCORE
      CHARACTER*(*)         CAUSE   ( 2 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     TEMP       I   A META/2 template
C     THNWDS     I   A cell of initial keywords for a following @then
C     STRING     I   A candidate META/2 sentence.
C     SBEG       I   Where to start processing this sentence
C     REASON     I   Flag indicating diagnostics should be produced.
C     CUTOFF     I   Threshold used for spelling error diagnostics.
C     PSSTHN     O   Flag to indicate we made it past a @then
C     M2CODE     O   META/2 code indicating how a match failed.
C     SCORE      O   a measure of how well STRING matched TEMP.
C     CAUSE      O   Diagnostic message if requested for non-matches.
C
C$ Detailed_Input
C
C     TEMP       is a META/2 template to be compared with a portion
C                of the candidate input sentence.
C
C     THNWDS     is a cell containing KEYWORDS that may be used as
C                terminators for the entire template.  Typically
C                this cell will contain the initial keywords of
C                a class of templates that can be branched to from
C                this template.
C
C     STRING     A string, a substring of which ( STRING(SBEG:) )
C                should be compared with the input META/2 template.
C
C     SBEG       is the beginning of the substring that should be
C                compared with TEMP.
C
C     REASON     Is a logical flag, that should be set to .TRUE.
C                if the user wishes to have error mismatch diagnostics
C                to be returned by this routine.
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
C                matched substring. ( Note that words in THNWDS do
C                not qualify as part of the template, but merely
C                serve to delimit the ends of variable length
C                templates. Thus if one of these words was actually
C                used to delimit the end of TEMP, SBEG will point to
C                the beginning of that word in STRING.)
C
C     PSSTHN     is set only if the template matches up to an
C                an unqualified @then directive.  In such a case
C                PSSTHN will be set to .TRUE.  Otherwise it will not
C                be changed from its input value.
C
C     M2CODE     is an integer META/2 code that indicates how an attempt
C                to match the input failed.  If the match was successful
C                M2CODE will be returned as zero.  Otherwise it will
C                be returned with a positive value.  Possible values
C                and meanings are:
C
C
C
C     SCORE      is a measure of how well STRING matched TEMP.  This
C                is useful primarily when looking through several
C                templates, none of which yield an M2CODE of zero.  In
C                this case, the template with the highest SCORE is
C                likely to be the template the input string was
C                "intended" to match.
C
C     CAUSE      If REASON is set to .TRUE. and the match fails
C                (M2CODE .NE. 0 ), this string will contain a
C                description of the suspected cause of the match
C                failure.  Moreover, the input string will be "marked"
C                at the location of the match failure.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1)  If the number of delimiting keywords is greater than 64 a
C         SPICE TOOLKIT error will be SIGERRled.
C
C         SPICE(TOOMANYKEYWORDS)
C
C         Delimiting keywords are:
C
C         a) Keywords that immediately follow group templates.
C         b) Keywords that are the initial keywords of a simple template
C            contained within a group template.
C         c) The keywords passed to the routine in the cell THNWDS.
C
C$ Particulars
C
C     This routine allows one to compare strings with those META/2
C     language templates that do not end with a qualified-'@then'.
C     Moreover, it serves as the principle tool for matching the
C     various pieces of full META/2 templates.  If a match occurs
C     the remainder of the string can be compared with the templates
C     pointed to by the @then  directive.
C
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     The maximum number of delimiting keywords is 64.
C
C     No checks are made to see if the template supplied is in fact
C     a valid META/2 template.
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
C-     META/2 Configured Version 2.1.0, 08-NOV-2005 (BVS)
C
C        Updated to remove non-standard use of duplicate arguments
C        in FNDNWD calls.
C
C-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 9, 1994
C
C
C-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
C
C         Added an extra blank after a carriage return "/cr"
C         substring in
C
C           DIAGNS = 'I was trying to match part of the input '  ...
C           //       'string with one of the expresions listed ' ...
C           //       'here: /cr/cr '                             ...
C           //        SUBTMP(1:RTRIM(SUBTMP))                    ...
C           //       './cr/cr The expression that came closest ' ...
C           //       'was: /cr/cr, '
C
C-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of META/2
C         software as of May 3, 1994
C
C
C     Beta Version 1.0.0, 19-MAY-1988 (WLT) (IMU)
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               BSRCHI
      INTEGER               CARDC
      INTEGER               CARDI
      INTEGER               QLSTNB
      INTEGER               LSTLTI
      INTEGER               NCPOS
      INTEGER               POS
      INTEGER               RTRIM
      INTEGER               SIZEC
      INTEGER               UPTO
      LOGICAL               M2KEYW
      LOGICAL               MATCH
 
C
C     Local variables
C
      LOGICAL               PASS1
 
      CHARACTER*32          TERMS  ( LBCELL : 64 )
 
      INTEGER               INDXES ( LBCELL : 64 )
 
      INTEGER               TLEN
      INTEGER               SIZE
 
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               TMPJ
 
      INTEGER               TBEG
      INTEGER               TEND
 
      LOGICAL               MORE
      LOGICAL               GROUP
      LOGICAL               SIMPLE
      CHARACTER*8           LAST
 
      INTEGER               POSITN
      CHARACTER*32          LABEL
 
      INTEGER               EO
      INTEGER               BO
      LOGICAL               OPTDIR
 
      INTEGER               BW
      INTEGER               EW
 
      INTEGER               FEWEST
      INTEGER               MOST
 
      INTEGER               GMATCH
      INTEGER               BSCORE
      INTEGER               BEGOFG
      INTEGER               ENDOFG
      INTEGER               AFTERG
 
      INTEGER               BS
      INTEGER               ES
 
      INTEGER               A
      INTEGER               B
      INTEGER               E
      INTEGER               BLSTWD
      INTEGER               CLSTWD
      INTEGER               ELSTWD
 
      CHARACTER*32          KEYWDS ( LBCELL : 64 )
      CHARACTER*32          SWORDS ( LBCELL : 64 )
 
      INTEGER               T1CODE
      INTEGER               TSCORE
 
      INTEGER               LOC
 
      INTEGER               BDIAGS
      INTEGER               EDIAGS
 
      INTEGER               BCODE
 
      CHARACTER*1024        SUBTMP
 
      LOGICAL               REDIAG
      LOGICAL               VTEMPL
 
      SAVE
 
 
      DATA                  PASS1   / .TRUE. /
 
 
C
C     If this is the first pass through this routine, set the size of
C     the cells TERMS and INDXES
C
      IF ( PASS1 ) THEN
 
         PASS1 = .FALSE.
 
         CALL SSIZEC ( 64, TERMS  )
         CALL SSIZEI ( 64, INDXES )
         CALL SSIZEC ( 64, KEYWDS )
         CALL SSIZEC ( 64, SWORDS )
 
      END IF
 
C
C     Clear out the parse table.
C
      CALL M2PCLR
 
C
C     Collect the list of potential terminating keywords.
C
      CALL M2TERM ( TEMP, TERMS, INDXES )
 
C
C     Append all of the '@then(*)'-keywords to the list of terminators.
C
      IF ( CARDC(TERMS) + CARDC (THNWDS) .GE. SIZEC(TERMS) - 2 ) THEN
 
         CALL CHKIN  ( 'M2GMCH' )
         CALL SIGERR ( 'SPICE(TOOMANYKEYWORDS)' )
         CALL CHKOUT ( 'M2GMCH' )
         RETURN
 
      END IF
 
      TLEN = LEN   ( TEMP  ) + 1
      J    = CARDC ( TERMS ) + 1
 
      DO I = 1, CARDC( THNWDS )
         TERMS (J) = THNWDS(I)
         INDXES(J) = TLEN
         J         = J + 1
      END DO
 
C
C     Append a '@end' and a '}' to the end of the terminators, and
C     adjust the cardinality of the TERMS cell
C
      TERMS (J) = '@end'
      INDXES(J) = TLEN
      J         = J + 1
 
      TERMS (J) = '}'
      INDXES(J) = TLEN
      SIZE      = CARDC(TERMS) + CARDC(THNWDS) + 2
 
      CALL SCARDC ( SIZE, TERMS  )
      CALL SCARDI ( SIZE, INDXES )
 
C
C     This routine will only use the portion of the template up
C     to a qualified @then.
C
      CALL M2THNQ ( TEMP, POSITN, LABEL )
 
      IF ( POSITN .LE. LEN ( TEMP ) ) THEN
         TEMP(POSITN:) = ' '
      END IF
 
C
C     Now initialize pointers and the loop control variable MORE so
C     that we can start the loop.
C
      TBEG = 1
      TEND = 1
 
      MORE = .TRUE.
 
      DO WHILE ( MORE )
C
C     As long as we are not told to exit
C
C        Look at the next word,
C
         CALL FNDNWD ( TEMP, TBEG, BW, EW )
 
         IF ( BW .EQ. 0 ) THEN
 
C
C           There wasn't a next word.  There is nothing left to do.
C           We set MORE to .FALSE. so that we can exit the loop.
C
            MORE   = .FALSE.
            GROUP  = .FALSE.
            SIMPLE = .FALSE.
 
         ELSE IF ( TEMP(BW:EW) .EQ. '@then' ) THEN
 
C
C           We have an unqualified @then directive.  This means that
C           we are on the right track as far as determining what
C           command we are working on.  Set the PASSED-A-@then flag
C           (PSSTHN) to .TRUE. and the other candidates to .FALSE.
C
            PSSTHN = .TRUE.
            GROUP  = .FALSE.
            SIMPLE = .FALSE.
 
            TBEG   = EW + 1
            TEND   = TBEG
 
         ELSE IF ( MATCH( TEMP(BW:EW), '(%*:%*){' ) ) THEN
 
C
C           We are about to enter a group template.  Determine
C           the FEWEST number of simple templates in the group
C           that must match and the MOST that we will check.
C
            CALL M2BEGR ( TEMP, BW, EW, FEWEST, MOST )
 
            GROUP  = .TRUE.
            SIMPLE = .FALSE.
            LAST   = 'GROUP'
 
C
C           Set up the pointers for looking for the simple
C           templates within this group.
C
            TBEG   = EW + 1
            TEND   = TBEG
 
         ELSE
 
C
C           The only possible candidate is a simple template.
C
            GROUP  = .FALSE.
            SIMPLE = .TRUE.
            TBEG   = BW
            LAST   = 'SIMPLE'
 
         END IF
 
 
         IF ( GROUP ) THEN
 
C
C           Set up the initial values for this group.  We need
C
C           1)  The number of simple template matches so far for
C               this group.
C
            GMATCH = 0
 
C
C           2)  A best score of the simple templates checked so far.
C
            BSCORE = -1
 
C
C           3)  A temporary place to store the M2CODE returned for
C               a simple template of this group.
C
            T1CODE = 0
 
C
C           4)  The position in the full template to jump to when we are
C               done with this template, the beginning and the end of
C               the group
C
            BEGOFG = TBEG
            ENDOFG = UPTO ( TEMP, ' }', TBEG  )
            AFTERG = ENDOFG + 3
 
C
C           Make sure there is a viable simple template within this
C           group.
C
            BS = NCPOS( TEMP,  ' ',    BEGOFG        )
            ES = MIN  ( UPTO ( TEMP, ' | ', BEGOFG  ),
     .                  UPTO ( TEMP, ' } ', BEGOFG  )  )
 
            IF (      ( BS          .EQ. 0      )
     .           .OR. ( BS          .GE. ES     )
     .           .OR. ( TEMP(BS:BS) .EQ. '}'    )
     .           .OR. ( ES          .GT. ENDOFG ) ) THEN
 
               GROUP = .FALSE.
            END IF
 
C
C           Finally, if FEWEST is 1 or 0,
C           remove the '}' that has index equal to the index
C           of the '}' that is the terminator of this group.
C
            IF ( FEWEST .LE. 1 ) THEN
               LOC = BSRCHI ( ENDOFG+2, CARDI(INDXES), INDXES(1) )
 
               IF ( LOC .NE. 0 ) THEN
                  CALL REMLAI ( 1, LOC, INDXES(1), CARDI(INDXES) )
                  CALL REMLAC ( 1, LOC, TERMS(1),  CARDC(TERMS)  )
 
                  CALL SCARDC ( CARDC(TERMS)  - 1, TERMS      )
                  CALL SCARDI ( CARDI(INDXES) - 1, INDXES     )
               END IF
            END IF
 
            DO WHILE ( GROUP )
 
C
C              We've got a viable simple template for this group.
C
C              If it ends with a variable template find out what the
C              possible terminating words are.
C
               A = 0
               B = 0
 
               CALL FNDPTK ( TEMP, ' ', ES + 1, BLSTWD, ELSTWD       )
               CALL M2BEGR ( TEMP,              BLSTWD, ELSTWD, A, B )
 
               CLSTWD = MIN ( ELSTWD, BLSTWD + 8 )
               VTEMPL =      A .NE. B
     .                .OR. (.NOT. M2KEYW        ( TEMP(BLSTWD:ELSTWD) )
     .                      .AND.'@calendar' .EQ. TEMP(BLSTWD:CLSTWD) )
     .
 
               IF ( VTEMPL ) THEN
C
C                 There is a variable length template, the keywords
C                 that might terminate this template are given
C                 in TERMS up to the first occurance of a '}'.
C
                  IF ( GMATCH .LT. MOST-1 ) THEN
                     I = LSTLTI ( BEGOFG, CARDI(INDXES), INDXES(1) ) + 1
                  ELSE
                     I = LSTLTI ( AFTERG, CARDI(INDXES), INDXES(1) ) + 1
                  END IF
 
                  J = 0
 
                  DO WHILE ( TERMS(I) .NE. '}' )
 
C
C                    Keep only those keywords that are not the initial
C                    keyword of this template.
C
 
                     IF ( INDXES(I) .NE. BS ) THEN
                        J          = J + 1
                        CALL M2TRIM ( TERMS(I), KEYWDS(J) )
                     END IF
 
                     I             = I + 1
                  END DO
 
                  CALL SCARDC ( J, KEYWDS )
 
               ELSE
 
                  CALL SCARDC ( 0, KEYWDS )
 
               END IF
 
C
C              Check the current template with M2MTCH.
C
 
               IF ( TEMP(BS:ES) .EQ. '@options' ) THEN
 
                  T1CODE = -1
                  TSCORE = -1
 
               ELSE
 
C
C                 Dump the temporary parse table.
C
                  CALL M2TCLR
                  CALL M2MTCH ( TEMP(BS:ES), 1,       KEYWDS,
     .                          STRING,      SBEG,    .FALSE., CUTOFF,
     .                          T1CODE,      TSCORE,  CAUSE           )
 
               END IF
C
C              If the attempt at a match succeeded ...
C
               IF ( T1CODE .EQ. 0 ) THEN
 
C
C                 Increment the number of group matches by 1.
C                 Increment the score for this template.
C                 Set the best score obtained thus far to zero
C                 in preparation for the next pass through the
C                 group.
C
                  GMATCH = GMATCH + 1
                  SCORE  = SCORE  + TSCORE
                  BSCORE = -1
 
C
C                 Move the temporary parse table to the keepers
C                 parse table.
C
                  CALL M2KEEP
C
C                 The current template should be taken off the viable
C                 list.
C
                  IF ( ES .LT. ENDOFG ) THEN
                     TEMP (BS:ES+2) = ' '
                  ELSE
                     CALL FNDPTK ( TEMP, ' ',  BS, A, B )
 
                     IF ( TEMP(A:B) .EQ. '|' ) THEN
                        TEMP (  A:ES ) = ' '
                     ELSE
                        TEMP ( BS:ES ) = ' '
                     END IF
 
                  END IF
 
 
C
C                 Reset ES to be the one before the beginning of
C                 the group template (BS will be set to ES + 1
C                 at the end of the group loop).
C
                  ES = BEGOFG - 1
 
C
C                 Adjust the possible terminating keyword set.
C                 (remove the initial keyword of the simple template
C                 just matched from the collection).
C
                  LOC    = BSRCHI ( BS, CARDI(INDXES), INDXES(1) )
 
                  CALL REMLAI ( 1, LOC, INDXES(1), CARDI(INDXES) )
                  CALL REMLAC ( 1, LOC, TERMS(1),  CARDC(TERMS)  )
 
                  CALL SCARDC ( CARDC(TERMS)  - 1, TERMS      )
                  CALL SCARDI ( CARDI(INDXES) - 1, INDXES     )
 
 
C
C                 Finally, if we have now exactly matched FEWEST-1,
C                 remove the '}' that has index equal to the index
C                 of the '}' that is the terminator of this group.
C
                  IF ( GMATCH .EQ. FEWEST - 1 ) THEN
                     LOC = BSRCHI ( ENDOFG+2, CARDI(INDXES), INDXES(1) )
 
                     IF ( LOC .NE. 0 ) THEN
                        CALL REMLAI ( 1, LOC, INDXES(1), CARDI(INDXES) )
                        CALL REMLAC ( 1, LOC, TERMS(1),  CARDC(TERMS)  )
 
                        CALL SCARDC ( CARDC(TERMS)  - 1, TERMS      )
                        CALL SCARDI ( CARDI(INDXES) - 1, INDXES     )
                     END IF
                  END IF
 
               ELSE
 
C
C                 Record the score if this is higher than a previous
C                 value.
C
                  IF ( TSCORE .GT. BSCORE ) THEN
 
                     BSCORE = TSCORE
                     BDIAGS = BS
                     EDIAGS = ES
                     BCODE  = T1CODE
 
                     CALL COPYC ( KEYWDS, SWORDS )
 
                  END IF
 
               END IF
 
C
C              Remove all introductory '@options' directives.
C
               OPTDIR = .TRUE.
 
               DO WHILE ( OPTDIR )
 
                  BO = NCPOS (        TEMP, ' ',    BEGOFG )
                  EO = MIN   ( UPTO ( TEMP, ' | ',  BEGOFG ),
     .                         UPTO ( TEMP, ' } ',  BEGOFG ) )
 
                  IF ( BO .LT. EO ) THEN
 
                     OPTDIR = TEMP(BO:EO) .EQ. '@options'
 
                     IF ( OPTDIR ) THEN
                        TEMP(BO:EO) = ' '
                        EO          = EO+2
 
                        IF ( TEMP(EO:EO) .EQ. '|' ) THEN
                           TEMP(EO:EO) = ' '
                        END IF
                     END IF
 
                  ELSE
                     OPTDIR = .FALSE.
                  END IF
               END DO
C
C              Should we stay in this group? Only if you can answer yes
C              to  all of the following:
C
C                  1.) Are more matches allowed for this group.
C
C                  2.) Is there another template in this group that
C                      hasn't been checked.
C
               IF ( GMATCH .GE. MOST ) THEN
 
                  GROUP = .FALSE.
 
               ELSE
 
C
C                 Make sure there is a viable simple template within
C                 this group.
C
 
                  BS = NCPOS( TEMP,  ' |',    ES + 1        )
                  ES = MIN  ( UPTO ( TEMP, ' | ', BS      ),
     .                        UPTO ( TEMP, ' } ', BS      )  )
 
                  IF (      ( BS          .EQ. 0      )
     .                 .OR. ( BS          .GE. ES     )
     .                 .OR. ( TEMP(BS:BS) .EQ. '}'    )
     .                 .OR. ( ES          .GT. ENDOFG ) ) THEN
 
                     GROUP = .FALSE.
 
                  END IF
 
               END IF
 
 
            END DO
 
C
C           When we leave the group, see if we had a sufficient number
C           of matches.  If we did, jump past the end of the group.
C           If we didn't, this is an error---head for home.
C
            OPTDIR = INDEX ( TEMP(BEGOFG:ENDOFG), ' @options ' ) .NE. 0
 
            IF (  .NOT. OPTDIR
     .            .AND. GMATCH .GE. FEWEST ) THEN
 
               TBEG = AFTERG
 
            ELSE IF ( OPTDIR .AND. ( GMATCH .GE. MOST  )  ) THEN
 
               IF ( REASON ) THEN
 
                  CALL CMPRSS ( ' ', 1, TEMP(BEGOFG:ENDOFG),
     .                                  TEMP(BEGOFG:ENDOFG)    )
 
                  B = BEGOFG - 1
                  E = INDEX ( TEMP(BEGOFG:ENDOFG), ' @options ' )
     .              + 1
 
                  CAUSE(1) = 'I had already matched the maximum '     //
     .                       'number of allowed simple templates in ' //
     .                       'a group without matching the  '         //
     .                       'following REQUIRED templates./cr/cr(3:3)'
                  CALL SUFFIX ( TEMP(B:E),        1, CAUSE(1) )
                  CALL SUFFIX ('} /cr/cr(-3:-3)', 1, CAUSE(1) )
 
                  M2CODE =  11000
                  MORE   = .FALSE.
 
               END IF
 
            ELSE IF ( OPTDIR .AND. ( GMATCH .GE. FEWEST ) ) THEN
 
               SCORE = SCORE + BSCORE
 
C
C              If diagnostics are requested then see what went wrong
C              with the best fitting simple template.
C
               IF ( REASON ) THEN
 
                  BS = BDIAGS
                  ES = EDIAGS
 
                  CALL M2MTCH ( TEMP(BS:ES), 1,       SWORDS,
     .                          STRING,      SBEG,    REASON, CUTOFF,
     .                          T1CODE,      TSCORE,  CAUSE           )
 
                  CAUSE(2) = CAUSE(1)
 
                  B        = BEGOFG - 1
                  E        = ENDOFG + 2
 
                  CALL CMPRSS ( ' ', 1, TEMP(B:E), SUBTMP )
 
                  IF (       INDEX( SUBTMP, ' | ' )
     .                 .EQ.  INDEX( SUBTMP, ' @options ' ) - 2 )
     .            THEN
 
                     CALL PREFIX ( '/cr/cr(-3:)',   1, CAUSE(2) )
                     CALL PREFIX ( SUBTMP,          1, CAUSE(2) )
                     CALL PREFIX ( 'Although I had matched a required'//
     .                             ' number of expressions in the '   //
     .                             'group below, I had not yet match' //
     .                             'ed the explicitely required '     //
     .                             'expression that appears prior to' //
     .                             ' the META/2 "@options" directive '//
     .                             'in the group shown here./cr(3:) ',
     .                              1, CAUSE(2)            )
 
                     K = POS( SUBTMP, ' | ', INDEX(SUBTMP,' @options '))
 
                     IF (     ( K                         .GT. 0 )
     .                  .AND. ( NCPOS (STRING, ' ', SBEG) .NE. 0 )) THEN
 
                        CALL SUFFIX ( '/cr/cr Of the remaining simple'//
     .                                ' templates '                   //
     .                                '(including the optional ones) '//
     .                                'the one that comes closest '   //
     .                                'to matching is: /cr/cr(3:) ',
     .                                 1, CAUSE(2)      )
                        CALL SUFFIX ( TEMP(BDIAGS:EDIAGS),
     .                                1, CAUSE(2)      )
                        CALL SUFFIX ( '/cr/cr(-3:)', 0, CAUSE(2)      )
 
                     END IF
 
                  ELSE
 
                     CALL PREFIX ( '/cr/cr(-3:)',     1, CAUSE(2) )
                     CALL PREFIX ( SUBTMP,            0, CAUSE(2) )
                     CALL PREFIX ( 'Although I had matched a required'//
     .                             ' number of expressions in the '   //
     .                             'group below, I had not yet match' //
     .                             'ed the explicitely required '     //
     .                             'expressions that appear prior to' //
     .                             ' the META/2 "@options" directive '//
     .                             'in the group shown here. '        //
     .                             './cr/cr(3:) ',   1, CAUSE(2) )
 
                     IF ( NCPOS (STRING, ' ', SBEG) .NE. 0 ) THEN
 
                        CALL SUFFIX ( '/cr/crOf the remaining simple' //
     .                                ' templates, '                  //
     .                                'the one that comes closest '   //
     .                                'to matching is: /cr/cr(3:) ',
     .                                                     1, CAUSE(2) )
                        CALL SUFFIX ( TEMP(BDIAGS:EDIAGS), 1, CAUSE(2) )
                        CALL SUFFIX ( '/cr/cr(-3:)',       0, CAUSE(2) )
 
                     END IF
 
                  END IF
 
               END IF
 
               M2CODE = BCODE
               MORE   = .FALSE.
 
            ELSE IF ( GMATCH .LT. FEWEST ) THEN
 
               SCORE = SCORE + BSCORE
 
C
C              If diagnostics are requested then see what went wrong
C              with the best fitting simple template.
C
               IF ( REASON ) THEN
 
                  BS = BDIAGS
                  ES = EDIAGS
 
                  CALL M2MTCH ( TEMP(BS:ES), 1,       SWORDS,
     .                          STRING,      SBEG,    REASON, CUTOFF,
     .                          T1CODE,      TSCORE,  CAUSE           )
 
                  CAUSE(2) = CAUSE(1)
 
                  B        = BEGOFG - 1
                  E        = ENDOFG + 2
 
                  CALL CMPRSS ( ' ', 1, TEMP(B:E), SUBTMP )
 
                  IF ( INDEX( SUBTMP, ' | ' ) .NE. 0 ) THEN
 
                     CALL PREFIX ( '''./cr/cr(-3:)',
     .                             1,         CAUSE (2)               )
                     CALL PREFIX ( TEMP ( BDIAGS:EDIAGS ),
     .                             0,         CAUSE (2)               )
                     CALL PREFIX ( 'I was trying to match part of '   //
     .                             'the input string with one of '    //
     .                             'the expressions listed here:'     //
     .                             '/cr/cr(3:) '                      //
     .                             SUBTMP(1:RTRIM(SUBTMP))           //
     .                             './cr/cr(-3:) The expression '    //
     .                             'that came '//
     .                             'closest was: /cr/cr(3:)''',
     .                             0,         CAUSE (2)               )
                  ELSE
 
                     CALL PREFIX ( '''./cr/cr(-3:)',
     .                             1,         CAUSE (2)                )
                     CALL PREFIX ( TEMP ( BDIAGS:EDIAGS ),
     .                             0,         CAUSE (2)                )
                     CALL PREFIX ( 'I was trying to match part of '   //
     .                             'the input string with the '       //
     .                             'expression: /cr/cr(3:) ''',
     .                              0,         CAUSE (2)              )
 
                  END IF
 
               END IF
 
               M2CODE = BCODE
               MORE   = .FALSE.
 
            END IF
C
         ELSE IF  ( SIMPLE ) THEN
 
            TEND   = MIN (  UPTO ( TEMP, ' @then', TBEG       ),
     .                      UPTO ( TEMP, '){ ',    TBEG       )
     .                   ) + 1
 
            CALL FNDPTK  (  TEMP,  ' ',  TEND, BLSTWD, ELSTWD )
 
            TEND = ELSTWD
C
C           See if the simple template ends with a variable template.
C           If it does, find out what the possible terminating words
C           are.
C
            A = 0
            B = 0
 
            CALL M2BEGR ( TEMP, BLSTWD, ELSTWD, A, B )
 
            CLSTWD = MIN ( ELSTWD, BLSTWD + 8 )
            VTEMPL =      A .NE. B
     .             .OR. (.NOT. M2KEYW        ( TEMP(BLSTWD:ELSTWD) )
     .                   .AND.'@calendar' .EQ. TEMP(BLSTWD:CLSTWD) )
 
            IF ( VTEMPL ) THEN
 
C
C              There is a variable length template, the keywords
C              that might terminate this template are given
C              in TERMS up to the first occurance of a '}'.
C
               I = LSTLTI( ELSTWD, CARDI(INDXES), INDXES(1) ) + 1
               J = 0
 
C
C              Just load keywords onto the list until we hit a '}'
C              (We are guarenteed that this will happen, because
C              we put a '}' on the end of the list at the beginning
C              of this routine.)
C
               DO WHILE ( TERMS(I) .NE. '}' )
 
                  J          = J + 1
                  CALL M2TRIM ( TERMS(I), KEYWDS(J) )
                  I          = I + 1
 
               END DO
 
               CALL SCARDC ( J , KEYWDS )
 
            ELSE
 
               CALL SCARDC ( 0,     KEYWDS )
 
            END IF
 
C
C           Check the current template with M2MTCH.
C
               CALL M2TCLR
               CALL M2MTCH ( TEMP(TBEG:TEND), 1,       KEYWDS,
     .                       STRING,          SBEG,    .FALSE., CUTOFF,
     .                       T1CODE,          TSCORE,  CAUSE           )
 
C
C           If the attempt at a match succeeded ...
C
            IF ( T1CODE .EQ. 0 ) THEN
 
               SCORE  = SCORE  + TSCORE
               TBEG   = TEND   + 1
               CALL     M2KEEP
 
            ELSE
 
               CALL M2MTCH ( TEMP(TBEG:TEND), 1,       KEYWDS,
     .                       STRING,          SBEG,    REASON, CUTOFF,
     .                       T1CODE,          TSCORE,  CAUSE           )
 
               SCORE  = SCORE + TSCORE
               M2CODE = T1CODE
               MORE   = .FALSE.
 
            END IF
 
         END IF
 
      END DO
 
 
C
C     If there were no THNWDS and there is stuff left in the string and
C     we haven't already noticed, we've got an error dude.
C
      IF (       ( CARDC(THNWDS) .EQ. 0                )
     .     .AND. ( SBEG          .LT. QLSTNB( STRING ) )
     .     .AND. ( M2CODE        .EQ. 0                ) ) THEN
 
 
C
C        Until we have evidence to justify looking for probable causes
C        of the current overage of input string, we assume that we
C        are not interested in offering conjectures about what the
C        problem is.  We'll just say there is extra stuff.
C
         REDIAG = .FALSE.
 
C
C        Now look for justification of fancier diagnostics.
C
C        Was the last thing we attempted to match part of a group
C        template?
C
         IF (       ( LAST   .EQ. 'GROUP' )
     .        .AND. ( GMATCH .LT.  MOST   ) ) THEN
 
C
C           We are going to see if one of the options of an ending group
C           template looks like it was the intention of the user.
C
            IF ( BCODE .LT. 100 ) THEN
 
C
C              We had a probable spelling error, set the flag to
C              diagnose the problem.
C
               REDIAG = .TRUE.
 
            ELSE
 
C
C              Look at what the score could have been for the
C              simple template that was the closest match.
C
               I      = 1
               J      = BDIAGS
               TSCORE = 0
 
               CALL FNDNWD ( TEMP, J, I, TMPJ )
               J = TMPJ
 
               DO WHILE ( ( I .NE. 0 ) .AND. ( I .LT. EDIAGS ) )
 
                  A = 1
                  B = 1
 
                  CALL M2BEGR (  TEMP, I, J, A, B )
 
                  IF ( M2KEYW( TEMP(I:J) ) ) THEN
                     TSCORE = TSCORE + 100
                  ELSE
                     TSCORE = TSCORE + A*15
                  END IF
 
                  CALL FNDNWD ( TEMP, J, I, TMPJ )
                  J = TMPJ
 
               END DO
 
C
C              If the score actually recorded made it at least a quarter
C              of the way, we will guess that this may have been the
C              root of the problem.
C
               REDIAG = BSCORE .GT. MAX( CUTOFF, TSCORE/4 )
 
            END IF
         END IF
 
C
C        If there was sufficient grounds to warrant second guessing,
C        run the best guess template through M2MTCH to get a diagnostic.
C
         IF ( REDIAG ) THEN
 
            IF ( REASON ) THEN
 
               BS = BDIAGS
               ES = EDIAGS
 
               CALL M2MTCH ( TEMP(BS:ES), 1,       KEYWDS,
     .                       STRING,      SBEG,    REASON, CUTOFF,
     .                       T1CODE,      TSCORE,  CAUSE           )
 
               CAUSE(2) = CAUSE(1)
 
               B = BEGOFG - 1
               E = ENDOFG + 2
 
               CALL CMPRSS ( ' ', 1, TEMP(B:E), SUBTMP )
 
               IF ( INDEX( SUBTMP, ' | ' ) .NE. 0 ) THEN
 
                  CALL PREFIX ( '''./cr/cr(-3:)',
     .                          1,         CAUSE(2)                 )
                  CALL PREFIX ( TEMP ( BDIAGS:EDIAGS ),
     .                          0,         CAUSE(2)                 )
                  CALL PREFIX ( 'Extra words appear in the input ' //
     .                          'string that are not part of a '   //
     .                          'valid expression. I think you '   //
     .                          'may have been trying to supply '  //
     .                          'one of the optional expressions ' //
     .                          'listed here:/cr/cr(3:) '          //
     .                          SUBTMP(1:RTRIM(SUBTMP))            //
     .                          '/cr/cr(-3:). '                    //
     .                          'The expression that came '        //
     .                          'closest was: /cr/cr(3:) ''',
     .                          0,         CAUSE(2)                 )
               ELSE
 
                  CALL PREFIX ( '''./cr/cr(-3:)',
     .                          1,         CAUSE(2)                 )
                  CALL PREFIX ( TEMP ( BDIAGS:EDIAGS ),
     .                          0,         CAUSE(2)                 )
                  CALL PREFIX ( 'Extra words appear in the input ' //
     .                          'string that are not part of a '   //
     .                          'valid expression. I think you '   //
     .                          'may have been trying to supply '  //
     .                          'the optional expression:/cr/cr(3:)''',
     .                           0,         CAUSE(2)                 )
 
               END IF
 
            END IF
 
C
C           Whatever error code we got back, add 10000 so that this
C           routine will have its stamp on it to indicate we are second
C           level guessing at what went wrong.
C
            M2CODE = 10000 + T1CODE
 
         ELSE
 
C
C           Sorry, we couldn't guess why there was extra stuff in the
C           command.  Maybe just happy fingers.  Anyway, just say there
C           was extra stuff and hit the road.
C
            IF ( REASON ) THEN
               CAUSE(1) = 'The input string contains extra words that'//
     .                    ' are not recognized as part of a valid '   //
     .                    'command.'
 
               CALL M2MARK ( STRING, SBEG, QLSTNB(STRING), CAUSE(1) )
            END IF
 
            M2CODE = 10200
 
         END IF
 
      END IF
 
      RETURN
      END
