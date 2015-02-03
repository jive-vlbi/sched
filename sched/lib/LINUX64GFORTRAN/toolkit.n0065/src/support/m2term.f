C$Procedure      M2TERM (Find possible terminators of variable template)
 
      SUBROUTINE M2TERM ( TEMP, TERMS, INDXES )
      IMPLICIT NONE
 
C$ Abstract
C
C     Find those keywords that are initial keywords of group templates
C     or immediately follow such a template.
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
C     The META/2 Book.
C
C$ Keywords
C
C     PARSING
C     SEARCH
C     UTILITY
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      CHARACTER*(*)         TEMP
      CHARACTER*(*)         TERMS ( LBCELL : * )
      INTEGER               INDXES( LBCELL : * )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     TEMP       I   A META/2 template.
C     TERMS      O   Possible terminating keywords.
C     INDXES     O   Indices  of the beginnings of TERMS within TEMP.
C
C$ Detailed_Input
C
C     TEMP       A META/2 template.
C
C$ Detailed_Output
C
C     TERMS      These are those keywords that begin the simple
C                templates of the groups templates of TEMP, as well
C                as the keywords that immediately follow group
C                templates.
C
C     INDXES     Contains the indexes of the first characters of
C                each of the words in TERMS within TEMP.  Specifically,
C                if we let L = LASTNB(TERMS(I)) then
C                TERMS(I)( 1 : L ) = TEMP( INDXES(I) : INDXES(I) + L )
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Particulars
C
C      This is a utility routine to aid the general M2 pattern matching
C      routine ( M2CHCK ).  It determines
C
C         1.) initial keywords of simple templates of group templates;
C         2.) the keywords that immediately follow groups;
C         3.) the keywords that immediately follow unqualified @then
C             directives.
C
C      These keywords together with their indexes are loaded in the
C      order they appear in the template into the cells TERMS and
C      INDXES.  Additionally, the marker '}' is inserted in the
C      cell of keywords immediately following
C
C         1.) the last initial keyword of a group, provided the
C             group has a range template that is NOT of the form
C             (0:n) (where n is any integer).  The index associated
C             with such a marker is the index in the template of the
C             '}' that ends the group associated with the marker.
C
C         2.) any keyword that immediately follows a group.  It
C             is assigned the index of the first blank that follows
C             the keyword.
C
C         3.) any keyword that immediatly follows an unqualified @then
C             directive.  It is given the index of the first blank
C             following the keyword.
C
C         4.) after all keywords provided that the template does not
C             end with a qualified @then directive.  The marker is
C             assigned an index equal to the length of the template
C             plus 1.
C
C
C      The marker can be used to determine what keywords might end
C      a variable length template.
C
C
C$ Examples
C
C      Suppose that the template was
C
C      (0:1){ PLEASE }
C             SEND (1:7)@english (0:1){ AND @english }
C
C                  (1:1){ A         @english(MESSAGE|CHECK|LETTER)
C                       | MEMO (0:1)@english(NUMBER)      @int(1:)
C                       | THE       @english(SCHEDULE|PROPOSAL)
C                       | HOME
C                       | FLOWERS }
C
C      Then the cells TERMS and INDXES (assuming that spaces have been
C      compressed down to 1 between words) would be returned as:
C
C      TERMS        INDXES
C      -------      -------
C      PLEASE          8
C      SEND           17
C      }              21
C      AND            43
C      A              65
C      MEMO          101
C      THE           139
C      HOME          173
C      FLOWERS       180
C      }             188
C      }             189
C
C$ Restrictions
C
C     It is expected that any template input to this routine satisfies
C     the rules required of META/2 templates.
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
C     Beta Version 1.0.0, 10-MAY-1987 (WLT) (IMU)
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               UPTO
      LOGICAL               MATCHW
      LOGICAL               M2KEYW
      INTEGER               SIZEC
      INTEGER               SIZEI
      INTEGER               POS
      INTEGER               POSR
      INTEGER               NCPOS
 
C
C     Local variables
C
      LOGICAL               MORE
      LOGICAL               DOTHEN
      LOGICAL               GROUP
 
      INTEGER               BEG
      INTEGER               NEXTT
      INTEGER               NEXTG
      INTEGER               NEXT
      INTEGER               MARK
      LOGICAL               MRKEND
      INTEGER               B
      INTEGER               E
      INTEGER               COUNT
      INTEGER               ROOM
 
      INTEGER               D1
      INTEGER               D2
 
 
      SAVE
 
 
      ROOM = MIN ( SIZEC(TERMS), SIZEI(INDXES) )
 
      CALL SCARDC ( 0, TERMS  )
      CALL SCARDI ( 0, INDXES )
 
C
C     Just look through the string and located the appropriate keywords.
C     First see if there are any group templates.
C
      BEG    = 1
      COUNT  = 0
      MORE   = .TRUE.
 
C
C     NEXT will point to the next group template so long as there are
C     more to find.
C
      DO WHILE ( MORE )
 
 
         NEXTG = UPTO ( TEMP, '){ ',     BEG )
         NEXTT = UPTO ( TEMP, ' @then ', BEG )
 
         IF      ( NEXTG .LT. NEXTT ) THEN
 
            GROUP  = .TRUE.
            DOTHEN = .FALSE.
            NEXT   =  NEXTG
 
         ELSE IF ( NEXTT .LT. NEXTG ) THEN
 
            DOTHEN = .TRUE.
            GROUP  = .FALSE.
            NEXT   =  NEXTT
 
         ELSE
 
            DOTHEN = .FALSE.
            GROUP  = .FALSE.
            MORE   = .FALSE.
 
         END IF
 
 
 
         IF      ( GROUP ) THEN
 
C
C           Find the beginning of the range template and see if
C           it has the form (0:*).  If it has that form we will
C           not want to mark the end of the group when we finish
C           with it.
C
            B      =      POSR  ( TEMP, '(', NEXTG ) + 1
            MRKEND =      NCPOS ( TEMP, '0', B     )
     .               .NE. POS   ( TEMP, ':', B     )
 
C
C           Find the end of the next group template and set BEG
C
            BEG    = POS ( TEMP, '}', NEXTG ) + 1
            MARK   = BEG - 1
 
            IF ( BEG .EQ. 1 ) THEN
               CALL CHKIN  ( 'M2TERM' )
               CALL SETMSG ( 'A switch was begun, but never ended.')
               CALL SIGERR ('SPICE(META2DEFERR)')
               CALL CHKOUT ( 'M2TERM' )
               RETURN
            END IF
 
C
C           Locate the first keyword of the group template.
C
            CALL FNDNWD ( TEMP, NEXTG, B, E )
 
            IF ( COUNT .GT. ROOM ) THEN
               CALL CHKIN  ( 'M2TERM' )
               CALL SETMSG ( 'There are too many possible terminating '
     .         //            'keywords. '             )
               CALL SIGERR ('SPICE(META2TOOMANYKEYS)' )
               CALL CHKOUT ( 'M2TERM' )
               RETURN
            END IF
 
            COUNT            = COUNT + 1
            TERMS  ( COUNT ) = TEMP(B:E)
            INDXES ( COUNT ) = B
C
C           See if there are anymore simple templates in the this
C           group template ( they will all be preceeded by ' | '.
C
            NEXTG        = E
            NEXTG        = POS ( TEMP(1:BEG), ' | ', NEXT ) + 2
 
            DO WHILE ( NEXTG .GE. 3 )
 
C
C              Locate the next keyword.
C
               CALL FNDNWD ( TEMP, NEXTG, B, E )
 
C
C              Take care of any errors that might occur.
C
               IF ( B .EQ. 0 ) THEN
                  CALL CHKIN  ( 'M2TERM' )
                  CALL SETMSG ( 'An improperly composed META/2 '
     .            //            'switch was encountered.'   )
                  CALL SIGERR ('SPICE(META2DEFERR)')
                  CALL CHKOUT ( 'M2TERM' )
                  RETURN
               END IF
 
               IF ( COUNT .GE. ROOM ) THEN
                  CALL CHKIN  ( 'M2TERM' )
                  CALL SETMSG ( 'There are too many possible '
     .            //            'terminating keywords. '  )
                  CALL SIGERR ( 'SPICE(META2TOOMANYKEYS)' )
                  CALL CHKOUT ( 'M2TERM' )
                  RETURN
               END IF
 
C
C              Put the keyword on the list and note its string position.
C
               COUNT            = COUNT + 1
               TERMS  ( COUNT ) = TEMP(B:E)
               INDXES ( COUNT ) = B
               NEXTG            = E
               NEXTG            = POS ( TEMP(1:BEG), ' | ', NEXTG ) + 2
 
            END DO
 
C
C           If the group template just processed DID NOT have a range
C           template of the form (0:*%), put the marker '}' into the
C           list of keywords.
C
            IF ( MRKEND ) THEN
               COUNT            = COUNT + 1
               TERMS  ( COUNT ) = '}'
               INDXES ( COUNT ) = MARK
            END IF
 
C
C           We are out of initial keywords in the group. Get the next
C           word and see if it is a keyword or the beginning of
C           another group template.
C
            CALL FNDNWD ( TEMP, BEG, B, E )
 
 
         ELSE IF ( DOTHEN ) THEN
 
            BEG    = NEXT + 6
            CALL FNDNWD ( TEMP, BEG, B, E )
 
         END IF
 
         IF      ( .NOT. MORE ) THEN
 
C
C           Don't do anything, just get ready to drop through the loop.
C
 
         ELSE IF ( B .EQ. 0 ) THEN
C
C           We are out of template
C
            MORE           = .FALSE.
 
            CALL SCARDC ( COUNT, TERMS  )
            CALL SCARDI ( COUNT, INDXES )
 
 
         ELSE IF ( MATCHW ( TEMP(B:E), '(%*:%*){', '*', '%' ) ) THEN
 
C
C           Do nothing, this will all be taken care of later.
C
 
         ELSE IF ( TEMP(B:E) .EQ. '@then' )                     THEN
C
C           Don't do anything, we'll get back to this in a moment.
C
 
         ELSE IF ( MATCHW ( TEMP(B:E), '@then(%*)', '*', '%' ) ) THEN
 
C
C           That's it.  I quit.
C
            CALL SCARDC ( COUNT, TERMS  )
            CALL SCARDI ( COUNT, INDXES )
            MORE = .FALSE.
 
         ELSE
 
C
C           Get rid of any beginning range template. (If there is a
C           range template we just dump the values into D1 and D2
C           and never use them.)
C
            CALL M2BEGR ( TEMP, B, E, D1, D2 )
 
            IF ( B .GT. E ) THEN
 
C
C              do nothing
C
 
            ELSE IF ( M2KEYW( TEMP(B:E) ) ) THEN
 
               COUNT            = COUNT + 1
               TERMS  ( COUNT ) = TEMP(B:E)
               INDXES ( COUNT ) = B
               BEG              = E     + 1
 
               COUNT            = COUNT + 1
               TERMS  ( COUNT ) = '}'
               INDXES ( COUNT ) = BEG
 
            END IF
 
         END IF
 
         GROUP  = .FALSE.
         DOTHEN = .FALSE.
 
      END DO
 
C
C     Set the cardinality and return
C
      CALL SCARDC ( COUNT, TERMS  )
      CALL SCARDI ( COUNT, INDXES )
 
      RETURN
      END
