 
C$Procedure      BESTWD ( Perform a spell match against a set of words )
 
      SUBROUTINE BESTWD ( WORD, KNOWN, CUTOFF, BEST, SCORES, MSSG )
      IMPLICIT NONE
 
C$ Abstract
C
C     Given a word and a list of known words, return those of the list
C     closest to the word along with a diagnostic message.
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
C     COMPARE
C     PARSING
C     UTILITY
C     WORD
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      CHARACTER*(*)         WORD
      CHARACTER*(*)         KNOWN  ( LBCELL : * )
      INTEGER               CUTOFF
      INTEGER               BEST   ( LBCELL : * )
      INTEGER               SCORES ( LBCELL : * )
      CHARACTER*(*)         MSSG
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     WORD       I   Word to compare against a list of known words.
C     KNOWN      I   List of known words.
C     CUTOFF     I   Fine tuning value.  A "good" value is 70.
C     BEST       O   Indices of the best matches.
C     SCORES     O   Scores of the best matches.
C     MSSG       O   Explanatory message.
C
C$ Detailed_Input
C
C     WORD       is any word.  Typically it will be a word that was not
C                equal to some "known" word and for which one wants to
C                find the "closest" known word. ONLY the first 32
C                characters of WORD are regarded as being significant.
C
C     KNOWN      is a cell containing "known" words.  These might be
C                keywords from a command, filenames, directories, etc.
C                From these a collection are found that most closely
C                match WORD.
C
C     CUTOFF     is an integer used to "fine tune" the matching
C                between WORD and the words in KNOWN.
C
C                CUTOFF should be between 0 and 100.  Values of
C                CUTOFF greater than 100 will effectively disable
C                the more detailed error diagnostics.  Values
C                less than 0 will simply cause the routine to work
C                harder with no gain in information.
C
C                A "good" value for CUTOFF is 70.  You will probably
C                want your input value to be close to this.
C
C$ Detailed_Output
C
C      BEST      BEST is a cell. On output BEST contains the indices of
C                the items in KNOWN that yielded the maximum comparison
C                score when compared to word. BEST will never contain
C                more than 10 indices. (It will rarely contain more
C                than two.)
C
C      SCORE     SCORE is a cell. SCORE is assumed to be declared the
C                same size as BEST.  On output SCORE(I) contains the
C                score that measures the similarity between between
C                KNOWN(BEST(I)) and WORD.
C
C                If WORD should happen to equal one of the KNOWN words
C                SCORE will be returned with a value of 1000.  Otherwise
C                it will be returned with a value between 0 and 100.
C                The higher the value of SCORE(I) the greater the
C                similarity between WORD and KNOWN(BEST(I)).
C
C                By comparing the values in SCORE with CUTOFF you can
C                determine how good a particular match is.
C                If SCORE is at least as big as CUTOFF the match is
C                regarded as a good one.  An attempt will have been
C                made at giving detailed diagnostics on the difference
C                between WORD and the best matching KNOWNs.
C
C      MSSG      is a message that identifies those KNOWN words that
C                best match WORD.  Moreover, if detailed diagnostics
C                are available, they will be reported in MSSG.
C
C$ Error_Handling
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C      This routine can be used to help a program recover from common
C      typing and spelling mistakes.  When a word is not recognized, it
C      is possible (perhaps likely) that a keystroke or two went awry in
C      the typing of the word.  If the list of legitimate words is
C      available, the unrecognized word can be compared with the
C      recognized words.  In this way, the one or ones that most nearly
C      resemble the unrecognized word can be identified.  From there the
C      program may point out the likely problem, offer to fix it, fix it
C      and continue (or any subset of these).  Thus to some extent the
C      program can do what you meant, not what you typed.
C
C      To measure the similarity between two words several techniques
C      are employed.  The first of these just compares the letter
C      sets of the two words.  For example the letter sets for the
C      words 'SIMILARITY' and 'SIMILITUDE' are
C
C         {A1, I1, I2, I3, L1, M1, R1, S1, T1, Y1 }
C
C      and
C
C         {E1, I1, I2, I3, L1, M1, S1, T1, U1 }
C
C      (Note that repeated letters are regardeds as distinct.)
C
C      By examining the symmetric difference between these two sets
C      one can get a measure of the how close the two words are.
C      The method used to compute the score will yield a value of
C      75 or higher for pairs of words whose letter sets have
C      a symmetric difference of 2 or fewer letters.
C
C      This does a good job of separating words such as
C      'CANADA' and 'ILLINOIS'.  However, it fails completely to
C      separate the words 'TRIANGLE', 'INTEGRAL', 'RELATING' and
C      'ALTERING'.  These four words all have the same letter sets.
C
C      Thus for words that compare well on the basis of letter sets
C      a second (more time consuming) comparison is made to see if
C      the words preserve the relative letter order.  In this step
C      each word is used to construct a sequence of templates
C      that are then matched against the other.  A tally of the
C      hits to misses is kept.  The roles of the two words are then
C      reversed and another tally computed.  The average of these
C      two scores is given to the word pair.
C      This is best illustrated with a simple example.
C
C      Consider the words ANGER and RANGE.
C
C      ANGER will be used to construct the 10 templates:
C
C         *A*N*, *A*G*, *A*E*, *A*R*, *N*G*,
C         *N*E*, *N*R*, *G*E*, *G*R*, *E*R*
C
C      Six of these match RANGE, namely
C
C         *A*N*, *A*G*, *A*E*, *N*G*, *N*E*, *G*E*, *E*R*
C
C      Next the 4 templates
C
C         *AN*,  *NG*, *GE*, *ER*
C
C      will be compared with RANGE,  The first three match.  Each
C      of these matches are "extra matches" that are added on to
C      the first 6 matches.  The score for ANGER to RANGE is
C
C        100 * MIN{1,(total matches / numer of templates of form *x*y*)}
C      = 100 * MIN{1, 9/10 }
C      = 90
C
C      The method extends in the obvious way to longer and shorter
C      words than ANGER and RANGE. As can be seen, this method of
C      comparing one word against another, requires not only the
C      correct letters to be present but they must also be in the
C      correct relative order.  Note that a perfect score of 100
C      does not mean the words are the same.  For example
C
C         AEAE and EAEA
C
C      yield an identical set of templates and hence have a matching
C      score of 100.  However, if both words have no letters repeated,
C      a score of 100 implies that the words are in fact the same.
C
C      If both methods of scoring exceed the value of CUTOFF, an
C      attempt is made to determine the exact difference between the
C      two words. The recognizable differences are: transposition of
C      two letters, a letter missing, a letter mistyped, an extra
C      letter. Thus CUTOFF allows the user to tune the circumstances
C      underwhich a attempts will be made to perform detailed
C      diagnosis of the the difference between a pair of words.
C
C      Empirically, it has been found that two words match up well if
C      both methods of scoring yield values of at least 70. This
C      is the recommended value for CUTOFF.
C
C      If both methods of scoring above yield values that exceed CUTOFF,
C      the two scores are averaged to give the score reported in SCORE.
C      If they do not both exceed CUTOFF but the average does, then
C      the score returned is CUTOFF-1.
C
C      CUTOFF can also be used as your means of determining how good
C      a match was.  Words with matching scores at least CUTOFF are
C      regarded as "good" matches, otherwise the match is viewed as
C      "poor."
C
C$ Examples
C
C      Suppose that
C
C      CUTOFF = 70
C      KNOWN  = 'ALTITUDE',      'CONTRACT',
C               'APPLE',         'INTEGRATE',
C               'LONGITUDE',     'EXTRACT',
C               'JUPITER',       'LATITUDE',
C               'EXPAND',        'SATURN',
C               'MERIDIAN',      'SHIFT',
C               'URANUS',        'ELEVATION',
C               'EPOCH',         'NEPTUNE',
C               'ASCENSION',     'DELTA',
C               'PLUTO',         'DECLINATION',
C               'COMPLEMENT'
C
C      If    WORD    = 'APPLY'   then     BEST(0)        = 1
C                                         KNOWN(BEST(1)) = 'APPLE'
C                                         SCORE(     1 ) = 89
C
C
C      If    WORD    = 'X'       then     BEST(0)        = 2
C                                         KNOWN(BEST(1)) = 'EXTRACT'
C                                         SCORE(     1 ) = 7
C
C                                         KNOWN(BEST(2)) = 'EXPAND'
C                                         SCORE(     2 ) = 8
C
C
C      If    WORD    = 'NEMPTUNE'  then   BEST(0)        = 1
C                                         KNOWN(BEST(1)) = 'NEPTUNE'
C                                         SCORE(     1 ) = 95
C
C
C      If    WORD    = 'ELATION'   then   BEST(0)        = 1
C                                         KNOWN(BEST(1)) = 'ELEVATION'
C                                         SCORE(     1 ) = 94
C
C
C      If    WORD    = 'QQQ'       then   BEST(0)        = 0
C
C
C      If    WORD    = 'COEMPLMENT' then  BEST(1)        = 1
C                                         KNOWN(BEST(1)) = 'COMPLEMENT'
C                                         SCORE(     1 ) = 100
C
C
C$ Restrictions
C
C     SCORES must be declared to be at least as large as BEST.
C
C     At most 10 best indices will be returned in BEST.
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
C     Version B1.0.0, 12-APR-1988 (WLT) (IMU)
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               CARDI
      INTEGER               CARDC
      INTEGER               SIZEI
      INTEGER               MATCHC
      INTEGER               MATCHO
      INTEGER               LSTLTI
      INTEGER               QRTRIM
 
      LOGICAL               EQSTR
 
C
C     Local variables
C
      INTEGER               BSIZE
      PARAMETER           ( BSIZE = 10 )
 
      INTEGER               BSCORE (BSIZE)
      INTEGER               ITEM   (BSIZE)
      INTEGER               HELP   (BSIZE)
      INTEGER               USIZE
      INTEGER               NBEST
 
      CHARACTER*80          NTH
 
      INTEGER               NKNOWN
      INTEGER               CSCORE
      INTEGER               OSCORE
      INTEGER               MAXSC
 
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               L
 
      INTEGER               LOC
      CHARACTER*16          TRANS
 
      LOGICAL               HIT
      INTEGER               HITS
      CHARACTER*32          CASE
      CHARACTER*(32)        MYWD
 
      INTEGER               TRIES
      INTEGER               LENGTH
 
 
C
C     First determine how many words we have to compare with
C     and the amount of room for reporting indices of "good"
C     matches.
C
      NKNOWN = CARDC( KNOWN )
      NBEST  = SIZEI( BEST  )
 
 
C
C     This routine only works on words of 32 or fewer characters
C
      MYWD   = ' '
      MYWD   = WORD
 
 
C
C     USIZE refers to the amount of space we will actually
C     use in the buffers that store the best MATCHC scores and
C     the associated KNOWN word.
C
      USIZE  = MIN  ( BSIZE, NKNOWN, NBEST )
 
      DO I = 1, USIZE
         BSCORE(I) = 0
         ITEM  (I) = 0
         HELP  (I) = 0
         SCORES(I) = 0
      END DO
 
C
C     First apply MATCHC against each of the KNOWNs and keep the
C     top USIZE words that match.
C
 
      DO I = 1, NKNOWN
 
 
 
C
C        Just in case, see if we have an exact match.
C
         IF ( EQSTR (MYWD,KNOWN(I)) ) THEN
 
            CALL SCARDI ( 1, BEST   )
            CALL SCARDI ( 1, SCORES )
 
            BEST  (1) = I
            SCORES(1) = 1000
 
            CALL INTORD ( I,   NTH )
            CALL LCASE  ( NTH, NTH )
 
            MSSG = MYWD
 
            CALL SUFFIX ( 'is equal to the ',        1,  MSSG )
            CALL SUFFIX ( NTH,                       1,  MSSG )
            CALL SUFFIX ( ' known word.',            1,  MSSG )
 
            RETURN
 
         END IF
 
         CSCORE  = MATCHC ( MYWD,    KNOWN(I)        )
         J       = LSTLTI ( CSCORE , USIZE,   BSCORE )
 
         DO K = 1, J-1
            BSCORE(K) = BSCORE(K+1)
            ITEM  (K) = ITEM  (K+1)
         END DO
 
         IF ( J .GT. 0 ) THEN
            BSCORE(J) = CSCORE
            ITEM  (J) = I
         END IF
 
      END DO
 
C
C     Now for the top USIZE matches, perform a MATCHO comparison.
C     If we get a match of CUTOFF or higher.  Run MATCHE against it
C     to see if we can guess at what went wrong.
C
C     So far our best score is 0 and we haven't HIT any good matches.
C
      MAXSC = 0
      HITS  = 0
 
      DO I = 1, USIZE
 
C
C        Only examine items that have legitimate indices.
C
         IF ( ITEM(I) .NE. 0 ) THEN
 
            BSCORE(I) = MATCHO(MYWD,      KNOWN(ITEM(I)) )
            CSCORE    = MATCHC(MYWD,      KNOWN(ITEM(I)) )
            MAXSC     = MAX   (BSCORE(I), MAXSC          )
 
            IF (       ( BSCORE(I) .GE. CUTOFF )
     .           .AND. ( CSCORE    .GE. CUTOFF ) ) THEN
 
C
C              We've HIT a good match.
C
               HITS    = HITS + 1
 
C
C              See if the problem with this word can be diagnosed
C              with MATCHE.
C
               CALL MATCHE ( MYWD, KNOWN(ITEM(I)), TRANS, LOC )
 
C
C              If a diagnosis can be performed on this item, we
C              say that HELP is available at level 2.  Otherwise
C              since we have a good match anyway we say HELP is
C              available at level 1.
C
 
               IF ( TRANS .NE. 'NONE' ) THEN
                  HELP(I) = 2
               ELSE
                  HELP(I) = 1
               END IF
 
            END IF
 
         END IF
 
      END DO
 
C
C     If none of the words had a sufficiently high score, just
C     report those that had the maximum score.
C
      IF ( HITS .EQ. 0 ) THEN
C
C        Just report the item(s) that had the biggest score.
C
C        First see how many had the maximum score.
C
         DO I = 1, USIZE
            IF ( ITEM(I) .NE. 0 ) THEN
 
               IF ( BSCORE(I) .EQ. MAXSC ) THEN
                  HITS = HITS + 1
               END IF
 
            END IF
         END DO
 
C
C        If there were no KNOWN words that had letters in common
C        with MYWD, all of the elements of the array ITEM will be
C        zero and we will not have made any HITS against MAXSC.
C        There is nothing at all we can do in this case.
C
         IF ( HITS .EQ. 0 ) THEN
 
            MSSG = 'The word'
 
            CALL SUFFIX ( MYWD,                                1, MSSG )
            CALL SUFFIX ( 'has nothing in common with any of', 1, MSSG )
            CALL SUFFIX ( 'the words I can recognize.  If ',   1, MSSG )
            CALL SUFFIX ( 'this word was typed interactively,',1, MSSG )
            CALL SUFFIX ( 'you may want to see if your ',      1, MSSG )
            CALL SUFFIX ( 'fingers are over the correct keys.',1, MSSG )
 
            CALL SCARDI ( 0 , BEST   )
            CALL SCARDI ( 0 , SCORES )
 
            RETURN
 
         END IF
 
 
C
C        Still here.  Then we have at least some item that has
C        something in common with MYWD.  Set up a closing string so
C        that grammar will be correct.
C
         IF ( HITS .GT. 1 ) THEN
            CASE = 'my closest matches are: '
         ELSE
            CASE = 'my closest match is: '
         END IF
 
 
         MSSG = 'The word '''
 
         CALL SUFFIX ( MYWD,                                   1, MSSG )
         CALL SUFFIX ( ''' did not match up well with any of', 1, MSSG )
         CALL SUFFIX ( 'the words I was comparing against.',   1, MSSG )
         CALL SUFFIX ( 'However,',                             1, MSSG )
         CALL SUFFIX ( CASE,                                   1, MSSG )
 
C
C        Now append the list of KNOWN words that matched MYWD with
C        the highest score.
C
         HIT  = .FALSE.
         J    = 0
 
         DO I = 1, USIZE
 
            IF ( ITEM(I) .EQ. 0 ) THEN
 
C
C              don't do anything
C
 
            ELSE IF ( BSCORE(I) .EQ. MAXSC ) THEN
 
               J       = J + 1
 
               BEST(J) = ITEM(I)
               L       = QRTRIM( KNOWN(ITEM(I)) )
 
               IF ( HIT ) THEN
 
                  CALL SUFFIX ( ',  ''', 0, MSSG )
                  CALL SUFFIX ( KNOWN(ITEM(I))(1:L), 0, MSSG)
                  CALL SUFFIX ( '''', 0, MSSG )
 
               ELSE
 
                  HIT = .TRUE.
 
                  CALL SUFFIX ( '''',                1, MSSG )
                  CALL SUFFIX ( KNOWN(ITEM(I))(1:L), 0, MSSG )
                  CALL SUFFIX ( '''',                0, MSSG )
 
               END IF
 
               CALL SUFFIX ( '.', 0, MSSG )
 
            END IF
 
         END DO
 
C
C        Set the cardinality of the window of BEST indices.
C
         CALL SCARDI ( J, BEST )
 
      ELSE IF ( HITS .EQ. 1 ) THEN
 
C
C        There was just one KNOWN word for which there was a good
C        match.  Call MSPELD to produce a diagnosis of the problem
C        and record the index of the item.
C
         I = 1
 
         DO WHILE ( HELP(I) .EQ. 0 )
            I = I + 1
         END DO
 
         CALL MSPELD ( MYWD, KNOWN(ITEM(I)), MSSG )
 
         BEST(1) = ITEM(I)
         CALL SCARDI ( 1,    BEST                 )
 
      ELSE
 
C
C        There were at least two "good" words.  If any of them
C        could be diagnosed, then report them.  Otherwise
C        report only those that had a maximum MATCHO score.
C
         TRIES = 0
 
         DO I = 1, 5
 
            IF ( HELP(I) .EQ. 2 ) THEN
               TRIES = TRIES + 1
            END IF
 
         END DO
 
         IF ( TRIES .EQ. 0 ) THEN
 
C
C           None of the KNOWN words had diagnostics available.
C
            MSSG =  'Although a the spelling '                        //
     .              'error can''t be described in a simple '          //
     .              'way,  I have found the following '               //
     .              'words that may be what you were trying to '      //
     .              'say.  '
            J = 0
 
            DO I = 1, USIZE
 
               IF ( HELP(I) .NE. 0 ) THEN
 
                  CALL SUFFIX( '''',           2, MSSG )
                  CALL SUFFIX( KNOWN(ITEM(I)), 0, MSSG )
                  CALL SUFFIX( ''',',          0, MSSG )
 
                  J       = J + 1
                  BEST(J) = ITEM(I)
 
               END IF
 
            END DO
 
            CALL SCARDI ( J, BEST  )
 
            MSSG(QRTRIM(MSSG):QRTRIM(MSSG)) = ' '
 
         ELSE IF ( TRIES .EQ. 1 ) THEN
 
C
C            Only one of the KNOWN words had diagnostics available.
C
             DO I = 1, 5
 
                IF ( HELP(I) .EQ. 2 ) THEN
                   CALL MSPELD ( MYWD, KNOWN(ITEM(I)), MSSG)
                   BEST(1) = ITEM(I)
                END IF
             END DO
 
             CALL SCARDI( 1, BEST )
 
         ELSE
 
C
C           At least two of the KNOWN words had diagnostics available.
C           Report all of them.
C
            MSSG =  'The following '                                  //
     .              'common spelling mistakes may be the reason I '   //
     .              'did not recognize '
 
 
            CALL SUFFIX ( MYWD,                                1, MSSG )
            CALL SUFFIX ('.',                                  1, MSSG )
 
            LENGTH = LEN ( MSSG )
            J      = 0
 
            DO I = 1, USIZE
 
               IF ( HELP(I) .EQ. 2) THEN
                  IF ( QRTRIM(MSSG) .LT. LENGTH - 3 ) THEN
 
                     CALL MSPELD ( MYWD, KNOWN(ITEM(I)),
     .                             MSSG(QRTRIM(MSSG)+3:)      )
 
                     J       = J + 1
                     BEST(J) = ITEM(I)
 
                  END IF
               END IF
 
            END DO
 
            CALL SCARDI ( J, BEST )
 
         END IF
 
      END IF
 
 
 
C
C     As for the scores, we will report the average of the MATCHO and
C     MATCHC scores for the best matches.
C
      DO I = 1, CARDI(BEST)
 
         OSCORE    = MATCHO ( MYWD, KNOWN(BEST(I)) )
         CSCORE    = MATCHC ( MYWD, KNOWN(BEST(I)) )
 
         SCORES(I) = ( OSCORE + CSCORE ) / 2
 
         IF (      ( OSCORE .LT. CUTOFF )
     .       .OR.  ( CSCORE .LT. CUTOFF ) ) THEN
 
            SCORES (I) = MIN ( SCORES(I), CUTOFF - 1 )
 
         END IF
 
      END DO
 
      CALL SCARDI ( CARDI(BEST), SCORES )
 
      RETURN
      END
