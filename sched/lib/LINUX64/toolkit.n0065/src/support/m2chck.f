C$Procedure      M2CHCK ( Meta-2, check a table of syntax definitions )
 
      SUBROUTINE M2CHCK ( STATMN, SYNKEY, SYNPTR, SYNVAL, ERROR )
      IMPLICIT NONE
 
C$ Abstract
C
C     Using a symbol table of syntax definition statement indexed by
C     initial keyword, determine if the input statement is syntactically
C     correct.
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
C     META-2 A command definition language and parser.
C
C$ Keywords
C
C     META-2
C     PARSING
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      CHARACTER*(*)         STATMN
      CHARACTER*(*)         SYNKEY ( LBCELL : * )
      INTEGER               SYNPTR ( LBCELL : * )
      CHARACTER*(*)         SYNVAL ( LBCELL : * )
      CHARACTER*(*)         ERROR  (          * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     STATMN     I   A statement to check for syntactic correctness.
C     SYNKEY     I   A symbol table of syntax definitions.
C     SYNPTR
C     SYNVAL
C     ERROR      O   Blank if STATMN correct, diagnosis otherwise.
C
C$ Detailed_Input
C
C     STATMN     is a string that is a candidate for a syntactically
C                correct statement.
C
C     SYNKEY     is a symbol table.  It is indexed by the initial
C     SYNPTR     keywords of META-2 syntax definition statements.
C     SYNVAL     This table is best prepared using the routine
C                M2INTS.
C
C$ Detailed_Output
C
C     ERROR      is an array of character strings that are used to
C                diagnose how well a STATMN matches one of the
C                syntax specificiations in the input symbol table.
C                If the STATMN is syntactically correct ERROR(1)
C                is returned as a blank.  Otherwise it is returned
C                with a diagnosis of why STATMN failed to be
C                syntactically correct.
C
C                Parsing of STATMN is usually accomplished by using
C                the various M2GET routines.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine can be used to compare a statement with a large
C     collection of syntax definitions provided all of the definitions
C     begin with a keyword.  To make use of this routine, you must first
C     prepare the symbol table.  The easiest way to to this is to use
C     the routine M2INTS.
C
C     To parse the input statement once it has been determine that it
C     is syntactically correct, one can use the M2GET routines to locate
C     the various substring corresponding to the meaning of STATMN.
C
C$ Examples
C
C     Typical useage looks like this:
C
C           IF ( FIRST ) THEN
C
C              CALL     M2INTS ( NSYN, SYNKEY, SYNPTR, SYNVAL )
C              FIRST = .FALSE.
C
C           END IF
C
C           CALL M2CHCK ( STATMN, SYNKEY, SYNPTR, SYNVAL, ERROR )
C
C           IF ( ERROR(1) .NE. ' ' ) THEN
C              CALL PREFIX ( 'MYNAME:', 1, ERROR(1) )
C              RETURN
C           END IF
C
C           Still here?  Determine what the string actually meant.
C
C$ Restrictions
C
C     To make use of STATMN for parsing with the M2GET routines, you
C     should not alter it after the call to M2CHCK until you have
C     finished parsing it.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber     (JPL)
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
C-    Beta Version 1.0.0, 4-MAY-1992 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Check a statement against a set of syntax definitions
C
C-&
 
 
 
C
C     Spicelib functions
C
      INTEGER               CARDI
      LOGICAL               RETURN
 
C
C     Local Variables
C
      INTEGER               ROOM
      PARAMETER           ( ROOM = 10 )
 
      INTEGER               BEST   ( LBCELL : ROOM )
      INTEGER               SCORES ( LBCELL : ROOM )
 
      INTEGER               B
      INTEGER               BST
      INTEGER               CUTOFF
      INTEGER               E
      INTEGER               I
      INTEGER               LOOKAT
      INTEGER               MXSCOR
      INTEGER               N
      INTEGER               PTR
 
      LOGICAL               FOUND
 
      LOGICAL               UNKNWN
 
      CHARACTER*(32)        KEYWRD
      CHARACTER*(160)       MSSG
 
 
 
 
 
      IF ( RETURN() ) THEN
         ERROR(1) = 'M2CHCK: The function RETURN was set to .TRUE. '  //
     .              'This situation is not supposed to happen.'
         RETURN
      END IF
 
C
C     Initialize the cell BEST and SCORES.
C
      CALL SSIZEI ( ROOM, BEST   )
      CALL SSIZEI ( ROOM, SCORES )
 
C
C     Get the first word of the input string.
C
      CALL FNDNWD ( STATMN,  1,  B, E   )
      CALL UCASE  ( STATMN(B:E), KEYWRD )
 
C
C     Find the syntax templates that match the first word of the
C     command.
C
      CALL SYPTRC ( KEYWRD, SYNKEY,
     .                      SYNPTR,
     .                      SYNVAL, PTR, N, FOUND )
 
C
C     If we didn't find our word, then we look for a word that
C     comes close spelling-wise
C
      IF ( .NOT. FOUND ) THEN
 
         CUTOFF = 70
         CALL BESTWD ( KEYWRD, SYNKEY, CUTOFF, BEST, SCORES, MSSG )
 
         IF ( CARDI(BEST) .EQ. 0 ) THEN
            UNKNWN = .TRUE.
         ELSE IF ( SCORES(1) .LT. 50 ) THEN
            UNKNWN = .TRUE.
         ELSE
            UNKNWN = .FALSE.
         END IF
 
         IF ( UNKNWN ) THEN
 
            ERROR (1) = 'Sorry but I didn''t recognize the word'
            CALL SUFFIX ( KEYWRD, 1, ERROR(1) )
            CALL SUFFIX ( 'as the beginning of any valid statement. ',
     .                     1,        ERROR(1) )
            RETURN
 
         END IF
 
C
C        Still here? fetch the set of likely syntax statements to check.
C
         MXSCOR = 0
 
         DO I = 1, CARDI(BEST)
            IF ( SCORES(I) .GT. MXSCOR )  THEN
               MXSCOR = SCORES(I)
               LOOKAT = I
            END IF
         END DO
 
         KEYWRD = SYNKEY( BEST(LOOKAT) )
 
         CALL SYPTRC ( KEYWRD, SYNKEY,
     .                         SYNPTR,
     .                         SYNVAL, PTR, N, FOUND )
 
      END IF
 
C
C     Until we find out otherwise, we shall assume that we have
C     a syntactically correct input statement.
C
      CALL META_2( STATMN, SYNVAL(PTR), N, SYNVAL(LBCELL), BST, ERROR )
 
 
      IF ( ERROR(1) .NE. ' ' ) THEN
         CALL PREFIX ( 'M2CHCK:', 1, ERROR(2) )
      END IF
 
      RETURN
      END
