C$Procedure      M2INTS (Meta 2 --- initialize syntax table)
 
      SUBROUTINE M2INTS ( NSYN, SYNKEY, SYNPTR, SYNVAL )
      IMPLICIT NONE
 
C$ Abstract
C
C     Construct a symbol table that uses the initial keywords of
C     META-2 syntax definitions as the keys to the same a set of
C     META-2 syntax definitions.
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
C     META-2 A language definition language and parser.
C
C$ Keywords
C
C     INITIALIZATION
C     PARSING
C     UTILITY
C
C$ Declarations
 
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               NSYN
 
      CHARACTER*(*)         SYNKEY ( LBCELL : * )
      INTEGER               SYNPTR ( LBCELL : * )
      CHARACTER*(*)         SYNVAL ( LBCELL : * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NSYN       I   The number of syntax definition statements.
C     SYNKEY     O   The key (names) portion of a symbol table.
C     SYNPTR     O   The pointer portion of a symbol table.
C     SYNVAL    I/O  The Meta-2 syntax statements.
C
C$ Detailed_Input
C
C     NSYN       is the number of syntax statements that will be
C                organized into an initial keyword based symbol table
C
C     SYNVAL     is a cell containing syntax definintion statements.
C                The defitions should be located at indices 1 through
C                NSYN.
C
C$ Detailed_Output
C
C     SYNKEY     is the names portion of a symbol table.  The names
C                in this array will be the initial keywords of the
C                syntax definition statments stored in SYNVAL.  Each
C                initial keyword will be associated with those
C                collection of definitions that begin with that keyword.
C
C     SYNPTR     is the pointer cell of the symbol table
C                SYNKEY, SYNPTR, SYNVAL
C
C     SYNVAL     is the input cell organized now as the values cell
C                of the symbol table SYNKEY, SYNPTR, SYNVAL.
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
C     This routine serves to initialize a syntax cell and list of
C     initial known keywords.  This is useful primarily for META2
C     languages that have all syntax definitions beginning with a
C     diverse set of keywords.  It is anticipated that users will
C     use this once in a module that accepts language statements.
C
C           if ( first ) then
C
C              first = .false.
C              call m2intp ( nsyn, synkey, synptr, synval )
C           end if
C
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     This routine is intended only for use with META-2 derived
C     languages whose syntax statements all begin with keywords.
C     It is assumed that all keywords are 32 or fewer characters
C     in length.
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
C
C     Initialize an intial keyword based META-2 syntax table
C-&
 
 
 
C
C     Spicelib functions.
C
      INTEGER               CARDC
 
C
C     Local variables.
C
      CHARACTER*(32)        KEYWRD
      CHARACTER*(32)        LSTKEY
 
      INTEGER               B
      INTEGER               E
      INTEGER               I
      INTEGER               PUT
 
 
C
C     Initialize the symbol table size attributes.
C
      CALL SSIZEC ( NSYN,   SYNKEY )
      CALL SSIZEI ( NSYN,   SYNPTR )
      CALL SSIZEC ( NSYN,   SYNVAL )
 
C
C     Just in case, left justify everything in the values cell
C     and set all of the pointer values to 0.
C
      DO I = 1, NSYN
         CALL LJUST ( SYNVAL(I), SYNVAL(I)  )
         SYNPTR(I) = 0
      END DO
 
C
C     Turn the collection of syntax definitions into an array ordered
C     by initial keyword (minus any labels).
C
      CALL M2SHLL ( NSYN, SYNVAL(1) )
C
C     Remove any duplicates including a blank at the beginning if
C     there is one.
C
      PUT       = 0
      SYNVAL(0) = ' '
      DO I = 1, NSYN
         IF ( SYNVAL(I) .NE. SYNVAL(I-1) ) THEN
            PUT         = PUT + 1
            SYNVAL(PUT) = SYNVAL(I)
         END IF
      END DO
 
      CALL SSIZEC ( NSYN,   SYNVAL )
      CALL SCARDC ( PUT,    SYNVAL )
 
C
C     Now we will construct the symbol table to go with this collection
C     of syntax definitions.
C
      LSTKEY = ' '
      PUT    =  0
 
      DO I = 1, CARDC(SYNVAL)
 
C
C        Get the first word, and trim off any attached label.  Note that
C        since this is supposed to be a keyword, there are no range
C        templates or qualifiers attached.
C
         CALL FNDNWD ( SYNVAL(I),      1,  B,  E )
         CALL M2TRIM ( SYNVAL(I)(B:E), KEYWRD    )
         CALL UCASE  ( KEYWRD,         KEYWRD    )
 
C
C        If this is a new keyword, put it into the list of keywords and
C        change the last keyword.
C
         IF ( KEYWRD .NE. LSTKEY ) THEN
            PUT         = PUT + 1
            SYNKEY(PUT) = KEYWRD
            LSTKEY      = KEYWRD
         END IF
 
C
C        Increment the value in the pointer array.
C
         SYNPTR(PUT) = SYNPTR(PUT) + 1
 
      END DO
 
C
C     Set the cardinality of the name and pointer cells.
C
      CALL SCARDC ( PUT, SYNKEY )
      CALL SCARDI ( PUT, SYNPTR )
 
C
C     Finally, blank out all of the non-used parts of the values cell.
C
      DO I = -5, -2
         SYNVAL(I) = ' '
      END DO
 
      RETURN
      END
