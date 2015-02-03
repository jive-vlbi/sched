 
 
C$Procedure UNITP ( Determine whether a string represents units)
 
      LOGICAL FUNCTION UNITP ( STRING )
      IMPLICIT NONE
 
C$ Abstract
C
C     Determine whether or not a string represents the units associated
C     with a physical quantity.
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
C     PARSING
C     UNITS
C     UTILITY
C
C$ Declarations
 
      CHARACTER*(*)         STRING
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     STRING     I   potentially, units describing a physical quantity
C
C     The function returns .TRUE. if the string represents some physical
C     units.  Otherwise it returns false.
C
C$ Detailed_Input
C
C     STRING     a string that potentially represents the units
C                associated with some physical quantity.  For
C                example KM/SEC.  A string represents a unit if
C                it consists of numbers and recognized
C                primitive units (of length, angle, mass, time or
C                charge) connected in a "sensible" way with
C                operations of multiplication, division and
C                exponentiation.
C
C$ Detailed_Output
C
C     UNITP      returns as TRUE if the string satisfies the following
C                rules.
C
C                1) All maximal substrings of STRING that do not contain
C                   any of the character '(', ')', '*', '/' are
C                   recognized as numbers or units of angle, length,
C                   time, mass or charge.
C
C                2) The string is a properly formed multiplicative
C                   expression.
C
C                3) At least one physical unit is present in the string.
C
C                4) No physical units appear in an exponent
C                   subexpression.
C
C                If these conditions are not met, the function returns
C                FALSE.
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
C     This routine examines a string to determine whether or not
C     it represents the units attached to a physical quantity.
C
C     Units are created by multiplicatively combining primitive units
C     of time, length, angle, charge and mass together with numeric
C     constants.
C
C$ Examples
C
C     Below are some sample strings and the response of UNITP
C     when applied to these strings.
C
C     String                 Value of UNITP
C     ----------------       --------------
C     KM                     T
C     KM/SEC                 T
C     KM**3/SEC**2           T
C     (KM**(SEC**-2))        F   ( a unit appears in the exponent )
C     (KM)/SEC               T
C     (KM/SEC                F   ( parentheses are unbalanced )
C     (KM/*(7*DAYS)          F   ( /* is not a legitimate operation )
C     12*7                   F   ( no physical units appear )
C     3*KG                   T
C     AU/(100*DAYS)          T
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
C
C$ Version
C
C-    Beta Version 1.0.0, 10-APR-1990 (WLT)
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               BSRCHC
      EXTERNAL              SCAN 
 
C
C     Parameters
C
      INTEGER               NMARKS
      PARAMETER           ( NMARKS = 6 )
 
      INTEGER               ROOM
      PARAMETER           ( ROOM = 32 )
 
C
C     Local variables
C
      CHARACTER*(2)         OP    ( NMARKS )
 
      DOUBLE PRECISION      VALUE
 
      INTEGER               B
      INTEGER               BEG    ( ROOM )
      INTEGER               BLANK
      INTEGER               CLASS
      INTEGER               DIV
      INTEGER               E
      INTEGER               END    ( ROOM )
      INTEGER               EXP
      INTEGER               EXPLEV
      INTEGER               I
      INTEGER               IDENT  ( ROOM )
      INTEGER               LASTTK
      INTEGER               MULT
      INTEGER               LPAREN
      INTEGER               NEST
      INTEGER               NOP
      INTEGER               NTOKNS
      INTEGER               OPLEN ( NMARKS )
      INTEGER               RPAREN
C
C       Here is the range of       Character      ASCII code
C       initial characters that    ---------      ----------
C       will be used by the        ' '             32
C       "known" marks.             '('             40
C                                  ')'             41
C                                  '*'             42
C                                  '/'             47
C
C     So the required number of pointers is 47 - 32 + 5 = 20.
C
      INTEGER               OPPTR ( 20 )
      INTEGER               START
 
      LOGICAL               EXPGRP
      LOGICAL               FIRST
      LOGICAL               KNOWN
      LOGICAL               PHYSCL
 
 
C
C     Saved variables
C
 
      SAVE                  BLANK
      SAVE                  FIRST
      SAVE                  NOP
      SAVE                  OP
      SAVE                  OPLEN
      SAVE                  OPPTR
      SAVE                  LPAREN
      SAVE                  RPAREN
      SAVE                  MULT
      SAVE                  DIV
      SAVE                  EXP
 
C
C     Initial values
C
      DATA                  FIRST   / .TRUE. /
      DATA                  NOP     /  6     /
      DATA                  OP      / ' ', '(', ')', '*', '**', '/' /
 
 
 
 
 
C
C     On the first pass through this routine, set up the stuff
C     required for scanning the input string.
C
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         CALL SCANPR ( NOP, OP, OPLEN, OPPTR )
 
         BLANK  = BSRCHC ( ' ',  NOP, OP )
         LPAREN = BSRCHC ( '(',  NOP, OP )
         RPAREN = BSRCHC ( ')',  NOP, OP )
         MULT   = BSRCHC ( '*',  NOP, OP )
         EXP    = BSRCHC ( '**', NOP, OP )
         DIV    = BSRCHC ( '/',  NOP, OP )
 
      END IF
 
C
C     To get started we will assume that the last token (before we
C     started looking at the string) was an introductory left
C     parenthesis.
C
      LASTTK =  LPAREN
      NEST   =  0
      PHYSCL = .FALSE.
      EXPGRP = .FALSE.
 
      START  = 1
 
      CALL  SCAN   ( STRING,
     .               OP,     OPLEN, OPPTR, ROOM,   START,
     .               NTOKNS, IDENT, BEG,   END            )
 
      DO WHILE ( NTOKNS .GT. 0 )
 
         DO I = 1, NTOKNS
 
C
C           Look at the identity of the next token ...
C
            IF      ( IDENT(I) .EQ. 0      ) THEN
 
C
C              A non-recognized item cannot follow a right parenthesis
C              or a non-recognized item.
C
               IF (      ( LASTTK .EQ. RPAREN )
     .              .OR. ( LASTTK .EQ. 0      ) ) THEN
                  UNITP = .FALSE.
                  RETURN
               END IF
 
C
C              So far, so good.  Determine whether this object is
C              a recognized unit or number.
C
               B = BEG(I)
               E = END(I)
 
               CALL FNDUCV ( STRING(B:E), KNOWN, CLASS, VALUE )
 
C
C              If it wasn't recognized we don't have a unit.
C
               IF ( .NOT. KNOWN ) THEN
                  UNITP = .FALSE.
                  RETURN
               END IF
 
C
C              We also need to make sure we don't have anything of
C              the form **UNIT or **( ... UNIT ... ) where UNIT is a
C              physical unit.
C
               IF ( CLASS  .GT. 0   ) THEN
 
                  IF (     ( LASTTK .EQ. EXP  )
     .                .OR. ( EXPGRP           ) ) THEN
 
                     UNITP = .FALSE.
                     RETURN
 
                  END IF
 
               END IF
 
C
C              Finally, we need to keep track of whether or not
C              we've seen a physical unit.
C
               PHYSCL = PHYSCL .OR. (CLASS .GT. 0)
 
            ELSE IF ( IDENT(I) .EQ. RPAREN ) THEN
 
C
C              A right parenthesis can only follow a right parenthesis,
C              a unit or a number.
C
               IF (       ( LASTTK .NE. 0      )
     .              .AND. ( LASTTK .NE. RPAREN )  ) THEN
 
                  UNITP = .FALSE.
                  RETURN
 
               END IF
 
               NEST = NEST - 1
 
            ELSE IF (      ( IDENT(I) .EQ. EXP  )
     .                .OR. ( IDENT(I) .EQ. MULT )
     .                .OR. ( IDENT(I) .EQ. DIV  ) ) THEN
 
C
C              An arithmetic operation can only follow a right
C              parenthesis, a unit or a number.
C
               IF (       ( LASTTK .NE. RPAREN )
     .              .AND. ( LASTTK .NE. 0      ) ) THEN
                  UNITP = .FALSE.
                  RETURN
               END IF
 
            ELSE IF ( IDENT(I) .EQ. LPAREN ) THEN
 
C
C              A left parenthesis must be the first thing in the
C              string or follow one of the following:
C
C                    '(', '*', '**', '/'
C
C              (Note by construction the last token prior to the
C              beginning of the string was '(' ).  If this is _not_
C              the case then this is not a unit.
C
               IF (       ( LASTTK .NE. LPAREN )
     .              .AND. ( LASTTK .NE. MULT   )
     .              .AND. ( LASTTK .NE. DIV    )
     .              .AND. ( LASTTK .NE. EXP    ) ) THEN
 
                  UNITP = .FALSE.
                  RETURN
 
               END IF
 
C
C              If the last token was exponentiation (and we were not
C              already in some exponentiation group), we can't have
C              anything but numbers until the nesting level returns
C              to the current level.
C
               IF (       ( LASTTK .EQ.  EXP    )
     .              .AND. (        .NOT. EXPGRP ) ) THEN
 
                  EXPLEV = NEST
                  EXPGRP = .TRUE.
 
               END IF
 
C
C              Increase the nesting level of the expression.
C
               NEST   = NEST + 1
 
            ELSE IF ( IDENT(I) .EQ. BLANK ) THEN
 
C
C              Don't do anything.
C
 
            END IF
 
C
C           Copy the identity of this token.
C
            LASTTK = IDENT(I)
 
C
C           Now for a few quick checks.  If the nesting level ever drops
C           below zero, we don't have a unit.
C
            IF ( NEST .LT. 0 ) THEN
               UNITP = .FALSE.
               RETURN
            END IF
 
C
C           We need to see if its ok to relax the restriction on the
C           use of physical units.
C
            IF ( EXPGRP ) THEN
               EXPGRP = NEST .GT. EXPLEV
            END IF
 
         END DO
 
C
C        Just in case we didn't get everything the first time,
C        scan the string again.
C
         CALL  SCAN   ( STRING,
     .                  OP,     OPLEN, OPPTR, ROOM,   START,
     .                  NTOKNS, IDENT, BEG,   END            )
 
      END DO
 
C
C     One last check.  If we didn't get a physical unit somewhere in
C     the string or if the nesting did not return to zero, we don't
C     have a unit.
C
      IF ( NEST .EQ. 0 ) THEN
         UNITP = PHYSCL
      ELSE
         UNITP = .FALSE.
      END IF
 
      RETURN
      END
