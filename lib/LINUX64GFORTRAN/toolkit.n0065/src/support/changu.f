C$Procedure      CHANGU ( Change units )
 
      SUBROUTINE CHANGU ( ANGLE, LENGTH, TIME, MASS, CHARGE, IN,
     .                    OUT,   ERROR )
 
C$ Abstract
C
C     Determine units having the same dimensions of angle, length,
C     time, mass and charge as some set of input units, but with
C     respect to a "standard" set of units of the user's choosing.
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
C     CONSTANTS
C     CONVERSION
C     PARSING
C     UNITS
C     UTILITY
C
C$ Declarations
      IMPLICIT NONE
      CHARACTER*(*)         ANGLE
      CHARACTER*(*)         LENGTH
      CHARACTER*(*)         TIME
      CHARACTER*(*)         MASS
      CHARACTER*(*)         CHARGE
      CHARACTER*(*)         IN
      CHARACTER*(*)         OUT
      CHARACTER*(*)         ERROR
 
      INTEGER               ROOM
      PARAMETER           ( ROOM   = 128 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ANGLE      I   Default unit to use for angles (see OUNITS).
C     LENGTH     I   Default unit to use for lengths (see OUNITS).
C     TIME       I   Default unit to use for time (see OUNITS).
C     MASS       I   Default unit to use for mass (see OUNITS).
C     CHARGE     I   Default unit to use for charge (see OUNITS).
C     IN         I   Units to be transformed to the "standard".
C     OUT        O   Units that the input will be transformed to.
C     ERROR      O   Contains a description of a problem if one occurs.
C     ROOM       P   Maximum number of components in a compound unit.
C
C$ Detailed_Input
C
C     See individual entry points
C
C     ANGLE      is a string indicating which angle unit should be
C                used for outputs.
C
C     LENGTH     is a string indicating which distance unit should
C                be used for outputs.
C
C     TIME       is a string indicating which time unit should be
C                used for outputs.
C
C     MASS       is a string indicating which mass unit should be
C                used for outputs.
C
C     CHARGE     is a string indicating which charge unit should be
C                used for outputs.
C
C     IN         is the set of units associated with some measurment.
C                The dimensionally equivalent "standard" units are
C                returned in OUT.
C
C$ Detailed_Output
C
C     See individual entry points.
C
C     OUT        is the set of "standard" units that are dimensionally
C                equivalent to the input units given by IN.
C
C     ERROR      Contains a descriptive error message if the
C                subroutine call can not be executed successfully.
C
C$ Parameters
C
C     ROOM       This routine uses internal storage to construct
C                the output for TRANSU.  ROOM is the parameter that
C                describes the maximum number of components that
C                are expected for any compound unit.  The components
C                of a compound unit are
C
C                   Left parenthesis  --- '('
C                   Right parenthesis --- ')'
C                   Exponentiation    --- '**'
C                   Multiplication    --- '*'
C                   Division          --- '/'
C                   Numbers
C                   Reconized units of angle, distance, time, mass or
C                   charge.
C
C                Thus  ((10**12*KG)*(10**9*KM)**3)/((2/3)*SEC**2)
C                      ^^ ^ ^ ^^ ^^^^ ^ ^^^ ^^ ^^^^^^^^^^^  ^ ^^^
C
C                Has 31 components. (Each '^' points to the end of a
C                component).
C
C                At the time this routine was written, it was assumed
C                that compound units would have fewer than 128
C                components.
C
C$ Exceptions
C
C     1) The units used as the "standard" set must be recognized.
C        If they are not the error 'SPICE(UNKNOWNUNITS)' is signalled
C        by the entry point OUNITS.
C
C     2) If the input string IN can not be parsed as a unit, the error
C        'SPICE(INVALIDUNITS)' is signalled by the entry point TRANSU.
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine (and its entry points) are utilities that work
C     in conjunction with the general units conversion routine
C     CONVRT_2.
C
C     Here's why it is needed.
C
C        For many applications it is convenient to have command
C        driven programs.  Such commands might look like
C
C           SET FIELD OF VIEW  <number> [units]
C
C        Where "<number>" is some number that represents the size of
C        the field of view and must be supplied for the command to
C        mean anything.  The field "[units]" is an optional argument
C        that specifies the units to associate with the numeric
C        part of the command.  For example you might type any of the
C        following:
C
C           SET FIELD OF VIEW 12 DEGREES
C
C           SET FIELD OF VIEW 5  10E-3*RADIANS
C
C           SET FIELD OF VIEW 12 NANORADIANS
C
C           SET FIELD OF VIEW 6 ARCSECONDS
C
C        Allowing this kind of flexibility for inputs, gives user's
C        a friendlier interface to the program.  Instead of spending
C        time converting to some standard set of inputs, the program
C        "understands" many different units.
C
C        Ultimately, the measurements written in these expressions
C        must be converted to a set of units that the program
C        "understands."  If the above command were the only one
C        recognized by the program, the problem of converting to
C        internal units would be relatively simple.  You could just
C        list the collection of recognized units and translate them.
C        For this command such a would probably not contain more than
C        30 different units.  However, when compound units are
C        allowed such as:
C
C           KM/SEC**2,  MILES/HOUR/DAY,  AU/(100*DAYS)**2, etc.
C
C        it is no longer practical to simply list all of the possible
C        compound expressions.  Instead it is much simpler to select a
C        set of primitive units in which all compound units will be
C        expressed and used internally.  For example you might decide
C        that the fundamental units best suited to your application are:
C
C           For angles   ---  Degrees
C           For distance ---  Astronomical Units (AU)
C           For time     ---  DAYS
C           For mass     ---  KG
C           For Charge   ---  ELECTRON_CHARGES
C
C        When a measurment is encountered, your program would convert
C        it to this set of standard units automatically.  For example
C        If an input had the form
C
C           3 KM/SEC
C
C        the program would automatically convert it to the appropriate
C        number of
C
C             AU/DAYS.
C
C        In terms of the primitive units of angle, length, time, mass
C        and charge.  These two quantities are dimensionally equivalent.
C
C
C        This routine serves as the umbrella for two functions:
C
C        1) Establishing what units to use as "standard" for the
C           fundamental quanities of angle, distance, time, mass and
C           charge.  (OUNITS)
C
C        2) Computing the standard units that are dimensionally
C           equivalent to any given input units.
C
C        With the dimensionally equivalent standard units in hand,
C        it is an easy matter (as the example below illustrates)
C        to convert inputs measurments to the standard units your
C        program needs.
C
C$ Examples
C
C        To set up your default units as above:
C
C           IF ( FIRST ) THEN
C
C              CALL OUNITS ( 'DEGREES',         'AU', 'DAYS', 'KG',
C          .                 'ELECTRON_CHARGES'                    )
C
C              FIRST = .FALSE.
C
C           END IF
C
C        To translate a measurement X UNITS to the default units.
C
C           CALL TRANSU   (    UNITS, MINE       )
C           CALL CONVRT_2 ( X, UNITS, MINE, MY_X )
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
C-    Beta Version 1.0.0, 29-MAY-1991 (WLT)
C
C-&
 
 
C
C     SPICELIB functions.
C
      INTEGER               BSRCHC
      INTEGER               LASTNB
      EXTERNAL              SCAN 
C
C     Local Parameters
C
      INTEGER               NMARKS
      PARAMETER           ( NMARKS =   6 )
 
 
      INTEGER               STRSIZ
      PARAMETER           ( STRSIZ = 2*ROOM )
C
C     Local Variables
C
      CHARACTER*(2)         OP     ( NMARKS )
 
      CHARACTER*(8)         TCLASS (   5 )
 
      CHARACTER*(32)        TYPE   ( 0:5 )
 
      CHARACTER*(256)       O
      CHARACTER*(256)       STRING
 
      DOUBLE PRECISION      VALUE
 
      INTEGER               B
      INTEGER               BEG    ( ROOM )
      INTEGER               BLANK
      INTEGER               CLASS
      INTEGER               DIV
      INTEGER               E
      INTEGER               END    ( ROOM )
      INTEGER               EXP
      INTEGER               F
      INTEGER               I
      INTEGER               IDENT  ( ROOM )
      INTEGER               SIZE   ( 0:5 )
      INTEGER               LPAREN
      INTEGER               MULT
      INTEGER               NEST
      INTEGER               NOP
      INTEGER               NTOKNS
      INTEGER               OPLEN ( NMARKS )
 
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
      INTEGER               PASS
      INTEGER               RPAREN
      INTEGER               S
      INTEGER               START
 
      LOGICAL               FIRST
      LOGICAL               FOUND
 
C
C     Saved variables
C
 
      SAVE
 
C
C     Initial Values
C
      DATA                  TCLASS  / 'ANGLE', 'LENGTH', 'TIME',
     .                                'MASS',  'CHARGE'          /
 
      DATA                  FIRST   / .TRUE.  /
      DATA                  NOP     /  NMARKS /
      DATA                  OP      / ' ', '(', ')', '*', '**', '/' /
 
      RETURN
 
      ENTRY OUNITS ( ANGLE, LENGTH, TIME, MASS, CHARGE, ERROR )
 
C
C        On the first pass through this routine, set up the stuff
C        required for scanning the input string.
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
 
         TYPE(0) = '1'
         TYPE(1) = ANGLE
         TYPE(2) = LENGTH
         TYPE(3) = TIME
         TYPE(4) = MASS
         TYPE(5) = CHARGE
 
         I     = 1
         ERROR = ' '
 
         DO WHILE ( I .LE. 5 )
 
            CALL FNDUCV ( TYPE(I), FOUND, CLASS, VALUE )
 
            IF ( .NOT. FOUND ) THEN
               ERROR = 'Unrecognized unit: ' // TYPE(I)
            ELSE IF ( CLASS .NE. I ) THEN
 
               CALL SUFFIX ( 'The',                          1, ERROR )
               CALL SUFFIX (  TCLASS(I),                     1, ERROR )
               CALL SUFFIX ( 'argument is ''',               1, ERROR )
               CALL SUFFIX (  TYPE(I),                       1, ERROR )
               CALL SUFFIX ( '''. This is not a unit ',      0, ERROR )
               CALL SUFFIX ( 'of type',                      1, ERROR )
               CALL SUFFIX (  TCLASS(I),                     1, ERROR )
               CALL SUFFIX ( '.',                            0, ERROR )
 
            END IF
 
            I = I + 1
 
         END DO
 
         DO I = 0, 5
            SIZE(I) = LASTNB( TYPE(I) )
         END DO
 
      RETURN
 
 
C
C     Construct the units having the same dimensions as the input
C     but that have fundamentals (angle, length, time, ... ) in the
C     form that are expected by the calling program.
C
      ENTRY TRANSU ( IN, OUT )
 
         STRING = IN
         O      = ' '
         NEST   = 0
         START  = 1
         F      = 0
 
         CALL  SCAN   ( STRING,
     .                  OP,     OPLEN, OPPTR, ROOM,   START,
     .                  NTOKNS, IDENT, BEG,   END            )
 
         I = 1
 
         DO WHILE ( I .LE. NTOKNS )
 
            B = BEG(I)
            E = END(I)
 
            IF ( IDENT(I) .EQ. BLANK ) THEN
C
C              Don't do anything....
C
 
            ELSE IF ( IDENT(I) .NE. 0 ) THEN
 
               S      = F+1
               F      = S+E-B
               O(S:F) = STRING(B:E)
C
C              We have to excercise a bit of caution.  If this
C              is an exponentiation operation, we need to just copy
C              the exponent to the output string.
C
               IF ( IDENT(I) .EQ.  EXP ) THEN
 
                  NEST  = 0
                  PASS  = 0
 
                  DO WHILE (      ( PASS .LT. 1 )
     .                       .OR. ( NEST .GT. 0 ) )
 
                     I       = I + 1
                     PASS    = PASS + 1
 
                     B       = BEG(I)
                     E       = END(I)
                     S       = F + 1
                     F       = S + B - E
 
                     O(S:F)  = STRING(B:E)
 
                     IF ( IDENT(I) .EQ. RPAREN ) THEN
 
                        NEST = NEST - 1
 
                     ELSE IF ( IDENT(I) .EQ. LPAREN ) THEN
 
                        NEST = NEST + 1
 
                     END IF
 
                  END DO
 
               END IF
 
            ELSE
 
C
C              If you get to this point, just copy the units
C              associated with the class of this token.
C
               CALL FNDUCV ( STRING(B:E), FOUND, CLASS, VALUE )
 
               S      = F           + 1
               F      = SIZE(CLASS) - 1 + S
               O(S:F) = TYPE(CLASS)
 
            END IF
 
            I = I + 1
 
         END DO
 
         OUT = O
 
      RETURN
      END
