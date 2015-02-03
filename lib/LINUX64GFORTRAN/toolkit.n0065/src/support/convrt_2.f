C$Procedure CONVRT_2  ( Convert Units )
 
      SUBROUTINE CONVRT_2  (  XIN, UNIN, UNOUT, XOUT )
      IMPLICIT NONE 
 
C$ Abstract
C
C     Convert a quantity in one system of units to another system.
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
C     CONVERSION
C     UTILITY
C
C$ Declarations

      DOUBLE PRECISION      XIN
      CHARACTER*(*)         UNIN
      CHARACTER*(*)         UNOUT
      DOUBLE PRECISION      XOUT
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     XIN        I   A quantity measured in UNIN units.
C     UNIN       I   The units of the input quantity.
C     UNOUT      I   The units desired for output.
C     XOUT       O   The value of XIN in the UNOUT units.
C
C$ Detailed_Input
C
C     XIN        is the measurement of a physical quantity in the
C                units given by UNIN.
C
C     UNIN       are the units associated with the input quantity
C                XIN.  These units should be expressed in terms
C                of units of angle, length, time, mass and charge
C                (no compound units such as newtons or joules.)
C
C
C     UNOUT      are the units that will be associated with the
C                output quantity XOUT.  UNOUT must be dimensionally
C                equivalent to UNIN and, like UNIN, must be expressed
C                in terms of units of angle, length, time, mass and
C                charge.
C
C$ Detailed_Output
C
C     XOUT       is the number of UNOUT units that are equal to
C                XIN units of UNIN.  XOUT may overwrite XIN.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If either UNIN or UNOUT is not a recognized physical unit,
C        the error 'SPICE(BADUNITS)' will be signalled.
C
C     2) If UNIN and UNOUT are not dimensionally equivalent, the
C        error 'SPICE(INCOMPATIBLEUNITS)' will be signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine provides a simple means of converting between
C     a quantity expressed in terms of one system of units to
C     another system of units.  The fundamental units recognized
C     are those recognized by FNDUCV.  The units recognized by
C     version 1.0.0 of FNDUCV are:
C
C
C
C
C     If the singular form a unit is not listed, but it is obtained
C     from the plural form by dropping a final 'S', you may use the
C     singular form.  For example,
C
C        instead of  SECONDS you may use SECOND;
C        instead of  MILES   you may use MILE;
C        instead of  DEGREES you may use DEGREE.
C
C     Thus the strings 'SECONDS/DEGREE', 'SECOND/DEGREES',
C     'SECOND/DEGREE', and 'SECONDS/DEGREES' are all recognized
C     and hav have the same meaning.
C
C$ Examples
C
C     Suppose you needed to convert a state, PV, from KM and KM/SEC to
C     AU and AU/365 days.  The following loop will do the job.
C
C        DO I = 1, 3
C           CALL CONVRT_2 ( PV(I),   'KM',     'AU',           PV(I)   )
C           CALL CONVRT_2 ( PV(I+3), 'KM/SEC', 'AU/(365*DAYS)' PV(I+3) )
C        END DO
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
C-    SPICELIB 1.1.0, 28-SEP-2010 (EDW)
C
C        Renamed CLASSS variable CLAS_S to eliminate ifort 11.1 Pro
C        compiler error. ifort interpreted CLASSS as 'CLASS S'.
C
C-    Beta Version 1.0.0, 31-MAY-1991 (WLT)
C
C-&
 
      INTEGER               NDIM
      PARAMETER           ( NDIM   = 5    )
 
      INTEGER               MAXDIM
      PARAMETER           ( MAXDIM = NDIM )
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               MAXPAR
      PARAMETER           ( MAXPAR = 128 )
 
C
C     These are the class id's for each of the various entities
C     that make up the variables of a unit.
C
      INTEGER               NUMBER
      PARAMETER           ( NUMBER = 0 )
 
      INTEGER               ANGLE
      PARAMETER           ( ANGLE  = 1 )
 
      INTEGER               LENGTH
      PARAMETER           ( LENGTH = 2 )
 
      INTEGER               TIME
      PARAMETER           ( TIME   = 3 )
 
      INTEGER               MASS
      PARAMETER           ( MASS   = 4 )
 
      INTEGER               CHARGE
      PARAMETER           ( CHARGE = 5 )
 
      INTEGER               INPUT
      PARAMETER           ( INPUT  = 1 )
 
      INTEGER               OUTPUT
      PARAMETER           ( OUTPUT = 2 )
 
C
C     These are the codes will will use for the various
C     operations.
C
      DOUBLE PRECISION      EXPIAT
      PARAMETER           ( EXPIAT = 3.0D0 )
 
      DOUBLE PRECISION      DIVIDE
      PARAMETER           ( DIVIDE = 2.0D0 )
 
      DOUBLE PRECISION      MULPLY
      PARAMETER           ( MULPLY = 1.0D0 )
 
C
C     Scanning Parameters
C
      INTEGER               NMARKS
      PARAMETER           ( NMARKS = 6 )
 
      INTEGER               ROOM
      PARAMETER           ( ROOM   = 128 )
 
C
C     SPICELIB functions
C
      INTEGER               BSRCHC
      LOGICAL               RETURN
      EXTERNAL              SCAN
 
C
C     Other functions
C
      LOGICAL               UNITP
 
 
      CHARACTER*(2)         OP     ( NMARKS )
      CHARACTER*(8)         NAME   ( MAXDIM )
 
      DOUBLE PRECISION      DIFF
      DOUBLE PRECISION      DIM    ( 0 : MAXDIM )
      DOUBLE PRECISION      DIMEN  ( 0 : MAXDIM )
      DOUBLE PRECISION      DIMENI ( 0 : MAXDIM )
      DOUBLE PRECISION      DIMENO ( 0 : MAXDIM )
      DOUBLE PRECISION      EXPONT ( LBCELL: MAXPAR )
      DOUBLE PRECISION      INVAL
      DOUBLE PRECISION      KEEP
      DOUBLE PRECISION      OPVAL  (     NMARKS )
      DOUBLE PRECISION      OUTVAL
      DOUBLE PRECISION      PARSED ( LBCELL: MAXPAR )
      DOUBLE PRECISION      VALUE
 
      INTEGER               ACTIVE
      INTEGER               B
      INTEGER               BEG    ( ROOM   )
      INTEGER               BLANK
      INTEGER               CLASS
      INTEGER               CLAS_S ( LBCELL: MAXPAR )
      INTEGER               DIV
      INTEGER               E
      INTEGER               END    ( ROOM )
      INTEGER               EXP
      INTEGER               I
      INTEGER               IDENT  ( ROOM )
      INTEGER               INOUT
      INTEGER               J
      INTEGER               L
      INTEGER               LPAREN
      INTEGER               MULT
      INTEGER               NOP
      INTEGER               NTOKNS
      INTEGER               O
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
C      So the required number of pointers is 47 - 32 + 5 = 20.
C
      INTEGER               OPPTR ( 20 )
      INTEGER               R
      INTEGER               RPAREN
      INTEGER               START
 
      LOGICAL               DONE
      LOGICAL               FIRST
      LOGICAL               KNOWN
      LOGICAL               MOVE
 
C
C     Saved Variables
C
      SAVE
C
C     Initial Values
C
 
      DATA                  DIM    / 0.0D0, 1.0D0, 1.0D0,
     .                               1.0D0, 1.0D0, 1.0D0  /
      DATA                  FIRST   / .TRUE. /
      DATA                  NAME    / 'angle', 'length', 'time',
     .                                'mass',  'charge'          /
      DATA                  NOP     /  6     /
      DATA                  OP      / ' ', '(', ')', '*', '**', '/' /
 
 
 
C
C     The game is afoot!
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CONVRT_2' )
      END IF
 
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         CALL SCANPR ( NOP, OP, OPLEN, OPPTR )
 
         BLANK  = BSRCHC ( ' ',  NOP, OP )
         LPAREN = BSRCHC ( '(',  NOP, OP )
         RPAREN = BSRCHC ( ')',  NOP, OP )
         MULT   = BSRCHC ( '*',  NOP, OP )
         EXP    = BSRCHC ( '**', NOP, OP )
         DIV    = BSRCHC ( '/',  NOP, OP )
 
         OPVAL( BLANK  ) = 0.0
         OPVAL( LPAREN ) = 0.0
         OPVAL( RPAREN ) = 0.0
         OPVAL( MULT   ) = MULPLY
         OPVAL( EXP    ) = EXPIAT
         OPVAL( DIV    ) = DIVIDE
 
      END IF
 
C
C     First make sure that both UNIN and UNOUT are recognized
C     units.
C
      IF ( .NOT. UNITP(UNIN) ) THEN
 
         CALL SETMSG ( 'The input unit, #, was not recognized '       //
     .                 'as a valid unit specification.'               )
         CALL ERRCH  ( '#',  UNIN                                     )
         CALL SIGERR ( 'SPICE(UNKNOWNUNITS)'                          )
         CALL CHKOUT ( 'CONVRT_2'                                     )
         RETURN
 
      END IF
 
      IF ( .NOT. UNITP(UNOUT) ) THEN
 
         CALL SETMSG ( 'The output unit, #, was not recognized '      //
     .                 'as a valid unit specification.'               )
         CALL ERRCH  ( '#',  UNIN                                     )
         CALL SIGERR ( 'SPICE(UNKNOWNUNITS)'                          )
         CALL CHKOUT ( 'CONVRT_2'                                     )
         RETURN
 
      END IF
 
 
C
C     We will need to keep track of the dimensions associated
C     with both input and output units.
C
      DIMENI ( ANGLE  ) = 0.0D0
      DIMENI ( LENGTH ) = 0.0D0
      DIMENI ( TIME   ) = 0.0D0
      DIMENI ( MASS   ) = 0.0D0
      DIMENI ( CHARGE ) = 0.0D0
 
      DIMENO ( ANGLE  ) = 0.0D0
      DIMENO ( LENGTH ) = 0.0D0
      DIMENO ( TIME   ) = 0.0D0
      DIMENO ( MASS   ) = 0.0D0
      DIMENO ( CHARGE ) = 0.0D0
 
 
C
C     We need to parse both the input and output units, we
C     do that in the loop that ranges from INPUT to OUTPUT.
C
      DO INOUT = INPUT, OUTPUT
 
C
C        Initialize the various pods we will need to use to
C        parse this set of units.
C
         CALL SSIZED ( MAXPAR, PARSED )
         CALL SSIZEI ( MAXPAR, CLAS_S )
         CALL SSIZED ( MAXPAR, EXPONT )
 
C
C        Zero out the dimension vector.
C
         DIMEN ( ANGLE  ) = 0.0D0
         DIMEN ( LENGTH ) = 0.0D0
         DIMEN ( TIME   ) = 0.0D0
         DIMEN ( MASS   ) = 0.0D0
         DIMEN ( CHARGE ) = 0.0D0
 
C
C        We haven't finished scanning this unit yet.
C
         DONE = .FALSE.
 
C
C        We are beginnin a group now.  After beginning a group we ALWAYS
C        append 1,0,0 and MULTPLY, -1, 0 to the PARSED, CLAS_S, and
C        EXPONT pod.  Why ask why?  Well in this case we do it because
C        it makes the processing MUCH simpler (you'll see).
C
         CALL APPNDD ( 1.0D0,  PARSED )
         CALL APPNDI ( 0,      CLAS_S )
         CALL APPNDD ( 0.0D0,  EXPONT )
 
         CALL APPNDD ( MULPLY, PARSED )
         CALL APPNDI ( -1,     CLAS_S )
         CALL APPNDD ( 0.0D0,  EXPONT )
C
C        We'll start scanning this string from the first character.
C
         START = 1
 
         IF      ( INOUT .EQ. INPUT ) THEN
 
            CALL  SCAN   ( UNIN,
     .                     OP,     OPLEN, OPPTR, ROOM,   START,
     .                     NTOKNS, IDENT, BEG,   END            )
 
 
         ELSE IF ( INOUT .EQ. OUTPUT ) THEN
 
            CALL  SCAN   ( UNOUT,
     .                     OP,     OPLEN, OPPTR, ROOM,   START,
     .                     NTOKNS, IDENT, BEG,   END            )
 
         END IF
 
 
C
C        For as long as there are tokens to look at...
C
         DO WHILE ( NTOKNS .GT. 0 )
 
 
C
C           ... examine each in turn, classify it and take
C           an appropriate action.
C
            DO I = 1, NTOKNS
 
C
C              If we have a left parenthesis ...
C
               IF ( IDENT(I) .EQ. LPAREN ) THEN
 
C
C                 We are beginnin a group now.  After beginning a
C                 group we ALWAYS append 1,0,0 and MULTPLY, -1, 0 to
C                 the PARSED, CLAS_S, and EXPONT pod.
C
                  CALL PODBGD (         PARSED )
                  CALL PODBGI (         CLAS_S )
                  CALL PODBGD (         EXPONT )
 
                  CALL APPNDD ( 1.0D0,  PARSED )
                  CALL APPNDI ( 0,      CLAS_S )
                  CALL APPNDD ( 0.0D0,  EXPONT )
 
                  CALL APPNDD ( MULPLY, PARSED )
                  CALL APPNDI ( -1,     CLAS_S )
                  CALL APPNDD ( 0.0D0,  EXPONT )
 
C
C              ... or if we have an arithmetic operations
C
               ELSE IF (      ( IDENT(I) .EQ. MULT )
     .                   .OR. ( IDENT(I) .EQ. DIV  )
     .                   .OR. ( IDENT(I) .EQ. EXP  ) ) THEN
 
C
C                 Append the operation to the current group.
C
                  CALL APPNDD ( OPVAL( IDENT(I) ), PARSED )
                  CALL APPNDI ( -1,                CLAS_S )
                  CALL APPNDD ( 0.0D0,             EXPONT )
 
C
C              ...or if we have a unit or number ...
C
               ELSE IF ( IDENT(I) .EQ. 0 ) THEN
 
C
C                  Look up the class and value for this token,
C                  append them to the current group.
C
                   B = BEG(I)
                   E = END(I)
 
                   IF      ( INOUT .EQ. INPUT ) THEN
 
                      CALL FNDUCV ( UNIN(B:E),  KNOWN, CLASS, VALUE )
 
                   ELSE IF ( INOUT .EQ. OUTPUT ) THEN
 
                      CALL FNDUCV ( UNOUT(B:E), KNOWN, CLASS, VALUE )
 
                   END IF
 
                   CALL APPNDD ( VALUE,      PARSED )
                   CALL APPNDI ( CLASS,      CLAS_S )
                   CALL APPNDD ( DIM(CLASS), EXPONT )
 
 
C
C              ...or if we have a right parenthesis, close off
C              this group by evaluating it, then close the group
C              and append the last value computed onto its list
C              of value/operation pairs.
C
               ELSE IF ( IDENT(I) .EQ. RPAREN ) THEN
 
C
C                 We are ending a group.  It's time to perform all
C                 indicated operations in this group.  Note the
C                 structure of a completed group is:
C
C                   Value  OP  Value OP Value ... OP Value
C
C                 Thus all operations are at even slots in the
C                 group.  The scheme for evaluating this expression
C                 is: identify the next operation to perform (more on
C                 how to locate the operation in a minute);
C
C                                      Do this one
C                                      _____^______
C                                     '            `
C                  Value OP Value OP  Value OP Value  OP Value OP ...
C
C                 replace the three entries by the result.
C
C                     Value OP Value OP  result OP Value OP  ...
C
C                 The hierarchy of operations is
C
C                    1.) exponentiation in left to right order.
C
C                    2.) multiplication and division in left
C                        to right order.
C
C                 Since the parsing is from left to right, as we
C                 simplify subexpression, we can shift items left
C                 to fill in the gaps left by the operator and
C                 second value of the expression that was simplified.
C
C                 To do all this we must fist identify the beginning
C                 and ends of this group.
C
                  CALL PODBED ( PARSED, B, E )
 
C
C                 First handle exponentiation.  So far we haven't
C                 moved anything, the ACTIVE left operand is at B;
C                 the first operator is located at B+1.  We will let
C                 ATOP (at operator) be the logical flag that indicates
C                 whether J points to an operator or an operand.
C
                  MOVE   = .FALSE.
                  ACTIVE =  B
                  J      =  B + 1
 
                  DO WHILE ( J .LE. E )
 
                     IF ( PARSED(J) .EQ. EXPIAT ) THEN
 
C
C                       We are going to simplify an expression
C                       of the form  X ** Y to its computed value.
C                       This means we will be freeing up room to
C                       move items to the left.
C
C
                        MOVE           = .TRUE.
 
                        PARSED(ACTIVE) = PARSED(ACTIVE) ** PARSED(J+1)
                        EXPONT(ACTIVE) = EXPONT(ACTIVE)  * PARSED(J+1)
 
 
                     ELSE
 
C
C                       If we are moving operators and right
C                       operands to the left, now is the time
C                       to do it.
C
                        IF ( MOVE ) THEN
 
                           O         = ACTIVE + 1
                           L         = ACTIVE + 2
                           R         = J      + 1
 
                           PARSED(O) = PARSED(J)
                           CLAS_S(O) = CLAS_S(J)
                           EXPONT(O) = EXPONT(J)
 
                           PARSED(L) = PARSED(R)
                           CLAS_S(L) = CLAS_S(R)
                           EXPONT(L) = EXPONT(R)
 
                        END IF
 
                        ACTIVE = ACTIVE + 2
 
                     END IF
 
C
C                    Make J point to the next operator.
C
                     J = J + 2
 
                  END DO
 
C
C                 Next handle multiplication and division.
C
                  E      =  ACTIVE
                  ACTIVE =  B
                  J      =  B + 1
 
                  DO WHILE ( J .LE. E )
 
                     R     = J + 1
                     CLASS = CLAS_S(R)
 
                     IF      ( PARSED(J) .EQ. MULPLY ) THEN
 
                        PARSED(ACTIVE) = PARSED(ACTIVE) * PARSED(R)
                        DIMEN (CLASS ) = DIMEN (CLASS)  + EXPONT(R)
 
                     ELSE IF ( PARSED(J) .EQ. DIVIDE ) THEN
 
                        PARSED(ACTIVE) = PARSED(ACTIVE) / PARSED(R)
                        DIMEN (CLASS ) = DIMEN (CLASS)  - EXPONT(R)
 
                     END IF
 
                     J = J + 2
 
                  END DO
 
C
C                 Finally, save the first value of the group, end the
C                 group, and append the saved value to the previous
C                 group.
C
                  KEEP = PARSED(ACTIVE)
 
                  CALL PODEGD (        PARSED )
                  CALL PODEGI (        CLAS_S )
                  CALL PODEGD (        EXPONT )
 
                  CALL APPNDD ( KEEP,  PARSED )
                  CALL APPNDI ( 0,     CLAS_S )
                  CALL APPNDD ( 0.0D0, EXPONT )
 
               END IF
 
            END DO
 
C
C           Just in case there are any left-overs, scan the
C           string for more tokens
C
            IF      ( INOUT .EQ. INPUT ) THEN
 
               CALL  SCAN   ( UNIN,
     .                        OP,     OPLEN, OPPTR, ROOM,   START,
     .                        NTOKNS, IDENT, BEG,   END            )
 
 
            ELSE IF ( INOUT .EQ. OUTPUT ) THEN
 
               CALL  SCAN   ( UNOUT,
     .                        OP,     OPLEN, OPPTR, ROOM,   START,
     .                        NTOKNS, IDENT, BEG,   END            )
 
            END IF
 
 
C
C           If there are no more tokens left, we need to be sure
C           to close the last group (the one we opened before we
C           had even begun to scan UNIN or UNOUT.
C
            IF (       ( NTOKNS .EQ.  0    )
     .           .AND. (        .NOT. DONE )  ) THEN
 
               DONE     = .TRUE.
               NTOKNS   = 1
               IDENT(1) = RPAREN
 
            END IF
 
         END DO
 
 
C
C        Put the result of the parse into the input or output storage
C        area as appropriate.
C
         IF        ( INOUT .EQ. INPUT ) THEN
 
            DIMENI ( ANGLE  ) = DIMEN ( ANGLE  )
            DIMENI ( LENGTH ) = DIMEN ( LENGTH )
            DIMENI ( TIME   ) = DIMEN ( TIME   )
            DIMENI ( MASS   ) = DIMEN ( MASS   )
            DIMENI ( CHARGE ) = DIMEN ( CHARGE )
            INVAL             = PARSED ( 1      )
 
         ELSE IF ( INOUT .EQ. OUTPUT ) THEN
 
            DIMENO ( ANGLE  ) = DIMEN ( ANGLE  )
            DIMENO ( LENGTH ) = DIMEN ( LENGTH )
            DIMENO ( TIME   ) = DIMEN ( TIME   )
            DIMENO ( MASS   ) = DIMEN ( MASS   )
            DIMENO ( CHARGE ) = DIMEN ( CHARGE )
            OUTVAL            = PARSED ( 1      )
 
         END IF
 
 
C
C        Finally, if this is only the first of the units that needs to
C        be parsed, loop back through the code above a second time.
C
      END DO
 
 
C
C     One final check must be performed.  The input and output
C     units must be dimensionally equivalent.
C
 
      DO I = 1, NDIM
 
         IF ( DIMENI(I) .NE. DIMENO(I) ) THEN
 
            DIFF = DIMENI(I) - DIMENO(I)
 
            CALL SETMSG ( 'The input and output units are not '       //
     .                    'dimensionally '                            //
     .                    'equivalent.  The difference between the '  //
     .                    'input and output dimension for # is #.'    )
 
            CALL ERRCH  ( '#', NAME(I)                                )
            CALL ERRDP  ( '#', DIFF                                   )
            CALL SIGERR ( 'SPICE(NOTDIMENSIONALLYEQUIV)'              )
            CALL CHKOUT ( 'CONVRT_2'                                  )
            RETURN
 
         END IF
 
      END DO
 
C
C     That was the last hurdle,  now we can just comput the output.
C
      XOUT = ( INVAL/OUTVAL ) * XIN
 
      CALL CHKOUT ( 'CONVRT_2' )
 
      RETURN
      END
