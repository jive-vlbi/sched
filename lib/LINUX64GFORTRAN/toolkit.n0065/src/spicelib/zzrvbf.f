C$Procedure ZZRVBF ( Private --- Pool, read the next buffer variable )
 
      SUBROUTINE ZZRVBF ( BUFFER, BSIZE,  LINNUM, NAMLST,
     .                    NMPOOL, NAMES,  DATLST,
     .                    DPPOOL, DPVALS,
     .                    CHPOOL, CHVALS,
     .                    VARNAM, EOF )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Read the next variable from a text buffer into the kernel pool.
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
C     PRIVATE KERNEL
C
C$ Keywords
C
C     POOL
C
C$ Declarations
 
      IMPLICIT NONE
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )
 
      CHARACTER*(*)         BUFFER     (  * )
      INTEGER               BSIZE
      INTEGER               LINNUM
 
      INTEGER               NAMLST     (           * )
      INTEGER               NMPOOL     ( 2, LBPOOL:* )
      CHARACTER*(*)         NAMES      (           * )
      INTEGER               DATLST     (           * )
      INTEGER               DPPOOL     ( 2, LBPOOL:* )
      DOUBLE PRECISION      DPVALS     (           * )
      INTEGER               CHPOOL     ( 2, LBPOOL:* )
      CHARACTER*(*)         CHVALS     (           * )
 
      CHARACTER*(*)         VARNAM
      LOGICAL               EOF
 
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 132 )

      INTEGER               MAXLEN
      PARAMETER           ( MAXLEN = 32 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BUFFER     I   Array of text to be read and parsed.
C     BSIZE      I   Number of lines in text buffer.
C     LINNUM    I/O  line number to begin reading buffer
C     NAMLST    I/O  array of collision resolution list heads
C     NMPOOL    I/O  linked list pool of collision resolution lists
C     NAMES     I/O  array of names of kernel pool variables
C     DATLST    I/O  array of heads of lists of variable values
C     DPPOOL    I/O  linked list pool of pointer lists to d.p. values
C     DPVALS    I/O  array of d.p. kernel pool values
C     CHPOOL    I/O  linked list pool of pointer lists to string values
C     CHVALS    I/O  array of string kernel pool values
C     VARNAM     O   name of variable parsed
C     EOF        O   if TRUE end of input file has been reached.
C
C$ Detailed_Input
C
C     BUFFER    is a string array that contains the text that should
C               be parsed and placed into the kernel pool data
C               structure.
C
C     BSIZE     is the number of lines of text in BUFFER.
C
C     LINNUM    the line number (in BUFFER) at which to begin parsing
C               text.
C
C     NAMLST    this collection of arrays together with the hash
C     NMPOOL    function ZZHASH provide the mechanism for storing
C     NAMES     and retrieving kernel pool variables.
C     DATLST
C     DPPOOL    Given a potential variable name NAME the function
C     DPVALS    ZZHASH(NAME) gives the location in the array in
C     CHPOOL    NAMLST where one should begin looking for the
C     CHVALS    kernel pool variable NAME.
C
C               If NAMLST( ZZHASH(NAME) ) is zero, there is no kernel
C               pool variable corresponding to NAME.  If it is non-zero
C               then NAMLST is the head node of a linked list of names
C               that evaluate to the same integer under the function
C               ZZHASH.  Letting NODE = NAMLST( ZZHASH(NAME) ) check
C               NAMES(NODE) for equality with NAME.  If there is
C               no match find the next node ( NMPOOL(NEXT,NODE) ) until
C               a match occurs or all nodes of the list have been
C               examined.  To insert a new NAME allocate a node NEW from
C               the free list of NMPOOL and append it to the tail of the
C               list pointed to by NAMLST ( ZZHASH(NAME) ).
C
C               Once a node for NAME is located (call it NAMEAT)
C               the values for NAME can be found by examining
C               DATLST(NAMEAT).  If zero, no values have yet been
C               given to NAME.  If less than zero, -DATLST(NAMEAT)
C               is the head node of a list in CHPOOL that gives the
C               indexes of the values of NAME in CHVALS.  If greater
C               than zero, DATLST(NAMEAT) is the head node of a list
C               in DPPOOL that gives the indexes of the values of NAME
C               in DPVALS.
C
C
C
C
C$ Detailed_Output
C
C     LINNUM     is the line number at which the "next" read should
C                begin.
C
C     NAMLST     is the same structure as input but updated to
C     NMPOOL     include the next variable read from the text buffer.
C     NAMES
C     DATLST
C     DPPOOL
C     DPVALS
C     CHPOOL
C     CHVALS
C
C     VARNAM     is the name of the variable. VARNAM is blank if
C                no variable is read.
C
C     EOF        is true when the end of the internal buffer has been
C                reached, and is false otherwise.
C
C$ Parameters
C
C      LINLEN      is the maximum length of a line in the buffer.
C
C      MAXLEN      is the maximum length of the variable names that
C                  can be stored in the kernel pool (defined in pool.f).
C
C$ Exceptions
C
C
C     1) The error 'SPICE(BADTIMESPEC)' is signalled if a value
C        beginning with '@' cannot be parsed as a time.
C
C     2) The error 'SPICE(BADVARASSIGN)' is signalled if variable
C        assignment does not have the form NAME = [(] value [ value ) ].
C
C     3) The error 'SPICE(KERNELPOOLFULL)' is signalled if there is
C        no room left in the kernel pool to store another variable
C        or value.
C
C     4) The error 'SPICE(NONPRINTINGCHAR)' is signalled if the name
C        in a variable assignment contains a non-printing character.
C
C     5) The error 'SPICE(NUMBEREXPECTED)' is signalled if a value
C        that is unquoted cannot be parsed as time or number.
C
C     6) The error 'SPICE(TYPEMISMATCH)' is signalled if a variable
C        has a first value of one type (numeric or character) and
C        a subsequent component has the other type.
C
C     7) The error 'SPICE(BADVARNAME)' signals if a kernel pool 
C        variable name length exceeds MAXLEN.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     See POOL (entry point LMPOOL).
C
C$ Restrictions
C
C     The input buffer should be no more than 132 characters in width.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 09-FEB-2010 (EDW)
C
C        Added an error check on the length of the kernel pool variable
C        names read from BUFFER.
C
C-    SPICELIB Version 1.0.0, 29-MAR-1999 (WLT)
C
C-&
 
C
C
C     SPICELIB functions
C
      INTEGER               LASTPC
      INTEGER               LNKNFN
      INTEGER               RTRIM
      INTEGER               ZZHASH
 
      LOGICAL               RETURN
      LOGICAL               FAILED
 
C
C     Local parameters.
C
C     Below are a collection of enumerated lists that are used
C     to discern what part of the processing we are in and what
C     kind of entity we are dealing with.  First the overall
C     processing flow of a variable assignment.
C
 
      INTEGER               BEGIN
      PARAMETER           ( BEGIN  = 1 )
 
      INTEGER               DONE
      PARAMETER           ( DONE   = BEGIN  + 1 )
 
      INTEGER               INVAR
      PARAMETER           ( INVAR  = DONE   + 1 )
 
C
C     Next we have the various types of tokens that can be found
C     in the parsing of an input line
C
C     Q   --- quoted (or protected tokens)
C     NQ  --- unquoted tokens
C     BV  --- beginning of a vector
C     EV  --- ending of a vector
C     EQ  --- equal sign
C     EQP --- equal sign plus
C
      INTEGER               Q
      PARAMETER           ( Q      = 1          )
 
      INTEGER               NQ
      PARAMETER           ( NQ     = Q      + 1 )
 
      INTEGER               BV
      PARAMETER           ( BV     = NQ     + 1 )
 
      INTEGER               EV
      PARAMETER           ( EV     = BV     + 1 )
 
      INTEGER               EQ
      PARAMETER           ( EQ     = EV     + 1 )
 
      INTEGER               EQP
      PARAMETER           ( EQP    = EQ     + 1 )
 
C
C     A variable can have one of three types as we process
C     it.  It can have an unknown type UNKNWN, STRTYP or NUMTYP.
C
C
 
      INTEGER               STRTYP
      PARAMETER           ( STRTYP = 1 )
 
      INTEGER               NUMTYP
      PARAMETER           ( NUMTYP = STRTYP + 1 )
 
      INTEGER               UNKNWN
      PARAMETER           ( UNKNWN = NUMTYP + 1 )
 
C
C     The next two parameters indicate which component of a linked
C     list node point to the previous node and the next node.
C
      INTEGER               PREV
      PARAMETER           ( PREV = 2 )
 
      INTEGER               NEXT
      PARAMETER           ( NEXT = 1 )
 
 
      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )
C
C     The next collection of variables are set up in first pass
C     through this routine.  They would be parameters if FORTRAN
C     allowed us to do this in a standard way.
C
      INTEGER               IBLANK
      INTEGER               ICOMMA
      INTEGER               IEQUAL
      INTEGER               ILPARN
      INTEGER               IPLUS
      INTEGER               IQUOTE
      INTEGER               IRPARN
      INTEGER               ITAB
      INTEGER               ITMARK
 
 
      CHARACTER*(LINLEN)    LINE
      CHARACTER*(LINLEN)    NAME
 
      CHARACTER*(256)       ERROR
 
      DOUBLE PRECISION      DVALUE
 
      INTEGER               AT
      INTEGER               B
      INTEGER               BADAT
      INTEGER               BEGS(LINLEN)
      INTEGER               CHNODE
      INTEGER               CODE
      INTEGER               COUNT
      INTEGER               DATAHD
      INTEGER               DIRCTV
      INTEGER               DPNODE
      INTEGER               E
      INTEGER               ENDS(LINLEN)
      INTEGER               FREE
      INTEGER               HEAD
      INTEGER               I
      INTEGER               J
      INTEGER               LOOKAT
      INTEGER               NAMEAT
      INTEGER               NCOMP
      INTEGER               NODE
      INTEGER               NXTTOK
      INTEGER               R1
      INTEGER               R2
      INTEGER               STATUS
      INTEGER               TAIL
      INTEGER               TYPE(LINLEN)
      INTEGER               VARLEN
      INTEGER               VARTYP
 
C
C     The logicals below are used to take apart the tokens in an
C     input line.
C
      LOGICAL               EVEN
      LOGICAL               INTOKN
      LOGICAL               INQUOT
      LOGICAL               INSEPF
 
      LOGICAL               FULL
      LOGICAL               FOUND
      LOGICAL               FIRST
 
C
C     The following logicals are in-line functions that are used
C     when processing the input strings.
C
      LOGICAL               ISSEP
      LOGICAL               ISQUOT
      LOGICAL               ISEQU
      LOGICAL               ISRPAR
      LOGICAL               ISLPAR
      LOGICAL               ISPLUS
      LOGICAL               ISBAD
      LOGICAL               ISTIME
C
C     Save everything.
C
      SAVE
 
 
      DATA                  FIRST  / .TRUE. /
 
C
C     Below are a collection of In-line function definitions that are
C     intended to make the code a bit easier to write and read.
C
 
      ISSEP ( CODE ) =      CODE .EQ. IBLANK
     .                 .OR. CODE .EQ. ICOMMA
     .                 .OR. CODE .EQ. ILPARN
     .                 .OR. CODE .EQ. IRPARN
     .                 .OR. CODE .EQ. IEQUAL
     .                 .OR. CODE .EQ. ITAB
 
      ISQUOT( CODE ) =      CODE .EQ. IQUOTE
      ISEQU ( CODE ) =      CODE .EQ. IEQUAL
      ISRPAR( CODE ) =      CODE .EQ. IRPARN
      ISLPAR( CODE ) =      CODE .EQ. ILPARN
      ISPLUS( CODE ) =      CODE .EQ. IPLUS
      ISTIME( CODE ) =      CODE .EQ. ITMARK
 
 
      ISBAD ( DIRCTV ) =       DIRCTV .NE. EQ
     .                   .AND. DIRCTV .NE. EQP
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZRVBF' )
      END IF
C
C     Initializations.
C
      IF ( FIRST ) THEN
 
         FIRST  = .FALSE.
         ICOMMA =  ICHAR( ','   )
         IBLANK =  ICHAR( ' '   )
         IQUOTE =  ICHAR( ''''  )
         ILPARN =  ICHAR( '('   )
         IRPARN =  ICHAR( ')'   )
         IEQUAL =  ICHAR( '='   )
         IPLUS  =  ICHAR( '+'   )
         ITMARK =  ICHAR( '@'   )
         ITAB   =  9
 
      END IF
 
C
C     No variable yet and no parsing errors so far.
C
      NAME  = ' '
      ERROR = ' '
      NCOMP = 0
 
 
C
C     Get the next data line. Unless something is terribly wrong,
C     this will begin a new variable definition. We have to read
C     the whole variable, unless we get an error, in which case
C     we can quit.
C
      STATUS = BEGIN
 
      DO WHILE ( ( STATUS .NE. DONE ) .AND. ( .NOT. FAILED() ) )
 
         LINE = ' '
C
C        We need to skip blank lines...
C
         DO WHILE ( LINE .EQ. ' ' )
            EOF    = LINNUM .GT. BSIZE
 
            IF ( EOF ) THEN
               CALL CHKOUT ( 'ZZRVBF' )
               RETURN
            END IF
 
            LINE   = BUFFER(LINNUM )
            LINNUM = LINNUM + 1
         END DO
 
C
C        Find the "tokens" in the input line. As you scan from left
C        to right along the line, exactly one of the following
C        conditions is true.
C
C        1) You are in a separator field
C        4) You are in a quoted substring
C        5) You are in a non-quoted substring that isn't a separator
C           field.
C
C        Stuff between separator fields are regarded as tokens.  Note
C        this includes quoted strings.
C
C        In addition we keep track of 3 separators: '=', '(', ')'
C        Finally, whenever we encounters the separator '=', we back
C        up and see if it is preceded by a '+', if so we attach
C        it to the '=' and treat the pair of characters as a single
C        separator.
C
 
         EVEN   = .TRUE.
         INTOKN = .FALSE.
         INQUOT = .FALSE.
         INSEPF = .TRUE.
 
         COUNT  =  0
         I      =  0
 
         DO WHILE ( I .LT. LEN(LINE) )
C
C           The current character is either a separator, quote or
C           some other character.
C
            I    = I + 1
            CODE = ICHAR(LINE(I:I))
 
            IF ( ISSEP(CODE) ) THEN
C
C              There are 3 possible states we could be in
C                 Separation Field
C                 A quoted substring with the last quote an odd one.
C                 A quoted substring with the last quote an even one.
C                 A non-quoted token.
C              In the first two cases nothing changes, but in the
C              next two cases we transition to a separation field.
C
               IF ( INTOKN .OR. ( INQUOT .AND. EVEN ) ) THEN
                  INQUOT = .FALSE.
                  INTOKN = .FALSE.
                  INSEPF = .TRUE.
               END IF
 
               IF ( INSEPF ) THEN
C
C                 We need to see if this is one of the special
C                 separators
C
                  IF      ( ISEQU(CODE)  ) THEN
 
                     COUNT       = COUNT + 1
                     BEGS(COUNT) = I
                     TYPE(COUNT) = EQ
                     ENDS(COUNT) = I
 
                     IF ( I .GT. 1 ) THEN
C
C                       Look back at the previous character.
C                       See if it is a plus character.
C
                        CODE = ICHAR( LINE(I-1:I-1) )
 
                        IF ( ISPLUS(CODE) ) THEN
C
C                          This is the directive '+=' we need
C                          to set the beginning of this token
C                          to the one before this and adjust
C                          the end of the last token.
C
                           TYPE(COUNT) = EQP
                           BEGS(COUNT) = I-1
 
                           IF (      BEGS(COUNT-1)
     .                          .EQ. ENDS(COUNT-1) ) THEN
 
                              COUNT       = COUNT - 1
                              BEGS(COUNT) = I - 1
                              ENDS(COUNT) = I
                              TYPE(COUNT) = EQP
 
                           ELSE
 
                              ENDS(COUNT-1) = ENDS(COUNT-1) - 1
 
                           END IF
 
                        END IF
 
                     END IF
 
                  ELSE IF ( ISRPAR(CODE) ) THEN
 
                     COUNT       = COUNT + 1
                     BEGS(COUNT) = I
                     ENDS(COUNT) = I
                     TYPE(COUNT) = EV
 
                  ELSE IF ( ISLPAR(CODE) ) THEN
 
                     COUNT       = COUNT + 1
                     BEGS(COUNT) = I
                     ENDS(COUNT) = I
                     TYPE(COUNT) = BV
 
                  END IF
 
               END IF
 
            ELSE IF ( ISQUOT(CODE) ) THEN
C
C              There are 3 cases of interest.
C                 We are in a quoted substring already
C                 We are in a separator field
C                 We are in a non-quoted token.
C              In the first case nothing changes.  In the second
C              two cases we change to being in a quoted substring.
C
               EVEN = .NOT. EVEN
 
               IF ( .NOT. INQUOT ) THEN
 
                  INSEPF      = .FALSE.
                  INTOKN      = .FALSE.
                  INQUOT      = .TRUE.
                  COUNT       =  COUNT + 1
                  BEGS(COUNT) =  I
                  TYPE(COUNT) =  Q
 
               END IF
 
               ENDS(COUNT) = I
 
            ELSE
C
C              This is some character other than a quote, or
C              separator character.
C
C              We are in one of four situations.
C
C                 1) We are in a quoted substring with an odd number of
C                    quotes.
C                 2) We are in a quoted substring with an even number of
C                    quotes.
C                 2) We are in a separator field
C                 3) We are in a non-quoted token.
C
C              In cases 1 and 3 nothing changes. So we won't check
C              those cases.
C
 
               IF ( INSEPF .OR. ( INQUOT .AND. EVEN ) ) THEN
                  INQUOT      = .FALSE.
                  INSEPF      = .FALSE.
                  INTOKN      = .TRUE.
                  COUNT       =  COUNT + 1
                  BEGS(COUNT) =  I
                  TYPE(COUNT) =  NQ
               END IF
 
               ENDS(COUNT) = I
 
            END IF
 
         END DO
 
 
C
C        The first word on the first line should be the name of a
C        variable. The second word should be a directive: = or +=.
C
         IF ( STATUS .EQ. BEGIN ) THEN
C
C           There must be at least 3 contributing tokens on this line.
C
            IF ( COUNT .LT. 3 ) THEN
 
               CALL SETMSG ( 'A kernel variable was not properly '
     .         //            'formed on line # text buffer.'
     .         //            'Such an assignment should have the '
     .         //            'form: ''<variable name> [+]= '
     .         //            '<values>''. This line was ''#''. ' )
 
               R2 = RTRIM(LINE)
 
               CALL ERRINT ( '#', LINNUM     )
               CALL ERRCH  ( '#', LINE(1:R2) )
               CALL SIGERR ( 'SPICE(BADVARASSIGN)' )
               CALL CHKOUT ( 'ZZRVBF' )
               RETURN
 
            END IF
C
C           See if the variable name is legitimate:
C
            BADAT = LASTPC(LINE(BEGS(1):ENDS(1)))
 
            IF ( BADAT .LE. ENDS(1) - BEGS(1) ) THEN
C
C              There is a non-printing character in the variable
C              name.  This isn't allowed.
C
               AT = BEGS(1) + BADAT
 
 
               CALL SETMSG ( 'There is a non-printing character '
     .         //            'embedded in line # of the text '
     .         //            'buffer.  Non-printing '
     .         //            'characters are not allowed in '
     .         //            'kernel variable assignments.  The '
     .         //            'non-printing character has ASCII '
     .         //            'code #. ' )
 
 
               CALL ERRINT ( '#', LINNUM             )
               CALL ERRINT ( '#', ICHAR(LINE(AT:AT)) )
               CALL SIGERR ( 'SPICE(NONPRINTINGCHAR)')
               CALL CHKOUT ( 'ZZRVBF' )
               RETURN
            END IF


C
C           Check the variable name length; signal an error
C           if longer than MAXLEN.
C
            VARLEN = LEN( LINE( BEGS(1): ENDS(1) ) )

            IF ( VARLEN .GT. MAXLEN ) THEN
            
               CALL SETMSG ( 'A kernel pool variable name in the '
     .         //            'input buffer exceeds the maximum '
     .         //            'allowed length #1. The actual length '
     .         //            'of the variable name is #2, the '
     .         //            'offending variable name to #3 characters:'
     .         //            ' ''#4''.')

               CALL ERRINT ( '#1', MAXLEN                   )
               CALL ERRINT ( '#2', VARLEN                   )
               CALL ERRINT ( '#3', LINLEN                   )
               CALL ERRCH  ( '#4', LINE( BEGS(1): ENDS(1) ) )
               CALL SIGERR ( 'SPICE(BADVARNAME)'            )

            END IF
 
C
C           The variable name is ok. How about the directive.
C
            VARNAM = LINE(BEGS(1):ENDS(1))
            DIRCTV = TYPE(2)
 
 
C
C           If this is replacement (=) and not an addition (+=),
C           delete the values currently associated with the variable.
C           They will be replaced later.
C
            IF ( ISBAD(DIRCTV) ) THEN
 
               CALL SETMSG ( 'A kernel variable was not properly '
     .         //            'formed on line # of the text buffer. '
     .         //            'Such an assignment should have the '
     .         //            'form: ''<variable name> [+]= '
     .         //            '<values>''.  More specifically, '
     .         //            'the assignment operator did not have '
     .         //            'one of the expected forms: ''='' or '
     .         //            '''+=''. The line was ''#''. ' )
 
               R2 = RTRIM(LINE)
 
               CALL ERRINT ( '#', LINNUM     )
               CALL ERRCH  ( '#', LINE(1:R2) )
               CALL SIGERR ( 'SPICE(BADVARASSIGN)' )
               CALL CHKOUT ( 'ZZRVBF' )
               RETURN
 
            END IF
C
C           Locate this variable name in the name pool or insert it
C           if it isn't there.  The location will be NAMEAT and
C           we will use the variable FOUND to indicate whether or
C           not it was already present.
C
            LOOKAT =  ZZHASH ( VARNAM )
            NODE   =  NAMLST ( LOOKAT )
            FULL   =  LNKNFN ( NMPOOL ) .LE. 0
            FOUND  = .FALSE.
C
C           See if this name (or one colliding with it in the
C           hash scheme) has already been stored in the name list.
C
            IF ( NODE .GT. 0 ) THEN
 
               HEAD =  NODE
               TAIL = -NMPOOL(PREV,HEAD)
 
               DO WHILE ( NODE .GT. 0 .AND. .NOT. FOUND )
 
                  FOUND  =  NAMES (NODE) .EQ. VARNAM
                  NAMEAT =  NODE
                  NODE   =  NMPOOL(NEXT,NODE)
 
               END DO
 
               IF (       .NOT. FOUND
     .              .AND. .NOT. FULL   ) THEN
C
C                 We didn't find this name on the conflict resolution
C                 list. Allocate a new slot for it.
C
                  CALL LNKAN  ( NMPOOL, NODE         )
                  CALL LNKILA ( TAIL,   NODE, NMPOOL )
 
                  NAMES(NODE) =  VARNAM
                  NAMEAT      =  NODE
 
               END IF
 
            ELSE IF ( .NOT. FULL ) THEN
C
C              Nothing like this variable name (in the hashing sense)
C              has been loaded so far.  We need to allocate
C              a name slot for this variable.
C
               CALL LNKAN ( NMPOOL, NODE )
 
               NAMLST(LOOKAT) =  NODE
               NAMES (NODE  ) =  VARNAM
               NAMEAT         =  NODE
 
            END IF
C
C           If the name pool was full and we didn't find this name
C           we've got an error. Diagnose it and return.
C
            IF ( FULL .AND. .NOT. FOUND ) THEN
 
               CALL SETMSG ( 'The kernel pool does not have room '
     .         //            'for any more variables.  It filled '
     .         //            'up at line # of the text buffer. ' )
 
               CALL ERRINT ( '#', LINNUM             )
               CALL SIGERR ( 'SPICE(KERNELPOOLFULL)' )
               CALL CHKOUT ( 'ZZRVBF' )
               RETURN
 
            END IF
 
 
C
C           Now depending upon the kind of directive, we will need
C           to remove data and allocate a new list or simply append
C           data to the existing list.
C
            IF ( DIRCTV .EQ. EQ ) THEN
C
C              We are going to dump whatever is associated with
C              this name and then we will need to allocate a new
C              linked list for the data.
C
               VARTYP =  UNKNWN
 
               IF ( FOUND ) THEN
C
C                 We need to free the data associated with this
C                 variable.
C
                  DATAHD         = DATLST( NAMEAT )
                  DATLST(NAMEAT) = 0
 
                  IF ( DATAHD .LT. 0 ) THEN
C
C                    This variable was character type we need to
C                    free a linked list from the character data
C                    pool.
C
                     HEAD = - DATAHD
                     TAIL = - CHPOOL(PREV,HEAD)
 
                     CALL LNKFSL ( HEAD, TAIL, CHPOOL )
 
                  ELSE
C
C                    This variable was numeric type. We need to
C                    free a linked list from the numeric pool.
C
                     HEAD =   DATAHD
                     TAIL = - DPPOOL(PREV,HEAD)
 
                     CALL LNKFSL ( HEAD, TAIL, DPPOOL )
 
                  END IF
 
               END IF
 
 
            ELSE IF ( DIRCTV .EQ. EQP ) THEN
C
C              We need to append to the current variable.
C
               IF ( FOUND ) THEN
 
                  IF ( DATLST(NAMEAT) .GT. 0 ) THEN
                     VARTYP = NUMTYP
                  ELSE IF ( DATLST(NAMEAT) .LT. 0 ) THEN
                     VARTYP = STRTYP
                  ELSE
                     VARTYP = UNKNWN
                  END IF
 
               ELSE
                  VARTYP = UNKNWN
               END IF
 
            END IF
 
C
C           If this is a vector, the next thing on the line will be a
C           left parenthesis. Otherwise, assume that this is a scalar.
C           If it's a vector, get the first value. If it's a scalar,
C           plant a bogus right parenthesis, to make the following loop
C           terminate after one iteration.
C
 
            IF ( TYPE(3) .EQ. BV ) THEN
               NXTTOK        = 4
            ELSE
               NXTTOK      = 3
               COUNT       = COUNT + 1
               TYPE(COUNT) = EV
            END IF
 
C
C        For subsequent lines, treat everything as a new value.
C
         ELSE
 
            NXTTOK = 1
 
         END IF
 
C
C        We have a value anyway. Store it in the table.
C
C        Keep going until the other shoe (the right parenthesis)
C        drops, or until the end of the line is reached.
C
C        Dates begin with @; anything else is presumed to be a number.
C
         DO WHILE ( TYPE(NXTTOK) .NE. EV .AND. NXTTOK .LE. COUNT )
C
C           Get the begin and end of this token.
C
            B = BEGS(NXTTOK)
            E = ENDS(NXTTOK)
 
            IF ( VARTYP .EQ. UNKNWN ) THEN
C
C              We need to determine which category of variable we
C              have by looking at this token and deducing the
C              type.
C
               IF      ( TYPE(NXTTOK) .EQ. Q  ) THEN
 
                  VARTYP = STRTYP
 
               ELSE IF ( TYPE(NXTTOK) .EQ. NQ ) THEN
 
                  VARTYP = NUMTYP
 
               ELSE
C
C                 This is an error. We should have had one of the
C                 two previous types.
C
C                 First perform the clean up function.
C
                  CALL ZZCLN ( LOOKAT, NAMEAT,
     .                         NAMLST, DATLST, NMPOOL, CHPOOL, DPPOOL )
 
 
                  CALL SETMSG ( 'The first item following the '
     .            //            'assignment operator should be '
     .            //            'the value of a variable or a '
     .            //            'left parenthesis ''('' followed '
     .            //            'by a value for a variable. This '
     .            //            'is not true on line # of the '
     .            //            'text buffer. ' )
 
 
                  CALL ERRINT ( '#', LINNUM     )
                  CALL SIGERR ( 'SPICE(BADVARASSIGN)' )
                  CALL CHKOUT ( 'ZZRVBF' )
                  RETURN
 
               END IF
 
            END IF
 
 
            IF ( VARTYP .EQ. STRTYP ) THEN
C
C              First make sure that this token represents a string.
C
               IF ( TYPE(NXTTOK) .NE. Q ) THEN
C
C                 First perform the clean up function.
C
                  CALL ZZCLN ( LOOKAT, NAMEAT,
     .                         NAMLST, DATLST, NMPOOL, CHPOOL, DPPOOL )
 
                  R1 = RTRIM  ( VARNAM        )
 
                  CALL SETMSG ( 'The kernel variable # has '
     .            //            'been set up as a string '
     .            //            'variable.  However, the value '
     .            //            'that you are attempting to '
     .            //            'assign to this variable on line '
     .            //            '# of the text buffer is '
     .            //            'not a string value. ' )
 
                  CALL ERRCH  ( '#', VARNAM(1:R1) )
                  CALL ERRINT ( '#', LINNUM       )
                  CALL SIGERR ( 'SPICE(TYPEMISMATCH)' )
                  CALL CHKOUT ( 'ZZRVBF' )
                  RETURN
 
               END IF
C
C              Still going? Make sure there is something between
C              the quotes.
C
               IF ( B+1 .GE. E ) THEN
C
C                 First perform the clean up function.
C
                  CALL ZZCLN ( LOOKAT, NAMEAT,
     .                         NAMLST, DATLST, NMPOOL, CHPOOL, DPPOOL )
 
 
                  CALL SETMSG ( 'There is a quoted string with '
     .            //            'no characters on line # of the '
     .            //            'text buffer. ' )
 
                  CALL ERRINT ( '#', LINNUM       )
                  CALL SIGERR ( 'SPICE(TYPEMISMATCH)' )
                  CALL CHKOUT ( 'ZZRVBF' )
                  RETURN
 
               END IF
 
C
C              We are ready to go.  Allocate a node for this data
C              item. First make sure there is room to do so.
C
               FREE = LNKNFN ( CHPOOL )
 
               IF ( FREE .LE. 0 ) THEN
 
 
                  CALL SETMSG ( 'There is no room available for '
     .            //            'adding another character value '
     .            //            'to the kernel pool.  The '
     .            //            'character values buffer became '
     .            //            'full at line # of the text '
     .            //            'buffer. ' )
 
 
                  CALL ERRINT ( '#', LINNUM       )
                  CALL SIGERR ( 'SPICE(KERNELPOOLFULL)' )
                  CALL CHKOUT ( 'ZZRVBF' )
                  RETURN
 
 
               END IF
C
C              Allocate a node for storing this string value:
C
               CALL LNKAN ( CHPOOL, CHNODE )
 
               IF ( DATLST(NAMEAT) .EQ. 0 ) THEN
C
C                 There was no data for this name yet.  We make
C                 CHNODE be the head of the data list for this name.
C
                  DATLST(NAMEAT) = -CHNODE
 
               ELSE
C
C                 Put this node after the tail of the current list.
C
                  HEAD = -DATLST(NAMEAT)
                  TAIL = -CHPOOL(PREV, HEAD )
 
                  CALL LNKILA ( TAIL, CHNODE, CHPOOL )
 
               END IF
C
C              Finally insert this data item in the data buffer
C              at CHNODE.  Note any quotes will be doubled so we
C              have to undo this affect when we store the data.
C
               CHVALS(CHNODE) = ' '
               NCOMP          = NCOMP + 1
 
               I = 1
               J = B+1
 
               DO WHILE ( J .LT. E )
                  CODE = ICHAR(LINE(J:J))
 
                  IF ( ISQUOT(CODE) ) THEN
                     J = J+1
                  END IF
 
                  CHVALS(CHNODE)(I:I) = LINE(J:J)
                  I = I + 1
                  J = J + 1
               END DO
C
C              That's all for this value. It's now time to loop
C              back through and get the next value.
C
            ELSE
 
               IF ( TYPE(NXTTOK) .NE. NQ ) THEN
C
C                 First perform the clean up function.
C
                  CALL ZZCLN ( LOOKAT, NAMEAT,
     .                         NAMLST, DATLST, NMPOOL, CHPOOL, DPPOOL )
 
                  R1 = RTRIM  ( VARNAM        )
 
                  CALL SETMSG ( 'The kernel variable # has '
     .            //            'been set up as a numeric or time '
     .            //            'variable.  However, the value '
     .            //            'that you are attempting to '
     .            //            'assign to this variable on line '
     .            //            '# of the kernel buffer is '
     .            //            'not a numeric or time value. ' )
 
                  CALL ERRCH  ( '#', VARNAM(1:R1) )
                  CALL ERRINT ( '#', LINNUM       )
                  CALL SIGERR ( 'SPICE(TYPEMISMATCH)' )
                  CALL CHKOUT ( 'ZZRVBF' )
                  RETURN
 
               END IF
C
C              Look at the first character to see if we have a time
C              or a number.
C
               CODE = ICHAR( LINE(B:B) )
 
               IF ( ISTIME(CODE) ) THEN
C
C                 We need to have more than a single character.
C
                  IF ( E .EQ. B ) THEN
C
C                    First perform the clean up function.
C
                     CALL ZZCLN (LOOKAT, NAMEAT,
     .                           NAMLST, DATLST, NMPOOL, CHPOOL, DPPOOL)
                     R1 = RTRIM (VARNAM        )
 
                     CALL SETMSG ( 'At character # of  line # in '
     .               //            'the text buffer the '
     .               //            'character ''@'' appears.  This '
     .               //            'character is reserved for '
     .               //            'identifying time values in '
     .               //            'assignments to kernel pool '
     .               //            'variables.  However it is not '
     .               //            'being used in this fashion for '
     .               //            'the variable ''#''. ' )
 
                     CALL ERRINT ( '#', B            )
                     CALL ERRINT ( '#', LINNUM       )
                     CALL ERRCH  ( '#', VARNAM(1:R1) )
                     CALL SIGERR ( 'SPICE(BADTIMESPEC)' )
                     CALL CHKOUT ( 'ZZRVBF' )
                     RETURN
 
                  END IF
 
 
                  CALL TPARSE ( LINE(B+1:E), DVALUE, ERROR )
 
                  IF ( ERROR .NE. ' ' ) THEN
C
C                    First perform the clean up function.
C
                     CALL ZZCLN (LOOKAT, NAMEAT,
     .                           NAMLST, DATLST, NMPOOL, CHPOOL, DPPOOL)
 
                     CALL SETMSG ( 'Encountered ''#'' while '
     .               //            'attempting to parse a time '
     .               //            'on line # of the text buffer. ' )
 
                     CALL ERRCH  ( '#', LINE(B+1:E) )
                     CALL ERRINT ( '#', LINNUM      )
                     CALL SIGERR ( 'SPICE(BADTIMESPEC)' )
                     CALL CHKOUT ( 'ZZRVBF' )
                     RETURN
                  END IF
 
               ELSE
 
                  CALL NPARSD ( LINE(B:E), DVALUE, ERROR, I )
 
                  IF ( ERROR .NE. ' ' ) THEN
 
                     CALL ZZCLN (LOOKAT, NAMEAT,
     .                           NAMLST, DATLST, NMPOOL, CHPOOL, DPPOOL)
 
                     CALL SETMSG ( 'Encountered ''#'' while '
     .               //            'attempting to parse a number on '
     .               //            'line # of the text buffer' )
                     CALL ERRCH  ( '#',  LINE(B:E) )
                     CALL ERRINT ( '#',  LINNUM    )
                     CALL SIGERR ( 'SPICE(NUMBEREXPECTED)' )
                     CALL CHKOUT ( 'ZZRVBF' )
                     RETURN
                  END IF
 
               END IF
C
C              OK. We have a parsed value.  See if there is room in
C              the numeric portion of the pool to store this value.
C
               FREE = LNKNFN ( DPPOOL )
 
               IF ( FREE .LE. 0 ) THEN
 
 
                  CALL SETMSG ( 'There is no room available for '
     .            //            'adding another numeric value '
     .            //            'to the kernel pool.  The '
     .            //            'numeric values buffer became '
     .            //            'full at line # of the text '
     .            //            'buffer.' )
 
 
                  CALL ERRINT ( '#', LINNUM       )
                  CALL SIGERR ( 'SPICE(KERNELPOOLFULL)' )
                  CALL CHKOUT ( 'ZZRVBF' )
                  RETURN
 
 
               END IF
 
C
C              Allocate a node for storing this numeric value:
C
               CALL LNKAN ( DPPOOL, DPNODE )
 
               IF ( DATLST(NAMEAT) .EQ. 0 ) THEN
C
C                 There was no data for this name yet.  We make
C                 DPNODE be the head of the data list for this name.
C
                  DATLST(NAMEAT) =  DPNODE

               ELSE
C
C                 Put this node after the tail of the current list.
C
                  HEAD =  DATLST(NAMEAT)
                  TAIL = -DPPOOL(PREV, HEAD )
 
                  CALL LNKILA ( TAIL, DPNODE, DPPOOL )
 
               END IF
C
C              Finally insert this data item into the numeric buffer.
C
               DPVALS(DPNODE) = DVALUE
               NCOMP          = NCOMP + 1
 
            END IF
 
C
C           Now process the next token in the list of tokens.
C
            NXTTOK = NXTTOK + 1
 
         END DO
C
C        We could have ended the above loop in one of two ways.
C
C        1) NXTTOK now exceeds count.  This means we did not reach
C           an end of vector marker.
C        2) We hit an end of vector marker.
C
         IF ( NXTTOK .GT. COUNT ) THEN
            STATUS = INVAR
         ELSE
            STATUS = DONE
         END IF
 
      END DO


C
C     It is possible that we reached this point without actually
C     assigning a value to the kernel pool variable.  This can
C     happen if there is a vector input of the form NAME = ( )
C
      IF ( NCOMP .LT. 1 ) THEN
 
         CALL ZZCLN ( LOOKAT, NAMEAT,
     .                NAMLST, DATLST, NMPOOL, CHPOOL, DPPOOL )
 
         CALL SETMSG ( 'The first item following the '
     .   //            'assignment operator should be '
     .   //            'the value of a variable or a '
     .   //            'left parenthesis ''('' followed '
     .   //            'by a value for a variable. This '
     .   //            'is not true on line # of the '
     .   //            'text buffer. ' )
 
 
         CALL ERRINT ( '#', LINNUM-1   )
         CALL SIGERR ( 'SPICE(BADVARASSIGN)' )
         CALL CHKOUT ( 'ZZRVBF' )
         RETURN
 
      END IF
  
 
C
C     Return the name of the variable.
C
      NAME = VARNAM
 
 
      CALL CHKOUT ( 'ZZRVBF' )
      RETURN
      END
