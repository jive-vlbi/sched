C$Procedure     STRAN
 
      SUBROUTINE STRAN ( INPUT, OUTPUT, TRAN )
 
C$ Abstract
C
C     Translate the symbols in an input string.
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
C$ Keywords
C
C     PARSE
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         INPUT
      CHARACTER*(*)         OUTPUT
      LOGICAL               TRAN
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     INPUT      I   Input string containing symbols to be translated.
C     OUTPUT     O   Output string, with all symbols translated.
C
C$ Detailed_Input
C
C     INPUT      is the input string to be translated. INPUT may contain
C                any number of known symbols.
C
C
C$ Detailed_Output
C
C     OUTPUT     is the translation of the input string. The first
C                of the symbols in INPUT will have been translated.
C                When INPUT is either a DEFINE or an UNDEFINE command,
C                OUTPUT is blank.
C
C                OUTPUT may overwrite INPUT.
C
C$ Input_Files
C
C     None.
C
C$ Output_Files
C
C     None.
C
C$ Input_Output_Common
C
C     None.
C
C$ Exceptions
C
C     The following exceptions are detected by this routine:
C
C     1)  Attempt to define or undefine a symbol that does
C         not begin with a letter.
C
C     2)  Attempt to define or undefine a symbol that ends with
C         a question mark '?' .
C
C     3)  Failure to specify a symbol to define or undefine.
C
C     4)  Attempting to define a reserved word.  The reserved
C         words are:
C
C            'START'
C            'STOP'
C            'EXIT'
C            'INQUIRE'
C            'SHOW'
C            'DEFINE'
C            'SHOW'
C            'UNDEFINE'
C            'HELP'
C
C      In all of the above cases OUTPUT is set to blank and TRAN to
C      FALSE.  No new symbol is placed in the table of symbol
C      definitions.
C
C      In all of these cases the error BAD_SYMBOL_SPC is signalled.
C
C      5) Recursive symbol definitions are detected and disallowed.
C         A long error message diagnosing the problem is set and
C         the error RECURSIVE_SYMBOL is signalled.
C
C      5) Overflow of the input command caused by symbol resolution.
C
C         In this case the OUTPUT is left at the state it had reached
C         prior to the overflow condition and TRAN is returned as
C         FALSE. The error SYMBOL_OVERFLOW is signalled.
C
C$ Detailed_Description
C
C     A new symbol may be defined with the DEFINE command. The
C     syntax is:
C
C            DEFINE  <symbol>  <definition>
C
C     where <symbol> is a valid symbol name and <definition> is any
C     valid definition. The DEFINE command, the symbol name, and the
C     definition are delimited by blanks.
C
C     When a symbol is defined, the symbol and definition are inserted
C     into the symbol table.
C
C     An existing symbol may be removed from the table with the
C     UNDEFINE command. The syntax is:
C
C            UNDEFINE <symbol>
C
C     where <symbol> is the name of an existing symbol. The UNDEFINE
C     command and the symbol name are delimited by blanks.
C
C     If the input string does not contain a definition statement,
C     STRANS searches the input string for potential symbol names.
C     When a valid symbol is encountered, it is removed from the
C     string and replaced by the corresponding definition. This
C     continues until no untranslated symbols remain.
C
C$ Examples
C
C     Suppose that we are given the following definitions:
C
C            DEFINE  BODIES      PLANET AND SATS
C            DEFINE  EUROPA      502
C            DEFINE  GANYMEDE    503
C            DEFINE  IO          501
C            DEFINE  JUPITER     599
C            DEFINE  PLANET      JUPITER
C            DEFINE  CALLISTO    504
C            DEFINE  SATS        IO EUROPA GANYMEDE CALLISTO
C
C      Then the string 'BODIES AND SOULS' would translate,
C      at various stages, to:
C
C           'PLANET AND SATS AND SOULS'
C
C           'JUPITER AND SATS AND SOULS'
C
C           '599 AND SATS AND SOULS'
C
C           '599 AND IO EUROPA GANYMEDE CALLISTO AND SOULS'
C
C           '599 AND 501 EUROPA GANYMEDE CALLISTO AND SOULS'
C
C           '599 AND 501 502 GANYMEDE CALLISTO AND SOULS'
C
C           '599 AND 501 502 503 CALLISTO AND SOULS'
C
C           '599 AND 501 502 503 504 AND SOULS'
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     I. M. Underwood (JPL)
C
C$ Version_and_Date
C
C     Version 1.2.0 29-Aug-1996 (WLT)
C
C        Fixed the error message for the case in which someone
C        tries to create a symbol that is more than 32 characters
C        in length.
C
C     Version 1.1, 14-SEP-1995
C
C        Reference to unused variable WORD deleted.
C
C     Version 1,    8-SEP-1986
C
C-&
C     SPICELIB Functions
C
      INTEGER               CARDC
      INTEGER               ISRCHC
      INTEGER               LASTNB
      INTEGER               NCPOS
      INTEGER               RTRIM
      LOGICAL               FAILED
      LOGICAL               MATCHM
      LOGICAL               RETURN
C
C     Other supporting functions
C
      LOGICAL               BATCH
 
C
C     The following parameters are used to define our table
C     of symbol translations.
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               LBCBUF
      PARAMETER           ( LBCBUF = 0  )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 1024 )
 
C
C     Longest allowed symbol name is given by WDSIZE
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE  = 32 )
C
C     Maximum number of allowed symbols is MAXN
C
      INTEGER               MAXN
      PARAMETER           ( MAXN   = 200 )
C
C     The longest we expect any symbol to be is MAXL characters
C
      INTEGER               MAXL
      PARAMETER           ( MAXL = 256 )
C
C     The average number of characters per symbol is AVGL
C
      INTEGER               AVGL
      PARAMETER           ( AVGL = 64 )
 
 
      INTEGER               PTRSIZ
      PARAMETER           ( PTRSIZ = MAXN*4 + 4 )
 
      INTEGER               MAXCHR
      PARAMETER           ( MAXCHR = MAXN *AVGL )
 
      INTEGER               NLINES
      PARAMETER           ( NLINES = MAXCHR/MAXL + 1 )
C
C     Finally, here are the arrays used to hold the symbol translations.
C
 
      CHARACTER*(WDSIZE)    NAMES  ( LBCELL: MAXN   )
      INTEGER               PTRS   ( LBCELL: PTRSIZ )
      CHARACTER*(MAXL)      BUFFER ( LBCBUF: NLINES )
 
C
C     Here's the storage we need for the reserved words.
C
      INTEGER               NRES
      PARAMETER           ( NRES = 12 )
 
      CHARACTER*(1)         DELIM
 
 
      CHARACTER*(WDSIZE)    RESVRD ( NRES )
 
      INTEGER               BIGWRD
      PARAMETER           ( BIGWRD = 33 )
 
      CHARACTER*(BIGWRD)    SYMBL
      CHARACTER*(BIGWRD)    SYMBOL
 
      CHARACTER*(WDSIZE)    KEY
 
      CHARACTER*(80)        PATTRN
      CHARACTER*(80)        MYPRMT
 
 
      CHARACTER*(WDSIZE)    ALPHAB
      CHARACTER*(LNSIZE)    DEF
 
      CHARACTER*(1)         EQUOTE
 
 
      INTEGER               I
      INTEGER               J
      INTEGER               L
      INTEGER               LDEF
      INTEGER               LENO
      INTEGER               LOC
      INTEGER               LOUT
      INTEGER               LSTTRY
      INTEGER               LSYM
      INTEGER               N
      INTEGER               NNAME
      INTEGER               NXTCHR
      INTEGER               PLACE
      INTEGER               PSIZE
      INTEGER               SLOT
      INTEGER               VDIM
 
      LOGICAL               CHECK ( MAXN )
      LOGICAL               CHECKD( MAXN )
      LOGICAL               FIRST
      LOGICAL               GOTONE
      LOGICAL               NEW
 
      SAVE
 
      DATA                  FIRST / .TRUE. /
C
C     Set up all of the data structures and special strings in
C     the first pass through the routine.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'STRAN' )
 
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         VDIM  = NLINES
         PSIZE = PTRSIZ
         NNAME = MAXN
 
         CALL SBINIT_1 ( NNAME, PSIZE, VDIM, NAMES,
     .                                       PTRS,
     .                                       BUFFER )
 
         RESVRD(  1 ) = 'START'
         RESVRD(  2 ) = 'STOP'
         RESVRD(  3 ) = 'EXIT'
         RESVRD(  4 ) = 'INQUIRE'
         RESVRD(  5 ) = 'SHOW'
         RESVRD(  6 ) = 'DEFINE'
         RESVRD(  7 ) = 'SHOW'
         RESVRD(  8 ) = 'UNDEFINE'
         RESVRD(  9 ) = 'HELP'
         RESVRD( 10 ) = 'RECALL'
         RESVRD( 11 ) = 'DO'
         RESVRD( 12 ) = 'EDIT'
 
         ALPHAB       = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
 
 
      END IF
C
C     Find out what the special marker character is for suppressing
C     symbol evaluation.
C
      CALL GETEQ ( EQUOTE )
 
C
C     Is this a definition statement? The presence of DEFINE, INQUIRE or
C     UNDEFINE at the beginning of the string will confirm this.
C
      CALL NTHWD ( INPUT, 1, KEY, LOC )
      CALL UCASE ( KEY,      KEY      )
 
C
C     The keyword must be followed by a valid symbol name.
C
      IF (      KEY .EQ. 'DEFINE'
     .     .OR. KEY .EQ. 'INQUIRE'
     .     .OR. KEY .EQ. 'UNDEFINE' ) THEN
 
         CALL NTHWD ( INPUT, 2, SYMBL,  LOC )
         CALL UCASE ( SYMBL,    SYMBOL      )
         L =  RTRIM ( SYMBOL )
 
         IF ( SYMBOL .EQ. ' ' ) THEN
 
            OUTPUT = ' '
            TRAN   = .FALSE.
            CALL SETMSG ( 'The "#" command must be followed by the '
     .      //            'name of the symbol that you want to '
     .      //            '#. ' )
            CALL ERRCH  ( '#', KEY )
            CALL LCASE  ( KEY, KEY )
            CALL ERRCH  ( '#', KEY )
            CALL SIGERR ( 'BAD_SYMBOL_SPEC' )
            CALL CHKOUT ( 'STRAN' )
            RETURN
 
         ELSE IF ( INDEX ( ALPHAB, SYMBOL(1:1) ) .EQ. 0 ) THEN
 
 
            OUTPUT = ' '
            TRAN   = .FALSE.
            CALL LCASE ( KEY, KEY )
 
            CALL SETMSG ( 'You cannot # "#".  Symbols must begin '
     .      //            'with a letter (A-Z) ' )
            CALL ERRCH  ( '#', KEY    )
            CALL ERRCH  ( '#', SYMBOL )
            CALL SIGERR ( 'BAD_SYMBOL_SPEC' )
            CALL CHKOUT ( 'STRAN' )
            RETURN
 
         ELSE IF ( L .GT. 32 ) THEN
 
            OUTPUT = ' '
            TRAN   = .FALSE.
            CALL LCASE ( KEY, KEY )
            CALL SETMSG ( 'You cannot # "#...".  Symbols may not '
     .      //            'be longer than 32 characters in length.' )
            CALL ERRCH  ( '#', KEY    )
            CALL ERRCH  ( '#', SYMBOL )
            CALL SIGERR ( 'BAD_SYMBOL_SPEC' )
            CALL CHKOUT ( 'STRAN' )
            RETURN
 
         ELSE IF ( SYMBOL(L:L) .EQ. '?' ) THEN
 
            OUTPUT = ' '
            TRAN   = .FALSE.
            CALL LCASE ( KEY, KEY )
 
            CALL SETMSG ( 'You cannot # "#".  Symbols may not '
     .      //            'end with a question mark ''?''. ' )
            CALL ERRCH  ( '#', KEY    )
            CALL ERRCH  ( '#', SYMBOL )
            CALL SIGERR ( 'BAD_SYMBOL_SPEC' )
            CALL CHKOUT ( 'STRAN' )
            RETURN
 
         ELSE IF (     ( KEY .EQ. 'DEFINE' .OR. KEY .EQ. 'INQUIRE' )
     .             .AND. ISRCHC ( SYMBOL, NRES, RESVRD ) .GT. 0 ) THEN
 
 
            OUTPUT = ' '
            TRAN   = .FALSE.
 
            CALL SETMSG ( 'The word ''#'' is a reserved word. '
     .      //            'You may not redefine it. ' )
            CALL ERRCH  ( '#', SYMBOL )
            CALL SIGERR ( 'BAD_SYMBOL_SPEC' )
            CALL CHKOUT ( 'STRAN' )
            RETURN
 
         END IF
 
      END IF
 
 
      IF ( KEY .EQ. 'INQUIRE' ) THEN
C
C        First of all we, can only INQUIRE for symbol definitions
C        if the program is not running in "batch" mode.
C
         IF ( BATCH() ) THEN
 
            CALL SETMSG ( 'You''ve attempted to INQUIRE for the '
     .      //            'value of a symbol while the program '
     .      //            'is running in "batch" mode. You can '
     .      //            'INQUIRE for a symbol value only if '
     .      //            'you are running in INTERACTIVE mode. ' )
            CALL SIGERR ( 'WRONG_MODE' )
            CALL CHKOUT ( 'STRAN' )
            RETURN
 
         END IF
C
C        See if there is anything following the symbol that is
C        to be defined.  This will be used as our prompt value.
C
         NXTCHR = MAX( LOC+L, NCPOS ( INPUT, ' ', LOC + L ) )
 
         IF ( INPUT(NXTCHR:) .NE. ' ' ) THEN
 
            MYPRMT = INPUT(NXTCHR:)
 
         ELSE
 
            MYPRMT = 'Enter definition for'
            CALL SUFFIX ( SYMBOL, 1, MYPRMT )
            CALL SUFFIX ( '>',    1, MYPRMT )
 
         END IF
 
         CALL GETDEL ( DELIM  )
         CALL RDSTMN ( MYPRMT, DELIM, DEF )
 
         CALL SBSET_1 ( SYMBOL, DEF, NAMES,
     .                               PTRS,
     .                               BUFFER )
 
      END IF
C
C     If this is a definition, and the symbol already exists in the
C     symbol table, simply replace the existing definition with the
C     string following the symbol name. If this is a new symbol,
C     find the first symbol in the list that should follow the new
C     one. Move the rest of the symbols back, and insert the new one
C     at this point.
C
      IF ( KEY .EQ. 'DEFINE' ) THEN
 
         NXTCHR = MAX( LOC+L, NCPOS ( INPUT, ' ', LOC + L ) )
 
         CALL SBSET_1 ( SYMBOL, INPUT(NXTCHR:), NAMES,
     .                                          PTRS,
     .                                          BUFFER )
 
      END IF
 
      IF ( KEY .EQ. 'DEFINE'  .OR. KEY .EQ. 'INQUIRE' ) THEN
 
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'STRAN' )
            RETURN
         END IF
C
C        Now check for a recursive definition.  To do this we have
C        two parallel arrays to the NAMES array of the string
C        buffer.  The first array CHECK is used to indicate that
C        in the course of the definition resolution of the
C        new symbol, another symbol shows up.  The second array
C        called CHECKD indicats whether or not we have examined this
C        existing symbol to see if contains the newly created
C        symbol as part of its definition.
C
C        So far we have nothing to check and haven't checked anything.
C
         N = CARDC ( NAMES )
 
         DO J = 1, N
            CHECK (J) = .FALSE.
            CHECKD(J) = .FALSE.
         END DO
C
C        Find the location of our new symbol in the NAMES cell.
C
 
         PLACE = ISRCHC ( SYMBOL, N, NAMES(1) )
 
         NEW = .TRUE.
 
         DO WHILE ( NEW )
C
C           Look up the definition currently associated with
C           the symbol we are checking.
C
            CALL SBGET_1 ( SYMBOL, NAMES,
     .                             PTRS,
     .                             BUFFER, DEF, I )
 
            J = 1
            CALL NTHUQW ( DEF, J, EQUOTE, SYMBOL, LOC )
 
            DO WHILE ( LOC .GT. 0 )
 
               CALL   UCASE  ( SYMBOL, SYMBOL )
               SLOT = ISRCHC ( SYMBOL, N, NAMES(1) )
C
C              If the word is located in the same place as the
C              symbol we've just defined, we've introduced
C              a recursive symbol definition.  Remove this
C              symbol and diagnose the error.
C
               IF ( SLOT .EQ. PLACE ) THEN
 
                  OUTPUT = ' '
                  TRAN   = .FALSE.
                  SYMBOL =  NAMES(PLACE)
                  CALL SBREM_1 ( SYMBOL, NAMES, PTRS, BUFFER )
 
 
                  CALL SETMSG ( 'The definition of ''#'' is '
     .            //            'recursive.  Recursively defined '
     .            //            'symbol definitions are not '
     .            //            'allowed. ' )
 
                  CALL ERRCH  ( '#', SYMBOL )
                  CALL SIGERR ( 'RECURSIVE_SYMBOL'  )
                  CALL CHKOUT ( 'STRAN' )
                  RETURN
 
               ELSE IF ( SLOT .GT. 0 ) THEN
C
C                 Otherwise if this word is in the names list
C                 we may need to check this symbol to see if
C                 it lists the just defined symbol in its definition.
C
                  IF ( CHECKD(SLOT) ) THEN
                     CHECK(SLOT) = .FALSE.
                  ELSE
                     CHECK(SLOT) = .TRUE.
                  END IF
 
               END IF
C
C              Locate the next unquoted word in the definition.
C
               J = J + 1
               CALL NTHUQW ( DEF, J, EQUOTE, SYMBOL, LOC )
 
            END DO
C
C           See if there are any new items to check.  If there
C           are create a new value for symbol, and mark the
C           new item as being checked.
C
            NEW = .FALSE.
 
            DO J = 1, N
 
               IF ( CHECK(J) .AND. .NOT. NEW ) THEN
                  SYMBOL    =  NAMES(J)
                  CHECK (J) = .FALSE.
                  CHECKD(J) = .TRUE.
                  NEW       = .TRUE.
               END IF
 
            END DO
 
 
         END DO
C
C        If we get to this point, we have a new non-recursively
C        defined symbol.
C
         OUTPUT = ' '
         TRAN   = .FALSE.
         CALL CHKOUT ( 'STRAN' )
         RETURN
 
      END IF
 
 
 
C
C     If this is a deletion, and the symbol already exists in the
C     symbol table, simply move the symbols that follow toward the
C     front of the table.
C
      IF ( KEY .EQ. 'UNDEFINE' ) THEN
 
         CALL SBREM_1 ( SYMBOL, NAMES,
     .                          PTRS,
     .                          BUFFER )
         OUTPUT = ' '
         TRAN   = .FALSE.
         CALL CHKOUT ( 'STRAN' )
         RETURN
 
      END IF
 
 
 
C
C     This is not a definition statement. Look for potential symbols.
C     Try to resolve the first symbol in the string by substituting the
C     corresponding definition for the existing symbol.
C
      OUTPUT = INPUT
      TRAN   = .FALSE.
      J      = 1
 
      CALL NTHUQW ( OUTPUT, J, EQUOTE, SYMBOL, LOC )
 
      DO WHILE ( ( .NOT. TRAN ) .AND. ( SYMBOL .NE. ' ' ) )
 
         CALL UCASE   ( SYMBOL, SYMBOL )
         CALL SBGET_1 ( SYMBOL, NAMES,
     .                             PTRS,
     .                             BUFFER, DEF, I )
 
         IF ( I .GT. 0 ) THEN
 
            LSYM   = LASTNB ( SYMBOL )
            LDEF   = LASTNB ( DEF    ) + 1
            LOUT   = LASTNB ( OUTPUT )
            LENO   = LEN    ( OUTPUT )
 
 
            IF ( LOUT - LSYM + LDEF .GT. LENO ) THEN
 
               TRAN = .FALSE.
 
               CALL SETMSG ( 'As a result of attempting to '
     .         //            'resolve the symbols in the input '
     .         //            'command, the command has '
     .         //            'overflowed the allocated memory. '
     .         //            'This is may be due to '
     .         //            'unintentionally using symbols that '
     .         //            'you had not intended to use.  You '
     .         //            'may protect portions of your '
     .         //            'string from symbol evaluation by '
     .         //            'enclosing that portion of your '
     .         //            'string between the character # as '
     .         //            'in ''DO #THIS PART WITHOUT '
     .         //            'SYMBOLS#'' . ' )
 
               CALL ERRCH  ( '#', EQUOTE )
               CALL ERRCH  ( '#', EQUOTE )
               CALL ERRCH  ( '#', EQUOTE )
               CALL SIGERR ( 'SYMBOL_OVERFLOW' )
               CALL CHKOUT ( 'STRAN' )
               RETURN
 
            END IF
 
            CALL REPSUB ( OUTPUT, LOC, LOC+LSYM-1, DEF(1:LDEF),
     .                    OUTPUT )
 
            TRAN = .TRUE.
 
         ELSE
 
            J = J + 1
 
         END IF
 
         CALL NTHUQW ( OUTPUT, J, EQUOTE, SYMBOL, LOC )
 
      END DO
 
      CALL CHKOUT ( 'STRAN' )
      RETURN
 
C
C     The following entry point allows us to set up a search
C     of defined symbols that match a wild-card pattern.  It must
C     be called prior to getting any symbol definitions.
C
      ENTRY SYMPAT ( INPUT )
 
         LSTTRY = 0
         PATTRN = INPUT
         RETURN
 
C
C     The following entry point fetches the next symbol and its
C     definition for the next SYMBOL whose name
C     matches a previously supplied template via the entry point
C     above --- SYMPAT.
C
C     If there is no matching symbol, we get back blanks.  Note
C     that no translation of the definition is performed.
C
      ENTRY SYMGET ( INPUT, OUTPUT )
 
         INPUT  = ' '
         OUTPUT = ' '
         N      = CARDC(NAMES)
         DO WHILE ( LSTTRY .LT. N )
 
            LSTTRY = LSTTRY + 1
            GOTONE = MATCHM ( NAMES(LSTTRY), PATTRN, '*','%','~','|' )
 
            IF ( GOTONE ) THEN
 
               SYMBOL = NAMES(LSTTRY)
               INPUT  = NAMES(LSTTRY)
 
               CALL SBGET_1 ( SYMBOL, NAMES,
     .                                PTRS,
     .                                BUFFER, OUTPUT, I )
               RETURN
 
            END IF
 
         END DO
         RETURN
 
 
      END
