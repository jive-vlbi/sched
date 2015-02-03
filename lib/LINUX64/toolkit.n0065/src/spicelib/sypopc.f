C$Procedure      SYPOPC ( Pop a value from a particular symbol )
 
      SUBROUTINE SYPOPC ( NAME, TABSYM, TABPTR, TABVAL, VALUE, FOUND )
 
C$ Abstract
C
C     Pop a value associated with a particular symbol in a character
C     symbol table. The first value associated with the symbol is
C     removed, and subsequent values are moved forward.
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
C     SYMBOLS
C
C$ Keywords
C
C     SYMBOLS
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      CHARACTER*(*)         NAME
      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      CHARACTER*(*)         TABVAL     ( LBCELL:* )
      CHARACTER*(*)         VALUE
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of the symbol whose associated value is to be
C                    popped.
C
C     TABSYM,
C     TABPTR,
C     TABVAL    I/O  Components of the symbol table.
C
C     VALUE      O   Value that was popped.
C     FOUND      O   True if the symbol exists, false if it does not.
C
C$ Detailed_Input
C
C     NAME       is the name of the symbol whose associated value is to
C                be popped. If NAME is not in the symbol table, FOUND
C                is false.
C
C     TABSYM,
C     TABPTR,
C     TABVAL     are the components of a character symbol table.
C
C$ Detailed_Output
C
C     TABSYM,
C     TABPTR,
C     TABVAL     are the components of a character symbol table.
C                The value is removed from the symbol table, and the
C                remaining values associated with the symbol are moved
C                forward in the value table. If no other values are
C                associated with the symbol, the symbol is removed from
C                the symbol table.
C
C     VALUE      is the value that was popped. This value was the first
C                value in the symbol table that was associated with the
C                symbol NAME.
C
C     FOUND      is true if NAME is in the symbol table, otherwise
C                it is false.
C
C$ Parameters
C
C     None.
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
C     If there are no remaining values associated with the symbol
C     after VALUE has been popped, the symbol is removed from the
C     symbol table.
C
C$ Examples
C
C     The contents of the symbol table are:
C
C        BOHR      -->   HYDROGEN ATOM
C        EINSTEIN  -->   SPECIAL RELATIVITY
C                        PHOTOELECTRIC EFFECT
C                        BROWNIAN MOTION
C        FERMI     -->   NUCLEAR FISSION
C
C     The call,
C
C     CALL SYPOPC ( 'EINSTEIN', TABSYM, TABPTR, TABVAL, VALUE, FOUND )
C
C     modifies the contents of the symbol table to be:
C
C        BOHR      -->   HYDROGEN ATOM
C        EINSTEIN  -->   PHOTOELECTRIC EFFECT
C                        BROWNIAN MOTION
C        FERMI     -->   NUCLEAR FISSION
C
C     FOUND is TRUE, and VALUE is 'SPECIAL RELATIVITY'.
C
C
C     The next call,
C
C     CALL SYPOPC ( 'FERMI', TABSYM, TABPTR, TABVAL, VALUE, FOUND )
C
C     modifies the contents of the symbol table to be:
C
C        BOHR      -->   HYDROGEN ATOM
C        EINSTEIN  -->   SPECIAL RELATIVITY
C                        PHOTOELECTRIC EFFECT
C                        BROWNIAN MOTION
C
C      FOUND is TRUE, and VALUE is 'NUCLEAR FISSION'. Note that because
C      "FERMI" had only one value associated with it, it was removed
C      from the symbol table.
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
C     H.A. Neilan     (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
C
C-&
 
C$ Index_Entries
C
C     pop a value from a particular symbol
C
C-&
 
 
 
 
 
C
C     SPICELIB functions
C
      INTEGER               BSRCHC
      INTEGER               CARDC
      INTEGER               CARDI
      LOGICAL               RETURN
      INTEGER               SUMAI
 
C
C     Local variables
C
      INTEGER               NSYM
      INTEGER               NPTR
      INTEGER               NVAL
 
      INTEGER               LOCSYM
      INTEGER               LOCVAL
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SYPOPC' )
      END IF
 
 
C
C     How many symbols to start with?
C
      NSYM = CARDC ( TABSYM )
      NPTR = CARDI ( TABPTR )
      NVAL = CARDC ( TABVAL )
 
C
C     Is this symbol even in the table?
C
      LOCSYM = BSRCHC ( NAME, NSYM, TABSYM(1) )
 
C
C     If it's not in the table, it's definitely a problem.
C
      IF ( LOCSYM .EQ. 0 ) THEN
 
         FOUND = .FALSE.
 
C
C     If it is in the table, we can proceed without fear of overflow.
C
      ELSE
 
         FOUND = .TRUE.
 
C
C        Begin by saving and removing the initial value for this
C        symbol from the value table.
C
         LOCVAL = SUMAI ( TABPTR(1), LOCSYM-1 ) + 1
         VALUE  = TABVAL(LOCVAL)
 
         CALL REMLAC ( 1, LOCVAL, TABVAL(1), NVAL )
         CALL SCARDC ( NVAL, TABVAL )
 
C
C        If this was the sole value for the symbol, remove the
C        symbol from the name and pointer tables. Otherwise just
C        decrement the dimension.
C
         IF ( TABPTR(LOCSYM) .EQ. 1 ) THEN
 
            CALL REMLAC ( 1, LOCSYM, TABSYM(1), NSYM )
            CALL SCARDC ( NSYM, TABSYM )
 
            CALL REMLAI ( 1, LOCSYM, TABPTR(1), NPTR )
            CALL SCARDI ( NPTR, TABPTR )
 
         ELSE
            TABPTR(LOCSYM) = TABPTR(LOCSYM) - 1
         END IF
 
      END IF
 
 
      CALL CHKOUT ( 'SYPOPC' )
      RETURN
      END
