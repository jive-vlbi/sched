C$Procedure      SYSETC ( Set the value associated with a symbol )
 
      SUBROUTINE SYSETC ( NAME, VALUE, TABSYM, TABPTR, TABVAL )
 
C$ Abstract
C
C     Set the value of a particular symbol in a character symbol table.
C     If the symbol already exists, the previous values associated with
C     it are removed, otherwise a new symbol is created.
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
      CHARACTER*(*)         VALUE
      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      CHARACTER*(*)         TABVAL     ( LBCELL:* )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of the symbol whose associated value is to be
C                    set.
C     VALUE      I   Associated value of the symbol NAME.
C
C     TABSYM,
C     TABPTR,
C     TABVAL    I/O  Components of the symbol table.
C
C$ Detailed_Input
C
C     NAME       is the name of the symbol whose associated value is to
C                be set. If NAME has values associated with it, they are
C                removed, and VALUE becomes the only value associated
C                with NAME. If NAME is not in the symbol table, a new
C                symbol is created, provided there is room in the
C                symbol table.
C
C     VALUE      is the new value associated with the symbol NAME.
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
C                If NAME has values associated with it, they are
C                removed, and VALUE becomes the only value associated
C                with NAME. If NAME is not in the symbol table, a new
C                symbol is created, provided there is room in the
C                symbol table.
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
C     1) If the addition of a new symbol causes an overflow in the
C        name table, the error SPICE(NAMETABLEFULL) is signalled.
C
C     2) If the addition of a new symbol causes an overflow in the
C        pointer table, the error SPICE(POINTERTABLEFULL) is signalled.
C
C     3) If the addition of a new symbolcauses an overflow in the
C        value table, the error SPICE(VALUETABLEFULL) is signalled.
C
C$ Particulars
C
C     If NAME has values associated with it, they are
C     removed, and VALUE becomes the only value associated
C     with NAME. If NAME is not in the symbol table, a new
C     symbol is created, provided there is room in the
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
C        PAULI     -->   EXCLUSION PRINCIPLE
C                        NEUTRINO
C
C     The call,
C
C     CALL SYSETC ( 'EINSTEIN', 'GENERAL RELATIVITY',
C    .               TABSYM,     TABPTR,      TABVAL  )
C
C     modifies the contents of the symbol table to be:
C
C        BOHR      -->   HYDROGEN ATOM
C        EINSTEIN  -->   GENERAL RELATIVITY
C        FERMI     -->   NUCLEAR FISSION
C        PAULI     -->   EXCLUSION PRINCIPLE
C                        NEUTRINO
C
C     Note that the previous values associated with the symbol
C     "EINSTEIN" have been deleted, and now only the new value is
C     associated with the symbol.
C
C
C     The next call,
C
C     CALL SYSETC ( 'MILLIKAN', 'PHOTOELECTRIC EFFECT'
C    .               TABSYM,     TABPTR,       TABVAL   )
C
C     modifies the contents of the symbol table to be:
C
C        BOHR      -->   HYDROGEN ATOM
C        EINSTEIN  -->   GENERAL RELATIVITY
C        FERMI     -->   NUCLEAR FISSION
C        MILLIKAN  -->   PHOTOELECTRIC EFFECT
C        PAULI     -->   EXCLUSION PRINCIPLE
C                        NEUTRINOC
C
C     Note that the new symbol "MILLIKAN" was created by the last call.
C     A new symbol is created only if there is room in the symbol
C     table.
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
C     set the value associated with a symbol
C
C-&
 
 
 
C
C     SPICELIB functions
C
 
      INTEGER               CARDC
      INTEGER               CARDI
      INTEGER               LSTLEC
      LOGICAL               RETURN
      INTEGER               SIZEC
      INTEGER               SIZEI
      INTEGER               SUMAI
 
C
C     Local variables
C
      INTEGER               NSYM
      INTEGER               NPTR
      INTEGER               NVAL
 
      INTEGER               LOCSYM
      INTEGER               LOCVAL
      INTEGER               DIMVAL
      LOGICAL               OLDSYM
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SYSETC' )
      END IF
 
C
C     How many symbols to start with?
C
      NSYM = CARDC ( TABSYM )
      NPTR = CARDI ( TABPTR )
      NVAL = CARDC ( TABVAL )
 
C
C     Where does this symbol belong? Is it already in the table?
C
      LOCSYM = LSTLEC ( NAME, NSYM, TABSYM(1) )
      OLDSYM = ( LOCSYM .NE. 0  .AND.  TABSYM(LOCSYM) .EQ. NAME )
 
C
C     If it's already in the table, there's no chance of overflow.
C     Leave the name where it is. Remove all but one of the existing
C     values, replacing that with the new value. And set the dimension
C     to one.
C
      IF ( OLDSYM ) THEN
 
         LOCVAL = SUMAI  ( TABPTR(1), LOCSYM-1 ) + 1
         DIMVAL = TABPTR(LOCSYM)
 
         IF ( DIMVAL .GT. 1 ) THEN
            CALL REMLAC ( DIMVAL-1, LOCVAL, TABVAL(1), NVAL )
            CALL SCARDC ( NVAL, TABVAL )
         END IF
 
         TABPTR(LOCSYM) = 1
         TABVAL(LOCVAL) = VALUE
 
C
C     Otherwise, we can't proceed unless we know that we have enough
C     room for one extra addition in all three tables.
C
      ELSE IF ( NSYM .GE. SIZEC ( TABSYM ) ) THEN
 
            CALL SETMSG ( 'SYSETC: Addition of the new symbol # '   //
     .                    'causes an overflow in the name table.' )
            CALL ERRCH  ( '#', NAME )
            CALL SIGERR ( 'SPICE(NAMETABLEFULL)' )
 
 
      ELSE IF ( NPTR .GE. SIZEI ( TABPTR ) ) THEN
 
            CALL SETMSG ( 'SYSETC: Addition of the new symbol # '     //
     .                    'causes an overflow in the pointer table.' )
            CALL ERRCH  ( '#', NAME )
            CALL SIGERR ( 'SPICE(POINTERTABLEFULL)' )
 
 
      ELSE IF ( NVAL .GE. SIZEC ( TABVAL ) ) THEN
 
            CALL SETMSG ( 'SYSETC: Addition of the new symbol #  '    //
     .                    'causes an overflow in the value table.' )
            CALL ERRCH  ( '#', NAME )
            CALL SIGERR ( 'SPICE(VALUETABLEFULL)' )
 
 
C
C     If there's room, add the new name to the name table. Give the
C     symbol dimension one, and put the value in the right place.
C
      ELSE
 
         CALL INSLAC ( NAME, 1, LOCSYM+1, TABSYM(1), NSYM )
         CALL SCARDC ( NSYM, TABSYM )
 
         CALL INSLAI ( 1, 1, LOCSYM+1, TABPTR(1), NPTR )
         CALL SCARDI ( NPTR, TABPTR )
 
         LOCVAL = SUMAI ( TABPTR(1), LOCSYM ) + 1
 
         CALL INSLAC ( VALUE, 1, LOCVAL, TABVAL(1), NVAL )
         CALL SCARDC ( NVAL, TABVAL )
 
      END IF
 
 
      CALL CHKOUT ( 'SYSETC' )
      RETURN
      END
