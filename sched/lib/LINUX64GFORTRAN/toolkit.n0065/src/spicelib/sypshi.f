C$Procedure      SYPSHI ( Push a value onto a particular symbol )
 
      SUBROUTINE SYPSHI ( NAME, VALUE, TABSYM, TABPTR, TABVAL )
 
C$ Abstract
C
C     Push a value onto a particular symbol in an integer symbol table.
C     The previous value(s) associated with the symbol is extended at
C     the front. A new symbol is created if necessary.
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
      INTEGER               VALUE
      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      INTEGER               TABVAL     ( LBCELL:* )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of the symbol onto which the value is to be
C                    pushed.
C     VALUE      I   Value that is to be pushed onto the symbol NAME.
C
C     TABSYM,
C     TABPTR,
C     TABVAL    I/O  Components of the symbol table.
C
C$ Detailed_Input
C
C     NAME       is the name of the symbol onto which the value is to
C                be pushed. If NAME is not in the symbol table, a new
C                symbol is created.
C
C     TABSYM,
C     TABPTR,
C     TABVAL     are the components of an integer symbol table.
C
C$ Detailed_Output
C
C     TABSYM,
C     TABPTR,
C     TABVAL     are the components of an integer symbol table.
C                The value is added to the symbol table at the front
C                of the previous value(s) associated with the symbol
C                NAME. If NAME is not originally in the symbol table,
C                a new symbol is created.
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
C     1) If the addition of the new value to the symbol table
C        causes an overflow in the value table, the error
C        SPICE(VALUETABLEFULL) is signalled.
C
C$ Particulars
C
C     If the symbol NAME is not in the symbol table, a new symbol
C     is created.
C
C$ Examples
C
C     The contents of the symbol table are:
C
C        books   -->   5
C        erasers -->   6
C        pencils -->  12
C                     18
C        pens    -->  10
C                     12
C                     24
C
C     The call,
C
C     CALL SYPSHI ( 'pencils', 45, TABSYM, TABPTR, TABVAL )
C
C     modifies the contents of the symbol table to be:
C
C        books   -->   5
C        erasers -->   6
C        pencils -->  45
C                     12
C                     18
C        pens    -->  10
C                     12
C                     24
C     The next call,
C
C     CALL SYPSHC ( 'desks', 31, TABSYM, TABPTR, TABVAL )
C
C     modifies the contents of the symbol table to be:
C
C        books   -->   5
C        desks   -->  31
C        erasers -->   6
C        pencils -->  45
C                     12
C                     18
C        pens    -->  10
C                     12
C                     24
C
C     Note that a new symbol "desks" was created by the last call.
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
C     push a value onto a particular symbol
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
 
      INTEGER               CARDC
      INTEGER               CARDI
      INTEGER               LSTLEC
      LOGICAL               RETURN
      INTEGER               SIZEI
      INTEGER               SUMAI
 
C
C     Local variables
C
      INTEGER               NSYM
      INTEGER               NVAL
 
      INTEGER               LOCSYM
      INTEGER               LOCVAL
      LOGICAL               OLDSYM
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SYPSHI' )
      END IF
 
C
C     How many symbols to start with?
C
      NSYM = CARDC ( TABSYM )
      NVAL = CARDI ( TABVAL )
 
C
C     Where does this symbol belong? Is it already in the table?
C
      LOCSYM = LSTLEC ( NAME, NSYM, TABSYM(1) )
      OLDSYM = ( LOCSYM .NE. 0  .AND.  TABSYM(LOCSYM) .EQ. NAME )
 
C
C     If it's not already in the table, use SET to create a brand new
C     symbol.
C
      IF ( .NOT. OLDSYM ) THEN
 
         CALL SYSETI ( NAME, VALUE, TABSYM, TABPTR, TABVAL )
 
C
C     If it is in the table, we can't proceed unless we know that we
C     have enough room for one extra addition in the value table.
C
      ELSE IF (  NVAL .GE. SIZEI ( TABVAL ) ) THEN
 
         CALL SETMSG ( 'SYPSHI: The addition of the value $ to the '  //
     .                 'symbol # causes an overflow in the value '    //
     .                 'table.' )
         CALL ERRINT ( '$', VALUE )
         CALL ERRCH  ( '#', NAME  )
         CALL SIGERR ( 'SPICE(VALUETABLEFULL)' )
 
C
C     If there's room, add the new value to the value table. Add one
C     to the dimension, and put the value in the right place.
C
      ELSE
 
         LOCVAL = SUMAI ( TABPTR(1), LOCSYM-1 ) + 1
 
         CALL INSLAI ( VALUE, 1, LOCVAL, TABVAL(1), NVAL )
         CALL SCARDI ( NVAL, TABVAL )
 
         TABPTR(LOCSYM) = TABPTR(LOCSYM) + 1
 
      END IF
 
 
      CALL CHKOUT ( 'SYPSHI' )
      RETURN
      END
