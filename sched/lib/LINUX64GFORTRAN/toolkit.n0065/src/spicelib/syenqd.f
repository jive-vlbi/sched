C$Procedure      SYENQD ( Enqueue a value onto a symbol )
 
      SUBROUTINE SYENQD ( NAME, VALUE, TABSYM, TABPTR, TABVAL )
 
C$ Abstract
C
C     Enqueue a value onto a particular symbol in a double precision
C     symbol table. If the symbol is not in the table, a new one
C     is created.
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
      DOUBLE PRECISION      VALUE
      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      DOUBLE PRECISION      TABVAL     ( LBCELL:* )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of the symbol onto which the value is
C                    enqueued.
C     VALUE      I   Value to be enqueued.
C     TABSYM,
C     TABPTR,
C     TABVAL    I/O  Components of the symbol table.
C
C$ Detailed_Input
C
C     NAME       is the name of the symbol onto which the value is to
C                be enqueued. If NAME is not in the symbol table, a new
C                symbol having the value VALUE is created.
C
C     VALUE      is the value to be enqueued onto the symbol, NAME.
C                The value is inserted in the value table after the
C                last value associated with the symbol.
C
C     TABSYM,
C     TABPTR,
C     TABVAL     are the components of a double precision symbol table.
C                The symbol NAME may or may not be in the symbol
C                table.
C
C$ Detailed_Output
C
C     TABSYM,
C     TABPTR,
C     TABVAL      are the components of a double precision symbol table.
C                 On output, the value table contains the new value
C                 in addition to the old values associated with the
C                 symbol NAME. The pointer table is updated to
C                 reflect the change in the dimension of the symbol.
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
C     If the symbol NAME is not in the symbol table, a new symbol is
C     created which has the value VALUE.
C
C$ Examples
C
C     The contents of the symbol table are:
C
C        DELTA_T_A -->   32.184
C        K         -->    1.657D-3
C        MEAN_ANOM -->    6.239996D0
C                         1.99096871D-7
C        ORBIT_ECC -->    1.671D-2
C
C     The call,
C
C     CALL SYENQD ( 'BODY399_POLE_RA', 0.0D0, TABSYM, TABPTR, TABVAL )
C
C     produces the symbol table:
C
C        BODY399_POLE_RA -->    0.0D0
C        DELTA_T_A       -->   32.184
C        K               -->    1.657D-3
C        MEAN_ANOM       -->    6.239996D0
C                               1.99096871D-7
C        ORBIT_ECC       -->    1.671D-2
C
C     Notice that the new symbol "BODY399_POLE_RA" has been created and
C     has the value 0.0D0 associated with it.
C
C     The next call,
C
C     CALL SYENQD ( 'BODY399_POLE_RA', -6.4061614D-1,
C    .               TABSYM, TABPTR, TABVAL           )
C
C     CALL SYENQD ( 'BODY399_POLE_RA', -8.386D-5,
C    .               TABSYM, TABPTR, TABVAL           )
C
C     then produces the symbol table:
C
C        BODY399_POLE_RA -->    0.0D0
C                              -6.4061614D-1
C                              -8.386D-5
C        DELTA_T_A       -->   32.184
C        K               -->    1.657D-3
C        MEAN_ANOM       -->    6.239996D0
C                               1.99096871D-7
C        ORBIT_ECC       -->    1.671D-2
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
C     enqueue a value onto a symbol
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
 
      INTEGER               CARDC
      INTEGER               CARDD
      INTEGER               LSTLEC
      LOGICAL               RETURN
      INTEGER               SIZED
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
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SYENQD' )
      END IF
 
C
C     How many symbols to start with?
C
      NSYM = CARDC ( TABSYM )
      NVAL = CARDD ( TABVAL )
 
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
 
         CALL SYSETD ( NAME, VALUE, TABSYM, TABPTR, TABVAL )
 
C
C     If it is in the table, we can't proceed unless we know that we
C     have enough room for one extra addition in the value table.
C
      ELSE IF (  NVAL .GE. SIZED ( TABVAL ) ) THEN
 
         CALL SETMSG ( 'SYENQD: The addition of the value $ to the '  //
     .                 'symbol # causes an overflow in the value '    //
     .                 'table.' )
         CALL ERRDP  ( '$', VALUE )
         CALL ERRCH  ( '#', NAME  )
         CALL SIGERR ( 'SPICE(VALUETABLEFULL)' )
 
C
C     If there's room, add the new value to the value table. Add one
C     to the dimension, and put the value in the right place.
C
      ELSE
 
         LOCVAL = SUMAI ( TABPTR(1), LOCSYM ) + 1
 
         CALL INSLAD ( VALUE, 1, LOCVAL, TABVAL(1), NVAL )
         CALL SCARDD ( NVAL, TABVAL )
 
         TABPTR(LOCSYM) = TABPTR(LOCSYM) + 1
 
      END IF
 
 
      CALL CHKOUT ( 'SYENQD' )
      RETURN
      END
