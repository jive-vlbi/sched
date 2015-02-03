C$Procedure      SYPUTD ( Set the values associated with a symbol )
 
      SUBROUTINE SYPUTD ( NAME, VALUES, N, TABSYM, TABPTR, TABVAL )
 
C$ Abstract
C
C     Set the values of a particular symbol in a double precision
C     symbol table. If the symbol already exists, the previous values
C     associated with it are removed, otherwise a new symbol is created.
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
      DOUBLE PRECISION      VALUES     (        * )
      INTEGER               N
      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      DOUBLE PRECISION      TABVAL     ( LBCELL:* )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of the symbol whose associated values are to
C                    be put into the symbol table.
C     VALUES     I   Values to be associated with the symbol NAME.
C     N          I   Number of values in VALUES.
C
C     TABSYM,
C     TABPTR,
C     TABVAL    I/O  Components of the symbol table.
C
C$ Detailed_Input
C
C     NAME       is the name of the symbol whose associated values are
C                to be set. If NAME has values associated with it,
C                they are removed, and the elements of VALUES become
C                the values associated with NAME. If NAME is not in the
C                symbol table, a new symbol is created, provided there
C                is room in the symbol table.
C
C     VALUES     are the new values associated with the symbol NAME.
C
C     N          is the number of elements in the VALUES array.
C                If N < 1, the symbol table is not modified.
C
C     TABSYM,
C     TABPTR,
C     TABVAL     are the components of a double precision symbol table.
C
C$ Detailed_Output
C
C     TABSYM,
C     TABPTR,
C     TABVAL     are the components of a double precision symbol table.
C                If NAME has values associated with it, they are
C                removed, and the elements of VALUES become the
C                values associated with NAME. If NAME is not in the
C                symbol table, a new symbol is created, provided
C                there is room in the symbol table.
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
C     3) If the addition of new values causes an overflow in the
C        value table, the error SPICE(VALUETABLEFULL) is signalled.
C
C     4) If N < 1, the error SPICE(INVALIDARGUMENT) is signalled.
C
C$ Particulars
C
C     This subroutine is like SYSETC, but SYPUTC allows several values
C     to be associated with a symbol.                   -------
C
C     If NAME has values associated with it, they are removed, and
C     the elements of VALUES become the values associated with NAME.
C     If NAME is not in the symbol table, a new symbol is created,
C     provided there is room in the symbol table.
C
C$ Examples
C
C     The contents of the symbol table are:
C
C        BODY4_POLE_RA -->    3.17681D2
C                             1.08D-1
C                             0.0D0
C        DELTA_T_A     -->    3.2184D1
C        K             -->    1.657D-3
C        MEAN_ANOM     -->    6.239996D0
C                             1.99096871D-7
C        ORBIT_ECC     -->    1.671D-2
C
C     If VALUES contains the elements,
C
C          3.17692D2
C          1.085D-1
C          1.000D-5
C
C     the call
C
C     CALL SYPUTC ( 'BODY4_POLE_RA', VALUES, 3, TABSYM, TABPTR, TABVAL )
C
C     modifies the contents of the symbol table to be:
C
C        BODY4_POLE_RA -->    3.17692D2
C                             1.085D-1
C                             1.000D-5
C        DELTA_T_A     -->    3.2184D1
C        K             -->    1.657D-3
C        MEAN_ANOM     -->    6.239996D0
C                             1.99096871D-7
C        ORBIT_ECC     -->    1.671D-2C
C
C     The call,
C
C     CALL SYPUTC ( 'K', VALUES, 3, TABSYM, TABPTR, TABVAL )
C
C     modifies the contents of the symbol table to be:
C
C        BODY4_POLE_RA -->    3.17692D2
C                             1.085D-1
C                             1.000D-5
C        DELTA_T_A     -->    3.2184D1
C        K             -->    3.17692D2
C                             1.085D-1
C                             1.000D-5
C        MEAN_ANOM     -->    6.239996D0
C                             1.99096871D-7
C        ORBIT_ECC     -->    1.671D-2
C
C     Note that the previous values associated with "K" have been
C     replaced by the values in VALUES.
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
C     N.J. Bachman    (JPL)
C     H.A. Neilan     (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-     SPICELIB Version 1.0.2, 06-AUG-1996 (WLT)
C
C         Fixed the error in the abstract noticed by Ian Jordan
C         at the University of Maryland, College Park.
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
C     set the values associated with a symbol
C
C-&
 
 
C$ Revisions
C
C-     Beta Version 1.1.0, 17-FEB-1989 (NJB)
C
C         Declaration of the unused variable I removed.
C
C-&
 
 
C
C     SPICELIB functions
C
 
      INTEGER               CARDC
      INTEGER               CARDD
      INTEGER               CARDI
      INTEGER               LSTLEC
      LOGICAL               RETURN
      INTEGER               SIZEC
      INTEGER               SIZED
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
      INTEGER               NEWSYM
      INTEGER               NEWVAL
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SYPUTD' )
      END IF
 
 
C
C     Check to see if the number of values is a valid quantity.
C
      IF ( N .LT. 1 ) THEN
         CALL SETMSG ( 'SYPUTD: The dimension of the values array is' //
     .                 'less than one.' )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)' )
         CALL CHKOUT ( 'SYPUTD' )
         RETURN
      END IF
 
 
C
C     How many symbols to start with?
C
      NSYM = CARDC ( TABSYM )
      NPTR = CARDI ( TABPTR )
      NVAL = CARDD ( TABVAL )
 
C
C     Where does this symbol belong? is it already in the table?
C
      LOCSYM = LSTLEC ( NAME, NSYM, TABSYM(1) )
      OLDSYM = ( LOCSYM .NE. 0  .AND.  TABSYM(LOCSYM) .EQ. NAME )
 
C
C     If the new symbol already exists, we need to know its dimension
C     to check for overflow.
C
      IF ( OLDSYM ) THEN
         LOCVAL = SUMAI ( TABPTR(1), LOCSYM-1 ) + 1
         DIMVAL = TABPTR(LOCSYM)
         NEWSYM = 0
      ELSE
         LOCVAL = SUMAI ( TABPTR(1), LOCSYM ) + 1
         DIMVAL = 0
         NEWSYM = 1
      END IF
 
      NEWVAL = N - DIMVAL
 
C
C     Can we do this without overflow?
C
 
      IF ( NSYM+NEWSYM .GT. SIZEC ( TABSYM ) ) THEN
 
         CALL SETMSG ( 'SYPUTD: Addition of the new symbol # causes ' //
     .                 'an overflow in the name table.' )
         CALL ERRCH  ( '#', NAME )
         CALL SIGERR ( 'SPICE(NAMETABLEFULL)' )
 
 
      ELSE IF ( NPTR+NEWSYM .GT. SIZEI ( TABPTR ) ) THEN
 
         CALL SETMSG ( 'SYPUTD: Addition of the new symbol # causes ' //
     .                 'an overflow in the pointer table.' )
         CALL ERRCH  ( '#', NAME )
         CALL SIGERR ( 'SPICE(POINTERTABLEFULL)' )
 
 
      ELSE IF ( NVAL+NEWVAL .GT. SIZED ( TABVAL ) ) THEN
 
         CALL SETMSG ( 'SYPUTD: Addition of the new symbol # causes ' //
     .                 'an overflow in the value table.' )
         CALL ERRCH  ( '#', NAME )
         CALL SIGERR ( 'SPICE(VALUETABLEFULL)' )
 
 
C
C     Looks like we can.
C
      ELSE
 
C
C        If the symbol exists, remove the current contents and
C        change the dimension. Otherwise add the new name and
C        dimension to the name and pointer tables.
C
         IF ( DIMVAL .GT. 0 ) THEN
            CALL REMLAD ( DIMVAL, LOCVAL, TABVAL(1), NVAL )
            CALL SCARDD ( NVAL, TABVAL )
 
            TABPTR(LOCSYM) = N
 
         ELSE
            CALL INSLAC ( NAME, 1, LOCSYM+1, TABSYM(1), NSYM )
            CALL SCARDC ( NSYM, TABSYM )
 
            CALL INSLAI ( N,    1, LOCSYM+1, TABPTR(1), NPTR )
            CALL SCARDI ( NPTR, TABPTR )
         END IF
 
C
C        In either case, insert the values from the input array into
C        the value table.
C
         CALL INSLAD ( VALUES, N, LOCVAL, TABVAL(1), NVAL )
         CALL SCARDD ( NVAL, TABVAL )
 
      END IF
 
 
      CALL CHKOUT ( 'SYPUTD' )
      RETURN
      END
