C$Procedure      SYDUPD ( Create a duplicate of a symbol )
 
      SUBROUTINE SYDUPD ( NAME, COPY, TABSYM, TABPTR, TABVAL )
 
C$ Abstract
C
C     Create a duplicate of a symbol within a double precision symbol
C     table. If a symbol with the new name already exists, its
C     components are replaced.
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
      CHARACTER*(*)         COPY
      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      DOUBLE PRECISION      TABVAL     ( LBCELL:* )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of the symbol to be duplicated.
C     COPY       I   Name of the new symbol.
C     TABSYM,
C     TABPTR,
C     TABVAL    I/O  Components of the symbol table.
C
C
C$ Detailed_Input
C
C     NAME       is the name of the symbol to be duplicated. The
C                components associated with NAME will be given to the
C                new symbol COPY. If NAME is not in the symbol table,
C                no duplicate symbol can be made.
C
C     COPY       is the name of the new symbol. If a symbol with the
C                name COPY already exists in the symbol table, its
C                components are replaced by the components of NAME.
C
C     TABSYM,
C     TABPTR,
C     TABVAL     are the components of a double precision symbol
C                table.
C
C$ Detailed_Output
C
C     TABSYM,
C     TABPTR,
C     TABVAL      are the components of a double precision symbol table.
C                 On output, the symbol table contains a new symbol COPY
C                 whose components are the same as the components of
C                 NAME.
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
C     1) If the symbol NAME is not in the symbol table, the error
C        SPICE(NOSUCHSYMBOL) is signalled.
C
C     2) If duplication of the symbol causes an overflow in the
C        name table, the error SPICE(NAMETABLEFULL) is signalled.
C
C     3) If duplication of the symbol causes an overflow in the
C        pointer table, the error SPICE(POINTERTABLEFULL) is signalled.
C
C     4) If duplication of the symbol causes an overflow in the
C        value table, the error SPICE(VALUETABLEFULL) is signalled.
C
C$ Particulars
C
C     If the symbol NAME is not in the symbol table, no duplicate symbol
C     can be made.
C     If the symbol COPY is already in the symbol table, its components
C     are replaced by the components of NAME.
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
C     The code,
C
C     CALL SYDUPD ( 'K', 'EB', TABSYM, TABPTR, TABVAL )
C
C     produces the symbol table:
C
C        DELTA_T_A -->   32.184
C        EB        -->    1.657D-3
C        K         -->    1.657D-3
C        MEAN_ANOM -->    6.239996D0
C                         1.99096871D-7
C        ORBIT_ECC -->    1.671D-2
C
C     The code,
C
C     CALL SYDUPD ( 'M0', 'M1', TABSYM, TABPTR, TABVAL )
C
C     produces the error SPICE(NOSUCHSYMBOL) because the symbol "M0" is
C     not in the symbol table.
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
C     create a duplicate of a symbol
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
 
      INTEGER               LOCSYM  ( 2 )
      INTEGER               LOCVAL  ( 2 )
      INTEGER               DIMVAL  ( 2 )
      LOGICAL               OLDSYM  ( 2 )
      INTEGER               NEWSYM
      INTEGER               NEWVAL
      INTEGER               I
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SYDUPD' )
      END IF
 
C
C     How many symbols to start with?
C
      NSYM = CARDC ( TABSYM )
      NPTR = CARDI ( TABPTR )
      NVAL = CARDD ( TABVAL )
 
C
C     Where do these symbols belong? Are they already in the table?
C
      LOCSYM(1) = LSTLEC ( NAME, NSYM, TABSYM(1) )
      LOCSYM(2) = LSTLEC ( COPY, NSYM, TABSYM(1) )
 
      OLDSYM(1) = (       LOCSYM(1)         .NE. 0
     .              .AND. TABSYM(LOCSYM(1)) .EQ. NAME )
 
      OLDSYM(2) = (       LOCSYM(2)         .NE. 0
     .              .AND. TABSYM(LOCSYM(2)) .EQ. COPY )
 
C
C     If the original symbol is not in the table, we can't make a copy.
C
      IF ( .NOT. OLDSYM(1) ) THEN
 
         CALL SETMSG ( 'SYDUPD: The symbol to be duplicated, #, is ' //
     .                 'not in the symbol table.' )
         CALL ERRCH  ( '#', NAME )
         CALL SIGERR ( 'SPICE(NOSUCHSYMBOL)' )
 
 
C
C     Otherwise, we need to know the dimension, to check for overflow.
C
      ELSE
 
         LOCVAL(1) = SUMAI  ( TABPTR(1), LOCSYM(1)-1 ) + 1
         DIMVAL(1) = TABPTR(LOCSYM(1))
 
C
C        If the new symbol already exists, we need to know its dimension
C        too, for the same reason.
C
         IF ( OLDSYM(2) ) THEN
            LOCVAL(2) = SUMAI ( TABPTR(1), LOCSYM(2)-1 ) + 1
            DIMVAL(2) = TABPTR(LOCSYM(2))
            NEWSYM    = 0
         ELSE
            LOCVAL(2) = SUMAI ( TABPTR(1), LOCSYM(2) ) + 1
            DIMVAL(2) = 0
            NEWSYM    = 1
         END IF
 
         NEWVAL = DIMVAL(1) - DIMVAL(2)
 
C
C        Can we make a copy without overflow?
C
         IF ( NSYM+NEWSYM .GT. SIZEC ( TABSYM ) ) THEN
 
            CALL SETMSG ( 'SYDUPD: Duplication of the symbol # '    //
     .                    'causes an overflow in the name table.' )
            CALL ERRCH  ( '#', NAME )
            CALL SIGERR ( 'SPICE(NAMETABLEFULL)' )
 
 
         ELSE IF ( NPTR+NEWSYM .GT. SIZEI ( TABPTR ) ) THEN
 
            CALL SETMSG ( 'SYDUPD: Duplication of the symbol # '    //
     .                    'causes an overflow in the pointer table.' )
            CALL ERRCH  ( '#', NAME )
            CALL SIGERR ( 'SPICE(POINTERTABLEFULL)' )
 
 
         ELSE IF ( NVAL+NEWVAL .GT. SIZED ( TABVAL ) ) THEN
 
            CALL SETMSG ( 'SYDUPD: Duplication of the symbol # '    //
     .                    'causes an overflow in the value table.' )
            CALL ERRCH  ( '#', NAME )
            CALL SIGERR ( 'SPICE(VALUETABLEFULL)' )
 
C
C        Looks like we can.
C
         ELSE
 
C
C           If the copy exists, remove the current contents and
C           change the dimension. Otherwise add the new name and
C           dimension to the name and pointer tables.
C
            IF ( DIMVAL(2) .GT. 0 ) THEN
              CALL REMLAD ( DIMVAL(2), LOCVAL(2), TABVAL(1), NVAL )
              CALL SCARDD ( NVAL, TABVAL )
 
              TABPTR(LOCSYM(2)) = DIMVAL(1)
 
              IF ( LOCVAL(1) .GT. LOCVAL(2) ) THEN
                 LOCVAL(1) = LOCVAL(1) - DIMVAL(2)
              END IF
 
            ELSE
              CALL INSLAC ( COPY, 1, LOCSYM(2)+1, TABSYM(1), NSYM )
              CALL SCARDC ( NSYM, TABSYM )
 
              CALL INSLAI ( DIMVAL(1), 1, LOCSYM(2)+1, TABPTR(1), NPTR )
              CALL SCARDI ( NPTR, TABPTR )
            END IF
 
C
C           In either case, allocate space for the new symbol values,
C           and copy them in one by one. (INSLAx won't work if the
C           copy is earlier in the table than the original.)
C
            DO I = NVAL, LOCVAL(2), -1
              TABVAL(I+DIMVAL(1)) = TABVAL(I)
            END DO
 
            IF ( LOCVAL(1) .GT. LOCVAL(2) ) THEN
               LOCVAL(1) = LOCVAL(1) + DIMVAL(1)
            END IF
 
            DO I = 0, DIMVAL(1) - 1
              TABVAL(LOCVAL(2)+I) = TABVAL(LOCVAL(1)+I)
            END DO
 
            CALL SCARDD ( NVAL + DIMVAL(1), TABVAL )
 
         END IF
 
      END IF
 
 
      CALL CHKOUT ( 'SYDUPD' )
      RETURN
      END
