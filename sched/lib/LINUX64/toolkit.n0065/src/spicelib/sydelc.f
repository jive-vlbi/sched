C$Procedure      SYDELC ( Delete a symbol from the symbol table )
 
      SUBROUTINE SYDELC ( NAME, TABSYM, TABPTR, TABVAL )
 
C$ Abstract
C
C     Delete a symbol from a character symbol table. The symbol and its
C     associated values are deleted.
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
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of the symbol to be deleted.
C     TABSYM,
C     TABPTR,
C     TABVAL    I/O  Components of the symbol table.
C
C$ Detailed_Input
C
C     NAME       is the name of the symbol to be deleted from the symbol
C                table. If the symbol does not exist, the symbol table
C                remains unchanged. This subroutine is case sensitive.
C                NAME must the symbol exactly.
C
C     TABSYM,
C     TABPTR,
C     TABVAL      are the components of a character symbol table.
C                 On input, the table may or may not contain the
C                 symbol NAME.
C
C$ Detailed_Output
C
C     TABSYM,
C     TABPTR,
C     TABVAL      are the components of a character symbol table.
C                 On output, the symbol table no longer contains the
C                 symbol NAME or its associated values. If NAME is not
C                 a symbol, the components of the symbol table remain
C                 unchanged.
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
C     None.
C
C$ Examples
C
C     In the following example the subroutine SYDELC is used to delete
C     the symbol "BOHR" and its values from the symbol table.
C
C     The contents of the symbol table are:
C
C        BOHR      -->   HYDROGEN ATOM
C        EINSTEIN  -->   SPECIAL RELATIVITY
C                        PHOTOELECTRIC EFFECT
C                        BROWNIAN MOTION
C        FERMI     -->   NUCLEAR FISSION
C
C     The call
C
C     CALL SYDELC ( 'BOHR', TABSYM, TABPTR, TABVAL )
C
C     deletes the symbol "BOHR" from the symbol table. The components
C     of the symbol table on output are:
C
C        EINSTEIN  -->   SPECIAL RELATIVITY
C                        PHOTOELECTRIC EFFECT
C                        BROWNIAN MOTION
C        FERMI     -->   NUCLEAR FISSION
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
C     delete a symbol from a symbol table
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
      INTEGER               DIMVAL
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SYDELC' )
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
C     If it's not in the table, we're done. If it is, we can proceed
C     without fear of overflow.
C
      IF ( LOCSYM .GT. 0 ) THEN
 
         LOCVAL = SUMAI ( TABPTR(1), LOCSYM-1 ) + 1
         DIMVAL = TABPTR(LOCSYM)
 
         CALL REMLAC ( 1, LOCSYM, TABSYM(1), NSYM )
         CALL SCARDC ( NSYM, TABSYM )
 
         CALL REMLAI ( 1, LOCSYM, TABPTR(1), NPTR )
         CALL SCARDI ( NPTR, TABPTR )
 
         CALL REMLAC ( DIMVAL, LOCVAL, TABVAL(1), NVAL )
         CALL SCARDC ( NVAL, TABVAL )
 
      END IF
 
 
      CALL CHKOUT ( 'SYDELC' )
      RETURN
      END
