C$Procedure            SYDIMD ( Return the dimension of a symbol )
 
      INTEGER FUNCTION SYDIMD ( NAME, TABSYM, TABPTR, TABVAL )
 
C$ Abstract
C
C     Return the dimension of a particular symbol in a double precision
C     symbol table. If the symbol is not found, the function returns the
C     value zero.
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
      DOUBLE PRECISION      TABVAL     ( LBCELL:* )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of the symbol whose dimension is desired.
C     TABSYM,
C     TABPTR,
C     TABVAL    I/O  Components of the symbol table.
C
C     The function returns the dimension of the symbol NAME. If NAME is
C     not in the symbol table, the function returns the value zero.
C
C$ Detailed_Input
C
C     NAME       is the name of the symbol whose dimension is to be
C                returned. If the symbol is not in the symbol table, the
C                function returns the value zero. This function is case
C                sensitive, NAME must match a symbol exactly.
C
C     TABSYM,
C     TABPTR,
C     TABVAL      are the components of a double precision symbol table.
C                 The table may or may not contain the symbol NAME.
C
C$ Detailed_Output
C
C     The function returns the dimension of the symbol NAME. The
C     dimension of a symbol is the number of values associated with
C     that symbol. If NAME is not in the symbol table, the function
C     returns the value zero.
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
C    None.
C
C$ Examples
C
C     The contents of the symbol table are:
C
C        DELTA_T_A -->   32.184
C        K         -->    1.657D-3
C        MEAN_ANOM -->    6.239996D0
C                         1.99096871D-7
C
C
C     Let NUMVAL be equal to the dimension of the symbols in the table.
C     The following code returns the values of NUMVAL indicated in the
C     table.
C
C     NUMVAL = SYDIMD ( 'MEAN_ANOM',   TABSYM, TABPTR, TABVAL )
C     NUMVAL = SYDIMD ( 'K',           TABSYM, TABPTR, TABVAL )
C     NUMVAL = SYDIMD ( 'DELTA_T_A',   TABSYM, TABPTR, TABVAL )
C     NUMVAL = SYDIMD ( 'BODY10_AXES', TABSYM, TABPTR, TABVAL )
C
C     ----SYMBOL----------NUMVAL------
C     | MEAN_ANOM    |       2       |
C     | K            |       1       |
C     | DELTA_T_A    |       1       |
C     | BODY10_AXES  |       0       |
C     --------------------------------
C
C     Note that the dimension of "BODY10_AXES" is zero. This is due to
C     the fact that "BODY10_AXES" is not in the symbol table.
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
C-     SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
C
C        If the value of the function RETURN is TRUE upon execution of
C        this module, this function is assigned a default value of
C        either 0, 0.0D0, .FALSE., or blank depending on the type of
C        the function.
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
C     fetch the dimension of a symbol
C
C-&
 
 
 
 
 
C
C     SPICELIB functions
C
      INTEGER               BSRCHC
      INTEGER               CARDC
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               NSYM
      INTEGER               LOCSYM
 
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         SYDIMD = 0
         RETURN
      ELSE
         CALL CHKIN ( 'SYDIMD' )
      END IF
 
C
C     How many symbols to start with?
C
      NSYM = CARDC ( TABSYM )
 
C
C     Is this symbol even in the table?
C
      LOCSYM = BSRCHC ( NAME, NSYM, TABSYM(1) )
 
C
C     If it's not in the table, return zero. Otherwise, look up
C     the dimension directly.
C
      IF ( LOCSYM .EQ. 0 ) THEN
         SYDIMD = 0
 
      ELSE
         SYDIMD = TABPTR(LOCSYM)
      END IF
 
 
      CALL CHKOUT ( 'SYDIMD' )
      RETURN
      END
