C$Procedure      SYORDC ( Order the components of a single symbol )
 
      SUBROUTINE SYORDC ( NAME, TABSYM, TABPTR, TABVAL )
 
C$ Abstract
C
C     Order the components of a single symbol in a character symbol
C     table. The components are ordered according to the ASCII collating
C     sequence.
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
C     NAME       I   Name of the symbol whose components are to be
C                    ordered.
C     TABSYM,
C     TABPTR,
C     TABVAL    I/O  Components of the symbol table.
C
C$ Detailed_Input
C
C     NAME       is the name of the symbol whose components are to be
C                ordered. If NAME is not in the symbol table, the symbol
C                table is not modified.
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
C                The components of the symbol are sorted according to
C                ASCII collating sequence.
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
C     If the symbol NAME is not in the symbol table, the symbol table
C     is not modified.
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
C     CALL SYORDC ( 'EINSTEIN', TABSYM, TABPTR, TABVAL )
C
C     modifies the contents of the symbol table to be:
C
C        BOHR      -->   HYDROGEN ATOM
C        EINSTEIN  -->   BROWNIAN MOTION
C                        PHOTOELECTRIC EFFECT
C                        SPECIAL RELATIVITY
C        FERMI     -->   NUCLEAR FISSIONC
C
C
C     Note that the call,
C
C     CALL SYORDC ( 'MAXWELL', TABSYM, TABPTR, TABVAL )
C
C     will not modify the symbol table because the symbol "MAXWELL" is
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
C     order the components of a single symbol
C
C-&
 
 
 
 
 
C
C     SPICELIB functions
C
      INTEGER               BSRCHC
      INTEGER               CARDC
      LOGICAL               RETURN
      INTEGER               SUMAI
 
C
C     Local variables
C
      INTEGER               NSYM
 
      INTEGER               LOCSYM
      INTEGER               LOCVAL
      INTEGER               N
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SYORDC' )
      END IF
 
C
C     How many symbols?
C
      NSYM = CARDC ( TABSYM )
 
C
C     Is this symbol even in the table?
C
      LOCSYM = BSRCHC ( NAME, NSYM, TABSYM(1) )
 
C
C     If so, sort the components in place.
C
      IF ( LOCSYM .GT. 0 ) THEN
 
         LOCVAL = SUMAI ( TABPTR(1), LOCSYM-1 ) + 1
         N      = TABPTR(LOCSYM)
 
         CALL SHELLC ( TABPTR(LOCSYM), TABVAL(LOCVAL) )
 
      END IF
 
 
      CALL CHKOUT ( 'SYORDC' )
      RETURN
      END
