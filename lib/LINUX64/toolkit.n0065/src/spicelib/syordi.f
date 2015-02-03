C$Procedure      SYORDI ( Order the components of a single symbol )
 
      SUBROUTINE SYORDI ( NAME, TABSYM, TABPTR, TABVAL )
 
C$ Abstract
C
C     Order the components of a single symbol in an integer symbol
C     table. The components are sorted in increasing order.
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
      INTEGER               TABVAL     ( LBCELL:* )
 
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
C     TABVAL     are the components of an integer symbol table.
C
C$ Detailed_Output
C
C     TABSYM,
C     TABPTR,
C     TABVAL     are the components of an integer symbol table.
C                The components of the symbol are sorted in increasing
C                order.
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
C        books   -->   5
C        erasers -->   6
C        pencils -->  12
C                     24
C        pens    -->  10
C                     24
C                     12
C                     36
C                      4
C
C     The call,
C
C     CALL SYORDI ( 'pens', TABSYM, TABPTR, TABVAL )
C
C     modifies the contents of the symbol table to be:
C
C        books   -->   5
C        erasers -->   6
C        pencils -->  12
C                     24
C        pens    -->   4
C                     10
C                     12
C                     24
C                     36
C
C     Note that the call,
C
C     CALL SYORDI ( 'desks', TABSYM, TABPTR, TABVAL )
C
C     will not modify the symbol table because the symbol "desks" is
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
         CALL CHKIN ( 'SYORDI' )
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
 
         CALL SHELLI ( TABPTR(LOCSYM), TABVAL(LOCVAL) )
 
      END IF
 
 
      CALL CHKOUT ( 'SYORDI' )
      RETURN
      END
