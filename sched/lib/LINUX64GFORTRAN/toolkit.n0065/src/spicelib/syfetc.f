C$Procedure      SYFETC ( Fetch the Nth symbol in the table )
 
      SUBROUTINE SYFETC ( NTH, TABSYM, TABPTR, TABVAL, NAME, FOUND )
 
C$ Abstract
C
C     Fetch the Nth symbol in a character symbol table.
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
 
      INTEGER               NTH
      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      CHARACTER*(*)         TABVAL     ( LBCELL:* )
      CHARACTER*(*)         NAME
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NTH        I   Index of symbol to be fetched.
C     TABSYM,
C     TABPTR,
C     TABVAL     I   Components of the symbol table.
C     NAME       O   Name of the NTH symbol in the symbol table.
C     FOUND      O   True if the NTH symbol is in the symbol table,
C                    false if it is not.
C
C$ Detailed_Input
C
C     NTH        is the index of the symbol to be fetched. If the NTH
C                symbol does not exist, FOUND is FALSE.
C
C     TABSYM,
C     TABPTR,
C     TABVAL     are the components of a character symbol table.
C                The NTH symbol may or may not be in the symbol
C                table. The symbol table is not modified by this
C                subroutine.
C
C$ Detailed_Output
C
C     NAME       is the name of the NTH symbol in the symbol table.
C
C     FOUND      is true if the NTH symbol is in the symbol table.
C                If the NTH symbol is not in the table, FOUND is false.
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
C     The contents of the symbol table are:
C
C        BOHR      -->   HYDROGEN ATOM
C        EINSTEIN  -->   SPECIAL RELATIVITY
C                        PHOTOELECTRIC EFFECT
C                        BROWNIAN MOTION
C        FERMI     -->   NUCLEAR FISSION
C
C     The calls,
C
C     CALL SYFETC (  2,  TABSYM, TABPTR, TABVAL, NAME, FOUND )
C     CALL SYFETC (  3,  TABSYM, TABPTR, TABVAL, NAME, FOUND )
C     CALL SYFETC ( -1,  TABSYM, TABPTR, TABVAL, NAME, FOUND )
C     CALL SYFETC (  4,  TABSYM, TABPTR, TABVAL, NAME, FOUND )
C
C     result in the values for NAME and FOUND:
C
C     NAME       FOUND
C    ----------  -----
C     EINSTEIN    TRUE
C     FERMI       TRUE
C                FALSE
C                FALSE
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
C     fetch the nth symbol in the table
C
C-&
 
 
C$ Revisions
C
C-     Beta Version 1.1.0, 17-FEB-1989 (NJB)
C
C         Declaration of the unused variable SUMAI removed.
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               CARDC
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               NSYM
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SYFETC' )
      END IF
 
C
C     How many symbols to start with?
C
      NSYM = CARDC ( TABSYM )
 
C
C     If the value of NTH is out of range, that's a problem.
C
      IF ( NTH .LT. 1  .OR.  NTH .GT. NSYM ) THEN
 
         FOUND = .FALSE.
 
C
C     Otherwise, we can proceed without fear of error. Merely locate
C     and return the appropriate component from the values table.
C
      ELSE
 
         FOUND = .TRUE.
         NAME  = TABSYM(NTH)
 
      END IF
 
 
      CALL CHKOUT ( 'SYFETC' )
      RETURN
      END
