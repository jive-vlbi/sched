C$Procedure      SYTRNI (Transpose two values associated with a symbol)
 
      SUBROUTINE SYTRNI ( NAME, I, J, TABSYM, TABPTR, TABVAL )
 
C$ Abstract
C
C     Transpose two values associated with a particular symbol in an
C     integer symbol table.
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
      INTEGER               I
      INTEGER               J
      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      INTEGER               TABVAL     ( LBCELL:* )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of the symbol whose associated values are to
C                    be transposed.
C     I          I   Index of the first associated value to be
C                    transposed.
C     J          I   Index of the second associated value to be
C                    transposed.
C
C     TABSYM,
C     TABPTR,
C     TABVAL    I/O  Components of the symbol table.
C
C$ Detailed_Input
C
C     NAME       is the name of the symbol whose associated values are
C                to be transposed. If NAME is not in the symbol table,
C                the symbol tables are not modified.
C
C     I          is the index of the first associated value to be
C                transposed.
C
C     J          is the index of the second associated value to be
C                transposed.
C
C     TABSYM,
C     TABPTR,
C     TABVAL     are components of the integer symbol table.
C
C$ Detailed_Output
C
C     TABSYM,
C     TABPTR,
C     TABVAL     are components of the integer symbol table.
C                If the symbol NAME is not in the symbol table
C                the symbol tables are not modified. Otherwise,
C                the values that I and J refer to are transposed
C                in the value table.
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
C     1) If I < 1, J < 1, I > the dimension of NAME, or J > the
C        dimension of NAME, the error SPICE(INVALIDINDEX) is signaled.
C
C     2) If NAME is not in the symbol table, the symbol tables are not
C        modified.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     The contents of the symbol table are:
C
C        books   -->   5
C        erasers -->   6
C        pencils -->  12
C                     18
C                     24
C        pens    -->  10
C                     20
C                     30
C                     40
C
C     The call,
C
C     CALL SYTRNI ( 'pens', 2, 3, TABSYM, TABPTR, TABVAL )
C
C     modifies the contents of the symbol table to be:
C
C        books   -->   5
C        erasers -->   6
C        pencils -->  12
C                     18
C                     24
C        pens    -->  10
C                     30
C                     20
C                     40
C     The next call,
C
C     CALL SYTRNI ( 'pencils', 2, 4, TABSYM, TABPTR, TABVAL )
C
C     causes the error SPICE(INVALIDINDEX) to be signaled.
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
C-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
C
C        Updated so no "exchange" occurs if I equals J.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
C
C-&
 
C$ Index_Entries
C
C     transpose two values associated with a symbol
C
C-&
 
 
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
C
C        Updated so no "exchange" occurs if I equals J.
C
C-     Beta Version 2.0.0, 16-JAN-1989 (HAN)
C
C         If one of the indices of the values to be transposed is
C         invalid, an error is signaled and the symbol table is
C         not modified.
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
         CALL CHKIN ( 'SYTRNI' )
      END IF
 
 
C
C     How many symbols?
C
      NSYM = CARDC ( TABSYM )
 
C
C     Is this symbol even in the table?
C
      LOCSYM = BSRCHC ( NAME, NSYM, TABSYM(1) )
 
      IF ( LOCSYM .GT. 0 ) THEN
 
C
C        Are there enough values associated with the symbol?
C
         N = TABPTR(LOCSYM)
 
C
C        Are the indices valid?
C
         IF (        I .GE. 1
     .        .AND.  I .LE. N
     .        .AND.  J .GE. 1
     .        .AND.  J .LE. N ) THEN
 
C
C           Exchange the values in place.
C
            IF ( I .NE. J ) THEN

               LOCVAL = SUMAI ( TABPTR(1), LOCSYM-1 ) + 1
 
               CALL SWAPI ( TABVAL(LOCVAL+I-1), TABVAL(LOCVAL+J-1) )
 
            END IF

         ELSE
 
            CALL SETMSG ( 'The first index was *. ' //
     .                    'The second index was *.' )
            CALL ERRINT ( '*', I                    )
            CALL ERRINT ( '*', J                    )
            CALL SIGERR ( 'SPICE(INVALIDINDEX)'     )
 
         END IF
 
      END IF
 
 
      CALL CHKOUT ( 'SYTRNI' )
      RETURN
      END
