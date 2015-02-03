C$Procedure       SYPTRI ( Symbol table, fetch pointers, generic )
 
      SUBROUTINE  SYPTRI ( NAME, SYMNAM, SYMPTR, SYMVAL, PTR, N, FOUND )
      IMPLICIT NONE
 
 
C$ Abstract
C
C     Return the address of the first value associated with a symbol
C     and the number of values associated with the symbol.
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
      CHARACTER*(*)         SYMNAM ( LBCELL : * )
      INTEGER               SYMPTR ( LBCELL : * )
      INTEGER               SYMVAL ( LBCELL : * )
 
      INTEGER               PTR
      INTEGER               N
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME       I   The name of a symbol.
C     SYMNAM     I   The name cell of symbol table.
C     SYMPTR     I   The pointer cell of a symbol table.
C     SYMVAL     I   The value cell of a symbol table.
C     PTR        O   The index of the first value associated with NAME.
C     N          O   The number of values associated with NAME.
C     FOUND      O   TRUE if NAME is in the symbol table, else FALSE
C
C$ Detailed_Input
C
C     NAME       is a string representing the name of some symbol that
C                might be in the symbol table SYMNAM, SYMPTR, ...
C
C     SYMNAM     is a symbol table.
C     SYMPTR
C     SYMVAL
C
C
C$ Detailed_Output
C
C     PTR        is the location in the values cell of the symbol table
C                where the values associated with NAME begin.
C
C     N          is the number of values in the symbol table
C                associated with NAME.
C
C     FOUND      is TRUE if NAME is the name of a symbol. Otherwise,
C                it is FALSE.
C
C
C$ Parameters
C
C
C     None.
C
C$ Exceptions
C
C     1) If NAME is not present in the symbol table, N and PTR will
C        both be returned with the value 0.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine returns the index of the first value associated with
C     a particular name in a symbol table.  It also returns the number
C     of values associated with the name.  In this way, routines that
C     "read" the values associated with a symbol table name, can read
C     them directly without having to declare local storage for these
C     values.
C
C$ Examples
C
C     Suppose that you need to count the number of values associated
C     with NAME that satisfy some property (computed by a logical
C     function PROP that you have written). The following block of code
C     would do the job.
C
C           COUNT = 0
C
C           CALL SYPTRI ( NAME, SYMNAM, SYMPTR, SYMVAL, PTR, N, FOUND )
C
C           DO I = PTR, PTR + N - 1
C
C              IF ( PROP(SYMVAL(I)) ) THEN
C                 COUNT = COUNT + 1
C              END IF
C
C           END DO
C
C
C$ Restrictions
C
C     User's should not attempt to access values beyond those in the
C     range returned returned by this routine. Also, any action that is
C     to be performed with the values associated with NAME should
C     be performed within a scope in which the symbol table cannot
C     be altered by other calls to symbol table routine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 4-MAY-1992 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Find pointers to values in a symbol table.
C
C-&
 
 
      INTEGER               BSRCHC
      INTEGER               SUMAI
      INTEGER               CARDC
      LOGICAL               RETURN
 
      INTEGER               TOUCHI
 
      INTEGER               LOC
      INTEGER               NUMBER
 
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SYPTRI' )
      END IF
C
C     We don't use the values of the symbol table in this routine
C     but it is passed for the sake of uniformity in the symbol
C     table routine calling sequences.  However, some compilers
C     generate warnings if a variable isn't used.  So we touch
C     the values cell to fake out the compiler.
C
      SYMVAL(LBCELL) = TOUCHI ( SYMVAL(LBCELL) )
 
C
C     Now for the real work of this routine.
C
      NUMBER = CARDC  (                SYMNAM    )
      LOC    = BSRCHC ( NAME,  NUMBER, SYMNAM(1) )
 
      IF ( LOC .EQ. 0 ) THEN
         FOUND = .FALSE.
         PTR   =  0
         N     =  0
         CALL CHKOUT ( 'SYPTRI' )
         RETURN
      END IF
 
      PTR   =  SUMAI  ( SYMPTR(1), LOC-1 ) + 1
      N     =  SYMPTR ( LOC )
      FOUND = .TRUE.
 
      CALL CHKOUT ( 'SYPTRI' )
 
      RETURN
      END
