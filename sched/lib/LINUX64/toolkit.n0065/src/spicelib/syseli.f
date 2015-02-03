C$Procedure      SYSELI ( Select a subset of the values of a symbol )
 
      SUBROUTINE SYSELI ( NAME,
     .                    BEGIN,
     .                    END,
     .                    TABSYM, TABPTR, TABVAL,
     .                    VALUES,
     .                    FOUND                     )
 
C$ Abstract
C
C     Select a subset of the values associated with a particular
C     symbol in an integer symbol table.
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
      INTEGER               BEGIN
      INTEGER               END
      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      INTEGER               TABVAL     ( LBCELL:* )
      INTEGER               VALUES     (        * )
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of the symbol whose associated values are to
C                    be returned.
C     BEGIN      I   Index of the first associated value to be returned.
C     END        I   Index of the last associated value to be returned.
C
C     TABSYM,
C     TABPTR,
C     TABVAL     I   Components of the symbol table.
C
C     VALUES     O   Subset of the values associated with the symbol
C                    NAME.
C     FOUND      O   True if the subset of values exists.
C
C$ Detailed_Input
C
C     NAME       is the name of the symbol whose subset of associated
C                values to be returned. If NAME is not in the symbol
C                table, FOUND is false.
C
C     BEGIN      is the index of the first associated value to be
C                returned. If BEGIN is out of range (BEGIN < 1 or
C                BEGIN > END) FOUND is false.
C
C     END        is the index of the last associated value to be
C                returned. If END is out of range (END < 1 or
C                END > is greater than the dimension of NAME)
C                FOUND is false.
C
C     TABSYM,
C     TABPTR,
C     TABVAL     are components of the integer symbol table.
C
C$ Detailed_Output
C
C     VALUES     is a subset of the values associated with the
C                symbol NAME. If the subset specified by BEGIN and
C                END exists, as many values as will fit in VALUES
C                are returned. If the subset does not exist, no
C                values are returned and FOUND is false.
C
C     FOUND      is true if the subset of values is exists.
C                FOUND is false if BEGIN < 1, BEGIN > END, END < 1,
C                END > the dimension of NAME, or NAME is not
C                in the symbol table.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  This subroutine does not check to see if the output array
C         VALUES is large enough to hold the selected set of values.
C         The caller must provide the required space.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     FOUND will be false if the bounds of the subset specified by
C     BEGIN and END are out of range. Values of BEGIN and END which
C     specify bounds out of range are BEGIN < 1, BEGIN > END,
C     END < 1, or END > the dimension of NAME. FOUND is also false
C     if the symbol NAME is not in the symbol table.
C
C$ Examples
C
C     The contents of the symbol table are:
C
C        books   -->   5
C                     10
C        erasers -->   3
C        pencils -->  12
C                     18
C                     24
C                     30
C        pens    -->  10
C                     20
C                     30
C
C     Let the dimension of the array VALUES be 4.
C
C     The ouput values of VALUES and FOUND for the input values of
C     NAME, BEGIN, and END are contained in this table:
C
C     NAME         BEGIN    END     VALUES    FOUND
C     ----------   -----    ---    --------  -------
C     pencils        2       3        18       TRUE
C                                     24
C
C     pencils        1       4        12       TRUE
C                                     18
C                                     24
C                                     30
C
C     desks          1       5                FALSE
C
C     books          2       1                FALSE
C
C     erasers        1      -2                FALSE
C
C     pens           1       5                FALSE
C     ----------------------------------------------------------------
C
C     Note that FOUND is FALSE for examples 3 through 6 because:
C
C        - In the 3rd example, the symbol 'desks' is not in the
C          symbol table.
C
C        - In the 4th example, BEGIN > END.
C
C        - In the 5th example, END < 0.
C
C        - In the 6th example, END is greater than the dimension of the
C          symbol 'pens'.
C
C$ Restrictions
C
C     1) See Exceptions section.
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
C-     SPICELIB Version 1.0.2, 03-NOV-2005 (NJB)
C
C         Various header corrections were made.  In particular, 
C         the header no longer asserts that this routine will
C         "return as many values as will fit" in the output array
C         VALUES.
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
C     select a subset of the values of a symbol
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
         CALL CHKIN ( 'SYSELI' )
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
C     If it's not in the table, it's definitely a problem.
C
      IF ( LOCSYM .EQ. 0 ) THEN
 
         FOUND = .FALSE.
 
      ELSE
 
C
C        We could still have a problem: do these components exist?
C        Does this request even make sense?
C
         N = TABPTR(LOCSYM)
 
         IF (       BEGIN .GE. 1
     .        .AND. BEGIN .LE. N
     .        .AND. END   .GE. 1
     .        .AND. END   .LE. N
     .        .AND. BEGIN .LE. END ) THEN
 
            FOUND  = .TRUE.
            LOCVAL = SUMAI ( TABPTR(1), LOCSYM-1 ) + 1
 
            CALL MOVEI ( TABVAL(LOCVAL+BEGIN-1), END-BEGIN+1, VALUES )
 
         ELSE
            FOUND = .FALSE.
         END IF
 
      END IF
 
 
      CALL CHKOUT ( 'SYSELI' )
      RETURN
      END
