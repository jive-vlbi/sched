C$Procedure      SYGETC ( Return all components for a symbol )
 
      SUBROUTINE SYGETC ( NAME,
     .                    TABSYM, TABPTR, TABVAL,
     .                    N,      VALUES,
     .                    FOUND                     )
 
C$ Abstract
C
C     Return the dimension and associated values for a particular
C     symbol.
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
      INTEGER               N
      CHARACTER*(*)         VALUES     (        * )
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of the symbol whose components are to be
C                    returned.
C     TABSYM,
C     TABPTR,
C     TABVAL     I   Components of the symbol table.
C
C     N          O   Dimension of the symbol.
C     VALUES     O   Values associated with the symbol.
C     FOUND      O   True if the symbol NAME is in the symbol table,
C                    false if it is not.
C
C$ Detailed_Input
C
C     NAME       is the name of the symbol whose components are to be
C                returned. If NAME is not in the symbol table, FOUND is
C                false.
C
C     TABSYM,
C     TABPTR,
C     TABVAL     are the components of a character symbol table.
C                The symbol NAME may or may not be in the symbol
C                table. The symbol table is not modified by this
C                subroutine.
C
C$ Detailed_Output
C
C     N          is the dimension of the symbol NAME. The dimension is
C                the number of values associated with the given symbol.
C                N is defined only if the output argument FOUND is
C                .TRUE.
C
C     VALUES     is an array containing the values associated with the
C                symbol. If the array is not large enough to hold all
C                of the values associated with NAME, as many as will
C                fit are returned.  VALUES is defined only if the
C                output argument FOUND is .TRUE.
C
C     FOUND      is true if NAME is in the symbol table. If NAME is not
C                in the table, FOUND is false.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  This subroutine does not check to see if the output array
C         VALUES is large enough to hold all of the values associated
C         with the symbol NAME.  The caller must provide the required
C         space.
C
C$ Files
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
C     Let the dimension of VALUES be 3.
C
C     The calls,
C
C     CALL SYGETC ( 'EINSTEIN', TABSYM, TABPTR, TABVAL,
C    .               N,         VALUES, FOUND           )
C
C     CALL SYGETC ( 'MILLIKAN', TABSYM, TABPTR, TABVAL,
C    .               N,         VALUES, FOUND           )
C                               
C     CALL SYGETC ( 'BORN',     TABSYM, TABPTR, TABVAL,
C    .               N,         VALUES, FOUND           )
C
C
C     return the values for N, VALUES, and FOUND associated with NAME:
C
C     NAME         N        VALUES                      FOUND
C     ----------  ---      -----------------------     -------
C     EINSTEIN     3        SPECIAL RELATIVITY           TRUE
C                           PHOTOELECTRIC EFFECT
C                           BROWNIAN MOTION
C     MILLIKAN                                          FALSE
C     BORN         1        HYDROGEN ATOM                TRUE
C
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
C     fetch all components for a symbol
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
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SYGETC' )
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
 
C
C     Otherwise, we can proceed without fear of error. Merely locate
C     and return the appropriate component from the values table.
C     We trust that the user has supplied enough room.
C
      ELSE
 
         FOUND = .TRUE.
 
         LOCVAL = SUMAI ( TABPTR(1), LOCSYM-1 ) + 1
         N      = TABPTR(LOCSYM)
 
         CALL MOVEC ( TABVAL(LOCVAL), N, VALUES )
 
      END IF
 
 
      CALL CHKOUT ( 'SYGETC' )
      RETURN
      END
