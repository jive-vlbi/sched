C$Procedure      SYNTHD ( Return the Nth component of a symbol )
 
      SUBROUTINE SYNTHD ( NAME,
     .                    NTH,
     .                    TABSYM, TABPTR, TABVAL,
     .                    VALUE,
     .                    FOUND                               )
 
C$ Abstract
C
C     Return the Nth component of a particular symbol in a double
C     precision symbol table.
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
      INTEGER               NTH
      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      DOUBLE PRECISION      TABVAL     ( LBCELL:* )
      DOUBLE PRECISION      VALUE
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of the symbol whose Nth component is to be
C                    returned.
C     NTH        I   Index of the value to be returned.
C     TABSYM,
C     TABPTR,
C     TABVAL     I   Components of the symbol table.
C     VALUE      O   Nth value associated with the symbol.
C     FOUND      O   True if the Nth value of the symbol exists, false
C                    if it does not.
C
C$ Detailed_Input
C
C     NAME       is the name of the symbol whose Nth component is to be
C                returned. If NAME is not in the symbol table, FOUND is
C                false.
C
C     NTH        is the index of the component to be returned. If the
C                value of NTH is out of range ( NTH < 1 or NTH is
C                greater than the dimension of the symbol ) FOUND is
C                false.
C
C     TABSYM,
C     TABPTR,
C     TABVAL     are the components of a double precision symbol table.
C                The symbol table is not modified by this subroutine.
C
C$ Detailed_Output
C
C     VALUES     is the NTH component of the symbol NAME.
C
C     FOUND      is true if NAME is in the symbol table and the NTH
C                component of NAME exists.  Otherwise FOUND is false.
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
C     Two conditions will cause the value of FOUND to be false:
C
C       1) The symbol NAME is not in the symbol table.
C
C       2) NTH is out of range ( NTH < 1 or NTH is greater than the
C          dimension of the symbol ).
C
C$ Examples
C
C     The contents of the symbol table are:
C
C        BODY4_POLE_RA -->    3.17681D2
C                             1.08D-1
C                             0.0D0
C        DELTA_T_A     -->    3.2184D1
C        K             -->    1.657D-3
C        MEAN_ANOM     -->    6.239996D0
C                             1.99096871D-7
C        ORBIT_ECC     -->    1.671D-2
C
C     The calls,
C
C     CALL SYNTHD ( 'MEAN_ANOM',     2, TABSYM, TABPTR, TABVAL, VALUE,
C    .               FOUND                                             )
C
C     CALL SYNTHD ( 'BODY4_PRIME',   1, TABSYM, TABPTR, TABVAL, VALUE,
C    .               FOUND                                             )
C
C     CALL SYNTHD ( 'ORBIT_ECC',    -5, TABSYM, TABPTR, TABVAL, VALUE,
C    .               FOUND                                             )
C
C     return the values of VALUE and FOUND corresponding to NAME and
C     NTH:
C
C     NAME            NTH           VALUE            FOUND
C     ----------     -----     ----------------      -------
C     MEAN_ANOM        2        1.99096871D-7          TRUE
C     BODY4_PRIME      1                              FALSE
C     ORBIT_ECC       -5                              FALSE
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
C     fetch nth value associated with a symbol
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
         CALL CHKIN ( 'SYNTHD' )
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
C     If the value of NTH is out of range, that's a problem too.
C
      ELSE IF ( NTH .LT. 1  .OR.  NTH .GT. TABPTR(LOCSYM) ) THEN
 
         FOUND = .FALSE.
 
C
C     Otherwise, we can proceed without fear of error. Merely locate
C     and return the appropriate component from the values table.
C
      ELSE
 
         FOUND  = .TRUE.
 
         LOCVAL = SUMAI ( TABPTR(1), LOCSYM-1 ) + NTH
         VALUE  = TABVAL(LOCVAL)
 
      END IF
 
 
      CALL CHKOUT ( 'SYNTHD' )
      RETURN
      END
