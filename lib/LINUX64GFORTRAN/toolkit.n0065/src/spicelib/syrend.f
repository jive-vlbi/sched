C$Procedure      SYREND ( Rename an existing symbol )
 
      SUBROUTINE SYREND ( OLD, NEW, TABSYM, TABPTR, TABVAL )
 
C$ Abstract
C
C     Rename an existing symbol in a double precision symbol table.
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
 
      CHARACTER*(*)         OLD
      CHARACTER*(*)         NEW
      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      DOUBLE PRECISION      TABVAL     ( LBCELL:* )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OLD        I   Name of the symbol to be renamed.
C     NEW        I   New name of the symbol.
C
C     TABSYM,
C     TABPTR,
C     TABVAL     I   Components of the symbol table.
C
C$ Detailed_Input
C
C     OLD        is the name of the symbol to be renamed. If OLD is
C                not in the symbol table, the tables are not modified.
C
C     NEW        is the new name of the symbol. If the symbol NEW
C                already exists in the symbol table, it is deleted.
C                OLD is then renamed to NEW.
C
C     TABSYM,
C     TABPTR,
C     TABVAL     are components of the double precision symbol table.
C
C$ Detailed_Output
C
C     TABSYM,
C     TABPTR,
C     TABVAL     are components of the double precision symbol table.
C                The values previously associated with OLD are now
C                associated with NEW. If OLD is not in the symbol
C                table, the symbol tables are not modified.
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
C     If the symbol OLD is not in the symbol table, the error
C     SPICE(NOSUCHSYMBOL) is signalled.
C
C$ Particulars
C
C     None.
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
C
C     The call,
C
C     CALL SYREND ( 'K', 'EB', TABSYM, TABPTR, TABVAL )
C
C     modifies the contents of the symbol table to be:
C
C        BODY4_POLE_RA -->    3.17681D2
C                             1.08D-1
C                             0.0D0
C        DELTA_T_A     -->    3.2184D1
C        EB            -->    1.657D-3
C        MEAN_ANOM     -->    6.239996D0
C                             1.99096871D-7
C        ORBIT_ECC     -->    1.671D-2
C                             1.08D-1
C                             0.0D0
C
C     The next call,
C
C     CALL SYREND ( 'EB', 'DELTA_T_A', TABSYM, TABPTR, TABVAL )
C
C     modifies the contents of the table to be:
C
C        BODY4_POLE_RA -->    3.17681D2
C                             1.08D-1
C                             0.0D0
C        DELTA_T_A     -->    1.657D-3
C        MEAN_ANOM     -->    6.239996D0
C                             1.99096871D-7
C        ORBIT_ECC     -->    1.671D-2
C                             1.08D-1
C                             0.0D0
C
C     Note that the symbol "DELTA_T_A" was deleted from the table,
C     and the symbol "EB" was then renamed to "DELTA_T_A". If the
C     new symbol exists, it is deleted from the table before its name
C     is given to another symbol.
C
C
C     The next call,
C
C     CALL SYREND ( 'K', 'EB', TABSYM, TABPTR, TABVAL )
C
C     does not modify the contents of the symbol table. It signals
C     the error SPICE(NOSUCHSYMBOL) because the symbol "K" does not
C     exist in the symbol table.
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
C     rename an existing symbol
C
C-&
 
 
C$ Revisions
C
C-     Beta Version 1.1.0, 17-FEB-1989 (NJB)
C
C         Declaration of the unused function SIZEC removed.
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER               BSRCHC
      INTEGER               CARDC
      INTEGER               LSTLEC
      LOGICAL               RETURN
      INTEGER               SUMAI
 
C
C     Local variables
C
      INTEGER               NSYM
 
      INTEGER               OLDLOC
      INTEGER               OLDVAL
      INTEGER               OLDDIM
      INTEGER               NEWLOC
      INTEGER               NEWVAL
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SYREND' )
      END IF
 
C
C     Where was the old symbol?
C
      NSYM   = CARDC  ( TABSYM )
      OLDLOC = BSRCHC ( OLD, NSYM, TABSYM(1) )
 
C
C     An overflow is simply not possible here. The only thing that can
C     go wrong is that the old symbol does not exist.
C
      IF ( OLDLOC .EQ. 0 ) THEN
 
 
         CALL SETMSG ( 'SYREND: The symbol # is not in the symbol ' //
     .                 'table.' )
         CALL ERRCH  ( '#', OLD )
         CALL SIGERR ( 'SPICE(NOSUCHSYMBOL)' )
 
C
C     Are these the same symbol?
C
      ELSE IF ( NEW .NE. OLD ) THEN
 
C
C        If the new symbol already exists, delete it.
C
         CALL SYDELD ( NEW, TABSYM, TABPTR, TABVAL )
 
         NSYM   = CARDC  (            TABSYM    )
         OLDLOC = BSRCHC ( OLD, NSYM, TABSYM(1) )
 
C
C        Swap N elements at the old location with zero elements
C        at the new location.
C
         NEWLOC = LSTLEC ( NEW, NSYM, TABSYM(1) ) + 1
 
         OLDVAL = SUMAI ( TABPTR(1), OLDLOC-1 ) + 1
         NEWVAL = SUMAI ( TABPTR(1), NEWLOC-1 ) + 1
 
         OLDDIM = TABPTR(OLDLOC)
 
         CALL SWAPAD ( OLDDIM, OLDVAL, 0, NEWVAL, TABVAL(1) )
 
C
C        Move the name and dimension the same way.
C
         CALL SWAPAC ( 1, OLDLOC, 0, NEWLOC, TABSYM(1) )
         CALL SWAPAI ( 1, OLDLOC, 0, NEWLOC, TABPTR(1) )
 
         IF ( OLDLOC .LT. NEWLOC ) THEN
            NEWLOC = NEWLOC - 1
         END IF
 
         TABSYM(NEWLOC) = NEW
 
      END IF
 
 
      CALL CHKOUT ( 'SYREND' )
      RETURN
      END
