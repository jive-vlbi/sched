C$Procedure                  CRTPTR (Create pointer)
 
      CHARACTER*(*) FUNCTION CRTPTR (BASE, INDEX, PNTER)
 
C$ Abstract
C
C     Returns the symbol 'BASE~INDEX~PNTER'.
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
C     None.
C
C$ Keywords
C
C     None.
C
C$ Declarations
 
      CHARACTER*(*)         BASE
      INTEGER               INDEX
      CHARACTER*(*)         PNTER
 
      INTEGER               SYMLEN
      PARAMETER           ( SYMLEN = 1024 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      BASE,
C      INDEX,
C      PNTER     I    Components of the symbol 'BASE~INDEX~PNTER'.
C
C      SYMLEN    P    Maximum length of the symbol.
C
C$ Detailed_Input
C
C     BASE,
C     INDEX,
C     PNTER      are components of the symbol 'BASE~INDEX~PNTER'.
C
C$ Detailed_Output
C
C     CRTPTR     is the symbol 'BASE~INDEX~PNTER'.
C
C$ Parameters
C
C     SYMLEN     is the maximum length of the symbol 'BASE~INDEX~PNTER'.
C
C$ Exceptions
C
C     1) If the length of the symbol 'BASE~INDEX~PNTER' exceeds SYMLEN,
C        the error SPICE(BUFFERTOOSMALL) is signalled.
C
C     2) If the length of the symbol 'BASE~INDEX~PNTER' exceeds
C        LEN(CRTPTR), the error SPICE(DIMENSIONTOOSMALL) is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine creates a symbol that may be used to look up
C     nodes in the symbol table created by CPARSE.
C
C$ Examples
C
C     None.
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
C     M.J. Spencer   (JPL)
C
C$ Version
C
C-    Version 1.1.1, 13-JAN-2007, (EDW)
C
C        Corrected typo in the previous version string;
C        from:
C
C           09-DEC-203
C
C        to
C
C           09-DEC-2003
C
C-    Version 1.1.0, 09-DEC-2003, (EDW)
C
C       Set the SYMLEN value to 1024 to match the same
C       value in niospk.
C
C-    Beta Version 1.0.0, 11-AUG-1992 (MJS)
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               RTRIM
      LOGICAL               RETURN
 
C
C     Local variables
C
      CHARACTER*(10)        CNUM
      CHARACTER*(SYMLEN)    SYM
 
      INTEGER               BLEN
      INTEGER               CLEN
      INTEGER               PLEN
      INTEGER               TOTAL
 
C
C     This routine will use discovery check-in.
C
      IF (RETURN()) THEN
         RETURN
      END IF
 
C
C     Compute the lengths of the strings involved.
C
      CALL INTSTR (INDEX, CNUM)
 
      CLEN  = RTRIM (CNUM)
      BLEN  = RTRIM (BASE)
      PLEN  = RTRIM (PNTER)
      TOTAL = CLEN+BLEN+PLEN+2
 
C
C     TOTAL must be SYMLEN characters, or fewer.
C
      IF (TOTAL .GT. SYMLEN) THEN
         CALL CHKIN  ('CRTPTR'                                     )
         CALL SETMSG ('Symbol exceeds # characters. Increase the '  //
     .                'value of SYMLEN.'                           )
         CALL ERRINT ('#', SYMLEN                                  )
         CALL SIGERR ('SPICE(BUFFERTOOSMALL)'                      )
         CALL CHKOUT ('CRTPTR'                                     )
         RETURN
      END IF
 
C
C     And TOTAL must be LEN(CRTPTR) characters, or fewer.
C
      IF (TOTAL .GT. LEN(CRTPTR)) THEN
         CALL CHKIN  ('CRTPTR'                                 )
         CALL SETMSG ('Symbol exceeds the dimension of CRTPTR.')
         CALL SIGERR ('SPICE(DIMENSIONTOOSMALL)'               )
         CALL CHKOUT ('CRTPTR'                                 )
         RETURN
      END IF
 
C
C     Form the symbol 'BASE~INDEX~PNTER'.
C
      SYM = ' '
      SYM = BASE(1:BLEN) //'~'// CNUM(1:CLEN) //'~'// PNTER(1:PLEN)
 
      CRTPTR = SYM
 
      RETURN
      END
