C$Procedure      WRDNLN (Write a definition line)
 
      SUBROUTINE WRDNLN (K, V, I, UNIT)
 
C$ Abstract
C
C     Write a line, keyword and value, to a NIOSPK command file.
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
 
      CHARACTER*(*)         K
      CHARACTER*(*)         V
      INTEGER               I
      INTEGER               UNIT
 
      INTEGER               LL
      PARAMETER           ( LL  = 79 )
 
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 350 )
 
      INTEGER               EQPOS
      PARAMETER           ( EQPOS = 22 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      K         I    Name of keyword.
C      V         I    Keyword's value.
C      I         I    Indentation of keyword.
C      UNIT      I    Unit of command file.
C
C      EQPOS     P    Default equal sign position.
C      LL        P    Preferred length of line.
C      LINLEN     P    Maximum length of line.
C
C$ Detailed_Input
C
C      K         is the name of the keyword in the assignment.
C
C      V         is the value of the assignment.
C
C      I         is the position the keyword should be indented to,
C                if possible.
C
C      UNIT      is the unit of the file to which the assignment will
C                be written.
C
C$ Detailed_Output
C
C      None.
C
C$ Parameters
C
C     EQPOS      is the default position of the equal sign.
C
C     LL         is the preferred length of the line. Lines will
C                be broken at this position, or before, if possible.
C
C     LINLEN      is the maximum length of the line. This value should
C                be set high enough so that the keyword and value (if
C                non-blank) can fit on one line.
C
C$ Exceptions
C
C     This routine does not signal any errors.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine will try to form the best looking assignment, taking
C     into account indentation, and equal sign position. The value may
C     be extended over multiple lines, if needed.
C
C     K and V are not changed.
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
C-    SPKMERGE Version 1.2.0, 30-APR-2014 (BVS)
C
C        Renamed LLMAX to LINLEN and increased it from 128 to 350
C        (consistent with other modules)
C
C-    SPKMERGE Version 1.1.0, 30-MAY-1996 (WLT)
C
C        Put the DO WHILE construct back in and replace WRITE
C        statement with a call to WRITLN.
C
C-    Beta Version 1.0.0, 10-AUG-1992 (MJS)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      INTEGER               RTRIM
      INTEGER               POSR
      INTEGER               POS
      INTEGER               LTRIM
 
C
C     Local variables
C
      CHARACTER*(LINLEN)     L
 
      INTEGER               KL
      INTEGER               VLMAX
      INTEGER               KP
      INTEGER               VP
      INTEGER               VP1
      INTEGER               VPMAX
      INTEGER               B
      INTEGER               E
      INTEGER               EP
 
      INTEGER               X
      INTEGER               C
 
C
C     Save all.
C
      SAVE

C
C     Statement Functions
C
C     C just returns the minimum of X and VLMAX.
C
      C(X) = MIN(X,VLMAX)
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
C
C     K   = keyword
C     V   = value
C     KP  = keyword position
C     VP  = value position
C     KL  = keyword length
C     VL  = value length
C     B:E = substring in value
C     L   = line to screen
C
C     If we assumed the keyword and value would fit on one line, then
C     this routine would be trivial. Unfortunately, values may
C     be long sometimes, longer than LL. In such cases,
C     we'll lose the indentation and any spaces between the
C     keyword and equal sign (all but one); if the value still
C     exceeds LL, we'll try breaking the value at word boundaries.
C     If the value is really ugly---long and no spaces---we'll just
C     have to let it extend past LL, but not past LINLEN.
C
      VLMAX = LEN(V)
      KL    = RTRIM(K)
      VPMAX = MAX(I+KL+1,EQPOS)+2
 
C
C     First find B and E, since this will determine where we place the
C     keyword and equal sign.
C
      E     = 0
      B     = E + LTRIM(V(C(E+1):))
      E     = C(B+LL-VPMAX+1)
 
      IF (E .NE. VLMAX) THEN
         E = POSR(V,' ',C(B+LL-VPMAX+1))-1
 
         IF (E .LT. B) THEN
            E = POS(V,' ',C(B+LL-VPMAX+1))-1
 
            IF (E .LT. B) THEN
               E = VLMAX
            END IF
         END IF
      END IF
 
C
C     Now we can figure out the keyword position (KP) and the value
C     position (VP). EP will always be two less than VP. We'll always
C     have a minimum, thus, of one space surrounding the equal sign.
C
      KP     = MAX(1,MIN(I,LL-E+B-KL-3))
      VP     = MIN(VPMAX,MAX(LL-E+B,KP+KL+3))
      VP1    = VP
      EP     = VP-2
 
      L      = ' '
      L(KP:) = K
      L(EP:) = '='
 
C
C     Same thing over again, except we have no keyword, or equal sign.
C
      DO WHILE (B .LE. VLMAX .AND. V(B:) .NE. ' ')
 
C
C        If E extends past VLMAX, we'd better do something about it.
C
         IF (VP+E-B .GT. VLMAX) THEN
            E = VLMAX-VP+B
         END IF
 
         L(VP:) = V(B:E)
 
         CALL WRITLN ( L, UNIT )
 
         B = E + LTRIM(V(C(E+1):))
         E = C(B+LL-VP1+1)
 
         IF (E .NE. VLMAX) THEN
            E = POSR(V,' ',C(B+LL-VP1+1))-1
 
            IF (E .LT. B) THEN
               E = POS(V,' ',C(B+LL-VP1+1))-1
 
               IF (E .LT. B) THEN
                  E = VLMAX
               END IF
            END IF
         END IF
 
C
C        Since we only have a value on this line, we can shift it to
C        the left if we have to to get it visible.
C
         VP = MIN(VP1,MAX(LL-E+B,1))
         L  = ' '
 
      END DO
 
C
C     That was fun.
C
      RETURN
      END
