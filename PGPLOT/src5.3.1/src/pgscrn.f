C*PGSCRN -- set color representation by name
C%void cpgscrn(int ci, const char *name, int *ier);
C+
      SUBROUTINE PGSCRN(CI, NAME, IER)
      INTEGER CI
      CHARACTER*(*) NAME
      INTEGER IER
C
C Set color representation: i.e., define the color to be
C associated with a color index.  Ignored for devices which do not
C support variable color or intensity.  This is an alternative to
C routine PGSCR. The color representation is defined by name or
C a hexadecimal character string instead of (R,G,B) components.
C
C The color may be specified either by a name, e.g., 'red', or by
C a hexadecimal code, e.g., '#ff0000'.
C
C The hexadecimal code consists of 7 characters; the first is '#',
C and the remaining 6 characters are pairs of hexadecimal digits
C (0-9, a-f, upper or lower case) giving R, G, B components for
C the color in the range 00 (black) to ff (maximum) (0-255 decimal).
C This convention is widely used to specify colors, e.g., in Web 
C browsers.
C
C Color names are defined in an external file which is read the first
C time that PGSCRN is called. The name of the external file is
C found as follows:
C 1. if environment variable (logical name) PGPLOT_RGB is defined,
C    its value is used as the file name;
C 2. otherwise, if environment variable PGPLOT_DIR is defined, a
C    file "rgb.txt" in the directory named by this environment
C    variable is used;
C 3. otherwise, file "rgb.txt" in the current directory is used.
C If all of these fail to find a file, an error is reported and
C the routine does nothing.
C
C Each line of the file defines one color, with four blank- or
C tab-separated fields per line. The first three fields are the
C R, G, B components, which are integers in the range 0 (zero
C intensity) to 255 (maximum intensity). The fourth field is the
C color name. The color name may include embedded blanks. Example:
C
C 255   0   0 red
C 255 105 180 hot pink
C 255 255 255 white
C   0   0   0 black
C
C Arguments:
C  CI     (input)  : the color index to be defined, in the range 0-max.
C                    If the color index greater than the device
C                    maximum is specified, the call is ignored. Color
C                    index 0 applies to the background color.
C  NAME   (input)  : the name or hexadecimal code of the color to be
C                    associated with this color index. If the first
C                    character of the string is '#', the string
C                    is interpreted as a hexadecimal code; otherwise
C                    the subroutine interprets it as a name and looks
C                    for it the external file. The names are not
C                    case-sensitive. If the hexadecimal code is illegal,
C                    or if the color is not listed in the file, the
C                    color representation is not changed.
C  IER    (output) : returns 0 if the routine was successful, 1
C                    if an error occurred (either the external file
C                    could not be read, or the requested color was
C                    not defined in the file, or an invalid hexadecimal
C                    code was supplied).
C--
C 12-Oct-1992 [TJP]
C 31-May-1993 [TJP] use GROPTX to open file.
C  7-Nov-1994 [TJP] better error messages.
C 10-Mar-1998 [TJP] add hexadecimal codes.
C-----------------------------------------------------------------------
      INTEGER MAXCOL
      PARAMETER (MAXCOL=1000)
      INTEGER I, IR, IG, IB, J, L, NCOL, UNIT, IOS
      INTEGER H(6)
      INTEGER GRCTOI, GROPTX, GRTRIM
      REAL RR(MAXCOL), RG(MAXCOL), RB(MAXCOL)
      CHARACTER*20 CREQ, CNAME(MAXCOL)
      CHARACTER*255 TEXT
      SAVE NCOL, CNAME, RR, RG, RB
      DATA NCOL/0/
C
C Check for hex-coded color.
C
      IF (NAME(1:1).EQ.'#') THEN
         CALL GRTOUP(CREQ, NAME)
         IF (CREQ(8:).NE.' ') GOTO 50
         DO 5 I=1,6
            H(I) = INDEX('0123456789ABCDEF', CREQ(I+1:I+1)) - 1
            IF (H(I).EQ.-1) GOTO 50
 5       CONTINUE
         IR = 16*H(1) + H(2)
         IG = 16*H(3) + H(4)
         IB = 16*H(5) + H(6)
C        WRITE (*,*) NAME, IR, IG, IB
         CALL PGSCR(CI, IR/255.0, IG/255.0, IB/255.0)
         IER = 0
         RETURN
      END IF
C
C On first call, read the database.
C
      IF (NCOL.EQ.0) THEN
         CALL GRGFIL('RGB', TEXT)
         L = GRTRIM(TEXT)
         IF (L.LT.1) L = 1
         CALL GRGLUN(UNIT)
         IOS = GROPTX(UNIT, TEXT(1:L), 'rgb.txt', 0)
         IF (IOS.NE.0) GOTO 40
         DO 10 I=1,MAXCOL
            READ (UNIT, '(A)', ERR=15, END=15) TEXT
            J = 1
            CALL GRSKPB(TEXT, J)
            IR = GRCTOI(TEXT, J)
            CALL GRSKPB(TEXT, J)
            IG = GRCTOI(TEXT, J)
            CALL GRSKPB(TEXT, J)
            IB = GRCTOI(TEXT, J)
            CALL GRSKPB(TEXT, J)
            NCOL = NCOL+1
            CALL GRTOUP(CNAME(NCOL), TEXT(J:))
            RR(NCOL) = IR/255.0
            RG(NCOL) = IG/255.0
            RB(NCOL) = IB/255.0
 10      CONTINUE
 15      CLOSE (UNIT)
         CALL GRFLUN(UNIT)
      END IF
C
C Look up requested color and set color representation if found.
C
      CALL GRTOUP(CREQ, NAME)
      DO 20 I=1,NCOL
         IF (CREQ.EQ.CNAME(I)) THEN
            CALL PGSCR(CI, RR(I), RG(I), RB(I))
            IER = 0
            RETURN
         END IF
 20   CONTINUE
C
C Color not found.
C
      IER = 1
      TEXT = 'Color not found: '//NAME
      CALL GRWARN(TEXT)
      RETURN
C
C Database not found.
C
 40   IER = 1
      NCOL = -1
      CALL GRFLUN(UNIT)
      CALL GRWARN('Unable to read color file: '//TEXT(1:L))
      CALL GRWARN('Use environment variable PGPLOT_RGB to specify '//
     :     'the location of the PGPLOT rgb.txt file.')
      RETURN
C
C Invalid numeric color.
C
 50   IER = 1
      TEXT = 'Invalid numeric color: '//NAME
      CALL GRWARN(TEXT)
      RETURN
C
      END
