C$Procedure      SIGDGT ( Retain significant digits )
 
      SUBROUTINE SIGDGT ( IN, OUT )
 
C$ Abstract
C
C      Retain only the significant digits in a numeric string.
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
C      None.
C
C$ Keywords
C
C      CHARACTER,  PARSING
C
C$ Declarations
 
      CHARACTER*(*)    IN
      CHARACTER*(*)    OUT
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      IN         I   Input numeric string.
C      OUT        O   Numeric string, with insignificant digits removed.
C
C$ Detailed_Input
C
C      IN          is a numeric string.
C
C$ Detailed_Output
C
C      OUT         is the same numeric string with insignificant
C                  zeros and spaces removed. The special case '.000...'
C                  becomes just '0'. OUT may overwrite IN. If the
C                  output string is too long, it is truncated on the
C                  right.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      There are only two interesting cases:
C
C         1) There is a decimal point and an exponent immediately
C            preceded by zero ('...0E', '...0D', '...0e', '...0d')
C            or by a space ('... E', '... D', '... e', '... d').
C
C         2) There is a decimal point and no exponent, and the last non-
C            blank character is a zero ('...0').
C
C      In each of these cases, go to the zero in question, and step
C      backwards until you find something other than a blank or a zero.
C
C      Finally, remove all leading spaces, and all occurrences of more
C      than one consecutive space within the string.
C
C$ Examples
C
C      The following examples illustrate the use of SIGDGT.
C
C      '0.123456000000D-04'        becomes     '0.123456D-04'
C      '  -9.2100000000000'                    '-9.21'
C      '       13'                             '13'
C      '    00013'                             '00013'
C      ' .314 159 265 300 000 e1'              '.314 159 265 3e1'
C      '   123    45     6'                    '123 45 6'
C      '  .000000000'                          '0'
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C      Error free.
C
C      If IN is a non-numeric string, the contents of OUT are
C      unpredictable.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      N.J. Bachman    (JPL)
C      H.A. Neilan     (JPL)
C      W.L. Taber      (JPL)
C      I.M. Underwood  (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     retain significant digits
C
C-&
 
 
C$ Revisions
C
C-     Beta Version 1.3.0, 21-MAR-1989 (WLT)
C
C         Previous fix was unbelievably bad, very buggy.  This
C         has been fixed along with other bugs and non-standard
C         code has been removed.
C
C-     Beta Version 1.2.0, 28-FEB-1989 (WLT)
C
C         Reference to INSSUB replaced by SUFFIX
C
C-     Beta Version 1.1.1, 17-FEB-1989 (HAN)  (NJB)
C
C         Declaration of the unused function ISRCHC removed.
C
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER               FRSTNB
      INTEGER               LASTNB
      INTEGER               CPOS
 
C
C     Local variables
C
      INTEGER               BEGIN
      INTEGER               END
      CHARACTER*1           LCHAR
      INTEGER               ZERO
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               L
 
 
C
C     Find the first and last non-blank characters in the string.
C
      BEGIN  = MAX ( 1, FRSTNB (IN) )
      END    = MAX ( 1, LASTNB (IN) )
      LCHAR  = ' '
C
C     Trivial case.
C
      IF ( BEGIN .EQ. END ) THEN
 
         OUT(1:1) = IN(BEGIN:BEGIN)
 
         IF ( LEN(OUT) .GT. 1 ) OUT(2:) = ' '
 
C
C     If there is no decimal point, all zeros are significant.
C
      ELSE IF ( INDEX (IN, '.') .EQ. 0 ) THEN
 
         L      = 1
         K      = BEGIN
 
         DO WHILE (       ( L .LE. LEN(OUT) )
     .              .AND. ( K .LE. END      ) )
 
            OUT(L:L) = IN(K:K)
 
C
C           Don't increment L if the last item copied was a space
C           (we don't want to copy extra spaces).
C
            IF ( IN(K:K) .NE. ' ' .OR. LCHAR .NE. ' ' )
     .      L        = L+1
            LCHAR    = IN(K:K)
            K        = K+1
 
         END DO
 
         IF ( L .LE. LEN(OUT) ) OUT(L:) = ' '
 
      ELSE
 
C
C        Is there is a decimal point and an exponent immediately
C        preceded by zero ('...0E', '...0D', '...0e', '...0d') or
C        by a space ('... E', '... D', '... e', '... d')?
C
         ZERO = INDEX ( IN, '0E' )
 
         IF ( ZERO .EQ. 0 )    ZERO = INDEX ( IN, '0D' )
         IF ( ZERO .EQ. 0 )    ZERO = INDEX ( IN, '0e' )
         IF ( ZERO .EQ. 0 )    ZERO = INDEX ( IN, '0d' )
         IF ( ZERO .EQ. 0 )    ZERO = INDEX ( IN, ' E' )
         IF ( ZERO .EQ. 0 )    ZERO = INDEX ( IN, ' D' )
         IF ( ZERO .EQ. 0 )    ZERO = INDEX ( IN, ' e' )
         IF ( ZERO .EQ. 0 )    ZERO = INDEX ( IN, ' d' )
 
C
C        Begin there, and move toward the front of the string until
C        something other than a blank or a zero is encountered. Then
C        remove the superfluous characters.
C
         IF ( ZERO .GT. 0 ) THEN
 
            J = ZERO + 1
            I = ZERO
 
            DO WHILE ( IN(I:I) .EQ. '0'  .OR. IN(I:I) .EQ. ' ' )
               I = I - 1
            END DO
 
            L = 1
            K = BEGIN
 
            DO WHILE (       ( L .LE. LEN(OUT) )
     .                 .AND. ( K .LE. I        ) )
 
               OUT(L:L) = IN(K:K)
 
C
C              Don't increment L if the last item copied was a space.
C
               IF ( IN(K:K) .NE. ' ' .OR. LCHAR .NE. ' ' )
     .         L        = L+1
               LCHAR    = IN(K:K)
               K        = K+1
 
            END DO
 
            K = J
 
            DO WHILE (       ( L .LE. LEN(OUT) )
     .                 .AND. ( K .LE. END      ) )
 
               OUT(L:L) = IN(K:K)
 
C
C              Increment L only if we don't have two consecutive
C              spaces.
C
               IF ( IN(K:K) .NE. ' ' .OR. LCHAR .NE. ' ' )
     .         L        = L+1
               LCHAR    = IN(K:K)
               K        = K+1
 
            END DO
 
            IF ( L .LE. LEN(OUT) ) OUT(L:) = ' '
C
C
C        Is there is a decimal point and no exponent, and is the last
C        non-blank character a zero ('...0')? Then truncate the string
C        after the last character that is neither a blank nor a zero.
C
         ELSE IF (       IN(END:END)            .EQ. '0'
     .             .AND. CPOS ( IN, 'EeDd', 1 ) .eq.  0 ) THEN
 
            I = END
 
            DO WHILE ( IN(I:I) .EQ. '0' .OR. IN(I:I) .EQ. ' ' )
               I = I - 1
            END DO
 
            L = 1
            K = BEGIN
 
            DO WHILE (       ( L .LE. LEN(OUT) )
     .                 .AND. ( K .LE. I        ) )
 
               OUT(L:L) = IN(K:K)
 
C
C              Increment L only if we don't have two consecutive
C              spaces.
C
               IF ( IN(K:K) .NE. ' ' .OR. LCHAR .NE. ' ' )
     .         L        = L+1
               LCHAR    = IN(K:K)
               K        = K+1
 
            END DO
 
            IF ( L .LE. LEN(OUT) ) OUT(L:) = ' '
 
         ELSE
 
            L = 1
            K = BEGIN
 
            DO WHILE (       ( L .LE. LEN(OUT) )
     .                 .AND. ( K .LE. END      ) )
 
               OUT(L:L) = IN(K:K)
 
C
C              Increment L only if we don't have two consecutive spaces.
C
               IF ( IN(K:K) .NE. ' ' .OR. LCHAR .NE. ' ' )
     .         L        = L+1
               LCHAR    = IN(K:K)
               K        = K+1
 
            END DO
 
            IF ( L .LE. LEN(OUT) ) OUT(L:) = ' '
 
         END IF
 
      END IF
 
C
C     Special case. The string '.0000....' reduces to '.' after the
C     zeros are removed.
C
      IF ( OUT .EQ. '.' ) THEN
         OUT = '0'
      END IF
 
      RETURN
      END
