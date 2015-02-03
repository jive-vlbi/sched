C$Procedure      PRTENC ( Encode a character string, portably )
 
      SUBROUTINE PRTENC ( NUMBER, STRING )
 
C$ Abstract
C
C     Encode a nonnegative integer number into a character string,
C     portably.  This routine uses 128 as the base for encoding.
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
C     CELLS, CHARACTER
C
C$ Declarations
 
      INTEGER            NUMBER
      CHARACTER*(*)      STRING
 
      INTEGER            MINLEN
      PARAMETER        ( MINLEN = 5 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NUMBER     I   Number to be encoded.
C     STRING     O   Encoded string.
C     MINLEN     P   Minimum length of string.
C
C$ Detailed_Input
C
C     NUMBER      is an arbitrary nonnegative integer.
C
C$ Detailed_Output
C
C     STRING      is the character string implied by the ASCII
C                 interpretation of NUMBER when converted to its
C                 base 128 representation.
C
C                 Let L be the declared length of STRING, and let
C                 NUMBER be given by
C
C                                     0           1                 L-1
C                    NUMBER = a    128  + a    128  + ... + a    128
C                              1           2                 L
C
C                 Then
C
C                    STRING(i:i) = CHAR(a )   for i = 1, L
C                                        i
C
C                 Note that, just as for any other "numbers",
C                 the "digits" in STRING are arranged from right
C                 to left in order of increasing significance.
C                 The string is, in effect, "padded with nulls"
C                 on the left.
C
C$ Parameters
C
C     MINLEN      is the minimum length of a string into which a
C                 number may be encoded. In order to avoid padding
C                 long strings with hundreds, possibly thousands
C                 of null characters, only the first MINLEN characters
C                 of the string are actually used. Note that this
C                 also allows the encoded number to be preserved
C                 during assignments,
C
C                    STR1 = STR2
C
C                 so long as both strings are of length MINLEN or
C                 greater.
C
C$ Particulars
C
C     This routine is identical to ENCHAR, except that this routine
C     does not use the machine-dependent encoding base returned by
C     the SPICELIB routine CHBASE.  Instead, the base 128 is used.
C     This base is expected to work on all systems supporting ASCII
C     encoding of characters.
C
C$ Examples
C
C     See: SCARDC, SSIZEC.
C
C$ Restrictions
C
C     None.
C
C$ Exceptions
C
C     1) If the length of the output string is less than MINLEN,
C        the error 'SPICE(INSUFFLEN)' is signalled.
C
C     2) If the number to be encoded is negative, the error
C        'SPICE(OUTOFRANGE)' is signalled.
C
C                                                      MINLEN
C     3) If the number to be encoded is larger than 128       - 1,
C        the error 'SPICE(OUTOFRANGE)' is signalled.
C
C$ Files
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 19-DEC-1995 (NJB)(WLT)
C
C
C-&
 
C$ Index_Entries
C
C     encode a character string, portably
C
C-&
 
C
C     Local parameters
C
      INTEGER               CHBASE
      PARAMETER           ( CHBASE = 128 )
 
C
C     Local variables
C
      INTEGER               BASE
      INTEGER               NUM
      INTEGER               REMAIN
      INTEGER               I
 
C
C     Standard SPICE error handling.
C
      IF ( LEN ( STRING ) .LT. MINLEN ) THEN
         CALL CHKIN  ( 'PRTENC' )
         CALL SIGERR ( 'SPICE(INSUFFLEN)' )
         CALL CHKOUT ( 'PRTENC' )
         RETURN
 
      ELSE IF ( NUMBER .LT. 0 ) THEN
         CALL CHKIN  ( 'PRTENC' )
         CALL SIGERR ( 'SPICE(OUTOFRANGE)' )
         CALL CHKOUT ( 'PRTENC' )
         RETURN
      END IF
 
C
C     Generate the digits from right to left.
C
      BASE = CHBASE
      NUM  = NUMBER
 
      DO I = MINLEN, 1, -1
 
         REMAIN      = MOD ( NUM, BASE )
         STRING(I:I) = CHAR ( REMAIN )
         NUM         = NUM / BASE
 
      END DO
 
C
C     More error handling.
C
      IF ( NUM .GT. 0 ) THEN
         CALL CHKIN  ( 'PRTENC' )
         CALL SIGERR ( 'SPICE(OUTOFRANGE)' )
         CALL CHKOUT ( 'PRTENC' )
      END IF
 
      RETURN
 
 
 
 
C$Procedure      PRTDEC ( Decode a character string )
 
      ENTRY PRTDEC ( STRING, NUMBER )
 
C$ Abstract
C
C     Decode a character string encoded by PRTENC.
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
C     CHARACTER
C
C$ Declarations
C
C     CHARACTER*(*)      STRING
C     INTEGER            NUMBER
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   Encoded character string.
C     NUMBER     O   Decoded number.
C
C$ Detailed_Input
C
C     STRING      is a character string previously encoded by PRTENC.
C                 This contains an integer in base 128 notation,
C                 where 128 is a function of the size of the
C                 available character set. See PRTENC for details
C                 about the format of STRING.
C
C$ Detailed_Output
C
C     NUMBER      is the integer encoded in the input string.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C     PRTDEC is the inverse of PRTENC. In the example below,
C
C           CALL PRTENC (      I, STRING )
C           CALL PRTDEC ( STRING,      J )
C
C           IF ( I .EQ. J ) THEN
C            .
C            .
C           END IF
C
C     the logical test (I .EQ. J) is always true.
C
C     This routine is identical to DECHAR, except that this routine
C     does not use the machine-dependent encoding base returned by
C     the SPICELIB routine CHBASE.  Instead, the base 128 is used.
C     This base is expected to work on all systems supporting ASCII
C     encoding of characters.
C
C$ Examples
C
C     See: CARDC, SIZEC.
C
C$ Restrictions
C
C     None.
C
C$ Exceptions
C
C     1) If the length of the input string is less than MINLEN,
C        the error 'SPICE(INSUFFLEN)' is signalled.
C
C$ Files
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     decode a portably encoded character string
C
C-&
 
      IF ( LEN ( STRING ) .LT. MINLEN ) THEN
         CALL CHKIN  ( 'PRTDEC' )
         CALL SIGERR ( 'SPICE(INSUFFLEN)' )
         CALL CHKOUT ( 'PRTDEC' )
         RETURN
      END IF
 
C
C     Sum the products of the 'digits' and the corresponding powers
C     of NDCHAR, just like any other base conversion.
C
      BASE   = CHBASE
      NUMBER = 0
 
      DO I = 1, MINLEN
         NUMBER = BASE*NUMBER + ICHAR ( STRING(I:I) )
      END DO
 
      RETURN
      END
