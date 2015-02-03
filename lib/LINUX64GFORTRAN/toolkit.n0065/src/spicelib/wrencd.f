 
C$Procedure  WRENCD  ( Write encoded d.p. numbers to text file )
 
      SUBROUTINE WRENCD ( UNIT, N, DATA )
 
C$ Abstract
C
C     Encode and write d.p. numbers to a text file.
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
C     CONVERSION
C     NUMBERS
C     UTILITY
C
C$ Declarations
 
      INTEGER               UNIT
      INTEGER               N
      DOUBLE PRECISION      DATA(*)
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      UNIT      I    Fortran unit number of output text file.
C      N         I    Number of d.p. numbers to encode and write.
C      DATA      I    List of d.p. numbers to encode and write.
C
C$ Detailed_Input
C
C     UNIT     The Fortran unit number for a previously opened text
C              file. All writing will begin at the CURRENT POSITION
C              in the text file.
C
C     N        The number of double precision numbers to be encoded
C              and written to the text file attached to UNIT.
C
C     DATA     List of double precision numbers to be encoded and
C              written to the text file attached to UNIT.
C
C$ Detailed_Output
C
C     See the Particulars section for a description of the effect of
C     this routine.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1)   If N, the number of data items, is not positive, the error
C          SPICE(INVALIDARGUMENT) will be signalled.
C
C     2)   If an error occurs while writing to the text file attached
C          to UNIT, the error SPICE(FILEWRITEFAILED) will be signalled.
C
C     3)   If the Fortran logical unit UNIT is not defined, the results
C          of this routine are unpredictable.
C
C$ Files
C
C     See the description of UNIT in the Detailed_Input section.
C
C$ Particulars
C
C     This routine will accept a list of one or more double precision
C     numbers which it will encode into equivalent text strings and
C     write to the current position in a text file. The current
C     position in a file is defined to be the text line immediately
C     following the last text line that was written or read. The
C     encoded d.p. numbers are written to the output text file as
C     quoted character strings so that a Fortran list directed read may
C     be used to read the encoded values, rather than a formatted read
C     with the format specifier FMT = '(A)'.
C
C     This routine is one of a pair of routines which are used to
C     encode and decode d.p. numbers:
C
C           WRENCD -- Encode and write d.p. numbers to a file.
C           RDENCD -- Read and decode d.p. numbers from a file.
C
C     The encoding/decoding of d.p.numbers is performed to provide a
C     portable means for transferring data values.
C
C     Currently the text string produced will be in a base 16
C     ``scientific notation.'' This format retains the full precision
C     available for d.p. numbers on any given computer architecture.
C     See DP2HX.FOR and HX2DP.FOR for details.
C
C$ Examples
C
C     Please note that the output format in the examples is not
C     intended to be exactly identical with the output format of this
C     routine in actual use. The output format used in the examples is
C     intended to aid in the understanding of how this routine works.
C     It is NOT intended to be a specification of the output format for
C     this routine.
C
C     Let
C
C        UNIT     be the Fortran logical unit of a previously opened
C                 text file.
C
C        N        = 100
C
C        DATA(I)  = DBLE(I), I = 1,N
C
C     Then, the subroutine call
C
C           CALL WRENCD( UNIT, N, DATA )
C
C     will write the first 100 integers as encoded d.p. numbers to the
C     output text file attached to UNIT, beginning at the current
C     position in the output file, which is marked by an arrow, '-->'.
C     The resulting output will look something like the following:
C
C        -->'1^1' '2^1' '3^1' '4^1' '5^1' '6^1' '7^1' '8^1' '9^1'
C           'A^1' 'B^1' 'C^1' 'D^1' 'E^1' 'F^1' '1^2' '11^2' '12^2'
C           '13^2' '14^2' '15^2' '16^2' '17^2' '18^2' '19^2' '1A^2'
C           '1B^2' '1C^2' '1D^2' '1E^2' '1F^2' '2^2' '21^2' '22^2'
C           '23^2' '24^2' '25^2' '26^2' '27^2' '28^2' '29^2' '2A^2'
C           '2B^2' '2C^2' '2D^2' '2E^2' '2F^2' '3^2' '31^2' '32^2'
C           '33^2' '34^2' '35^2' '36^2' '37^2' '38^2' '39^2' '3A^2'
C           '3B^2' '3C^2' '3D^2' '3E^2' '3F^2' '4^2'
C           '41^2' '42^2' '43^2' '44^2' '45^2' '46^2' '47^2' '48^2'
C           '49^2' '4A^2' '4B^2' '4C^2' '4D^2' '4E^2' '4F^2' '5^2'
C           '51^2' '52^2' '53^2' '54^2' '55^2' '56^2' '57^2' '58^2'
C           '59^2' '5A^2' '5B^2' '5C^2' '5D^2' '5E^2' '5F^2' '6^2'
C           '61^2' '62^2' '63^2' '64^2'
C        -->
C
C     At this point, the arrow marks the position of the file pointer
C     immediately after the call to WRENCD.
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
C     K.R. Gehringer (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 09-SEP-1993 (KRG)
C
C        The list directed write was changed to a formatted write using
C        the specifier FMT='(A)'. This was done in order to prevent a
C        space from appearing as the first character on each line of the
C        file for certian computer platforms.
C
C-    SPICELIB Version 1.1.0, 21-JUN-1993 (KRG)
C
C        This routine was modified to avoid the creation of long output
C        lines on some of the supported systems, such as the NeXT with
C        Absoft Fortran 3.2.
C
C        A disclaimer was added to the $ Examples section concerning
C        the output format used. The disclaimer simply states that the
C        output format used in the example is not necessarily the
C        output format actually used by the routine.
C
C-    SPICELIB Version 1.0.0, 20-OCT-1992 (KRG)
C
C-&
 
C$ Index_Entries
C
C      encode and write d.p. numbers to a text file
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.2.0, 09-SEP-1993 (KRG)
C
C        The list directed write was changed to a formatted write using
C        the  specifier FMT='(A)'. This was done in order to prevent a
C        space from appearing as the first character on each line of the
C        file for certian computer platforms.
C
C-    SPICELIB Version 1.1.0, 21-JUN-1993 (KRG)
C
C        This routine was modified to avoid the creation of long output
C        lines on some of the supported systems, such as the NeXT with
C        Absoft Fortran 3.2.
C
C        On some of the supported computers this routine would produce
C        very long (greater than 1000 characters) output lines due to
C        the implicit DO loop used in the WRITE statment:
C
C            WRITE (UNIT,IOSTAT=IOSTAT,FMT=*)
C           .   ( QUOTE//WORK(I)(1:LENGTH(I))//QUOTE//' ', I=1,NITMS )
C
C        This problem was fixed by removing the implicit DO loop from
C        the WRITE statement and placing an equivalent DO loop around
C        the WRITE statemtent:
C
C            DO I = 1, NITMS
C               WRITE (UNIT,IOSTAT=IOSTAT,FMT=*)
C           .       QUOTE//WORK(I)(1:LENGTH(I))//QUOTE
C            END DO
C
C        The net effect of this will be that only a single datum will
C        be written on each line of output.
C
C        A disclaimer was added to the $ Examples section concerning
C        the output format used. The disclaimer simply states that the
C        output format used in the example is not necessarily the
C        output format actually used by the routine.
C
C-    SPICELIB Version 1.0.0, 20-OCT-1992 (KRG)
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
C
C     Local parameters
C
      CHARACTER*(1)         QUOTE
      PARAMETER           ( QUOTE = '''' )
 
      INTEGER               MAXENC
      PARAMETER           ( MAXENC = 64 )
 
      INTEGER               WRKSIZ
      PARAMETER           ( WRKSIZ = 64 )
C
C     Local variables
C
      CHARACTER*(MAXENC)    WORK(WRKSIZ)
 
      INTEGER               I
      INTEGER               IOSTAT
      INTEGER               ITMBEG
      INTEGER               NITMS
      INTEGER               LENGTH(WRKSIZ)
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'WRENCD' )
      END IF
C
C     Check to see if the number of data items is less than or equal
C     to zero. If it is, signal an error.
C
      IF ( N .LT. 1 ) THEN
 
         CALL SETMSG ( 'The number of data items to be written was' //
     .                 ' not positive: #.'                           )
         CALL ERRINT ( '#', N                                        )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                      )
         CALL CHKOUT ( 'WRENCD'                                      )
         RETURN
 
      END IF
C
C     Initialize the beginning location for the data items to be
C     encoded.
C
      ITMBEG = 1
C
C     Begin encoding the input data items in blocks of size NITMS.
C     Each time the number of data items NITMS is reached, write
C     out the encoded items in the workspace.
C
      DO WHILE ( ITMBEG .LE. N )
C
C        The number of items is either the size of the workspace, or
C        the number of data items which remain to be processed, which
C        should always be less than or equal to the size of the
C        workspace.
C
         NITMS = MIN ( WRKSIZ, N - ITMBEG + 1 )
C
C        Encode each of the numbers into an equivalent character string.
C
         DO I = 1, NITMS
 
            CALL DP2HX ( DATA(ITMBEG + I - 1), WORK(I), LENGTH(I) )
 
         END DO
C
C        Write out the current workspace, placing single quotes around
C        each of the character strings so that they may be read using
C        Fortran list directed read statements rather than the format
C        specifier FMT = '(A)'.
C
         DO I = 1, NITMS
 
            WRITE ( UNIT,FMT='(A)',IOSTAT=IOSTAT )
     .         QUOTE//WORK(I)(1:LENGTH(I))//QUOTE
C
C           Check to see if we got a write error, IOSTAT .NE. 0.
C
            IF ( IOSTAT .NE. 0 ) THEN
 
               CALL SETMSG ( 'Error writing to logical unit #,'  //
     .                       ' IOSTAT = #.'                       )
               CALL ERRINT ( '#', UNIT                            )
               CALL ERRINT ( '#', IOSTAT                          )
               CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'             )
               CALL CHKOUT ( 'WRENCD'                             )
               RETURN
 
            END IF
 
         END DO
C
C        Position the data item pointer at the next location to begin
C        encoding the items in the array DATA, and continue processing
C        the data items until done.
C
         ITMBEG = ITMBEG + NITMS
 
      END DO
 
      CALL CHKOUT ( 'WRENCD' )
      RETURN
      END
