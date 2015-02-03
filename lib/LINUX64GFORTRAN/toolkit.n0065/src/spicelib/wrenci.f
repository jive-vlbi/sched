 
C$Procedure  WRENCI  ( Write encoded integers to text file )
 
      SUBROUTINE WRENCI ( UNIT, N, DATA )
 
C$ Abstract
C
C     Encode and write integers to a text file.
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
      INTEGER               DATA(*)
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      UNIT      I    Fortran unit number of output text file.
C      N         I    Number of integers to encode and write.
C      DATA      I    List of integers to be encoded and written.
C
C$ Detailed_Input
C
C     UNIT     The Fortran unit number for a previously opened text
C              file. All writing will begin at the CURRENT POSITION
C              in the text file.
C
C     N        The number of integers to be encoded and written to the
C              text file attached to UNIT.
C
C     DATA     List of integers to be encoded and written to the text
C              file attached to UNIT.
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
C     This routine will accept a list of one or more integers which
C     it will encode into equivalent text strings and write to the
C     current position in a text file. The current position in a file
C     is defined to be the text line immediately following the last
C     text line that was written or read. The encoded integers are
C     written to the output text file as quoted character strings so
C     that a Fortran list directed read may be used to read the
C     encoded values, rather than a formatted read with the format
C     specifier FMT = '(A)'.
C
C     This routine is one of a pair of routines which are used to
C     encode and decode integers:
C
C           WRENCI -- Encode and write integers to a file.
C           RDENCI -- Read and decode integers from a file.
C
C     The encoding/decoding of integers is performed to provide a
C     portable means for transferring data values.
C
C     Currently the text string produced will be a signed hexadecimal
C     number See INT2HX.FOR and HX2INT.FOR for details.
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
C        DATA(I)  = I, I = 1, N
C
C     Then, the subroutine call
C
C           CALL WRENCI( UNIT, N, DATA )
C
C     will write the first 100 integers, encoded, to the output text
C     file attached to UNIT, beginning at the current position in the
C     output file, which is marked by an arrow, '-->'. The resulting
C     output will look something like the following:
C
C        -->'1' '2' '3' '4' '5' '6' '7' '8' '9' 'A' 'B' 'C' 'D' 'E'
C           'F' '10' '11' '12' '13' '14' '15' '16' '17' '18' '19'
C           '1A' '1B' '1C' '1D' '1E' '1F' '20' '21' '22' '23' '24'
C           '25' '26' '27' '28' '29' '2A' '2B' '2C' '2D' '2E' '2F'
C           '30' '31' '32' '33' '34' '35' '36' '37' '38' '39' '3A'
C           '3B' '3C' '3D' '3E' '3F' '40'
C           '41' '42' '43' '44' '45' '46' '47' '48' '49' '4A' '4B'
C           '4C' '4D' '4E' '4F' '50' '51' '52' '53' '54' '55' '56'
C           '57' '58' '59' '5A' '5B' '5C' '5D' '5E' '5F' '60' '61'
C           '62' '63' '64'
C        -->
C
C     At this point, the arrow marks the position of the file pointer
C     immediately after the call to WRENCI.
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
C        A disclaimer was added to the $ Examples section concerning
C        the output format used. The disclaimer simply states that the
C        output format used in the example is not necessarily the
C        output format actually used by the routine.
C
C-    SPICELIB Version 1.0.0, 19-OCT-1992 (KRG)
C
C-&
 
C$ Index_Entries
C
C      encode and write integers to a text file
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
      INTEGER               LENGTH(WRKSIZ)
      INTEGER               NITMS
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'WRENCI' )
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
         CALL CHKOUT ( 'WRENCI'                                      )
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
 
            CALL INT2HX ( DATA(ITMBEG + I - 1), WORK(I), LENGTH(I) )
 
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
               CALL CHKOUT ( 'WRENCI'                             )
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
 
      CALL CHKOUT ( 'WRENCI' )
      RETURN
      END
 
