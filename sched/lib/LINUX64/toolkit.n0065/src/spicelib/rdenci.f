 
C$Procedure  RDENCI  ( Read encoded integers from text file )
 
      SUBROUTINE RDENCI ( UNIT, N, DATA )
 
C$ Abstract
C
C     Read N encoded integers from a text file, decoding them into
C     their equivalent integers.
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
C      UNIT      I    Fortran unit number of input text file.
C      N         I    Number of integers to read and decode.
C      DATA      I    List of decoded integers.
C
C$ Detailed_Input
C
C     UNIT     The Fortran unit number for a previously opened text
C              file. All reading will begin at the CURRENT POSITION
C              in the text file.
C
C     N        The number of encoded integers to be read from the
C              text file attached to UNIT.
C
C$ Detailed_Output
C
C     DATA     List of decoded integers read from the text file
C              attached to UNIT.
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
C     2)   If an error occurs while reading from the text file
C          attached to  UNIT, the error SPICE(FILEREADFAILED) will
C          be signalled.
C
C     3)   If an error occurs while decoding a number, the error
C          SPICE(DECODINGERROR) will be signalled.
C
C$ Files
C
C     See the description of UNIT in Detailed_Input.
C
C$ Particulars
C
C     This routine will read N encoded integers beginning at the
C     current position in a previously opened text file. The current
C     position in a file is defined to be the text line immediately
C     following the last text line that was written or read. The
C     integers will be decoded and placed into a list of integers
C     which will be passed back to the caller. The encoded integers
C     are represented as quoted character strings so that a Fortran
C     list directed read may be used to read the encoded values,
C     rather than a formatted read with the format specifier
C     FMT = '(A)'.
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
C     Currently the encoded integers are represented as signed
C     hexadecimal numbers See INT2HX.FOR and HX2INT.FOR for details.
C
C$ Examples
C
C     Suppose we have the following input file which contains the values
C     1 - 100 encoded, and that the input file has already been opened
C     for reading. The arrow, '-->', indicates the current position in
C     the file.
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
C
C     Then, the following code fragment would read and decode these
C     100 values.
C
C           N = 100
C           CALL RDENCI( UNIT, N, DATA )
C
C      Upon returning, the array data would contain the values 1 - 100.
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
C-    SPICELIB Version 1.0.0, 19-OCT-1992 (KRG)
C
C-&
 
C$ Index_Entries
C
C      read and decode encoded integers from a text file
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
C
C     Local parameters
C
      INTEGER               ERRLEN
      PARAMETER           ( ERRLEN = 80 )
 
      INTEGER               MAXENC
      PARAMETER           ( MAXENC = 64 )
 
      INTEGER               WRKSIZ
      PARAMETER           ( WRKSIZ = 64 )
C
C     Local variables
C
      CHARACTER*(ERRLEN)    ERRMSG
      CHARACTER*(MAXENC)    WORK(WRKSIZ)
 
      INTEGER               I
      INTEGER               IOSTAT
      INTEGER               ITMBEG
      INTEGER               NITMS
 
      LOGICAL               ERROR
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'RDENCI' )
      END IF
C
C     Check to see if the number of data items is less than or equal
C     to zero. If it is, signal an error.
C
      IF ( N .LT. 1 ) THEN
 
         CALL SETMSG ( 'The number of data items to be read was' //
     .                 ' not positive: #.'                        )
         CALL ERRINT ( '#', N                                     )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                   )
         CALL CHKOUT ( 'RDENCI'                                   )
         RETURN
 
      END IF
C
C     Initialize the beginning location to place the decoded data
C     items.
C
      ITMBEG = 1
C
C     We read in the encoded numbers in blocks of size WRKSIZ, and if
C     there was not a read error we will attempt to decode the numbers.
C     We signal an error if either:
C
C                (1) there is a read error
C                (2) there is an error decoding a number.
C
C     NOTE: EOF is interpreted as a read error because we know a priori
C           exactly how many data items we need to read: N.
C
C     Begin decoding the encoded data items read from the input file
C     in blocks of size NITMS. Each time the number of data items
C     NITMS is reached, decode the encoded numbers into the data array.
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
C        Read in a block of data items to be decoded.
C
         READ (UNIT,*,IOSTAT=IOSTAT) ( WORK(I), I = 1, NITMS )
C
C        Check to see if we got a read error: IOSTAT .NE. 0. If we did,
C        then signal an error. EOF is considered to be a read error,
C        since we know exactly how many data items we expect to read.
C
         IF ( IOSTAT .NE. 0 ) THEN
 
            CALL SETMSG ( 'Error reading from logical unit #,' //
     .                    ' IOSTAT = #.'                        )
            CALL ERRINT ( '#', UNIT                             )
            CALL ERRINT ( '#', IOSTAT                           )
            CALL SIGERR ( 'SPICE(FILEREADFAILED)'               )
            CALL CHKOUT ( 'RDENCI'                              )
            RETURN
 
         END IF
C
C        Begin to decode the data items into the data array. Signal an
C        error if we cannot decode a data item.
C
         DO I = 1, NITMS
 
            CALL HX2INT ( WORK(I), DATA(ITMBEG + I - 1), ERROR, ERRMSG )
            IF ( ERROR ) THEN
 
               CALL SETMSG ( 'Decoding error occurred while'       //
     .                       ' attempting to decode item #: #. #'   )
               CALL ERRINT ( '#', I                                 )
               CALL ERRCH  ( '#', WORK(I)                           )
               CALL ERRCH  ( '#', ERRMSG                            )
               CALL SIGERR ( 'SPICE(DECODINGERROR)'                 )
               CALL CHKOUT ( 'RDENCI'                               )
               RETURN
 
            END IF
 
         END DO
C
C        Position the data item pointer at the next location to begin
C        placing the decoded items in the array DATA, and continue
C        processing the until done.
C
         ITMBEG = ITMBEG + NITMS
 
      END DO
 
      CALL CHKOUT ( 'RDENCI' )
      RETURN
      END
