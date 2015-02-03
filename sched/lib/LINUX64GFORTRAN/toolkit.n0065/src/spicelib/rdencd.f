 
C$Procedure  RDENCD  ( Read encoded d.p. numbers from file )
 
      SUBROUTINE RDENCD ( UNIT, N, DATA )
 
C$ Abstract
C
C     Read N encoded d.p. numbers from a text file, decoding them
C     into their equivalent d.p. numbers.
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
C      UNIT      I    Fortran unit number of input text file.
C      N         I    Number of d.p. numbers to read and decode.
C      DATA      I    List of decoded d.p. numbers.
C
C$ Detailed_Input
C
C     UNIT     The Fortran unit number for a previously opened text
C              file. All reading will begin at the CURRENT POSITION
C              in the text file.
C
C     N        The number of encoded double precision numbers, to be
C              read from the text file attached to UNIT.
C
C$ Detailed_Output
C
C     DATA     List of decoded double precision numbers read from the
C              text file attached to UNIT.
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
C     This routine will read N encoded double precision numbers from
C     the current position in a previously opened text file. The
C     current position in a file is defined to be the text line
C     immediately following the last text line that was written or
C     read. The numbers will be decoded and placed into a list of
C     double precision numbers which will be passed back to the caller.
C     The encoded double precision numbers are represented as quoted
C     character strings so that a Fortran list directed read may be
C     used to read the encoded values, rather than a formatted read
C     with the format specifier FMT = '(A)'.
C
C     This routine is one of a pair of routines which are used to
C     encode and decode d.p. numbers:
C
C           WRENCD -- Encode and write d.p. numbers to a file.
C           RDENCD -- Read and decode d.p. numbers from a file.
C
C     The encoding/decoding of d.p. numbers is performed to provide a
C     portable means for transferring data values.
C
C     Currently the encoded d.p. numbers are represented in a base
C     16 ``scientific notation.'' See DP2HX.FOR and HX2DP.FOR for
C     details.
C
C$ Examples
C
C     Suppose we have the following input file which contains the
C     values 1.0D0 - 100.0D0 in encoded format, and that the input
C     file has already been opened for reading. The arrow, '-->',
C     indicates the current position in the file.
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
C
C     Then the following code fragment would read and decode these
C     100 values.
C
C           N = 100
C           CALL RDENCD( UNIT, N, DATA )
C
C      Upon returning, the array data would contain the values
C      1.0D0 - 100.0D0.
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
C     F.S. Turner    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 19-MAR-1999 (FST)
C
C        Modified the long error message for SPICE(FILEREADFAILED)
C        to indicate the possibility of an incomplete text transfer
C        file as the cause.
C
C-    SPICELIB Version 1.0.0, 20-OCT-1992 (KRG)
C
C-&
 
C$ Index_Entries
C
C      read and decode encoded d.p. numbers from a text file
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
      INTEGER               ITMBEG
      INTEGER               NITMS
      INTEGER               IOSTAT
 
      LOGICAL               ERROR
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'RDENCD' )
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
         CALL CHKOUT ( 'RDENCD'                                   )
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
C                (2) there is an error decoding the number.
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
         READ (UNIT,IOSTAT=IOSTAT,FMT=*) ( WORK(I), I = 1, NITMS )
C
C        Check to see if we got a read error: IOSTAT .NE. 0. If we did,
C        then signal an error. EOF is considered to be a read error,
C        since we know exactly how many data items we expect to read.
C
         IF ( IOSTAT .NE. 0 ) THEN
 
            CALL SETMSG ( 'Error reading from logical unit #,'  //
     .                    ' IOSTAT = #. One possible cause is'  //
     .                    ' an incomplete text transfer file.'   )
            CALL ERRINT ( '#', UNIT                              )
            CALL ERRINT ( '#', IOSTAT                            )
            CALL SIGERR ( 'SPICE(FILEREADFAILED)'                )
            CALL CHKOUT ( 'RDENCD'                               )
            RETURN
 
         END IF
C
C        Begin to decode the data items into the data array. Signal an
C        error if we cannot decode a data item.
C
         DO I = 1, NITMS
 
            CALL HX2DP ( WORK(I), DATA(ITMBEG + I - 1), ERROR, ERRMSG )
            IF ( ERROR ) THEN
 
               CALL SETMSG ( 'Decoding error occurred while'     //
     .                    ' attempting to decode item #: #. #'    )
               CALL ERRINT ( '#', I                               )
               CALL ERRCH  ( '#', WORK(I)                         )
               CALL ERRCH  ( '#', ERRMSG                          )
               CALL SIGERR ( 'SPICE(DECODINGERROR)'               )
               CALL CHKOUT ( 'RDENCD'                             )
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
 
      CALL CHKOUT ( 'RDENCD' )
      RETURN
      END
