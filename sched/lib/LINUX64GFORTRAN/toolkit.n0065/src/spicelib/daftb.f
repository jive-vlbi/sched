C$Procedure DAFTB ( DAF, convert transfer file to binary file )
 
      SUBROUTINE DAFTB ( XFRLUN, BINFIL )
 
C$ Abstract
C
C     Convert the contents of an DAF transfer file into an equivalent
C     binary DAF file.
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
C     DAF
C
C$ Keywords
C
C     CONVERSION
C     FILES
C
C$ Declarations
 
      INTEGER               XFRLUN
      CHARACTER*(*)         BINFIL
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     XFRLUN     I   Logical unit of an open DAF transfer file.
C     BINFIL     I   Name of a binary DAF file to be created.
C
C$ Detailed_Input
C
C     XFRLUN      The Fortran logical unit number of a previously opened
C                 DAF transfer file has been.
C
C                 The file pointer should be positioned ready to read
C                 the file ID word.
C
C     BINFIL      The name of the binary DAF file to be created.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     See arguments XFRLUN, BINFIL.
C
C$ Exceptions
C
C     1)   If the DAF transfer file cannot be read, the error
C          SPICE(FILEREADFAILED) will be signalled.
C
C     2)   If the architecture of the file is not DAF, as specified by
C          the ID word, the error SPICE(NOTADAFFILE) will be signalled.
C
C     3)   If an error occurs while attempting to decode data in the
C          DAF transfer file, the error SPICE(BADDAFTRANSFERFILE) will
C          be signalled.
C
C     4)   If the DAF file cannot be written, a DAF file access routine
C          will signal an error with an appropriate error message.
C
C     5)   The binary DAF file opened by this routine, BINFIL, is only
C          GUARANTEED to be closed upon successful completion of the
C          transfer file to binary file conversion process. In the event
C          of an error, the caller of this routine is required to close
C          the binary DAF file BINFIL.
C
C$ Particulars
C
C     Any binary DAF file may be transferred between heterogeneous
C     Fortran environments by converting it to an equivalent file
C     containing only ASCII characters. Such a file can be transferred
C     almost universally, using any number of established protocols.
C     Once transferred, the ASCII file can be converted to a binary
C     file, using the representations native to the new host
C     environment.
C
C     This routine provides a mechanism for converting an DAF transfer
C     file created by DAFBT, or an equivalent procedure, into an
C     equivalent binary DAF file which may be used with the SPICE
C     system. It is one of a pair of routines for performing conversions
C     between the binary format of a DAF file and the DAF transfer file.
C     The inverse of this routine is the routine DAFBT.
C
C     This routine makes NO use of the DAF reserved record area. It
C     can only deal with the data portion of a DAF file in the DAF
C     transfer file.
C
C     Upon successful completion, the binary DAF file specified by
C     BINFIL will have been created. The binary DAF file that was
C     created will be closed when this routine exits. The DAF transfer
C     file will remain open, as it was on entry, and it will be
C     positioned to read the first line after the encoded DAF file data.
C
C$ Examples
C
C     Let
C
C        XFRLUN   be the Fortran logical unit attached to a DAF
C                 transfer file which is to be converted into its binary
C                 DAF equivalent.
C
C        BINFIL   be the name of the binary DAF file which will be
C                 created from the DAF transfer file.
C
C     The following subroutine call would read the DAF transfer file
C     attached to the Fortran logical unit XFRLUN, convert its data into
C     binary format, and write that data to the binary DAF file which
C     has been created:
C
C        CALL DAFTB( XFRLUN, BINFIL )
C
C$ Restrictions
C
C     1) This routine assumes that it is positioned ready to read the
C        file ID word from the DAF transfer file.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer  (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.0.1, 22-AUG-2001 (EDW)
C
C        Corrected ENDIF to END IF.
C     
C-    SPICELIB Version 3.0.0, 25-JAN-1995 (KRG)
C
C        Updated the header and in line comments to reflect the change
C        from calling files text files to calling them transfer files.
C
C        Changed the variable name TXTLUN to XFRLUN to make it
C        compatible with the change in terminology.
C
C        Changed the short error message from "BADDAFTEXTFILE" to
C        "BADDAFTRANSFERFILE".
C
C-    SPICELIB Version 2.0.0, 04-SEP-1993 (KRG)
C
C        This routine was modified to incorporate the file ID word
C        changes which will allow run time identification of the type of
C        data in a SPICE binary file.
C
C        Removed the error SPICE(IDWORDNOTKNOWN) as it was no longer
C        relevant.
C
C        Added the error SPICE(NOTADAFFILE) if this routine is called
C        with a file that does not contain an ID word identifying the
C        file as a DAF file.
C
C-    SPICELIB Version 1.0.1, 24-JUN-1993 (KRG)
C
C        Modified the description of the DAF encoded text file format
C        appearing before the program code.
C
C-    SPICELIB Version 1.0.0, 02-NOV-1992 (KRG)
C
C-&
 
C$ Index_Entries
C
C     convert daf transfer file to binary
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 3.0.0, 25-JAN-1995 (KRG)
C
C        Updated the header and in line comments to reflect the change
C        from calling files text files to calling them transfer files.
C
C        Changed the variable name TXTLUN to XFRLUN to make it
C        compatible with the change in terminology.
C
C        Changed the short error message from "BADDAFTEXTFILE" to
C        "BADDAFTRANSFERFILE".
C
C-    SPICELIB Version 2.0.0, 04-SEP-1993 (KRG)
C
C        This routine was modified to incorporate the file ID word
C        changes which will allow runtime identification of the type of
C        data in a binary file SPICE binary file.
C
C        Removed the error SPICE(IDWORDNOTKNOWN) as it was no longer
C        relevant.
C
C        Added the error SPICE(NOTADAFFILE) if this routine is called
C        with a file that does not contain an ID word identifying the
C        file as a DAF file.
C
C-    SPICELIB Version 1.0.1, 24-JUN-1993 (KRG)
C
C        Modified the description of the DAF encoded text file format
C        appearing before the program code. Changed the line:
C
C           C        < DAF ND value > < DAF NI value >
C
C        to the lines:
C
C           C        < DAF ND value >
C           C        < DAF NI value >
C
C        This change was necessary because the output format for the
C        low level routines which encode and write the data were
C        modified to fix a problem. See the routines WRENCD and WRENCI
C        for details of the modification.
C
C-    SPICELIB Version 1.0.0, 29-OCT-1992 (KRG)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
C
C     Local Parameters
C
      CHARACTER*(*)         BEGARR
      PARAMETER           ( BEGARR = 'BEGIN_ARRAY' )
 
      CHARACTER*(*)         ENDARR
      PARAMETER           ( ENDARR = 'END_ARRAY' )
 
      CHARACTER*(*)         TOTARR
      PARAMETER           ( TOTARR = 'TOTAL_ARRAYS' )
 
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 1024 )
 
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 255 )
 
      INTEGER               ERRLEN
      PARAMETER           ( ERRLEN = 320 )
 
      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN = 8    )
 
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60   )
 
      INTEGER               CRECL
      PARAMETER           ( CRECL  = 1000 )
 
      INTEGER               RESREC
      PARAMETER           ( RESREC = 0 )
C
C     Local variables
C
      CHARACTER*(IDWLEN)    IDWORD
      CHARACTER*(IFNLEN)    IFNAME
      CHARACTER*(ERRLEN)    ERRMSG
      CHARACTER*(LINLEN)    LINE
      CHARACTER*(CRECL)     NAME
      CHARACTER*(LINLEN)    REST
      CHARACTER*(IDWLEN)    TARCH
      CHARACTER*(IDWLEN)    TTYPE
      CHARACTER*(LINLEN)    WORD
 
      DOUBLE PRECISION      BUFFER( BUFSIZ )
      DOUBLE PRECISION      DSUMRY( 125    )
      DOUBLE PRECISION      SUMMRY( 125    )
 
      INTEGER               ARRCNT
      INTEGER               BARR
      INTEGER               BCNT
      INTEGER               BINHDL
      INTEGER               DTACNT
      INTEGER               EARR
      INTEGER               ECNT
      INTEGER               ERRPTR
      INTEGER               IOSTAT
      INTEGER               ISUMRY( 250    )
      INTEGER               LFTOVR
      INTEGER               ND
      INTEGER               NI
      INTEGER               NUMARR
      INTEGER               NUMDTA
      INTEGER               NUMLFT
      INTEGER               SNMLEN
 
      LOGICAL               INARR
      LOGICAL               MORE
C
C     Standard/ SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFTB' )
      END IF
C
C     A brief description of the DAF transfer file format and its
C     intended use follows. This description is intended to provide a
C     simple ``picture'' of the DAF transfer file format to aid in the
C     understanding of this routine. This description is NOT intended to
C     be a detailed specification of the file format.
C
C     A DAF transfer file contains all of the data from a binary
C     DAF file, except for the reserved record area, in an encoded
C     ASCII format. The file also contains some bookkeeping information
C     for maintaining the integrity of the data. The DAF transfer file
C     format allows the full precision of both integer and floating
C     point numeric data to be maintained in a portable fashion. The DAF
C     transfer file format is intended to provide a reliable and
C     accurate means for porting data among multiple computer systems
C     and for the archival storage of data.
C
C     A DAF transfer file is not intended to be used directly to
C     provide data to a program, the equivalent binary DAF file is
C     to be used for this purpose. In no way should any program, other
C     than a DAF binary <-> transfer conversion program, rely on the DAF
C     encoded transfer file format.
C
C     To correctly understand the DAF transfer file description
C     the reader should be familiar with the DAF file architecture.
C     Items enclosed in angle brackets, '<' and '>', are used to
C     represent the data which is to be placed at that position in
C     the file. The bookkeeping information is represented exactly
C     as it would appear in a DAF transfer file.
C
C     Let
C
C       BOF    denote the beginning of the file
C       EOF    denote the end of the file
C
C    and
C
C       n      denote the total number of arrays in a DAF file
C       NA(i)  denote the number of double precision numbers in array i
C       m(i)   denote the number of blocks of encoded data for array i
C       N(i,j) denote the number of encoded double precision numbers
C              in block j of array i
C
C     and
C
C              m(i)
C             -----
C             \
C              >   N(i,k) = NA(i),   i = 1, ..., n.
C             /
C             -----
C              k=1
C
C     A DAF encoded transfer file has the following format:
C
C        <BOF>
C        < Information line >
C        < DAF file ID word >
C        < DAF ND value >
C        < DAF NI value >
C        < DAF internal file name >
C        BEGIN_ARRAY 1 NA(1)
C        < Name for array 1 >
C        < ND double precision summary values >
C        < NI-2 integer summary values >
C        N(1,1)
C        < N(1,1) Encoded double precision numbers >
C        N(1,2)
C        < N(1,2) Encoded double precision numbers >
C                          .
C                          .
C                          .
C        N(1,m(1))
C        < N(1,m(1)) Encoded double precision numbers >
C        END_ARRAY 1 NA(1)
C        BEGIN_ARRAY 2 NA(2)
C        < Name for array 2 >
C        < ND double precision summary values >
C        < NI-2 integer summary values >
C        N(2,1)
C        < N(2,1) Encoded double precision numbers >
C        N(2,2)
C        < N(2,2) Encoded double precision numbers >
C                          .
C                          .
C                          .
C        N(2,m(2))
C        < N(2,m(2)) Encoded double precision numbers >
C        END_ARRAY 2 NA(2)
C                          .
C                          .
C                          .
C        BEGIN_ARRAY n NA(n)
C        < Name for array n >
C        < ND double precision summary values >
C        < NI-2 integer summary values >
C        N(n,1)
C        < N(n,1) Encoded double precision numbers >
C        N(n,2)
C        < N(n,2) Encoded double precision numbers >
C                          .
C                          .
C                          .
C        N(n,m(n))
C        < N(n,m(n)) Encoded double precision numbers >
C        END_ARRAY n NA(n)
C        TOTAL_ARRAYS  n
C        <EOF>
C
C
C     Initialize a few things.
C
      TARCH  = ' '
      TTYPE  = ' '
      IDWORD = ' '
C
C     We begin by reading the DAF file ID word from the DAF transfer
C     file. We should have been positioned ready to read this. If an
C     error occurs, set an appropriate error message and signal the
C     error.
C
      READ (XFRLUN,*,IOSTAT=IOSTAT) IDWORD
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL SETMSG ( 'Error reading the file ID word from the'  //
     .                 ' DAF transfer file ''#''. IOSTAT = #.'     )
         CALL ERRFNM ( '#', XFRLUN                                 )
         CALL ERRINT ( '#', IOSTAT                                 )
         CALL SIGERR ( 'SPICE(FILEREADFAILED)'                     )
         CALL CHKOUT ( 'DAFTB'                                     )
         RETURN
 
      END IF
C
C     Separate the ID word into its components and verify that we are
C     looking at a DAF transfer file. If we're not, then this routine
C     should not be used.
C
      CALL IDW2AT ( IDWORD, TARCH, TTYPE )
 
      IF ( TARCH .NE. 'DAF' ) THEN
 
         CALL SETMSG ( 'File architecture is not ''DAF'' for' //
     .                 ' file ''#'''                           )
         CALL ERRFNM ( '#', XFRLUN                             )
         CALL SIGERR ( 'SPICE(NOTADAFFILE)'                    )
         CALL CHKOUT ( 'DAFTB'                                 )
         RETURN
 
      END IF
C
C     The file architecture is OK, but before we can open the binary
C     DAF, we need to get the summary format and the internal file name
C     from the DAF transfer file. We begin doing this here.
C
C     Read in the ND and NI values for the DAF file.
C
      CALL RDENCI( XFRLUN, 2, ISUMRY )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'DAFTB' )
         RETURN
 
      END IF
 
      ND = ISUMRY(1)
      NI = ISUMRY(2)
C
C     Read the internal filename for the DAF file.
C
      READ (XFRLUN,*,IOSTAT=IOSTAT) IFNAME
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL SETMSG ( 'Error reading the internal filename from'  //
     .                 ' the DAF transfer file ''#''. IOSTAT = #.'  )
         CALL ERRFNM ( '#', XFRLUN                                  )
         CALL ERRINT ( '#', IOSTAT                                  )
         CALL SIGERR ( 'SPICE(FILEREADFAILED)'                      )
         CALL CHKOUT ( 'DAFTB'                                      )
         RETURN
 
      END IF
C
C     Open a new binary DAF file. Call the proper open routine,
C     depending on whether it's a new file or an old file.
C
      IF ( TTYPE .NE. '?' ) THEN
         CALL DAFONW ( BINFIL, TTYPE, ND, NI, IFNAME, RESREC, BINHDL )
      ELSE
         CALL DAFOPN ( BINFIL, ND, NI, IFNAME, RESREC, BINHDL )
      END IF
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'DAFTB' )
         RETURN
 
      END IF
C
C     Calculate the length of the segment names.
C
      SNMLEN = 8 * ( ND + ( NI + 1 ) / 2 )
C
C     Initialize a few things: the array counter and the data counter.
C
      ARRCNT = 0
      DTACNT = 0
C
C     We currently have more to process.
C
      MORE  = .TRUE.
C
C     We are currently not processing an array.
C
      INARR = .FALSE.
C
C     Begin converting the DAF transfer file into a binary DAF file
C     here.
C
      DO WHILE ( MORE )
 
         READ (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE
 
         IF ( IOSTAT .NE. 0 ) THEN
 
            CALL SETMSG ( 'Error reading from the DAF transfer'  //
     .                    ' file ''#''. IOSTAT = #.'              )
            CALL ERRFNM ( '#', XFRLUN                             )
            CALL ERRINT ( '#', IOSTAT                             )
            CALL SIGERR ( 'SPICE(FILEREADFAILED)'                 )
            CALL CHKOUT ( 'DAFTB'                                 )
            RETURN
 
         END IF
C
C        At this point, we should be beginning an array, ending an
C        array, or scanning for the total number of arrays. So look
C        for the appropriate keyword.
C
         CALL NEXTWD( LINE, WORD, REST )
 
         IF ( WORD .EQ. BEGARR ) THEN
C
C           Get the array number.
C
            CALL NEXTWD( REST, WORD, REST           )
            CALL NPARSI( WORD, BARR, ERRMSG, ERRPTR )
 
            IF ( ERRMSG .NE. ' ' ) THEN
 
               CALL SETMSG ( 'Begin array error, could not parse'  //
     .                       ' array number. Error: # File: #'      )
               CALL ERRCH  ( '#', ERRMSG                            )
               CALL ERRFNM ( '#', XFRLUN                            )
               CALL SIGERR ( 'SPICE(BADDAFTRANSFERFILE)'            )
               CALL CHKOUT ( 'DAFTB'                                )
               RETURN
 
            END IF
C
C           Parse the count of double precision numbers in the array.
C
            CALL NEXTWD( REST, WORD, REST           )
            CALL NPARSI( WORD, BCNT, ERRMSG, ERRPTR )
 
            IF ( ERRMSG .NE. ' ' ) THEN
 
               CALL SETMSG ( 'Begin array error, could not parse' //
     .                       ' the data count for array: #.'      //
     .                       ' Error: # File: #'                   )
               CALL ERRINT ( '#', BARR                             )
               CALL ERRCH  ( '#', ERRMSG                           )
               CALL ERRFNM ( '#', XFRLUN                           )
               CALL SIGERR ( 'SPICE(BADDAFTRANSFERFILE)'           )
               CALL CHKOUT ( 'DAFTB'                               )
               RETURN
 
            END IF
C
C           If we got to here, we are inside an array, so set the in
C           array flag, INARR, to .TRUE. and increment the array
C           counter.
C
            INARR  = .TRUE.
            ARRCNT = ARRCNT + 1
 
         ELSE IF ( WORD .EQ. ENDARR ) THEN
C
C           Get the array number.
C
            CALL NEXTWD( REST, WORD, REST           )
            CALL NPARSI( WORD, EARR, ERRMSG, ERRPTR )
 
            IF ( ERRMSG .NE. ' ' ) THEN
 
               CALL SETMSG ( 'End array error, could not parse'  //
     .                       ' array number. Error: # File: #'    )
               CALL ERRCH  ( '#', ERRMSG                          )
               CALL ERRFNM ( '#', XFRLUN                          )
               CALL SIGERR ( 'SPICE(BADDAFTRANSFERFILE)'          )
               CALL CHKOUT ( 'DAFTB'                              )
               RETURN
 
            END IF
C
C           Parse the count of double precision numbers in the array.
C
            CALL NEXTWD( REST, WORD, REST           )
            CALL NPARSI( WORD, ECNT, ERRMSG, ERRPTR )
 
            IF ( ERRMSG .NE. ' ' ) THEN
 
               CALL SETMSG ( 'End array error, could not parse' //
     .                       ' the data count for array: #.'    //
     .                       ' Error: # File: #'                 )
               CALL ERRINT ( '#', EARR                           )
               CALL ERRCH  ( '#', ERRMSG                         )
               CALL ERRFNM ( '#', XFRLUN                         )
               CALL SIGERR ( 'SPICE(BADDAFTRANSFERFILE)'         )
               CALL CHKOUT ( 'DAFTB'                             )
               RETURN
 
            END IF
C
C           Check to see if the beginning and ending array numbers
C           match. If not, signal an appropriate error.
C
            IF ( EARR .NE. BARR ) THEN
 
               CALL SETMSG ( 'Data array number mismatch:'         //
     .                       ' Beginning number: #; Ending'        //
     .                       ' number: #. File: #'                  )
               CALL ERRINT ( '#', BARR                              )
               CALL ERRINT ( '#', EARR                              )
               CALL ERRFNM ( '#', XFRLUN                            )
               CALL SIGERR ( 'SPICE(BADDAFTRANSFERFILE)'            )
               CALL CHKOUT ( 'DAFTB'                                )
               RETURN
 
            END IF
C
C           Check to see if the beginning and ending array data counts
C           match. If not, signal an appropriate error.
C
            IF ( ECNT .NE. BCNT ) THEN
 
               CALL SETMSG ( 'Data array count mismatch:'      //
     .                       ' Beginning count: #;'            //
     .                       ' Ending count: #. File: #'        )
               CALL ERRINT ( '#', BCNT                          )
               CALL ERRINT ( '#', ECNT                          )
               CALL ERRFNM ( '#', XFRLUN                        )
               CALL SIGERR ( 'SPICE(BADDAFTRANSFERFILE)'        )
               CALL CHKOUT ( 'DAFTB'                            )
               RETURN
 
            END IF
C
C           If we got to here, we have successfully ended the
C           processing of an array, so set the in array flag, INARR,
C           to  .FALSE..
C
            INARR = .FALSE.
 
         ELSE IF ( WORD .EQ. TOTARR ) THEN
C
C           We have the total arrays keyword to parse, so get
C           the total number of arrays processed.
C
            CALL NEXTWD( REST, WORD, REST             )
            CALL NPARSI( WORD, NUMARR, ERRMSG, ERRPTR )
 
            IF ( ERRMSG .NE. ' ' ) THEN
 
               CALL SETMSG ( 'Array count error, could not parse' //
     .                       ' the total number of arrays: #.'    //
     .                       ' File: #'                            )
               CALL ERRCH  ( '#', ERRMSG                           )
               CALL ERRFNM ( '#', XFRLUN                           )
               CALL SIGERR ( 'SPICE(BADDAFTRANSFERFILE)'           )
               CALL CHKOUT ( 'DAFTB'                               )
               RETURN
 
            END IF
 
            IF ( ARRCNT .NE. NUMARR ) THEN
 
                  CALL SETMSG ( 'The number of data arrays'          //
     .                          ' processed (#) was not equal to'    //
     .                          ' the number of data arrays placed'  //
     .                          ' in the DAF transfer file (#).'     //
     .                          ' File: #'                            )
                  CALL ERRINT ( '#', ARRCNT                           )
                  CALL ERRINT ( '#', NUMARR                           )
                  CALL ERRFNM ( '#', XFRLUN                           )
                  CALL SIGERR ( 'SPICE(BADDAFTRANSFERFILE)'           )
                  CALL CHKOUT ( 'DAFTB'                               )
                  RETURN
 
            END IF
C
C           If we got to here, we have successfully processed the
C           entir data portion of the DAF transfer file, so there is
C           no more data.
C
            MORE = .FALSE.
 
         ELSE
 
            CALL SETMSG ( 'Unknown keyword ''#'' encountered'      //
     .                    ' while processing the DAF transfer'     //
     .                    ' file #.'                                )
            CALL ERRCH  ( '#', WORD                                 )
            CALL ERRFNM ( '#', XFRLUN                               )
            CALL SIGERR ( 'SPICE(BADDAFTRANSFERFILE)'               )
            CALL CHKOUT ( 'DAFTB'                                   )
            RETURN
 
         END IF
C
C        If we have begun an array, then process it. Otherwise, we
C        have either ended an array or ended the file.
C
         IF ( INARR ) THEN
 
            DTACNT = 0
 
            READ (XFRLUN,*,IOSTAT=IOSTAT) NAME( :SNMLEN)
 
            IF ( IOSTAT .NE. 0 ) THEN
 
               CALL SETMSG ( 'Error reading the array name from the' //
     .                       ' DAF transfer file #. IOSTAT = #.'      )
               CALL ERRFNM ( '#', XFRLUN                              )
               CALL ERRINT ( '#', IOSTAT                              )
               CALL SIGERR ( 'SPICE(FILEREADFAILED)'                  )
               CALL CHKOUT ( 'DAFTB'                                  )
               RETURN
 
            END IF
C
C           Read in the double precision part of the summary.
C
            CALL RDENCD ( XFRLUN, ND, DSUMRY )
 
            IF ( FAILED() ) THEN
 
               CALL CHKOUT ( 'DAFTB' )
               RETURN
 
            END IF
C
C           Read in the integer part of the summary. The beginning and
C           ending addresses, ISUMRY(NI-1) and ISUMRY(NI), for the
C           array are not known currently. They will be filled in when
C           the array is actually written to the DAF file.
C
            CALL RDENCI( XFRLUN, NI-2, ISUMRY )
 
            IF ( FAILED() ) THEN
 
               CALL CHKOUT ( 'DAFTB' )
               RETURN
 
            END IF
C
C           Pack the summary information into the DAF array summary.
C
            CALL DAFPS  ( ND, NI, DSUMRY, ISUMRY, SUMMRY )
C
C           Begin a new array in the binary DAF file.
C
            CALL DAFBNA ( BINHDL, SUMMRY, NAME( :SNMLEN) )
 
            IF ( FAILED() ) THEN
 
               CALL CHKOUT ( 'DAFTB' )
               RETURN
 
            END IF
C
C           Read and decode the data in the current DAF array.
C
C           First set the count of numbers yet to be decoded and placed
C           in the binary DAF file.
C
            NUMLFT = BCNT
 
            DO WHILE ( NUMLFT .GT. 0 )
C
C              First, read in the count of encoded numbers in the
C              current data block.
C
               READ (XFRLUN,*,IOSTAT=IOSTAT) NUMDTA
 
               IF ( IOSTAT .NE. 0 ) THEN
 
                  CALL SETMSG ( 'Error reading array data from the' //
     .                          ' DAF transfer file #. IOSTAT = #.'  )
                  CALL ERRFNM ( '#', XFRLUN                          )
                  CALL ERRINT ( '#', IOSTAT                          )
                  CALL SIGERR ( 'SPICE(FILEREADFAILED)'              )
                  CALL CHKOUT ( 'DAFTB'                              )
                  RETURN
 
               END IF
C
C              Now read and decode the data in the current data block,
C              placing the data in the current array in the binary DAF
C              file.
C
               LFTOVR = NUMDTA
 
               DO WHILE ( LFTOVR .GT. 0 )
 
                  IF ( LFTOVR .GE. BUFSIZ ) THEN
 
                     NUMDTA = BUFSIZ
 
                  ELSE
 
                     NUMDTA = LFTOVR
 
                  END IF
C
C                 Read and decode a buffer of encoded double precision
C                 data from the DAF transfer file.
C
                  CALL RDENCD ( XFRLUN, NUMDTA, BUFFER )
 
                  IF ( FAILED() ) THEN
 
                     CALL CHKOUT ( 'DAFTB' )
                     RETURN
 
                  END IF
C
C                 Write the double precision data to the current array
C                 in the binary DAF file.
C
                  CALL DAFADA ( BUFFER, NUMDTA )
 
                  IF ( FAILED() ) THEN
 
                     CALL CHKOUT ( 'DAFTB' )
                     RETURN
 
                  END IF
C
C                 Decrement the counters for the amount of data
C                 remaining to be moved from the current data block,
C                 LFTOVR, and the current array, NUMLFT.
C
                  LFTOVR = LFTOVR - NUMDTA
                  NUMLFT = NUMLFT - NUMDTA
C
C                 Increment the counter for the amount of data that
C                 has been successfully moved into the current array
C                 in the binary DAF file.
C
                  DTACNT = DTACNT + NUMDTA
 
               END DO
C
C              At this point, we have either finished reading in the
C              entire array, or we have just completed reading the
C              current encoded block of data for the current array
C              from the DAF transfer file.
C
            END DO
C
C           If we got to here, we have successfully written an array
C           to the binary file, so we need to end it.
C
            CALL DAFENA
 
            IF ( FAILED() ) THEN
 
               CALL CHKOUT ( 'DAFTB' )
               RETURN
 
            END IF
 
         END IF
 
      END DO
C
C     Close only the binary file.
C
      CALL DAFCLS ( BINHDL )
 
      CALL CHKOUT ( 'DAFTB' )
      RETURN
      END
