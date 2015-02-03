 
C$Procedure DASTB ( DAS, convert transfer file to binary file )
 
      SUBROUTINE DASTB ( XFRLUN, BINFIL )
 
C$ Abstract
C
C     Convert the contents of a DAS transfer file into an equivalent
C     binary DAS file.
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
C     DAS
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
C     XFRLUN     I   Logical unit of an open DAS transfer file.
C     BINFIL     I   Name of the binary DAS file to be created.
C
C$ Detailed_Input
C
C     XFRLUN   The Fortran logical unit number of a previously opened
C              DAS transfer file.
C
C              The file pointer should be positioned ready to read
C              the DAS file ID word.
C
C     BINFIL   The name of the binary DAS file to be created.
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
C     1)   If the DAS transfer file cannot be read, the error
C          SPICE(FILEREADFAILED) will be signalled.
C
C     2)   If the specified file is not a DAS file, as indicated by the
C          file's ID word, the error SPICE(NOTADASFILE) is signalled.
C
C     3)   If an error occurs while attempting to decode data in the
C          DAS transfer file, the error SPICE(BADDASTRANSFERFILE) will
C          be signalled.
C
C     4)   If the DAS file cannot be written, a DAS file access routine
C          will signal an error with an appropriate error message.
C
C     5)   The binary DAS file opened by this routine, BINFIL, is only
C          GUARANTEED to be closed upon successful completion of the
C          text to binary conversion process. In the event of an error,
C          the caller of this routine is required to close the binary
C          DAS file BINFIL.
C
C$ Particulars
C
C     Any binary DAS file may be transferred between heterogeneous
C     Fortran environments by converting it to an equivalent file
C     containing only ASCII characters called a DAS transfer file.
C     Such a file can be transferred almost universally using any number
C     of established protocols. Once transferred, the DAS transfer file
C     can be converted to a binary file using the representations native
C     to the new host environment.
C
C     This routine provides a mechanism for converting a DAS
C     transfer file created by DASBT, or an equivalent procedure,
C     into an equivalent binary DAS file which may be used with the
C     SPICE system. It is one of a pair of routines for performing
C     conversions between the binary format of a DAS file and the DAS
C     transfer file. The inverse of this routine is the routine DASTB.
C
C     Upon successful completion, the binary DAS file specified by
C     BINFIL will have been created. The binary DAS file that was
C     created will be closed when this routine exits. The DAS transfer
C     file will remain open, as it was on entry, and it will be
C     positioned to read the first line after the encoded DAS file data.
C
C$ Examples
C
C     Let
C
C        XFRLUN   be the Fortran logical unit attached to a DAS transfer
C                 file which is to be converted into its binary DAS
C                 equivalent.
C
C        BINFIL   be the name of the binary DAS file which will be
C                 created.
C
C     Then, the following subroutine call would read the DAS transfer
C     file attached to the Fortran logical unit XFRLUN, convert its data
C     into binary format, and write that data to the binary DAS file
C     which is being created:
C
C        CALL DASTB( XFRLUN, BINFIL )
C
C$ Restrictions
C
C     1) This routine assumes that it is positioned ready to read the
C        DAS file ID word from the encoded text DAS file.
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
C-    SPICELIB Version 3.1.0, 06-DEC-1995 (KRG)
C
C        Updated the call to DASONW; a new argument was added to the 
C        call for reserving comment records.
C
C-    SPICELIB Version 3.0.0, 13-AUG-1994 (KRG)
C
C        Updated the header and in line comments to reflect the change
C        from calling files text files to calling them transfer files.
C
C        Changed the variable name XFRLUN to XFRLUN to make it
C        compatible with the change in terminology.
C
C        Changed the short error message "BADDASTEXTFILE" to the
C        message "BADDASTRANSFERFILE".
C
C-    SPICELIB Version 2.0.0, 27-OCT-1993 (KRG)
C
C       Updated the routine to use the new format ID words which
C       contain type as well as architecture information.
CC
C       Fixed a typo in the description of the DAS encoded text file:
C       ncomc appeared where nresvc should have been.
C
C-    SPICELIB Version 1.0.0, 02-NOV-1992 (KRG)
C
C-&
 
C$ Index_Entries
C
C     convert das transfer file to binary das
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 3.1.0, 06-DEC-1995 (KRG)
C
C        Updated the call to DASONW; a new argument was added to the 
C        call for reserving comment records. The value used here is 
C        zero (0).
C
C-    SPICELIB Version 3.0.0, 13-AUG-1994 (KRG)
C
C        Updated the header and in line comments to reflect the change
C        from calling files text files to calling them transfer files.
C
C        Changed the variable name XFRLUN to XFRLUN to make it
C        compatible with the change in terminology.
C
C        Changed the short error message "BADDASTEXTFILE" to the
C        message "BADDASTRANSFERFILE".
C
C-    SPICELIB Version 2.0.0, 27-OCT-1993 (KRG)
C
C        Updated the routine to use the new format ID words which
C        contain type as well as architecture information.
C
C        Changed the wording of exception '2)' so that it would make
C        sense with the ID word format change that was made.
C
C        Changed the error
C
C           SPICE(DASIDWORDNOTKNOWN)
C
C        to
C
C           SPICE(NOTADASFILE)
C
C        Added variables to support the file architecture and type
C        stored in the ID word. These are used in order to verify that
C        the text file that is to be converted is indeed a DAS file.
C        This test is performed instead of testing whether the ID word
C        is equal to 'NAIF/DAS'.
C
C        Modified the long error message that was set to conform to the
C        ID word change.
C
C        Changed the DASOPN call to DASONW to support the addition of
C        type information to the ID word.
C
C        Fixed a typo in the description of the DAS encoded text file:
C        ncomc appeared where nresvc should have been.
C
C-    SPICELIB Version 1.0.0, 02-NOV-1992 (KRG)
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
C      CHARACTER*(*)         BEGRES
C      PARAMETER           ( BEGRES = 'BEGIN_RESERVED_BLOCK'   )
C
C      CHARACTER*(*)         ENDRES
C      PARAMETER           ( ENDRES = 'END_RESERVED_BLOCK'     )
C
C      CHARACTER*(*)         TRRBLK
C      PARAMETER           ( TRRBLK = 'TOTAL_RESERVED_BLOCKS'  )
C
      CHARACTER*(*)         BEGCOM
      PARAMETER           ( BEGCOM = 'BEGIN_COMMENT_BLOCK'    )
 
      CHARACTER*(*)         ENDCOM
      PARAMETER           ( ENDCOM = 'END_COMMENT_BLOCK'      )
 
      CHARACTER*(*)         BCBLK
      PARAMETER           ( BCBLK  = 'BEGIN_CHARACTER_BLOCK'  )
 
      CHARACTER*(*)         ECBLK
      PARAMETER           ( ECBLK  = 'END_CHARACTER_BLOCK'    )
 
      CHARACTER*(*)         BDBLK
      PARAMETER           ( BDBLK  = 'BEGIN_DP_BLOCK'         )
 
      CHARACTER*(*)         EDBLK
      PARAMETER           ( EDBLK  = 'END_DP_BLOCK'           )
 
      CHARACTER*(*)         BIBLK
      PARAMETER           ( BIBLK  = 'BEGIN_INTEGER_BLOCK'    )
 
      CHARACTER*(*)         EIBLK
      PARAMETER           ( EIBLK  = 'END_INTEGER_BLOCK'      )
 
      CHARACTER*(*)         TCMBLK
      PARAMETER           ( TCMBLK = 'TOTAL_COMMENT_BLOCKS'   )
 
      CHARACTER*(*)         TCBLKS
      PARAMETER           ( TCBLKS = 'TOTAL_CHARACTER_BLOCKS' )
 
      CHARACTER*(*)         TDBLKS
      PARAMETER           ( TDBLKS = 'TOTAL_DP_BLOCKS'        )
 
      CHARACTER*(*)         TIBLKS
      PARAMETER           ( TIBLKS = 'TOTAL_INTEGER_BLOCKS'   )
 
      INTEGER               CBFLEN
      PARAMETER           ( CBFLEN = 4                        )
 
      INTEGER               CRLEN
      PARAMETER           ( CRLEN  = 1024                     )
 
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 255                      )
 
      INTEGER               ERRLEN
      PARAMETER           ( ERRLEN = 320                      )
 
      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN = 8                        )
 
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60                       )
 
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 1024                     )
 
      INTEGER               BCBPOS
      PARAMETER           ( BCBPOS = 1                        )
 
      INTEGER               ECBPOS
      PARAMETER           ( ECBPOS = CBFLEN                   )
C
C     Local variables
C
      CHARACTER*(CBFLEN)    CBUFFR(BUFSIZ)
      CHARACTER*(CRLEN)     CRECRD
      CHARACTER*(ERRLEN)    ERRMSG
      CHARACTER*(IDWLEN)    IDWORD
      CHARACTER*(IFNLEN)    IFNAME
      CHARACTER*(LINLEN)    LINE
      CHARACTER*(LINLEN)    REST
      CHARACTER*(IDWLEN)    TARCH
      CHARACTER*(IDWLEN)    TTYPE
      CHARACTER*(LINLEN)    WORD
 
      DOUBLE PRECISION      DBUFFR(BUFSIZ)
 
      INTEGER               BCOUNT
      INTEGER               BINDEX
      INTEGER               BLKCNT
      INTEGER               DASLUN
      INTEGER               DTACNT
      INTEGER               ECOUNT
      INTEGER               EINDEX
      INTEGER               ERRPTR
      INTEGER               HANDLE
      INTEGER               IBUFFR(BUFSIZ)
      INTEGER               IOSTAT
      INTEGER               NCDATA
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NDDATA
      INTEGER               NIDATA
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               NUMBLK
      INTEGER               NUMDTA
      INTEGER               NUMLFT
      INTEGER               RECNO
      INTEGER               TCOUNT
 
      LOGICAL               INBLK
      LOGICAL               MORE
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASTB' )
      END IF
C
C     A DAS transfer file contains in an encoded form all of the data
C     from the original binary DAS file. This includes the reserved
C     record area, the comment area, and the character, double
C     precision, and integer data arrays as well.
C
C     Currently, the reserved record area has not been implemented, as
C     there is no need for it at this time. If, or when, the reserved
C     record area is implemented, this routine will need to be modified
C     in order to support it. See the code for details.
C
C     The data in the DAS transfer file are available as sequences of
C     small blocks of data. This is to provide a means for performing
C     some error detection when converting a DAS transfer file into its
C     binary equivalent. Each block of data is enclosed within begin and
C     end block markers which hold the count of data items in a data
C     block. When all of the data blocks for a data area have been
C     written, a total blocks line is read to verify that all of the
C     data has been converted.
C
C     The data in the DAS transfer file MUST appear in the following
C     order for this routine to work properly.
C
C           1) Reserved records (when/if implemented)
C           2) Comment area
C           3) Character data array
C           4) Double precision data array
C           5) Integer data array
C
C     If the data count for any of these DAS data areas is zero,
C     conversion proceeds with the next DAS data area in the list.
C
C     For example, suppose that we have a binary DAS file where there
C     are 0 reserved characters in the reserved record area, 5000
C     comment characters in the comment area, and that the character,
C     double precision, and integer array counts are 0, 2300, and
C     6900, respectively. Then, the DAS encoded text file will contain
C     no reserved record data blocks, 2 comment data blocks, no
C     character data blocks, 3 double precision data blocks, and 7
C     integer data blocks, in that order.
C
C     DAS encoded text file description.
C     ----------------------------------
C
C
C     A brief description of the DAS encoded file format and its
C     intended use follows. This description is intended to provide a
C     simple ``picture'' of the DAS transfer file format to aid in the
C     understanding of this routine. This description is NOT intended to
C     be a detailed specification of the file format.
C
C     A DAS transfer file contains all of the data from a binary
C     DAS file in an encoded ASCII format. It also contains some
C     bookkeeping information for maintaining the integrity of the
C     data. The DAS transfer file format allows the full precision of
C     character, integer, and floating point numeric data to be
C     maintained in a portable fashion. The DAS transfer file format is
C     intended to provide a reliable and accurate means for porting data
C     among multiple computer systems and for the archival storage of
C     data.
C
C     A DAS transfer file is not intended to be used directly to provide
C     data to a program. The equivalent binary DAS file is to be used
C     for this purpose. In no way should any program, other than a DAS
C     binary <-> transfer conversion program, rely on the DAS transfer
C     file format.
C
C     To correctly understand the DAS transfer file description  the
C     reader should be familiar with the DAS file architecture. Items
C     enclosed in angle brackets, '<' and '>', are used to represent the
C     data which are to be placed at that position in the file. The
C     bookkeeping information which appears is represented exactly as it
C     would appear in a DAS transfer file.
C
C     Let
C
C        <BOF>  denote the beginning of the file
C        <EOF>  denote the end of the file
C
C     and
C
C        nresvb  denote the number of encoded reserved record data
C                blocks generated
C        nresvc  denote the total number of reserved record characters
C                in the  reserved record area of a DAS file
C        ncomb   denote the number of encoded comment data blocks
C                generated
C        ncomc   denote the total number of comment characters in the
C                comment area of a DAS file
C        nchrb   denote the number of encoded character data blocks
C                generated
C        nchrs   denote the count of characters in the DAS character
C                data array
C        ndpb    denote the number of encoded double precision data
C                blocks generated
C        ndps    denote the count of double precision numbers in the DAS
C                double precision data array
C        nintb   denote the number of encoded integer data blocks
C                generated
C        nints   denote the count of integers in the DAS integer data
C                array
C
C     A DAS encoded transfer file has the following format:
C
C        <BOF>
C        < Information line >
C        < DAS file ID word >
C        < Internal filename >
C        < Encoded count of reserved records >
C        < Encoded count of reserved characters >
C        < Encoded count of comment records >
C        < Encoded count of comment characters >
C        < Blocks of encoded reserved record data, if nresvc > 0 >
C        TOTAL_RESERVED_BLOCKS nresvb nresvc
C        < Blocks of encoded comment data, if ncomc > 0 >
C        TOTAL_COMMENT_BLOCKS ncomb ncomc
C        < Encoded count of character data >
C        < Encoded count of double precision data >
C        < Encoded count of integer data >
C        < Blocks of encoded character data, if nchrs > 0 >
C        TOTAL_CHARACTER_BLOCKS nchrb nchrs
C        < Blocks of encoded double precision data, if ndps > 0 >
C        TOTAL_DP_BLOCKS ndpb ndps
C        < Blocks of encoded integer data, if nints > 0 >
C        TOTAL_INTEGER_BLOCKS nintb nints
C        <EOF>
C
C     This routine will check the SPICELIB function FAILED() after
C     each call, or consecutive sequence of calls, to data encoding
C     routines, and if an error was signalled it will simply check out
C     and return to the caller.
C
C     This routine will check the SPICELIB function FAILED() after
C     each DAS file access call, and if an error was signalled it will
C     simply check out and return to the caller.
C
C     We begin by reading the DAS file ID word from the DAS transfer
C     file. We should have been positioned ready to read this. If an
C     error occurs, set an appropriate error message and signal the
C     error.
C
      IDWORD = ' '
      READ (XFRLUN,*,IOSTAT=IOSTAT) IDWORD
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL SETMSG ( 'Error reading the file ID word from the'  //
     .                 ' DAS transfer file: #. IOSTAT = #.'        )
         CALL ERRFNM ( '#', XFRLUN                                 )
         CALL ERRINT ( '#', IOSTAT                                 )
         CALL SIGERR ( 'SPICE(FILEREADFAILED)'                     )
         CALL CHKOUT ( 'DASTB'                                     )
         RETURN
 
      END IF
C
C     Check the DAS ID word. When checking the ID word all we care about
C     is that we are attempting to convert a DAS file. So, split the
C     ID word into its architecture and type and check the architecture.
C
      CALL IDW2AT ( IDWORD, TARCH, TTYPE )
 
      IF ( TARCH .NE. 'DAS' ) THEN
 
         CALL SETMSG ( 'File architecture was not ''DAS'' for' //
     .                 ' file #.'                               )
         CALL ERRFNM ( '#', XFRLUN                              )
         CALL SIGERR ( 'SPICE(NOTADASFILE)'                     )
         CALL CHKOUT ( 'DASTB'                                  )
         RETURN
 
      END IF
C
C     Read the internal filename for the DAS file.
C
      IFNAME = ' '
      READ (XFRLUN,*,IOSTAT=IOSTAT) IFNAME
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL SETMSG ( 'Error reading the internal filename from' //
     .                 ' the DAS transfer file: #. IOSTAT = #.'    )
         CALL ERRFNM ( '#', XFRLUN                                 )
         CALL ERRINT ( '#', IOSTAT                                 )
         CALL SIGERR ( 'SPICE(FILEREADFAILED)'                     )
         CALL CHKOUT ( 'DASTB'                                     )
         RETURN
 
      END IF
C
C     Open a new binary DAS file and write its file record.
C
      CALL DASONW ( BINFIL, TTYPE, IFNAME, 0, HANDLE )
 
      IF ( FAILED() ) THEN
C
C        If an error occurred while opening the new DAS file,
C        then check out and return.
C
         CALL CHKOUT ( 'DASTB' )
         RETURN
 
      END IF
C
C     Write the initial file record to the newly opened DAS file. This
C     call will overwrite the ID word set when we opened the file with
C     the ID word from the DAS transfer file. We got to this point, so
C     we know that the ID word was a good one.
C
      NCOMR  = 0
      NCOMC  = 0
      NRESVR = 0
      NRESVC = 0
      CALL DASWFR ( HANDLE,
     .              IDWORD, IFNAME,
     .              NRESVR, NRESVC,
     .              NCOMR,  NCOMC   )
 
      IF ( FAILED() ) THEN
C
C        If an error occurred while writing the DAS file record,
C        attempt to close the binary file, then check out and return.
C
         CALL DASCLS ( HANDLE  )
         CALL CHKOUT ( 'DASTB' )
         RETURN
 
      END IF
C
C     Read and decode the number of reserved records and reserved
C     characters.
C
      CALL RDENCI ( XFRLUN, 1, NRESVR )
      CALL RDENCI ( XFRLUN, 1, NRESVC )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'DASTB' )
         RETURN
 
      END IF
C
C     Check to be sure that the number of reserved records and the
C     number of reserved characters are not being used. The DAS
C     reserved record area is not currently implemented, so nobody
C     should be using it.
C
      IF ( NRESVC .NE. 0 ) THEN
C
C        Close the file, signal the error, and exit.
C
         CALL DASCLS ( HANDLE                                        )
         CALL SETMSG ( 'The number of reserved characters was'      //
     .                 ' nonzero (#) in file: #, but the DAS'       //
     .                 ' reserved record area has NOT been'         //
     .                 ' implemented yet!'                           )
         CALL ERRINT ( '#', NRESVC                                   )
         CALL ERRFNM ( '#', XFRLUN                                   )
         CALL SIGERR ( 'SPICE(BADDASFILE)'                           )
         CALL CHKOUT ( 'DASTB' )
         RETURN
 
      END IF
 
      IF ( NRESVR .NE. 0 ) THEN
C
C        Close the file, signal the error, and exit.
C
         CALL DASCLS ( HANDLE                                        )
         CALL SETMSG ( 'The number of reserved records was'         //
     .                 ' nonzero (#) in file: #, but the DAS'       //
     .                 ' reserved record area has NOT been'         //
     .                 ' implemented yet!'                           )
         CALL ERRINT ( '#', NRESVR                                   )
         CALL ERRFNM ( '#', XFRLUN                                   )
         CALL SIGERR ( 'SPICE(BADDASFILE)'                           )
         CALL CHKOUT ( 'DASTB' )
         RETURN
 
      END IF
C
C     Read and decode the number of comment records and comment
C     characters.
C
      CALL RDENCI ( XFRLUN, 1, NCOMR )
      CALL RDENCI ( XFRLUN, 1, NCOMC )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'DASTB' )
         RETURN
 
      END IF
C
C     Begin converting the DAS transfer file into an equivalent
C     binary DAS file here.
C
C     The reserved records, if there are any.
C
C     **************************************************************
C     When/if the reserved record area is implemented, the code to
C     read it from the DAS transfer file and convert it to binary
C     should go here. It should be possible to simply copy the code
C     for the comment area, making all of the necessary variable
C     name changes, etc., since the reserved record area is going
C     to contain ONLY character data.
C     **************************************************************
C
 
C
C     The comments, if there are any.
C
      IF ( NCOMC .GT. 0 ) THEN
C
C        We assume that the condition NCOMC > 0 and NCOMR <= 0
C        cannot occur.
C
C        The binary DAS file that we are creating is already open,
C        so just add the comments. But first, convert the DAS file
C        handle into its equivalent logical unit.
C
         CALL DASHLU ( HANDLE, DASLUN )
 
         IF ( FAILED() ) THEN
C
C           If an error occurred, attempt to close the binary file,
C           then check out and return.
C
            CALL DASCLS ( HANDLE  )
            CALL CHKOUT ( 'DASTB' )
            RETURN
 
         END IF
C
C        Allocate the necessary comment records.
C
         CALL DASACR ( HANDLE, NCOMR )
 
         IF ( FAILED() ) THEN
C
C           If an error occurred, attempt to close the binary file,
C           then checkout and return.
C
            CALL DASCLS ( HANDLE  )
            CALL CHKOUT ( 'DASTB' )
            RETURN
 
         END IF
C
C        Initialize a few things: the block counter, the data
C        counter, and the starting record position. The starting
C        record position is one short of the actual first comment
C        record. We will increment the record number before we
C        write anything.
C
         BLKCNT = 0
         DTACNT = 0
         RECNO  = 1 + NRESVR
C
C        We currently have more to process.
C
         MORE  = .TRUE.
C
C        We are currently not processing a comment block.
C
         INBLK = .FALSE.
 
         DO WHILE ( MORE )
 
            CRECRD = ' '
 
            READ (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE
 
            IF ( IOSTAT .NE. 0 ) THEN
C
C              If an error occurred while reading from the DAS transfer
C              file close the binary file, set an appropriate error
C              message, then check out and return.
C
               CALL DASCLS ( HANDLE                              )
               CALL SETMSG ( 'Error reading from the DAS'       //
     .                       ' transfer file #. IOSTAT = #.'     )
               CALL ERRFNM ( '#', XFRLUN                         )
               CALL ERRINT ( '#', IOSTAT                         )
               CALL SIGERR ( 'SPICE(FILEREADFAILED)'             )
               CALL CHKOUT ( 'DASTB'                             )
               RETURN
 
            END IF
C
C           At this point, we should be beginning a comment block,
C           ending a comment block, or scanning for the total number
C           of comment blocks. So look for the appropriate keyword.
C
            CALL NEXTWD ( LINE, WORD, REST )
 
            IF ( WORD .EQ. BEGCOM ) THEN
C
C              Get the comment block index.
C
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, BINDEX, ERRMSG, ERRPTR )
 
               IF ( ERRMSG .NE. ' ' ) THEN
C
C                 If an error occurred while parsing the begin block
C                 index, close the binary file, set an appropriate
C                 error  message, then check out and return.
C
                  CALL DASCLS ( HANDLE                                )
                  CALL SETMSG ( 'Begin comment block error, could'   //
     .                          ' not parse block number. Error: #'  //
     .                          ' File: #'                            )
                  CALL ERRCH  ( '#', ERRMSG                           )
                  CALL ERRFNM ( '#', XFRLUN                           )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'           )
                  CALL CHKOUT ( 'DASTB'                               )
                  RETURN
 
               END IF
C
C              Parse the count of characters in the block.
C
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, BCOUNT, ERRMSG, ERRPTR )
 
               IF ( ERRMSG .NE. ' ' ) THEN
C
C                 If an error occurred while parsing the beginning
C                 data count, close the binary file, set an
C                 appropriate error message, then check out and return.
C
                  CALL DASCLS ( HANDLE                                )
                  CALL SETMSG ( 'Begin comment block error, could'   //
     .                          ' not parse the data count for'      //
     .                          ' block: #. Error: # File: #'         )
                  CALL ERRINT ( '#', BINDEX                           )
                  CALL ERRCH  ( '#', ERRMSG                           )
                  CALL ERRFNM ( '#', XFRLUN                           )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'           )
                  CALL CHKOUT ( 'DASTB'                               )
                  RETURN
 
               END IF
C
C              If we got to here, we are inside a comment block, so set
C              the in block flag, INBLK, to .TRUE. and increment the
C              block counter.
C
               INBLK  = .TRUE.
               BLKCNT = BLKCNT + 1
 
            ELSE IF ( WORD .EQ. ENDCOM ) THEN
C
C              Get the data block index.
C
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, EINDEX, ERRMSG, ERRPTR )
 
               IF ( ERRMSG .NE. ' ' ) THEN
C
C                 If an error occurred while parsing the end comment
C                 block index, close the binary file, set an appropriate
C                 error message, then check out and return.
C
                  CALL DASCLS ( HANDLE                                )
                  CALL SETMSG ( 'End comment block error, could'     //
     .                          ' not parse block number. Error: #'  //
     .                          ' File: #'                            )
                  CALL ERRCH  ( '#', ERRMSG                           )
                  CALL ERRFNM ( '#', XFRLUN                           )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'           )
                  CALL CHKOUT ( 'DASTB'                               )
                  RETURN
 
               END IF
C
C              Parse the count of characters in the DAS array.
C
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, ECOUNT, ERRMSG, ERRPTR )
 
               IF ( ERRMSG .NE. ' ' ) THEN
C
C                 If an error occurred while parsing the ending data
C                 count, close the binary file, set an appropriate
C                 error message, then check out and return.
C
                  CALL DASCLS ( HANDLE                              )
                  CALL SETMSG ( 'End comment block error, could'   //
     .                          ' not parse the data count for '   //
     .                          ' block: #. Error: # File: #'       )
                  CALL ERRINT ( '#', EINDEX                         )
                  CALL ERRCH  ( '#', ERRMSG                         )
                  CALL ERRFNM ( '#', XFRLUN                         )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'         )
                  CALL CHKOUT ( 'DASTB'                             )
                  RETURN
 
               END IF
C
C              Check to see if the beginning and ending array indices
C              match.
C
               IF ( EINDEX .NE. BINDEX ) THEN
C
C                 If the begin and end data block indices do not match,
C                 close the binary file, set an appropriate error
C                 message, then check out and return.
C
                  CALL DASCLS ( HANDLE                              )
                  CALL SETMSG ( 'Comment block index mismatch:'    //
     .                          ' Beginning index: #; Ending'      //
     .                          ' index: #. File: #'                )
                  CALL ERRINT ( '#', BINDEX                         )
                  CALL ERRINT ( '#', EINDEX                         )
                  CALL ERRFNM ( '#', XFRLUN                         )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'         )
                  CALL CHKOUT ( 'DASTB'                             )
                  RETURN
 
               END IF
C
C              Check to see if the beginning and ending comment data
C              counts match.
C
               IF ( ECOUNT .NE. BCOUNT ) THEN
C
C                 If the begin and end data block counts do not match,
C                 close the binary file, set an appropriate error
C                 message, then check out and return.
C
                  CALL DASCLS ( HANDLE                             )
                  CALL SETMSG ( 'Comment block count mismatch:'   //
     .                          ' Beginning count: #;'            //
     .                          ' Ending count: #. File: #'        )
                  CALL ERRINT ( '#', BCOUNT                        )
                  CALL ERRINT ( '#', ECOUNT                        )
                  CALL ERRFNM ( '#', XFRLUN                        )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'        )
                  CALL CHKOUT ( 'DASTB'                            )
                  RETURN
 
               END IF
C
C              If we got to here, we have successfully ended the
C              processing of a comment block, so set the in block
C              flag INBLK, to .FALSE..
C
               INBLK = .FALSE.
 
            ELSE IF ( WORD .EQ. TCMBLK ) THEN
C
C              We have the total comment blocks keyword to parse, so
C              get the total number of comment blocks processed.
C
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, NUMBLK, ERRMSG, ERRPTR )
 
               IF ( ERRMSG .NE. ' ' ) THEN
C
C                 If an error occurred while parsing the total number of
C                 data blocks, close the binary file, set an appropriate
C                 error  message, then check out and return.
C
                  CALL DASCLS ( HANDLE                               )
                  CALL SETMSG ( 'Comment block count error, could'  //
     .                          ' not parse the total number of'    //
     .                          ' character blocks: #. File: #'      )
                  CALL ERRCH  ( '#', ERRMSG                          )
                  CALL ERRFNM ( '#', XFRLUN                          )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'          )
                  CALL CHKOUT ( 'DASTB'                              )
                  RETURN
 
               END IF
C
C              Parse the total count of comment characters.
C
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, TCOUNT, ERRMSG, ERRPTR )
 
               IF ( ERRMSG .NE. ' ' ) THEN
C
C                 If an error occurred while parsing the comment
C                 data count, close the binary file, set an
C                 appropriate error message, then check out and return.
C
                  CALL DASCLS ( HANDLE                           )
                  CALL SETMSG ( 'Comment count error, could'    //
     .                          ' not parse the total count.'   //
     .                          ' Error: # File: #'              )
                  CALL ERRCH  ( '#', ERRMSG                      )
                  CALL ERRFNM ( '#', XFRLUN                      )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'      )
                  CALL CHKOUT ( 'DASTB'                          )
                  RETURN
 
               END IF
C
C              Compare the computed block count with the block count
C              from the file.
C
               IF ( BLKCNT .NE. NUMBLK ) THEN
C
C                 If the computed number of comment blocks and the
C                 number of comment blocks from the text file do
C                 not match, close the binary file, set an appropriate
C                 error message, then check out and return.
C
                  CALL DASCLS ( HANDLE                                )
                  CALL SETMSG ( 'The number of comment data'         //
     .                          ' blocks processed (#) was not'      //
     .                          ' equal to the number of comment'    //
     .                          ' data blocks placed in the DAS'     //
     .                          ' text file (#). File: #'             )
                  CALL ERRINT ( '#', BLKCNT                           )
                  CALL ERRINT ( '#', NUMBLK                           )
                  CALL ERRFNM ( '#', XFRLUN                           )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'           )
                  CALL CHKOUT ( 'DASTB'                               )
                  RETURN
 
               END IF
C
C              Check to see if the total count and the computed count
C              match.
C
               IF ( TCOUNT .NE. DTACNT ) THEN
C
C                 If the total count and computed count do not match,
C                 close the binary file, set an appropriate error
C                 message, then check out and return.
C
                  CALL DASCLS ( HANDLE                             )
                  CALL SETMSG ( 'Comment count mismatch:'         //
     .                          ' computed count: #;'             //
     .                          ' expected count: #. File: #'      )
                  CALL ERRINT ( '#', DTACNT                        )
                  CALL ERRINT ( '#', TCOUNT                        )
                  CALL ERRFNM ( '#', XFRLUN                        )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'        )
                  CALL CHKOUT ( 'DASTB'                            )
                  RETURN
 
               END IF
C
C              If we got to here, we have successfully processed the
C              entire DAS comment area in the text file, so there is
C              no more comment data.
C
               MORE = .FALSE.
 
            ELSE
C
C              We got an unknown keyword of some sort, so set an
C              appropriate error message, close the DAS file, and
C              return.
C
               CALL DASCLS ( HANDLE                                    )
               CALL SETMSG ( 'Unknown keyword ''#'' encountered'      //
     .                       ' while processing the DAS transfer'     //
     .                       ' file #.'                                )
               CALL ERRCH  ( '#', WORD                                 )
               CALL ERRFNM ( '#', XFRLUN                               )
               CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'               )
               CALL CHKOUT ( 'DASTB'                                   )
               RETURN
 
            END IF
C
C           If we have begun a block, then process it. Otherwise, we
C           have ended a block.
C
            IF ( INBLK ) THEN
C
C              Increment the record number by one for each comment
C              data block we process, because each block contains a
C              comment record.
C
               RECNO = RECNO + 1
C
C              Set the count of comment characters yet to be decoded and
C              placed in the binary DAS file.
C
               NUMLFT = BCOUNT
 
               DO WHILE ( NUMLFT .GT. 0 )
C
C                 Now read and decode the data in the current
C                 comment data block, placing the data in the
C                 comment area of the binary DAS file.
C
                  IF ( NUMLFT .GE. CRLEN ) THEN
 
                     NUMDTA = CRLEN
 
                  ELSE
 
                     NUMDTA = NUMLFT
 
                  END IF
C
C                 Read and decode a record of encoded comment data
C                 from the text file.
C
                  CALL RDENCC ( XFRLUN, NUMDTA, CRECRD )
C
C                 Write the comment data to the comment area in the
C                 binary DAS file.
C
                  CALL DASIOC ( 'WRITE', DASLUN, RECNO, CRECRD )
 
                  IF ( FAILED() ) THEN
C
C                    If an error occurred, attempt to close the
C                    binary file, then checkout and return.
C
                     CALL DASCLS ( HANDLE  )
                     CALL CHKOUT ( 'DASTB' )
                     RETURN
 
                  END IF
C
C                 Decrement the counter for the amount of data
C                 remaining to be moved from the current comment
C                 block, NUMLFT.
C
                  NUMLFT = NUMLFT - NUMDTA
C
C                 Increment the counter for the amount of data that
C                 has been successfully moved into the comment area
C                 of the binary DAS file.
C
                  DTACNT = DTACNT + NUMDTA
 
               END DO
C
C              At this point, we have finished reading in an entire
C              comment block.
C
            END IF
C
C           If we got to here, we have successfully written a comment
C           block to the binary file.
C
         END DO
C
C        At this point, we will have successfully written the entire
C        comment area to the binary DAS file, if there was a comment
C        area.
C
C        Write the file record to the DAS file, to update the number
C        of comment characters.
C
         CALL DASWFR ( HANDLE,
     .                 IDWORD, IFNAME,
     .                 NRESVR, NRESVC,
     .                 NCOMR,  NCOMC   )
 
      END IF
C
C     Read the data counts from the DAS transfer file. These will be
C     useful in determining which data types to expect in the text file
C     when converting back to binary.
C
      CALL RDENCI ( XFRLUN, 1, NCDATA )
      CALL RDENCI ( XFRLUN, 1, NDDATA )
      CALL RDENCI ( XFRLUN, 1, NIDATA )
C
C     Process the character data array, if there is some character data.
C
      IF ( NCDATA .GT. 0 ) THEN
C
C        Initialize a few things: the block counter, and the data
C        counter.
C
         BLKCNT = 0
         DTACNT = 0
C
C        We currently have more to process.
C
         MORE  = .TRUE.
C
C        We are currently not processing a data block.
C
         INBLK = .FALSE.
 
         DO WHILE ( MORE )
 
            READ (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE
 
            IF ( IOSTAT .NE. 0 ) THEN
C
C              If an error occurred while reading from the encoded text
C              DAS file close the binary file, set an appropriate error
C              message, then check out and return.
C
               CALL DASCLS ( HANDLE                              )
               CALL SETMSG ( 'Error reading from the DAS'       //
     .                       ' transferfile #. IOSTAT = #.'      )
               CALL ERRFNM ( '#', XFRLUN                         )
               CALL ERRINT ( '#', IOSTAT                         )
               CALL SIGERR ( 'SPICE(FILEREADFAILED)'             )
               CALL CHKOUT ( 'DASTB'                             )
               RETURN
 
            END IF
C
C           At this point, we should be beginning a data block, ending a
C           data block, or scanning for the total number of data blocks.
C           So look for the appropriate keyword.
C
            CALL NEXTWD ( LINE, WORD, REST )
 
            IF ( WORD .EQ. BCBLK ) THEN
C
C              Get the block number.
C
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, BINDEX, ERRMSG, ERRPTR )
 
               IF ( ERRMSG .NE. ' ' ) THEN
C
C                 If an error occurred while parsing the begin block
C                 index, close the binary file, set an appropriate
C                 error  message, then check out and return.
C
                  CALL DASCLS ( HANDLE                              )
                  CALL SETMSG ( 'Begin character block error,'     //
     .                          ' could not parse block number.'   //
     .                          ' Error: # File: #'                 )
                  CALL ERRCH  ( '#', ERRMSG                         )
                  CALL ERRFNM ( '#', XFRLUN                         )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'         )
                  CALL CHKOUT ( 'DASTB'                             )
                  RETURN
 
               END IF
C
C              Parse the count of characters in the block.
C
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, BCOUNT, ERRMSG, ERRPTR )
 
               IF ( ERRMSG .NE. ' ' ) THEN
C
C                 If an error occurred while parsing the beginning
C                 data count, close the binary file, set an
C                 appropriate error message, then check out and return.
C
                  CALL DASCLS ( HANDLE                               )
                  CALL SETMSG ( 'Begin character block error,'      //
     .                          ' could not parse the data count'   //
     .                          ' for block: #. Error: # File: #'    )
                  CALL ERRINT ( '#', BINDEX                          )
                  CALL ERRCH  ( '#', ERRMSG                          )
                  CALL ERRFNM ( '#', XFRLUN                          )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'          )
                  CALL CHKOUT ( 'DASTB'                              )
                  RETURN
 
               END IF
C
C              If we got to here, we are inside a data block, so set
C              the in block flag, INBLK, to .TRUE. and increment the
C              data block counter.
C
               INBLK  = .TRUE.
               BLKCNT = BLKCNT + 1
 
            ELSE IF ( WORD .EQ. ECBLK ) THEN
C
C              Get the data block index.
C
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, EINDEX, ERRMSG, ERRPTR )
 
               IF ( ERRMSG .NE. ' ' ) THEN
C
C                 If an error occurred while parsing the end block
C                 index, close the binary file, set an appropriate
C                 error message, then check out and return.
C
                  CALL DASCLS ( HANDLE                              )
                  CALL SETMSG ( 'End character block error,'       //
     .                          ' could not parse block number.'   //
     .                          ' Error: # File: #'                 )
                  CALL ERRCH  ( '#', ERRMSG                         )
                  CALL ERRFNM ( '#', XFRLUN                         )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'         )
                  CALL CHKOUT ( 'DASTB'                             )
                  RETURN
 
               END IF
C
C              Parse the count of characters in the DAS array.
C
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, ECOUNT, ERRMSG, ERRPTR )
 
               IF ( ERRMSG .NE. ' ' ) THEN
C
C                 If an error occurred while parsing the ending data
C                 count, close the binary file, set an appropriate
C                 error message, then check out and return.
C
                  CALL DASCLS ( HANDLE                              )
                  CALL SETMSG ( 'End character block error, could' //
     .                          ' not parse the data count for'    //
     .                          ' block: #. Error: # File: #'       )
                  CALL ERRINT ( '#', EINDEX                         )
                  CALL ERRCH  ( '#', ERRMSG                         )
                  CALL ERRFNM ( '#', XFRLUN                         )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'         )
                  CALL CHKOUT ( 'DASTB'                             )
                  RETURN
 
               END IF
C
C              Check to see if the beginning and ending array indices
C              match.
C
               IF ( EINDEX .NE. BINDEX ) THEN
C
C                 If the begin and end data block indices do not match,
C                 close the binary file, set an appropriate error
C                 message, then check out and return.
C
                  CALL DASCLS ( HANDLE                              )
                  CALL SETMSG ( 'Character block index mismatch:'  //
     .                          ' Beginning index: #; Ending'      //
     .                          ' index: #. File: #'                )
                  CALL ERRINT ( '#', BINDEX                         )
                  CALL ERRINT ( '#', EINDEX                         )
                  CALL ERRFNM ( '#', XFRLUN                         )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'         )
                  CALL CHKOUT ( 'DASTB'                             )
                  RETURN
 
               END IF
C
C              Check to see if the beginning and ending array data
C              counts match.
C
               IF ( ECOUNT .NE. BCOUNT ) THEN
C
C                 If the begin and end data block counts do not match,
C                 close the binary file, set an appropriate error
C                 message, then check out and return.
C
                  CALL DASCLS ( HANDLE                             )
                  CALL SETMSG ( 'Character block count mismatch:' //
     .                          ' Beginning count: #;'            //
     .                          ' Ending count: #. File: #'        )
                  CALL ERRINT ( '#', BCOUNT                        )
                  CALL ERRINT ( '#', ECOUNT                        )
                  CALL ERRFNM ( '#', XFRLUN                        )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'        )
                  CALL CHKOUT ( 'DASTB'                            )
                  RETURN
 
               END IF
C
C              If we got to here, we have successfully ended the
C              processing of a data block, so set the in block flag,
C              INBLK, to  .FALSE..
C
               INBLK = .FALSE.
 
            ELSE IF ( WORD .EQ. TCBLKS ) THEN
C
C              We have the total data blocks keyword to parse, so get
C              the total number of character data blocks processed.
C
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, NUMBLK, ERRMSG, ERRPTR )
 
               IF ( ERRMSG .NE. ' ' ) THEN
C
C                 If an error occurred while parsing the total number of
C                 data blocks, close the binary file, set an appropriate
C                 error  message, then check out and return.
C
                  CALL DASCLS ( HANDLE                                )
                  CALL SETMSG ( 'Block count error, could not parse' //
     .                          ' the total number of character'     //
     .                          ' blocks: #. File: #'                 )
                  CALL ERRCH  ( '#', ERRMSG                           )
                  CALL ERRFNM ( '#', XFRLUN                           )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'           )
                  CALL CHKOUT ( 'DASTB'                               )
                  RETURN
 
               END IF
C
C              Parse the total count of characters.
C
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, TCOUNT, ERRMSG, ERRPTR )
 
               IF ( ERRMSG .NE. ' ' ) THEN
C
C                 If an error occurred while parsing the character
C                 data count, close the binary file, set an
C                 appropriate error message, then check out and return.
C
                  CALL DASCLS ( HANDLE                           )
                  CALL SETMSG ( 'Character count error, could'  //
     .                          ' not parse the total count.'   //
     .                          ' Error: # File: #'              )
                  CALL ERRCH  ( '#', ERRMSG                      )
                  CALL ERRFNM ( '#', XFRLUN                      )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'      )
                  CALL CHKOUT ( 'DASTB'                          )
                  RETURN
 
               END IF
C
C              Compare the computed block count with the block count
C              from the file.
C
               IF ( BLKCNT .NE. NUMBLK ) THEN
C
C                 If the  calculated data block count and the data
C                 block count from the text file do not match, close
C                 the binary file, set an appropriate error message,
C                 then check out and return.
C
                  CALL DASCLS ( HANDLE                                )
                  CALL SETMSG ( 'The number of character data'       //
     .                          ' blocks processed (#) was not'      //
     .                          ' equal to the number of character'  //
     .                          ' data blocks placed in the DAS'     //
     .                          ' transfer file (#). File: #'         )
                  CALL ERRINT ( '#', BLKCNT                           )
                  CALL ERRINT ( '#', NUMBLK                           )
                  CALL ERRFNM ( '#', XFRLUN                           )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'           )
                  CALL CHKOUT ( 'DASTB'                               )
                  RETURN
 
               END IF
C
C              Check to see if the total count and the computed count
C              match.
C
               IF ( TCOUNT .NE. DTACNT ) THEN
C
C                 If the total count and computed count do not match,
C                 close the binary file, set an appropriate error
C                 message, then check out and return.
C
                  CALL DASCLS ( HANDLE                             )
                  CALL SETMSG ( 'Character count mismatch:'       //
     .                          ' computed count: #;'             //
     .                          ' expected count: #. File: #'      )
                  CALL ERRINT ( '#', DTACNT                        )
                  CALL ERRINT ( '#', TCOUNT                        )
                  CALL ERRFNM ( '#', XFRLUN                        )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'        )
                  CALL CHKOUT ( 'DASTB'                            )
                  RETURN
 
               END IF
C
C              If we got to here, we have successfully processed the
C              entire character data portion of the DAS transfer file,
C              so there is no more character data.
C
               MORE = .FALSE.
 
            ELSE
C
C              We got an unknown keyword of some sort, so set an
C              appropriate error message, close the DAS file, and
C              return.
C
               CALL DASCLS ( HANDLE                                    )
               CALL SETMSG ( 'Unknown keyword ''#'' encountered'      //
     .                       ' while processing the DAS trtansfer'    //
     .                       ' file #.'                                )
               CALL ERRCH  ( '#', WORD                                 )
               CALL ERRFNM ( '#', XFRLUN                               )
               CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'               )
               CALL CHKOUT ( 'DASTB'                                   )
               RETURN
 
            END IF
C
C           If we have begun a block, then process it. Otherwise, we
C           have ended a block.
C
            IF ( INBLK ) THEN
C
C              Read and decode the data in the current DAS character
C              array data block.
C
C              Set the count of characters yet to be decoded and placed
C              in the binary DAS file.
C
               NUMLFT = BCOUNT
 
               DO WHILE ( NUMLFT .GT. 0 )
C
C                 Now read and decode the data in the current
C                 character data block, placing the data in the
C                 character array in the binary DAS file.
C
                  IF ( NUMLFT .GE. ( BUFSIZ * CBFLEN ) ) THEN
 
                     NUMDTA = BUFSIZ * CBFLEN
 
                  ELSE
 
                     NUMDTA = NUMLFT
 
                  END IF
C
C                 Read and decode a buffer of encoded character data
C                 from the text file.
C
                  CALL RDENCC ( XFRLUN, NUMDTA, CBUFFR )
C
C                 Write the character data to the DAS character
C                 array in the binary DAS file.
C
                  CALL DASADC ( HANDLE, NUMDTA,
     .                          BCBPOS, ECBPOS,
     .                          CBUFFR          )
 
                  IF ( FAILED() ) THEN
C
C                    If an error occurred, attempt to close the
C                    binary file, then checkout and return.
C
                     CALL DASCLS ( HANDLE  )
                     CALL CHKOUT ( 'DASTB' )
                     RETURN
 
                  END IF
C
C                 Decrement the counter for the amount of data
C                 remaining to be moved from the current data block,
C                 NUMLFT.
C
                  NUMLFT = NUMLFT - NUMDTA
C
C                 Increment the counter for the amount of data that
C                 has been successfully moved into the current array
C                 in the binary DAS file.
C
                  DTACNT = DTACNT + NUMDTA
C
C                 At this point, we have either finished reading in an
C                 entire data block, or we have more data to read in
C                 the current data block.
C
               END DO
 
            END IF
C
C           If we got to here, we have successfully written a data
C           block to the binary file.
C
         END DO
C
C        At this point, we will have successfully written the entire
C        character data array to the binary DAS file, if there was
C        any character data to be written.
 
      END IF
C
C     Process the double precision data array, if there is some
C     double precision data.
C
      IF ( NDDATA .GT. 0 ) THEN
C
C        Initialize a few things: the block counter, and the data
C        counter.
C
         BLKCNT = 0
         DTACNT = 0
C
C        We currently have more to process.
C
         MORE  = .TRUE.
C
C        We are currently not processing a data block.
C
         INBLK = .FALSE.
 
         DO WHILE ( MORE )
 
            READ (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE
 
            IF ( IOSTAT .NE. 0 ) THEN
C
C              If an error occurred while reading from the encoded text
C              DAS file close the binary file, set an appropriate error
C              message, then check out and return.
C
               CALL DASCLS ( HANDLE                              )
               CALL SETMSG ( 'Error reading from the DAS'       //
     .                       ' transfer file #. IOSTAT = #.'     )
               CALL ERRFNM ( '#', XFRLUN                         )
               CALL ERRINT ( '#', IOSTAT                         )
               CALL SIGERR ( 'SPICE(FILEREADFAILED)'             )
               CALL CHKOUT ( 'DASTB'                             )
               RETURN
 
            END IF
C
C           At this point, we should be beginning a data block, ending a
C           data block, or scanning for the total number of data blocks.
C           So look for the appropriate keyword.
C
            CALL NEXTWD ( LINE, WORD, REST )
 
            IF ( WORD .EQ. BDBLK ) THEN
C
C              Get the block number.
C
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, BINDEX, ERRMSG, ERRPTR )
 
               IF ( ERRMSG .NE. ' ' ) THEN
C
C                 If an error occurred while parsing the begin block
C                 index, close the binary file, set an appropriate
C                 error  message, then check out and return.
C
                  CALL DASCLS ( HANDLE                            )
                  CALL SETMSG ( 'Begin double precision block'   //
     .                          ' error, could not parse block'  //
     .                          ' number. Error: # File: #'       )
                  CALL ERRCH  ( '#', ERRMSG                       )
                  CALL ERRFNM ( '#', XFRLUN                       )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'       )
                  CALL CHKOUT ( 'DASTB'                           )
                  RETURN
 
               END IF
C
C              Parse the count of double precision numbers in the block.
C
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, BCOUNT, ERRMSG, ERRPTR )
 
               IF ( ERRMSG .NE. ' ' ) THEN
C
C                 If an error occurred while parsing the beginning
C                 data count, close the binary file, set an
C                 appropriate error message, then check out and return.
C
                  CALL DASCLS ( HANDLE                            )
                  CALL SETMSG ( 'Begin double precision block'   //
     .                          ' error, could not parse the'    //
     .                          ' data count for block: #.'      //
     .                          ' Error: # File: #'               )
                  CALL ERRINT ( '#', BINDEX                       )
                  CALL ERRCH  ( '#', ERRMSG                       )
                  CALL ERRFNM ( '#', XFRLUN                       )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'       )
                  CALL CHKOUT ( 'DASTB'                           )
                  RETURN
 
               END IF
C
C              If we got to here, we are inside a data block, so set
C              the in block flag, INBLK, to .TRUE. and increment the
C              data block counter.
C
               INBLK  = .TRUE.
               BLKCNT = BLKCNT + 1
 
            ELSE IF ( WORD .EQ. EDBLK ) THEN
C
C              Get the data block index.
C
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, EINDEX, ERRMSG, ERRPTR )
 
               IF ( ERRMSG .NE. ' ' ) THEN
C
C                 If an error occurred while parsing the end block
C                 index, close the binary file, set an appropriate
C                 error message, then check out and return.
C
                  CALL DASCLS ( HANDLE                               )
                  CALL SETMSG ( 'End double precision block'        //
     .                          ' error, could not parse block'     //
     .                          ' number. Error: # File: #'          )
                  CALL ERRCH  ( '#', ERRMSG                          )
                  CALL ERRFNM ( '#', XFRLUN                          )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'          )
                  CALL CHKOUT ( 'DASTB'                              )
                  RETURN
 
               END IF
C
C              Parse the count of double precision numbers in the block.
C
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, ECOUNT, ERRMSG, ERRPTR )
 
               IF ( ERRMSG .NE. ' ' ) THEN
C
C                 If an error occurred while parsing the ending data
C                 count, close the binary file, set an appropriate
C                 error message, then check out and return.
C
                  CALL DASCLS ( HANDLE                           )
                  CALL SETMSG ( 'End double precision block'    //
     .                          ' error, could not parse the'   //
     .                          ' data count for block: #.'     //
     .                          ' Error: # File: #'              )
                  CALL ERRINT ( '#', EINDEX                      )
                  CALL ERRCH  ( '#', ERRMSG                      )
                  CALL ERRFNM ( '#', XFRLUN                      )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'      )
                  CALL CHKOUT ( 'DASTB'                          )
                  RETURN
 
               END IF
C
C              Check to see if the beginning and ending array indices
C              match.
C
               IF ( EINDEX .NE. BINDEX ) THEN
C
C                 If the begin and end data block indices do not match,
C                 close the binary file, set an appropriate error
C                 message, then check out and return.
C
                  CALL DASCLS ( HANDLE                              )
                  CALL SETMSG ( 'Double precision block index'     //
     .                          ' mismatch: Beginning index: #;'   //
     .                          ' Ending index: #. File: #'         )
                  CALL ERRINT ( '#', BINDEX                         )
                  CALL ERRINT ( '#', EINDEX                         )
                  CALL ERRFNM ( '#', XFRLUN                         )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'         )
                  CALL CHKOUT ( 'DASTB'                             )
                  RETURN
 
               END IF
C
C              Check to see if the beginning and ending array data
C              counts match.
C
               IF ( ECOUNT .NE. BCOUNT ) THEN
C
C                 If the begin and end data block counts do not match,
C                 close the binary file, set an appropriate error
C                 message, then check out and return.
C
                  CALL DASCLS ( HANDLE                             )
                  CALL SETMSG ( 'Double precision block count'    //
     .                          ' mismatch: Beginning count: #;'  //
     .                          ' Ending count: #. File: #'        )
                  CALL ERRINT ( '#', BCOUNT                        )
                  CALL ERRINT ( '#', ECOUNT                        )
                  CALL ERRFNM ( '#', XFRLUN                        )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'        )
                  CALL CHKOUT ( 'DASTB'                            )
                  RETURN
 
               END IF
C
C              If we got to here, we have successfully ended the
C              processing of a data block, so set the in block flag,
C              INBLK, to  .FALSE..
C
               INBLK = .FALSE.
 
            ELSE IF ( WORD .EQ. TDBLKS ) THEN
C
C              We have the total data blocks keyword to parse, so get
C              the total number of character data blocks processed.
C
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, NUMBLK, ERRMSG, ERRPTR )
 
               IF ( ERRMSG .NE. ' ' ) THEN
C
C                 If an error occurred while parsing the total number of
C                 data blocks, close the binary file, set an appropriate
C                 error  message, then check out and return.
C
                  CALL DASCLS ( HANDLE                                )
                  CALL SETMSG ( 'Block count error, could not parse' //
     .                          ' the total number of double'        //
     .                          ' precision data blocks: #. File: #'  )
                  CALL ERRCH  ( '#', ERRMSG                           )
                  CALL ERRFNM ( '#', XFRLUN                           )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'           )
                  CALL CHKOUT ( 'DASTB'                               )
                  RETURN
 
               END IF
C
C              Parse the total count of double precision numbers.
C
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, TCOUNT, ERRMSG, ERRPTR )
 
               IF ( ERRMSG .NE. ' ' ) THEN
C
C                 If an error occurred while parsing the double
C                 precision data count, close the binary file, set an
C                 appropriate error message, then check out and return.
C
                  CALL DASCLS ( HANDLE                           )
                  CALL SETMSG ( 'Double precision count error,' //
     .                          ' could not parse the total'    //
     .                          ' count. Error: # File: #'       )
                  CALL ERRCH  ( '#', ERRMSG                      )
                  CALL ERRFNM ( '#', XFRLUN                      )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'      )
                  CALL CHKOUT ( 'DASTB'                          )
                  RETURN
 
               END IF
C
C              Compare the computed block count with the block count
C              from the file.
C
               IF ( BLKCNT .NE. NUMBLK ) THEN
C
C                 If the  calculated data block count and the data
C                 block count from the text file do not match, close
C                 the binary file, set an appropriate error message,
C                 then check out and return.
C
                  CALL DASCLS ( HANDLE                                )
                  CALL SETMSG ( 'The number of double precision'     //
     .                          ' data blocks processed (#) was'     //
     .                          ' not equal to the number of'        //
     .                          ' double precision data blocks'      //
     .                          ' placed in the DAS transfer file'   //
     .                          ' (#). File: #'                       )
                  CALL ERRINT ( '#', BLKCNT                           )
                  CALL ERRINT ( '#', NUMBLK                           )
                  CALL ERRFNM ( '#', XFRLUN                           )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'           )
                  CALL CHKOUT ( 'DASTB'                               )
                  RETURN
 
               END IF
C
C              Check to see if the total count and the computed count
C              match.
C
               IF ( TCOUNT .NE. DTACNT ) THEN
C
C                 If the total count and computed count do not match,
C                 close the binary file, set an appropriate error
C                 message, then check out and return.
C
                  CALL DASCLS ( HANDLE                               )
                  CALL SETMSG ( 'Double precision count mismatch:'  //
     .                          ' computed count: #;'               //
     .                          ' expected count: #. File: #'        )
                  CALL ERRINT ( '#', DTACNT                          )
                  CALL ERRINT ( '#', TCOUNT                          )
                  CALL ERRFNM ( '#', XFRLUN                          )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'          )
                  CALL CHKOUT ( 'DASTB'                              )
                  RETURN
 
               END IF
C
C              If we got to here, we have successfully processed the
C              entire DAS double precision data portion of the text
C              file, so there is no more double precision data.
C
               MORE = .FALSE.
 
            ELSE
C
C              We got an unknown keyword of some sort, so set an
C              appropriate error message, close the DAS file, and
C              return.
C
               CALL DASCLS ( HANDLE                                    )
               CALL SETMSG ( 'Unknown keyword ''#'' encountered'      //
     .                       ' while processing the DAS transfer'     //
     .                       '  file #.'                               )
               CALL ERRCH  ( '#', WORD                                 )
               CALL ERRFNM ( '#', XFRLUN                               )
               CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'               )
               CALL CHKOUT ( 'DASTB'                                   )
               RETURN
 
            END IF
C
C           If we have begun a block, then process it. Otherwise, we
C           have ended a block.
 
            IF ( INBLK ) THEN
C
C              Read and decode the data in the current DAS double
C              precision array data block.
C
C              Set the count of double precision numbers yet to be
C              decoded and placed in the binary DAS file.
C
               NUMLFT = BCOUNT
 
               DO WHILE ( NUMLFT .GT. 0 )
C
C                 Now read and decode the data in the current double
C                 precision data block, placing the data in the double
C                 precision array in the binary DAS file.
C
                  IF ( NUMLFT .GE. BUFSIZ ) THEN
 
                     NUMDTA = BUFSIZ
 
                  ELSE
 
                     NUMDTA = NUMLFT
 
                  END IF
C
C                 Read and decode a buffer of encoded double precision
C                 data from the text file.
C
                  CALL RDENCD ( XFRLUN, NUMDTA, DBUFFR )
C
C                 Write the double precision data to the DAS double
C                 precision array in the binary DAS file.
C
                  CALL DASADD ( HANDLE, NUMDTA, DBUFFR )
 
                  IF ( FAILED() ) THEN
C
C                    If an error occurred, attempt to close the
C                    binary file, then checkout and return.
C
                     CALL DASCLS ( HANDLE  )
                     CALL CHKOUT ( 'DASTB' )
                     RETURN
 
                  END IF
C
C                 Decrement the counter for the amount of data
C                 remaining to be moved from the current data block,
C                 NUMLFT.
C
                  NUMLFT = NUMLFT - NUMDTA
C
C                 Increment the counter for the amount of data that
C                 has been successfully moved into the current array
C                 in the binary DAS file.
C
                  DTACNT = DTACNT + NUMDTA
C
C                 At this point, we have either finished reading in an
C                 entire data block, or there is still some data
C                 remaining to be read.
C
               END DO
 
            END IF
C
C           If we got to here, we have successfully written a data
C           block to the binary file.
C
         END DO
C
C        At this point, we will have successfully written the entire
C        double precision data array to the binary DAS file, if there
C        was any double precision data to be written.
 
      END IF
C
C     Process the integer data array, if there is some integer data.
C
      IF ( NIDATA .GT. 0 ) THEN
C
C        Initialize a few things: the block counter, and the data
C        counter.
C
         BLKCNT = 0
         DTACNT = 0
C
C        We currently have more to process.
C
         MORE  = .TRUE.
C
C        We are currently not processing a data block.
C
         INBLK = .FALSE.
 
         DO WHILE ( MORE )
 
            READ (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE
 
            IF ( IOSTAT .NE. 0 ) THEN
C
C              If an error occurred while reading from the encoded text
C              DAS file close the binary file, set an appropriate error
C              message, then check out and return.
C
               CALL DASCLS ( HANDLE                              )
               CALL SETMSG ( 'Error reading from the DAS'       //
     .                       ' transfer file #. IOSTAT = #.'     )
               CALL ERRFNM ( '#', XFRLUN                         )
               CALL ERRINT ( '#', IOSTAT                         )
               CALL SIGERR ( 'SPICE(FILEREADFAILED)'             )
               CALL CHKOUT ( 'DASTB'                             )
               RETURN
 
            END IF
C
C           At this point, we should be beginning a data block, ending a
C           data block, or scanning for the total number of data blocks.
C           So look for the appropriate keyword.
C
            CALL NEXTWD ( LINE, WORD, REST )
 
            IF ( WORD .EQ. BIBLK ) THEN
C
C              Get the block number.
C
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, BINDEX, ERRMSG, ERRPTR )
 
               IF ( ERRMSG .NE. ' ' ) THEN
C
C                 If an error occurred while parsing the begin block
C                 index, close the binary file, set an appropriate
C                 error  message, then check out and return.
C
                  CALL DASCLS ( HANDLE                                )
                  CALL SETMSG ( 'Begin integer block error, could'   //
     .                          ' not parse block number. Error: #'  //
     .                          ' File: #'                            )
                  CALL ERRCH  ( '#', ERRMSG                           )
                  CALL ERRFNM ( '#', XFRLUN                           )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'           )
                  CALL CHKOUT ( 'DASTB'                               )
                  RETURN
 
               END IF
C
C              Parse the count of integers in the block.
C
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, BCOUNT, ERRMSG, ERRPTR )
 
               IF ( ERRMSG .NE. ' ' ) THEN
C
C                 If an error occurred while parsing the beginning
C                 data count, close the binary file, set an
C                 appropriate error message, then check out and return.
C
                  CALL DASCLS ( HANDLE                                )
                  CALL SETMSG ( 'Begin integer block error, could'   //
     .                          ' not parse the data count for '     //
     .                          ' block: #. Error: # File: #'         )
                  CALL ERRINT ( '#', BINDEX                           )
                  CALL ERRCH  ( '#', ERRMSG                           )
                  CALL ERRFNM ( '#', XFRLUN                           )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'           )
                  CALL CHKOUT ( 'DASTB'                               )
                  RETURN
 
               END IF
C
C              If we got to here, we are inside a data block, so set
C              the in block flag, INBLK, to .TRUE. and increment the
C              data block counter.
C
               INBLK  = .TRUE.
               BLKCNT = BLKCNT + 1
 
            ELSE IF ( WORD .EQ. EIBLK ) THEN
C
C              Get the data block index.
C
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, EINDEX, ERRMSG, ERRPTR )
 
               IF ( ERRMSG .NE. ' ' ) THEN
C
C                 If an error occurred while parsing the end block
C                 index, close the binary file, set an appropriate
C                 error message, then check out and return.
C
                  CALL DASCLS ( HANDLE                              )
                  CALL SETMSG ( 'End integer block error, could'   //
     .                          ' not parse block number.'         //
     .                          ' Error: # File: #'                 )
                  CALL ERRCH  ( '#', ERRMSG                         )
                  CALL ERRFNM ( '#', XFRLUN                         )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'         )
                  CALL CHKOUT ( 'DASTB'                             )
                  RETURN
 
               END IF
C
C              Parse the count of integers in the block.
C
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, ECOUNT, ERRMSG, ERRPTR )
 
               IF ( ERRMSG .NE. ' ' ) THEN
C
C                 If an error occurred while parsing the ending data
C                 count, close the binary file, set an appropriate
C                 error message, then check out and return.
C
                  CALL DASCLS ( HANDLE                              )
                  CALL SETMSG ( 'End integer block error, could'   //
     .                          ' not parse the data count for'    //
     .                          ' block: #.Error: # File: #'        )
                  CALL ERRINT ( '#', EINDEX                         )
                  CALL ERRCH  ( '#', ERRMSG                         )
                  CALL ERRFNM ( '#', XFRLUN                         )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'         )
                  CALL CHKOUT ( 'DASTB'                             )
                  RETURN
 
               END IF
C
C              Check to see if the beginning and ending array indices
C              match.
C
               IF ( EINDEX .NE. BINDEX ) THEN
C
C                 If the begin and end data block indices do not match,
C                 close the binary file, set an appropriate error
C                 message, then check out and return.
C
                  CALL DASCLS ( HANDLE                             )
                  CALL SETMSG ( 'Integer block index mismatch:'   //
     .                          ' Beginning index: #; Ending'     //
     .                          ' index: #. File: #'               )
                  CALL ERRINT ( '#', BINDEX                        )
                  CALL ERRINT ( '#', EINDEX                        )
                  CALL ERRFNM ( '#', XFRLUN                        )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'        )
                  CALL CHKOUT ( 'DASTB'                            )
                  RETURN
 
               END IF
C
C              Check to see if the beginning and ending array data
C              counts match.
C
               IF ( ECOUNT .NE. BCOUNT ) THEN
C
C                 If the begin and end data block counts do not match,
C                 close the binary file, set an appropriate error
C                 message, then check out and return.
C
                  CALL DASCLS ( HANDLE                             )
                  CALL SETMSG ( 'Integer block count mismatch:'   //
     .                          ' Beginning count: #;'            //
     .                          ' Ending count: #. File: #'        )
                  CALL ERRINT ( '#', BCOUNT                        )
                  CALL ERRINT ( '#', ECOUNT                        )
                  CALL ERRFNM ( '#', XFRLUN                        )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'        )
                  CALL CHKOUT ( 'DASTB'                            )
                  RETURN
 
               END IF
C
C              If we got to here, we have successfully ended the
C              processing of a data block, so set the in block flag,
C              INBLK, to .FALSE..
C
               INBLK = .FALSE.
 
            ELSE IF ( WORD .EQ. TIBLKS ) THEN
C
C              We have the total data blocks keyword to parse, so get
C              the total number of character data blocks processed.
C
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, NUMBLK, ERRMSG, ERRPTR )
 
               IF ( ERRMSG .NE. ' ' ) THEN
C
C                 If an error occurred while parsing the total number of
C                 data blocks, close the binary file, set an appropriate
C                 error  message, then check out and return.
C
                  CALL DASCLS ( HANDLE                                )
                  CALL SETMSG ( 'Block count error, could not'       //
     .                          ' parse the total number of'         //
     .                          ' integer data blocks: #. File: #'    )
                  CALL ERRCH  ( '#', ERRMSG                           )
                  CALL ERRFNM ( '#', XFRLUN                           )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'           )
                  CALL CHKOUT ( 'DASTB'                               )
                  RETURN
 
               END IF
C
C              Parse the total count of integers.
C
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, TCOUNT, ERRMSG, ERRPTR )
 
               IF ( ERRMSG .NE. ' ' ) THEN
C
C                 If an error occurred while parsing the integer
C                 data count, close the binary file, set an
C                 appropriate error message, then check out and return.
C
                  CALL DASCLS ( HANDLE                           )
                  CALL SETMSG ( 'Integer count error, could'    //
     .                          ' not parse the total count.'   //
     .                          ' Error: # File: #'              )
                  CALL ERRCH  ( '#', ERRMSG                      )
                  CALL ERRFNM ( '#', XFRLUN                      )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'      )
                  CALL CHKOUT ( 'DASTB'                          )
                  RETURN
 
               END IF
C
C              Compare the computed block count with the block count
C              from the file.
C
               IF ( BLKCNT .NE. NUMBLK ) THEN
C
C                 If the  calculated data block count and the data
C                 block count from the text file do not match, close
C                 the binary file, set an appropriate error message,
C                 then check out and return.
C
                  CALL DASCLS ( HANDLE                               )
                  CALL SETMSG ( 'The number of integer data'        //
     .                          ' blocks processed (#) was not'     //
     .                          ' equal to the number of integer'   //
     .                          ' data blocks placed in the DAS'    //
     .                          ' transfer file (#). File: #'        )
                  CALL ERRINT ( '#', BLKCNT                          )
                  CALL ERRINT ( '#', NUMBLK                          )
                  CALL ERRFNM ( '#', XFRLUN                          )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'          )
                  CALL CHKOUT ( 'DASTB'                              )
                  RETURN
 
               END IF
C
C              Check to see if the total count and the computed count
C              match.
C
               IF ( TCOUNT .NE. DTACNT ) THEN
C
C                 If the total count and computed count do not match,
C                 close the binary file, set an appropriate error
C                 message, then check out and return.
C
                  CALL DASCLS ( HANDLE                             )
                  CALL SETMSG ( 'Integer count mismatch:'         //
     .                          ' computed count: #;'             //
     .                          ' expected count: #. File: #'      )
                  CALL ERRINT ( '#', DTACNT                        )
                  CALL ERRINT ( '#', TCOUNT                        )
                  CALL ERRFNM ( '#', XFRLUN                        )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'        )
                  CALL CHKOUT ( 'DASTB'                            )
                  RETURN
 
               END IF
C
C              If we got to here, we have successfully processed the
C              entire DAS integer data portion of the text file, so
C              there is no more integer data.
C
               MORE = .FALSE.
 
            ELSE
C
C              We got an unknown keyword of some sort, so set an
C              appropriate error message, close the DAS file, and
C              return.
C
               CALL DASCLS ( HANDLE                                    )
               CALL SETMSG ( 'Unknown keyword ''#'' encountered'      //
     .                       ' while processing the DAS transfer'     //
     .                       '  file #.'                               )
               CALL ERRCH  ( '#', WORD                                 )
               CALL ERRFNM ( '#', XFRLUN                               )
               CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'               )
               CALL CHKOUT ( 'DASTB'                                   )
               RETURN
 
            END IF
C
C           If we have begun a block, then process it. Otherwise, we
C           have ended a block.
 
            IF ( INBLK ) THEN
C
C              Read and decode the data in the current DAS integer
C              array data block.
C
C              Set the count of integers yet to be decoded and placed
C              in the binary DAS file.
C
               NUMLFT = BCOUNT
 
               DO WHILE ( NUMLFT .GT. 0 )
C
C                 Now read and decode the data in the current
C                 integer data block, placing the data in the
C                 integer precision array in the binary DAS file.
C
                  IF ( NUMLFT .GE. BUFSIZ ) THEN
 
                     NUMDTA = BUFSIZ
 
                  ELSE
 
                     NUMDTA = NUMLFT
 
                  END IF
C
C                 Read and decode a buffer of encoded integer data
C                 from the text file.
C
                  CALL RDENCI ( XFRLUN, NUMDTA, IBUFFR )
C
C                 Write the integer data to the DAS integer array in
C                 the binary DAS file.
C
                  CALL DASADI ( HANDLE, NUMDTA, IBUFFR )
 
                  IF ( FAILED() ) THEN
C
C                    If an error occurred, attempt to close the
C                    binary file, then checkout and return.
C
                     CALL DASCLS ( HANDLE  )
                     CALL CHKOUT ( 'DASTB' )
                     RETURN
 
                  END IF
C
C                 Decrement the counter for the amount of data
C                 remaining to be moved from the current data block,
C                 NUMLFT.
C
                  NUMLFT = NUMLFT - NUMDTA
C
C                 Increment the counter for the amount of data that
C                 has been successfully moved into the current array
C                 in the binary DAS file.
C
                  DTACNT = DTACNT + NUMDTA
C
C                 At this point, we have either finished reading in an
C                 entire data block, or there is still data remaining
C                 to be read.
C
               END DO
 
            END IF
C
C           If we got to here, we have successfully written a data
C           block to the binary file.
C
         END DO
C
C        At this point, we will have successfully written the entire
C        integer data array to the binary DAS file, if there was any
C        integer data to be written.
 
      END IF
C
C     Close only the binary file.
C
      CALL DASCLS ( HANDLE )
 
      CALL CHKOUT ( 'DASTB' )
      RETURN
      END
