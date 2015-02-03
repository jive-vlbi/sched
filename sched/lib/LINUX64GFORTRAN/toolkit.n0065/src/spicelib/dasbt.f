 
C$Procedure DASBT ( DAS, convert binary file to transfer file )
 
      SUBROUTINE DASBT ( BINFIL, XFRLUN )
 
C$ Abstract
C
C     Convert the contents of a binary DAS file to an equivalent DAS
C     transfer file.
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
 
      CHARACTER*(*)         BINFIL
      INTEGER               XFRLUN
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     BINFIL     I   Name of the binary DAS file to be converted.
C     XFRLUN     I   Logical unit of a previously opened file.
C
C$ Detailed_Input
C
C     BINFIL   The name of a binary DAS file which is to be converted
C              to an equivalent DAS transfer file.
C
C     XFRLUN   The Fortran logical unit number of a previously opened
C              file. The DAS transfer file will be written to the
C              file attached to this logical unit beginning at the
C              current position in the file.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C     See arguments BINFIL, XFRLUN.
C
C$ Exceptions
C
C     1)   If the binary DAS file specified by the filename BINFIL
C          cannot be opened for read access, an appropriate error
C          message will be signalled by a DAS file access routine that
C          is called by this routine.
C
C     2)   If for some reason the DAS transfer file cannot be written
C          to, the error SPICE(FILEWRITEFAILED) is signalled.
C
C     3)   If, for any reason, the DAS file cannot be read, a DAS file
C          access routine will signal an error with appropriate error
C          message.
C
C     4)   The binary DAS file opened by this routine, BINFIL, is only
C          GUARANTEED to be closed upon successful completion of the
C          binary to transfer conversion process. In the event of an
C          error, the caller of this routine is required to close the
C          binary DAS file BINFIL.
C
C     5)   If the values for the number of reserved records or the
C          number of reserved characters in a DAS file is nonzero,
C          the error SPICE(BADDASFILE) will be signalled. THIS ERROR
C          IS SIGNALLED ONLY BECAUSE THE RESERVED RECORD AREA HAS
C          NOT YET BEEN IMPLEMENTED.
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
C     This routine provides a mechanism for converting a binary DAS
C     file into an equivalent DAS transfer file. It is one of a pair of
C     routines for performing conversions between the binary format of a
C     DAS file and the DAS transfer file. The inverse of this routine is
C     the routine DASTB.
C
C     Upon successful completion, the DAS transfer file attached to
C     Fortran logical unit XFRLUN will contain the same data as the
C     binary DAS file BINFIL in an encoded ASCII format. The binary DAS
C     file BINFIL will be closed when this routine exits successfully.
C     The DAS transfer file will remain open, as it was on entry, and it
C     will be positioned to write on the first line following the
C     encoded data from the binary DAS file.
C
C$ Examples
C
C     Let
C
C        BINFIL   be the name of a binary DAS file which is to be
C                 converted to an equivalent DAS transfer file. This
C                 could be for purposes of porting the data to a
C                 different computer platform, or possibly for
C                 archival storage of the data.
C
C        XFRLUN   be the Fortran logical unit to which the DAS transfer
C                 file is to be written.
C
C     Then, the following subroutine call would read the binary DAS
C     file BINFIL, convert its contents into an encoded format, and
C     then write that data to the DAS transfer file attached to XFRLUN,
C     beginning at the current position in that file.
C
C        CALL DASBT ( BINFIL, XFRLUN )
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
C     K.R. Gehringer  (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.0.0, 13-AUG-1994 (KRG)
C
C        Updated the header and in line comments to reflect the change
C        from calling files text files to calling them transfer files.
C
C        Changed the variable name TXTLUN to XFRLUN to make it
C        compatible with the change in terminology.
C
C-    SPICELIB Version 2.0.0, 13-AUG-1994 (KRG)
C
C        A potential problem with list directed writes was fixed. Some
C        compilers have list directed writes that write multiple comma
C        separated items to one line and other compilers write these to
C        multiple lines even when all of the output will fit on a single
C        line. This was fixed by replacing all of the affected list
C        directed write statements with code to put the desired data
C        into a character string and then write the character string.
C
C-    SPICELIB Version 1.0.0, 29-OCT-1992 (KRG)
C
C-&
 
C$ Index_Entries
C
C     convert binary das to das transfer file
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 3.0.0, 13-AUG-1994 (KRG)
C
C        Updated the header and in line comments to reflect the change
C        from calling files text files to calling them transfer files.
C
C        Changed the variable name TXTLUN to XFRLUN to make it
C        compatible with the change in terminology.
C
C-    SPICELIB Version 2.0.0, 13-AUG-1994 (KRG)
C
C        A potential problem with list directed writes was fixed. Some
C        compilers have list directed writes that write multiple comma
C        separated items to one line and other compilers write these to
C        multiple lines even when all of the output will fit on a single
C        line. This was fixed by replacing all of the affected list
C        directed write statements with code to put the desired data
C        into a character string and then write the character string.
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               RTRIM
 
      LOGICAL               RETURN
      LOGICAL               FAILED
C
C     Local parameters
C
      CHARACTER*(*)         QUOTE
      PARAMETER           ( QUOTE  = ''''                         )
C
C      CHARACTER*(*)         BEGRES
C      PARAMETER           ( BEGRES = 'BEGIN_RESERVED_BLOCK'      )
C
C      CHARACTER*(*)         ENDRES
C      PARAMETER           ( ENDRES = 'END_RESERVED_BLOCK'        )
C
C      CHARACTER*(*)         TRRBLK
C      PARAMETER           ( TRRBLK = 'TOTAL_RESERVED_BLOCKS'     )
C
      CHARACTER*(*)         BEGCOM
      PARAMETER           ( BEGCOM = 'BEGIN_COMMENT_BLOCK'        )
 
      CHARACTER*(*)         ENDCOM
      PARAMETER           ( ENDCOM = 'END_COMMENT_BLOCK'          )
 
      CHARACTER*(*)         TCMBLK
      PARAMETER           ( TCMBLK = 'TOTAL_COMMENT_BLOCKS'       )
 
      CHARACTER*(*)         BCBLK
      PARAMETER           ( BCBLK  = 'BEGIN_CHARACTER_BLOCK'      )
 
      CHARACTER*(*)         ECBLK
      PARAMETER           ( ECBLK  = 'END_CHARACTER_BLOCK'        )
 
      CHARACTER*(*)         TCBLKS
      PARAMETER           ( TCBLKS = 'TOTAL_CHARACTER_BLOCKS'     )
 
      CHARACTER*(*)         BDBLK
      PARAMETER           ( BDBLK  = 'BEGIN_DP_BLOCK'             )
 
      CHARACTER*(*)         EDBLK
      PARAMETER           ( EDBLK  = 'END_DP_BLOCK'               )
 
      CHARACTER*(*)         TDBLKS
      PARAMETER           ( TDBLKS = 'TOTAL_DP_BLOCKS'            )
 
      CHARACTER*(*)         BIBLK
      PARAMETER           ( BIBLK  = 'BEGIN_INTEGER_BLOCK'        )
 
      CHARACTER*(*)         EIBLK
      PARAMETER           ( EIBLK  = 'END_INTEGER_BLOCK'          )
 
      CHARACTER*(*)         TIBLKS
      PARAMETER           ( TIBLKS = 'TOTAL_INTEGER_BLOCKS'       )
 
      CHARACTER*(*)         FTYPID
      PARAMETER           ( FTYPID = 'DASETF'                     )
 
      CHARACTER*(*)         INFOLN
      PARAMETER           ( INFOLN = 'NAIF DAS ENCODED TRANSFER FILE' )
C
C     Some parameters for writing the array markers
C
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 80 )
C
C     Length of a character buffer array element.
C
      INTEGER               CBFLEN
      PARAMETER           ( CBFLEN = 4                            )
C
C     Length of a DAS file ID word.
C
      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN = 8                            )
C
C     Length of a DAS internal filename.
C
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60                           )
C
C     Length of a DAS comment record, in characters.
C
      INTEGER               CRLEN
      PARAMETER           ( CRLEN = 1024                          )
C
C     Size of the character, double precision, and integer data buffers.
C
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 1024                         )
C
C     Beginning and ending string positions for reading/writing
C     character data from/to a DAS file using the character data
C     buffer.
C
      INTEGER               BCBPOS
      PARAMETER           ( BCBPOS = 1                            )
 
      INTEGER               ECBPOS
      PARAMETER           ( ECBPOS = CBFLEN                       )
C
C     Local variables
C
      CHARACTER*(CBFLEN)    CBUFFR(BUFSIZ)
      CHARACTER*(CRLEN)     CRECRD
      CHARACTER*(IDWLEN)    IDWORD
      CHARACTER*(IFNLEN)    IFNAME
      CHARACTER*(LINLEN)    LINE
 
      DOUBLE PRECISION      DBUFFR(BUFSIZ)
 
      INTEGER               DASLUN
      INTEGER               DTABEG
      INTEGER               HANDLE
      INTEGER               IBUFFR(BUFSIZ)
      INTEGER               IOSTAT
      INTEGER               NCDATA
      INTEGER               NDDATA
      INTEGER               NIDATA
      INTEGER               NRESVR
      INTEGER               NRESVC
      INTEGER               NCOMR
      INTEGER               NCOMC
      INTEGER               NUMBLK
      INTEGER               NUMDTA
      INTEGER               NUMLFT
      INTEGER               RECNO
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASBT' )
      END IF
C
C     When converting a binary DAS file into its DAS transfer file
C     equivalent, all of the data contained in the binary file is
C     placed into the DAS transfer file by this routine. This includes
C     the reserved record area, the comment area, and the character,
C     double precision, and integer data arrays as well.
C
C     Currently, the reserved record area has not been implemented, as
C     there is no need for it at this time. If, or when, the reserved
C     record area is implemented, this routine will need to be modified
C     in order to support it. See the code for details.
C
C     The data from the binary file are written to the DAS transfer
C     file as sequences of small blocks of data. This is to provide
C     a means for performing some error detection when converting a
C     DAS transfer file into its binary equivalent. Each block of
C     data is enclosed within begin and end block markers which hold
C     the count of data items in a data block. When all of the data
C     blocks for a data area have been written, a total blocks line is
C     written to the DAS transfer file.
C
C     The data from the binary DAS file MUST appear in the following
C     order in the DAS transfer file.
C
C           1) Reserved records (when/if implemented)
C           2) Comment area
C           3) Character data array
C           4) Double precision data array
C           5) Integer data array
C
C     If the data count for any of these DAS data areas is zero, no
C     data or markers for it are placed into the DAS transfer file.
C     Conversion proceeds with the next DAS data area in the list.
C
C     For example, suppose that we have a binary DAS file where there
C     are 0 reserved characters in the reserved record area, 5000
C     comment characters in the comment area, and that the character,
C     double precision, and integer array counts are 0, 2300, and
C     6900, respectively. Then, the DAS transfer file will contain
C     no reserved record data blocks, 2 comment data blocks, no
C     character data blocks, 3 double precision data blocks, and 7
C     integer data blocks, in that order.
C
C     DAS transfer file description.
C     ----------------------------------
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
C     We begin by opening the binary DAS file specified by BINFIL for
C     read access, obtaining a file handle.
C
      CALL DASOPR( BINFIL, HANDLE )
 
      IF ( FAILED() ) THEN
C
C        If an error occurred while opening the file check out and
C        return to the caller.
C
         CALL CHKOUT ( 'DASBT' )
         RETURN
 
      END IF
C
C     Get the contents of the DAS file record.
C
      CALL DASRFR ( HANDLE,
     .              IDWORD, IFNAME,
     .              NRESVR, NRESVC,
     .              NCOMR,  NCOMC   )
C
C     Convert the DAS file handle into its equivalent Fortran logical
C     unit. We need the logical unit so that we can read the reserved
C     records and the comment records.
C
      CALL DASHLU ( HANDLE, DASLUN )
 
      IF ( FAILED() ) THEN
C
C        If an error occurred while converting the DAS file handle to
C        a logical unit, attempt to close the binary file, then check
C        out and return.
C
         CALL DASCLS ( HANDLE  )
         CALL CHKOUT ( 'DASBT' )
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
C        Set the error message, close the file, signal the error, and
C        exit.
C
         CALL SETMSG ( 'The number of reserved characters was'      //
     .                 ' nonzero (#) in file: #, but the DAS'       //
     .                 ' reserved record area has NOT been'         //
     .                 ' implemented yet!'                           )
         CALL ERRINT ( '#', NRESVC                                   )
         CALL ERRFNM ( '#', DASLUN                                   )
         CALL DASCLS ( HANDLE                                        )
         CALL SIGERR ( 'SPICE(BADDASFILE)'                           )
         CALL CHKOUT ( 'DASBT'                                       )
         RETURN
 
      END IF
 
      IF ( NRESVR .NE. 0 ) THEN
C
C        Set the error message, close the file, signal the error, and
C        exit.
C
         CALL SETMSG ( 'The number of reserved records was'         //
     .                 ' nonzero (#) in file: #, but the DAS'       //
     .                 ' reserved record area has NOT been'         //
     .                 ' implemented yet!'                           )
         CALL ERRINT ( '#', NRESVR                                   )
         CALL ERRFNM ( '#', DASLUN                                   )
         CALL DASCLS ( HANDLE                                        )
         CALL SIGERR ( 'SPICE(BADDASFILE)'                           )
         CALL CHKOUT ( 'DASBT'                                       )
         RETURN
 
      END IF
C
C     Write the information line containing the file type information
C     and format version for the DAS transfer to the current position in
C     the file. The file format version information must be the first
C     ``word'' on the information line. The rest of the line may be used
C     for other purposes. Right now, it simply contains an expanded
C     description of the file format version information ``word.''
C
      WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) FTYPID // ' ' // INFOLN
 
      IF ( IOSTAT .NE. 0 ) THEN
C
C        An error occurred, so close the binary DAS file, set an
C        appropriate error message, and return to the caller.
C
         CALL DASCLS ( HANDLE                                 )
         CALL SETMSG ( 'Error writing to the DAS transfer'   //
     .                 ' file: #. IOSTAT = #.'                )
         CALL ERRFNM ( '#', XFRLUN                            )
         CALL ERRINT ( '#', IOSTAT                            )
         CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
         CALL CHKOUT ( 'DASBT'                                )
         RETURN
 
       END IF
C
C     Write the DAS ID word to the DAS transfer file.
C
      WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) QUOTE // IDWORD // QUOTE
 
      IF ( IOSTAT .NE. 0 ) THEN
C
C        An error occurred, so close the binary DAS file, set an
C        appropriate error message, and return to the caller.
C
         CALL DASCLS ( HANDLE                                 )
         CALL SETMSG ( 'Error writing to the DAS transfer'   //
     .                 ' file: #. IOSTAT = #.'                )
         CALL ERRFNM ( '#', XFRLUN                            )
         CALL ERRINT ( '#', IOSTAT                            )
         CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
         CALL CHKOUT ( 'DASBT'                                )
         RETURN
 
      END IF
C
C     Write the internal file name of the DAS file to the DAS transfer
C     file.
C
      WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) QUOTE // IFNAME // QUOTE
 
      IF ( IOSTAT .NE. 0 ) THEN
C
C        An error occurred, so close the binary DAS file, set an
C        appropriate error message, and return to the caller.
C
         CALL DASCLS ( HANDLE  )
         CALL SETMSG ( 'Error writing to the DAS transfer'   //
     .                 ' file: #. IOSTAT = #.'                )
         CALL ERRFNM ( '#', XFRLUN                            )
         CALL ERRINT ( '#', IOSTAT                            )
         CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
         CALL CHKOUT ( 'DASBT'                                )
         RETURN
 
      END IF
C
C     Write the number of reserved records and reserved characters to
C     the DAS transfer file.
C
      CALL WRENCI ( XFRLUN, 1, NRESVR )
      CALL WRENCI ( XFRLUN, 1, NRESVC )
 
      IF ( FAILED() ) THEN
C
C        If an error occurred while writing the number of reserved
C        records or number of reserved characters, attempt to close
C        the binary file, then check out and return.
C
         CALL DASCLS ( HANDLE  )
         CALL CHKOUT ( 'DASBT' )
         RETURN
 
      END IF
C
C     Write the number of comment records and comment characters to
C     the DAS transfer file.
C
      CALL WRENCI ( XFRLUN, 1, NCOMR )
      CALL WRENCI ( XFRLUN, 1, NCOMC )
 
      IF ( FAILED() ) THEN
C
C        If an error occurred while writing the number of comment
C        records or number of comment characters, attempt to close
C        the binary file, then check out and return.
C
         CALL DASCLS ( HANDLE  )
         CALL CHKOUT ( 'DASBT' )
         RETURN
 
      END IF
C
C     **************************************************************
C     When/if the reserved record area is implemented, the code to
C     convert it and place it into the DAS transfer file should go
C     here. It should be possible to simply copy the code for the
C     comment area, making all of the necessary variable name changes,
C     etc., since the reserved record area is going to contain ONLY
C     character data.
C     **************************************************************
C
C     Write out the comment area of the DAS file, if there are any
C     comment characters stored in it.
C
      IF ( NCOMC .GT. 0 ) THEN
C
C        Write out the comment records, one at a time.
C
         CRECRD = ' '
         NUMLFT = NCOMC
         NUMBLK = 0
         RECNO  = 1 + NRESVR
 
         DO WHILE ( NUMLFT .GT. 0 )
 
            NUMBLK = NUMBLK + 1
            RECNO  = RECNO  + 1
 
            IF ( NUMLFT .GT. CRLEN ) THEN
 
               NUMDTA = CRLEN
 
            ELSE
 
               NUMDTA = NUMLFT
 
            END IF
C
C           Write out the begin comment block marker and the number of
C           comment characters.
C
            LINE = BEGCOM // ' #' // ' #'
            CALL REPMI ( LINE, '#', NUMBLK, LINE )
            CALL REPMI ( LINE, '#', NUMDTA, LINE  )
            WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))
 
            IF ( IOSTAT .NE. 0 ) THEN
C
C              An error occurred, so close the binary DAS file, set an
C              appropriate error message, and return to the caller.
C
               CALL DASCLS ( HANDLE                                 )
               CALL SETMSG ( 'Error writing to the DAS transfer'   //
     .                       ' file: #. IOSTAT = #.'                )
               CALL ERRFNM ( '#', XFRLUN                            )
               CALL ERRINT ( '#', IOSTAT                            )
               CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
               CALL CHKOUT ( 'DASBT'                                )
               RETURN
 
            END IF
C
C           Read a comment record and then encode and write it.
C
            CALL DASIOC ( 'READ', DASLUN, RECNO, CRECRD )
            CALL WRENCC ( XFRLUN, NUMDTA, CRECRD        )
 
            IF ( FAILED() ) THEN
C
C              We want to check failed here because were in a loop.
C              We should exit the loop, and the routine, as soon as
C              an error is detected, so we don't continue doing things
C              for a long time. Attempt to close the binary DAS file
C              that we opened and then return to the caller.
C
               CALL DASCLS ( HANDLE  )
               CALL CHKOUT ( 'DASBT' )
               RETURN
 
            END IF
C
C           Write out the end comment block marker and the number of
C           comment characters.
C
            LINE = ENDCOM // ' #' // ' #'
            CALL REPMI ( LINE, '#', NUMBLK, LINE )
            CALL REPMI ( LINE, '#', NUMDTA, LINE )
            WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))
 
            IF ( IOSTAT .NE. 0 ) THEN
C
C              An error occurred, so close the binary DAS file, set an
C              appropriate error message, and return to the caller.
C
               CALL DASCLS ( HANDLE                                 )
               CALL SETMSG ( 'Error writing to the DAS transfer'   //
     .                       ' file: #. IOSTAT = #.'                )
               CALL ERRFNM ( '#', XFRLUN                            )
               CALL ERRINT ( '#', IOSTAT                            )
               CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
               CALL CHKOUT ( 'DASBT'                                )
               RETURN
 
            END IF
C
C           Update the number of comment characters remaining to be
C           written.
C
            NUMLFT = NUMLFT - NUMDTA
 
         END DO
C
C        Write out the number of comment blocks processed, and the
C        count of comment characters
C
         LINE = TCMBLK // ' #' // ' #'
         CALL REPMI ( LINE, '#', NUMBLK, LINE )
         CALL REPMI ( LINE, '#', NCOMC, LINE  )
         WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))
 
         IF ( IOSTAT .NE. 0 ) THEN
C
C           An error occurred, so close the binary DAS file, set an
C           appropriate error message, and return to the caller.
C
            CALL DASCLS ( HANDLE                                  )
            CALL SETMSG ( 'Error writing to the DAS transfer'   //
     .                    ' file: #. IOSTAT = #.'                )
            CALL ERRFNM ( '#', XFRLUN                             )
            CALL ERRINT ( '#', IOSTAT                             )
            CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'                )
            CALL CHKOUT ( 'DASBT'                                 )
            RETURN
 
         END IF
 
      END IF
C
C     Read in the data counts for each of the data types from the binary
C     DAS file.
C
      CALL DASLLA ( HANDLE, NCDATA, NDDATA, NIDATA )
C
C     Write the data counts to the DAS transfer file. These will be
C     useful in determining which data types to expect in the DAS
C     transfer file when converting it back to binary.
C
      CALL WRENCI( XFRLUN, 1, NCDATA )
      CALL WRENCI( XFRLUN, 1, NDDATA )
      CALL WRENCI( XFRLUN, 1, NIDATA )
 
      IF ( FAILED() ) THEN
C
C        If an error occurred while writing any of the data counts to
C        the DAS transfer file, attempt to close the binary file, then
C        check out and return.
C
         CALL DASCLS ( HANDLE  )
         CALL CHKOUT ( 'DASBT' )
         RETURN
 
      END IF
C
C     Encode and write the CHARACTER data to the DAS transfer file, if
C     there is any character data.
C
      IF ( NCDATA .GT. 0 ) THEN
 
         NUMBLK = 0
         DTABEG = 1
         NUMLFT = NCDATA
 
         DO WHILE ( NUMLFT .GT. 0 )
 
            NUMBLK = NUMBLK + 1
 
            IF ( NUMLFT .GE. ( CBFLEN * BUFSIZ ) ) THEN
 
               NUMDTA = CBFLEN * BUFSIZ
 
            ELSE
 
               NUMDTA = NUMLFT
 
            END IF
C
C           Write out the begin data block identifier, the block
C           number, and the data count for the block.
C
            LINE = BCBLK // ' #' // ' #'
            CALL REPMI ( LINE, '#', NUMBLK, LINE )
            CALL REPMI ( LINE, '#', NUMDTA, LINE )
            WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))
 
            IF ( IOSTAT .NE. 0 ) THEN
C
C              An error occurred, so close the binary DAS file, set an
C              appropriate error message, and return to the caller.
C
               CALL DASCLS ( HANDLE                                 )
               CALL SETMSG ( 'Error writing to the DAS transfer'   //
     .                       ' file: #. IOSTAT = #.'                )
               CALL ERRFNM ( '#', XFRLUN                            )
               CALL ERRINT ( '#', IOSTAT                            )
               CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
               CALL CHKOUT ( 'DASBT'                                )
               RETURN
 
            END IF
C
C           Read in NUMDTA characters. The desired data are specified by
C           beginning and ending indices into the array, inclusive: thus
C           the subtraction of 1 in the call.
C
            CALL DASRDC ( HANDLE,
     .                    DTABEG, DTABEG+NUMDTA-1,
     .                    BCBPOS, ECBPOS,
     .                    CBUFFR                   )
C
C           Encode and write out a buffer of characters.
C
            CALL WRENCC ( XFRLUN, NUMDTA, CBUFFR )
 
            IF ( FAILED() ) THEN
C
C              We want to check failed here because were in a loop.
C              We should exit the loop, and the routine, as soon as
C              an error is detected, so we don't continue doing things
C              for a long time. Attempt to close the binary DAS file
C              that we opened and then returrn to the caller.
C
               CALL DASCLS ( HANDLE  )
               CALL CHKOUT ( 'DASBT' )
               RETURN
 
            END IF
C
C           Write out the end data block identifier, the block number,
C           and the data count for the block.
C
            LINE = ECBLK // ' #' // ' #'
            CALL REPMI ( LINE, '#', NUMBLK, LINE )
            CALL REPMI ( LINE, '#', NUMDTA, LINE )
            WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))
 
            IF ( IOSTAT .NE. 0 ) THEN
C
C              An error occurred, so close the binary DAS file, set an
C              appropriate error message, and return to the caller.
C
               CALL DASCLS ( HANDLE                                  )
               CALL SETMSG ( 'Error writing to the DAS transfer'    //
     .                       ' file: #. IOSTAT = #.'                 )
               CALL ERRFNM ( '#', XFRLUN                             )
               CALL ERRINT ( '#', IOSTAT                             )
               CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'                )
               CALL CHKOUT ( 'DASBT'                                 )
               RETURN
 
            END IF
C
C           Increment the data pointer and decrement the amount of data
C           left to move.
C
            DTABEG = DTABEG + NUMDTA
            NUMLFT = NUMLFT - NUMDTA
 
         END DO
C
C        Write out the number of character data blocks processed
C        processed, and the count of double precision data items.
C
         LINE = TCBLKS // ' #' // ' #'
         CALL REPMI ( LINE, '#', NUMBLK, LINE )
         CALL REPMI ( LINE, '#', NCDATA, LINE )
         WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))
 
         IF ( IOSTAT .NE. 0 ) THEN
C
C           An error occurred, so close the binary DAS file, set an
C           appropriate error message, and return to the caller.
C
            CALL DASCLS ( HANDLE                                  )
            CALL SETMSG ( 'Error writing to the DAS transfer'    //
     .                    ' file: #. IOSTAT = #.'                 )
            CALL ERRFNM ( '#', XFRLUN                             )
            CALL ERRINT ( '#', IOSTAT                             )
            CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'                )
            CALL CHKOUT ( 'DASBT'                                 )
            RETURN
 
         END IF
 
      END IF
C
C     Encode and write the DOUBLE PRECISION data to the DAS transfer
C     file.
C
      IF ( NDDATA .GT. 0 ) THEN
 
         NUMBLK = 0
         DTABEG = 1
         NUMLFT = NDDATA
 
         DO WHILE ( NUMLFT .GT. 0 )
 
            NUMBLK = NUMBLK + 1
 
            IF ( NUMLFT .GE. BUFSIZ ) THEN
 
               NUMDTA = BUFSIZ
 
            ELSE
 
               NUMDTA = NUMLFT
 
            END IF
C
C           Write out the begin data block identifier, the block
C           number, and the data count for the block.
C
            LINE = BDBLK // ' #' // ' #'
            CALL REPMI ( LINE, '#', NUMBLK, LINE )
            CALL REPMI ( LINE, '#', NUMDTA, LINE )
            WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))
 
            IF ( IOSTAT .NE. 0 ) THEN
C
C              An error occurred, so close the binary DAS file, set an
C              appropriate error message, and return to the caller.
C
               CALL DASCLS ( HANDLE                                 )
            CALL SETMSG ( 'Error writing to the DAS transfer'      //
     .                    ' file: #. IOSTAT = #.'                   )
               CALL ERRFNM ( '#', XFRLUN                            )
               CALL ERRINT ( '#', IOSTAT                            )
               CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
               CALL CHKOUT ( 'DASBT'                                )
               RETURN
 
            END IF
C
C           Read in NUMDTA double precision numbers.The desired data are
C           specified by beginning and ending indices into the array,
C           inclusive: thus the subtraction of 1 in the call.
C
            CALL DASRDD ( HANDLE, DTABEG, DTABEG+NUMDTA-1, DBUFFR )
C
C           Encode and write out a buffer of double precision numbers.
C
            CALL WRENCD ( XFRLUN, NUMDTA, DBUFFR )
 
            IF ( FAILED() ) THEN
C
C              We want to check failed here because were in a loop.
C              We should exit the loop, and the routine, as soon as
C              an error is detected, so we don't continue doing things
C              for a long time. Attempt to close the binary DAS file
C              that we opened and then returrn to the caller.
C
               CALL DASCLS ( HANDLE  )
               CALL CHKOUT ( 'DASBT' )
               RETURN
 
            END IF
C
C           Write out the end data block identifier, the block number,
C           and the data count for the block.
C
            LINE = EDBLK // ' #' // ' #'
            CALL REPMI ( LINE, '#', NUMBLK, LINE )
            CALL REPMI ( LINE, '#', NUMDTA, LINE )
            WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))
 
            IF ( IOSTAT .NE. 0 ) THEN
C
C              An error occurred, so close the binary DAS file, set an
C              appropriate error message, and return to the caller.
C
               CALL DASCLS ( HANDLE                                  )
               CALL SETMSG ( 'Error writing to the DAS transfer'    //
     .                       ' file: #. IOSTAT = #.'                 )
               CALL ERRFNM ( '#', XFRLUN                             )
               CALL ERRINT ( '#', IOSTAT                             )
               CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'                )
               CALL CHKOUT ( 'DASBT'                                 )
               RETURN
 
            END IF
C
C           Increment the data pointer and decrement the amount of data
C           left to move.
C
            DTABEG = DTABEG + NUMDTA
            NUMLFT = NUMLFT - NUMDTA
 
         END DO
C
C        Write out the number of double precision processed data blocks
C        processed, and the count of double precision data items.
C
         LINE = TDBLKS// ' #' // ' #'
         CALL REPMI ( LINE, '#', NUMBLK, LINE )
         CALL REPMI ( LINE, '#', NDDATA, LINE )
         WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))
 
         IF ( IOSTAT .NE. 0 ) THEN
C
C           An error occurred, so close the binary DAS file, set an
C           appropriate error message, and return to the caller.
C
            CALL DASCLS ( HANDLE                                  )
            CALL SETMSG ( 'Error writing to the DAS transfer'    //
     .                    ' file: #. IOSTAT = #.'                 )
            CALL ERRFNM ( '#', XFRLUN                             )
            CALL ERRINT ( '#', IOSTAT                             )
            CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'                )
            CALL CHKOUT ( 'DASBT'                                 )
            RETURN
 
         END IF
 
      END IF
C
C     Encode and write the INTEGER data to the DAS transfer file, if
C     there is any.
C
      IF ( NIDATA .GT. 0 ) THEN
 
         NUMBLK = 0
         DTABEG = 1
         NUMLFT = NIDATA
 
         DO WHILE ( NUMLFT .GT. 0 )
 
            NUMBLK = NUMBLK + 1
 
            IF ( NUMLFT .GE. BUFSIZ ) THEN
 
               NUMDTA = BUFSIZ
 
            ELSE
 
               NUMDTA = NUMLFT
 
            END IF
C
C           Write out the begin data block identifier, the block number,
C           and the data count for the block.
C
            LINE = BIBLK // ' #' // ' #'
            CALL REPMI ( LINE, '#', NUMBLK, LINE )
            CALL REPMI ( LINE, '#', NUMDTA, LINE )
            WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))
 
            IF ( IOSTAT .NE. 0 ) THEN
C
C              An error occurred, so close the binary DAS file, set an
C              appropriate error message, and return to the caller.
C
               CALL DASCLS ( HANDLE                                 )
               CALL SETMSG ( 'Error writing to the DAS transfer'   //
     .                       ' file: #. IOSTAT = #.'                )
               CALL ERRFNM ( '#', XFRLUN                            )
               CALL ERRINT ( '#', IOSTAT                            )
               CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
               CALL CHKOUT ( 'DASBT'                                )
               RETURN
 
            END IF
C
C           Read in NUMDTA integers. The desired data are specified by
C           beginning and ending indices into the array,inclusive: thus
C           the subtraction of 1 in the call.
C
            CALL DASRDI ( HANDLE, DTABEG, DTABEG+NUMDTA-1, IBUFFR )
C
C           Encode and write out a buffer of integers.
C
            CALL WRENCI ( XFRLUN, NUMDTA, IBUFFR )
 
            IF ( FAILED() ) THEN
C
C              We want to check failed here because were in a loop.
C              We should exit the loop, and the routine, as soon as
C              an error is detected, so we don't continue doing things
C              for a long time. Attempt to close the binary DAS file
C              that we opened and then returrn to the caller.
C
               CALL DASCLS ( HANDLE  )
               CALL CHKOUT ( 'DASBT' )
               RETURN
 
            END IF
C
C           Write out the end data block identifier, the block number,
C           and the data count for the block.
C
            LINE = EIBLK // ' #' // ' #'
            CALL REPMI ( LINE, '#', NUMBLK, LINE )
            CALL REPMI ( LINE, '#', NUMDTA, LINE )
            WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))
 
            IF ( IOSTAT .NE. 0 ) THEN
C
C              An error occurred, so close the binary DAS file, set an
C              appropriate error message, and return to the caller.
C
               CALL DASCLS ( HANDLE                                  )
               CALL SETMSG ( 'Error writing to the DAS transfer'    //
     .                       ' file: #. IOSTAT = #.'                 )
               CALL ERRFNM ( '#', XFRLUN                             )
               CALL ERRINT ( '#', IOSTAT                             )
               CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'                )
               CALL CHKOUT ( 'DASBT'                                 )
               RETURN
 
            END IF
C
C           Increment the data pointers and decrement the amount of data
C           left.
C
            DTABEG = DTABEG + NUMDTA
            NUMLFT = NUMLFT - NUMDTA
 
         END DO
C
C        Write out the number of processed integer data blocks
C        processed, and the count of double precision data items.
C
         LINE = TIBLKS // ' #' // ' #'
         CALL REPMI ( LINE, '#', NUMBLK, LINE )
         CALL REPMI ( LINE, '#', NIDATA, LINE )
         WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))
 
         IF ( IOSTAT .NE. 0 ) THEN
C
C           An error occurred, so close the binary DAS file, set an
C           appropriate error message, and return to the caller.
C
            CALL DASCLS ( HANDLE                                  )
            CALL SETMSG ( 'Error writing to the DAS transfer'    //
     .                    ' file: #. IOSTAT = #.'                 )
            CALL ERRFNM ( '#', XFRLUN                             )
            CALL ERRINT ( '#', IOSTAT                             )
            CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'                )
            CALL CHKOUT ( 'DASBT'                                 )
            RETURN
 
         END IF
 
      END IF
C
C     Close only the binary DAS file.
C
      CALL DASCLS ( HANDLE )
 
      CALL CHKOUT ( 'DASBT' )
      RETURN
      END
