 
C$Procedure DAFBT ( DAF, convert binary file to transfer file )
 
      SUBROUTINE DAFBT ( BINFIL, XFRLUN )
 
C$ Abstract
C
C     Convert the contents of a binary DAF file to an equivalent DAF
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
C     DAF
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
C     BINFIL     I   The name of a binary DAF file to be converted.
C     XFRLUN     I   Logical unit of a previously opened file.
C
C$ Detailed_Input
C
C     BINFIL   The name of a binary DAF file which is to be converted
C              to an equivalent DAF transfer file.
C
C     XFRLUN   The Fortran logical unit number of a previously opened
C              file. The DAF transfer file will be written to the
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
C
C     1)   If the binary DAF file specified by the filename BINFIL
C          cannot be opened for read access, an appropriate error
C          message will be signalled by a DAF file access routine that
C          is called.
C
C     2)   If for some reason the DAF transfer file cannot be written
C          to, the error SPICE(FILEWRITEFAILED) is signalled.
C
C     3)   If, for any reason, the DAF file cannot be read, a DAF file
C          access routine will signal an error with appropriate error
C          message.
C
C     4)   If the ID word cannot be read from the binary file, the error
C          SPICE(FILEREADFAILED) will be signalled.
C
C     5)   The binary DAF file opened by this routine, BINFIL, is only
C          GUARANTEED to be closed upon successful completion of the
C          conversion process. In the event of an error, the caller of
C          this routine is required to close the binary DAF file BINFIL.
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
C     This routine provides a mechanism for converting a binary DAF
C     file into an equivalent encoded ASCII file called a DAF transfer
C     file. It is one of a pair of routines for performing conversions
C     between the binary format of a DAF file and the DAF transfer file.
C     The inverse of this routine is the routine DAFTB.
C
C     The contents of the reserved records in a binary DAF file are
C     ignored by this routine. They are not written to the DAF transfer
C     file. The reserved records must be dealt with separately from the
C     data in a DAF file.
C
C     Upon successful completion, the DAF transfer file attached to
C     Fortran logical unit XFRLUN will contain the same data as the
C     binary DAF file BINFIL. The binary DAF file BINFIL will be closed
C     when this routine exits. The DAF transfer file will remain open,
C     as it was on entry, and it will be positioned to write on the
C     first line following the encoded DAF data.
C
C$ Examples
C
C     Let
C
C        BINFIL   be the name of a binary DAF file which is to be
C                 converted to an equivalent DAF transfer file.
C
C        XFRLUN   be the Fortran logical unit to which the DAF transfer
C                 file is to be written.
C
C     The following subroutine call would read the binary DAF
C     file with the name BINFIL, convert its data into an encoded
C     format, and write that data to the DAF transfer file attached
C     to the Fortran logical unit XFRLUN, beginning at the current
C     position in the file.
C
C        CALL DAFBT( BINFIL, XFRLUN )
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
C-    SPICELIB Version 4.0.0, 16-NOV-2001 (FST)
C
C        Updated the routine to utilize the new handle manager
C        interfaces.
C
C-    SPICELIB Version 3.0.0, 25-JAN-1995 (KRG)
C
C        Updated the header and in line comments to reflect the change
C        from calling files text files to calling them transfer files.
C
C        Changed the variable name TXTLUN to XFRLUN to make it
C        compatible with the change in terminology.
C
C-    SPICELIB Version 2.0.0, 04-OCT-1993 (KRG)
C
C        No changes to this routine were necessary to incorporate the
C        new file ID word format. This routine already read and copied
C        the ID word to the text file being created.
C
C        Also, all list directed writes in this routine were replaced by
C        formatted writes with FMT = '(A)'. This routine only writes
C        character data.
C
C        Added a test of FAILED() after the call to DAFHLU for
C        completeness.
C
C-    SPICELIB Version 1.0.1, 24-JUN-1993 (KRG)
C
C        Modified the description of the DAF encoded text file format
C        appearing before the program code.
C
C-    SPICELIB Version 1.0.0, 29-OCT-1992 (KRG)
C
C-&
 
C$ Index_Entries
C
C     convert binary daf into a daf transfer file
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 4.0.0, 16-NOV-2001 (FST)
C
C        This routine still uses a naked READ to retrieve the
C        file IDWORD from the first 8 characters stored in the
C        file record.  It may be that future environments
C        will have characters whose storage exceeds 1 byte,
C        in which case this routine will require modification.
C        One possibility is to call the private file record
C        reader ZZDAFGFR, which must address the translation
C        for all supported non-native binary file formats on this
C        platform.
C
C        The existing call to DAFHLU was replaced with ZZDDHHLU.
C        The call to DAFRDA was replaced with a call to the new,
C        translation-aware routine DAFGDA.
C
C-    SPICELIB Version 2.0.0, 04-OCT-1993 (KRG)
C
C        No changes to this routine were necessary to incorporate the
C        new file ID word format. This routine already read and copied
C        the ID word to the text file being created.
C
C        Also, all list directed writes in this routine were replaced by
C        formatted writes with FMT = '(A)'. This routine only writes
C        character data.
C
C        Added a test of FAILED() after the call to DAFHLU for
C        completeness.
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
      INTEGER               RTRIM
 
      LOGICAL               RETURN
      LOGICAL               FAILED
C
C     Local parameters
C
      CHARACTER*(*)         QUOTE
      PARAMETER           ( QUOTE  = ''''           )
 
      CHARACTER*(*)         BEGARR
      PARAMETER           ( BEGARR = 'BEGIN_ARRAY'  )
 
      CHARACTER*(*)         ENDARR
      PARAMETER           ( ENDARR = 'END_ARRAY'    )
 
      CHARACTER*(*)         TOTARR
      PARAMETER           ( TOTARR = 'TOTAL_ARRAYS' )
 
      CHARACTER*(*)         FTYPID
      PARAMETER           ( FTYPID = 'DAFETF'       )
 
      CHARACTER*(*)         INFOLN
      PARAMETER           ( INFOLN = 'NAIF DAF ENCODED TRANSFER FILE' )
 
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 1024 )
 
      INTEGER               CRECL
      PARAMETER           ( CRECL  = 1000 )
 
      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN = 8    )
 
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60   )
 
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 80   )
C
C     Local variables
C
      CHARACTER*(IDWLEN)    IDWORD
      CHARACTER*(IFNLEN)    IFNAME
      CHARACTER*(LINLEN)    LINE
      CHARACTER*(CRECL)     NAME
 
      DOUBLE PRECISION      BUFFER( BUFSIZ )
      DOUBLE PRECISION      DSUMRY( 125    )
      DOUBLE PRECISION      SUMMRY( 125    )
 
      INTEGER               BINHDL
      INTEGER               BINLUN
      INTEGER               BWARD
      INTEGER               DTABEG
      INTEGER               DTACNT
      INTEGER               FREE
      INTEGER               FWARD
      INTEGER               IOSTAT
      INTEGER               ISUMRY( 250    )
      INTEGER               ND
      INTEGER               NI
      INTEGER               NUMARR
      INTEGER               NUMDTA
      INTEGER               NUMLFT
      INTEGER               SNMLEN
 
      LOGICAL               FOUND
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFBT' )
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
C     This routine will check the SPICELIB function FAILED() after
C     each call, or consecutive sequence of calls, to data encoding
C     routines, and if an error was signalled it will simply check out
C     and return to the caller.
C
C     This routine will check the SPICELIB function FAILED() after
C     each DAF file access call, and if an error was signalled it will
C     simply check out and return to the caller.
C
C     We begin by opening the binary DAF file specified by BINFIL for
C     read access, obtaining a DAF file handle.
C
      CALL DAFOPR( BINFIL, BINHDL )
C
C     If the open failed, check out and return, as an appropriate error
C     message should have already been set.
C
      IF ( FAILED () ) THEN
 
         CALL CHKOUT( 'DAFBT' )
         RETURN
 
      END IF
C
C     At this point, we know that we have a DAF file, because we were
C     able to successfully open it, so we will attempt to proceed with
C     the file conversion process.
C
C     Convert the DAF file handle to its equivalent Fortran logical
C     unit. We need to do this in order to accurately move the file
C     ID word to the DAF transfer file.
C
      CALL ZZDDHHLU ( BINHDL, 'DAF', .FALSE., BINLUN )
 
C
C     If the translation failed, checkout and return, as an appropriate
C     error message should have already been set.
C
      IF ( FAILED () ) THEN
         CALL CHKOUT( 'DAFBT' )
         RETURN
      END IF
C
C     Read the ID word from the binary file. It should be the first 8
C     characters on the first record in the file.
C
      READ( BINLUN,REC=1,IOSTAT=IOSTAT ) IDWORD
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL SETMSG ( 'Error reading the file ID word from the'  //
     .                 ' binary DAF file ''#''. IOSTAT = #.'       )
         CALL ERRFNM ( '#', BINLUN                                 )
         CALL ERRINT ( '#', IOSTAT                                 )
         CALL SIGERR ( 'SPICE(FILEREADFAILED)'                     )
         CALL CHKOUT ( 'DAFBT'                                     )
         RETURN
 
      END IF
C
C     Get the contents of the file record: the number of double
C     precision numbers in the summary (ND), the number of integers
C     in the summary (NI), the internal filename (IFNAME), and some
C     data pointer information (FWARD, BWARD, FREE).
C
      CALL DAFRFR ( BINHDL, ND, NI, IFNAME, FWARD, BWARD, FREE )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'DAFBT' )
         RETURN
 
      END IF
C
C     Write the information line containing the file type information
C     for the DAF transfer file format to the current position in the
C     DAF transfer file. The file type information must be the first
C     ``word'' on the information line. The rest of the line may be used
C     for other purposes. Right now, it simply contains an expanded
C     description of the file type information ``word.''
C
      WRITE(XFRLUN,FMT='(A)',IOSTAT=IOSTAT) FTYPID // ' ' // INFOLN
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL SETMSG ( 'Error writing to the DAF transfer'  //
     .                 ' file ''#''.IOSTAT = #.'             )
         CALL ERRFNM ( '#', XFRLUN                           )
         CALL ERRINT ( '#', IOSTAT                           )
         CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'              )
         CALL CHKOUT ( 'DAFBT'                               )
         RETURN
 
       END IF
C
C     Write the ID word to the DAF transfer file.
C
      WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) QUOTE // IDWORD // QUOTE
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL SETMSG ( 'Error writing to the DAF transfer'   //
     .                 ' file ''#''. IOSTAT = #.'             )
         CALL ERRFNM ( '#', XFRLUN                            )
         CALL ERRINT ( '#', IOSTAT                            )
         CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
         CALL CHKOUT ( 'DAFBT'                                )
         RETURN
 
      END IF
C
C     Write out the ND and NI values for the DAF file architecture.
C
      ISUMRY(1) = ND
      ISUMRY(2) = NI
 
      CALL WRENCI( XFRLUN, 2, ISUMRY )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'DAFBT' )
         RETURN
 
      END IF
C
C     Write out the internal file name.
C
      WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) QUOTE // IFNAME // QUOTE
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL SETMSG ( 'Error writing to the DAF transfer'   //
     .                 ' file ''#''. IOSTAT = #.'             )
         CALL ERRFNM ( '#', XFRLUN                            )
         CALL ERRINT ( '#', IOSTAT                            )
         CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
         CALL CHKOUT ( 'DAFBT'                                )
         RETURN
 
      END IF
C
C     Calculate the length of the segment names.
C
      SNMLEN = 8 * ( ND + ( NI + 1 ) / 2 )
C
C     Get ready to begin a forward search through the DAF file for the
C     data.
C
      CALL DAFBFS ( BINHDL )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'DAFBT' )
         RETURN
 
      END IF
C
C     Initialize the number of arrays processed to zero.
C
      NUMARR = 0
C
C     We'll assume that we will find some data, until proven otherwise.
C
      FOUND = .TRUE.
C
C     Begin looking for and processing the arrays in the binary DAF
C     file.
C
      DO WHILE ( FOUND )
C
C        Look for a DAF array.
C
         CALL DAFFNA ( FOUND  )
 
         IF ( FAILED() ) THEN
 
            CALL CHKOUT ( 'DAFBT' )
            RETURN
 
         END IF
C
C        If we found an array, then we need to process it. Start
C        by incrementing the number of arrays processed. If not,
C        we just skip to the bottom of the loop.
C
         IF ( FOUND ) THEN
 
            NUMARR = NUMARR + 1
C
C           Get and unpack the summary information for the current
C           array.
C
            CALL DAFGS ( SUMMRY )
            CALL DAFUS ( SUMMRY, ND, NI, DSUMRY, ISUMRY )
C
C           Get the name of the current array.
C
            CALL DAFGN ( NAME )
 
            IF ( FAILED() ) THEN
C
C              If an error occurred on any of the DAF system calls
C              above, return to the caller. An appropriate error
C              message will have already been set by the routine which
C              signalled the error.
C
               CALL CHKOUT ( 'DAFBT' )
               RETURN
 
            END IF
C
C           Get the beginning address for the data in the current array.
C
            DTABEG = ISUMRY(NI-1)
C
C           Set the number of double precision numbers in the current
C           array.
C
            DTACNT = ISUMRY(NI) - ISUMRY(NI-1) + 1
 
            LINE = BEGARR // ' #' // ' #'
            CALL REPMI ( LINE, '#', NUMARR, LINE )
            CALL REPMI ( LINE, '#', DTACNT, LINE )
            WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))
 
            IF ( IOSTAT .NE. 0 ) THEN
 
               CALL SETMSG ( 'Error writing to the DAF transfer'   //
     .                       ' file ''#''. IOSTAT = #.'             )
               CALL ERRFNM ( '#', XFRLUN                            )
               CALL ERRINT ( '#', IOSTAT                            )
               CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
               CALL CHKOUT ( 'DAFBT'                                )
               RETURN
 
            END IF
C
C           Write the name of the current array.
C
            WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT)
     .                              QUOTE // NAME( :SNMLEN) // QUOTE
 
            IF ( IOSTAT .NE. 0 ) THEN
 
               CALL SETMSG ( 'Error writing to the DAF transfer'   //
     .                       ' file ''#''. IOSTAT = #.'             )
               CALL ERRFNM ( '#', XFRLUN                            )
               CALL ERRINT ( '#', IOSTAT                            )
               CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
               CALL CHKOUT ( 'DAFBT'                                )
               RETURN
 
            END IF
C
C           Write out the double precision part of the summary.
C
            CALL WRENCD( XFRLUN, ND, DSUMRY )
C
C           Write out the integer part of the summary, excluding the
C           beginning and ending addresses of the data in the array,
C           ISUMRY(NI-1) and ISUMRY(NI), since these values vary with
C           the number of reserved records allocated.
C
            CALL WRENCI( XFRLUN, NI-2, ISUMRY )
 
            IF ( FAILED() ) THEN
C
C              If an error occurred on any of the data encoding calls
C              above, return to the caller. An appropriate error message
C              will have already been set by the routine which signalled
C              the error.
C
               CALL CHKOUT ( 'DAFBT' )
               RETURN
 
            END IF
 
            NUMLFT = DTACNT
 
            DO WHILE ( NUMLFT .GT. 0 )
 
               IF ( NUMLFT .GE. BUFSIZ ) THEN
 
                  NUMDTA = BUFSIZ
 
               ELSE
 
                  NUMDTA = NUMLFT
 
               END IF
C
C              Read in NUMDTA numbers from the current array. The
C              desired data are specified by beginning and ending
C              indices into the array, inclusive: thus the subtraction
C              of 1 in the call.
C
               CALL DAFGDA ( BINHDL, DTABEG, DTABEG+NUMDTA-1, BUFFER )
 
               IF ( FAILED() ) THEN
C
C                 We want to check failed here because were in a loop.
C                 We should exit the loop, and the routine, as soon as
C                 an error is detected, so we don't continue doing
C                 things for a long time.
C
                  CALL CHKOUT ( 'DAFBT' )
                  RETURN
 
               END IF
C
C              Write out the count of double precision numbers which are
C              in the buffer.
C
               LINE = '#'
               CALL REPMI ( LINE, '#', NUMDTA, LINE )
               WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))
 
               IF ( IOSTAT .NE. 0 ) THEN
 
               CALL SETMSG ( 'Error writing to the DAF transfer'   //
     .                       ' file ''#''. IOSTAT = #.'             )
                  CALL ERRFNM ( '#', XFRLUN                         )
                  CALL ERRINT ( '#', IOSTAT                         )
                  CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'            )
                  CALL CHKOUT ( 'DAFBT'                             )
                  RETURN
 
               END IF
C
C              Encode and write out a buffer of double precision
C              numbers.
C
               CALL WRENCD( XFRLUN, NUMDTA, BUFFER )
 
               IF ( FAILED() ) THEN
C
C                 We want to check failed here because were in a loop.
C                 We should exit the loop, and the routine, as soon as
C                 an error is detected, so we don't continue doing
C                 things for a long time.
C
                  CALL CHKOUT ( 'DAFBT' )
                  RETURN
 
               END IF
 
               NUMLFT = NUMLFT - NUMDTA
               DTABEG = DTABEG + NUMDTA
 
            END DO
 
            LINE = ENDARR // ' #' // ' #'
            CALL REPMI ( LINE, '#', NUMARR, LINE )
            CALL REPMI ( LINE, '#', DTACNT, LINE )
            WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))
 
            IF ( IOSTAT .NE. 0 ) THEN
 
               CALL SETMSG ( 'Error writing to the DAF transfer'   //
     .                       ' file ''#''. IOSTAT = #.'             )
               CALL ERRFNM ( '#', XFRLUN                            )
               CALL ERRINT ( '#', IOSTAT                            )
               CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
               CALL CHKOUT ( 'DAFBT'                                )
               RETURN
 
            END IF
 
         END IF
C
C        At this point, one complete DAF array has been written to the
C        DAF transfer file.
C
      END DO
C
C     Write out the number of arrays processed.
C
      LINE = TOTARR // ' #'
      CALL REPMI ( LINE, '#', NUMARR, LINE )
      WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL SETMSG ( 'Error writing to the DAF transfer'   //
     .                 ' file ''#''. IOSTAT = #.'             )
         CALL ERRFNM ( '#', XFRLUN                            )
         CALL ERRINT ( '#', IOSTAT                            )
         CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
         CALL CHKOUT ( 'DAFBT'                                )
         RETURN
 
      END IF
C
C     Close only the binary file.
C
      CALL DAFCLS ( BINHDL )
 
      CALL CHKOUT ( 'DAFBT' )
      RETURN
      END
