C$Procedure DAFB2T ( DAF, binary to text )
 
      SUBROUTINE DAFB2T ( BINARY, TEXT )
 
C$ Abstract
C
C     Deprecated. The routine DAFBT supersedes this routine.
C     NAIF supports this routine only to provide backward 
C     compatibility.
C
C     Write the contents of a binary DAF to a text file opened by
C     the calling program.
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
C     FILES
C
C$ Declarations
 
      CHARACTER*(*)         BINARY
      INTEGER               TEXT
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     BINARY     I   Name of an existing binary DAF.
C     TEXT       I   Logical unit connected to text file.
C
C$ Detailed_Input
C
C     BINARY      is the name of an existing binary DAF.
C
C     TEXT        is a logical unit number, to which a text file has
C                 been connected by the calling program, and into
C                 which the contents of BINARY are to be written
C                 (in a form more suitable for transfer between
C                 heterogeneous computing environments).
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If for some reason the text file cannot be written,
C        the error SPICE(DAFWRITEFAIL) is signalled.
C
C     2) If for some reason the ID word cannot be read from the DAF
C        file, the error SPICE(DAFREADFAIL) will be signalled.
C
C$ Files
C
C     See arguments BINARY, TEXT.
C
C$ Particulars
C
C     This routine has been made obsolete by the new DAF binary to text
C     conversion routine DAFBT. This routine remains available for
C     reasons of backward compatibility. We strongly recommend that you
C     use the new conversion routines for any new software development.
C     Please see the header of the routine DAFBT for details.
C
C     Any binary DAF may be transferred between heterogeneous
C     Fortran environments by converting it to an equivalent file
C     containing only ASCII characters. Such a file can be transferred
C     almost universally, using any number of established protocols
C     (Kermit, FTP, and so on). Once transferred, the ASCII file can
C     be converted to a binary file, using the representations
C     native to the new host environment.
C
C     There are two pairs of routines that can be used to convert
C     DAFs between binary and text. The first pair, DAFB2A
C     and DAFA2B, works with complete files. That is, DAFB2A creates
C     a complete ASCII file containing all of the information in
C     a particular binary file, and nothing else; this file can
C     be fed directly into DAFA2B to produce a complete binary file.
C     In each case, the names of the files are specified.
C
C     A related pair of routines, DAFB2T and DAFT2B, assume that
C     the ASCII data are to be stored in the midst of a text file.
C     This allows the calling program to surround the data with
C     standardized labels, to append several binary files into a
C     single text file, and so on.
C
C     Note that the contents of reserved records in the binary file
C     are not written by this routine (although they may be stored
C     in the ASCII file by the calling program).
C
C$ Examples
C
C     DAFB2A and DAFA2B are typically used for simple file transfers.
C     If file A.DAF is a binary DAF in environment 1, it can be
C     transferred to environment 2 in three steps.
C
C        1) Convert it to ASCII:
C
C              CALL DAFB2A ( 'A.DAF', 'A.ASCII' )
C
C        2) Transfer the ASCII file, using FTP, Kermit, or some other
C           file transfer utility:
C
C              ftp> put a.ascii
C
C        3) Convert it to binary on the new machine,
C
C              CALL DAFA2B ( 'A.ASCII', 'A.DAF', RESV )
C
C     Note that DAFB2A and DAFA2B work in any standard Fortran-77
C     environment.
C
C     If the file needs to contain other information---a standard
C     label, for instance---the first and third steps must be modified
C     to use DAFB2T and DAFT2B. The first step becomes
C
C        (Open a text file)
C        (Write the label)
C        CALL DAFB2T ( BINARY, UNIT  )
C        (Close the text file)
C
C     The third step becomes
C
C        (Open the text file)
C        (Read the label)
C        CALL DAFT2B ( UNIT, BINARY, RESV )
C        (Close the text file)
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     NAIF Document 167.0, "Double Precision Array Files (DAF)
C     Specification and User's Guide"
C
C$ Author_and_Institution
C
C     K.R. Gehringer  (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.0.1, 26-JUL-2012 (EDW)
C
C        Edited Abstract section to use "Deprecated" keyword
C        and state replacement routine.
C
C        Eliminated unneeded Revisions section.
C
C-    SPICELIB Version 3.0.0, 16-NOV-2001 (FST)
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
C        Added the variable IDWORD to the routine for storing the ID
C        word from the file being converted. This replaces a hard coded
C        value of 'NAIF/DAF', and supports the new interpretation of the
C        ID word.
C
C        Removed the error SPICE(DAFNOIDWORD) as it was no longer
C        relevant.
C
C        There were no checks of the IOSTAT variable after attempting to
C        write to the text file, a single test of the IOSTAT variable
C        was made at the end of the routine. This was not adequate to
C        detect errors when writing to the text file. So after all of
C        these write statements, an IF ... END IF block was added to
C        signal an error if IOSTAT .NE. 0.
C
C           IF ( IOSTAT .NE. 0 ) THEN
C
C              CALL DAFCLS ( HANDLE                                )
C              CALL SETMSG ( 'The attempt to write to file ''#''' //
C        .                   ' failed. IOSTAT = #.'                )
C              CALL ERRFNM ( '#', TEXT                             )
C              CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
C              CALL CHKOUT ( 'DAFB2T'                              )
C              RETURN
C
C           END IF
C
C        Removed the code from the end of the routine that purported to
C        check for read errors:
C
C           C
C           C     If any write screws up, they should all screw up. Why
C           C     make a billion separate checks?
C           C
C                 IF ( IOSTAT .NE. 0 ) THEN
C                    CALL SETMSG ( 'Value of IOSTAT was: #. ' )
C                    CALL ERRINT ( '#', IOSTAT                )
C                    CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'      )
C                  END IF
C
C        The answer to the question is:
C
C           You have to do a billion separate checks because the IOSTAT
C           value is only valid for the most recently executed write.
C
C        Added the following error message to the routine:
C
C           C     2) If for some reason the ID word cannot be read from
C           C        the DAF file, the error SPICE(DAFREADFAIL) will be
C           C        signalled.
C
C        because the file ID word is now read from the binary DAF file
C        rather than being hard coded as 'NAIF/DAF' in this routine.
C
C        Added a statement to the $ Particulars section to the effect
C        that this routine has been made obsolete by the introduction of
C        the routine DAFBT, and that we strongly recommend the use of
C        the new routine.
C
C        Modified the $ Abstract section to reflect the fact that this
C        routine is obsolete.
C
C-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     binary daf to text
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               BSIZE
      PARAMETER           ( BSIZE = 100 )
 
      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN =  8 )
 
      CHARACTER*(*)         QUOTE
      PARAMETER           ( QUOTE = '''' )
 
C
C     Local variables
C
      CHARACTER*(IDWLEN)    IDWORD
      CHARACTER*(60)        IFNAME
      CHARACTER*(1000)      NAME
 
      DOUBLE PRECISION      BUFFER   ( BSIZE )
      DOUBLE PRECISION      DC       ( 125 )
      DOUBLE PRECISION      SUM      ( 125 )
 
      INTEGER               BEGIN
      INTEGER               BWARD
      INTEGER               CHUNK
      INTEGER               CSIZE
      INTEGER               DAFLUN
      INTEGER               END
      INTEGER               FREE
      INTEGER               FWARD
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IC       ( 250 )
      INTEGER               IOSTAT
      INTEGER               ISIZE
      INTEGER               LSIZE
      INTEGER               ND
      INTEGER               NI
 
      LOGICAL               FOUND
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFB2T' )
      END IF
C
C     Initialize the IDWORD.
C
      IDWORD = ' '
 
C
C     Open the binary file for reading and read the ID word from the
C     first record of the file.
C
      CALL DAFOPR ( BINARY, HANDLE )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'DAFB2T' )
         RETURN
 
      END IF
C
C     At this point, we know that we have a DAF file, because we were
C     able to successfully open it, so we will attempt to proceed with
C     the file conversion process.
C
C     Convert the DAF file handle to its equivalent Fortran logical
C     unit. We need to do this in order to accurately move the file
C     ID word to the text file.
C
      CALL ZZDDHHLU ( HANDLE, 'DAF', .FALSE., DAFLUN )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFB2T' )
         RETURN
      END IF
 
      READ ( DAFLUN, REC=1, IOSTAT=IOSTAT ) IDWORD
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL SETMSG ( 'Could not read ID word from file ''#''.' //
     .                 ' IOSTAT = #.'                             )
         CALL ERRCH  ( '#', BINARY                                )
         CALL ERRINT ( '#', IOSTAT                                )
         CALL SIGERR ( 'SPICE(DAFREADFAIL)'                       )
         CALL CHKOUT ( 'DAFB2T'                                   )
         RETURN
 
      END IF
C
C     Get the contents of the file record. The ASCII file begins
C     with the ID word which is followed by the summary format,
C     which is followed by the internal file name.
C
      CALL DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'DAFB2T' )
         RETURN
 
      END IF
 
      WRITE (TEXT,*,IOSTAT=IOSTAT) QUOTE // IDWORD // QUOTE
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL DAFCLS ( HANDLE                                )
         CALL SETMSG ( 'The attempt to write to file ''#''' //
     .                 ' failed. IOSTAT = #.'                )
         CALL ERRFNM ( '#', TEXT                             )
         CALL ERRINT ( '#', IOSTAT                           )
         CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
         CALL CHKOUT ( 'DAFB2T'                              )
         RETURN
 
      END IF
 
      WRITE (TEXT,*,IOSTAT=IOSTAT) ND
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL DAFCLS ( HANDLE                                )
         CALL SETMSG ( 'The attempt to write to file ''#''' //
     .                 ' failed. IOSTAT = #.'                )
         CALL ERRFNM ( '#', TEXT                             )
         CALL ERRINT ( '#', IOSTAT                           )
         CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
         CALL CHKOUT ( 'DAFB2T'                              )
         RETURN
 
      END IF
 
      WRITE (TEXT,*,IOSTAT=IOSTAT) NI
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL DAFCLS ( HANDLE                                )
         CALL SETMSG ( 'The attempt to write to file ''#''' //
     .                 ' failed. IOSTAT = #.'                )
         CALL ERRFNM ( '#', TEXT                             )
         CALL ERRINT ( '#', IOSTAT                           )
         CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
         CALL CHKOUT ( 'DAFB2T'                              )
         RETURN
 
      END IF
 
      WRITE (TEXT,*,IOSTAT=IOSTAT) QUOTE //  IFNAME    // QUOTE
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL DAFCLS ( HANDLE                                )
         CALL SETMSG ( 'The attempt to write to file ''#''' //
     .                 ' failed. IOSTAT = #.'                )
         CALL ERRFNM ( '#', TEXT                             )
         CALL ERRINT ( '#', IOSTAT                           )
         CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
         CALL CHKOUT ( 'DAFB2T'                              )
         RETURN
 
      END IF
C
C     Each array is preceded by a '1', which indicates that more
C     arrays are to come. The array itself begins with the name
C     and the summary components, and ends with the name again.
C     The elements are written in arbitrary chunks. The final
C     chunk is followed by a '0', which indicates that no chunks
C     remain.
C
C     Write the arrays in forward order.
C
      LSIZE = ND + (NI - 1) / 2 + 1
      ISIZE = LSIZE * 8
 
      CALL DAFBFS ( HANDLE )
      CALL DAFFNA ( FOUND  )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'DAFB2T' )
         RETURN
 
      END IF
 
      DO WHILE ( FOUND )
 
         CALL DAFGS ( SUM  )
         CALL DAFGN ( NAME )
         CALL DAFUS ( SUM, ND, NI, DC, IC )
 
         IF ( FAILED() ) THEN
 
            CALL CHKOUT ( 'DAFB2T' )
            RETURN
 
         END IF
 
         WRITE (TEXT,*,IOSTAT=IOSTAT) '1'
 
         IF ( IOSTAT .NE. 0 ) THEN
 
            CALL DAFCLS ( HANDLE                                )
            CALL SETMSG ( 'The attempt to write to file ''#''' //
     .                    ' failed. IOSTAT = #.'                )
            CALL ERRFNM ( '#', TEXT                             )
            CALL ERRINT ( '#', IOSTAT                           )
            CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
            CALL CHKOUT ( 'DAFB2T'                              )
            RETURN
 
         END IF
 
         WRITE (TEXT,*,IOSTAT=IOSTAT) QUOTE // NAME( :ISIZE) // QUOTE
 
         IF ( IOSTAT .NE. 0 ) THEN
 
            CALL DAFCLS ( HANDLE                                )
            CALL SETMSG ( 'The attempt to write to file ''#''' //
     .                    ' failed. IOSTAT = #.'                )
            CALL ERRFNM ( '#', TEXT                             )
            CALL ERRINT ( '#', IOSTAT                           )
            CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
            CALL CHKOUT ( 'DAFB2T'                              )
            RETURN
 
         END IF
 
         WRITE (TEXT,*,IOSTAT=IOSTAT) ( DC(I), I = 1, ND     )
 
         IF ( IOSTAT .NE. 0 ) THEN
 
            CALL DAFCLS ( HANDLE                                )
            CALL SETMSG ( 'The attempt to write to file ''#''' //
     .                    ' failed. IOSTAT = #.'                )
            CALL ERRFNM ( '#', TEXT                             )
            CALL ERRINT ( '#', IOSTAT                           )
            CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
            CALL CHKOUT ( 'DAFB2T'                              )
            RETURN
 
         END IF
 
         WRITE (TEXT,*,IOSTAT=IOSTAT) ( IC(I), I = 1, NI - 2 )
 
         IF ( IOSTAT .NE. 0 ) THEN
 
            CALL DAFCLS ( HANDLE                                )
            CALL SETMSG ( 'The attempt to write to file ''#''' //
     .                    ' failed. IOSTAT = #.'                )
            CALL ERRFNM ( '#', TEXT                             )
            CALL ERRINT ( '#', IOSTAT                           )
            CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
            CALL CHKOUT ( 'DAFB2T'                              )
            RETURN
 
         END IF
 
         BEGIN = IC(NI-1)
         END   = IC(NI  )
 
         DO WHILE ( BEGIN .LE. END )
 
            CHUNK = MIN ( BEGIN + BSIZE - 1, END )
            CSIZE =       CHUNK - BEGIN + 1
 
            CALL DAFGDA ( HANDLE, BEGIN, CHUNK, BUFFER )
 
            IF ( FAILED() ) THEN
 
               CALL CHKOUT ( 'DAFB2T' )
               RETURN
 
            END IF
 
            WRITE (TEXT,*,IOSTAT=IOSTAT) CSIZE
 
            IF ( IOSTAT .NE. 0 ) THEN
 
               CALL DAFCLS ( HANDLE                                )
               CALL SETMSG ( 'The attempt to write to file ''#''' //
     .                       ' failed. IOSTAT = #.'                )
               CALL ERRFNM ( '#', TEXT                             )
               CALL ERRINT ( '#', IOSTAT                           )
               CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
               CALL CHKOUT ( 'DAFB2T'                              )
               RETURN
 
            END IF
 
            WRITE (TEXT,*,IOSTAT=IOSTAT) ( BUFFER(I), I = 1, CSIZE )
 
            IF ( IOSTAT .NE. 0 ) THEN
 
               CALL DAFCLS ( HANDLE                                )
               CALL SETMSG ( 'The attempt to write to file ''#''' //
     .                       ' failed. IOSTAT = #.'                )
               CALL ERRFNM ( '#', TEXT                             )
               CALL ERRINT ( '#', IOSTAT                           )
               CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
               CALL CHKOUT ( 'DAFB2T'                              )
               RETURN
 
            END IF
 
            BEGIN = BEGIN + BSIZE
 
         END DO
 
         WRITE (TEXT,*,IOSTAT=IOSTAT) '0'
 
         IF ( IOSTAT .NE. 0 ) THEN
 
            CALL DAFCLS ( HANDLE                                )
            CALL SETMSG ( 'The attempt to write to file ''#''' //
     .                    ' failed. IOSTAT = #.'                )
            CALL ERRFNM ( '#', TEXT                             )
            CALL ERRINT ( '#', IOSTAT                           )
            CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
            CALL CHKOUT ( 'DAFB2T'                              )
            RETURN
 
         END IF
 
         WRITE (TEXT,*,IOSTAT=IOSTAT) QUOTE // NAME( :ISIZE) // QUOTE
 
         IF ( IOSTAT .NE. 0 ) THEN
 
            CALL DAFCLS ( HANDLE                                )
            CALL SETMSG ( 'The attempt to write to file ''#''' //
     .                    ' failed. IOSTAT = #.'                )
            CALL ERRFNM ( '#', TEXT                             )
            CALL ERRINT ( '#', IOSTAT                           )
            CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
            CALL CHKOUT ( 'DAFB2T'                              )
            RETURN
 
         END IF
 
         CALL DAFFNA ( FOUND )
 
         IF ( FAILED() ) THEN
 
            CALL CHKOUT ( 'DAFB2T' )
            RETURN
 
         END IF
 
      END DO
 
C
C     A final '0' indicates that no arrays remain. The first shall be
C     last: the internal file name brings up the rear.
C
      WRITE (TEXT,*,IOSTAT=IOSTAT) '0'
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL DAFCLS ( HANDLE                                )
         CALL SETMSG ( 'The attempt to write to file ''#''' //
     .                 ' failed. IOSTAT = #.'                )
         CALL ERRFNM ( '#', TEXT                             )
         CALL ERRINT ( '#', IOSTAT                           )
         CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
         CALL CHKOUT ( 'DAFB2T'                              )
         RETURN
 
      END IF
 
      WRITE (TEXT,*,IOSTAT=IOSTAT) QUOTE // IFNAME // QUOTE
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL DAFCLS ( HANDLE                                )
         CALL SETMSG ( 'The attempt to write to file ''#''' //
     .                 ' failed. IOSTAT = #.'                )
         CALL ERRFNM ( '#', TEXT                             )
         CALL ERRINT ( '#', IOSTAT                           )
         CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
         CALL CHKOUT ( 'DAFB2T'                              )
         RETURN
 
      END IF
 
C
C     Close only the binary file.
C
      CALL DAFCLS ( HANDLE )
 
      CALL CHKOUT ( 'DAFB2T' )
      RETURN
      END
