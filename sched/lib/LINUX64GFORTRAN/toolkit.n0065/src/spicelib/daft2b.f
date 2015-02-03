C$Procedure DAFT2B ( DAF, text to binary )
 
      SUBROUTINE DAFT2B ( TEXT, BINARY, RESV )
 
C$ Abstract
C
C     Deprecated. The routine DAFTB supersedes this routine.
C     NAIF supports this routine only to provide backward 
C     compatibility.
C
C     Reconstruct a binary DAF from a text file opened by
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
 
      INTEGER               TEXT
      CHARACTER*(*)         BINARY
      INTEGER               RESV
 
      INTEGER               BSIZE
      PARAMETER           ( BSIZE = 1024 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TEXT       I   Logical unit connected to text file.
C     BINARY     I   Name of a binary DAF to be created.
C     RESV       I   Number of records to reserve.
C     BSIZE      P   Buffer size.
C
C$ Detailed_Input
C
C     TEXT        is a logical unit number, to which a text file has
C                 been connected by the calling program, and into
C                 which the contents of binary DAF have been
C                 written. The file pointer should be placed just
C                 before the file ID word.
C
C     BINARY      is the name of a binary DAF to be created.
C                 The binary DAF contains the same data as the
C                 text file, but in a form more suitable for use
C                 by application programs.
C
C     RESV        is the number of records to be reserved in the
C                 binary DAF.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     BSIZE       is the size of the buffer used to read array elements
C                 from the text file. No single group of elements should
C                 contains more than BSIZE elements.
C
C$ Exceptions
C
C     1) If for some reason the text file cannot be read,
C        the error SPICE(DAFREADFAIL) is signalled.
C
C     2) If the architecture of the file is not DAF, as specified by
C        the ID word, the error SPICE(NOTADAFFILE) will be signalled.
C
C     3) If the text file does not contain matching internal file
C        names, the error SPICE(DAFNOIFNMATCH) is signalled.
C
C     4) If the text file does not contain matching array names,
C        the error SPICE(DAFNONAMEMATCH) is signalled.
C
C     5) If the buffer size is not sufficient, the error
C        SPICE(DAFOVERFLOW) is signalled.
C
C$ Files
C
C     See arguments TEXT, BINARY.
C
C$ Particulars
C
C     This routine has been made obsolete by the new DAF text to binary
C     conversion routine DAFTB. This routine remains available for
C     reasons of backward compatibility. We strongly recommend that you
C     use the new conversion routines for any new software development.
C     Please see the header of the routine DAFTB for details.
C
C     This routine is necessary for converting older DAF text files into
C     their equivalent binary formats, as DAFTB uses a different text
C     file format that is incompatible with the text file format
C     expected by this routine.
C
C     Any binary DAF may be transferred between heterogeneous
C     Fortran environments by converting it to an equivalent file
C     containing only ASCII characters. Such a file can be transferred
C     almost universally, using any number of established protocols
C     (Kermit, FTP, and so on). Once transferred, the ASCII file can
C     be reconverted to a binary DAF, using the representations
C     native to the new host environment.
C
C     There are two pairs of routines that can be used to convert
C     DAFs between binary and ASCII. The first pair, DAFB2A
C     and DAFA2B, works with complete files. That is, DAFB2A creates
C     a complete ASCII file containing all of the information in
C     a particular binary DAF, and nothing else; this file can
C     be fed directly into DAFA2B to produce a complete binary DAF.
C     In each case, the names of the files are specified.
C
C     A related pair of routines, DAFB2T and DAFT2B, assume that
C     the ASCII data are to be stored in the midst of a text file.
C     This allows the calling program to surround the data with
C     standardized labels, to append several binary DAFs into a
C     single text file, and so on.
C
C     Note that you must select the number of records to be reserved
C     in the binary DAF. The contents of reserved records are ignored
C     by the normal transfer process.
C
C$ Examples
C
C     DAFB2A and DAFA2B are typically used for simple transfers.
C     If A.DAF is a binary DAF in environment 1, it can be transferred
C     to environment 2 in three steps.
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
C     DAFT2B cannot be executed while any other DAF is open
C     for writing.
C
C$ Literature_References
C
C     NAIF Document 167.0, "Double Precision Array Files (DAF)
C     Specification and User's Guide"
C
C$ Author_and_Institution
C
C     K. R. Gehringer (JPL)
C     J.E. McLean     (JPL)
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
C-    SPICELIB Version 3.0.0, 04-OCT-1993 (KRG)
C
C        Removed the error SPICE(DAFNOIDWORD) as it was no longer
C        relevant.
C
C        Added the error SPICE(NOTADAFFILE) if this routine is called
C        with a file that does not contain an ID word identifying the
C        file as a DAF file.
C
C        There were no checks of the IOSTAT variable after attempting to
C        read from the text file, a single test of the IOSTAT variable
C        was made at the end of the routine. This was not adequate to
C        detect errors when writing to the text file. So after all of
C        these read statements, an IF ... END IF block was added to
C        signal an error if IOSTAT .NE. 0.
C
C            IF ( IOSTAT .NE. 0 ) THEN
C
C               CALL SETMSG ( 'The attempt to read from file ''#''' //
C         .                   ' failed. IOSTAT = #.'                 )
C               CALL ERRFNM ( '#', UNIT                              )
C               CALL SIGERR ( 'SPICE(DAFREADFAIL)'                   )
C               CALL CHKOUT ( 'DAFT2B'                               )
C               RETURN
C
C            END IF
C
C        Removed the code from the end of the routine that purported to
C        check for read errors:
C
C            C
C            C     If any read screws up, they should all screw up. Why
C            C     make a billion separate checks?
C            C
C                  IF ( IOSTAT .NE. 0 ) THEN
C                     CALL SETMSG ( 'Value of IOSTAT was: #. ' )
C                     CALL ERRINT ( '#', IOSTAT                )
C                     CALL SIGERR ( 'SPICE(DAFREADFAIL)'       )
C                   END IF
C
C        The answer to the question is:
C
C            You have to do a billion separate checks because the IOSTAT
C            value is only valid for the most recently executed read.
C
C        Added a statment to the $ Particulars section to the effect
C        that this routine has been made obsolete by the introduction of
C        the routine DAFTB, and that we strongly recommend the use of
C        the new routine. This routine must, however, be used when
C        converting older text files to binary, as the old and new
C        formats are not compatible.
C
C        Modified the $ Abstract section to reflect the fact that this
C        routine is obsolete and maintained for purposes of backward
C        compatibility only.
C
C-    SPICELIB Version 2.0.2, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.1,  6-AUG-1990 (HAN)
C
C        Header documentation was corrected. This routine will
C        convert a file containing either ID word, 'NAIF/DAF' or
C        'NAIF/NIP'. (Previous versions of SPICELIB software used
C        the ID word 'NAIF/NIP'.)
C
C-    SPICELIB Version 2.0.0,  2-AUG-1990 (JEM)
C
C        The previous version of this routine always failed and
C        signalled the error SPICE(DAFNOIDWORD) because of a faulty
C        logical expression in an error-checking IF statement.
C        The error SPICE(DAFNOIDWORD) should be signalled if the
C        next non-blank line in the text file does not begin with the
C        word 'NAIF/DAF' AND does not begin with the word 'NAIF/NIP'.
C        Previously the logic was incorrect causing the error to be
C        signalled every time no matter what the word was. The
C        correction consisted of replacing '.OR.' with '.AND.'
C        in the logical expression.
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
C     text daf to binary
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
      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN = 8 )
 
C
C     Local variables
C
      CHARACTER*(1000)      NAME     ( 2 )
      CHARACTER*(IDWLEN)    IDWORD
      CHARACTER*(60)        IFNAME   ( 2 )
      CHARACTER*(IDWLEN)    TARCH
      CHARACTER*(IDWLEN)    TTYPE
 
      DOUBLE PRECISION      BUFFER   ( BSIZE )
      DOUBLE PRECISION      DC       ( 125 )
      DOUBLE PRECISION      SUM      ( 125 )
 
      INTEGER               CHUNK
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IC       ( 250 )
      INTEGER               IOSTAT
      INTEGER               ISIZE
      INTEGER               LSIZE
      INTEGER               MORE
      INTEGER               ND
      INTEGER               NI
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFT2B' )
      END IF
 
      IDWORD = ' '
      TARCH  = ' '
      TTYPE  = ' '
C
C     We should be positioned and ready to read the file ID word from
C     the text file, so let's try it.
C
      READ (TEXT,*,IOSTAT=IOSTAT) IDWORD
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL SETMSG ( 'The attempt to read from file ''#''' //
     .                 ' failed. IOSTAT = #.'                 )
         CALL ERRFNM ( '#', TEXT                              )
         CALL ERRINT ( '#', IOSTAT                            )
         CALL SIGERR ( 'SPICE(DAFREADFAIL)'                   )
         CALL CHKOUT ( 'DAFT2B'                               )
         RETURN
 
      END IF
C
C     Split the ID word into an architecture and type, and verify that
C     the architecture is 'DAF'. If it is not, this is the wrong
C     routine, and an error will be signalled.
C
      CALL IDW2AT ( IDWORD, TARCH, TTYPE )
 
      IF ( TARCH .NE. 'DAF' ) THEN
 
         CALL SETMSG ( 'File architecture is not ''DAF'' for' //
     .                 ' file ''#'''                           )
         CALL ERRFNM ( '#', TEXT                               )
         CALL SIGERR ( 'SPICE(NOTADAFFILE)'                    )
         CALL CHKOUT ( 'DAFT2B'                                )
         RETURN
 
      END IF
 
      READ (TEXT,*,IOSTAT=IOSTAT) ND, NI, IFNAME(1)
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL SETMSG ( 'The attempt to read from file ''#''' //
     .                 ' failed. IOSTAT = #.'                 )
         CALL ERRFNM ( '#', TEXT                              )
         CALL ERRINT ( '#', IOSTAT                            )
         CALL SIGERR ( 'SPICE(DAFREADFAIL)'                   )
         CALL CHKOUT ( 'DAFT2B'                               )
         RETURN
 
      END IF
 
C
C     Open the new binary file.
C
      CALL DAFOPN ( BINARY, ND, NI, IFNAME(1), RESV, HANDLE )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'DAFT2B' )
         RETURN
 
      END IF
C
C     Each array is preceded by a '1', which indicates that more
C     arrays are to come. The array itself begins with the name
C     and the summary components, and ends with the name again.
C     The contents are written in arbitrary chunks. The final
C     chunk is followed by a '0', which indicates that no chunks
C     remain. The names must match, or the array should not
C     be terminated normally.
C
C     If the chunks in the file are bigger than the local buffer
C     size, we are in trouble.
C
      LSIZE = ND + (NI - 1) / 2 + 1
      ISIZE = LSIZE * 8
 
      READ (TEXT,*,IOSTAT=IOSTAT) MORE
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL DAFCLS ( HANDLE                                 )
         CALL SETMSG ( 'The attempt to read from file ''#''' //
     .                 ' failed. IOSTAT = #.'                 )
         CALL ERRFNM ( '#', TEXT                              )
         CALL ERRINT ( '#', IOSTAT                            )
         CALL SIGERR ( 'SPICE(DAFREADFAIL)'                   )
         CALL CHKOUT ( 'DAFT2B'                               )
         RETURN
 
      END IF
 
      DO WHILE ( MORE .GT. 0 )
 
         READ (TEXT,*,IOSTAT=IOSTAT) NAME(1)( :ISIZE)
 
         IF ( IOSTAT .NE. 0 ) THEN
 
            CALL DAFCLS ( HANDLE                                 )
            CALL SETMSG ( 'The attempt to read from file ''#''' //
     .                    ' failed. IOSTAT = #.'                 )
            CALL ERRFNM ( '#', TEXT                              )
            CALL ERRINT ( '#', IOSTAT                            )
            CALL SIGERR ( 'SPICE(DAFREADFAIL)'                   )
            CALL CHKOUT ( 'DAFT2B'                               )
            RETURN
 
         END IF
 
         READ (TEXT,*,IOSTAT=IOSTAT) ( DC(I), I = 1, ND     )
 
         IF ( IOSTAT .NE. 0 ) THEN
 
            CALL DAFCLS ( HANDLE                                 )
            CALL SETMSG ( 'The attempt to read from file ''#''' //
     .                    ' failed. IOSTAT = #.'                 )
            CALL ERRFNM ( '#', TEXT                              )
            CALL ERRINT ( '#', IOSTAT                            )
            CALL SIGERR ( 'SPICE(DAFREADFAIL)'                   )
            CALL CHKOUT ( 'DAFT2B'                               )
            RETURN
 
         END IF
 
         READ (TEXT,*,IOSTAT=IOSTAT) ( IC(I), I = 1, NI - 2 )
 
         IF ( IOSTAT .NE. 0 ) THEN
 
            CALL DAFCLS ( HANDLE                                 )
            CALL SETMSG ( 'The attempt to read from file ''#''' //
     .                    ' failed. IOSTAT = #.'                 )
            CALL ERRFNM ( '#', TEXT                              )
            CALL ERRINT ( '#', IOSTAT                            )
            CALL SIGERR ( 'SPICE(DAFREADFAIL)'                   )
            CALL CHKOUT ( 'DAFT2B'                               )
            RETURN
 
         END IF
 
         CALL DAFPS  ( ND, NI, DC, IC, SUM           )
         CALL DAFBNA ( HANDLE, SUM, NAME(1)( :ISIZE) )
 
         IF ( FAILED () ) THEN
 
            CALL CHKOUT ( 'DAFT2B' )
            RETURN
 
         END IF
 
         READ (TEXT,*,IOSTAT=IOSTAT) CHUNK
 
         IF ( IOSTAT .NE. 0 ) THEN
 
            CALL DAFCLS ( HANDLE                                 )
            CALL SETMSG ( 'The attempt to read from file ''#''' //
     .                    ' failed. IOSTAT = #.'                 )
            CALL ERRFNM ( '#', TEXT                              )
            CALL ERRINT ( '#', IOSTAT                            )
            CALL SIGERR ( 'SPICE(DAFREADFAIL)'                   )
            CALL CHKOUT ( 'DAFT2B'                               )
            RETURN
 
         END IF
 
         DO WHILE ( CHUNK .GT. 0 )
 
            IF ( CHUNK .GT. BSIZE ) THEN
 
               CALL DAFCLS ( HANDLE                                 )
               CALL SETMSG ( 'Buffer size exceeded. Increase to #.' )
               CALL ERRINT ( '#', CHUNK                             )
               CALL SIGERR ( 'SPICE(DAFOVERFLOW)'                   )
               CALL CHKOUT ( 'DAFT2B'                               )
               RETURN
 
            ELSE
 
               READ (TEXT,*,IOSTAT=IOSTAT) ( BUFFER(I), I = 1, CHUNK )
 
               IF ( IOSTAT .NE. 0 ) THEN
 
                  CALL DAFCLS ( HANDLE                                 )
                  CALL SETMSG ( 'The attempt to read from file ''#''' //
     .                          ' failed. IOSTAT = #.'                 )
                  CALL ERRFNM ( '#', TEXT                              )
                  CALL ERRINT ( '#', IOSTAT                            )
                  CALL SIGERR ( 'SPICE(DAFREADFAIL)'                   )
                  CALL CHKOUT ( 'DAFT2B'                               )
                  RETURN
 
               END IF
 
               CALL DAFADA ( BUFFER, CHUNK )
 
               IF ( FAILED () ) THEN
 
                  CALL CHKOUT ( 'DAFT2B' )
                  RETURN
 
               END IF
 
            END IF
 
            READ (TEXT,*,IOSTAT=IOSTAT) CHUNK
 
            IF ( IOSTAT .NE. 0 ) THEN
 
               CALL DAFCLS ( HANDLE                                 )
               CALL SETMSG ( 'The attempt to read from file ''#''' //
     .                       ' failed. IOSTAT = #.'                 )
               CALL ERRFNM ( '#', TEXT                              )
               CALL ERRINT ( '#', IOSTAT                            )
               CALL SIGERR ( 'SPICE(DAFREADFAIL)'                   )
               CALL CHKOUT ( 'DAFT2B'                               )
               RETURN
 
            END IF
 
         END DO
 
         READ (TEXT,*,IOSTAT=IOSTAT) NAME(2)( :ISIZE)
 
         IF ( IOSTAT .NE. 0 ) THEN
 
            CALL DAFCLS ( HANDLE                                 )
            CALL SETMSG ( 'The attempt to read from file ''#''' //
     .                    ' failed. IOSTAT = #.'                 )
            CALL ERRFNM ( '#', TEXT                              )
            CALL ERRINT ( '#', IOSTAT                            )
            CALL SIGERR ( 'SPICE(DAFREADFAIL)'                   )
            CALL CHKOUT ( 'DAFT2B'                               )
            RETURN
 
         END IF
 
         IF ( NAME(1)( :ISIZE) .NE. NAME(2)( :ISIZE) ) THEN
 
            CALL DAFCLS ( HANDLE                          )
            CALL SETMSG ( 'Array name mismatch: # and #.' )
            CALL ERRCH  ( '#', NAME(1)( :ISIZE)           )
            CALL ERRCH  ( '#', NAME(2)( :ISIZE)           )
            CALL SIGERR ( 'SPICE(DAFNONAMEMATCH)'         )
            CALL CHKOUT ( 'DAFT2B'                        )
            RETURN
 
         ELSE
 
            CALL DAFENA
 
            IF ( FAILED () ) THEN
 
               CALL CHKOUT ( 'DAFT2B' )
               RETURN
 
            END IF
 
         END IF
 
         READ (TEXT,*,IOSTAT=IOSTAT) MORE
 
         IF ( IOSTAT .NE. 0 ) THEN
 
            CALL DAFCLS ( HANDLE                                 )
            CALL SETMSG ( 'The attempt to read from file ''#''' //
     .                    ' failed. IOSTAT = #.'                 )
            CALL ERRFNM ( '#', TEXT                              )
            CALL ERRINT ( '#', IOSTAT                            )
            CALL SIGERR ( 'SPICE(DAFREADFAIL)'                   )
            CALL CHKOUT ( 'DAFT2B'                               )
            RETURN
 
         END IF
 
      END DO
 
C
C     The final '0' indicates that no arrays remain. The first shall
C     be last: the internal file name brings up the rear. If it doesn't
C     match the one at the front, complain.
C
      READ (TEXT,*,IOSTAT=IOSTAT) IFNAME(2)
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL DAFCLS ( HANDLE                                 )
         CALL SETMSG ( 'The attempt to read from file ''#''' //
     .                 ' failed. IOSTAT = #.'                 )
         CALL ERRFNM ( '#', TEXT                              )
         CALL ERRINT ( '#', IOSTAT                            )
         CALL SIGERR ( 'SPICE(DAFREADFAIL)'                   )
         CALL CHKOUT ( 'DAFT2B'                               )
         RETURN
 
      END IF
 
      IF ( IFNAME(1) .NE. IFNAME(2) ) THEN
 
         CALL DAFCLS ( HANDLE                                 )
         CALL SETMSG ( 'Internal file name mismatch: # and #' )
         CALL ERRCH  ( '#', IFNAME(1)                         )
         CALL ERRCH  ( '#', IFNAME(2)                         )
         CALL SIGERR ( 'SPICE(DAFNOIFNMATCH)'                 )
         CALL CHKOUT ( 'DAFT2B'                               )
         RETURN
 
      END IF
 
C
C     Close the DAF file we just created.
C
      CALL DAFCLS ( HANDLE )
 
      CALL CHKOUT ( 'DAFT2B' )
      RETURN
      END
