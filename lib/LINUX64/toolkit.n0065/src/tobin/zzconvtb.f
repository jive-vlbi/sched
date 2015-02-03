C$ Procedure ZZCONVTB ( Convert kernel file from text to binary )

      SUBROUTINE ZZCONVTB ( TXTFIL, ARCH, TYPE, BINFIL, NUMBER )

C$ Abstract
C
C     Convert a SPICE text file into its equivalent binary format.
C
C     NOTE: This routine is currently for use ONLY with the SPACIT
C           utility program. Use it at your own risk.
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
C     FILES
C
C$ Declarations

      CHARACTER*(*)         TXTFIL
      CHARACTER*(*)         BINFIL
      CHARACTER*(*)         ARCH
      CHARACTER*(*)         TYPE
      INTEGER               NUMBER

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TXTFIL     I   Name of text file to be converted.
C     BINARY     I   Name of a binary file to be created.
C
C$ Detailed_Input
C
C     None.
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
C     1)  This routine uses a Fortran scratch file to temporarily
C         store any lines of comments.
C
C$ Exceptions
C
C     1) If there is a problem opening or writing to the binary
C        file, a routine that ZZCONVTB calls diagnoses and signals
C        an error.
C
C     2) If there is a problem reading from the text file, the
C        error SPICE(FILEREADFAILED) is signalled.
C
C     3) If there is a problem opening the scratch file, the error
C        SPICE(FILEOPENERROR) is signalled.
C
C     4) If there is a problem writing to the scratch file, the
C        error SPICE(FILEWRITEFAILED) is signalled.
C
C     5) If the binary file archictecture is not recognized, the error
C        SPICE(UNSUPPBINARYARCH) will be signalled.
C
C     7) If the transfer file format is not recognized, the error
C        SPICE(NOTATRANSFERFILE) will be signalled.
C
C     8) If the input file format cannot be identified, the error
C        SPICE(UNRECOGNIZABLEFILE) will be signalled..
C
C$ Particulars
C
C     This routine is currently only for use with the SPACIT program.
C
C$ Examples
C
C
C
C$ Restrictions
C
C     1)  This routine assumes that the data and comments in the
C         text format SPK, PCK or CK file come from a binary file
C         and were written by one of the SPICELIB binary to text
C         conversion routines. Data and/or comments written any
C         other way may not be in the correct format and, therefore,
C         may not be handled properly.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer  (JPL)
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 20-MAR-1999 (EDW)
C
C        This routine is a modification of the CONVTB routine.
C        Both have the same basic functionality, but this routine
C        takes the unit number of the text file opened by ZZGETFAT,
C        the architecture, and file type as input.  ZZCONVTB does
C        not open the file, ZZGETFAT performs that function.
C
C-&

C$ Index_Entries
C
C     convert text SPICE files to binary
C
C-&

C
C     SPICELIB functions
C
      INTEGER               LTRIM
      INTEGER               RTRIM

      LOGICAL               FAILED
      LOGICAL               RETURN
C
C     Local parameters
C
C     Begin and end markers in the file for the comment area.
C
      CHARACTER*(*)         BCMARK
      PARAMETER           ( BCMARK ='~NAIF/SPC BEGIN COMMENTS~')

      CHARACTER*(*)         ECMARK
      PARAMETER           ( ECMARK ='~NAIF/SPC END COMMENTS~'  )
C
C     Maximum length of an input text line.
C
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 255 )
C
C     Maximum length of a file architecture.
C
      INTEGER               ARCHLN
      PARAMETER           ( ARCHLN = 3 )
C
C     Maximum length of a file type.
C
      INTEGER               TYPELN
      PARAMETER           ( TYPELN = 4 )
C
C     Number of reserved records to use when creating a binar DAF file.
C
      INTEGER               RESREC
      PARAMETER           ( RESREC = 0 )
C
C     Local variables
C
      CHARACTER*(LINLEN)    LINE

      INTEGER               HANDLE
      INTEGER               IOSTAT
      INTEGER               SCRLUN

      LOGICAL               HAVCOM
      LOGICAL               EOC
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZCONVTB' )
      END IF


C
C     Process the file based on the derived architecture and type.
C
       IF ( ( ARCH .EQ. 'XFR' ) .AND. ( TYPE .EQ. 'DAF' ) ) THEN
C
C        We got a DAF file.
C
C        Convert the data portion of the text file to binary. At this
C        point, we know that we have a current DAF text file format.
C
C        We expect to have comments.
C
         HAVCOM = .TRUE.
C
C        Convert it.
C
         CALL DAFTB ( NUMBER, BINFIL )

         IF ( FAILED() ) THEN
C
C           If there was an error then we need to close the
C           text file, and then check out and return to the
C           caller.
C
            CLOSE       ( NUMBER     )
            CALL CHKOUT ( 'ZZCONVTB' )
            RETURN

         END IF

      ELSE IF ( ( ARCH .EQ. 'XFR' ) .AND. ( TYPE .EQ. 'DAS' ) ) THEN
C
C        We got a DAS file. So we should begin converting it to binary.
C        DAS files are easier: all we do is call one routine.
C
C        We do not have comments. Actually, we might but they are
C        included as part of the DAS file conversion process.
C
         HAVCOM = .FALSE.
C
C        Convert it.
C
         CALL DASTB ( NUMBER, BINFIL )

         IF ( FAILED() ) THEN
C
C           If there was an error then we need to close the
C           text file, and then check out and return to the
C           caller.
C
            CLOSE       ( NUMBER     )
            CALL CHKOUT ( 'ZZCONVTB' )
            RETURN

         END IF

      ELSE IF ( ARCH .EQ. 'DAS' ) THEN
C
C        This is an error case, most likely caused by reading a binary
C        DAS file by accident. So signal an appropriate error.
C
         CALL SETMSG ( 'The file ''#'' appears to be a binary' //
     .                 ' DAS file and not a transfer file.'     )
         CALL ERRCH  ( '#', TXTFIL                              )
         CALL SIGERR ( 'SPICE(NOTATRANSFERFILE)'                )
         CALL CHKOUT ( 'ZZCONVTB'                               )
         RETURN

      ELSE IF ( ( ARCH .EQ. 'DAS' ) .AND. ( TYPE .EQ. 'PRE' ) ) THEN
C
C        This is an error case, most likely caused by reading a binary
C        DAS file by accident. So signal an appropriate error.
C
         CLOSE       ( NUMBER                                  )
         CALL SETMSG ( 'The file ''#'' appears to be a'       //
     .                 ' pre-release binary DAS file and not' //
     .                 ' a transfer file.'                     )
         CALL ERRCH  ( '#', TXTFIL                             )
         CALL SIGERR ( 'SPICE(NOTATRANSFERFILE)'               )
         CALL CHKOUT ( 'ZZCONVTB'                              )
         RETURN

      ELSE IF ( ARCH .EQ. 'DAF' ) THEN
C
C        This is an error case, most likely caused by reading a binary
C        DAF file by accident. So signal an appropriate error.
C
         CALL SETMSG ( 'The file ''#'' appears to be a binary' //
     .                 ' DAF file and not a transfer file.'     )
         CALL ERRCH  ( '#', TXTFIL                              )
         CALL SIGERR ( 'SPICE(NOTATRANSFERFILE)'                )
         CALL CHKOUT ( 'ZZCONVTB'                               )
         RETURN

      ELSE IF ( ( ARCH .EQ. 'DEC') .AND. ( TYPE .EQ. 'DAF' ) ) THEN
C
C        This is the case for the old text file format. It has no
C        identifying marks whatsoever, so we simply have to try and
C        convert it.
C
C        We expect to have comments.
C
         HAVCOM = .TRUE.
C
C        Back up one record so that we are positioned in the file where
C        we were when this routine was entered.
C
         BACKSPACE   ( NUMBER )
C
C        Convert it.
C
         CALL DAFT2B ( NUMBER, BINFIL, RESREC )

         IF ( FAILED() ) THEN
C
C           If there was an error then we need to close the text
C           file, and then check out and return to the caller.
C
            CLOSE       ( NUMBER     )
            CALL CHKOUT ( 'ZZCONVTB' )
            RETURN

         END IF

      ELSE
C
C        This is the catch all error case. At this point, we didn't
C        match any of the files whose architecture and types are
C        recognized. So, we toss our hands in the air and signal an
C        error.
C
         CALL SETMSG ( 'The architecture and type of the file'  //
     .                 ' ''#''could not be determined.'          )
         CALL ERRCH  ( '#', TXTFIL                               )
         CALL SIGERR ( 'SPICE(UNRECOGNIZABLEFILE)'               )
         CALL CHKOUT ( 'ZZCONVTB'                                )
         RETURN

      END IF
C
C     If we have comments to process, then process them.
C
      IF ( HAVCOM ) THEN
C
C        There are three situations that we need to consider here:
C
C           1) We have a SPICE text file with comments. This implies
C              that we have a bunch of comments to be put into the
C              comment area that are surrounded by the begin comments
C              marker, BCMARK, and the end comemnts marker, ECMARK.
C
C           2) We are at the end of the file. This means that we have
C              an old SPICE kernel file, from the good old days before
C              the comment area was implemented, or we ahve a plain old
C              ordinary DAF file.
C
C           3) We are not at the end of the file, but there are no
C              comments. This means a text DAF file may be embedded
C              in a larger text file or something. PDS does things like
C              this: SFDUs and such.
C
C        So, we need to look out for and deal with each of these
C        possibilities.
C
         READ ( NUMBER, FMT='(A)', IOSTAT=IOSTAT ) LINE

         IF ( IOSTAT .GT. 0 ) THEN
C
C           If there was an error then we need to close the text
C           file, and then check out and return to the caller.
C
            CLOSE       ( NUMBER                             )
            CALL SETMSG ( 'Error reading the text file: #.' //
     .                    ' IOSTAT = #.'                     )
            CALL ERRCH  ( '#', TXTFIL                        )
            CALL ERRINT ( '#', IOSTAT                        )
            CALL SIGERR ( 'SPICE(FILEREADFAILED)'            )
            CALL CHKOUT ( 'ZZCONVTB'                         )
            RETURN

         END IF
C
C        If we encountered the end of the file, just check out and
C        return. This is not an error.
C
         IF ( IOSTAT .LT.  0 ) THEN

            CALL CHKOUT ( 'ZZCONVTB' )
            RETURN

         END IF
C
C        We got a line, so left justify it and see if it matches the
C        begin comments marker. If not, then use the Fortran BACKSPACE
C        command to reposition the file pointer to be ready to read the
C        line we just read.
C
         IF ( LINE ( LTRIM(LINE): ) .NE. BCMARK ) THEN

            BACKSPACE   ( NUMBER     )
            CALL CHKOUT ( 'ZZCONVTB' )
            RETURN

         END IF
C
C        We're not at the end of the file, and the line we read
C        is BCMARK, so we write the comments to a scratch file.
C        We do this because we have to use SPCAC to add the comments
C        to the comment area of the binary file, and SPCAC rewinds
C        the file. It's okay for SPCAC to rewind a scratch file, since
C        it will probably not be very big, but it's not okay to rewind
C        the file connected to NUMBER -- we don't know the initial
C        location of the file pointer or how big the file is.
C
         CALL GETLUN ( SCRLUN )
         OPEN ( UNIT            =  SCRLUN,
     .          FORM            = 'FORMATTED',
     .          ACCESS          = 'SEQUENTIAL',
     .          STATUS          = 'SCRATCH',
     .          IOSTAT          =  IOSTAT      )

         IF ( IOSTAT .NE. 0 ) THEN
C
C           If there was an error then we need to close the text
C           file, and then check out and return to the caller.
C
            CLOSE       ( SCRLUN                               )
            CLOSE       ( NUMBER                               )
            CALL SETMSG ( 'Error opening temporary file.'     //
     .                    ' IOSTAT = #.'                       )
            CALL ERRINT ( '#', IOSTAT                          )
            CALL SIGERR ( 'SPICE(FILEOPENERROR)'               )
            CALL CHKOUT ( 'ZZCONVTB'                           )
            RETURN

         END IF
C
C        Continue reading lines from the text file and storing them
C        in the scratch file until we get to the end marker. We do not
C        write the begin and end markers to the scratch file. We do not
C        need them.
C
         EOC = .FALSE.

         DO WHILE ( .NOT. EOC )

            READ ( NUMBER, FMT='(A)', IOSTAT=IOSTAT ) LINE

            IF ( IOSTAT .NE. 0 ) THEN
C
C              If there was an error then we need to close the
C              scratch file, the text file, and then check out
C              and return to the caller.
C
               CLOSE       ( SCRLUN                              )
               CLOSE       ( NUMBER                              )
               CALL SETMSG ( 'Error reading the text file: #.'  //
     .                       ' IOSTAT = #.'                      )
               CALL ERRCH  ( '#', TXTFIL                         )
               CALL ERRINT ( '#', IOSTAT                         )
               CALL SIGERR ( 'SPICE(FILEREADFAILED)'             )
               CALL CHKOUT ( 'ZZCONVTB'                          )
               RETURN

            END IF
C
C           If we are not at the end of the comments, then write the
C           line ot the scratch file. Otherwise set the end of comments
C           flag to .TRUE..
C
            IF ( LINE ( LTRIM(LINE): ) .NE. ECMARK ) THEN

               WRITE ( SCRLUN, FMT='(A)', IOSTAT=IOSTAT )
     .                                           LINE(1:RTRIM(LINE))

               IF ( IOSTAT .NE. 0 ) THEN
C
C                 If there was an error then we need to close the
C                 scratch file, the text file, and then check out
C                 and return to the caller.
C
                  CLOSE       ( SCRLUN                                 )
                  CLOSE       ( NUMBER                                 )
                  CALL SETMSG ( 'Error writing to temporary file.'    //
     .                          ' IOSTAT = #.'                         )
                  CALL ERRINT ( '#', IOSTAT                            )
                  CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
                  CALL CHKOUT ( 'ZZCONVTB'                             )
                  RETURN

               END IF

            ELSE

               EOC = .TRUE.

            END IF

         END DO
C
C        Open the new binary file and add the comments that have been
C        stored temporarily in a scratch file.
C
         CALL DAFOPW ( BINFIL, HANDLE )

         IF ( FAILED() ) THEN
C
C           If there was an error then we need to close the scratch
C           file and the text file, and then check out and return to
C           the caller.
C
            CLOSE       ( SCRLUN     )
            CLOSE       ( NUMBER     )
            CALL CHKOUT ( 'ZZCONVTB' )
            RETURN

         END IF

         CALL SPCAC  ( HANDLE, SCRLUN, ' ', ' ' )

         IF ( FAILED() ) THEN
C
C           If there was an error then we need to close the scratch
C           file and the text file, and then check out and return to
C           the caller.
C
            CLOSE       ( SCRLUN     )
            CLOSE       ( NUMBER     )
            CALL DAFCLS ( HANDLE     )
            CALL CHKOUT ( 'ZZCONVTB' )
            RETURN

         END IF
C
C        We succeeded, so close the files we opened to deal with the
C        comments. The scratch file is automatically deleted.
C
         CLOSE       ( SCRLUN )
         CALL DAFCLS ( HANDLE )

      END IF
C
C     Close the transfer file. We know it is open, because we got here.
C
      CLOSE       ( NUMBER )

      CALL CHKOUT ( 'ZZCONVTB' )
      RETURN
      END
