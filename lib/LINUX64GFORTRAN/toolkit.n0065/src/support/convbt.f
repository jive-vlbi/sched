
C$ Procedure CONVBT ( Convert Kernel file from binary to text )

      SUBROUTINE CONVBT ( BINFIL, TXTFIL )

C$ Abstract
C
C     Convert a SPICE binary file to an equivalent text file format.
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
C     CONVERSION
C     FILES
C     UTILITY
C
C$ Declarations

      CHARACTER*(*)         BINFIL
      CHARACTER*(*)         TXTFIL

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      BINFIL    I   Name of an existing SPICE binary file.
C      TXTFIL    I   Name of the text file to be created.
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
C     None.
C
C$ Exceptions
C
C     1)
C
C$ Particulars
C
C     This routine accepts as inputs the name of a binary file to be
C     converted to text and the name of the text file to be created.
C     The binary file must already exist and the text file must not 
C     exist for this routine to work correctly. The architecture and the 
C     file type are determined and then an appropriate file conversion 
C     is performed.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1)
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
C-    Beta Version 3.2.0, 30-AUG-1994 (KRG)
C
C        Improved the error diagnostics when incorrect inputs are 
C        provided, e.g., a transfer filename instead of a binary kernel 
C        filename.
C
C-    Beta Version 3.1.0, 12-AUG-1994 (KRG)
C
C        Fixed a minor bug that would occur when formatting a long error 
C        message. ERRFNM was called with a logical unit that had already 
C        been closed. 
C
C-    Beta Version 3.0.0, 22-APR-1994 (KRG)
C
C        Made updates to the routine to make use of the new SPICE 
C        capability of determining binary kernel file types at run time.
C        
C        Removed the arguments for the file architecture and file type
C        from the calling list. This information was no longer 
C        necessary.
C
C        Rearranged some of the code to make it easier to understand.
C
C        Added a new error: if the architecture or type are not 
C        recognized, we can't process the file.
C
C-    Beta Version 2.0.0, 28-JAN-1994 (KRG)
C
C-&

C$ Index_Entries
C
C     convert binary SPICE files to text
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
C     Begin and end markers in the file for the comment area.
C
      CHARACTER*(*)         BCMARK
      PARAMETER           ( BCMARK ='~NAIF/SPC BEGIN COMMENTS~')

      CHARACTER*(*)         ECMARK
      PARAMETER           ( ECMARK ='~NAIF/SPC END COMMENTS~'  )
C
C     File types that are recognized.
C
      CHARACTER*(*)         CKTYP
      PARAMETER           ( CKTYP  = 'CK' )

      CHARACTER*(*)         PCKTYP
      PARAMETER           ( PCKTYP = 'PCK' )

      CHARACTER*(*)         SPKTYP
      PARAMETER           ( SPKTYP = 'SPK' )
C
C     Length of a file architecture.
C
      INTEGER               ARCLEN
      PARAMETER           ( ARCLEN = 3 )
C
C     Maximum length for a file type.
C
      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN = 4 )
C
C     Local variables
C
      CHARACTER*(ARCLEN)    FARCH
      CHARACTER*(TYPLEN)    FTYPE

      INTEGER               HANDLE
      INTEGER               TXTLUN
      INTEGER               IOSTAT

      LOGICAL               COMNTS
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CONVBT' )
      END IF
C
C     Initialize the file architecture and the file type. 
C
      FARCH  = ' '
      FTYPE  = ' '
C
C     Get the architecture and type of the binary file.
C
      CALL GETFAT ( BINFIL, FARCH, FTYPE )

      IF ( FAILED() ) THEN
C
C        If there was an error getting the file architecture, just
C        return. An appropriate error message should have been set.
C        So, all we need to do here is return to the caller.
C
         CALL CHKOUT ( 'CONVBT' )
         RETURN

      END IF
C
C     Check to see that we got back a valid architecture and type.
C
C
C     Open the text file for output, obtaining a Fortran logical
C     unit.
C
      CALL TXTOPN ( TXTFIL, TXTLUN )

      IF ( FAILED() ) THEN
C
C        If there was an error opening the text file, just return.
C        An appropriate error message should have been set by TXTOPN.
C        So, all we need to do here is return to the caller.
C
         CALL CHKOUT ( 'CONVBT' )
         RETURN

      END IF
C
C     Process the files based on their binary architectures
C
      IF ( FARCH .EQ. 'DAF' ) THEN
C
C        If the file is a NAIF SPK, CK, or PCK binary file, it may have 
C        a comment area. So set the COMNTS flag appropriately.
C
         COMNTS = FTYPE .EQ. SPKTYP
         COMNTS = COMNTS .OR. ( FTYPE .EQ. CKTYP  )
         COMNTS = COMNTS .OR. ( FTYPE .EQ. PCKTYP )
C
C        First, convert the data portion of the binary file to text.
C        We only support the latest and greatest text file format for
C        conversion of the binary files to text.
C
         CALL DAFBT ( BINFIL, TXTLUN )

         IF ( FAILED() ) THEN
C
C           If an error occurred while attempting to convert the
C           data portion of the DAF file to text, we need to close
C           the text file and return to the caller. We will delete
C           the text file when we close it.
C
            CLOSE       ( TXTLUN, STATUS='DELETE' )
            CALL CHKOUT ( 'CONVBT'                )
            RETURN

         END IF
C
C        The DAF file may or may not have a comment area. If it is a 
C        NAIF SPICE kernel file, then it does and we need to deal with 
C        it. Otherwise we do nothing.
C
         IF ( COMNTS ) THEN
C
C           We need to open the binary DAF file so that we can extract
C           the comments from its comment area and place them in the
C           text file.
C
            CALL DAFOPR ( BINFIL, HANDLE )

            IF ( FAILED() ) THEN
C
C              If an error occurred, we need to close the text file and
C              return to the caller. We will delete the text file when
C              we close it.
C
               CLOSE       ( TXTLUN, STATUS='DELETE' )
               CALL CHKOUT ( 'CONVBT'                )
               RETURN

            END IF
C
C           Write the begin comments marker to the text file.
C
            WRITE ( TXTLUN, *, IOSTAT=IOSTAT ) BCMARK

            IF ( IOSTAT .NE. 0 ) THEN
C
C              An error occurred, so close both the text and binary
C              files, set an appropriate error message, and return to
C              the caller. The text file is deleted when it is closed.
C
               CLOSE       ( TXTLUN, STATUS='DELETE'                )
               CALL DAFCLS ( HANDLE                                 )
               CALL SETMSG ( 'Error writing the begin comments'    //
     .                       ' marker to the text file: #.'        //
     .                       ' IOSTAT = #.'                         )
               CALL ERRCH  ( '#', TXTFIL                            )
               CALL ERRINT ( '#', IOSTAT                            )
               CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
               CALL CHKOUT ( 'CONVBT'                               )
               RETURN

            END IF
C
C           Extract the comment area of the binary file to the text
C           file.
C
            CALL SPCEC  ( HANDLE, TXTLUN )

            IF ( FAILED() ) THEN
C
C              If the comment extraction failed, then an appropriate
C              error message should have already been set, so close
C              the text and binary files and return to the caller. The
C              text file is deleted when it is closed.
C
               CLOSE       ( TXTLUN, STATUS='DELETE' )
               CALL CHKOUT ( 'CONVBT'                )
               RETURN

            END IF
C
C           Write the end comments marker.
C
            WRITE ( TXTLUN, *, IOSTAT=IOSTAT ) ECMARK

            IF ( IOSTAT .NE. 0 ) THEN
C
C              An error occurred, so close both the text and binary
C              files, set an appropriate error message, and return to
C              the caller. The text file is deleted when it is closed.
C
               CLOSE       ( TXTLUN, STATUS='DELETE'                )
               CALL DAFCLS ( HANDLE                                 )
               CALL SETMSG ( 'Error writing the end comments'      //
     .                       ' marker to the text file: #.'        //
     .                    ' IOSTAT = #.'                            )
               CALL ERRCH  ( '#', TXTFIL                            )
               CALL ERRINT ( '#', IOSTAT                            )
               CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
               CALL CHKOUT ( 'CONVBT'                               )
               RETURN

            END IF
C
C           Close the binary DAF file that we opened to extract the
C           comments.
C
            CALL DAFCLS ( HANDLE )

         END IF

      ELSE IF ( FARCH .EQ. 'DAS' ) THEN
C
C        DAS files are easy. Everything is integrated into the files
C        so we do not need to worry about comments or reserved records 
C        or anything. We just convert it.
C
C        Convert the data portion of the binary file to text. We
C        only support the latest and greatest text file format for
C        conversion of the binary files to text.
C
         CALL DASBT ( BINFIL, TXTLUN )

         IF ( FAILED() ) THEN
C
C           If an error occurred while attempting to convert the
C           DAS file to text, we need to close the text file and
C           return to the caller. We will delete the text file
C           when we close it.
C
            CLOSE       ( TXTLUN, STATUS='DELETE' )
            CALL CHKOUT ( 'CONVBT'                )
            RETURN

         END IF

      ELSE IF ( FARCH .EQ. 'XFR' ) THEN
C
C        This is an error case, most likely caused by reading a transfer
C        file by accident. So signal an appropriate error.
C
         CLOSE       ( TXTLUN, STATUS='DELETE' )
         CALL SETMSG ( 'The file ''#'' appears to be a transfer' //
     .                 ' file and not a binary kernel file.'      )
         CALL ERRCH  ( '#', BINFIL                                )
         CALL SIGERR ( 'SPICE(NOTABINARYKERNEL)'                  )
         CALL CHKOUT ( 'CONVBT'                                   )
         RETURN

      ELSE IF ( FARCH .EQ. 'DEC' ) THEN
C
C        This is an error case, most likely caused by reading a transfer
C        file by accident. So signal an appropriate error.
C
         CLOSE       ( TXTLUN, STATUS='DELETE' )
         CALL SETMSG ( 'The file ''#'' appears to be a decimal' //
     .                 ' transfer file and not a binary kernel' //
     .                 ' file.'                                  )
         CALL ERRCH  ( '#', BINFIL                               )
         CALL SIGERR ( 'SPICE(NOTABINARYKERNEL)'                 )
         CALL CHKOUT ( 'CONVBT'                                  )
         RETURN

      ELSE
C
C        This is the catch all error case. At this point, we didn't 
C        match any of the files whose architecture and types are 
C        recognized. So, we toss our hands in the air and signal an 
C        error.
C
         CLOSE       ( TXTLUN, STATUS='DELETE' )
         CALL SETMSG ( 'The architecture and type of the file'     //
     .                 ' ''#'' were not recognized.'                )
         CALL ERRCH  ( '#', BINFIL                                  )
         CALL SIGERR ( 'SPICE(BADFILEFORMAT)'                       )
         CALL CHKOUT ( 'CONVBT'                                     )
         RETURN

      END IF
C
C     Close the text file that was created.
C
      CLOSE ( TXTLUN )

      CALL CHKOUT ( 'CONVBT' )
      RETURN
      END
