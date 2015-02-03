C$Procedure      DELFIL ( Delete a file  )
 
      SUBROUTINE DELFIL ( FILNAM )
 
C$ Abstract
C
C     Delete a file.
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
C     UTILITY
C
C$ Declarations
C
      CHARACTER*(*)         FILNAM
C
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      FILNAM     I   The name of a file to be deleted.
C
C$ Detailed_Input
C
C     FILNAM      is the name of a file that is to be deleted. Upon
C                 successful completion of this routine this file will
C                 no longer exist. The file to be deleted must be closed
C                 when this routine is called.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None
C
C$ Exceptions
C
C      1)  If the file name is blank, the error SPICE(BLANKFILENAME)
C          is signalled.
C
C      2)  If the inquire on the filename specified by FILNAM fails for
C          some reason, the error SPICE(INQUIREERROR) will be signalled.
C
C      3)  If the file specified by FILNAM is already open, the error
C          SPICE(FILECURRENTLYOPEN) will be signalled.
C
C      4)  If the file specified by FILNAM does not exist, the error
C          SPICE(NOSUCHFILE) will be signalled.
C
C      5)  If the attempt to open the file specified by FILNAM fails,
C          the error SPICE(FILEOPENFAILED) will be signalled.
C
C      6)  If the attempt to close the file with STATUS='DELETE' fails
C          the error SPICE(FILEDELETEFAILED) will be signalled.
C
C$ Files
C
C     The  file specified by FILNAM is opened and then closed by this
C     routine with STATUS = 'DELETE' to delete it. The file must be
C     closed for this routine to delete it.
C
C$ Particulars
C
C     This subroutine is a support utility that deletes a file.
C
C$ Examples
C
C     Suppose you wish to delete a file named 'delete.me' in the
C     current directory. The code fragment below would accomplish this.
C
C        FILE = 'delete.me'
C        CALL DELFIL ( FILE )
C
C$ Restrictions
C
C     The file to be deleted must be closed when this routine is
C     invoked.
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
C-     SPICELIB Version 1.0.0, 20-DEC-1995 (KRG)
C
C-&
 
C
C$ Index_Entries
C
C     delete a file
C
C-&
 
C
C     Spicelib Routines
C
      LOGICAL               RETURN
C
C     Local Variables
C
      INTEGER               IOSTAT
      INTEGER               LUNIT
 
      LOGICAL               EXISTS
      LOGICAL               OPENED
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'DELFIL' )
      END IF
C
C     Check to see if the filename we got is blank. If it is, signal an
C     error and return.
C
      IF ( FILNAM .EQ. ' ' ) THEN
 
         CALL SETMSG ( 'The file name is blank.' )
         CALL SIGERR ( 'SPICE(BLANKFILENAME)'    )
         CALL CHKOUT ( 'DELFIL'                  )
         RETURN
 
      END IF
C
C     We inquire before we try opening anything to see if the file
C     exists or is currently open.
C
      INQUIRE ( FILE   = FILNAM,
     .          EXIST  = EXISTS,
     .          IOSTAT = IOSTAT,
     .          OPENED = OPENED  )
C
C     Not too likely, but if the INQUIRE statement fails signal an error
C     and return.
C
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL SETMSG ( 'INQUIRE statement failed for file ''#''.' //
     .                 ' IOSTAT = #.'                              )
         CALL ERRCH  ( '#', FILNAM                                 )
         CALL ERRINT ( '#', IOSTAT                                 )
         CALL SIGERR ( 'SPICE(INQUIREFAILED)'                      )
         CALL CHKOUT ( 'DELFIL'                                    )
         RETURN
 
      END IF
C
C     The file ought to exist if you're trying to delete it. If not,
C     signal an error and return.
C
      IF ( .NOT. EXISTS ) THEN
 
         CALL SETMSG ( 'The file ''#'' does not exist.' )
         CALL ERRCH  ( '#', FILNAM                      )
         CALL SIGERR ( 'SPICE(NOSUCHFILE)'              )
         CALL CHKOUT ( 'DELFIL'                         )
         RETURN
 
      END IF
C
C     The file that is to be deleted should not be in use, indicated by
C     it being open, by anything when we try to delete it. If it is
C     open, signal an error and return.
C
      IF ( OPENED ) THEN
 
         CALL SETMSG ( 'The file ''#'' is currently open and'  //
     .                 ' cannot be deleted.'                    )
         CALL ERRCH  ( '#', FILNAM                              )
         CALL SIGERR ( 'SPICE(FILECURRENTLYOPEN)'               )
         CALL CHKOUT ( 'DELFIL'                                 )
         RETURN
 
      END IF
C
C     Get an available logical unit and attempt to open the file.
C
      CALL GETLUN ( LUNIT )
 
      OPEN ( UNIT=LUNIT, FILE=FILNAM, STATUS='OLD', IOSTAT=IOSTAT )
C
C     If we had trouble opening the file, signal an appropriate error
C     and return.
C
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL SETMSG ( 'Attempt to open the file ''#'' failed.' )
         CALL ERRCH  ( '#', FILNAM                              )
         CALL SIGERR ( 'SPICE(FILEOPENFAILED)'                  )
         CALL CHKOUT ( 'DELFIL'                                 )
         RETURN
 
      END IF
C
C     We opened the file successfully, so let's try to close it with
C     STATUS = 'DELETE'. If this fails, attempt to just close the file,
C     signal an error and return.
C
      CLOSE ( LUNIT, STATUS='DELETE', IOSTAT=IOSTAT )
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CLOSE       ( LUNIT                                      )
         CALL SETMSG ( 'Attempt to delete the file ''#'' failed.' )
         CALL ERRCH  ( '#', FILNAM                                )
         CALL SIGERR ( 'SPICE(FILEDELETEFAILED)'                  )
         CALL CHKOUT ( 'DELFIL'                                   )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'DELFIL' )
      RETURN
 
      END
