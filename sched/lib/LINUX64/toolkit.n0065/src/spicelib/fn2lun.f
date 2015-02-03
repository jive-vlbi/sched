C$Procedure      FN2LUN ( Map name of open file to its logical unit. )
 
      SUBROUTINE FN2LUN ( FILNAM, LUNIT )
 
C$ Abstract
C
C      Map the name of an open file to its associated logical unit.
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
C      FILES
C
C$ Declarations
 
      CHARACTER*(*)    FILNAM
      INTEGER          LUNIT
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FILNAM     I   Name of the file to be mapped to its logical unit.
C     LUNIT      O   The logical unit associated with the filename.
C
C$ Detailed_Input
C
C     FILNAM   is the filename that is to be mapped to its associated
C              Fortran logical unit.
C
C$ Detailed_Output
C
C     LUNIT    is the Fortran logical unit that is associated with the
C              filename FILNAM. The file must be open for this routine
C              to work properly.
C
C$ Parameters
C
C     None.
C
C
C$ Exceptions
C
C     1) If the filename is blank, the error SPICE(BLANKFILENAME) will
C        be signalled.
C
C     2) If an error occurs during the execution of the Fortran INQUIRE
C        statement, the error SPICE(INQUIREFAILED) is signalled.
C
C     3) If the filename is not associated with an open file, the
C        error SPICE(FILENOTOPEN) will be signalled.
C
C     4) If the filename is not associated with an existing file, the
C        error SPICE(FILEDOESNOTEXIST) will be signalled.
C
C     5) In the event of an error the contents of the variable LUNIT
C        are not defined, and should not be used.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Use the Fortran INQUIRE statement to determine the filename
C     that is associated with the Fortran logical unit LUNIT.
C
C$ Examples
C
C     The following code fragment illustrates the use of FN2LUN.
C
C     C
C     C      Convert the logical unit to its filename and display it.
C     C
C            CALL FN2LUN ( FNAME, LUNIT )
C            WRITE (*,*) 'The logical unit is: ', LUNIT
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer  (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 16-AUG-1994 (KRG)
C
C-&
 
C$ Index_Entries
C
C     map filename to logical unit
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               IOSTAT
 
      LOGICAL               EXISTS
      LOGICAL               OPENED
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'FN2LUN' )
      END IF
 
C
C     First we test to see if the filename is blank.
C
      IF ( FILNAM .EQ. ' ' ) THEN
 
         CALL SETMSG ( 'The filename is blank.' )
         CALL SIGERR ( 'SPICE(BLANKFILENAME)'   )
         CALL CHKOUT ( 'FN2LUN'                 )
         RETURN
 
      END IF
 
C
C     So simple, it defies explanation: just INQUIRE.
C
      INQUIRE ( FILE   = FILNAM,
     .          NUMBER = LUNIT,
     .          EXIST  = EXISTS,
     .          OPENED = OPENED,
     .          IOSTAT = IOSTAT  )
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL SETMSG ( 'INQUIRE error on file ''#''.'  //
     .                 ' The value of IOSTAT is: #.'    )
         CALL ERRCH  ( '#', FILNAM                      )
         CALL ERRINT ( '#', IOSTAT                      )
         CALL SIGERR ( 'SPICE(INQUIREFAILED)'           )
         CALL CHKOUT ( 'FN2LUN'                         )
         RETURN
 
      END IF
 
C
C     A file cannot be open if it does not exist. We need to check this
C     because for some environments files are considered to be open if
C     they do not exist.
C
      IF ( .NOT. EXISTS ) THEN
 
         CALL SETMSG ( 'No file with the name ''#'' was found.' )
         CALL ERRCH  ( '#', FILNAM                              )
         CALL SIGERR ( 'SPICE(FILEDOESNOTEXIST)'                )
         CALL CHKOUT ( 'FN2LUN'                                 )
         RETURN
 
      END IF
 
C
C     Now check to see if the file is opened. If not, then it is an
C     error, there cannot be a logical unit associated with it..
C
      IF ( .NOT. OPENED ) THEN
 
         CALL SETMSG ( 'There was not an open file associated with' //
     .                 ' the filename ''#''.'                        )
         CALL ERRCH  ( '#', FILNAM                                   )
         CALL SIGERR ( 'SPICE(FILENOTOPEN)'                          )
         CALL CHKOUT ( 'FN2LUN'                                      )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'FN2LUN' )
      RETURN
      END
