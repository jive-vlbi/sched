 
C$Procedure      LUN2FN ( Map logical unit of open file to its name. )
 
      SUBROUTINE LUN2FN ( LUNIT, FILNAM )
 
C$ Abstract
C
C     Map the logical unit of an open file to its associated filename.
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
 
      INTEGER          LUNIT
      CHARACTER*(*)    FILNAM
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LUNIT      I   A logical unit to be mapped to a filename.
C     FILNAM     O   Name of the file associated with LUNIT.
C
C$ Detailed_Input
C
C     LUNIT    is the Fortran logical unit that is to be mapped to the
C              filename with which it is associated. The file must be
C              open for this routine to work properly.
C
C$ Detailed_Output
C
C     FILNAM   is the filename that is associated with the Fortran
C              logical unit LUNIT.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the logical unit is not positive, the error
C        SPICE(INVALIDARGUMENT) will be signalled.
C
C     2) If an error occurs during the execution of the Fortran INQUIRE
C        statement, the error SPICE(INQUIREFAILED) is signalled.
C
C     3) If the logical unit is not attached to an open file, the
C        error SPICE(FILENOTOPEN) will be signalled.
C
C     4) In the event of an error the contents of the variable FILNAM
C        are not defined and should not be used.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Uses the Fortran INQUIRE statement to determine the filename that
C     is associated with the Fortran logical unit LUNIT.
C
C$ Examples
C
C     The following code fragment illustrates the use of LUN2FN.
C
C     C
C     C      Convert the logical unit to its filename and display it.
C     C
C            CALL LUN2FN ( UNIT1, FNAME1 )
C            WRITE (*,*) 'The filename is: ', FNAME1
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
C     map logical unit to filename
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
 
      LOGICAL               OPENED
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LUN2FN' )
      END IF
 
C
C     First we test to see if the filename is blank.
C
      IF ( LUNIT .LE. 0 ) THEN
 
         CALL SETMSG ( 'The Fortran logical unit was not' //
     .                 ' positive: #.'                     )
         CALL ERRINT ( '#', LUNIT                          )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'            )
         CALL CHKOUT ( 'LUN2FN'                            )
         RETURN
 
      END IF
 
C
C     So simple, it defies explanation: just INQUIRE.
C
      INQUIRE ( UNIT   = LUNIT,
     .          NAME   = FILNAM,
     .          OPENED = OPENED,
     .          IOSTAT = IOSTAT  )
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL SETMSG ( 'An error occurred while INQUIRing on'  //
     .                 ' unit #. The IOSTAT value is #.'        )
         CALL ERRINT ( '#', LUNIT                               )
         CALL ERRINT ( '#', IOSTAT                              )
         CALL SIGERR ( 'SPICE(INQUIREFAILED)'                   )
         CALL CHKOUT ( 'LUN2FN'                                 )
         RETURN
 
      END IF
 
C
C     If there is no open file associated with the logical unit LUNIT
C     we cannot get a filename. So signal an error.
C
      IF ( .NOT. OPENED ) THEN
 
         CALL SETMSG ( 'There was no open file associated with the' //
     .                 ' logical unit #.'                            )
         CALL ERRINT ( '#', LUNIT                                    )
         CALL SIGERR ( 'SPICE(FILENOTOPEN)'                          )
         CALL CHKOUT ( 'LUN2FN'                                      )
         RETURN
 
      END IF
 
C
C     If we made it to here, we are done. Just check out and return.
C
      CALL CHKOUT ( 'LUN2FN' )
      RETURN
      END
