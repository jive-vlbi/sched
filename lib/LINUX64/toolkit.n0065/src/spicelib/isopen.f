 
C$Procedure             ISOPEN ( Is a file currently open? )
 
      LOGICAL FUNCTION ISOPEN ( FILE )
 
C$ Abstract
C
C     Determine whether a named file is currently open.
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
 
      CHARACTER*(*)    FILE
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FILE       I   Name of the file in question.
C
C     The function returns the value TRUE if the file is open, FALSE
C     otherwise.
C
C$ Detailed_Input
C
C     FILE        is the name of the file in question.
C
C$ Detailed_Output
C
C     The function returns the value TRUE if the file is open, FALSE
C     otherwise.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the filename is blank, the error SPICE(BLANKFILENAME) will
C        be signalled.
C
C     2) If an error occurs during the execution of the Fortran INQUIRE
C        statement, the error SPICE(INQUIREFAILED) is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Use the Fortran INQUIRE statement to determine the open status
C     of FILE.
C
C$ Examples
C
C     The following code fragment illustrates the use of ISOPEN.
C
C           IF ( .NOT. ISOPEN ( FILE ) ) THEN
C              Open the file here
C           ELSE
C              ERROR = 'Input file is already open.'
C           END IF
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
C-     SPICELIB Version 1.1.0, 29-FEB-1996 (KRG)
C
C         Added a local logical variable that is used as temporary
C         storage for the results from the INQUIRE statement rather
C         than using the function name. This solved a problem on the
C         macintosh.
C
C-    SPICELIB Version 1.0.0, 05-OCT-1994 (KRG)
C
C-&
 
C$ Index_Entries
C
C     test for file already open
C     is a file open
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
      LOGICAL               MYOPEN
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         ISOPEN = .FALSE.
         RETURN
      ELSE
         CALL CHKIN ( 'ISOPEN' )
      END IF
 
C
C     First we test to see if the filename is blank.
C
      IF ( FILE .EQ. ' ' ) THEN
 
         ISOPEN = .FALSE.
         CALL SETMSG ( 'The file name is blank. ' )
         CALL SIGERR ( 'SPICE(BLANKFILENAME)'     )
         CALL CHKOUT ( 'ISOPEN'                   )
         RETURN
 
      END IF
 
C
C     So simple, it defies explanation.
C
      INQUIRE ( FILE   = FILE,
     .          EXIST  = EXISTS,
     .          OPENED = MYOPEN,
     .          IOSTAT = IOSTAT  )
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         ISOPEN = .FALSE.
         CALL SETMSG ( 'Value of IOSTAT was *.' )
         CALL ERRINT ( '*', IOSTAT              )
         CALL SIGERR ( 'SPICE(INQUIREFAILED)'   )
         CALL CHKOUT ( 'ISOPEN'                 )
         RETURN
 
      END IF
C
C     A file cannot be open if it does not exist. We do actually need to
C     check this because some operating environments return .TRUE. for
C     the value of OPENED if a file does not exist.
C
      IF ( .NOT. EXISTS ) THEN
 
         MYOPEN = .FALSE.
 
      END IF
C
C     Set the function value, check out, and return.
C
 
      ISOPEN = MYOPEN

      CALL CHKOUT ( 'ISOPEN' )
      RETURN
      END
