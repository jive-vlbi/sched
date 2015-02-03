C$Procedure             EXISTS ( Does the file exist? )
 
      LOGICAL FUNCTION  EXISTS ( FILE )
 
C$ Abstract
C
C      Determine whether a file exists.
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
 
      CHARACTER*(*)    FILE
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      FILE       I   Name of the file in question.
C
C      The function returns the value TRUE if the file exists, FALSE
C      otherwise.
C
C$ Detailed_Input
C
C      FILE        is the name of the file in question. This may be
C                  any unambigous file name valid on the user's
C                  computer, for example
C
C                     '/usr/dir1/dir2/DATA.DAT'
C                     './DATA.DAT'  
C                     'c:\usr\dir1\dir2\data.dat'
C
C                  Environment or shell variables may not be used.
C
C$ Detailed_Output
C
C      The function returns the value TRUE if the file exists, FALSE
C      otherwise.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C      1) If the filename is blank, the error SPICE(BLANKFILENAME) will
C         be signaled.
C
C      2) If an error occurs during the execution of the Fortran INQUIRE
C         statement, the error SPICE(INQUIREFAILED) is signaled.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      Use the Fortran INQUIRE statement to determine the existence
C      of FILE.
C
C$ Examples
C
C      The following code fragment illustrates the use of EXISTS.
C
C            IF ( EXISTS ( FILE ) ) THEN
C               CALL UPDATE ( FILE )
C            ELSE
C               ERROR = 'Input file does not exist.'
C               RETURN
C            END IF
C
C$ Restrictions
C
C      None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      K.R. Gehringer  (JPL)
C      H.A. Neilan     (JPL)
C      I.M. Underwood  (JPL)
C
C$ Version
C
C-     SPICELIB Version 2.2.1, 01-JUL-2014 (NJB)
C
C         VAX examples were deleted from the header.
C
C-     SPICELIB Version 2.2.0, 9-DEC-1999 (WLT)
C
C         The input file name is now "trimmed" of trailing blanks
C         before checking its existance.
C
C-     SPICELIB Version 2.1.0, 4-MAR-1996 (KRG)
C
C         Added a local logical variable that is used as temporary
C         storage for the results from the INQUIRE statement rather
C         than using the function name. This solved a problem on the
C         macintosh.
C
C-     SPICELIB Version 2.0.0, 04-AUG-1994 (KRG)
C
C         Added a test to see if the filename was blank before the
C         INQUIRE statement. This allows a meaningful error message to
C         be presented.
C
C-     SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
C
C        If the value of the function RETURN is TRUE upon execution of
C        this module, this function is assigned a default value of
C        either 0, 0.0D0, .FALSE., or blank depending on the type of
C        the function.
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     does the file exist
C
C-&
 
 
 
C$ Revisions
C
C-     SPICELIB Version 2.1.0, 4-MAR-1996 (KRG)
C
C         Added a local logical variable that is used as temporary
C         storage for the results from the INQUIRE statement rather
C         than using the function name. This solved a problem on the
C         macintosh.
C
C-     Beta Version 2.0.0, 29-DEC-1988 (HAN)
C
C        The IOSTAT specifier was added to the INQUIRE statement.
C        If the value of IOSTAT is not equal to zero, an error
C        occurred during the execution of the INQUIRE statement.
C        In this case, a SPICELIB error is signaled and the routine
C        checks out.
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      INTEGER               RTRIM
 
C
C     Local variables
C
      INTEGER               IOSTAT
      INTEGER               R
 
      LOGICAL               MYEXST
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         EXISTS = .FALSE.
         RETURN
      ELSE
         CALL CHKIN ( 'EXISTS' )
      END IF
C
C     Initialize the local variable MYEXST to be .FALSE.
C
      MYEXST = .FALSE.
  
C
C     First we test to see if the filename is blank.
C
      IF ( FILE .EQ. ' ' ) THEN
 
         EXISTS = .FALSE.
         CALL SETMSG ( 'The file name is blank. ' )
         CALL SIGERR ( 'SPICE(BLANKFILENAME)'     )
         CALL CHKOUT ( 'EXISTS'                   )
         RETURN
 
      END IF

      R = RTRIM(FILE) 
C
C     So simple, it defies explanation.
C
      INQUIRE ( FILE = FILE(1:R), EXIST = MYEXST, IOSTAT = IOSTAT )
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         EXISTS = .FALSE.
         CALL SETMSG ( 'Value of IOSTAT was *.' )
         CALL ERRINT ( '*', IOSTAT              )
         CALL SIGERR ( 'SPICE(INQUIREFAILED)'   )
         CALL CHKOUT ( 'EXISTS'                 )
         RETURN
 
      END IF
C
C     Set the value of the function, check out and return.
C
      EXISTS = MYEXST
  
      CALL CHKOUT ( 'EXISTS' )
      RETURN
      END
