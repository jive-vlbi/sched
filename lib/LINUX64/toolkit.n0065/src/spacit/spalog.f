C$Procedure      SPALOG ( SPACIT, read comments from binary file )

      SUBROUTINE SPALOG ( VERSN, LOGFIL, LOGLUN )

C$ Abstract
C
C     SPACIT utility subroutine used to open a log file. This subroutine
C     is for use only be the SPACIT program. Use it at your own risk.
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
C     None.
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         VERSN
      LOGICAL               LOGFIL
      INTEGER               LOGLUN

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      LOGFIL    O    Logical flag indicating a log file was opened.
C      LOGLUN    O    The logical unit of the log file. 
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C      LOGFIL   Logical flag indicating a log file was opened. This
C               Variable has the value of .TRUE. if a log file is being 
C               written, and a value of .FALSE. otherwise.
C
C      LOGLUN   The logical unit of the log file. If LOGFIL has the 
C               value .TRUE. then LOGLUN will be the Fortran logical 
C               unit of the log file.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     xxx
C
C$ Examples
C
C     xxx
C
C$ Restrictions
C
C     xxx
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer     (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 11-JUL-1995 (KRG)
C
C        
C
C-&

C$ Index_Entries
C
C     spacit convert binary to transfer
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
C     Set a value for the logical unit which represents the standard
C     output device, commonly a terminal. A value of 6 is widely used,
C     but the Fortran standard does not specify a value, so it may be
C     different for different Fortran implementations.
C
      INTEGER               STDOUT
      PARAMETER           ( STDOUT = 6 )
C
C     Set a value for the length of an input text line.
C
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 255 )
C
C     Set a value for the length of a filename.
C
      INTEGER               FNMLEN
      PARAMETER           ( FNMLEN = 128 )
C
C     Set a length for the prompt.
C
      INTEGER               PRMLEN
      PARAMETER           ( PRMLEN = 80 )
C
C     Local variables
C
      CHARACTER*(FNMLEN)    LOGFNM
      CHARACTER*(LINLEN)    LINE
      CHARACTER*(PRMLEN)    PRMPT

      LOGICAL               FILEOK
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPALOG' )
      END IF
C
C     Get the filename.
C
      FILEOK = .FALSE.
      PRMPT  = '   Log file: '
      CALL GETFNM_1 ( PRMPT, 'NEW', LOGFNM, FILEOK )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPALOG' )
         RETURN
      END IF

      IF ( LOGFIL ) THEN
C
C        If a log file has already been opened, then display
C        a message to that effect and go get the next 
C        option.
C
         CALL SETMSG ( 'A log file with the name ''#'' has already'
     .   //            ' been opened. Only one log file is allowed'
     .   //            ' per SPACIT session.'                       )
         CALL ERRFNM ( '#', LOGLUN                                  )
         CALL SIGERR ( 'SPACIT(LOGFILEISOPEN)'                      )
         CALL CHKOUT ( 'SPALOG'                                     )
         RETURN

      END IF
C
C     Open the SPACIT log file here
C
      CALL TXTOPN( LOGFNM, LOGLUN )

      IF ( FAILED () ) THEN
         LOGFIL = .FALSE.
         CALL CHKOUT ( 'SPALOG' )
         RETURN
      END IF

      LOGFIL = .TRUE.

      LINE = '   Opening log file: #'
      CALL REPMC ( LINE, '#', LOGFNM, LINE )

      CALL WRITLN ( ' ',   STDOUT )
      CALL WRITLN (  LINE, STDOUT )
      CALL WRITLN ( ' ',   STDOUT )

      LINE = '   The log file ''#'' was opened successfully.'
      CALL REPMC ( LINE, '#', LOGFNM, LINE )

      CALL WRITLN (  LINE, STDOUT )
      CALL WRITLN ( ' ',   STDOUT )
C
C     Now put a short header into the log file.
C     Eventually this should contain the date that 
C     log file was created and possible some other 
C     things as well.
C
      LINE = 'Log file for SPACIT Version #.'
      CALL REPMC ( LINE, '#', VERSN, LINE )

      CALL WRITLN ( ' ',  LOGLUN )
      CALL WRITLN ( LINE, LOGLUN )
      CALL WRITLN ( ' ',  LOGLUN )

      IF ( FAILED() ) THEN
         CLOSE ( LOGLUN )
         LOGFIL = .FALSE.
         LOGLUN = -1
      END IF

      CALL CHKOUT ( 'SPALOG' )
      RETURN
      END
