C$Procedure      SPAB2T ( SPACIT, convert binary to transfer )

      SUBROUTINE SPAB2T ( LOGFIL, LOGLUN )

C$ Abstract
C
C     SPACIT utility subroutine used to manage the conversion of SPICE 
C     binary format files into their equivalent transfer formats. This
C     subroutine is for use only be the SPACIT program. Use it at your 
C     own risk.
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

      LOGICAL               LOGFIL
      INTEGER               LOGLUN

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      LOGFIL    I    Logical flag indicating a log file is being kept.
C      LOGLUN    I    The logical unit of the log file. 
C
C$ Detailed_Input
C
C      LOGFIL   Logical flag indicating a log file is being kept. This
C               Variable has the value of .TRUE. if a log file is being 
C               written, and a value of .FALSE. otherwise.
C
C      LOGLUN   The logical unit of the log file. If LOGFIL has the 
C               value .TRUE. then LOGLUN will be the Fortran logical 
C               unit of the log file.
C
C$ Detailed_Output
C
C     None.
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
C-    Beta Version 1.0.0, 10-JUL-1995 (KRG)
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
      LOGICAL               EXISTS
      LOGICAL               FAILED
      LOGICAL               ISOPEN
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
C     Set a length for the status of a file: 'OLD' or 'NEW'.
C
      INTEGER               STALEN
      PARAMETER           ( STALEN = 3 )
C
C     Set the length for the type of a file.
C
      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN = 4 )
C
C     Set the length for the architecture of a file.
C
      INTEGER               ARCLEN
      PARAMETER           ( ARCLEN = 3 )
C
C     Local variables
C
      CHARACTER*(ARCLEN)    ARCH
      CHARACTER*(FNMLEN)    BINFNM
      CHARACTER*(STALEN)    BFSTAT
      CHARACTER*(LINLEN)    LINE
      CHARACTER*(LINLEN)    MESSAG
      CHARACTER*(PRMLEN)    PRMPT
      CHARACTER*(FNMLEN)    XFRFNM
      CHARACTER*(STALEN)    XFSTAT
      CHARACTER*(TYPLEN)    TYPE

      INTEGER               HANDLE
      INTEGER               XFRLUN

      LOGICAL               FILEOK

C
C     Save everything to keep configuration control happy.
C
      SAVE

C
C     Initial values
C
      DATA MESSAG / 'Converting from binary file to transfer file.' /

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPAB2T' )
      END IF
C
C
C
      BFSTAT = 'OLD'
      XFSTAT = 'NEW'

      FILEOK = .FALSE.
      PRMPT  = '   Binary file  : '

      CALL GETFNM_1 ( PRMPT, BFSTAT, BINFNM, FILEOK )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPAB2T' )
         RETURN
      END IF

      CALL GETFAT ( BINFNM, ARCH, TYPE )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPAB2T' )
         RETURN
      END IF

      IF ( ( ARCH .EQ. '?' ) .OR. ( TYPE .EQ. '?' ) ) THEN

         CALL SETMSG ( 'The architecture and type of the'
     .   //            ' file ''#'' could not be'
     .   //            ' determined.'                      )
         CALL ERRCH  ( '#', BINFNM                         )
         CALL SIGERR ( 'SPICE(BADFILEFORMAT)'              )
         CALL CHKOUT ( 'SPAB2T'                            )
         RETURN

      ELSE IF (       ( ARCH .NE. 'DAF' ) 
     .          .AND. ( ARCH .NE. 'DAS' ) ) THEN

         CALL SETMSG ( 'The file ''#'' was not a binary'
     .   //            ' SPICE file. In order to convert'
     .   //            ' a file it must be a binary CK, EK,'
     .   //            ' PCK, or SPK file.'                  )
         CALL ERRCH  ( '#', BINFNM                           )
         CALL SIGERR ( 'SPICE(IMPROPERFILE)'                 )
         CALL CHKOUT ( 'SPAB2T'                              )
         RETURN
                                    
      END IF

      FILEOK = .FALSE.
      PRMPT  = '   Transfer file: '

      CALL GETFNM_1 ( PRMPT, XFSTAT, XFRFNM, FILEOK )

      IF ( .NOT. FILEOK ) THEN
         CALL CHKOUT ( 'SPAB2T' )
         RETURN
      END IF            

      LINE = '   #'
      CALL REPMC ( LINE, '#', MESSAG, LINE )
      CALL WRITLN ( ' ',   STDOUT )
      CALL WRITLN (  LINE, STDOUT )
      IF ( LOGFIL ) THEN
         CALL WRITLN ( ' ',  LOGLUN )
         CALL WRITLN ( LINE, LOGLUN )
      END IF

      LINE = '   Converting Binary File: #'
      CALL REPMC  ( LINE, '#', BINFNM, LINE )
      CALL WRITLN ( ' ',  STDOUT )
      CALL WRITLN ( LINE, STDOUT )
      IF ( LOGFIL ) THEN
         CALL WRITLN ( ' ',  LOGLUN )
         CALL WRITLN ( LINE, LOGLUN )
      END IF

      LINE = '   To Transfer File      : #'
      CALL REPMC ( LINE, '#', XFRFNM, LINE )
      CALL WRITLN ( LINE, STDOUT )
      CALL WRITLN ( ' ',  STDOUT )
      IF ( LOGFIL ) THEN
         CALL WRITLN ( LINE, LOGLUN )
         CALL WRITLN ( ' ',  LOGLUN )
      END IF

      CALL WRITLN ( '   Please wait...', STDOUT )

      CALL CONVBT ( BINFNM, XFRFNM )

      IF ( FAILED() ) THEN

         CALL RESET

         IF ( ARCH .EQ. 'DAF' ) THEN
            CALL DAFFNH ( BINFNM, HANDLE )
            CALL DAFCLS ( HANDLE )
         ELSE
            CALL DASFNH ( BINFNM, HANDLE )
            CALL DASCLS ( HANDLE )
         END IF

         IF ( EXISTS( XFRFNM ) ) THEN
            IF ( ISOPEN ( XFRFNM )  ) THEN
               CALL FN2LUN ( XFRFNM, XFRLUN )
               CLOSE ( XFRLUN )
            END IF
            CALL DELFIL( XFRFNM )
         END IF

         CALL CHKOUT ( 'SPAB2T' )
         RETURN

      END IF           

      LINE = '   Transfer file ''#'' created.'
      CALL REPMC ( LINE, '#', XFRFNM, LINE )
      CALL WRITLN ( ' ',  STDOUT )
      CALL WRITLN ( LINE, STDOUT )
      CALL WRITLN ( ' ',  STDOUT )
      IF ( LOGFIL ) THEN
         CALL WRITLN ( ' ',  LOGLUN )
         CALL WRITLN ( LINE, LOGLUN )
         CALL WRITLN ( ' ',  LOGLUN )
      END IF

      CALL CHKOUT ( 'SPAB2T' )
      RETURN
      END
