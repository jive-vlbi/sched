C$Procedure SPAT2B ( SPACIT, convert transfer to binary )

      SUBROUTINE SPAT2B ( LOGFIL, LOGLUN )

C$ Abstract
C
C     SPACIT utility subroutine used to manage the conversion of SPICE
C     transfer format files into their equivalent binary formats. This
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
C
C-    SPICE Version 2.0.0, 13-MAR-1999 (EDW)
C
C        Replaced WRITLN's to standard output with TOSTDO,
C        calls to GETFAT with ZZGETFAT, calls to CONVTB to
C        ZZCONVTB.
C
C        Added a condition to the ARCH-TYPE test block following
C        the call to ZZGETFAT.  The block test for both ARCH and
C        TYPE as unknows, then for ARCH as XFR, then either ARCH
C        or TYPE as unknown.
C
C-    Beta Version 1.0.0, 13-NOV-1994 (KRG)
C
C-&

C$ Index_Entries
C
C     spacit convert transfer to binary
C
C-&

C
C     SPICELIB functions
C
      INTEGER               CARDI

      LOGICAL               EXISTS
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
C     Lower bound for a SPICELIB CELL data structure.
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
C
C     Maximum number of open binary files allowed.
C
      INTEGER               MAXOPN
      PARAMETER           ( MAXOPN = 1 )
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

      INTEGER               I
      INTEGER               NUMOPN
      INTEGER               OPNSET(LBCELL:MAXOPN)
      INTEGER               NUMBER

      LOGICAL               FILEOK

C
C     Save everything to keep configuration control happy.
C
      SAVE

C
C     Initial values
C
      DATA MESSAG / 'Converting from transfer file to binary file.' /

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPAT2B' )
      END IF
C
C
C
      BFSTAT = 'NEW'
      XFSTAT = 'OLD'

      FILEOK = .FALSE.
      PRMPT  = '   Transfer file: '

      CALL GETFNM_1 ( PRMPT, XFSTAT, XFRFNM, FILEOK )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPAT2B' )
         RETURN
      END IF


C
C     Get the architecture, file type and logical unit number for
C     the transfer file.
C
      CALL ZZGETFAT ( XFRFNM, ARCH, TYPE, NUMBER )


C
C     If something went wrong, close the transfer file and checkout.
C
      IF ( FAILED() ) THEN
         CLOSE ( NUMBER )
         CALL CHKOUT ( 'SPAT2B' )
         RETURN
      END IF


C
C     The file should be a transfer file.  If not, flag an error
C     then exit.
C
      IF ( ( ARCH .EQ. '?' ) .AND. ( TYPE .EQ. '?' ) ) THEN

         CALL SETMSG ( 'The architecture and type of the'
     .   //            ' file ''#'' could not be'
     .   //            ' determined.'                      )
         CALL ERRCH  ( '#', XFRFNM                         )
         CALL SIGERR ( 'SPICE(BADFILEFORMAT)'              )
         CALL CHKOUT ( 'SPAT2B' )
         RETURN

      ELSE IF ( (ARCH .NE. 'XFR') .AND. (ARCH .NE. 'DEC') ) THEN

         CALL SETMSG ( 'The file ''#'' was not a transfer'
     .   //            ' format SPICE file. In order to'
     .   //            ' convert a transfer file to binary'
     .   //            ' format, it must be a CK, EK, PCK,'
     .   //            ' or SPK file in one of the'
     .   //            ' supported transfer file formats.' )
         CALL ERRCH  ( '#', XFRFNM                         )
         CALL SIGERR ( 'SPICE(IMPROPERFILE)'               )
         CALL CHKOUT ( 'SPAT2B' )
         RETURN

      ELSE IF ( ( ARCH .EQ. '?' ) .OR. ( TYPE .EQ. '?' ) ) THEN

         CALL SETMSG ( 'The architecture or type of the'
     .   //            ' file ''#'' could not be'
     .   //            ' determined.  The file did not'
     .   //            ' read as a transfer file'          )
         CALL ERRCH  ( '#', XFRFNM                         )
         CALL SIGERR ( 'SPICE(BADFILEFORMAT)'              )
         CALL CHKOUT ( 'SPAT2B' )
         RETURN

      END IF


C
C     No get the binary file name.
C
      FILEOK = .FALSE.
      PRMPT  = '   Binary file  : '

      CALL GETFNM_1 ( PRMPT, BFSTAT, BINFNM, FILEOK )

      IF ( .NOT. FILEOK ) THEN
         CALL CHKOUT ( 'SPAT2B' )
         RETURN
      END IF

      LINE = '   #'
      CALL REPMC ( LINE, '#', MESSAG, LINE )
      CALL TOSTDO ( ' ' )
      CALL TOSTDO (  LINE )
      IF ( LOGFIL ) THEN
         CALL WRITLN ( ' ',  LOGLUN )
         CALL WRITLN ( LINE, LOGLUN )
      END IF

      LINE = '   Converting Transfer File: #'
      CALL REPMC  ( LINE, '#', XFRFNM, LINE )
      CALL TOSTDO ( ' ')
      CALL TOSTDO ( LINE )
      IF ( LOGFIL ) THEN
         CALL WRITLN ( ' ',  LOGLUN )
         CALL WRITLN ( LINE, LOGLUN )
      END IF

      LINE = '   To Binary File          : #'
      CALL REPMC  ( LINE, '#', BINFNM, LINE )
      CALL TOSTDO ( LINE )
      CALL TOSTDO ( ' ')
      IF ( LOGFIL ) THEN
         CALL WRITLN ( LINE, LOGLUN )
         CALL WRITLN ( ' ',  LOGLUN )
      END IF


      CALL TOSTDO ( '   Please wait...' )

C
C     Now call the converted.  The real work starts here.
C
      CALL ZZCONVTB ( XFRFNM, ARCH, TYPE, BINFNM, NUMBER )


C
C      Important question, did the conversion work?
C
      IF ( FAILED() ) THEN


C
C        Something failed.  Reset the error system.
C
         CALL RESET

         CALL SCARDI ( 0, OPNSET         )
         CALL CLEARI ( MAXOPN, OPNSET(1) )
         CALL DAFHOF ( OPNSET )
         NUMOPN = CARDI ( OPNSET )

C
C        Close the DAF files.
C
         IF ( NUMOPN .GT. 0 ) THEN
            DO I = 1, NUMOPN
               CALL DAFCLS ( OPNSET(I) )
            END DO
         END IF

         CALL SCARDI ( 0, OPNSET )
         CALL CLEARI ( MAXOPN, OPNSET(1) )
         CALL DASHOF ( OPNSET )


C
C        Close the DAS files.
C
         NUMOPN = CARDI ( OPNSET )
         IF ( NUMOPN .GT. 0 ) THEN
            DO I = 1, NUMOPN
               CALL DASCLS ( OPNSET(I) )
            END DO
         END IF

C
C        Erase any partially converted binary file, then checkout.
C
         IF ( EXISTS( BINFNM ) ) THEN
            CALL DELFIL( BINFNM )
         END IF

         CALL CHKOUT ( 'SPAT2B' )
         RETURN

      END IF


C
C     If we got this far, the conversion worked (yippie!).
C
      LINE = '   Binary file ''#'' created.'
      CALL REPMC ( LINE, '#', BINFNM, LINE )
      CALL TOSTDO ( ' ' )
      CALL TOSTDO ( LINE)
      CALL TOSTDO ( ' ' )
      IF ( LOGFIL ) THEN
         CALL WRITLN ( ' ',  LOGLUN )
         CALL WRITLN ( LINE, LOGLUN )
         CALL WRITLN ( ' ',  LOGLUN )
      END IF

      CALL CHKOUT ( 'SPAT2B' )
      RETURN
      END
