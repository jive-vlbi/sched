 
C$Procedure      SPASUM ( SPACIT, summarize binary file )
 
      SUBROUTINE SPASUM ( LOGFIL, LOGLUN )
 
C$ Abstract
C
C     SPACIT utility subroutine used to summarize the segments in SPICE
C     data kernel files. This subroutine is for use only be the SPACIT
C     program. Use it at your own risk.
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
C     W.L. Taber         (JPL)
C     K.R. Gehringer     (JPL)
C
C$ Version
C
C-    Beta Version 2.1.0, 02-OCT-2006 (BVS)
C
C        Replaced LDPOOL with FURNSH.
C
C-    Beta Version 2.0.0, 14-MAR-1997 (WLT)
C
C        The routine was enhanced to provide a diagnostic in the
C        event that the type of the file does belong to EK, CK, SPK
C        or PCK
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
C     Set values for the NAIF SPICE file types
C
      CHARACTER*(*)         CKTYP
      PARAMETER           ( CKTYP  = 'CK'  )
 
      CHARACTER*(*)         EKTYP
      PARAMETER           ( EKTYP  = 'EK'  )
 
      CHARACTER*(*)         PCKTYP
      PARAMETER           ( PCKTYP = 'PCK' )
 
      CHARACTER*(*)         SPKTYP
      PARAMETER           ( SPKTYP = 'SPK' )
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
      CHARACTER*(FNMLEN)    LPSFNM
      CHARACTER*(FNMLEN)    SCLFNM
      CHARACTER*(STALEN)    BFSTAT
      CHARACTER*(STALEN)    LFSTAT
      CHARACTER*(STALEN)    SFSTAT
      CHARACTER*(LINLEN)    LINE
      CHARACTER*(PRMLEN)    PRMPT
      CHARACTER*(TYPLEN)    TYPE
 
      INTEGER               HANDLE
 
 
      LOGICAL               FILEOK
      LOGICAL               NDLPS
      LOGICAL               NDSCLK
      LOGICAL               LPSLDD
      LOGICAL               SCLLDD
C
C     Saved values
C
      SAVE                  LPSLDD
      SAVE                  LPSFNM
      SAVE                  SCLLDD
      SAVE                  SCLFNM
C
C     Initial values
C
      DATA LPSLDD / .FALSE. /
      DATA SCLLDD / .FALSE. /
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPASUM' )
      END IF
C
C
C
      BFSTAT = 'OLD'
 
      FILEOK = .FALSE.
      PRMPT  = '   Binary file     : '
      CALL GETFNM_1 ( PRMPT, BFSTAT, BINFNM, FILEOK )
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPASUM' )
         RETURN
      END IF
 
      CALL GETFAT ( BINFNM, ARCH, TYPE )
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPASUM' )
         RETURN
      END IF
 
      IF ( ( ARCH .EQ. '?' ).OR.( TYPE .EQ. '?' ) ) THEN
 
         CALL SETMSG ( 'The architecture and type of the'
     .   //            ' file ''#'' could not be'
     .   //            ' determined.'                   )
         CALL ERRCH  ( '#', BINFNM                      )
         CALL SIGERR ( 'SPICE(BADFILEFORMAT)'           )
         CALL CHKOUT ( 'SPASUM'                         )
         RETURN
 
      ELSE IF ( ( ARCH .NE. 'DAF' ) .AND. ( ARCH .NE. 'DAS' ) ) THEN
 
 
         CALL SETMSG ( 'The file ''#'' was not a binary'
     .   //            ' SPICE file. In order to'
     .   //            ' summarize a file it must be a'
     .   //            ' binary CK, EK, PCK, or SPK'
     .   //            ' file.'                        )
         CALL ERRCH  ( '#', BINFNM                     )
         CALL SIGERR ( 'SPICE(IMPROPERFILE)'           )
         CALL CHKOUT ( 'SPASUM'                        )
         RETURN
 
      END IF
 
      IF ( TYPE .EQ. 'PRE' ) THEN
         TYPE = EKTYP
      END IF
 
      IF ( LPSLDD .OR. ( TYPE .EQ. EKTYP ) ) THEN
         NDLPS = .FALSE.
      ELSE
         NDLPS = .TRUE.
      END IF
 
      NDSCLK = .FALSE.
 
      IF ( ( .NOT. SCLLDD ) .AND. ( TYPE .EQ. CKTYP ) ) THEN
         NDSCLK = .TRUE.
      END IF
 
      IF ( NDLPS ) THEN
         LFSTAT = 'OLD'
         FILEOK = .FALSE.
         PRMPT  = '   Leapseconds file: '
         CALL GETFNM_1 ( PRMPT, LFSTAT, LPSFNM, FILEOK )
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPASUM' )
            RETURN
         END IF
      END IF
 
      IF ( NDSCLK ) THEN
         SFSTAT = 'OLD'
         FILEOK = .FALSE.
         PRMPT  = '   SCLK file       : '
         CALL GETFNM_1 ( PRMPT, SFSTAT, SCLFNM, FILEOK )
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPASUM' )
            RETURN
         END IF
      END IF
 
      CALL WRITLN ( ' ', STDOUT )
      IF( NDLPS ) THEN
 
         LINE = '   Loading the Leapseconds kernel file.'
     .   //     ' Please wait ...'
         CALL WRITLN ( LINE, STDOUT )
 
         CALL FURNSH( LPSFNM )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPASUM' )
            RETURN
         END IF
 
         LPSLDD = .TRUE.
      END IF
 
      IF( NDSCLK ) THEN
         LINE = '   Loading the SCLK kernel file.'
     .   //     ' Please wait ...'
         CALL WRITLN ( LINE, STDOUT )
 
         CALL FURNSH( SCLFNM )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPASUM' )
            RETURN
         END IF
 
         SCLLDD = .TRUE.
      END IF
 
      CALL WRITLN ( ' ',STDOUT )
      IF ( LOGFIL ) THEN
         CALL WRITLN ( ' ',LOGLUN )
      END IF
 
      IF ( TYPE .EQ. CKTYP ) THEN
C
C        Summarize a binary CK file.
C
         CALL DAFOPR ( BINFNM, HANDLE )
         IF ( FAILED() ) THEN
            CALL CHKOUT( 'SPASUM' )
            RETURN
         END IF
 
         CALL SUMCK ( HANDLE, BINFNM, LPSFNM, SCLFNM, LOGFIL, LOGLUN )
         CALL DAFCLS ( HANDLE                 )
 
      ELSE IF ( TYPE .EQ. SPKTYP ) THEN
C
C        Summarize a binary SPK file.
C
         CALL DAFOPR ( BINFNM, HANDLE )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT( 'SPASUM' )
            RETURN
         END IF
 
         CALL SUMSPK ( HANDLE, BINFNM, LPSFNM, LOGFIL, LOGLUN )
         CALL DAFCLS ( HANDLE                 )
 
      ELSE IF ( TYPE .EQ. PCKTYP ) THEN
C
C        Summarize a binary PCK file.
C
         CALL DAFOPR ( BINFNM, HANDLE )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT( 'SPASUM' )
            RETURN
         END IF
 
         CALL SUMPCK ( HANDLE, BINFNM, LPSFNM, LOGFIL, LOGLUN )
         CALL DAFCLS ( HANDLE                 )
 
      ELSE IF ( TYPE .EQ. EKTYP ) THEN
C
C        Summarize a binary EK file.
C
         CALL DASOPR ( BINFNM, HANDLE )
         CALL SUMEK ( HANDLE, BINFNM, LOGFIL, LOGLUN )
         CALL DASCLS( HANDLE )
 
       ELSE
 
         CALL SETMSG ( 'The specified file is not of a "type" '
     .   //            'that can be summarized. The types of '
     .   //            'files that can be summarized are: CK, '
     .   //            'EK, PCK, and SPK.  According to the '
     .   //            'type in the internal id-word of the '
     .   //            'file, this file has type: ''#''.  You '
     .   //            'will need to get an upgrade of SPACIT '
     .   //            'to summarize this file. ' )
 
         CALL ERRCH  ( '#', TYPE                      )
         CALL SIGERR ( 'SPICE(UNKNOWNTYPE)'           )
         CALL CHKOUT ( 'SPASUM'                       )
         RETURN
 
       END IF
 
      CALL CHKOUT ( 'SPASUM' )
      RETURN
      END
