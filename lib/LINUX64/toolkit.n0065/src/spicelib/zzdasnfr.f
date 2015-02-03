C$Procedure ZZDASNFR ( Private --- DAS write New File Record )
 
      SUBROUTINE ZZDASNFR ( LUN,
     .                      IDWORD,
     .                      IFNAME,
     .                      NRESVR,
     .                      NRESVC,
     .                      NCOMR,
     .                      NCOMC,
     .                      FORMAT  )
 
C$ Abstract
C
C     Write the file record to a new DAS file.
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
C     DAS
C     UTILITY
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE              'zzftprms.inc'
 
      INTEGER               LUN
      CHARACTER*(*)         IDWORD
      CHARACTER*(*)         IFNAME
      INTEGER               NRESVR
      INTEGER               NRESVC
      INTEGER               NCOMR
      INTEGER               NCOMC
      CHARACTER*(*)         FORMAT
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LUN        I   Logical unit number of an open DAS file.
C     IDWORD     I   DAS File ID word.
C     IFNAME     I   DAS internal file name.
C     NRESVR     I   Number of reserved records in file.
C     NRESVC     I   Number of characters in use in reserved rec. area.
C     NCOMR      I   Number of comment records in file.
C     NCOMC      I   Number of characters in use in comment area.
C     FORMAT     I   File binary format identifier string.
C
C$ Detailed_Input
C
C     LUN        is a logical unit number of a DAS whose first record is
C                to be created with a DAS file record bearing the
C                attributes specified by the other arguments.
C
C     IDWORD     is the 'ID word' contained in the first eight
C                characters of the file record.
C
C     IFNAME     is the internal file name of the DAS file.  The
C                maximum length of the internal file name is 60
C                characters.
C
C     NRESVR     is the number of reserved records in the DAS file
C                specified by HANDLE.
C
C     NRESVC     is the number of characters in use in the reserved
C                record area of the DAS file specified by HANDLE.
C
C     NCOMR      is the number of comment records in the DAS file
C                specified by HANDLE.
C
C     NCOMC      is the number of characters in use in the comment area
C                of the DAS file specified by HANDLE.
C
C     FORMAT     is a character string that indicates what the numeric
C                binary format the DAS is utilizing.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     See include file zzftprms.inc.
C
C$ Files
C
C     This routine writes to the first record of the DAS whose
C     logical unit is LUN.
C
C$ Exceptions
C
C     1) If any errors occur from the WRITE to the logical unit LUN,
C        the error SPICE(DASWRITEFAIL) is signaled.  Before returing
C        to the caller, the file attached to LUN is closed and deleted.
C
C$ Particulars
C
C     This routine assembles the file record and writes it to the
C     first record in a DAS.  Its purpose is to write new file
C     records only.  For updates, use DASWFR.
C
C     Make certain the caller checks FAILED() after this returns,
C     since on error it closes and deletes the file.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1) An individual character must occupy 1 byte of space and
C        conform to the ASCII standard.
C
C     2) The word size for the machine should be at least 32 bits,
C        else the computations to null pad the gaps in the file
C        record may overstep record boundaries.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 11-DEC-2001 (FST)
C
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               RTRIM
      LOGICAL               RETURN
 
C
C     Local Parameters
C
C     Amount of space measured in characters necessary to null pad
C     between the last character of FORMAT and the first character
C     of FTPSTR to keep FTPSTR at character 700 in a 1024 byte
C     record.
C
      INTEGER               PREFTP
      PARAMETER           ( PREFTP = 607 )
 
C
C     Amount of space measured in characters necessary to
C     null pad from the last character of FTPSTR to the
C     end of the file record. Note: This value assumes the
C     length of the file record is 1024 bytes.
C
      INTEGER               PSTFTP
      PARAMETER           ( PSTFTP = 297 )
 
C
C     Lengths of internal file name, ID word, and format word.
C
      INTEGER               FMTLEN
      PARAMETER           ( FMTLEN = 8 )
 
      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN = 8 )
 
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )
 
C
C     Local Variables
C
      CHARACTER*(1)         NULLCH
      CHARACTER*(FMTLEN)    LOCFMT
      CHARACTER*(IDWLEN)    LOCIDW
      CHARACTER*(IFNLEN)    LOCIFN
      CHARACTER*(PREFTP)    PRENUL
      CHARACTER*(PSTFTP)    PSTNUL
      CHARACTER*(SIZDLM)    DELIM
      CHARACTER*(SIZEND)    LFTBKT
      CHARACTER*(SIZEND)    RGTBKT
      CHARACTER*(SIZFTP)    FTPSTR
      CHARACTER*(SIZSTR)    TSTSTR
 
      INTEGER               I
      INTEGER               IOSTAT
 
      LOGICAL               FIRST
 
C
C     Saved Variables
C
      SAVE                FIRST
      SAVE                FTPSTR
      SAVE                PRENUL
      SAVE                PSTNUL
 
C
C     Data Statements
C
      DATA   FIRST        / .TRUE. /
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZDASNFR')
      END IF
 
C
C     On the first pass, format the PRENUL and PSTNUL strings,
C     and build FTPSTR from its components.
C
      IF ( FIRST ) THEN
C
C        Store NULL into NULLCH.
C
         NULLCH = CHAR(0)
 
C
C        Set all of the characters of PRENUL to nulls.
C
         DO I = 1, PREFTP
 
            PRENUL (I:I) = NULLCH
 
         END DO
 
C
C        Set all of the characters of PSTNUL to nulls.
C
         DO I = 1, PSTFTP
 
            PSTNUL (I:I) = NULLCH
 
         END DO
 
C
C        Build FTPSTR from its components that come back from
C        ZZFTPSTR.  This private SPICE routine returns the
C        following components:
C
C           TSTSTR - The test component of the FTP string
C           LFTBKT - The left bracketing, printable, component of
C                    the FTP string.
C           RGTBKT - The right bracketing, printable, component of
C                    the FTP string.
C           DELIM  - The printable delimiter that separates the
C                    individual test character blocks in TSTSTR.
C
C        which are assembled into the FTP string as it appears in
C        the DAS file record.
C
         CALL ZZFTPSTR ( TSTSTR, LFTBKT, RGTBKT, DELIM )
 
         FTPSTR = LFTBKT( 1:RTRIM(LFTBKT) )                          //
     .            TSTSTR( 1:RTRIM(TSTSTR) )                          //
     .            RGTBKT( 1:RTRIM(RGTBKT) )
 
C
C        Stop this block from executing except on the first pass.
C
         FIRST = .FALSE.
 
      END IF
 
C
C     Make local copies of each of the string arguments.  This way we
C     maintain the proper sizes for each of the string objects, in
C     the event larger or smaller strings are passed in.
C
      LOCIDW = IDWORD
      LOCIFN = IFNAME
      LOCFMT = FORMAT
 
C
C     Write the file record components out to the first record of the
C     file.
C
      WRITE ( UNIT = LUN, REC=1, IOSTAT=IOSTAT ) LOCIDW,
     .                                           LOCIFN,
     .                                           NRESVR,
     .                                           NRESVC,
     .                                           NCOMR,
     .                                           NCOMC,
     .                                           LOCFMT,
     .                                           PRENUL,
     .                                           FTPSTR,
     .                                           PSTNUL
 
C
C     Check IOSTAT for errors.
C
      IF ( IOSTAT .NE. 0 ) THEN
 
C
C        Since we are unable to write to the file record, make
C        certain the output file is destroyed.
C
         CALL SETMSG ( 'Attempt to write file ''#'' failed. '   //
     .                 'Value of IOSTAT was #. The file has '   //
     .                 'been deleted.'                           )
         CALL ERRFNM ( '#', LUN                                  )
         CALL ERRINT ( '#', IOSTAT                               )
 
         CLOSE ( UNIT = LUN, STATUS = 'DELETE' )
 
         CALL SIGERR ( 'SPICE(DASWRITEFAIL)' )
         CALL CHKOUT ( 'ZZDASNFR'            )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'ZZDASNFR' )
 
      RETURN
      END
