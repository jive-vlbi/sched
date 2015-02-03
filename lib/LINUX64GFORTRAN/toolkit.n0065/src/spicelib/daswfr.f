C$Procedure DASWFR ( DAS write file record )
 
      SUBROUTINE DASWFR ( HANDLE,
     .                    IDWORD,
     .                    IFNAME,
     .                    NRESVR,
     .                    NRESVC,
     .                    NCOMR,
     .                    NCOMC  )
 
C$ Abstract
C
C     Update the contents of the file record of a specified DAS file.
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
C     DAS
C
C$ Keywords
C
C     DAS
C     FILES
C     UTILITY
C
C$ Declarations
 
      INTEGER               HANDLE
      CHARACTER*(*)         IDWORD
      CHARACTER*(*)         IFNAME
      INTEGER               NRESVR
      INTEGER               NRESVC
      INTEGER               NCOMR
      INTEGER               NCOMC
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   DAS file handle.
C     IDWORD     I   ID word.
C     IFNAME     I   DAS internal file name.
C     NRESVR     I   Number of reserved records in file.
C     NRESVC     I   Number of characters in use in reserved rec. area.
C     NCOMR      I   Number of comment records in file.
C     NCOMC      I   Number of characters in use in comment area.
C
C$ Detailed_Input
C
C     HANDLE     is a file handle for a DAS file open for writing.
C
C     IDWORD     is the `ID word' contained in the first eight
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
C     1) If the handle passed to this routine is not the handle of an
C        open DAS file, the error will be signaled by a routine called
C        by this routine.
C
C     2) If the specified DAS file is not open for write access, the
C        error will be diagnosed by a routine called by this routine.
C
C     3) If the attempt to read the file record fails, the error
C        SPICE(DASREADFAIL) is signaled.
C
C     4) If the file write attempted by this routine fails, the error
C        SPICE(DASFILEWRITEFAILED) is signaled.
C
C$ Files
C
C     See the description of HANDLE under $Detailed_Input.
C
C$ Particulars
C
C     This routine provides a convenient way of updating the internal
C     file name of a DAS file.
C
C     The `ID word' contained in the file record is a string of eight
C     characters that identifies the file as a DAS file and optionally
C     indicates a specific file format, for example, `EK'.
C
C$ Examples
C
C     1)  Update the internal file name of an existing DAS file.
C
C            C
C            C     Open the file for writing.
C            C
C                  CALL DASOPW ( FNAME, HANDLE  )
C
C            C
C            C     Retrieve the ID word and current reserved record
C            C     and comment area record and character counts.
C            C
C                  CALL DASRFR ( HANDLE,
C                 .              IDWORD,
C                 .              IFNAME,
C                 .              NRESVR,
C                 .              NRESVC,
C                 .              NCOMR,
C                 .              NCOMC  )
C
C            C
C            C     Set the internal file name and update the file
C            C     with it.
C            C
C                  IFNAME = 'New internal file name'
C
C                  CALL DASWFR ( HANDLE,
C                 .              IDWORD,
C                 .              IFNAME,
C                 .              NRESVR,
C                 .              NRESVC,
C                 .              NCOMR,
C                 .              NCOMC  )
C
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer (JPL)
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C     F.S. Turner    (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.0.0, 11-DEC-2001 (FST)
C
C        This routine was modified to accomodate the preservation
C        of the FTP validation and binary file format strings that
C        are not part of the DAS file record.
C
C-    SPICELIB Version 2.0.0, 27-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C        Added a check of FAILED after the call to DASHLU which will
C        check out and return if DASHLU fails. This is so that when in
C        return mode of the error handling the READ following the call
C        to DASHLU will not be executed.
C
C        Reworded some of the descriptions contained in the
C        $ Detailed_Output section of the header so that they were more
C        clear.
C
C-    SPICELIB Version 1.0.0, 24-NOV-1992 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     write DAS file record
C     write DAS internal file name
C     update DAS internal file name
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 3.0.0, 11-DEC-2001 (FST)
C
C        In order to preserve the additional information that
C        now resides in the file record, this routine reads
C        the entire record into local buffers, including the
C        TAILEN characters that follow the actual data content.
C        The contents of the local buffers that correspond to
C        information brought in from the call sequence of the
C        routine are ignored when the record is rewritten.
C        However, the ID word, the file format string, and the
C        trailing TAILEN characters that contain the FTP validation
C        string are rewritten along with the input values.
C
C        This routine does not simply replace the FTP validation
C        string with the components from ZZFTPSTR, since that
C        would possibly validate a corrupt file created using a newer
C        Toolkit.
C
C        The string arguments passed into this routine are now
C        copied to local buffers of the appropriate length.
C
C-    SPICELIB Version 2.0.0, 27-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C        Added a check of FAILED after the call to DASHLU which will
C        check out and return if DASHLU fails. This is so that when in
C        return mode of the error handling the READ following the call
C        to DASHLU will not be executed.
C
C        Reworded some of the descriptions contained in the
C        $ Detailed_Output section of the header so that they were more
C        clear.
C
C-    SPICELIB Version 1.0.0, 24-NOV-1992 (NJB) (WLT)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local Parameters
C
      INTEGER               FMTLEN
      PARAMETER           ( FMTLEN =  8 )
 
      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN =  8 )
 
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )
 
C
C     The parameter TAILEN determines the tail length of a DAS file
C     record.  This is the number of bytes (characters) that
C     occupy the portion of the file record that follows the
C     integer holding the first free address.  For environments
C     with a 32 bit word length, 1 byte characters, and DAS
C     record sizes of 1024 bytes, we have:
C
C           8 bytes - IDWORD
C          60 bytes - IFNAME
C           4 bytes - NRESVR (32 bit integer)
C           4 bytes - NRESVC (32 bit integer)
C           4 bytes - NCOMR  (32 bit integer)
C         + 4 bytes - NCOMC  (32 bit integer)
C          ---------
C          84 bytes - (All file records utilize this space.)
C
C     So the size of the remaining portion (or tail) of the DAS
C     file record for computing enviroments as described above
C     would be:
C
C        1024 bytes - DAS record size
C      -    8 bytes - DAS Binary File Format Word
C      -   84 bytes - (from above)
C       ------------
C         932 bytes - DAS file record tail length
C
C     Note: environments that do not have a 32 bit word length,
C     1 byte characters, and a DAS record size of 1024 bytes, will
C     require the adjustment of this parameter.
C
      INTEGER               TAILEN
      PARAMETER           ( TAILEN = 932 )
 
C
C     Local variables
C
      CHARACTER*(FMTLEN)    FORMAT
      CHARACTER*(IDWLEN)    LOCIDW
      CHARACTER*(IFNLEN)    IFN
      CHARACTER*(IFNLEN)    LOCIFN
      CHARACTER*(TAILEN)    TAIL
 
      INTEGER               FREE
      INTEGER               IOSTAT
      INTEGER               LASTLA ( 3 )
      INTEGER               LASTRC ( 3 )
      INTEGER               LASTWD ( 3 )
      INTEGER               LOCNVR
      INTEGER               LOCNVC
      INTEGER               LOCNCR
      INTEGER               LOCNCC
      INTEGER               OLDRRC
      INTEGER               OLDRCH
      INTEGER               OLDCRC
      INTEGER               OLDCCH
      INTEGER               UNIT
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASWFR' )
      END IF
 
C
C     Check to be sure that HANDLE is attached to a file that is open
C     with write access.  If the call fails, check out and return.
C
      CALL DASSIH ( HANDLE, 'WRITE' )
 
C
C     Get the logical unit for this DAS file.
C
      CALL DASHLU ( HANDLE, UNIT )
 
      IF ( FAILED () ) THEN
         CALL CHKOUT ( 'DASWFR' )
         RETURN
      END IF
 
C
C     In order to maintain the integrity of the file ID word, the
C     file FORMAT, and the FTP string if present, we need to
C     read the entire file record into the appropriate sized local
C     buffers. The values of the LOCxxx variables are simply
C     ignored, since the caller passes new values in for updates.
C
      READ ( UNIT, REC=1, IOSTAT=IOSTAT) LOCIDW,
     .                                   LOCIFN,
     .                                   LOCNVR,
     .                                   LOCNVC,
     .                                   LOCNCR,
     .                                   LOCNCC,
     .                                   FORMAT,
     .                                   TAIL
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL SETMSG ( 'Attempt to read the file record failed for'   //
     .                 ' file ''#''. IOSTAT = #'                      )
         CALL ERRFNM ( '#', UNIT                                      )
         CALL ERRINT ( '#', IOSTAT                                    )
         CALL SIGERR ( 'SPICE(DASREADFAIL)'                           )
         CALL CHKOUT ( 'DASWFR'                                       )
         RETURN
 
      END IF
 
C
C     Set the value of the internal file name and IDWORD before
C     writing.  This is to guarantee that their lengths are ok.
C
      IFN    = IFNAME
      LOCIDW = IDWORD
 
      WRITE ( UNIT    =  UNIT,
     .        REC     =  1,
     .        IOSTAT  =  IOSTAT )     LOCIDW,
     .                                IFN,
     .                                NRESVR,
     .                                NRESVC,
     .                                NCOMR,
     .                                NCOMC,
     .                                FORMAT,
     .                                TAIL
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL SETMSG ( 'Could not write file record.  File was '
     .   //            '#.  IOSTAT was #.'                       )
         CALL ERRFNM ( '#', UNIT                                 )
         CALL ERRINT ( '#', IOSTAT                               )
         CALL SIGERR ( 'SPICE(DASFILEWRITEFAILED)'               )
         CALL CHKOUT ( 'DASWFR'                                  )
         RETURN
 
      END IF
 
C
C     Update the file summary, in case the values of the reserved
C     record or comment area counts have changed.
C
      CALL DASHFS ( HANDLE,
     .              OLDRRC,
     .              OLDRCH,
     .              OLDCRC,
     .              OLDCCH,
     .              FREE,
     .              LASTLA,
     .              LASTRC,
     .              LASTWD )
 
      CALL DASUFS ( HANDLE,
     .              NRESVR,
     .              NRESVC,
     .              NCOMR,
     .              NCOMC,
     .              FREE,
     .              LASTLA,
     .              LASTRC,
     .              LASTWD )
 
      CALL CHKOUT ( 'DASWFR' )
      RETURN
      END
