C$Procedure      DASRFR ( DAS, read file record )
 
      SUBROUTINE DASRFR ( HANDLE,
     .                    IDWORD,
     .                    IFNAME,
     .                    NRESVR,
     .                    NRESVC,
     .                    NCOMR,
     .                    NCOMC  )
 
C$ Abstract
C
C     Return the contents of the file record of a specified DAS
C     file.
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
C     IDWORD     O   ID word.
C     IFNAME     O   DAS internal file name.
C     NRESVR     O   Number of reserved records in file.
C     NRESVC     O   Number of characters in use in reserved rec. area.
C     NCOMR      O   Number of comment records in file.
C     NCOMC      O   Number of characters in use in comment area.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle for a previously opened DAS file.
C
C$ Detailed_Output
C
C     IDWORD      is the `ID word' contained in the first eight
C                 characters of the file record.
C
C     IFNAME      is the internal file name of the DAS file.  The
C                 maximum length of the internal file name is 60
C                 characters.
C
C     NRESVR      is the number of reserved records in the DAS file
C                 specified by HANDLE.
C
C     NRESVC      is the number of characters in use in the reserved
C                 record area of the DAS file specified by HANDLE.
C
C     NCOMR       is the number of comment records in the DAS file
C                 specified by HANDLE.
C
C     NCOMC       is the number of characters in use in the comment area
C                 of the DAS file specified by HANDLE.
C
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the file read attempted by this routine fails, the error
C        SPICE(DASFILEREADFAILED) will be signalled.
C
C$ Files
C
C     See the description of HANDLE under $Detailed_Input.
C
C$ Particulars
C
C     This routine provides a convenient way of retrieving the
C     information contained in the file record of a DAS file.
C
C$ Examples
C
C     1)  Obtain the internal file name of an existing DAS file.
C
C
C            C
C            C     Open the file for reading.
C            C
C                  CALL DASOPR ( FNAME, HANDLE  )
C
C            C
C            C     Retrieve the internal file name and print it.
C            C
C
C                  CALL DASRFR ( HANDLE,
C                 .              IDWORD,
C                 .              IFNAME,
C                 .              NRESVR,
C                 .              NRESVC,
C                 .              NCOMR,
C                 .              NCOMC  )
C
C
C                  WRITE (*,*) 'Internal file name is: '
C                  WRITE (*,*)  IFNAME
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
C
C$ Version
C
C-    SPICELIB Version 2.1.0, 25-AUG-1995 (NJB)
C
C        Bug fix:  local variables are now used in the direct
C        access of the file record.  Previously, the routine read
C        directly into the CHARACTER*(*) arguments IDWORD and IFNAME.
C
C-    SPICELIB Version 2.0.0, 27-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C        Removed the DASID parameter which had the value 'NAIF/DAS', as
C        it was not used and is also made obsolete by the change in the
C        format of the ID word being implemented.
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
C        Changed the example so that it does not set a value for IFNAME
C        before calling DASRFR. This appears to have been a cut and
C        paste bug from DASWFR.
C
C-    SPICELIB Version 1.0.0, 15-JUL-1992 (NJB) (WLT)
C
C-&
 
 
C$ Index_Entries
C
C     read DAS file record
C     read DAS internal file name
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.1.0, 25-AUG-1995 (NJB)
C
C        Bug fix:  local variables are now used in the direct
C        access of the file record.  Previously, the routine read
C        directly into the CHARACTER*(*) arguments IDWORD and IFNAME.
C
C-    SPICELIB Version 2.0.0, 27-OCT-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C        Removed the DASID parameter which had the value 'NAIF/DAS', as
C        it was not used and is also made obsolute by the change in the
C        format of the ID word being implemented.
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
C        Changed the example so that it does not set a value for IFNAME
C        before calling DASRFR. This appears to have been a cut and
C        paste bug from DASWFR.
C
C-    SPICELIB Version 1.0.0, 15-JUL-1992 (NJB) (WLT)
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
      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN =  8 )
 
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )
 
C
C     Local variables
C
      CHARACTER*(IDWLEN)    TMPIDW
      CHARACTER*(IFNLEN)    TMPIFN
 
      INTEGER               IOSTAT
      INTEGER               UNIT
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASRFR' )
      END IF
 
C
C     Get the logical unit for this DAS file.
C
      CALL DASHLU ( HANDLE, UNIT )
 
      IF ( FAILED () ) THEN
         CALL CHKOUT ( 'DASRFR' )
         RETURN
      END IF
 
      READ ( UNIT,
     .       REC     =  1,
     .       IOSTAT  =  IOSTAT )     TMPIDW,
     .                               TMPIFN,
     .                               NRESVR,
     .                               NRESVC,
     .                               NCOMR,
     .                               NCOMC
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL SETMSG ( 'Could not read file record.  File was ' //
     .                 '#.  IOSTAT was #.'                       )
         CALL ERRFNM ( '#', UNIT                                 )
         CALL ERRINT ( '#', IOSTAT                               )
         CALL SIGERR ( 'SPICE(DASFILEREADFAILED)'                )
         CALL CHKOUT ( 'DASRFR'                                  )
         RETURN
 
      END IF
 
 
      IDWORD  =  TMPIDW
      IFNAME  =  TMPIFN
 
 
      CALL CHKOUT ( 'DASRFR' )
      RETURN
      END
