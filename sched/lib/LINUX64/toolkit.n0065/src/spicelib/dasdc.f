C$Procedure      DASDC    ( DAS delete comments )
 
      SUBROUTINE DASDC ( HANDLE )
 
C$ Abstract
C
C     Delete the entire comment area of a previously opened binary
C     DAS file.
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
C     None.
C
C$ Declarations
 
      INTEGER               HANDLE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   The handle of a binary DAS file opened for writing.
C
C$ Detailed_Input
C
C     HANDLE    The handle of a binary DAS file that is to have its
C               entire comment area deleted. The DAS file should have
C               been opened with write access.
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
C     1)   If the binary DAS file attached to HANDLE is not open with
C          write access, an error will be signalled by a routine called
C          by this routine.
C
C$ Files
C
C     See argument HANDLE in $ Detailed_Input.
C
C$ Particulars
C
C     Binary DAS files contain an area which is reserved for storing
C     annotations or descriptive textual information about the data
C     contained in a file. This area is referred to as the ``comment
C     area'' of the file. The comment area of a DAS file is a line
C     oriented medium for storing textual information. The comment
C     area preserves any leading or embedded white space in the line(s)
C     of text which are stored, so that the appearance of the of
C     information will be unchanged when it is retrieved (extracted) at
C     some other time. Trailing blanks, however, are NOT preserved,
C     due to the way that character strings are represented in
C     standard Fortran 77.
C
C     This routine will delete the entire comment area from the binary
C     DAS file attached to HANDLE. The size of the binary DAS file will
C     remain unchanged. The space that was used by the comment records
C     is reclaimed.
C
C$ Examples
C
C     Let
C
C           HANDLE   be the handle for a DAS file which has been opened
C                    with write access.
C
C     The call
C
C           CALL DASDC ( HANDLE )
C
C     will delete the entire comment area of the binary DAS file
C     attached to HANDLE.
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
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 26-OCT-1993 (KRG)
C
C        Changed the $Brief_I/O description of handle. It now mentions
C        that the file must be open for writing. Also added a statement
C        to the $ Detailed_Input section to the effect that the DAS file
C        should have been opened with write access.
C
C-    SPICELIB Version 1.0.0, 24-NOV-1992 (KRG)
C
C-&
 
C$ Index_Entries
C
C      delete das comment area
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.0.1, 26-OCT-1993 (KRG)
C
C        Changed the $Brief_I/O description of handle. It now mentions
C        that the file must be open for writing. Also added a statement
C        to the $ Detailed_Input section to the effect that the DAS file
C        should have been opened with write access.
C
C-    SPICELIB Version 1.0.0, 24-NOV-1992 (KRG)
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
C     Length of a DAS file ID word.
C
      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN = 8 )
C
C     Length of a DAS file internal filename.
C
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )
C
C     Local variables
C
      CHARACTER*(IDWLEN)    IDWORD
      CHARACTER*(IFNLEN)    IFNAME
 
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NRESVC
      INTEGER               NRESVR
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASDC' )
      END IF
C
C     Verify that the DAS file attached to HANDLE is opened with write
C     access.
C
      CALL DASSIH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'DASDC' )
         RETURN
 
      END IF
C
C     Read the file record to obtain the current number of comment
C     records in the DAS file attached to HANDLE. We will also get
C     back some extra stuff that we do not use.
C
      CALL DASRFR ( HANDLE,
     .              IDWORD, IFNAME,
     .              NRESVR, NRESVC,
     .              NCOMR,  NCOMC   )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'DASDC' )
         RETURN
 
      END IF
C
C     Now we will attempt to remove the comment records, if there are
C     any, otherwise we do nothing.
C
      IF ( NCOMR .GT. 0 ) THEN
 
         CALL DASRCR ( HANDLE, NCOMR )
 
         IF ( FAILED() ) THEN
 
            CALL CHKOUT ( 'DASDC' )
            RETURN
 
         END IF
C
C        Now we need to update the DAS file record.
C
C        Read in the updated file record since it has been modified:
C        we deleted all of the comment records.
C
         CALL DASRFR ( HANDLE,
     .                 IDWORD, IFNAME,
     .                 NRESVR, NRESVC,
     .                 NCOMR,  NCOMC   )
 
         IF ( FAILED() ) THEN
 
            CALL CHKOUT ( 'DASDC' )
            RETURN
 
         END IF
C
C        Zero out the number of comment characters, and write the
C        updated file record to the file.
C
         NCOMC = 0
 
         CALL DASWFR ( HANDLE,
     .                 IDWORD, IFNAME,
     .                 NRESVR, NRESVC,
     .                 NCOMR,  NCOMC   )
 
         IF ( FAILED() ) THEN
 
            CALL CHKOUT ( 'DASDC' )
            RETURN
 
         END IF
 
      END IF
C
C     We're done now, so goodbye.
C
      CALL CHKOUT ( 'DASDC' )
      RETURN
      END
