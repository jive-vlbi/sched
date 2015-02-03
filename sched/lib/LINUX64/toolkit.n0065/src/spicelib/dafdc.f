C$Procedure      DAFDC ( DAF delete comments )

      SUBROUTINE DAFDC ( HANDLE )

C$ Abstract
C
C     Delete the entire comment area of a previously opened binary
C     DAF attached to HANDLE.
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
C     DAF
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
C     HANDLE     I   The handle of a binary DAF opened for writing.
C
C$ Detailed_Input
C
C     HANDLE    The handle of a binary DAF that is to have its entire 
C               comment area deleted. The DAF must have been opened 
C               with write access.
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
C     1)   If the binary DAF attached to HANDLE is not open with write 
C          access, an error will be signalled by a routine called by 
C          this routine.
C
C$ Files
C
C     See argument HANDLE in $ Detailed_Input.
C
C$ Particulars
C
C     A binary DAF contains an area which is reserved for storing
C     annotations or descriptive textual information about the data
C     contained in a file. This area is referred to as the ``comment
C     area'' of the file. The comment area of a DAF is a line 
C     oriented medium for storing textual information. The comment
C     area preserves any leading or embedded white space in the line(s)
C     of text which are stored, so that the appearance of the of
C     information will be unchanged when it is retrieved (extracted) at
C     some other time. Trailing blanks, however, are NOT preserved,
C     due to the way that character strings are represented in
C     standard Fortran 77.
C
C     This routine will delete the entire comment area from the binary
C     DAF attached to HANDLE. The size of the binary DAF will remain 
C     unchanged. The space that was used by the comment records
C     is reclaimed.
C
C$ Examples
C
C     Let 
C     
C           HANDLE   be the handle of a DAF which has been opened
C                    with write access.
C                    
C     The call
C     
C           CALL DAFDC ( HANDLE )
C          
C     deletes the entire comment area of the binary DAF attached to 
C     HANDLE.
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
C-    Beta Version 1.0.0, 23-SEP-1994 (KRG)
C
C-&

C$ Index_Entries
C
C      delete DAF comment area
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
C     Length of a DAF file internal filename.
C     
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )
C
C     Local variables
C
      CHARACTER*(IFNLEN)    IFNAME

      INTEGER               BWARD
      INTEGER               FREE
      INTEGER               FWARD
      INTEGER               NCOMR
      INTEGER               ND
      INTEGER               NI

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFDC' )
      END IF
C
C     Verify that the DAF attached to HANDLE was opened with write
C     access.
C     
      CALL DAFSIH ( HANDLE, 'WRITE' )
      
      IF ( FAILED() ) THEN
      
         CALL CHKOUT ( 'DAFDC' )
         RETURN
      
      END IF
C
C     Read the file record to obtain the current number of comment
C     records in the DAF attached to HANDLE. We will also get back some 
C     extra stuff that we do not use.
C     
      CALL DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )
 
      NCOMR = FWARD - 2
     
      IF ( FAILED() ) THEN
         
         CALL CHKOUT ( 'DAFDC' )
         RETURN
         
      END IF
C
C     Now we will attempt to remove the comment records, if there are
C     any, otherwise we do nothing.
C     
      IF ( NCOMR .GT. 0 ) THEN
C
C        We have some comment records, so remove them.
C
         CALL DAFRRR ( HANDLE, NCOMR )

         IF ( FAILED() ) THEN
      
            CALL CHKOUT ( 'DAFDC' )
            RETURN
         
         END IF

      END IF
C
C     We're done now, so goodbye.
C     
      CALL CHKOUT ( 'DAFDC' )
      RETURN
      END
