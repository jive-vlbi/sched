 
C$Procedure SPCDC ( SPK and CK, delete comments )
 
      SUBROUTINE SPCDC ( HANDLE )
 
C$ Abstract
C
C     Empty the comment area of a binary SPK or CK file.
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
C     SPC
C
C$ Keywords
C
C     FILES
C
C$ Declarations
 
      INTEGER               HANDLE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle assigned to binary SPK or CK file.
C
C$ Detailed_Input
C
C     HANDLE      is the handle assigned to the binary SPK or CK file
C                 which has been opened for write access.
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
C     1) If the file does not contain any comments in its comment area
C        on input, it will be unchanged by this routine.
C
C$ Files
C
C     HANDLE      is the handle assigned to the binary SPK or CK file.
C                 Use DAFOPW to open it for write access and get its
C                 handle.  Upon exit, this binary file will have an
C                 empty comment area:  all previous comments are
C                 deleted.  Note, however, that the size of the
C                 file does not change.
C
C$ Particulars
C
C     The structure of SPK and CK files accommodates comments in
C     addition to data.  The following three routines are available
C     for accessing the comment area of a binary SPK or CK file:
C
C           SPCAC           add comments
C
C           SPCEC           extract comments
C
C           SPCDC           delete comments
C
C     Note that comments must consist of only text, that is, printable
C     ASCII characters, specifically ASCII 32-126.  This excludes
C     tabs (ASCII 9) and control characters.
C
C     The SPC conversion routines---SPCB2A, SPCA2B, SPCB2T, and
C     SPCT2B---include these comments when converting SPK and CK
C     files between binary and text formats.
C
C$ Examples
C
C     1)  Suppose we have a binary SPK file called A.BSP.  The following
C         code fragment deletes any comments that may have been stored
C         in the comment area of the file.
C
C                 CALL DAFOPW ( 'A.BSP', HANDLE )
C
C                 CALL SPCDC  ( HANDLE )
C
C     2)  Suppose B.BSP is a binary SPK file with comments in its
C         comment area.  The routine TXTOPN opens a new text file.
C
C           C
C           C     Open the binary SPK file with write access and
C           C     get its handle.
C           C
C                 CALL DAFOPW ( 'B.BSP', HANDLE )
C
C           C
C           C     Open a new text file and write the comments
C           C     from the SPK file to it.
C           C
C                 CALL TXTOPN ( 'COMMENTS.TXT',   UNIT1 )
C                 CALL SPCEC  ( HANDLE,           UNIT1 )
C
C           C
C           C     Delete the comments in the SPK file.
C           C
C                 CALL SPCDC  ( HANDLE )
C
C           C
C           C     Open another new text file and try to write
C           C     comments from the SPK file to it.
C           C
C                 CALL TXTOPN ( 'NOCOMMENTS.TXT', UNIT2 )
C                 CALL SPCEC  ( HANDLE,           UNIT2 )
C
C         After executing this code fragment, COMMENTS.TXT would
C         contain the comments from the SPK file.  NOCOMMENTS.TXT
C         would be empty because of the call to SPCDC.
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
C     J.E. McLean (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 05-APR-1991 (JEM)
C
C-&
 
C$ Index_Entries
C
C     delete comments from spk or ck file
C
C-&
 
 
 
C
C     SPICELIB functions
C
 
      LOGICAL               RETURN
 
C
C     Local parameters
C
C     IFNLEN      is the length of a DAF internal file name.
C
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60     )
 
C
C     Local variables
C
      CHARACTER*(IFNLEN)    IFNAME
 
      INTEGER               BWARD
      INTEGER               FREE
      INTEGER               FWARD
      INTEGER               ND
      INTEGER               NI
      INTEGER               NRR
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPCDC' )
      END IF
 
C
C     The comment area IS the reserved records.  To empty the comment
C     area we just remove the reserved records.
C
C     Read the file record to find out how many reserved records are
C     in the DAF.  The reserved records are stored between the first
C     record (the file record) and the first summary record.  FWARD
C     is the record number of that first summary record, and NRR is
C     the number of reserved records in the file.
C
      CALL DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )
 
      NRR = FWARD - 2
 
C
C     Once we know how many there are, we can remove them.
C
      CALL DAFRRR ( HANDLE, NRR )
 
 
      CALL CHKOUT ( 'SPCDC' )
      RETURN
      END
 
