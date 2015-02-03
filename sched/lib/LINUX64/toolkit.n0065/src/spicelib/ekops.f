C$Procedure   EKOPS ( EK, open scratch file )
 
      SUBROUTINE EKOPS ( HANDLE )
 
C$ Abstract
C
C     Open a scratch E-kernel file and prepare the file for writing.
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
C     EK
C
C$ Keywords
C
C     EK
C     FILES
C     UTILITY
C
C$ Declarations
 
      INCLUDE 'ektype.inc'
      INCLUDE 'ekfilpar.inc'
 
      INTEGER               HANDLE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     O   File handle attached to new EK file.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     HANDLE         is the EK file handle of the file designated by
C                    FNAME.  This handle is used to identify the file
C                    to other EK routines.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the indicated file cannot be opened, the error will be
C         diagnosed by routines called by this routine.  The new file
C         will be deleted.
C
C     2)  If an I/O error occurs while reading or writing the indicated
C         file, the error will be diagnosed by routines called by this
C         routine.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine operates by side effects:  it opens and prepares
C     an EK for addition of data.
C
C$ Examples
C
C     1)  Open a scratch EK.  The EK should be closed via EKCLS.
C         The EK file will be deleted when closed.
C
C
C             CALL EKOPS ( HANDLE )
C
C                [Write/Read EK]
C
C             CALL EKCLS ( HANDLE )
C
C
C$ Restrictions
C
C     1)  No more than FTSIZE DAS files may be opened simultaneously.
C         See DASFM for the value of FTSIZE.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 26-SEP-1995 (NJB)
C
C-&
 
 
C$ Index_Entries
C
C     open scratch E-kernel
C     open scratch EK
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               BASE
      INTEGER               P
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKOPS' )
      END IF
 
      CALL DASOPS ( HANDLE )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKOPS' )
         RETURN
      END IF
 
C
C     Initialize the file for paged access.  The EK architecture
C     code is automatically set by the paging initialization routine.
C
      CALL ZZEKPGIN ( HANDLE )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKOPS' )
         RETURN
      END IF
 
C
C     Allocate the first integer page for the file's metadata.  We
C     don't need to examine the page number; it's 1.
C
      CALL ZZEKPGAN ( HANDLE, INT, P, BASE )
 
C
C     Initialize a new tree.  This tree will point to the file's
C     segments.
C
      CALL ZZEKTRIT ( HANDLE, P )
 
C
C     Save the segment pointer's root page number.
C
      CALL DASUDI ( HANDLE, BASE+SGTIDX, BASE+SGTIDX, P )
 
C
C     That's it.  We're ready to add data to the file.
C
      CALL CHKOUT ( 'EKOPS' )
      RETURN
      END
