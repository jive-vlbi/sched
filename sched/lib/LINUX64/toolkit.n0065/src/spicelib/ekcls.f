C$Procedure   EKCLS ( EK, close file )
 
      SUBROUTINE EKCLS ( HANDLE )
 
C$ Abstract
C
C     Close an E-kernel.
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
 
 
      INTEGER               HANDLE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   EK file handle.
C
C$ Detailed_Input
C
C     HANDLE         is the file handle of an EK to be closed.  Note
C                    that EKs open for writing must be closed by this
C                    routine in order by be valid.
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
C     1)  If the indicated file is not recognized, no error is
C         signalled.
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
C     This routine should be used to close open EK files.  EK files
C     open for writing *must* be closed by this routine in order to be
C     valid.  EK files open for read access should also be closed using
C     this routine.
C
C     EKs open for reading won't be corrupted if closed via a FORTRAN
C     CLOSE statement, but the underlying bookkeeping software will
C     become confused if an EK is closed this way---so we recommend
C     closing EK files with EKCLS exclusively.
C
C$ Examples
C
C     1)  Add data to an existing EK file, then close the file.
C
C            CALL EKOPW ( 'MY.EK', HANDLE )
C
C               [add data]
C
C            CALL EKCLS ( HANDLE )
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
C-    SPICELIB Version 1.0.1, 31-MAR-1998 (NJB)
C
C        Corrected Index_Entries section.
C
C-    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)
C
C-&
 
 
C$ Index_Entries
C
C     close EK 
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKCLS' )
      END IF
 
C
C     Close the file as a DAS file.
C
      CALL DASCLS ( HANDLE )
 
      CALL CHKOUT ( 'EKCLS' )
      RETURN
      END
