C$Procedure   EKOPW ( EK, open file for writing )
 
      SUBROUTINE EKOPW ( FNAME, HANDLE )
 
C$ Abstract
C
C     Open an existing E-kernel file for writing.
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
 
 
      CHARACTER*(*)         FNAME
      INTEGER               HANDLE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     FNAME      I   Name of EK file.
C     HANDLE     O   Handle attached to EK file.
C
C$ Detailed_Input
C
C     FNAME          is the name of an existing E-kernel file to be
C                    opened for write access.
C
C$ Detailed_Output
C
C     HANDLE         is the DAS file handle of the EK designate by
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
C     2)  If the indicated file has the wrong architecture version, the
C         error will be diagnosed by routines called by this routine.
C
C     3)  If an I/O error occurs while reading or writing the indicated
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
C     This routine should be used to open an EK file for write access.
C
C     Opening an EK file with this routine makes the EK accessible to
C     the following SPICELIB EK access routines, all of which modify
C     the target EK file:
C
C        Begin segment:
C
C           EKBSEG
C
C        Append, insert, delete records:
C
C           EKAPPR
C           EKINSR
C           EKDELR
C
C        Add column entries:
C
C           EKACEC
C           EKACED
C           EKACEI
C
C        Update existing column entries:
C
C           EKUCEC
C           EKUCED
C           EKUCEI
C
C        Execute fast write:
C
C           EKIFLD
C           EKFFLD
C           EKACEC
C           EKACED
C           EKACEI
C
C     An EK opened for write access is also accessible for reading.
C     The file may be accessed by the SPICELIB EK readers
C
C           EKRCEC
C           EKRCED
C           EKRCEI
C
C        and summary routines:
C
C           EKNSEG
C           EKSSUM
C
C
C     An EK opened for write access cannot be queried.  To make an EK
C     available to the EK query system, the file must be loaded via
C     EKLEF, rather than by this routine.  See the EK Required Reading
C     for further information.
C
C$ Examples
C
C     1)  Open the file MY.EK for write access:
C
C            CALL EKOPW ( 'MY.EK', HANDLE )
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
C-    SPICELIB Version 1.0.1, 09-JAN-2002 (NJB)
C
C        Documentation change:  instances of the phrase "fast load"
C        were replaced with "fast write."
C
C-    Beta Version 1.0.0, 26-SEP-1995 (NJB)
C
C-&
 
 
C$ Index_Entries
C
C     open EK for writing
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKOPW' )
      END IF
 
C
C     Open the file as a DAS file.
C
      CALL DASOPW ( FNAME, HANDLE )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKOPW' )
         RETURN
      END IF
 
C
C     Nothing doing unless the architecture is correct.  This file
C     should be a paged DAS EK.
C
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )
 
 
      CALL CHKOUT ( 'EKOPW' )
      RETURN
      END
