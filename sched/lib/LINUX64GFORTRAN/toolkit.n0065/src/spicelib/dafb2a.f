C$Procedure DAFB2A ( DAF, binary to ASCII )
 
      SUBROUTINE DAFB2A ( BINARY, ASCII )
 
C$ Abstract
C
C     Deprecated. The routine DAFBT supersedes this routine.
C     NAIF supports this routine only to provide backward 
C     compatibility.
C
C     Convert a binary DAF to an equivalent ASCII (text) DAF.
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
C     FILES
C
C$ Declarations
 
      CHARACTER*(*)         BINARY
      CHARACTER*(*)         ASCII
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     BINARY     I   Name of an existing binary DAF.
C     ASCII      I   Name of an ASCII (text) DAF to be created.
C
C$ Detailed_Input
C
C     BINARY      is the name of an existing binary DAF.
C
C     ASCII       is the name of an ASCII (text) DAF to be created.
C                 The ASCII file contains the same data as the binary
C                 file, but in a form more suitable for transfer
C                 between heterogeneous computing environments.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     None.
C
C     Errors are detected and signalled by routines called by this
C     routine.
C
C$ Files
C
C     See arguments BINARY, ASCII.
C
C$ Particulars
C
C     This routine has been made obsolete by the new DAF binary to text
C     conversion routine DAFBT. This routine remains available for
C     reasons of backward compatibility. We strongly recommend that the
C     conversion routine DAFBT be used for any new software development.
C     Please see the header of the routine DAFBT for details.
C
C     Note that the contents of reserved records in the binary file
C     are not stored in the ASCII file.
C
C$ Examples
C
C     DAFB2A and DAFA2B are typically used to transfer files.
C     If file A.DAF is a binary DAF in environment 1, it can be
C     transferred to environment 2 in three steps.
C
C        1) Convert it to ASCII,
C
C              CALL DAFB2A ( 'A.DAF', 'A.ASCII' )
C
C        2) Transfer the ASCII file, using FTP, Kermit, or some other
C           file transfer utility,
C
C              ftp> put a.ascii
C
C        3) Convert it to binary on the new machine,
C
C              CALL DAFA2B ( 'A.ASCII', 'A.DAF', RESV )
C
C     Note that DAFB2A and DAFA2B work in any standard Fortran-77
C     environment.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     NAIF Document 167.0, "Double Precision Array Files (DAF)
C     Specification and User's Guide"
C
C$ Author_and_Institution
C
C     K.R. Gehringer  (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.1.1, 26-JUL-2012 (EDW)
C
C        Edited Abstract section to use "Deprecated" keyword
C        and state replacement routine.
C
C        Eliminated unneeded Revisions section.
C
C-    SPICELIB Version 2.1.0, 18-JUN-1999 (WLT)
C
C        Fixed call to CHKOUT with wrong name.
C
C-    SPICELIB Version 2.0.0, 04-OCT-1993 (KRG)
C
C        This routine was completely rewritten to make use of the
C        routines DAFB2T and TXTOPN, for converting a text file to
C        binary and opening a text file. It now simply calls the
C        routine DAFT2B after opening the text file with TXTOPN.
C
C        Added a statement to the $ Particulars section to the effect
C        that this routine has been made obsolete by the introduction of
C        the routine DAFBT, and that we strongly recommend the use of
C        the new routine.
C
C        Modified the $ Abstract section to reflect the fact that this
C        routine is obsolete.
C
C-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     binary daf to ascii
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
      INTEGER               UNIT
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFB2A' )
      END IF
C
C     Open the ASCII file for writing. If an error occurs, then check
C     out and return. An appropriate error message will have already
C     been set.
C
      CALL TXTOPN ( ASCII, UNIT )
 
      IF ( FAILED () ) THEN
 
         CALL CHKOUT ( 'DAFB2A' )
         RETURN
 
      END IF
C
C     Attempt to perform the file conversion. If it fails, close the
C     text file with STATUS = 'DELETE', check out and return, as an
C     appropriate error message should have already been set.
C
      CALL DAFB2T ( BINARY, UNIT )
 
      IF ( FAILED() ) THEN
 
         CLOSE       ( UNIT, STATUS='DELETE' )
         CALL CHKOUT ( 'DAFB2A'              )
         RETURN
 
      END IF
C
C     Close the text file.
C
      CLOSE ( UNIT )
 
      CALL CHKOUT ( 'DAFB2A' )
      RETURN
      END
