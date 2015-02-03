C$Procedure DAFA2B ( DAF, ASCII to binary )
 
      SUBROUTINE DAFA2B ( ASCII, BINARY, RESV )
 
C$ Abstract
C
C     Deprecated. The routine DAFTB supersedes this routine.
C     NAIF supports this routine only to provide backward 
C     compatibility.
C
C     Convert an ASCII (text) DAF to an equivalent binary DAF.
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
 
      CHARACTER*(*)         ASCII
      CHARACTER*(*)         BINARY
      INTEGER               RESV
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ASCII      I   Name of an existing ASCII (text) DAF.
C     BINARY     I   Name of a binary DAF to be created.
C     RESV       I   Number of records to reserve.
C
C$ Detailed_Input
C
C     ASCII       is the name of an existing ASCII (text) DAF.
C
C     BINARY      is the name of the binary DAF to be created.
C                 The binary DAF contains the same data as the
C                 ASCII DAF, but in a form more suitable for use
C                 by application programs.
C
C     RESV        is the number of records to be reserved in the
C                 binary DAF.
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
C     None.
C
C     Errors are detected and signalled by routines called by this
C     routine.
C
C$ Files
C
C     See arguments ASCII, BINARY.
C
C$ Particulars
C
C     This routine has been made obsolete by the new DAF text to binary
C     conversion routine DAFTB. This routine remains available for
C     reasons of backward compatibility. We strongly recommend that the
C     conversion routine DAFTB be used for any new software development.
C     Please see the header of the routine DAFTB for details.
C
C     This routine is used for converting older DAF text files, which
C     use a decimal format for numbers, into their equivalent binary
C     formats. Note that the routine DAFTB makes use of a text file
C     format that is incompatible with the text file format expected by
C     the routines called by this routine.
C
C     Note that you must select the number of records to be reserved
C     in the binary DAF. The contents of reserved records are ignored
C     by the normal transfer process.
C
C$ Examples
C
C     DAFB2A and DAFA2B are typically used to transfer files.
C     If file A.DAF is a binary DAF in environment 1, it
C     can be transferred to environment 2 in three steps.
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
C     DAFA2B cannot be executed while any other DAF is open
C     for writing.
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
C-    SPICELIB Version 2.0.1, 26-JUL-2012 (EDW)
C
C        Edited Abstract section to use "Deprecated" keyword
C        and state replacement routine.
C
C        Eliminated unneeded Revisions section.
C
C-    SPICELIB Version 2.0.0, 30-SEP-1993 (KRG)
C
C        This routine was completely rewritten to make use of the
C        routines DAFT2B and TXTOPR, for converting a text file to
C        binary and opening a text file. It now simply calls the
C        routine DAFT2B after opening the text file.
C
C        Added a statement to the $ Particulars section to the effect
C        that this routine has been made obsolete by the introduction of
C        the routine DAFTB, and that the use of the new routine is
C        strongly recommended for new software development.
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
C     ascii daf to binary
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
         CALL CHKIN ( 'DAFA2B' )
      END IF
 
C
C     Open the ASCII file for reading. If an error occurs, then check
C     out and return. An appropriate error message will have already
C     been set.
C
      CALL TXTOPR ( ASCII, UNIT )
 
      IF ( FAILED () ) THEN
 
         CALL CHKOUT ( 'DAFA2B' )
         RETURN
 
      END IF
 
C
C     Call DAFT2B to perform the conversion. If it fails, then just
C     check out and return, as an appropriate error message should have
C     already been set. Also close the text file that we opened.
C
      CALL DAFT2B ( UNIT, BINARY, RESV )
 
      IF ( FAILED() ) THEN
 
         CLOSE       ( UNIT     )
         CALL CHKOUT ( 'DAFA2B' )
         RETURN
 
      END IF
 
C
C     Close the file.
C
      CLOSE ( UNIT )
 
      CALL CHKOUT ( 'DAFA2B' )
      RETURN
      END
