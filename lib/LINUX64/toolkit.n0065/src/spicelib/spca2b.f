C$Procedure SPCA2B ( SPK and CK, ASCII to binary )
 
      SUBROUTINE SPCA2B ( TEXT, BINARY )
 
C$ Abstract
C
C     Convert a text (ASCII) format SPK or CK file to an equivalent
C     binary file, including comments.
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
 
      CHARACTER*(*)         TEXT
      CHARACTER*(*)         BINARY
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TEXT       I   Name of an existing text format SPK or CK file.
C     BINARY     I   Name of a binary SPK or CK file to be created.
C
C$ Detailed_Input
C
C     TEXT        is the name of an existing text format SPK or CK
C                 file that may contain comments in the appropriate
C                 SPC format, as written by SPCB2A or SPCB2T.  This
C                 file is unchanged by calling SPCA2B.
C
C     BINARY      is the name of a binary SPK or CK file to be created.
C                 The binary file contains the same data and comments
C                 as the text file, but in the binary format required
C                 for use with the SPICELIB reader subroutines.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     See arguments TEXT and BINARY above.
C
C$ Exceptions
C
C     1) If there is an IOSTAT error while opening, reading,
C        or writing a file, a routine that SPCA2B calls will
C        diagnose and signal an error.
C
C     2) If the text file is not in the correct format, a
C        routine that SPCA2B calls will diagnose and signal
C        an error.
C
C$ Particulars
C
C     The SPICELIB SPK and CK reader subroutines read binary files.
C     However, because different computing environments have different
C     binary representations of numbers, you must convert SPK and CK
C     files to text format when porting from one system to another.
C     After converting the file to text, you can transfer it using
C     a transfer protocol program like Kermit or FTP.  Then, convert
C     the text file back to binary format.
C
C     The following is a list of the SPICELIB routines that convert
C     SPK and CK files between binary and text format:
C
C        SPCA2B    converts text to binary.  It opens the text file,
C                  creates a new binary file, and closes both files.
C
C        SPCB2A    converts binary to text.  It opens the binary file,
C                  creates a new text file, and closes both files.
C
C        SPCT2B    converts text to binary.  It creates a new binary
C                  file and closes it.  The text file is open on
C                  entrance and exit.
C
C        SPCB2T    converts binary to text.  It opens the binary
C                  file and closes it.  The text file is open on
C                  entrance and exit
C
C     See the SPC required reading for more information
C     about SPC routines and the SPK and CK file formats.
C
C$ Examples
C
C     This is an example of how to use SPCB2A and SPCA2B for
C     transferring files.  Suppose A.BSP is a binary SPK file in
C     environment 1; to transfer it to environment 2, follow
C     these three steps:
C
C        1) Call SPCB2A within a program in environment 1 to convert
C           the file to text:
C
C              CALL SPCB2A ( 'A.BSP', 'A.TSP' )
C
C        2) Transfer the text file from environment 1 to environment 2
C           using FTP, Kermit, or some other file transfer utility,
C           for example,
C
C              ftp> put A.TSP
C
C        3) Call SPCA2B within a program in environment 2 to convert
C           the file to binary on the new machine,
C
C              CALL SPCA2B ( 'A.TSP', 'A.BSP' )
C
C$ Restrictions
C
C     1)  This routine assumes that the data and comments in the
C         text format SPK or CK file come from a binary file
C         and were written by one of the routines SPCB2A or SPCB2T.
C         Data and/or comments written any other way may not be
C         in the correct format and, therefore, may not be handled
C         properly.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J.E. McLean    (JPL)
C     H.A. Neilan    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.1.0, 05-SEP-1991 (HAN)
C
C        Removed declarations of unused variables.
C
C-    SPICELIB Version 1.0.0, 05-APR-1991 (JEM)
C
C-&
 
C$ Index_Entries
C
C     ascii spk or ck to binary
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local parameters
C
      CHARACTER*(*)         BMARK
      PARAMETER           ( BMARK ='~NAIF/SPC BEGIN COMMENTS~')
 
      CHARACTER*(*)         EMARK
      PARAMETER           ( EMARK ='~NAIF/SPC END COMMENTS~'  )
 
C     Local variables
C
      INTEGER               UNIT
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPCA2B' )
      END IF
 
C
C     Open the text file with read access.  SPCT2B will
C     create the binary file and write the data and comments
C     to it.  Then we close the text file, and we're done.
C
      CALL TXTOPR ( TEXT,   UNIT )
 
      CALL SPCT2B ( UNIT, BINARY )
 
      CLOSE       (         UNIT )
 
 
      CALL CHKOUT ( 'SPCA2B' )
      RETURN
      END
