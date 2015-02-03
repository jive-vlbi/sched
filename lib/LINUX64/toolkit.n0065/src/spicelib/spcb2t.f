 
C$Procedure SPCB2T ( SPK and CK, binary to text )
 
      SUBROUTINE SPCB2T ( BINARY, UNIT )
 
C$ Abstract
C
C     Convert the contents of a binary SPK or CK file to text,
C     including comments if present, and write them to a text file
C     opened by the calling program.
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
 
      CHARACTER*(*)         BINARY
      INTEGER               UNIT
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     BINARY     I   Name of an existing binary SPK or CK file.
C     UNIT       I   Logical unit connected to a text file.
C
C$ Detailed_Input
C
C     BINARY      is the name of an existing binary SPK or CK file
C                 that may contain comments in its comment area.
C
C     UNIT        is the logical unit connected to a text file that
C                 has been opened for write access.  Use the routine
C                 TXTOPN to open this file.  Upon exit, this file will
C                 contain the same data and comments as the binary
C                 file, but in text format which is more suitable for
C                 transfer between heterogeneous computing environments.
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
C     See arguments BINARY and UNIT above.
C
C$ Exceptions
C
C     1) If there is a problem opening or reading from the binary file,
C        a routine that SPCB2T calls diagnoses and signals an error.
C
C     2) If there is a problem writing to the text file,
C        the error SPICE(FILEWRITEFAILED) is signalled.
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
C     The following code fragment creates a text file containing
C     text format SPK data and comments preceded and followed
C     by a standard label.
C
C     The SPICELIB routine TXTOPN opens a new text file and TXTOPR
C     opens an existing text file for read access.  TEXT and
C     BINARY are character strings that contain the names of the
C     text and binary files.
C
C            CALL TXTOPN ( TEXT, UNIT )
C
C            (Write header label to UNIT)
C
C            CALL SPCB2T ( BINARY, UNIT )
C
C            (Write trailing label to UNIT)
C
C            CLOSE ( UNIT )
C
C
C     The following code fragment reconverts the text format
C     SPK data and comments back into binary format.
C
C            CALL TXTOPR ( TEXT, UNIT )
C
C            (Read, or just read past, header label from UNIT)
C
C            CALL SPCT2B ( UNIT, BINARY )
C
C            (Read trailing label from UNIT, if desired )
C
C            CLOSE ( UNIT )
C
C$ Restrictions
C
C     1)  This routine assumes that the comment area of the binary SPK
C         or CK file contains only text stored by SPCAC.  Comments
C         written any other way may not be handled properly.
C
C     2)  UNIT must be obtained via TXTOPN.  Use TXTOPN to open new
C         text files for write access and get the logical unit.
C         System dependencies regarding opening text files have
C         been isolated in the routines TXTOPN and TXTOPR.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J.E. McLean    (JPL)
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
C     binary spk or ck to text
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local parameters
C
C     IFNLEN is the length of a DAF internal file name.
C
      CHARACTER*(*)         BMARK
      PARAMETER           ( BMARK ='~NAIF/SPC BEGIN COMMENTS~')
 
      CHARACTER*(*)         EMARK
      PARAMETER           ( EMARK ='~NAIF/SPC END COMMENTS~'  )
 
C
C     Local variables
C
      INTEGER               HANDLE
      INTEGER               IOSTAT
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPCB2T' )
      END IF
 
C
C     First, convert the binary data to text and write it to
C     the text file.
C
      CALL DAFB2T ( BINARY, UNIT )
 
C
C     Next, write the begin comments marker.
C
      WRITE ( UNIT, *, IOSTAT=IOSTAT ) BMARK
 
      IF ( IOSTAT .NE. 0 ) THEN
         CALL SETMSG ( 'Error writing the begin comments marker '     //
     .                 'to the text file named FNM.  IOSTAT = #.'     )
         CALL ERRFNM ( 'FNM', UNIT                                    )
         CALL ERRINT ( '#', IOSTAT                                    )
         CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'                       )
         CALL CHKOUT ( 'SPCB2T'                                       )
         RETURN
      END IF
 
C
C     Open the DAF for read access, extract the comments from
C     it and write them to the text file, then close the DAF.
C     If the comment area of the binary file is empty, SPCEC
C     writes nothing to the text file, but even so, we still
C     want the markers.
C
      CALL DAFOPR ( BINARY, HANDLE )
      CALL SPCEC  ( HANDLE, UNIT   )
      CALL DAFCLS ( HANDLE         )
 
C
C     Finally, write the end comments marker.
C
      WRITE ( UNIT, *, IOSTAT=IOSTAT ) EMARK
 
      IF ( IOSTAT .NE. 0 ) THEN
         CALL SETMSG ( 'Error writing the end comments marker '       //
     .                 'to the text file named FNM.  IOSTAT = #.'     )
         CALL ERRFNM ( 'FNM', UNIT                                    )
         CALL ERRINT ( '#', IOSTAT                                    )
         CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'                       )
         CALL CHKOUT ( 'SPCB2T'                                       )
         RETURN
      END IF
 
      CALL CHKOUT ( 'SPCB2T' )
      RETURN
      END
