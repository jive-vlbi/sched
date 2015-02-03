 
C$Procedure SPCT2B ( SPK and CK, text to binary )
 
      SUBROUTINE SPCT2B ( UNIT, BINARY )
 
C$ Abstract
C
C     Reconstruct a binary SPK or CK file including comments
C     from a text file opened by the calling program.
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
 
      INTEGER               UNIT
      CHARACTER*(*)         BINARY
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     UNIT       I   Logical unit connected to the text format file.
C     BINARY     I   Name of a binary SPK or CK file to be created.
C
C$ Detailed_Input
C
C     UNIT        is the logical unit connected to an existing text
C                 format SPK or CK file that may contain comments in
C                 the appropriate SPC format, as written by SPCB2A or
C                 SPCB2T.  This file must be opened for read access
C                 using the routine TXTOPR.
C
C                 This file may contain text that precedes and
C                 follows the SPK or CK data and comments, however,
C                 when calling this routine, the file pointer must be
C                 in a position in the file such that the next line
C                 returned by a READ statement is
C
C                      ''NAIF/DAF''
C
C                 which marks the beginning of the data.
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
C     1)  See arguments UNIT and BINARY above.
C
C     2)  This routine uses a Fortran scratch file to temporarily
C         store the lines of comments if there are any.
C
C$ Exceptions
C
C     1) If there is a problem opening or writing to the binary
C        file, a routine that SPCT2B calls diagnoses and signals
C        an error.
C
C     2) If there is a problem reading from the text file, the
C        error SPICE(FILEREADFAILED) is signalled.
C
C     3) If there is a problem opening a scratch file, the error
C        SPICE(FILEOPENERROR) is signalled.
C
C     4) If there is a problem writing to the scratch file, the
C        error SPICE(FILEWRITEFAILED) is signalled.
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
C     1)  The following code fragment creates a text file containing
C         text format SPK data and comments preceded and followed
C         by a standard label.
C
C         The SPICELIB routine TXTOPN opens a new text file and TXTOPR
C         opens an existing text file for read access.  TEXT and
C         BINARY are character strings that contain the names of the
C         text and binary files.
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
C         The following code fragment reconverts the text format
C         SPK data and comments back into binary format.
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
C
C     2)  Suppose three text format SPK files have been appended
C         together into one text file called THREE.TSP.  The following
C         code fragment converts each set of data and comments into
C         its own binary file.
C
C            CALL TXTOPR ( 'THREE.TSP', UNIT  )
C
C            CALL SPCT2B ( UNIT, 'FIRST.BSP'  )
C            CALL SPCT2B ( UNIT, 'SECOND.BSP' )
C            CALL SPCT2B ( UNIT, 'THIRD.BSP'  )
C
C            CLOSE ( UNIT )
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
C     2)  Older versions of SPK and CK files did not have a comment
C         area.  These files, in text format, may still be converted
C         to binary using SPCT2B.  However, upon exit, the file pointer
C         will not be in position ready to read the first line of text
C         after the data.  Instead, the next READ statement after
C         calling SPCT2B will return the second line of text after
C         the data.  Therefore, example 1 may not work as desired
C         if the trailing label begins on the first line after the
C         data.  To solve this problem, use DAFT2B instead of SPCT2B.
C
C     3)  UNIT must be obtained via TXTOPR.  Use TXTOPR to open text
C         files for read access and get the logical unit.  System
C         dependencies regarding opening text files have been isolated
C         in the routines TXTOPN and TXTOPR.
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
C     text spk or ck to binary
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER               LTRIM
      INTEGER               RTRIM
      LOGICAL               RETURN
 
C
C     Local parameters
C
      CHARACTER*(*)         BMARK
      PARAMETER           ( BMARK ='~NAIF/SPC BEGIN COMMENTS~')
 
      CHARACTER*(*)         EMARK
      PARAMETER           ( EMARK ='~NAIF/SPC END COMMENTS~'  )
 
      INTEGER               MAXCPR
      PARAMETER           ( MAXCPR = 1000 )
C
C     Local variables
C
      CHARACTER*(MAXCPR)    LINE
 
      INTEGER               HANDLE
      INTEGER               IOSTAT
      INTEGER               SCRTCH
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPCT2B' )
      END IF
 
C
C     DAFT2B creates the new binary file and writes the data to
C     it.  If the 'NAIF/DAF' keyword is not the first line that
C     it reads from the text file, it will signal an error.
C     Initially, no records are reserved.
C
      CALL DAFT2B ( UNIT, BINARY, 0 )
 
C
C     The comments follow the data and are surrounded by markers.
C     BMARK should be the next line that we read.  If it isn't,
C     then this is an old file, created before the comment area
C     existed.  In this case, we've read one line too far, but
C     we can't backspace because the file was written using list-
C     directed formatting (See the ANSI standard).  All we can do
C     is check out, leaving the file pointer where it is, but
C     that's better than signalling an error.
C
      READ ( UNIT, FMT='(A)', IOSTAT=IOSTAT ) LINE
 
      IF ( IOSTAT .GT. 0 ) THEN
         CALL SETMSG ( 'Error reading the text file named FNM.  '     //
     .                 'Value of IOSTAT is #.'                        )
         CALL ERRINT ( '#', IOSTAT                                    )
         CALL ERRFNM ( 'FNM', UNIT                                    )
         CALL SIGERR ( 'SPICE(FILEREADFAILED)'                        )
         CALL CHKOUT ( 'SPCT2B'                                       )
         RETURN
      END IF
 
 
      IF (   (  LINE ( LTRIM(LINE): )  .NE.  BMARK  )   .OR.
     .       (  IOSTAT                 .LT.  0      )   ) THEN
         CALL CHKOUT ( 'SPCT2B' )
         RETURN
      END IF
 
C
C     We're not at the end of the file, and the line we read
C     is BMARK, so we write the comments to a scratch file.
C     We do this because we have to use SPCAC to add the comments
C     to the comment area of the binary file, and SPCAC rewinds
C     the file.  It's okay for SPCAC to rewind a scratch file,
C     but it's not okay to rewind the file connected to UNIT --
C     we don't know the initial location of the file pointer.
C
      CALL GETLUN ( SCRTCH )
 
      OPEN ( UNIT            =  SCRTCH,
     .       FORM            = 'FORMATTED',
     .       ACCESS          = 'SEQUENTIAL',
     .       STATUS          = 'SCRATCH',
     .       IOSTAT          =  IOSTAT      )
 
      IF ( IOSTAT .NE. 0 ) THEN
         CALL SETMSG ( 'Error opening a scratch file.  File name '    //
     .                 'was FNM.  Value of IOSTAT is #.'              )
         CALL ERRINT ( '#', IOSTAT                                    )
         CALL ERRFNM ( 'FNM', SCRTCH                                  )
         CALL SIGERR ( 'SPICE(FILEOPENERROR)'                         )
         CALL CHKOUT ( 'SPCT2B'                                       )
         RETURN
      END IF
 
      WRITE ( SCRTCH, FMT='(A)', IOSTAT=IOSTAT ) LINE(1:RTRIM(LINE))
 
      IF ( IOSTAT .NE. 0 ) THEN
         CALL SETMSG ( 'Error writing to scratch file. File name '    //
     .                 'is FNM.  Value of IOSTAT is #.'               )
         CALL ERRINT ( '#', IOSTAT                                    )
         CALL ERRFNM ( 'FNM', SCRTCH                                  )
         CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'                       )
         CALL CHKOUT ( 'SPCT2B'                                       )
         RETURN
      END IF
 
C
C     Continue reading lines from the text file and storing them
C     in the scratch file until we get to the end marker.
C
      DO WHILE (  LINE ( LTRIM(LINE): ) .NE. EMARK  )
 
         READ ( UNIT, FMT='(A)', IOSTAT=IOSTAT ) LINE
 
         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'Error reading the text file named FNM.  ' //
     .                    'Value of IOSTAT is #.'                    )
            CALL ERRINT ( '#', IOSTAT                                )
            CALL ERRFNM ( 'FNM', UNIT                                )
            CALL SIGERR ( 'SPICE(FILEREADFAILED)'                    )
            CALL CHKOUT ( 'SPCT2B'                                   )
            RETURN
         END IF
 
         WRITE ( SCRTCH, FMT='(A)', IOSTAT=IOSTAT ) LINE(1:RTRIM(LINE))
 
         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'Error writing to scratch file.  File name '//
     .                    'is FNM.  Value of IOSTAT is #.'            )
            CALL ERRINT ( '#', IOSTAT                                 )
            CALL ERRFNM ( 'FNM', SCRTCH                               )
            CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'                    )
            CALL CHKOUT ( 'SPCT2B'                                    )
            RETURN
         END IF
 
      END DO
 
C
C     Open the new binary file and add the comments that have been
C     stored temporarily in a scratch file.
C
      CALL DAFOPW ( BINARY, HANDLE )
 
      CALL SPCAC  ( HANDLE, SCRTCH, BMARK, EMARK )
 
C
C     Close the files.  The scratch file is automatically deleted.
C
      CALL DAFCLS ( HANDLE )
 
      CLOSE ( SCRTCH )
 
      CALL CHKOUT ( 'SPCT2B' )
      RETURN
      END
