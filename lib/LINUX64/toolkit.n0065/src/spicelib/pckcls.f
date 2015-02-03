C$Procedure PCKCLS ( PCK, close file )

      SUBROUTINE PCKCLS ( HANDLE )

C$ Abstract
C
C     Close an open PCK file.
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
C     PCK
C
C$ Keywords
C
C     PCK
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of the PCK file to be closed.
C
C$ Detailed_Input
C
C     HANDLE   The handle of the PCK file that is to be closed.
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
C     1) If there are no segments in the file the error
C        SPICE(NOSEGMENTSFOUND) will be signalled.
C
C$ Files
C
C      See argument HANDLE.
C
C$ Particulars
C
C     Close the PCK file attached to HANDLE.
C
C$ Examples
C
C     Suppose that you want to create a new PCK file called 'new.PCK'
C     that contains a single type 2 PCK segment and has room for at
C     least 5000 comment characters. The following code fragment should
C     take care of this for you, assuming that all of the variables
C     passed to the PCK type 2 segment writer have appropriate values.
C
C        NAME   = 'new.pck'
C        IFNAME = 'Test PCK file'
C
C        CALL PCKOPN ( NAME, IFNAME, 5000, HANDLE )
C        CALL PCKW02 ( HANDLE, BODY,   FRAME,  FIRST,   LAST,
C     .                SEGID,  INTLEN, N,      POLYDG,  CDATA,
C     .                BTIME                                   )
C        CALL PCKCLS ( HANDLE )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      K.R. Gehringer    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 03-JAN-2014 (EDW)
C
C        Minor edits to Procedure; clean trailing whitespace.
C        Corrected order of header sections to conform to NAIF 
C        standard.
C
C-    SPICELIB Version 1.1.0, 27-NOV-2001 (FST)
C
C        Removed DAFHLU call; replaced ERRFN call with ERRHAN.
C
C-    SPICELIB Version 1.0.0, 27-JAN-1995 (KRG)
C
C-&

C$ Index_Entries
C
C     close a pck file
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
      INTEGER               ACCLEN
      PARAMETER           ( ACCLEN = 5 )

C
C     Local Variables
C
      CHARACTER*(ACCLEN)    ACCESS

      LOGICAL               FOUND
C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'PCKCLS' )

C
C     Get the access method for the file. Currently, if HANDLE < 0, the
C     access method is 'WRITE'. If HANDLE > 0, the access method is
C     'READ'.  In the future this should make use of the private entry
C     in the handle manager umbrella, ZZDDHNFO.
C
      IF ( HANDLE .LT. 0 ) THEN
         ACCESS = 'WRITE'
      ELSE IF ( HANDLE .GT. 0 ) THEN
         ACCESS = 'READ'
      END IF

C
C     If the file is open for writing and there are segments in the file
C     fix the ID word and close the file, or just close the file.
C
      IF ( ACCESS .EQ. 'WRITE' ) THEN
C
C        Check to see if there are any segments in the file. If there
C        are no segments, we signal an error. This probably indicates a
C        programming error of some sort anyway. Why would you create a
C        file and put nothing in it?
C
         CALL DAFBFS ( HANDLE )
         CALL DAFFNA ( FOUND )

         IF ( FAILED () ) THEN
            CALL CHKOUT ( 'PCKCLS' )
            RETURN
         END IF

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'No segments were found in the PCK file'
     .      //            ' ''#''. There must be at least one segment'
     .      //            ' in the file when this subroutine is'
     .      //            ' called.'                                   )
            CALL ERRHAN ( '#', HANDLE                                  )
            CALL SIGERR ( 'SPICE(NOSEGMENTSFOUND)'                     )
            CALL CHKOUT ( 'PCKCLS'                                     )
            RETURN

         END IF

      END IF
C
C     Close the file.
C
      CALL DAFCLS ( HANDLE )

C
C     No need to check FAILED() here, since we just return. The caller
C     should check it though.
C
      CALL CHKOUT ( 'PCKCLS' )
      RETURN

      END
