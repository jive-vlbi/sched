 
C$Procedure      SPKOPN ( SPK, open new file. )
 
      SUBROUTINE SPKOPN ( NAME, IFNAME, NCOMCH, HANDLE )
 
C$ Abstract
C
C     Create a new SPK file, returning the handle of the opened file.
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
C     SPK
C
C$ Keywords
C
C     SPK
C
C$ Declarations
 
      IMPLICIT NONE
 
      CHARACTER*(*)         NAME
      CHARACTER*(*)         IFNAME
      INTEGER               NCOMCH
      INTEGER               HANDLE
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   The name of the new SPK file to be created.
C     IFNAME     I   The internal filename for the SPK file.
C     NCOMCH     I   The number of characters to reserve for comments.
C     HANDLE     O   The handle of the opened SPK file.
C
C$ Detailed_Input
C
C     NAME     The name of the new SPK file to be created.
C
C     IFNAME   The internal filename for the SPK file that is being
C              created. The internal filename may be up to 60 characters
C              long. If you do not have any conventions for tagging your
C              files, an internal filename of 'SPK_file' is perfectly
C              acceptable. You may also leave it blank if you like.
C
C     NCOMCH   This is the space, measured in characters, to be
C              initially set aside for the comment area when a new SPK
C              file is opened. The amount of space actually set aside
C              may be greater than the amount requested, due to the
C              manner in which comment records are allocated in an SPK
C              file. However, the amount of space set aside for comments
C              will always be at least the amount that was requested.
C
C              The value of NCOMCH should be greater than or equal to
C              zero, i.e., 0 <= NCOMCH. A negative value, should one
C              occur, will be assumed to be zero.
C
C$ Detailed_Output
C
C     HANDLE   The handle of the opened SPK file. If an error occurs
C              when opening the file, the value of this variable should
C              not be used, as it will not represent a valid handle.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the value of NCOMCH is negative, a value of zero (0) will
C        be used for the number of comment characters to be set aside
C        for comments.
C
C     2) If an error occurs while attempting to open a CK file the
C        value of HANDLE will not represent a valid file handle.
C
C$ Files
C
C     See NAME and HANDLE.
C
C$ Particulars
C
C     Open a new SPK file, reserving room for comments if requested.
C
C$ Examples
C
C     Suppose that you want to create a new SPK file called 'new.spk'
C     that contains a single type 5 SPK segment and has room for at
C     least 5000 comment characters. The following code fragment should
C     take care of this for you, assuming that all of the variables
C     passed to the SPK type 5 segment writer have appropriate values
C     and no errors occur.
C
C        NAME   = 'new.spk'
C        IFNAME = 'Test SPK file'
C
C        CALL SPKOPN ( NAME, IFNAME, 5000,  HANDLE )
C        CALL SPKW05 ( HANDLE, OBJID, CNTRID, CFRAME, ETBEG,
C       .              ETEND, SEGMID, CNTRGM, NSTATE, STATE,
C       .              EPOCH                                 )
C        CALL SPKCLS ( HANDLE )
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman      (JPL)
C     K.R. Gehringer    (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 09-NOV-2006 (NJB)
C
C        Routine has been upgraded to support comment 
C        area allocation using NCOMCH.
C
C-    SPICELIB Version 1.0.0, 26-JAN-1995 (KRG)
C
C-&
 
C$ Index_Entries
C
C     open a new spk file
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
      CHARACTER*(*)         TYPE
      PARAMETER           ( TYPE = 'SPK'    )
 
C
C     DAF ND and NI values for SPK files.
C
      INTEGER               ND
      PARAMETER           ( ND = 2 )
 
      INTEGER               NI
      PARAMETER           ( NI = 6 )
 
C
C     Length of a DAF comment record, in characters.
C
      INTEGER               MXCREC
      PARAMETER           ( MXCREC = 1000 )
 
C
C     Local variables
C
      INTEGER               NCOMR
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'SPKOPN' )
C
C     Compute the number of comment records that we want to allocate, if
C     the number of comment characters requested is greater than zero,
C     we always allocate an extra record to account for the end of line
C     marks in the comment area.
C
C
      IF ( NCOMCH .GT. 0 ) THEN
         NCOMR = ( NCOMCH - 1 )/MXCREC  +  1
      ELSE
         NCOMR = 0
      END IF
 
C
C     Just do it. All of the error handling is taken care of for us.
C
      CALL DAFONW( NAME, TYPE, ND, NI, IFNAME, NCOMR, HANDLE )
 
      IF ( FAILED() ) THEN
C
C        If we failed, make sure that HANDLE does not contain a value
C        that represents a valid DAF file handle.
C
         HANDLE = 0
 
      END IF
 
      CALL CHKOUT ( 'SPKOPN' )
      RETURN
      END
