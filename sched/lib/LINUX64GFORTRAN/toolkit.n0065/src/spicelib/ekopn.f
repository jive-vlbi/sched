C$Procedure   EKOPN ( EK, open new file )
 
      SUBROUTINE EKOPN ( FNAME, IFNAME, NCOMCH, HANDLE )
 
C$ Abstract
C
C     Open a new E-kernel file and prepare the file for writing.
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
C     NAIF_IDS
C     TIME
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
 
      CHARACTER*(*)         FNAME
      CHARACTER*(*)         IFNAME
      INTEGER               NCOMCH
      INTEGER               HANDLE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     FNAME      I   Name of EK file.
C     IFNAME     I   Internal file name.
C     NCOMCH     I   The number of characters to reserve for comments.
C     HANDLE     O   Handle attached to new EK file.
C
C$ Detailed_Input
C
C     FNAME          is the name of a new E-kernel file to be created.
C
C     IFNAME         is the internal file name of a new E-kernel.  The
C                    internal file name may be up to 60 characters in
C                    length.
C
C     NCOMCH         is the amount of space, measured in characters, to
C                    be allocated in the comment area when the new EK
C                    file is created.  It is not necessary to allocate
C                    space in advance in order to add comments, but
C                    doing so may greatly increase the efficiency with
C                    which comments may be added.  Making room for
C                    comments after data has already been added to the
C                    file involves moving the data, and thus is slower.
C
C                    NCOMCH must be greater than or equal to zero.
C
C$ Detailed_Output
C
C     HANDLE         is the EK handle of the file designated by FNAME.
C                    This handle is used to identify the file to other
C                    EK routines.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If NCOMCH is less than zero, the error SPICE(INVALIDCOUNT)
C         will be signalled.  No file will be created.
C
C     2)  If IFNAME is invalid, the error will be diagnosed by routines
C         called by this routine.
C
C     3)  If the indicated file cannot be opened, the error will be
C         diagnosed by routines called by this routine.  The new file
C         will be deleted.
C
C     4)  If an I/O error occurs while reading or writing the indicated
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
C     1)  Open a new EK file with name 'my.ek' and internal file
C         name 'test ek/1995-JUL-17':
C
C         CALL EKOPN ( 'my.ek',  'test ek/1995-JUL-17',  HANDLE  )
C
C$ Restrictions
C
C     None.
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
C     open new E-kernel
C     open new EK
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
      INTEGER               NWC
      PARAMETER           ( NWC    = 1024 )
 
C
C     Local variables
C
      INTEGER               BASE
      INTEGER               NCR
      INTEGER               P
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKOPN' )
      END IF
 
C
C     Check the comment character count.
C
      IF ( NCOMCH .LT. 0 ) THEN
 
         CALL SETMSG ( 'The number of reserved comment characters ' //
     .                 'must be non-negative but was #.'            )
         CALL ERRINT ( '#',  NCOMCH                                 )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                        )
         CALL CHKOUT ( 'EKOPN'                                      )
         RETURN
 
      END IF
 
C
C     A new DAS file is a must.  The file type is EK.
C     Reserve enough comment records to accommodate the requested
C     number of comment characters.
C
      NCR  =  ( NWC + NCOMCH - 1 ) / NWC
 
      CALL DASONW ( FNAME, 'EK', IFNAME, NCR, HANDLE )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKOPN' )
         RETURN
      END IF
 
C
C     Initialize the file for paged access.  The EK architecture
C     code is automatically set by the paging initialization routine.
C
      CALL ZZEKPGIN ( HANDLE )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKOPN' )
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
      CALL CHKOUT ( 'EKOPN' )
      RETURN
      END
