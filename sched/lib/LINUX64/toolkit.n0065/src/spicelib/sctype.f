C$Procedure      SCTYPE ( SCLK type )
 
      INTEGER FUNCTION SCTYPE ( SC )
 
C$ Abstract
C
C     Return the spacecraft clock type for a specified spacecraft.
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
C     SCLK
C
C$ Keywords
C
C     TIME
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE               'zzctr.inc'

      INTEGER               SC
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SC         I   NAIF spacecraft ID code.
C
C     The function returns the spacecraft clock type associated with the
C     spacecraft specified by SC.
C
C$ Detailed_Input
C
C     SC             is a NAIF ID code for a spacecraft, whose
C                    spacecraft clock `type' is desired.
C
C$ Detailed_Output
C
C     The function returns the spacecraft clock type associated with the
C     spacecraft specified by SC.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the kernel variable that assigns a SCLK type to the
C         spacecraft specified by SC is not found in the kernel pool,
C         the error is diagnosed by routines called by this routine.
C         SCTYPE returns the value 0 in this case.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The raison d'etre of this routine is that it consolidates the code
C     that maps spacecraft ID's to clock types.  While any routine may
C     call SCTYPE, it is unlikely that there will be a need for
C     non-SPICELIB routines to call this routine directly.
C
C$ Examples
C
C     1)  Find the SCLK type for Galileo.
C
C            During program initialization, we load a SCLK kernel file
C            into the kernel pool.  We will pretend that the name of
C            this file is GLLSCLK.KER.  You must use the actual name of
C            an SCLK kernel that is accessible by your program to try
C            this example.
C
C                C
C                C     Load the SCLK kernel.
C                C
C                      CALL FURNSH ( 'GLLSCLK.KER' )
C                                 .
C                                 .
C                                 .
C                C
C                C     Print out the clock type for Galileo.
C                C
C                      TYPE = SCTYPE ( -77 )
C
C                      PRINT *, 'Galileo clock type is ', TYPE
C
C
C     2)  Find the SCLK type for Mars Observer.
C
C
C                C
C                C     Load the SCLK kernel.
C                C
C                      CALL FURNSH ( 'MOSCLK.KER' )
C                                 .
C                                 .
C                                 .
C                C
C                C     Print out the clock type for Mars Observer.
C                C
C                      TYPE = SCTYPE ( -94 )
C
C                      PRINT *, 'Mars Observer clock type is ', TYPE
C
C$ Restrictions
C
C     This routine assumes that an SCLK kernel appropriate to the
C     spacecraft specified by SC has been loaded into the kernel pool.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     J.M. Lynch     (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.3.0, 09-SEP-2013 (BVS)
C
C        Updated to keep track of the POOL counter and call ZZCVPOOL.
C
C-    SPICELIB Version 1.2.0, 05-MAR-2009 (NJB)
C
C        Bug fix: this routine now keeps track of whether its
C        kernel pool look-up succeeded. If not, a kernel pool
C        lookup is attempted on the next call to this routine.
C
C-    SPICELIB Version 1.1.1, 22-AUG-2006 (EDW)
C
C        Replaced references to LDPOOL with references 
C        to FURNSH.
C
C-    SPICELIB Version 1.1.0, 22-MAR-1993 (JML)
C
C        1) The routine now uses the kernel pool watch capability.
C
C        2) The routine now returns a value of zero if RETURN is
C           true on entry.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 04-SEP-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     spacecraft_clock type
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
      CHARACTER*(*)         TYPLBL
      PARAMETER           ( TYPLBL = 'SCLK_DATA_TYPE' )
 
C
C     Local variables
C
      CHARACTER*(60)        KVNAME
 
      INTEGER               N
      INTEGER               TYPE
      INTEGER               OLDSC
      INTEGER               USRCTR ( CTRSIZ ) 
 
      LOGICAL               FIRST
      LOGICAL               NODATA
      LOGICAL               UPDATE
 
C
C     Saved variables
C
      SAVE     TYPE
      SAVE     OLDSC
      SAVE     NODATA
      SAVE     FIRST
      SAVE     USRCTR
 
 
C
C     Initial values
C
      DATA                  FIRST   / .TRUE. /
      DATA                  NODATA  / .TRUE. /
      DATA                  OLDSC   / 0      /
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
 
         SCTYPE = 0
 
         RETURN
 
      END IF
 
      CALL CHKIN ( 'SCTYPE' )
 
 
C
C     On the first pass through the subroutine, or if the spacecraft
C     ID code changes, set a watch on the SCLK kernel variable for
C     the current clock type.
C
      IF (  FIRST  .OR.  ( SC .NE. OLDSC )  ) THEN
C
C        Construct the name of the kernel variable that is needed.
C
         KVNAME = TYPLBL
 
         CALL SUFFIX ( '_#',     0,         KVNAME )
         CALL REPMI  ( KVNAME,  '#',  -SC,  KVNAME )
 
C
C        Set a watch on the kernel variable needed.
C
         CALL SWPOOL ( 'SCTYPE', 1, KVNAME )
C
C        Keep track of the last spacecraft ID encountered.
C
         OLDSC =  SC
 
C
C        Initialize the local POOL counter to user value.
C
         CALL ZZCTRUIN( USRCTR )

         FIRST = .FALSE.
 
      END IF
 
C
C     If the kernel pool variable that this routine uses has
C     been updated, or if the spacecraft id code changes, look
C     up the new value from the kernel pool.
C
      CALL ZZCVPOOL ( 'SCTYPE', USRCTR, UPDATE )
 
      IF ( UPDATE .OR. NODATA ) THEN
C
C        Find the clock type for the specified mission.
C
         TYPE   = 0
 
         CALL SCLI01 ( TYPLBL, SC, 1, N, TYPE )

         IF ( FAILED() ) THEN

            NODATA = .TRUE.
            SCTYPE = 0

            CALL CHKOUT ( 'SCTYPE' )
            RETURN

         END IF

         NODATA = .FALSE.
 
      END IF

      SCTYPE = TYPE
 
      CALL CHKOUT ( 'SCTYPE' )
      RETURN
      END
