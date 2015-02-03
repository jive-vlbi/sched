C$Procedure    ZZSCLK ( Is there and SCLK for a CKID )
 
      LOGICAL FUNCTION ZZSCLK ( CKID, SCLKID )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Determine whether or not an SCLK kernel is available for mapping
C     ET to Ticks and back again for a particular C-kernel ID.
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
C     None.
C
C$ Keywords
C
C     UTITILITY
C
C$ Declarations
 
      IMPLICIT NONE
      INTEGER               CKID
      INTEGER               SCLKID
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     CKID       I   CK ID-code for the object of interest.
C     SCLKID     I   Idcode to pass to the SCLOCK routines for CKID
C
C     The function returns TRUE is SCLK information is available.
C
C$ Detailed_Input
C
C     CKID       is the C-kernel ID-code for some object for which
C                and SCLK is required.
C
C     SCLKID     is the ID-code to pass to SCE2C to convert ET times
C                to ticks.
C
C$ Detailed_Output
C
C     The function returns TRUE if an SCLK specification is present
C     in the kernel pool that is suitable for mapping ticks to ET and
C     back for the C-kernel object specified by CKID.  If such
C     information is not available, or is deemed to be corrupt or
C     incomplete, the function returns FALSE.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Particulars
C
C     This is a utility for checking that sufficient SCLK information
C     is available for mapping between ET and SCLK for the object
C     specified by CKID
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 15-AUG-2000 (WLT)
C
C        Removed the check fo the SCLK Time system as it is not 
C        formally required for an SCLK specification to be complete.
C
C-    SPICELIB Version 1.0.0, 17-FEB-2000 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Determine whether a file specifies and SCLK
C
C-&
C     SPICELIB Functions
C
      INTEGER               CARDI
      INTEGER               SIZEI
 
      LOGICAL               RETURN
      LOGICAL               ELEMI
 
C
C     Local Variables
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               MAXIDS
      PARAMETER           ( MAXIDS = 10 )
 
 
      CHARACTER*(WDSIZE)    SCLK
      CHARACTER*(WDSIZE)    SCLKVR ( 7 )
      CHARACTER*(WDSIZE)    TYPE
      CHARACTER*(WDSIZE)    AGENT
 
      INTEGER               DTSIZE ( 7 )
      INTEGER               I
      INTEGER               N
 
      INTEGER               KNOWN  ( LBCELL : MAXIDS )
      INTEGER               PASSED ( LBCELL : MAXIDS )
 
      LOGICAL               KEEPID
      LOGICAL               UPDATE
      LOGICAL               WATCH
      LOGICAL               FIRST
 
      LOGICAL               FOUND
 
      SAVE                  KNOWN
      SAVE                  PASSED
      SAVE                  FIRST
      SAVE                  DTSIZE
 
      DATA                  FIRST / .TRUE. /
 
 
      ZZSCLK = .FALSE.
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'ZZSCLK')
 
      IF ( FIRST ) THEN
         FIRST = .FALSE.
 
         DTSIZE(1) = 1
         DTSIZE(2) = 1
         DTSIZE(3) = 1
         DTSIZE(4) = 1
         DTSIZE(5) = 3
         DTSIZE(6) = 1
         DTSIZE(7) = 1
 
         CALL SSIZEI ( MAXIDS, KNOWN )
         CALL SSIZEI ( MAXIDS, PASSED )
      END IF
C
C     We've got a text kernel (or meta kernel). See if there is an
C     SCLK kernel loaded for the CKID provided in the calling inputs.
C     If not, we'll use the default -CKID/1000 for the SCLK ID.
C
      CALL INTSTR ( -SCLKID, SCLK )
      AGENT = 'ZZSCLK' // SCLK
 
C
C     See if this is an ID-code we've encountered before.  If it
C     is we can make use of stored knowledge about this ID-code.
C
      IF ( ELEMI( SCLKID, KNOWN ) ) THEN
 
         WATCH  = .FALSE.
         KEEPID = .TRUE.
         CALL CVPOOL ( AGENT, UPDATE )
 
      ELSE IF ( CARDI(KNOWN) .LT. SIZEI(KNOWN) ) THEN
C
C        The SCLKID specified is not in the list of SCLKIDs for
C        this routine and there is room left in the pool of
C        SCLKIDs to keep track of one more.  Put this ID into
C        the list of known IDS
C
         CALL INSRTI( SCLKID, KNOWN )
         UPDATE = .TRUE.
         WATCH  = .TRUE.
         KEEPID = .TRUE.
 
      ELSE
 
         UPDATE = .TRUE.
         KEEPID = .FALSE.
         WATCH  = .FALSE.
 
      END IF
 
      IF ( .NOT. UPDATE ) THEN
C
C        Nothing has changed in the kernel pool w.r.t this agent.
C        The test for an SCLK will not have changed either.
C
         ZZSCLK = ELEMI( SCLKID, PASSED )
         CALL CHKOUT ( 'ZZSCLK' )
         RETURN
 
      END IF
C
C     If we are still here, we need to look in the kernel pool
C     to see if we have an SCLK for this object.
C
C     Construct all of the expected SCLK variables are
C     available for this SCLK.
C
      SCLKVR(1) = 'SCLK_DATA_TYPE_'       // SCLK
      SCLKVR(2) = 'SCLK01_N_FIELDS_'      // SCLK
      SCLKVR(3) = 'SCLK01_MODULI_'        // SCLK
      SCLKVR(4) = 'SCLK01_OFFSETS_'       // SCLK
      SCLKVR(5) = 'SCLK01_COEFFICIENTS_'  // SCLK
      SCLKVR(6) = 'SCLK_PARTITION_START_' // SCLK
      SCLKVR(7) = 'SCLK_PARTITION_END_'   // SCLK
C
C     If we are supposed to watch for this agent, we add him to
C     the list of kernel pool agents.
C
      IF ( WATCH ) THEN
         CALL SWPOOL ( AGENT, 7, SCLKVR )
         CALL CVPOOL ( AGENT,    UPDATE )
      END IF
C
C     Check for all of the required variables and structure in
C     the kernel pool.
C
      DO I = 1, 7
 
         CALL DTPOOL ( SCLKVR(I), FOUND, N, TYPE )
 
         IF (       .NOT. FOUND
     .        .OR.  TYPE .NE. 'N'
     .        .OR. (N/DTSIZE(I))*DTSIZE(I) .NE. N   )THEN
C
C           We don't have adequate SCLK data for the specified
C           object.  Remove this AGENT from the list of agents
C           that have passed the test.
C
            CALL REMOVI ( SCLKID, PASSED )
            CALL CHKOUT ( 'ZZSCLK' )
            RETURN
         END IF
 
      END DO
C
C     Once we get to this point, we know we have SCLK data.  If
C     there is room to WATCH for this agent,
C
      IF ( KEEPID ) THEN
         CALL INSRTI ( SCLKID, PASSED )
      END IF
 
C
C     As far as we can tell, everything looks ok.
C
      ZZSCLK = .TRUE.
      CALL CHKOUT ( 'ZZSCLK' )
      RETURN
      END
