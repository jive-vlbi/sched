C$Procedure      CKFROT ( C-kernel, find rotation )
 
      SUBROUTINE CKFROT ( INST, ET, ROTATE, REF, FOUND )
 
C$ Abstract
C
C     Find the rotation from a C-kernel Id to the native
C     frame at the time requested.
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
C     CK
C
C$ Keywords
C
C     POINTING
C
C$ Declarations
 
      IMPLICIT NONE
      INTEGER               INST
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ROTATE  ( 3, 3 )
      INTEGER               REF
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     INST       I   NAIF instrument ID.
C     ET         I   Epoch measured in seconds past J2000.
C     ROTATE     O   rotation from CK platform to frame REF.
C     REF        O   Reference frame.
C     FOUND      O   True when requested pointing is available.
C
C$ Detailed_Input
C
C     INST       is the unique NAIF integer ID for the spacecraft
C                instrument for which data is being requested.
C
C     ET         is the epoch for which the state rotation
C                is desired. ET should be given in seconds past the
C                epoch of J2000.
C
C
C$ Detailed_Output
C
C     ROTATE     is a rotation matrix that converts
C                positions relative to the input frame (given by INST)
C                to positions relative to the frame REF.
C
C                Thus, if a state S has components x,y,z,dx,dy,dz
C                in the frame of INST, frame, then S has components
C                x', y', z', dx', dy', dz' in frame REF.
C
C                     [  x' ]     [           ] [  x ]
C                     |  y' |  =  |   ROTATE  | |  y |
C                     [  z' ]     [           ] [  z ]
C
C
C     REF        is the id-code reference frame to which ROTATE will
C                transform states.
C
C     FOUND      is true if a record was found to satisfy the pointing
C                request.  FOUND will be false otherwise.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If a C-kernel file is not loaded using CKLPF prior to calling
C         this routine, an error is signalled by a routine that this
C         routine calls.
C
C
C$ Files
C
C     CKFROT searches through files loaded by CKLPF to locate a segment
C     that can satisfy the request for position rotation
C     for instrument INST at time ET.  You must load a C-kernel
C     file using CKLPF before calling this routine.
C
C$ Particulars
C
C     CKFROT searches through files loaded by CKLPF to satisfy a
C     pointing request. Last-loaded files are searched first, and
C     individual files are searched in backwards order, giving
C     priority to segments that were added to a file later than the
C     others. CKFROT considers only those segments that contain
C     angular velocity data.
C
C     The search ends when a segment is found that can give pointing
C     for the specified instrument at the request time.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     A C-kernel file should have been loaded by CKLPF.
C
C     In addition it is helpful to load a CK-info file into the
C     Kernel pool.  This file should have the following variables
C     defined.
C
C       CK_<INST>_SCLK = SCLK idcode that yields SCLK mapping for INST.
C       CK_<INST>_SPK  = SPK idcode  that yields ephemeris for INST.
C
C     where <INST> is the integer string corresponding to INST.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 17-FEB-2000 (WLT)
C
C        The routine now checks to make sure convert ET to TICKS
C        and that at least one C-kernel is loaded before trying
C        to look up the transformation.  Also the routine now calls
C        SCE2C instead of SCE2T.
C
C-    SPICELIB Version 1.0.0, 03-MAR-1999 (WLT)
C
C-&
 
C$ Index_Entries
C
C     get instrument frame rotation and reference frame
C
C-&
 
 
 
 
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               FAILED
      LOGICAL               ZZSCLK
 
C
C     Local parameters
C
C        NDC        is the number of double precision components in an
C                   unpacked C-kernel segment descriptor.
C
C        NIC        is the number of integer components in an unpacked
C                   C-kernel segment descriptor.
C
C        NC         is the number of components in a packed C-kernel
C                   descriptor.  All DAF summaries have this formulaic
C                   relationship between the number of its integer and
C                   double precision components and the number of packed
C                   components.
C
C        IDLEN      is the length of the C-kernel segment identifier.
C                   All DAF names have this formulaic relationship
C                   between the number of summary components and
C                   the length of the name (You will notice that
C                   a name and a summary have the same length in bytes.)
C
 
      INTEGER               NDC
      PARAMETER           ( NDC = 2 )
 
      INTEGER               NIC
      PARAMETER           ( NIC = 6 )
 
      INTEGER               NC
      PARAMETER           ( NC = NDC + ( NIC + 1 )/2 )
 
      INTEGER               IDLEN
      PARAMETER           ( IDLEN = NC * 8 )
 
C
C     Local variables
C
      INTEGER               HANDLE
      INTEGER               ICD      ( NIC  )
      INTEGER               SCLKID
 
      DOUBLE PRECISION      AV       ( 3    )
      DOUBLE PRECISION      CLKOUT
      DOUBLE PRECISION      DESCR    ( NC   )
      DOUBLE PRECISION      DCD      ( NDC  )
      DOUBLE PRECISION      ROT      ( 3, 3 )
      DOUBLE PRECISION      TIME
      DOUBLE PRECISION      TOL
 
      CHARACTER*(IDLEN)     SEGID
 
      LOGICAL               NEEDAV
      LOGICAL               SFND
      LOGICAL               PFND
      LOGICAL               HAVE
C
C     Set FOUND to FALSE right now in case we end up
C     returning before doing any work.
C
      FOUND = .FALSE.
      REF   =  0
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKFROT' )
      END IF
 
C
C     We don't need angular velocity data.
C     Assume the segment won't be found until it really is.
C
      NEEDAV = .FALSE.
      TOL    =  0.0D0
 
C
C     Begin a search for this instrument and time, and get the first
C     applicable segment.
C
      CALL CKHAVE ( HAVE )
      CALL CKMETA ( INST,  'SCLK', SCLKID )
 
      IF ( .NOT. HAVE ) THEN
         CALL CHKOUT ( 'CKFROT' )
         RETURN
      ELSE IF ( .NOT. ZZSCLK ( INST, SCLKID ) ) THEN
         CALL CHKOUT ( 'CKFROT' )
         RETURN
      END IF
 
      CALL SCE2C  ( SCLKID, ET,            TIME   )
      CALL CKBSS  ( INST,   TIME,   TOL,   NEEDAV )
      CALL CKSNS  ( HANDLE, DESCR,  SEGID, SFND   )
 
C
C     Keep trying candidate segments until a segment can produce a
C     pointing instance within the specified time tolerance of the
C     input time.
C
C     Check FAILED to prevent an infinite loop if an error is detected
C     by a SPICELIB routine and the error handling is not set to abort.
C
      DO WHILE ( ( SFND ) .AND. ( .NOT. FAILED () ) )
 
         CALL CKPFS ( HANDLE, DESCR, TIME,   TOL, NEEDAV,
     .                ROT,    AV,    CLKOUT, PFND          )
 
         IF ( PFND ) THEN
C
C           Found one. Fetch the ID code of the reference frame
C           from the descriptor.
C
            CALL DAFUS  ( DESCR, NDC, NIC, DCD, ICD )
            REF   =  ICD( 2 )
            FOUND = .TRUE.
C
C           We now have the rotation matrix from
C           REF to INS. We invert ROT to get the rotation
C           from INST to REF.
C
            CALL XPOSE ( ROT, ROTATE )
 
            CALL CHKOUT ( 'CKFROT' )
            RETURN
 
         END IF
 
         CALL CKSNS ( HANDLE, DESCR, SEGID, SFND )
 
      END DO
 
 
      CALL CHKOUT ( 'CKFROT' )
      RETURN
      END
