C$Procedure      SPKW10 (SPK - write a type 10 segment )
 
      SUBROUTINE SPKW10 ( HANDLE, BODY,   CENTER, FRAME,  FIRST, LAST,
     .                    SEGID,  CONSTS, N,      ELEMS,  EPOCHS      )
 
C$ Abstract
C
C     Write an SPK type 10 segment to the DAF open and attached to
C     the input HANDLE.
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
C      None.
C
C$ Keywords
C
C       SPK
C
C$ Declarations
 
      IMPLICIT NONE
      INCLUDE              'sgparam.inc'
      INTEGER               HANDLE
      INTEGER               BODY
      INTEGER               CENTER
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      CHARACTER*(*)         SEGID
      DOUBLE PRECISION      CONSTS ( * )
      INTEGER               N
      DOUBLE PRECISION      ELEMS  ( * )
      DOUBLE PRECISION      EPOCHS ( * )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      HANDLE     I   The handle of a DAF file open for writing.
C      BODY       I   The NAIF ID code for the body of the segment.
C      CENTER     I   The center of motion for BODY.
C      FRAME      I   The reference frame for this segment.
C      FIRST      I   The first epoch for which the segment is valid.
C      LAST       I   The last  epoch for which the segment is valid.
C      SEGID      I   The string to use for segment identifier.
C      CONSTS     I   The array of geophysical constants for the segment
C      N          I   The number of element/epoch pairs to be stored
C      ELEMS      I   The collection of "two-line" element sets.
C      EPOCHS     I   The epochs associated with the element sets.
C
C$ Detailed_Input
C
C     HANDLE      is the file handle of an SPK file that has been
C                 opened for writing by SPCOPN, DAFOPN, or DAFOPW.
C
C     BODY        is the SPICE ID for the body whose states are
C                 to be recorded in an SPK file.
C
C     CENTER      is the SPICE ID for the center of motion associated
C                 with BODY.
C
C     FRAME       is the reference frame that states are referenced to,
C                 for example 'J2000'.
C
C     FIRST       are the bounds on the ephemeris times, expressed as
C     LAST        seconds past J2000, for which the states can be used
C                 to interpolate a state for BODY.
C
C     SEGID       is the segment identifier. An SPK segment identifier
C                 may contain up to 40 characters.
C
C     CONSTS      are the geophysical constants needed for evaluation
C                 of the two line elements sets.  The order of these
C                 constants must be:
C
C                 CONSTS(1) = J2 gravitational harmonic for earth
C                 CONSTS(2) = J3 gravitational harmonic for earth
C                 CONSTS(3) = J4 gravitational harmonic for earth
C                 CONSTS(4) = Square root of the GM for earth where GM
C                             is expressed in earth radii cubed per
C                             minutes squared
C                 CONSTS(5) = Equatorial radius of the earth in km
C                 CONSTS(6) = Low altitude bound for atmospheric
C                             model in km
C                 CONSTS(7) = High altitude bound for atmospheric
C                             model in km
C                 CONSTS(8) = Distance units/earth radius (normally 1)
C
C     N           is the number of "two-line" element sets  and epochs
C                 to be stored in the segment.
C
C     ELEMS       contains a time-ordered array of two-line elements
C                 as supplied in NORAD two-line element files.  The
C                 I'th set of elements should be stored as shown here:
C
C                    BASE = (I-1)*10
C
C                    ELEMS ( BASE + 1 )  = NDT20
C                    ELEMS ( BASE + 2 )  = NDD60
C                    ELEMS ( BASE + 3 )  = BSTAR
C                    ELEMS ( BASE + 4 )  = INCL
C                    ELEMS ( BASE + 5 )  = NODE0
C                    ELEMS ( BASE + 6 )  = ECC
C                    ELEMS ( BASE + 7 )  = OMEGA
C                    ELEMS ( BASE + 8 )  = MO
C                    ELEMS ( BASE + 9 )  = NO
C                    ELEMS ( BASE + 10 ) = EPOCH
C
C                 The meaning of these variables is defined by the
C                 format of the two-line element files available from
C                 NORAD
C
C     EPOCHS      contains the epochs (ephemeris seconds past J2000)
C                 corresponding to the elements in ELEMS.  The I'th
C                 epoch must equal the epoch of the I'th element set
C                 Epochs must form a strictly increasing sequence.
C
C$ Detailed_Output
C
C     None.       The data input is stored in an SPK segment in the
C                 DAF connected to the input HANDLE.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C     This routine writes a type 10 SPK segment to the DAF open
C     for writing that is attached to HANDLE.  A routine, GETELM, that
C     reads two-line element data from files distributed by
C     NORAD is available from NAIF.
C
C$ Examples
C
C     Suppose that you have collected the two-line element data
C     and geophysical constants as prescribed above.  The following
C     code fragment demonstrates how you could go about creating
C     a type 10 SPK segment.
C
C        Open a new SPK file using DAF and get a file handle.
C
C        BODY   = <integer code for the body>
C        CENTER = <integer code for central body for the trajectory>
C        FRAME  = 'J2000'
C        SEGID  = <string that gives the bodies name>
C
C        FNAME  = 'SAMPLE.SPK'
C        ND     =  2
C        NI     =  6
C        IFNAME = 'SAMPLE SPK FILE FOR PRIVATE USE'
C        RESV   =  0
C
C        CALL DAFONW ( FNAME, 'SPK', ND, NI, IFNAME, RESV, HANDLE )
C
C
C        Add the type 10 data.
C
C        CALL SPKW10 ( HANDLE, BODY,   CENTER, FRAME,  FIRST, LAST,
C       .              SEGID,  CONSTS, N,      ELEMS,  EPOCHS      )
C
C        Close the DAF properly.
C
C        CALL DAFCLS ( HANDLE )
C
C$ Restrictions
C
C     None.
C
C$ Exceptions
C
C     1) Errors in the structure or content of the inputs are
C        diagnosed by routines called by this one.
C
C     2) File access errors are diagnosed by routines in the
C        call tree of this routine.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 2006-OCT-30 (BVS)
C
C        Deleted "inertial" from the FRAME description in the Brief_I/O
C        section of the header.
C
C-    SPICELIB Version 1.0.1, 1999-JUN-21 (WLT)
C
C        Cleaned up the header.
C
C-    SPICELIB Version 1.0.0, 1994-JAN-5 (WLT)
C
C-&
 
 
 
C$ Index_Entries
C
C     WRITE A TYPE 10 SPK SEGMENT
C
C-&
 
C
C     Spicelib functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local Variables
C
 
C
C     The type of this segment
C
      INTEGER               SPKTYP
      PARAMETER           ( SPKTYP = 10 )
C
C     The number of geophysical constants:
C
      INTEGER               NCONST
      PARAMETER           ( NCONST = 8 )
C
C     The number of elements per two-line set:
C
      INTEGER               NELEMS
      PARAMETER           ( NELEMS = 10 )
 
      INTEGER               NUOBL
      PARAMETER           ( NUOBL  = 11 )
 
      INTEGER               NULON
      PARAMETER           ( NULON  = 12 )
 
      INTEGER               DNUOBL
      PARAMETER           ( DNUOBL = 13 )
 
      INTEGER               DNULON
      PARAMETER           ( DNULON = 14 )
 
      INTEGER               PKTSIZ
      PARAMETER           ( PKTSIZ = DNULON )
 
 
      DOUBLE PRECISION      DESCR ( 6 )
      DOUBLE PRECISION      PACKET( PKTSIZ )
      DOUBLE PRECISION      DNUT  ( 4 )
 
      INTEGER               BASE
      INTEGER               I
      INTEGER               NPKTS
      INTEGER               NEPOCH
 
 
 
C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'SPKW10' )
C
C     First we need to create a descriptor for the segment
C     we are about to write.
C
      CALL SPKPDS ( BODY,  CENTER, FRAME, SPKTYP, FIRST, LAST,
     .              DESCR  )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKW10' )
         RETURN
      END IF
 
C
C     We've got a valid descriptor, write the data to a DAF
C     segment using the generic segment writer.
C
      NPKTS  = N
      NEPOCH = N
 
 
      CALL SGBWFS ( HANDLE, DESCR,   SEGID,
     .              NCONST, CONSTS,  PKTSIZ, EXPCLS          )
 
      DO I = 1, NEPOCH
C
C        Move the elements into the next packet.
C
         BASE = (I-1)*NELEMS
 
         CALL MOVED ( ELEMS(BASE + 1), 10, PACKET )
C
C        For each epoch, we need to get the nutation in obliquity,
C        nutation in longitude and mean obliquity.
C
         CALL ZZWAHR  ( EPOCHS(I), DNUT )
 
         PACKET(NULON)  = DNUT(1)
         PACKET(NUOBL)  = DNUT(2)
         PACKET(DNULON) = DNUT(3)
         PACKET(DNUOBL) = DNUT(4)
 
C
C        Now write the packet into the generic segment.
C
         CALL SGWFPK ( HANDLE, 1, PACKET, 1, EPOCHS(I) )
 
      END DO
 
      CALL SGWES  ( HANDLE )
 
      CALL CHKOUT ( 'SPKW10' )
 
      RETURN
 
      END
