C$Procedure      CKGPAV ( C-kernel, get pointing and angular velocity )
 
      SUBROUTINE CKGPAV ( INST, SCLKDP, TOL, REF, CMAT, AV, CLKOUT,
     .                    FOUND )
 
C$ Abstract
C
C     Get pointing (attitude) and angular velocity for a specified
C     spacecraft clock time.
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
C     SCLK
C
C$ Keywords
C
C     POINTING
C
C$ Declarations
 
      IMPLICIT NONE
      INCLUDE               'frmtyp.inc'
      INCLUDE               'zzctr.inc'

      INTEGER               INST
      DOUBLE PRECISION      SCLKDP
      DOUBLE PRECISION      TOL
      CHARACTER*(*)         REF
      DOUBLE PRECISION      CMAT   ( 3, 3 )
      DOUBLE PRECISION      AV     ( 3    )
      DOUBLE PRECISION      CLKOUT
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     INST       I   NAIF ID of instrument, spacecraft, or structure. 
C     SCLKDP     I   Encoded spacecraft clock time.
C     TOL        I   Time tolerance.
C     REF        I   Reference frame.
C     CMAT       O   C-matrix pointing data.
C     AV         O   Angular velocity vector.
C     CLKOUT     O   Output encoded spacecraft clock time.
C     FOUND      O   True when requested pointing is available.
C
C$ Detailed_Input
C
C     INST       is the NAIF integer ID for the instrument, spacecraft,
C                or other structure for which pointing and angular
C                velocity are requested. For brevity we will refer to
C                this object as the "instrument," and the frame fixed
C                to this object as the "instrument frame" or
C                "instrument-fixed" frame.
C
C     SCLKDP     is the encoded spacecraft clock time for which
C                pointing and angular velocity are requested.
C
C                The SPICELIB routines SCENCD and SCE2C respectively
C                convert spacecraft clock strings and ephemeris time to
C                encoded spacecraft clock.  The inverse conversions are
C                performed by SCDECD and SCT2E.
C
C     TOL        is a time tolerance in ticks, the units of encoded
C                spacecraft clock time.  
C
C                The SPICELIB routine SCTIKS converts a spacecraft
C                clock tolerance duration from its character string
C                representation to ticks.  SCFMT performs the inverse
C                conversion.
C
C                The C-matrix - angular velocity vector pair returned by
C                CKGPAV is the one whose time tag is closest to SCLKDP
C                and within TOL units of SCLKDP.  (More in Particulars,
C                below.)
C
C                In general, because using a non-zero tolerance 
C                affects selection of the segment from which the
C                data is obtained, users are strongly discouraged 
C                from using a non-zero tolerance when reading CKs 
C                with continuous data. Using a non-zero tolerance
C                should be reserved exclusively to reading CKs with 
C                discrete data because in practice obtaining data 
C                from such CKs using a zero tolerance is often not 
C                possible due to time round off. 
C
C     REF        is the desired reference frame for the returned
C                pointing and angular velocity.  The returned C-matrix
C                CMAT gives the orientation of the instrument
C                designated by INST relative to the frame designated by
C                REF.  When a vector specified relative to frame REF is
C                left-multiplied by CMAT, the vector is rotated to the
C                frame associated with INST. The returned angular
C                velocity vector AV expresses the angular velocity of
C                the instrument designated by INST relative to the
C                frame designated by REF.  See the discussion of CMAT
C                and AV below for details.
C
C                Consult the SPICE document "Frames" for a discussion 
C                of supported reference frames.
C
C$ Detailed_Output
C
C     CMAT       is a rotation matrix that transforms the components of
C                a vector expressed in the reference frame specified by
C                REF to components expressed in the frame tied to the
C                instrument, spacecraft, or other structure at time
C                CLKOUT (see below).
C
C                Thus, if a vector v has components x,y,z in the REF
C                reference frame, then v has components x',y',z' in the
C                instrument fixed frame at time CLKOUT:
C
C                     [ x' ]     [          ] [ x ]
C                     | y' |  =  |   CMAT   | | y |
C                     [ z' ]     [          ] [ z ]
C
C                If you know x', y', z', use the transpose of the
C                C-matrix to determine x, y, z as follows:
C
C                     [ x ]      [          ]T    [ x' ]
C                     | y |  =   |   CMAT   |     | y' |
C                     [ z ]      [          ]     [ z' ]
C                              (Transpose of CMAT)
C
C     AV         is the angular velocity vector. This is the axis about
C                which the reference frame tied to the instrument is
C                rotating in the right-handed sense at time CLKOUT. The
C                magnitude of AV is the magnitude of the instantaneous
C                velocity of the rotation, in radians per second.  AV
C                is expressed relative to the frame designated by REF.
C
C     CLKOUT     is the encoded spacecraft clock time associated with
C                the returned C-matrix and the returned angular
C                velocity vector. This value may differ from the
C                requested time, but never by more than the input
C                tolerance TOL.
C
C                The particulars section below describes the search
C                algorithm used by CKGPAV to satisfy a pointing
C                request.  This algorithm determines the pointing
C                instance (and therefore the associated time value)
C                that is returned.
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
C     1)  If a C-kernel file has not been loaded using FURNSH prior to
C         a call to this routine, an error is signaled by a routine in
C         the call tree of this routine.
C
C     2)  If TOL is negative, found is set to .FALSE.
C
C     3)  If REF is not a supported reference frame, an error is
C         signaled by a routine in the call tree of this routine and
C         FOUND is set to .FALSE.
C
C$ Files
C
C     CKGPAV searches through files loaded by FURNSH to locate a
C     segment that can satisfy the request for pointing and angular
C     velocity for instrument INST at time SCLKDP.  You must load a
C     C-kernel file using FURNSH prior to calling this routine.
C
C$ Particulars
C
C     How the tolerance argument is used
C     ==================================
C
C
C     Reading a type 1 CK segment (discrete pointing instances)
C     ---------------------------------------------------------
C
C     In the diagram below
C
C        - "0" is used to represent discrete pointing instances
C          (quaternions, angular velocity vectors, and associated
C          time tags).
C
C        - "( )" are used to represent the end points of the time
C          interval covered by a segment in a CK file.
C
C        - SCLKDP is the time at which you requested pointing.
C          The location of SCLKDP relative to the time tags of the
C          pointing instances is indicated by the "+" sign.
C
C        - TOL is the time tolerance specified in the pointing
C          request.  The square brackets "[ ]" represent the
C          endpoints of the time interval
C
C             SCLKDP-TOL : SCLKDP+TOL
C
C        - The quaternions occurring in the segment need not be
C          evenly spaced in time.
C
C
C     Case 1:  pointing is available
C     ------------------------------
C
C                              SCLKDP
C                                   \   TOL
C                                    | /
C                                    |/\
C     Your request                [--+--]
C                                 .  .  .
C     Segment      (0-----0--0--0--0--0--0---0--0------------0--0--0--0)
C                                     ^
C                                     |
C                         CKGPAV returns this instance.
C
C
C     Case 2:  pointing is not available
C     ----------------------------------
C
C                                                   SCLKDP
C                                                      \   TOL
C                                                       | /
C                                                       |/\
C     Your request                                   [--+--]
C                                                    .  .  .
C     Segment      (0-----0--0--0--0--0--0---0--0--0---------0--0--0--0)
C
C
C                         CKGPAV returns no pointing; the output
C                         FOUND flag is set to .FALSE.
C
C
C
C     Reading a type 2, 3, 4, or 5 CK segment (continuous pointing)
C     -------------------------------------------------------------
C
C     In the diagrams below
C
C        - "==" is used to represent periods of continuous pointing.
C
C        - "--" is used to represent gaps in the pointing coverage.
C
C        - "( )" are used to represent the end points of the time
C          interval covered by a segment in a CK file.
C
C        - SCLKDP is the time at which you requested pointing.
C          The location of SCLKDP relative to the time tags of the
C          pointing instances is indicated by the "+" sign.
C
C        - TOL is the time tolerance specified in the pointing
C          request.  The square brackets "[ ]" represent the
C          endpoints of the time interval
C
C             SCLKDP-TOL : SCLKDP+TOL
C
C        - The quaternions occurring in the periods of continuous
C          pointing need not be evenly spaced in time.
C
C
C     Case 1:  pointing is available at the request time
C     --------------------------------------------------
C
C                             SCLKDP
C                                   \   TOL
C                                    | /
C                                    |/\
C     Your request                [--+--]
C                                 .  .  .
C                                 .  .  .
C                                 .  .  .
C     Segment            (==---===========---=======----------===--)
C                                    ^
C                                    |
C
C                   The request time lies within an interval where
C                   continuous pointing is available. CKGPAV returns
C                   pointing at the requested epoch.
C
C
C     Case 2:  pointing is available "near" the request time
C     ------------------------------------------------------
C
C                                    SCLKDP
C                                          \   TOL
C                                           | /
C                                           |/\
C     Your request                       [--+--]
C                                        .  .  .
C     Segment            (==---===========----=======---------===--)
C                                             ^
C                                             |
C
C                   The request time lies in a gap:  an interval where
C                   continuous pointing is *not* available.  CKGPAV
C                   returns pointing for the epoch closest to the
C                   request time SCLKDP.
C
C
C     Case 3:  pointing is not available
C     ----------------------------------
C
C                                                 SCLKDP
C                                                       \   TOL
C                                                        | /
C                                                        |/\
C     Your request                                    [--+--]
C                                                     .  .  .
C     Segment            (==---===========----=======---------===--)
C
C                         CKGPAV returns no pointing; the output
C                         FOUND flag is set to .FALSE.
C
C
C
C     Tolerance and segment priority
C     ==============================
C 
C     CKGPAV searches through loaded C-kernels to satisfy a pointing
C     request. Last-loaded files are searched first. Individual files
C     are searched in backwards order, so that between competing
C     segments (segments containing data for the same object, for
C     overlapping time ranges), the one closest to the end of the file
C     has highest priority. CKGPAV considers only those segments that
C     contain both pointing and angular velocity data, as indicated by
C     the segment descriptor.
C
C     The search ends when a segment is found that can provide pointing
C     and angular velocity for the specified instrument at a time
C     falling within the specified tolerance on either side of the
C     request time. Within that segment, the instance closest to the
C     input time is located and returned.
C
C     The following four cases illustrate this search procedure.
C     Segments A and B are in the same file, with segment A located
C     further towards the end of the file than segment B. Both segments
C     A and B contain discrete pointing data, indicated by the number
C     0.
C
C
C     Case 1:  Pointing is available in the first segment searched.
C              Because segment A has the highest priority and can
C              satisfy the request, segment B is not searched.
C
C
C                                  SCLKDP
C                                        \  TOL
C                                         | /
C                                         |/\
C     Your request                     [--+--]
C                                      .  .  .
C     Segment A          (0-----------------0--------0--0-----0)
C                                           ^
C                                           |
C                                           |
C                               CKGPAV returns this instance
C
C     Segment B     (0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0)
C
C
C
C     Case 2:  Pointing is not available in the first segment searched.
C              Because segment A cannot satisfy the request, segment B
C              is searched.
C
C
C                             SCLKDP
C                                  \   TOL
C                                   | /
C                                   |/\
C     Your request               [--+--]
C                                .  .  .
C     Segment A          (0-----------------0--------0--0-----0)
C                                .  .  .
C     Segment B     (0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0)
C                                   ^
C                                   |
C                       CKGPAV returns this instance
C
C
C     Segments that contain continuous pointing data are searched in
C     the same manner as segments containing discrete pointing data.
C     For request times that fall within the bounds of continuous
C     intervals, CKGPAV will return pointing at the request time. When
C     the request time does not fall within an interval, then a time at
C     an endpoint of an interval may be returned if it is the closest
C     time in the segment to the user request time and is also within
C     the tolerance.
C
C     In the following examples, segment A is located further towards
C     the end of the file than segment C. Segment A contains discrete
C     pointing data and segment C contains continuous data, indicated
C     by the "=" character.
C
C
C     Case 3:  Pointing is not available in the first segment searched.
C              Because segment A cannot satisfy the request, segment C
C              is searched.
C
C                             SCLKDP
C                                   \  TOL
C                                    | /
C                                    |/\
C     Your request                [--+--]
C                                 .  .  .
C                                 .  .  .
C     Segment A          (0-----------------0--------0--0-----0)
C                                 .  .  .
C                                 .  .  .
C     Segment C          (---=============-----====--------==--)
C                                    ^
C                                    |
C                                    |
C                         CKGPAV returns this instance
C
C
C     In the next case, assume that the order of segments A and C in the
C     file is reversed:  A is now closer to the front, so data from
C     segment C are considered first.
C
C
C     Case 4:  Pointing is available in the first segment searched.
C              Because segment C has the highest priority and can
C              satisfy the request, segment A is not searched.
C
C                                             SCLKDP
C                                            /
C                                           |  TOL
C                                           | /
C                                           |/\
C     Your request                       [--+--]
C                                        .  .  .
C                                        .  .  .
C     Segment C          (---=============-----====--------==--)
C                                             ^
C                                             |
C                                CKGPAV returns this instance
C
C     Segment A          (0-----------------0--------0--0-----0)
C                                           ^
C                                           |
C                                     "Best" answer
C
C
C     The next case illustrates an unfortunate side effect of using 
C     a non-zero tolerance when reading multi-segment CKs with
C     continuous data. In all cases when the look-up interval 
C     formed using tolerance overlaps a segment boundary and 
C     the request time falls within the coverage of the lower 
C     priority segment, the data at the end of the higher priority
C     segment will be picked instead of the data from the lower 
C     priority segment.
C
C
C     Case 5:  Pointing is available in the first segment searched.
C              Because segment C has the highest priority and can
C              satisfy the request, segment A is not searched.
C
C                                             SCLKDP
C                                            /
C                                           |  TOL
C                                           | /
C                                           |/\
C     Your request                       [--+--]
C                                        .  .  .
C                                        .  .  .
C     Segment C                                (===============)
C                                              ^
C                                              |
C                                CKGPAV returns this instance
C
C     Segment A          (=====================)
C                                           ^
C                                           |
C                                     "Best" answer
C
C$ Examples
C
C
C     Suppose you have two C-kernel files containing data for the
C     Voyager 2 narrow angle camera.  One file contains predict values,
C     and the other contains corrected pointing for a selected group
C     of images, that is, for a subset of images from the first file.
C
C     The following example program uses CKGPAV to get C-matrices and
C     associated angular velocity vectors for a set of images whose
C     SCLK counts (un-encoded character string versions) are contained
C     in the array SCLKCH.
C
C     If available, the program will get the corrected pointing values.
C     Otherwise, predict values will be used.
C
C     For each C-matrix, a unit  pointing vector is constructed
C     and printed along with the angular velocity vector.
C
C     Note: if the C-kernels of interest do not contain angular
C     velocity data, then the SPICELIB routine CKGP should be used to
C     read the pointing data.  An example program in the header of the
C     SPICELIB routine CKGP demonstrates this.
C
C
C
C     C
C     C     Constants for this program.
C     C
C     C     -- The code for the Voyager 2 spacecraft clock is -32
C     C
C     C     -- The code for the narrow angle camera on the Voyager 2
C     C        spacecraft is -32001.
C     C
C     C    --  Spacecraft clock times for successive Voyager images 
C     C        always differ by more than 0:0:400.  This is an
C     C        acceptable tolerance, and must be converted to "ticks"
C     C        (units of encoded SCLK) for input to CKGPAV.
C     C
C     C     -- The reference frame we want is FK4.
C     C
C     C     -- The narrow angle camera boresight defines the third
C     C        axis of the instrument-fixed coordinate system.
C     C        Therefore, the vector ( 0, 0, 1 ) represents
C     C        the boresight direction in the camera-fixed frame.
C     C
C           IMPLICIT NONE
C
C           INTEGER               FILEN
C           PARAMETER           ( FILEN  = 255 )
C
C           INTEGER               NPICS
C           PARAMETER           ( NPICS  = 2 )
C   
C           INTEGER               TIMLEN
C           PARAMETER           ( TIMLEN = 30 )
C
C           INTEGER               REFLEN
C           PARAMETER           ( REFLEN = 32 )
C
C           CHARACTER*(TIMLEN)    CLKCH
C           CHARACTER*(FILEN)     CKPRED
C           CHARACTER*(FILEN)     CKCORR
C           CHARACTER*(REFLEN)    REF
C           CHARACTER*(FILEN)     SCLK
C           CHARACTER*(TIMLEN)    SCLKCH ( NPICS )
C           CHARACTER*(TIMLEN)    TOLVGR
C           
C           DOUBLE PRECISION      AV     ( 3 )
C           DOUBLE PRECISION      CLKOUT
C           DOUBLE PRECISION      CMAT   ( 3, 3 )
C           DOUBLE PRECISION      SCLKDP
C           DOUBLE PRECISION      TOLTIK
C           DOUBLE PRECISION      VCFIX  ( 3 )
C           DOUBLE PRECISION      VINERT ( 3 )
C
C           INTEGER               SC
C           INTEGER               I
C           INTEGER               INST
C
C           LOGICAL               FOUND
C
C           CKPRED     = 'voyager2_predict.bc'
C           CKCORR     = 'voyager2_corrected.bc'
C           SCLK       = 'voyager2_sclk.tsc'
C           SC         = -32
C           INST       = -32001
C           SCLKCH(1)  = '4/08966:30:768'
C           SCLKCH(2)  = '4/08970:58:768'
C           TOLVGR     = '0:0:400'
C           REF        = 'FK4'
C           VCFIX( 1 ) =  0.D0
C           VCFIX( 2 ) =  0.D0
C           VCFIX( 3 ) =  1.D0
C
C     C
C     C     Loading the files in this order ensures that the
C     C     corrected file will get searched first.
C     C
C           CALL FURNSH ( CKPRED )
C           CALL FURNSH ( CKCORR )
C
C     C
C     C     Need to load a Voyager 2 SCLK kernel to convert from
C     C     clock strings to ticks.
C     C
C           CALL FURNSH ( SCLK )
C
C     C
C     C     Convert tolerance from VGR formatted character string
C     C     SCLK to ticks which are units of encoded SCLK.
C     C
C           CALL SCTIKS ( SC, TOLVGR, TOLTIK )
C
C
C           DO I = 1, NPICS
C     C
C     C        CKGPAV requires encoded spacecraft clock.
C     C
C              CALL SCENCD ( SC, SCLKCH( I ), SCLKDP )
C
C              CALL CKGPAV ( INST,   SCLKDP, TOLTIK, REF, CMAT, AV,
C          .                 CLKOUT, FOUND                        )
C
C              IF ( FOUND ) THEN
C
C     C
C     C           Use the transpose of the C-matrix to transform the
C     C           boresight vector from camera-fixed to reference
C     C           coordinates.
C     C
C                 CALL MTXV   ( CMAT, VCFIX,  VINERT )
C                 CALL SCDECD ( SC,   CLKOUT, CLKCH  )
C
C                 WRITE (*,*) 'VGR 2 SCLK Time:         ', CLKCH
C                 WRITE (*,*) 'VGR 2 NA ISS boresight ' 
C          .      //          'pointing vector: ',         VINERT
C                 WRITE (*,*) 'Angular velocity vector: ', AV
C
C              ELSE
C
C                 WRITE (*,*) 'Pointing not found for time ', SCLKCH(I)
C
C              END IF
C
C           END DO
C
C           END
C
C
C$ Restrictions
C
C     Only loaded C-kernel segments containing both pointing and
C     angular velocity data will be searched by this reader.  Segments
C     containing only pointing data will be skipped over.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     C.H. Acton     (JPL)
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C     J.M. Lynch     (JPL)
C     B.V. Semenov   (JPL)
C     M.J. Spencer   (JPL)
C     R.E. Thurman   (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-    SPICELIB Version 5.3.0, 23-SEP-2013 (BVS)
C
C        Updated to save the input frame name and POOL state counter
C        and to do frame name-ID conversion only if the counter has
C        changed.
C
C-    SPICELIB Version 5.2.1, 03-JUN-2010 (BVS)
C
C        Header update: description of the tolerance and Particulars
C        section were expanded to address some problems arising from
C        using a non-zero tolerance.  
C
C-    SPICELIB Version 5.2.0, 25-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in MTXV, MXM and VADD calls.
C
C-    SPICELIB Version 5.1.2, 29-JAN-2004 (NJB) 
C
C        Header update:  descriptions of input arguments REF and
C        AV were expanded.
C
C-    SPICELIB Version 5.1.1, 27-JUL-2003 (CHA) (NJB) 
C
C        Various header corrections were made.
C
C-    SPICELIB Version 5.1.0, 23-FEB-1999 (WLT)
C
C        The previous editions of this routine did not properly handle
C        the case when TOL was negative.  The routine now returns a
C        value of .FALSE. for FOUND as is advertised above.
C
C-    SPICELIB Version 5.0.0, 28-JUL-1997 (WLT)
C
C        The previous routine incorrectly computed the angular
C        velocity of the transformation from the request frame
C        to the platform frame of the C-matrix for non-inertial
C        reference frames.
C
C-    SPICELIB Version 4.0.0, 19-SEP-1995 (WLT)
C
C        The routine was upgraded so that the reference frame may
C        be non-inertial.
C
C-    SPICELIB Version 3.0.0, 5-OCT-1994 (WLT)
C
C        The previous versions all computed an incorrect
C        value for the angular velocity if the frame specified by
C        REF was different from the reference frame of the segment
C        from which the angular velocity was extracted. This has
C        now been corrected.
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 30-AUG-1991 (JML)
C
C        1) The Particulars section was updated to show how the
C           search algorithm processes segments with continuous
C           pointing data.
C
C        2) It was specified that the angular velocity vector
C           gives the right-handed axis about which the instrument
C           frame rotates.
C
C        3) The example program now loads an SCLK kernel.
C
C        4) FAILED is checked after the call to IRFROT to handle the
C           case where the reference frame is invalid and the error
C           handling is not set to abort.
C
C        5) FAILED is checked in the DO WHILE loop to handle the case
C           where an error is detected by a SPICELIB routine inside the
C           loop and the error handling is not set to abort.
C
C-    SPICELIB Version 1.1.0, 02-NOV-1990 (JML)
C
C        1) The variable NEEDAV is no longer being saved.
C        2) In the example program, the calling sequences
C           for SCENCD and CKGPAV were corrected.
C        3) The restriction that a C-kernel file must be loaded
C           was explicitly stated.
C
C-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU)
C
C-&
 
C$ Index_Entries
C
C     get ck pointing and angular velocity
C
C-&
 
 
 
 
C$ Revisions
C
C-    SPICELIB Version 5.2.0, 25-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in MTXV, MXM and VADD calls.
C
C-    SPICELIB Version 4.1.0, 20-DEC-1995 (WLT)
C
C        A call to FRINFO did not have enough arguments and
C        went undetected until Howard Taylor of ACT.  Many
C        thanks go out to Howard for tracking down this error.
C
C-    SPICELIB Version 4.0.0, 19-SEP-1995 (WLT)
C
C        The routine was upgraded so that the reference frame may
C        be non-inertial.
C
C-    SPICELIB Version 3.0.0, 5-OCT-1994 (WLT)
C
C        The previous versions all computed an incorrect
C        value for the angular velocity if the frame specified by
C        REF was different from the reference frame of the segment
C        from which the angular velocity was extracted. This has
C        now been corrected.
C
C        Previously we were multiplying by the inverse of the
C        rotation that transforms frames.
C
C-    SPICELIB Version 2.0.0, 30-AUG-1991 (JML)
C
C        1) The Particulars section was updated to show how the
C           search algorithm processes segments with continuous
C           pointing data.
C
C        2) It was specified that the angular velocity vector
C           gives the right-handed axis about which the instrument
C           frame rotates.
C
C        3) The example program now loads an SCLK kernel.
C
C        4) FAILED is checked after the call to IRFROT to handle the
C           case where the reference frame is invalid and the error
C           handling is not set to abort.
C
C        5) FAILED is checked in the DO WHILE loop to handle the case
C           where an error is detected by a SPICELIB routine inside the
C           loop and the error handling is not set to abort.
C
C-    SPICELIB Version 1.1.0, 02-NOV-1990 (JML)
C
C        1) The variable NEEDAV is no longer being saved.
C        2) In the example program, the calling sequences
C           for SCENCD and CKGPAV were corrected.
C        3) The restriction that a C-kernel file must be loaded
C           was explicitly stated.
C
C-    Beta Version 1.1.0, 30-AUG-1990 (MJS)
C
C        The following changes were made as a result of the
C        NAIF CK Code and Documentation Review:
C
C        1) The variable SCLK was changed to SCLKDP.
C        2) The variable INSTR was changed to INST.
C        3) The variable IDENT was changed to SEGID.
C        4) The declarations for the parameters NDC, NIC, NC, and
C           IDLEN were moved from the "Declarations" section of the
C           header to the "Local parameters" section of the code below
C           the header. These parameters are not meant to modified by
C           users.
C        5) The header was updated to reflect the changes.
C
C-    Beta Version 1.0.0, 04-JUN-1990 (RET) (IMU)
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
      LOGICAL               FAILED
 
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
C     Saved frame name length.
C
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

 
C
C     Local variables
C
      CHARACTER*(IDLEN)     SEGID
 
      DOUBLE PRECISION      DCD      ( NDC  )
      DOUBLE PRECISION      DESCR    ( NC   )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      OMEGA    ( 3    )
      DOUBLE PRECISION      ROT      ( 3, 3 )
      DOUBLE PRECISION      TMPMAT   ( 3, 3 )
      DOUBLE PRECISION      TMPV     ( 3 )
      DOUBLE PRECISION      XFORM    ( 6, 6 )
 
      INTEGER               CENTER
      INTEGER               HANDLE
      INTEGER               ICD      ( NIC  )
      INTEGER               REFREQ
      INTEGER               REFSEG
      INTEGER               SCLK
      INTEGER               TYPE1
      INTEGER               TYPE2
      INTEGER               TYPEID
 
      LOGICAL               NEEDAV
      LOGICAL               PFND
      LOGICAL               SFND
      LOGICAL               GOTIT

C
C     Saved frame name/ID item declarations.
C
      INTEGER               SVCTR1 ( CTRSIZ )
      CHARACTER*(FRNMLN)    SVREF
      INTEGER               SVREFR

      LOGICAL               FIRST

C
C     Saved frame name/ID items.
C
      SAVE                  SVCTR1
      SAVE                  SVREF
      SAVE                  SVREFR

      SAVE                  FIRST

C
C     Initial values.
C
      DATA                  FIRST   / .TRUE. /


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKGPAV' )
      END IF
 
C
C     Initialization.
C
      IF ( FIRST ) THEN

C
C        Initialize counter.
C
         CALL ZZCTRUIN( SVCTR1 )

         FIRST = .FALSE.

      END IF
 
C
C     Need angular velocity data.
C     Assume the segment won't be found until it really is.
C
      NEEDAV = .TRUE.
      FOUND  = .FALSE.
 
C
C     If the tolerance is less than zero, we go no further.
C
      IF ( TOL .LT. 0.0D0 ) THEN
         CALL CHKOUT ( 'CKGPAV' )
         RETURN
      END IF
 
C
C     Begin a search for this instrument and time, and get the first
C     applicable segment.
C
      CALL CKBSS ( INST,   SCLKDP, TOL,   NEEDAV )
      CALL CKSNS ( HANDLE, DESCR,  SEGID, SFND   )
 
C
C     Keep trying candidate segments until a segment can produce a
C     pointing instance within the specified time tolerance of the
C     input time.
C
C     Check FAILED to prevent an infinite loop if an error is detected
C     by a SPICELIB routine and the error handling is not set to abort.
C
      DO WHILE ( ( SFND ) .AND. ( .NOT. FAILED () ) )
 
         CALL CKPFS ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
     .                CMAT,   AV,    CLKOUT, PFND          )
 
         IF ( PFND ) THEN
C
C           Found one. If the data aren't already referenced to the
C           requested frame, rotate them.
C
            CALL DAFUS  ( DESCR, NDC, NIC, DCD, ICD )
            REFSEG = ICD( 2 )
C
C           Look up the id code for the requested reference frame.
C
            CALL ZZNAMFRM ( SVCTR1, SVREF, SVREFR, REF, REFREQ )
 
            IF ( REFREQ .NE. REFSEG ) THEN
C
C              We may need to convert the output ticks CLKOUT to ET
C              so that we can get the needed state transformation
C              matrix.  This is the case if either of the frames
C              is non-inertial.
C
               CALL FRINFO ( REFREQ, CENTER, TYPE1, TYPEID, GOTIT )
               CALL FRINFO ( REFSEG, CENTER, TYPE2, TYPEID, GOTIT )
 
               IF (       TYPE1 .EQ. INERTL
     .              .AND. TYPE2 .EQ. INERTL ) THEN
C
C                 Any old value of ET will do in this case.  We'll
C                 use zero.
C
                  ET = 0.0D0
 
               ELSE
C
C                 Look up the spacecraft clock id to use to convert
C                 the output CLKOUT to ET.
C
                  CALL CKMETA ( INST, 'SCLK', SCLK )
                  CALL SCT2E  ( SCLK, CLKOUT, ET   )
 
               END IF
C
C              Get the transformation from the requested frame to
C              the segment frame at ET.
C
               CALL FRMCHG ( REFREQ, REFSEG, ET, XFORM )
C
C              If FRMCHG detects that the reference frame is invalid
C              then return from this routine with FOUND equal to false.
C
               IF ( FAILED () ) THEN
                  CALL CHKOUT ( 'CKGPAV' )
                  RETURN
               END IF
C
C              First transform the attitude information. Get the
C              rotation and angular velocity associated with the
C              transformation from request frame to segment frame.
C              Then convert CMAT so that it maps from request frame
C              to C-matrix frame.
C
               CALL XF2RAV ( XFORM,   ROT,  OMEGA  )
               CALL MXM    ( CMAT,    ROT,  TMPMAT )
               CALL MOVED  ( TMPMAT,  9,    CMAT   )
C
C              Now transform the angular velocity information.
C              Currently we have OMEGA (the angular velocity of
C              the transformation from REF frame to the base
C              frame of the C-matrix), and AV the angular velocity
C              of the transformation from the C-MATRIX reference
C              system to the platform of the C-matrix.
C
C              The angular velocity of the C-MATRIX relative to
C              requested frame is given by
C
C                            T
C                 OMEGA + ROT * AV
C
C              Here's why.
C
C              The transformation from the request frame to the frame
C              of the C-kernel looks like this:
C
C                     [                 ]
C                     [   ROT   :   0   ]
C                     [................ ]
C                     [  dROT   :       ]
C                     [  ----   : ROT   ]
C                     [    dt   :       ]
C
C              The transformation from the C-kernel reference frame to
C              the C-kernel platform frame looks like:
C
C
C                     [                 ]
C                     [  CMAT   :   0   ]
C                     [ ............... ]
C                     [  dCMAT  :       ]
C                     [  ----   : CMAT  ]
C                     [    dt   :       ]
C
C
C              The transformation from the request frame to the platform
C              frame is the product shown below
C
C
C               [                 ][                 ]
C               [  CMAT   :   0   ][   ROT   :   0   ]
C               [ ............... ][................ ]
C               [  dCMAT  :       ][  dROT   :       ]
C               [  ----   : CMAT  ][  ----   : ROT   ]
C               [    dt   :       ][    dt   :       ]
C
C
C                     [                            :             ]
C                     [  CMAT * ROT                :   0         ]
C              =      [ ........................................ ]
C                     [  dCMAT                dROT :             ]
C                     [  ----  * ROT + CMAT * ---- : CMAT * ROT  ]
C                     [    dt                  dt  :             ]
C
C
C              In general, the angular velocity matrix of a
C              transformation R is given by
C
C                   T
C                 dR
C                 --  * R
C                 dt
C
C              Substituting the appropriate components of the matrix
C              in for R we have:
C
C                                  T        T
C              OMEGA         =  ROT  * dCMAT  * CMAT * ROT
C                   CMAT*ROT           -----
C                                        dt
C
C                                        T
C                                    dROT       T
C                               +    ---- * CMAT  * CMAT * ROT
C                                     dt
C
C
C                                  T
C                            =  ROT  * OMEGA      * ROT   + OMEGA
C                                            CMAT                ROT
C
C
C              Consider the first term of the final expression. If we
C              let "x" stand for the cross product operation, then by
C              definition for any vector V:
C
C
C                  T
C               ROT  * OMEGA     * ROT  * V
C                           CMAT
C
C
C                             T
C                        = ROT  * (AV     x  ROT*V )
C                                    CMAT
C
C                          (since rotations distribute across cross
C                           products)
C
C                              T                  T
C                        = (ROT * AV    ) x  ( ROT * ROT*V )
C                                   CMAT
C
C
C                              T
C                        = (ROT * AV    ) x  V
C                                   CMAT
C
C              Thus OMEGA         is the matrix form of the cross
C                        CMAT*ROT
C
C                                      T
C              product operation {( ROT *AV    )   +  AV   } x  .
C                                          CMAT         ROT
C
C
               CALL MTXV ( ROT,    AV,    TMPV )               
               CALL VADD ( OMEGA,  TMPV,  AV   )
 
            END IF
 
            FOUND = .TRUE.
 
            CALL CHKOUT ( 'CKGPAV' )
            RETURN
 
         END IF
 
         CALL CKSNS ( HANDLE, DESCR, SEGID, SFND )
 
      END DO
 
 
      CALL CHKOUT ( 'CKGPAV' )
      RETURN
      END
