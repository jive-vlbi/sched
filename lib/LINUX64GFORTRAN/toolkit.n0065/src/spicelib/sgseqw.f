C$Procedure  SGSEQW ( Generic segements: Sequential writer. )
 
      SUBROUTINE SGSEQW ( HANDLE, DESCR,  SEGID,  NCONST, CONST,
     .                    NPKTS,  PKTSIZ, PKTDAT, NREFS, REFDAT,
     .                    IDXTYP                                 )
      IMPLICIT NONE
 
C$ Abstract
C
C     This is the umbrella routine for managing the sequential writing
C     of generic segments to DAF files. It should never be called
C     directly, it provides the mechanism whereby data are shared by
C     its entry points.
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
C     DAF Required Reading.
C
C$ Keywords
C
C     GENERIC SEGMENTS
C
C$ Declarations
 
      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR  ( * )
      CHARACTER*(*)         SEGID
      INTEGER               NCONST
      DOUBLE PRECISION      CONST  ( * )
      INTEGER               NPKTS
      INTEGER               PKTSIZ ( * )
      DOUBLE PRECISION      PKTDAT ( * )
      INTEGER               NREFS
      DOUBLE PRECISION      REFDAT ( * )
      INTEGER               IDXTYP
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C      HANDLE    I    Handle of a DAF file opened with write access.
C      DESCR     I    Descriptor for a generic segment.
C      SEGID     I    Identifier for a generic segment.
C      NCONST    I    Number of constant values in a generic segment.
C      CONST     I    Array of constant values for a generic segment.
C      NPKTS     I    Number of data packets to write to a segment.
C      PKTSIZ    I    Size of fixed size packets or sizes of variable
C                     size packets.
C      PKTDAT    I    Array of packet data.
C      NREFS     I    Number of reference values.
C      REFDAT    I    Reference data.
C      IDXTYP    I    Index type for the reference values.
C
C$ Detailed_Input
C
C      HANDLE   Handle of a DAF file opened with write access. This is
C               the handle of the file in which a generic segment will
C               be started, or the handle of a file in which a generic
C               segment is currently being written.
C
C      DESCR    Descriptor for the generic segment that is being
C               written. This is the packed form of the DAF double
C               precision and integer summaries which contains ND double
C               precision numbers and NI integers, respectively.
C
C      SEGID    Identifier for the generic segment that is being
C               written. This is a character string containing at most
C               NC printing ASCII characters where
C
C                                 /  ND + ( NI + 1 )  \
C                      NC =  8 *  | ----------------- |
C                                 \         2         /
C
C                SEGID may be blank.
C
C      NCONST   The number of constant values to be placed in the
C               generic segment.
C
C      CONST    An array of NCONST constant values for the generic
C               segment.
C
C      NPKTS    Number of data packets to write to a generic segment.
C
C      PKTSIZ   Size of fixed size packets or sizes of variable size
C               packets.
C
C               The size of a packet is the number of double precision
C               numbers it contains.
C
C               When writing a segment with fixed size packets, only
C               the first element of the array, PKTSIZ(1), is used, and
C               it should contain the size of the fixed size packets. In
C               this instance, the calling program need not declare this
C               variable as an array of one integer; it may be declared
C               as an integer variable.
C
C               When writing a segment with variable size packets,
C               there must be an element in the array PKTSIZ for each of
C               the data packets.
C
C      PKTDAT   A singly dimensioned array containing the double
C               precision data for the fixed or variable size data
C               packets to be added to the generic segment associated
C               with HANDLE.
C
C               For fixed size data packets, PKTDAT will have the
C               following structure:
C
C               Packet #  Range of locations for the packet data.
C               --------  ---------------------------------------------
C
C                     1   PKTDAT(1)              to PKTDAT(PS)
C                     2   PKTDAT(PS+1)           to PKTDAT(2*PS)
C                     3   PKTDAT(2*PS+1)         to PKTDAT(3*PS)
C                     4   PKTDAT(3*PS+1)         to PKTDAT(4*PS)
C
C                                          .
C                                          .
C                                          .
C
C                 NPKTS   PKTDAT((NPKTS-1)*PS+1) to PKTDAT(NPKTS*PS)
C
C               where PS = PKTSIZ(1).
C
C               For variable size data packets, PKTDAT will have the
C               following structure:
C
C               Packet #  Range of locations for the packet data.
C               --------  ---------------------------------------------
C
C                     1   PKTDAT(1)           to PKTDAT(P(1))
C                     2   PKTDAT(P(1)+1)      to PKTDAT(P(2))
C                     3   PKTDAT(P(2)+1)      to PKTDAT(P(3))
C                     4   PKTDAT(P(3)+1)      to PKTDAT(P(4))
C
C                                          .
C                                          .
C                                          .
C
C                 NPKTS   PKTDAT(P(NPKTS-1)+1) to PKTDAT(P(NPKTS))
C
C                                I
C                               ---
C               where P(I) =    >   PKTSIZ(K).
C                               ---
C                              K = 1
C
C      NREFS    Number of reference values.
C
C               For implicitly indexed packets, NREFS must have a value
C               of two (2).
C
C               When writing packets to a generic segment which uses an
C               implicit index type, the value specified by NREFS is
C               used only on the first call to SGWFPK or SGWVPK. On all
C               subsequent calls to these subroutines for a particular
C               implicitly indexed generic segment, the value of NREFS
C               is ignored.
C
C               For explicitly indexed packets, NREFS must be equal to
C               NPKTS; there should be a reference value for each data
C               packet being written to the generic segment.
C
C               When writing packets to a segment which uses an explicit
C               index type, the value specified by NREFS is used on
C               every call to SGWFPK or SGWVPK and it must always be
C               equal to NPKTS.
C
C      REFDAT   Reference data values.
C
C               For implicitly indexed packets, there must be two (2)
C               values. The values represent a starting value, which
C               will have an index of 1, and a step size between
C               reference values, which are used to compute an index and
C               a reference value associated with a specified key value.
C
C               In order to avoid, or at least minimize, numerical
C               difficulties associated with computing index values for
C               generic segments with implicit index types, the value of
C               the stepsize must be an integer, i.e., DINT(REFDAT(2))
C               must equal REFDAT(2). In this case, we also recommend
C               that REFDAT(1) be an integer, although this is not
C               enforced.
C
C               When writing packets to a generic segment which uses an
C               implicit index type, the values specified by REFDAT are
C               used only on the first call to SGWFPK or SGWVPK. On all
C               subsequent calls to these subroutines for a particular
C               implicitly indexed generic segment REFDAT is ignored.
C
C               For explicitly indexed packets, there must be NPKTS
C               reference values and the values must be in increasing
C               order:
C
C                  REFDAT(I) < REFDAT(I+1), I = 1, NPKTS-1
C
C               When writing packets to a segment which uses an explicit
C               index type, the values specified by REFDAT are used on
C               every call to SGWFPK or SGWVPK. On all calls to these
C               subroutines after the first, the value of REFDAT(1) must
C               be strictly greater than than the value of REFDAT(NPKTS)
C               from the previous call. This preserves the ordering of
C               the reference values for the entire segment.
C
C      IDXTYP   Index type to use for the reference values.
C
C               Two forms of indexing are provided:
C
C                  1) An implicit form of indexing based on using two
C                     values, a starting value, which will have an index
C                     of 1, and a step size between reference values,
C                     which are used to compute an index and a reference
C                     value associated with a specified key value. See
C                     the descriptions of the implicit types below for
C                     the particular formula used in each case.
C
C                  2) An explicit form of indexing based on a reference
C                     value for each data packet.
C
C               See the chapter on Generic segments in the DAF required
C               or the include file 'sgparam.inc' for more details
C               about the index types that are available.
C
C$ Detailed_Output
C
C     None.
C
C     The data passed to the various entry points of this subroutine are
C     used to construct a generic segment in one or more DAF files, with
C     the current file specified by the input argument HANDLE.
C
C$ Parameters
C
C     The entry points in this subroutine make use of parameters defined
C     in the file 'sgparam.inc'.
C
C$ Exceptions
C
C     1) If this subroutine is called directly rather than through one
C        of its entry points, the error SPICE(BOGUSENTRY) will be
C        signalled.
C
C     See the individual entry points for descriptions of their
C     exceptions.
C
C$ Files
C
C     See HANDLE in the $ Detailed_Input section above.
C
C$ Particulars
C
C     This is the umbrella routine for managing the sequential writing
C     of generic segments to DAF files. It should never be called
C     directly, but provides the mechanism whereby data are shared by
C     its entry points. The entry points included in this subroutine
C     are:
C
C     SGBWFS ( HANDLE, DESCR, SEGID, NCONST, CONST, PKTSIZ, IDXTYP )
C        Begin writing a generic segment with fixed size packets.
C
C     SGBWVS ( HANDLE, DESCR, SEGID, NCONST, CONST, IDXTYP )
C        Begin writing a generic segment with variable size packets.
C
C     SGWFPK ( HANDLE, NPKTS, PKTDAT, NREFS, REFDAT )
C        Write fixed size packets to a generic segment started by
C        calling SGBWFS.
C
C     SGWVPK ( HANDLE, NPKTS, PKTSIZ, PKTDAT, NREFS, REFDAT )
C        Write variable size packets to a generic segment started by
C        calling SGBWVS.
C
C     SGWES ( HANDLE )
C        End a generic segment.
C
C     A DAF generic segment contains several logical data partitions:
C
C        1) A partition for constant values to be associated with each
C           data packet in the segment.
C
C        2) A partition for the data packets.
C
C        3) A partition for reference values.
C
C        4) A partition for a packet directory, if the segment contains
C           variable sized packets.
C
C        5) A partition for a reference value directory.
C
C        6) A reserved partition that is not currently used. This
C           partition is only for the use of the NAIF group at the Jet
C           Propulsion Laboratory (JPL).
C
C        7) A partition for the meta data which describes the locations
C           and sizes of other partitions as well as providing some
C           additional descriptive information about the generic
C           segment.
C
C                 +============================+
C                 |         Constants          |
C                 +============================+
C                 |          Packet 1          |
C                 |----------------------------|
C                 |          Packet 2          |
C                 |----------------------------|
C                 |              .             |
C                 |              .             |
C                 |              .             |
C                 |----------------------------|
C                 |          Packet N          |
C                 +============================+
C                 |      Reference Values      |
C                 +============================+
C                 |      Packet Directory      |
C                 +============================+
C                 |    Reference  Directory    |
C                 +============================+
C                 |       Reserved  Area       |
C                 +============================+
C                 |     Segment Meta Data      |
C                 +----------------------------+
C
C     Only the placement of the meta data at the end of a generic
C     segment is required. The other data partitions may occur in any
C     order in the generic segment because the meta data will contain
C     pointers to their appropriate locations within the generic
C     segment.
C
C     The meta data for a generic segment should only be obtained
C     through use of the subroutine SGMETA. The meta data should not be
C     written through any mechanism other than the ending of a generic
C     segment begun by SGBWFS or SGBWVS using SGWES.
C
C     The entry points of this subroutine when used together provide the
C     following capabilities:
C
C        1) The ability to write a generic segment with fixed size data
C           packets to a DAF.
C
C        2) the ability to write a generic segment with variable size
C           data packets to a DAF.
C
C        3) The ability to write generic segments to multiple files.
C           Only a single generic segment may be written to a particular
C           file at any time, but several files may each have a generic
C           segment being written to them at the same time.
C
C     Packets may be placed into a generic segment one at a time or N at
C     at time, depending upon the whim of the programmer, limitations
C     of the computing equipment (memory), or requirements placed upon
C     the software that will write a generic segment.
C
C     Packets are retrieved from a generic segment by an index which may
C     be obtained by using the subroutine SGFRVI (generic segments fetch
C     reference value and index).
C
C$ Examples
C
C     In examples 1 and 3, we make use of the fictitious subroutines
C
C        GET_FIX_PKT ( PACKET, REF, DONE )
C
C     and
C
C        GET_VAR_PKT ( PACKET, SIZE, REF, DONE )
C
C     where
C
C        DONE   is a logical flag indicating whether there is more data
C               available. DONE = .TRUE. implies there is no more data.
C               DONE = .FALSE. implies there is more data available.
C
C        PACKET is a double precision array of an appropriate size to
C               hold all of the data returned.
C
C        REF    is a double precision reference value that will be used
C               to create an index for the data packets in the segment.
C               The values of this variable are always increasing, e.g.,
C               the value of REF on the second call to GET_FIX_PKT or
C               GET_VAR_PKT will be greater than the value on the first
C               call to the subroutine.
C
C        SIZE   is an integer for the size of the variable size data
C               packet that is returned.
C
C     These subroutines return a fixed size data packet and a variable
C     size data packet, respectively. We make use of these fictitious
C     subroutines in the examples to avoid adding unnecessary or
C     distracting complications.
C
C     You may think of these subroutines as methods for acquiring data
C     from a "black-box" process. In the first case, the data is always
C     returned in fixed size blocks from a black-box that fills a local
C     buffer with data and always returned the entire buffer when data
C     is requested, e.g., an instrument that measures the concentrations
C     of carbon dioxide, sulfer dioxide, ozone, and other constituents
C     of the air. In the second case, the data is returned in variably
C     sized blocks from a black-box, e.g., an algorithm which integrates
C     a function using polynomials of varying degree; different numbers
C     of coefficients are required for polynomials of differing degrees.
C
C     In examples 2 and 4, we make use of the fictitious subroutines
C
C        GET_FIX_PKTS ( NPKTS, PKTS, REFS, DONE )
C
C     and
C
C        GET_VAR_PKTS ( NPKTS, PKTS, SIZES, REFS, DONE )
C
C     where
C
C        DONE   is a logical flag indicating whether there is more data
C               available. DONE = .TRUE. implies there is no more data.
C               DONE = .FALSE. implies there is more data available;
C
C        NPKTS  is the number of data packets returned in the array
C               PKTS.
C
C        PKTS   is a double precision array containing NPKTS data
C               packets, either fixed size or variable size, and is of
C               an appropriate size to hold all of the data returned.
C               See the description of PKTDAT above for the exact manner
C               in which fixed size packets and variable size packets
C               are stored in an array.
C
C        REFS   is a double precision array which contains NPKTS
C               reference values that will be used to create an index
C               for the data packets in the segment. The values of this
C               variable are always increasing, e.g., the first value of
C               REFS on the second call to GET_FIX_PKTS or GET_VAR_PKTS
C               will be greater than the last value of REFS on the first
C               call to the subroutine.
C
C        SIZES  is an array of integers containing the sizes of each of
C               the variable size data packets that is returned in PKTS.
C
C     These subroutines return arrays containing one or more fixed size
C     data packets and one or more variable size data packets,
C     respectively. We make use of these fictitious subroutines in the
C     examples to avoid adding unnecessary or distracting complications.
C
C     For each example, we provide a simple code fragment that
C     demonstrates the use of the entry points to create generic
C     segments. We assume that all of the relevant variables are defined
C     at the time that the entry points are invoked. These code
C     fragments are for illustrative purposes; they do not necessarily
C     conform to what would be considered good programming practice.
C
C     Example 1-A: Adding fixed size packets one at a time.
C
C        For this example, we make no assumptions about the reference
C        values returned by GET_VAR_PKT other than they are increasing.
C        Having no other information about the reference values, we must
C        use an explicit indexing method to store the packets.
C
C                                .
C                                .
C                                .
C        C
C        C     First we begin a fixed size segment. To do this, we
C        C     need:
C        C
C        C        HANDLE -- The handle of a DAF opened with write
C        C                  access.
C        C        DESCR  -- The packed descriptor for the segment that
C        C                  we want to create.
C        C        SEGID  -- A short character string that provides an
C        C                  identifier for the segment.
C        C        NCONST -- The number of constant values to be
C        C                  associated with all of the packets in the
C        C                  segment.
C        C        CONST  -- An array of constant values to be associated
C        C                  with all of the packets in a segment.
C        C        PKTSIZ -- The size of the packets that will be stored
C        C                  in this segment, i.e., the number of double
C        C                  precision numbers necessary to store a
C        C                  complete data packet.
C        C        EXPCLS -- The type of indexing scheme that we will use
C        C                  for searching the segment to obtain a data
C        C                  packet. In this case, we are going to use
C        C                  an exlicit index, which requires a reference
C        C                  value for each data packet, and when
C        C                  searching for a data packet we will choose
C        C                  the packet with a reference value closest to
C        C                  the requested value. See the include file
C        C                  'sgparam.inc' for the value of EXPCLS.
C        C
C              CALL SGBWFS ( HANDLE, DESCR,  SEGID,  NCONST,
C             .              CONST,  PKTSIZ, EXPCLS          )
C        C
C        C     We loop until done, obtaining a fixed size packet
C        C     and writing it to the generic segment in the file.
C        C
C              DONE = .FALSE.
C              DO WHILE ( .NOT. DONE )
C        C
C        C        Get a fixed size packet and a reference value.
C        C
C                 CALL GET_FIX_PKT ( PACKET, REF, DONE )
C        C
C        C        Write the packet to the segment, unless we're done.
C        C
C                 IF ( .NOT. DONE ) THEN
C
C                    CALL SGWFPK ( HANDLE, 1, PACKET, 1, REF )
C
C                 END IF
C
C              END DO
C        C
C        C     End the segment and move on to other things.
C        C
C              CALL SGWES ( HANDLE )
C                                .
C                                .
C                                .
C
C     Example 1-B: Adding fixed size packets with uniformly spaced
C                  reference values.
C
C        In the previous example, we made no assumptions about the
C        reference values other than that they were increasing. We now
C        will assume that the reference values are also equally spaced
C        and that we have a priori values for a beginning reference
C        value, BEGIN_REF, and a stepsize, STEP_SIZE, that is the
C        difference between two consecutive reference values. We have
C
C           BEGIN_REF <= REF <= BEGIN_REF + (N-1) * STEP_SIZE
C
C        where BEGIN_REF equals the first reference value returned by
C        GET_FIX_PKT and BEGIN_REF + (N-1) * STEP_SIZE equals the last
C        reference value returned. Under these assumptions we can use an
C        implicit index for the data packets which will provide a more
C        space efficient method for putting the data packets into a
C        generic segment.  We repeat the example under these assumptions
C        using an implicit indexing method. Nothing else has changed.
C
C        The index for a data packet in the implicitly indexed generic
C        segment we create is computed from the formula:
C
C                          /          VALUE - REFDAT(1)    \
C            INDEX = IDINT | 1.5 + ----------------------- |
C                          \              REFDAT(2)        /
C
C        where the index for the data packet associated with VALUE is
C        desired.
C
C        The reference value associated with this index is:
C
C            REF   =  REFDAT(1) + REFDAT*(INDEX - 1)
C
C                                .
C                                .
C                                .
C        C
C        C     First we begin a fixed size segment. To do this, we
C        C     need:
C        C
C        C        HANDLE -- The handle of a DAF opened with write
C        C                  access.
C        C        DESCR  -- The packed descriptor for the segment that
C        C                  we want to create.
C        C        SEGID  -- A short character string that provides an
C        C                  identifier for the segment.
C        C        NCONST -- The number of constant values to be
C        C                  associated with all of the packets in the
C        C                  segment.
C        C        CONST  -- An array of constant values to be associated
C        C                  with all of the packets in a segment.
C        C        PKTSIZ -- The size of the packets that will be stored
C        C                  in this segment, i.e., the number of double
C        C                  precision numbers necessary to store a
C        C                  complete data packet.
C        C        IMPCLS -- The type of indexing scheme that we will use
C        C                  for searching the segment to obtain a data
C        C                  packet. In this case, we are going to use
C        C                  an implicit index, which requires beginning
C        C                  and ending times which bound all reference
C        C                  values, and when searching for a data packet
C        C                  we will choose the packet whose index is
C        C                  computed by the formula above. See the
C        C                  include file 'sgparam.inc' for the value
C        C                  of IMPCLS
C        C
C              CALL SGBWFS ( HANDLE, DESCR,  SEGID,  NCONST,
C             .              CONST,  PKTSIZ, IMPCLS          )
C        C
C        C     Set the beginning and ending reference values for the
C        C     implicit indexing method.
C        C
C              REFS(1) = BEGIN_REF
C              REFS(2) = STEP_SIZE
C        C
C        C     Get the first data packet and put it in the generic
C        C     segment. At the same time, we write the bounds used for
C        C     the implicit indexing. We ignore the value of REF since
C        C     the reference values are equally spaced and we are using
C        C     an implicit indexing method. We do not check DONE here
C        C     because we assume that there is at least one data packet.
C        C
C              CALL GET_FIX_PKT ( PACKET, REF, DONE )
C
C              CALL SGWFPK ( HANDLE, 1, PACKET, 2, REFS )
C        C
C        C     We loop until done, obtaining a fixed size packet
C        C     and writing it to the generic segment in the file.
C        C
C              DO WHILE ( .NOT. DONE )
C        C
C        C        Get a fixed size packet and a reference value.
C        C
C                 CALL GET_FIX_PKT ( PACKET, REF, DONE )
C        C
C        C        Write the packet to the segment, unless we're done.
C        C        Because this segment is implicitly indexed, the last
C        C        two calling arguments are only used in the first call
C        C        to SGWFPK above. they are ignored in all subsequent
C        C        calls, so we may pass "dummy" arguments.
C        C
C                 IF ( .NOT. DONE ) THEN
C
C                    CALL SGWFPK ( HANDLE, 1, PACKET, DUM1, DUM2 )
C
C                 END IF
C
C              END DO
C        C
C        C     End the segment and move on to other things.
C        C
C              CALL SGWES ( HANDLE )
C                                .
C                                .
C                                .
C
C     Example 2: Adding fixed size packets more efficiently.
C
C        It is possible to add more than one fixed size data packet to a
C        generic segment at one time. Doing this will usually prove to
C        be a more efficient way of adding the data packets, provided
C        there is sufficient storage to hold more than one data packet
C        available. This example demonstrates this capability.
C
C        For this example, we make no assumptions about the reference
C        values returned by GET_FIX_PKTS other than they are increasing.
C        Having no other information about the reference values, we must
C        use an explicit indexing method to store the packets.
C
C                                .
C                                .
C                                .
C        C
C        C     First we begin a fixed size segment. To do this, we
C        C     need:
C        C
C        C        HANDLE -- The handle of a DAF opened with write
C        C                  access.
C        C        DESCR  -- The packed descriptor for the segment that
C        C                  we want to create.
C        C        SEGID  -- A short character string that provides an
C        C                  identifier for the segment.
C        C        NCONST -- The number of constant values to be
C        C                  associated with all of the packets in the
C        C                  segment.
C        C        CONST  -- An array of constant values to be associated
C        C                  with all of the packets in a segment.
C        C        PKTSIZ -- The size of the packets that will be stored
C        C                  in this segment, i.e., the number of double
C        C                  precision numbers necessary to store a
C        C                  complete data packet.
C        C        EXPCLS -- The type of indexing scheme that we will use
C        C                  for searching the segment to obtain a data
C        C                  packet. In this case, we are going to use
C        C                  an exlicit index, which requires a reference
C        C                  value for each data packet, and when
C        C                  searching for a data packet we will choose
C        C                  the packet with a reference value closest to
C        C                  the requested value. See the include file
C        C                  'sgparam.inc' for the value of EXPCLS
C        C
C              CALL SGBWFS ( HANDLE, DESCR,  SEGID,  NCONST,
C             .              CONST,  PKTSIZ, EXPCLS          )
C        C
C        C     We loop until done, obtaining a fixed size packet
C        C     and writing it to the generic segment in the file.
C        C
C              DONE = .FALSE.
C              DO WHILE ( .NOT. DONE )
C        C
C        C        Get a collection of fixed size packet and associated
C        C        array of increasing reference values.
C        C
C                 CALL GET_FIX_PKTS ( NPKTS, PKTS, REFS, DONE )
C        C
C        C        Write the packets to the segment if we have any. Since
C        C        we are using an explicit index, the number of
C        C        reference values is the same as the number of data
C        C        packets.
C        C
C                 IF ( .NOT. DONE ) THEN
C
C                    CALL SGWFPK ( HANDLE, NPKTS, PKTS, NPKTS, REFS )
C
C                 END IF
C
C              END DO
C        C
C        C     End the segment and move on to other things.
C        C
C              CALL SGWES ( HANDLE )
C                                .
C                                .
C                                .
C
C        If we are using an implicit indexing method, multiple data
C        packets may be added with one call to SGWFPK as in the above
C        example for an explicit index, with the exception that there
C        are only two reference values, and they are specified on the
C        first call to SGWFPK, as in Example 1-B.
C
C     Example 3-A: Adding variable size packets one at a time.
C
C        For this example, we make no assumptions about the reference
C        values returned by GET_VAR_PKT other than they are increasing.
C        Having no other information about the reference values, we must
C        use an explicit indexing method to store the packets.
C
C                                .
C                                .
C                                .
C        C
C        C     First we begin a variable size segment. To do this, we
C        C     need:
C        C
C        C        HANDLE -- The handle of a DAF opened with write
C        C                  access.
C        C        DESCR  -- The packed descriptor for the segment that
C        C                  we want to create.
C        C        SEGID  -- A short character string that provides an
C        C                  identifier for the segment.
C        C        NCONST -- The number of constant values to be
C        C                  associated with all of the packets in the
C        C                  segment.
C        C        CONST  -- An array of constant values to be associated
C        C                  with all of the packets in a segment.
C        C        EXPCLS -- The type of indexing scheme that we will use
C        C                  for searching the segment to obtain a data
C        C                  packet. In this case, we are going to use
C        C                  an exlicit index, which requires a reference
C        C                  value for each data packet, and when
C        C                  searching for a data packet we will choose
C        C                  the packet with a reference value closest to
C        C                  the requested value. See the include file
C        C                  'sgparam.inc' for the value of EXPCLS.
C        C
C              CALL SGBVFS ( HANDLE, DESCR, SEGID,
C             .              NCONST, CONST, EXPCLS )
C        C
C        C     We loop until done, obtaining a variable size packet
C        C     and writing it to the generic segment in the file.
C        C
C              DONE = .FALSE.
C              DO WHILE ( .NOT. DONE )
C        C
C        C        Get a variable size packet and a reference value.
C        C
C                 CALL GET_VAR_PKT ( PACKET, SIZE, REF, DONE )
C        C
C        C        Write the packet to the segment, unless we're done.
C        C
C                 IF ( .NOT. DONE ) THEN
C
C                    CALL SGWVPK ( HANDLE, 1, SIZE, PACKET, 1, REF )
C
C                 END IF
C
C              END DO
C        C
C        C     End the segment and move on to other things.
C        C
C              CALL SGWES ( HANDLE )
C                                .
C                                .
C                                .
C
C     Example 3-B: Adding variable size packets one at a time with
C                  uniformly spaced reference values.
C
C        In the previous example, we made no assumptions about the
C        reference values other than that they were increasing. We now
C        will assume that the reference values are also equally spaced
C        and that we have a priori values for a beginning reference
C        value, BEGIN_REF, and a stepsize, STEP_SIZE, that is the
C        difference between two consecutive reference values. We have
C
C           BEGIN_REF <= REF <= BEGIN_REF + (N-1) * STEP_SIZE
C
C        where BEGIN_REF equals the first reference value returned by
C        GET_VAR_PKT and BEGIN_REF + (N-1) * STEP_SIZE equals the last
C        reference value returned. Putting all of this together means
C        that we can use an implicit index for the data packets which
C        will provide a more space efficient method for putting the data
C        packets into a generic segment.  We repeat the example under
C        these assumptions using an implicit indexing method. Nothing
C        else has changed.
C
C        The index for a data packet in the implicitly indexed generic
C        segment we create is computed from the formula:
C
C                          /          VALUE - REFDAT(1)    \
C            INDEX = IDINT | 1.5 + ----------------------- |
C                          \              REFDAT(2)        /
C
C        where the index for the data packet associated with VALUE is
C        desired.
C
C        The reference value associated with this index is:
C
C            REF   =  REFDAT(1) + REFDAT*(INDEX - 1)
C
C                                .
C                                .
C                                .
C        C
C        C     First we begin a variable size segment. To do this, we
C        C     need:
C        C
C        C        HANDLE -- The handle of a DAF opened with write
C        C                  access.
C        C        DESCR  -- The packed descriptor for the segment that
C        C                  we want to create.
C        C        SEGID  -- A short character string that provides an
C        C                  identifier for the segment.
C        C        NCONST -- The number of constant values to be
C        C                  associated with all of the packets in the
C        C                  segment.
C        C        CONST  -- An array of constant values to be associated
C        C                  with all of the packets in a segment.
C        C        IMPCLS -- The type of indexing scheme that we will use
C        C                  for searching the segment to obtain a data
C        C                  packet. In this case, we are going to use
C        C                  an implicit index, which requires beginning
C        C                  and ending times which bound all reference
C        C                  values, and when searching for a data packet
C        C                  we will choose the packet whose index is
C        C                  computed by the formula above. See the
C        C                  include file 'sgparam.inc' for the value of
C        C                  IMPCLS.
C        C
C              CALL SGBWVS ( HANDLE, DESCR,  SEGID,  NCONST,
C             .              CONST,  IMPCLS                   )
C        C
C        C     Set the beginning and ending reference values for the
C        C     implicit indexing method.
C        C
C              REFS(1) = BEGIN_REF
C              REFS(2) = STEP_SIZE
C        C
C        C     Get the first data packet and put it in the generic
C        C     segment. At the same time, we write the bounds used for
C        C     the implicit indexing. We ignore the value of REF since
C        C     the reference values are equally spaced and we are using
C        C     an implicit indexing method. We do not check DONE here
C        C     because we assume that there is at least one data packet.
C        C
C              CALL GET_VAR_PKT ( PACKET, SIZE, REF, DONE )
C
C              CALL SGWVPK ( HANDLE, 1, SIZE, PACKET, 2, REFS )
C        C
C        C     We loop until done, obtaining a fixed size packet
C        C     and writing it to the generic segment in the file.
C        C
C              DO WHILE ( .NOT. DONE )
C        C
C        C        Get a variable size packet and a unique reference
C        C        value.
C        C
C                 CALL GET_VAR_PKT ( PACKET, SIZE, REF, DONE )
C        C
C        C        Write the packet to the segment, unless we're done.
C        C        Because this segment is implicitly indexed, the last
C        C        two calling arguments are only used in the first call
C        C        to SGWFPK above. they are ignored in all subsequent
C        C        calls, so we may pass "dummy" arguments.
C        C
C                 IF ( .NOT. DONE ) THEN
C
C                    CALL SGVFPK ( HANDLE, 1, SIZE, PACKET, DUM1, DUM2 )
C
C                 END IF
C
C              END DO
C        C
C        C     End the segment and move on to other things.
C        C
C              CALL SGWES ( HANDLE )
C                                .
C                                .
C                                .
C
C     Example 4: Adding variable size packets more efficiently.
C
C        It is possible to add more than one variable size data packet
C        to a generic segment at one time. Doing this will usually prove
C        to be a more efficient way of adding the data packets, provided
C        there is sufficient storage to hold more than one data packet
C        available. This example demonstrates this capability.
C
C        For this example, we make no assumptions about the reference
C        values returned by GET_VAR_PKTS other than they are increasing.
C        Having no other information about the reference values, we must
C        use an explicit indexing method to store the packets.
C
C                                .
C                                .
C                                .
C        C
C        C     First we begin a variable size segment. To do this, we
C        C     need:
C        C
C        C        HANDLE -- The handle of a DAF opened with write
C        C                  access.
C        C        DESCR  -- The packed descriptor for the segment that
C        C                  we want to create.
C        C        SEGID  -- A short character string that provides an
C        C                  identifier for the segment.
C        C        NCONST -- The number of constant values to be
C        C                  associated with all of the packets in the
C        C                  segment.
C        C        CONST  -- An array of constant values to be associated
C        C                  with all of the packets in a segment.
C        C        EXPCLS -- The type of indexing scheme that we will use
C        C                  for searching the segment to obtain a data
C        C                  packet. In this case, we are going to use
C        C                  an exlicit index, which requires a reference
C        C                  value for each data packet, and when
C        C                  searching for a data packet we will choose
C        C                  the packet with a reference value closest to
C        C                  the requested value. See the include file
C        C                  sgparam.inc for the value of EXPCLS.
C        C
C              CALL SGBWVS ( HANDLE, DESCR,  SEGID,
C        C    .              NCONST, CONST, EXPCLS  )
C        C
C        C     We loop until done, obtaining a fixed size packet
C        C     and writing it to the generic segment in the file.
C        C
C              DONE = .FALSE.
C              DO WHILE ( .NOT. DONE )
C        C
C        C        Get a collection of variable size packets and an
C        C        array of increasing reference values.
C        C
C                 GET_VAR_PKTS ( NPKTS, PKTS, SIZES, REFS, DONE )
C        C
C        C        Write the packets to the segment if we have any. Since
C        C        we are using an explicit index, the number of
C        C        reference values is the same as the number of data
C        C        packets.
C        C
C                 IF ( NPKTS .GT. 0 ) THEN
C
C                    CALL SGWVPK ( HANDLE, NPKTS, SIZES,
C             .                    PKTS,   NPKTS, REFS   )
C
C                 END IF
C
C              END DO
C        C
C        C     End the segment and move on to other things.
C        C
C              CALL SGWES ( HANDLE )
C                                .
C                                .
C                                .
C
C        If we are using an implicit indexing method, multiple data
C        packets may be added with one call to SGWVPK as in the above
C        example for an explicit index, with the exception that there
C        are only two reference values, and they are specified on the
C        first call to SGWVPK, as in Example 3-B.
C
C     Example 5: Adding packets to multiple files.
C
C        It is possible to write multiple generic segments to different
C        DAFs at the same time. Only one generic segment may be written
C        to a particular DAF at any given time, however.
C
C        For this example we assume that we have previously opened four
C        DAF files, having the handles HANDL1, HANDL2, HANDL3, HANDL4.
C        We will be writing fixed size data packets to the DAFs
C        associated with handles HANDL2 and HANDL3, with packet sizes of
C        21 and 53, respectively. We will be writing variable size data
C        packets to the DAFs associated with handles HANDL1 and HANDL4.
C        We will be writing individual data packets to the files
C        associated with handles HANDL2 and HANDL4, and one or more data
C        packets to the files associated with handles HANDL1 and HANDL3.
C        On each trip through the loop in the example below, we will add
C        data to any of the segments whose status flags are not set. We
C        are done with the loop below when we have finished each of the
C        segments, as indicated by its status flag.
C
C        For this example, we make no assumptions about the reference
C        values returned by the GET_*_* subroutines other than they are
C        increasing. Having no other information about the reference
C        values, we must use an explicit indexing method to store the
C        packets.
C
C                                .
C                                .
C                                .
C        C
C        C     First we begin a generic segment of the appropriate type
C        C     in each of the files. segment. To do this, we need:
C        C
C        C        HANDL1, HANDL2, HANDL3, HANDL4 --
C        C
C        C           The handles of a DAFs opened with write access to
C        C           which we wish to add a new generic segment.
C        C
C        C        DESCR1, DESCR2, DESCR3, DESCR4  --
C        C
C        C           The packed descriptors for the segments that
C        C           we want to create.
C        C
C        C        SEGID1, SEGID2, SEGID3, SEGID4 --
C        C
C        C           A short character string that provides an
C        C           identifier for each of the segments we will be
C        C           creating.
C        C
C        C        NCON1, NCON2, NCON3, NCON4 --
C        C
C        C           The number of constant values to be associated with
C        C           all of the packets in each the segments we will be
C        C           creating.
C        C
C        C
C        C        CONST1, CONST2, CONST3, CONST4 --
C        C
C        C           An array of constant values to be associated with
C        C           all of the packets in each of the segments that we
C        C           are creating.
C        C
C        C        IDXT1, IDXT2, IDXT3, IDXT4 --
C        C
C        C          The type of indexing scheme that we will use for
C        C          searching each of the segments to obtain a data
C        C          packet. In this example, each of the generic
C        C          segments will use an explicit index, which requires
C        C          a reference value for each data packet. When
C        C          searching for a data packet we will choose the
C        C          packet with a reference value closest to the
C        C          requested value.
C        C
C        C            IDXT1 = EXPCLS
C        C            IDXT2 = EXPCLS
C        C            IDXT3 = EXPCLS
C        C            IDXT4 = EXPCLS
C        C
C              CALL SGBWVS ( HANDL1, DESCR1, SEGID1,
C             .              NCON1,  CONST1, IDXT1   )
C              CALL SGBWFS ( HANDL2, DESCR2, SEGID2, 21,
C             .              NCON2,  CONST2, IDXT2   )
C              CALL SGBWFS ( HANDL3, DESCR3, SEGID3, 53,
C             .              NCON3,  CONST3, IDXT3   )
C              CALL SGBWVS ( HANDL4, DESCR4, SEGID4,
C             .              NCON4,  CONST4, IDXT4   )
C        C
C        C     We loop until done, obtaining data packets and writing
C        C     them to the generic segments in the appropriate DAFs.
C        C
C        C     We keep track of a status flag, DONE1, DONE2, DONE3,
C        C     DONE4, for each of the segments we are writing. When we
C        C     have finished writing all of the segments, we exit the
C        C     loop.
C        C
C              DONE  = .FALSE.
C              DONE1 = .FALSE.
C              DONE2 = .FALSE.
C              DONE3 = .FALSE.
C              DONE4 = .FALSE.
C
C              DO WHILE ( .NOT. DONE )
C        C
C        C        Get data packets and reference values for HANDL1 and
C        C        write them to the generic segment in that file.
C        C
C                 IF ( .NOT. DONE1 ) THEN
C                    GET_VAR_PKTS ( NPKTS, PKTS, SIZES, REFS, DONE1 )
C
C                    IF ( NPKTS .GT. 0 ) THEN
C                       CALL SGWVPK ( HANDL1, NPKTS, SIZES,
C             .                       PKTS,   NPKTS, REFS   )
C                    END IF
C                 END IF
C        C
C        C        Get a data packet and reference value for HANDL2 and
C        C        write it to the generic segment in that file.
C        C
C                 IF ( .NOT. DONE2 ) THEN
C                    CALL GET_FIX_PKT ( PACKET, REF, DONE2 )
C
C                    IF ( .NOT. DONE2 ) THEN
C                       CALL SGWFPK ( HANDL2, 1, PACKET, 1, REF )
C                    END IF
C                 END IF
C        C
C        C        Get data packets and reference values for HANDL3 and
C        C        write them to the generic segment in that file.
C        C
C                 IF ( .NOT. DONE3 ) THEN
C                    CALL GET_FIX_PKTS ( NPKTS, PKTS, REFS, DONE3 )
C
C                    IF ( NPKTS .GT. 0 ) THEN
C                       CALL SGWFPK ( HANDL3, NPKTS, PKTS, NPKTS, REFS )
C                    END IF
C                 END IF
C        C
C        C        Get a data packet and reference value for HANDL4 and
C        C        write it to the generic segment in that file.
C        C
C                 IF ( .NOT. DONE4 ) THEN
C                    GET_VAR_PKT ( PACKET, SIZE, REF, DONE4 )
C
C                    IF ( .NOT. DONE4 ) THEN
C                       CALL SGWVPK ( HANDL4, 1, SIZES, PKTS, 1, REFS )
C                    END IF
C                 END IF
C        C
C        C        Set the DONE flag.
C        C
C                 DONE = DONE1 .AND. DONE2 .AND. DONE3 .AND. DONE4
C
C              END DO
C        C
C        C     End the segments and move on to other things.
C        C
C              CALL SGWES ( HANDL1 )
C              CALL SGWES ( HANDL2 )
C              CALL SGWES ( HANDL3 )
C              CALL SGWES ( HANDL4 )
C                                .
C                                .
C                                .
C
C$ Restrictions
C
C     See the individual entry points for any restrictions thay may
C     have.
C
C$ Author_and_Institution
C
C     K.R. Gehringer    (JPL)
C     W.L. Taber        (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 07-SEP-2001 (EDW)
C
C        Replaced DAFRDA calls with DAFGDA.
C        Removed DAFHLU calls; replaced ERRFN calls with ERRHAN.
C
C-    SPICELIB Version 1.1.0, 30-JUL-1996 (KRG) (NJB)
C
C        Fixed an annoying little bug in the variable segments code
C        when ending a segment. Rather than storing an appropriate
C        offset from the beginning of the segment as the packet
C        address in the packet directory, the absolute address, the
C        DAF address, was stored. This bug has been fixed.
C
C        See SGWES for the details of the changes.
C
C-    SPICELIB Version 1.0.0, 03-APR-1995 (KRG) (WLT)
C
C-&
 
C$ Index_Entries
C
C     generic segments sequential writer
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               LASTNB
      INTEGER               ISRCHI
 
      LOGICAL               FAILED
      LOGICAL               RETURN
C
C     Local parameters
C
C     FPRINT is the integer value of the first printable ASCII
C     character.
C
C     LPRINT is the integer value of the last printable ASCII character.
C
      INTEGER               FPRINT
      PARAMETER           ( FPRINT =  32 )
 
      INTEGER               LPRINT
      PARAMETER           ( LPRINT = 126 )
C
C     The number of reference values it takes to get a reference
C     directory value.
C
      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ = 100 )
C
C     The length of a DAF internal filename.
C
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )
C
C     The file table size. This needs to be the same as the file table
C     size in DAFAH.
C
      INTEGER               FTSIZE
      PARAMETER           ( FTSIZE = 20 )
C
C     Include the mnemonic values for the generic segment declarations
C     and the meta data information.
C
      INCLUDE 'sgparam.inc'
C
C     Local variables
C
C     Variables with the name DUMMY* are used as place holders when
C     calling various subroutines. Their values are not used in any of
C     the entry points of this subroutine.
C
      CHARACTER*(IFNLEN)    DUMMY1
 
      DOUBLE PRECISION      XMETA  ( MXMETA )
      DOUBLE PRECISION      MYREF
      DOUBLE PRECISION      MYADDR
      DOUBLE PRECISION      MYSIZE
      DOUBLE PRECISION      DPKSIZ
 
      INTEGER               BEGADR
      INTEGER               DUMMY2
      INTEGER               DUMMY3
      INTEGER               I
      INTEGER               ICH
      INTEGER               INDEX
      INTEGER               LSTHAN
      INTEGER               META   ( MXMETA )
      INTEGER               NC
      INTEGER               ND
      INTEGER               NI
      INTEGER               NUMFXD
      INTEGER               NUMVAR
      INTEGER               PKTADR
      INTEGER               PKTPOS
      INTEGER               REFADR
      INTEGER               SIDLEN
      INTEGER               SIZE
 
      LOGICAL               EXPLCT
      LOGICAL               FXDSEG
C
C     File table declarations. The file table is used to keep track of
C     the vital statistics for each of the generic segments being
C     written.
C
      DOUBLE PRECISION      FTREFS ( 2, FTSIZE )
 
      INTEGER               FTBADR ( FTSIZE )
      INTEGER               FTHAN  ( FTSIZE )
      INTEGER               FTITYP ( FTSIZE )
      INTEGER               FTMXSZ ( FTSIZE )
      INTEGER               FTNCON ( FTSIZE )
      INTEGER               FTNPKT ( FTSIZE )
      INTEGER               FTNREF ( FTSIZE )
      INTEGER               FTNRES ( FTSIZE )
      INTEGER               FTOFF  ( FTSIZE )
      INTEGER               FTPKSZ ( FTSIZE )
 
      INTEGER               NFT
 
      LOGICAL               FTFIXD ( FTSIZE )
      LOGICAL               FTEXPL ( FTSIZE )
C
C     Saved values.
C
      SAVE                  EXPLCT
      SAVE                  INDEX
      SAVE                  NUMFXD
      SAVE                  NUMVAR
      SAVE                  LSTHAN
      SAVE                  FXDSEG
C
C     Save the file table.
C
      SAVE                  FTREFS
      SAVE                  FTBADR
      SAVE                  FTHAN
      SAVE                  FTMXSZ
      SAVE                  FTNCON
      SAVE                  FTNPKT
      SAVE                  FTNREF
      SAVE                  FTNRES
      SAVE                  FTITYP
      SAVE                  FTEXPL
      SAVE                  FTOFF
      SAVE                  FTPKSZ
      SAVE                  FTFIXD
      SAVE                  NFT
C
C     Initial values
C
      DATA                  NUMFXD / 0 /
      DATA                  NUMVAR / 0 /
      DATA                  NFT    / 0 /
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
C
C     Signal an error if this routine is called directly.
C
      CALL CHKIN  ( 'SGSEQW'                                      )
      CALL SETMSG ( 'This routine should never be called'
     .//            ' directly. It exists as an umbrella routine'
     .//            ' to maintain all of the variables for the'
     .//            ' generic segment sequential writing entry'
     .//            ' points.'                                    )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)'                           )
      CALL CHKOUT ( 'SGSEQW'                                      )
      RETURN
 
 
C$Procedure SGBWFS ( Generic segements: Begin a fixed size segment. )
 
      ENTRY SGBWFS ( HANDLE, DESCR, SEGID,
     .               NCONST, CONST, PKTSIZ, IDXTYP )
 
C$ Abstract
C
C     Begin writing a generic segment that will contain fixed size data
C     packets.
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
C     DAF Required Reading.
C
C$ Keywords
C
C     GENERIC SEGMENTS
C
C$ Declarations
C
C     INTEGER               HANDLE
C     DOUBLE PRECISION      DESCR  ( * )
C     CHARACTER*(*)         SEGID
C     INTEGER               NCONST
C     DOUBLE PRECISION      CONST  ( * )
C     INTEGER               PKTSIZ
C     INTEGER               IDXTYP
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C      HANDLE    I    Handle of a DAF file opened with write access.
C      DESCR     I    Descriptor for a generic segment.
C      SEGID     I    Identifier for a generic segment.
C      NCONST    I    Number of constant values in a generic segment.
C      CONST     I    Array of constant values for a generic segment.
C      PKTSIZ    I    Size of the data packets.
C      IDXTYP    I    Index type for the reference values.
C
C$ Detailed_Input
C
C      HANDLE   Handle of a DAF file opened with write access. This is
C               the handle of the file in which a generic segment will
C               be written.
C
C      DESCR    Descriptor for a segment that is being written. This is
C               the packed form of the DAF double precision and integer
C               summaries which contain ND double precision numbers and
C               NI integers.
C
C      SEGID    Identifier for a segment that is being written. This is
C               a character string containing at most NC printing ASCII
C               characters where
C
C                                 /  ND + ( NI + 1 )  \
C                      NC =  8 *  | ----------------- |
C                                 \         2         /
C
C                SEGID may be blank.
C
C      NCONST   The number of constant values to be placed in a segment.
C
C      CONST    An array of NCONST constant values for a segment.
C
C      PKTSIZ   Size of fixed size packets. The size of a packet
C               is the number of double precision numbers contained in
C               the data packet.
C
C      IDXTYP   Index type to use for the reference values.
C
C               Two forms of indexing are provided:
C
C                  1) An implicit form of indexing based on using two
C                     values, a starting value, which will have an index
C                     of 1, and a step size between reference values,
C                     which are used to compute an index and a reference
C                     value associated with a specified key value. See
C                     the descriptions of the implicit types below for
C                     the particular formula used in each case.
C
C                  2) An explicit form of indexing based on a reference
C                     value for each data packet.
C
C               See the chapter on generic segments in the DAF required
C               or the include file 'sgparam.inc' for more details
C               about the index types that are available.
C
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     This subroutine makes use of parameters defined in the file
C     'sgparam.inc'.
C
C$ Exceptions
C
C     1) If this routine is called more than once for a particular file
C        and segment, the error SPICE(CALLEDOUTOFORDER) will be
C        signalled.
C
C     2) If the length of the segment identifier, SEGID, is greater than
C        NC, as determined from the ND and NI values for a particular
C        DAF file, the error SPICE(SEGIDTOOLONG) will be signalled.
C
C     3) If the segment identifier contains nonprinting characters, the
C        error SPICE(NONPRINTINGCHARS) will be signalled.
C
C     4) If the number of constant values, NCONST, is negative, the
C        error SPICE(NUMCONSTANTSNEG) will be signalled.
C
C     5) If the packet size, PKTSIZ, is not positive, the error
C        SPICE(NONPOSPACKETSIZE) will be signalled.
C
C     6) If the index type for the reference values is not recognized,
C        the error SPICE(UNKNOWNINDEXTYPE) will be signalled.
C
C     7) If the file table is full, the error SPICE(FILETABLEFULL) will
C        be signalled.
C
C$ Files
C
C     See HANDLE in the $ Detailed_Input section.
C
C$ Particulars
C
C     Begin writing a generic segment for fixed size data packets to
C     the DAF file associated with HANDLE.
C
C$ Examples
C
C     See the $ Examples section in the header for the main subroutine.
C     It contains examples wich demonstrate the use of the entry points
C     in the generic segments sequential writer. The entry points which
C     comprise the generic segments sequential writer must be used
C     together in the proper manner. Rather than repeating the examples
C     for each entry point they are provided in a single location.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer    (JPL)
C     W.L. Taber        (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 05-APR-1995 (KRG) (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     begin writing a fixed packet size generic segment
C
C-&
 
C
C     SPICELIB functions
C
C     INTEGER               LASTNB
C     INTEGER               ISRCHI
C
C     LOGICAL               FAILED
C     LOGICAL               RETURN
C
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'SGBWFS' )
C
C     We need to do some sanity checks on our input arguments before we
C     should attempt to write anything to the file. So, let's start with
C     that.
C
C     Check to see if the file attached to the handle is open for
C     writing. If not, an error is signalled.
C
      CALL DAFSIH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'SGBWFS' )
         RETURN
 
      END IF
C
C     Check to see if the handle is currently in the file table. If it
C     is, we've got a problem. This routine may only be called once for
C     each segment that is to contain fixed size packets, and it places
C     a handle in the file table. If the handle is currently in the
C     file table a segment has already been started by this routine or
C     SGBWVS. In either case, we cannot continue, so we signal an error.
C
      IF ( NFT .GT. 0 ) THEN
 
         INDEX = ISRCHI ( HANDLE, NFT, FTHAN )
 
         IF ( INDEX .NE. 0 ) THEN
 
            CALL SETMSG ( 'A segment is already being written to'
     .      //            ' the file ''#''. A new segment cannot'
     .      //            ' be started for this file until the'
     .      //            ' current segment is finished. '          )
            CALL ERRHAN ( '#', HANDLE                               )
            CALL SIGERR ( 'SPICE(CALLEDOUTOFORDER)'                 )
            CALL CHKOUT ( 'SGBWFS'                                  )
            RETURN
 
         END IF
 
      END IF
C
C     Get the ND and NI values from the DAF file. We need these to know
C     the size of the descriptor and the length of the segment ID. The
C     length of the segment ID is determined by the following formula
C     using integer division:
C
C                                 /  ND + ( NI + 1 )  \
C                      NC =  8 *  | ----------------- |
C                                 \         2         /
C
      CALL DAFHSF ( HANDLE, ND, NI )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SGBWFS' )
         RETURN
      END IF
 
      NC = 8 * ( ND + ( NI + 1 ) / 2 )
C
C     Get the length of the segment ID. Leading blanks are considered to
C     be important. A blank segment ID is OK too.
C
      SIDLEN = LASTNB ( SEGID )
C
C     Check the segment ID to see if it is OK. Its length must be less
C     than NC and it must consist of only printing ASCII characters.
C
      IF ( SIDLEN .GT. NC )THEN
 
         CALL SETMSG ( 'Segment identifier contains more than '
     .   //            '# characters.'                           )
         CALL ERRINT ( '#', NC                                   )
         CALL SIGERR ( 'SPICE(SEGIDTOOLONG)'                     )
         CALL CHKOUT ( 'SGBWFS'                                  )
         RETURN
 
      END IF
 
      DO I = 1, SIDLEN
 
         ICH = ICHAR( SEGID(I:I) )
 
         IF ( ( ICH .LT. FPRINT ) .OR. ( ICH .GT. LPRINT ) ) THEN
 
            CALL SETMSG ( 'The segment identifier contains '
     .      //            ' a nonprinting character at location #.' )
            CALL ERRINT ( '#', I                                    )
            CALL SIGERR ( 'SPICE(NONPRINTINGCHARS)'                 )
            CALL CHKOUT ( 'SGBWFS'                                  )
            RETURN
 
         END IF
 
      END DO
C
C     Check to see if the number of constants is negative. This is all
C     we can do here, we cannot check the constant values.
C
      IF ( NCONST .LT. 0 ) THEN
 
         CALL SETMSG ( 'The number of constants specified was #. '
     .   //            'This number must be non-negative. '
     .   //            'Perhaps the variable was not properly'
     .   //            'initialized. '                           )
         CALL ERRINT ( '#', NCONST                               )
         CALL SIGERR ( 'SPICE(NUMCONSTANTSNEG) '                 )
         CALL CHKOUT (  'SGBWFS'                                 )
         RETURN
 
      END IF
C
C     Check to see that the packet size is OK. It should be positive.
C
      IF ( PKTSIZ(1) .LE. 0 ) THEN
 
         CALL SETMSG ( 'The size of the data packets must be'
     .   //            ' positive. It was specified as #.'
     .   //            ' Perhaps the input variable was not'
     .   //            ' properly initialized. '              )
         CALL ERRINT ( '#', PKTSIZ                            )
         CALL SIGERR ( 'SPICE(NONPOSPACKETSIZE)'              )
         CALL CHKOUT (  'SGBWFS'                              )
         RETURN
 
      END IF
C
C     Check to see if the index type is one that we recognize.
C
      IF ( ( IDXTYP .LT. MNIDXT) .OR. ( IDXTYP .GT. MXIDXT ) ) THEN
 
         CALL SETMSG ( 'The index type specified was #.  This is'
     .   //            ' not a valid index type. Valid types are'
     .   //            ' in the range from # to #.'               )
         CALL ERRINT ( '#', IDXTYP                                )
         CALL ERRINT ( '#', MNIDXT                                )
         CALL ERRINT ( '#', MXIDXT                                )
         CALL SIGERR ( 'SPICE(UNKNOWNINDEXTYPE)'                  )
         CALL CHKOUT ( 'SGBWFS'                                   )
         RETURN
 
      END IF
C
C     Check to see whether we still have room in the file table.
C
      IF ( NFT .EQ. FTSIZE ) THEN
 
         CALL SETMSG ( 'There are already # files being'
     .   //            ' written by generic segment writing'
     .   //            ' routines. No more files may be written'
     .   //            ' by the generic segment writers until'
     .   //            ' one of those currently being written is'
     .   //            ' closed via a call to SGWES.'             )
         CALL ERRINT ( '#', NFT                                   )
         CALL SIGERR ( 'SPICE(FILETABLEFULL)'                     )
         CALL CHKOUT ( 'SGBWFS'                                   )
         RETURN
 
      END IF
C
C     Set the flag which indicate whether this index type is an
C     explicit type or an implicit type.
C
      EXPLCT = ( IDXTYP .EQ. EXPLT  ) .OR. ( IDXTYP .EQ. EXPLE ) .OR.
     .         ( IDXTYP .EQ. EXPCLS )
C
C     At this point, we know that the input data is OK, in so far as we
C     can validate it, and we have room in the file table. So we proceed
C     with starting a segment for fixed size packets.
C
C     Set the flag that indicate that this segment is a fixed size
C     segment.
C
      FXDSEG = .TRUE.
C
C     Get the address for the beginning of the array that we are going
C     to create. We have to get this by reading the file record.
C
      CALL DAFRFR ( HANDLE, ND, NI, DUMMY1, DUMMY2, DUMMY3, BEGADR )
C
C     Begin a new segment in the DAF file.
C
      CALL DAFBNA ( HANDLE, DESCR, SEGID )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SGBWFS' )
         RETURN
      END IF
C
C     Write out the constants to the new segment, if there are any
C     constants.
C
      IF ( NCONST .GT. 0 ) THEN
 
         CALL DAFADA ( CONST, NCONST )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SGBWFS' )
            RETURN
         END IF
 
      END IF
C
C     Store the information for this file and segment in the file table.
C
      NFT              = NFT + 1
 
      FTITYP ( NFT )   = IDXTYP
      FTPKSZ ( NFT )   = PKTSIZ(1)
      FTMXSZ ( NFT )   = 0
 
      FTNCON ( NFT )   = NCONST
      FTNPKT ( NFT )   = 0
      FTNREF ( NFT )   = 0
      FTNRES ( NFT )   = 0
 
      FTEXPL ( NFT )   = EXPLCT
 
      FTFIXD ( NFT )   = FXDSEG
 
      FTHAN  ( NFT )   = HANDLE
      FTBADR ( NFT )   = BEGADR
 
      FTREFS( 1, NFT ) = 0.0D0
      FTREFS( 2, NFT ) = 0.0D0
 
      IF ( EXPLCT ) THEN
         FTOFF(NFT) = 1
      ELSE
         FTOFF(NFT) = 0
      END IF
 
      LSTHAN = HANDLE
      INDEX  = NFT
      NUMFXD = NUMFXD + 1
 
      CALL CHKOUT ( 'SGBWFS' )
      RETURN
 
 
C$Procedure SGBWVS ( Generic segements: Begin a variable size segment. )
 
      ENTRY SGBWVS ( HANDLE, DESCR, SEGID, NCONST, CONST, IDXTYP )
 
C$ Abstract
C
C     Begin writing a generic segment that will contain variable size
C     data packets.
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
C     DAF Required Reading.
C
C$ Keywords
C
C     GENERIC SEGMENTS
C
C$ Declarations
C
C     INTEGER               HANDLE
C     DOUBLE PRECISION      DESCR  ( * )
C     CHARACTER*(*)         SEGID
C     INTEGER               NCONST
C     DOUBLE PRECISION      CONST  ( * )
C     INTEGER               IDXTYP
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C      HANDLE    I    Handle of a DAF file opened with write access.
C      DESCR     I    Descriptor for a segment.
C      SEGID     I    Identifier for a segment.
C      NCONST    I    Number of constant values in a segment.
C      CONST     I    Array of constant values for a segment.
C      IDXTYP    I    Index type for the reference values.
C
C$ Detailed_Input
C
C      HANDLE   Handle of a DAF file opened with write access. This is
C               the handle of the file in which a generic segment will
C               be written.
C
C      DESCR    Descriptor for a segment that is being written. This is
C               the packed form of the DAF double precision and integer
C               summaries which contain ND double precision numbers and
C               NI integers.
C
C      SEGID    Identifier for a segment that is being written. This is
C               a character string containing at most NC printing ASCII
C               characters where
C
C                                 /  ND + ( NI + 1 )  \
C                      NC =  8 *  | ----------------- |
C                                 \         2         /
C
C                SEGID may be blank.
C
C      NCONST   The number of constant values to be placed in a segment.
C
C      CONST    An array of NCONST constant values for a segment.
C
C      IDXTYP   Index type to use for the reference values.
C
C               Two forms of indexing are provided:
C
C                  1) An implicit form of indexing based on using two
C                     values, a starting value, which will have an index
C                     of 1, and a step size between reference values,
C                     which are used to compute an index and a reference
C                     value associated with a specified key value. See
C                     the descriptions of the implicit types below for
C                     the particular formula used in each case.
C
C                  2) An explicit form of indexing based on a reference
C                     value for each data packet.
C
C               See the chapter on generic segments in the DAF required
C               or the include file 'sgparam.inc' for more details
C               about the index types that are available.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     This subroutine makes use of parameters defined in the file
C     'sgparam.inc'.
C
C$ Exceptions
C
C     1) If this routine is called more than once for a particular file
C        and segment, the error SPICE(CALLEDOUTOFORDER) will be
C        signalled.
C
C     2) If the length of the segment identifier, SEGID, is greater than
C        NC, as determined from the ND and NI values for a particular
C        DAF file, the error SPICE(SEGIDTOOLONG) will be signalled.
C
C     3) If the segment identifier contains nonprinting characters, the
C        error SPICE(NONPRINTINGCHARS) will be signalled.
C
C     4) If the number of constant values, NCONST, is negative, the
C        error SPICE(NUMCONSTANTSNEG) will be signalled.
C
C     5) If the index type for the reference values is not recognized,
C        the error SPICE(UNKNOWNINDEXTYPE) will be signalled.
C
C     6) If the file table is full, the error SPICE(FILETABLEFULL) will
C        be signalled.
C
C$ Files
C
C     See HANDLE in the $ Detailed_Input section.
C
C$ Particulars
C
C     Begin writing a generic segment for variable size data packets to
C     the DAF file associated with HANDLE.
C
C$ Examples
C
C     See the $ Examples section in the header for the main subroutine.
C     It contains examples wich demonstrate the use of the entry points
C     in the generic segments sequential writer. The entry points which
C     comprise the generic segments sequential writer must be used
C     together in the proper manner. Rather than repeating the examples
C     for each entry point they are provided in a single location.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer    (JPL)
C     W.L. Taber        (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 05-APR-1995 (KRG) (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     begin writing a variable packet size generic segment
C
C-&
 
C
C     SPICELIB functions
C
C     INTEGER               LASTNB
C     INTEGER               ISRCHI
C
C     LOGICAL               FAILED
C     LOGICAL               RETURN
C
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
      CALL CHKIN  ( 'SGBWVS' )
C
C     We need to do some sanity checks on our input arguments before we
C     should attempt to write anything to the file. So, let's start with
C     that.
C
C     Check to see if the file attached to the handle is open for
C     writing. If not, an error is signalled.
C
      CALL DAFSIH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'SGBWVS' )
         RETURN
 
      END IF
C
C     Check to see if the handle is currently in the file table. If it
C     is, we've got a problem. This routine may only be called once for
C     each segment that is to contain variable size packets, and it
C     places a handle into the file table. If the handle is currently in
C     the file table a segment has already been started by this routine
C     or SGBWFS. In either case, we cannot continue, so we signal an
C     error.
C
      IF ( NFT .GT. 0 ) THEN
 
         INDEX = ISRCHI ( HANDLE, NFT, FTHAN )
 
         IF ( INDEX .NE. 0 ) THEN
 
            CALL SETMSG ( 'A segment is already being written to'
     .      //            ' the file ''#''. A new segment cannot'
     .      //            ' be started for this file until the'
     .      //            ' current segment is finished. '          )
            CALL ERRHAN ( '#', HANDLE                               )
            CALL SIGERR ( 'SPICE(CALLEDOUTOFORDER)'                 )
            CALL CHKOUT ( 'SGBWVS'                                  )
            RETURN
 
         END IF
 
      END IF
C
C     Get the ND and NI values from the DAF file. We need these to know
C     the size of the descriptor and the length of the segment ID. The
C     length of the segment ID is determined by the following formula
C     using integer division:
C
C                                 /  ND + ( NI + 1 )  \
C                      NC =  8 *  | ----------------- |
C                                 \         2         /
C
      CALL DAFHSF ( HANDLE, ND, NI )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SGBWVS' )
         RETURN
      END IF
 
      NC = 8 * ( ND + ( NI + 1 ) / 2 )
C
C     Get the length of the segment ID. Leading blanks are considered to
C     be important. A blank segment ID is OK too.
C
      SIDLEN = LASTNB ( SEGID )
C
C     Check the segment ID to see if it is OK. Its length must be less
C     than NC and it must consist of only printing ASCII characters.
C
      IF ( SIDLEN .GT. NC )THEN
 
         CALL SETMSG ( 'Segment identifier contains more than '
     .   //            '# characters.'                           )
         CALL ERRINT ( '#', NC                                   )
         CALL SIGERR ( 'SPICE(SEGIDTOOLONG)'                     )
         CALL CHKOUT ( 'SGBWVS'                                  )
         RETURN
 
      END IF
 
      DO I = 1, SIDLEN
 
         ICH = ICHAR( SEGID(I:I) )
 
         IF ( ( ICH .LT. FPRINT ) .OR. ( ICH .GT. LPRINT ) ) THEN
 
            CALL SETMSG ( 'The segment identifier contains '
     .      //            ' a nonprinting character at location #.' )
            CALL ERRINT ( '#', I                                    )
            CALL SIGERR ( 'SPICE(NONPRINTINGCHARS)'                 )
            CALL CHKOUT ( 'SGBWVS'                                  )
            RETURN
 
         END IF
 
      END DO
C
C     Check to see if the number of constants is negative. This is all
C     we can do here, we cannot check the constant values.
C
      IF ( NCONST .LT. 0 ) THEN
 
         CALL SETMSG ( 'The number of constants specified was #. '
     .   //            'This number must be non-negative. '
     .   //            'Perhaps the variable was not '
     .   //            'initialized. '                           )
         CALL ERRINT ( '#', NCONST                               )
         CALL SIGERR ( 'SPICE(NUMCONSTANTSNEG) '                 )
         CALL CHKOUT (  'SGBWVS'                                 )
         RETURN
 
      END IF
 
C
C     Check to see if the index type is one that we recognize.
C
      IF ( ( IDXTYP .LT. MNIDXT) .OR. ( IDXTYP .GT. MXIDXT ) ) THEN
 
         CALL SETMSG ( 'The index type specified was #.  This is'
     .   //            ' not a valid index type. Valid types are'
     .   //            ' in the range from # to #.'               )
         CALL ERRINT ( '#', IDXTYP                                )
         CALL ERRINT ( '#', MNIDXT                                )
         CALL ERRINT ( '#', MXIDXT                                )
         CALL SIGERR ( 'SPICE(UNKNOWNINDEXTYPE)'                  )
         CALL CHKOUT ( 'SGBWVS'                                   )
         RETURN
 
      END IF
C
C     Check to see if there is room in the file table.
C
      IF ( NFT .EQ. FTSIZE ) THEN
 
         CALL SETMSG ( 'There are already # files being'
     .   //            ' written by generic segment writing'
     .   //            ' routines. No more files may be written'
     .   //            ' by the generic segment writers until'
     .   //            ' one of those currently being written is'
     .   //            ' closed via a call to SGWES. '            )
         CALL ERRINT ( '#', NFT                                   )
         CALL SIGERR ( 'SPICE(FILETABLEFULL)'                     )
         CALL CHKOUT ( 'SGBWVS'                                   )
         RETURN
 
      END IF
 
C
C     Set the flag which indicate whether this index type is an
C     explicit type or an implicit type.
C
      EXPLCT = ( IDXTYP .EQ. EXPLT  ) .OR. ( IDXTYP .EQ. EXPLE ) .OR.
     .         ( IDXTYP .EQ. EXPCLS )
C
C     At this point, we know that the input data is OK, in so far as we
C     can validate it and that there is room in the file table. So we
C     proceed with starting a segment for fixed size packets.
C
C     Set the flag that indicate that this segment is a variable size
C     segment.
C
      FXDSEG = .FALSE.
C
C     Get the address for the beginning of the array that we are going
C     to create. We have to get this by reading the file record.
C
      CALL DAFRFR ( HANDLE, ND, NI, DUMMY1, DUMMY2, DUMMY3, BEGADR )
C
C     Begin a new segment in the DAF file.
C
      CALL DAFBNA ( HANDLE, DESCR, SEGID )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SGBWVS' )
         RETURN
      END IF
C
C     Write out the constants to the new segment, if there are any
C     constants.
C
      IF ( NCONST .GT. 0 ) THEN
 
         CALL DAFADA ( CONST, NCONST )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SGBWVS' )
            RETURN
         END IF
 
      END IF
C
C     Save the information for this file and segment in the file table.
C
      NFT              = NFT + 1
 
      FTITYP ( NFT )   = IDXTYP
      FTPKSZ ( NFT )   = 0
      FTMXSZ ( NFT )   = 0
 
      FTNCON ( NFT )   = NCONST
      FTNPKT ( NFT )   = 0
      FTNREF ( NFT )   = 0
      FTNRES ( NFT )   = 0
 
      FTEXPL ( NFT )   = EXPLCT
 
      FTFIXD ( NFT )   = FXDSEG
 
      FTHAN  ( NFT )   = HANDLE
      FTBADR ( NFT )   = BEGADR
 
      FTREFS( 1, NFT ) = 0.0D0
      FTREFS( 2, NFT ) = 0.0D0
 
      IF ( EXPLCT ) THEN
         FTOFF(NFT) = 2
      ELSE
         FTOFF(NFT) = 1
      END IF
 
      LSTHAN = HANDLE
      INDEX  = NFT
      NUMVAR = NUMVAR + 1
 
      CALL CHKOUT ( 'SGBWVS' )
      RETURN
 
 
C$Procedure SGWFPK ( Generic segements: Write fixed size packets. )
 
      ENTRY SGWFPK ( HANDLE, NPKTS, PKTDAT, NREFS, REFDAT )
 
C$ Abstract
C
C     Write one or more fixed size data packets to the generic segment
C     currently being written to the DAF file associated with HANDLE.
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
C     DAF Required Reading.
C
C$ Keywords
C
C     GENERIC SEGMENTS
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               NPKTS
C     DOUBLE PRECISION      PKTDAT ( * )
C     INTEGER               NREFS
C     DOUBLE PRECISION      REFDAT ( * )
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C      HANDLE    I    Handle of a DAF file opened with write access.
C      NPKTS     I    Number of data packets to write to a segment.
C      PKTDAT    I    Array of packet data.
C      NREFS     I    Number of reference values.
C      REFDAT    I    Reference data.
C
C$ Detailed_Input
C
C      HANDLE   Handle of a DAF file opened with write access. This is
C               the handle of a file in which a generic segment has
C               been started and is currently being written.
C
C      NPKTS    Number of data packets to write to a segment.
C
C      PKTDAT   A singly dimensioned array containing the fixed size
C               data packets to be added to the segment associated with
C               HANDLE.
C
C               For fixed size data packets, PKTDAT will have the
C               following structure:
C
C               Packet #  Range of Locations
C               --------  ---------------------------------------------
C
C                     1   PKTDAT(1)              to PKTDAT(PS)
C                     2   PKTDAT(PS+1)           to PKTDAT(2*PS)
C                     3   PKTDAT(2*PS+1)         to PKTDAT(3*PS)
C                     4   PKTDAT(3*PS+1)         to PKTDAT(4*PS)
C
C                                          .
C                                          .
C                                          .
C
C                 NPKTS   PKTDAT((NPKTS-1)*PS+1) to PKTDAT(NPKTS*PS)
C
C               where PS = PKTSIZ.
C
C      NREFS    Number of reference values.
C
C               For implicitly indexed packets, NREFS must have a value
C               of two (2).
C
C               When writing packets to a segment which uses an implicit
C               index type, the value specified by NREFS is used only on
C               the first call to SGWFPK. On all subsequent calls to
C               these subroutines for a particular implicitly indexed
C               segment, the value of NREFS is ignored.
C
C               For explicitly indexed packets, NREFS must be equal to
C               NPKTS, i.e., there should ba a reference value for each
C               data packet being written to the segment.
C
C               When writing packets to a segment which uses an explicit
C               index type, the value specified by NREFS is used on
C               every call to SGWFPK and it must be equal to NPKTS.
C
C      REFDAT   Reference data values.
C
C               For implicitly indexed packets, there must be two (2)
C               values. The reference values represent a starting
C               reference value and a stepsize between consecutive
C               reference values, respectively.
C
C               In order to avoid, or at least minimize, numerical
C               difficulties associated with computing index values for
C               generic segments with implicit index types, the value of
C               the stepsize must be an integer, i.e., DINT(REFDAT(2))
C               must equal REFDAT(2).
C
C               When writing packets to a segment which uses an implicit
C               index type, the values specified by REFDAT are used only
C               on the first call to SGWFPK. On all subsequent calls to
C               this subroutine for a particular implicitly indexed
C               segment, REFDAT is ignored.
C
C               For explicitly indexed packets, there must be NPKTS
C               referencevalues and the values must be in increasing
C               order:
C
C                  REFDAT(I) < REFDAT(I+1), I = 1, NPKTS-1
C
C               When writing packets to a segment which uses an explicit
C               index type, the values specified by REFDAT are used on
C               every call to SGWFPK. On all calls to these subroutines
C               after the first, the value of REFDAT(1) must be greater
C               than than the value of REFDAT(NPKTS) from the previous
C               call. This preserves the ordering of the reference
C               values for the entire segment.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     This subroutine makes use of parameters defined in the file
C     'sgparam.inc'.
C
C$ Exceptions
C
C     1) If there are no generic segments with fixed packet sizes
C        currently being written, the error SPICE(CALLEDOUTOFORDER) will
C        be signalled.
C
C     2) If there is not a generic segment with fixed packet size being
C        written to the file associated with HANDLE, the error
C        SPICE(SEGMENTNOTFOUND) will be signalled.
C
C     3) If the type of generic segment being written to this file is
C        not a fixed packet size generic segment, the error
C        SPICE(SEGTYPECONFLICT) will be signalled.
C
C     4) If the number of packets to be written to the generic segment
C        is not positive, the error SPICE(NUMPACKETSNOTPOS) will be
C        signalled.
C
C     5) If an explicitly indexed generic segment is being written and
C        the number of reference values, NREFS, is not equal to the
C        number of data packets being written, NPKTS, the error
C        SPICE(INCOMPATIBLENUMREF) will be signalled.
C
C     6) If an explicitly indexed generic segment is being written and
C        the reference values are not in increasing order, the error
C        SPICE(UNORDEREDREFS) will be signalled.
C
C     7) If an explicitly indexed generic segment is being written and
C        the first reference value on the second or later additions
C        of packets to the generic segment is not greater than the last
C        reference value from the previous addition of packets, the
C        error SPICE(UNORDEREDREFS) will be signalled.
C
C     8) If an implicitly indexed generic segment is being written and
C        the number of reference values, NREFS, is not equal to two (2)
C        on the first call to this subroutine for a particular segment,
C        then the error SPICE(INCOMPATIBLENUMREF) will be signalled.
C
C     9) If an implicitly indexed generic segment is being written and
C        the second reference value, the step size used for indexing, is
C        not integral, i.e., DINT(REFDAT(2)) .NE. REFDAT(2), the error
C        SPICE(REFVALNOTINTEGER) will be signalled.
C
C$ Files
C
C     See HANDLE in the $ Detailed_Input section.
C
C$ Particulars
C
C     This routine will write one or more fixed size data packets to a
C     generic segment in the DAF file associated with HANDLE. The
C     generic segment must have been started by a call to SGBWFS.
C
C$ Examples
C
C     See the $ Examples section in the header for the main subroutine.
C     It contains examples wich demonstrate the use of the entry points
C     in the generic segments sequential writer. The entry points which
C     comprise the generic segments sequential writer must be used
C     together in the proper manner. Rather than repeating the examples
C     for each entry point they are provided in a single location.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer    (JPL)
C     W.L. Taber        (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 05-APR-1995 (KRG) (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     write fixed size packets to a generic segment
C
C-&
 
C
C     SPICELIB functions
C
C     INTEGER               LASTNB
C     INTEGER               ISRCHI
C
C     LOGICAL               FAILED
C     LOGICAL               RETURN
C
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'SGWFPK' )
C
C     Check to see if this is the first time here. If it is, we have
C     been called out of order, so signal an error.
C
      IF ( NUMFXD .EQ. 0 ) THEN
 
         CALL SETMSG ( 'No segment with fixed size packets is'
     .   //            ' currently being written. This routine'
     .   //            ' has been called out of order. The routine'
     .   //            ' SGBWFS must be called before his routine'
     .   //            ' may be called.'                            )
         CALL SIGERR ( 'SPICE(CALLEDOUTOFORDER)'                    )
         CALL CHKOUT ( 'SGWFPK'                                       )
         RETURN
 
      END IF
C
C     Check to see if the last handle used is the same as the current
C     handle. This saves us a table lookup to get the appropriate index
C     into the file table to restore the information for that handle.
C
      IF ( HANDLE .NE. LSTHAN ) THEN
 
         INDEX = ISRCHI ( HANDLE, NFT, FTHAN )
 
         IF ( INDEX .EQ. 0 ) THEN
 
            CALL SETMSG ( 'No segment with fixed size packets is'
     .      //            ' associated with the file ''#''. In'
     .      //            ' order to write fixed size packets to'
     .      //            ' a file the routine SGBWFS must be'
     .      //            ' called to begin the segment.'           )
            CALL ERRHAN ( '#', HANDLE                               )
            CALL SIGERR ( 'SPICE(SEGMENTNOTFOUND)'                  )
            CALL CHKOUT ( 'SGWFPK'                                  )
            RETURN
 
         END IF
 
         EXPLCT = FTEXPL(INDEX)
         FXDSEG = FTFIXD(INDEX)
         LSTHAN = HANDLE
 
         CALL DAFCAD ( HANDLE )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SGWFPK' )
            RETURN
         END IF
 
      END IF
C
C     Check to see if the segment being written is a fixed size packet
C     segment or a variable size packet segment. If the latter, then
C     this is the wrong routine.
C
      IF ( .NOT. FXDSEG ) THEN
 
            CALL SETMSG ( 'The segment being written to the file'
     .      //            '  ''#'' is a variable packet size'
     .      //            ' segment, not a fixed packet size'
     .      //            ' segment.  The routine SGWVPK may be used'
     .      //            ' to write variable size packets.'          )
            CALL ERRHAN ( '#', HANDLE                                 )
            CALL SIGERR ( 'SPICE(SEGTYPECONFLICT)'                    )
            CALL CHKOUT ( 'SGWFPK'                                    )
            RETURN
 
      END IF
C
C     At this point, we have a good file handle, an index into the file
C     table, and we know that we are working with a fixed packet size
C     segment. So, what we need to do now is verify the input arguments.
C
C     Check the number of packets to be sure that it is positive.
C
      IF ( NPKTS .LE. 0 ) THEN
 
         CALL SETMSG ( 'The number of packets to store is not'
     .   //            ' positive.  The value supplied was #.'
     .   //            ' Perhaps this packet count was'
     .   //            ' unitialized.'                         )
         CALL ERRINT ( '#', NPKTS                              )
         CALL SIGERR ( 'SPICE(NUMPACKETSNOTPOS)'               )
         CALL CHKOUT ( 'SGWFPK'                                )
         RETURN
 
      END IF
 
C
C     Now we get to some of the more interesting bits. We now need to
C     differentiate between the explicitly indexed types and the
C     implicitly indexed types, because they have different
C     characteristics and assumptions about how they are stored.
C
      IF ( EXPLCT ) THEN
C
C        For explicitly indexed packets the number of reference values
C        must be equal to the number of packets. The references must
C        also be in increasing order.
C
         IF ( NREFS .NE. NPKTS ) THEN
 
            CALL SETMSG ( 'The number of reference values'
     .      //            ' supplied, #, is not compatible with'
     .      //            ' explicitly indexed packets. Explicitly'
     .      //            ' indexed packets require the number of'
     .      //            ' reference values to equal the number of'
     .      //            ' packets, in this case, #.'               )
            CALL ERRINT ( '#', NREFS                                 )
            CALL ERRINT ( '#', NPKTS                                 )
            CALL SIGERR ( 'SPICE(INCOMPATIBLENUMREF)'                )
            CALL CHKOUT ( 'SGWFPK'                                   )
            RETURN
 
         END IF
C
C        If this is not the first time we have asdded data to this
C        segment, we need to be sure that all of the current reference
C        values are greater then the last reference value from the
C        previous addition of packets to the segment.
C
         IF ( FTNPKT(INDEX) .GT. 0 ) THEN
 
            IF ( FTREFS(1,INDEX) .GE. REFDAT(1) ) THEN
 
               CALL SETMSG ( 'Reference values are out of order.'
     .         //            ' The offending value, #, was found'
     .         //            ' to be out of order. The reference'
     .         //            ' values for explicitly indexed packets'
     .         //            ' must be in increasing order, and the'
     .         //            ' first reference value is less than or'
     .         //            ' equal to the last reference value, #,'
     .         //            ' from the previous addition of packets.' )
               CALL ERRDP  ( '#',  REFDAT(1)                           )
               CALL ERRDP  ( '#',  FTREFS(1,INDEX)                     )
               CALL SIGERR ( 'SPICE(UNORDEREDREFS)'                    )
               CALL CHKOUT ( 'SGWFPK'                                  )
               RETURN
 
            END IF
 
         END IF
 
         DO I = 2, NREFS
 
            IF ( REFDAT(I-1) .GE. REFDAT(I) ) THEN
 
               CALL SETMSG ( 'Reference values are out of order.'
     .         //            ' The offending value, #, was found'
     .         //            ' to be out of order for index #. The'
     .         //            ' reference values for explicitly'
     .         //            ' indexed packets must be in increasing'
     .         //            ' order.'                                )
               CALL ERRDP  ( '#',  REFDAT(I-1)                        )
               CALL ERRINT ( '#',  I-1                                )
               CALL SIGERR ( 'SPICE(UNORDEREDREFS)'                   )
               CALL CHKOUT ( 'SGWFPK'                                 )
               RETURN
 
            END IF
 
         END DO
C
C        Add the packets preceded by their reference values to the
C        segment. We put the reference values with the packets so that
C        we do not need to open a scratch file. We will use them to
C        construct a reference directory after all of the packets have
C        been added to the segment.
C
         DO I = 1, NPKTS
 
            CALL DAFADA ( REFDAT(I), 1                   )
            CALL DAFADA ( PKTDAT((I-1)*FTPKSZ(INDEX)+1),
     .                    FTPKSZ(INDEX)                  )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'SGWFPK' )
               RETURN
            END IF
 
         END DO
C
C        Save the last reference value in the file table so that we
C        can use it to verify that the next addition does not violate
C        the increasing order of the reference values.
C
         FTREFS(1,INDEX) = REFDAT(NREFS)
C
C        Update the counts for the number of packets, the number of
C        references.
C
         FTNPKT(INDEX) = FTNPKT(INDEX) + NPKTS
         FTNREF(INDEX) = FTNREF(INDEX) + NREFS
 
      ELSE
C
C        For implicitly indexed packets the number of reference values
C        must be two (2), and the second reference value must be an
C        integer, i.e., DINT(REFDAT(2)) .eq. REFDAT(2). The number of
C        reference values and the integrality of the second reference
C        value are checked only on the first call to add variable length
C        data packets to a generic segment. In all subsequent calls,
C        these arguments are ignored.
C
 
         IF ( FTNPKT (INDEX) .EQ. 0 ) THEN
 
            IF ( NREFS .NE. 2 ) THEN
 
               CALL SETMSG ( 'The number of reference values'
     .         //            ' supplied, #, is not compatible with'
     .         //            ' implicitly indexed packets. Implicitly'
     .         //            ' indexed packets require the number of'
     .         //            ' reference values to be two (2).'      )
               CALL ERRINT ( '#', NREFS                              )
               CALL SIGERR ( 'SPICE(INCOMPATIBLENUMREF)'             )
               CALL CHKOUT ( 'SGWFPK'                                )
               RETURN
 
            END IF
 
            IF ( DINT(REFDAT(2)) .NE. REFDAT(2) ) THEN
 
               CALL SETMSG ( 'For implicitly indexed packets the'
     .         //            ' step size must be an integer.'      )
               CALL SIGERR ( 'SPICE(REFVALNOTINTEGER)'             )
               CALL CHKOUT ( 'SGWFPK'                              )
               RETURN
 
            END IF
 
         END IF
C
C        Add the packets to the segment.
C
         CALL DAFADA ( PKTDAT, FTPKSZ(INDEX)*NPKTS )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SGWFPK' )
            RETURN
         END IF
C
C        Save the last reference values and the number of reference
C        values in the file table. We only do this on the first time
C        through the routine.
C
         IF ( FTNPKT(INDEX) .EQ. 0 ) THEN
 
            FTNREF(INDEX)   = NREFS
            FTREFS(1,INDEX) = REFDAT(1)
            FTREFS(2,INDEX) = REFDAT(2)
 
         END IF
C
C        Update the count for the number of packets.
C
         FTNPKT(INDEX) = FTNPKT(INDEX) + NPKTS
 
      END IF
 
      CALL CHKOUT ( 'SGWFPK' )
      RETURN
 
 
C$Procedure SGWVPK ( Generic segement: Write variable size packets. )
 
      ENTRY SGWVPK ( HANDLE, NPKTS, PKTSIZ, PKTDAT, NREFS, REFDAT )
 
C$ Abstract
C
C     Write one or more variable size data packets to the generic
C     segment currently being written to the DAF file associated with
C     HANDLE.
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
C     DAF Required Reading.
C
C$ Keywords
C
C     GENERIC SEGMENTS
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               NPKTS
C     INTEGER               PKTSIZ ( * )
C     DOUBLE PRECISION      PKTDAT ( * )
C     INTEGER               NREFS
C     DOUBLE PRECISION      REFDAT ( * )
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C      HANDLE    I    Handle of a DAF file opened with write access.
C      NPKTS     I    Number of data packets to write to a segment.
C      PKTSIZ    I    Array of sizes of variable size packets.
C      PKTDAT    I    Array of packet data.
C      NREFS     I    Number of reference values.
C      REFDAT    I    Reference data.
C
C$ Detailed_Input
C
C      HANDLE   Handle of a DAF file opened with write access. This is
C               the handle of a file in which a generic segment has
C               been started and is currently being written.
C
C      NPKTS    Number of data packets to write to a segment.
C
C      PKTSIZ   Sizes of variable size packets.
C
C               By the size of a packet we mean the number of double
C               precision numbers contained in a data packet.
C
C               When writing a segment with variable size packets,
C               there must be an element in the array PKTSIZ for each of
C               the variable size data packets.
C
C      PKTDAT   A singly dimensioned array containing the variable
C               size data packets to be added to the generic segment
C               associated with HANDLE.
C
C               For variable size data packets, PKTDAT will have the
C               following structure:
C
C               Packet #  Range of Locations
C               --------  ---------------------------------------------
C
C                     1   PKTDAT(1)           to PKTDAT(P(1))
C                     2   PKTDAT(P(1)+1)      to PKTDAT(P(2))
C                     3   PKTDAT(P(2)+1)      to PKTDAT(P(3))
C                     4   PKTDAT(P(3)+1)      to PKTDAT(P(4))
C
C                                          .
C                                          .
C                                          .
C
C                 NPKTS   PKTDAT(P(NPKTS-1)+1) to PKTDAT(P(NPKTS))
C
C                                I
C                               ---
C               where P(I) =    >   PKTSIZ(K).
C                               ---
C                              K = 1
C
C      NREFS    Number of reference values.
C
C               For implicitly indexed packets, NREFS must have a value
C               of two (2).
C
C               When writing packets to a segment which uses an implicit
C               index type, the value specified by NREFS is used only on
C               the first call to SGWVPK. On all subsequent calls to
C               these subroutines for a particular implicitly indexed
C               segment, the value of NREFS is ignored.
C
C               For explicitly indexed packets, NREFS must be equal to
C               NPKTS, i.e., there should be a reference value for each
C               data packet being written to the segment.
C
C               When writing packets to a segment which uses an explicit
C               index type, the value specified by NREFS is used on
C               every call to SGWVPK and it must be equal to NPKTS.
C
C      REFDAT   Reference data values.
C
C               For implicitly indexed packets, there must be two (2)
C               values. The reference values represent a starting
C               reference value and a stepsize between consecutive
C               reference values, respectively.
C
C               In order to avoid, or at least minimize, numerical
C               difficulties associated with computing index values for
C               generic segments with implicit index types, the value of
C               the stepsize must be an integer, i.e., DINT(REFDAT(2))
C               must equal REFDAT(2).
C
C               When writing packets to a segment which uses an implicit
C               index type, the values specified by REFDAT are used only
C               on the first call to SGWVPK. On all subsequent calls to
C               this subroutine for a particular implicitly indexed
C               segment, REFDAT is ignored.
C
C               For explicitly indexed packets, there must be NPKTS
C               reference values and the values must be in increasing
C               order:
C
C                  REFDAT(I) < REFDAT(I+1), I = 1, NPKTS-1
C
C               When writing packets to a segment which uses an explicit
C               index type, the values specified by REFDAT are used on
C               every call to SGWVPK. On all calls to this subroutine
C               after the first, the value of REFDAT(1) must be greater
C               than than the value of REFDAT(NPKTS) from the previous
C               call. This preserves the ordering of the reference
C               values for the entire segment.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     This subroutine makes use of parameters defined in the file
C     'sgparam.inc'.
C
C$ Exceptions
C
C     1) If there are no generic segments with variable packet sizes
C        currently being written, the error SPICE(CALLEDOUTOFORDER) will
C        be signalled.
C
C     2) If there is not a generic segment with variable packet size
C        being written to the file associated with HANDLE, the error
C        SPICE(SEGMENTNOTFOUND) will be signalled.
C
C     3) If the type of generic segment being written to this file is
C        not a variable packet size generic segment, the error
C        SPICE(SEGTYPECONFLICT) will be signalled.
C
C     4) If the number of packets to be written to the generic segment
C        is not positive, the error SPICE(NUMPACKETSNOTPOS) will be
C        signalled.
C
C     5) If an explicitly indexed generic segment is being written and
C        the number of reference values, NREFS, is not equal to the
C        number of data packets being written, NPKTS, the error
C        SPICE(INCOMPATIBLENUMREF) will be signalled.
C
C     6) If an explicitly indexed generic segment is being written and
C        the reference values are not in increasing order, the error
C        SPICE(UNORDEREDREFS) will be signalled.
C
C     7) If an explicitly indexed generic segment is being written and
C        the first reference value on the second or later additions
C        of packets to the generic segment is not greater than the last
C        reference value from the previous addition of packets, the
C        error SPICE(UNORDEREDREFS) will be signalled.
C
C     8) If an explicitly indexed generic segment is being written and
C        one or more of the packet sizes is not positive, the error
C        SPICE(NONPOSPACKETSIZE) will be signalled.
C
C     9) If an implicitly indexed generic segment is being written and
C        the number of reference values, NREFS, is not equal to two (2)
C        on the first call to this subroutine for a particular segment,
C        then the error SPICE(INCOMPATIBLENUMREF) will be signalled.
C
C    10) If an implicitly indexed generic segment is being written and
C        the second reference value, the step size used for indexing, is
C        not integral, i.e., DINT(REFDAT(2)) .NE. REFDAT(2), the error
C        SPICE(REFVALNOTINTEGER) will be signalled.
C
C$ Files
C
C     See HANDLE in the $ Detailed_Input section.
C
C$ Particulars
C
C     This routine will write one or more variable size data packets to
C     a generic segment in the DAF file associated with HANDLE. The
C     generic segment must have been started by a call to SGBWVS.
C
C$ Examples
C
C     See the $ Examples section in the header for the main subroutine.
C     It contains examples wich demonstrate the use of the entry points
C     in the generic segments sequential writer. The entry points which
C     comprise the generic segments sequential writer must be used
C     together in the proper manner. Rather than repeating the examples
C     for each entry point they are provided in a single location.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer    (JPL)
C     W.L. Taber        (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 05-APR-1995 (KRG) (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     write variable size packets to a generic segment
C
C-&
 
C
C     SPICELIB functions
C
C     INTEGER               LASTNB
C     INTEGER               ISRCHI
C
C     LOGICAL               FAILED
C     LOGICAL               RETURN
C
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
      CALL CHKIN  ( 'SGWVPK' )
C
C     Check to see if this is the first time here. If it is, we have
C     been called out of order, so signal an error.
C
      IF ( NUMVAR .EQ. 0 ) THEN
 
         CALL SETMSG ( 'No segment with variable size packets is'
     .   //            ' currently being written. This routine'
     .   //            ' has been called out of order. The routine'
     .   //            ' SGBWVS must be called before his routine'
     .   //            ' may be called.'                            )
         CALL SIGERR ( 'SPICE(CALLEDOUTOFORDER)'                    )
         CALL CHKOUT ( 'SGWVPK'                                     )
         RETURN
 
      END IF
C
C     Check to see if the last handle used is the same as the current
C     handle. This saves us a table lookup to get the appropriate index
C     into the file table to restore the information for that handle.
C
      IF ( HANDLE .NE. LSTHAN ) THEN
 
         INDEX = ISRCHI ( HANDLE, NFT, FTHAN )
 
         IF ( INDEX .EQ. 0 ) THEN
 
            CALL SETMSG ( 'No segment with variable size packets is'
     .      //            ' associated with the file ''#''. In'
     .      //            ' order to write variable size packets to'
     .      //            ' a file the routine SGBWVS must be'
     .      //            ' called to begin the segment.'           )
            CALL ERRHAN ( '#', HANDLE                               )
            CALL SIGERR ( 'SPICE(SEGMENTNOTFOUND)'                  )
            CALL CHKOUT ( 'SGWVPK'                                  )
            RETURN
 
         END IF
 
         EXPLCT = FTEXPL(INDEX)
         FXDSEG = FTFIXD(INDEX)
         LSTHAN = HANDLE
 
         CALL DAFCAD ( HANDLE )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SGWVPK' )
            RETURN
         END IF
 
      END IF
C
C     Check to see if the segment being written is a fixed size packet
C     segment or a variable size packet segment. If the former, then
C     this is the wrong routine.
C
      IF ( FXDSEG ) THEN
 
            CALL SETMSG ( 'The segment being written to the file'
     .      //            '  ''#'' is a fixed packet size'
     .      //            ' segment, not a variable packet size'
     .      //            ' segment.  The routine SGWFPK may be used'
     .      //            ' to write fixed size packets.'             )
            CALL ERRHAN ( '#', HANDLE                                 )
            CALL SIGERR ( 'SPICE(SEGTYPECONFLICT)'                    )
            CALL CHKOUT ( 'SGWVPK'                                    )
            RETURN
 
      END IF
C
C     At this point, we have a good file handle, an index into the file
C     table, and we know that we are working with a variable packet
C     size segment. So, what we need to do now is verify the input
C     arguments.
C
C     Check the number of packets to be sure that it is positive.
C
      IF ( NPKTS .LE. 0 ) THEN
 
         CALL SETMSG ( 'The number of packets to store is not'
     .   //            ' positive.  The value supplied was #.'
     .   //            ' Perhaps this packet count was'
     .   //            ' unitialized.'                         )
         CALL ERRINT ( '#', NPKTS                              )
         CALL SIGERR ( 'SPICE(NUMPACKETSNOTPOS)'               )
         CALL CHKOUT ( 'SGWVPK'                                )
         RETURN
 
      END IF
 
C
C     Now we get to some of the more interesting bits. We now need to
C     differentiate between the explicitly indexed types and the
C     implicitly indexed types, because they have different
C     characteristics and assumptions about how they are stored.
C
      IF ( EXPLCT ) THEN
C
C        For explicitly indexed packets the number of reference values
C        must be equal to the number of packets. The references must
C        also be in increasing order.
C
         IF ( NREFS .NE. NPKTS ) THEN
 
            CALL SETMSG ( 'The number of reference values'
     .      //            ' supplied, #, is not compatible with'
     .      //            ' explicitly indexed packets. Explicitly'
     .      //            ' indexed packets require the number of'
     .      //            ' reference values to equal the number of'
     .      //            ' packets, in this case, #.'               )
            CALL ERRINT ( '#', NREFS                                 )
            CALL ERRINT ( '#', NPKTS                                 )
            CALL SIGERR ( 'SPICE(INCOMPATIBLENUMREF)'                )
            CALL CHKOUT ( 'SGWVPK'                                   )
            RETURN
 
         END IF
C
C        If this is not the first time we have added data to this
C        segment, we need to be sure that all of the current reference
C        values are greater then the last reference value from the
C        provious addition of packets to the segment.
C
         IF ( FTNPKT(INDEX) .GT. 0 ) THEN
 
            IF ( FTREFS(1,INDEX) .GE. REFDAT(1) ) THEN
 
               CALL SETMSG ( 'Reference values are out of order.'
     .         //            ' The offending value, #, was found'
     .         //            ' The reference values for explicitly'
     .         //            ' to be out of order. indexed packets'
     .         //            ' must be in increasing order, and the'
     .         //            ' first reference value is less than or'
     .         //            ' equal to the last reference value, #,'
     .         //            ' from the previous addition of packets.' )
               CALL ERRDP  ( '#',  REFDAT(1)                           )
               CALL ERRDP  ( '#',  FTREFS(1,INDEX)                     )
               CALL SIGERR ( 'SPICE(UNORDEREDREFS)'                    )
               CALL CHKOUT ( 'SGWVPK'                                  )
               RETURN
 
            END IF
 
         END IF
 
         DO I = 2, NREFS
 
            IF ( REFDAT(I-1) .GE. REFDAT(I) ) THEN
 
               CALL SETMSG ( 'Reference values are out of order.'
     .         //            ' The offending value, #, was found'
     .         //            ' to be out of order for index #.'
     .         //            ' The reference values for explicitly'
     .         //            ' indexed packets must be in increasing'
     .         //            ' order.'                                )
               CALL ERRDP  ( '#',  REFDAT(I-1)                        )
               CALL ERRINT ( '#',  I-1                                )
               CALL SIGERR ( 'SPICE(UNORDEREDREFS)'                   )
               CALL CHKOUT ( 'SGWVPK'                                 )
               RETURN
 
            END IF
 
         END DO
C
C        Check the packet size to be sure that it is positive.
C
         DO I = 1, NPKTS
 
            IF ( PKTSIZ(I) .LE. 0 ) THEN
 
               CALL SETMSG ( 'The packet size for packet # was not'
     .         //            ' positive. It had a value of #. All'
     .         //            ' packet sizes must be greater then'
     .         //            ' zero.'                                )
               CALL ERRINT ( '#', I                                  )
               CALL ERRINT ( '#', PKTSIZ(I)                          )
               CALL SIGERR ( 'SPICE(NONPOSPACKETSIZE)'               )
               CALL CHKOUT ( 'SGWVPK'                                )
               RETURN
 
            END IF
 
         END DO
C
C        Add the packets preceded by their reference values and sizes to
C        the segment. We put the reference values with the packets so
C        that we do not need to open a scratch file. We will use them to
C        construct a reference directory after all of the packets have
C        been added to the segment.
C
         PKTPOS = 1
 
         DO I = 1, NPKTS
 
            DPKSIZ = DBLE( PKTSIZ(I) )
 
            CALL DAFADA ( REFDAT(I), 1              )
            CALL DAFADA ( DPKSIZ,    1              )
            CALL DAFADA ( PKTDAT(PKTPOS), PKTSIZ(I) )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'SGWVPK' )
               RETURN
            END IF
 
            PKTPOS        = PKTPOS        + PKTSIZ(I)
            FTPKSZ(INDEX) = FTPKSZ(INDEX) + PKTSIZ(I)
C
C           Remember the maximum packet size encountered.
C
            IF ( PKTSIZ(I) .GT. FTMXSZ(INDEX) ) THEN
 
               FTMXSZ(INDEX) = PKTSIZ(I)
 
            END IF
 
         END DO
C
C        Save the last reference value in the file table so that we
C        can use it to verify that the next addition does not violate
C        the increasing order of the reference values.
C
         FTREFS(1,INDEX) = REFDAT(NREFS)
C
C        Update the counts for the number of packets, the number of
C        references.
C
         FTNPKT(INDEX) = FTNPKT(INDEX) + NPKTS
         FTNREF(INDEX) = FTNREF(INDEX) + NREFS
 
      ELSE
C
C        For implicitly indexed packets the number of reference values
C        must be two (2), and the second reference value must be an
C        integer, i.e., DINT(REFDAT(2)) .eq. REFDAT(2). The number of
C        reference values and the integrality of the second reference
C        value are checked only on the first call to add variable length
C        data packets to a generic segment. In all subsequent calls,
C        these arguments are ignored.
C
         IF ( FTNPKT(INDEX) .EQ. 0 ) THEN
 
            IF ( NREFS .NE. 2 ) THEN
 
               CALL SETMSG ( 'The number of reference values'
     .         //            ' supplied, #, is not compatible with'
     .         //            ' implicitly indexed packets. Implicitly'
     .         //            ' indexed packets require the number of'
     .         //            ' reference values to be two (2).'        )
               CALL ERRINT ( '#', NREFS                                )
               CALL SIGERR ( 'SPICE(INCOMPATIBLENUMREF)'               )
               CALL CHKOUT ( 'SGWVPK'                                  )
               RETURN
 
            END IF
 
            IF ( DINT(REFDAT(2)) .NE. REFDAT(2) ) THEN
 
               CALL SETMSG ( 'For implicitly indexed packets the'
     .         //            ' step size must be an integer.'      )
               CALL SIGERR ( 'SPICE(REFVALNOTINTEGER)'             )
               CALL CHKOUT ( 'SGWVPK'                              )
               RETURN
 
            END IF
 
         END IF
 
C
C        Add the packets to the segment preceded by the size of the
C        packet.
C
         PKTPOS = 1
 
         DO I = 1, NPKTS
 
            DPKSIZ = DBLE ( PKTSIZ(I) )
 
            CALL DAFADA ( DPKSIZ, 1                 )
            CALL DAFADA ( PKTDAT(PKTPOS), PKTSIZ(I) )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'SGWVPK' )
               RETURN
            END IF
 
            PKTPOS        = PKTPOS        + PKTSIZ(I)
            FTPKSZ(INDEX) = FTPKSZ(INDEX) + PKTSIZ(I)
 
         END DO
C
C        Save the reference values and the number of reference values
C        in the file table. We only do this on the first time through
C        the routine.
C
         IF ( FTNPKT(INDEX) .EQ. 0 ) THEN
 
            FTNREF(INDEX)   = NREFS
            FTREFS(1,INDEX) = REFDAT(1)
            FTREFS(2,INDEX) = REFDAT(2)
 
         END IF
C
C        Update the counts for the number of packets.
C
         FTNPKT(INDEX) = FTNPKT(INDEX) + NPKTS
 
      END IF
 
      CALL CHKOUT ( 'SGWVPK' )
      RETURN
 
 
C$Procedure SGWES ( Generic segements: End a segment. )
 
      ENTRY SGWES ( HANDLE )
 
C$ Abstract
C
C     End the generic segment in the DAF file associated with HANDLE.
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
C     DAF Required Reading.
C
C$ Keywords
C
C     GENERIC SEGMENTS
C
C$ Declarations
C
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C      HANDLE    I    Handle of a DAF file opened with write access.
C
C$ Detailed_Input
C
C      HANDLE   Handle of a DAF file opened with write access. This is
C               the handle of the file which contains the generic
C               segment that we wish to end.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     This subroutine makes use of parameters defined in the file
C     'sgparam.inc'.
C
C$ Exceptions
C
C     1) If there are no generic segments currently being written, the
C        error SPICE(CALLEDOUTOFORDER) will be signalled.
C
C     2) If there is no generic segment being written to the file
C        associated with HANDLE, the error SPICE(SEGMENTNOTFOUND) will
C        be signalled.
C
C$ Files
C
C     See HANDLE in the $ Detailed_Input section.
C
C$ Particulars
C
C     This routine will end the generic segment started by a call to
C     either SGBWFS or SGBWVS that is currently being written to the DAF
C     file associated with HANDLE.
C
C$ Examples
C
C     See the $ Examples section in the header for the main subroutine.
C     It contains examples wich demonstrate the use of the entry points
C     in the generic segments sequential writer. The entry points which
C     comprise the generic segments sequential writer must be used
C     together in the proper manner. Rather than repeating the examples
C     for each entry point they are provided in a single location.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer    (JPL)
C     W.L. Taber        (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 30-JUL-1996 (KRG) (NJB)
C
C        Fixed an annoying little bug in the variable segments code
C        when ending a segment. Rather than storing an appropriate
C        offset from the beginning of the segment as the packet
C        address in the packet directory, the absolute address, the
C        DAF address, was stored. This bug has been fixed.
C
C        The address calculations, see the variable MYADDR, were fixed.
C        This involved initializing the variable outside of the loop
C        that scans throught the packet data and then incrementing this
C        variable in the same way as PKTADR.
C
C        The changes were made in two places, for the explicitly indexed
C        case and for the implicitly indexed case.
C
C-    SPICELIB Version 1.0.0, 05-APR-1995 (KRG) (WLT)
C
C-&
 
C$ Index_Entries
C
C     end a generic segment
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 30-JUL-1996 (KRG) (NJB)
C
C        Fixed an annoying little bug in the variable segments code
C        when ending a segment. Rather than storing an appropriate
C        offset from the beginning of the segment as the packet
C        address in the packet directory, the absolute address, the
C        DAF address, was stored. This bug has been fixed.
C
C        The address calculations, see the variable MYADDR, were fixed.
C        This involved initializing the variable outside of the loop
C        that scans throught the packet data and then incrementing this
C        variable in the same way as PKTADR.
C
C        The changes were made in two places, for the explicitly indexed
C        case and for the implicitly indexed case.
C
C-&
 
C
C     SPICELIB functions
C
C     INTEGER               LASTNB
C     INTEGER               ISRCHI
C
C     LOGICAL               FAILED
C     LOGICAL               RETURN
C
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'SGWES' )
C
C     Check to see if we have any fixed or variable segments being
C     written.
C
      IF ( NFT .EQ. 0 ) THEN
 
         CALL SETMSG ( 'No segment is currently being written.'
     .   //            ' This routine has been called out of'
     .   //            ' order. One of the routines SGBWFS or'
     .   //            ' SGBWVS must be called before his routine'
     .   //            ' may be called.'                            )
         CALL SIGERR ( 'SPICE(CALLEDOUTOFORDER)'                    )
         CALL CHKOUT ( 'SGWES'                                      )
         RETURN
 
      END IF
C
C     Check to see if the last handle used is the same as the current
C     handle. This saves us a table lookup to get the appropriate index
C     into the file table to restore the information for that handle.
C
      IF ( HANDLE .NE. LSTHAN ) THEN
 
         INDEX = ISRCHI ( HANDLE, NFT, FTHAN )
 
         IF ( INDEX .EQ. 0 ) THEN
 
            CALL SETMSG ( 'No segment is associated with the file'
     .      //            ' ''#''. In order to write packets to a'
     .      //            ' segment one of the routines SGBWFS or'
     .      //            ' SGBWVS must be called to begin a'
     .      //            ' segment.'                               )
            CALL ERRHAN ( '#', HANDLE                               )
            CALL SIGERR ( 'SPICE(SEGMENTNOTFOUND)'                  )
            CALL CHKOUT ( 'SGWES'                                   )
            RETURN
 
         END IF
 
         EXPLCT = FTEXPL(INDEX)
         FXDSEG = FTFIXD(INDEX)
         LSTHAN = HANDLE
 
         CALL DAFCAD ( HANDLE )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SGWES' )
            RETURN
         END IF
 
      END IF
C
C     We need to do different things depending on whether the reference
C     values are implicitly or explicitly defined. We will also need to
C     treat the cases of fixed size packets and variable size packets
C     differently.
C
      IF ( EXPLCT ) THEN
C
C        We have an explicit segment.
C
         IF ( FXDSEG ) THEN
C
C           We need to do a little bit of work to finish this case off.
C           We know that we do not need a list of packet starting
C           addresses or a packet directory, but we do need to store in
C           a contiguous block the references and a reference directory
C           if the number of references is greater than DIRSIZ.
C
C           We need to do the following things:
C
C           1) Initialize the offset of the packet data from the
C              beginning of the packet, set the size of the packet, and
C              set the beginning address of the packet data area in the
C              segment.
C
            SIZE   = FTOFF(INDEX)  + FTPKSZ(INDEX)
            REFADR = FTBADR(INDEX) + FTNCON(INDEX)
C
C           2) Collect all of the references stored with the packets
C              when they were written, and copy them into the
C              reference area.
C
            DO I  = 1, FTNPKT(INDEX)
 
               CALL DAFGDA ( HANDLE, REFADR, REFADR, MYREF )
               CALL DAFADA ( MYREF, 1 )
 
               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'SGWES' )
                  RETURN
               END IF
 
               REFADR = REFADR + SIZE
 
            END DO
C
C           3) Create a reference directory if the number of
C              references is greater than DIRSIZ.
C
            IF ( FTNREF(INDEX) .GT. DIRSIZ ) THEN
 
               REFADR = FTBADR(INDEX) + FTNCON(INDEX)
               REFADR = REFADR + FTNPKT(INDEX) * SIZE + DIRSIZ - 1
 
               DO I = 1, (FTNREF(INDEX)-1)/DIRSIZ
 
                  CALL DAFGDA ( HANDLE, REFADR, REFADR, MYREF )
                  CALL DAFADA ( MYREF, 1 )
 
                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( 'SGWES' )
                     RETURN
                  END IF
 
                  REFADR = REFADR + DIRSIZ
 
               END DO
 
            END IF
C
C           4) Construct the meta data for the segment.
C
            SIZE = ( FTOFF(INDEX) + FTPKSZ(INDEX) ) * FTNPKT(INDEX)
 
            META ( CONBAS ) = 0
            META ( NCON   ) = FTNCON(INDEX)
            META ( PKTBAS ) = META(CONBAS) + META(NCON)
            META ( NPKT   ) = FTNPKT(INDEX)
            META ( PKTOFF ) = FTOFF(INDEX)
            META ( PDRBAS ) = 0
            META ( NPDR   ) = 0
            META ( PDRTYP ) = 0
            META ( REFBAS ) = META(PKTBAS) + SIZE
            META ( NREF   ) = FTNREF(INDEX)
            META ( RDRBAS ) = META(REFBAS) + META(NREF)
            META ( NRDR   ) = (FTNREF(INDEX)-1)/DIRSIZ
            META ( RDRTYP ) = FTITYP(INDEX)
            META ( RSVBAS ) = 0
            META ( NRSV   ) = 0
            META ( PKTSZ  ) = FTPKSZ(INDEX)
            META ( NMETA  ) = MXMETA
 
         ELSE
C
C           We need to do a little bit of work to finish this case off.
C           We know that we need a packet directory and we need to store
C           in a contiguous block the references and a reference
C           directory if the number of references is greater than
C           DIRSIZ.
C
C           We need to do the following things:
C
C           1) Set the beginning address of the packet data area in the
C              segment and initialize the address of the first data
C              packet.
C
            PKTADR = FTBADR(INDEX) + FTNCON(INDEX) + FTOFF(INDEX)
            MYADDR = DBLE ( FTOFF(INDEX) + 1 )
C
C           2) Create a packet directory. The packet directory consists
C              of the beginning addresses for each of the packets and a
C              fake beginning for an extra packet so that we can easily
C              compute the size of the last packet.
C
            DO I  = 1, FTNPKT(INDEX)
 
 
               CALL DAFGDA ( HANDLE, PKTADR-1, PKTADR-1, MYSIZE )
               CALL DAFADA ( MYADDR, 1                          )
 
               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'SGWES' )
                  RETURN
               END IF
 
               SIZE   = INT ( MYSIZE )
               PKTADR = PKTADR +        SIZE + FTOFF(INDEX)
               MYADDR = MYADDR + DBLE ( SIZE + FTOFF(INDEX) )
 
            END DO
C
C           Put in the fake beginning for an extra packet. PKTADR should
C           contain the proper value.
C
            MYADDR = DBLE ( MYADDR )
 
            CALL DAFADA ( MYADDR, 1 )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'SGWES' )
               RETURN
            END IF
 
C
C           3) Collect all of the references, stored with the packets
C              when they were written, and copy them into the
C              reference area.
C
            REFADR = FTBADR(INDEX) + FTNCON(INDEX)
 
            DO I  = 1, FTNPKT(INDEX)
 
               CALL DAFGDA ( HANDLE, REFADR,   REFADR,   MYREF  )
               CALL DAFGDA ( HANDLE, REFADR+1, REFADR+1, MYSIZE )
               CALL DAFADA ( MYREF,  1                          )
 
               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'SGWES' )
                  RETURN
               END IF
 
               SIZE   = INT ( MYSIZE )
               REFADR = REFADR + SIZE + FTOFF(INDEX)
 
            END DO
C
C           3) Create a reference directory if the number of
C              references is greater than DIRSIZ. Note that we have one
C              more packet directory item than we have data packets.
C              This allows us to compute the size of the last data
C              packet.
C
            IF ( FTNREF(INDEX) .GT. DIRSIZ ) THEN
 
               REFADR = FTBADR(INDEX) + FTNCON(INDEX)
               REFADR = REFADR        + FTPKSZ(INDEX)
               REFADR = REFADR        + FTOFF(INDEX) * FTNPKT(INDEX)
               REFADR = REFADR        + FTNPKT(INDEX) + 1
               REFADR = REFADR        + DIRSIZ - 1
 
               DO I = 1, (FTNREF(INDEX)-1)/DIRSIZ
 
                  CALL DAFGDA ( HANDLE, REFADR, REFADR, MYREF )
                  CALL DAFADA ( MYREF,  1                     )
 
                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( 'SGWES' )
                     RETURN
                  END IF
 
                  REFADR = REFADR + DIRSIZ
 
               END DO
 
            END IF
C
C           4) Construct the meta data for the segment.
C
            META ( CONBAS ) = 0
            META ( NCON   ) = FTNCON(INDEX)
            META ( PKTBAS ) = META(CONBAS) + META(NCON)
            META ( NPKT   ) = FTNPKT(INDEX)
            META ( PKTOFF ) = FTOFF(INDEX)
            META ( PDRBAS ) = META(PKTBAS) + FTPKSZ(INDEX)
     .                                     + FTOFF(INDEX)*FTNPKT(INDEX)
            META ( NPDR   ) = FTNPKT(INDEX) + 1
            META ( PDRTYP ) = 1
            META ( REFBAS ) = META(PDRBAS) + META(NPDR)
            META ( NREF   ) = FTNREF(INDEX)
            META ( RDRBAS ) = META(REFBAS) + META(NREF)
            META ( NRDR   ) = (FTNREF(INDEX)-1)/DIRSIZ
            META ( RDRTYP ) = FTITYP(INDEX)
            META ( RSVBAS ) = 0
            META ( NRSV   ) = 0
            META ( PKTSZ  ) = FTMXSZ(INDEX)
            META ( NMETA  ) = MXMETA
 
         END IF
 
      ELSE
C
C        We have an implicitly indexed segment.
C
         IF ( FXDSEG ) THEN
C
C           There is no packet directory, so we just write the reference
C           values. There is no reference directory either, because
C           implicitly indexed packets only have two (2) reference
C           values.
C
            CALL DAFADA ( FTREFS(1,INDEX), FTNREF(INDEX) )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'SGWES' )
               RETURN
            END IF
C
C           Now we need to construct the meta data for this segment. We
C           will write it to the file a bit later.
C
            SIZE = ( FTOFF(INDEX) + FTPKSZ(INDEX) ) * FTNPKT(INDEX)
 
            META ( CONBAS ) = 0
            META ( NCON   ) = FTNCON(INDEX)
            META ( PKTBAS ) = META(CONBAS) + META(NCON)
            META ( NPKT   ) = FTNPKT(INDEX)
            META ( PKTOFF ) = FTOFF(INDEX)
            META ( PDRBAS ) = 0
            META ( NPDR   ) = 0
            META ( PDRTYP ) = 0
            META ( REFBAS ) = META(PKTBAS) + SIZE
            META ( NREF   ) = FTNREF(INDEX)
            META ( RDRBAS ) = META(REFBAS) + META(NREF)
            META ( NRDR   ) = 0
            META ( RDRTYP ) = FTITYP(INDEX)
            META ( RSVBAS ) = 0
            META ( NRSV   ) = 0
            META ( PKTSZ  ) = FTPKSZ(INDEX)
            META ( NMETA  ) = MXMETA
 
         ELSE
C
C           We need to do a little bit of work to finish this case off.
C           We know that we need a packet directory, but we do not need
C           a reference directory.
C
C           We need to do the following things:
C
C           1) Set the beginning address of the packet data area in the
C              segment and initialize the address of the first data
C              packet.
C
            PKTADR = FTBADR(INDEX) + FTNCON(INDEX) + FTOFF(INDEX)
            MYADDR = DBLE ( FTOFF(INDEX) + 1 )
C
C           2) Create a packet directory. The packet directory consists
C              of the beginning addresses for each of the packets and a
C              fake beginning for an extra packet so that we can easily
C              compute the size of the last packet.
C
            DO I  = 1, FTNPKT(INDEX)
 
               CALL DAFGDA ( HANDLE, PKTADR-1, PKTADR-1, MYSIZE )
               CALL DAFADA ( MYADDR, 1                          )
 
               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'SGWES' )
                  RETURN
               END IF
 
               SIZE   = INT ( MYSIZE )
               PKTADR = PKTADR +        SIZE + FTOFF(INDEX)
               MYADDR = MYADDR + DBLE ( SIZE + FTOFF(INDEX) )
 
            END DO
C
C           Put in the fake beginning for an extra packet. PKTADR should
C           contain the proper value.
C
            MYADDR = DBLE ( PKTADR - FTBADR(INDEX) )
 
            CALL DAFADA ( MYADDR, 1 )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'SGWES' )
               RETURN
            END IF
C
C           3) Construct the meta data for the segment.
C
            META ( CONBAS ) = 0
            META ( NCON   ) = FTNCON(INDEX)
            META ( PKTBAS ) = META(CONBAS) + META(NCON)
            META ( NPKT   ) = FTNPKT(INDEX)
            META ( PKTOFF ) = FTOFF(INDEX)
            META ( PDRBAS ) = META(PKTBAS) + FTPKSZ(INDEX)
     .                                     + FTOFF(INDEX)*FTNPKT(INDEX)
            META ( NPDR   ) = FTNPKT(INDEX) + 1
            META ( PDRTYP ) = 1
            META ( REFBAS ) = META(PDRBAS) + META(NPDR)
            META ( NREF   ) = FTNREF(INDEX)
            META ( RDRBAS ) = META(REFBAS) + META(NREF)
            META ( NRDR   ) = 0
            META ( RDRTYP ) = FTITYP(INDEX)
            META ( RSVBAS ) = 0
            META ( NRSV   ) = 0
            META ( PKTSZ  ) = FTMXSZ(INDEX)
            META ( NMETA  ) = MXMETA
 
         END IF
 
      END IF
C
C     Write the meta data to the segment and end the segment.
C
      DO I = 1, MXMETA
         XMETA(I) = DBLE ( META(I) )
      END DO
 
      CALL DAFADA ( XMETA, MXMETA )
C
C     End the segment.
C
      CALL DAFENA
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SGWES' )
         RETURN
      END IF
C
C     Now we need to clean up after ourselves, removing the information
C     for the segment we just ended from the file table.
C
      NFT = NFT - 1
 
      DO I = INDEX, NFT
         FTBADR ( I ) = FTBADR ( I+1 )
         FTHAN  ( I ) = FTHAN  ( I+1 )
         FTITYP ( I ) = FTITYP ( I+1 )
         FTNCON ( I ) = FTNCON ( I+1 )
         FTNPKT ( I ) = FTNPKT ( I+1 )
         FTNREF ( I ) = FTNREF ( I+1 )
         FTNRES ( I ) = FTNRES ( I+1 )
         FTOFF  ( I ) = FTOFF  ( I+1 )
         FTPKSZ ( I ) = FTPKSZ ( I+1 )
         FTFIXD ( I ) = FTFIXD ( I+1 )
         FTEXPL ( I ) = FTEXPL ( I+1 )
      END DO
 
      IF ( FXDSEG ) THEN
         NUMFXD = NUMFXD - 1
      ELSE
         NUMVAR = NUMVAR - 1
      END IF
 
      CALL CHKOUT ( 'SGWES' )
      RETURN
 
      END
