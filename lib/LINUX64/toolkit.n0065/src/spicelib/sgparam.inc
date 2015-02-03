C
C$ Abstract
C
C     Parameter declarations for the generic segments subroutines.
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
C      DAF Required Reading
C
C$ Keywords
C
C       GENERIC SEGMENTS
C
C$ Particulars
C
C     This include file contains the parameters used by the generic
C     segments subroutines, SGxxxx. A generic segment is a
C     generalization of a DAF array which places a particular structure
C     on the data contained in the array, as described below.
C
C     This file defines the mnemonics that are used for the index types
C     allowed in generic segments as well as mnemonics for the meta data
C     items which are used to describe a generic segment.
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
C$ Restrictions
C
C     1) If new reference index types are added, the new type(s) should
C        be defined to be the consecutive integer(s) after the last
C        defined reference index type used. In this way a value for
C        the maximum allowed index type may be maintained. This value
C        must also be updated if new reference index types are added.
C
C     2) If new meta data items are needed, mnemonics for them must be
C        added to the end of the current list of mnemonics and before
C        the NMETA mnemonic. In this way compatibility with files having
C        a different, but smaller, number of meta data items may be
C        maintained. See the description and example below.
C
C$ Author_and_Institution
C
C     N.J. Bachman      (JPL)
C     K.R. Gehringer    (JPL)
C     W.L. Taber        (JPL)
C     F.S. Turner       (JPL)
C
C$ Literature_References
C
C     Generic Segments Required Reading.
C     DAF Required Reading.
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 28-JAN-2004 (NJB)
C
C        Header update: equations for comptutations of packet indices
C        for the cases of index types 0 and 1 were corrected.
C
C-    SPICELIB Version 1.1.0, 25-09-98 (FST)
C
C        Added parameter MNMETA, the minimum number of meta data items
C        that must be present in a generic DAF segment.
C
C-    SPICELIB Version 1.0.0, 04-03-95 (KRG) (WLT)
C
C-&
 
C
C     Mnemonics for the type of reference value index.
C
C     Two forms of indexing are provided:
C
C        1) An implicit form of indexing based on using two values, a
C           starting value, which will have an index of 1, and a step
C           size between reference values, which are used to compute an
C           index and a reference value associated with a specified key
C           value. See the descriptions of the implicit types below for
C           the particular formula used in each case.
C
C        2) An explicit form of indexing based on a reference value for
C           each data packet.
C
C
C     Reference Index Type 0
C     ----------------------
C
C     Implied index. The index and reference value of a data packet
C     associated with a specified key value are computed from the two
C     generic segment reference values using the formula below. The two
C     generic segment reference values, REF(1) and REF(2), represent,
C     respectively, a starting value and a step size between reference
C     values. The index of the data packet associated with a key value
C     of VALUE is given by:
C
C                          /    VALUE - REF(1)    \
C        INDEX = 1  +  INT | -------------------- |
C                          \        REF(2)        /
C
C     and the reference value associated with VALUE is given by:
C
C        REFVAL = REF(1) + DBLE (INDEX-1) * REF(2)
C
      INTEGER               IMPLE
      PARAMETER           ( IMPLE  = 0 )
 
C
C     Reference Index Type 1
C     ----------------------
C
C     Implied index. The index and reference value of a data packet
C     associated with a specified key value are computed from the two
C     generic segment reference values using the formula below. The two
C     generic segment reference values, REF(1) and REF(2), represent,
C     respectively, a starting value and a step size between reference
C     values. The index of the data packet associated with a key value
C     of VALUE is given by:
C
C                          /          VALUE - REF(1)    \
C        INDEX = 1  +  INT | 0.5 + -------------------- |
C                          \              REF(2)        /
C
C
C     and the reference value associated with VALUE is given by:
C
C        REFVAL = REF(1) + DBLE (INDEX-1) * REF(2)
C
C     We get the larger index in the event that VALUE is halfway between
C     X(I) and X(I+1), where X(I) = BUFFER(1) + DBLE (I-1) * REFDAT(2).
C
      INTEGER               IMPCLS
      PARAMETER           ( IMPCLS = 1 )
 
C
C     Reference Index Type 2
C     ----------------------
C
C     Explicit index. In this case the number of packets must equal the
C     number of reference values. The index of the packet associated
C     with a key value of VALUE is the index of the last reference item
C     that is strictly less than VALUE. The reference values must be in
C     ascending order, REF(I) < REF(I+1).
C
      INTEGER               EXPLT
      PARAMETER           ( EXPLT  = 2 )
 
C
C     Reference Index Type 3
C     ----------------------
C
C     Explicit index. In this case the number of packets must equal the
C     number of reference values. The index of the packet associated
C     with a key value of VALUE is the index of the last reference item
C     that is less than or equal to VALUE. The reference values must be
C     in ascending order, REF(I) < REF(I+1).
C
      INTEGER               EXPLE
      PARAMETER           ( EXPLE  = 3 )
 
C
C     Reference Index Type 4
C     ----------------------
C
C     Explicit index. In this case the number of packets must equal the
C     number of reference values. The index of the packet associated
C     with a key value of VALUE is the index of the reference item
C     that is closest to the value of VALUE. In the event of a "tie"
C     the larger index is selected. The reference values must be in
C     ascending order, REF(I) < REF(I+1).
C
      INTEGER               EXPCLS
      PARAMETER           ( EXPCLS = 4 )
 
C
C     These parameters define the valid range for the index types. An
C     index type code, MYTYPE, for a generic segment must satisfy the
C     relation MNIDXT <= MYTYPE <= MXIDXT.
C
      INTEGER               MNIDXT
      PARAMETER           ( MNIDXT = 0 )
 
      INTEGER               MXIDXT
      PARAMETER           ( MXIDXT = 4 )
 
C
C     The following meta data items will appear in all generic segments.
C     Other meta data items may be added if a need arises.
C
C       1)  CONBAS  Base Address of the constants in a generic segment.
C
C       2)  NCON    Number of constants in a generic segment.
C
C       3)  RDRBAS  Base Address of the reference directory for a
C                   generic segment.
C
C       4)  NRDR    Number of items in the reference directory of a
C                   generic segment.
C
C       5)  RDRTYP  Type of the reference directory 0, 1, 2 ... for a
C                   generic segment.
C
C       6)  REFBAS  Base Address of the reference items for a generic
C                   segment.
C
C       7)  NREF    Number of reference items in a generic segment.
C
C       8)  PDRBAS  Base Address of the Packet Directory for a generic
C                   segment.
C
C       9)  NPDR    Number of items in the Packet Directory of a generic
C                   segment.
C
C      10)  PDRTYP  Type of the packet directory 0, 1, ... for a generic
C                   segment.
C
C      11)  PKTBAS  Base Address of the Packets for a generic segment.
C
C      12)  NPKT    Number of Packets in a generic segment.
C
C      13)  RSVBAS  Base Address of the Reserved Area in a generic
C                   segment.
C
C      14)  NRSV    Number of items in the reserved area of a generic
C                   segment.
C
C      15)  PKTSZ   Size of the packets for a segment with fixed width
C                   data packets or the size of the largest packet for a
C                   segment with variable width data packets.
C
C      16)  PKTOFF  Offset of the packet data from the start of a packet
C                   record. Each data packet is placed into a packet
C                   record which may have some bookkeeping information
C                   prepended to the data for use by the generic
C                   segments software.
C
C      17)  NMETA   Number of meta data items in a generic segment.
C
C     Meta Data Item  1
C     -----------------
C
      INTEGER               CONBAS
      PARAMETER           ( CONBAS =  1         )
C
C     Meta Data Item  2
C     -----------------
C
 
      INTEGER               NCON
      PARAMETER           ( NCON   = CONBAS + 1 )
C
C     Meta Data Item  3
C     -----------------
C
      INTEGER               RDRBAS
      PARAMETER           ( RDRBAS = NCON   + 1 )
C
C     Meta Data Item  4
C     -----------------
C
      INTEGER               NRDR
      PARAMETER           ( NRDR   = RDRBAS + 1 )
C
C     Meta Data Item  5
C     -----------------
C
      INTEGER               RDRTYP
      PARAMETER           ( RDRTYP = NRDR   + 1 )
C
C     Meta Data Item  6
C     -----------------
C
      INTEGER               REFBAS
      PARAMETER           ( REFBAS = RDRTYP + 1 )
C
C     Meta Data Item  7
C     -----------------
C
      INTEGER               NREF
      PARAMETER           ( NREF   = REFBAS + 1 )
C
C     Meta Data Item  8
C     -----------------
C
      INTEGER               PDRBAS
      PARAMETER           ( PDRBAS = NREF   + 1 )
C
C     Meta Data Item  9
C     -----------------
C
      INTEGER               NPDR
      PARAMETER           ( NPDR   = PDRBAS + 1 )
C
C     Meta Data Item 10
C     -----------------
C
      INTEGER               PDRTYP
      PARAMETER           ( PDRTYP = NPDR   + 1 )
C
C     Meta Data Item 11
C     -----------------
C
      INTEGER               PKTBAS
      PARAMETER           ( PKTBAS = PDRTYP + 1 )
C
C     Meta Data Item 12
C     -----------------
C
      INTEGER               NPKT
      PARAMETER           ( NPKT   = PKTBAS + 1 )
C
C     Meta Data Item 13
C     -----------------
C
      INTEGER               RSVBAS
      PARAMETER           ( RSVBAS = NPKT   + 1 )
C
C     Meta Data Item 14
C     -----------------
C
      INTEGER               NRSV
      PARAMETER           ( NRSV   = RSVBAS + 1 )
C
C     Meta Data Item 15
C     -----------------
C
      INTEGER               PKTSZ
      PARAMETER           ( PKTSZ  = NRSV   + 1 )
C
C     Meta Data Item 16
C     -----------------
C
      INTEGER               PKTOFF
      PARAMETER           ( PKTOFF = PKTSZ  + 1 )
C
C     If new meta data items are to be added to this list, they should
C     be added above this comment block as described below.
C
C        INTEGER               NEW1
C        PARAMETER           ( NEW1   = PKTOFF + 1 )
C
C        INTEGER               NEW2
C        PARAMETER           ( NEW2   = NEW1   + 1 )
C
C        INTEGER               NEWEST
C        PARAMETER           ( NEWEST = NEW2   + 1 )
C
C     and then the value of NMETA must be changed as well to be:
C
C        INTEGER               NMETA
C        PARAMETER           ( NMETA  = NEWEST + 1 )
C
C     Meta Data Item 17
C     -----------------
C
      INTEGER               NMETA
      PARAMETER           ( NMETA  = PKTOFF + 1 )
C
C     Maximum number of meta data items. This is always set equal to
C     NMETA.
C
      INTEGER               MXMETA
      PARAMETER           ( MXMETA = NMETA )
C
C     Minimum number of meta data items that must be present in a DAF
C     generic segment.  This number is to remain fixed even if more
C     meta data items are added for compatibility with old DAF files.
C
      INTEGER               MNMETA
      PARAMETER           ( MNMETA = 15 )
