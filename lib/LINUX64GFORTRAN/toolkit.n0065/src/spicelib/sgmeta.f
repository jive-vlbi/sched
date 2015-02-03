C$Procedure      SGMETA ( Generic segments: Fetch meta data value )
 
      SUBROUTINE SGMETA ( HANDLE, DESCR, MNEMON, VALUE )
 
C$ Abstract
C
C     Obtain the value of a specified generic segment meta data item.
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
C$ Declarations
 
      IMPLICIT NONE
 
      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR  ( * )
      INTEGER               MNEMON
      INTEGER               VALUE
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      HANDLE     I   Handle of a DAF open for reading.
C      DESCR      I   Descriptor for a generic segment in the DAF.
C      MNEMON     I   An integer mnemonic for the desired meta data.
C      VALUE      O   The value of the meta data item requested.
C
C$ Detailed_Input
C
C     HANDLE     is the handle of a DAF opened for reading that
C                contains the generic segment described by DESCR.
C
C     DESCR      is the descriptor of a generic segment. This must
C                be the descriptor for a generic segment in the DAF
C                associated with HANDLE.
C
C     MNEMON     is the mnemonic used to represent the desired piece of
C                meta data. See the file 'sgparam.inc' for details, the
C                mnemonics, and their values.
C
C$ Detailed_Output
C
C     VALUE      is the value of the meta data item associated with
C                the mnemonic MNEMON that is in the generic segment
C                specified by HANDLE and DESCR.
C
C$ Parameters
C
C     This subroutine makes use of parameters defined in the file
C     'sgparam.inc'.
C
C$ Files
C
C      See the description of HANDLE above.
C
C$ Exceptions
C
C     1) If the mnemonic for the meta data item is not valid, the error
C        SPICE(UNKNOWNMETAITEM) will be signalled.
C
C     2) If the last address in the DAF segment that reports the number
C        of meta data items that exist in the segment is less than
C        MNMETA, the error SPICE(INVALIDMETADATA) will be signaled.
C
C$ Particulars
C
C     This routine is a utility for fetching the meta data associated
C     with a DAF generic segment.
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
C     Only the placement of the meta data at the end of a segment is
C     required. The other data partitions may occur in any order in the
C     segment because the meta data will contain pointers to the
C     appropriate locations of the other data partitions within the
C     segment.
C
C     The meta data for the segment should be obtained only through
C     use of this routine, SGMETA.
C
C$ Examples
C
C     Suppose that we would like to know how many constants, data
C     packets, and reference values are in the generic segment that we
C     have located in the DAF file associated with HANDLE.
C
C     C
C     C     Get the number of constants.
C     C
C           CALL SGMETA ( HANDLE, DESCR, NCON, NCONST )
C     C
C     C     Get the number of data packets.
C     C
C           CALL SGMETA ( HANDLE, DESCR, NPKT, NPKTS )
C     C
C     C     Get the number of constants.
C     C
C           CALL SGMETA ( HANDLE, DESCR, NREF, NREFS )
C
C     C
C     C     Print the values.
C     C
C           WRITE (*, *) 'Number of Constants       : ', NCONST
C           WRITE (*, *) 'Number of Data Packets    : ', NPKTS
C           WRITE (*, *) 'Number of Reference Values: ', NREFS
C
C$ Restrictions
C
C     The segment described by DESCR MUST be a generic segment,
C     otherwise the results of this routine are not predictable.
C
C$ Author_and_Institution
C
C      K.R. Gehringer  (JPL)
C      W.L. Taber      (JPL)
C      F.S. Turner     (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    SPICELIB Version 1.4.0, 07-SEP-2001 (EDW)
C
C        Replaced DAFRDA call with DAFGDA.
C
C-    SPICELIB Version 1.3.0, 14-JUN-1999 (FST)
C
C        Altered the check in/out structure to be more reasonable.
C        This introduced redundant code, but only to increase the
C        efficiency of the normal mode of operation.
C
C-    SPICELIB Version 1.2.0, 24-SEP-1998 (FST)
C
C        Modified the code that handles reading the meta data from the
C        DAF to handle the case when the number of meta data items in
C        the file exceeds the current maximum defined in sgparam.inc.
C        In the event that this situation occurs, the routine loads
C        what meta data it can interpret and ignores the rest.  In
C        this event if NMETA is requested, it is returned as MXMETA in
C        sgparam.inc.
C
C        An additional exception is now trapped by the routine. If
C        a generic segment in a DAF reports less than the known minimum
C        number of meta data items, then the routine signals the
C        error SPICE(INVALIDMETADATA).
C
C        The conditions that cause the SPICE(UNKNOWNMETAITEM) to be
C        signaled have been altered. Now if the integer mnemonic
C        is not between 1 and METASZ inclusive, or NMETA the error
C        is signaled.  In the versions preceding this change, for
C        segments that reported less than NMETA items of meta data
C        could not use this routine to request the number of meta
C        data items without signalling SPICE(UNKNOWNMETAITEM).
C
C-    SPICELIB Version 1.1.0, 11-APR-1995 (KRG)
C
C        Modified the code that deals with the EQUIVALENCEd part
C        descriptor. We now call MOVED rather than using a direct
C        assignment.
C
C-    SPICELIB Version 1.0.0, 11-APR-1995 (KRG) (WLT)
C
C-&
 
 
C$ Index_Entries
C
C     retrieve a meta data value for a generic segment
C
C-&
 
C
C     Spicelib Functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local Parameters
C
C     Include the mnemonic values for the generic segment declarations.
C
      INCLUDE 'sgparam.inc'
 
C
C     Local Variables
C
      INTEGER               AMETAS
      INTEGER               BEGIN
      INTEGER               BEGMTA
      INTEGER               BEGM1
      INTEGER               END
      INTEGER               ENDMTA
      INTEGER               I
      INTEGER               IOFFST
      INTEGER               LSTBEG
      INTEGER               LSTHAN
      INTEGER               META   ( MXMETA )
      INTEGER               METASZ
      INTEGER               ND
      INTEGER               NI
      INTEGER               NIOVR2
 
      DOUBLE PRECISION      DMTASZ
      DOUBLE PRECISION      XMETA  ( MXMETA )
 
      DOUBLE PRECISION      DTEMP  (      2 )
      INTEGER               ITEMP  (      4 )
 
      EQUIVALENCE         ( DTEMP, ITEMP )
 
      LOGICAL               NIEVEN
 
      SAVE                  LSTBEG
      SAVE                  LSTHAN
      SAVE                  META
      SAVE                  METASZ
      SAVE                  NIEVEN
      SAVE                  IOFFST
 
      DATA                  LSTBEG  / -1 /
      DATA                  LSTHAN  /  0 /
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
C
C     Handle the case when we are looking at the same file and segment
C     descriptor first.  This will result in duplicated code, but will
C     increase efficiency for the usual execution case. We need not
C     worry about the first time through, since LSTHAN and LSTBEG are
C     set to values that are bogus for actual DAF files.
C
      IF ( HANDLE .EQ. LSTHAN ) THEN
 
C
C        Get the begin and end values from the descriptor. They are
C        located in the last two "integer" positions of the descriptor.
C
         IF ( NIEVEN ) THEN
            CALL MOVED ( DESCR(IOFFST), 1, DTEMP )
            BEGIN = ITEMP(1)
            END   = ITEMP(2)
         ELSE
            CALL MOVED ( DESCR(IOFFST), 2, DTEMP )
            BEGIN = ITEMP(2)
            END   = ITEMP(3)
         END IF
 
C
C        Check the segment start address. This will tell us whether we
C        are looking at the same segment.
C
         IF ( LSTBEG .EQ. BEGIN ) THEN
 
C
C        The only acceptable integer mnemonics at this point are 1
C        through METASZ inclusive, and NMETA.  All other requests
C        should signal the SPICE(UNKNOWNMETAITEM) error, since the
C        current segment has no knowledge of these values.
C
            IF (   ( MNEMON .LE. 0      )       .OR.
     .           ( ( MNEMON .GT. METASZ ) .AND.
     .             ( MNEMON .NE. NMETA  ) )           ) THEN
 
               CALL CHKIN ( 'SGMETA' )
               VALUE = -1
               CALL SETMSG ( 'The item requested, #, is not one of the'
     .         //            ' recognized meta data items associated'
     .         //            ' with this generic segment.'             )
               CALL ERRINT ( '#',   MNEMON                             )
               CALL SIGERR ( 'SPICE(UNKNOWNMETAITEM)'                  )
               CALL CHKOUT ( 'SGMETA'                                  )
               RETURN
 
            END IF
 
C
C           Set the value for the desired meta data item and return.
C
            VALUE = META ( MNEMON )
 
            RETURN
 
         END IF
 
      END IF
 
C
C     At this point we are going to have to load the meta data.  If
C     the new handle and the old handle are the same, then the above
C     code has already retrieved the relevant segment addresses. If not
C     we need to fetch them.  First check in.
C
 
      CALL CHKIN ( 'SGMETA' )
 
      IF ( HANDLE .NE. LSTHAN ) THEN
 
         CALL DAFHSF ( HANDLE, ND,  NI )
 
         IF ( FAILED () ) THEN
            CALL CHKOUT ( 'SGMETA' )
            RETURN
         END IF
 
         NIOVR2 = NI / 2
 
         NIEVEN = ( 2 * NIOVR2 ) .EQ. NI
 
         IOFFST = ND + NIOVR2
         LSTHAN = HANDLE
 
C
C        Get the begin and end values from the descriptor. They are
C        located in the last two "integer" positions of the descriptor.
C
         IF ( NIEVEN ) THEN
            CALL MOVED ( DESCR(IOFFST), 1, DTEMP )
            BEGIN = ITEMP(1)
            END   = ITEMP(2)
         ELSE
            CALL MOVED ( DESCR(IOFFST), 2, DTEMP )
            BEGIN = ITEMP(2)
            END   = ITEMP(3)
         END IF
 
      END IF
 
C
C     Save the new begin address. Remember we have either just computed
C     this from the IF block above, or we computed it in the very
C     first IF block.
C
      LSTBEG = BEGIN
 
C
C     Compute the begin address of the meta data and compute the
C     end address of the number we will be collecting.
C
      CALL DAFGDA ( HANDLE, END, END, DMTASZ )
 
      IF ( FAILED () ) THEN
         CALL CHKOUT ( 'SGMETA' )
         RETURN
      END IF
 
      METASZ = NINT ( DMTASZ )
C
C     Store the actual meta size in AMETAS, in case METASZ ends up
C     being modified to conform to our current understanding of
C     meta data items.
C
      AMETAS = METASZ
 
C
C     Check to see if METASZ is an unacceptable value.
C
      IF ( METASZ .LT. MNMETA ) THEN
 
         VALUE = -1
         CALL SETMSG ( 'This segment reports that it has # meta '
     .   //            'data items. Every generic segment must '
     .   //            'have at least #.'                          )
         CALL ERRINT ( '#',   METASZ                               )
         CALL ERRINT ( '#',   MNMETA                               )
         CALL SIGERR ( 'SPICE(INVALIDMETADATA)'                    )
         CALL CHKOUT ( 'SGMETA'                                    )
         RETURN
 
C
C     If it is not, we may need to fix a few things to work around some
C     older files that have been delivered. We perform these kludges
C     here. Originally, the number of meta data items was not
C     considered to be part of the meta data. It now is, so if we
C     encounter an older version of the file, we need to increment the
C     meta data size by 1. The number of meta data items is always
C     after all of the meta data items, so we can do this.
C
      ELSE IF ( METASZ .EQ. 15 ) THEN
 
         METASZ = METASZ + 1
         AMETAS = METASZ
 
C
C     If not check to see if METASZ is greater than the known MXMETA.
C     If it is then this segment most likely was constructed from
C     some newer version of the toolkit.  Load what meta data we
C     currently know about as laid out in sgparam.inc.
C
      ELSE IF ( METASZ .GT. MXMETA ) THEN
 
C
C        Leave AMETAS alone, since we need to know how far back
C        into the DAF file to begin reading.
C
          METASZ = MXMETA
 
      END IF
 
C
C     The address computations that follow are precisely the same
C     as the previous version of the file, except when AMETAS is not
C     METASZ.  This only happens when METASZ is greater than MXMETA.
C
      BEGMTA = END - AMETAS + 1
      ENDMTA = BEGMTA + METASZ - 1
 
      CALL DAFGDA ( HANDLE, BEGMTA, ENDMTA, XMETA )
 
      IF ( FAILED () ) THEN
         CALL CHKOUT ( 'SGMETA' )
         RETURN
      END IF
 
C
C     Convert all of the meta data values into integers.
C
      DO I = 1, METASZ
         META(I) = NINT ( XMETA(I) )
      END DO
 
C
C     The kludge continues... NMETA and MXMETA are ALWAYS the same
C     value, and any missing values must appear between the last known
C     value, META(METASZ-1), and the end value, META(NMETA), so we zero
C     them out.
C
      META(NMETA) = METASZ
 
      DO I = METASZ, MXMETA-1
         META(I) = 0
      END DO
 
C
C     Adjust the bases so that the N'th item of a partition is at
C     address META(PARTITION_BASE) + N
C
      BEGM1          = BEGIN - 1
      META( CONBAS ) = META( CONBAS ) + BEGM1
      META( REFBAS ) = META( REFBAS ) + BEGM1
      META( RDRBAS ) = META( RDRBAS ) + BEGM1
      META( PDRBAS ) = META( PDRBAS ) + BEGM1
      META( PKTBAS ) = META( PKTBAS ) + BEGM1
      META( RSVBAS ) = META( RSVBAS ) + BEGM1
 
C
C     The only acceptable integer mnemonics at this point are 1 through
C     METASZ inclusive, and NMETA.  All other requests should signal
C     the SPICE(UNKNOWNMETAITEM) error, since the current segment has
C     no knowledge of these values.
C
      IF (   ( MNEMON .LE. 0      )       .OR.
     .     ( ( MNEMON .GT. METASZ ) .AND.
     .       ( MNEMON .NE. NMETA  ) )           ) THEN
 
         VALUE = -1
         CALL SETMSG ( 'The item requested, #, is not one of the'
     .   //            ' recognized meta data items associated'
     .   //            ' with this generic segment.'               )
         CALL ERRINT ( '#',   MNEMON                               )
         CALL SIGERR ( 'SPICE(UNKNOWNMETAITEM)'                    )
         CALL CHKOUT ( 'SGMETA'                                    )
         RETURN
 
      END IF
 
C
C     Set the value for the desired meta data item, check out if we
C     need to, and return.
C
      VALUE = META ( MNEMON )
 
      CALL CHKOUT ( 'SGMETA' )
      RETURN
      END
