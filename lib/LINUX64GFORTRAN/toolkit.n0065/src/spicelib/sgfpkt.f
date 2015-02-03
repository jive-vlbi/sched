C$Procedure      SGFPKT ( Generic Segment: Fetch data packets )
 
      SUBROUTINE SGFPKT ( HANDLE, DESCR, FIRST, LAST, VALUES, ENDS )
 
C$ Abstract
C
C     Given the descriptor for a generic segment in a DAF file
C     associated with HANDLE, fetch the data packets indexed from FIRST
C     to LAST from the packet partition of the generic segment.
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
C      GENERIC SEGMENTS
C
C$ Declarations
 
      IMPLICIT NONE
 
      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR   ( * )
      INTEGER               FIRST
      INTEGER               LAST
      DOUBLE PRECISION      VALUES  ( * )
      INTEGER               ENDS    ( * )
 
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      HANDLE     I   The file handle attached to an open DAF.
C      DESCR      I   The descriptor associated with a generic segment.
C      FIRST      I   The index of the first data packet to fetch.
C      LAST       I   The index of the last data packet to fetch.
C      VALUES     O   The data packets that have been fetched.
C      ENDS       O   An array of pointers to the ends of the packets.
C
C$ Detailed_Input
C
C      HANDLE     is the handle of a DAF opened for reading that
C                 contains the segment described by DESCR.
C
C      DESCR      is the descriptor of the segment with the desired
C                 constant values. This must be the descriptor for a
C                 generic segment in the DAF associated with HANDLE.
C
C      FIRST      is the index of the first value to fetch from the
C                 constants section of the DAF segment described
C                 by DESCR.
C
C      LAST       is the index of the last value to fetch from the
C                 constants section of the DAF segment described
C                 by DESCR
C
C$ Detailed_Output
C
C     VALUES      is the array of values constructed by concatenating
C                 requested packets one after the other into
C                 an array.  Pictorially we can represent VALUES
C                 as:
C
C                    +--------------------------+
C                    | first requested packet   |
C                    +--------------------------+
C                    | second requested packet  |
C                    +--------------------------+
C                               .
C                               .
C                               .
C                    +--------------------------+
C                    | first requested packet   |
C                    +--------------------------+
C
C     ENDS        is an array of pointers to the ends of the
C                 fetched packets.  ENDS(1) gives the index
C                 of the last item of the first packet fetched.
C                 ENDS(2) gives the index of the last item of
C                 the second packet fetched, etc.
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
C     1) The error SPICE(REQUESTOUTOFBOUNDS) will be signalled
C        if FIRST is less than 1 or LAST is greater than the
C        number of packets.
C
C     2) The error SPICE(REQUESTOUTOFORDER) will be signalled
C        if LAST is less than FIRST.
C
C     3) The error SPICE(UNKNOWNPACKETDIR) will be signalled if
C        the packet directory structure is unrecognized.  The most
C        likely cause of this error is that an upgrade to your
C        version of the SPICE toolkit is needed.
C
C$ Particulars
C
C     This routine fetches requested packets from a generic
C     DAF segment.  The two arrays returned have the following
C     relationship to one another.  The first packet returned
C     resides in VALUES between indexes 1 and ENDS(1).  If a
C     second packet is returned it resides in VALUES between
C     indices ENDS(1)+1 and ENDS(2).  This relations ship is
C     repeated so that if I is greater than 1 and at least I
C     packets were returned then the I'th packet resides in
C     VALUES between index ENDS(I-1) + 1 and ENDS(I).
C
C$ Examples
C
C     Suppose that you have located a generic DAF segment (as
C     identified by the contents of a segment descriptor).  The
C     fragment of code below shows how you could fetch packets
C     3 through 7 (assuming that many packets are present).
C     from the segment.
C
C        Declarations:
C
C        DOUBLE PRECISION   MYPKSZ (<enough room to hold all packets>)
C
C        INTEGER               ENDS  ( 5 )
C        INTEGER               MYNPKT
C
C        get the number of packets
C
C        CALL SGMETA ( HANDLE, DESCR, NPKT, MYNPKT )
C
C        finally, fetch the packets from the segment.
C
C        IF ( 7 .LE. MYNPKT ) THEN
C           CALL SGFPKT ( HANDLE, DESCR, 3, 7,  MYPKSZ, ENDS )
C        END IF
C
C$ Restrictions
C
C      The segment described by DESCR must be a generic segment,
C      otherwise the results of this routine are not predictable.
C
C$ Author_and_Institution
C
C     K.R. Gehringer  (JPL)
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 07-SEP-2001 (EDW)
C
C        Replaced DAFRDA calls with DAFGDA.
C
C-    SPICELIB Version 1.1.0, 30-JUL-1996 (KRG) (NJB)
C
C        Found and fixed a bug in the calculation of the beginning
C        address for variable length packet fetching. The base address
C        for the packet directory was not added into the value. This
C        bug went unnoticed because of a bug in SGSEQW, entry SGWES,
C        that put absolute addresses into the packet directory rather
C        than addresses that were relative to the start of the DAF
C        array. The bug in SGSEQW has also been fixed.
C
C-    SPICELIB Version 1.0.0, 06-JAN-1994 (KRG) (WLT)
C
C-&
 
C$ Index_Entries
C
C     fetch packets from a generic segment
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
C     Include the mnemonic values.
C
      INCLUDE 'sgparam.inc'
 
C
C     Local Variables
C
      DOUBLE PRECISION      DTEMP(2)
 
      INTEGER               B
      INTEGER               BEGIN1
      INTEGER               BEGIN2
      INTEGER               E
      INTEGER               I
      INTEGER               MYNPDR
      INTEGER               MYNPKT
      INTEGER               MYPDRB
      INTEGER               MYPDRT
      INTEGER               MYPKSZ
      INTEGER               MYPKTB
      INTEGER               MYPKTO
      INTEGER               SIZE
      INTEGER               SOFFST
      INTEGER               VOFFST
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'SGFPKT' )
C
C     Perform the needed initialization
C
      CALL SGMETA ( HANDLE, DESCR, NPKT,   MYNPKT )
      CALL SGMETA ( HANDLE, DESCR, PDRTYP, MYPDRT )
      CALL SGMETA ( HANDLE, DESCR, PKTOFF, MYPKTO )
      CALL SGMETA ( HANDLE, DESCR, PKTSZ,  MYPKSZ )
      CALL SGMETA ( HANDLE, DESCR, PKTBAS, MYPKTB )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SGFPKT' )
         RETURN
      END IF
 
C
C     Perform checks on the inputs for reasonableness.
C
      IF ( ( FIRST .LT. 1 ) .OR. ( LAST .GT. MYNPKT ) ) THEN
 
         CALL SETMSG ( 'The range of packets requested '
     .   //            'extends beyond the available packet '
     .   //            'data.  The packet data is available '
     .   //            'for indexes 1 to #.  You''ve requested '
     .   //            'data from # to #. '                       )
         CALL ERRINT ( '#', MYNPKT                                )
         CALL ERRINT ( '#', FIRST                                 )
         CALL ERRINT ( '#', LAST                                  )
         CALL SIGERR ( 'SPICE(REQUESTOUTOFBOUNDS)'                )
         CALL CHKOUT ( 'SGFPKT'                                   )
         RETURN
 
      END IF
 
      IF ( LAST .LT. FIRST ) THEN
 
         CALL SETMSG ( 'The last packet requested, #, is '
     .   //            'before the first packet '
     .   //            'requested, #. '                     )
         CALL ERRINT ( '#', LAST                            )
         CALL ERRINT ( '#', FIRST                           )
         CALL SIGERR ( 'SPICE(REQUESTOUTOFORDER)'           )
         CALL CHKOUT ( 'SGFPKT'                             )
         RETURN
 
      END IF
C
C     We've passed the sanity tests, if the packet directory structure
C     is recognized fetch the values and ends. We assume that we are
C     reading data from a correctly constructed generic segment, so we
C     do not need to worry about the type of reference index, as this is
C     not needed to fetch a data packet.
C     Currently, only two packet directory types are supported, and this
C     subroutine is the only place that this is documented. The types
C     have values zero (0) and one (1) for, respectively, fixed size
C     packets and variable size packets.
C
      IF ( MYPDRT .EQ. 0 ) THEN
C
C        All packets have the same size MYPKSZ so the address of the
C        start of the first packet and end of the last packet are easily
C        computed.
C
         IF ( MYPKTO .EQ. 0 ) THEN
C
C           Compute tha addresses for the packet data in the generic
C           segment.
C
            B    =  MYPKTB   + ( FIRST - 1 ) * MYPKSZ + 1
            E    =  MYPKTB   +  LAST         * MYPKSZ
C
C           Get the packet data all in one shot since we know it's
C           contiguous.
C
            CALL DAFGDA ( HANDLE, B, E, VALUES )
 
         ELSE
C
C           Compute the addresses for the packet data in the generic
C           segment. Remember that we need to account for an offset
C           here to get to the start of the actual data packet.
C
            SIZE = MYPKSZ + MYPKTO
C
C           Get the packet data. Because there is an offset from the
C           address to the start of the packet data, we need to get
C           the data one packet at a time rather than all at once.
C
            DO I = FIRST, LAST
 
               SOFFST = ( I -     1 ) * SIZE   + 1
               VOFFST = ( I - FIRST ) * MYPKSZ + 1
 
               B =  MYPKTB + SOFFST + MYPKTO
               E =  MYPKTB + SOFFST + MYPKSZ
 
               CALL DAFGDA ( HANDLE, B, E, VALUES(VOFFST) )
 
               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'SGFPKT' )
                  RETURN
               END IF
 
            END DO
 
         END IF
C
C        Compute the ends for each of the data packets. This is the
C        same for both of the cases above because we have fixed size
C        data packets.
C
         DO I = 1, LAST-FIRST+1
            ENDS(I) = I * MYPKSZ
         END DO
 
      ELSE
C
C        In addition to the other meta data items already retrieved, we
C        will also need a few others.
C
         CALL SGMETA ( HANDLE, DESCR, PDRBAS, MYPDRB )
         CALL SGMETA ( HANDLE, DESCR, NPDR,   MYNPDR )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SGFPKT' )
            RETURN
         END IF
C
C        Each packet has a different size, so we need to fetch each one
C        individually, keeping track of the ends and things. We assume
C        that there is enough room in the array of values to hold all of
C        the packets. For the variable packet case, however, we do not
C        need to treat the implicit indexing and explicit indexing cases
C        separately.
C
         VOFFST = 1
 
         DO I = 1, LAST-FIRST+1
C
C           Compute the addresses in the generic segment for the
C           beginning of data packets I and I+1. We need these to
C           compute the size of the packet.
C
            B = MYPDRB + FIRST + I - 1
            E = B + 1
C
C           Get the beginning addresses for the two data packets and
C           convert them into integers.
C
            CALL DAFGDA( HANDLE, B, E, DTEMP )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'SGFPKT' )
               RETURN
            END IF
 
            BEGIN1 = INT ( DTEMP(1) )
            BEGIN2 = INT ( DTEMP(2) )
C
C           Compute the size of data packet I, remembering to deal with
C           the packet offset that might be present, and the beginning
C           and ending addresses for the packet data.
C
            SIZE = BEGIN2 - BEGIN1 - MYPKTO
            B    = MYPKTB + BEGIN1
            E    = B      + SIZE - 1
C
C           Get the data for packet I.
C
            CALL DAFGDA ( HANDLE, B, E, VALUES(VOFFST) )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'SGFPKT' )
               RETURN
            END IF
C
C           Compute the end for packet I and store it.
C
            VOFFST  = VOFFST + SIZE
            ENDS(I) = VOFFST - 1
 
         END DO
 
      END IF
 
      CALL CHKOUT ( 'SGFPKT' )
      RETURN
 
      END
