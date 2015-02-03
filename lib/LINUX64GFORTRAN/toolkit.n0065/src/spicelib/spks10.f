 
C$Procedure      SPKS10 ( S/P Kernel, subset, type 10 )
 
      SUBROUTINE SPKS10 ( SRCHAN, SRCDSC, DSTHAN, DSTDSC, DSTSID )
 
C$ Abstract
C
C     Extract a subset of the data in a type 10 SPK segment into a new
C     type 10 segment.
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
C     SPK
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations
 
      IMPLICIT NONE
      INCLUDE              'sgparam.inc'
      INTEGER               SRCHAN
      DOUBLE PRECISION      SRCDSC(*)
      INTEGER               DSTHAN
      DOUBLE PRECISION      DSTDSC(*)
      CHARACTER*(*)         DSTSID
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SRCHAN     I   Handle of the SPK file with the source segment.
C     SRCDSC     I   Descriptor for the source segment.
C     DSTHAN     I   Handle of the SPK file for the destination segment.
C     DSTDSC     I   Descriptor for the destination segment.
C     DSTSID     I   Segment identifier for the new segment.
C
C$ Detailed_Input
C
C     SRCHAN   The handle of the SPK file containing the source segment.
C
C     SRCDSC   The SPK descriptor for the source segment.
C
C     DSTHAN   The handle of the SPK file containing the new segment.
C
C     DSTDSC   The SPK descriptor for the destination segment. It
C              contains the desired start and stop times for the
C              requested subset.
C
C     DSTSID   The segment identifier for the destination segment.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     See arguments SRCHAN, DSTHAN.
C
C$ Particulars
C
C     This subroutine copies a subset of the data form one SPK segment
C     to another.
C
C     The exact structure of a segment of SPK type 10 is detailed in
C     the SPK Required Reading. Please see this document for details.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1) We assume that the source descriptor actually describes a
C        segment in the source SPK file containing the time coverage
C        that is desired for the subsetting operation.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 30-JUN-1997 (KRG)
C
C-&
 
C$ Index_Entries
C
C     subset type_10 spk segment
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local Parameters
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 40 )
C
C     DAF ND and NI values for SPK files.
C
      INTEGER               ND
      PARAMETER           ( ND = 2 )
 
      INTEGER               NI
      PARAMETER           ( NI = 6 )
 
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
 
      INTEGER               NANGS
      PARAMETER           ( NANGS  =  4 )
 
      INTEGER               PKTSIZ
      PARAMETER           ( PKTSIZ = NELEMS + NANGS )
 
C
C     Local Variables
C
      CHARACTER*(WDSIZE)    TIME
 
      DOUBLE PRECISION      BEGTIM
      DOUBLE PRECISION      CONSTS ( NCONST )
      DOUBLE PRECISION      DTEMP  ( ND     )
      DOUBLE PRECISION      ENDTIM
      DOUBLE PRECISION      MYREF
      DOUBLE PRECISION      PACKET ( PKTSIZ )
 
      INTEGER               BEGIDX
      INTEGER               DUMMY
      INTEGER               ENDIDX
      INTEGER               I
      INTEGER               ITEMP  ( NI )
      INTEGER               NEPOCH
 
      LOGICAL               FOUND
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKS10' )
      END IF
C
C     First, unpack the destination segment descriptor and set some
C     local variables.
C
      CALL DAFUS( DSTDSC, ND, NI, DTEMP, ITEMP )
 
      BEGTIM = DTEMP(1)
      ENDTIM = DTEMP(2)
C
C     Get the constants for the input segment and send them to the
C     output segment by beginning a fixed packet size segment.
C
      CALL SGFCON ( SRCHAN, SRCDSC, 1, NCONST, CONSTS )
      CALL SGBWFS ( DSTHAN, DSTDSC,    DSTSID,
     .              NCONST, CONSTS,    PKTSIZ, EXPCLS )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKS10' )
         RETURN
      END IF
 
C
C     Get the beginning and ending indices for the packets we need for
C     the destination segment.  Note we need to get the preceding
C     and succeeding packets (if there are any) corresponding to the
C     start and end times of the output segments
C
      CALL SGFRVI ( SRCHAN, SRCDSC, BEGTIM, MYREF, BEGIDX, FOUND )
 
      IF ( .NOT. FOUND ) THEN
 
         CALL ETCAL  ( BEGTIM, TIME )
         CALL SETMSG ( 'An error has occurred while attempting '
     .   //            'to subset the a type 10 SPK segment. The '
     .   //            'error occurred while attempting to '
     .   //            'locate a packet for the epoch #.  There '
     .   //            'does not appear to be such a packet. ' )
         CALL ERRCH  ( '#', TIME )
         CALL SIGERR ( 'SPICE(CANNOTGETPACKET)'  )
         CALL CHKOUT ( 'SPKS10' )
         RETURN
 
      END IF
 
      IF ( MYREF .GT. BEGTIM ) THEN
         BEGIDX = MAX ( 1, BEGIDX - 1 )
      END IF
 
 
      CALL SGFRVI ( SRCHAN, SRCDSC, ENDTIM, MYREF, ENDIDX, FOUND )
 
      IF ( .NOT. FOUND ) THEN
 
         CALL ETCAL  ( ENDTIM, TIME )
         CALL SETMSG ( 'An error has occurred while attempting '
     .   //            'to subset the a type 10 SPK segment. The '
     .   //            'error occurred while attempting to '
     .   //            'locate a packet for the epoch #.  There '
     .   //            'does not appear to be such a packet. ' )
         CALL ERRCH  ( '#', TIME )
         CALL SIGERR ( 'SPICE(CANNOTGETPACKET)'  )
         CALL CHKOUT ( 'SPKS10' )
         RETURN
 
      END IF
 
C
C     Get the total number of epochs.
C
      CALL SGMETA ( SRCHAN, SRCDSC, NREF, NEPOCH )
 
      IF ( MYREF .LT. ENDTIM ) THEN
         ENDIDX = MIN ( NEPOCH, ENDIDX + 1 )
      END IF
 
C
C     Now we get the data one record at a time from the source segment
C     and write it out to the destination segment.
C
      DO I = BEGIDX, ENDIDX
 
         CALL SGFPKT ( SRCHAN, SRCDSC, I, I,   PACKET, DUMMY )
         CALL SGFREF ( SRCHAN, SRCDSC, I, I,           MYREF )
         CALL SGWFPK ( DSTHAN, 1,      PACKET, 1,      MYREF )
 
      END DO
 
C
C     Now all we need to do is end the segment.
C
      CALL SGWES  ( DSTHAN   )
 
      CALL CHKOUT ( 'SPKS10' )
      RETURN
 
      END
