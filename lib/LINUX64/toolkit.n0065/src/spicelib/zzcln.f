C$Procedure      ZZCLN ( Private --- clean up )
 
      SUBROUTINE ZZCLN ( LOOKAT, NAMEAT,
     .                   NAMLST, DATLST, NMPOOL, CHPOOL, DPPOOL )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This routine cleans up changes to the kernel pool that were
C     made prior to the detection of a parsing error.  It is purely
C     a utility for use only by ZZRVAR.
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
C       PRIVATE UTILITY
C
C$ Declarations
 
      IMPLICIT NONE
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )
 
      INTEGER               LOOKAT
      INTEGER               NAMEAT
      INTEGER               NAMLST ( * )
      INTEGER               DATLST ( * )
      INTEGER               NMPOOL ( 2, LBPOOL:* )
      INTEGER               CHPOOL ( 2, LBPOOL:* )
      INTEGER               DPPOOL ( 2, LBPOOL:* )
 
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C
C      LOOKAT     I   The hash value of some name.
C      NAMEAT     I   The actual node where the name was stored
C      NAMLST    I/O  The array of heads of name lists.
C      DATLST    I/O  The array of heads of lists of values
C      NMPOOL    I/O  The linked list pool of variable names.
C      CHPOOL    I/O  The linked list pool of variable d.p. values.
C      DPPOOL    I/O  The linked list pool of variable string values.
C
C
C$ Detailed_Input
C
C     LOOKAT      is the hash value of some string.  NAMLST(LOOKAT) is
C                 the head of some collision resolution list of names.
C
C     NAMEAT      is the node in the list headed by NAMLST(LOOKAT) where
C                 some name has been stored in the kernel pool
C                 collection of NAMES. The node NAMEAT needs to be
C                 removed from its list in NMPOOL.
C
C     NAMLST      is an array of heads of collision
C                 resolution lists in NMPOOL.  If NAMLST(LOOKAT) is
C                 the same as NAMEAT, we need to adjust NAMLST(LOOKAT)
C                 so that it points to the next node in the list.
C
C     DATLST      is an array of heads of data value lists for the
C                 variables in the kernel pool.  We will need to free
C                 the data list pointed to by DATLST(NAMEAT) and
C                 zero out DATLST(NAMEAT).
C
C     NMPOOL      is a linked list pool for collision resolutions of
C                 a string hash function.  The node NAMEAT needs to
C                 be freed.
C
C     CHPOOL      is a linked list pool for string values associated
C                 with a kernel pool variable  If DATLST(NAMEAT) points
C                 into CHPOOL, then the list containing this node must
C                 be freed.
C
C     DPPOOL      is a linked list pool for d.p. values associated
C                 with a kernel pool variable. If DATLST(NAMEAT) points
C                 into DPPOOL, then the list containing this node must
C                 be freed.
C
C
C$ Detailed_Output
C
C      NAMLST    are the same structures as the input with the
C      DATLST    corrections made for the freeing of the NMPOOL
C      NMPOOL    node NAMEAT.
C      CHPOOL
C      DPPOOL
C
C$ Parameters
C
C      None.
C
C$ Files
C
C      None.
C
C$ Exceptions
C
C      None.
C
C$ Particulars
C
C     During the course of reading and parsing a kernel pool variable
C     it may happen that an error in the input text is encountered after
C     a kernel pool variable update has been initiated.  This routine
C     removes all traces of that variable from the kernel pool storage
C     structures.
C
C$ Examples
C
C     See ZZRVAR
C
C$ Restrictions
C
C     None.
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
C-    SPICELIB Version 1.0.0, 20-SEP-1995 (WLT)
C
C-&
 
 
C
C     Local Parameters and Variables
C
      INTEGER               NEXT
      PARAMETER           ( NEXT   = 1 )
 
      INTEGER               PREV
      PARAMETER           ( PREV   = 2 )
 
 
      INTEGER               HEAD
      INTEGER               TAIL
C
C     First perform the clean up function. This variable
C     has been corrupted so there's no point in hanging
C     on to it.
C
C     First remove the data...
C
      CALL CHKIN ( 'ZZCLN' )
 
      HEAD = DATLST( NAMEAT )
 
      IF      ( HEAD .LT. 0 ) THEN
 
         HEAD = -HEAD
         TAIL = -CHPOOL(PREV,HEAD)
         CALL LNKFSL ( HEAD, TAIL, CHPOOL )
 
      ELSE IF ( HEAD .GT. 0 ) THEN
 
         TAIL = -DPPOOL(PREV,HEAD)
         CALL LNKFSL ( HEAD, TAIL, DPPOOL )
 
      END IF
C
C     Remove the sub-list head from the data list.
C
      DATLST ( NAMEAT ) = 0
C
C     If this was a singleton list remove the pointer to
C     the head of the list.
C
      HEAD =  NAMLST(LOOKAT)
      TAIL = -NMPOOL(PREV,HEAD)
 
      IF ( HEAD .EQ. TAIL ) THEN
         NAMLST(LOOKAT) = 0
      ELSE IF ( NAMLST(LOOKAT) .EQ. NAMEAT ) THEN
         NAMLST(LOOKAT) = NMPOOL(NEXT,NAMEAT)
      END IF
C
C     Finally free up this node in the NMPOOL.
C
      HEAD = NAMEAT
      TAIL = NAMEAT
 
      CALL LNKFSL ( HEAD, TAIL, NMPOOL )
 
      CALL CHKOUT ('ZZCLN' )
      RETURN
      END
