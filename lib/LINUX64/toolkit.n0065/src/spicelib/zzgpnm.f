C$Procedure      ZZGPNM ( Get position of a name )
 
      SUBROUTINE ZZGPNM ( NAMLST,
     .                    NMPOOL, NAMES, DATLST,
     .                    DPPOOL, DPVALS,
     .                    CHPOOL, CHVALS,
     .                    VARNAM, FOUND,  LOOKAT, NAMEAT )
 
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Locate the node in the array NAMES where a variable is located
C     or will be inserted.
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
C     PRIVATE KERNEL
C
C$ Keywords
C
C     FILES
C
C$ Declarations
 
 
      IMPLICIT NONE
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )
 
      INTEGER               NAMLST     (           * )
      INTEGER               NMPOOL     ( 2, LBPOOL:* )
      CHARACTER*(*)         NAMES      (           * )
      INTEGER               DATLST     (           * )
      INTEGER               DPPOOL     ( 2, LBPOOL:* )
      DOUBLE PRECISION      DPVALS     (           * )
      INTEGER               CHPOOL     ( 2, LBPOOL:* )
      CHARACTER*(*)         CHVALS     (           * )
 
      CHARACTER*(*)         VARNAM
      LOGICAL               FOUND
      INTEGER               LOOKAT
      INTEGER               NAMEAT
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAMLST    I/O  array of collision resolution list heads
C     NMPOOL    I/O  linked list pool of collision resolution lists
C     NAMES     I/O  array of names of kernel pool variables
C     DATLST    I/O  array of heads of lists of variable values
C     DPPOOL    I/O  linked list pool of pointer lists to d.p. values
C     DPVALS    I/O  array of d.p. kernel pool values
C     CHPOOL    I/O  linked list pool of pointer lists to string values
C     CHVALS    I/O  array of string kernel pool values
C     VARNAM     I   A name to find/put into the kernel pool name list.
C     FOUND      O   TRUE if VARNAM is already in the list of names
C     LOOKAT     O   The value ZZHASH(VARNAM).
C     NAMEAT     O   The location where VARNAM is to be located.
C
C$ Detailed_Input
C
C     NAMLST    this collection of arrays together with the hash
C     NMPOOL    function ZZHASH provide the mechanism for storing
C     NAMES     and retrieving kernel pool variables.
C     DATLST
C     DPPOOL    Given a potential variable name NAME the function
C     DPVALS    ZZHASH(NAME) gives the location in the array in
C     CHPOOL    NAMLST where one should begin looking for the
C     CHVALS    kernel pool variable NAME.
C               If NAMLST( ZZHASH(NAME) ) is zero, there is no kernel
C               pool variable corresponding to NAME.  If it is non-zero
C               then NAMLST is the head node of a linked list of names
C               that evaluate to the same integer under the function
C               ZZHASH.  Letting NODE = NAMLST( ZZHASH(NAME) ) check
C               NAMES(NODE) for equality with NAME.  If there is
C               no match find the next node ( NMPOOL(NEXT,NODE) ) until
C               a match occurs or all nodes of the list have been
C               examined.  To insert a new NAME allocate a node NEW from
C               the free list of NMPOOL and append it to the tail of the
C               list pointed to by NAMLST ( ZZHASH(NAME) ).
C
C               Once a node for NAME is located (call it NAMEAT)
C               the values for NAME can be found by examining
C               DATLST(NAMEAT).  If zero, no values have yet been
C               given to NAME.  If less than zero, -DATLST(NAMEAT)
C               is the head node of a list in CHPOOL that gives the
C               indexes of the values of NAME in CHVALS.  If greater
C               than zero, DATLST(NAMEAT) is the head node of a list
C               in DPPOOL that gives the indexes of the values of NAME
C               in DPVALS.
C
C     VARNAM    is the name of a variable that is either already present
C               or that should be placed in the kernel pool
C$ Detailed_Output
C
C     NAMLST     is the same structure as input but updated to
C     NMPOOL     include the new variable specified by VARNAM if
C     NAMES      it is a new name.
C     DATLST
C     DPPOOL
C     DPVALS
C     CHPOOL
C     CHVALS
C
C     FOUND      is TRUE if VARNAM was already present in the name list.
C
C     LOOKAT     is the location in NAMLST where the head of the
C                ZZHASH collision linked list is stored.
C
C     NAMEAT     is the location within the array NAMES where VARNAM
C                is located.
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
C     1) If the NAMES array cannot accomodate any more kernel variable
C        names, the error 'SPICE(KERNELPOOLFULL)' is signalled.
C
C$ Particulars
C
C     This is a utility routine designed to assist the kernel pool
C     entry points PDPOOL, PCPOOL and PIPOOL.  It handles the task
C     of inserting a new variable name into the kernel pool name
C     structure and returns information on the location of that
C     name.
C
C$ Examples
C
C     See the entry points PDPOOL, PCPOOL or PIPOOL.
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
C-    SPICELIB Version 1.0.0, 29-MAR-1999 (WLT)
C
C
C-&
 
 
 
C
C     SPICELIB Functions
C
      INTEGER               LNKNFN
      INTEGER               ZZHASH
      LOGICAL               RETURN
 
C
C     Parameters
C
      INTEGER               PREV
      PARAMETER           ( PREV = 2 )
 
      INTEGER               NEXT
      PARAMETER           ( NEXT = 1 )
 
 
C
C     Local Variables
C
      INTEGER               NODE
      LOGICAL               FULL
 
      INTEGER               HEAD
      INTEGER               TAIL
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'ZZGPNM' )
 
      NAMEAT = 0
 
C
C
C     Locate this variable name in the name pool or insert it
C     if it isn't there.  The location will be NAMEAT and
C     we will use the variable FOUND to indicate whether or
C     not it was already present.
C
      LOOKAT =  ZZHASH ( VARNAM )
      NODE   =  NAMLST ( LOOKAT )
      FULL   =  LNKNFN ( NMPOOL ) .LE. 0
      FOUND  = .FALSE.
 
C
C     See if this name (or one colliding with it in the
C     hash scheme) has already been stored in the name list.
C
      IF ( NODE .GT. 0 ) THEN
 
         HEAD =  NODE
         TAIL = -NMPOOL(PREV,HEAD)
 
         DO WHILE ( NODE .GT. 0 .AND. .NOT. FOUND )
 
            FOUND  =  NAMES (NODE) .EQ. VARNAM
            NAMEAT =  NODE
            NODE   =  NMPOOL(NEXT,NODE)
 
         END DO
 
         IF (        .NOT. FOUND
     .         .AND. .NOT. FULL   ) THEN
 
C
C           We didn't find this name on the conflict resolution
C           list. Allocate a new slot for it.
C
            CALL LNKAN  ( NMPOOL, NODE         )
            CALL LNKILA ( TAIL,   NODE, NMPOOL )
 
            NAMES(NODE) =  VARNAM
            NAMEAT      =  NODE
 
         END IF
 
      ELSE IF ( .NOT. FULL ) THEN
 
C
C        Nothing like this variable name (in the hashing sense)
C        has been loaded so far.  We need to allocate
C        a name slot for this variable.
C
         CALL LNKAN ( NMPOOL, NODE )
 
         NAMLST(LOOKAT) =  NODE
         NAMES (NODE  ) =  VARNAM
         NAMEAT         =  NODE
 
      END IF
 
C
C     If the name pool was full and we didn't find this name
C     we've got an error. Diagnose it and return.
C
      IF ( FULL .AND. .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'The kernel pool does not have room '
     .   //            'for any more variables.' )
         CALL SIGERR ( 'SPICE(KERNELPOOLFULL)' )
         CALL CHKOUT ( 'ZZGPNM' )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'ZZGPNM' )
      RETURN
 
      END
