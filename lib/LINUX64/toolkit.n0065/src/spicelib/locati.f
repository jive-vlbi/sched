C$Procedure      LOCATI ( Locate an identifier in a list )
 
      SUBROUTINE LOCATI ( ID, IDSZ,  LIST,  POOL, AT, PRESNT )
 
C$ Abstract
C
C     This routine locates the current location of an identifier
C     within a list or finds a location within the list to
C     store it and then does so.  It returns the location of
C     the identifier and a flag indicating whether or not the
C     identifier was already present.
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
C       UTILITY
C
C$ Declarations
 
      IMPLICIT NONE
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )
 
      INTEGER               ID   (            * )
      INTEGER               IDSZ
      INTEGER               LIST ( IDSZ,      * )
      INTEGER               POOL ( 2, LBPOOL: * )
      INTEGER               AT
      LOGICAL               PRESNT
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      ID         I   An array of integers that comprise an identifier
C      IDSZ       I   The number of integer components per identifier
C      LIST      I/O  A list of known identifiers
C      POOL      I/O  A doubly linked list used for search the list
C      AT        I/O  Location of the ID in the list
C      PRESNT     O   If ID was already in the list TRUE otherwise FALSE
C
C$ Detailed_Input
C
C     ID          is an integer array that serves as an identifier
C                 for some object.  For example it might be a SPICE
C                 id code for a planet or satellite; it might be the
C                 instrument id and mode of operation of an instrument.
C                 See the examples section for more details.
C
C     IDSZ        is the number of components in the array ID.
C
C     LIST        is an array containing several ID's.  The array
C                 should be declared so as to have the same upper
C                 bound at least as large as the upper bound used
C                 in the declaration of POOL.
C
C     POOL        is a linked list pool that gives the search order
C                 for examining LIST to locate ID's.  The declaration
C                 of POOL and LIST need to be compatible.  Normally,
C                 the declaration should look like this:
C
C                    INTEGER   LIST (IDSZ,         LSTSIZ )
C                    INTEGER   POOL (   2, LBPOOL: LSTSIZ )
C
C                 If POOL is declared with the statement
C
C                    INTEGER   POOL (   2, LBPOOL: PSIZE  )
C
C                 then you must make sure that PSIZE is less than
C                 or equal to LSTSIZ.
C
C                 POOL should be initialized before the first
C                 call to this routine with the SPICE routine
C                 LNKINI.
C
C     AT          is a value that is set by this routine and that
C                 you should never reset yourself.  It points
C                 to the head of the linked list used for
C                 searching LIST.  Changing AT will destroy the
C                 link between POOL and LIST.
C
C                 There is one exception to these restrictions.
C                 The first call to this routine that occurs after
C                 initializing POOL, AT may have any value. It will
C                 be set upon output and from that time on, you should
C                 not alter its value except by calling this routine
C                 to do so.
C
C$ Detailed_Output
C
C     AT          on output AT points to the location in LIST
C                 of ID.
C
C     PRESNT      is a logical flag.  It indicates whether or not
C                 ID was already present in the LIST when this
C                 routine was called.  If ID was already in LIST
C                 PRESNT is returned with the value TRUE.  Otherwise
C                 it is returned with the value FALSE.
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
C     1) If the value of AT is less than zero or greater than
C        the declared size of POOL (except immediately after
C        initializing or re-initializing POOL) the
C        error 'SPICE(ADDRESSOUTOFBOUNDS)' will be signalled.
C
C     2) If the linked list pool POOL is corrupted by a higher
C        level routine, a diagnosis of the problem will be
C        made by a routine called by this one.
C
C$ Particulars
C
C     This routine serves as a utility for managing the bookkeeping
C     needed when using a local buffering scheme which removes
C     the last used item when the local buffer becomes full.
C
C     It is primarily a programming utility.  Unless you are dealing
C     with a problem very similar to the one just described, you
C     probably shouldn't be using this routine.
C
C     The example below illustrates the intended use of this
C     routine.
C
C$ Examples
C
C     Consider the following programming situation.
C
C     Suppose that a routine is being written that will
C     access large amounts of data stored in the SPICE
C     kernel pool.  Kernel pool access requires overhead that
C     may be prohibitive under some circumstances.  Buffering
C     data locally and only fetching data from the kernel pool
C     when it has not been buffered locally, may substantially
C     improve the performance of the routine being written.
C
C     However, since FORTRAN does not allow dynamic memory allocation
C     the local data storage must be set at compile time.  As
C     a result the local data buffer might become full during
C     an execution of your program.  If data for an item needs
C     to be fetched from the kernel pool once the buffer has become
C     full, you must either repeatedly call the kernel pool to fetch
C     the new data or overwrite some of the data in your local buffer.
C
C     This routine helps with the decisions of which items to
C     overwrite.  In addition it always moves the last requested
C     item to the head of the index used for searching the buffered
C     ID's.  In this way if the same item is needed many times
C     in succession, there will be very little overhead associated
C     with finding the item.  Thus the routine spends its time
C     in computing the desired quantities, not in looking up the
C     parameters needed for the computation.
C
C     Below is a fragment of code that illustrates how this routine
C     should be used. In the situation outlined above.  We'll suppose
C     that we are fetching MDLSIZ double precision numbers from the
C     kernel pool that are associated with the item
C
C         'BODYid_MAGMODEL'
C
C     And that we are computing something with this model data.
C
C
C        INTEGER               MDLSIZ
C        PARAMETER           ( MDLSIZ = xxxxxx )
C
C        We'll create room to buffer this data for 8 bodies.
C
C
C        INTEGER               PSIZE
C        PARAMETER           ( PSIZE = 8 )
C
C
C        The ID's we shall be using are 1-dimensional. They are body
C        ID's for planets or and their satellites.
C
C        INTEGER               IDSZ
C        PARAMETER           ( IDSZ = 1 )
C
C        INTEGER               AT
C        INTEGER               DIM
C        INTEGER               LIST   (   IDSZ,  PSIZE        )
C        INTEGER               POOL   (      2,  LBPOOL:PSIZE )
C
C        DOUBLE PRECISION      MAGMDL ( MDLSIZ,  PSIZE        )
C        DOUBLE PRECISION      MODEL  ( MDLSIZ                )
C
C        LOGICAL               FIRST
C        LOGICAL               PRESNT
C
C        SAVE
C
C        DATA                  FIRST / .TRUE. /
C
C
C        The block below handles initializing the linked list pool.
C
C        IF ( FIRST ) THEN
C
C           FIRST = .FALSE.
C
C           CALL LNKINI ( PSIZE, POOL )
C
C        END IF
C
C        See if the data associated with ID has already been
C        buffered.
C
C        CALL LOCATI ( ID, IDSZ, LIST, POOL, AT, PRESNT )
C
C        IF ( .NOT. PRESNT ) THEN
C
C           The data has not yet been buffered, look it up.  Normally
C           you might want to check to see if the data exists and
C           handle things appropriately if it doesn't but this is just
C           to give you the idea...
C
C           CALL BODVCD ( ID, 'MAGMODEL', 3, DIM, MAGMDL ( 1, AT ) )
C
C        END IF
C
C        Put the model data into the array MODEL for ease of
C        reading the rest of the code.
C
C        CALL MOVED ( MAGMDL(1,AT), MDLSIZ, MODEL )
C
C
C        Now do whatever processing is needed ....
C
C     There are a few things to note about the code fragment above.
C     First the handling of the buffering of data was very easy.
C     Second, if this routine is called again using the same ID,
C     the buffer will already contain the needed model.  Moreover
C     the routine LOCATI will return very quickly because the
C     ID will already be at the head of the list indexed by POOL.
C
C     You can also easily add an entry point to this routine that
C     will force it to look up data from the kernel pool on the
C     next call.  All that needs to be done is re-initialize the
C     linked list pool.
C
C        ENTRY DOLOOK
C
C        CALL LNKINI ( PSIZE, POOL )
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C      N.J. Bachman    (JPL)
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 24-OCT-2005 (NJB)
C
C        Header update:  changed reference to BODVAR to reference
C        to BODVCD.
C
C-    SPICELIB Version 1.0.0, 9-APR-1997 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Locate an item in a linked list indexed list of items
C     Remove least recently used item buffering
C
C-&
 
 
C
C     Spicelib functions
C
 
      INTEGER               LNKNFN
      INTEGER               LNKSIZ
 
C
C     Linked list parameters
C
      INTEGER               NEXT
      PARAMETER           ( NEXT = 1 )
 
      INTEGER               PREV
      PARAMETER           ( PREV = 2 )
 
C
C     Local Variables.
C
 
      INTEGER               LAST
      INTEGER               NFREE
      INTEGER               PSIZE
      INTEGER               I
      INTEGER               HEAD
      INTEGER               NEW
 
      LOGICAL               MORE
      LOGICAL               SAME
 
 
 
      CALL CHKIN ( 'LOCATI' )
C
C     We begin by looking through the list of items at our disposal.
C     One way or the other we will need the number of free nodes
C     in the linked list.
C
      NFREE = LNKNFN(POOL)
      PSIZE = LNKSIZ(POOL)
 
      IF ( NFREE .EQ. PSIZE ) THEN
C
C        There's nothing in the list so far. Allocate a
C        node and begin a list.
C
         CALL LNKAN ( POOL, AT )
 
         DO I = 1, IDSZ
            LIST(I,AT) = ID(I)
         END DO
 
         PRESNT = .FALSE.
 
         CALL CHKOUT ( 'LOCATI' )
         RETURN
 
      END IF
 
      IF ( AT .LE. 0 .OR. AT .GT. PSIZE ) THEN
 
         CALL SETMSG ( 'The input value for the head of the ID '
     .   //            'address linked list is out of bounds. '
     .   //            'It should be between 0 and #. The value '
     .   //            'supplied was #.' )
         CALL ERRINT( '#', PSIZE )
         CALL ERRINT( '#', AT    )
         CALL SIGERR ( 'SPICE(ADDRESSOUTOFBOUNDS)'  )
         CALL CHKOUT ( 'LOCATI' )
         RETURN
 
      END IF
 
C
C     If we are still here then there is actually something in
C     the list.  We begin at start and traverse the list.
C     Since we are unlikely to ever have large ID's (their purpose
C     after all is to be a label for something more complex) we
C     will handle the cases where IDSZ is 1 or 2 as special
C     cases since the tests for equality are a lot easier.
C
      SAME = .FALSE.
 
      HEAD =  AT
 
 
 
      IF ( IDSZ .EQ. 1 ) THEN
 
         SAME = ID(1) .EQ. LIST(1,AT)
         MORE = AT .GT. 0 .AND. .NOT. SAME
 
         DO WHILE ( MORE )
 
            AT   =  POOL ( NEXT, AT )
 
            IF ( AT .GT. 0 ) THEN
               SAME =  ID(1) .EQ. LIST(1,AT)
               MORE = .NOT. SAME
            ELSE
               MORE = .FALSE.
            END IF
 
         END DO
 
      ELSE IF ( IDSZ .EQ. 2 ) THEN
 
         SAME =       ID(1) .EQ. LIST(1,AT)
     .          .AND. ID(2) .EQ. LIST(2,AT)
 
         MORE = AT .GT. 0 .AND. .NOT. SAME
 
         DO WHILE ( MORE )
 
            AT  = POOL ( NEXT, AT )
 
            IF ( AT .GT. 0 ) THEN
               IF (      ID(1) .EQ. LIST(1,AT) ) THEN
                  SAME = ID(2) .EQ. LIST(2,AT)
                  MORE = .NOT. SAME
               END IF
            ELSE
               MORE = .FALSE.
            END IF
 
         END DO
 
      ELSE
 
         I      =  1
         SAME   = .TRUE.
 
         DO WHILE ( SAME .AND. I .LT. IDSZ )
            SAME = SAME .AND. ID(I) .EQ. LIST(I,AT)
            I    = I + 1
         END DO
 
         MORE = AT .GT. 0 .AND. .NOT. SAME
 
         DO WHILE ( MORE )
 
            AT = POOL ( NEXT, AT )
 
            IF ( AT .GT. 0 ) THEN
               I      =  1
               SAME   = .TRUE.
 
               DO WHILE ( SAME .AND. I .LT. IDSZ )
                  SAME = SAME .AND. ID(I) .EQ. LIST(I,AT)
                  I    = I + 1
               END DO
 
               MORE = .NOT. SAME
            ELSE
               MORE = .FALSE.
            END IF
 
         END DO
 
      END IF
 
C
C     The hunting is over either we found it or we need to
C     allocate space to put this ID into storage.
C
      IF ( SAME ) THEN
 
         PRESNT = .TRUE.
         LAST   =  POOL(PREV,AT)
 
 
C
C        If AT is not already at the head of the list, we
C        move this node to the front of the list.
C
         IF ( LAST .GT. 0 ) THEN
            CALL LNKXSL ( AT, AT,   POOL )
            CALL LNKILB ( AT, HEAD, POOL )
         END IF
 
         CALL CHKOUT ( 'LOCATI' )
         RETURN
 
      END IF
 
C
C     If we got to this point, we traversed the entire linked
C     list and did not find a matching ID.  AT is negative
C     and -AT points to the head of the list.
C
      PRESNT =  .FALSE.
C
C     We'll put it in the list. First see if there are any free nodes.
C
      IF ( NFREE .GT. 0 ) THEN
C
C        Allocate a free node and put our ID at the NEW address.
C
         CALL LNKAN ( POOL, NEW )
         DO I = 1, IDSZ
            LIST(I,NEW) = ID(I)
         END DO
C
C        Put the new node at the head of the linked list.
C
         CALL LNKILB ( NEW, HEAD, POOL )
         AT =  NEW
 
      ELSE
 
C
C        The last item in the list is pointed to as being the
C        previous item to the head of the list. But we have to
C        change the sign to get a legitimate address.  Overwrite
C        the ID information in this last slot of the list.
C
         LAST = -POOL(PREV,HEAD)
 
         DO I = 1, IDSZ
            LIST(I,LAST) = ID(I)
         END DO
C
C        Extract the last item as a sublist and insert it before
C        the current head of the list.
C
         CALL LNKXSL ( LAST,   LAST,   POOL )
         CALL LNKILB ( LAST,   HEAD,   POOL )
 
         AT    = LAST
 
      END IF
 
      CALL CHKOUT ( 'LOCATI' )
      RETURN
 
      END
