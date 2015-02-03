C$Procedure      DASSDR ( DAS, segregate data records )
 
      SUBROUTINE DASSDR ( HANDLE )
 
C$ Abstract
C
C     Segregate the data records in a DAS file into clusters, using
C     one cluster per data type present in the file.
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
C     DAS
C
C$ Keywords
C
C     DAS
C     FILES
C     ORDER
C     SORT
C
C$ Declarations
 
      INTEGER               HANDLE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   DAS file handle.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of a DAS file opened for writing.
C
C$ Detailed_Output
C
C     None.  See $Particulars for a description of the effect of this
C     routine.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input file handle is invalid, the error will be
C         diagnosed by routines called by this routine.
C
C     2)  If a Fortran read attempted by this routine fails, the
C         error will be diagnosed by routines called by this routine.
C         The state of the DAS file undergoing re-ordering will be
C         indeterminate.
C
C     3)  If a Fortran write attempted by this routine fails, the
C         error will be diagnosed by routines called by this routine.
C         The state of the DAS file undergoing re-ordering will be
C         indeterminate.
C
C     4)  If any other I/O error occurs during the re-arrangement of
C         the records in the indicated DAS file, the error will be
C         diagnosed by routines called by this routine.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     Normally, there should be no need for routines outside of
C     SPICELIB to call this routine.
C
C     The effect of this routine is to re-arrange the data records
C     in a DAS file so that the file contains a single cluster for
C     each data type present in the file:  in the general case, there
C     will be a single cluster of each of the integer, double
C     precision, and character data types.
C
C     The relative order of data records of a given type is not
C     affected by this re-ordering.  After the re-ordering, the DAS
C     file contains a single directory record that has one descriptor
C     for each cluster.  After that point, the order in the file of the
C     sets of data records of the various data types will be:
C
C        +-------+
C        |  CHAR |
C        +-------+
C        |  DP   |
C        +-------+
C        |  INT  |
C        +-------+
C
C     Files that contain multiple directory records will have all but
C     the first directory record moved to the end of the file when the
C     re-ordering is complete.  These records are not visible to the
C     DAS system and will be overwritten if data is subsequently added
C     to the DAS file.
C
C     The purpose of segregating a DAS file's data records into three
C     clusters is to make read access more efficient:  when a DAS file
C     contains a single directory with at most three cluster type
C     descriptors, mapping logical to physical addresses can be done
C     in constant time.
C
C$ Examples
C
C     1)  Segregate data records in a DAS file designated by
C         HANDLE:
C
C            CALL DASSDR ( HANDLE )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer (JPL)
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.1 19-DEC-1995 (NJB)
C
C        Corrected title of permuted index entry section.
C
C-    EKLIB Version 2.0.0, 17-NOV-1993 (KRG)
C
C        Added test of FAILED after each DAS call, or sequence of calls,
C        which returns immediately if FAILED is true. This fixes a bug
C        where DASOPS signals an error and then DASSDR has a
C        segmentation fault.
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C-    EKLIB Version 1.2.0, 07-OCT-1993 (NJB) (HAN) (MJS)
C
C        Bug fix:  call to CLEARD replaced with call to
C        CLEARI.
C
C-    EKLIB Version 1.1.0, 08-JUL-1993 (NJB) (MJS)
C
C        Bug fix:  extraneous commas removed from argument lists
C        in calls to DASADI.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     segregate the data records in a DAS file
C
C-&
 
 
C$ Revisions
C
C-    EKLIB Version 2.0.0, 17-NOV-1993 (KRG)
C
C        Added test of failed after each DAS call, or sequence of calls,
C        which returns immediately if FAILED is true. This fixes a bug
C        where DASOPS signals an error and then DASSDR has a
C        segmentation fault.
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C-    EKLIB Version 1.2.0, 07-OCT-1993 (NJB) (HAN) (MJS)
C
C        Bug fix:  call to CLEARD replaced with call to
C        CLEARI.
C
C-    EKLIB Version 1.1.0, 08-JUL-1993 (NJB)
C
C        Bug fix:  extraneous commas removed from argument lists
C        in calls to DASADI.  This bug had no visible effect on
C        VAX and Sun systems, but generated a compile error under
C        Lahey Fortran.
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               SUMAI
 
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               NWC
      PARAMETER           ( NWC   = 1024 )
 
      INTEGER               NWD
      PARAMETER           ( NWD   =  128 )
 
      INTEGER               NWI
      PARAMETER           ( NWI   =  256 )
 
C
C     Data type parameters
C
      INTEGER               CHAR
      PARAMETER           ( CHAR   =  1  )
 
      INTEGER               DP
      PARAMETER           ( DP     =  2  )
 
      INTEGER               INT
      PARAMETER           ( INT    =  3  )
 
      INTEGER               DIR
      PARAMETER           ( DIR    =  4  )
 
C
C     Directory pointer locations (backward and forward):
C
      INTEGER               BWDLOC
      PARAMETER           ( BWDLOC = 1 )
 
      INTEGER               FWDLOC
      PARAMETER           ( FWDLOC = 2 )
 
C
C     Directory address range location base
C
      INTEGER               RNGBAS
      PARAMETER           ( RNGBAS  =  2  )
 
 
C
C     Location of first type descriptor
C
      INTEGER               BEGDSC
      PARAMETER           ( BEGDSC = 9 )
 
C
C     Local variables
C
      CHARACTER*(NWC)       CREC
      CHARACTER*(NWC)       SAVEC
 
      DOUBLE PRECISION      DREC   ( NWD )
      DOUBLE PRECISION      SAVED  ( NWD )
 
      INTEGER               BASE
      INTEGER               COUNT  ( 4 )
      INTEGER               DEST
      INTEGER               DRBASE
      INTEGER               FREE
      INTEGER               I
      INTEGER               IREC   ( NWI    )
      INTEGER               J
      INTEGER               LASTLA ( 3 )
      INTEGER               LASTRC ( 3 )
      INTEGER               LASTWD ( 3 )
      INTEGER               LOC
      INTEGER               LREC
      INTEGER               LTYPE
      INTEGER               LWORD
      INTEGER               MAXADR
      INTEGER               MINADR
      INTEGER               N
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               OFFSET
      INTEGER               POS
      INTEGER               PRVTYP
      INTEGER               RECNO
      INTEGER               SAVEI  ( NWI    )
      INTEGER               SAVTYP
      INTEGER               SCRHAN
      INTEGER               START
      INTEGER               TOTAL
      INTEGER               TYPE
      INTEGER               UNIT
 
      LOGICAL               MORE
 
 
 
C
C     Saved variables
C
 
C
C     NEXT and PREV map the DAS data type codes to their
C     successors and predecessors, respectively.
C
      INTEGER               NEXT   ( 3 )
      SAVE                  NEXT
 
      INTEGER               PREV   ( 3 )
      SAVE                  PREV
 
C
C     Initial values
C
      DATA                  NEXT   /  2,   3,   1  /
      DATA                  PREV   /  3,   1,   2  /
 
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASSDR' )
      END IF
 
C
C     Before starting, make sure that this DAS file is open for
C     writing.
C
      CALL DASSIH ( HANDLE, 'WRITE' )
 
C
C     Get the logical unit for this file.
C
      CALL DASHLU ( HANDLE, UNIT )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASSDR' )
         RETURN
      END IF
 
C
C     Write out any buffered records that belong to the file.
C
      CALL DASWBR ( HANDLE )
 
      IF ( FAILED () ) THEN
         CALL CHKOUT ( 'DASSDR' )
         RETURN
      END IF
 
C
C     We're going to re-order the physical records in the DAS file,
C     starting with the first record after the first directory.
C     The other directory records are moved to the end of the file
C     as a result of the re-ordering.
C
C     The re-ordering algorithm is based on that used in the REORDx
C     routines.  To use this algorithm, we'll build an order vector
C     for the records to be ordered; we'll construct this order vector
C     in a scratch DAS file.  First, we'll traverse the directories
C     to build up a sort of inverse order vector that tells us the
C     final destination and data type of each data record;  from this
C     inverse vector we can easily build a true order vector.  The
C     cycles of the true order vector can be traversed without
C     repetitive searching, and with a minimum of assignment of the
C     contents of data records to temporary variables.
C
 
C
C     Allocate a scratch DAS file to keep our vectors in.
C
      CALL DASOPS ( SCRHAN )
 
      IF ( FAILED () ) THEN
         CALL CHKOUT ( 'DASSDR' )
         RETURN
      END IF
 
C
C     Now build up our `inverse order vector'.   This array is an
C     inverse order vector only in loose sense:  it actually consists
C     of an integer array that contains a sequence of pairs of integers,
C     the first of which indicates a data type, and the second of which
C     is an ordinal number.  There is one pair for each data record in
C     the file.  The ordinal number gives the ordinal position of the
C     record described by the number pair, relative to the other records
C     of the same type.  Directory records are considered to have type
C     `directory', which is represented by the code DIR.
C
C     We also must maintain a count of records of each type.
C
      CALL CLEARI ( 4, COUNT )
 
C
C     Get the file summary for the DAS file to be segregated.
C
      CALL DASHFS ( HANDLE,
     .              NRESVR,
     .              NRESVC,
     .              NCOMR,
     .              NCOMC,
     .              FREE,
     .              LASTLA,
     .              LASTRC,
     .              LASTWD )
 
      IF ( FAILED () ) THEN
         CALL CHKOUT ( 'DASSDR' )
         RETURN
      END IF
C
C     Find the record and word positions LREC and LWORD of the last
C     descriptor in the file, and also find the type of the descriptor
C     LTYPE.
C
      CALL MAXAI  ( LASTRC,  3,  LREC,  LOC )
      LWORD = 0
 
      DO I = 1, 3
 
         IF (       ( LASTRC(I) .EQ. LREC  )
     .        .AND. ( LASTWD(I) .GT. LWORD )  ) THEN
 
            LWORD = LASTWD(I)
            LTYPE = I
 
         END IF
 
      END DO
 
C
C     The first directory starts after the last comment record.
C
      RECNO  =  NRESVR + NCOMR + 2
 
 
      DO WHILE (  ( RECNO .LE. LREC ) .AND. ( RECNO .GT. 0 )  )
C
C        Read the directory record.
C
         CALL DASRRI ( HANDLE, RECNO, 1, NWI, IREC )
 
         IF ( FAILED () ) THEN
             CALL CHKOUT ( 'DASSDR' )
           RETURN
         END IF
C
C        Increment the directory count.
C
         COUNT(DIR)  =  COUNT(DIR) + 1
 
C
C        Add the data type (`directory') and count (1) of the current
C        record to the inverse order vector.
C
         CALL DASADI ( SCRHAN,  1,  DIR        )
         CALL DASADI ( SCRHAN,  1,  COUNT(DIR) )
 
         IF ( FAILED () ) THEN
            CALL CHKOUT ( 'DASSDR' )
            RETURN
         END IF
C
C        Set up our `finite state machine' that tells us the data
C        types of the records described by the last read directory.
C
         TYPE    =  IREC ( BEGDSC )
         PRVTYP  =  PREV ( TYPE   )
 
C
C        Now traverse the directory and update the inverse order
C        vector based on the descriptors we find.
C
         MORE  =  .TRUE.
         I     =   BEGDSC + 1
 
 
         DO WHILE ( MORE )
C
C           Obtain the count for the current descriptor.
C
            N  =  ABS (  IREC(I)  )
 
C
C           Update our inverse order vector to describe the positions
C           of the N records described by the current descriptor.
C
            DO J  =  1, N
 
               CALL DASADI (  SCRHAN,  1,  TYPE           )
               CALL DASADI (  SCRHAN,  1,  COUNT(TYPE)+J  )
 
               IF ( FAILED () ) THEN
                  CALL CHKOUT ( 'DASSDR' )
                  RETURN
               END IF
 
            END DO
 
C
C           Adjust the count of records of data type TYPE.
C
            COUNT(TYPE)  =  COUNT(TYPE)   +   N
 
C
C           Find the next type.
C
            I = I + 1
 
            IF (      (  I     .GT. NWI                        )
     .           .OR. ( (RECNO .EQ. LREC) .AND. (I .GT. LWORD) )  ) THEN
 
               MORE = .FALSE.
 
            ELSE
 
               IF ( IREC(I) .GT. 0 ) THEN
                  TYPE = NEXT(TYPE)
 
               ELSE IF ( IREC(I) .LT. 0 ) THEN
                  TYPE = PREV(TYPE)
 
               ELSE
                  MORE = .FALSE.
 
               END IF
 
            END IF
 
         END DO
 
C
C        The forward pointer in this directory tells us where the
C        next directory record is.  When there are no more directory
C        records, this pointer will be zero.
C
         RECNO = IREC ( FWDLOC )
 
      END DO
 
C
C     At this point, the inverse order vector is set up.  The array
C     COUNT contains counts of the number of records of each type we've
C     seen.  Set TOTAL to the total number of records that we've going
C     to permute.
C
      TOTAL =  SUMAI ( COUNT, 4 )
 
C
C     The next step is to build a true order vector.  Let BASE be
C     the base address for the order vector; this address is the
C     last logical address of the inverse order vector.
C
      BASE  =  2 * TOTAL
 
C
C     We'll store the actual order vector in locations BASE + 1
C     through BASE + TOTAL.  In addition, we'll build a parallel array
C     that contains, for each element of the order vector, the type of
C     data corresponding to that element.  This type vector will
C     reside in locations BASE + TOTAL + 1 through BASE + 2*TOTAL.
C
C     Before setting the values of the order vector and its parallel
C     type vector, we'll allocate space in the scratch DAS file by
C     zeroing out the locations we plan to use.  After this, locations
C     BASE+1 through BASE + 2*TOTAL can be written to in random access
C     fashion using DASUDI.
C
C
      DO I = 1, 2*TOTAL
 
         CALL DASADI ( SCRHAN, 1, 0 )
 
      END DO
 
      IF ( FAILED () ) THEN
         CALL CHKOUT ( 'DASSDR' )
         RETURN
      END IF
C
C     We note that the way to construct the inverse of a permutation
C     SIGMA in a single loop is suggested by the relation
C
C             -1
C        SIGMA   (  SIGMA(I)  )   =   I
C
C     We'll use this method.  In our case, our order vector plays
C     the role of
C
C             -1
C        SIGMA
C
C     and the `inverse order vector' plays the role of SIGMA.  We'll
C     exclude the first directory from the order vector, since it's
C     an exception:  we wish to reserve this record.  Since the first
C     element of the order vector (logically) contains the index 1, we
C     can ignore it.
C
C
      DO I = 2, TOTAL
 
         CALL DASRDI ( SCRHAN,  (2*I)-1,  (2*I)-1,  TYPE  )
         CALL DASRDI ( SCRHAN,   2*I,      2*I,     DEST  )
 
         IF ( FAILED () ) THEN
            CALL CHKOUT ( 'DASSDR' )
            RETURN
         END IF
C
C        Set DEST to the destination location, measured as an offset
C        from the last comment record, of the Ith record by adding
C        on the count of the predecessors of the block of records of
C        TYPE.
C
         DO J = 1, 3
 
            IF ( TYPE .GT. J ) THEN
               DEST = DEST + COUNT(J)
            END IF
 
         END DO
 
C
C        The destination offset of each record should be incremented to
C        allow room for the first directory record.  However, we don't
C        need to do this for directory records; they'll already have
C        this offset accounted for.
C
         IF ( TYPE .NE. DIR ) THEN
            DEST = DEST + 1
         END IF
 
C
C        The value of element DEST of the order vector is I.
C        Write this value to location BASE + DEST.
C
         CALL DASUDI ( SCRHAN, BASE+DEST, BASE+DEST, I )
C
C        We want the ith element of the order vector to give us the
C        number of the record to move to position i (offset from the
C        last comment record),  but we want the corresponding element
C        of the type array to give us the type of the record currently
C        occupying position i.
C
         CALL DASUDI ( SCRHAN, BASE+I+TOTAL, BASE+I+TOTAL, TYPE )
 
         IF ( FAILED () ) THEN
            CALL CHKOUT ( 'DASSDR' )
            RETURN
         END IF
 
      END DO
 
C
C     Ok, here's what we've got in the scratch file that's still of
C     interest:
C
C        -- In integer logical addresses BASE + 1 : BASE + TOTAL,
C           we have an order vector.  The Ith element of this
C           vector indicates the record that should be moved to
C           location DRBASE + I in the DAS file we're re-ordering,
C           where DRBASE is the base address of the data records
C           (the first directory record follows the record having this
C           index).
C
C
C        -- In integer logical addresses BASE + TOTAL + 1  :  BASE +
C           2*TOTAL, we have data type indicators for the records to
C           be re-ordered.  The type for the Ith record in the file,
C           counted from the last comment record, is located in logical
C           address BASE + TOTAL + I.
C
C
      DRBASE  =  NRESVR + NCOMR + 1
 
C
C     As we traverse the order vector, we flip the sign of elements
C     we've accessed, so that we can tell when we encounter an element
C     of a cycle that we've already traversed.
C
C     Traverse the order vector.  The variable START indicates the
C     first element to look at.  Ignore the first element; it's a
C     singleton cycle.
C
C
      START = 2
 
      DO WHILE ( START .LT. TOTAL )
C
C        Traverse the current cycle of the order vector.
C
C        We `make a hole' in the file by saving the record in position
C        START, then we traverse the cycle in reverse order, filling in
C        the hole at the ith position with the record whose number is
C        the ith element of the order vector.  At the end, we deposit
C        the saved record into the `hole' left behind by the last
C        record we moved.
C
C        We're going to read and write records to and from the DAS file
C        directly, rather than going through the buffering system.
C        This will allow us to avoid any untoward interactions between
C        the buffers for different data types.
C
         CALL DASRDI ( SCRHAN,
     .                 BASE + TOTAL + START,
     .                 BASE + TOTAL + START,
     .                 SAVTYP                )
 
         CALL DASRDI ( SCRHAN,  BASE + START,  BASE + START,  OFFSET )
 
C
C        Save the record at the location DRBASE + START.
C
         IF ( SAVTYP .EQ. CHAR ) THEN
 
            CALL DASIOC ( 'READ', UNIT, DRBASE+START, SAVEC )
 
         ELSE IF ( SAVTYP .EQ. DP ) THEN
 
            CALL DASIOD ( 'READ', UNIT, DRBASE+START, SAVED )
 
         ELSE
 
            CALL DASIOI ( 'READ', UNIT, DRBASE+START, SAVEI )
 
         END IF
 
         IF ( FAILED () ) THEN
            CALL CHKOUT ( 'DASSDR' )
            RETURN
         END IF
C
C        Let I be the index of the record that we are going to move
C        data into next.  I is an offset from the last comment record.
C
         I  =  START
 
         DO WHILE ( OFFSET .NE. START )
C
C           Mark the order vector element by writing its negative
C           back to the location it came from.
C
            CALL DASUDI ( SCRHAN,  BASE+I,  BASE+I,  -OFFSET )
C
C           Move the record at location
C
C              DRBASE + OFFSET
C
C           to location
C
C              DRBASE + I
C
C           There is no need to do anything about the corresponding
C           elements of the type vector; we won't need them again.
C
C           The read and write operations, as well as the temporary
C           record required to perform the move, are dependent on the
C           data type of the record to be moved.
C
            CALL DASRDI ( SCRHAN,
     .                    BASE + TOTAL + OFFSET,
     .                    BASE + TOTAL + OFFSET,
     .                    TYPE                    )
 
            IF ( FAILED () ) THEN
               CALL CHKOUT ( 'DASSDR' )
               RETURN
            END IF
C
C           Only pick records up if we're going to put them down in
C           a location other than their original one.
C
            IF ( I .NE. OFFSET ) THEN
 
               IF ( TYPE .EQ. CHAR ) THEN
 
                  CALL DASIOC ( 'READ',  UNIT, DRBASE+OFFSET, CREC )
                  CALL DASIOC ( 'WRITE', UNIT, DRBASE+I,      CREC )
 
               ELSE IF ( TYPE .EQ. DP ) THEN
 
                  CALL DASIOD ( 'READ',  UNIT, DRBASE+OFFSET, DREC )
                  CALL DASIOD ( 'WRITE', UNIT, DRBASE+I,      DREC )
 
               ELSE
 
                  CALL DASIOI ( 'READ',  UNIT, DRBASE+OFFSET, IREC )
                  CALL DASIOI ( 'WRITE', UNIT, DRBASE+I,      IREC )
 
               END IF
 
               IF ( FAILED () ) THEN
                  CALL CHKOUT ( 'DASSDR' )
                  RETURN
               END IF
 
            END IF
 
C
C           OFFSET is the index of the next order vector element to
C           look at.
C
            I  =  OFFSET
 
            CALL DASRDI ( SCRHAN, BASE+I,       BASE+I,       OFFSET )
            CALL DASRDI ( SCRHAN, BASE+I+TOTAL, BASE+I+TOTAL, TYPE   )
 
            IF ( FAILED () ) THEN
               CALL CHKOUT ( 'DASSDR' )
               RETURN
            END IF
 
         END DO
 
C
C        The last value of I is the location in the cycle that element
C        START followed.  Therefore, the saved record corresponding
C        to index START should be written to this location.
C
         IF ( SAVTYP .EQ. CHAR ) THEN
 
            CALL DASIOC ( 'WRITE',  UNIT,  DRBASE+I,  SAVEC )
 
         ELSE IF ( SAVTYP .EQ. DP ) THEN
 
            CALL DASIOD ( 'WRITE',  UNIT,  DRBASE+I,  SAVED )
 
         ELSE
 
            CALL DASIOI ( 'WRITE',  UNIT,  DRBASE+I,  SAVEI )
 
         END IF
C
C        Mark the order vector element by writing its negative
C        back to the location it came from.
C
         CALL DASUDI ( SCRHAN,  BASE+I,  BASE+I, -START )
 
         IF ( FAILED () ) THEN
            CALL CHKOUT ( 'DASSDR' )
            RETURN
         END IF
C
C        Update START so that it points to the first element of a cycle
C        of the order vector that has not yet been traversed.  This will
C        be the first positive element of the order vector in a location
C        indexed higher than the current value of START.  Note that
C        this way of updating START guarantees that we don't have to
C        backtrack to find an element in the next cycle.
C
 
         OFFSET = -1
 
         DO WHILE ( ( OFFSET .LT. 0 ) .AND. ( START .LT. TOTAL ) )
 
            START  =  START  + 1
 
            CALL DASRDI (  SCRHAN,  BASE+START,  BASE+START,  OFFSET  )
 
            IF ( FAILED () ) THEN
               CALL CHKOUT ( 'DASSDR' )
               RETURN
            END IF
 
         END DO
 
C
C        At this point, START is the index of an element in the order
C        vector that belongs to a cycle where no routine has gone
C        before, or else START is the last index in the order vector,
C        in which case we're done.
C
 
      END DO
 
C
C     At this point, the records in the DAS are organized as follows:
C
C        +----------------------------------+
C        |           File record            |  ( 1 )
C        +----------------------------------+
C        |         Reserved records         |  ( 0 or more )
C        |                                  |
C        +----------------------------------+
C        |          Comment records         |  ( 0 or more )
C        |                                  |
C        |                                  |
C        +----------------------------------+
C        |      First directory  record     |  ( 1 )
C        +----------------------------------+
C        |      Character data records      |  ( 0 or more )
C        |                                  |
C        +----------------------------------+
C        |   Double precision data records  |  ( 0 or more )
C        |                                  |
C        +----------------------------------+
C        |       Integer data records       |  ( 0 or more )
C        |                                  |
C        +----------------------------------+
C        |   Additional directory records   |  ( 0 or more )
C        |                                  |
C        +----------------------------------+
C
C
C     Not all of the indicated components must be present; only the
C     file record and first directory record will exist in all cases.
C     The `additional directory records' at the end of the file serve
C     no purpose; if more data is appended to the file, they will be
C     overwritten.
C
C     The last step in preparing the file is to fill in the first
C     directory record with the correct information, and to update
C     the file summary.
C
C
      RECNO  =  DRBASE + 1
 
      CALL CLEARI ( NWI, IREC )
 
C
C     Set the logical address ranges in the directory record, for each
C     data type.
C
 
      DO TYPE = 1, 3
 
         MAXADR  =  LASTLA(TYPE)
 
         IF ( MAXADR .GT. 0 ) THEN
            MINADR = 1
         ELSE
            MINADR = 0
         END IF
 
         IREC (  RNGBAS + (2*TYPE) - 1  )  =  MINADR
         IREC (  RNGBAS + (2*TYPE)      )  =  MAXADR
 
      END DO
 
C
C     Set the descriptors in the directory.  Determine which type
C     comes first:  the order of priority is character, double
C     precision, integer.
C
      POS = BEGDSC
 
      DO TYPE = 1, 3
 
         IF (  LASTLA(TYPE)  .GT.  0  ) THEN
 
            IF ( POS .EQ. BEGDSC ) THEN
C
C              This is the first type for which any data is present.
C              We must enter a type code at position BEGDSC in the
C              directory, and we must enter a count at position
C              BEGDSC+1.
C
               IREC   ( BEGDSC       )  =  TYPE
               IREC   ( BEGDSC + 1   )  =  COUNT(TYPE)
               LASTRC ( TYPE )          =  RECNO
               LASTWD ( TYPE )          =  BEGDSC + 1
               POS                      =  POS + 2
               PRVTYP                   =  TYPE
 
            ELSE
C
C              Place an appropriately signed count at location POS in
C              the directory.
C
               IF ( TYPE .EQ. NEXT(PRVTYP) ) THEN
                  IREC ( POS )  =   COUNT(TYPE)
               ELSE
                  IREC ( POS )  =  -COUNT(TYPE)
               END IF
 
               LASTRC ( TYPE )  =  RECNO
               LASTWD ( TYPE )  =  POS
               POS              =  POS + 1
               PRVTYP           =  TYPE
 
            END IF
 
         END IF
 
      END DO
 
C
C     Since we've done away with all but the first directory, the first
C     free record is decremented by 1 less than the directory count.
C
      FREE  =  FREE  -  COUNT( DIR )  +  1
 
C
C     Write out the new directory record.  Don't use the DAS buffered
C     write mechanism; this could trash the file by dumping buffered
C     records in the wrong places.
C
      CALL DASIOI ( 'WRITE',  UNIT,  RECNO,  IREC  )
C
C     Write out the updated file summary.
C
      CALL DASUFS ( HANDLE,
     .              NRESVR,
     .              NRESVC,
     .              NCOMR,
     .              NCOMC,
     .              FREE,
     .              LASTLA,
     .              LASTRC,
     .              LASTWD )
C
C     Clean up the DAS data buffers:  we don't want buffered scratch
C     file records hanging around there.  Then get rid of the scratch
C     file.
C
      CALL DASWBR ( SCRHAN )
      CALL DASLLC ( SCRHAN )
 
 
      CALL CHKOUT ( 'DASSDR' )
      RETURN
      END
