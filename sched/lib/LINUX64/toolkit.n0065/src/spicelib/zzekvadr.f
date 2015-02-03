C$Procedure  ZZEKVADR  ( Compute row vector address )
 
      SUBROUTINE ZZEKVADR ( NJRS, BASES, RWVIDX, RWVBAS, SGVBAS )
 
 
C$ Abstract
C
C     Given a union of EK join row sets and a row vector index,
C     compute the EK scratch area base address of the row vector having
C     the specified index.  Also return the base address of the row
C     vector's parent segment vector.
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
C     EK
C
C$ Keywords
C
C     EK
C     UTILITY
C
C$ Declarations
 
      INTEGER               NJRS
      INTEGER               BASES  ( * )
      INTEGER               RWVIDX
      INTEGER               RWVBAS
      INTEGER               SGVBAS
 
      INTEGER               MXJOIN
      PARAMETER           ( MXJOIN = 10 )
 
      INTEGER               MXJRS
      PARAMETER           ( MXJRS  = 200 )
 
C$ Brief_I/O
C
C     Variable  I/O  Entry points
C     --------  ---  --------------------------------------------------
C     NJRS       I   ZZEKVSET
C     BASES      I   ZZEKVSET
C     RWVIDX     I   ZZEKVACL
C     RWVBAS     O   ZZEKVACL
C     SGVBAS     O   ZZEKVACL
C     MXJOIN     P   Maximum number of tables that can be joined.
C     MXJRS      P   Maximum number of join row sets allowed in union.
C
C$ Detailed_Input
C
C     See the entry points for a discussion of their arguments.
C
C$ Detailed_Output
C
C     See the entry points for a discussion of their arguments.
C
C$ Parameters
C
C     MXJOIN         is the maximum number of tables that can be joined.
C
C     MXJRS          is the maximum number of join row sets allowed in
C                    in the input union identified by BASES and NJRS.
C
C$ Exceptions
C
C     1)  This is an umbrella routine which contains declarations
C         for its entry points.  This routine should never be called
C         directly.  If it is, the error SPICE(BOGUSENTRY) will be
C         signalled.
C
C     See the entry points for discussions of the exceptions specific
C     to those entry points.
C
C$ Files
C
C     1)  This routine uses the EK scratch area, which employs a scratch
C         DAS file.
C
C$ Particulars
C
C     In the course of query resolution, the EK system builds a set of
C     data structures called `join row sets' that represent the rows
C     that satisfy the query constraints.  These rows belong to a table
C     formed by taking the Cartesian product of the tables in the FROM
C     clause of the query.  One join row set is formed for each
C     conjunction of join constraints; the total number of join row sets
C     is equal to the number of conjunctions of join constraints in
C     the query.  Join row sets are described below.
C
C     This group of routines allows the EK system to view the rows
C     matching a query as a sequence of vectors, where each vector is an
C     n-tuple of row numbers designating rows in segments of the
C     Cartesian product of tables specified in the input query.  These
C     vectors are called `row vectors'.  Each row vector also points to
C     a vector of segments that contain the rows represented by the row
C     vector.
C
C     These routines centralize the calculations needed to locate the
C     nth row vector.
C
C     Each join row set consists of:
C
C         - a base address in the scratch area
C         - a table count
C         - a segment vector count
C         - a set of segment vectors
C         - a set of segment vector row vector base addresses
C           (these are relative to the base of the join row set)
C         - a set of segment vector row vector counts
C         - a set of row vectors, augmented by offsets of their
C           parent segment vectors (these offsets are at the
C           end of each row vector)
C
C     The layout of a join row set in the EK scratch area is shown
C     below:
C
C        +--------------------------------------------+
C        |              join row set size             |  1 element
C        +--------------------------------------------+
C        |    number of row vectors in join row set   |  1 element
C        +--------------------------------------------+
C        |               table count (TC)             |  1 element
C        +--------------------------------------------+
C        |          segment vector count (SVC)        |  1 element
C        +--------------------------------------------+
C        |               segment vector 1             |  TC elements
C        +--------------------------------------------+
C                              .
C                              .
C                              .
C        +--------------------------------------------+
C        |               segment vector SVC           |  TC elements
C        +--------------------------------------------+
C        |   segment vector 1 row set base address    |  1 element
C        +--------------------------------------------+
C        |      segment vector 1 row count (RC_1)     |  1 element
C        +--------------------------------------------+
C                              .
C                              .
C                              .
C        +--------------------------------------------+
C        |  segment vector SVC row set base address   |  1 element
C        +--------------------------------------------+
C        |   segment vector SVC row count (RC_SVC)    |  1 element
C        +--------------------------------------------+
C        | Augmented row vectors for segment vector 1 |  TC*(RC_1 + 1 )
C        +--------------------------------------------+  elements
C                              .
C                              .
C                              .
C        +--------------------------------------------+
C        |Augmented row vectors for segment vector SVC|  TC*(RC_SVC + 1)
C        +--------------------------------------------+  elements
C
C
C$ Examples
C
C     1)  For a given join row set union, initialize the addressing
C         routines, then look up row vectors.
C
C
C            C
C            C     Tell the addressing routines where the join row set
C            C     union is.  NJRS is the number of join row sets in
C            C     the union, BASES is an array of EK scratch area base
C            C     addresses of each join row set.  A base address is
C            C     the predecessor of the first address actually
C            C     occupied by a join row set.
C            C
C                  CALL ZZEKVSET ( NJRS, BASES )
C
C            C
C            C     Find the base address of the each row vector, as well
C            C     as the base address of the corresponding segment
C            C     vector.
C            C
C                  DO I = 1, NJRS
C
C                     CALL EKVCAL ( I, RWVBAS, SGVBAS )
C
C                     [Do something with the row vector....]
C
C                  END DO
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
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 06-SEP-2006 (NJB)
C
C        Filled in Particulars section of header in entry point
C        ZZEKVCAL.  Changed previous version line's product from "Beta"
C        to "SPICELIB" both here and in ZZEKVCAL.
C
C-    SPICELIB Version 1.0.0, 28-SEP-1994 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     EK row vector address calculation
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               LSTLEI
 
      LOGICAL               RETURN
 
C
C     Local parameters
C
 
 
C
C     Include Section:  EK Join Row Set Parameters
C
C        JRS$INC Version 1    17-SEP-1994 (NJB)
C
C     Base-relative index of join row set size
C
      INTEGER               JSZIDX
      PARAMETER           ( JSZIDX = 1 )
 
C
C     Index of row vector count
C
      INTEGER               JRCIDX
      PARAMETER           ( JRCIDX = 2 )
 
C
C     Index of table count
C
      INTEGER               JTCIDX
      PARAMETER           ( JTCIDX = 3 )
 
C
C     Index of segment vector count
C
      INTEGER               JSCIDX
      PARAMETER           ( JSCIDX = 4 )
 
C
C     Base address of first segment vector
C
      INTEGER               JSVBAS
      PARAMETER           ( JSVBAS = 4 )
C
C
C     End Include Section:  EK Join Row Set Parameters
C
 
 
C
C     Local variables
C
      INTEGER               ADDRSS
      INTEGER               BEGIDX ( MXJRS )
      INTEGER               I
      INTEGER               J
      INTEGER               JRSIDX
      INTEGER               MAXRWV
      INTEGER               NSV
      INTEGER               NTABS
      INTEGER               RELOFF
      INTEGER               RBAS   ( MXJRS )
      INTEGER               SVBAS  ( MXJRS )
      INTEGER               SVNJRS
      INTEGER               TOP
 
C
C     Saved variables
C
      SAVE
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKVADR' )
      END IF
 
C
C     Never come here.
C
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
 
 
      CALL CHKOUT ( 'ZZEKVADR' )
      RETURN
 
 
 
 
 
C$Procedure  ZZEKVSET  ( Row vector address calculation set-up )
 
      ENTRY ZZEKVSET ( NJRS, BASES )
 
C$ Abstract
C
C     Given a union of EK join row sets, prepare EKVCAL to
C     compute addresses of row vectors in that union.
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
C     EK
C
C$ Keywords
C
C     EK
C     UTILITY
C
C$ Declarations
C
C     INTEGER               NJRS
C     INTEGER               BASES  ( * )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NJRS       I   Number of join row sets in union.
C     BASES      I   EK scratch area base addresses of join row sets.
C
C$ Detailed_Input
C
C     NJRS           is the number of join row sets in a join row set
C                    for which address calculations will be performed.
C
C     BASES          is an array of base addresses of the join row sets
C                    comprising the union.  These addresses are the
C                    predecessors of the addresses actually occupied by
C                    the join row sets.  There are NJRS base addresses
C                    in the array.  The order in which addresses are
C                    listed in BASES determines the order of the union
C                    of the row vectors:  the first row vector in the
C                    join row set whose base address is BASES(1) has
C                    index 1, and so on.  The last row vector in the
C                    join row set whose base address is BASES(NJRS) has
C                    the highest index of any row vector in the union.
C
C$ Detailed_Output
C
C     None.  See $Particulars for a discussion of the effect of this
C     routine.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the join row set count is less than 1 or greater than
C         MXJRS, the error SPICE(INVALIDCOUNT) is signalled.
C
C     2)  If any base address is less than zero or greater than TOP,
C         the EK scratch area stack top, the error
C         SPICE(BADADDRESS) is signalled.
C
C     3)  If the table count for any join row set is less than 1 or
C         greater than MXJOIN, the error SPICE(INVALIDCOUNT) is
C         signalled.
C
C     4)  If the table count for any join row set unequal to the count
C         for the first join row set, the error SPICE(INVALIDCOUNT) is
C         signalled.
C
C     5)  If any join row set has a row vector count that is less than
C         zero or greater than TOP, the EK scratch area stack top, the
C         error SPICE(BADADDRESS) is signalled.
C
C     6)  If any join row set has a segment vector count that is less
C         than zero or greater than TOP, the EK scratch area stack top,
C         the error SPICE(BADADDRESS) is signalled.
C
C$ Files
C
C     1)  This routine uses the EK scratch area, which employs a scratch
C         DAS file.
C
C$ Particulars
C
C     This routine speeds up EK row vectors address calculations by
C     centralizating the activities that need be performed only once
C     for a series of address  calculations for a given join row set
C     union.
C
C$ Examples
C
C     See the $Examples section of ZZEKVADR.
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
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 28-SEP-1994 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     EK row vector address calculation
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKVSET' )
      END IF
 
C
C     Validate join row set count.
C
      IF (  ( NJRS .LT. 1 ) .OR. ( NJRS .GT. MXJRS )  ) THEN
 
         CALL SETMSG ( 'Number of join row sets was #; valid ' //
     .                 'range is 1:#'                           )
         CALL ERRINT ( '#',  NJRS                               )
         CALL ERRINT ( '#',  MXJRS                              )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                    )
         CALL CHKOUT ( 'ZZEKVSET'                               )
         RETURN
 
      END IF
 
C
C     Validate the join row set bases.
C
      CALL ZZEKSTOP ( TOP )
 
      DO I = 1, NJRS
 
         IF (  ( BASES(I) .LT. 0 ) .OR. ( BASES(I) .GT. TOP )  ) THEN
 
            CALL SETMSG ( 'Base address # was #; valid range is ' //
     .                    '1:#'                                    )
            CALL ERRINT ( '#',  I                                  )
            CALL ERRINT ( '#',  BASES(I)                           )
            CALL ERRINT ( '#',  TOP                                )
            CALL SIGERR ( 'SPICE(BADADDRESS)'                      )
            CALL CHKOUT ( 'ZZEKVSET'                               )
            RETURN
 
         END IF
 
         SVBAS(I) = BASES(I)
 
      END DO
 
C
C     Validate and save the table count.  It's an error for this
C     count not to be identical for all of the join row sets in the
C     union.
C
      ADDRSS  =  BASES(1) + JTCIDX
      CALL ZZEKSRD ( ADDRSS, ADDRSS, NTABS )
 
      IF (  ( NTABS .LT. 1 ) .OR. ( NTABS .GT. MXJOIN )  ) THEN
 
         CALL SETMSG ( 'Table count for first join row set was #; '  //
     .                 'valid range is 1:#'                           )
         CALL ERRINT ( '#',  NTABS                                    )
         CALL ERRINT ( '#',  MXJOIN                                   )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                          )
         CALL CHKOUT ( 'ZZEKVSET'                                     )
         RETURN
 
      END IF
 
      DO I = 2, NJRS
 
         ADDRSS  =  BASES(I) + JTCIDX
         CALL ZZEKSRD ( ADDRSS, ADDRSS, J )
 
         IF ( J .NE. NTABS ) THEN
 
            CALL SETMSG ( 'Join row set # contains # tables; first ' //
     .                    'join row set contains # tables.  These '  //
     .                    'counts are supposed to match.'             )
            CALL ERRINT ( '#',  I                                     )
            CALL ERRINT ( '#',  J                                     )
            CALL ERRINT ( '#',  NTABS                                 )
            CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                       )
            CALL CHKOUT ( 'ZZEKVSET'                                  )
            RETURN
 
         END IF
 
      END DO
 
C
C     Validate the row vector counts for each join row set.
C     These counts must be in range.  Save the start indices of
C     the row vectors in each join row set.
C
      CALL CLEARI ( MXJRS, BEGIDX )
      BEGIDX(1) = 1
 
      DO I = 1, NJRS
 
         ADDRSS  =  BASES(I) + JRCIDX
         CALL ZZEKSRD ( ADDRSS, ADDRSS, J )
 
         IF (  ( J .LT. 0 ) .OR. ( J .GT. TOP )  ) THEN
 
            CALL SETMSG ( 'Join row set # has row count #; valid ' //
     .                    'range is 0:#'                            )
            CALL ERRINT ( '#',  I                                   )
            CALL ERRINT ( '#',  J                                   )
            CALL ERRINT ( '#',  TOP                                 )
            CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                     )
            CALL CHKOUT ( 'ZZEKVSET'                                )
            RETURN
 
         END IF
 
         IF ( I .LT. NJRS ) THEN
            BEGIDX(I+1)  =  BEGIDX(I)  +  J
         END IF
 
      END DO
 
 
 
C
C     Retain the index of the last row vector.
C
      MAXRWV  =  BEGIDX(NJRS) + J
 
 
C
C     Save the base addresses of the row vectors in each join row set.
C     Validate the segment vector counts while we're at it.
C
      DO I = 1, NJRS
 
         ADDRSS  =  BASES(I) + JSCIDX
         CALL ZZEKSRD ( ADDRSS, ADDRSS, NSV )
 
         IF ( NSV .LT. 0 ) THEN
 
            CALL SETMSG ( 'Join row set # has segment vector '     //
     .                    'count #; count must be non-negative.'    )
            CALL ERRINT ( '#',  I                                   )
            CALL ERRINT ( '#',  NSV                                 )
            CALL ERRINT ( '#',  TOP                                 )
            CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                     )
            CALL CHKOUT ( 'ZZEKVSET'                                )
            RETURN
 
         END IF
 
         RBAS(I) =  ADDRSS  +  NSV * ( NTABS + 2 )
 
      END DO
 
C
C     Retain the count of join row sets in the union.
C
      SVNJRS = NJRS
 
      CALL CHKOUT ( 'ZZEKVSET' )
      RETURN
 
 
 
 
 
 
 
C$Procedure  ZZEKVCAL  ( Row vector address calculation  )
 
      ENTRY ZZEKVCAL ( RWVIDX, RWVBAS, SGVBAS )
 
C$ Abstract
C
C     Find the EK scratch area base address of a row vector and the
C     corresponding segment vector, where the row vector has a
C     specified index within a union of join row sets.
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
C     EK
C
C$ Keywords
C
C     EK
C     UTILITY
C
C$ Declarations
C
C     INTEGER               RWVIDX
C     INTEGER               RWVBAS
C     INTEGER               SGVBAS
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     RWVIDX     I   Index of row vector.
C     RWVBAS     O   EK scratch area base address of row vector.
C     SGVBAS     O   Base address of parent segment vector.
C
C$ Detailed_Input
C
C     RWVIDX         is the index of a row vector in a join row set
C                    union.  The union is presumed to have been
C                    specified by a call to ZZEKVSET.
C
C$ Detailed_Output
C
C     RWVBAS         is the EK scratch area base address of the row
C                    vector specified by RWVIDX.  This address is
C                    the predecessor of the first address occupied by
C                    the row vector.  The row vector occupies NTAB
C                    consecutive addresses, where NTAB is the common
C                    table count for all join row sets in the union
C                    containing the specified row vector.
C
C     SGVBAS         is the EK scratch area base address of the segment
C                    vector corresponding to the specified row vector.
C                    The segment vector also occupies NTAB consecutive
C                    addresses.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input index is less than 1 or greater than
C         the highest index in the join row set union being addressed,
C         the error SPICE(INVALIDINDEX) is signalled.
C
C$ Files
C
C     1)  This routine uses the EK scratch area, which employs a scratch
C         DAS file.
C
C$ Particulars
C
C     See header of umbrella routine ZZEKVADR.
C
C$ Examples
C
C     See the $Examples section of ZZEKVADR.
C
C$ Restrictions
C
C     1)  ZZEKVSET must be called before this routine is called for the
C         first time.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 06-SEP-2006 (NJB)
C
C        Filled in Particulars section of header.  Changed
C        previous version line's product from "Beta" to "SPICELIB."
C
C-    SPICELIB Version 1.0.0, 22-SEP-1994 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     EK row vector address calculation
C
C-&
 
 
C
C     Use discovery check-in for speed; don't check RETURN.
C
C
C     If the index is out of range, that's an error.
C
      IF (  ( RWVIDX .LT. 1 ) .OR. ( RWVIDX .GT. MAXRWV )  ) THEN
 
         CALL CHKIN  ( 'ZZEKVCAL'                                   )
         CALL SETMSG ( 'Row vector index was #; valid range is 0:#' )
         CALL ERRINT ( '#',  RWVIDX                                 )
         CALL ERRINT ( '#',  MAXRWV                                 )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                        )
         CALL CHKOUT ( 'ZZEKVCAL'                                   )
         RETURN
 
      END IF
 
C
C     Identify the join row set containing the indicated row.  Our error
C     check guarantees a non-zero result.
C
      JRSIDX  =  LSTLEI ( RWVIDX, SVNJRS, BEGIDX )
 
C
C     Compute the offset of the indicated row vector relative to the
C     first row vector in the parent join row set.  This offset is one
C     less than the relative index of the row vector, multiplied by
C     the augmented row vector size.
C
      RELOFF  =  ( RWVIDX - BEGIDX(JRSIDX) ) * ( NTABS + 1 )
 
C
C     Find the base address of the row vector.
C
      RWVBAS  =  RBAS(JRSIDX) + RELOFF
 
C
C     Compute the base address of the parent segment vector.  The base-
C     relative address of the segment vector is stored at the end of the
C     row vector.
C
      CALL ZZEKSRD ( RWVBAS+NTABS+1, RWVBAS+NTABS+1, SGVBAS )
 
      SGVBAS  =  SVBAS(JRSIDX) + SGVBAS
 
      RETURN
      END
