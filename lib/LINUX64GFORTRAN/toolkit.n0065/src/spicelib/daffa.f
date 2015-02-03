C$Procedure DAFFA ( DAF, find array )

      SUBROUTINE DAFFA ( HANDLE, SUM, NAME, FOUND )

C$ Abstract
C
C     Find arrays in a DAF.
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
C     DAF
C
C$ Keywords
C
C     FILES
C
C$ Declarations

      INCLUDE              'zzddhman.inc'

      INTEGER               TBSIZE
      PARAMETER           ( TBSIZE = FTSIZE )

      INTEGER               HANDLE
      DOUBLE PRECISION      SUM    ( * )
      CHARACTER*(*)         NAME
      LOGICAL               FOUND

C$ Brief_I/O
C
C     Variable  I/O  Entry
C     --------  ---  --------------------------------------------------
C     HANDLE    I,O  DAFBFS, DAFBBS, DAFGH, DAFCS
C     SUM       I,O  DAFGS,  DAFRS,  DAFWS
C     NAME      I,O  DAFGN,  DAFRN
C     FOUND      O   DAFFNA, DAFFPA
C
C$ Detailed_Input
C
C     HANDLE      on input is the handle of the DAF to be searched.
C
C     SUM         on input is an array summary that replaces the
C                 summary of the current array in the DAF currently
C                 being searched.
C
C     NAME        on input is an array name that replaces the name
C                 of the current array in the DAF currently being
C                 searched.
C
C$ Detailed_Output
C
C     HANDLE      on output is the handle of the DAF currently being
C                 searched.
C
C     SUM         on output is the summary for the array found most
C                 recently.
C
C     NAME        on output is the name for the array found
C                 most recently.
C
C     FOUND       is true whenever the search for the next or the
C                 previous array is successful, and is false otherwise.
C
C$ Parameters
C
C     TBSIZE      the maximum number of files (DAS and DAF) that may be
C                 simultaneously open. TBSIZE is set to FTSIZE which is
C                 assigned and defined in zzdhman.inc.
C
C$ Exceptions
C
C     1) If DAFFA is called directly, the error SPICE(BOGUSENTRY)
C        is signaled.
C
C     2) See entry points DAFBFS, DAFFNA, DAFBBS, DAFFPA, DAFGS, DAFGN,
C        DAFGH, DAFRS, DAFWS, DAFRN, and DAFCS for exceptions specific
C        to those entry points.
C
C$ Files
C
C     DAFs read by DAFFA and its entry points are opened
C     elsewhere, and referred to only by their handles.
C
C$ Particulars
C
C     DAFFA serves as an umbrella, allowing data to be shared by its
C     entry points:
C
C        DAFBFS         Begin forward search.
C        DAFFNA         Find next array.
C
C        DAFBBS         Begin backward search.
C        DAFFPA         Find previous array.
C
C        DAFGS          Get summary.
C        DAFGN          Get name.
C        DAFGH          Get handle.
C
C        DAFRS          Replace summary.
C        DAFWS          Write summary.
C        DAFRN          Replace name.
C
C        DAFCS          Continue search.
C
C     The main function of these entry points is to allow the
C     contents of any DAF to be examined on an array-by-array
C     basis.
C
C     Conceptually, the arrays in a DAF form a doubly linked list,
C     which can be searched in either of two directions: forward or
C     backward. It is possible to search multiple DAFs simultaneously.
C
C     DAFBFS (begin forward search) and DAFFNA are used to search the
C     arrays in a DAF in forward order. In applications that search a
C     single DAF at a time, the normal usage is
C
C        CALL DAFBFS ( HANDLE )
C        CALL DAFFNA ( FOUND  )
C
C        DO WHILE ( FOUND )
C           CALL DAFGS ( SUM  )
C           CALL DAFGN ( NAME )
C            .
C            .
C
C           CALL DAFFNA ( FOUND )
C        END DO
C
C
C
C     DAFBBS (begin backward search) and DAFFPA are used to search the
C     arrays in a DAF in backward order. In applications that search
C     a single DAF at a time, the normal usage is
C
C        CALL DAFBBS ( HANDLE )
C        CALL DAFFPA ( FOUND  )
C
C        DO WHILE ( FOUND )
C           CALL DAFGS ( SUM  )
C           CALL DAFGN ( NAME )
C            .
C            .
C
C           CALL DAFFPA ( FOUND )
C        END DO
C
C
C     In applications that conduct multiple searches simultaneously,
C     the above usage must be modified to specify the handle of the
C     file to operate on, in any case where the file may not be the
C     last one specified by DAFBFS or DAFBBS. The routine DAFCS
C     (DAF, continue search) is used for this purpose. Below, we
C     give an example of an interleaved search of two files specified
C     by the handles HANDL1 and HANDL2. The directions of searches
C     in different DAFs are independent; here we conduct a forward
C     search on one file and a backward search on the other.
C     Throughout, we use DAFCS to specify which file to operate on,
C     before calling DAFFNA, DAFFPA, DAFGS, DAFRS, DAFWS, DAFGN, or
C     DAFRN.
C
C
C        CALL DAFBFS ( HANDL1 )
C        CALL DAFBBS ( HANDL2 )
C
C        CALL DAFCS  ( HANDL1 )
C        CALL DAFFNA ( FOUND1 )
C
C        CALL DAFCS  ( HANDL2 )
C        CALL DAFFPA ( FOUND2 )
C
C        DO WHILE ( FOUND1 .OR. FOUND2 )
C
C           IF ( FOUND1 ) THEN
C
C              CALL DAFCS ( HANDL1 )
C              CALL DAFGS ( SUM    )
C              CALL DAFGN ( NAME   )
C               .
C               .
C              CALL DAFCS  ( HANDL1 )
C              CALL DAFFNA ( FOUND1 )
C
C           END IF
C
C           IF ( FOUND2 ) THEN
C
C              CALL DAFCS ( HANDL2 )
C              CALL DAFGS ( SUM    )
C              CALL DAFGN ( NAME   )
C               .
C               .
C              CALL DAFCS  ( HANDL2 )
C              CALL DAFFPA ( FOUND2 )
C
C           END IF
C
C        END DO
C
C
C     At any time, the latest array found (whether by DAFFNA or DAFFPA)
C     is regarded as the `current' array for the file in which the
C     array was found. The last DAF in which a search was started,
C     executed, or continued by any of DAFBFS, DAFBBS, DAFFNA, DAFFPA
C     or DAFCS is regarded as the `current' DAF. The summary and name
C     for the current array in the current DAF can be returned
C     separately, as shown above, by calls to DAFGS (get summary) and
C     DAFGN (get name). The handle of the current DAF can also be
C     returned by calling DAFGH (get handle).
C
C     The summary and name of the current array in the current DAF can
C     be updated (again, separately) by providing new ones through DAFRS
C     (replace summary) and DAFRN (replace name). This feature
C     should not be used except to correct errors that occurred during
C     the creation of a file. Note that changes can only be made to
C     files opened for write access. Also, the addresses of an array
C     cannot be changed using these routines. (Another routine,
C     DAFWS, is provided for this purpose, but should be used only
C     to reorder the arrays in a file.)
C
C     Once a search has been begun, it may be continued in either
C     direction. That is, DAFFPA may be used to back up during a
C     forward search, and DAFFNA may be used to advance during a
C     backward search.
C
C$ Examples
C
C     1) The following code fragment illustrates the way the entry
C        points of DAFFA might be used to edit the summaries and names
C        for the arrays contained in a DAF. (All subroutines and
C        functions are from SPICELIB.)
C
C        In this example, the user begins by supplying the name of
C        the file to be edited, followed by any number of the following
C        commands.
C
C           NEXT      finds the next array.
C
C           PREV      finds the previous array.
C
C           EDIT      changes the value of an item in the summary or
C                     of the entire name. The keyword EDIT is
C                     always followed by the name of the item to be
C                     edited,
C
C                        DC n
C                        IC n
C                        NAME
C
C                     and the value, e.g.,
C
C                        EDIT IC 2 315
C                        EDIT NAME NAIF test K2905-1
C
C        The user may terminate the session at any time by typing END.
C        Commands other than those listed above are ignored.
C
C           READ (*,FMT='(A)') FNAME
C           CALL DAFOPW ( FNAME, HANDLE )
C           CALL DAFBFS ( HANDLE )
C
C           READ (*,FMT='(A)') COMMAND
C
C           DO WHILE ( COMMAND .NE. 'END' )
C              CALL NEXTWD ( COMMAND, VERB, COMMAND )
C
C              IF ( VERB .EQ. 'NEXT' ) THEN
C                 CALL DAFFNA ( FOUND )
C                 IF ( .NOT. FOUND ) THEN
C                    WRITE (*,*) 'At end of array list.'
C                 END IF
C
C              IF ( VERB .EQ. 'PREV' ) THEN
C                 CALL DAFFPA ( FOUND )
C                 IF ( .NOT. FOUND ) THEN
C                    WRITE (*,*) 'At beginning of array list.'
C                 END IF
C
C              IF ( VERB .EQ. 'EDIT' ) THEN
C                 CALL DAFGS ( SUM )
C                 CALL DAFGN ( NAME )
C                 CALL DAFUS ( SUM, ND, NI, DC, IC )
C
C                 CALL NEXTWD ( COMMAND, ITEM, VALUE )
C
C                 IF ( ITEM .EQ. 'DC' ) THEN
C                    CALL NEXTWD ( VALUE, INDEX, VALUE )
C                    CALL NPARSI ( INDEX, LOC,     ERR, PTR )
C                    CALL NPARSD ( VALUE, DC(LOC), ERR, PTR )
C
C                 ELSE IF ( ITEM .EQ. 'IC' ) THEN
C                    CALL NEXTWD ( VALUE, INDEX, VALUE )
C                    CALL NPARSI ( INDEX, LOC,     ERR, PTR )
C                    CALL NPARSI ( VALUE, IC(LOC), ERR, PTR )
C
C                 ELSE IF ( ITEM .EQ. 'NAME' ) THEN
C                    NAME = VALUE
C                 END IF
C
C                 CALL DAFPS ( ND, NI, DC, IC, SUM )
C                 CALL DAFRS ( SUM )
C                 CALL DAFRN ( NAME )
C              END IF
C
C              READ (*,FMT='(A)') COMMAND
C           END DO
C
C
C     2)  The following program compares data in two DAFs. The DAFs are
C         expected to have the same number of arrays, the same number
C         of elements in each corresponding array, and the same summary
C         format.
C
C         Each difference whose magnitude exceeds a specified tolerance
C         is flagged. The difference information is written to a file.
C
C
C                  PROGRAM CMPDAF
C
C            C
C            C     Compare data in two DAFs having identical structures.
C            C     No array in either DAF is longer than ARRYSZ d.p.
C            C     numbers.
C            C
C
C            C
C            C     Local parameters
C            C
C                  INTEGER               ARRYSZ
C                  PARAMETER           ( ARRYSZ = 1000 )
C
C                  INTEGER               ERRLEN
C                  PARAMETER           ( ERRLEN =  240 )
C
C                  INTEGER               FILEN
C                  PARAMETER           ( FILEN  =  128 )
C
C                  INTEGER               LINLEN
C                  PARAMETER           ( LINLEN =   80 )
C
C                  INTEGER               MAXND
C                  PARAMETER           ( MAXND  =  125 )
C
C                  INTEGER               MAXNI
C                  PARAMETER           ( MAXNI  =  250 )
C
C                  INTEGER               MAXSUM
C                  PARAMETER           ( MAXSUM =  128 )
C
C                  INTEGER               RLEN
C                  PARAMETER           ( RLEN   = 1000 )
C
C
C            C
C            C     Local variables
C            C
C                  CHARACTER*(RLEN)      ANAME1
C                  CHARACTER*(RLEN)      ANAME2
C                  CHARACTER*(FILEN)     DAF1
C                  CHARACTER*(FILEN)     DAF2
C                  CHARACTER*(FILEN)     LOG
C                  CHARACTER*(ERRLEN)    PRSERR
C                  CHARACTER*(LINLEN)    STR
C                  CHARACTER*(LINLEN)    TOLCH
C
C                  DOUBLE PRECISION      ARRAY1 ( ARRYSZ )
C                  DOUBLE PRECISION      ARRAY2 ( ARRYSZ )
C                  DOUBLE PRECISION      DC1    ( MAXND )
C                  DOUBLE PRECISION      DC2    ( MAXND )
C                  DOUBLE PRECISION      TOL
C                  DOUBLE PRECISION      DIFF
C                  DOUBLE PRECISION      SUM1   ( MAXSUM )
C                  DOUBLE PRECISION      SUM2   ( MAXSUM )
C
C                  INTEGER               FA1
C                  INTEGER               FA2
C                  INTEGER               I
C                  INTEGER               IA1
C                  INTEGER               IA2
C                  INTEGER               IC1    ( MAXNI )
C                  INTEGER               IC2    ( MAXNI )
C                  INTEGER               FA
C                  INTEGER               HANDL1
C                  INTEGER               HANDL2
C                  INTEGER               LEN1
C                  INTEGER               LEN2
C                  INTEGER               ND1
C                  INTEGER               ND2
C                  INTEGER               NI1
C                  INTEGER               NI2
C                  INTEGER               PTR
C
C                  LOGICAL               FOUND
C
C            C
C            C     Start out by obtaining the names of the DAFs to be
C            C     compared.
C            C
C                  WRITE (*,*) 'Enter name of first DAF.'
C                  READ  (*,FMT='(A)') DAF1
C
C                  WRITE (*,*) 'Enter name of second DAF.'
C                  READ  (*,FMT='(A)') DAF2
C
C                  WRITE (*,*) 'Enter name of log file.'
C                  READ  (*,FMT='(A)') LOG
C
C                  WRITE (*,*) 'Enter tolerance for data comparison.'
C                  READ  (*,FMT='(A)') TOLCH
C
C                  CALL NPARSD ( TOLCH, TOL, PRSERR, PTR )
C
C                  DO WHILE ( PRSERR .NE. ' ' )
C
C                     WRITE (*,*) PRSERR
C                     WRITE (*,*) 'Enter tolerance for data comparison.'
C                     READ  (*,FMT='(A)') TOLCH
C
C                     CALL NPARSD ( TOLCH, TOL, PRSERR, PTR )
C
C                  END DO
C
C            C
C            C     Open both DAFs for reading.
C            C
C                  CALL DAFOPR ( DAF1, HANDL1 )
C                  CALL DAFOPR ( DAF2, HANDL2 )
C
C            C
C            C     Start forward searches in both DAFS.
C            C
C                  CALL DAFBFS ( HANDL1 )
C                  CALL DAFBFS ( HANDL2 )
C
C            C
C            C     Obtain the summary formats for each DAF. Stop now
C            C     if the summary formats don't match.
C            C
C                  CALL DAFHSF ( HANDL1, ND1, NI1 )
C                  CALL DAFHSF ( HANDL2, ND2, NI2 )
C
C                  IF (  ( ND1 .NE. ND2 ) .OR. ( NI1 .NE. NI2 )  ) THEN
C
C                     STR = 'Summary formats do not match.  NI1 = #, '//
C                 .                      'NI2 = #, ND1 = #, ND2 = #.'
C
C                     CALL REPMI  ( STR, '#', NI1, STR )
C                     CALL REPMI  ( STR, '#', NI2, STR )
C                     CALL REPMI  ( STR, '#', ND1, STR )
C                     CALL REPMI  ( STR, '#', ND2, STR )
C
C                     CALL WRLINE ( LOG,  STR )
C
C                     CALL SIGERR ( 'Incompatible DAFs' )
C
C                  END IF
C
C            C
C            C     Find the first array in each DAF. Use DAFCS
C            C     (DAF, continue search) to set the handle of the DAF
C            C     to search in before calling DAFFNA.
C            C
C                  CALL DAFCS  ( HANDL1 )
C                  CALL DAFFNA ( FOUND  )
C
C                  IF ( FOUND ) THEN
C                     CALL DAFCS  ( HANDL2 )
C                     CALL DAFFNA ( FOUND  )
C                  END IF
C
C                  DO WHILE ( FOUND )
C
C            C
C            C        Get the summary and name of each array, using
C            C        DAFCS to select the DAF to get the information
C            C        from. Unpack the summaries and find the beginning
C            C        and ending addresses of the arrays. Read the
C            C        arrays into the variables ARRAY1 and ARRAY2.
C            C
C                     CALL DAFCS ( HANDL1 )
C                     CALL DAFGN ( ANAME1 )
C                     CALL DAFGS ( SUM1   )
C                     CALL DAFUS ( SUM1, ND1, NI1, DC1, IC1 )
C
C                     IA1  = IC1 ( NI1 - 1 )
C                     FA1  = IC1 ( NI1     )
C                     LEN1 = FA1 - IA1  + 1
C
C                     IF (  LEN1  .GT.  ARRYSZ  ) THEN
C                        CALL SETMSG ( 'Buffer too small; need # elts.')
C                        CALL ERRINT ( '#', LEN1                       )
C                        CALL SIGERR ( 'ARRAYTOOSMALL'                 )
C                     ELSE
C                        CALL DAFRDA ( HANDL1, IA1, FA1, ARRAY1 )
C                     END IF
C
C                     CALL DAFCS ( HANDL2 )
C                     CALL DAFGN ( ANAME2 )
C                     CALL DAFGS ( SUM2   )
C                     CALL DAFUS ( SUM2, ND2, NI2, DC2, IC2 )
C
C                     IA2 = IC2 ( NI2 - 1 )
C                     FA2 = IC2 ( NI2     )
C
C                     LEN2 = FA2 - IA2  + 1
C
C                     IF (  LEN1  .GT.  ARRYSZ  ) THEN
C
C                        CALL SETMSG ( 'Buffer too small; need # elts.')
C                        CALL ERRINT ( '#', LEN2                       )
C                        CALL SIGERR ( 'ARRAYTOOSMALL'                 )
C
C                     ELSE IF ( LEN1 .NE. LEN2 ) THEN
C
C                        CALL SETMSG ( 'DAF structures do not match. '//
C                    .                 'LEN1 = #, LEN2 = #. ' )
C                        CALL ERRINT ( '#', LEN1              )
C                        CALL ERRINT ( '#', LEN2              )
C                        CALL SIGERR ( 'Incompatible DAFs' )
C
C                     ELSE
C                        CALL DAFRDA ( HANDL2, IA2, FA2, ARRAY2 )
C                     END IF
C            C
C            C
C            C        Compare the data in the two arrays. Log a message
C            C        for every instance of data that differs by more
C            C        than the allowed tolerance. Use the array names
C            C        to label the data sources.
C            C
C                     DO I = 1, LEN1
C
C                        DIFF  =  ABS( ARRAY1(I) - ARRAY2(I) )
C
C                        IF (  DIFF  .GT.  TOL  ) THEN
C            C
C            C              Get the array names.
C            C
C                           CALL DAFCS ( HANDL1 )
C                           CALL DAFGN ( ANAME1 )
C                           CALL DAFCS ( HANDL2 )
C                           CALL DAFGN ( ANAME2 )
C
C            C
C            C              Construct the report strings. The number 14
C            C              below is the number of significant digits to
C            C              show in the strings representing d.p.
C            C              numbers.
C            C
C
C                           CALL WRLINE ( LOG, ' ' )
C                           CALL WRLINE ( LOG, 'Difference of array ' //
C                    .                         'elements exceeded '   //
C                    .                         'tolerance.'            )
C                           CALL WRLINE ( LOG, 'First array:  '//ANAME1)
C                           CALL WRLINE ( LOG, 'Second array: '//ANAME2)
C
C                           STR = 'First value:  #'
C                           CALL REPMD  ( STR, '#', ARRAY1(I), 14, STR )
C                           CALL WRLINE ( LOG, STR                     )
C
C                           STR = 'Second value: #'
C                           CALL REPMD  ( STR, '#', ARRAY2(I), 14, STR )
C                           CALL WRLINE ( LOG, STR                     )
C
C                           STR = 'Difference:   #'
C                           CALL REPMD  ( STR, '#', DIFF,      14, STR )
C                           CALL WRLINE ( LOG, STR                     )
C                           CALL WRLINE ( LOG, ' '                     )
C
C                        END IF
C
C                     END DO
C
C            C
C            C        Find the next pair of arrays.
C            C
C                     CALL DAFCS  ( HANDL1 )
C                     CALL DAFFNA ( FOUND  )
C
C                     IF ( FOUND ) THEN
C                        CALL DAFCS  ( HANDL2 )
C                        CALL DAFFNA ( FOUND  )
C                     END IF
C
C                  END DO
C
C            C
C            C     Close the DAFs.
C            C
C                  CALL DAFCLS ( HANDL1 )
C                  CALL DAFCLS ( HANDL2 )
C
C                  END
C
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
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C     E.D. Wright     (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.1.0, 10-FEB-2014 (EDW) (BVS)
C
C        Added a functional code example to the Examples section
C        in DAFBFS, DAFFNA, DAFGS.
C
C        Added check on value of "found" boolean returned from
C        DAFGSR calls. Failure to check this value can cause an
C        infinite loop during segment searches on damaged SPKs.
C
C        Eliminated unneeded Revisions section.
C
C        Removed the obsolete Reference citation to "NAIF
C        Document 167.0."
C
C        Added full declaration of HANDLE to the Declarations section
C        of the DAFCS header.
C
C-    SPICELIB Version 3.0.0, 16-NOV-2001 (FST)
C
C        This umbrella and its entry points were updated to
C        work properly with the changes in the DAF system as
C        a result of its utilization of the new handle manager.
C        Calls to DAFRDR were replaced with the translation-aware
C        interface DAFGSR for retrieving summary records from
C        DAFs.
C
C        Updated the entry points of DAFFA to enable its
C        internal state table size, TBSIZE, to be smaller
C        than the file table maintained by DAFAH: FTSIZE.
C
C        Since DAFAH now tracks FTSIZE files as defined in
C        the include file 'zzddhman.inc', it was decided that
C        in the interest of releasing the toolkit this module
C        would undergo simple changes. As such most previous
C        references to FTSIZE in this umbrella have been replaced
C        with TBSIZE where appropriate. DAFBFS and DAFBBS now signal
C        errors if there is not enough room to add a new DAF's
C        dossier to the state table. Also, after attempting to
C        clean up all files listed in the state table that are
C        not currently open, DAFBFS and DAFBBS attempt to locate
C        the first dossier with STADDG set to FALSE. This is then
C        freed to make room for the new DAF. If DAFBNA fails
C        to locate such a dossier in the state table, it
C        signals the error SPICE(STFULL).
C
C        The parameter FILEN was removed, as it is defined
C        on an environmental basis in the include file
C        'zzddhman.inc'.
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
C
C        Updated to support simultaneous searches of multiple DAFs.
C
C        In previous versions of DAFFA, only one search could be
C        conducted at a time. Therefore, there was no question about
C        which DAF was being operated on by any of the DAFFA entry
C        points that don't accept file handles as input arguments.
C        In the current version of DAFFA, the entry points that don't
C        accept file handles as inputs operate on the `current DAF'.
C        The current DAF is the last one in which a search was
C        started by DAFBFS or DAFBBS, or continued by the new entry
C        point DAFCS. DAFCS was added to allow users to set the
C        current DAF, so that searches of multiple DAFs can be
C        interleaved.
C
C        Note that the notion of `current DAF' as discussed here applies
C        only to DAFs acted upon by entry points of DAFFA. In DAFANA,
C        there is a DAF that is treated as the `current DAF' for
C        adding data; there is no connection between the DAFs regarded
C        as current by DAFFA and DAFANA.
C
C        The two principal changes to DAFFA are the addition of the
C        new entry point DAFCS, and the addition of a data structure
C        called the `state table'. The state table is a collection of
C        parallel arrays that maintain information about the state
C        of each search that is currently in progress. The arrays are
C        indexed by a singly linked list pool; this mechanism allows
C        addition and deletion of information about searches without
C        requiring movement of data already in the state table. The
C        linked list pool contains an `active' list and a `free' list.
C        Nodes in the active list are used to index elements of the
C        state table where data about searches in progress is stored.
C        The head node of the active list is of particular significance:
C        the state information pointed to by this node is that of the
C        current DAF. Nodes in the free list index elements of the
C        state table that are available for use.
C
C        When a search is started on a DAF that is not already `known'
C        to DAFFA, information about the DAF is added to the state
C        table. If there are no free elements in the state table,
C        the routine starting the search (DAFBFS or DAFBBS) will
C        perform garbage collection:  the routine will test the handles
C        of each file about which information in stored in the state
C        table to see whether that file is still open. Nodes containing
C        information about DAFs that are no longer open will be moved
C        to the free list.
C
C        Whenever a DAF becomes the current DAF, the linked list
C        that indexes the state table is adjusted so that the
C        information about the current DAF is at the head of the list.
C        This way, a slight efficiency is gained when repeated search
C        accesses are made to the same DAF, since the linear search
C        through the state table for information on that DAF will
C        be shortened.
C
C        Since the algorithms for maintenance of linked lists are well
C        known, they are not documented here. However, see the
C        internals of the SPICELIB routine SPKBSR for a nice diagram
C        describing a similar data structure.
C
C        The state table contains two arrays that are quite large:
C        there are buffers that contain the last character record
C        and summary record read from each DAF. A parallel situation
C        exists in DAFANA, where the name and array summary for each
C        array under construction are buffered. The total storage
C        required for these arrays (in DAFANA and DAFFA together) is
C        4000 * TBSIZE bytes. For this reason, it may be a good idea
C        to reduce the value of TBSIZE in SPICELIB versions for
C        machines where memory is scarce.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&

C$ Index_Entries
C
C     find DAF array
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               ELEMI
      LOGICAL               FAILED
      LOGICAL               RETURN



C
C     Local parameters
C
      INTEGER               CRLEN
      PARAMETER           ( CRLEN  = 1000 )

      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN =   60 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL =   -5 )

      INTEGER               MAXNDC
      PARAMETER           ( MAXNDC =  124 )

      INTEGER               DPRSIZ
      PARAMETER           ( DPRSIZ =  128 )

      INTEGER               MAXNIC
      PARAMETER           ( MAXNIC =  250 )

      INTEGER               NIL
      PARAMETER           ( NIL    =   -1 )


C
C     Local variables
C

C
C     State variables.
C
C     These variables define the state of each DAF to which data
C     is currently being added. For each DAF that we're writing to, we
C     maintain a copy of:
C
C        STFH           File handle.
C
C        STPREV         Record number of previous array summary.
C
C        STTHIS         Record number of current array summary.
C
C        STNEXT         Record number of next array summary.
C
C        STNSEG         Number of summaries in current summary record.
C
C        STCURR         Index of current summary within summary record.
C
C        STNR           Last name record read.
C
C        STHVNR         Flag indicating whether name record containing
C                       name of current array is buffered.
C
C        STSR           Last summary record read.
C
C     These variables are maintained in a table of parallel arrays;
C     the size of the table is TBSIZE.
C
      INTEGER               STFH   (         TBSIZE )
      INTEGER               STPREV (         TBSIZE )
      INTEGER               STTHIS (         TBSIZE )
      INTEGER               STNEXT (         TBSIZE )
      INTEGER               STNSEG (         TBSIZE )
      INTEGER               STCURR (         TBSIZE )
      CHARACTER*(CRLEN)     STNR   (         TBSIZE )
      LOGICAL               STHVNR (         TBSIZE )
      DOUBLE PRECISION      STSR   ( DPRSIZ, TBSIZE )

C
C     The table of state variables is indexed by a singly linked list
C     of pointers. This mechanism avoids the work of moving
C     the state variable data about as information about DAFs is
C     added to or deleted from the table.
C
C     The structure containing the linked list pointers is called a
C     `pool'. The pool contains a list of `active' nodes and a list
C     of free nodes. The head nodes of the active and free lists are
C     maintained as the variables STHEAD (`state table head') and
C     STFPTR (`state table free pointer'), respectively. Every node in
C     the pool is on exactly one of these lists.
C
      INTEGER               STPOOL ( TBSIZE )
      INTEGER               STHEAD
      INTEGER               STFPTR

C
C     The pool starts out with all of the nodes on the free list. The
C     first one of DAFBFS or DAFBBS to be called initializes the pool.
C     As new DAFs are searched, DAFBFS and DAFBBS add information about
C     them to the state table. Every time a search is started by DAFBFS
C     or DAFBBS, the routine in question `moves' the DAF's state
C     information to the head of the active list, if the state
C     information is not already there. This re-organization is
C     accomplished by deleting the node for the DAF from its current
C     position in the active list and inserting the node at the head of
C     the list. Thus, the change is made merely by setting pointers,
C     not by moving chunks of data in the state table.
C
C     It may happen that there is no room left in the state table
C     to accommodate information about a new DAF. In this case,
C     garbage collection must be performed:  whichever of DAFBFS or
C     DAFBBS needs more room frees all nodes in the table that index
C     DAFs that are not currently open.
C
C     Note that the routines DAFGS, DAFGN, DAFRS, DAFRN, and DAFWS do
C     not modify the state table; they merely act on the current array
C     in the DAF that is at the head of the active list.
C


C
C     Other local variables
C
      CHARACTER*(FILEN)     DAFNAM
      CHARACTER*(IFNLEN)    IFNAME

      DOUBLE PRECISION      EXDC   ( MAXNDC )
      DOUBLE PRECISION      EXSUM  ( MAXNDC )
      DOUBLE PRECISION      NEWDC  ( MAXNDC )

      INTEGER               BWARD
      INTEGER               EXIC   ( MAXNIC )
      INTEGER               FREE
      INTEGER               FWARD
      INTEGER               I
      INTEGER               NAMSIZ
      INTEGER               ND
      INTEGER               NEWIC  ( MAXNIC )
      INTEGER               NEXTP
      INTEGER               NI
      INTEGER               OPNSET ( LBCELL : FTSIZE )
      INTEGER               P
      INTEGER               PREV
      INTEGER               SUMSIZ

      LOGICAL               FIRST
      LOGICAL               FND
      INTEGER               OFFSET


C
C     Save everything between calls
C
      SAVE


C
C     Initial values
C
      DATA                  FIRST    / .TRUE.             /
      DATA                  STHVNR   /  TBSIZE * .FALSE.  /
      DATA                  STFPTR   /  NIL               /
      DATA                  STHEAD   /  NIL               /


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'DAFFA'             )
         CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
         CALL CHKOUT ( 'DAFFA'             )
      END IF

      RETURN




C$Procedure DAFBFS ( DAF, begin forward search )

      ENTRY DAFBFS ( HANDLE )

C$ Abstract
C
C     Begin a forward search for arrays in a DAF.
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
C     DAF
C
C$ Keywords
C
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of file to be searched.
C
C$ Detailed_Input
C
C     HANDLE      is the handle of a DAF on which a forward
C                 search is to be conducted.
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
C     1)  If the input handle is invalid, the error will be diagnosed
C         by routines called by this routine.
C
C     2)  If DAFBSR retuns with the read success flag as false, a
C         SPICE(RECORDNOTFOUND) error signals.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     See DAFFA.
C
C$ Examples
C
C     Example (1):
C
C        See DAFFA.
C
C     Example (2):
C
C     Use a simple routine to output the double precision and integer
C     values stored in an SPK's segments descriptors. This function
C     opens a DAF for read, performs a forwards search for the DAF
C     arrays, prints segments description for each array found, then
C     closes the DAF.
C
C           PROGRAM DAF_T
C
C           INTEGER             HANDLE
C
C     C
C     C     Define the summary parameters appropriate
C     C     for an SPK file.
C     C
C           INTEGER             ND
C           PARAMETER         ( ND = 2 )
C
C           INTEGER             NI
C           PARAMETER         ( NI = 6 )
C
C           INTEGER             IC( NI )
C
C           DOUBLE PRECISION    DC( ND )
C
C           CHARACTER*(32)      KERNEL
C
C           LOGICAL             FOUND
C
C
C     C
C     C     Open a DAF for read. Return a HANDLE referring to the file.
C     C
C           KERNEL = 'de421.bsp'
C           CALL DAFOPR ( KERNEL, HANDLE )
C
C     C
C     C     Begin a forward search on the file.
C     C
C           CALL DAFBFS ( HANDLE )
C
C     C
C     C     Search until a DAF array is found.
C     C
C           CALL DAFFNA ( FOUND )
C
C     C
C     C     Loop while the search finds subsequent DAF arrays.
C     C
C           DO WHILE ( FOUND )
C
C              CALL DAFGS ( SUM )
C              CALL DAFUS ( SUM, ND, NI, DC, IC )
C
C              WRITE(*,*)                'Doubles: ', DC(1:ND)
C              WRITE(*, FMT='(A,6I9)' ) 'Integers: ', IC(1:NI)
C
C     C
C     C        Check for another segment.
C     C
C              CALL DAFFNA ( FOUND )
C
C           END DO
C
C     C
C     C     Safely close the DAF.
C     C
C           CALL DAFCLS ( HANDLE )
C
C           END
C
C     The program outputs:
C
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         1        0        1        2      641   310404
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         2        0        1        2   310405   423048
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         3        0        1        2   423049   567372
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         4        0        1        2   567373   628976
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         5        0        1        2   628977   674740
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         6        0        1        2   674741   715224
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         7        0        1        2   715225   750428
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         8        0        1        2   750429   785632
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         9        0        1        2   785633   820836
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:        10        0        1        2   820837   944040
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       301        3        1        2   944041  1521324
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       399        3        1        2  1521325  2098608
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       199        1        1        2  2098609  2098620
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       299        2        1        2  2098621  2098632
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       499        4        1        2  2098633  2098644
C
C      Note, the final entries in the integer array contains the segment
C      start/end indexes. The output indicates the search proceeded
C      from the start of the file (low value index) towards the end
C      (high value index).
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
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.1.0, 10-OCT-2012 (EDW)
C
C        Added a functional code example to the Examples section.
C
C        Added check on value of "found" boolean returned from
C        DAFGSR calls. Failure to check this value can cause an
C        infinite loop during segment searches on damaged SPKs.
C
C        Eliminated unneeded Revisions section.
C
C        Removed the obsolete Reference citation to "NAIF
C        Document 167.0."
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
C
C        Updated to support simultaneous searches of multiple DAFs.
C
C        This routine now operates on the current DAF---the one at
C        the head of the active list. All saved state variables
C        used by this routine are now part of the state table, or
C        its associated set of pointers.
C
C        Also, the $Exceptions section was filled out.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&

C$ Index_Entries
C
C     begin DAF forward search
C
C-&


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFBFS' )
      END IF

C
C     Check out the file handle before going any further.
C
      CALL DAFSIH ( HANDLE, 'READ' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFBFS' )
         RETURN
      END IF

C
C     Initialize the state table pool, if this hasn't been done yet.
C     Also initialize the cell used to obtain the set of handles of
C     open DAFs.
C
      IF ( FIRST ) THEN

         CALL SSIZEI ( FTSIZE, OPNSET )

         DO I = 1,  TBSIZE - 1
            STPOOL(I)   =  I + 1
         END DO

         STPOOL(TBSIZE) =  NIL
         STFPTR         =  1
         FIRST          = .FALSE.

      END IF

C
C     See whether we already have an entry for this DAF in the
C     state table. Find the previous node if possible.
C
      P      =   STHEAD
      PREV   =   NIL
      FND    =  .FALSE.

      DO WHILE (  ( P .NE. NIL )  .AND.  ( .NOT. FND )  )

         IF ( STFH(P) .EQ. HANDLE ) THEN
            FND   =  .TRUE.
         ELSE
            PREV  =  P
            P     =  STPOOL( P )
         END IF

      END DO

C
C     At this point, either FND is false, or P points to a
C     state table entry describing the DAF indicated by HANDLE.
C     In the latter case, PREV is the predecessor of P.
C
      IF ( FND ) THEN
C
C        We already have a dossier on this DAF. We already have
C        the information on the summary format, but we must re-set
C        our summary record pointers and our name record availability
C        flag.
C
C        Rather than doing the update here, we do it outside of this
C        IF block. That way, the update gets done in just one place.
C        This just makes life easier:  if the collection of state
C        variables is changed, there are fewer places to forget to
C        make the required code changes.
C
C        Move the node for this DAF to the head of the active list,
C        if it is not already there:
C
C           - Make the predecessor of P point to the successor of P.
C
C           - Make P point to the head of the active list.
C
C           - Make P the active list head node.
C
C
         IF ( P .NE. STHEAD ) THEN
C
C           P is in the active list, but is not at the head. So,
C           the predecessor of P is not NIL.
C
            STPOOL(PREV)  =  STPOOL(P)
            STPOOL(P)     =  STHEAD
            STHEAD        =  P

         END IF


      ELSE
C
C        We don't yet have any information on this DAF. Make a new
C        state table entry for the DAF. We may need to make room for
C        the new information by freeing space allocated to DAFs that
C        are no longer open.
C
         IF ( STFPTR .EQ. NIL ) THEN
C
C           Oops, we're out of space. Time for garbage collection.
C           Test each file handle to see whether it designates a DAF
C           that is still open. DAFHOF will tell us which handles
C           point to open DAFs.
C
            CALL DAFHOF ( OPNSET )

            P     =  STHEAD
            PREV  =  NIL
C
C           For every DAF file represented in the state table, we'll
C           delete the corresponding state information if the DAF is
C           now closed. We traverse the active list, examining each
C           file handle as we go.
C
            DO WHILE ( P .NE. NIL )

               IF (  ELEMI ( STFH(P), OPNSET )   )  THEN
C
C                 The file is open. Have a look at the next node.
C
                  PREV = P
                  P    = STPOOL( P )

               ELSE
C
C                 This file handle is not on the list, so free the
C                 node pointing to the information about the DAF it
C                 designated:
C
C                    - Save the successor of P.
C
C                    - Link the predecessor of node P to the successor
C                      of P, if the predecessor is not NIL.
C
C                    - If it happens that P is the head node of the
C                      active list, set the head equal to the
C                      successor of P.
C
C                    - Link P into the free list.
C
C                    - Set P equal to its saved successor.
C
C                    - (PREV remains unchanged.)
C
C
                  NEXTP  =  STPOOL( P )


                  IF ( P .EQ. STHEAD ) THEN
C
C                    Re-assign STHEAD so that we don't lose the head
C                    of the active list. P has no predecessor in this
C                    case, so there's no need to set the forward pointer
C                    of node PREV.
C
                     STHEAD = NEXTP

                  ELSE
C
C                    Since P is not the head node of the active list,
C                    PREV is not NIL, so we'll need to set the forward
C                    pointer of node PREV.
C
                     STPOOL( PREV ) = NEXTP

                  END IF

                  STPOOL( P )  =  STFPTR
                  STFPTR       =  P
                  P            =  NEXTP

               END IF

            END DO
C
C           At this point, we've freed all nodes from the active
C           list that were used to index information about DAFs that
C           are no longer open. If there's any more room in the state
C           table, we have it now.
C
         END IF

C
C        If there still is no room, there is a bug in DAFAH, since DAFAH
C        should not allow more than TBSIZE DAFs to be open. So, we
C        assume that we've found some room. The first free node is
C        indicated by STFPTR. We'll allocate this node and use it to
C        index the state information for the new DAF.
C
         P  =  STFPTR

C
C        Update the free list pointer, link P to the previous head
C        of the active list, and make P the head of the active list.
C
         STFPTR       =  STPOOL( P )
         STPOOL( P )  =  STHEAD
         STHEAD       =  P

      END IF

C
C     At this point, P is the head node of the active list, and P is
C     the index in the state table of the information for the current
C     DAF.
C

C
C     Read the file record and first summary record. Do not read the
C     corresponding name record until necessary. In most searches,
C     names are of no interest.
C
      CALL DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )
      CALL DAFGSR ( HANDLE, FWARD, 1, 128, STSR(1,P), FND )

      IF ( .NOT. FND ) THEN

         CALL DAFHFN ( STFH(P), DAFNAM )

         CALL SETMSG ( 'Attempt to read descriptor record # '    //
     .                 'of DAF ''#'' failed; record was '        //
     .                 'not found. This condition may indicate ' //
     .                 'a corrupted DAF.' )
         CALL ERRINT ( '#', STNEXT(P)                            )
         CALL ERRCH  ( '#', DAFNAM                               )
         CALL SIGERR ( 'SPICE(RECORDNOTFOUND)'                   )
         CALL CHKOUT ( 'DAFBFS'                                  )
         RETURN

      END IF

C
C     Set up the state information for this file. Note that we
C     don't have a name record yet, and we have no current array
C     yet.
C
      STFH  ( P )  =  HANDLE
      STTHIS( P )  =  FWARD
      STNEXT( P )  =  INT ( STSR(1,P) )
      STPREV( P )  =  INT ( STSR(2,P) )
      STNSEG( P )  =  INT ( STSR(3,P) )
      STHVNR( P )  = .FALSE.

C
C     The arrays are returned in forward order within each summary
C     record.
C
      STCURR( P )  =  0

      CALL CHKOUT ( 'DAFBFS' )
      RETURN




C$Procedure DAFFNA ( DAF, find next array )

      ENTRY DAFFNA ( FOUND )

C$ Abstract
C
C     Find the next (forward) array in the current DAF.
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
C     DAF
C
C$ Keywords
C
C     FILES
C
C$ Declarations
C
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     FOUND      O   True if an array was found.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     FOUND       is true if an array was found, and is false if,
C                 when this routine is called, the current array is
C                 the tail of the array list. (Recall that the
C                 arrays in a DAF may be viewed as a doubly linked
C                 list, with the tail being the last array in the file.)
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If this routine is called before a search is begun, the
C         error SPICE(DAFNOSEARCH) is signaled.
C
C     2)  If the DAF to be searched has actually been closed, the error
C         will be diagnosed by routines called by this routine.
C
C     3)  If the end of the array list has already been reached when
C         this routine is called, this routine has no effect.
C
C     4)  If DAFBSR retuns with the read success flag as false, a
C         SPICE(RECORDNOTFOUND) error signals.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     See DAFFA.
C
C$ Examples
C
C     Example (1):
C
C        See DAFFA.
C
C     Example (2):
C
C     Use a simple routine to output the double precision and integer
C     values stored in an SPK's segments descriptors. This function
C     opens a DAF for read, performs a forwards search for the DAF
C     arrays, prints segments description for each array found, then
C     closes the DAF.
C
C           PROGRAM DAF_T
C
C           INTEGER             HANDLE
C
C     C
C     C     Define the summary parameters appropriate
C     C     for an SPK file.
C     C
C           INTEGER             ND
C           PARAMETER         ( ND = 2 )
C
C           INTEGER             NI
C           PARAMETER         ( NI = 6 )
C
C           INTEGER             IC( NI )
C
C           DOUBLE PRECISION    DC( ND )
C
C           CHARACTER*(32)      KERNEL
C
C           LOGICAL             FOUND
C
C
C     C
C     C     Open a DAF for read. Return a HANDLE referring to the file.
C     C
C           KERNEL = 'de421.bsp'
C           CALL DAFOPR ( KERNEL, HANDLE )
C
C     C
C     C     Begin a forward search on the file.
C     C
C           CALL DAFBFS ( HANDLE )
C
C     C
C     C     Search until a DAF array is found.
C     C
C           CALL DAFFNA ( FOUND )
C
C     C
C     C     Loop while the search finds subsequent DAF arrays.
C     C
C           DO WHILE ( FOUND )
C
C              CALL DAFGS ( SUM )
C              CALL DAFUS ( SUM, ND, NI, DC, IC )
C
C              WRITE(*,*)                'Doubles: ', DC(1:ND)
C              WRITE(*, FMT='(A,6I9)' ) 'Integers: ', IC(1:NI)
C
C     C
C     C        Check for another segment.
C     C
C              CALL DAFFNA ( FOUND )
C
C           END DO
C
C     C
C     C     Safely close the DAF.
C     C
C           CALL DAFCLS ( HANDLE )
C
C           END
C
C     The program outputs:
C
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         1        0        1        2      641   310404
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         2        0        1        2   310405   423048
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         3        0        1        2   423049   567372
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         4        0        1        2   567373   628976
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         5        0        1        2   628977   674740
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         6        0        1        2   674741   715224
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         7        0        1        2   715225   750428
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         8        0        1        2   750429   785632
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         9        0        1        2   785633   820836
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:        10        0        1        2   820837   944040
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       301        3        1        2   944041  1521324
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       399        3        1        2  1521325  2098608
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       199        1        1        2  2098609  2098620
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       299        2        1        2  2098621  2098632
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       499        4        1        2  2098633  2098644
C
C      Note, the final entries in the integer array contains the segment
C      start/end indexes. The output indicates the search proceeded
C      from the start of the file (low value index) towards the end
C      (high value index).
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
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.1.0, 10-OCT-2012 (EDW)
C
C        Added a functional code example to the Examples section.
C
C        Added check on value of "found" boolean returned from
C        DAFGSR calls. Failure to check this value can cause an
C        infinite loop during segment searches on damaged SPKs.
C
C        Eliminated unneeded Revisions section.
C
C        Removed the obsolete Reference citation to "NAIF
C        Document 167.0."
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
C
C        Updated to support simultaneous searches of multiple DAFs.
C
C        This routine now operates on the current DAF---the one at
C        the head of the active list. All saved state variables
C        used by this routine are now part of the state table, or
C        its associated set of pointers.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&

C$ Index_Entries
C
C     find next DAF array
C
C-&


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFFNA' )
      END IF


C
C     FOUND will be false until we make it past the error checks.
C
      FOUND = .FALSE.

C
C     Operate on the last DAF in which a search has been started.
C
      P  =  STHEAD

C
C     Make sure that a search has been started in this DAF.
C
      IF ( P .EQ. NIL ) THEN

         CALL SETMSG ( 'No DAF is currently being searched.' )
         CALL SIGERR ( 'SPICE(DAFNOSEARCH)'                  )
         CALL CHKOUT ( 'DAFFNA'                              )
         RETURN

C
C     Make sure that the `current' DAF is still open.
C
      ELSE

         CALL DAFSIH ( STFH(P), 'READ' )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DAFFNA' )
            RETURN
         END IF

      END IF


C
C     Now that we know a search is going on, assume that we will find
C     an array until proven otherwise.
C
      FOUND = .TRUE.


C
C     Either there are more summaries left in this record, or
C     there aren't. If there are, just incrementing the pointer
C     is sufficient. If there aren't, we have to find the next
C     record and point to the first array there. (If that
C     record is empty, or doesn't exist, then there are simply
C     no more arrays to be found.)
C

      STCURR(P) = STCURR(P) + 1

      IF ( STCURR(P) .GT. STNSEG(P) ) THEN

         IF ( STNEXT(P) .EQ. 0 ) THEN
C
C           There are no more arrays in the list.
C
            FOUND     =  .FALSE.
C
C           Make sure that the array pointer stays pointing to
C           the position following the end of the list. Otherwise,
C           a call to DAFFPA might fail to find the last array in
C           the list.
C
            STCURR(P) =   STNSEG(P) + 1
C
C           The careful reader may note that we're not updating any
C           of the pointers
C
C              STTHIS
C              STNEXT
C              STPREV
C
C           These will not be accessed if there is no current array.
C           If the array pointer is backed up again by a call to
C           DAFFPA, the values we have right now will be correct.
C
         ELSE

            CALL DAFGSR ( STFH(P), STNEXT(P), 1, 128, STSR(1,P), FND )

            IF ( .NOT. FND ) THEN

               CALL DAFHFN ( STFH(P), DAFNAM )

               CALL SETMSG ( 'Attempt to read descriptor record # '   //
     .                       'of DAF ''#'' failed; record was '       //
     .                       'not found. This condition may indicate '//
     .                       'a corrupted DAF.' )
               CALL ERRINT ( '#', STNEXT(P)                            )
               CALL ERRCH  ( '#', DAFNAM                               )
               CALL SIGERR ( 'SPICE(RECORDNOTFOUND)'                   )
               CALL CHKOUT ( 'DAFFNA'                                  )
               RETURN

            END IF

C
C           The name (character) record we've saved no longer applies
C           to the current summary record. However, we've just updated
C           the summary record, so the summary record remains valid.
C
            STHVNR( P )  = .FALSE.

            STTHIS( P )  =  STNEXT(P)
            STNEXT( P )  =  INT ( STSR(1,P) )
            STPREV( P )  =  INT ( STSR(2,P) )
            STNSEG( P )  =  INT ( STSR(3,P) )
            STCURR( P )  =  1

            FOUND        = ( STNSEG(P) .GT. 0 )

         END IF

      END IF

      CALL CHKOUT ( 'DAFFNA' )
      RETURN




C$Procedure DAFBBS ( DAF, begin backward search )

      ENTRY DAFBBS ( HANDLE )

C$ Abstract
C
C     Begin a backward search for arrays in a DAF.
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
C     DAF
C
C$ Keywords
C
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of DAF to be searched.
C
C$ Detailed_Input
C
C     HANDLE      is the handle of a DAF on which a backward
C                 search is to be conducted.
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
C     1)  If the input handle is invalid, the error will be diagnosed
C         by routines called by this routine.
C
C     2)  If DAFBSR retuns with the read success flag as false, a
C         SPICE(RECORDNOTFOUND) error signals.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     See DAFFA.
C
C$ Examples
C
C     Example (1):
C
C        See DAFFA.
C
C     Example (2):
C
C     Use a simple routine to output the double precision and integer
C     values stored in an SPK's segments descriptors. This function
C     opens a DAF for read, performs a backward search for the DAF
C     arrays, prints segments description for each array found, then
C     closes the DAF.
C
C           PROGRAM DAFB_T
C
C           INTEGER             HANDLE
C
C     C
C     C     Define the summary parameters appropriate
C     C     for an SPK file.
C     C
C           INTEGER             ND
C           PARAMETER         ( ND = 2 )
C
C           INTEGER             NI
C           PARAMETER         ( NI = 6 )
C
C           INTEGER             IC( NI )
C
C           DOUBLE PRECISION    DC( ND )
C
C           CHARACTER*(32)      KERNEL
C
C           LOGICAL             FOUND
C
C
C     C
C     C     Open a DAF for read. Return a HANDLE referring to the file.
C     C
C           KERNEL = 'de421.bsp'
C           CALL DAFOPR ( KERNEL, HANDLE )
C
C     C
C     C     Begin a backward search on the file.
C     C
C           CALL DAFBBS ( HANDLE )
C
C     C
C     C     Search until a DAF array is found.
C     C
C           CALL DAFFPA ( FOUND )
C
C     C
C     C     Loop while the search finds subsequent DAF arrays.
C     C
C           DO WHILE ( FOUND )
C
C              CALL DAFGS ( SUM )
C              CALL DAFUS ( SUM, ND, NI, DC, IC )
C
C              WRITE(*,*)                'Doubles: ', DC(1:ND)
C              WRITE(*, FMT='(A,6I9)' ) 'Integers: ', IC(1:NI)
C
C     C
C     C        Check for another segment.
C     C
C              CALL DAFFPA ( FOUND )
C
C           END DO
C
C     C
C     C     Safely close the DAF.
C     C
C           CALL DAFCLS ( HANDLE )
C
C           END
C
C     The program outputs:
C
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       499        4        1        2  2098633  2098644
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       299        2        1        2  2098621  2098632
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       199        1        1        2  2098609  2098620
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       399        3        1        2  1521325  2098608
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       301        3        1        2   944041  1521324
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:        10        0        1        2   820837   944040
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         9        0        1        2   785633   820836
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         8        0        1        2   750429   785632
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         7        0        1        2   715225   750428
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         6        0        1        2   674741   715224
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         5        0        1        2   628977   674740
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         4        0        1        2   567373   628976
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         3        0        1        2   423049   567372
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         2        0        1        2   310405   423048
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         1        0        1        2      641   310404
C
C      Note, the final entries in the integer arrays record the segment
C      start/end indexes. The output indicates the search proceeded
C      from the end of the file (high value index) towards the beginning
C      (low value index).
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
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.1.0, 10-OCT-2012 (EDW)
C
C        Added a functional code example to the Examples section.
C
C        Added check on value of "found" boolean returned from
C        DAFGSR calls. Failure to check this value can cause an
C        infinite loop during segment searches on damaged SPKs.
C
C        Eliminated unneeded Revisions section.
C
C        Removed the obsolete Reference citation to "NAIF
C        Document 167.0."
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
C
C        Updated to support simultaneous searches of multiple DAFs.
C
C        This routine now makes the DAF designated by HANDLE the
C        current DAF---the one at the head of the active list. All
C        saved state variables used by this routine are now part of the
C        state table, or its associated set of pointers.
C
C        Also, the $Exceptions section was filled out.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&

C$ Index_Entries
C
C     begin DAF backward search
C
C-&

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFBBS' )
      END IF

C
C     Check out the file handle before going any further.
C
      CALL DAFSIH ( HANDLE, 'READ' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFBBS' )
         RETURN
      END IF

C
C     Initialize the state table pool, if this hasn't been done yet.
C     Also initialize the cell used to obtain the set of handles of
C     open DAFs.
C
      IF ( FIRST ) THEN

         CALL SSIZEI ( FTSIZE, OPNSET )

         DO I = 1,  TBSIZE - 1
            STPOOL(I)   =  I + 1
         END DO

         STPOOL(TBSIZE) =  NIL
         STFPTR         =  1
         FIRST          = .FALSE.

      END IF

C
C     See whether we already have an entry for this DAF in the
C     state table. Find the previous node if possible.
C
      P     =   STHEAD
      PREV  =   NIL
      FND   =  .FALSE.

      DO WHILE (  ( P .NE. NIL )  .AND.  ( .NOT. FND )  )

         IF ( STFH(P) .EQ. HANDLE ) THEN
            FND   =  .TRUE.
         ELSE
            PREV  =  P
            P     =  STPOOL( P )
         END IF

      END DO

C
C     At this point, either FND is false, or P points to a
C     state table entry describing the DAF indicated by HANDLE.
C     In the latter case, PREV is the predecessor of P.
C
      IF ( FND ) THEN
C
C        We already have a dossier on this DAF. We already have
C        the information on the summary format, but we must re-set
C        our summary record pointers and our name record availability
C        flag.
C
C        Rather than doing the update here, we do it outside of this
C        IF block. That way, the update gets done in just one place.
C        This just makes life easier:  if the collection of state
C        variables is changed, there are fewer places to forget to
C        make the required code changes.
C
C        Move the node for this DAF to the head of the active list,
C        if it is not already there:
C
C           - Make the predecessor of P point to the successor of P.
C
C           - Make P point to the head of the active list.
C
C           - Make P the active list head node.
C
C
         IF ( P .NE. STHEAD ) THEN
C
C           P is in the active list, but is not at the head. So,
C           the predecessor of P is not NIL.
C
            STPOOL(PREV)  =  STPOOL(P)
            STPOOL(P)     =  STHEAD
            STHEAD        =  P

         END IF


      ELSE
C
C        We don't yet have any information on this DAF. Make a new
C        state table entry for the DAF. We may need to make room for
C        the new information by freeing space allocated to DAFs that
C        are no longer open.
C
         IF ( STFPTR .EQ. NIL ) THEN
C
C           Oops, we're out of space. Time for garbage collection.
C           Test each file handle to see whether it designates a DAF
C           that is still open. DAFHOF will tell us which handles
C           point to open DAFs.
C
            CALL DAFHOF ( OPNSET )

            P     =  STHEAD
            PREV  =  NIL
C
C           For every DAF file represented in the state table, we'll
C           delete the corresponding state information if the DAF is
C           now closed. We traverse the active list, examining each
C           file handle as we go.
C
            DO WHILE ( P .NE. NIL )

               IF (  ELEMI ( STFH(P), OPNSET )   )  THEN
C
C                 The file is open. Have a look at the next node.
C
                  PREV = P
                  P    = STPOOL( P )

               ELSE
C
C                 This file handle is not on the list, so free the
C                 node pointing to the information about the DAF it
C                 designated:
C
C                    - Save the successor of P.
C
C                    - Link the predecessor of node P to the successor
C                      of P, if the predecessor is not NIL.
C
C                    - If it happens that P is the head node of the
C                      active list, set the head equal to the
C                      successor of P.
C
C                    - Link P into the free list.
C
C                    - Set P equal to its saved successor.
C
C                    - (PREV remains unchanged.)
C
C
                  NEXTP  =  STPOOL( P )


                  IF ( P .EQ. STHEAD ) THEN
C
C                    Re-assign STHEAD so that we don't lose the head
C                    of the active list. P has no predecessor in this
C                    case, so there's no need to set the forward pointer
C                    of node PREV.
C
                     STHEAD = NEXTP

                  ELSE
C
C                    Since P is not the head node of the active list,
C                    PREV is not NIL, so we'll need to set the forward
C                    pointer of node PREV.
C
                     STPOOL( PREV ) = NEXTP

                  END IF


                  STPOOL( P )  =  STFPTR
                  STFPTR       =  P
                  P            =  NEXTP

               END IF

            END DO
C
C           At this point, we've freed all nodes from the active
C           list that were used to index information about DAFs that
C           are no longer open. If there's any more room in the state
C           table, we have it now.
C
         END IF

C
C        If there still is no room, there is a bug in DAFAH, since DAFAH
C        should not allow more than TBSIZE DAFs to be open. So, we
C        assume that we've found some room. The first free node is
C        indicated by STFPTR. We'll allocate this node and use it to
C        index the state information for the new DAF.
C
         P  =  STFPTR
C
C        Update the free list pointer, link P to the previous head
C        of the active list, and make P the head of the active list.
C
         STFPTR       =  STPOOL( P )
         STPOOL( P )  =  STHEAD
         STHEAD       =  P

      END IF

C
C     At this point, P is the head node of the active list, and P is
C     the index in the state table of the information for the current
C     DAF.
C

C
C     Read the file record and last summary record. Do not read the
C     corresponding name record until necessary. In most searches,
C     names are of no interest.
C
      CALL DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )
      CALL DAFGSR ( HANDLE, BWARD, 1, 128, STSR(1,P), FND )

      IF ( .NOT. FND ) THEN

         CALL DAFHFN ( STFH(P), DAFNAM )

         CALL SETMSG ( 'Attempt to read descriptor record # '    //
     .                 'of DAF ''#'' failed; record was '        //
     .                 'not found. This condition may indicate ' //
     .                 'a corrupted DAF.' )
         CALL ERRINT ( '#', STNEXT(P)                            )
         CALL ERRCH  ( '#', DAFNAM                               )
         CALL SIGERR ( 'SPICE(RECORDNOTFOUND)'                   )
         CALL CHKOUT ( 'DAFBBS'                                  )
         RETURN

      END IF

      STFH  ( P )  =  HANDLE
      STTHIS( P )  =  BWARD
      STNEXT( P )  =  INT ( STSR(1,P) )
      STPREV( P )  =  INT ( STSR(2,P) )
      STNSEG( P )  =  INT ( STSR(3,P) )
      STHVNR( P )  = .FALSE.

C
C     The arrays are returned in backward order from each summary
C     record.
C
      STCURR( P )  =  STNSEG( P ) + 1

      CALL CHKOUT ( 'DAFBBS' )
      RETURN




C$Procedure DAFFPA ( DAF, find previous array )

      ENTRY DAFFPA ( FOUND )

C$ Abstract
C
C     Find the previous (backward) array in the current DAF.
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
C     DAF
C
C$ Keywords
C
C     FILES
C
C$ Declarations
C
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     FOUND      O   True if an array was found.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     FOUND       is true if an array was found, and is false if,
C                 when this routine is called, the current array is
C                 the head of the array list. (Recall that the
C                 arrays in a DAF may be viewed as a doubly linked
C                 list, with the head being the first array in the
C                 file.)
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If this routine is called before a search is begun, the
C        error SPICE(DAFNOSEARCH) is signaled.
C
C     2) If the DAF to be searched has actually been closed, the error
C        will be diagnosed by routines called by this routine.
C
C     3) If the beginning of the array list has already been reached
C        when this routine is called, this routine will not change the
C        current array. FOUND will be false on output.
C
C     4) If DAFBSR retuns with the read success flag as false, a
C        SPICE(RECORDNOTFOUND) error signals.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     See DAFFA.
C
C$ Examples
C
C     Example (1):
C
C        See DAFFA.
C
C     Example (2):
C
C     Use a simple routine to output the double precision and integer
C     values stored in an SPK's segments descriptors. This function
C     opens a DAF for read, performs a backward search for the DAF
C     arrays, prints segments description for each array found, then
C     closes the DAF.
C
C           PROGRAM DAFB_T
C
C           INTEGER             HANDLE
C
C     C
C     C     Define the summary parameters appropriate
C     C     for an SPK file.
C     C
C           INTEGER             ND
C           PARAMETER         ( ND = 2 )
C
C           INTEGER             NI
C           PARAMETER         ( NI = 6 )
C
C           INTEGER             IC( NI )
C
C           DOUBLE PRECISION    DC( ND )
C
C           CHARACTER*(32)      KERNEL
C
C           LOGICAL             FOUND
C
C
C     C
C     C     Open a DAF for read. Return a HANDLE referring to the file.
C     C
C           KERNEL = 'de421.bsp'
C           CALL DAFOPR ( KERNEL, HANDLE )
C
C     C
C     C     Begin a backward search on the file.
C     C
C           CALL DAFBBS ( HANDLE )
C
C     C
C     C     Search until a DAF array is found.
C     C
C           CALL DAFFPA ( FOUND )
C
C     C
C     C     Loop while the search finds subsequent DAF arrays.
C     C
C           DO WHILE ( FOUND )
C
C              CALL DAFGS ( SUM )
C              CALL DAFUS ( SUM, ND, NI, DC, IC )
C
C              WRITE(*,*)                'Doubles: ', DC(1:ND)
C              WRITE(*, FMT='(A,6I9)' ) 'Integers: ', IC(1:NI)
C
C     C
C     C        Check for another segment.
C     C
C              CALL DAFFPA ( FOUND )
C
C           END DO
C
C     C
C     C     Safely close the DAF.
C     C
C           CALL DAFCLS ( HANDLE )
C
C           END
C
C     The program outputs:
C
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       499        4        1        2  2098633  2098644
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       299        2        1        2  2098621  2098632
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       199        1        1        2  2098609  2098620
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       399        3        1        2  1521325  2098608
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       301        3        1        2   944041  1521324
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:        10        0        1        2   820837   944040
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         9        0        1        2   785633   820836
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         8        0        1        2   750429   785632
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         7        0        1        2   715225   750428
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         6        0        1        2   674741   715224
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         5        0        1        2   628977   674740
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         4        0        1        2   567373   628976
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         3        0        1        2   423049   567372
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         2        0        1        2   310405   423048
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         1        0        1        2      641   310404
C
C      Note, the final entries in the integer arrays record the segment
C      start/end indexes. The output indicates the search proceeded
C      from the end of the file (high value index) towards the beginning
C      (low value index).
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
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.1.0, 10-OCT-2012 (EDW)
C
C        Added a functional code example to the Examples section.
C
C        Added check on value of "found" boolean returned from
C        DAFGSR calls. Failure to check this value can cause an
C        infinite loop during segment searches on damaged SPKs.
C
C        Eliminated unneeded Revisions section.
C
C        Removed the obsolete Reference citation to "NAIF
C        Document 167.0."
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
C
C        Updated to support simultaneous searches of multiple DAFs.
C
C        This routine now operates on the current DAF---the one at
C        the head of the active list. All saved state variables
C        used by this routine are now part of the state table, or
C        its associated set of pointers.
C
C        Also, a bug fix was made to the array pointer adjustment
C        algorithm:  the pointer is no longer decremented if it
C        is already less than 1 and the array summary pointer
C        is already pointing to the first array summary. In
C        addition, a test made to detect this condition was fixed:
C        the test
C
C           CURR .EQ. 0
C
C        was replaced by
C
C           STCURR(P) .LE. 0
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&

C$ Index_Entries
C
C     find previous DAF array
C
C-&


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFFPA' )
      END IF


C
C     Operate on the last DAF in which a search has been started.
C
      P  =  STHEAD

C
C     FOUND will be false until we make it past the error checks.
C
      FOUND = .FALSE.

C
C     Make sure that a search has been started in this DAF.
C
      IF ( P .EQ. NIL ) THEN

         CALL SETMSG ( 'No DAF is currently being searched.' )
         CALL SIGERR ( 'SPICE(DAFNOSEARCH)'                  )
         CALL CHKOUT ( 'DAFFPA'                              )
         RETURN

C
C     Make sure that the `current' DAF is still open.
C
      ELSE

         CALL DAFSIH ( STFH(P), 'READ' )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DAFFPA' )
            RETURN
         END IF

      END IF

C
C     Now that we know a search is going on, assume that we will find
C     an array until proven otherwise.
C
      FOUND = .TRUE.

C
C     Either there are more summaries left in this record, or
C     there aren't. If there are, just decrementing the pointer
C     is sufficient. If there aren't, we have to find the previous
C     record and point to the last array there. (If that
C     record is empty, or doesn't exist, then there are simply
C     no more arrays to be found.)
C
      STCURR( P )  =  STCURR( P ) - 1

      IF ( STCURR(P) .LE. 0 ) THEN

         IF ( STPREV(P) .EQ. 0 ) THEN
C
C           There is no predecessor of the current array in the list.
C
            FOUND = .FALSE.
C
C           Make sure that the array pointer stays pointing to
C           the position preceding the front of the list. Otherwise,
C           a call to DAFFNA might fail to find the first array in
C           the list.
C
            STCURR(P) = 0
C
C           The careful reader may note that we're not updating any
C           of the pointers
C
C              STTHIS
C              STNEXT
C              STPREV
C
C           These will not be accessed if there is no current array.
C           If the array pointer is moved forward again by a call to
C           DAFFNA, the values we have right now will be correct.
C
         ELSE

            CALL DAFGSR ( STFH(P), STPREV(P), 1, 128, STSR(1,P), FND )

            IF ( .NOT. FND ) THEN

               CALL DAFHFN ( STFH(P), DAFNAM )

               CALL SETMSG ( 'Attempt to read descriptor record # '   //
     .                       'of DAF ''#'' failed; record was '       //
     .                       'not found. This condition may indicate '//
     .                       'a corrupted DAF.' )
               CALL ERRINT ( '#', STNEXT(P)                            )
               CALL ERRCH  ( '#', DAFNAM                               )
               CALL SIGERR ( 'SPICE(RECORDNOTFOUND)'                   )
               CALL CHKOUT ( 'DAFFPA'                                  )
               RETURN

            END IF

C
C           The name (character) record we've saved no longer applies
C           to the current summary record. However, we've just updated
C           the summary record, so the summary record remains valid.
C
            STHVNR( P )  = .FALSE.

            STTHIS( P )  =  STPREV( P )
            STNEXT( P )  =  INT ( STSR(1,P) )
            STPREV( P )  =  INT ( STSR(2,P) )
            STNSEG( P )  =  INT ( STSR(3,P) )
            STCURR( P )  =  STNSEG( P )

            FOUND        =  ( STNSEG(P) .GT. 0 )

         END IF

      END IF

      CALL CHKOUT ( 'DAFFPA' )
      RETURN




C$Procedure DAFGS ( DAF, get summary )

      ENTRY DAFGS ( SUM )

C$ Abstract
C
C     Return (get) the summary for the current array in the current
C     DAF.
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
C     DAF
C
C$ Keywords
C
C     FILES
C
C$ Declarations
C
C     DOUBLE PRECISION      SUM    ( * )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SUM        O   Summary for current array.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     SUM         is the summary for the current array (the array
C                 found by the latest call to DAFFNA or DAFFPA).
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If this routine is called when no search is in progress in the
C         the current DAF, the error SPICE(DAFNOSEARCH) is signaled.
C
C     2)  If the DAF for which the `current' array's summary is to be
C         returned has actually been closed, the error will be diagnosed
C         by routines called by this routine.
C
C     3)  If no array is current in the current DAF, the error
C         SPICE(NOCURRENTARRAY) is signaled. There is no current
C         array when a search is started by DAFBFS or DAFBBS, but no
C         calls to DAFFNA or DAFBNA have been made yet, or whenever
C         DAFFNA or DAFFPA return the value .FALSE. in the FOUND
C         argument.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     See DAFFA.
C
C$ Examples
C
C     Example (1):
C
C        See DAFFA.
C
C     Example (2):
C
C     Use a simple routine to output the double precision and integer
C     values stored in an SPK's segments descriptors. This function
C     opens a DAF for read, performs a forwards search for the DAF
C     arrays, prints segments description for each array found, then
C     closes the DAF.
C
C           PROGRAM DAF_T
C
C           INTEGER             HANDLE
C
C     C
C     C     Define the summary parameters appropriate
C     C     for an SPK file.
C     C
C           INTEGER             ND
C           PARAMETER         ( ND = 2 )
C
C           INTEGER             NI
C           PARAMETER         ( NI = 6 )
C
C           INTEGER             IC( NI )
C
C           DOUBLE PRECISION    DC( ND )
C
C           CHARACTER*(32)      KERNEL
C
C           LOGICAL             FOUND
C
C
C     C
C     C     Open a DAF for read. Return a HANDLE referring to the file.
C     C
C           KERNEL = 'de421.bsp'
C           CALL DAFOPR ( KERNEL, HANDLE )
C
C     C
C     C     Begin a forward search on the file.
C     C
C           CALL DAFBFS ( HANDLE )
C
C     C
C     C     Search until a DAF array is found.
C     C
C           CALL DAFFNA ( FOUND )
C
C     C
C     C     Loop while the search finds subsequent DAF arrays.
C     C
C           DO WHILE ( FOUND )
C
C              CALL DAFGS ( SUM )
C              CALL DAFUS ( SUM, ND, NI, DC, IC )
C
C              WRITE(*,*)                'Doubles: ', DC(1:ND)
C              WRITE(*, FMT='(A,6I9)' ) 'Integers: ', IC(1:NI)
C
C     C
C     C        Check for another segment.
C     C
C              CALL DAFFNA ( FOUND )
C
C           END DO
C
C     C
C     C     Safely close the DAF.
C     C
C           CALL DAFCLS ( HANDLE )
C
C           END
C
C     The program outputs:
C
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         1        0        1        2      641   310404
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         2        0        1        2   310405   423048
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         3        0        1        2   423049   567372
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         4        0        1        2   567373   628976
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         5        0        1        2   628977   674740
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         6        0        1        2   674741   715224
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         7        0        1        2   715225   750428
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         8        0        1        2   750429   785632
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         9        0        1        2   785633   820836
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:        10        0        1        2   820837   944040
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       301        3        1        2   944041  1521324
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       399        3        1        2  1521325  2098608
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       199        1        1        2  2098609  2098620
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       299        2        1        2  2098621  2098632
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       499        4        1        2  2098633  2098644
C
C      Note, the final entries in the integer array contains the segment
C      start/end indexes. The output indicates the search proceeded
C      from the start of the file (low value index) towards the end
C      (high value index).
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
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.2, 10-OCT-2012 (EDW)
C
C        Added a functional code example to the Examples section.
C
C        Eliminated unneeded Revisions section.
C
C        Removed the obsolete Reference citation to "NAIF
C        Document 167.0."
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
C
C        Updated to support simultaneous searches of multiple DAFs.
C        Bug fix made to handle case of having no current array.
C
C        This routine now operates on the current DAF---the one at
C        the head of the active list. All saved state variables
C        used by this routine are now part of the state table, or
C        its associated set of pointers.
C
C        In addition, this routine now checks whether an array
C        is current before trying to read its summary. The routine
C        previously crashed under these conditions.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&

C$ Index_Entries
C
C     get DAF summary
C
C-&


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFGS' )
      END IF

C
C     Operate on the last DAF in which a search has been started.
C
      P  =  STHEAD

C
C     Make sure that a search has been started in this DAF.
C
      IF ( P .EQ. NIL ) THEN

         CALL SETMSG ( 'No DAF is currently being searched.' )
         CALL SIGERR ( 'SPICE(DAFNOSEARCH)'                  )
         CALL CHKOUT ( 'DAFGS'                               )
         RETURN

C
C     Make sure that the `current' DAF is still open.
C
      ELSE

         CALL DAFSIH ( STFH(P), 'READ' )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DAFGS' )
            RETURN
         END IF

      END IF

C
C     Check the current pointer position to make sure that it's in
C     bounds. If there is no current array, then we cannot return
C     a summary. This situation occurs if DAFFNA was called when the
C     current array was the last, or if DAFFPA was called when the
C     current array was the first.
C
      IF ( STCURR(P) .EQ. 0 ) THEN

         CALL DAFHFN ( STFH(P), DAFNAM )

         CALL SETMSG ( 'No array is current; the `next'' array is '   //
     .                 'the first array of DAF #'                      )
         CALL ERRCH  ( '#',     DAFNAM                                 )
         CALL SIGERR ( 'SPICE(NOCURRENTARRAY)'                         )
         CALL CHKOUT ( 'DAFGS'                                         )
         RETURN

      ELSE IF ( STCURR(P) .GT. STNSEG(P) ) THEN

         CALL DAFHFN ( STFH(P), DAFNAM )

         CALL SETMSG ( 'No array is current; the `previous'' array is'//
     .                 ' the last array of DAF #'                      )
         CALL ERRCH  ( '#',     DAFNAM                                 )
         CALL SIGERR ( 'SPICE(NOCURRENTARRAY)'                         )
         CALL CHKOUT ( 'DAFGS'                                         )
         RETURN

      END IF

C
C     The location of the summary depends on the current pointer
C     position.
C
      CALL DAFHSF ( STFH(P), ND, NI )

      SUMSIZ  =  ND   +   ( NI + 1 ) / 2

      OFFSET  =  3    +   ( STCURR(P) - 1 )  *  SUMSIZ

      CALL MOVED (  STSR( OFFSET+1, P ),  SUMSIZ,  SUM )

      CALL CHKOUT ( 'DAFGS' )
      RETURN




C$Procedure DAFGN ( DAF, get array name )

      ENTRY DAFGN ( NAME )

C$ Abstract
C
C     Return (get) the name for the current array in the current DAF.
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
C     DAF
C
C$ Keywords
C
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         NAME
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME       O   Name of current array.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     NAME        is the name for the current array (the array
C                 found by the latest call to DAFFNA or DAFFPA).
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If this routine is called when no search is in progress in the
C         the current DAF, the error SPICE(DAFNOSEARCH) is signaled.
C
C     2)  If the DAF for which the `current' array's name is to be
C         returned has actually been closed, the error will be diagnosed
C         by routines called by this routine.
C
C     3)  If no array is current in the current DAF, the error
C         SPICE(NOCURRENTARRAY) is signaled. There is no current
C         array when a search is started by DAFBFS or DAFBBS, but no
C         calls to DAFFNA or DAFBNA have been made yet, or whenever
C         DAFFNA or DAFFPA return the value .FALSE. in the FOUND
C         argument.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     See DAFFA.
C
C$ Examples
C
C     See DAFFA.
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
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.2, 18-AUG-2011 (EDW)
C
C        Eliminated unneeded Revisions section.
C
C        Removed the obsolete Reference citation to "NAIF
C        Document 167.0."
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
C
C        Updated to support simultaneous searches of multiple DAFs.
C        Bug fix made to handle case of having no current array.
C
C        This routine now operates on the current DAF---the one at
C        the head of the active list. All saved state variables
C        used by this routine are now part of the state table, or
C        its associated set of pointers.
C
C        In addition, this routine now checks whether an array
C        is current before trying to read its summary. The routine
C        previously crashed under these conditions.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&

C$ Index_Entries
C
C     get DAF array name
C
C-&


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFGN' )
      END IF


C
C     Operate on the last DAF in which a search has been started.
C
      P  =  STHEAD

C
C     Make sure that a search has been started in this DAF.
C
      IF ( P .EQ. NIL ) THEN

         CALL SETMSG ( 'No DAF is currently being searched.' )
         CALL SIGERR ( 'SPICE(DAFNOSEARCH)'                  )
         CALL CHKOUT ( 'DAFGN'                               )
         RETURN

C
C     Make sure that the `current' DAF is still open.
C
      ELSE

         CALL DAFSIH ( STFH(P), 'READ' )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DAFGN' )
            RETURN
         END IF

      END IF

C
C     Check the current pointer position to make sure that it's in
C     bounds. If there is no current array, then we cannot get the
C     array's summary's name. This situation occurs if DAFFNA was
C     called when the current array was the last, or if DAFFPA was
C     called when the current array was the first.
C
      IF ( STCURR(P) .EQ. 0 ) THEN

         CALL DAFHFN ( STFH(P), DAFNAM )

         CALL SETMSG ( 'No array is current; the `next'' array is '   //
     .                 'the first array of DAF #'                      )
         CALL ERRCH  ( '#',     DAFNAM                                 )
         CALL SIGERR ( 'SPICE(NOCURRENTARRAY)'                         )
         CALL CHKOUT ( 'DAFGN'                                         )
         RETURN

      ELSE IF ( STCURR(P) .GT. STNSEG(P) ) THEN

         CALL DAFHFN ( STFH(P), DAFNAM )

         CALL SETMSG ( 'No array is current; the `previous'' array is'//
     .                 ' the last array of DAF #'                      )
         CALL ERRCH  ( '#',     DAFNAM                                 )
         CALL SIGERR ( 'SPICE(NOCURRENTARRAY)'                         )
         CALL CHKOUT ( 'DAFGN'                                         )
         RETURN

      END IF


C
C     Read the name record for this summary record, if we don't have it
C     already.
C
      IF ( .NOT. STHVNR(P) ) THEN

         CALL DAFRCR ( STFH(P), STTHIS(P)+1, STNR(P) )

         STHVNR( P )  =  .TRUE.

      END IF


C
C     The location of the name depends on the current pointer
C     position.
C
      CALL DAFHSF ( STFH(P), ND, NI )

      SUMSIZ  =  ND   +   ( NI + 1 ) / 2

      NAMSIZ  =  SUMSIZ * 8

      OFFSET  =  ( STCURR(P) - 1 )  *  NAMSIZ

      NAME    =  STNR(P) (  OFFSET + 1  :  OFFSET + NAMSIZ  )

      CALL CHKOUT ( 'DAFGN' )
      RETURN





C$Procedure DAFGH ( DAF, get handle )

      ENTRY DAFGH ( HANDLE )

C$ Abstract
C
C     Return (get) the handle of the DAF currently being searched.
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
C     DAF
C
C$ Keywords
C
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     O   Handle for current DAF.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     HANDLE      is the handle for the current DAF (the handle
C                 connected to the DAF that is currently being
C                 searched).
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If this routine is called when no search is in progress in the
C         the current DAF, the error SPICE(DAFNOSEARCH) is signaled.
C
C     2)  If the DAF whose handle is to be returned has actually been
C         closed, the error will be diagnosed by routines called by
C         this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Under rare circumstances, it may be necessary to identify
C     the particular DAF that is being searched (such as when
C     the search is begun by one module and continued by another).
C
C$ Examples
C
C     Consider a program like the following, which examines the
C     individual arrays in a DAF and examines the contents of those
C     meeting certain criteria.
C
C        CALL DAFOPW ( FNAME, HANDLE )
C        CALL DAFBFS ( HANDLE )
C        CALL DAFFNA ( FOUND  )
C
C        DO WHILE ( FOUND )
C           CALL CHECK_DAF ( STATUS )
C
C           IF ( STATUS .EQ. 'EXAMINE' ) THEN
C              CALL EXAMINE_DAF
C           END IF
C
C           CALL DAFFNA ( FOUND )
C        END DO
C
C     The subroutine CHECK_DAF, which assumes that a search is in
C     progress, gets the summary and name for the current array, and
C     uses them to decide whether the data in the array merit further
C     consideration.
C
C        SUBROUTINE CHECK_DAF ( STATUS )
C
C        CALL DAFGS ( SUM )
C        CALL DAFGN ( NAME )
C        CALL DAFUS ( SUM, ND, NI, DC, IC )
C         .
C         .
C
C     The subroutine EXAMINE_DAF needs to examine the data in
C     the array itself. In order to do do, it needs to have access
C     not only to the summary, but to the handle of the file
C     containing the array. This is provided by DAFGH.
C
C        SUBROUTINE EXAMINE_DAF
C
C        CALL DAFGS ( SUM  )
C        CALL DAFGH ( HANDLE )
C        CALL DAFUS ( SUM, ND, NI, DC, IC )
C
C        CALL DAFRDA ( HANDLE, BEGIN, END, DATA )
C         .
C         .
C
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
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.2, 18-AUG-2011 (EDW)
C
C        Eliminated unneeded Revisions section.
C
C        Removed the obsolete Reference citation to "NAIF
C        Document 167.0."
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
C
C        Updated to support simultaneous searches of multiple DAFs.
C
C        This routine now operates on the current DAF---the one at
C        the head of the active list. All saved state variables
C        used by this routine are now part of the state table, or
C        its associated set of pointers.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&

C$ Index_Entries
C
C     get DAF handle
C
C-&


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFGH' )
      END IF

C
C     Operate on the last DAF in which a search has been started.
C
      P  =  STHEAD

C
C     Make sure that a search has been started in this DAF.
C
      IF ( P .EQ. NIL ) THEN

         CALL SETMSG ( 'No DAF is currently being searched.' )
         CALL SIGERR ( 'SPICE(DAFNOSEARCH)'                  )
         CALL CHKOUT ( 'DAFGH'                               )
         RETURN

C
C     Make sure that the `current' DAF is still open.
C
      ELSE

         CALL DAFSIH ( STFH(P), 'READ' )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DAFGH' )
            RETURN
         END IF

      END IF

      HANDLE = STFH( P )

      CALL CHKOUT ( 'DAFGH' )
      RETURN





C$Procedure DAFRS ( DAF, replace summary )

      ENTRY DAFRS ( SUM )

C$ Abstract
C
C     Change the summary for the current array in the current DAF.
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
C     DAF
C
C$ Keywords
C
C     FILES
C
C$ Declarations
C
C     DOUBLE PRECISION      SUM
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SUM        I   New summary for current array.
C
C$ Detailed_Input
C
C     SUM         is the new summary for the current array. This
C                 replaces the existing summary. However, the addresses
C                 (the final two integer components) of the original
C                 summary are not changed.
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
C     1)  If this routine is called when no search is in progress in the
C         the current DAF, the error SPICE(DAFNOSEARCH) is signaled.
C
C     2)  If the DAF containing the `current' array has actually been
C         closed, the error will be diagnosed by routines called by
C         this routine.
C
C     3)  If the DAF containing the `current' array is not open for
C         writing, the error will be diagnosed by routines called by
C         this routine.
C
C     4)  If no array is current in the current DAF, the error
C         SPICE(NOCURRENTARRAY) is signaled. There is no current
C         array when a search is started by DAFBFS or DAFBBS, but no
C         calls to DAFFNA or DAFBNA have been made yet, or whenever
C         DAFFNA or DAFFPA return the value .FALSE. in the FOUND
C         argument.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     See DAFFA.
C
C$ Examples
C
C     See DAFFA.
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
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.2, 18-AUG-2011 (EDW)
C
C        Eliminated unneeded Revisions section.
C
C        Removed the obsolete Reference citation to "NAIF
C        Document 167.0."
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
C
C        Updated to support simultaneous searches of multiple DAFs.
C        Bug fix made to handle case of having no current array.
C
C        This routine now operates on the current DAF---the one at
C        the head of the active list. All saved state variables
C        used by this routine are now part of the state table, or
C        its associated set of pointers.
C
C        In addition, this routine now checks whether an array
C        is current before trying to read its summary. The routine
C        previously crashed under these conditions.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&

C$ Index_Entries
C
C     replace DAF summary
C
C-&


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFRS' )
      END IF

C
C     Operate on the last DAF in which a search has been started.
C
      P  =  STHEAD

C
C     Make sure that a search has been started in this DAF.
C
      IF ( P .EQ. NIL ) THEN

         CALL SETMSG ( 'No DAF is currently being searched.' )
         CALL SIGERR ( 'SPICE(DAFNOSEARCH)'                  )
         CALL CHKOUT ( 'DAFRS'                               )
         RETURN

C
C     Make sure that the `current' DAF is still open, and that it
C     is open for writing.
C
      ELSE

         CALL DAFSIH ( STFH(P), 'WRITE' )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DAFRS' )
            RETURN
         END IF

      END IF

C
C     Check the current pointer position to make sure that it's in
C     bounds. If there is no current array, then we cannot replace the
C     array's  summary. This situation occurs if DAFFNA was called
C     when the current array was the last, or if DAFFPA was called when
C     the current array was the first.
C
      IF ( STCURR(P) .EQ. 0 ) THEN

         CALL DAFHFN ( STFH(P), DAFNAM )

         CALL SETMSG ( 'No array is current; the `next'' array is '   //
     .                 'the first array of DAF #'                      )
         CALL ERRCH  ( '#',     DAFNAM                                 )
         CALL SIGERR ( 'SPICE(NOCURRENTARRAY)'                         )
         CALL CHKOUT ( 'DAFRS'                                         )
         RETURN

      ELSE IF ( STCURR(P) .GT. STNSEG(P) ) THEN

         CALL DAFHFN ( STFH(P), DAFNAM )

         CALL SETMSG ( 'No array is current; the `previous'' array is'//
     .                 ' the last array of DAF #'                      )
         CALL ERRCH  ( '#',     DAFNAM                                 )
         CALL SIGERR ( 'SPICE(NOCURRENTARRAY)'                         )
         CALL CHKOUT ( 'DAFRS'                                         )
         RETURN

      END IF

C
C     The location of the summary depends on the current pointer
C     position.
C
      CALL DAFHSF ( STFH(P), ND, NI )

      SUMSIZ  =  ND   +   ( NI + 1 ) / 2

      OFFSET  =  3    +   ( STCURR(P) - 1 ) * SUMSIZ

C
C     Get the existing summary, and unpack it. Replace everything
C     but the addresses (the final two integer components), and
C     repack. Then replace the existing summary within the record.
C
      CALL MOVED (  STSR( OFFSET+1, P ),  SUMSIZ,  EXSUM )

      CALL DAFUS ( EXSUM, ND, NI, EXDC,  EXIC  )
      CALL DAFUS ( SUM,   ND, NI, NEWDC, NEWIC )

      CALL MOVED ( NEWDC, ND,   EXDC )
      CALL MOVEI ( NEWIC, NI-2, EXIC )
      CALL DAFPS ( ND, NI, EXDC, EXIC, EXSUM )

      CALL MOVED (  EXSUM,  SUMSIZ,  STSR( OFFSET+1, P )  )

C
C     Rewrite the modified summary record.
C
      CALL DAFWDR ( STFH(P), STTHIS(P), STSR(1,P) )

      CALL CHKOUT ( 'DAFRS' )
      RETURN




C$Procedure DAFRN ( DAF, change array name )

      ENTRY DAFRN ( NAME )

C$ Abstract
C
C     Replace the name for the current array in the current DAF.
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
C     DAF
C
C$ Keywords
C
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         NAME
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME       I   New name for current array.
C
C$ Detailed_Input
C
C     NAME        is the new name for the current array.
C                 This replaces the existing name.
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
C     1)  If this routine is called when no search is in progress in the
C         the current DAF, the error SPICE(DAFNOSEARCH) is signaled.
C
C     2)  If the DAF containing the `current' array has actually been
C         closed, the error will be diagnosed by routines called by
C         this routine.
C
C     3)  If the DAF containing the `current' array is not open for
C         writing, the error will be diagnosed by routines called by
C         this routine.
C
C     4)  If no array is current in the current DAF, the error
C         SPICE(NOCURRENTARRAY) is signaled. There is no current
C         array when a search is started by DAFBFS or DAFBBS, but no
C         calls to DAFFNA or DAFBNA have been made yet, or whenever
C         DAFFNA or DAFFPA return the value .FALSE. in the FOUND
C         argument.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     See DAFFA.
C
C$ Examples
C
C     See DAFFA.
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
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.2, 18-AUG-2011 (EDW)
C
C        Eliminated unneeded Revisions section.
C
C        Removed the obsolete Reference citation to "NAIF
C        Document 167.0."
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
C
C        Updated to support simultaneous searches of multiple DAFs.
C
C        This routine now operates on the current DAF---the one at
C        the head of the active list. All saved state variables
C        used by this routine are now part of the state table, or
C        its associated set of pointers.
C
C        In addition, this routine now checks whether an array
C        is current before trying to read its summary. The routine
C        previously crashed under these conditions.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&

C$ Index_Entries
C
C     change DAF array name
C
C-&


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFRN' )
      END IF

C
C     Operate on the last DAF in which a search has been started.
C
      P  =  STHEAD

C
C     Make sure that a search has been started in this DAF.
C
      IF ( P .EQ. NIL ) THEN

         CALL SETMSG ( 'No DAF is currently being searched.' )
         CALL SIGERR ( 'SPICE(DAFNOSEARCH)'                  )
         CALL CHKOUT ( 'DAFRN'                               )
         RETURN

C
C     Make sure that the `current' DAF is still open, and that it
C     is open for writing.
C
      ELSE

         CALL DAFSIH ( STFH(P), 'WRITE' )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DAFRN' )
            RETURN
         END IF

      END IF

C
C     Check the current pointer position to make sure that it's in
C     bounds. If there is no current array, then we cannot replace
C     the array's summary's name. This situation occurs if DAFFNA was
C     called when the current array was the last, or if DAFFPA was
C     called when the current array was the first.
C
      IF ( STCURR(P) .EQ. 0 ) THEN

         CALL DAFHFN ( STFH(P), DAFNAM )

         CALL SETMSG ( 'No array is current; the `next'' array is '   //
     .                 'the first array of DAF #'                      )
         CALL ERRCH  ( '#',     DAFNAM                                 )
         CALL SIGERR ( 'SPICE(NOCURRENTARRAY)'                         )
         CALL CHKOUT ( 'DAFRN'                                         )
         RETURN

      ELSE IF ( STCURR(P) .GT. STNSEG(P) ) THEN

         CALL DAFHFN ( STFH(P), DAFNAM )

         CALL SETMSG ( 'No array is current; the `previous'' array is'//
     .                 ' the last array of DAF #'                      )
         CALL ERRCH  ( '#',     DAFNAM                                 )
         CALL SIGERR ( 'SPICE(NOCURRENTARRAY)'                         )
         CALL CHKOUT ( 'DAFRN'                                         )
         RETURN

      END IF



C
C     Read the name record for this summary record, if we don't have it
C     already.
C
      IF ( .NOT. STHVNR(P) ) THEN

         CALL DAFRCR ( STFH(P), STTHIS(P)+1, STNR(P) )

         STHVNR( P )  =  .TRUE.

      END IF


C
C     The location of the name depends on the current pointer
C     position.
C
      CALL DAFHSF ( STFH(P), ND, NI )

      SUMSIZ  =  ND   +   ( NI + 1 ) / 2

      NAMSIZ  =  SUMSIZ * 8

      OFFSET  =  ( STCURR(P) - 1 )  *  NAMSIZ

      STNR(P) (  OFFSET + 1  :  OFFSET + NAMSIZ  )   =   NAME

C
C     Rewrite the character record.
C
      CALL DAFWCR ( STFH(P), STTHIS(P)+1, STNR(P) )

      CALL CHKOUT ( 'DAFRN' )
      RETURN




C$Procedure DAFWS ( DAF, write summary )

      ENTRY DAFWS ( SUM )

C$ Abstract
C
C     Write a new summary for the current array in the current DAF.
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
C     DAF
C
C$ Keywords
C
C     FILES
C
C$ Declarations
C
C     DOUBLE PRECISION      SUM ( * )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SUM        I   New summary for current array in the current DAF.
C
C$ Detailed_Input
C
C     SUM         is the new summary for the current array. This
C                 replaces the existing summary, including the
C                 addresses (the final two integer components) of
C                 the original summary.
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
C     1)  If this routine is called when no search is in progress in the
C         the current DAF, the error SPICE(DAFNOSEARCH) is signaled.
C
C     2)  If the DAF containing the `current' array has actually been
C         closed, the error will be diagnosed by routines called by
C         this routine.
C
C     3)  If the DAF containing the `current' array is not open for
C         writing, the error will be diagnosed by routines called by
C         this routine.
C
C     4)  If no array is current in the current DAF, the error
C         SPICE(NOCURRENTARRAY) is signaled. There is no current
C         array when a search is started by DAFBFS or DAFBBS, but no
C         calls to DAFFNA or DAFBNA have been made yet, or whenever
C         DAFFNA or DAFFPA return the value .FALSE. in the FOUND
C         argument.
C
C$ Files
C
C     DAFWS updates the DAF currently being searched. The handle
C     of this DAF can be retrieved using the routine DAFGH.
C
C$ Particulars
C
C     Unless you are reordering the arrays in the file being searched,
C     you should be using DAFRS instead of this routine.
C
C     See also DAFFA, DAFRS.
C
C$ Examples
C
C     See DAFFA.
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
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.2, 18-AUG-2011 (EDW)
C
C        Eliminated unneeded Revisions section.
C
C        Removed the obsolete Reference citation to "NAIF
C        Document 167.0."
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
C
C        Updated to support simultaneous searches of multiple DAFs.
C        Bug fix made to handle case of having no current array.
C
C        This routine now operates on the current DAF---the one at
C        the head of the active list. All saved state variables
C        used by this routine are now part of the state table, or
C        its associated set of pointers.
C
C        In addition, this routine now checks whether an array
C        is current before trying to read its summary. The routine
C        previously crashed under these conditions.
C
C-    SPICELIB Version 1.0.0, 28-MAR-1991 (IMU)
C
C-&

C$ Index_Entries
C
C     write DAF summary
C
C-&


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFWS' )
      END IF

C
C     Operate on the last DAF in which a search has been started.
C
      P  =  STHEAD

C
C     Make sure that a search has been started in this DAF.
C
      IF ( P .EQ. NIL ) THEN

         CALL SETMSG ( 'No DAF is currently being searched.' )
         CALL SIGERR ( 'SPICE(DAFNOSEARCH)'                  )
         CALL CHKOUT ( 'DAFWS'                               )
         RETURN

C
C     Make sure that the `current' DAF is still open, and that it is
C     open for writing.
C
      ELSE

         CALL DAFSIH ( STFH(P), 'READ' )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DAFWS' )
            RETURN
         END IF

      END IF

C
C     Check the current pointer position to make sure that it's in
C     bounds. If there is no current array, then we cannot write a
C     new array summary. This situation occurs if DAFFNA was called
C     when the current array was the last, or if DAFFPA was called
C     when the current array was the first.
C
      IF ( STCURR(P) .EQ. 0 ) THEN

         CALL DAFHFN ( STFH(P), DAFNAM )

         CALL SETMSG ( 'No array is current; the `next'' array is '   //
     .                 'the first array of DAF #'                      )
         CALL ERRCH  ( '#',     DAFNAM                                 )
         CALL SIGERR ( 'SPICE(NOCURRENTARRAY)'                         )
         CALL CHKOUT ( 'DAFWS'                                         )
         RETURN

      ELSE IF ( STCURR(P) .GT. STNSEG(P) ) THEN

         CALL DAFHFN ( STFH(P), DAFNAM )

         CALL SETMSG ( 'No array is current; the `previous'' array is'//
     .                 ' the last array of DAF #'                      )
         CALL ERRCH  ( '#',     DAFNAM                                 )
         CALL SIGERR ( 'SPICE(NOCURRENTARRAY)'                         )
         CALL CHKOUT ( 'DAFWS'                                         )
         RETURN

      END IF



C
C     The location of the summary depends on the current pointer
C     position.
C
      CALL DAFHSF ( STFH(P), ND, NI )

      SUMSIZ  =  ND   +   ( NI + 1 ) / 2

      OFFSET  =  3    +   ( STCURR(P) - 1 ) * SUMSIZ

      CALL MOVED (  SUM,  SUMSIZ,  STSR( OFFSET+1, P )  )

C
C     Rewrite the modified summary record.
C
      CALL DAFWDR ( STFH(P), STTHIS(P), STSR(1,P) )

      CALL CHKOUT ( 'DAFWS' )
      RETURN




C$Procedure DAFCS ( DAF, continue search )

      ENTRY DAFCS ( HANDLE )

C$ Abstract
C
C     Select a DAF that already has a search in progress as the
C     one to continue searching.
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
C     DAF
C
C$ Keywords
C
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of DAF to continue searching.
C
C$ Detailed_Input
C
C     HANDLE         is the handle of a DAF in which either a forward
C                    or backward search has already been started by
C                    DAFBFS or DAFBBS. The DAF may be open for read
C                    or write access.
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
C     1)  If the input handle is invalid, the error will be diagnosed
C         by routines called by this routine.
C
C     2)  If this routine is called when no search is in progress in the
C         the current DAF, the error SPICE(DAFNOSEARCH) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     DAFCS supports simultaneous searching of multiple DAFs. In
C     applications that use this capability, DAFCS should be called
C     prior to each call to DAFFNA, DAFFPA, DAFGN, DAFGS, DAFRS, or
C     DAFWS, to specify which DAF is to be acted upon.
C
C$ Examples
C
C     See DAFFA.
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
C     W.L. Taber     (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 10-FEB-2014 (BVS)
C
C        Added full declaration of HANDLE to the Declarations section
C        of the header.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 04-SEP-1991 (NJB) (WLT)
C
C-&

C$ Index_Entries
C
C     select a DAF to continue searching
C
C-&


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFCS' )
      END IF

C
C     Validate the DAF's handle before going any further. DAFSIH will
C     signal an error if HANDLE doesn't designate an open DAF.
C
      CALL DAFSIH ( HANDLE, 'READ' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFCS' )
         RETURN
      END IF

C
C     See whether we already have an entry for this DAF in the
C     state table. Find the previous node if possible.
C
      P     =   STHEAD
      PREV  =   NIL
      FND   =  .FALSE.

      DO WHILE (  ( P .NE. NIL )  .AND.  ( .NOT. FND )  )

         IF ( STFH(P) .EQ. HANDLE ) THEN
            FND   =  .TRUE.
         ELSE
            PREV  =  P
            P     =  STPOOL( P )
         END IF

      END DO

C
C     Either FND is false, or P is the index in the state table of
C     the DAF specified by HANDLE, and PREV is the predecessor of P.
C

C
C     You can't continue searching a DAF that you're not already
C     searching.
C
      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'No DAF is currently being searched.' )
         CALL SIGERR ( 'SPICE(DAFNOSEARCH)'                  )
         CALL CHKOUT ( 'DAFCS'                               )
         RETURN

      END IF


C
C     Move the node for this DAF to the head of the active list,
C     if it is not already there:
C
C        - Make the predecessor of P point to the successor of P.
C
C        - Make P point to the head of the active list.
C
C        - Make P the active list head node.
C

      IF ( P .NE. STHEAD ) THEN
C
C        P is in the active list, but is not at the head. So,
C        the predecessor of P is not NIL.
C
         STPOOL(PREV)  =  STPOOL(P)
         STPOOL(P)     =  STHEAD
         STHEAD        =  P

      END IF

      CALL CHKOUT ( 'DAFCS' )
      RETURN
      END
