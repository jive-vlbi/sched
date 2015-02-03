C$Procedure DAFANA ( DAF, add new array )
 
      SUBROUTINE DAFANA ( HANDLE, SUM, NAME, DATA, N )
 
C$ Abstract
C
C     Add a new array to an existing DAF.
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
 
      INTEGER               HANDLE
      DOUBLE PRECISION      SUM      ( * )
      CHARACTER*(*)         NAME
      DOUBLE PRECISION      DATA     ( * )
      INTEGER               N
 
      INTEGER               TBSIZE
      PARAMETER           ( TBSIZE =   20  )
 
C$ Brief_I/O
C
C     Variable  I/O  Entry
C     --------  ---  --------------------------------------------------
C     HANDLE     I   DAFBNA, DAFCAD
C     SUM        I   DAFBNA
C     NAME       I   DAFBNA
C     DATA       I   DAFADA
C     N          I   DAFADA
C     TBSIZE     P   DAFANA
C
C$ Detailed_Input
C
C     HANDLE      is the handle of a DAF opened for write access
C                 by a previous call to DAFOPW or DAFOPN.
C
C     SUM         is the summary for the array being added.
C
C     NAME        is the name of the array being added.
C
C     DATA        contains all or part of the data in the array.
C
C     N           is the number of elements in DATA.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     TBSIZE      is the size of the file table maintained internally
C                 by DAFANA,  TBSIZE is the maximum number of DAFs
C                 that can be in use simultaneously by this routine.
C
C$ Files
C
C     See argument HANDLE, above.
C
C$ Exceptions
C
C     1) If DAFANA is called directly, the error SPICE(BOGUSENTRY)
C        is signalled.
C
C     2) See entry points DAFBNA, DAFADA, DAFENA, and DAFCAD
C        for exceptions specific to those entry points.
C
C$ Particulars
C
C     DAFANA serves as an umbrella, allowing data to be shared by its
C     entry points:
C
C        DAFBNA         Begin new array.
C        DAFADA         Add data to array.
C        DAFCAD         Continue adding data.
C        DAFENA         End new array.
C
C     The main function of these entry points is to simplify the
C     addition of new arrays to existing DAFs.
C
C     An application can add data to a single DAF, or to multiple DAFs,
C     simultaneously.  In the case of writing to a single DAF, the
C     creation of a new array requires four steps:
C
C        1) Open a DAF for write access, using either DAFOPW
C           (if the file already exists) or DAFOPN (if it does not).
C
C              CALL DAFOPW ( FNAME, HANDLE )
C
C        2) Begin the new DAF by calling DAFBNA,
C
C              CALL DAFBNA ( HANDLE, SUM, NAME )
C
C        3) Add data to the array by calling DAFADA as many times
C           as necessary,
C
C              CALL GET_DATA ( DATA, N, FOUND )
C
C              DO WHILE ( FOUND )
C                 CALL DAFADA   ( DATA, N        )
C                 CALL GET_DATA ( DATA, N, FOUND )
C              END DO
C
C        4) End the array by calling DAFENA,
C
C              CALL DAFENA
C
C     Note that the data can be added in chunks of any size, so long
C     as the chunks are ordered correctly.
C
C     In applications that add data to multiple DAFs simultaneously, it
C     is necessary to specify which DAF to add data to.  The DAFANA
C     entry points that allow specification of a DAF via a file handle
C     argument are DAFBNA (DAF, begin new array) and DAFCAD (DAF,
C     continue adding data).  As in the single-DAF case, arrays are
C     started by calls to DAFBNA, and data is added to arrays by calls
C     to DAFADA.  The last DAF designated by the input file handle
C     supplied to DAFBNA or DAFCAD is the `current DAF'.  If a
C     DAF contains an array started by a call to DAFBNA but not yet
C     completed by a call to DAFENA, we call this array the `current
C     array' for that DAF.  Each call to DAFADA will add data to the
C     current array in the current DAF.  A call to DAFENA will make the
C     current array in the current DAF a permanent addition to that DAF.
C
C     The notion of `current DAF' as discussed here applies only to
C     DAFs acted upon by entry points of DAFANA.  In DAFFA, there is a
C     DAF that is treated as the `current DAF' for searching; there is
C     no connection between the DAFs regarded as current by DAFANA and
C     DAFFA.
C
C     In the following example, we write data obtained from the routine
C     GET_DATA into two separate DAFs.  The first N/2 elements of the
C     array DATA will be written to the first DAF; the rest of the
C     array will be written to the second DAF.
C
C
C        1) Open the DAFs for write access, using either DAFOPW
C           (if the files already exist) or DAFOPN (if they do not).
C
C              CALL DAFOPW ( FNAME1, HANDL1 )
C              CALL DAFOPW ( FNAME2, HANDL2 )
C
C        2) Begin the new DAFs by calling DAFBNA,
C
C              CALL DAFBNA ( HANDL1, SUM1, NAME1 )
C              CALL DAFBNA ( HANDL2, SUM2, NAME2 )
C
C        3) Add data to the arrays by calling DAFCAD and DAFADA as many
C           times as necessary, selecting the file to add data to by
C           calling DAFCAD:
C
C              CALL GET_DATA ( DATA, N, FOUND )
C
C              DO WHILE ( FOUND )
C
C                 CALL DAFCAD   ( HANDL1                          )
C                 CALL DAFADA   ( DATA,               N/2         )
C
C                 CALL DAFCAD   ( HANDL2                          )
C                 CALL DAFADA   ( DATA( N/2 + 1 ),    N - N/2     )
C
C                 CALL GET_DATA ( DATA, N, FOUND )
C
C              END DO
C
C        4) End each array by calling DAFENA, selecting the file
C           in which to end the array by calling DAFCAD:
C
C              CALL DAFCAD ( HANDL1 )
C              CALL DAFENA
C
C              CALL DAFCAD ( HANDL2 )
C              CALL DAFENA
C
C
C$ Examples
C
C     1)  The following code fragment illustrates one possible way
C         to copy an array from one DAF (with handle ORIGIN) to another
C         (with handle COPY), SIZE words at a time.
C
C            CALL DAFGS  ( SUM  )
C            CALL DAFGN  ( NAME )
C            CALL DAFHSF ( ORIGIN, ND, NI )
C            CALL DAFUS  ( SUM,    ND, NI, DC, IC )
C
C            BEGIN = IC(NI-1)
C            END   = IC(NI  )
C
C            CALL DAFBNA ( COPY, SUM, NAME )
C
C            DO WHILE ( BEGIN .LE. END )
C               CHUNK = MIN ( BEGIN + SIZE - 1, END )
C
C               CALL DAFRDA ( ORIGIN, BEGIN, CHUNK, DATA )
C               CALL DAFADA ( DATA,   SIZE )
C
C               BEGIN = BEGIN + SIZE
C            END DO
C
C            CALL DAFENA
C
C
C     2)  A simple example demonstrating simultaneous addition
C         of data to multiple DAFs.  We read data from a text
C         file containing three columns of numbers, and we write
C         the data from each column out to a separate DAF.  The
C         format of the input text file is as follows:
C
C            +-                   -+
C            |  n11    n12    n13  |
C            |  n21    n22    n23  |
C            |   .      .      .   |
C            |   .      .      .   |
C            |   .      .      .   |
C            +-                   -+
C
C         Here the symbol nij indicates the jth number on the ith line
C         of the file.
C
C         The delimiters between the numbers in each column may be
C         commas or blanks.
C
C         The input file is called NUMBERS.TXT.  The output files are
C         called
C
C            COLUMN1.DAF
C            COLUMN2.DAF
C            COLUMN3.DAF
C
C         To confirm that the DAFs created by this program contain the
C         correct contents, we will read the data from each DAF and
C         combine it to create a new text file call RESULT.TXT.  This
C         file should contain the same data as NUMBERS.TXT.  If
C         RESULT.TXT is copied as NUMBERS.TXT and used as the input for
C         a second run of this program, the output file RESULT.TXT
C         from the second program run should match, up to round-off
C         error in the numbers, the input file NUMBERS.TXT containing
C         the output of the first program run.  If the numbers in
C         NUMBERS.TXT are integers, the match should be exact.
C
C
C                  PROGRAM WRTDAF
C            C
C            C     Read columns of d.p. numbers from a text file
C            C     and write the data from each column into a
C            C     separate DAF.  Read these DAFs and create a
C            C     second text file containing the same data as
C            C     the input text file.
C            C
C            C     Since we do not need to retain any descriptive
C            C     information about the DAFs inside of the files
C            C     themselves, we'll use a summary format having
C            C     two integer components (the minimum--these are
C            C     reserved for use by the DAF routines) and zero
C            C     double precision components.
C            C
C            C     The internal file names and array names will
C            C     simply indicate the data sources.
C            C
C
C            C
C            C     Local parameters
C            C
C                  INTEGER               FNMLEN
C                  PARAMETER           ( FNMLEN = 20 )
C
C                  INTEGER               LINLEN
C                  PARAMETER           ( LINLEN = 80 )
C
C                  INTEGER               MAXCOL
C                  PARAMETER           ( MAXCOL =  3 )
C
C                  INTEGER               ND
C                  PARAMETER           ( ND     =  0 )
C
C                  INTEGER               NDAF
C                  PARAMETER           ( NDAF   =  3 )
C
C                  INTEGER               NI
C                  PARAMETER           ( NI     =  2 )
C
C                  INTEGER               NUMLEN
C                  PARAMETER           ( NUMLEN = 30 )
C
C                  INTEGER               SIG
C                  PARAMETER           ( SIG    = 14 )
C
C            C
C            C     Local variables
C            C
C                  CHARACTER*(FNMLEN)    DAF    ( NDAF   )
C                  CHARACTER*(FNMLEN)    INFILE
C                  CHARACTER*(LINLEN)    LINE
C                  CHARACTER*(NUMLEN)    NUMCH  ( MAXCOL )
C                  CHARACTER*(LINLEN)    PRSERR
C                  CHARACTER*(FNMLEN)    RESULT
C
C                  DOUBLE PRECISION      DC     ( 1      )
C                  DOUBLE PRECISION      NUMBER ( MAXCOL )
C                  DOUBLE PRECISION      SUMMRY ( 1      )
C
C                  INTEGER               FA
C                  INTEGER               HAN    ( NDAF   )
C                  INTEGER               I
C                  INTEGER               IA
C                  INTEGER               IC     ( NI     )
C                  INTEGER               J
C                  INTEGER               LENGTH
C                  INTEGER               NCOLS
C                  INTEGER               PTR
C
C                  LOGICAL               EOF
C                  LOGICAL               FOUND
C
C            C
C            C     Initial values
C            C
C                  DATA   DAF     /  'COLUMN1.DAF',
C                 .                  'COLUMN2.DAF',
C                 .                  'COLUMN3.DAF'  /
C
C                  DATA   INFILE  /  'NUMBERS.TXT'  /
C                  DATA   RESULT  /  'RESULT.TXT'   /
C
C
C            C
C            C     Use SPICELIB call tracing.
C            C
C                  CALL CHKIN ( 'WRTDAF' )
C
C            C
C            C     Create the new DAFs, and start a new array in each
C            C     one.  Just use the file name for the internal file
C            C     name and array name, for each DAF.  No assignments
C            C     are required for the array summaries.
C            C
C                  DO I = 1, 3
C                     CALL DAFOPN ( DAF(I), ND, NI, DAF(I), 0, HAN(I) )
C                     CALL DAFBNA ( HAN(I), SUMMRY, DAF(I)            )
C                  END DO
C
C            C
C            C     Now read numbers from the text file, line by line,
C            C     and add the numbers from each column to the
C            C     corresponding DAF.
C            C
C                  CALL RDTEXT ( INFILE, LINE, EOF )
C
C                  DO WHILE ( .NOT. EOF )
C            C
C            C        Parse the numbers in the input line.  They
C            C        may be separated by commas or blanks (the second
C            C        argument of LPARSM is a list of allowed
C            C        delimiters).  Parse the strings found by LPARSM.
C            C
C            C        For brevity, we won't check the number of columns
C            C        found, or the parse error flag.
C            C
C                     CALL LPARSM ( LINE, ' ,', MAXCOL, NCOLS, NUMCH )
C
C                     DO I = 1, NCOLS
C                        CALL NPARSD ( NUMCH(I), NUMBER(I), PRSERR, PTR)
C                     END DO
C
C            C
C            C        Add the number from the ith column to the array
C            C        in the ith DAF.  We'll use DAFCAD to select
C            C        the correct DAF to add data to.
C            C
C                     DO I = 1, NDAF
C                        CALL DAFCAD ( HAN(I)       )
C                        CALL DAFADA ( NUMBER(I), 1 )
C                     END DO
C
C            C
C            C        Get the next line.
C            C
C                     CALL RDTEXT ( INFILE, LINE, EOF )
C
C                  END DO
C
C            C
C            C     Finish (`end') the arrays.  Again, we'll use DAFCAD
C            C     to select the DAFs in which the arrays are to be
C            C     finished.  After finishing each array, close the DAF
C            C     containing it.
C            C
C                  DO I = 1, NDAF
C                     CALL DAFCAD ( HAN(I) )
C                     CALL DAFENA
C                     CALL DAFCLS ( HAN(I) )
C                  END DO
C
C            C
C            C     Now for the verification step.  We'll try to
C            C     build a text file containing the same data as
C            C     the orginal input file.  The format of the numbers,
C            C     the delimiters separating the numbers, spacing, and
C            C     non-printing characters may differ.  However, if this
C            C     file is used as the input file, and if the numbers
C            C     used in the file are integers, WRTDAF will create an
C            C     exact copy of it.
C            C
C
C            C
C            C     Open the DAFs for reading.
C            C
C                  DO I = 1, NDAF
C                     CALL DAFOPR ( DAF(I), HAN(I) )
C                  END DO
C
C            C
C            C     Obtain the start and end addresses of the
C            C     data in each DAF.  To do this, we'll need to
C            C     obtain and unpack the array summaries.
C            C
C            C     If all went well, the addresses should be the
C            C     same for each DAF.  We'll assume that the initial
C            C     and final addresses in the first DAF are correct
C            C     for all three.
C            C
C                  CALL DAFBFS ( HAN(1) )
C                  CALL DAFFNA ( FOUND  )
C                  CALL DAFGS  ( SUMMRY )
C                  CALL DAFUS  ( SUMMRY, ND, NI, DC, IC )
C
C                  IA      =  IC( NI-1 )
C                  FA      =  IC( NI   )
C                  LENGTH  =  FA - IA + 1
C
C            C
C            C     Now read numbers from the DAFs and build up
C            C     lines of text.  Write these lines out to our
C            C     output text file.
C            C
C                  DO I = 0,  LENGTH - 1
C
C                     LINE = ' '
C
C                     DO J = 1, NDAF
C                        CALL DAFRDA ( HAN(J),    IA+I, IA+I, NUMBER(J))
C                        CALL DPSTR  ( NUMBER(J), SIG,        NUMCH(J) )
C                        CALL SUFFIX ( NUMCH(J),  5,          LINE     )
C                     END DO
C
C                     CALL WRLINE ( RESULT, LINE )
C
C                  END DO
C
C            C
C            C     Close the output text file and the DAFs.
C            C
C                  CALL CLLINE ( RESULT )
C
C                  DO I = 1, NDAF
C                     CALL DAFCLS( HAN(I) )
C                  END DO
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
C     NAIF Document 167.0, "Double Precision Array Files (DAF)
C     Specification and User's Guide"
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.0.0, 16-NOV-2001 (FST)
C
C        Updated the entry points of DAFANA to enable its
C        internal state table size, TBSIZE, to be smaller
C        than the file table maintained by DAFAH: FTSIZE.
C
C-    SPICELIB Version 2.1.0, 11-JUL-1995 (KRG)
C
C        Updated to remove potential compiler warnings from the
C        truncation of double precision numbers to integers.
C
C        Also changed was a numeric constant from 1.D0 to the
C        equivalent, but more aesthetically pleasing 1.0D0.
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
C
C        Updated to support simultaneous writes to multiple DAFs.
C        The $Examples section of this routine now illustrates
C        usage of the routine DAFCAD.
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
C     add new daf array
C
C-&
 
 
 
 
C$ Revisions
C
C-    SPICELIB Version 3.0.0, 16-NOV-2001 (FST)
C
C        This umbrella and its entry points were updated to
C        work properly with the changes in the DAF system as
C        a result of its utilization of the new handle manager.
C
C        Since DAFAH now tracks FTSIZE files as defined in
C        the include file 'zzddhman.inc', it was decided that
C        in the interest of releasing the toolkit this module
C        would undergo simple changes.  As such most previous
C        references to FTSIZE in this umbrella have been replaced
C        with TBSIZE where appropriate.  DAFBNA now signals an
C        error if there is not enough room to add a new DAF's
C        dossier to the state table.  Also, after attempting to
C        clean up all files listed in the state table that are
C        not currently open, DAFBNA attempts to locate the
C        first dossier with STADDG set to FALSE.  This is then
C        freed to make room for the new DAF.  If DAFBNA fails
C        to locate such a dossier in the state table, it
C        signals the error SPICE(STFULL).
C
C        The parameter FILEN was removed, as it is defined
C        on an environmental basis in the include file
C        'zzddhman.inc'.
C
C
C-    SPICELIB Version 2.1.0, 11-JUL-1995 (KRG)
C
C        Updated to remove potential compiler warnings from the
C        truncation of double precision numbers to integers. Two
C        assignments to NARRAY were updated, being changed from:
C
C           NARRAY = SUMREC(ARYCNT)
C
C        to
C
C           NARRAY = IDINT ( SUMREC(ARYCNT) )
C
C        Also changed was a numeric constant from 1.D0 to the
C        equivalent, but more aesthetically pleasing 1.0D0.
C
C-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
C
C        Updated to support simultaneous writes to multiple DAFs.
C
C        In previous versions of DAFANA, data could be added to only
C        one DAF array at a time.  In fact, DAFAH allowed only one
C        DAF to be open for writing at any time.  Therefore, there was
C        no question about which DAF was being operated on by either of
C        the DAFANA entry points that don't accept file handles as
C        input arguments:  DAFADA and DAFENA.  In the current version
C        of DAFANA, the entry points that don't accept file handles as
C        inputs operate on the `current DAF'.  The current DAF is the
C        last one in which a new array was started by DAFBNA, or in
C        which addition of data to an array was continued by the new
C        entry point DAFCAD.  DAFCAD was added to allow users to set
C        the current DAF, so that additions of data to arrays in
C        multiple DAFs can be interleaved.
C
C        Note that the notion of `current DAF' as discussed here applies
C        only to DAFs acted upon by entry points of DAFANA.  In DAFFA,
C        there is a DAF that is treated as the `current DAF' for
C        searching; there is no connection between the DAFs regarded
C        as current by DAFANA and DAFFA.
C
C        The two principal changes to DAFANA are the addition of the
C        new entry point DAFCAD, and the addition of a data structure
C        called the `state table'.  The state table is a collection of
C        parallel arrays that maintain information about the state
C        of each data addition that is currently in progress.  The
C        state table arrays are indexed by a singly linked list pool;
C        this mechanism allows addition and deletion of information
C        about data additions without requiring movement of data
C        already in the state table.
C
C        The linked list pool contains an `active' list and a `free'
C        list. Nodes in the active list are used to index elements of
C        the state table where information about additions in progress
C        is stored.  The head node of the active list is of particular
C        significance:  the state information pointed to by this node
C        is that of the current DAF.  Nodes in the free list index
C        elements of the state table that are available for use.
C
C        When an array is started in a DAF that is not already `known'
C        to DAFANA, information about the DAF is added to the state
C        table.  If there are no free elements in the state table,
C        the routine starting the array (DAFBNA) will perform garbage
C        collection:  the routine will test the handles of each file
C        about which information in stored in the state table to see
C        whether that file is still open.  Nodes containing information
C        about DAFs that are no longer open will be moved to the free
C        list.
C
C        Whenever a DAF becomes the current DAF, the linked list
C        that indexes the state table is adjusted so that the node
C        pointing to information about the current DAF is at the head
C        of the active list.  This way, a slight efficiency is gained
C        when repeated data additions are made to the same DAF, since
C        the linear search through the state table for information on
C        that DAF will be shortened.
C
C        Since the algorithms for maintenance of linked lists are well
C        known, they are not documented here.  However, see the
C        internals of the SPICELIB routine SPKBSR for a nice diagram
C        describing a similar data structure.
C
C        The state table contains two arrays that are quite large:
C        there are buffers that contain the name and array summary for
C        each array under construction.   A parallel situation exists
C        in DAFFA, where there are buffers that contain the last
C        character record and summary record read from each DAF.  The
C        total storage required for these arrays (in DAFANA and DAFFA
C        together) is 4000 * TBSIZE bytes.  For this reason, it may be
C        a good idea to reduce the value of TBSIZE in SPICELIB versions
C        for machines where memory is scarce.
C
C        On a completely different topic:  the local declarations in
C        DAFANA have been alphabetized and separated by type, except
C        for those relating to the state table.  Several hard-coded
C        constants have been replaced by parameters.
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
      INTEGER               ARYCNT
      PARAMETER           ( ARYCNT =    3 )
 
      INTEGER               BWDPTR
      PARAMETER           ( BWDPTR =    2 )
 
      INTEGER               CRLEN
      PARAMETER           ( CRLEN  = 1000 )
 
      INTEGER               DPRSIZ
      PARAMETER           ( DPRSIZ =  128 )
 
      INTEGER               FWDPTR
      PARAMETER           ( FWDPTR =    1 )
 
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN =   60 )
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL =   -5 )
 
      INTEGER               MAXNDC
      PARAMETER           ( MAXNDC =  124 )
 
      INTEGER               MAXNIC
      PARAMETER           ( MAXNIC =  250 )
 
      INTEGER               MAXSUM
      PARAMETER           ( MAXSUM =  125 )
 
      INTEGER               NIL
      PARAMETER           ( NIL    =   -1 )
 
 
C
C     Local variables
C
 
C
C     State variables.
C
C     These variables define the state of each DAF to which data
C     is currently being added.  For each DAF that we're writing to, we
C     maintain a copy of:
C
C        STFH           File handle.
C
C        STIFNM         Internal file name.
C
C        STADDG         (`State table: adding') Flag indicating
C                       whether addition of data to an array is in
C                       progress.
C
C        STFRST         Record number of initial summary record.
C
C        STLAST         Record number of final summary record.
C
C        STBEGN         Beginning address of new array.
C
C        STFREE         Address of next free word.
C
C        STLSUM         Local copy of the array summary for the current
C                       array.
C
C        STNAME         Local copy of the array name for the current
C                       array.
C
C
C     These variables are maintained in a table of parallel arrays;
C     the size of the table is TBSIZE.
C
C
      INTEGER               STFH   (         TBSIZE )
      CHARACTER*(IFNLEN)    STIFNM (         TBSIZE )
      LOGICAL               STADDG (         TBSIZE )
      INTEGER               STFRST (         TBSIZE )
      INTEGER               STLAST (         TBSIZE )
      INTEGER               STFREE (         TBSIZE )
      INTEGER               STBEGN (         TBSIZE )
      DOUBLE PRECISION      STLSUM ( MAXSUM, TBSIZE )
      CHARACTER*(CRLEN)     STNAME (         TBSIZE )
 
C
C     The table of state variables is indexed by a singly linked list
C     of pointers.  This mechanism avoids the work of moving
C     the state variable data about as information about DAFs is
C     added to or deleted from the table.
C
C     The structure containing the linked list pointers is called a
C     `pool.'  The pool contains a list of `active' nodes and a list
C     of free nodes.  The head nodes of the active and free lists are
C     maintained as the variables STHEAD (`state table head') and
C     STFPTR (`state table free pointer'), respectively.  Every node in
C     the pool is on exactly one of these lists.
C
      INTEGER               STPOOL ( TBSIZE )
      INTEGER               STHEAD
      INTEGER               STFPTR
 
C
C     The pool starts out with all of the nodes on the free list.
C     DAFBNA initializes the pool.  As new DAFs are written to,
C     DAFBNA adds information about them to the state table.  Every
C     time a DAF array is started by DAFBNA, or selected for
C     continuation by DAFCAD, the routine in question `moves' the
C     DAF's state information to the head of the active list, if the
C     state information is not already there.  This re-organization is
C     accomplished by deleting the node for the DAF from its current
C     position in the active list and inserting the node at the head of
C     the list.  Thus, the change is made merely by setting pointers,
C     not by moving chunks of data in the state table.
C
C     It may happen that there is no room left in the state table
C     to accommodate information about a new DAF.  In this case,
C     garbage collection must be performed:  DAFBNA frees all nodes in
C     the table that index DAFs that are not currently open.
C
C     Note that the routine DAFADA does not modify the state table; it
C     merely adds data to the DAF that is at the head of the active
C     list.
C
 
 
C
C     Other local variables
C
      CHARACTER*(CRLEN)     NAMREC
      CHARACTER*(FILEN)     DAFNAM
      CHARACTER*(IFNLEN)    IFNAME
 
      DOUBLE PRECISION      DC     ( MAXNDC )
      DOUBLE PRECISION      SUMREC ( DPRSIZ )
 
      INTEGER               BWARD
      INTEGER               CLOC
      INTEGER               DLOC
      INTEGER               FREE
      INTEGER               FWARD
      INTEGER               I
      INTEGER               IC     ( MAXNIC )
      INTEGER               NAMSIZ
      INTEGER               ND
      INTEGER               NI
      INTEGER               NARRAY
      INTEGER               NEXT
      INTEGER               NEXTP
      INTEGER               OPNSET ( LBCELL : FTSIZE )
      INTEGER               P
      INTEGER               PREV
      INTEGER               SUMSIZ
      INTEGER               WORD
 
      LOGICAL               FIRST
      LOGICAL               FOUND
 
C
C     Save everything between calls
C
      SAVE
 
C
C     Initial values
C
      DATA                  FIRST       / .TRUE.  /
      DATA                  STHEAD      /  NIL    /
      DATA                  STFPTR      /  NIL    /
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'DAFANA'            )
         CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
         CALL CHKOUT ( 'DAFANA'            )
      END IF
 
      RETURN
 
 
 
C$Procedure DAFBNA ( DAF, begin new array )
 
      ENTRY DAFBNA ( HANDLE, SUM, NAME )
 
C$ Abstract
C
C     Begin a new array in a DAF.
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
C     DOUBLE PRECISION      SUM     ( * )
C     CHARACTER*(*)         NAME
C
C$ Brief_I/O
C
C     Variable  I/O  Entry
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of DAF.
C     SUM        I   Summary of new array.
C     NAME       I   Name of new array.
C
C$ Detailed_Input
C
C     HANDLE      is the handle of a DAF opened for write access
C                 by a previous call to DAFOPW or DAFOPN.
C
C     SUM         is the summary of a new array to be added to the
C                 specified file. The addresses (the final two integer
C                 components) need not be filled in.
C
C     NAME        is the name of the new array.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C     See argument HANDLE, above.
C
C$ Exceptions
C
C     1) If the input handle is not that of a DAF that is open
C        for writing, the error is diagnosed by routines called by
C        this routine.  These files are implicitly of the native
C        binary file format.
C
C     2) If the input array name is too long to fit in the number
C        of characters allowed by the summary format of the DAF
C        designated by HANDLE, the excess characters are truncated.
C        No error is signalled.
C
C     3) If there is not enough room in the state table to add
C        the DAF associated with HANDLE, the error SPICE(STFULL)
C        is signaled.
C
C$ Particulars
C
C     Only one array can be added to a DAF at any one time, so
C     calling DAFBNA cancels any addition to the file specified
C     by HANDLE that may be in progress. No warning is issued.
C
C$ Examples
C
C     See DAFANA.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     NAIF Document 167.0, "Double Precision Array Files (DAF)
C     Specification and User's Guide"
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.0.0, 16-NOV-2001 (FST)
C
C        Updated DAFBNA to support changes made to the DAF
C        system that utilize the new handle manager.  See
C        the Revisions section of DAFANA for a detailed
C        discussion of the changes.
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
C
C        Modified to support simultaneous writes to multiple DAFs.
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
C     begin new daf array
C
C-&
 
 
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
C
C        Modified to support simultaneous writes to multiple DAFs.
C        DAFBNA now adds information about DAFs to the state table,
C        deletes information about closed DAFs from the state table,
C        and intializes the state pool.
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFBNA' )
      END IF
 
C
C     Check out the file handle before going any further.
C
      CALL DAFSIH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFBNA' )
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
 
         STPOOL(TBSIZE) = NIL
         STFPTR         = 1
         STHEAD         = NIL
         FIRST          = .FALSE.
 
      END IF
 
C
C     We know that the beginning of the array will be the first
C     free address in the file. We also need the summary format.
C     Get both items from the file record.
C
C     We won't use the information we're obtaining now until
C     after we've placed the state information for the current
C     DAF at the head of the active list, but we want to make sure
C     that we can actually read the file record first.  So, we
C     do the read now and avoid modifying the active list if the
C     read fails.
C
      CALL DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )
 
C
C     If we couldn't read the file record, bail out now.
C
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFBNA' )
         RETURN
      END IF
 
C
C     See whether we already have an entry for this DAF in the
C     state table.  Find the previous node if possible.
C
      P      =   STHEAD
      PREV   =   NIL
      FOUND  =  .FALSE.
 
      DO WHILE (  ( P .NE. NIL )  .AND.  ( .NOT. FOUND )  )
 
         IF ( STFH(P) .EQ. HANDLE ) THEN
            FOUND  =  .TRUE.
         ELSE
            PREV   =  P
            P      =  STPOOL( P )
         END IF
 
      END DO
 
C
C     At this point, either FOUND is false, or P points to a
C     state table entry describing the DAF indicated by HANDLE.
C     In the latter case, PREV is the predecessor of P.
C
C
      IF ( FOUND ) THEN
C
C        We already have a dossier on this DAF.  We already have
C        the information on the summary format, but we must re-set
C        the rest of our state information.
C
C        Rather than doing the update here, we do it outside of this
C        IF block.  That way, the update gets done in just one place.
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
C           P is in the active list, but is not at the head.  So,
C           the predecessor of P is not NIL.
C
            STPOOL(PREV)  =  STPOOL(P)
            STPOOL(P)     =  STHEAD
            STHEAD        =  P
 
         END IF
 
 
      ELSE
C
C        We don't yet have any information on this DAF.  Make a new
C        state table entry for the DAF.  We may need to make room for
C        the new information by freeing space allocated to DAFs that
C        are no longer open.
C
         IF ( STFPTR .EQ. NIL ) THEN
C
C           Oops, we're out of space.  Time for garbage collection.
C           Test each file handle to see whether it designates a DAF
C           that is still open.  DAFHOF will tell us which handles
C           point to open DAFs.
C
            CALL DAFHOF ( OPNSET )
 
            P     =  STHEAD
            PREV  =  NIL
C
C           For every DAF file represented in the state table, we'll
C           delete the corresponding state information if the DAF is
C           now closed.  We traverse the active list, examining each
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
C                    of the active list.  P has no predecessor in this
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
C           are no longer open.  Now see if we still need to make
C           room.  If so, locate the first dossier with STADDG(P)
C           set to FALSE.  We know then that this file is not
C           currently involved in an array addition.
C
            IF ( STFPTR .EQ. NIL ) THEN
 
               FOUND = .FALSE.
               P     = STHEAD
               PREV  = NIL
 
               DO WHILE ( ( P .NE. NIL ) .AND. ( .NOT. FOUND ) )
 
C
C                 If STADDG(P) is TRUE, then we must continue
C                 searching.
C
                  IF ( STADDG(P) ) THEN
 
                     PREV = P
                     P    = STPOOL(P)
 
                  ELSE
 
                     FOUND = .TRUE.
C
C                    No array is presently being added to the DAF
C                    associated with this dossier, so free the
C                    node pointing to the information about the DAF it
C                    designated:
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
C                       Re-assign STHEAD so that we don't lose the head
C                       of the active list.  P has no predecessor in
C                       this case, so there's no need to set the
C                       forward pointer of node PREV.
C
                        STHEAD = NEXTP
 
                     ELSE
C
C                       Since P is not the head node of the active list,
C                       PREV is not NIL, so we'll need to set the
C                       forward pointer of node PREV.
C
                        STPOOL( PREV ) = NEXTP
 
                     END IF
 
 
                     STPOOL( P )  =  STFPTR
                     STFPTR       =  P
                     P            =  NEXTP
 
                     END IF
 
               END DO
 
            END IF
 
C
C           Now, check to see if there is now room to add the dossier
C           for the new DAF to the state table.  If not signal an error.
C
            IF ( STFPTR .EQ. NIL ) THEN
 
               CALL SETMSG ( 'Attempt to initiate create a new '
     .         //            'array in DAF ''#'' has failed. '
     .         //            'DAFANA''s state table has room '
     .         //            'to manage writing to # new arrays '
     .         //            'simultaneously, but there is no room '
     .         //            'left in the table for this DAF.'       )
               CALL ERRHAN ( '#', HANDLE                             )
               CALL ERRINT ( '#', TBSIZE                             )
               CALL SIGERR ( 'SPICE(STFULL)'                         )
               CALL CHKOUT ( 'DAFBNA'                                )
               RETURN
 
            END IF
 
         END IF
 
C
C        If we reach here, then we have room in the state table for
C        the new DAF.  The first free node is indicated by SFTPTR.
C        Allocate this node and use it to index the state information
C        for the new DAF.
C
         P =  STFPTR
 
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
C     Set the state information for the current array.
C
      STFH  ( P )  =  HANDLE
      STIFNM( P )  =  IFNAME
      STADDG( P )  = .TRUE.
      STFRST( P )  =  FWARD
      STLAST( P )  =  BWARD
      STBEGN( P )  =  FREE
      STFREE( P )  =  FREE
 
C
C     Find out how big the array summary is supposed to be.
C
      CALL DAFHSF ( STFH(P), ND, NI )
 
      SUMSIZ  =  ND + (NI+1)/2
 
C
C     Set the local copies of the array's summary and name.
C
      CALL MOVED ( SUM, SUMSIZ, STLSUM(1,P) )
 
      STNAME( P )  =  NAME
 
 
      CALL CHKOUT ( 'DAFBNA' )
      RETURN
 
 
 
 
 
C$Procedure DAFADA ( DAF, add data to array )
 
      ENTRY DAFADA ( DATA, N )
 
C$ Abstract
C
C     Add one or more double precision words of data to the newest
C     array in the current DAF.
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
C     DOUBLE PRECISION      DATA     ( * )
C     INTEGER               N
C
C$ Brief_I/O
C
C     Variable  I/O  Entry
C     --------  ---  --------------------------------------------------
C     DATA       I   Elements of the new array.
C     N          I   Number of elements in DATA.
C
C$ Detailed_Input
C
C     DATA        is an arbitrary number of double precision words to
C                 be added to the data in the array being created.
C
C     N           is the number of double precision words in DATA.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If there are no DAFs to which data is currently being added,
C        the error SPICE(DAFNOWRITE) is signalled.
C
C     2) If a new array has not been started in the current DAF (by a
C        call to DAFBNA), the error SPICE(DAFNEWCONFLICT) is signalled.
C
C     3) If N is less than one, no data are added to the file.
C
C$ Particulars
C
C     DAFADA adds data to the last array begun by DAFBNA or selected
C     by DAFCAD.
C
C     Data can be added to a DAF in chunks of any size, so long
C     as the chunks are added in the proper order.
C
C$ Examples
C
C     See example for DAFADA in the header of DAFANA.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     NAIF Document 167.0, "Double Precision Array Files (DAF)
C     Specification and User's Guide"
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.0.0, 16-NOV-2001 (FST)
C
C        Updated entry points to support changes made to the DAF
C        system that utilize the new handle manager.  See
C        the Revisions section of DAFANA for a detailed
C        discussion of the changes.
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
C
C        Updated to work with new DAF routines that allow writing
C        to multiple DAFs simultaneously.  Functionality for
C        applications that write to one DAF at a time is unchanged.
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
C     add data to daf array
C
C-&
 
 
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
C
C        Updated to work with new DAF routines that allow writing
C        to multiple DAFs simultaneously.  Functionality for
C        applications that write to one DAF at a time is unchanged.
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFADA' )
      END IF
 
C
C     This routine operates on the DAF at the head of the active list.
C
      P = STHEAD
 
C
C     We must make sure that the requested addition can be performed.
C     We don't validate the file handle here because this is one place
C     where we are concerned about speed.  The low-level writer routine
C     DAFWDR will handle the check.
C
      IF ( P .EQ. NIL ) THEN
 
         CALL SETMSG ( 'No DAF is currently being written.' )
         CALL SIGERR ( 'SPICE(DAFNOWRITE)'                  )
         CALL CHKOUT ( 'DAFADA'                             )
         RETURN
 
C
C     An array cannot be extended unless begun first.
C
      ELSE IF ( .NOT. STADDG(P) ) THEN
C
C        Validate the current handle, then get the name of the DAF.
C
         CALL DAFSIH ( STFH(P), 'WRITE' )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DAFADA' )
            RETURN
         END IF
 
         CALL DAFHFN (  STFH(P), DAFNAM                                )
         CALL SETMSG ( 'An attempt was made to add data to an array ' //
     .                 'that has not yet been begun, in file #.'       )
         CALL ERRCH  ( '#', DAFNAM                                     )
         CALL SIGERR ( 'SPICE(DAFNEWCONFLICT)'                         )
         CALL CHKOUT ( 'DAFADA'                                        )
         RETURN
 
C
C     Start adding data at the first free address, then update that
C     address to get ready for the next addition.
C
      ELSE IF ( N .GE. 1 ) THEN
 
         CALL DAFWDA ( STFH(P), STFREE(P), STFREE(P)+N-1, DATA )
         STFREE(P) = STFREE(P) + N
 
      END IF
 
      CALL CHKOUT ( 'DAFADA' )
      RETURN
 
 
 
 
 
 
C$Procedure DAFENA ( DAF, end new array )
 
      ENTRY DAFENA
 
C$ Abstract
C
C     End the addition of data to the newest array in the current DAF.
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
C     None.
C
C$ Brief_I/O
C
C     None.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     None.
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
C     1) If there are no DAFs to which data is currently being added,
C        the error SPICE(DAFNOWRITE) is signalled, or the error will
C        be detected by routines called by this routine.
C
C     2) If a new array has not been started in the current DAF (by a
C        call to DAFBNA), the error SPICE(DAFNEWCONFLICT) is signalled.
C
C$ Particulars
C
C     DAFENA makes the current array a permanent addition to the
C     current DAF.
C
C     The pointers within the file are not changed until an array
C     is ended successfully.  If an error occurs or if the current
C     DAF is closed before DAFENA is called, the last array will
C     not be visible to the DAF reader routines.
C
C$ Examples
C
C     See DAFANA.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     NAIF Document 167.0, "Double Precision Array Files (DAF)
C     Specification and User's Guide"
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.0.0, 16-NOV-2001 (FST)
C
C        Updated entry points to support changes made to the DAF
C        system that utilize the new handle manager.  See
C        the Revisions section of DAFANA for a detailed
C        discussion of the changes.
C
C-    SPICELIB Version 2.1.0, 11-JUL-1995 (KRG)
C
C        Updated to remove potential compiler warnings from the
C        truncation of double precision numbers to integers.
C
C        Also changed was a numeric constant from 1.D0 to the
C        equivalent, but more aesthetically pleasing 1.0D0.
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
C
C        Updated to work with new DAF routines that allow writing
C        to multiple DAFs simultaneously.  Functionality for
C        applications that write to one DAF at a time is unchanged.
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
C     end new daf array
C
C-&
 
 
 
C$ Revisions
C
C-    SPICELIB Version 2.1.0, 11-JUL-1995 (KRG)
C
C        Updated to remove potential compiler warnings from the
C        truncation of double precision numbers to integers. Two
C        assignments to NARRAY were updated, being changed from:
C
C           NARRAY = SUMREC(ARYCNT)
C
C        to
C
C           NARRAY = IDINT ( SUMREC(ARYCNT) )
C
C        Also changed was a numeric constant from 1.D0 to the
C        equivalent, but more aesthetically pleasing 1.0D0.
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
C
C        Updated to work with new DAF routines that allow writing
C        to multiple DAFs simultaneously.  Functionality for
C        applications that write to one DAF at a time is unchanged.
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFENA' )
      END IF
 
C
C     This routine operates on the DAF at the head of the active list.
C
      P = STHEAD
 
      IF ( P .EQ. NIL ) THEN
 
         CALL SETMSG ( 'No DAF is currently being written.' )
         CALL SIGERR ( 'SPICE(DAFNOWRITE)'                  )
         CALL CHKOUT ( 'DAFENA'                             )
         RETURN
 
C
C     A new array cannot be ended unless begun first.
C
      ELSE IF ( .NOT. STADDG(P) ) THEN
C
C        Validate the current handle, then get the name of the DAF.
C
         CALL DAFSIH ( STFH(P), 'WRITE' )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DAFENA' )
            RETURN
         END IF
 
         CALL DAFHFN (  STFH(P), DAFNAM                                )
         CALL SETMSG ( 'An attempt was made to end an array that has '//
     .                 'not yet been begun, in file #.'                )
         CALL ERRCH  ( '#', DAFNAM                                     )
         CALL SIGERR ( 'SPICE(DAFNEWCONFLICT)'                         )
         CALL CHKOUT ( 'DAFENA'                                        )
         RETURN
 
      END IF
 
C
C     No more data. The array ends just before the next free
C     address. The summary should be complete except for the
C     initial and final addresses of the data, of which we
C     have been keeping track.
C
      CALL DAFHSF ( STFH(P), ND, NI )
 
      CALL DAFUS  ( STLSUM(1,P), ND, NI, DC, IC )
 
      IC( NI-1 )  =  STBEGN(P)
      IC( NI   )  =  STFREE(P) - 1
 
      CALL DAFPS ( ND, NI, DC, IC, STLSUM(1,P) )
 
C
C     The summary should be stored in the final summary record (the
C     one at the end of the file). Get that entire record, and the
C     corresponding name record.
C
      CALL DAFRDR ( STFH(P), STLAST(P), 1, DPRSIZ, SUMREC, FOUND )
      CALL DAFRCR ( STFH(P), STLAST(P)+1,          NAMREC        )
      NARRAY = IDINT ( SUMREC(ARYCNT) )
 
C
C     The number of arrays determines where the summary and name
C     are stored within the summary record. Adding this array increases
C     the number of arrays by one.
C
      SUMSIZ  =  ND      +         ( NI + 1 ) / 2
      DLOC    =  ARYCNT  +  1  +   NARRAY * SUMSIZ
 
      CALL MOVED ( STLSUM(1,P), SUMSIZ, SUMREC(DLOC) )
 
      NAMSIZ  =        8      * SUMSIZ
      CLOC    =  1  +  NARRAY * NAMSIZ
 
      NAMREC(CLOC : CLOC+NAMSIZ-1) = STNAME(P)
 
 
      SUMREC(ARYCNT) =  SUMREC(ARYCNT) + 1.0D0
      NARRAY         =  IDINT ( SUMREC(ARYCNT) )
 
C
C     Usually, adding an array does not fill the final summary
C     record, and it can simply be replaced.
C
      IF ( NARRAY .LT. MAXSUM / SUMSIZ ) THEN
         CALL DAFWDR ( STFH(P), STLAST(P),   SUMREC )
         CALL DAFWCR ( STFH(P), STLAST(P)+1, NAMREC )
 
C
C     When the record becomes full, a new one must be written.
C     However, this fact should be transparent to the user.
C
      ELSE
 
C
C        The new summary record will be stored in the next free record
C        in the file. This summary record should point to it.
C
C        To find out which record the next free address is in, we use
C        DAFARW (`address to record and word').
C
         CALL DAFARW ( STFREE(P)-1, NEXT, WORD )
         NEXT           = NEXT + 1
         SUMREC(FWDPTR) = DBLE ( NEXT )
 
         CALL DAFWDR ( STFH(P), STLAST(P),   SUMREC )
         CALL DAFWCR ( STFH(P), STLAST(P)+1, NAMREC )
 
C
C        The new summary record should point backwards to the one just
C        written, and should point forwards to nothing. Of course,
C        it contains no summaries, and no names.
C
         CALL CLEARD ( DPRSIZ, SUMREC )
         SUMREC(FWDPTR)  =  0.0D0
         SUMREC(BWDPTR)  =  DBLE ( STLAST(P) )
         SUMREC(ARYCNT)  =  0.0D0
         NAMREC          = ' '
 
         CALL DAFWDR ( STFH(P), NEXT,   SUMREC )
         CALL DAFWCR ( STFH(P), NEXT+1, NAMREC )
 
C
C        If a new summary record  was added, the first free address
C        lies just beyond the end of the matching character record.
C
C        We use DAFRWA (`record and word to address') to calculate
C        the next free address.
C
         STLAST(P) = NEXT
         CALL DAFRWA ( STLAST(P)+2, 1, STFREE(P) )
 
      END IF
 
C
C     The new value STFREE(P) must be rewritten in the file record each
C     time a new array is added. If a new record was added, the new
C     value of STLAST(P) will be rewritten as well.
C
      CALL DAFWFR ( STFH(P),
     .              ND,
     .              NI,
     .              STIFNM(P),
     .              STFRST(P),
     .              STLAST(P),
     .              STFREE(P) )
 
C
C     Ready for another array.
C
      STADDG(P) = .FALSE.
 
      CALL CHKOUT ( 'DAFENA' )
      RETURN
 
 
 
 
 
 
C$Procedure      DAFCAD ( DAF, continue adding data )
 
      ENTRY DAFCAD ( HANDLE )
 
C$ Abstract
C
C     Select a DAF that already has a new array in progress as the
C     one to continue adding data to.
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
C     HANDLE     I   Handle of DAF to continue adding data to.
C
C$ Detailed_Input
C
C     HANDLE         is the handle of a DAF that is open for write
C                    access and in which a new array has been
C                    started by a call to DAFBNA.
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
C     1) If the input handle is not that of a DAF that is open
C        for writing, the error will be diagnosed by routines called
C        by this routine.
C
C     2) If no array is currently being added to in the file indicated
C        by HANDLE, the error will be diagnosed by this routine or
C        routines called by this routine.  If DAFCAD can detect the
C        problem, the error SPICE(NOARRAYSTARTED) will be signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     DAFCAD supports simultaneous addition of data to arrays in
C     multiple DAFs.  In applications that use this capability,
C     DAFCAD should be called prior to each call to DAFADA or DAFENA
C     to specify which DAF is to be acted upon.
C
C     Here is a code fragment that adds a new array to each of N
C     existing DAFs, simultaneously.  The data to be added to each
C     is broken up into M chunks; one chunk is written to each DAF
C     at a time.  The data is contained in the array CHUNK, dimensioned
C
C         DOUBLE PRECISION      CHUNK ( MAXDAT, M, N )
C
C     The actual amount of data in the Jth chunk for the Ith file is
C     given by
C
C         AMOUNT (J,I)
C
C
C
C         DO I = 1, N
C            CALL DAFOPW ( HANDLE(I) )
C            CALL DAFBNA ( HANDLE(I) )
C         END DO
C
C         DO J = 1, M
C
C            DO I = 1, N
C               CALL DAFCAD  ( HANDLE(I)                  )
C               CALL DAFADA  ( CHUNK(1,J,I),  AMOUNT(J,I) )
C            END DO
C
C         END DO
C
C         DO I = 1, N
C            CALL DAFCAD  ( HANDLE(I) )
C            CALL DAFENA
C         END DO
C
C
C     Note that if we write all of the data for each array to just one
C     DAF at a time, we don't need to use DAFCAD:
C
C        DO I = 1, N
C
C           CALL DAFOPW ( HANDLE(I) )
C           CALL DAFBNA ( HANDLE(I) )
C
C           DO J = 1, M
C              CALL DAFADA ( CHUNK(1,J,I),  AMOUNT(J,I) )
C           END DO
C
C           CALL DAFENA
C
C        END DO
C
C
C$ Examples
C
C     See DAFANA.
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
C
C$ Version
C
C-    SPICELIB Version 3.0.0, 16-NOV-2001 (FST)
C
C        Updated entry points to support changes made to the DAF
C        system that utilize the new handle manager.  See
C        the Revisions section of DAFANA for a detailed
C        discussion of the changes.
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
C     continue adding data to a daf
C     select a daf to continue adding data to
C
C-&
 
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFCAD' )
      END IF
 
C
C     Check out the file handle before going any further.
C
      CALL DAFSIH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFCAD' )
         RETURN
      END IF
 
C
C     See whether we already have an entry for this DAF in the
C     state table.  Find the previous node if possible.
C
      P      =   STHEAD
      PREV   =   NIL
      FOUND  =  .FALSE.
 
      DO WHILE (  ( P .NE. NIL )  .AND.  ( .NOT. FOUND )  )
 
         IF ( STFH(P) .EQ. HANDLE ) THEN
            FOUND  =  .TRUE.
         ELSE
            PREV   =  P
            P      =  STPOOL( P )
         END IF
 
      END DO
 
C
C     Either FOUND is false, or P is the index in the state table of
C     the DAF specified by HANDLE, and PREV is the predecessor of P.
C
 
C
C     You can't continue writing to a DAF that you're not
C     already writing to.
C
      IF ( .NOT. FOUND ) THEN
 
         CALL DAFHFN (  HANDLE, DAFNAM                               )
         CALL SETMSG ( 'No write in progress to #. (Handle was #.) ' )
         CALL ERRCH  ( '#',  DAFNAM                                  )
         CALL ERRINT ( '#',  HANDLE                                  )
         CALL SIGERR ( 'SPICE(NOARRAYSTARTED)'                       )
         CALL CHKOUT ( 'DAFCAD'                                      )
         RETURN
 
      ELSE IF ( .NOT. STADDG(P) ) THEN
 
         CALL DAFHFN (  HANDLE, DAFNAM                               )
         CALL SETMSG ( 'No write in progress to #. (Handle was #.) ' )
         CALL ERRCH  ( '#',  DAFNAM                                  )
         CALL ERRINT ( '#',  HANDLE                                  )
         CALL SIGERR ( 'SPICE(NOARRAYSTARTED)'                       )
         CALL CHKOUT ( 'DAFCAD'                                      )
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
C
 
      IF ( P .NE. STHEAD ) THEN
C
C        P is in the active list, but is not at the head.  So,
C        the predecessor of P is not NIL.
C
         STPOOL(PREV)  =  STPOOL(P)
         STPOOL(P)     =  STHEAD
         STHEAD        =  P
 
      END IF
 
 
      CALL CHKOUT ( 'DAFCAD' )
      RETURN
      END
