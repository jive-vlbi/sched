C$Procedure POOL ( Maintain a pool of kernel variables )

      SUBROUTINE POOL ( KERNEL, UNIT,   NAME,   NAMES,  NNAMES,
     .                  AGENT,  N,      VALUES, FOUND,  UPDATE,
     .                  START,  ROOM,   CVALS,  IVALS,  TYPE,
     .                  UWVARS, UWPTRS, UWPOOL, UWAGNT, USRCTR )

C$ Abstract
C
C     Maintain a pool of variables read from SPICE ASCII kernel files.
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
C     KERNEL
C
C$ Keywords
C
C     CONSTANTS
C     FILES
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE              'zzctr.inc'

      INTEGER               MAXVAR
      PARAMETER           ( MAXVAR =  26003 )
 
      INTEGER               MAXVAL
      PARAMETER           ( MAXVAL = 400000 )
 
      INTEGER               MAXLIN
      PARAMETER           ( MAXLIN =  15000 )
 
      INTEGER               MAXCHR
      PARAMETER           ( MAXCHR =     80 )
  
      INTEGER               MAXLEN
      PARAMETER           ( MAXLEN =     32 )
 
      INTEGER               MAXAGT
      PARAMETER           ( MAXAGT =   1000 )
 
      INTEGER               MXNOTE
      PARAMETER           ( MXNOTE =  MAXVAR * 5 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )
 

      CHARACTER*(*)         KERNEL
      INTEGER               UNIT
      CHARACTER*(*)         NAME
      CHARACTER*(*)         NAMES     ( * )
      INTEGER               NNAMES
      CHARACTER*(*)         AGENT
      INTEGER               N
      DOUBLE PRECISION      VALUES    ( * )
      LOGICAL               FOUND
      LOGICAL               UPDATE
      INTEGER               START
      INTEGER               ROOM
      CHARACTER*(*)         CVALS     ( * )
      INTEGER               IVALS     ( * )
      CHARACTER*(*)         TYPE
      CHARACTER*(*)         UWVARS    ( LBCELL : * )
      INTEGER               UWPTRS    ( * )
      INTEGER               UWPOOL    ( 2, LBCELL : * )
      CHARACTER*(*)         UWAGNT    ( * )
      INTEGER               USRCTR    ( CTRSIZ )


C$ Brief_I/O
C
C     Variable  I/O  Entry
C     --------  ---  --------------------------------------------------
C     KERNEL     I   LDPOOL
C     UNIT       I   WRPOOL
C     NAME       I   RTPOOL, EXPOOL, GIPOOL, GDPOOL, GCPOOL, PCPOOL,
C                    PDPOOL, PIPOOL, DTPOOL, SZPOOL, DVPOOL, GNPOOL
C     NAMES      I   SWPOOL
C     NNAMES     I   SWPOOL
C     AGENT      I   CVPOOL, DWPOOL, SWPOOL
C     N         I/O  RTPOOL, GIPOOL, GCPOOL, GDPOOL, DTPOOL, PCPOOL,
C                    PDPOOL, PIPOOL, LMPOOL, SZPOOL, GNPOOL
C     VALUES    I/O  RTPOOL  GDPOOL, PDPOOL
C     FOUND      O   RTPOOL, EXPOOL, GIPOOL, GCPOOL, GDPOOL, DTPOOL,
C                    SZPOOL, GNPOOL
C     UPDATE     O   CVPOOL, ZZPCTRCK
C     START      I   GIPOOL, GDPOOL, GCPOOL, GNPOOL
C     ROOM       I   GIPOOL, GDPOOL, GCPOOL. GNPOOL
C     CVALS     I/O  GCPOOL, PCPOOL, LMPOOL, GNPOOL
C     IVALS     I/O  GIPOOL, PIPOOL
C     TYPE       O   DTPOOL
C     UWVARS     O   ZZVUPOOL
C     UWPTRS     O   ZZVUPOOL
C     UWPOOL     O   ZZVUPOOL
C     UWAGNT     O   ZZVUPOOL
C     USRCTR    I/O  ZZPCTRCK
C
C     MAXVAR     P   (All)
C     MAXLEN     P   (All)
C     MAXVAL     P   (All)
C     MAXAGT     P   (All)
C     MXNOTE     P   (All)
C     BEGDAT     P   WRPOOL
C     BEGTXT     P   WRPOOL
C     CTRSIZ     P   ZZPCTRCK
C
C$ Detailed_Input
C
C     See the ENTRY points for a discussion of their arguments.
C
C$ Detailed_Output
C
C     See the ENTRY points for a discussion of their arguments.
C
C$ Parameters
C
C     MAXVAR      is the maximum number of variables that the
C                 kernel pool may contain at any one time.
C                 MAXVAR should be a prime number.
C
C                 Here's a list of primes that should make
C                 it easy to upgrade MAXVAR when/if the need arises.
C
C                     103
C                     199
C                     307
C                     401
C                     503
C                     601
C                     701
C                     751
C                     811
C                     911
C                    1013
C                    1213
C                    1303
C                    1511
C                    1811
C                    1913
C                    2003 
C                    2203
C                    2503
C                    2803
C                    3203
C                    3607
C                    4001
C                    4507
C                    4801
C                    5003 Current Value
C                    6007
C                    6521
C                    7001
C                    7507
C                    8009
C                    8501
C                    9001
C                    9511
C                   10007
C                   10501
C                   11003
C                   11503
C
C
C     MAXLEN      is the maximum length of the variable names that
C                 can be stored in the kernel pool (also set in
C                 zzrvar.f).
C
C     MAXVAL      is the maximum number of distinct values that
C                 may belong to the variables in the kernel pool.
C                 Each variable must have at least one value, and
C                 may have any number, so long as the total number
C                 does not exceed MAXVAL. MAXVAL must be at least
C                 as large as MAXVAR.
C
C     MAXAGT      is the maximum number of agents that can be 
C                 associated with a given kernel variable.
C
C     MAXCHR      is the maximum number of characters that can be
C                 stored in a component of a string valued kernel
C                 variable.
C
C     MXNOTE      is the maximum sum of the sizes of the sets of
C                 agents in the range of the mapping that associates
C                 with each watched kernel variable a set of agents
C                 that "watch" that variable.
C
C     MAXLIN      is the maximum number of character strings that
C                 can be stored as data for kernel pool variables.
C
C     CTRSIZ      is the dimension of the counter array used by
C                 various SPICE subsystems to uniquely identify
C                 changes in their states. This parameter is 
C                 defined in the private include file 'zzctr.inc'.
C
C$ Exceptions
C
C     1) If POOL is called directly, the error SPICE(BOGUSENTRY) is
C        signaled.
C
C$ Files
C
C     See the ENTRY points for a discussion of their arguments.
C
C$ Particulars
C
C     POOL should never be called directly, but should instead be
C     accessed only through its entry points.
C
C     The purpose of this routine is to maintain a pool of variables
C     read from ASCII kernel files. The following entry points may be
C     used to access the pool.
C
C           CLPOOL         Clears the pool.
C
C           LDPOOL         Loads the variables from a kernel file into
C                          the pool.
C
C           RTPOOL         Returns the value of a variable from
C                          the pool. (Obsolete use GDPOOL)
C
C           EXPOOL         Confirms the existence of a numeric
C                          variable in the pool.
C
C           WRPOOL         Writes the contents of the pool to an
C                          ASCII kernel file.
C
C           SWPOOL         Sets up a "watcher" on a variable so that
C                          various "agents" can be notified when a
C                          variable has been updated.
C
C           CVPOOL         Indicates whether or not an agent's
C                          variable has been updated since the last
C                          time an agent checked with the pool.
C
C           GCPOOL         Returns the value of a string valued
C                          variable in the pool.
C
C           GDPOOL         Returns the d.p. value of a numeric valued
C                          variable in the pool.
C
C           GIPOOL         Returns the integer value of a numeric valued
C                          variable in the pool.
C
C           DTPOOL         Returns the attributes of a variable in the
C                          pool.
C
C           PCPOOL         Allows the insertion of a character variable
C                          directly into the kernel pool without
C                          supplying a text kernel.
C
C           PDPOOL         Allows the insertion of a double precision
C                          variable directly into the kernel pool
C                          without supplying a text kernel.
C
C           PIPOOL         Allows the insertion of an integer variable
C                          directly into the kernel pool without
C                          supplying a text kernel.
C
C           LMPOOL         Similar to LDPOOL, but the text kernel is
C                          stored in an array of strings instead of an
C                          external file.
C
C           SZPOOL         allows run time retrieval of kernel pool
C                          memory parameters.
C
C           DVPOOL         allows deletion of a specific variable from
C                          the kernel pool.  (CLPOOL deletes all
C                          variables from the kernel pool.)
C
C           GNPOOL         assists in determining which variables are
C                          defined in the kernel pool via variable name
C                          template matching.
C
C           DWPOOL         deletes a watch from the watcher system.
C
C     Nominally, the kernel pool contains up to MAXVAR separate
C     variables, up to MAXVAL numeric values, and up to MAXLIN string
C     values. The names of the individual variables may contain up to
C     MAXLEN characters.
C
C$ Examples
C
C     The following code fragment demonstrates how the data from
C     several kernel files can be loaded into a kernel pool. After the
C     pool is loaded, the values in the pool are written to a kernel
C     file.
C
C     C
C     C     Store in an array the names of the kernel files whose
C     C     values will be loaded into the kernel pool.
C     C
C           KERNEL (1) = 'AXES.KER'
C           KERNEL (2) = 'GM.KER'
C           KERNEL (3) = 'LEAP_SECONDS.KER'
C
C     C
C     C     Clear the kernel pool. (This is optional.)
C     C
C           CALL CLPOOL
C
C     C
C     C     Load the variables from the three kernel files into the
C     C     the kernel pool.
C     C
C           DO I = 1, 3
C             CALL LDPOOL ( KERNEL (I) )
C           END DO
C
C     C
C     C     We can examine the values associated with any d.p. variable
C     C     in the kernel pool using GDPOOL.
C     C
C           CALL GDPOOL ( VARIABLE, START, ROOM, NVALS, VALUES, FOUND )
C
C     C
C     C     Open the text file 'NEWKERNEL.KER'.
C     C
C           CALL TXTOPN ( NEWKERNEL.KER', UNIT )
C
C     C
C     C     Write the values in the kernel pool to the file.
C     C
C           CALL WRPOOL ( UNIT )
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
C     H.A. Neilan     (JPL)
C     B.V. Semenov    (JPL)
C     W.L. Taber      (JPL)
C     F.S. Turner     (JPL)
C     R.E. Thurman    (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 10.1.0, 14-JUL-2014 (NJB) (BVS)
C
C        Updated header of WRPOOL to improve accuracy of 
C        that routine's output description.
C
C        Updated header of CVPOOL to improve accuracy of 
C        the description of the output argument UPDATE.
C
C        Updated header of CLPOOL to improve the description
C        of behavior of the watcher subsystem.
C
C     Last update was 17-JAN-2014 (BVS) (NJB)
C
C        Increased key POOL parameters as follows:
C
C           MAXVAR    5003 ->  26003
C           MAXVAL  200000 -> 400000
C           MAXLIN    4000 ->  15000
C
C        Decreased MXNOTE factor F (MXNOTE=MAXVAR*F) from 10 to 5.
C
C        Updated the main umbrella argument list to include the POOL
C        state counter. Added the private entry point ZZPCTRCK allowing
C        other routines to check their saved POOL state counter against
C        the current POOL state counter to detect and act on the POOL
C        state change.
C
C        Updated Index_Entries sections of entry points PCPOOL, PDPOOL,
C        and PIPOOL.
C
C-    SPICELIB Version 10.0.0, 24-MAY-2010 (EDW) (NJB)
C
C        Added an error check on the length of the kernel pool variable
C        name argument in:
C
C           PCPOOL
C           PDPOOL
C           PIPOOL
C
C        to enforce the variable name length does not exceed MAXLEN.
C
C        Increased MAXVAL to 200000.
C
C-    SPICELIB Version 9.0.0, 19-MAR-2009 (NJB)
C
C        Added watch deletion entry point DWPOOL and private entry
C        point ZZVUPOOL. Re-implemented watcher system to improve
C        efficiency, particularly of watch deletion. Bug fix: corrected
C        watcher overflow detection logic in SWPOOL. Updated header
C        code examples to use TXTOPN instead of GETLUN and a Fortran
C        OPEN statement; also to use GDPOOL instead of RTPOOL, except in
C        the header of RTPOOL itself. 
C
C        Code examples in SWPOOL and CVPOOL were updated to handle
C        kernel pool fetch failures.
C
C        Existing entry points modified as part of this update were:
C
C           POOL
C           CLPOOL
C           CVPOOL
C           DTPOOL
C           DVPOOL
C           EXPOOL
C           GCPOOL
C           GDPOOL
C           GIPOOL
C           GNPOOL
C           LDPOOL
C           LMPOOL
C           PCPOOL
C           PDPOOL
C           PIPOOL
C           RTPOOL
C           SWPOOL
C           WRPOOL
C       
C        Code examples using RTPOOL were updated to use GDPOOL, except
C        in the header of RTPOOL itself. Code examples using GETLUN and
C        an in-line Fortran OPEN statement were updated to use TXTOPN.
C
C        Various typos in comments throughout this file were fixed.
C
C
C-    SPICELIB Version 8.3.0, 22-DEC-2004 (NJB)
C
C        Fixed bug in DVPOOL.  Made corrections to comments in
C        other entry points.  The updated routines are DTPOOL,
C        DVPOOL, EXPOOL, GCPOOL, GDPOOL, GIPOOL, RTPOOL.
C
C-    SPICELIB Version 8.2.0, 24-JAN-2003 (BVS)
C
C        Increased MAXVAL to 40000.
C
C-    SPICELIB Version 8.1.0, 13-MAR-2001 (FST) (NJB)
C
C        Increased kernel pool size and agent parameters. MAXVAR is now
C        5003, MAXVAL is 10000, MAXLIN is 4000, MXNOTE is 2000, and 
C        MAXAGT is 1000.
C
C        Modified Fortran output formats used in entry point WRPOOL to
C        remove list-directed formatting.  This change was made to
C        work around problems with the way f2c translates list-
C        directed I/O.
C 
C
C-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
C
C        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
C        direct insertion of data into the kernel pool without having
C        to read an external file.
C
C        Added the interface LMPOOL that allows SPICE
C        programs to load text kernels directly from memory
C        instead of requiring a text file.
C
C        Added the entry point SZPOOL to return kernel pool definition
C        parameters.
C
C        Added the entry point DVPOOL to allow the removal of a variable
C        from the kernel pool.
C
C        Added the entry point GNPOOL to allow users to determine
C        variables that are present in the kernel pool
C
C-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
C
C        The implementation of the kernel pool was completely redone
C        to improve performance in loading and fetching data.  In
C        addition the pool was upgraded so that variables may be
C        either string or numeric valued.
C
C        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
C        to the routine.
C
C        The entry point RTPOOL should now be regarded as obsolete
C        and is maintained solely for backward compatibility with
C        existing routines that make use of it.
C
C-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
C
C        The entry points SWPOOL and CVPOOL were added.
C
C-    SPICELIB Version 5.0.0, 22-AUG-1990 (NJB)
C
C        Increased value of parameter MAXVAL to 5000 to accommodate
C        storage of SCLK coefficients in the kernel pool.
C
C-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
C
C        All entry points except POOL and CLPOOL now initialize the
C        pool if it has not been done yet.
C
C-    SPICELIB Version 3.0.0, 23-OCT-1989 (HAN)
C
C        Added declaration of FAILED. FAILED is checked in the
C        DO-loops in LDPOOL and WRPOOL to prevent infinite looping.
C
C-    SPICELIB Version 2.0.0, 18-OCT-1989 (RET)
C
C       A FAILED test was inserted into the control of the DO-loop which
C       reads in each kernel variable in LDPOOL.
C
C-    SPICELIB Version 1.2.0, 9-MAR-1989 (HAN)
C
C        Parameters BEGDAT and BEGTXT have been moved into the
C        Declarations section.
C
C-    SPICELIB Version 1.1.0, 16-FEB-1989 (IMU) (NJB)
C
C        Parameters MAXVAR, MAXVAL, MAXLEN moved into Declarations.
C        (Actually, MAXLEN was implicitly 32 characters, and has only
C        now been made an explicit---and changeable---limit.)
C
C        Declaration of unused function FAILED removed.
C
C-    SPICELIB Version 1.0.0, 8-JAN-1989 (IMU)
C
C-&
 
C$ Index_Entries
C
C     MAINTAIN a pool of kernel variables
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 8.3.0, 22-DEC-2004 (NJB)
C
C        Fixed bug in DVPOOL.  Made corrections to comments in
C        other entry points.  The updated routines are DTPOOL,
C        DVPOOL, EXPOOL, GCPOOL, GDPOOL, GIPOOL, RTPOOL.
C
C-    SPICELIB Version 8.2.0, 24-JAN-2003 (BVS)
C
C        Increased MAXVAL to 40000.
C
C-    SPICELIB Version 8.1.0, 13-MAR-2001 (FST) (NJB)
C
C        Increased kernel pool size and agent parameters. MAXVAR is now
C        5003, MAXVAL is 10000, MAXLIN is 4000, MXNOTE is 2000, and 
C        MAXAGT is 1000.
C
C        Modified Fortran output formats used in entry point WRPOOL to
C        remove list-directed formatting.  This change was made to
C        work around problems with the way f2c translates list-
C        directed I/O.
C
C-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
C
C        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
C        direct insertion of data into the kernel pool without having
C        to read an external file.
C
C        Added the interface LMPOOL that allows SPICE
C        programs to load text kernels directly from memory
C        instead of requiring a text file.
C
C        Added the entry point SZPOOL to return kernel pool definition
C        parameters.
C
C-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
C
C        The implementation of the kernel pool was completely redone
C        to improve performance in loading and fetching data.  In
C        addition the pool was upgraded so that variables may be
C        either string or numeric valued.
C
C        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
C        to the routine.
C
C        The entry point RTPOOL should now be regarded as obsolete
C        and is maintained solely for backward compatibility with
C        existing routines that make use of it.
C
C        The basic data structure used to maintain the list of
C        variable names and values was replaced with a hash table
C        implementation.  Data and names are accessed by means
C        of a hash function and linked lists of pointers to existing
C        variable names and data values.
C
C-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
C
C        The entry points SWPOOL (set watch on a pool variable)
C        and CVPOOL (check variable for update) so that routines
C        that buffer data stored in the kernel pool can fetch
C        that data only when it is updated.
C
C        Also the control of initializations was modified to be
C        consistent with other SPICELIB practices.
C
C        Finally, the revision history was upgraded so that the
C        version number increases over time.  This wasn't true
C        before. In addition some early revision data that referred to
C        pre-SPICELIB modifications were removed. This editing of
C        the version numbers makes it unlikely that anyone can track
C        down which previous version of this routine they have by
C        looking at the version number.  The best way to determine
C        the routine you had previously is to compare the dates
C        stored in the Version line of the routine.
C
C-    SPICELIB Version 5.0.0, 22-AUG-1990 (NJB)
C
C        Increased value of parameter MAXVAL to 5000 to accommodate
C        storage of SCLK coefficients in the kernel pool.
C
C        Also, changed version number in previous `Revisions' entry
C        from SPICELIB Version 2.0.0 to SPICELIB Version 2.0.0.  The
C        last version entry in the `Version' section had been
C        Version 1.0.0, dated later than the entry for `version 2'
C        in the revisions section!
C
C-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
C
C        All entry points except POOL and CLPOOL now initialize the
C        pool if it has not been done yet.
C
C-    SPICELIB Version 3.0.0, 23-OCT-1989 (HAN)
C
C        Added declaration of FAILED. FAILED is checked in the
C        DO-loops in LDPOOL and WRPOOL to prevent infinite looping.
C
C-    SPICELIB Version 2.0.0, 18-OCT-1989 (RET)
C
C       A FAILED test was inserted into the control of the DO-loop which
C       reads in each kernel variable.
C
C       Previously, if the error action 'RETURN' had been set by a
C       calling program, and the call to RDKNEW by LDPOOL failed,
C       then execution would continue through LDPOOL, with SPICELIB
C       routines returning upon entry. This meant that the routine
C       RDKVAR never got a chance to set the EOF flag, which was the
C       only control of the DO-loop. An infinite loop resulted in such
C       cases.  The FAILED test resolves that situation.
C
C-    SPICELIB Version 1.2.0, 9-MAR-1989 (HAN)
C
C        Parameters BEGDAT and BEGTXT have been moved into the
C        Declarations section.
C
C-    SPICELIB Version 1.1.0, 16-FEB-1989 (IMU) (NJB)
C
C        Parameters MAXVAR, MAXVAL, MAXLEN moved into Declarations.
C        (Actually, MAXLEN was implicitly 32 characters, and has only
C        now been made an explicit---and changeable---limit.)
C
C        Declaration of unused function FAILED removed.
C
C-    SPICELIB Version 1.0.0, 8-JAN-1989 (IMU)
C
C-&
 
 
C
C     SPICELIB functions
C 
      INTEGER               BSRCHC
      INTEGER               CARDC
      INTEGER               INTMAX
      INTEGER               INTMIN
      INTEGER               LNKNFN
      INTEGER               LNKNXT
      INTEGER               LNKTL
      INTEGER               LSTLTC
      INTEGER               RTRIM
      INTEGER               SIZEC
      INTEGER               LASTNB

 
      LOGICAL               ELEMC
      LOGICAL               EQSTR
      LOGICAL               FAILED
      LOGICAL               MATCHI
      LOGICAL               RETURN

C
C     Private SPICELIB functions
C
      INTEGER               ZZHASH

C
C     Local Parameters
C 
      CHARACTER*(1)         QUOTE
      PARAMETER           ( QUOTE = '''' )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 132 )

C
C     The next two variables are for use in traversing linked lists.
C
      INTEGER               PREV
      PARAMETER           ( PREV = 2 )
 
      INTEGER               NEXT
      PARAMETER           ( NEXT = 1 )
 
 
 
C
C     Local variables
C
  
C
C     Because some environments (such as the SUN) are too stupid to
C     treat the backslash character correctly we have to go through
C     some gyrations to put it into a variable in a "portable" way.
C     This is the reason for the following block of declarations.
C     Admittedly this is bizarre, but it works.
C
      INTEGER               MRKLEN
      PARAMETER           ( MRKLEN = 10 )
 
 
 
      CHARACTER*(MRKLEN)    BEGDAT
      CHARACTER*(MRKLEN)    BEGTXT
 
 
C
C     The following is the hash table used for holding kernel pool
C     variables.  Here's the basic structure:
C
C     The function ZZHASH computes the address of the head of a linked
C     list that contains the collisions for the range of ZZHASH.
C
C     The head node of the collision lists is stored in NAMLST.
C
C     If NAMLST has a value zero then
C
C        there is no name corresponding to that value of the
C        hash function.
C
C     If NAMLST is non-zero then
C
C        it is the head node of the list of names that have been
C        stored so far.
C
C        The list of addresses of names is stored in NMPOOL.
C        The names that have been stored so far are in PNAMES.
C
C     The data associated with  PNAMES is pointed to by DATLST
C     and CHPOOL or DPPOOL.  If a name of interest is stored in
C     PNAMES(I) then the DATLST(I) points to the first data node
C     associated with the name.
C
C     If DATLST(I) is less than zero then
C
C        its opposite is the address of the first node of
C        character data associated with PNAMES(I).
C
C     If DATLST(I) is positive then
C
C        it points to the address of the first node of numeric
C        data associated with PNAMES(I).
C
C     If DATLST(I) is zero
C
C        there is no data associated with PNAMES(I).
C
C
C     The arrays DPPOOL and CHPOOL are linked list pools that
C     give the address lists of values associated with a name.
C
C     The actual data is stored in DPVALS and CHVALS.
C
C     Here's a picture of how this all works.
C
C
C                                             Linked list Pool
C                                             of HASH collisions
C                       NAMLST                  NMPOOL         PNAME
C                     +------------+          +---------+    +--------+
C                     |            |          |         |    |        |
C                     +------------+ if not 0 +---------+    +--------+
C  ZZHASH( NAME ) --->|  Head Node | ---.     |         |    |        |
C                     +------------+    |     +---------+    +--------+
C                                       |     |         |    |        |
C                                       |     +---------+    +--------+
C                                       `-->  |Head of  |    |Name    |
C                                             |collision|    |corresp.|
C                                             |list for | -. |to head |
C                                             | NAME    |  | |of list |
C                                             +---------+  | +--------+
C                                             |         |  | |        |
C                                             +---------+  | +--------+
C                                             |         |  | |        |
C                                             +---------+  | +--------+
C                                             |Next Node|<-' |NextName|
C                                             +---------+etc.+--------+
C                                                  .              .
C                                                  .              .
C                                                  .              .
C                                             +---------+    +--------+
C                                             |         |    |        |
C                                             +---------+    +--------+
C
C
C
C
C      Linked       Variable    Heads of
C      List Pool     Names      Data lists
C       NMPOOL       PNAME       DATLST
C     +--------+   +--------+   +---------+          Head of linked list
C     |        |   |        |   |         |     .--> in DPPOOL linked
C     +--------+   +--------+   +---------+    |     list pool
C     |        |   |        |   |         |    |
C     +--------+   +--------+   +---------+    | Positive Value
C     |        |<->|        |<->|         |---<
C     +--------+   +--------+   +---------+    |
C     |        |   |        |   |         |    | Negative Value
C     +--------+   +--------+   +---------+    |
C     |        |   |        |   |         |    `--> Opposite of head
C     +--------+   +--------+   +---------+          of linked list
C     |        |   |        |   |         |          in CHPOOL linked
C     +--------+   +--------+   +---------+          list pool.
C
C
C
C
C
C      Linked                Values
C      List Pool             of data
C       DPPOOL (CHPOOL)      DPVALS (CHVALS)
C     +------------+         +------------+
C     |            |         |            |
C     +------------+         +------------+
C     |            |         |            |
C     +------------+         +------------+
C     | HEAD       |--. <--> | head value |
C     +------------+  |      +------------+
C     |            |  |      |            |
C     +------------+  |      +------------+
C     |            |  |      |            |
C     +------------+  |      +------------+
C     | Node 2     |<-' <--> | 2nd value  |
C     +------------+ etc.    +------------+
C     |            |         |            |
C     +------------+         +------------+
C     |            |         |            |
C     +------------+         +------------+
C     |            |         |            |
C     +------------+         +------------+
C     |            |         |            |
C     +------------+         +------------+
C     |            |         |            |
C     +------------+         +------------+
C     |            |         |            |
C     +------------+         +------------+
C
C
      CHARACTER*(MAXLEN)    PNAMES   (           MAXVAR )
      INTEGER               NAMLST   (           MAXVAR )
      INTEGER               NMPOOL   ( 2, LBPOOL:MAXVAR )
      INTEGER               DATLST   (           MAXVAR )
 
      INTEGER               CHPOOL   ( 2, LBPOOL:MAXLIN )
      INTEGER               DPPOOL   ( 2, LBPOOL:MAXVAL )
      CHARACTER*(MAXCHR)    CHVALS   (           MAXLIN )
      DOUBLE PRECISION      DPVALS   (           MAXVAL )
 
C
C     The WT... variables make up the data structure that
C     maps variables to their associated agents (WTAGNT).
C     A diagram of the watcher data structure is shown below.
C
C      Watched     Heads of     Agent linked  Agent names
C      variables   agent lists  list pool
C       WTVARS       WTPTR        WTPOOL        WTAGNT
C     +--------+   +--------+   +---------+   +---------+
C     |        |   |        |   |         |   |         |
C     +--------+   +--------+   +---------+   +---------+
C     |        |   |        |   |         |   |         |
C     +--------+   +--------+   +---------+   +---------+
C     |        |<->|        |<->|         |<->|         |
C     +--------+   +--------+   +---------+   +---------+
C     |        |   |        |   |         |   |         |
C     +--------+   +--------+   +---------+   +---------+
C     |        |   |        |   |         |   |         |
C     +--------+   +--------+   +---------+   +---------+
C     |        |   |        |   |         |   |         |
C     +--------+   +--------+   +---------+   +---------+
C     
C
      CHARACTER*(MAXLEN)    WTVARS   (    LBCELL : MAXVAR )
      INTEGER               WTPOOL   ( 2, LBPOOL : MXNOTE )
      CHARACTER*(MAXLEN)    WTAGNT   ( MXNOTE )
      INTEGER               WTPTRS   ( MAXVAR )
 
C
C     Agents contains the list of agents that need to be notified
C     about updates to their variables.  NOTIFY and ACTIVE are both
C     temporary sets.
C
C     These variables are declared with the size MXNOTE because
C     they must be able to hold the largest possible number
C     of agents that could be associated with a kernel variable.
C
      CHARACTER*(MAXLEN)    ACTIVE   ( LBCELL:MXNOTE )
      CHARACTER*(MAXLEN)    AGENTS   ( LBCELL:MXNOTE )
      CHARACTER*(MAXLEN)    NOTIFY   ( LBCELL:MXNOTE )
 
C
C     First is our initialization flag.
C
      LOGICAL               FIRST
 
C
C     POOL state counter.
C
      INTEGER               SUBCTR ( CTRSIZ )
 
C
C     The remaining local variables...
C
      CHARACTER*(2)         FINISH
      CHARACTER*(LNSIZE)    CVALUE
      CHARACTER*(LNSIZE)    LINE
      CHARACTER*(MAXLEN)    VARNAM
 
      DOUBLE PRECISION      BIG
      DOUBLE PRECISION      DVALUE
      DOUBLE PRECISION      SMALL
 
      INTEGER               AGNODE
      INTEGER               AVAIL
      INTEGER               BEGIN
      INTEGER               CHNODE
      INTEGER               CODE
      INTEGER               DATAHD
      INTEGER               DNODE
      INTEGER               DPNODE
      INTEGER               FREE
      INTEGER               HEAD
      INTEGER               HITS
      INTEGER               I
      INTEGER               IOSTAT
      INTEGER               IQUOTE
      INTEGER               J
      INTEGER               K
      INTEGER               LINNUM
      INTEGER               LOOKAT
      INTEGER               MARGIN
      INTEGER               NAMEAT
      INTEGER               NEED
      INTEGER               NFETCH
      INTEGER               NNODE
      INTEGER               NNODES
      INTEGER               NODE
      INTEGER               NPTRS
      INTEGER               NVARS
      INTEGER               NW
      INTEGER               R
      INTEGER               SPACE
      INTEGER               TAIL
      INTEGER               TOFREE
      INTEGER               VARLEN

      LOGICAL               CHR
      LOGICAL               DP
      LOGICAL               EOF
      LOGICAL               GOTIT
      LOGICAL               ISQUOT
      LOGICAL               NOAGNT
      LOGICAL               SUCCES
      LOGICAL               VECTOR
 
C
C     Save EVERYTHING.
C
      SAVE
 
C
C     Initial values
C
      DATA                  FIRST / .TRUE. /
 
C
C     Set up the definition of our in-line functions.
C
      ISQUOT(CODE) = CODE .EQ. IQUOTE

 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'POOL' )
      END IF
 
C
C     This routine should never be called. If this routine is called,
C     an error is signaled.
C
      CALL SETMSG ( 'POOL: You have called an entry which performs '  //
     .              'performs no run-time function. This may '        //
     .              'indicate a bug. Please check the documentation ' //
     .              'for the subroutine POOL.' )
 
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
 
      CALL CHKOUT ( 'POOL' )
      RETURN
 
 
 
 
 
C$Procedure CLPOOL ( Clear the pool of kernel variables )
 
      ENTRY CLPOOL
 
C$ Abstract
C
C     Remove all kernel variables from the kernel pool. Watches
C     on kernel variables are retained.
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
C     KERNEL
C
C$ Keywords
C
C     CONSTANTS
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
C$ Exceptions
C
C     1) All known agents (those established through SWPOOL) will
C        be "notified" that their watched variables have been updated
C        whenever CLPOOL is called.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     CLPOOL clears the pool of kernel variables maintained by
C     the subroutine POOL. All the variables in the pool are deleted.
C     However, all watcher information is retained.
C
C     Each watched variable will be regarded as having been updated.
C     Any agent associated with that variable will have a notice
C     posted for it indicating that its watched variable has been
C     updated.
C
C     Application programs can delete watches by calling the entry
C     point DWPOOL. See the header of DWPOOL for details.
C
C$ Examples
C
C
C     The following code fragment demonstrates how the data from
C     several kernel files can be loaded into a kernel pool. After the
C     pool is loaded, the values in the pool are written to a kernel
C     file.
C
C
C     C
C     C     Store in an array the names of the kernel files whose
C     C     values will be loaded into the kernel pool.
C     C
C           KERNEL (1) = 'AXES.KER'
C           KERNEL (2) = 'GM.KER'
C           KERNEL (3) = 'LEAP_SECONDS.KER'
C
C     C
C     C     Clear the kernel pool. (This is optional.)
C     C
C           CALL CLPOOL
C
C     C
C     C     Load the variables from the three kernel files into the
C     C     the kernel pool.
C     C
C           DO I = 1, 3
C             CALL LDPOOL ( KERNEL (I) )
C           END DO
C
C     C
C     C     We can examine the values associated with any d.p. variable
C     C     in the kernel pool using GDPOOL.
C     C
C           CALL GDPOOL ( VARIABLE, START, ROOM, NVALS, VALUES, FOUND )
C
C     C
C     C     Open the text file 'NEWKERNEL.KER'.
C     C
C           CALL TXTOPN ( NEWKERNEL.KER', UNIT )
C
C     C
C     C     Write the values in the kernel pool to the file.
C     C
C           CALL WRPOOL ( UNIT )
C
C
C$ Restrictions
C
C     1) This routine should not be used to unload kernels that
C        have been loaded via FURNSH.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     I.M. Underwood  (JPL)
C     W.L. Taber      (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.2.0, 01-JUL-2014 (NJB) (BVS)
C
C        Description of behavior of watcher subsystem was expanded.
C
C     Last update was 30-JUL-2013 (BVS)
C
C        Updated to increment POOL state counter.
C
C-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
C
C        Watcher update code was re-written for compatibility
C        with new watcher system implementation. Updated Restrictions
C        header section. Updated code example to use TXTOPN.
C
C-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
C
C        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
C        direct insertion of data into the kernel pool without having
C        to read an external file.
C
C        Added the interface LMPOOL that allows SPICE
C        programs to load text kernels directly from memory
C        instead of requiring a text file.
C
C        Added the entry point SZPOOL to return kernel pool definition
C        parameters.
C
C        Added the entry point DVPOOL to allow the removal of a variable
C        from the kernel pool.
C
C        Added the entry point GNPOOL to allow users to determine
C        variables that are present in the kernel pool
C
C-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
C
C        The implementation of the kernel pool was completely redone
C        to improve performance in loading and fetching data.  In
C        addition the pool was upgraded so that variables may be
C        either string or numeric valued.
C
C        This entry point clears the string valued variables as well as
C        the numeric valued variables.
C
C-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
C
C        The entry points SWPOOL and CVPOOL were added.
C
C-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
C
C        All entry points except POOL and CLPOOL now initialize the
C        pool if it has not been done yet.
C
C-    SPICELIB Version 1.0.0, 8-JAN-1989 (IMU)
C
C-&
 
C$ Index_Entries
C
C     CLEAR the pool of kernel variables
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
C
C        Watcher update code was re-written for compatibility
C        with new watcher system implementation.
C
C        ZZNWPOOL is called to update the list of agents
C        to notify of watched variable updates.
C
C-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
C
C        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
C        direct insertion of data into the kernel pool without having
C        to read an external file.
C
C        Added the interface LMPOOL that allows SPICE
C        programs to load text kernels directly from memory
C        instead of requiring a text file.
C
C        Added the entry point SZPOOL to return kernel pool definition
C        parameters.
C
C-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
C
C        The implementation of the kernel pool was completely redone
C        to improve performance in loading and fetching data.  In
C        addition the pool was upgraded so that variables may be
C        either string or numeric valued.
C
C        This entry point clears the string valued variables as well as
C        the numeric valued variables.
C
C-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
C
C        The entry points SWPOOL (set watch on a pool variable)
C        and CVPOOL (check variable for update) so that routines
C        that buffer data stored in the kernel pool can fetch
C        that data only when it is updated.
C
C
C        Also the control of initializations was modified to be
C        consistent with other SPICELIB practices.
C
C        Finally, the revision history was upgraded so that the
C        version number increases over time.  This wasn't true
C        before. In addition some early revision data that referred to
C        pre-SPICELIB modifications were removed. This editing of
C        the version numbers makes it unlikely that anyone can track
C        down which previous version of this routine they have by
C        looking at the version number.  The best way to determine
C        the routine you had previously is to compare the dates
C        stored in the Version line of the routine.
C
C-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
C
C        All entry points except POOL and CLPOOL now initialize the
C        pool if it has not been done yet.
C
C-    SPICELIB Version 1.0.0, 8-JAN-1989 (IMU)
C
C-&
 
 
 
 
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CLPOOL' )
      END IF
 
 
C
C     Initialize the pool if necessary.
C
      IF ( FIRST ) THEN

         CALL ZZPINI (  FIRST,
     .                  MAXVAR, MAXVAL, MAXLIN,
     .                  BEGDAT, BEGTXT,
     .                  NMPOOL, DPPOOL, CHPOOL,
     .                  NAMLST, DATLST,
     .                  MAXAGT, MXNOTE, WTVARS, 
     .                  WTPTRS, WTPOOL, WTAGNT,
     .                  AGENTS, ACTIVE, NOTIFY, SUBCTR )

      END IF

C
C     Increment POOL state counter.
C
      CALL ZZCTRINC ( SUBCTR ) 

C
C     Wipe out all of the PNAMES data.
C
      DO I = 1, MAXVAR
         NAMLST(I) = 0
         DATLST(I) = 0
         PNAMES(I) = ' '
      END DO
C
C     Free up all of the space in all of the linked list pools, except
C     for the watcher pool.
C
      CALL LNKINI ( MAXVAR, NMPOOL )
      CALL LNKINI ( MAXVAL, DPPOOL )
      CALL LNKINI ( MAXLIN, CHPOOL )
  
      DO I = 1, CARDC ( WTVARS ) 
C
C        Union the update set AGENTS with the set of agents 
C        associated with the Ith watched variable.
C
         CALL ZZNWPOOL ( WTVARS(I), WTVARS, WTPTRS, WTPOOL, 
     .                   WTAGNT,    ACTIVE, NOTIFY, AGENTS )
      END DO
  
      CALL CHKOUT ( 'CLPOOL' )
      RETURN
 
 


 
C$Procedure LDPOOL ( Load variables from a kernel file into the pool )
 
      ENTRY LDPOOL ( KERNEL )
 
C$ Abstract
C
C     Load the variables contained in a NAIF ASCII kernel file into the
C     kernel pool.
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
C     KERNEL
C
C$ Keywords
C
C     CONSTANTS
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         KERNEL
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     KERNEL     I   Name of the kernel file.
C
C$ Detailed_Input
C
C     KERNEL     is the name of the kernel file whose variables will be
C                loaded into the pool.
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
C     1)  Any I/O errors that occur while opening or reading a text
C         kernel will be diagnosed by routines in the call tree of this
C         routine.
C
C     2)  Any text kernel parsing errors will be diagnosed by routines
C         in the call tree of this routine.
C
C     3)  Any kernel pool overflow errors will be diagnosed by routines
C         in the call tree of this routine.
C
C$ Files
C
C     The NAIF ASCII kernel file KERNEL is opened by RDKNEW.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     The following code fragment demonstrates how the data from
C     several kernel files can be loaded into a kernel pool. After the
C     pool is loaded, the values in the pool are written to a kernel
C     file.
C
C     C
C     C     Store in an array the names of the kernel files whose
C     C     values will be loaded into the kernel pool.
C     C
C           KERNEL (1) = 'AXES.KER'
C           KERNEL (2) = 'GM.KER'
C           KERNEL (3) = 'LEAP_SECONDS.KER'
C
C     C
C     C     Clear the kernel pool. (This is optional.)
C     C
C           CALL CLPOOL
C
C     C
C     C     Load the variables from the three kernel files into the
C     C     the kernel pool.
C     C
C           DO I = 1, 3
C             CALL LDPOOL ( KERNEL (I) )
C           END DO
C
C     C
C     C     We can examine the values associated with any d.p. variable
C     C     in the kernel pool using GDPOOL.
C     C
C           CALL GDPOOL ( VARIABLE, START, ROOM, NVALS, VALUES, FOUND )
C
C     C
C     C     Open the new text file 'NEWKERNEL.KER'.
C     C
C           CALL TXTOPN ( 'NEWKERNEL.KER', UNIT )
C
C     C
C     C     Write the values in the kernel pool to the file.
C     C
C           CALL WRPOOL ( UNIT )
C
C
C$ Restrictions
C
C     1) Normally SPICE applications should load kernels via the
C        FURNSH entry point of the KEEPER routine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     R.E. Thurman    (JPL)
C     I.M. Underwood  (JPL)
C     W.L. Taber      (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.2.0, 30-JUL-2013 (BVS)
C
C        Updated to increment POOL state counter.
C
C-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
C
C        Watcher update code was re-written for compatibility
C        with new watcher system implementation.
C
C        Filled out Exceptions section of header, which previously
C        contained only the word "None."
C
C        Updated code example to use TXTOPN.
C
C-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
C
C        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
C        direct insertion of data into the kernel pool without having
C        to read an external file.
C
C        Added the interface LMPOOL that allows SPICE
C        programs to load text kernels directly from memory
C        instead of requiring a text file.
C
C        Added the entry point SZPOOL to return kernel pool definition
C        parameters.
C
C        Added the entry point DVPOOL to allow the removal of a variable
C        from the kernel pool.
C
C        Added the entry point GNPOOL to allow users to determine
C        variables that are present in the kernel pool
C
C-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
C
C        The implementation of the kernel pool was completely redone
C        to improve performance in loading and fetching data.  In
C        addition the pool was upgraded so that variables may be
C        either string or numeric valued.
C
C        In addition much greater error checking is performed on
C        the input file to guarantee valid inputs.
C
C-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
C
C        The entry points SWPOOL and CVPOOL were added.
C
C-    SPICELIB Version 5.0.0, 22-AUG-1990 (NJB)
C
C        Increased value of parameter MAXVAL to 5000 to accommodate
C        storage of SCLK coefficients in the kernel pool.
C
C-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
C
C        All entry points except POOL and CLPOOL now initialize the
C        pool if it has not been done yet.
C
C-    SPICELIB Version 3.0.0, 23-OCT-1989 (HAN)
C
C        Added declaration of FAILED. FAILED is checked in the
C        DO-loops in LDPOOL and WRPOOL to prevent infinite looping.
C
C-    SPICELIB Version 2.0.0, 18-OCT-1989 (RET)
C
C       A FAILED test was inserted into the control of the DO-loop which
C       reads in each kernel variable in LDPOOL.
C
C-    SPICELIB Version 1.2.0, 9-MAR-1989 (HAN)
C
C        Parameters BEGDAT and BEGTXT have been moved into the
C        Declarations section.
C
C-    SPICELIB Version 1.1.0, 16-FEB-1989 (IMU) (NJB)
C
C        Parameters MAXVAR, MAXVAL, MAXLEN moved into Declarations.
C        (Actually, MAXLEN was implicitly 32 characters, and has only
C        now been made an explicit---and changeable---limit.)
C
C        Declaration of unused function FAILED removed.
C
C-    SPICELIB Version 1.0.0, 8-JAN-1989 (IMU)
C
C-&
 
C$ Index_Entries
C
C     LOAD variables from a text kernel file into the pool
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
C
C        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
C        direct insertion of data into the kernel pool without having
C        to read an external file.
C
C        Added the interface LMPOOL that allows SPICE
C        programs to load text kernels directly from memory
C        instead of requiring a text file.
C
C        Added the entry point SZPOOL to return kernel pool definition
C        parameters.
C
C-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
C
C        The implementation of the kernel pool was completely redone
C        to improve performance in loading and fetching data.  In
C        addition the pool was upgraded so that variables may be
C        either string or numeric valued.
C
C        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
C        to the routine.
C
C        The entry point RTPOOL should now be regarded as obsolete
C        and is maintained solely for backward compatibility with
C        existing routines that make use of it.
C
C        The basic data structure used to maintain the list of
C        variable names and values was replaced with a hash table
C        implementation.  Data and names are accessed by means
C        of a hash function and linked lists of pointers to existing
C        variable names and data values.
C
C        In addition much greater error checking is performed on
C        the input file to guarantee valid inputs.
C
C-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
C
C        The entry points SWPOOL (set watch on a pool variable)
C        and CVPOOL (check variable for update) so that routines
C        that buffer data stored in the kernel pool can fetch
C        that data only when it is updated.
C
C        In addition, the revision history was upgraded so that the
C        version number increases over time.  This wasn't true
C        before. In addition some early revision data that referred to
C        pre-SPICELIB modifications were removed. This editing of
C        the version numbers makes it unlikely that anyone can track
C        down which previous version of this routine they have by
C        looking at the version number.  The best way to determine
C        the routine you had previously is to compare the dates
C        stored in the Version line of the routine.
C
C-    SPICELIB Version 5.0.0, 22-AUG-1990 (NJB)
C
C        Increased value of parameter MAXVAL to 5000 to accommodate
C        storage of SCLK coefficients in the kernel pool.
C
C        Also, changed version number in previous `Revisions' entry
C        from SPICELIB Version 2.0.0 to SPICELIB Version 2.0.0.  The
C        last version entry in the `Version' section had been
C        Version 1.0.0, dated later than the entry for `version 2'
C        in the revisions section!
C
C-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
C
C        All entry points except POOL and CLPOOL now initialize the
C        pool if it has not been done yet.
C
C-    SPICELIB Version 3.0.0, 23-OCT-1989 (HAN)
C
C        Added declaration of FAILED. FAILED is checked in the
C        DO-loops in LDPOOL and WRPOOL to prevent infinite looping.
C
C-    SPICELIB Version 2.0.0, 18-OCT-1989 (RET)
C
C       A FAILED test was inserted into the control of the DO-loop which
C       reads in each kernel variable.
C
C       Previously, if the error action 'RETURN' had been set by a
C       calling program, and the call to RDKNEW by LDPOOL failed,
C       then execution would continue through LDPOOL, with SPICELIB
C       routines returning upon entry. This meant that the routine
C       RDKVAR never got a chance to set the EOF flag, which was the
C       only control of the DO-loop. An infinite loop resulted in such
C       cases.  The FAILED test resolves that situation.
C
C-    SPICELIB Version 1.2.0, 9-MAR-1989 (HAN)
C
C        Parameters BEGDAT and BEGTXT have been moved into the
C        Declarations section.
C
C-    SPICELIB Version 1.1.0, 16-FEB-1989 (IMU) (NJB)
C
C        Parameters MAXVAR, MAXVAL, MAXLEN moved into Declarations.
C        (Actually, MAXLEN was implicitly 32 characters, and has only
C        now been made an explicit---and changeable---limit.)
C
C        Declaration of unused function FAILED removed.
C
C-    SPICELIB Version 1.0.0, 8-JAN-1989 (IMU)
C
C-&
 
 
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LDPOOL' )
      END IF
 
C
C     Initialize the pool if necessary.
C
      IF ( FIRST ) THEN

         CALL ZZPINI (  FIRST,
     .                  MAXVAR, MAXVAL, MAXLIN,
     .                  BEGDAT, BEGTXT,
     .                  NMPOOL, DPPOOL, CHPOOL,
     .                  NAMLST, DATLST,
     .                  MAXAGT, MXNOTE, WTVARS, 
     .                  WTPTRS, WTPOOL, WTAGNT,
     .                  AGENTS, ACTIVE, NOTIFY, SUBCTR )

      END IF
 
C
C     Increment POOL state counter.
C
      CALL ZZCTRINC ( SUBCTR ) 
 
C
C     Open the kernel file and read the first variable.
C
      CALL RDKNEW ( KERNEL )
      CALL ZZRVAR ( NAMLST,
     .              NMPOOL, PNAMES, DATLST,
     .              DPPOOL, DPVALS,
     .              CHPOOL, CHVALS,
     .              VARNAM, EOF )
 
C
C     Read the variables in the file, one at a time.
C
      DO WHILE (  ( .NOT. EOF ) .AND. (.NOT. FAILED() )   )
  
         IF ( VARNAM .NE. ' ' ) THEN
C
C           See if this variable is being watched; if it is, add its
C           associated agents to the list of AGENTS to be notified of a
C           watched variable update.
C
            IF (  ELEMC( VARNAM, WTVARS )  ) THEN
C
C              Union the update set AGENTS with the set of agents 
C              associated with the variable NAME.
C
               CALL ZZNWPOOL ( VARNAM, WTVARS, WTPTRS, WTPOOL, 
     .                         WTAGNT, ACTIVE, NOTIFY, AGENTS )

            END IF
 
 
         END IF


         CALL ZZRVAR ( NAMLST,
     .                 NMPOOL, PNAMES, DATLST,
     .                 DPPOOL, DPVALS,
     .                 CHPOOL, CHVALS,
     .                 VARNAM, EOF )
 
      END DO
C
C     We need to make sure that the kernel file gets closed.  Normally
C     the calling tree of ZZRVAR take care of this, but if a parsing
C     or syntax error occurs there,  ZZRVAR just returns and the
C     closing of the kernel is never handled.  This takes care
C     of the problem.  If the file has been closed already, this
C     doesn't hurt anything.
C
      CALL CLTEXT (  KERNEL  )
      CALL CHKOUT ( 'LDPOOL' )
      RETURN
 
 
 
 
C$Procedure RTPOOL ( Return the value of a pooled kernel variable )
 
      ENTRY RTPOOL ( NAME, N, VALUES, FOUND )
 
C$ Abstract
C
C     Return the value of a kernel variable from the kernel pool.
C
C     This routine is maintained only for backward compatibility.
C     It should be regarded as obsolete.  Use one of the entry points
C     GDPOOL, GIPOOL or GCPOOL in its place.
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
C     KERNEL
C
C$ Keywords
C
C     CONSTANTS
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         NAME
C     INTEGER               N
C     DOUBLE PRECISION      VALUES   ( * )
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of the variable whose value is to be returned.
C     N          O   Number of values associated with NAME.
C     VALUES     O   Values associated with NAME.
C     FOUND      O   True if variable is in pool.
C
C$ Detailed_Input
C
C     NAME       is the name of the variable whose values are to be
C                returned. If the variable is not in the pool, FOUND
C                will be FALSE.
C
C$ Detailed_Output
C
C     N          is the number of values associated with NAME.
C                If NAME is not in the pool, no value is given to
C                N.
C
C     VALUES     is the array of values associated with NAME.
C                If NAME is not in the pool, no values are given to
C                the elements of VALUES.
C
C     FOUND      is TRUE if the variable is in the pool, FALSE if it
C                is not.
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
C     None.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C
C     The following code fragment demonstrates how the data from
C     several kernel files can be loaded into a kernel pool. After the
C     pool is loaded, the values in the pool are written to a kernel
C     file.
C
C
C     C
C     C     Store in an array the names of the kernel files whose
C     C     values will be loaded into the kernel pool.
C     C
C           KERNEL (1) = 'AXES.KER'
C           KERNEL (2) = 'GM.KER'
C           KERNEL (3) = 'LEAP_SECONDS.KER'
C
C     C
C     C     Clear the kernel pool. (This is optional.)
C     C
C           CALL CLPOOL
C
C     C
C     C     Load the variables from the three kernel files into the
C     C     the kernel pool.
C     C
C           DO I = 1, 3
C             CALL LDPOOL ( KERNEL (I) )
C           END DO
C
C     C
C     C     We can examine the values associated with any variable
C     C     in the kernel pool using RTPOOL.
C     C
C           CALL RTPOOL ( VARIABLE, NUMVAL, VALUES, FOUND )
C
C     C
C     C     Open the new text file 'NEWKERNEL.KER'.
C     C
C           CALL TXTOPN ( 'NEWKERNEL.KER', UNIT )
C
C     C
C     C     Write the values in the kernel pool to the file.
C     C
C           CALL WRPOOL ( UNIT )
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
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
C
C        ZZPINI call was updated for compatibility
C        with new watcher system implementation.
C
C        Updated code example to use TXTOPN.
C
C-    SPICELIB Version 8.0.1, 22-DEC-2004 (NJB)
C
C        Corrected an in-line comment relating to finding the
C        head node of the conflict resolution list for NAME.
C        
C-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
C
C        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
C        direct insertion of data into the kernel pool without having
C        to read an external file.
C
C        Added the interface LMPOOL that allows SPICE
C        programs to load text kernels directly from memory
C        instead of requiring a text file.
C
C        Added the entry point SZPOOL to return kernel pool definition
C        parameters.
C
C        Added the entry point DVPOOL to allow the removal of a variable
C        from the kernel pool.
C
C        Added the entry point GNPOOL to allow users to determine
C        variables that are present in the kernel pool
C
C-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
C
C        The implementation of the kernel pool was completely redone
C        to improve performance in loading and fetching data.  In
C        addition the pool was upgraded so that variables may be
C        either string or numeric valued.
C
C        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
C        to the routine.
C
C        The entry point RTPOOL should now be regarded as obsolete
C        and is maintained solely for backward compatibility with
C        existing routines that make use of it.
C
C-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
C
C        All entry points except POOL and CLPOOL now initialize the
C        pool if it has not been done yet.
C
C-    SPICELIB Version 1.0.0, 8-JAN-1989 (IMU)
C
C-&
 
C$ Index_Entries
C
C     RETURN the value of a pooled kernel variable
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
C
C        All entry points except POOL and CLPOOL now initialize the
C        pool if it has not been done yet.
C
C-    SPICELIB Version 1.0.0, 8-JAN-1989 (IMU)
C
C-&
 
 
 
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'RTPOOL' )
      END IF
 
 
C
C     Initialize the pool if necessary.
C
      IF ( FIRST ) THEN

         CALL ZZPINI (  FIRST,
     .                  MAXVAR, MAXVAL, MAXLIN,
     .                  BEGDAT, BEGTXT,
     .                  NMPOOL, DPPOOL, CHPOOL,
     .                  NAMLST, DATLST,
     .                  MAXAGT, MXNOTE, WTVARS, 
     .                  WTPTRS, WTPOOL, WTAGNT,
     .                  AGENTS, ACTIVE, NOTIFY, SUBCTR )

      END IF

C
C     Compute the hash value of this name.
C
      LOOKAT = ZZHASH ( NAME )

C
C     Now see if there is a non-empty conflict resolution list for the
C     input string NAME.  If so, NAMLST(LOOKAT) contains the head node
C     of the conflict resolution list; this node is a positive value.
C
      IF ( NAMLST(LOOKAT) .EQ. 0 ) THEN
 
         FOUND = .FALSE.
         CALL CHKOUT ( 'RTPOOL' )
         RETURN
 
      END IF
C
C     If were are still here NAMLST(LOOKAT) is the first node of
C     a conflict resolution list.  See if the NAME corresponding
C     to this node is the one we are looking for.
C
      NODE   = NAMLST( LOOKAT      )
      SUCCES = NAME .EQ. PNAMES(NODE)
 
      DO WHILE ( .NOT. SUCCES )
 
         NODE = NMPOOL ( NEXT, NODE )
 
         IF ( NODE .LT. 0 ) THEN
 
            FOUND = .FALSE.
            CALL CHKOUT ( 'RTPOOL' )
            RETURN
 
         END IF
 
         SUCCES = NAME .EQ. PNAMES(NODE)
 
      END DO
C
C     If you get to this point, the variable NAME is present in the
C     list of names at PNAMES(NODE), ABS( DATLST(NODE) ) points to the
C     head of a linked list of values for this NAME.
C
C     However, recall that RTPOOL can only return d.p. values.
C     DATLST(NODE) is the head of a d.p. list of values if it
C     is positive.  We use negative values to point to character
C     values.
C
      IF ( DATLST(NODE) .LE. 0 ) THEN
 
         FOUND = .FALSE.
 
      ELSE
 
         FOUND = .TRUE.
         N     =  0
         NODE  =  DATLST(NODE)
 
         DO WHILE ( NODE .GT. 0 )
            N         = N + 1
            VALUES(N) = DPVALS(     NODE)
            NODE      = DPPOOL(NEXT,NODE)
         END DO
 
      END IF
 
      CALL CHKOUT ( 'RTPOOL' )
      RETURN
 
 
 
 
C$Procedure EXPOOL ( Confirm the existence of a pooled kernel variable )
 
      ENTRY EXPOOL ( NAME, FOUND )
 
C$ Abstract
C
C     Confirm the existence of a kernel variable in the kernel pool.
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
C     KERNEL
C
C$ Keywords
C
C     CONSTANTS
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         NAME
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of the variable whose value is to be returned.
C     FOUND      O   True when the variable is in the pool.
C
C$ Detailed_Input
C
C     NAME       is the name of the variable whose values are to be
C                returned.
C
C$ Detailed_Output
C
C     FOUND      is true whenever the specified variable is included
C                in the pool.
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
C     None.
C
C$ Particulars
C
C     This routine determines whether or not a numeric kernel pool
C     variable exists.  It does not detect the existence of
C     string valued kernel pool variables.
C
C     A better routine for determining the existence of kernel pool
C     variables is the entry point DTPOOL which determines the
C     existence, size and type of kernel pool variables.
C
C$ Examples
C
C     See BODFND.
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
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
C
C        ZZPINI call was updated for compatibility
C        with new watcher system implementation.
C
C        Fixed typos.
C
C-    SPICELIB Version 8.0.1, 22-DEC-2004 (NJB)
C
C        Corrected an in-line comment relating to finding the
C        head node of the conflict resolution list for NAME.
C
C-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
C
C        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
C        direct insertion of data into the kernel pool without having
C        to read an external file.
C
C        Added the interface LMPOOL that allows SPICE
C        programs to load text kernels directly from memory
C        instead of requiring a text file.
C
C        Added the entry point SZPOOL to return kernel pool definition
C        parameters.
C
C        Added the entry point DVPOOL to allow the removal of a variable
C        from the kernel pool.
C
C        Added the entry point GNPOOL to allow users to determine
C        variables that are present in the kernel pool
C
C-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
C
C        The implementation of the kernel pool was completely redone
C        to improve performance in loading and fetching data.  In
C        addition the pool was upgraded so that variables may be
C        either string or numeric valued.
C
C        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
C        to the routine.
C
C-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
C
C        All entry points except POOL and CLPOOL now initialize the
C        pool if it has not been done yet.
C
C-    SPICELIB Version 1.0.0, 8-JAN-1989 (IMU)
C
C-&
 
C$ Index_Entries
C
C     CONFIRM the existence of a pooled kernel variable
C
C-&
 
C$ Revisions
C
C        The implementation of the kernel pool was completely redone
C        to improve performance in loading and fetching data.  In
C        addition the pool was upgraded so that variables may be
C        either string or numeric valued.
C
C        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
C        to the routine.
C
C        The entry point RTPOOL should now be regarded as obsolete
C        and is maintained solely for backward compatibility with
C        existing routines that make use of it.
C
C        The basic data structure used to maintain the list of
C        variable names and values was replaced with a hash table
C        implementation.  Data and names are accessed by means
C        of a hash function and linked lists of pointers to existing
C        variable names and data values.
C
C-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
C
C        All entry points except POOL and CLPOOL now initialize the
C        pool if it has not been done yet.
C
C-    SPICELIB Version 1.0.0, 8-JAN-1989 (IMU)
C
C-&
 

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EXPOOL' )
      END IF
 
 
C
C     Initialize the pool if necessary.
C
      IF ( FIRST ) THEN

         CALL ZZPINI (  FIRST,
     .                  MAXVAR, MAXVAL, MAXLIN,
     .                  BEGDAT, BEGTXT,
     .                  NMPOOL, DPPOOL, CHPOOL,
     .                  NAMLST, DATLST,
     .                  MAXAGT, MXNOTE, WTVARS, 
     .                  WTPTRS, WTPOOL, WTAGNT,
     .                  AGENTS, ACTIVE, NOTIFY, SUBCTR )

      END IF

C
C     Compute the hash value of this name.
C
      LOOKAT = ZZHASH ( NAME )

C
C     Now see if there is a non-empty conflict resolution list for the
C     input string NAME.  If so, NAMLST(LOOKAT) contains the head node
C     of the conflict resolution list; this node is a positive value.
C
      IF ( NAMLST(LOOKAT) .EQ. 0 ) THEN
 
         FOUND = .FALSE.
         CALL CHKOUT ( 'EXPOOL' )
         RETURN
 
      END IF
C
C     If were are still here NAMLST(LOOKAT) is the first node of
C     a conflict resolution list.  See if the NAME corresponding
C     to this node is the one we are looking for.
C
      NODE   = NAMLST ( LOOKAT      )
      SUCCES = NAME .EQ. PNAMES(NODE)
 
      DO WHILE ( .NOT. SUCCES )
 
         NODE = NMPOOL ( NEXT, NODE )
 
         IF ( NODE .LT. 0 ) THEN
 
            FOUND = .FALSE.
            CALL CHKOUT ( 'EXPOOL' )
            RETURN
 
         END IF
 
         SUCCES = NAME .EQ. PNAMES(NODE)
 
      END DO
C
C     If you get to this point, the variable NAME is present in the
C     list of names at PNAMES(NODE), ABS( DATLST(NODE) ) points to the
C     head of a linked list of values for this NAME.
C
C     However, recall that EXPOOL indicates the existence only of
C     d.p. values.
C
      FOUND = DATLST(NODE) .GT. 0
 
      CALL CHKOUT ( 'EXPOOL' )
      RETURN
 


 
 
C$Procedure WRPOOL ( Write the variables in pool to a specified unit )
 
      ENTRY WRPOOL ( UNIT )
 
C$ Abstract
C
C     Write to a specified unit a set of "keyword = value" assignments
C     for all currently defined kernel variables. The assignments 
C     constitute a text kernel from which the current state of the 
C     kernel pool can be restored.
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
C     KERNEL
C
C$ Keywords
C
C     CONSTANTS
C     FILES
C
C$ Declarations
C
C     INTEGER        UNIT
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     UNIT       I   Logical unit to which the variables in the pool
C                    will be written.
C
C$ Detailed_Input
C
C     UNIT       is the logical unit to which the variables in the pool
C                will be written.
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
C     This routine writes to the specified logical unit the kernel
C     variables present in the kernel pool. Each variable consists of a
C     name and a set of associated values. The variables are written in
C     the form of a series of "keyword = value" assignments. The
C     assignments are preceded by a SPICE text kernel "\begindata"
C     marker.
C     
C     The output of this routine, if written to a text file, is a SPICE
C     text kernel. The current contents of the kernel pool can be
C     restored by clearing the pool and then loading this text kernel.
C
C     If the values are to be written to an output kernel file, the
C     file should be opened with a logical unit determined by the
C     calling program.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C
C     The following code fragment demonstrates how the data from
C     several kernel files can be loaded into a kernel pool. After the
C     pool is loaded, the values in the pool are written to a kernel
C     file.
C
C
C     C
C     C     Store in an array the names of the kernel files whose
C     C     values will be loaded into the kernel pool.
C     C
C           KERNEL (1) = 'AXES.KER'
C           KERNEL (2) = 'GM.KER'
C           KERNEL (3) = 'LEAP_SECONDS.KER'
C
C     C
C     C     Clear the kernel pool. (This is optional.)
C     C
C           CALL CLPOOL
C
C     C
C     C     Load the variables from the three kernel files into the
C     C     the kernel pool.
C     C
C           DO I = 1, 3
C             CALL LDPOOL ( KERNEL (I) )
C           END DO
C
C     C
C     C     We can examine the values associated with any double
C     C     precision variable in the kernel pool using GDPOOL.
C     C
C           CALL GDPOOL ( VARIABLE, 1, NMAX, NUMVAL, VALUES, FOUND )
C
C     C
C     C     Open the new text file 'NEWKERNEL.KER'.
C     C
C           CALL TXTOPN ( 'NEWKERNEL.KER', UNIT )
C
C     C
C     C     Write the values in the kernel pool to the file.
C     C
C           CALL WRPOOL ( UNIT )
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
C     H.A. Neilan     (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.1.1, 30-JUN-2014 (NJB)
C
C        Updated header to more accurately describe the output
C        of this routine.
C
C-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
C
C        Updated code example to use TXTOPN.
C
C-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
C
C        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
C        direct insertion of data into the kernel pool without having
C        to read an external file.
C
C        Added the interface LMPOOL that allows SPICE
C        programs to load text kernels directly from memory
C        instead of requiring a text file.
C
C        Added the entry point SZPOOL to return kernel pool definition
C        parameters.
C
C        Added the entry point DVPOOL to allow the removal of a variable
C        from the kernel pool.
C
C        Added the entry point GNPOOL to allow users to determine
C        variables that are present in the kernel pool
C
C-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
C
C        The implementation of the kernel pool was completely redone
C        to improve performance in loading and fetching data.  In
C        addition the pool was upgraded so that variables may be
C        either string or numeric valued.  Both types are supported
C        by WRPOOL.
C
C-    SPICELIB Version 5.0.0, 22-AUG-1990 (NJB)
C
C        Increased value of parameter MAXVAL to 5000 to accommodate
C        storage of SCLK coefficients in the kernel pool.
C
C-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
C
C        All entry points except POOL and CLPOOL now initialize the
C        pool if it has not been done yet.
C
C-    SPICELIB Version 3.0.0, 23-OCT-1989 (HAN)
C
C        Added declaration of FAILED. FAILED is checked in the
C        DO-loops in LDPOOL and WRPOOL to prevent infinite looping.
C
C-    SPICELIB Version 1.2.0, 9-MAR-1989 (HAN)
C
C        Parameters BEGDAT and BEGTXT have been moved into the
C        Declarations section.
C
C-    SPICELIB Version 1.1.0, 16-FEB-1989 (IMU) (NJB)
C
C        Parameters MAXVAR, MAXVAL, MAXLEN moved into Declarations.
C        (Actually, MAXLEN was implicitly 32 characters, and has only
C        now been made an explicit---and changeable---limit.)
C
C        Declaration of unused function FAILED removed.
C
C-    SPICELIB Version 1.0.0, 8-JAN-1989 (IMU)
C
C-&
 
C$ Index_Entries
C
C     WRITE the values in pool to a specified unit
C
C-&
 
C$ Revisions
C
C        The implementation of the kernel pool was completely redone
C        to improve performance in loading and fetching data.  In
C        addition the pool was upgraded so that variables may be
C        either string or numeric valued.
C
C        The basic data structure used to maintain the list of
C        variable names and values was replaced with a hash table
C        implementation.  Data and names are accessed by means
C        of a hash function and linked lists of pointers to existing
C        variable names and data values.
C
C-    SPICELIB Version 5.0.0, 22-AUG-1990 (NJB)
C
C        Increased value of parameter MAXVAL to 5000 to accommodate
C        storage of SCLK coefficients in the kernel pool.
C
C        Also, changed version number in previous `Revisions' entry
C        from SPICELIB Version 2.0.0 to SPICELIB Version 2.0.0.  The
C        last version entry in the `Version' section had been
C        Version 1.0.0, dated later than the entry for `version 2'
C        in the revisions section!
C
C-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
C
C        All entry points except POOL and CLPOOL now initialize the
C        pool if it has not been done yet.
C
C-    SPICELIB Version 3.0.0, 23-OCT-1989 (HAN)
C
C        Added declaration of FAILED. FAILED is checked in the
C        DO-loops in LDPOOL and WRPOOL to prevent infinite looping.
C
C-    SPICELIB Version 2.0.0, 18-OCT-1989 (RET)
C
C       A FAILED test was inserted into the control of the DO-loop which
C       reads in each kernel variable.
C
C       Previously, if the error action 'RETURN' had been set by a
C       calling program, and the call to RDKNEW by LDPOOL failed,
C       then execution would continue through LDPOOL, with SPICELIB
C       routines returning upon entry. This meant that the routine
C       RDKVAR never got a chance to set the EOF flag, which was the
C       only control of the DO-loop. An infinite loop resulted in such
C       cases.  The FAILED test resolves that situation.
C
C-    SPICELIB Version 1.2.0, 9-MAR-1989 (HAN)
C
C        Parameters BEGDAT and BEGTXT have been moved into the
C        Declarations section.
C
C-    SPICELIB Version 1.1.0, 16-FEB-1989 (IMU) (NJB)
C
C        Parameters MAXVAR, MAXVAL, MAXLEN moved into Declarations.
C        (Actually, MAXLEN was implicitly 32 characters, and has only
C        now been made an explicit---and changeable---limit.)
C
C        Declaration of unused function FAILED removed.
C
C-    SPICELIB Version 1.0.0, 8-JAN-1989 (IMU)
C
C-&
 
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'WRPOOL' )
      END IF
 
C
C     Indicate the beginning of a data section.
C
      WRITE ( UNIT, '(1X,A)', IOSTAT=IOSTAT ) BEGDAT
      WRITE ( UNIT, '(1X,A)', IOSTAT=IOSTAT )
 
      IF ( IOSTAT .NE. 0 ) THEN
         CALL IOERR ( 'writing a variable to the output '
     .   //           'kernel file ',
     .                ' ',
     .                 IOSTAT             )
         CALL SIGERR ( 'SPICE(WRITEERROR)' )
         CALL CHKOUT ( 'WRPOOL'            )
         RETURN
      END IF
 
C
C     Next prepare for writing out the data.
C
      IQUOTE = ICHAR ( QUOTE    )
      MARGIN = MAXLEN + 6
 
      DO K = 1, MAXVAR
C
C        Get the head of this list.
C
         NNODE = NAMLST(K)
 
         DO WHILE ( NNODE .GT. 0 )
 
            LINE   = PNAMES( NNODE )
            DATAHD = DATLST( NNODE )
            DP     = DATAHD .GT. 0
            CHR    = DATAHD .LT. 0
            DNODE  = ABS(DATAHD)
C
C           Determine whether or not this is a vector object.
C
            IF ( DP ) THEN
               VECTOR = DPPOOL( NEXT, DNODE ) .GT. 0
            ELSE IF ( CHR ) THEN
               VECTOR = CHPOOL( NEXT, DNODE ) .GT. 0
            ELSE
               CALL SETMSG ( 'This error is never supposed to '
     .         //            'occur. No data was available for '
     .         //            'the variable ''#''. ' )
 
               R = RTRIM(PNAMES(NNODE))
               CALL ERRCH  ( '#', PNAMES(NNODE)(1:R) )
               CALL SIGERR ( 'SPICE(BUG)'           )
               CALL CHKOUT ( 'WRPOOL' )
               RETURN
            END IF
C
C           If still here, then we can set up the beginning of this
C           output line.
C
            LINE(MAXLEN+2:) = '= '
 
            IF ( VECTOR ) THEN
               LINE(MAXLEN+4:) = '( '
            END IF
C
C           Now fetch all of the data associated with this variable.
C           We'll write them out one per line.
C
            DO WHILE ( DNODE .GT. 0 )
C
C              Get the next data value and the address of the next node.
C
               IF ( DP ) THEN
                  DVALUE = DPVALS(DNODE)
                  DNODE  = DPPOOL(NEXT,DNODE)
               ELSE
                  CVALUE = QUOTE
                  J      = 1
C
C                 We have to double up each of the quotes on output.
C                 For this reason we copy the letters one at a time
C                 into the output holding area CVALUE.
C
                  DO I = 1, RTRIM(CHVALS(DNODE))
                     J           = J + 1
                     CVALUE(J:J) = CHVALS(DNODE)(I:I)
 
                     CODE = ICHAR(CHVALS(DNODE)(I:I))
 
                     IF ( ISQUOT(CODE) ) THEN
                        J           = J+1
                        CVALUE(J:J) = CHVALS(DNODE)(I:I)
                     END IF
                  END DO
 
                  J           = J + 1
                  CVALUE(J:J) = QUOTE
                  DNODE       = CHPOOL(NEXT,DNODE)
               END IF
 
C
C              We will need to properly finish off this write with
C              either a comma, a blank or a right parenthesis.
C
               IF ( DNODE .GT. 0 ) THEN
                  FINISH = ', '
               ELSE IF ( VECTOR ) THEN
                  FINISH = ' )'
               ELSE
                  FINISH = ' '
               END IF
C
C              Now write out our data.
C
               IF ( DP ) THEN
                  WRITE ( UNIT, '(1X,A,D25.17,A)', IOSTAT=IOSTAT )
     .            LINE(1:MARGIN), DVALUE, FINISH
               ELSE
                  WRITE ( UNIT, '(1X,3A)', IOSTAT=IOSTAT )
     .            LINE(1:MARGIN), CVALUE(1:J), FINISH
               END IF

C
C              Check the IOSTAT code.  After all, that's why it's there.
C
               IF ( IOSTAT .NE. 0 ) THEN
                  CALL IOERR ( 'writing a variable to the output '
     .            //           'kernel file ',
     .                          ' ',
     .                          IOSTAT              )
                  CALL SIGERR ( 'SPICE(WRITEERROR)' )
                  CALL CHKOUT ( 'WRPOOL'            )
                  RETURN
               END IF
 
C
C              Blank out the output line so that we'll have
C              leading blanks for subsequent components of the
C              vector (if we are in fact writing one).
C
               LINE = ' '
 
            END DO
C
C           Get the next name for this node:
C
            NNODE = NMPOOL(NEXT,NNODE)
 
         END DO
C
C        Get the next node (if there is one).
C
      END DO
 
 
C
C     Indicate the beginning of a text section. Data sections and
C     text sections must alternate, even if the text section is blank.
C
      WRITE ( UNIT, '(1X,A)', IOSTAT=IOSTAT )
      WRITE ( UNIT, '(1X,A)', IOSTAT=IOSTAT ) BEGTXT
 
      IF ( IOSTAT .NE. 0 ) THEN
         CALL IOERR ( 'writing a variable to the output '
     .   //           'kernel file ',
     .                  ' ',
     .                  IOSTAT              )
         CALL SIGERR ( 'SPICE(WRITEERROR)' )
         CALL CHKOUT ( 'WRPOOL'            )
         RETURN
      END IF
 
      CALL CHKOUT ( 'WRPOOL' )
      RETURN
 
 


C$Procedure SWPOOL ( Set watch on a pool variable )
 
      ENTRY SWPOOL ( AGENT, NNAMES, NAMES )
 
C$ Abstract
C
C     Add a name to the list of agents to notify whenever a member of
C     a list of kernel variables is updated.
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
C     KERNEL
C
C$ Keywords
C
C     CONSTANTS
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         AGENT
C     INTEGER               NNAMES
C     CHARACTER*(*)         NAMES  ( * )
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     AGENT      I   The name of an agent to be notified after updates.
C     NNAMES     I   The number of variables to associate with AGENT.
C     NAMES      I   Variable names whose update causes the notice.
C
C$ Detailed_Input
C
C     AGENT       is the name of a routine or entry point (agency) that
C                 will want to know when the kernel pool variables
C                 designated by NAMES have been updated. 
C
C     NNAMES      is the number of kernel pool variable names that will
C                 be associated with AGENT.
C
C     NAMES       is an array of names of variables in the kernel pool.
C                 Whenever any of these is updated, a notice will be
C                 posted for AGENT so that one can quickly check
C                 whether needed data has been modified.
C
C                 Any kernel variable may be associated with multiple
C                 agents; this call adds AGENT to each set of agents
C                 associated with a member of NAMES.
C
C                 The variables designated by NAMES need not exist in
C                 the kernel pool at the time a watch is set.
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
C     1) If sufficient room is not available to hold a new kernel
C        variable name, the error SPICE(KERVARSETOVERFLOW) will be
C        signaled.
C
C     2) If sufficient room is not available to hold a new agent
C        name, the error SPICE(TOOMANYWATCHES) will be signaled.
C
C     3) If any kernel variable in the array NAMES is already watched
C        by MAXAGT agents, and AGENT is not already associated with
C        that kernel variable, the error (AGENTLISTOVERFLOW) will be
C        signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The kernel pool is a convenient place to store a wide
C     variety of data needed by routines in SPICELIB and routines
C     that interface with SPICELIB routines.  However, when
C     a single name has a large quantity of data associated with
C     it, it becomes inefficient to constantly query the kernel
C     pool for values that are not updated on a frequent basis.
C
C     This entry point allows a routine to instruct the kernel pool
C     to post a message whenever a particular value gets updated.
C     In this way, a routine can quickly determine whether or not
C     data it requires has been updated since the last time the
C     data was accessed.  This makes it reasonable to buffer
C     the data in local storage and update it only when
C     a variable in the kernel pool that affects this data has
C     been updated.
C
C     Note that SWPOOL has a side effect.  Whenever a call to
C     SWPOOL is made, the agent specified in the calling sequence
C     is added to the list of agents that should be notified that
C     an update of its variables has occurred.  In other words
C     the code
C
C         CALL SWPOOL ( AGENT, NNAMES, NAMES  )
C         CALL CVPOOL ( AGENT,         UPDATE )
C
C     will always return UPDATE as .TRUE.
C
C     This feature allows for a slightly cleaner use of SWPOOL and
C     CVPOOL as shown in the example below.  Because SWPOOL
C     automatically loads AGENT into the list of agents to notify of
C     a kernel pool update, you do not have to include the code for
C     fetching the initial values of the kernel variables in the
C     initialization portion of a subroutine.  Instead, the code for
C     the first fetch from the pool is the same as the code for
C     fetching when the pool is updated.
C
C$ Examples
C
C     Suppose that you have an application subroutine, MYTASK, that
C     needs to access a large data set in the kernel pool.  If this
C     data could be kept in local storage and kernel pool queries
C     performed only when the data in the kernel pool has been
C     updated, the routine can perform much more efficiently.
C
C     The code fragment below illustrates how you might make use of this
C     feature.
C
C     C
C     C     On the first call to this routine establish those variables
C     C     that we will want to read from the kernel pool only when
C     C     new values have been established.
C     C
C           IF ( FIRST ) THEN
C
C              FIRST = .FALSE.
C              HAVE  = .FALSE.
C
C              CALL SWPOOL ( 'MYTASK', NNAMES, NAMES )
C
C           END IF
C
C     C
C     C     If any of the variables has been updated, fetch
C     C     it from the kernel pool. (Note that this also
C     C     handles getting variables for the first time.)
C     C     We use HAVE to indicate the fetch succeeded. If it
C     C     didn't, we need to attempt the fetch on the next 
C     C     pass into this routine.
C     C
C           CALL CVPOOL ( 'MYTASK', UPDATE )
C
C           IF (  UPDATE  .OR (.NOT. HAVE ) ) THEN
C
C              CALL GDPOOL ( 'MYTASK_VAR_1', 1, M, N1, VALS1, FOUND(1) )
C              CALL GDPOOL ( 'MYTASK_VAR_2', 1, M, N2, VALS2, FOUND(2) )
C                      .
C                      .
C                      .
C              CALL GDPOOL ( 'MYTASK_VAR_N', 1, M, NN, VALSN, FOUND(N) )
C
C           END IF
C
C           IF ( FAILED() ) THEN
C                 .
C                 .
C              do something about the failure
C                 .
C                 .
C
C           ELSE
C
C              HAVE = .TRUE.
C
C           END IF
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
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.2.0, 30-JUL-2013 (BVS)
C
C        Updated to increment POOL state counter.
C
C-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
C
C        This routine was re-written to work with the new
C        watcher system implementation. Several bugs related
C        to watch system overflow were fixed.
C
C        The code example was updated to handle kernel pool
C        fetch failure.
C
C-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
C
C        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
C        direct insertion of data into the kernel pool without having
C        to read an external file.
C
C        Added the interface LMPOOL that allows SPICE
C        programs to load text kernels directly from memory
C        instead of requiring a text file.
C
C        Added the entry point SZPOOL to return kernel pool definition
C        parameters.
C
C        Added the entry point DVPOOL to allow the removal of a variable
C        from the kernel pool.
C
C        Added the entry point GNPOOL to allow users to determine
C        variables that are present in the kernel pool
C
C-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
C
C        The implementation of the kernel pool was completely redone
C        to improve performance in loading and fetching data.  In
C        addition the pool was upgraded so that variables may be
C        either string or numeric valued.
C
C-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
C
C        The entry points SWPOOL and CVPOOL were added.
C
C-&
 
C$ Index_Entries
C
C     Watch for an update to a kernel pool variable
C     Notify a routine of an update to a kernel pool variable
C-&
 
C$ Revisions
C
C-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
C
C        This routine was re-written to work with the new
C        watcher system implementation. 
C
C        Several bugs related to watch system overflow were fixed.
C        Now overflow error checks are performed *before* the
C        watcher system is updated, so a partial update won't
C        occur if there's not enough room for a full update.
C
C-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
C
C        The implementation of the kernel pool was completely redone
C        to improve performance in loading and fetching data.  In
C        addition the pool was upgraded so that variables may be
C        either string or numeric valued.
C
C        The basic data structure used to maintain the list of
C        variable names and values was replaced with a hash table
C        implementation.  Data and names are accessed by means
C        of a hash function and linked lists of pointers to existing
C        variable names and data values.
C
C-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
C
C        The entry points SWPOOL (set watch on a pool variable)
C        and CVPOOL (check variable for update) so that routines
C        that buffer data stored in the kernel pool can fetch
C        that data only when it is updated.
C
C        In addition, the revision history was upgraded so that the
C        version number increases over time.  This wasn't true
C        before. In addition some early revision data that referred to
C        pre-SPICELIB modifications were removed. This editing of
C        the version numbers makes it unlikely that anyone can track
C        down which previous version of this routine they have by
C        looking at the version number.  The best way to determine
C        the routine you had previously is to compare the dates
C        stored in the Version line of the routine.
C
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SWPOOL' )
      END IF

C
C     Initialize the pool if necessary.
C
      IF ( FIRST ) THEN

         CALL ZZPINI (  FIRST,
     .                  MAXVAR, MAXVAL, MAXLIN,
     .                  BEGDAT, BEGTXT,
     .                  NMPOOL, DPPOOL, CHPOOL,
     .                  NAMLST, DATLST,
     .                  MAXAGT, MXNOTE, WTVARS, 
     .                  WTPTRS, WTPOOL, WTAGNT,
     .                  AGENTS, ACTIVE, NOTIFY, SUBCTR )

      END IF

C
C     Increment POOL state counter. Although setting a watcher does not
C     change the POOL we will increment the POOL state counter to make
C     sure that the next call to CVPOOL with this watcher triggers the
C     initial update.
C
      CALL ZZCTRINC ( SUBCTR ) 

C
C     Do all of the error checking we need to do BEFORE touching
C     the watcher data structure. We don't want to end up with
C     a partial update due to running out of room in mid-update.
C
C     First make sure we can handle any new kernel variable names.
C
      NEED = 0

      DO I = 1, NNAMES
         
         IF (  .NOT.  ELEMC( NAMES(I), WTVARS )  ) THEN
            NEED = NEED + 1
         END IF

      END DO
         
      SPACE = SIZEC(WTVARS) - CARDC(WTVARS)


      IF ( NEED .GT. SPACE ) THEN

         CALL SETMSG ( 'The watched kernel variable name '
     .   //            'list WTVARS has room for # more '
     .   //            'elements, so the # new names (in ' 
     .   //            'a list of # names) ' 
     .   //            'associated with agent # '
     .   //            'cannot be inserted.'              )
         CALL ERRINT ( '#', SPACE                         )
         CALL ERRINT ( '#', NEED                          )
         CALL ERRINT ( '#', NNAMES                        )
         CALL ERRCH  ( '#', AGENT                         )
         CALL SIGERR ( 'SPICE(KERVARSETOVERFLOW)'         )
         CALL CHKOUT ( 'SWPOOL'                           )
         RETURN

      END IF
 

C
C     If the input agent is a new one for any member of NAMES, 
C     make sure we have enough room to store this agent. Also
C     check for kernel variables that would have more than
C     MAXAGT agents watching them if this watch were established.
C
      NEED = 0

      DO I = 1, NNAMES
C
C        Get the agents associated with NAMES(I). The output argument
C        ACTIVE is a SPICE set.
C
         CALL ZZGAPOOL ( NAMES(I), WTVARS, WTPTRS, 
     .                   WTPOOL,   WTAGNT, ACTIVE )


         NFETCH = CARDC( ACTIVE )
        

         NOAGNT =       ( NFETCH .EQ.   0               ) 
     .             .OR. ( .NOT.  ELEMC( AGENT, ACTIVE ) )

         IF ( NOAGNT ) THEN

            NEED = NEED + 1
C
C           Check the number of agents already associated with the 
C           current kernel variable.
C
            IF ( NFETCH .EQ. MAXAGT ) THEN
            
               CALL SETMSG ( 'The list of agents to notify when # '   //
     .                       'is updated is too big. The maximum '    //
     .                       'number of agents that any kernel'       //
     .                       'pool variable can activate is #.'       )
               CALL ERRCH  ( '#', NAMES(I)                            )
               CALL ERRINT ( '#', MAXAGT                              )
               CALL SIGERR ( 'SPICE(TOOMANYWATCHES)'                  )
               CALL CHKOUT ( 'SWPOOL'                                 )
               RETURN
 
            END IF

         END IF

      END DO

C
C     See whether WTAGNT has enough room to set this watch.
C
      SPACE = LNKNFN( WTPOOL )

      IF ( NEED .GT. SPACE ) THEN

         CALL SETMSG ( 'The watched kernel variable agent '
     .   //            'list WTAGNT has room for # more '
     .   //            'elements, so the # new occurrences ' 
     .   //            'of agent # required for the input '
     .   //            'watch cannot be inserted.'          )
         CALL ERRINT ( '#', SPACE                           )
         CALL ERRINT ( '#', NEED                            )
         CALL ERRCH  ( '#', AGENT                           )
         CALL SIGERR ( 'SPICE(AGENTLISTOVERFLOW)'           )
         CALL CHKOUT ( 'SWPOOL'                             )
         RETURN

      END IF


C     
C     All of the overflow checks have been done. We finally can
C     get on with setting the specified watch.
C
C     For each variable specified by the array NAMES, put AGENT
C     into its list of guys to be notified when a variable change
C     occurs.
C
      DO I = 1, NNAMES
C
C        Get the agents associated with NAMES(I). The output argument
C        ACTIVE is a SPICE set.
C
         CALL ZZGAPOOL ( NAMES(I), WTVARS, WTPTRS, 
     .                   WTPOOL,   WTAGNT, ACTIVE )

         NFETCH = CARDC( ACTIVE )
        
C
C        Three things can happen now:
C
C           1) The kernel variable NAMES(I) is already watched by at
C              least one agent, but not by AGENT. We need to add AGENT
C              to the list of agents watching NAMES(I).
C
C           2) The kernel variable NAMES(I) isn't yet watched by any
C              agent, so we need to insert NAMES(I) into WTVARS, as
C              well as add AGENT to the (empty) list of agents watching
C              NAMES(I).
C
C           3) The kernel variable NAMES(I) is already watched by AGENT.
C              No action is needed.
C
C        We could get fancy and try to minimize the number of lines of
C        code required to handle the first two cases...but we won't.
C        We'll just take them one at a time.
C
C         
         IF ( NFETCH .GT. 0 ) THEN

            IF (  .NOT.  ELEMC( AGENT, ACTIVE )  ) THEN 
C
C              Case 1: at least one agent is already watching NAMES(I),
C              but AGENT is not watching NAMES(I). We need the head of
C              the agent list for this kernel variable.
C
               J    = BSRCHC ( NAMES(I), CARDC(WTVARS), WTVARS(1) )

               HEAD = WTPTRS(J)

C
C              Allocate a free node in the watch pool; append this node
C              to the tail of the agent list for the kernel variable;
C              we know that list is non-empty.
C
               CALL LNKAN  ( WTPOOL, NODE )

               TAIL = LNKTL( HEAD,   WTPOOL ) 

               CALL LNKILA ( TAIL,   NODE, WTPOOL )

C
C              Store the agent name at index NODE in the agent list.
C
               WTAGNT( NODE ) = AGENT

C
C              The insertion is complete. We update AGENTS, which is
C              the set of agents to notify, at the end of this routine.
C
            END IF

         ELSE
C
C           Case 2: the kernel variable NAMES(I) isn't watched. Add it
C           the watcher system. We've already ensured that there's
C           room in WTVARS and WTAGNT and that the insertion won't give 
C           NAMES(I) an excessive number of agents.
C
C           Let J be the insertion index in WTVARS. Since NAMES(I)
C           isn't yet a member of WTWARS, the insertion index will
C           always follow that of the last element in WTVARS
C           less than NAMES(I).
C
            J = 1 + LSTLTC ( NAMES(I), CARDC(WTVARS), WTVARS(1) )

C
C           Note that we don't use INSRTC to add NAMES(I) to WTVARS
C           because we need the insertion index, and we don't want
C           to execute a redundant search to find it.
C
C           We're now going to expand both the set WTVARS and the
C           parallel array WTPTRS by inserting new values at index J.
C           WTVARS(J) will receive the new kernel variable name
C           NAMES(I) and WTPTRS(J) will receive a new node in the watch
C           pool: this node provides an index into the agent list for
C           NAMES(I).
C          
C           Let NVARS be the size of the array WTVARS(1:*) prior to 
C           the insertion. NVARS will be updated by INSLAC. 
C
C           NPTRS is the size of the associated pointer table WTPTRS.
C
            NVARS = CARDC( WTVARS )
            NPTRS = NVARS

            CALL INSLAC ( NAMES(I), 1, J, WTVARS(1), NVARS )

C
C           WTVARS is actually a set, so we must update its cardinality.
C
            CALL SCARDC ( NVARS, WTVARS )

C
C           Allocate a free node in the watch pool.
C
            CALL LNKAN ( WTPOOL, NODE )

C
C           Now insert NODE in the pointer table WTPTRS at index J.
C
            CALL INSLAI ( NODE, 1, J, WTPTRS, NPTRS )
 
C
C           Store the agent name at index NODE in the agent list.
C
            WTAGNT( NODE ) = AGENT

C
C           The insertion is complete. We update AGENTS, which is the
C           set of agents to notify, at the end of this routine.

         END IF
             
      END DO
  
C
C     We ALWAYS put this agent into the list of agents to be notified.
C
      CALL INSRTC ( AGENT, AGENTS )

C
C     That is all.
C
      CALL CHKOUT ( 'SWPOOL' )
      RETURN




 
 
 
C$Procedure CVPOOL ( Check variable in the pool for update)
 
      ENTRY CVPOOL ( AGENT, UPDATE )
 
C$ Abstract
C
C     Indicate whether or not any watched kernel variables that have a
C     specified agent on their notification list have been updated.
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
C     KERNEL
C
C$ Keywords
C
C     SYMBOLS
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         AGENT
C     LOGICAL               UPDATE
C
C$ Brief_I/O
C
C     Variable  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     AGENT      I   Name of the agent to check for notices.
C     UPDATE     O   .TRUE. if variables for AGENT have been updated.
C
C$ Detailed_Input
C
C     AGENT     is the name of a subroutine, entry point, or significant
C               portion of code that needs to access variables in the
C               kernel pool.  Generally this agent will buffer these
C               variables internally and fetch them from the kernel
C               pool only when they are updated.
C
C$ Detailed_Output
C
C     UPDATE    is a logical flag that will be set to .TRUE. if the
C               variables in the kernel pool that are associated with
C               AGENT have been updated since the last call to CVPOOL.
C
C               UPDATE will be set to .TRUE. on the first call made for
C               the specified agent, whether or not the associated
C               variables have been updated since the agent was placed
C               on their notification list, as long as the agent is
C               associated with any watched variables.
C
C$ Parameters
C
C     See the umbrella subroutine POOL.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This entry point allows the calling program to determine
C     whether or not variables associated with with AGENT have
C     been updated.  Making use of this entry point in conjunction
C     with the entry point SWPOOL (set watch on pool variables)
C     modules can buffer kernel pool variables they need and
C     fetch values from the kernel pool only when variables have
C     been updated.
C
C     Note that the call to CVPOOL has a side effect.
C     Two consecutive calls to CVPOOL with the same
C     AGENT will always result in the UPDATE being .FALSE.
C     on the second call.  In other words, if you embed
C     the following two lines of code in a piece of code
C
C        CALL CVPOOL ( AGENT, UPDATE )
C        CALL CVPOOL ( AGENT, UPDATE )
C
C     and then test UPDATE, it will be FALSE.  The idea is
C     that once a call to CVPOOL has been made, the
C     kernel pool has performed its duty and notified the
C     calling routine that one of the AGENT's variables
C     has been updated.  Consequently, on the second call
C     to CVPOOL above, the kernel pool will not have any
C     updates to report about any of AGENT's variables.
C
C     If, on the other hand, you have code such as
C
C        CALL CVPOOL ( AGENT, UPDATE )
C        CALL LDPOOL ( 'MYFILE.DAT'  )
C        CALL CVPOOL ( AGENT, UPDATE )
C
C     the value of UPDATE will be true if one of the variables
C     associated with AGENT was updated by the call to
C     LDPOOL (and that variable has been specified as one
C     to watch by call a call to SWPOOL).
C
C     It should also be noted that any call to CVPOOL that
C     occurs immediately after a call to SWPOOL will result in
C     UPDATE being returned as .TRUE.  In other words, code
C     such as shown below, will always result in the value
C     of UPDATE as being returned .TRUE.
C
C        CALL SWPOOL ( AGENT, NNAMES, NAMES  )
C        CALL CVPOOL ( AGENT,         UPDATE )
C
C     See the header for SWPOOL for a full discussion of this
C     feature.
C
C$ Examples
C
C     Suppose that you have an application subroutine, MYTASK, that
C     needs to access a large data set in the kernel pool.  If this
C     data could be kept in local storage and kernel pool queries
C     performed only when the data in the kernel pool has been
C     updated, the routine can perform much more efficiently.
C
C     The code fragment below illustrates how you might make use of this
C     feature.
C
C     C
C     C     On the first call to this routine establish those variables
C     C     that we will want to read from the kernel pool only when
C     C     new values have been established.
C     C
C           IF ( FIRST ) THEN
C
C              FIRST = .FALSE.
C              HAVE  = .FALSE.
C
C              CALL SWPOOL ( 'MYTASK', NNAMES, NAMES )
C
C           END IF
C
C     C
C     C     If any of the variables has been updated, fetch
C     C     it from the kernel pool. (Note that this also
C     C     handles getting variables for the first time.)
C     C     We use HAVE to indicate the fetch succeeded. If it
C     C     didn't, we need to attempt the fetch on the next 
C     C     pass into this routine.
C     C
C           CALL CVPOOL ( 'MYTASK', UPDATE )
C
C           IF (  UPDATE  .OR (.NOT. HAVE ) ) THEN
C
C              CALL GDPOOL ( 'MYTASK_VAR_1', 1, M, N1, VALS1, FOUND(1) )
C              CALL GDPOOL ( 'MYTASK_VAR_2', 1, M, N2, VALS2, FOUND(2) )
C                      .
C                      .
C                      .
C              CALL GDPOOL ( 'MYTASK_VAR_N', 1, M, NN, VALSN, FOUND(N) )
C
C           END IF
C
C           IF ( FAILED() ) THEN
C                 .
C                 .
C              do something about the failure
C                 .
C                 .
C
C           ELSE
C
C              HAVE = .TRUE.
C
C           END IF
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
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.1.1, 30-JUN-2014 (NJB)
C
C        Description of the output variable UPDATE now
C        mentions that the initial value of .TRUE. will
C        be returned after an agent is associated with
C        kernel variables.
C
C-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
C
C        ZZPINI call was updated for compatibility
C        with new watcher system implementation.
C
C        The code example was updated to handle kernel pool
C        fetch failure.
C
C-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
C
C        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
C        direct insertion of data into the kernel pool without having
C        to read an external file.
C
C        Added the interface LMPOOL that allows SPICE
C        programs to load text kernels directly from memory
C        instead of requiring a text file.
C
C        Added the entry point SZPOOL to return kernel pool definition
C        parameters.
C
C        Added the entry point DVPOOL to allow the removal of a variable
C        from the kernel pool.
C
C        Added the entry point GNPOOL to allow users to determine
C        variables that are present in the kernel pool
C
C-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
C
C        The implementation of the kernel pool was completely redone
C        to improve performance in loading and fetching data.  In
C        addition the pool was upgraded so that variables may be
C        either string or numeric valued.
C
C-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
C
C        The entry points SWPOOL and CVPOOL were added.
C
C-&
 
C$ Index_Entries
C
C     Check the kernel pool for updated variables
C
C-&
 
C$ Revisions
C
C        The implementation of the kernel pool was completely redone
C        to improve performance in loading and fetching data.  In
C        addition the pool was upgraded so that variables may be
C        either string or numeric valued.
C
C        The basic data structure used to maintain the list of
C        variable names and values was replaced with a hash table
C        implementation.  Data and names are accessed by means
C        of a hash function and linked lists of pointers to existing
C        variable names and data values.
C
C-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
C
C        The entry points SWPOOL (set watch on a pool variable)
C        and CVPOOL (check variable for update) so that routines
C        that buffer data stored in the kernel pool can fetch
C        that data only when it is updated.
C
C        In addition, the revision history was upgraded so that the
C        version number increases over time.  This wasn't true
C        before. In addition some early revision data that referred to
C        pre-SPICELIB modifications were removed. This editing of
C        the version numbers makes it unlikely that anyone can track
C        down which previous version of this routine they have by
C        looking at the version number.  The best way to determine
C        the routine you had previously is to compare the dates
C        stored in the Version line of the routine.
C
C-&
 
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CVPOOL' )
      END IF

C
C     Initialize the pool if necessary.
C
      IF ( FIRST ) THEN

         CALL ZZPINI (  FIRST,
     .                  MAXVAR, MAXVAL, MAXLIN,
     .                  BEGDAT, BEGTXT,
     .                  NMPOOL, DPPOOL, CHPOOL,
     .                  NAMLST, DATLST,
     .                  MAXAGT, MXNOTE, WTVARS, 
     .                  WTPTRS, WTPOOL, WTAGNT,
     .                  AGENTS, ACTIVE, NOTIFY, SUBCTR )

      END IF
 
C
C     Check to see if our agent is on the list of agents to be
C     notified.  If it is, we take this agent off the list---he's
C     now considered to have been notified.
C
      UPDATE = ELEMC  ( AGENT, AGENTS )
 
      IF ( UPDATE ) THEN
         CALL REMOVC ( AGENT, AGENTS )
      END IF
 

      CALL CHKOUT ( 'CVPOOL' )
      RETURN
 
 
 
 
 
C$Procedure GCPOOL (Get character data from the kernel pool)
 
      ENTRY GCPOOL ( NAME, START, ROOM, N, CVALS, FOUND )
 
C$ Abstract
C
C     Return the character value of a kernel variable from the
C     kernel pool.
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
C     KERNEL
C
C$ Keywords
C
C     CONSTANTS
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         NAME
C     INTEGER               START
C     INTEGER               ROOM
C     INTEGER               N
C     CHARACTER*(*)         CVALS    ( * )
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of the variable whose value is to be returned.
C     START      I   Which component to start retrieving for NAME
C     ROOM       I   The largest number of values to return.
C     N          O   Number of values returned for NAME.
C     CVALS      O   Values associated with NAME.
C     FOUND      O   True if variable is in pool.
C
C$ Detailed_Input
C
C     NAME       is the name of the variable whose values are to be
C                returned. If the variable is not in the pool with
C                character type, FOUND will be FALSE.
C
C     START      is the index of the first component of NAME to return.
C                If START is less than 1, it will be treated as 1.  If
C                START is greater than the total number of components
C                available for NAME, no values will be returned (N will
C                be set to zero).  However, FOUND will still be set to
C                .TRUE.
C
C     ROOM       is the maximum number of components that should be
C                returned for this variable.  (Usually it is the amount
C                of ROOM available in the array CVALS). If ROOM is
C                less than 1 the error 'SPICE(BADARRAYSIZE)' will be
C                signaled.
C
C$ Detailed_Output
C
C     N          is the number of values associated with NAME that
C                are returned.  It will always be less than or equal
C                to ROOM.
C
C                If NAME is not in the pool with character type, no
C                value is given to N.
C
C     CVALS      is the array of values associated with NAME.
C                If NAME is not in the pool with character type, no
C                values are given to the elements of CVALS.
C
C                If the length of CVALS is less than the length of
C                strings stored in the kernel pool (see MAXCHR) the
C                values returned will be truncated on the right.
C
C     FOUND      is TRUE if the variable is in the pool and has
C                character type, FALSE if it is not.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the value of ROOM is less than one the error
C        'SPICE(BADARRAYSIZE)' is signaled.
C
C     2) If CVALS has declared length less than the size of a
C        string to be returned, the value will be truncated on
C        the right.  See MAXCHR for the maximum stored size of
C        string variables.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine provides the user interface to retrieving
C     character data stored in the kernel pool.  This interface
C     allows you to retrieve the data associated with a variable
C     in multiple accesses.  Under some circumstances this alleviates
C     the problem of having to know in advance the maximum amount
C     of space needed to accommodate all kernel variables.
C
C     However, this method of access does come with a price. It is
C     always more efficient to retrieve all of the data associated
C     with a kernel pool data in one call than it is to retrieve
C     it in sections.
C
C     See also the entry points GDPOOL and GIPOOL.
C
C$ Examples
C
C
C     The following code fragment demonstrates how the data stored
C     in a kernel pool variable can be retrieved in pieces.
C
C     First we need some declarations.
C
C        INTEGER               ROOM
C        PARAMETER           ( ROOM = 3 )
C
C        CHARACTER*(8)         VARNAM
C        CHARACTER*(3)         INDENT
C        INTEGER               START
C        INTEGER               N
C        LOGICAL               FOUND
C        CHARACTER*(80)        CVALS(ROOM)
C
C
C     Next load the data in the file 'typical.ker' into the
C     kernel pool.
C
C        CALL LDPOOL ( 'typical.ker' )
C
C     Next we shall print the values stored for the kernel pool
C     variable 'MYDATA'
C
C        VARNAM = 'MYDATA'
C        INDENT = ' '
C        START  =  1
C
C        CALL GCPOOL ( VARNAM, START, ROOM, N, CVALS, FOUND )
C
C        IF ( .NOT. FOUND )
C           WRITE (*,*) 'There is no string data available for MYDATA.'
C        ELSE
C
C           WRITE (*,*) 'Values for MYDATA.'
C           WRITE (*,*)
C
C           DO I = 1, N
C              WRITE (*,*) INDENT, CVALS(I)
C           END DO
C
C           DO WHILE ( N .EQ. ROOM )
C
C              START = START + N
C              CALL GCPOOL ( VARNAM, START, ROOM, N, CVALS, FOUND )
C
C              DO I = 1, N
C                 WRITE (*,*) INDENT, CVALS(I)
C              END DO
C
C           END DO
C
C        END IF
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
C     W.L. Taber  (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
C
C        ZZPINI call was updated for compatibility
C        with new watcher system implementation.
C
C-    SPICELIB Version 8.0.1, 22-DEC-2004 (NJB)
C
C        Corrected an in-line comment relating to finding the
C        head node of the conflict resolution list for NAME.
C
C-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
C
C        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
C        direct insertion of data into the kernel pool without having
C        to read an external file.
C
C        Added the interface LMPOOL that allows SPICE
C        programs to load text kernels directly from memory
C        instead of requiring a text file.
C
C        Added the entry point SZPOOL to return kernel pool definition
C        parameters.
C
C        Added the entry point DVPOOL to allow the removal of a variable
C        from the kernel pool.
C
C        Added the entry point GNPOOL to allow users to determine
C        variables that are present in the kernel pool
C
C-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
C
C        The implementation of the kernel pool was completely redone
C        to improve performance in loading and fetching data.  In
C        addition the pool was upgraded so that variables may be
C        either string or numeric valued.
C
C        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
C        to the routine.
C
C-&
 
C$ Index_Entries
C
C     RETURN the character value of a pooled kernel variable
C     RETURN the string value of a pooled kernel variable
C
C-&
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'GCPOOL' )
      END IF


C
C     Initialize the pool if necessary.
C
      IF ( FIRST ) THEN

         CALL ZZPINI (  FIRST,
     .                  MAXVAR, MAXVAL, MAXLIN,
     .                  BEGDAT, BEGTXT,
     .                  NMPOOL, DPPOOL, CHPOOL,
     .                  NAMLST, DATLST,
     .                  MAXAGT, MXNOTE, WTVARS, 
     .                  WTPTRS, WTPOOL, WTAGNT,
     .                  AGENTS, ACTIVE, NOTIFY, SUBCTR )

      END IF

C
C     Perform the one obvious error check first.
C
      IF ( ROOM .LT. 1 ) THEN
 
         CALL SETMSG ( 'The amount of room specified as '
     .   //            'available for output in the output array '
     .   //            'was: #.  The amount of room must be '
     .   //            'positive. ' )
 
 
         CALL ERRINT ( '#', ROOM )
         CALL SIGERR ( 'SPICE(BADARRAYSIZE)'  )
         CALL CHKOUT ( 'GCPOOL' )
         RETURN
 
      END IF
 

 
C
C     Compute the hash value of this name.
C 
      LOOKAT = ZZHASH ( NAME )

C
C     Now see if there is a non-empty conflict resolution list for the
C     input string NAME.  If so, NAMLST(LOOKAT) contains the head node
C     of the conflict resolution list; this node is a positive value.
C
      IF ( NAMLST(LOOKAT) .EQ. 0 ) THEN
 
         FOUND = .FALSE.
         CALL CHKOUT ( 'GCPOOL' )
         RETURN
 
      END IF
C
C     If were are still here NAMLST(LOOKAT) is the first node of
C     a conflict resolution list.  See if the NAME corresponding
C     to this node is the one we are looking for.
C
      NODE   = NAMLST( LOOKAT      )
      SUCCES = NAME .EQ. PNAMES(NODE)
 
      DO WHILE ( .NOT. SUCCES )
 
         NODE = NMPOOL ( NEXT, NODE )
 
         IF ( NODE .LT. 0 ) THEN
 
            FOUND = .FALSE.
            CALL CHKOUT ( 'GCPOOL' )
            RETURN
 
         END IF
 
         SUCCES = NAME .EQ. PNAMES(NODE)
 
      END DO
C
C     If you get to this point, the variable NAME is present in the
C     list of names at PNAMES(NODE), ABS( DATLST(NODE) ) points to the
C     head of a linked list of values for this NAME.
C
      DATAHD = DATLST(NODE)
 
      IF ( DATAHD .GT. 0 ) THEN
 
         N     =  0
         FOUND = .FALSE.
         CALL CHKOUT ( 'GCPOOL' )
         RETURN
 
      ELSE IF ( DATAHD .EQ. 0 ) THEN
 
         CALL SETMSG ( 'This is never supposed to happen.  The '
     .   //            'requested name, ''#'', was found in the '
     .   //            'name list, but the pointer to the head '
     .   //            'of the data for this variable is zero. '
     .   //            'Please note your activities and report '
     .   //            'this error to NAIF. '   )
         CALL ERRCH  ( '#', NAME(1:RTRIM(NAME)) )
         CALL SIGERR ( 'SPICE(BUG)'             )
         CALL CHKOUT ( 'GCPOOL'                 )
         RETURN
 
      END IF
 
      FOUND = .TRUE.
      K     =  0
      N     =  0
      BEGIN =  MAX( START, 1 )
      NODE  =  -DATAHD
 
      DO WHILE ( NODE .GT. 0  )
 
         K = K + 1
 
         IF ( K .GE. BEGIN ) THEN
            N        = N+1
            CVALS(N) = CHVALS(     NODE)
 
            IF ( N .EQ. ROOM ) THEN
               CALL CHKOUT ( 'GCPOOL' )
               RETURN
            END IF
 
         END IF
 
         NODE = CHPOOL(NEXT,NODE)
 
      END DO
 
      CALL CHKOUT ( 'GCPOOL' )
      RETURN
 
 
 
 
 
 
 
 
C$Procedure GDPOOL (Get d.p. values from the kernel pool)
 
      ENTRY GDPOOL ( NAME, START, ROOM, N, VALUES, FOUND )
 
C$ Abstract
C
C     Return the d.p. value of a kernel variable from the kernel pool.
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
C     KERNEL
C
C$ Keywords
C
C     CONSTANTS
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         NAME
C     INTEGER               START
C     INTEGER               ROOM
C     INTEGER               N
C     DOUBLE PRECISION      VALUES   ( * )
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of the variable whose value is to be returned.
C     START      I   Which component to start retrieving for NAME
C     ROOM       I   The largest number of values to return.
C     N          O   Number of values returned for NAME.
C     VALUES     O   Values associated with NAME.
C     FOUND      O   True if variable is in pool.
C
C$ Detailed_Input
C
C     NAME       is the name of the variable whose values are to be
C                returned. If the variable is not in the pool with
C                numeric type, FOUND will be FALSE.
C
C     START      is the index of the first component of NAME to return.
C                If START is less than 1, it will be treated as 1.  If
C                START is greater than the total number of components
C                available for NAME, no values will be returned (N will
C                be set to zero).  However, FOUND will still be set to
C                .TRUE.
C
C     ROOM       is the maximum number of components that should be
C                returned for this variable.  (Usually it is the amount
C                of ROOM available in the array VALUES). If ROOM is
C                less than 1 the error 'SPICE(BADARRAYSIZE)' will be
C                signaled.
C
C$ Detailed_Output
C
C     N          is the number of values associated with NAME that
C                are returned.  It will always be less than or equal
C                to ROOM.
C
C                If NAME is not in the pool with numeric type, no value
C                is given to N.
C
C     VALUES     is the array of values associated with NAME.
C                If NAME is not in the pool with numeric type, no
C                values are given to the elements of VALUES.
C
C     FOUND      is TRUE if the variable is in the pool and has numeric
C                type, FALSE if it is not.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the value of ROOM is less than one the error
C        'SPICE(BADARRAYSIZE)' is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine provides the user interface to retrieving
C     numeric data stored in the kernel pool.  This interface
C     allows you to retrieve the data associated with a variable
C     in multiple accesses.  Under some circumstances this alleviates
C     the problem of having to know in advance the maximum amount
C     of space needed to accommodate all kernel variables.
C
C     However, this method of access does come with a price. It is
C     always more efficient to retrieve all of the data associated
C     with a kernel pool data in one call than it is to retrieve
C     it in sections.
C
C     This routine should be used in place of RTPOOL when possible
C     as it avoids errors associated with writing data past the
C     end of an array.
C
C     See also the entry points GIPOOL and GCPOOL.
C
C$ Examples
C
C
C     The following code fragment demonstrates how the data stored
C     in a kernel pool variable can be retrieved in pieces.
C
C     First we need some declarations.
C
C        INTEGER               ROOM
C        PARAMETER           ( ROOM = 3 )
C
C        CHARACTER*(8)         VARNAM
C        CHARACTER*(3)         INDENT
C        INTEGER               START
C        INTEGER               N
C        LOGICAL               FOUND
C        DOUBLE PRECISION      VALUES(ROOM)
C
C
C     Next load the data in the file 'typical.ker' into the
C     kernel pool.
C
C
C
C        CALL LDPOOL ( 'typical.ker' )
C
C     Next we shall print the values stored for the kernel pool
C     variable 'MYDATA'
C
C        VARNAM = 'MYDATA'
C        INDENT = ' '
C        START  =  1
C
C        CALL GDPOOL ( VARNAM, START, ROOM, N, VALUES, FOUND )
C
C        IF ( .NOT. FOUND )
C           WRITE (*,*) 'There is no numeric data available for MYDATA.'
C        ELSE
C
C           WRITE (*,*) 'Values for MYDATA.'
C           WRITE (*,*)
C
C           DO I = 1, N
C              WRITE (*,*) INDENT, VALUES(I)
C           END DO
C
C           DO WHILE ( N .EQ. ROOM )
C
C              START = START + N
C              CALL GDPOOL ( VARNAM, START, ROOM, N, VALUES, FOUND )
C
C              DO I = 1, N
C                 WRITE (*,*) INDENT, VALUES(I)
C              END DO
C
C           END DO
C
C        END IF
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
C     W.L. Taber  (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
C
C        ZZPINI call was updated for compatibility
C        with new watcher system implementation.
C
C-    SPICELIB Version 8.0.1, 22-DEC-2004 (NJB)
C
C        Corrected an in-line comment relating to finding the
C        head node of the conflict resolution list for NAME.
C
C-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
C
C        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
C        direct insertion of data into the kernel pool without having
C        to read an external file.
C
C        Added the interface LMPOOL that allows SPICE
C        programs to load text kernels directly from memory
C        instead of requiring a text file.
C
C        Added the entry point SZPOOL to return kernel pool definition
C        parameters.
C
C        Added the entry point DVPOOL to allow the removal of a variable
C        from the kernel pool.
C
C        Added the entry point GNPOOL to allow users to determine
C        variables that are present in the kernel pool
C
C-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
C
C        The implementation of the kernel pool was completely redone
C        to improve performance in loading and fetching data.  In
C        addition the pool was upgraded so that variables may be
C        either string or numeric valued.
C
C        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
C        to the routine.
C
C-&
 
C$ Index_Entries
C
C     RETURN the d.p. value of a pooled kernel variable
C     RETURN the numeric value of a pooled kernel variable
C
C-&
 

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'GDPOOL' )
      END IF


C
C     Initialize the pool if necessary.
C
      IF ( FIRST ) THEN

         CALL ZZPINI (  FIRST,
     .                  MAXVAR, MAXVAL, MAXLIN,
     .                  BEGDAT, BEGTXT,
     .                  NMPOOL, DPPOOL, CHPOOL,
     .                  NAMLST, DATLST,
     .                  MAXAGT, MXNOTE, WTVARS, 
     .                  WTPTRS, WTPOOL, WTAGNT,
     .                  AGENTS, ACTIVE, NOTIFY, SUBCTR )

      END IF

C
C     Perform the one obvious error check first.
C
      IF ( ROOM .LT. 1 ) THEN
 
         CALL SETMSG ( 'The amount of room specified as '
     .   //            'available for output in the output array '
     .   //            'was: #.  The amount of room must be '
     .   //            'positive. ' )
 
 
         CALL ERRINT ( '#', ROOM )
         CALL SIGERR ( 'SPICE(BADARRAYSIZE)'  )
         CALL CHKOUT ( 'GDPOOL' )
         RETURN
 
      END IF
  
 
C
C     Compute the hash value of this name.
C
      LOOKAT = ZZHASH ( NAME )

C
C     Now see if there is a non-empty conflict resolution list for the
C     input string NAME.  If so, NAMLST(LOOKAT) contains the head node
C     of the conflict resolution list; this node is a positive value.
C
      IF ( NAMLST(LOOKAT) .EQ. 0 ) THEN
 
         FOUND = .FALSE.
         CALL CHKOUT ( 'GDPOOL' )
         RETURN
 
      END IF
C
C     If were are still here NAMLST(LOOKAT) is the first node of
C     a conflict resolution list.  See if the NAME corresponding
C     to this node is the one we are looking for.
C
      NODE   = NAMLST( LOOKAT      )
      SUCCES = NAME .EQ. PNAMES(NODE)
 
      DO WHILE ( .NOT. SUCCES )
 
         NODE = NMPOOL ( NEXT, NODE )
 
         IF ( NODE .LT. 0 ) THEN
 
            FOUND = .FALSE.
            CALL CHKOUT ( 'GDPOOL' )
            RETURN
 
         END IF
 
         SUCCES = NAME .EQ. PNAMES(NODE)
 
      END DO
C
C     If you get to this point, the variable NAME is present in the
C     list of names at PNAMES(NODE), ABS( DATLST(NODE) ) points to the
C     head of a linked list of values for this NAME.
C
      DATAHD = DATLST(NODE)
 
      IF ( DATAHD .LT. 0 ) THEN
 
         N     =  0
         FOUND = .FALSE.
         CALL CHKOUT ( 'GDPOOL' )
         RETURN
 
      ELSE IF ( DATAHD .EQ. 0 ) THEN
 
         CALL SETMSG ( 'This is never supposed to happen.  The '
     .   //            'requested name, ''#'', was found in the '
     .   //            'name list, but the pointer to the head '
     .   //            'of the data for this variable is zero. '
     .   //            'Please note your activities and report '
     .   //            'this error to NAIF. '   )
         CALL ERRCH  ( '#', NAME(1:RTRIM(NAME)) )
         CALL SIGERR ( 'SPICE(BUG)'             )
         CALL CHKOUT ( 'GDPOOL'                 )
         RETURN
 
      END IF
 
      FOUND = .TRUE.
      K     =  0
      N     =  0
      BEGIN =  MAX( START, 1 )
      NODE  =  DATAHD
 
      DO WHILE ( NODE .GT. 0  )
 
         K = K + 1
 
         IF ( K .GE. BEGIN ) THEN
            N = N+1
            VALUES(N) = DPVALS(     NODE)
 
            IF ( N .EQ. ROOM ) THEN
               CALL CHKOUT ( 'GDPOOL' )
               RETURN
            END IF
 
         END IF
 
         NODE = DPPOOL(NEXT,NODE)
 
      END DO
 
      CALL CHKOUT ( 'GDPOOL' )
      RETURN
 
 
 
 
 
C$Procedure GIPOOL (Get integers from the kernel pool)
 
      ENTRY GIPOOL ( NAME, START, ROOM, N, IVALS, FOUND )
 
C$ Abstract
C
C     Return the integer value of a kernel variable from the
C     kernel pool.
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
C     KERNEL
C
C$ Keywords
C
C     CONSTANTS
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         NAME
C     INTEGER               START
C     INTEGER               ROOM
C     INTEGER               N
C     INTEGER               IVALS    ( * )
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of the variable whose value is to be returned.
C     START      I   Which component to start retrieving for NAME
C     ROOM       I   The largest number of values to return.
C     N          O   Number of values returned for NAME.
C     IVALS      O   Values associated with NAME.
C     FOUND      O   True if variable is in pool.
C
C$ Detailed_Input
C
C     NAME       is the name of the variable whose values are to be
C                returned. If the variable is not in the pool with
C                numeric type, FOUND will be FALSE.
C
C     START      is the index of the first component of NAME to return.
C                If START is less than 1, it will be treated as 1.  If
C                START is greater than the total number of components
C                available for NAME, no values will be returned (N will
C                be set to zero).  However, FOUND will still be set to
C                .TRUE.
C
C     ROOM       is the maximum number of components that should be
C                returned for this variable.  (Usually it is the amount
C                of ROOM available in the array IVALS). If ROOM is
C                less than 1 the error 'SPICE(BADARRAYSIZE)' will be
C                signaled.
C
C$ Detailed_Output
C
C     N          is the number of values associated with NAME that
C                are returned.  It will always be less than or equal
C                to ROOM.
C
C                If NAME is not in the pool with numeric type, no value
C                is given to N.
C
C     IVALS      is the array of values associated with NAME. Any
C                numeric value having non-zero fractional part is
C                rounded to the closest integer. If NAME is not in the
C                pool or does not have numeric type, no values are
C                assigned to the elements of IVALS.
C
C     FOUND      is TRUE if the variable is in the pool and has numeric
C                type, FALSE if it is not.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the value of ROOM is less than one the error
C        'SPICE(BADARRAYSIZE)' is signaled.
C
C     2) If a value requested is outside the valid range
C        of integers, the error 'SPICE(INTOUTOFRANGE)' is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine provides the user interface for retrieving
C     integer data stored in the kernel pool.  This interface
C     allows you to retrieve the data associated with a variable
C     in multiple accesses.  Under some circumstances this alleviates
C     the problem of having to know in advance the maximum amount
C     of space needed to accommodate all kernel variables.
C
C     However, this method of access does come with a price. It is
C     always more efficient to retrieve all of the data associated
C     with a kernel pool data in one call than it is to retrieve
C     it in sections.
C
C     See also the entry points GDPOOL and GCPOOL.
C
C$ Examples
C
C
C     The following code fragment demonstrates how the data stored
C     in a kernel pool variable can be retrieved in pieces.
C
C     First we need some declarations.
C
C        INTEGER               ROOM
C        PARAMETER           ( ROOM = 3 )
C
C        CHARACTER*(8)         VARNAM
C        CHARACTER*(3)         INDENT
C        INTEGER               START
C        INTEGER               N
C        LOGICAL               FOUND
C        INTEGER               IVALS(ROOM)
C
C
C     Next load the data in the file 'typical.ker' into the
C     kernel pool.
C
C        CALL LDPOOL ( 'typical.ker' )
C
C     Next we shall print the values stored for the kernel pool
C     variable 'MYDATA'
C
C        VARNAM = 'MYDATA'
C        INDENT = ' '
C        START  =  1
C
C        CALL GIPOOL ( VARNAM, START, ROOM, N, IVALS, FOUND )
C
C        IF ( .NOT. FOUND )
C           WRITE (*,*) 'There is no numeric data available for MYDATA.'
C        ELSE
C
C           WRITE (*,*) 'Values for MYDATA.'
C           WRITE (*,*)
C
C           DO I = 1, N
C              WRITE (*,*) INDENT, IVALS(I)
C           END DO
C
C           DO WHILE ( N .EQ. ROOM )
C
C              START = START + N
C              CALL GIPOOL ( VARNAM, START, ROOM, N, IVALS, FOUND )
C
C              DO I = 1, N
C                 WRITE (*,*) INDENT, IVALS(I)
C              END DO
C
C           END DO
C
C        END IF
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
C     W.L. Taber  (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.1.1, 14-JUL-2014 (NJB)
C
C        Updated description of IVALS in Detailed_Output
C        header section.
C
C-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
C
C        ZZPINI call was updated for compatibility
C        with new watcher system implementation.
C
C-    SPICELIB Version 8.0.1, 22-DEC-2004 (NJB)
C
C        Corrected an in-line comment relating to finding the
C        head node of the conflict resolution list for NAME.
C
C-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
C
C        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
C        direct insertion of data into the kernel pool without having
C        to read an external file.
C
C        Added the interface LMPOOL that allows SPICE
C        programs to load text kernels directly from memory
C        instead of requiring a text file.
C
C        Added the entry point SZPOOL to return kernel pool definition
C        parameters.
C
C        Added the entry point DVPOOL to allow the removal of a variable
C        from the kernel pool.
C
C        Added the entry point GNPOOL to allow users to determine
C        variables that are present in the kernel pool
C
C-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
C
C        The implementation of the kernel pool was completely redone
C        to improve performance in loading and fetching data.  In
C        addition the pool was upgraded so that variables may be
C        either string or numeric valued.
C
C        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
C        to the routine.
C
C-&
 
C$ Index_Entries
C
C     RETURN the integer value of a pooled kernel variable
C
C-&
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'GIPOOL' )
      END IF


C
C     Initialize the pool if necessary.
C
      IF ( FIRST ) THEN

         CALL ZZPINI (  FIRST,
     .                  MAXVAR, MAXVAL, MAXLIN,
     .                  BEGDAT, BEGTXT,
     .                  NMPOOL, DPPOOL, CHPOOL,
     .                  NAMLST, DATLST,
     .                  MAXAGT, MXNOTE, WTVARS, 
     .                  WTPTRS, WTPOOL, WTAGNT,
     .                  AGENTS, ACTIVE, NOTIFY, SUBCTR )

      END IF

C
C     Perform the one obvious error check first.
C
      IF ( ROOM .LT. 1 ) THEN
 
         CALL SETMSG ( 'The amount of room specified as '
     .   //            'available for output in the output array '
     .   //            'was: #.  The amount of room must be '
     .   //            'positive. ' )
 
 
         CALL ERRINT ( '#', ROOM )
         CALL SIGERR ( 'SPICE(BADARRAYSIZE)'  )
         CALL CHKOUT ( 'GIPOOL' )
         RETURN
 
      END IF


C
C     Compute the hash value of this name.
C
      LOOKAT = ZZHASH ( NAME )

C
C     Now see if there is a non-empty conflict resolution list for the
C     input string NAME.  If so, NAMLST(LOOKAT) contains the head node
C     of the conflict resolution list; this node is a positive value.
C
      IF ( NAMLST(LOOKAT) .EQ. 0 ) THEN
 
         FOUND = .FALSE.
         CALL CHKOUT ( 'GIPOOL' )
         RETURN
 
      END IF
C
C     If were are still here NAMLST(LOOKAT) is the first node of
C     a conflict resolution list.  See if the NAME corresponding
C     to this node is the one we are looking for.
C
      NODE   = NAMLST( LOOKAT      )
      SUCCES = NAME .EQ. PNAMES(NODE)
 
      DO WHILE ( .NOT. SUCCES )
 
         NODE = NMPOOL ( NEXT, NODE )
 
         IF ( NODE .LT. 0 ) THEN
 
            FOUND = .FALSE.
            CALL CHKOUT ( 'GIPOOL' )
            RETURN
 
         END IF
 
         SUCCES = NAME .EQ. PNAMES(NODE)
 
      END DO
C
C     If you get to this point, the variable NAME is present in the
C     list of names at PNAMES(NODE), ABS( DATLST(NODE) ) points to the
C     head of a linked list of values for this NAME.
C
      DATAHD = DATLST(NODE)
 
      IF ( DATAHD .LT. 0 ) THEN
 
         N     =  0
         FOUND = .FALSE.
         CALL CHKOUT ( 'GIPOOL' )
         RETURN
 
      ELSE IF ( DATAHD .EQ. 0 ) THEN
 
         CALL SETMSG ( 'This is never supposed to happen.  The '
     .   //            'requested name, ''#'', was found in the '
     .   //            'name list, but the pointer to the head '
     .   //            'of the data for this variable is zero. '
     .   //            'Please note your activities and report '
     .   //            'this error to NAIF. '   )
         CALL ERRCH  ( '#', NAME(1:RTRIM(NAME)) )
         CALL SIGERR ( 'SPICE(BUG)'             )
         CALL CHKOUT ( 'GIPOOL'                )
         RETURN
 
      END IF
C
C     Prepare for fetching values.
C
      BIG   =  DBLE( INTMAX() )
      SMALL =  DBLE( INTMIN() )
      FOUND = .TRUE.
      K     =  0
      N     =  0
      BEGIN =  MAX( START, 1 )
      NODE  =  DATAHD
 
      DO WHILE ( NODE .GT. 0  )
 
         K = K + 1
 
         IF ( K .GE. BEGIN ) THEN
            N = N+1
 
            IF (       DPVALS(NODE) .GE. SMALL
     .           .AND. DPVALS(NODE) .LE. BIG   ) THEN
 
               IVALS(N) = IDNINT( DPVALS( NODE) )
 
            ELSE
 
               CALL SETMSG ( 'The value associated with index # '
     .         //            'of the kernel variable # is '
     .         //            'outside the range of integers. '
     .         //            'The value stored was: # .'      )
 
 
               CALL ERRINT ( '#', K                     )
               CALL ERRCH  ( '#', NAME(1:RTRIM(NAME))   )
               CALL ERRDP  ( '#', DPVALS(NODE)          )
               CALL SIGERR ( 'SPICE(INTOUTOFRANGE)'     )
               CALL CHKOUT ( 'GIPOOL' )
               RETURN
 
 
            END IF
            IF ( N .EQ. ROOM ) THEN
               CALL CHKOUT ( 'GIPOOL' )
               RETURN
            END IF
 
         END IF
 
         NODE = DPPOOL(NEXT,NODE)
 
      END DO
 
      CALL CHKOUT ( 'GIPOOL' )
      RETURN
 
 
 
 
C$Procedure      DTPOOL (Data for a kernel pool variable)
 
      ENTRY DTPOOL ( NAME, FOUND, N, TYPE )
 
C$ Abstract
C
C     Return the data about a kernel pool variable.
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
C     KERNEL
C
C$ Keywords
C
C     CONSTANTS
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         NAME
C     LOGICAL               FOUND
C     INTEGER               N
C     CHARACTER*(*)         TYPE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of the variable whose value is to be returned.
C     FOUND      O   True if variable is in pool.
C     N          O   Number of values returned for NAME.
C     TYPE       O   Type of the variable 'C', 'N', 'X'
C
C$ Detailed_Input
C
C     NAME       is the name of the variable whose values are to be
C                returned.
C
C
C$ Detailed_Output
C
C
C     FOUND      is TRUE if the variable is in the pool FALSE if it
C                is not.
C
C     N          is the number of values associated with NAME.
C                If NAME is not present in the pool N will be returned
C                with the value 0.
C
C     TYPE       is the type of the variable associated with NAME.
C
C                    'C' if the data is character data
C                    'N' if the data is numeric.
C                    'X' if there is no variable NAME in the pool.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the name requested is not in the kernel pool FOUND
C        will be set to FALSE, N to zero and TYPE to 'X'.
C
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine allows you to determine whether or not a kernel
C     pool variable is present and to determine its size and type
C     if it is.
C
C
C$ Examples
C
C
C     The following code fragment demonstrates how to determine the
C     properties of a stored kernel variable.
C
C        CALL DTPOOL ( VARNAM, FOUND, N, TYPE )
C
C        IF ( FOUND ) THEN
C
C           WRITE (*,*) 'Properties of variable: ', VARNAME
C           WRITE (*,*)
C
C           WRITE (*,*) '   Size: ', N
C
C           IF ( TYPE .EQ. 'C' ) THEN
C              WRITE (*,*) '   Type: Character'
C           ELSE
C              WRITE (*,*) '   Type: Numeric'
C           END IF
C
C        ELSE
C
C           WRITE (*,*) VARNAM(1:RTRIM(VARNAM)), ' is not present.'
C
C        END IF
C
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
C     W.L. Taber  (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
C
C        ZZPINI call was updated for compatibility
C        with new watcher system implementation.
C
C-    SPICELIB Version 8.0.1, 22-DEC-2004 (NJB)
C
C        Corrected an in-line comment relating to finding the
C        head node of the conflict resolution list for NAME.
C
C-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
C
C        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
C        direct insertion of data into the kernel pool without having
C        to read an external file.
C
C        Added the interface LMPOOL that allows SPICE
C        programs to load text kernels directly from memory
C        instead of requiring a text file.
C
C        Added the entry point SZPOOL to return kernel pool definition
C        parameters.
C
C        Added the entry point DVPOOL to allow the removal of a variable
C        from the kernel pool.
C
C        Added the entry point GNPOOL to allow users to determine
C        variables that are present in the kernel pool
C
C-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
C
C        The implementation of the kernel pool was completely redone
C        to improve performance in loading and fetching data.  In
C        addition the pool was upgraded so that variables may be
C        either string or numeric valued.
C
C        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
C        to the routine.
C
C-&
 
C$ Index_Entries
C
C     RETURN summary information about a kernel pool variable
C
C-&
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DTPOOL' )
      END IF

C
C     Initialize the pool if necessary.
C
      IF ( FIRST ) THEN

         CALL ZZPINI (  FIRST,
     .                  MAXVAR, MAXVAL, MAXLIN,
     .                  BEGDAT, BEGTXT,
     .                  NMPOOL, DPPOOL, CHPOOL,
     .                  NAMLST, DATLST,
     .                  MAXAGT, MXNOTE, WTVARS, 
     .                  WTPTRS, WTPOOL, WTAGNT,
     .                  AGENTS, ACTIVE, NOTIFY, SUBCTR )

      END IF

C
C     Until we find otherwise, we shall assume there is no data
C     for this variable.
C
      FOUND = .FALSE.
      N     =  0
      TYPE  = 'X'
 
C
C     Compute the hash value of this name.
C
      LOOKAT = ZZHASH ( NAME )

C
C     Now see if there is a non-empty conflict resolution list for the
C     input string NAME.  If so, NAMLST(LOOKAT) contains the head node
C     of the conflict resolution list; this node is a positive value.
C
      IF ( NAMLST(LOOKAT) .EQ. 0 ) THEN
 
         CALL CHKOUT ( 'DTPOOL' )
         RETURN
 
      END IF
C
C     If were are still here NAMLST(LOOKAT) is the first node of
C     a conflict resolution list.  See if the NAME corresponding
C     to this node is the one we are looking for.
C
      NODE   = NAMLST( LOOKAT      )
      SUCCES = NAME .EQ. PNAMES(NODE)
 
      DO WHILE ( .NOT. SUCCES )
 
         NODE = NMPOOL ( NEXT, NODE )
 
         IF ( NODE .LT. 0 ) THEN
 
            CALL CHKOUT ( 'DTPOOL' )
            RETURN
 
         END IF
 
         SUCCES = NAME .EQ. PNAMES(NODE)
 
      END DO
C
C     If you get to this point, the variable NAME is present in the
C     list of names at PNAMES(NODE), ABS( DATLST(NODE) ) points to the
C     head of a linked list of values for this NAME.
C
 
      DATAHD = DATLST(NODE)
 
      IF ( DATAHD .LT. 0 ) THEN
 
         TYPE  = 'C'
         FOUND = .TRUE.
         NODE  = -DATAHD
 
         DO WHILE ( NODE .GT. 0 )
            N    = N + 1
            NODE = CHPOOL(NEXT,NODE)
         END DO
 
      ELSE IF ( DATAHD .GT. 0 ) THEN
 
         TYPE  = 'N'
         FOUND = .TRUE.
         NODE  =  DATAHD
 
         DO WHILE ( NODE .GT. 0 )
            N    = N + 1
            NODE = DPPOOL(NEXT,NODE)
         END DO
 
      ELSE IF ( DATAHD .EQ. 0 ) THEN
 
         CALL SETMSG ( 'This is never supposed to happen.  The '
     .   //            'requested name, ''#'', was found in the '
     .   //            'name list, but the pointer to the head '
     .   //            'of the data for this variable is zero. '
     .   //            'Please note your activities and report '
     .   //            'this error to NAIF. '   )
         CALL ERRCH  ( '#', NAME(1:RTRIM(NAME)) )
         CALL SIGERR ( 'SPICE(BUG)'             )
         CALL CHKOUT ( 'DTPOOL'                 )
 
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'DTPOOL' )
      RETURN
 


 
C$Procedure PCPOOL ( Put character strings into the kernel pool )

      ENTRY PCPOOL ( NAME, N, CVALS )

C$ Abstract
C
C     This entry point provides toolkit programmers a method for
C     programmatically inserting character data into the
C     kernel pool.
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
C      POOL
C
C$ Declarations
C
C     CHARACTER*(*)         NAME
C     INTEGER               N
C     CHARACTER*(*)         CVALS ( * )
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   The kernel pool name to associate with CVALS.
C     N          I   The number of values to insert.
C     CVALS      I   An array of strings to insert into the kernel pool.
C
C$ Detailed_Input
C
C     NAME       is the name of the kernel pool variable to associate
C                with the values supplied in the array CVALS
C
C     N          is the number of values to insert into the kernel pool.
C
C     CVALS      is an array of strings to insert into the kernel
C                pool.
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
C     1) If NAME is already present in the kernel pool and there
C        is sufficient room to hold all values supplied in CVALS,
C        the old values associated with NAME will be overwritten.
C
C     2) If there is not sufficient room to insert a new variable
C        into the kernel pool and NAME is not already present in
C        the kernel pool, the error SPICE(KERNELPOOLFULL) is
C        signaled by a routine in the call tree to this routine.
C
C     3) If there is not sufficient room to insert the values associated
C        with NAME, the error 'SPICE(NOMOREROOM)' will be signaled.
C
C     4) The error 'SPICE(BADVARNAME)' signals if the kernel pool 
C        variable name length exceeds MAXLEN.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This entry point provides a programmatic interface for inserting
C     character data into the SPICE kernel pool without reading an
C     external file.
C
C$ Examples
C
C     Suppose that you wish to supply default values for a program
C     so that it may function even in the absence of the appropriate
C     text kernels.  You can use the entry points PCPOOL, PDPOOL
C     and PIPOOL to initialize the kernel pool with suitable
C     values at program initialization.  The example below shows
C     how you might set up various kernel pool variables that might
C     be required by a program.
C
C
C        Set up the relationship between the EARTH_BODYFIXED frame
C        and the IAU_EARTH frame.
C
C        CALL IDENT  ( MATRIX )
C        CALL PCPOOL ( 'TKFRAME_EARTH_FIXED_SPEC',     1, 'MATRIX'    )
C        CALL PCPOOL ( 'TKFRAME_EARTH_FIXED_RELATIVE', 1, 'IAU_EARTH' )
C        CALL PDPOOL ( 'TKFRAME_EARTH_FIXED_MATRIX',   9,  MATRIX )
C
C
C        Load the IAU model for the earth's rotation and shape.
C
C
C        RA ( 1 ) =  0.0D0
C        RA ( 2 ) = -0.641D0
C        RA ( 3 ) =  0.0D0
C
C        DEC( 1 ) = 90.0D0
C        DEC( 2 ) = -0.557D0
C        DEC( 3 ) =  0.0D0
C
C        PM ( 1 ) = 190.16D0
C        PM ( 2 ) = 360.9856235D0
C        PM ( 3 ) =   0.0D0
C
C        R  ( 1 ) =  6378.140D0
C        R  ( 2 ) =  6378.140D0
C        R  ( 3 ) =  6356.75D0
C
C        CALL PDPOOL ( 'BODY399_POLE_RA',   3, RA  )
C        CALL PDPOOL ( 'BODY399_POLE_DEC',  3, DEC )
C        CALL PDPOOL ( 'BODY399_PM',        3, PM  )
C        CALL PDPOOL ( 'BODY399_RADII',     3, R   )
C
C
C        Set up a preliminary set of leapsecond values.
C
C        CALL PDPOOL ( 'DELTET/DELTA_T_A/',1, 32.184D0  )
C        CALL PDPOOL ( 'DELTET/K',         1,  1.657D-3 )
C        CALL PDPOOL ( 'DELTET/EB',        1,  1.671D-2 )
C
C        VALUES(1) = 6.23999600D0
C        VALUES(2) = 1.99096871D-7
C
C        CALL PDPOOL ( 'DELTET/M', 2, VALUES )
C
C
C        VALUES(  1 ) = 10
C        VALUES(  3 ) = 11
C        VALUES(  5 ) = 12
C        VALUES(  7 ) = 13
C        VALUES(  9 ) = 14
C        VALUES( 11 ) = 15
C        VALUES( 13 ) = 16
C        VALUES( 15 ) = 17
C        VALUES( 17 ) = 18
C        VALUES( 19 ) = 19
C        VALUES( 21 ) = 20
C        VALUES( 23 ) = 21
C        VALUES( 25 ) = 22
C        VALUES( 27 ) = 23
C        VALUES( 29 ) = 24
C        VALUES( 31 ) = 25
C        VALUES( 33 ) = 26
C        VALUES( 35 ) = 27
C        VALUES( 37 ) = 28
C        VALUES( 39 ) = 29
C        VALUES( 41 ) = 30
C        VALUES( 43 ) = 31
C
C        CALL TPARSE ( '1972-JAN-1', VALUES(2),  ERROR )
C        CALL TPARSE ( '1972-JUL-1', VALUES(4),  ERROR )
C        CALL TPARSE ( '1973-JAN-1', VALUES(6),  ERROR )
C        CALL TPARSE ( '1974-JAN-1', VALUES(8),  ERROR )
C        CALL TPARSE ( '1975-JAN-1', VALUES(10), ERROR )
C        CALL TPARSE ( '1976-JAN-1', VALUES(12), ERROR )
C        CALL TPARSE ( '1977-JAN-1', VALUES(14), ERROR )
C        CALL TPARSE ( '1978-JAN-1', VALUES(16), ERROR )
C        CALL TPARSE ( '1979-JAN-1', VALUES(18), ERROR )
C        CALL TPARSE ( '1980-JAN-1', VALUES(20), ERROR )
C        CALL TPARSE ( '1981-JUL-1', VALUES(22), ERROR )
C        CALL TPARSE ( '1982-JUL-1', VALUES(24), ERROR )
C        CALL TPARSE ( '1983-JUL-1', VALUES(26), ERROR )
C        CALL TPARSE ( '1985-JUL-1', VALUES(28), ERROR )
C        CALL TPARSE ( '1988-JAN-1', VALUES(30), ERROR )
C        CALL TPARSE ( '1990-JAN-1', VALUES(32), ERROR )
C        CALL TPARSE ( '1991-JAN-1', VALUES(34), ERROR )
C        CALL TPARSE ( '1992-JUL-1', VALUES(36), ERROR )
C        CALL TPARSE ( '1993-JUL-1', VALUES(38), ERROR )
C        CALL TPARSE ( '1994-JUL-1', VALUES(40), ERROR )
C        CALL TPARSE ( '1996-JAN-1', VALUES(42), ERROR )
C        CALL TPARSE ( '1997-JUL-1', VALUES(44), ERROR )
C
C        CALL PDPOOL ( 'DELTET/DELTA_AT',  44, VALUES )
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
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 9.1.0, 17-JAN-2014 (BVS) (NJB)
C
C        Updated to increment POOL state counter.
C        Updated Index_Entries section.
C
C-    SPICELIB Version 9.0.0, 24-MAY-2010 (EDW)
C
C        Added an error check on the length of the kernel pool variable
C        name argument to enforce the variable name length does not
C        exceed MAXLEN.
C
C-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
C
C        Watcher update code was re-written for compatibility
C        with new watcher system implementation.
C
C-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
C
C        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
C        direct insertion of data into the kernel pool without having
C        to read an external file.
C
C        Added the interface LMPOOL that allows SPICE
C        programs to load text kernels directly from memory instead
C        of requiring a text file.
C
C        Added the entry point SZPOOL to return kernel pool definition
C        parameters.
C
C        Added the entry point DVPOOL to allow the removal of a variable
C        from the kernel pool.
C
C        Added the entry point GNPOOL to allow users to determine
C        variables that are present in the kernel pool
C
C-&
 
C$ Index_Entries
C
C     Set the value of a character_variable in the kernel_pool
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( N .LE. 0 ) THEN
         RETURN
      END IF
 
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'PCPOOL' )


C
C     Check the variable name length; signal an error
C     if longer than MAXLEN.
C
      VARLEN = LEN( NAME(1: LASTNB(NAME) )  )

      IF ( VARLEN .GT. MAXLEN ) THEN
            
         CALL SETMSG ( 'The input kernel pool variable name '
     .   //            'exceeds the maximum allowed '
     .   //            'length of #1. The length of the variable '
     .   //            'name is #2, the offending variable name: '
     .   //            '''#3''.')

         CALL ERRINT ( '#1', MAXLEN                   )
         CALL ERRINT ( '#2', VARLEN                   )
         CALL ERRCH  ( '#3', NAME                     )
         CALL SIGERR ( 'SPICE(BADVARNAME)'            )
         CALL CHKOUT ( 'PCPOOL'                       )
         RETURN

      END IF

C
C     Initialize the pool if necessary.
C
      IF ( FIRST ) THEN

         CALL ZZPINI (  FIRST,
     .                  MAXVAR, MAXVAL, MAXLIN,
     .                  BEGDAT, BEGTXT,
     .                  NMPOOL, DPPOOL, CHPOOL,
     .                  NAMLST, DATLST,
     .                  MAXAGT, MXNOTE, WTVARS, 
     .                  WTPTRS, WTPOOL, WTAGNT,
     .                  AGENTS, ACTIVE, NOTIFY, SUBCTR )

      END IF

C
C     Increment POOL state counter.
C
      CALL ZZCTRINC ( SUBCTR ) 

C
C     Find out where the name for this item is located
C     in the data tables.
C
      CALL ZZGPNM ( NAMLST,
     .              NMPOOL, PNAMES, DATLST,
     .              DPPOOL, DPVALS,
     .              CHPOOL, CHVALS,
     .              NAME,   GOTIT,  LOOKAT, NAMEAT )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'PCPOOL' )
         RETURN
      END IF
C
C     Determine how much room is available for inserting new d.p.s
C     values into the kernel pool.
C
      AVAIL = LNKNFN(CHPOOL)
 
      IF ( GOTIT ) THEN
C
C        If we found the specified variable in the kernel pool, we
C        may be able to free up some space before inserting data.
C        We need to take this into account when determining
C        the amount of free room in the pool.
C
         DATAHD         = DATLST( NAMEAT )
 
         IF ( DATAHD .GT. 0 ) THEN
C
C           No extra strings will be freed.  We have whatever
C           free space is in the CHPOOL right now.
C
         ELSE
C
C           Find out how many items are in the current
C           list of strings associated with the variable.
C
            TOFREE =   0
            NODE   = - DATAHD
 
            DO WHILE ( NODE .GT. 0 )
               TOFREE = TOFREE + 1
               NODE   = CHPOOL(NEXT,NODE)
            END DO
C
C           Add the number we will free to the amount currently
C           free in the dp pool.
C
            AVAIL = AVAIL + TOFREE
 
         END IF
 
      END IF
 
C
C     If the AVAIL for new data is less than the number of items
C     to be added, we just bail out here.
C
      IF ( AVAIL .LT. N ) THEN
 
         IF ( .NOT. GOTIT ) THEN
C
C           We need to perform some clean up.  We've allocated
C           a new name but it has nothing in it. On the other hand
C           if we found it don't need to do anything because we've
C           only read from the pool. We haven't altered anything.
C           But in that case we'll never get into this block of code.
C
            CALL ZZCLN ( LOOKAT, NAMEAT,
     .                   NAMLST, DATLST, NMPOOL, CHPOOL, DPPOOL )
 
         END IF
 
 
         CALL SETMSG ( 'There is not sufficient space available '
     .   //            'in the kernel pool to store the # items '
     .   //            'associated with the name #.  There is '
     .   //            'room to store only # items. ' )
         CALL ERRINT ( '#', N  )
         CALL ERRCH  ( '#', NAME )
         CALL ERRINT ( '#', AVAIL   )
         CALL SIGERR ( 'SPICE(NOMOREROOM)' )
         CALL CHKOUT ( 'PCPOOL' )
         RETURN
 
      END IF
 
C
C     There is room to insert the data.  Free up any required
C     nodes.
C
      IF ( GOTIT ) THEN
 
C
C        We need to free the data associated with this
C        variable.  But first make sure there will be room
C        to add data.
C
         DATAHD         = DATLST( NAMEAT )
         DATLST(NAMEAT) = 0
 
         IF ( DATAHD .GT. 0 ) THEN
 
C
C           This variable was character type we need to
C           free a linked list from the character data
C           pool.
C
            HEAD =   DATAHD
            TAIL = - DPPOOL(PREV,HEAD)
 
            CALL LNKFSL ( HEAD, TAIL, DPPOOL )
 
         ELSE
 
C
C           This variable was character type. We need to
C           free a linked list from the numeric pool.
C
            HEAD = - DATAHD
            TAIL = - CHPOOL(PREV,HEAD)
 
            CALL LNKFSL ( HEAD, TAIL, CHPOOL )
 
         END IF
 
      END IF
C
C     We have done all of the freeing and checking that
C     needs to be done.  Now add the data.
C
      DO I = 1, N
 
C
C        We are ready to go.  Allocate a node for this data
C        item. First make sure there is room to do so.
C
         FREE = LNKNFN ( CHPOOL )
 
         IF ( FREE .LE. 0 ) THEN
 
            CALL SETMSG ( 'There is no room available for '
     .      //            'adding another character value '
     .      //            'to the kernel pool.' )
            CALL SIGERR ( 'SPICE(KERNELPOOLFULL)' )
            CALL CHKOUT ( 'PCPOOL' )
            RETURN
 
         END IF
 
C
C        Allocate a node for storing this string value:
C
         CALL LNKAN ( CHPOOL, CHNODE )
 
         IF ( DATLST(NAMEAT) .EQ. 0 ) THEN
 
C
C           There was no data for this name yet.  We make
C           CHNODE be the head of the data list for this name.
C
            DATLST(NAMEAT) = -CHNODE
 
         ELSE
 
C
C           Put this node after the tail of the current list.
C
            HEAD = -DATLST(NAMEAT)
            TAIL = -CHPOOL(PREV, HEAD )
 
            CALL LNKILA ( TAIL, CHNODE, CHPOOL )
 
         END IF
 
C
C        Finally insert this data item in the data buffer
C        at CHNODE.  Note any quotes will be doubled so we
C        have to undo this affect when we store the data.
C
         CHVALS(CHNODE) = CVALS(I)
 
C
C        That's all for this value. It's now time to loop
C        back through and get the next value.
C
 
      END DO
 
 
C
C     One last thing, see if this variable is being watched, 
C     If it is, add its associated agents to the list of
C     AGENTS to be notified of a watched variable update.
C
      IF (  ELEMC( NAME, WTVARS )  ) THEN
C
C        Union the update set AGENTS with the set of agents 
C        associated with the variable NAME.
C
         CALL ZZNWPOOL ( NAME,   WTVARS, WTPTRS, WTPOOL, 
     .                   WTAGNT, ACTIVE, NOTIFY, AGENTS )

      END IF

      CALL CHKOUT ( 'PCPOOL' )
      RETURN
 
 


C$Procedure PDPOOL ( Put d.p.'s into the kernel pool )

      ENTRY PDPOOL ( NAME, N, VALUES )

C$ Abstract
C
C     This entry point provides toolkit programmers a method for
C     programmatically inserting double precision data into the
C     kernel pool.
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
C      POOL
C
C$ Declarations
C
C     CHARACTER*(*)         NAME
C     INTEGER               N
C     DOUBLE PRECISION      VALUES ( * )
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   The kernel pool name to associate with VALUES.
C     N          I   The number of values to insert.
C     VALUES     I   An array of values to insert into the kernel pool.
C
C$ Detailed_Input
C
C     NAME       is the name of the kernel pool variable to associate
C                with the values supplied in the array VALUES
C
C     N          is the number of values to insert into the kernel pool.
C
C     VALUES     is an array of d.p. values to insert into the kernel
C                pool.
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
C     1) If NAME is already present in the kernel pool and there
C        is sufficient room to hold all values supplied in VALUES,
C        the old values associated with NAME will be overwritten.
C
C     2) If there is not sufficient room to insert a new variable
C        into the kernel pool and NAME is not already present in
C        the kernel pool, the error SPICE(KERNELPOOLFULL) is
C        signaled by a routine in the call tree to this routine.
C
C     3) If there is not sufficient room to insert the values associated
C        with NAME, the error 'SPICE(NOMOREROOM)' will be signaled.
C
C     4) The error 'SPICE(BADVARNAME)' signals if the kernel pool 
C        variable name length exceeds MAXLEN.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This entry point provides a programmatic interface for inserting
C     data into the SPICE kernel pool without reading an external file.
C
C$ Examples
C
C     Suppose that you wish to supply default values for a program
C     so that it may function even in the absence of the appropriate
C     text kernels.  You can use the entry points PCPOOL, PDPOOL
C     and PIPOOL to initialize the kernel pool with suitable
C     values at program initialization.  The example below shows
C     how you might set up various kernel pool variables that might
C     be required by a program.
C
C
C        Set up the relationship between the EARTH_BODYFIXED frame
C        and the IAU_EARTH frame.
C
C        CALL IDENT  ( MATRIX )
C        CALL PCPOOL ( 'TKFRAME_EARTH_FIXED_SPEC',     1, 'MATRIX'    )
C        CALL PCPOOL ( 'TKFRAME_EARTH_FIXED_RELATIVE', 1, 'IAU_EARTH' )
C        CALL PDPOOL ( 'TKFRAME_EARTH_FIXED_MATRIX',   9,  MATRIX )
C
C
C        Load the IAU model for the earth's rotation and shape.
C
C
C        RA ( 1 ) =  0.0D0
C        RA ( 2 ) = -0.641D0
C        RA ( 3 ) =  0.0D0
C
C        DEC( 1 ) = 90.0D0
C        DEC( 2 ) = -0.557D0
C        DEC( 3 ) =  0.0D0
C
C        PM ( 1 ) = 190.16D0
C        PM ( 2 ) = 360.9856235D0
C        PM ( 3 ) =   0.0D0
C
C        R  ( 1 ) =  6378.140D0
C        R  ( 2 ) =  6378.140D0
C        R  ( 3 ) =  6356.75D0
C
C        CALL PDPOOL ( 'BODY399_POLE_RA',   3, RA  )
C        CALL PDPOOL ( 'BODY399_POLE_DEC',  3, DEC )
C        CALL PDPOOL ( 'BODY399_PM',        3, PM  )
C        CALL PDPOOL ( 'BODY399_RADII',     3, R   )
C
C
C        Set up a preliminary set of leapsecond values.
C
C        CALL PDPOOL ( 'DELTET/DELTA_T_A', 1, 32.184D0  )
C        CALL PDPOOL ( 'DELTET/K',         1,  1.657D-3 )
C        CALL PDPOOL ( 'DELTET/EB',        1,  1.671D-2 )
C
C        VALUES(1) = 6.23999600D0
C        VALUES(2) = 1.99096871D-7
C
C        CALL PDPOOL ( 'DELTET/M', 2, VALUES )
C
C
C        VALUES(  1 ) = 10
C        VALUES(  3 ) = 11
C        VALUES(  5 ) = 12
C        VALUES(  7 ) = 13
C        VALUES(  9 ) = 14
C        VALUES( 11 ) = 15
C        VALUES( 13 ) = 16
C        VALUES( 15 ) = 17
C        VALUES( 17 ) = 18
C        VALUES( 19 ) = 19
C        VALUES( 21 ) = 20
C        VALUES( 23 ) = 21
C        VALUES( 25 ) = 22
C        VALUES( 27 ) = 23
C        VALUES( 29 ) = 24
C        VALUES( 31 ) = 25
C        VALUES( 33 ) = 26
C        VALUES( 35 ) = 27
C        VALUES( 37 ) = 28
C        VALUES( 39 ) = 29
C        VALUES( 41 ) = 30
C        VALUES( 43 ) = 31
C
C        CALL TPARSE ( '1972-JAN-1', VALUES(2),  ERROR )
C        CALL TPARSE ( '1972-JUL-1', VALUES(4),  ERROR )
C        CALL TPARSE ( '1973-JAN-1', VALUES(6),  ERROR )
C        CALL TPARSE ( '1974-JAN-1', VALUES(8),  ERROR )
C        CALL TPARSE ( '1975-JAN-1', VALUES(10), ERROR )
C        CALL TPARSE ( '1976-JAN-1', VALUES(12), ERROR )
C        CALL TPARSE ( '1977-JAN-1', VALUES(14), ERROR )
C        CALL TPARSE ( '1978-JAN-1', VALUES(16), ERROR )
C        CALL TPARSE ( '1979-JAN-1', VALUES(18), ERROR )
C        CALL TPARSE ( '1980-JAN-1', VALUES(20), ERROR )
C        CALL TPARSE ( '1981-JUL-1', VALUES(22), ERROR )
C        CALL TPARSE ( '1982-JUL-1', VALUES(24), ERROR )
C        CALL TPARSE ( '1983-JUL-1', VALUES(26), ERROR )
C        CALL TPARSE ( '1985-JUL-1', VALUES(28), ERROR )
C        CALL TPARSE ( '1988-JAN-1', VALUES(30), ERROR )
C        CALL TPARSE ( '1990-JAN-1', VALUES(32), ERROR )
C        CALL TPARSE ( '1991-JAN-1', VALUES(34), ERROR )
C        CALL TPARSE ( '1992-JUL-1', VALUES(36), ERROR )
C        CALL TPARSE ( '1993-JUL-1', VALUES(38), ERROR )
C        CALL TPARSE ( '1994-JUL-1', VALUES(40), ERROR )
C        CALL TPARSE ( '1996-JAN-1', VALUES(42), ERROR )
C        CALL TPARSE ( '1997-JUL-1', VALUES(44), ERROR )
C
C        CALL PDPOOL ( 'DELTET/DELTA_AT',  44, VALUES )
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
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 9.1.0, 17-JAN-2014 (BVS) (NJB)
C
C        Updated to increment POOL state counter.
C        Updated Index_Entries section.
C
C-    SPICELIB Version 9.0.0, 24-MAY-2010 (EDW)
C
C        Added an error check on the length of the kernel pool variable
C        name argument to enforce the variable name length does not
C        exceed MAXLEN.
C
C-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
C
C        Watcher update code was re-written for compatibility
C        with new watcher system implementation.
C
C-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
C
C        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
C        direct insertion of data into the kernel pool without having
C        to read an external file.
C
C        Added the interface LMPOOL that allows SPICE
C        programs to load text kernels directly from memory instead
C        of requiring a text file.
C
C        Added the entry point SZPOOL to return kernel pool definition
C        parameters.
C
C        Added the entry point DVPOOL to allow the removal of a variable
C        from the kernel pool.
C
C        Added the entry point GNPOOL to allow users to determine
C        variables that are present in the kernel pool
C
C-&
 
C$ Index_Entries
C
C     Set the value of a d.p._variable in the kernel_pool
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( N .LE. 0 ) THEN
         RETURN
      END IF
 
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'PDPOOL' )


C
C     Check the variable name length; signal an error
C     if longer than MAXLEN.
C
      VARLEN = LEN( NAME(1: LASTNB(NAME) )  )

      IF ( VARLEN .GT. MAXLEN ) THEN
            
         CALL SETMSG ( 'The input kernel pool variable name '
     .   //            'exceeds the maximum allowed '
     .   //            'length of #1. The length of the variable '
     .   //            'name is #2, the offending variable name: '
     .   //            '''#3''.')

         CALL ERRINT ( '#1', MAXLEN                   )
         CALL ERRINT ( '#2', VARLEN                   )
         CALL ERRCH  ( '#3', NAME                     )
         CALL SIGERR ( 'SPICE(BADVARNAME)'            )
         CALL CHKOUT ( 'PDPOOL'                       )
         RETURN

      END IF
 
C
C     Initialize the pool if necessary.
C
      IF ( FIRST ) THEN

         CALL ZZPINI (  FIRST,
     .                  MAXVAR, MAXVAL, MAXLIN,
     .                  BEGDAT, BEGTXT,
     .                  NMPOOL, DPPOOL, CHPOOL,
     .                  NAMLST, DATLST,
     .                  MAXAGT, MXNOTE, WTVARS, 
     .                  WTPTRS, WTPOOL, WTAGNT,
     .                  AGENTS, ACTIVE, NOTIFY, SUBCTR )

      END IF

C
C     Increment POOL state counter.
C
      CALL ZZCTRINC ( SUBCTR ) 

C
C     Find out where the name for this item is located
C     in the data tables.
C
      CALL ZZGPNM ( NAMLST,
     .              NMPOOL, PNAMES, DATLST,
     .              DPPOOL, DPVALS,
     .              CHPOOL, CHVALS,
     .              NAME,   GOTIT,  LOOKAT, NAMEAT )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'PDPOOL' )
         RETURN
      END IF
C
C     Determine how much room is available for inserting new d.p.s
C     values into the kernel pool.
C
      AVAIL = LNKNFN(DPPOOL)
 
      IF ( GOTIT ) THEN
C
C        If we found the specified variable in the kernel pool, we
C        may be able to free up some space before inserting data.
C        We need to take this into account when determining
C        the amount of free room in the pool.
C
         DATAHD         = DATLST( NAMEAT )
 
         IF ( DATAHD .LT. 0 ) THEN
C
C           No extra d.p.s will be freed.  We have whatever
C           free space is in the DPPOOL right now.
C
         ELSE
C
C           Find out how many items are in the current
C           list of d.p. associated with the variable.
C
            TOFREE = 0
            NODE   =  DATAHD
 
            DO WHILE ( NODE .GT. 0 )
               TOFREE = TOFREE + 1
               NODE   = DPPOOL(NEXT,NODE)
            END DO
C
C           Add the number we will free to the amount currently
C           free in the dp pool.
C
            AVAIL = AVAIL + TOFREE
 
         END IF
 
      END IF
 
C
C     If the AVAIL for new data is less than the number of items
C     to be added, we just bail out here.
C
      IF ( AVAIL .LT. N ) THEN
 
         IF ( .NOT. GOTIT ) THEN
C
C           We need to perform some clean up.  We've allocated
C           a new name but it has nothing in it. On the other hand
C           if we found it don't need to do anything because we've
C           only read from the pool. We haven't altered anything.
C           But in that case we'll never get into this block of code.
C
            CALL ZZCLN ( LOOKAT, NAMEAT,
     .                   NAMLST, DATLST, NMPOOL, CHPOOL, DPPOOL )
 
         END IF
 
 
         CALL SETMSG ( 'There is not sufficient space available '
     .   //            'in the kernel pool to store the # items '
     .   //            'associated with the name #.  There is '
     .   //            'room to store only # items. ' )
         CALL ERRINT ( '#', N  )
         CALL ERRCH  ( '#', NAME )
         CALL ERRINT ( '#', AVAIL   )
         CALL SIGERR ( 'SPICE(NOMOREROOM)' )
         CALL CHKOUT ( 'PDPOOL' )
         RETURN
 
      END IF
 
C
C     There is room to insert the data.  Free up any required
C     nodes.
C
      IF ( GOTIT ) THEN
 
C
C        We need to free the data associated with this
C        variable.  But first make sure there will be room
C        to add data.
C
         DATAHD         = DATLST( NAMEAT )
         DATLST(NAMEAT) = 0
 
         IF ( DATAHD .LT. 0 ) THEN
 
C
C           This variable was character type we need to
C           free a linked list from the character data
C           pool.
C
            HEAD = - DATAHD
            TAIL = - CHPOOL(PREV,HEAD)
 
            CALL LNKFSL ( HEAD, TAIL, CHPOOL )
 
         ELSE
 
C
C           This variable was numeric type. We need to
C           free a linked list from the numeric pool.
C
            HEAD =   DATAHD
            TAIL = - DPPOOL(PREV,HEAD)
 
            CALL LNKFSL ( HEAD, TAIL, DPPOOL )
 
         END IF
 
      END IF
C
C     We have done all of the freeing and checking that
C     needs to be done.  Now add the data.
C
      DO I = 1, N
 
C
C        OK. See if there is room in
C        the numeric portion of the pool to store this value.
C
         FREE = LNKNFN ( DPPOOL )
 
         IF ( FREE .LE. 0 ) THEN
C
C           This branch of the code should never be exercised,
C           but it doesn't hurt to program in a redundant check.
C
            CALL ZZCLN ( LOOKAT, NAMEAT,
     .                   NAMLST, DATLST, NMPOOL, CHPOOL, DPPOOL )
 
            CALL SETMSG ( 'There is no room available for '
     .      //            'adding another numeric value '
     .      //            'to the kernel pool.' )
            CALL SIGERR ( 'SPICE(KERNELPOOLFULL)' )
            CALL CHKOUT ( 'PDPOOL' )
            RETURN
 
         END IF
 
C
C        Allocate a node for storing this numeric value:
C
         CALL LNKAN ( DPPOOL, DPNODE )
 
         IF ( DATLST(NAMEAT) .EQ. 0 ) THEN
 
C
C           There was no data for this name yet.  We make
C           DPNODE be the head of the data list for this name.
C
            DATLST(NAMEAT) =  DPNODE
 
         ELSE
 
C
C           Put this node after the tail of the current list.
C
            HEAD =  DATLST(NAMEAT)
            TAIL = -DPPOOL(PREV, HEAD )
 
            CALL LNKILA ( TAIL, DPNODE, DPPOOL )
 
         END IF
 
C
C        Finally insert this data item into the numeric buffer.
C
         DPVALS(DPNODE) = VALUES(I)
 
      END DO
 
 
C
C     One last thing, see if this variable is being watched, 
C     If it is, add its associated agents to the list of
C     AGENTS to be notified of a watched variable update.
C
      IF (  ELEMC( NAME, WTVARS )  ) THEN
C
C        Union the update set AGENTS with the set of agents 
C        associated with the variable NAME.
C
         CALL ZZNWPOOL ( NAME,   WTVARS, WTPTRS, WTPOOL, 
     .                   WTAGNT, ACTIVE, NOTIFY, AGENTS )
      END IF

 
      CALL CHKOUT ( 'PDPOOL' )
      RETURN
 


 
C$Procedure PIPOOL ( Put integers into the kernel pool )

      ENTRY PIPOOL ( NAME, N, IVALS )

C$ Abstract
C
C     This entry point provides toolkit programmers a method for
C     programmatically inserting integer data into the kernel pool.
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
C      POOL
C
C$ Declarations
C
C     CHARACTER*(*)         NAME
C     INTEGER               N
C     INTEGER               IVALS ( * )
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   The kernel pool name to associate with IVALS.
C     N          I   The number of values to insert.
C     IVALS      I   An array of integers to insert into the pool.
C
C$ Detailed_Input
C
C     NAME       is the name of the kernel pool variable to associate
C                with the values supplied in the array IVALS
C
C     N          is the number of values to insert into the kernel pool.
C
C     IVALS      is an array of integers to insert into the kernel
C                pool.
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
C     1) If NAME is already present in the kernel pool and there
C        is sufficient room to hold all values supplied in IVALS,
C        the old values associated with NAME will be overwritten.
C
C     2) If there is not sufficient room to insert a new variable
C        into the kernel pool and NAME is not already present in
C        the kernel pool, the error SPICE(KERNELPOOLFULL) is
C        signaled by a routine in the call tree to this routine.
C
C     3) If there is not sufficient room to insert the values associated
C        with NAME, the error 'SPICE(NOMOREROOM)' will be signaled.
C
C     4) The error 'SPICE(BADVARNAME)' signals if the kernel pool 
C        variable name length exceeds MAXLEN.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This entry point provides a programmatic interface for inserting
C     data into the SPICE kernel pool without reading an external file.
C
C$ Examples
C
C     Suppose that you wish to supply default values for a program
C     so that it may function even in the absence of the appropriate
C     text kernels.  You can use the entry points PCPOOL, PDPOOL
C     and PIPOOL to initialize the kernel pool with suitable
C     values at program initialization.  The example below shows
C     how you might set up various kernel pool variables that might
C     be required by a program.
C
C
C        Set up the relationship between the EARTH_BODYFIXED frame
C        and the IAU_EARTH frame.
C
C        CALL IDENT ( MATRIX )
C        CALL PCPOOL ( 'TKFRAME_EARTH_FIXED_SPEC',     1, 'MATRIX' )
C        CALL PIPOOL ( 'TKFRAME_EARTH_FIXED_RELATIVE', 1,  10081   )
C        CALL PDPOOL ( 'TKFRAME_EARTH_FIXED_MATRIX',   9,  MATRIX  )
C
C
C        Load the IAU model for the earth's rotation and shape.
C
C
C        RA ( 1 ) =  0.0D0
C        RA ( 2 ) = -0.641D0
C        RA ( 3 ) =  0.0D0
C
C        DEC( 1 ) = 90.0D0
C        DEC( 2 ) = -0.557D0
C        DEC( 3 ) =  0.0D0
C
C        PM ( 1 ) = 190.16D0
C        PM ( 2 ) = 360.9856235D0
C        PM ( 3 ) =   0.0D0
C
C        R  ( 1 ) =  6378.140D0
C        R  ( 2 ) =  6378.140D0
C        R  ( 3 ) =  6356.75D0
C
C        CALL PDPOOL ( 'BODY399_POLE_RA',   3, RA  )
C        CALL PDPOOL ( 'BODY399_POLE_DEC',  3, DEC )
C        CALL PDPOOL ( 'BODY399_PM',        3, PM  )
C        CALL PDPOOL ( 'BODY399_RADII',     3, R   )
C
C
C        Set up a preliminary set of leapsecond values.
C
C        CALL PDPOOL ( 'DELTET/DELTA_T_A/',1, 32.184D0  )
C        CALL PDPOOL ( 'DELTET/K',         1,  1.657D-3 )
C        CALL PDPOOL ( 'DELTET/EB',        1,  1.671D-2 )
C
C        VALUES(1) = 6.23999600D0
C        VALUES(2) = 1.99096871D-7
C
C        CALL PDPOOL ( 'DELTET/M', 2, VALUES )
C
C
C        VALUES(  1 ) = 10
C        VALUES(  3 ) = 11
C        VALUES(  5 ) = 12
C        VALUES(  7 ) = 13
C        VALUES(  9 ) = 14
C        VALUES( 11 ) = 15
C        VALUES( 13 ) = 16
C        VALUES( 15 ) = 17
C        VALUES( 17 ) = 18
C        VALUES( 19 ) = 19
C        VALUES( 21 ) = 20
C        VALUES( 23 ) = 21
C        VALUES( 25 ) = 22
C        VALUES( 27 ) = 23
C        VALUES( 29 ) = 24
C        VALUES( 31 ) = 25
C        VALUES( 33 ) = 26
C        VALUES( 35 ) = 27
C        VALUES( 37 ) = 28
C        VALUES( 39 ) = 29
C        VALUES( 41 ) = 30
C        VALUES( 43 ) = 31
C
C        CALL TPARSE ( '1972-JAN-1', VALUES(2),  ERROR )
C        CALL TPARSE ( '1972-JUL-1', VALUES(4),  ERROR )
C        CALL TPARSE ( '1973-JAN-1', VALUES(6),  ERROR )
C        CALL TPARSE ( '1974-JAN-1', VALUES(8),  ERROR )
C        CALL TPARSE ( '1975-JAN-1', VALUES(10), ERROR )
C        CALL TPARSE ( '1976-JAN-1', VALUES(12), ERROR )
C        CALL TPARSE ( '1977-JAN-1', VALUES(14), ERROR )
C        CALL TPARSE ( '1978-JAN-1', VALUES(16), ERROR )
C        CALL TPARSE ( '1979-JAN-1', VALUES(18), ERROR )
C        CALL TPARSE ( '1980-JAN-1', VALUES(20), ERROR )
C        CALL TPARSE ( '1981-JUL-1', VALUES(22), ERROR )
C        CALL TPARSE ( '1982-JUL-1', VALUES(24), ERROR )
C        CALL TPARSE ( '1983-JUL-1', VALUES(26), ERROR )
C        CALL TPARSE ( '1985-JUL-1', VALUES(28), ERROR )
C        CALL TPARSE ( '1988-JAN-1', VALUES(30), ERROR )
C        CALL TPARSE ( '1990-JAN-1', VALUES(32), ERROR )
C        CALL TPARSE ( '1991-JAN-1', VALUES(34), ERROR )
C        CALL TPARSE ( '1992-JUL-1', VALUES(36), ERROR )
C        CALL TPARSE ( '1993-JUL-1', VALUES(38), ERROR )
C        CALL TPARSE ( '1994-JUL-1', VALUES(40), ERROR )
C        CALL TPARSE ( '1996-JAN-1', VALUES(42), ERROR )
C        CALL TPARSE ( '1997-JUL-1', VALUES(44), ERROR )
C
C        CALL PDPOOL ( 'DELTET/DELTA_AT',  44, VALUES )
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
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 9.1.0, 17-JAN-2014 (BVS) (NJB)
C
C        Updated to increment POOL state counter.
C        Updated Index_Entries section.
C
C-    SPICELIB Version 9.0.0, 24-MAY-2010 (EDW)
C
C        Added an error check on the length of the kernel pool variable
C        name argument to enforce the variable name length does not
C        exceed MAXLEN.
C
C-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
C
C        Watcher update code was re-written for compatibility
C        with new watcher system implementation.
C
C-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
C
C        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
C        direct insertion of data into the kernel pool without having
C        to read an external file.
C
C        Added the interface LMPOOL that allows SPICE
C        programs to load text kernels directly from memory instead
C        of requiring a text file.
C
C        Added the entry point SZPOOL to return kernel pool definition
C        parameters.
C
C        Added the entry point DVPOOL to allow the removal of a variable
C        from the kernel pool.
C
C        Added the entry point GNPOOL to allow users to determine
C        variables that are present in the kernel pool
C
C-&
 
C$ Index_Entries
C
C     Set the value of an integer_variable in the kernel_pool
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( N .LE. 0 ) THEN
         RETURN
      END IF
 
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'PIPOOL' )


C
C     Check the variable name length; signal an error
C     if longer than MAXLEN.
C
      VARLEN = LEN( NAME(1: LASTNB(NAME) )  )

      IF ( VARLEN .GT. MAXLEN ) THEN
            
         CALL SETMSG ( 'The input kernel pool variable name '
     .   //            'exceeds the maximum allowed '
     .   //            'length of #1. The length of the variable '
     .   //            'name is #2, the offending variable name: '
     .   //            '''#3''.')

         CALL ERRINT ( '#1', MAXLEN                   )
         CALL ERRINT ( '#2', VARLEN                   )
         CALL ERRCH  ( '#3', NAME                     )
         CALL SIGERR ( 'SPICE(BADVARNAME)'            )
         CALL CHKOUT ( 'PIPOOL'                       )
         RETURN

      END IF
 
C
C     Initialize the pool if necessary.
C
      IF ( FIRST ) THEN

         CALL ZZPINI (  FIRST,
     .                  MAXVAR, MAXVAL, MAXLIN,
     .                  BEGDAT, BEGTXT,
     .                  NMPOOL, DPPOOL, CHPOOL,
     .                  NAMLST, DATLST,
     .                  MAXAGT, MXNOTE, WTVARS, 
     .                  WTPTRS, WTPOOL, WTAGNT,
     .                  AGENTS, ACTIVE, NOTIFY, SUBCTR )

      END IF

C
C     Increment POOL state counter.
C
      CALL ZZCTRINC ( SUBCTR ) 

C
C     Find out where the name for this item is located
C     in the data tables.
C
      CALL ZZGPNM ( NAMLST,
     .              NMPOOL, PNAMES, DATLST,
     .              DPPOOL, DPVALS,
     .              CHPOOL, CHVALS,
     .              NAME, GOTIT,  LOOKAT, NAMEAT )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'PIPOOL' )
         RETURN
      END IF
C
C     Determine how much room is available for inserting new d.p.s
C     values into the kernel pool.
C
      AVAIL = LNKNFN(DPPOOL)
 
      IF ( GOTIT ) THEN
C
C        If we found the specified variable in the kernel pool, we
C        may be able to free up some space before inserting data.
C        We need to take this into account when determining
C        the amount of free room in the pool.
C
         DATAHD         = DATLST( NAMEAT )
 
         IF ( DATAHD .LT. 0 ) THEN
C
C           No extra d.p.s will be freed.  We have whatever
C           free space is in the DPPOOL right now.
C
         ELSE
C
C           Find out how many items are in the current
C           list of d.p. associated with the variable.
C
            TOFREE = 0
            NODE   =  DATAHD
 
            DO WHILE ( NODE .GT. 0 )
               TOFREE = TOFREE + 1
               NODE   = DPPOOL(NEXT,NODE)
            END DO
C
C           Add the number we will free to the amount currently
C           free in the dp pool.
C
            AVAIL = AVAIL + TOFREE
 
         END IF
 
      END IF
 
C
C     If the AVAIL for new data is less than the number of items
C     to be added, we just bail out here.
C
      IF ( AVAIL .LT. N ) THEN
 
         IF ( .NOT. GOTIT ) THEN
C
C           We need to perform some clean up.  We've allocated
C           a new name but it has nothing in it. On the other hand
C           if we found it don't need to do anything because we've
C           only read from the pool. We haven't altered anything.
C           But in that case we'll never get into this block of code.
C
            CALL ZZCLN ( LOOKAT, NAMEAT,
     .                   NAMLST, DATLST, NMPOOL, CHPOOL, DPPOOL )
 
         END IF
 
         CALL SETMSG ( 'There is not sufficient space available '
     .   //            'in the kernel pool to store the # items '
     .   //            'associated with the name #.  There is '
     .   //            'room to store only # items. ' )
         CALL ERRINT ( '#', N  )
         CALL ERRCH  ( '#', NAME )
         CALL ERRINT ( '#', AVAIL   )
         CALL SIGERR ( 'SPICE(NOMOREROOM)' )
         CALL CHKOUT ( 'PIPOOL' )
         RETURN
 
      END IF
 
C
C     There is room to insert the data.  Free up any required
C     nodes.
C
      IF ( GOTIT ) THEN
 
C
C        We need to free the data associated with this
C        variable.  But first make sure there will be room
C        to add data.
C
         DATAHD         = DATLST( NAMEAT )
         DATLST(NAMEAT) = 0
 
         IF ( DATAHD .LT. 0 ) THEN
 
C
C           This variable was character type we need to
C           free a linked list from the character data
C           pool.
C
            HEAD = - DATAHD
            TAIL = - CHPOOL(PREV,HEAD)
 
            CALL LNKFSL ( HEAD, TAIL, CHPOOL )
 
         ELSE
 
C
C           This variable was numeric type. We need to
C           free a linked list from the numeric pool.
C
            HEAD =   DATAHD
            TAIL = - DPPOOL(PREV,HEAD)
 
            CALL LNKFSL ( HEAD, TAIL, DPPOOL )
 
         END IF
 
      END IF
C
C     We have done all of the freeing and checking that
C     needs to be done.  Now add the data.
C
      DO I = 1, N
 
C
C        OK. See if there is room in
C        the numeric portion of the pool to store this value.
C
         FREE = LNKNFN ( DPPOOL )
 
         IF ( FREE .LE. 0 ) THEN
C
C           This branch of the code should never be exercised,
C           but it doesn't hurt to program in a redundant check.
C
            CALL ZZCLN ( LOOKAT, NAMEAT,
     .                   NAMLST, DATLST, NMPOOL, CHPOOL, DPPOOL )
 
            CALL SETMSG ( 'There is no room available for '
     .      //            'adding another numeric value '
     .      //            'to the kernel pool.' )
            CALL SIGERR ( 'SPICE(KERNELPOOLFULL)' )
            CALL CHKOUT ( 'PIPOOL' )
            RETURN
 
         END IF
 
C
C        Allocate a node for storing this numeric value:
C
         CALL LNKAN ( DPPOOL, DPNODE )
 
         IF ( DATLST(NAMEAT) .EQ. 0 ) THEN
 
C
C           There was no data for this name yet.  We make
C           DPNODE be the head of the data list for this name.
C
            DATLST(NAMEAT) =  DPNODE
 
         ELSE
 
C
C           Put this node after the tail of the current list.
C
            HEAD =  DATLST(NAMEAT)
            TAIL = -DPPOOL(PREV, HEAD )
 
            CALL LNKILA ( TAIL, DPNODE, DPPOOL )
 
         END IF
 
C
C        Finally insert this data item into the numeric buffer.
C
         DPVALS(DPNODE) = DBLE( IVALS(I) )
 
      END DO
 
 
C
C     One last thing, see if this variable is being watched, 
C     If it is, add its associated agents to the list of
C     AGENTS to be notified of a watched variable update.
C
      IF (  ELEMC( NAME, WTVARS )  ) THEN
C
C        Union the update set AGENTS with the set of agents 
C        associated with the variable NAME.
C
         CALL ZZNWPOOL ( NAME,   WTVARS, WTPTRS, WTPOOL, 
     .                   WTAGNT, ACTIVE, NOTIFY, AGENTS )

      END IF

 
      CALL CHKOUT ( 'PIPOOL' )
      RETURN

 
 
 
 
C$Procedure LMPOOL ( Load variables from memory into the pool )
 
      ENTRY LMPOOL ( CVALS, N )
 
C$ Abstract
C
C     Load the variables contained in an internal buffer into the
C     kernel pool.
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
C     KERNEL
C
C$ Keywords
C
C     CONSTANTS
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         CVALS ( * )
C     INTEGER               N
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     CVALS      I   An array that contains a SPICE text kernel
C     N          I   The number of entries in CVALS.
C
C$ Detailed_Input
C
C     CVALS      is an array that contains lines of text that
C                could serve as a SPICE text kernel.
C
C     N          the number of entries in CVALS.
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
C     1) All exceptions are diagnosed by routines called by the
C        private routine ZZRVBF.
C
C     2) The error 'SPICE(BADVARNAME)' signals from a routine in the
C        call tree of LMPOOL if a kernel pool variable name length
C        exceeds MAXLEN characters (defined in pool.f).
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine allows you to store a text kernel in an internal
C     array of your program and load this array into the kernel pool
C     without first storing its contents as a text kernel.
C
C$ Examples
C
C     Suppose that your application is not particularly sensitive
C     to the current number of leapseconds but that you would
C     still like to use a relatively recent leapseconds kernel
C     without requiring users to load a leapseconds kernel into
C     the program.  The example below shows how you might set up
C     the initialization portion of your program.
C
C        INTEGER               LNSIZE
C        PARAMETER           ( LNSIZE = 80 )
C
C        CHARACTER*(LNSIZE)    TEXT ( 27 )
C
C        TEXT(  1 ) = 'DELTET/DELTA_T_A =   32.184'
C        TEXT(  2 ) = 'DELTET/K         =    1.657D-3'
C        TEXT(  3 ) = 'DELTET/EB        =    1.671D-2'
C        TEXT(  4 ) = 'DELTET/M = (  6.239996D0   1.99096871D-7 )'
C        TEXT(  5 ) = 'DELTET/DELTA_AT  = ( 10,   @1972-JAN-1'
C        TEXT(  6 ) = '                     11,   @1972-JUL-1'
C        TEXT(  7 ) = '                     12,   @1973-JAN-1'
C        TEXT(  8 ) = '                     13,   @1974-JAN-1'
C        TEXT(  9 ) = '                     14,   @1975-JAN-1'
C        TEXT( 10 ) = '                     15,   @1976-JAN-1'
C        TEXT( 11 ) = '                     16,   @1977-JAN-1'
C        TEXT( 12 ) = '                     17,   @1978-JAN-1'
C        TEXT( 13 ) = '                     18,   @1979-JAN-1'
C        TEXT( 14 ) = '                     19,   @1980-JAN-1'
C        TEXT( 15 ) = '                     20,   @1981-JUL-1'
C        TEXT( 16 ) = '                     21,   @1982-JUL-1'
C        TEXT( 17 ) = '                     22,   @1983-JUL-1'
C        TEXT( 18 ) = '                     23,   @1985-JUL-1'
C        TEXT( 19 ) = '                     24,   @1988-JAN-1'
C        TEXT( 20 ) = '                     25,   @1990-JAN-1'
C        TEXT( 21 ) = '                     26,   @1991-JAN-1'
C        TEXT( 22 ) = '                     27,   @1992-JUL-1'
C        TEXT( 23 ) = '                     28,   @1993-JUL-1'
C        TEXT( 24 ) = '                     29,   @1994-JUL-1'
C        TEXT( 25 ) = '                     30,   @1996-JAN-1'
C        TEXT( 26 ) = '                     31,   @1997-JUL-1'
C        TEXT( 27 ) = '                     32,   @1999-JAN-1 )'
C
C        CALL LMPOOL ( TEXT, 27 )
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
C     R.E. Thurman    (JPL)
C     I.M. Underwood  (JPL)
C     W.L. Taber      (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.3.0, 30-JUL-2013 (BVS)
C
C        Updated to increment POOL state counter.
C
C-    SPICELIB Version 8.2.0, 10-FEB-2010 (EDW)
C
C        Added mention of the restriction on kernel pool variable 
C        names to MAXLEN characters or less.
C
C-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
C
C        Watcher update code was re-written for compatibility
C        with new watcher system implementation.
C
C-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
C
C        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
C        direct insertion of data into the kernel pool without having
C        to read an external file.
C
C        Added the interface LMPOOL that allows SPICE
C        programs to load text kernels directly from memory
C        instead of requiring a text file.
C
C        Added the entry point SZPOOL to return kernel pool definition
C        parameters.
C
C        Added the entry point DVPOOL to allow the removal of a variable
C        from the kernel pool.
C
C        Added the entry point GNPOOL to allow users to determine
C        variables that are present in the kernel pool
C
C-&
 
C$ Index_Entries
C
C     Load the kernel pool from an internal text buffer
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LMPOOL' )
      END IF
 
C
C     Initialize the pool if necessary.
C
      IF ( FIRST ) THEN

         CALL ZZPINI (  FIRST,
     .                  MAXVAR, MAXVAL, MAXLIN,
     .                  BEGDAT, BEGTXT,
     .                  NMPOOL, DPPOOL, CHPOOL,
     .                  NAMLST, DATLST,
     .                  MAXAGT, MXNOTE, WTVARS, 
     .                  WTPTRS, WTPOOL, WTAGNT,
     .                  AGENTS, ACTIVE, NOTIFY, SUBCTR )

      END IF
 
C
C     Increment POOL state counter.
C
      CALL ZZCTRINC ( SUBCTR ) 

C
C     Read from the internal SPICE pool buffer
C
      LINNUM = 1
 
      CALL ZZRVBF ( CVALS,  N,      LINNUM,  NAMLST,
     .              NMPOOL, PNAMES, DATLST,
     .              DPPOOL, DPVALS,
     .              CHPOOL, CHVALS,
     .              VARNAM, EOF )


C
C     Read the variables in the file, one at a time.
C
      DO WHILE ( .NOT. EOF .AND. .NOT. FAILED() )
 
         IF ( VARNAM .NE. ' ' ) THEN

            IF (  ELEMC( VARNAM, WTVARS )  ) THEN
C
C              The variable VARNAM is watched.
C
C              Union the update set AGENTS with the set of agents
C              associated with the variable VARNAM.
C
               CALL ZZNWPOOL ( VARNAM, WTVARS, WTPTRS, WTPOOL, 
     .                         WTAGNT, ACTIVE, NOTIFY, AGENTS )

            END IF

         END IF
C
C        We've processed VARNAM if it was non-blank.
C 
         CALL ZZRVBF ( CVALS,  N,      LINNUM,  NAMLST,
     .                 NMPOOL, PNAMES, DATLST,
     .                 DPPOOL, DPVALS,
     .                 CHPOOL, CHVALS,
     .                 VARNAM, EOF )
 
      END DO
 
C
C     That's it, the buffer supplied has been completely parsed
C     and placed into the kernel pool.
C
      CALL CHKOUT  ( 'LMPOOL' )
      RETURN
 



C$Procedure SZPOOL (Get size limitations of the kernel pool)
 
      ENTRY SZPOOL ( NAME, N, FOUND )
 
C$ Abstract
C
C     Return the kernel pool size limitations.
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
C     KERNEL
C
C$ Keywords
C
C     CONSTANTS
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         NAME
C     INTEGER               N
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of the parameter to be returned.
C     N          O   Value of parameter specified by NAME.
C     FOUND      O   .TRUE. if NAME is recognized.
C
C$ Detailed_Input
C
C     NAME       is the name of a kernel pool size parameter.
C                The following parameters may be specified.
C
C                   'MAXVAR'
C                   'MAXVAL'
C                   'MAXLIN'
C                   'MAXCHR'
C                   'MXNOTE'
C                   'MAXLEN'
C                   'MAXAGT'
C
C                See the main entry point for a description of the
C                meaning of these parameters.  Note that the case
C                of NAME is insignificant.
C
C$ Detailed_Output
C
C     N          is the value of the parameter specified by NAME. If
C                NAME is not one of the items specified above, N will
C                be returned with the value 0.
C
C     FOUND      is TRUE if the parameter is recognized FALSE if it
C                is not.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the specified parameter is not recognized the value of N
C        returned will be zero and FOUND will be set to FALSE.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine provides the a programmatic interface to the
C     parameters used to define the kernel pool.  It is not
C     anticipated that most kernel pool users will need to use this
C     routine.
C
C$ Examples
C
C     None.
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
C     W.L. Taber  (JPL)
C     H.W. Taylor (ACT)
C
C$ Version
C
C-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)(HWT)
C
C        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
C        direct insertion of data into the kernel pool without having
C        to read an external file.
C
C        Added the interface LMPOOL that allows SPICE
C        programs to load text kernels directly from memory
C        instead of requiring a text file.
C
C        Added the entry point SZPOOL to return kernel pool definition
C        parameters.
C
C        Added the entry point DVPOOL to allow the removal of a variable
C        from the kernel pool.
C
C        Added the entry point GNPOOL to allow users to determine
C        variables that are present in the kernel pool
C
C-&
 
C$ Index_Entries
C
C     return a kernel pool definition parameter
C
C-&
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'SZPOOL')
 
      FOUND = .TRUE.
 
      IF ( EQSTR( NAME, 'MAXVAR' ) ) THEN
         N = MAXVAR
      ELSE IF ( EQSTR( NAME, 'MAXVAL' ) ) THEN
         N = MAXVAL
      ELSE IF ( EQSTR( NAME, 'MAXLIN' ) ) THEN
         N = MAXLIN
      ELSE IF ( EQSTR( NAME, 'MAXCHR' ) ) THEN
         N = MAXCHR
      ELSE IF ( EQSTR( NAME, 'MXNOTE' ) ) THEN
         N = MXNOTE
      ELSE IF ( EQSTR( NAME, 'MAXLEN' ) ) THEN
         N = MAXLEN
      ELSE IF ( EQSTR( NAME, 'MAXAGT' ) ) THEN
         N = MAXAGT
      ELSE
         N     = 0
         FOUND = .FALSE.
      END IF
 
      CALL CHKOUT ( 'SZPOOL' )
      RETURN
 
 
 
 
C$Procedure DVPOOL ( Delete a variable from the kernel pool )
 
      ENTRY DVPOOL ( NAME )
 
C$ Abstract
C
C     Delete a variable from the kernel pool.
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
C     KERNEL
C
C$ Keywords
C
C     CONSTANTS
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         NAME
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of the variable to be deleted.
C
C$ Detailed_Input
C
C     NAME       is the name of the kernel pool variable to delete.
C                The name and associated values are removed from the
C                kernel pool, freeing the occupied space.
C
C                If a watches are set on the variable designated by
C                NAME, the corresponding agents are placed on the list
C                of agents to be notified of a kernel variable update.
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
C     1) If the specified variable is not present in the kernel pool,
C        this routine simply returns.  No error is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine enables users to selectively remove variables from
C     the kernel pool, as opposed to having to clear the pool and
C     reload it.
C
C     Note that it is not necessary to remove kernel variables in order
C     to simply update them; this routine should be used only when
C     variables are to be removed.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1) Remove triaxial radii of Jupiter from the kernel pool.
C
C           CALL DVPOOL ( 'BODY599_RADII' )
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman  (JPL)
C     W.L. Taber    (JPL)
C     B.V. Semenov  (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.3.0, 30-JUL-2013 (BVS)
C
C        Updated to increment POOL state counter.
C
C-    SPICELIB Version 8.2.0, 19-MAR-2009 (NJB)
C
C        Watcher update code was re-written for compatibility
C        with new watcher system implementation.
C
C-    SPICELIB Version 8.1.0, 22-DEC-2004 (NJB)
C
C        Bug fix:  corrected logic for determining when a 
C        conflict resolution list is non-empty.
C
C        Corrected an in-line comment relating to finding the
C        head node of the conflict resolution list for NAME.
C
C-    SPICELIB Version 8.0.0, 04-JUN-1999 (NJB) (WLT)
C
C        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
C        direct insertion of data into the kernel pool without having
C        to read an external file.
C
C        Added the interface LMPOOL that allows SPICE
C        programs to load text kernels directly from memory
C        instead of requiring a text file.
C
C        Added the entry point SZPOOL to return kernel pool definition
C        parameters.
C
C        Added the entry point DVPOOL to allow the removal of a variable
C        from the kernel pool.
C
C        Added the entry point GNPOOL to allow users to determine
C        variables that are present in the kernel pool
C
C-&
 
C$ Index_Entries
C
C     delete a kernel pool variable
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 8.1.0, 22-DEC-2004 (NJB)
C
C        Bug fix:  corrected logic for determining when a 
C        conflict resolution list is non-empty.  The test
C
C           IF ( NAMEAT .LT. 0 ) THEN
C
C        formerly tested the variable NODE instead of NAMEAT.
C
C
C        Corrected an in-line comment relating to finding the
C        head node of the conflict resolution list for NAME.
C
C-&

 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DVPOOL' )
      END IF
 
C
C     Initialize the pool if necessary.
C
      IF ( FIRST ) THEN

         CALL ZZPINI (  FIRST,
     .                  MAXVAR, MAXVAL, MAXLIN,
     .                  BEGDAT, BEGTXT,
     .                  NMPOOL, DPPOOL, CHPOOL,
     .                  NAMLST, DATLST,
     .                  MAXAGT, MXNOTE, WTVARS, 
     .                  WTPTRS, WTPOOL, WTAGNT,
     .                  AGENTS, ACTIVE, NOTIFY, SUBCTR )

      END IF

C
C     Increment POOL state counter.
C
      CALL ZZCTRINC ( SUBCTR )

C
C     Locate the variable name in the hash table.  If the variable
C     is not present, just return.
C
C
C     Compute the hash value of this name.
C
      LOOKAT = ZZHASH ( NAME )

C
C     Now see if there is a non-empty conflict resolution list for the
C     input string NAME.  If so, NAMLST(LOOKAT) contains the head node
C     of the conflict resolution list; this node is a positive value.
C
      IF ( NAMLST(LOOKAT) .EQ. 0 ) THEN
 
         CALL CHKOUT ( 'DVPOOL' )
         RETURN
 
      END IF
C
C     If were are still here NAMLST(LOOKAT) is the first node of
C     a conflict resolution list.  See if the NAME corresponding
C     to this node is the one we are looking for.
C
      NAMEAT = NAMLST( LOOKAT )
      SUCCES = NAME .EQ. PNAMES(NAMEAT)
 
      DO WHILE ( .NOT. SUCCES )
 
         NAMEAT = NMPOOL ( NEXT, NAMEAT )

         IF ( NAMEAT .LT. 0 ) THEN
 
            CALL CHKOUT ( 'DVPOOL' )
            RETURN
 
         END IF
 
         SUCCES = NAME .EQ. PNAMES(NAMEAT)
 
      END DO
 
C
C     Ok, the variable's here.  The head node of its value list is
C     DATLST(NAMEAT).  Delete the list pointing to the associated
C     values.  This list is in the numeric pool DPPOOL if the head
C     node is positive; otherwise the list is in the character pool
C     CHPOOL.
C
C
      CALL ZZCLN ( LOOKAT, NAMEAT,
     .             NAMLST, DATLST, NMPOOL, CHPOOL, DPPOOL )
 
C
C     For consistency with CLPOOL, blank out the PNAMES entry containing
C     the name of this variable.  This is a bit of a flourish since
C     when errors occur during the population of the kernel pool, PNAMES
C     is not cleaned out
C
      PNAMES ( NAMEAT )  =  ' '
 
C
C     There may be agents watching the variable we just wiped out.  If
C     so, add these agents to the list of agents to be notified of a
C     watched variable update.
C
      IF (  ELEMC( NAME, WTVARS )  ) THEN
C
C        Union the update set AGENTS with the set of agents 
C        associated with the variable NAME.
C
         CALL ZZNWPOOL ( NAME,   WTVARS, WTPTRS, WTPOOL, 
     .                   WTAGNT, ACTIVE, NOTIFY, AGENTS )

      END IF
   
      CALL CHKOUT ( 'DVPOOL' )
      RETURN

 
 
 
C$Procedure GNPOOL (Get names of kernel pool variables)
 
      ENTRY GNPOOL ( NAME, START, ROOM, N, CVALS, FOUND )
 
C$ Abstract
C
C     Return names of kernel variables matching a specified template.
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
C     KERNEL
C
C$ Keywords
C
C     CONSTANTS
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         NAME
C     INTEGER               START
C     INTEGER               ROOM
C     INTEGER               N
C     CHARACTER*(*)         CVALS    ( * )
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   Template that names should match.
C     START      I   Index of first matching name to retrieve.
C     ROOM       I   The largest number of values to return.
C     N          O   Number of values returned for NAME.
C     CVALS      O   Kernel pool variables whose names match NAME.
C     FOUND      O   True if there is at least one match.
C
C$ Detailed_Input
C
C     NAME       is a MATCHI template which will be used when searching
C                for variable names in the kernel pool.  The characters
C                '*' and '%' are used for the wild string and wild
C                characters respectively.  For details of string
C                pattern matching see the header of the routine MATCHI.
C
C
C     START      is the index of the first variable name to return that
C                matches the NAME template.  The matching names are
C                assigned indices ranging from 1 to NVAR, where NVAR is
C                the number of matching names.  The index of a name does
C                not indicate how it compares alphabetically to another
C                name.
C
C                If START is less than 1, it will be treated as 1.  If
C                START is greater than the total number of matching
C                variable names, no values will be returned and N will
C                be set to zero.  However, FOUND will still be set to
C                .TRUE.
C
C
C     ROOM       is the maximum number of variable names that should
C                be returned for this template.  If ROOM is less than 1
C                the error 'SPICE(BADARRAYSIZE)' will be signaled.
C
C$ Detailed_Output
C
C     N          is the number of variable names matching NAME that are
C                returned.  It will always be less than or equal to
C                ROOM.
C
C                If no variable names match NAME, N is set to zero.
C
C
C     CVALS      is an array of kernel pool variables whose names match
C                the template NAME and which have indices ranging from
C                START to START+N-1.
C
C                Note that in general the names returned in CVALS are
C                not sorted.
C
C                If no variables match NAME, no values are assigned to
C                the elements of CVALS.
C
C                If the length of CVALS is less than the length of the
C                variable names, the values returned will be truncated
C                on the right. To ensure that names are not truncated,
C                CVALS should be declared to be at least
C                CHARACTER*(32).
C
C
C     FOUND      is TRUE if the some variable name in the kernel pool
C                matches NAME, FALSE if it is not.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the value of ROOM is less than one the error
C        'SPICE(BADARRAYSIZE)' is signaled.
C
C     2) If CVALS has declared length less than the size of a
C        name to be returned, the name will be truncated on
C        the right.  See MAXCHR for the maximum stored size of
C        string variables.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine provides the user interface for retrieving the names
C     of kernel pool variables. This interface allows you to retrieve
C     the names matching a template via multiple accesses.  Under some
C     circumstances this alleviates the problem of having to know in
C     advance the maximum amount of space needed to accommodate all
C     matching names.
C
C     However, this method of access does come with a price. It is
C     always more efficient to retrieve all of the data associated with
C     a kernel pool variable in one call than it is to retrieve it in
C     sections.  The parameter MAXVAR defines the upper bound on the
C     number of possible matching names.
C
C$ Examples
C
C
C     The following code fragment demonstrates how the names of kernel
C     pool variables matching a template can be retrieved in pieces.
C
C     First we need some declarations.
C
C        INTEGER               ROOM
C        PARAMETER           ( ROOM = 3 )
C
C        CHARACTER*(3)         INDENT
C        CHARACTER*(80)        CVALS  (ROOM)
C        CHARACTER*(8)         VARNAM
C
C        INTEGER               START
C        INTEGER               N
C
C        LOGICAL               FOUND
C
C
C     Next load the data in the file 'typical.ker' into the
C     kernel pool.
C
C        CALL LDPOOL ( 'typical.ker' )
C
C     Next we shall print the names of kernel variables that match the
C     template 'BODY599*'.
C
C        VARNAM = 'BODY599*'
C        INDENT = ' '
C        START  =  1
C
C        CALL GNPOOL ( VARNAM, START, ROOM, N, CVALS, FOUND )
C
C        IF ( .NOT. FOUND ) THEN
C
C           WRITE (*,*) 'There are no matching variables ' //
C       .               'in the kernel pool.'
C        ELSE
C
C           WRITE (*,*) 'Kernel pool variables:'
C           WRITE (*,*)
C
C           DO I = 1, N
C              WRITE (*,*) INDENT, CVALS(I)
C           END DO
C
C           DO WHILE ( N .EQ. ROOM )
C
C              START = START + N
C              CALL GNPOOL ( VARNAM, START, ROOM, N, CVALS, FOUND )
C
C              DO I = 1, N
C                 WRITE (*,*) INDENT, CVALS(I)
C              END DO
C
C           END DO
C
C        END IF
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
C     N.J. Bachman (JPL)
C     W.L. Taber   (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
C
C        ZZPINI call was updated for compatibility
C        with new watcher system implementation.
C
C-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
C
C        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
C        direct insertion of data into the kernel pool without having
C        to read an external file.
C
C        Added the interface LMPOOL that allows SPICE
C        programs to load text kernels directly from memory
C        instead of requiring a text file.
C
C        Added the entry point SZPOOL to return kernel pool definition
C        parameters.
C
C        Added the entry point DVPOOL to allow the removal of a variable
C        from the kernel pool.
C
C        Added the entry point GNPOOL to allow users to determine
C        variables that are present in the kernel pool
C
C-&
 
C$ Index_Entries
C
C     return names of kernel pool variables matching a template
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'GNPOOL')


C
C     Initialize the pool if necessary.
C
      IF ( FIRST ) THEN

         CALL ZZPINI (  FIRST,
     .                  MAXVAR, MAXVAL, MAXLIN,
     .                  BEGDAT, BEGTXT,
     .                  NMPOOL, DPPOOL, CHPOOL,
     .                  NAMLST, DATLST,
     .                  MAXAGT, MXNOTE, WTVARS, 
     .                  WTPTRS, WTPOOL, WTAGNT,
     .                  AGENTS, ACTIVE, NOTIFY, SUBCTR )

      END IF

C
C     Perform the one obvious error check first.
C
      IF ( ROOM .LT. 1 ) THEN
 
         CALL SETMSG ( 'The amount of room specified as '
     .   //            'available for output in the output array '
     .   //            'was: #.  The amount of room must be '
     .   //            'positive. ' )
 
 
         CALL ERRINT ( '#', ROOM )
         CALL SIGERR ( 'SPICE(BADARRAYSIZE)'  )
         CALL CHKOUT ( 'GNPOOL' )
         RETURN
 
      END IF
 
C
C     So far we've encountered no matching names.
C
      HITS  = 0
      N     = 0
      BEGIN = MAX( 1, START )
 
      DO K = 1, MAXVAR
C
C        See if there is any variable associated with this hash value.
C
         NNODE = NAMLST(K)
 
         DO WHILE (  NNODE .GT. 0 )
C
C           There is some name list associated with this node. See if
C           it the current one matches the supplied template.
C
            IF ( MATCHI( PNAMES(NNODE), NAME, '*', '%' ) ) THEN
 
C
C              We've got a match.  Record this fact and if we have
C              reached (or passed) the starting point, put this name
C              on the output list.
C
               HITS = HITS + 1
 
               IF ( HITS .GE. START ) THEN
 
                  IF ( N .LT. ROOM ) THEN
                     N        = N + 1
                     CVALS(N) = PNAMES(NNODE)
                  END IF
C
C                 If we've filled up the buffer, we may as well
C                 quit now.
C
                  IF ( N .EQ. ROOM ) THEN
                     FOUND = .TRUE.
                     CALL CHKOUT ( 'GNPOOL' )
                     RETURN
                  END IF
 
               END IF
 
            END IF
 
C
C           Get the next name for this node.
C
            NNODE = NMPOOL(NEXT,NNODE)
 
         END DO
C
C        Advance to the next hash value.
C
      END DO
 
 
      FOUND = HITS .GT. 0
 
      CALL CHKOUT ( 'GNPOOL' )
      RETURN
 


C$Procedure DWPOOL ( Delete watch from kernel pool )
 
      ENTRY DWPOOL ( AGENT )
      
C$ Abstract
C
C     Delete a name from the list of agents to notify whenever a member
C     of a list of kernel variables is updated.
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
C     KERNEL
C
C$ Keywords
C
C     CONSTANTS
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         AGENT
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     AGENT      I   The name of an agent to be notified after updates.
C
C$ Detailed_Input
C
C     AGENT       is any agent name that has previously been associated
C                 with a kernel pool watch via a call to SWPOOL. The
C                 agent name will be deleted from the notification list
C                 of every watched kernel variable.
C
C                 Watched variables whose notification lists become
C                 empty will be deleted.
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
C     1) It's not an error to delete a non-existent agent---one
C        that is not present in the watcher system. A call to
C        delete a non-existent agent has no effect on the state
C        of the watcher system.
C
C     2) If an attempt is made to delete an agent that
C        has an unchecked update, the error SPICE(UPDATEPENDING)
C        is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Kernel pool watches are a limited resource; the ability
C     to delete watches when they're no longer needed is essential
C     to allow programs that make heavy use of kernel pool watches
C     to run for extended periods.
C
C$ Examples
C
C     Suppose that you have an application subroutine, MYTASK, that
C     needs to access a large data set in the kernel pool.  If this
C     data could be kept in local storage and kernel pool queries
C     performed only when the data in the kernel pool has been
C     updated, the routine can perform much more efficiently.
C
C     If at some point the local stored data no longer need to be
C     watched---for example, if they're removed from the local
C     buffer to make room for other data---the watch set by the
C     agent 'MYTASK' on those data can be deleted via the call
C
C        CALL DWPOOL ( 'MYTASK' )
C
C$ Restrictions
C
C     1) It is recommended that watches be deleted only by
C        routines that established them.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 11-SEP-2013 (BVS)
C
C        Updated to increment POOL state counter. Updated description
C        of the exception 1).
C
C-    SPICELIB Version 1.0.0, 19-MAR-2009 (NJB)
C
C-&
 
C$ Index_Entries
C
C     delete kernel pool watch
C     delete agent from kernel pool watch lists
C
C-&
 
C$ Revisions
C
C     None.
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'DWPOOL' )

C
C     Initialize the pool if necessary.
C
      IF ( FIRST ) THEN

         CALL ZZPINI (  FIRST,
     .                  MAXVAR, MAXVAL, MAXLIN,
     .                  BEGDAT, BEGTXT,
     .                  NMPOOL, DPPOOL, CHPOOL,
     .                  NAMLST, DATLST,
     .                  MAXAGT, MXNOTE, WTVARS, 
     .                  WTPTRS, WTPOOL, WTAGNT,
     .                  AGENTS, ACTIVE, NOTIFY, SUBCTR )

      END IF
 
C
C     Make sure we're not silencing an agent who has something
C     to say.
C
      IF (  ELEMC( AGENT, AGENTS )  ) THEN

         CALL SETMSG ( 'Could not delete AGENT # from the watch '
     .   //            'symbol table because AGENT is associated '
     .   //            'with at least one updated kernel variable. ' )
         CALL ERRCH  ( '#', AGENT                                    )
         CALL SIGERR ( 'SPICE(UPDATEPENDING)'                        )
         CALL CHKOUT ( 'DWPOOL'                                      )
         RETURN

      END IF

C
C     Increment POOL state counter.
C
      CALL ZZCTRINC ( SUBCTR ) 

C
C     AGENT is no longer on the list of agents associated with a
C     kernel variable update.
C
      CALL REMOVC ( AGENT, AGENTS )

C
C     For each kernel variable in the watcher's list, remove
C     AGENT from its list of guys to be notified when a variable change
C     occurs. If AGENT is the only value associated with the variable,
C     delete the kernel variable's entry from the table.
C
C     This outer loop is relatively tricky, since 
C
C        1) The upper loop bound can change during loop execution.
C
C        2) The loop index I doesn't necessary increase on every
C           loop pass.
C
C     Infinite loops can lurk in code with the above attributes. We
C     need to know that the loop will always terminate. Presume that
C     no SPICE error occurs during the loop: then we observe
C     that on each loop pass, either I increases or the loop bound
C     CARDC(WTVARS) decreases, so the difference
C
C        CARDC(WTVARS) - I
C
C     does in fact decrease on every loop iteration. When this
C     difference becomes -1, the loop will end.
C
C     If a SPICE error occurs during the loop, the FAILED test
C     will terminate the loop.
C    
C     Since WTVARS may shrink due to deletion of watches, we
C     fetch the cardinality of WTVARS on each loop iteration.
C
      I = 1

      DO WHILE (  ( I .LE. CARDC(WTVARS) )  .AND.  ( .NOT. FAILED() )  )
C
C        Search the list of agents associated with the Ith watched
C        variable for AGENT. We want the list count as well, so
C        we'll traverse the whole list (which likely is short).
C
C        We don't use ZZGAPOOL here because we need to get the
C        watcher pool nodes associated with AGENT.
C
C        If we find AGENT, we'll use AGNODE to designate
C        the node associated with AGENT.
C        
         NODE   = WTPTRS(I)
         NNODES = 0
         AGNODE = 0

         DO WHILE ( NODE .GT. 0 )

            NNODES = NNODES + 1
C
C           Fetch the next agent for the Ith kernel variable.
C            
            IF ( WTAGNT(NODE) .EQ. AGENT ) THEN
C
C               Save the current node.
C
                AGNODE = NODE

            END IF
C
C           Find the next node in the list.
C
            NODE = LNKNXT( NODE, WTPOOL )

         END DO
 
         IF ( AGNODE .GT. 0 ) THEN
C
C           The input agent is on the agent list for the Ith watched
C           kernel variable. Delete this agent from the list. Delete
C           the node corresponding to AGENT from the watch pool. First
C           set the corresponding agent name to blank.
C
            WTAGNT(AGNODE) = ' '

C
C           If we're about to delete the head node of the agent list,
C           we'll need to update WTPTRS(I) to point to the new head.
C           It's possible that this agent list is empty after deletion
C           of AGNODE; we'll handle that case after the LNKFSL call
C           below.
C
            IF ( WTPTRS(I) .EQ. AGNODE ) THEN

               WTPTRS(I) = LNKNXT( AGNODE, WTPOOL )

            END IF

C
C           Now free AGNODE.
C
            CALL LNKFSL ( AGNODE, AGNODE, WTPOOL )


            IF ( NNODES .EQ. 1 ) THEN
C
C              In fact AGENT is the *only* agent for the Ith variable.
C              Deleting AGENT means that nobody's watching this
C              variable any more, so delete the variable from the
C              watched variable set.

               NW     = CARDC( WTVARS )

               VARNAM = WTVARS(I)

               CALL REMOVC ( VARNAM, WTVARS )

C
C              Remove the associated pointer from the pointer array.
C
               CALL REMLAI ( 1, I, WTPTRS, NW )

C
C              Since we deleted the current variable table entry and
C              compressed the set WTVARS and the array WTPTRS, I now
C              points to the next variable in the table. Decrement I
C              here to compensate for the increment operation at the
C              bottom of the loop.
C              
               I = I - 1

            END IF
C
C           We've now deleted AGENT from the AGENT list for WTVARS(I).
C           If the deletion left no agents watching WTVARS(I), we 
C           deleted WTVARS(I) and its associated pointer WTPTRS(I).
C
         END IF
C
C        We've processed the Ith kernel variable in the watcher table.
C
C        If we deleted the Ith WTVARS entry, we decremented I
C        at that time, so the increment operation here always is
C        applicable.
C
         I = I + 1

C
C        At this point in the loop, either I has increased or
C        CARDC(WTVARS) has decreased; hence we've made progress
C        toward loop termination.
C
      END DO

      CALL CHKOUT ( 'DWPOOL' )
      RETURN




C$Procedure ZZVUPOOL ( Private: view kernel pool watch system )
 
      ENTRY ZZVUPOOL ( UWVARS, UWPTRS, UWPOOL, UWAGNT )
      
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due to the
C     volatile nature of this routine.
C
C     Delete a name from the list of agents to notify whenever a member
C     of a list of kernel variables is updated.
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
C     KERNEL
C
C$ Keywords
C
C     KERNEL
C     PRIVATE
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         UWVARS    ( LBCELL : * )
C     INTEGER               UWPTRS    ( * )
C     INTEGER               UWPOOL    ( 2, LBCELL : * )
C     CHARACTER*(*)         UWAGNT    ( * )
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     UWVARS     O   Watched kernel variable set.
C     UWPTRS     O   Pointers from variables into the watch pool.
C     UWPOOL     O   Watch pool used for managing agent names.
C     UWAGNT     O   Array of agent names.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     UWVARS      is a set into which the local watcher system
C                 set WTVARS has been copied.
C
C     UWPTRS      is an array into which the local watcher system
C                 array WTPTRS has been copied.
C
C     UWPOOL      is a doubly linked list pool into which the local
C                 watcher system doubly linked list pool WTPOOL has
C                 been copied.
C
C     UWAGNT      is an array into which the local watcher system
C                 array WTAGNT has been copied.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the output array UWVARS is too small to hold the
C        set WTVARS, the error will be diagnosed by routines
C        in the call tree of this routine.
C
C     2) If any output array other than UWVARS is to small 
C        to hold the corresponding watch system component,
C        memory corruption will occur.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is not part of the SPICELIB API. This routine
C     may be removed in a later version of the SPICE Toolkit, or
C     its interface may change.
C
C     SPICE-based application code should not call this routine.
C
C     This is an "inspection hatch" routine used for SPICELIB 
C     testing.
C
C$ Examples
C
C     See the TSPICE test family F_DWPOOL.
C
C$ Restrictions
C
C     1) This is a private routine. See $Particulars above.
C
C     2) The caller must provide output arrays of adequate
C        size. See the declarations of the watch system
C        components in the umbrella routine POOL for size
C        requirements.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 27-MAR-2014 (BVS)
C
C        Set Index_Entries to "None." to make this entry no appear in
C        permutted index.
C
C-    SPICELIB Version 1.0.0, 19-MAR-2009 (NJB)
C
C-&
 
C$ Index_Entries
C
C     None.
C
C-&
 
C$ Revisions
C
C     None.
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZVUPOOL' )

      CALL COPYC ( WTVARS, UWVARS                )
      CALL MOVEI ( WTPTRS, CARDC(WTVARS), UWPTRS )

C
C     UWPOOL is expected to have dimensions
C
C        ( 2,  LBPOOL : MXNOTE )
C

      I = 2 * ( 6 + MXNOTE )

      CALL MOVEI ( WTPOOL, I, UWPOOL )

      CALL MOVEC ( WTAGNT, MXNOTE, UWAGNT )


      CALL CHKOUT ( 'ZZVUPOOL' )
      RETURN


C$Procedure ZZPCTRCK ( Private: check/update user's POOL state counter )
 
      ENTRY ZZPCTRCK ( USRCTR, UPDATE )
      
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due to the
C     volatile nature of this routine.
C
C     Check and update the POOL state counter tracked by a caller
C     (user) routine.
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
C     KERNEL
C
C$ Keywords
C
C     KERNEL
C     PRIVATE
C     UTILITY
C
C$ Declarations
C
C     INTEGER               USRCTR    ( CTRSIZ )
C     LOGICAL               UPDATE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     USRCTR    I/O  POOL state counter tracked by the caller
C     UPDATE     O   Flag indicating if input counter was updated
C
C     CTRSIZ     P   Dimension of the counter array
C
C$ Detailed_Input
C
C     USRCTR      is the value of the POOL state counter tracked by
C                 (saved in) the caller (user) routine.
C
C$ Detailed_Output
C
C     USRCTR      is the current POOL state counter. 
C
C     UPDATE      is the logical flag indicating whether the input POOL
C                 state counter was different from the current POOL
C                 state counter and, therefore, had to be updated
C                 (UPDATE = .TRUE.) or if it was the same (UPDATE =
C                 .FALSE.).
C
C$ Parameters
C
C     CTRSIZ      is the dimension of the counter array used by
C                 various SPICE subsystems to uniquely identify
C                 changes in their states. This parameter is 
C                 defined in the private include file 'zzctr.inc'.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is not part of the SPICELIB API. This routine
C     may be removed in a later version of the SPICE Toolkit, or
C     its interface may change.
C
C     SPICE-based application code should not call this routine.
C
C     This routine allows other routines to be aware of POOL state
C     change due to addition or deletion variables or watchers. Such 
C     awareness is needed to be able to locally save some POOL-based
C     data (e.g. body name/ID mappings or frame definitions) and only 
C     update these locally saved values if the POOL has changed. 
C
C     To make use of the POOL state counter to achieve this goal the
C     caller routines save the POOL state counter returned by the first
C     call to this routine and then check that saved value against the
C     current POOL state counter and update it by subsequent calls this
C     routine.
C
C$ Examples
C
C     The routines that need to be aware of and act on the POOL state
C     change initialize a local POOL counter array using ZZCTRUIN, save
C     it, and check it against the current POOL state counter and
C     update it, if needed, using this entry point, as follows:
C
C        C
C        C     Include zzctr.inc to access CTRSIZ.
C        C
C              INCLUDE              'zzctr.inc'
C              ...
C
C        C
C        C     In local variable declarations declare and save
C        C     the local POOL state counter. Also declare the 
C        C     update flag.
C        C
C              INTEGER               USRCTR ( CTRSIZ )
C              LOGICAL               UPDATE
C              ...      
C              SAVE                  USRCTR
C              ...
C
C        C
C        C     In all places where initialization is done  
C        C     initialize the local POOL state counter using  
C        C     ZZCTRUIN to ensure an update on the first check. 
C        C
C              IF ( FIRST ) THEN
C                 ...
C                 CALL ZZCTRUIN( USRCTR )
C                 FIRST = .FALSE.
C              END IF
C              ...
C
C        C
C        C     In all places where there is a need to check for 
C        C     the POOL state change call this entry to 
C        C     check and update the local POOL state counter.
C        C
C              CALL ZZPCTRCK ( USRCTR, UPDATE )
C
C              IF ( UPDATE ) THEN
C
C        C
C        C        It the POOL state changed, do what needs to 
C        C        be done to deal with saved values based 
C        C        on POOL data.
C        C
C                 ...
C
C              END IF
C
C$ Restrictions
C
C     1) This is a private routine. See $Particulars above.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 27-MAR-2014 (BVS)
C
C-&
 
C$ Index_Entries
C
C     None.
C
C-&
 
C$ Revisions
C
C     None.
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL ZZCTRCHK ( SUBCTR, USRCTR, UPDATE )
      
      RETURN
      END
