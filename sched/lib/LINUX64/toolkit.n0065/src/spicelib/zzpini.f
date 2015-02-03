C$Procedure      ZZPINI ( Private --- kernel pool initialization )
 
      SUBROUTINE ZZPINI ( FIRST,
     .                    MAXVAR, MAXVAL, MAXLIN,
     .                    BEGDAT, BEGTXT,
     .                    NMPOOL, DPPOOL, CHPOOL,
     .                    NAMLST, DATLST,
     .                    MAXAGT, MXNOTE, WTVARS, 
     .                    WTPTRS, WTPOOL, WTAGNT,
     .                    AGENTS, ACTIVE, NOTIFY, SUBCTR  )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This routine initializes the data structures needed for
C     maintaining the kernel pool and initializes the hash function
C     used for the name list in the kernel pool.
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
 
      INCLUDE              'zzctr.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )
 
      LOGICAL               FIRST
      INTEGER               MAXVAR
      INTEGER               MAXVAL
      INTEGER               MAXLIN
      CHARACTER*(*)         BEGDAT
      CHARACTER*(*)         BEGTXT
      INTEGER               NMPOOL ( 2,  LBPOOL : MAXVAR )
      INTEGER               DPPOOL ( 2,  LBPOOL : MAXVAL )
      INTEGER               CHPOOL ( 2,  LBPOOL : MAXLIN )
      INTEGER               NAMLST (              MAXVAR )
      INTEGER               DATLST (              MAXVAR )
      INTEGER               MAXAGT
      INTEGER               MXNOTE
      CHARACTER*(*)         WTVARS ( LBCELL : MAXVAR )
      INTEGER               WTPTRS ( MAXVAR )
      INTEGER               WTPOOL ( 2, LBPOOL:MXNOTE ) 
      CHARACTER*(*)         WTAGNT ( MXNOTE )
      CHARACTER*(*)         AGENTS ( LBCELL : * )
      CHARACTER*(*)         ACTIVE ( LBCELL : * )
      CHARACTER*(*)         NOTIFY ( LBCELL : * )
      INTEGER               SUBCTR ( CTRSIZ )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      FIRST     I/O  Used to determine if this is the first pass
C      MAXVAR     I   Maximum number of variables in the pool
C      MAXVAL     I   Maximum number of d.p. values in the pool
C      MAXLIN     I   Maximum number of string values in the pool
C      BEGDAT     O   Marker used to begin data section of a kernel
C      BEGTXT     O   Marker used to begin text section of a kernel
C      NMPOOL     O   Linked list for resolving hash collisions of names
C      DPPOOL     O   Linked list for maintaining d.p. values.
C      CHPOOL     O   Linked list for maintaining string values
C      NAMLST     O   Heads of collision resolution lists
C      DATLST     O   Heads of data values lists
C      MAXAGT     I   Maximum number of agents that can be supported
C      MXNOTE     I   Maximum number of agents that can be notified
C      WTPTR     O   Name array of watcher symbol table
C      WATPTR     O   Pointer array of watcher symbol table
C      WATVAL     O   Values array of watcher symbol table.
C      AGENTS     O   Set of agents
C      ACTIVE     O   Watchers that are active.
C      NOTIFY     O   Agents to notify
C      SUBCTR     O   POOL state counter.
C
C$ Detailed_Input
C
C     FIRST       is a logical indicating whether or not this is
C                 the first call to this routine.  If FIRST is .TRUE.
C                 the various items are initialized and FIRST is
C                 set to .FALSE.  If FIRST is .FALSE. no action is
C                 taken by this routine.
C
C     MAXVAR      is the maximum number of variables that the
C                 kernel pool may contain at any one time.
C
C
C     MAXVAL      is the maximum number of distinct values that
C                 may belong to the variables in the kernel pool.
C
C     MAXLIN      is the maximum number of character strings that
C                 can be stored as data for kernel pool variables.
C
C     MXNOTE      is the maximum number of distinct variable-agents
C                 pairs that can be maintained by the kernel pool.
C                 (A variable is "paired" with an agent, if that agent
C                 is to be notified whenever the variable is updated.)
C
C     MAXAGT      is the maximum number of agents that can be kept
C                 on the distribution list for notification of updates
C                 to kernel variables.
C
C$ Detailed_Output
C
C      FIRST      is set to .FALSE. on output.
C
C      BEGDAT     Marker used to begin data section of a kernel
C
C      BEGTXT     Marker used to begin text section of a kernel
C
C      NMPOOL     Linked list pool for resolving hash collisions
C                 of names of kernel pool variables.  Each list
C                 other than the free list, is a sequence of pointers
C                 to names that have the same hash value.  On output
C                 from this routine all nodes of the pool are in the
C                 free list.
C
C      DPPOOL     Linked list pool for maintaining d.p. values.
C                 On output all nodes in the pool are in the free list
C                 of DPPOOL
C
C      CHPOOL     Linked list pool for maintaining string values.
C                 On output all nodes in the pool are in the free list
C                 of CHPOOL
C
C      NAMLST     is an array that contains the heads of lists from
C                 NMPOOL.  NAMLST( ZZHASH( NAME ) ) points to the head
C                 of the first name in the collision resolution list
C                 for NAME.  If there is no head for the collision
C                 resolution list for NAME (i.e. no name with the
C                 same hash value as name has been stored)
C                 NAMLST( ZZHASH(NAME) ) will be zero.  On output from
C                 this routine all values in NAMLST are set to zero.
C
C      DATLST     is an array that contains the "heads" of lists of
C                 pointers to the values associated with a variable.
C                 Suppose that NAME has been located in the list of
C                 variable names at location LOC.  Then DATLST(LOC)
C                 is the head node of the list of pointers to the
C                 values of NAME.  If DATLST(LOC) is positive then
C                 the values are d.p.'s If the value of DATLST(LOC)
C                 is negative, the values are strings.  The absolute
C                 value of DATLST(LOC) is the head node to the list
C                 of values associated with NAME.  If DATLST(LOC) is
C                 zero then no values have been assigned to the variable
C                 NAME.  On output all entries of DATLST are set to
C                 zero.
C
C      WTPTR     is a symbol table of variables to watch for.  WTPTR
C      WATPTR     contains the names of variables to watch. The
C      WATVAL     values associated with a name are the names of agents
C                 that have requested that the variable be watched.
C
C      AGENTS     Agents contains the list of agents that need to be
C                 notified about updates to their variables.
C
C      ACTIVE     A temporary set.
C      NOTIFY     A temporary set.
C
C      SUBCTR     Initialized POOL state counter.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C     This is a utility routine that centralizes the initialization
C     code that is common to all entry points of POOL.
C
C$ Examples
C
C     See POOL.
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
C-    SPICELIB Version 3.0.0, 30-JUL-2013 (BVS)
C
C        Added POOL state counter to the argument list, and included 
C        'zzctr.inc' to provide the counter array dimension.
C
C-    SPICELIB Version 2.0.0, 19-MAR-2009 (NJB)
C
C        Argument list was changed to accommodate re-implementation
C        of watcher system. Initialization tasks performed by this
C        routine were updated accordingly.
C
C-    SPICELIB Version 1.1.0, 13-OCT-1995 (WLT)
C
C        An integer variable was renamed to better indicate
C        its role in the routine and to make maintenance a bit
C        easier
C
C-    SPICELIB Version 1.0.0, 20-SEP-1995 (WLT)
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 13-OCT-1995 (WLT)
C
C        An integer variable was renamed to better indicate
C        its role in the routine and to make maintenance a bit
C        easier.  The integer variable was 'DONE' which looks
C        a lot like a logical.  It's been changed to 'DUMMY'.
C
C-&
 
C
C     SPICELIB Functions.
C
      INTEGER               TOUCHI
      INTEGER               ZZSHSH

      LOGICAL               FAILED

C
C     Local parameters
C
      INTEGER               SHRTLN
      PARAMETER           ( SHRTLN =  9 )
 
      INTEGER               BSLASH
      PARAMETER           ( BSLASH = 92 )
 
      CHARACTER*(SHRTLN)    DAT
      PARAMETER           ( DAT = 'begindata' )
 
      CHARACTER*(SHRTLN)    TXT
      PARAMETER           ( TXT = 'begintext' )

C
C     Local variables
C
 
      INTEGER               DUMMY
      INTEGER               I
 
 
      IF ( FIRST ) THEN
 
         CALL CHKIN ( 'ZZPINI' )
 
         DO I = 1, MAXVAR
            NAMLST(I) = 0
            DATLST(I) = 0
         END DO
 
C
C        Set up hash function. Use TOUCHI to suppress
C        compiler warnings.
C
         DUMMY  = ZZSHSH ( MAXVAR )
         DUMMY  = TOUCHI ( DUMMY  )

         BEGDAT = CHAR(BSLASH) // DAT
         BEGTXT = CHAR(BSLASH) // TXT
 
         CALL LNKINI ( MAXVAR, NMPOOL )
         CALL LNKINI ( MAXVAL, DPPOOL )
         CALL LNKINI ( MAXLIN, CHPOOL )
 
         CALL SSIZEC ( MAXVAR, WTVARS )
         CALL CLEARI ( MAXVAR, WTPTRS )
         CALL LNKINI ( MXNOTE, WTPOOL )
         CALL CLEARC ( MXNOTE, WTAGNT )
 
         CALL SSIZEC ( MXNOTE, AGENTS )
         CALL SSIZEC ( MXNOTE, ACTIVE )
         CALL SSIZEC ( MXNOTE, NOTIFY )

         CALL ZZCTRSIN ( SUBCTR )
 
         IF ( .NOT. FAILED() ) THEN
            FIRST = .FALSE.
         END IF
 
         CALL CHKOUT ( 'ZZPINI' )
         RETURN
 
      END IF
 
      RETURN
      END
