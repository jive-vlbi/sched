C$Procedure ZZGAPOOL ( Private: get agent set for watched variable )
 
      SUBROUTINE ZZGAPOOL ( VARNAM, WTVARS, WTPTRS, 
     .                      WTPOOL, WTAGNT, AGTSET )

       
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due to the
C     volatile nature of this routine.
C
C     Return a SPICE set containing the names of agents watching 
C     a specified kernel variable.
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

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      CHARACTER*(*)         VARNAM
      CHARACTER*(*)         WTVARS ( LBCELL : * )
      INTEGER               WTPTRS ( * )
      INTEGER               WTPOOL ( 2, LBPOOL : * )
      CHARACTER*(*)         WTAGNT ( * )
      CHARACTER*(*)         AGTSET ( LBCELL : * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     VARNAM     I   Kernel variable name.
C     WTVARS     I   Watched kernel variable set.
C     WTPTRS     I   Pointers from variables into the watch pool.
C     WTPOOL     I   Watch pool used for managing agent names.
C     WTAGNT     I   Array of agent names.
C     AGTSET     O   Set of agents for VARNAM.
C  
C$ Detailed_Input
C
C     VARNAM      is the name of a kernel variable.
C
C     WTVARS      is a SPICE set containing the contents of the kernel
C                 pool watcher system's set WTVARS.
C
C     WTPTRS      is an array containing the contents of the kernel
C                 pool watcher system's array WTPTRS.
C
C     WTPOOL      is a SPICE doubly linked list pool containing the
C                 contents of the kernel pool watcher system's pool
C                 WTPOOL.
C
C     WTAGNT      is an array containing the contents of the kernel
C                 pool watcher system's array WTAGNT.
C
C$ Detailed_Output
C
C     AGTSET      is a SPICE set containing the names of the agents
C                 associated with the kernel variable designated by
C                 VARNAM.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the output set AGTSET is too small to hold the set of
C        agents watching VARNAM, the error will be diagnosed by routines
C        in the call tree of this routine.
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
C$ Examples
C
C     See POOL entry point SWPOOL.
C
C$ Restrictions
C
C     1) This is a private routine. See $Particulars above.
C
C     2) Contents of the input arrays are assumed to be valid.
C        The output returned by this routine is meaningless
C        otherwise.
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
C-    SPICELIB Version 1.0.0, 17-MAR-2009 (NJB)
C
C-&
 
C$ Index_Entries
C
C     get agent set for watched kernel variable
C
C-&
 
C$ Revisions
C
C     None.
C
C-&


C
C     SPICELIB functions
C      
      INTEGER               BSRCHC
      INTEGER               CARDC
      INTEGER               LNKNXT
      INTEGER               SIZEC

      LOGICAL               RETURN

C
C     Local variables
C
      INTEGER               LOC
      INTEGER               NFETCH
      INTEGER               NODE


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGAPOOL' )

C
C     The output agent set is empty until we find any
C     agents.
C
      CALL SCARDC ( 0, AGTSET )

C
C     Find the location of VARNAM in the set of watched
C     variables.
C      
      LOC = BSRCHC ( VARNAM, CARDC(WTVARS), WTVARS(1) )

      IF ( LOC .EQ. 0 ) THEN
C
C        This variable is not watched. The agent set is 
C        empty.
C
         CALL CHKOUT ( 'ZZGAPOOL' )
         RETURN

      END IF

C
C     Set NODE to the head node of the agent list for VARNAM.
C     Traverse the agent list for VARNAM. Collect the agents
C     as an unordered list, then turn the list into a set.
C
      NODE   = WTPTRS(LOC)
      NFETCH = 0

      DO WHILE ( NODE .GT. 0 )

         NFETCH         = NFETCH + 1
         AGTSET(NFETCH) = WTAGNT(NODE)

         NODE           = LNKNXT( NODE, WTPOOL )

      END DO

      CALL VALIDC ( SIZEC(AGTSET), NFETCH, AGTSET )


      CALL CHKOUT ( 'ZZGAPOOL' )
      RETURN
      END
