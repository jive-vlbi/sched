C$Procedure ZZNWPOOL ( Private: notify watchers of update )
 
      SUBROUTINE ZZNWPOOL ( VARNAM, WTVARS, WTPTRS, WTPOOL, 
     .                      WTAGNT, AGTWRK, NOTIFY, AGENTS )

       
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due to the
C     volatile nature of this routine.
C
C     Union the set of agents for a specified, watched kernel variable
C     with the set of agents on the kernel pool's update notification
C     list.
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
      CHARACTER*(*)         AGTWRK ( LBCELL : * )
      CHARACTER*(*)         NOTIFY ( LBCELL : * )
      CHARACTER*(*)         AGENTS ( LBCELL : * )
     

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     VARNAM     I   Kernel variable name.
C     WTVARS     I   Watched kernel variable set.
C     WTPTRS     I   Pointers from variables into the watch pool.
C     WTPOOL     I   Watch pool used for managing agent names.
C     WTAGNT     I   Array of agent names.
C     AGTWRK    I-O  Agent workspace cell.
C     NOTIFY    I-O  Another agent workspace cell.
C     AGENTS    I-O  Set of agents to be notified of updates.
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
C     AGTWRK,
C     NOTIFY      are two workspace cells used to hold list of agents.
C                 Both cells must have size at least equal to MXNOTE.
C
C$ Detailed_Output
C
C     AGTWRK,
C     NOTIFY      are the input workspace cells after use. Contents
C                 of these cells are undefined.
C
C     AGTSET      is a SPICE set containing the names of the agents
C                 associated with the kernel variable designated by
C                 VARNAM.
C
C$ Parameters
C
C     MXNOTE      Maximum size of the agent list WTAGNT in POOL.
C                 See that routine for the parameter's value.
C                 
C
C$ Exceptions
C
C     1) If the output set AGENTS is too small to hold the result
C        of the union performed by this routine, the error will be
C        diagnosed by routines in the call tree of this routine.
C
C     2) If either workspace cell AGTWRK or NOTIFY has insufficient
C        size, the error will be diagnosed by routines in the call tree
C        of this routine.
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
C     This routine centralizes the work of updating the kernel
C     pool's update notification list to account for an update
C     of a specified kernel variable. Most kernel pool entry
C     points that perform kernel pool updates should call this
C     routine to update the notification list.
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
C-    SPICELIB Version 1.0.0, 18-MAR-2009 (NJB)
C
C-&
 
C$ Index_Entries
C
C     add agents to watcher system notification list
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
      LOGICAL               RETURN


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZNWPOOL' )

C
C     Fetch the agents watching VARNAM into the set NOTIFY.
C     
      CALL ZZGAPOOL ( VARNAM, WTVARS, WTPTRS, WTPOOL, WTAGNT, NOTIFY )

C
C     Compute the union of NOTIFY and the agent list AGENTS.
C     Place the result in the workspace set AGTWRK; then copy
C     the result to AGENTS.
C
      CALL UNIONC ( NOTIFY, AGENTS, AGTWRK )
      CALL COPYC  ( AGTWRK,         AGENTS )

      CALL CHKOUT ( 'ZZNWPOOL' )
      RETURN
      END 
