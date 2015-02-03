C$Procedure ZZCVPOOL ( Private---Check variable update, with counter )

      SUBROUTINE ZZCVPOOL ( AGENT, USRCTR, UPDATE )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due to the
C     volatile nature of this routine.
C
C     Determine whether or not any of the POOL variables that are to be
C     watched and have AGENT on their distribution list have been
C     updated, but do the full watcher check only if the POOL state
C     counter has changed.
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

      IMPLICIT NONE

      INCLUDE              'zzctr.inc'

      CHARACTER*(*)         AGENT
      INTEGER               USRCTR    ( CTRSIZ )
      LOGICAL               UPDATE

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     AGENT      I   Name of the agent to check for notices
C     USRCTR    I/O  POOL state counter tracked by the caller
C     UPDATE     O   .TRUE. if variables for AGENT have been updated
C
C$ Detailed_Input
C
C     AGENT       is the name of a subroutine, entry point, or
C                 significant portion of code that needs to access
C                 variables in the kernel pool. Generally this agent
C                 will buffer these variables internally and fetch them
C                 from the kernel pool only when they are updated.
C
C     USRCTR      is the value of the POOL state counter tracked by
C                 (saved in) the caller (user) routine specifically
C                 for this agent.
C
C$ Detailed_Output
C
C     USRCTR      is the current POOL state counter. 
C
C     UPDATE      is a logical flag that will be set to true if the
C                 variables in the kernel pool that are required by
C                 AGENT have been updated since the last call to
C                 CVPOOL.
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
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine uses ZZPCTRCK to check the caller's saved POOL state
C     counter specific for the input AGENT against the current POOL
C     state counter to bypass the call to CVPOOL in cases when the POOL
C     has not changed.
C
C     If the POOL state counter specific for the input AGENT did not
C     change it bypasses the call to CVPOOL and returns the UPDATE flag
C     set to .FALSE.
C
C     If the POOL state counter specific for the input AGENT did change
C     it calls CVPOOL and returns the UPDATE flag set to whatever value
C     CVPOOL sets it to.
C
C     For details on ZZPCTRCK and CVPOOL see POOL the umbrella routine.
C
C$ Examples
C
C     Suppose that you have an application subroutine, TASK, that
C     needs to access to two large independent data set in the kernel
C     pool. If this data could be kept in local storage and kernel pool
C     queries performed only when the data in the kernel pool has been
C     updated, the routine can perform much more efficiently.
C
C     The code fragment below illustrates how you might make use of this
C     feature.
C
C     C
C     C     On the first call to this routine set two separate 
C     C     watchers for the two sets of POOL variables that we are 
C     C     interested in and initialize the two POOL state counters,
C     C     one for each POOL agent.
C     C
C           IF ( FIRST ) THEN
C
C
C              FIRST = .FALSE.
C              HAVE1 = .FALSE.
C              HAVE2 = .FALSE.
C
C              CALL ZZCTRUIN ( USRCT1 )
C              CALL ZZCTRUIN ( USRCT2 )
C
C              CALL SWPOOL ( 'TASK1', NNAMS1, NAMES1 )
C              CALL SWPOOL ( 'TASK2', NNAMS2, NAMES2 )
C
C           END IF
C
C     C
C     C     If any of the variables has been updated, fetch
C     C     it from the kernel pool. (Note that this also
C     C     handles getting variables for the first time.)
C     C     We use HAVE1 to indicate the fetch succeeded. If it
C     C     didn't, we need to attempt the fetch on the next 
C     C     pass into this routine. Do the first set of variable.
C     C
C           CALL CVPOOL ( 'TASK1', USRCT1, UPDATE )
C
C           IF (  UPDATE  .OR (.NOT. HAVE1 ) ) THEN
C
C              CALL GDPOOL ( 'TASK1_VAR_1', 1, M, N1, VALS1, FOUND(1) )
C              CALL GDPOOL ( 'TASK1_VAR_2', 1, M, N2, VALS2, FOUND(2) )
C                      .
C              CALL GDPOOL ( 'TASK1_VAR_N', 1, M, NN, VALSN, FOUND(N) )
C
C           END IF
C
C           IF ( FAILED() ) THEN
C                 .
C              do something about the failure
C                 .
C           ELSE
C              HAVE1 = .TRUE.
C           END IF
C
C     C
C     C     Do the second set of variable.
C     C
C           CALL CVPOOL ( 'TASK2', USRCT2, UPDATE )
C
C           IF (  UPDATE  .OR (.NOT. HAVE2 ) ) THEN
C
C              CALL GDPOOL ( 'TASK2_VAR_1', 1, M, N1, VALS1, FOUND(1) )
C              CALL GDPOOL ( 'TASK2_VAR_2', 1, M, N2, VALS2, FOUND(2) )
C                      .
C              CALL GDPOOL ( 'TASK2_VAR_N', 1, M, NN, VALSN, FOUND(N) )
C
C           END IF
C
C           IF ( FAILED() ) THEN
C                 .
C              do something about the failure
C                 .
C           ELSE
C              HAVE2 = .TRUE.
C           END IF
C
C
C$ Restrictions
C
C     The POOL counter passed to the routine by the caller must be
C     specific to the POOL agent that's being checked.
C
C     The caller routines watching more than one agent must set up a
C     separate local POOL counter for each agent.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 06-SEP-2013 (BVS)
C
C-&
 
C$ Index_Entries
C
C     Check the kernel pool for updated variables, with counter
C
C-&
 
C
C     SPICELIB functions.
C
      LOGICAL               RETURN

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

C
C     Check/update counter.
C
      CALL ZZPCTRCK ( USRCTR, UPDATE )
      
C
C     If counter was updated, check in and call CVPOOL.
C
      IF ( UPDATE ) THEN
         
         CALL CHKIN  ( 'ZZCVPOOL' )

         CALL CVPOOL ( AGENT, UPDATE )

         CALL CHKOUT ( 'ZZCVPOOL' )

      END IF

      RETURN

      END 
