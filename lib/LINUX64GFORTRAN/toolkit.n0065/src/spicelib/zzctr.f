C$Procedure ZZCTR ( Manipulate Counter Array )

      SUBROUTINE ZZCTR ( NEWCTR, OLDCTR, UPDATE )

C$ Abstract
C
C     Manipulate counter array.
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
C     None.
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE                'zzctr.inc'

      INTEGER               NEWCTR ( CTRSIZ )
      INTEGER               OLDCTR ( CTRSIZ )
      LOGICAL               UPDATE

C$ Brief_I/O
C
C     Variable  I/O  Entry
C     --------  ---  --------------------------------------------------
C     NEWCTR     I   ZZCTRCHK
C     OLDCTR    I/O  ZZCTRUIN, ZZCTRSIN, ZZCTRINC, ZZCTRCHK
C     UPDATE     O   ZZCTRCHK
C
C     CTRSIZ     P   (All)
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
C     CTRSIZ      is the dimension of the counter array used by
C                 various SPICE subsystems to identify
C                 changes in their states. This parameter is 
C                 defined in the include file.
C
C$ Exceptions
C
C     1) If ZZCTR is called directly, the error SPICE(BOGUSENTRY) is
C        signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     ZZCTR should never be called directly, but should instead be
C     accessed only through its entry points.
C
C     The purpose of this routine is to manipulate counter arrays used
C     by various SPICE subsystems to identify changes in their states.
C
C           ZZCTRUIN       Initialize counter array kept by a user
C                          routine.
C
C           ZZCTRSIN       Initialize counter array kept by a
C                          subsystem routine.
C
C           ZZCTRINC       Increment counter array.
C
C           ZZCTRCHK       Check/update counter array.
C
C     A counter array consists of CTRSIZ elements representing
C     cascading counters. The fastest counter is at index 1, the
C     slowest counter is at index CTRSIZ. At the start of counting all
C     counter array elements are set to INTMIN. In the process of
C     counting the fastest element is incremented by one. As with any
C     cascading counters when the fastest counter reaches INTMAX it
C     rolls back to INTMIN and the next counter is incremented by 1.
C     When all counters reach INTMAX, ZZCTRINC signals an error.
C
C$ Examples
C
C     The counter array gives subsystems like POOL a way to provide to
C     the user routines of that subsystem's services a simple indicator
C     of whether the subsystem's state has changed, meaning that some
C     saved values may have become stale and need to be updated or some
C     other action needs to be taken in the user routines.
C
C     For that the subsystem initializes a counter array ("subsystem
C     counter") using ZZCTRSIN, saves it, increments it using ZZCTRINC
C     each time the subsystem's state changes, and resets a user
C     counter to subsystem counter and indicates the need for update
C     when counters are not the same, like this:
C
C        C
C        C     Include zzctr.inc to access CTRSIZ.
C        C
C              INCLUDE              'zzctr.inc'
C              ...
C        C
C        C     In input/output variable declarations declare user
C        C     user counter (USRCTR) as I/O and the update flag
C        C     (UPDATE) as O.
C        C
C              INTEGER               USRCTR ( CTRSIZ )
C              LOGICAL               UPDATE
C              ...
C        C
C        C     In local variable declarations declare and save the 
C        C     subsystem counter (SUBCTR) that will incremented 
C        C     with each change of state.
C        C
C              INTEGER               SUBCTR ( CTRSIZ )
C              ...      
C              SAVE                  SUBCTR
C              ...
C        C
C        C     In all places where initialization is done set 
C        C     subsystem counter using ZZCTRSIN. 
C        C
C              IF ( FIRST ) THEN
C                 ...
C                 CALL ZZCTRSIN( SUBCTR )
C                 FIRST = .FALSE.
C              END IF
C              ...
C        C
C        C     In all places where the subsystem state changes
C        C     increment the subsystem counter using ZZCTRINC. 
C        C
C              CALL ZZCTRINC( SUBCTR )
C              ...
C        C
C        C     Make a special entry that checks and resets input 
C        C     user counter to the current subsystem counter and 
C        C     indicates whether it was updated using ZZCTRCHK.
C        C
C              ENTRY <entry_name> ( USRCTR, UPDATE )
C
C              CALL ZZCTRCHK( SUBCTR, USRCTR, UPDATE )
C
C              RETURN
C              ...
C
C     The users of the subsystem services initialize a counter array
C     ("user counter") using ZZCTRUIN, save it, and check against the
C     subsystem's counter and update, if needed, using the subsystem's
C     entry point, like this:
C
C        C
C        C     Include zzctr.inc to access CTRSIZ.
C        C
C              INCLUDE              'zzctr.inc'
C              ...
C        C
C        C     In local variable declarations declare and save
C        C     user counter. Also declare an update flag.
C        C
C              INTEGER               USRCTR ( CTRSIZ )
C              LOGICAL               UPDATE
C              ...      
C              SAVE                  USRCTR
C              ...
C        C
C        C     In all places where initialization is done initialize 
C        C     the user counter using ZZCTRUIN to ensure update on  
C        C     the first check. 
C        C
C              IF ( FIRST ) THEN
C                 ...
C                 CALL ZZCTRUIN( USRCTR )
C                 FIRST = .FALSE.
C              END IF
C              ...
C        C
C        C     In all places where there is a need to check for 
C        C     the subsystem state change call subsystem's entry
C        C     that checks and updates the user counter based on
C        C     value of the subsystem counter.
C        C
C              CALL <entry_name> ( USRCTR, UPDATE )
C
C              IF ( UPDATE ) THEN
C
C        C
C        C        Do what needs to be done when the subsystem
C        C        state has changed.
C        C
C                 ...
C
C              END IF
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
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS)
C
C-&
 
C$ Index_Entries
C
C     manipulate counter array
C
C-& 
 
C
C     SPICELIB functions
C 
      INTEGER               INTMAX
      INTEGER               INTMIN

      LOGICAL               RETURN

C
C     Local variables.
C
      INTEGER               CTRHGH
      INTEGER               CTRLOW

      LOGICAL               FIRST
 
C
C     Save EVERYTHING.
C
      SAVE

C
C     Initial values.
C 
      DATA                  FIRST  / .TRUE. /

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
C
C     This routine should never be called. If this routine is called,
C     an error is signaled.
C
      CALL CHKIN  ( 'ZZCTR' )

      CALL SETMSG ( 'ZZCTR: You have called an entry which performs ' //
     .              'performs no run-time function. This may '        //
     .              'indicate a bug. Please check the documentation ' //
     .              'for the subroutine ZZCTR.' )
 
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
 
      CALL CHKOUT ( 'ZZCTR' )

      RETURN


C$Procedure ZZCTRUIN ( CounTeR array, User counter INitialization )

      ENTRY ZZCTRUIN ( OLDCTR )

C$ Abstract
C
C     Set counter array to the initial values for the user routines.
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
C     None.
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
C
C     INTEGER               OLDCTR ( CTRSIZ )
C
C$ Brief_I/O
C
C     Variable  I/O  Entry
C     --------  ---  --------------------------------------------------
C     OLDCTR     O   Counter array to be set to initial user values.
C
C     CTRSIZ     P   Counter array size.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OLDCTR      is the counter array set to the initial values for
C                 the user routines.
C
C$ Parameters
C
C     CTRSIZ      is the dimension of the counter array used by
C                 various SPICE subsystems to identify
C                 changes in their states.
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
C     This type of initialization must be done by all user routines
C     to ensure update on the first check. 
C
C$ Examples
C
C     See umbrella Examples.
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
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS)
C
C-&
 
C$ Index_Entries
C
C     initialize counter array to user values
C
C-& 

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

C
C     Initialize the high and low values.
C
      IF ( FIRST ) THEN

         CTRHGH = INTMAX()
         CTRLOW = INTMIN()

         FIRST = .FALSE.

      END IF

C
C     Set counter.
C
      OLDCTR(1) = CTRHGH
      OLDCTR(2) = CTRHGH

      RETURN


C$Procedure ZZCTRSIN ( CounTeR array, Subsystem counter INitialization )

      ENTRY ZZCTRSIN ( OLDCTR )

C$ Abstract
C
C     Set counter array to the initial values for the subsystem
C     routines.
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
C     None.
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
C
C     INTEGER               OLDCTR ( CTRSIZ )
C
C$ Brief_I/O
C
C     Variable  I/O  Entry
C     --------  ---  --------------------------------------------------
C     OLDCTR     O   Counter array to be set to initial subsystem values
C
C     CTRSIZ     P   Counter array size.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OLDCTR      is the counter array set to the initial values for
C                 the subsystem routines.
C
C$ Parameters
C
C     CTRSIZ      is the dimension of the counter array used by
C                 various SPICE subsystems to identify
C                 changes in their states.
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
C     This type of initialization must be done by all subsystem
C     routines to set the counters to the lowest initial values.
C
C$ Examples
C
C     See umbrella Examples.
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
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS)
C
C-&
 
C$ Index_Entries
C
C     initialize counter array to subsystem values
C
C-& 

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

C
C     Initialize the high and low values.
C
      IF ( FIRST ) THEN

         CTRHGH = INTMAX()
         CTRLOW = INTMIN()

         FIRST = .FALSE.

      END IF

C
C     Set counter.
C
      OLDCTR(1) = CTRLOW
      OLDCTR(2) = CTRLOW

      RETURN




C$Procedure ZZCTRINC ( CounTeR Array, INCrement counter )

      ENTRY ZZCTRINC ( OLDCTR )

C$ Abstract
C
C     Increment counter array.
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
C     None.
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
C
C     INTEGER               OLDCTR ( CTRSIZ )
C
C$ Brief_I/O
C
C     Variable  I/O  Entry
C     --------  ---  --------------------------------------------------
C     OLDCTR    I/O  Counter array to be incremented.
C
C     CTRSIZ     P   Counter array size.
C
C$ Detailed_Input
C
C     OLDCTR      is the counter array to be incremented.
C
C$ Detailed_Output
C
C     OLDCTR      is the counter array that was incremented.
C
C$ Parameters
C
C     CTRSIZ      is the dimension of the counter array used by
C                 various SPICE subsystems to identify
C                 changes in their states.
C
C$ Exceptions
C
C     1) If all elements of the counter array on the input are equal
C        to INTMAX, the error '(SPICEISTIRED)' is signaled.
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
C     See umbrella Examples.
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
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS)
C
C-&
 
C$ Index_Entries
C
C     increment counter array
C
C-& 

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

C
C     Initialize the high and low values.
C
      IF ( FIRST ) THEN

         CTRHGH = INTMAX()
         CTRLOW = INTMIN()

         FIRST = .FALSE.

      END IF

C
C     Signal an error if both input counter array elements have high
C     values.
C
      IF ( OLDCTR(1) .EQ. CTRHGH .AND. OLDCTR(2) .EQ. CTRHGH ) THEN

         CALL CHKIN  ( 'ZZCTRINC' )

         CALL SETMSG ( 'A subsystem state counter overflowed. '    //
     .                 'For this to happen there must be '         //
     .                 'a SPICE bug or you must have been '        //
     .                 'running your SPICE-based application for ' //
     .                 'a very long time. Please contact NAIF.'    //
     .                 'and report the circumstances under '       //
     .                 'which this happened.'                      )
 
         CALL SIGERR ( 'SPICE(SPICEISTIRED)' )

         CALL CHKOUT ( 'ZZCTRINC' )

         RETURN

      END IF

C
C     Increment counters.
C
      IF ( OLDCTR(1) .EQ. CTRHGH ) THEN

         OLDCTR(1) = CTRLOW
         OLDCTR(2) = OLDCTR(2) + 1

      ELSE

         OLDCTR(1) = OLDCTR(1) + 1

      END IF

      RETURN



C$Procedure ZZCTRCHK ( CounTeR array, CHecK and update )

      ENTRY ZZCTRCHK ( NEWCTR, OLDCTR, UPDATE )

C$ Abstract
C
C     Check and update, if needed, counter array.
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
C     None.
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
C
C     INTEGER               NEWCTR ( CTRSIZ )
C     INTEGER               OLDCTR ( CTRSIZ )
C     LOGICAL               UPDATE
C
C$ Brief_I/O
C
C     Variable  I/O  Entry
C     --------  ---  --------------------------------------------------
C     NEWCTR     I   New counter value.
C     OLDCTR    I/O  Old counter value.
C     UPDATE     O   Update flag.
C
C     CTRSIZ     P   Counter array size.
C
C$ Detailed_Input
C
C     NEWCTR      is the new counter value against which the old
C                 value should be checked.
C
C     OLDCTR      is the old counter value.
C
C$ Detailed_Output
C
C     OLDCTR      is the updated the old counter value, set equal to
C                 the new counter value if counters were different or
C                 kept the same if counters were the same.
C
C     UPDATE      is the logical flag indicating whether the old
C                 counter was updated.
C
C$ Parameters
C
C     CTRSIZ      is the dimension of the counter array used by
C                 various SPICE subsystems to identify
C                 changes in their states.
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
C     None.
C
C$ Examples
C
C     See umbrella Examples.
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
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS)
C
C-&
 
C$ Index_Entries
C
C     check and update counter array
C
C-& 

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

C
C     Do counters differ?
C
      UPDATE = NEWCTR(1) .NE. OLDCTR(1) .OR. NEWCTR(2) .NE. OLDCTR(2)

C
C     If they do, set old counter to new value.
C
      IF ( UPDATE ) THEN

         OLDCTR(1) = NEWCTR(1)
         OLDCTR(2) = NEWCTR(2)

      END IF

      RETURN

      END
