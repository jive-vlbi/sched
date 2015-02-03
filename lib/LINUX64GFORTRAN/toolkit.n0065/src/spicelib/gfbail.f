C$Procedure GFBAIL ( GF, default bailout function )

      LOGICAL FUNCTION GFBAIL()
      IMPLICIT NONE

C$ Abstract
C
C     This routine serves as a placeholder for an interrupt
C     detection function.
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
C     GF
C
C$ Keywords
C
C     INTERRUPT
C     UTILITY
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     This function always returns the value .FALSE.
C
C$ Detailed_Input
C
C     None
C
C$ Detailed_Output
C
C     This function always returns the value .FALSE.
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
C     This routine serves as a stub for interrupt function input
C     arguments in GF mid-level search routines such as
C
C        GFEVNT
C        GFOCCE
C        GFFOVE
C
C     Those routines allow the caller to pass in a custom interrupt
C     detection function.
C
C     Searches conducted with the GF APIs can be unexpectedly
C     time-consuming. When such searches are carried out by an
C     interactive application, it can be useful to be able to stop a
C     search without stopping the application itself. This enables a
C     user to avoid loss of previous work that may have been performed
C     during the program run.
C
C     The mid-level GF search APIs named above provide partial support
C     for interrupt handling. They allow the caller to pass in an
C     interrupt detection function; when their input "bail-out" flag
C     argument is set to .TRUE. by the caller, the low-level GF
C     root-finding routines invoked by these APIs will, over regular,
C     short time intervals (these intervals are usually determined by
C     the completion of loop passes), call the interrupt detection
C     function. These routines will return immediately if the function
C     indicates that an interrupt has occurred.
C
C     However, SPICELIB doesn't fully support interrupt handling
C     because ANSI Fortran 77 doesn't provide the features necessary to
C     implement an interrupt detection function.
C
C     Some Fortran platforms do provide non-standard routines that
C     support interrupt handling, so for these systems, SPICE users may
C     be able to create their own interrupt detection routines. Such
C     routines should have calling sequences identical to that of this
C     function. These routines should have a "reset" feature that
C     enables an application to make them return .FALSE. after an
C     interrupt has been indicated and processed.
C
C     For platforms where interrupt detection can't be implemented, or
C     in cases where applications must call mid-level GF APIs but don't
C     need interrupt handling, this routine can be used.
C
C     This routine has no interrupt detection capability: it always
C     returns the value .FALSE.
C
C     Developers of SPICE-based applications who have the choice of
C     writing code in Fortran or C may wish to consider the fact that
C     the CSPICE Toolkit does support interrupt detection: gfbail_c,
C     the CSPICE analog of this routine, is fully functional on all
C     platforms on which CSPICE is supported.
C
C$ Examples
C
C     This example shows how to call a mid-level GF search API that
C     requires an input interrupt detection function.
C
C     If a custom interrupt detection function is available, it
C     can be referenced exactly as is GFBAIL in this example.
C
C     The code fragment below is from the first code example in the
C     header of
C
C        gfocce.for
C
C     Only the portions of that program relevant to use of GFBAIL are
C     copied here. Deleted portions of code are indicated by ellipses.
C
C     Note that GFBAIL is the third-to-last argument in the
C     GFOCCE call.
C
C
C              PROGRAM EX1
C
C              IMPLICIT NONE
C
C              ...
C
C              LOGICAL               GFBAIL
C              EXTERNAL              GFBAIL
C
C              ...
C
C        C
C        C     Turn on progress reporting; turn off interrupt
C        C     handling.
C        C
C
C              ...
C
C              BAIL = .FALSE.
C
C        C
C        C     Perform the search.
C        C
C              CALL GFOCCE ( 'ANY',
C             .              'MOON',   'ellipsoid',  'IAU_MOON',
C             .              'SUN',    'ellipsoid',  'IAU_SUN',
C             .              'LT',     'EARTH',      CNVTOL,
C             .              GFSTEP,   GFREFN,       RPT,
C             .              GFREPI,   GFREPU,       GFREPF,
C             .              BAIL,     GFBAIL,       CNFINE,  RESULT )
C
C
C             ...
C
C
C
C$ Restrictions
C
C     This is a stub routine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     L.S. Elson     (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1  23-JUN-2010 (EDW)
C
C        Minor edit to Declarations for correct header format.
C
C-    SPICELIB Version 1.0.0  05-MAR-2009 (NJB) (LSE) (EDW)
C
C-&

C$ Index_Entries
C
C     GF standard bail out routine
C
C-&

      GFBAIL  =  .FALSE.

      RETURN
      END


