C$Procedure ZZDYNVAC ( Fetch array, character frame kernel variable )

      SUBROUTINE ZZDYNVAC ( FRNAME, FRCODE, ITEM, MAXN, N, VALUES )

      IMPLICIT NONE 
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Look up array-valued character frame kernel variable.  The frame
C     name or frame ID may be used as part of the variable's name.
C
C     If the kernel variable is not present, or if the variable
C     has the wrong data type, signal an error.
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
C     FRAMES
C     KERNEL
C     PRIVATE
C     UTILITY
C
C$ Declarations
      
      INCLUDE 'zzdyn.inc'
     
      CHARACTER*(*)         FRNAME
      INTEGER               FRCODE
      CHARACTER*(*)         ITEM
      INTEGER               MAXN
      INTEGER               N
      CHARACTER*(*)         VALUES  ( * )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  -------------------------------------------------
C     FRNAME     I   Frame name.
C     FRCODE     I   Frame ID code.
C     ITEM       I   Item associated with frame definition.
C     MAXN       I   Maximum number of values to return.
C     N          O   Number of returned values.
C     VALUES     O   Output kernel variable.
C
C$ Detailed_Input
C
C     FRNAME         is the name of the reference frame with which
C                    the requested variable is associated. 
C
C     FRCODE         is the frame ID code of the reference frame with
C                    which the requested variable is associated.
C
C     ITEM           is a string identifying the specific datum
C                    to be fetched.  The kernel variable name
C                    has the form
C
C                       FRAME_<frame ID code>_<ITEM>
C
C                    or
C
C                       FRAME_<frame name>_<ITEM>
C
C                    The former of the two names takes precedence:
C                    this routine will look for a character variable
C                    of that name first.
C
C$ Detailed_Output
C
C     N              is the number of values returned in the array
C                    VALUES.
C
C     VALUES         are the values associated with the requested
C                    array-valued, character kernel variable. The
C                    kernel variable name of the form
C
C                       FRAME_<frame ID code>_<ITEM>
C
C                    will be looked up first; if this variable
C                    is found and has character type, the associated
C                    values will be returned.  If this variable is
C                    not found, the variable                    
C
C                       FRAME_<frame name>_<ITEM>
C
C                    will be looked up.  If a character variable
C                    having that name is found, the associated
C                    values will be returned.
C
C$ Parameters
C
C     See zzdyn.inc.
C
C$ Exceptions
C
C     1) If neither the frame-ID-based or frame-name-based form of the 
C        requested kernel variable name matches a kernel variable
C        present in the kernel pool, the error SPICE(KERNELVARNOTFOUND)
C        will be signaled.
C
C     2) If either the frame-ID-based or frame-name-based form of the 
C        requested kernel variable name has length greater than KVNMLN,
C        the excessively long name will not be searched for. A search
C        will still be done using the alternative form of the name if
C        that form has length less than or equal to KVNMLN.
C
C     3) If both the frame-ID-based and frame-name-based forms of the 
C        requested kernel variable name have length greater than KVNMLN,
C        the error SPICE(VARNAMETOOLONG) will be signaled.
C        
C     4) If kernel variable matching one form of the requested kernel
C        variable names is found, but that variable has numeric data
C        type, the error SPICE(BADVARIABLETYPE) will be signaled.
C
C     5) If kernel variable matching one form of the requested kernel
C        variable names is found, but that variable has more than MAXN
C        associated values, the error SPICE(BADVARIABLESIZE) will be 
C        signaled.
C
C$ Files
C
C     1) Kernel variables fetched by this routine are normally
C        introduced into the kernel pool by loading one or more
C        frame kernels.  See the Frames Required Reading for 
C        details.
C 
C$ Particulars
C
C     This routine centralizes logic for kernel variable lookups that
C     must be performed by the SPICELIB frame subsystem. Part of the
C     functionality of this routine consists of handling error
C     conditions such as the unavailability of required kernel
C     variables; hence no "found" flag is returned to the caller.
C
C     As indicated above, the requested kernel variable may have a name
C     of the form
C
C        FRAME_<frame ID code>_<ITEM>
C
C     or
C
C        FRAME_<frame name>_<ITEM>
C
C     Because most frame definition keywords have the first form, this
C     routine looks for a name of that form first.
C
C     Note that although this routine considers the two forms of the
C     names to be synonymous, from the point of view of the kernel pool
C     data structure, these names are distinct.  Hence kernel variables
C     having names of both forms, but having possibly different
C     attributes, can be simultaneously present in the kernel pool.
C     Intentional use of this kernel pool feature is discouraged.
C
C$ Examples
C
C     See ZZDYNFRM.
C
C$ Restrictions
C
C     1) This is a SPICE private routine; the routine is subject
C        to change without notice.  User applications should not
C        call this routine.
C
C     2) A scalar-valued kernel variable matching the "ID code form"
C        of the requested kernel variable name could potentially
C        mask an array-valued kernel variable matching the "name
C        form" of the requested name.  This problem can be prevented
C        by sensible frame kernel design.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB) 
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               RTRIM

      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local Parameters
C
      CHARACTER*(*)         TEMPLT
      PARAMETER           ( TEMPLT = 'FRAME_#_#' )

C
C     TEMPLN is the length of the keyword template, minus
C     the sum of the lengths of the two substitution markers ('#').
C
      INTEGER               TEMPLN
      PARAMETER           ( TEMPLN = 7 )

C
C     Local Variables
C
      CHARACTER*(KVNMLN)    CDESTR
      CHARACTER*(1)         DTYPE
      CHARACTER*(KVNMLN)    KVNAME

      INTEGER               CODELN
      INTEGER               ITEMLN
      INTEGER               NAMELN
      INTEGER               REQNAM
      INTEGER               REQNUM

      LOGICAL               FOUND

C
C     Standard SPICE error handling
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZDYNVAC' )

C
C     Prepare to check the name of the kernel variable we're about
C     to look up.
C
C     Convert the frame code to a string.
C
      CALL INTSTR ( FRCODE, CDESTR )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZDYNVAC' )
         RETURN
      END IF

C
C     Get the lengths of the input frame code, name and item.
C     Compute the length of the ID-based kernel variable name;
C     check this length against the maximum allowed value.  If
C     the name is too long, proceed to look up the form of the
C     kernel variable name based on the frame name.
C     
      CODELN = RTRIM ( CDESTR )
      NAMELN = RTRIM ( FRNAME )
      ITEMLN = RTRIM ( ITEM   )

      REQNUM = CODELN + ITEMLN + TEMPLN 

      IF ( REQNUM .LE. KVNMLN ) THEN
C
C        First try looking for a kernel variable including the frame ID
C        code.
C
C        Note the template is
C
C            'FRAME_#_#'
C
         CALL REPMI  ( TEMPLT, '#',   FRCODE, KVNAME )
         CALL REPMC  ( KVNAME, '#',   ITEM,   KVNAME )
         CALL DTPOOL ( KVNAME, FOUND, N,      DTYPE  )

      ELSE
C
C        The ID-based name is too long. We can't find the variable if
C        we can't look it up.
C
         FOUND = .FALSE.

      END IF


      IF ( .NOT. FOUND ) THEN
C
C        We need to look up the frame name-based kernel variable.
C        Determine the length of the name of this variable; make
C        sure it's not too long.
C
         REQNAM = NAMELN + ITEMLN + TEMPLN 

         IF (       ( REQNAM .GT. KVNMLN )
     .        .AND. ( REQNUM .GT. KVNMLN ) ) THEN
C
C           Both forms of the name are too long.
C
            CALL SETMSG ( 'Kernel variable FRAME_#_# has length #; '//
     .                    'kernel variable FRAME_#_# has length #; '//
     .                    'maximum allowed length is #.  Neither '  //
     .                    'variable could be searched for in the '  //
     .                    'kernel pool due to these name length '   //
     .                    'errors.'                                 )
            CALL ERRINT ( '#',  FRCODE                              )
            CALL ERRCH  ( '#',  ITEM                                )
            CALL ERRINT ( '#',  REQNUM                              )
            CALL ERRCH  ( '#',  FRNAME                              )
            CALL ERRCH  ( '#',  ITEM                                )
            CALL ERRINT ( '#',  REQNAM                              )
            CALL ERRINT ( '#',  KVNMLN                              )
            CALL SIGERR ( 'SPICE(VARNAMETOOLONG)'                   )
            CALL CHKOUT ( 'ZZDYNVAC'                                )
            RETURN


         ELSE IF  ( REQNAM .GT. KVNMLN ) THEN
C
C           We couldn't find the variable having the ID-based name,
C           and the frame name-based variable name is too long to
C           look up.
C
C           Note that at this point KVNAME contains the ID-based
C           kernel variable name.
C
            CALL SETMSG ( 'Kernel variable # was expected to '      //
     .                    'be present in the kernel pool but '      //
     .                    'was not found.  The alternative '        //
     .                    'form of kernel variable name '           //
     .                    'FRAME_#_# was not searched for '         //
     .                    'because this name has excessive '        //
     .                    'length (# characters vs allowed '        //
     .                    'maximum of #).  One of these variables ' //
     .                    'is needed to define the parameterized '  //
     .                    'dynamic frame #.  Usually this type '    //
     .                    'of problem is due to an error in a '     //
     .                    'frame definition provided in a frame '   //
     .                    'kernel.'                                ) 
            CALL ERRCH  ( '#',  KVNAME                              )
            CALL ERRCH  ( '#',  FRNAME                              )
            CALL ERRCH  ( '#',  ITEM                                )
            CALL ERRINT ( '#',  REQNAM                              )
            CALL ERRINT ( '#',  KVNMLN                              )
            CALL ERRCH  ( '#',  FRNAME                              )
            CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)'                )
            CALL CHKOUT ( 'ZZDYNVAC'                                )
            RETURN
         
         END IF

C
C        Now try looking for a kernel variable including the frame
C        name.
C
         CALL REPMC  ( TEMPLT, '#',   FRNAME, KVNAME )
         CALL REPMC  ( KVNAME, '#',   ITEM,   KVNAME )
         CALL DTPOOL ( KVNAME, FOUND, N,      DTYPE  )


         IF (  ( .NOT. FOUND ) .AND. ( REQNUM .GT. KVNMLN )  ) THEN
C
C           The kernel variable's presence (in one form or the other)
C           is mandatory:  signal an error.  The error message
C           depends on which variables we were able to try to
C           look up.  In this case, we never tried to look up the
C           frame ID-based name.
C
C           Note that at this point KVNAME contains the name-based
C           kernel variable name.
C
            CALL SETMSG ( 'Kernel variable # was expected to '      //
     .                    'be present in the kernel pool but '      //
     .                    'was not found.  The alternative '        //
     .                    'form of kernel variable name '           //
     .                    'FRAME_#_# was not searched for '         //
     .                    'because this name has excessive '        //
     .                    'length (# characters vs allowed '        //
     .                    'maximum of #).  One of these variables ' //
     .                    'is needed to define the parameterized '  //
     .                    'dynamic frame #.  Usually this type '    //
     .                    'of problem is due to an error in a '     //
     .                    'frame definition provided in a frame '   //
     .                    'kernel.'                                 ) 
            CALL ERRCH  ( '#',  KVNAME                              )
            CALL ERRINT ( '#',  FRCODE                              )
            CALL ERRCH  ( '#',  ITEM                                )
            CALL ERRINT ( '#',  REQNUM                              )
            CALL ERRINT ( '#',  KVNMLN                              )
            CALL ERRCH  ( '#',  FRNAME                              )
            CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)'                )
            CALL CHKOUT ( 'ZZDYNVAC'                                )
            RETURN
         

         ELSE IF ( .NOT. FOUND ) THEN
C
C           We tried to look up both names and failed.
C
            CALL SETMSG ( 'At least one of the kernel variables '   //
     .                    'FRAME_#_# or FRAME_#_# was expected to ' //
     .                    'be present in the kernel pool but '      //
     .                    'neither was found. One of these '        // 
     .                    'variables is needed to define the '      //
     .                    'parameterized dynamic frame #.  Usually '//
     .                    'this type of problem is due to a '       //
     .                    'missing keyword assignment in a frame '  //
     .                    'kernel.  Another, less likely, '         //
     .                    'possibility is that other errors in a '  //
     .                    'frame kernel have confused the '         //
     .                    'frame subsystem into wrongly '           //
     .                    'deciding these variables are needed.'    )
            CALL ERRINT ( '#',  FRCODE                              )
            CALL ERRCH  ( '#',  ITEM                                )
            CALL ERRCH  ( '#',  FRNAME                              )
            CALL ERRCH  ( '#',  ITEM                                )
            CALL ERRCH  ( '#',  FRNAME                              )
            CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)'                )
            CALL CHKOUT ( 'ZZDYNVAC'                                )
            RETURN
 
         END IF

      END IF

C
C     Getting to this point means we found a kernel variable. The name
C     of the variable is KVNAME.  The data type is DTYPE and the
C     cardinality is N.
C
C     Rather than using BADKPV, we check the data type and cardinality
C     of the kernel variable in-line so we can create a more detailed
C     error message if need be.
C
      IF ( DTYPE .EQ. 'N' ) THEN

         CALL SETMSG ( 'The kernel variable # has used to define '  //
     .                 'frame # was expected to have character '    //
     .                 'data type but in fact has numeric '         //
     .                 'data type.  Usually this type of '          //
     .                 'problem is due to an error in a frame '     //
     .                 'definition provided in a frame kernel.'     )
         CALL ERRCH  ( '#',  KVNAME                                 )
         CALL ERRCH  ( '#',  FRNAME                                 )
         CALL SIGERR ( 'SPICE(BADVARIABLETYPE)'                     )
         CALL CHKOUT ( 'ZZDYNVAC'                                   )
         RETURN
 
      END IF


      IF ( N .GT. MAXN ) THEN

         CALL SETMSG ( 'The kernel variable # has used to define '  //
     .                 'frame # was expected to have size not '     //
     .                 'exceeding # but in fact has size #. '       //
     .                 'Usually this type of '                      //
     .                 'problem is due to an error in a frame '     //
     .                 'definition provided in a frame kernel.'     )
         CALL ERRCH  ( '#',  KVNAME                                 )
         CALL ERRCH  ( '#',  FRNAME                                 )
         CALL ERRINT ( '#',  MAXN                                   )
         CALL ERRINT ( '#',  N                                      )
         CALL SIGERR ( 'SPICE(BADVARIABLESIZE)'                     )
         CALL CHKOUT ( 'ZZDYNVAC'                                   )
         RETURN

      END IF

C
C     Look up the kernel variable.
C
      CALL GCPOOL ( KVNAME, 1, MAXN, N, VALUES, FOUND )
     
      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'Variable # not found after DTPOOL ' //
     .                 'indicated it was present in pool.'  )
         CALL ERRCH  ( '#',  KVNAME                         )
         CALL SIGERR ( 'SPICE(BUG)'                         )
         CALL CHKOUT ( 'ZZDYNVAC'                           )
         RETURN

      END IF

      CALL CHKOUT ( 'ZZDYNVAC' )
      RETURN
      END
