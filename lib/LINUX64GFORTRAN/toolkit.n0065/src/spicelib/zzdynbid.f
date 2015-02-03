C$Procedure ZZDYNBID ( Fetch body ID kernel variable )

      SUBROUTINE ZZDYNBID ( FRNAME, FRCODE, ITEM, IDCODE )
      IMPLICIT NONE 
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Look up a frame definition kernel variable whose associated value
C     is a body name or body ID code.  The returned value is always an
C     ID code.  The frame name or frame ID may be used as part of the
C     variable's name.
C
C     If the kernel variable is not present, or if the variable
C     is not a body name or a numeric value, signal an error.
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
 
      INCLUDE 'zzbodtrn.inc'
      INCLUDE 'zzdyn.inc'
     
      CHARACTER*(*)         FRNAME
      INTEGER               FRCODE
      CHARACTER*(*)         ITEM      
      INTEGER               IDCODE
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  -------------------------------------------------
C     FRNAME     I   Frame name.
C     FRCODE     I   Frame ID code.
C     ITEM       I   Item associated with frame definition.
C     IDCODE     O   Body ID code.
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
C                    this routine will look for a numeric variable
C                    of that name first.
C
C                    The value associated with the kernel variable
C                    must be one of
C
C                       - a nbody ID code 
C 
C                       - a string representation of an integer,
C                         for example '5'
C
C                       - a body frame name
C
C$ Detailed_Output
C
C     IDCODE         is the requested body ID code.  
C
C                    The kernel variable name of the form
C
C                       FRAME_<frame ID code>_<ITEM>
C
C                    will be looked up first; if this variable
C                    is found and has numeric type, the associated
C                    value will be returned.  If this variable is
C                    found and has character type, the value will
C                    be converted to a body ID code, and that
C                    code will be returned.
C
C                    If this variable is not found, the variable
C
C                       FRAME_<frame name>_<ITEM>
C
C                    will be looked up.  If this variable is found and
C                    has numeric type, the associated value will be
C                    returned.  If this variable is found and has
C                    character type, the value will be converted to a
C                    body ID code, and that code will be returned.
C
C                    If a numeric value associated with the selected
C                    kernel variable is not integral, it will be
C                    rounded to the closest integer.
C
C$ Parameters
C
C     See zzdyn.inc for definition of KVNMLN.
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
C        that variable will not be searched for.
C
C     3) If both the frame-ID-based and frame-name-based forms of the 
C        requested kernel variable name have length greater than KVNMLN,
C        the error SPICE(VARNAMETOOLONG) will be signaled.
C        
C     4) If kernel variable matching one form of the requested kernel
C        variable names is found, but that variable has more than 1
C        associated value, the error SPICE(BADVARIABLESIZE) will be 
C        signaled.
C
C     5) If a name match is found for a character kernel variable, but
C        the value associated with the variable cannot be mapped to a
C        body ID code, the error SPICE(NOTRANSLATION) will be
C        signaled.
C
C     6) If a name match is found for a numeric kernel variable,
C        but that variable has a value that cannot be rounded to an
C        integer representable on the host platform, an error will 
C        be signaled by a routine in the call tree of this routine.
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
C     access routines, these names are distinct.  Hence kernel
C     variables having names of both forms, but having possibly
C     different attributes, can be simultaneously present in the kernel
C     pool. Intentional use of this kernel pool feature is discouraged.
C
C$ Examples
C
C     1) See ZZDYNFRM.
C
C     2) Applications of this routine include finding ID codes of 
C        observer or target bodies serving to define two-vector dynamic
C        frames.
C
C$ Restrictions
C
C     1) This is a SPICE private routine; the routine is subject
C        to change without notice.  User applications should not
C        call this routine.
C
C     2) An array-valued kernel variable matching the "ID code form"
C        of the requested kernel variable name could potentially
C        mask a scalar-valued kernel variable matching the "name
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
C-    SPICELIB Version 2.0.0, 05-AUG-2005 (NJB) 
C
C        References to parameterized dynamic frames in long error
C        messages were changed to references to "reference frames."
C        This change was made to enable this utility to support
C        kernel variable look-ups for non-dynamic frames.
C
C-    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB) 
C
C-&

C$ Revisions
C
C-    SPICELIB Version 2.0.0, 05-AUG-2005 (NJB) 
C
C        References to parameterized dynamic frames in long error
C        messages were changed to references to "reference frames."
C        This change was made to enable this utility to support
C        kernel variable look-ups for non-dynamic frames.
C
C-&

C
C     SPICELIB functions
C
      INTEGER               RTRIM

      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
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
C     Local variables
C
      CHARACTER*(BDNMLN)    BODNAM
      CHARACTER*(KVNMLN)    CDESTR
      CHARACTER*(1)         DTYPE
      CHARACTER*(KVNMLN)    KVNAME

      INTEGER               CODELN
      INTEGER               ITEMLN
      INTEGER               N
      INTEGER               NAMELN
      INTEGER               REQNAM
      INTEGER               REQNUM

      LOGICAL               FOUND


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZDYNBID' )

C
C     Prepare to check the name of the kernel variable we're about
C     to look up.
C
C     Convert the frame code to a string.
C
      CALL INTSTR ( FRCODE, CDESTR )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZDYNBID' )
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
            CALL CHKOUT ( 'ZZDYNBID'                                )
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
     .                    'is needed to define the reference '      //
     .                    'frame #.  Usually '                      //
     .                    'this type of problem is due to a '       //
     .                    'missing keyword assignment in a frame '  //
     .                    'kernel.  Another, less likely, '         //
     .                    'possibility is that other errors in a '  //
     .                    'frame kernel have confused the '         //
     .                    'frame subsystem into wrongly '           //
     .                    'deciding these variables are needed.'    )
            CALL ERRCH  ( '#',  KVNAME                              )
            CALL ERRCH  ( '#',  FRNAME                              )
            CALL ERRCH  ( '#',  ITEM                                )
            CALL ERRINT ( '#',  REQNAM                              )
            CALL ERRINT ( '#',  KVNMLN                              )
            CALL ERRCH  ( '#',  FRNAME                              )
            CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)'                )
            CALL CHKOUT ( 'ZZDYNBID'                                )
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
     .                    'is needed to define the reference '      //
     .                    'frame #.  Usually '                      //
     .                    'this type of problem is due to a '       //
     .                    'missing keyword assignment in a frame '  //
     .                    'kernel.  Another, less likely, '         //
     .                    'possibility is that other errors in a '  //
     .                    'frame kernel have confused the '         //
     .                    'frame subsystem into wrongly '           //
     .                    'deciding these variables are needed.'    )
            CALL ERRCH  ( '#',  KVNAME                              )
            CALL ERRINT ( '#',  FRCODE                              )
            CALL ERRCH  ( '#',  ITEM                                )
            CALL ERRINT ( '#',  REQNUM                              )
            CALL ERRINT ( '#',  KVNMLN                              )
            CALL ERRCH  ( '#',  FRNAME                              )
            CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)'                )
            CALL CHKOUT ( 'ZZDYNBID'                                )
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
     .                    'reference frame #.  Usually '            //
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
            CALL CHKOUT ( 'ZZDYNBID'                                )
            RETURN
 
         END IF

      END IF

C
C     Getting to this point means we found a kernel variable. The name
C     of the variable is KVNAME.  The data type is DTYPE and the
C     cardinality is N.
C
      IF ( DTYPE .EQ. 'C' ) THEN
C
C        Rather than using BADKPV, we check the cardinality of the
C        kernel variable in-line so we can create a more detailed error
C        message if need be.
C
         IF ( N .GT. 1 ) THEN

            CALL SETMSG ( 'The kernel variable # has used to define ' //
     .                    'frame # was expected to have size not '    //
     .                    'exceeding 1 but in fact has size #. '      //
     .                    'Usually this type of '                     //
     .                    'problem is due to an error in a frame '    //
     .                    'definition provided in a frame kernel.'    )
            CALL ERRCH  ( '#',  KVNAME                                )
            CALL ERRCH  ( '#',  FRNAME                                )
            CALL ERRINT ( '#',  N                                     )
            CALL SIGERR ( 'SPICE(BADVARIABLESIZE)'                    )
            CALL CHKOUT ( 'ZZDYNBID'                                  )
            RETURN

         END IF

C
C        Look up the kernel variable.
C
         CALL GCPOOL ( KVNAME, 1, 1, N, BODNAM, FOUND )     

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'Variable # not found after DTPOOL ' //
     .                    'indicated it was present in pool.'  )
            CALL ERRCH  ( '#',  KVNAME                         )
            CALL SIGERR ( 'SPICE(BUG)'                         )
            CALL CHKOUT ( 'ZZDYNBID'                           )
            RETURN

         END IF

C
C        Convert the body name to a body code.
C
         CALL BODS2C ( BODNAM, IDCODE, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'Body name # could not be translated ' //
     .                    'to an ID code.'                       )
            CALL ERRCH  ( '#',  BODNAM                           )
            CALL SIGERR ( 'SPICE(NOTRANSLATION)'                 )
            CALL CHKOUT ( 'ZZDYNBID'                             )
            RETURN

         END IF


      ELSE
C
C        The variable has numeric type.
C
         IF ( N .GT. 1 ) THEN

            CALL SETMSG ( 'The kernel variable # has used to define ' //
     .                    'frame # was expected to have size not '    //
     .                    'exceeding 1 but in fact has size #. '      //
     .                    'Usually this type of '                     //
     .                    'problem is due to an error in a frame '    //
     .                    'definition provided in a frame kernel.'    )
            CALL ERRCH  ( '#',  KVNAME                                )
            CALL ERRCH  ( '#',  FRNAME                                )
            CALL ERRINT ( '#',  N                                     )
            CALL SIGERR ( 'SPICE(BADVARIABLESIZE)'                    )
            CALL CHKOUT ( 'ZZDYNBID'                                  )
            RETURN

         END IF

C
C        Look up the kernel variable.
C
         CALL GIPOOL ( KVNAME, 1, 1, N, IDCODE, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'Variable # not found after DTPOOL ' //
     .                    'indicated it was present in pool.'  )
            CALL ERRCH  ( '#',  KVNAME                         )
            CALL SIGERR ( 'SPICE(BUG)'                         )
            CALL CHKOUT ( 'ZZDYNBID'                           )
            RETURN

         END IF

      END IF

      CALL CHKOUT ( 'ZZDYNBID' )
      RETURN
      END


