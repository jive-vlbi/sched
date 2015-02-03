C$Procedure KPLFRM ( Kernel pool frame IDs )
 
      SUBROUTINE KPLFRM ( FRMCLS, IDSET )
 
C$ Abstract
C
C     Return a SPICE set containing the frame IDs of all reference
C     frames of a given class having specifications in the kernel pool.
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
C     CELLS
C     FRAMES
C     KERNEL
C     NAIF_IDS
C     SETS
C
C$ Keywords
C
C     FRAME
C     SET
C     UTILITY
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'frmtyp.inc'
      INCLUDE 'ninert.inc'
      INCLUDE 'nninrt.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               FRMCLS
      INTEGER               IDSET  ( LBCELL : * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FRMCLS     I   Frame class.
C     IDSET      O   Set of ID codes of frames of the specified class.
C
C$ Detailed_Input
C
C     FRMCLS         is an integer code specifying the frame class or
C                    classes for which frame ID codes are requested.
C                    The applicable reference frames are those having
C                    specifications present in the kernel pool.
C
C                    FRMCLS may designate a single class or "all
C                    classes."
C
C                    The include file frmtyp.inc declares parameters
C                    identifying frame classes. The supported values
C                    and corresponding meanings of FRMCLS are
C
C                       Parameter      Value    Meaning
C                       =========      =====    =================
C                       ALL              -1     All frame classes
C                                               specified in the 
C                                               kernel pool. Class 1
C                                               is not included.
C
C                       INERTL            1     Built-in inertial.
C                                               No frames will be 
C                                               returned in the 
C                                               output set.
C                                               
C                       PCK               2     PCK-based frame
C
C                       CK                3     CK-based frame
C
C                       TK                4     Fixed rotational
C                                               offset ("text
C                                               kernel") frame
C
C                       DYN               5     Dynamic frame
C
C$ Detailed_Output
C
C     IDSET          is a SPICE set containing the ID codes of all
C                    reference frames having specifications present in
C                    the kernel pool and belonging to the specified
C                    class or classes.
C
C                    Note that if FRMCLS is set to INERTL, IDSET
C                    will be empty on output.
C
C$ Parameters
C
C     See the INCLUDE file frmtyp.inc.
C
C$ Exceptions
C
C     1)  If the input frame class argument is not defined in
C         frmtyp.inc, the error SPICE(BADFRAMECLASS) is signaled.
C
C     2)  If the size of IDSET is too small to hold the requested frame
C         ID set, the error SPICE(SETTOOSMALL) is signaled.
C
C     3)  Frames of class 1 may not be specified in the kernel pool.
C         However, for the convenience of users, this routine does not
C         signal an error if the input class is set to INERTL. In this
C         case the output set will be empty.
C
C     4)  This routine relies on the presence of just three kernel
C         variable assignments for a reference frame in order to
C         determine that that reference frame has been specified:
C
C           FRAME_<frame name>       = <ID code>
C           FRAME_<ID code>_NAME     = <frame name>
C
C        and either
C
C           FRAME_<ID code>_CLASS    = <class>
C
C        or
C
C           FRAME_<frame name>_CLASS = <class>
C
C        It is possible for the presence of an incomplete frame 
C        specification to trick this routine into incorrectly 
C        deciding that a frame has been specified. This routine
C        does not attempt to diagnose this problem.
C
C$ Files
C
C     1) Reference frame specifications for frames that are not
C        built in are typically established by loading frame kernels.
C
C$ Particulars
C
C     This routine enables SPICE-based applications to conveniently
C     find the frame ID codes of reference frames having specifications
C     present in the kernel pool. Such frame specifications are 
C     introduced into the kernel pool either by loading frame kernels
C     or by means of calls to the kernel pool "put" API routines
C 
C        PCPOOL
C        PDPOOL
C        PIPOOL
C     
C     Given a reference frame's ID code, other attributes of the
C     frame can be obtained via calls to entry points of the 
C     umbrella routine FRAMEX:
C
C        FRMNAM {Return a frame's name}
C        FRINFO {Return a frame's center, class, and class ID}
C
C     This routine has a counterpart 
C
C        BLTFRM
C
C     which fetches the frame IDs of all built-in reference frames.
C     
C$ Examples
C
C     1)  Display the IDs and names of all reference frames having
C         specifications present in the kernel pool. Group the outputs
C         by frame class. Also fetch and display the entire set of IDs
C         and names using the parameter ALL.
C
C         The meta-kernel used for this example is shown below. The
C         Rosetta kernels referenced by the meta-kernel are available
C         in the path
C
C            pub/naif/ROSETTA/kernels/fk
C
C         on the NAIF server. Older, but officially archived versions
C         of these kernels are available in the path
C
C            pub/naif/pds/data/ros-e_m_a_c-spice-6-v1.0/
C            rossp_1000/DATA/FK
C
C         The referenced PCK is available from the pck path under the
C         generic_kernels path on the same server.
C
C
C            KPL/MK
C
C            \begindata
C
C               KERNELS_TO_LOAD = ( 'pck00010.tpc'
C                                   'EARTHFIXEDITRF93.TF'
C                                   'ROS_LUTETIA_RSOC_V03.TF'
C                                   'ROS_V18.TF'
C                                   'RSSD0002.TF'            )
C            \begintext
C
C
C         Program source code:
C
C
C                PROGRAM EX1
C                IMPLICIT NONE
C
C                INCLUDE 'frmtyp.inc'
C          C
C          C     SPICELIB functions
C          C
C                INTEGER               CARDI
C          C
C          C     Local parameters
C          C
C                CHARACTER*(*)         META
C                PARAMETER           ( META   = 'kplfrm.tm' )
C
C                INTEGER               NFRAME
C                PARAMETER           ( NFRAME = 1000 )
C
C                INTEGER               LBCELL
C                PARAMETER           ( LBCELL = -5 )
C
C                INTEGER               LNSIZE
C                PARAMETER           ( LNSIZE = 80 )
C
C                INTEGER               FRNMLN
C                PARAMETER           ( FRNMLN = 32 )
C
C          C
C          C     Local variables
C          C
C                CHARACTER*(FRNMLN)    FRNAME
C                CHARACTER*(LNSIZE)    OUTLIN
C
C                INTEGER               I
C                INTEGER               IDSET ( LBCELL : NFRAME )
C                INTEGER               J
C
C          C
C          C     Initialize the frame set.
C          C
C                CALL SSIZEI ( NFRAME, IDSET )
C
C          C
C          C     Load kernels that contain frame specifications.
C          C
C                CALL FURNSH ( META )
C
C          C
C          C     Fetch and display the frames of each class.
C          C
C                DO I = 1, 6
C
C                   IF ( I .LT. 6 ) THEN
C          C
C          C           Fetch the frames of class I.
C          C
C                      CALL KPLFRM ( I, IDSET )
C
C                      OUTLIN = 'Number of frames of class #: #'
C                      CALL REPMI ( OUTLIN, '#', I,            OUTLIN )
C                      CALL REPMI ( OUTLIN, '#', CARDI(IDSET), OUTLIN )
C
C                   ELSE
C          C
C          C           Fetch IDs of all frames specified in the kernel
C          C           pool.
C          C
C                      CALL KPLFRM ( ALL, IDSET )
C
C                      OUTLIN = 'Number of frames in the kernel pool: #'
C                      CALL REPMI ( OUTLIN, '#', CARDI(IDSET), OUTLIN )
C
C                   END IF
C
C                   CALL TOSTDO ( ' '    )
C                   CALL TOSTDO ( OUTLIN )
C                   CALL TOSTDO ( '   Frame IDs and names' )
C
C                   DO J = 1, CARDI(IDSET)
C                      CALL FRMNAM ( IDSET(J), FRNAME )
C                      WRITE (*,*) IDSET(J), '  ', FRNAME
C                   END DO
C
C                END DO
C
C                END
C
C
C         The output from the program, when the program was linked
C         against the N0064 SPICE Toolkit, is shown below. The output
C         shown here has been abbreviated.
C
C
C            Number of frames of class 1: 0
C               Frame IDs and names
C
C            Number of frames of class 2: 3
C               Frame IDs and names
C                 1000012   67P/C-G_FIXED
C                 2000021   LUTETIA_FIXED
C                 2002867   STEINS_FIXED
C
C            Number of frames of class 3: 7
C               Frame IDs and names
C                 -226570   ROS_RPC_BOOM2
C                 -226215   ROS_VIRTIS-M_SCAN
C                 -226072   ROS_HGA_AZ
C                 -226071   ROS_HGA_EL
C                 -226025   ROS_SA-Y
C                 -226015   ROS_SA+Y
C                 -226000   ROS_SPACECRAFT
C
C            Number of frames of class 4: 64
C               Frame IDs and names
C                -2260021   ROS_LUTETIA
C                 -226999   ROSLND_LOCAL_LEVEL
C                 -226900   ROSLND_LANDER
C                 -226560   ROS_RPC_BOOM1
C
C                    ...
C
C                 -226030   ROS_MGA-S
C                 -226020   ROS_SA-Y_ZERO
C                 -226010   ROS_SA+Y_ZERO
C                 1502010   HCI
C                 1502301   LME2000
C                 1503299   VME2000
C                 1503499   MME2000
C
C            Number of frames of class 5: 19
C               Frame IDs and names
C                 -226967   2867/STEINS_CSO
C                 -226945   45P/H-M-P_CSO
C                 -226921   21/LUTETIA_CSO
C                 -226920   21/LUTETIA_CSEQ
C                 -226912   67P/C-G_CSO
C                 -226910   67P/C-G_CSEQ
C                 1500010   HEE
C                 1500299   VSO
C                 1500301   LSE
C                 1500399   GSE
C                 1500499   MME
C                 1501010   HEEQ
C                 1501299   VME
C                 1501301   LME
C                 1501399   EME
C                 1501499   MME_IAU2000
C                 1502399   GSEQ
C                 1502499   MSO
C                 1503399   ECLIPDATE
C
C            Number of frames in the kernel pool: 93
C               Frame IDs and names
C                -2260021   ROS_LUTETIA
C                 -226999   ROSLND_LOCAL_LEVEL
C                 -226967   2867/STEINS_CSO
C                 -226945   45P/H-M-P_CSO
C                 -226921   21/LUTETIA_CSO
C
C                    ...
C
C                 1503299   VME2000
C                 1503399   ECLIPDATE
C                 1503499   MME2000
C                 2000021   LUTETIA_FIXED
C                 2002867   STEINS_FIXED
C
C
C$ Restrictions
C
C     1) This routine will work correctly if the kernel pool
C        contains no invalid frame specifications. See the
C        description of exception 4 above. Users must ensure
C        that no invalid frame specifications are introduced
C        into the kernel pool, either by loaded kernels or
C        by means of the kernel pool "put" APIs.        
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
C-    SPICELIB Version 1.0.0, 22-MAY-2012 (NJB)
C
C-&
 
C$ Index_Entries
C
C     fetch IDs of reference_frames from the kernel_pool
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               SIZEI
      LOGICAL               RETURN

C
C     Local parameters
C      
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               KVNMLN
      PARAMETER           ( KVNMLN = 32 )

      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 100 )

C
C     Local variables
C
      CHARACTER*(FRNMLN)    FRNAME
      CHARACTER*(KVNMLN)    KVBUFF ( BUFSIZ )
      CHARACTER*(KVNMLN)    KVNAME
      CHARACTER*(KVNMLN)    KVTEMP
      CHARACTER*(KVNMLN)    KVCODE
      CHARACTER*(KVNMLN)    KVCLAS
      CHARACTER*(FRNMLN)    TMPNAM
      
      INTEGER               FCLASS
      INTEGER               I
      INTEGER               IDCODE
      INTEGER               L
      INTEGER               M
      INTEGER               N
      INTEGER               TO
      INTEGER               W

      LOGICAL               FOUND
      
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'KPLFRM' )

C
C     The output set starts out empty.
C
      CALL SCARDI ( 0, IDSET )

C
C     Check the input frame class. 
C
C     This block of code must be kept in sync with frmtyp.inc.
C
      IF  (      ( FRMCLS .GT. DYN )
     .      .OR. ( FRMCLS .EQ. 0   )
     .      .OR. ( FRMCLS .LT. ALL )  ) THEN

         CALL SETMSG ( 'Frame class specifier FRMCLS was #; this '
     .   //            'value is not supported.'                  )
         CALL ERRINT ( '#',  FRMCLS                               )
         CALL SIGERR ( 'SPICE(BADFRAMECLASS)'                     )
         CALL CHKOUT ( 'KPLFRM'                                   )
         RETURN

      END IF

C
C     Initialize the output buffer index. The
C     index is to be incremented prior to each
C     write to the buffer.
C
      TO = 0

C
C     Find all of the kernel variables having names
C     that could correspond to frame name assignments.
C
C     We expect that all frame specifications will
C     include assignments of the form
C
C         FRAME_<ID code>_NAME = <frame name>
C
C     We may pick up some additional assignments that are not part of
C     frame specifications; we plan to filter out as many as possible
C     by looking the corresponding frame ID and frame class
C     assignments.
C
      KVTEMP = 'FRAME_*_NAME'
      
      CALL GNPOOL ( KVTEMP, 1, BUFSIZ, N, KVBUFF, FOUND )

      DO WHILE ( N .GT. 0 )
C
C        At least one kernel variable was found by the last
C        GNPOOL call. Each of these variables is a possible
C        frame name. Look up each of these candidate names.
C
         DO I = 1, N
C
C           Attempt to fetch the right hand side value for
C           the Ith kernel variable found on the previous
C           GNPOOL call.
C
            CALL GCPOOL ( KVBUFF(I), 1, 1, M, FRNAME, FOUND )

            IF ( FOUND ) THEN
C
C              We found a possible frame name. Attempt to look 
C              up an ID code variable for the name. The assignment
C              for the ID code, if present, will have the form
C
C                 FRAME_<name> = <ID code>
C            
C              Create the kernel variable name on the left hand
C              side of the assignment.
C              
               KVCODE = 'FRAME_<name>'

               CALL REPMC ( KVCODE, '<name>', FRNAME, KVCODE )
               
C
C              Try to fetch the ID code.
C
               CALL GIPOOL ( KVCODE, 1, 1, L, IDCODE, FOUND )

               IF ( FOUND ) THEN
C
C                 We found an integer on the right hand side
C                 of the assignment. We probably have a 
C                 frame specification at this point. Check that
C                 the variable
C
C                    FRAME_<ID code>_NAME 
C
C                 is present in the kernel pool and maps to 
C                 the name FRNAME.
C
                  KVNAME = 'FRAME_<code>_NAME'

                  CALL REPMI ( KVNAME, '<code>', IDCODE, KVNAME )
                  
                  CALL GCPOOL ( KVNAME, 1, 1, W, TMPNAM, FOUND )


                  IF ( FOUND ) THEN
C
C                    Try to look up the frame class using a 
C                    kernel variable name of the form
C
C                       FRAME_<integer ID code>_CLASS
C
C                    Create the kernel variable name on the left
C                    hand side of the frame class assignment.
C                 
                     KVCLAS = 'FRAME_<integer>_CLASS'

                     CALL REPMI ( KVCLAS, '<integer>', IDCODE, KVCLAS )

C
C                    Look for the frame class.
C
                     CALL GIPOOL ( KVCLAS, 1, 1, W, FCLASS, FOUND )

                     IF ( .NOT. FOUND ) THEN
C
C                       Try to look up the frame class using a kernel
C                       variable name of the form
C  
C                          FRAME_<frame name>_CLASS
C
                        KVCLAS = 'FRAME_<name>_CLASS'

                        CALL REPMC ( KVCLAS, '<name>', FRNAME, KVCLAS )

                        CALL GIPOOL ( KVCLAS, 1, 1, W, FCLASS, FOUND )

                     END IF

C
C                    At this point FOUND indicates whether we found
C                    the frame class.
C                 
                     IF ( FOUND ) THEN
C
C                       Check whether the frame class is one
C                       we want. 
C
                        IF (      ( FRMCLS .EQ. ALL    )
     .                       .OR. ( FRMCLS .EQ. FCLASS )  ) THEN
C
C                          We have a winner. Add it to the output set.
C
C                          First make sure the set is large enough to
C                          hold another element.
C
                           IF ( TO .EQ. SIZEI(IDSET) ) THEN

                              CALL SETMSG ( 'Frame ID set argument '
     .                        //            'IDSET has size #; required'
     .                        //            ' size is at least #. Make '
     .                        //            'sure that the caller of '
     .                        //            'this routine has initial'
     .                        //            'ized IDSET via SSIZEI.'   )
                              CALL ERRINT ( '#', SIZEI(IDSET)          )
                              CALL ERRINT ( '#', TO+1                  )
                              CALL SIGERR ( 'SPICE(SETTOOSMALL)'       )
                              CALL CHKOUT ( 'KPLFRM'                   )
                              RETURN

                           END IF

                           TO        = TO + 1
                           IDSET(TO) = IDCODE

                        END IF
C
C                       End of IF block for processing a frame having
C                       a frame class matching the request.
C
                     END IF
C
C                    End of IF block for finding the frame class.
C
                  END IF
C
C                 End of IF block for finding the frame name.
C
               END IF
C
C              End of IF block for finding the frame ID.
C
            END IF
C
C           End of IF block for finding string value corresponding to
C           the Ith kernel variable matching the name template.
C
         END DO
C
C        End of loop for processing last batch of potential
C        frame names.
C
C        Fetch next batch of potential frame names.
C
         CALL GNPOOL ( KVTEMP, N+1, BUFSIZ, N, KVBUFF, FOUND )

      END DO
C
C     At this point all kernel variables that matched the frame name
C     keyword template have been processed. All frames of the specified
C     class or classes have had their ID codes appended to IDSET. In
C     general IDSET is not yet a SPICELIB set, since it's not sorted
C     and it may contain duplicate values.
C
C     Turn IDSET into a set. VALIDI sorts and removes duplicates.
C
      CALL VALIDI ( SIZEI(IDSET), TO, IDSET )

      CALL CHKOUT ( 'KPLFRM' )
      RETURN
      END

