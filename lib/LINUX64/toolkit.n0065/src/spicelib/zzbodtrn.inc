C$ Abstract
C
C     This include file lists the parameter collection 
C     defining the number of SPICE ID -> NAME mappings.
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
C$ Parameters
C
C     MAXL        is the maximum length of a body name.
C
C     MAXP        is the maximum number of additional names that may
C                 be added via the ZZBODDEF interface.
C
C     NPERM       is the count of the mapping assignments built into
C                 SPICE.
C 
C     MAXE        is the size of the lists and hashes storing combined
C                 built-in and ZZBODDEF-defined name/ID mappings. To
C                 ensure efficient hashing this size is the set to the
C                 first prime number greater than ( MAXP + NPERM ).
C
C     NROOM       is the size of the lists and hashes storing the
C                 POOL-defined name/ID mappings. To ensure efficient
C                 hashing and to provide the ability to store nearly as
C                 many names as can fit in the POOL, this size is
C                 set to the first prime number less than MAXLIN
C                 defined in the POOL umbrella routine.
C
C$ Required_Reading
C
C     naif_ids.req
C
C$ Keywords
C
C     BODY
C     CONVERSION
C
C$ Author_and_Institution
C
C     B.V. Semenov (JPL)
C     E.D. Wright  (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 07-MAY-2014 (BVS)(EDW)
C
C        Increased NROOM to 14983. Added a comment note explaining 
C        NROOM and MAXE 
C
C-    SPICELIB Version 1.0.0, 20-MAY-2010 (EDW)
C
C        N0064 version with MAXP = 150, NPERM = 563, 
C        MAXE = MAXP + NPERM, and NROOM = 2000.


C
C     A script generates this file. Do not edit by hand.
C     Edit the creation script to modify the contents of
C     ZZBODTRN.INC.
C

C
C     Maximum size of a NAME string
C
      INTEGER               MAXL
      PARAMETER           ( MAXL  =  36 )

C
C     Maximum number of additional names that may be added via the
C     ZZBODDEF interface.
C  
      INTEGER               MAXP
      PARAMETER           ( MAXP  =  150 )

C
C     Count of default SPICE mapping assignments.
C 
      INTEGER               NPERM
      PARAMETER           ( NPERM = 592 )

C
C     Size of the lists and hashes storing the built-in and
C     ZZBODDEF-defined name/ID mappings. To ensure efficient hashing
C     this size is the set to the first prime number greater than
C     ( MAXP + NPERM ).
C
      INTEGER               MAXE
      PARAMETER           ( MAXE  = 743 )

C
C     Size of the lists and hashes storing the POOL-defined name/ID
C     mappings. To ensure efficient hashing and to provide the ability
C     to store nearly as many names as can fit in the POOL, this size
C     is set to the first prime number less than MAXLIN defined in 
C     the POOL umbrella routine.
C
      INTEGER               NROOM
      PARAMETER           ( NROOM = 14983 )

