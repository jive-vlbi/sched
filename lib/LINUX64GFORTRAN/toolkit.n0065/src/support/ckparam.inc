C$ Abstract
C
C     Declarations of the CK data type specific and general CK low
C     level routine parameters.
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
C     CK.REQ
C
C$ Keywords
C
C     CK
C
C$ Restrictions
C
C     1) If new CK types are added, the size of the record passed 
C        between CKRxx and CKExx must be registered as separate 
C        parameter. If this size will be greater than current value 
C        of the CKMRSZ parameter (which specifies the maximum record 
C        size for the record buffer used inside CKPFS) then it should
C        be assigned to CKMRSZ as a new value.
C
C$ Author_and_Institution
C
C     N.J. Bachman      (JPL)
C     B.V. Semenov      (JPL)
C
C$ Literature_References
C
C     CK Required Reading.
C
C$ Version
C
C-    SPICELIB Version 3.0.0, 27-JAN-2014 (NJB)
C
C        Updated to support CK type 6. Maximum degree for 
C        type 5 was updated to be consistent with the 
C        maximum degree for type 6.
C        
C-    SPICELIB Version 2.0.0, 19-AUG-2002 (NJB)
C
C        Updated to support CK type 5.
C
C-    SPICELIB Version 1.0.0, 05-APR-1999 (BVS)
C
C-&

C
C     Number of quaternion components and number of quaternion and 
C     angular rate components together.
C
      INTEGER               QSIZ
      PARAMETER           ( QSIZ   = 4 )
      
      INTEGER               QAVSIZ
      PARAMETER           ( QAVSIZ = 7 )

C
C     CK Type 1 parameters:
C
C     CK1DTP   CK data type 1 ID;
C
C     CK1RSZ   maximum size of a record passed between CKR01
C              and CKE01.
C
      INTEGER               CK1DTP
      PARAMETER           ( CK1DTP = 1 )

      INTEGER               CK1RSZ
      PARAMETER           ( CK1RSZ = 8 )

C
C     CK Type 2 parameters:
C
C     CK2DTP   CK data type 2 ID;
C
C     CK2RSZ   maximum size of a record passed between CKR02 
C              and CKE02.
C
      INTEGER               CK2DTP
      PARAMETER           ( CK2DTP = 2 )

      INTEGER               CK2RSZ
      PARAMETER           ( CK2RSZ = 10 )


C
C     CK Type 3 parameters:
C
C     CK3DTP   CK data type 3 ID;
C
C     CK3RSZ   maximum size of a record passed between CKR03 
C              and CKE03.
C
      INTEGER               CK3DTP
      PARAMETER           ( CK3DTP = 3 )

      INTEGER               CK3RSZ
      PARAMETER           ( CK3RSZ = 17 )

C
C     CK Type 4 parameters:
C
C     CK4DTP   CK data type 4 ID;
C
C     CK4PCD   parameter defining integer to DP packing schema that 
C              is applied when seven number integer array containing 
C              polynomial degrees for quaternion and angular rate 
C              components packed into a single DP number stored in 
C              actual CK records in a file; the value of must not be
C              changed or compatibility with existing type 4 CK files 
C              will be lost.
C
C     CK4MXD   maximum Chebychev polynomial degree allowed in type 4
C              records; the value of this parameter must never exceed 
C              value of the CK4PCD;
C
C     CK4SFT   number of additional DPs, which are not polynomial 
C              coefficients, located at the beginning of a type 4 
C              CK record that passed between routines CKR04 and CKE04; 
C              
C     CK4RSZ   maximum size of type 4 CK record passed between CKR04 
C              and CKE04; CK4RSZ is computed as follows:
C
C                 CK4RSZ = ( CK4MXD + 1 ) * QAVSIZ + CK4SFT 
C
      INTEGER               CK4DTP
      PARAMETER           ( CK4DTP = 4 )

      DOUBLE PRECISION      CK4PCD
      PARAMETER           ( CK4PCD = 128.D0 )
      
      INTEGER               CK4MXD
      PARAMETER           ( CK4MXD = 18 )
      
      INTEGER               CK4SFT
      PARAMETER           ( CK4SFT = 10 )
      
      INTEGER               CK4RSZ
      PARAMETER           ( CK4RSZ = ( CK4MXD + 1 ) * QAVSIZ + CK4SFT )

C
C     CK Type 5 parameters:
C
C
C     CK5DTP   CK data type 5 ID;
C
C     CK5MXD   maximum polynomial degree allowed in type 5
C              records.
C
C     CK5MET   number of additional DPs, which are not polynomial 
C              coefficients, located at the beginning of a type 5
C              CK record that passed between routines CKR05 and CKE05; 
C     
C     CK5MXP   maximum packet size for any subtype.  Subtype 2 
C              has the greatest packet size, since these packets
C              contain a quaternion, its derivative, an angular
C              velocity vector, and its derivative.  See ck05.inc
C              for a description of the subtypes.
C         
C     CK5RSZ   maximum size of type 5 CK record passed between CKR05
C              and CKE05; CK5RSZ is computed as follows:
C
C                 CK5RSZ = ( CK5MXD + 1 ) * CK5MXP + CK5MET
C
      INTEGER               CK5DTP
      PARAMETER           ( CK5DTP = 5 )

      INTEGER               CK5MXD
      PARAMETER           ( CK5MXD = 23 )

      INTEGER               CK5MET
      PARAMETER           ( CK5MET =  4 )

      INTEGER               CK5MXP
      PARAMETER           ( CK5MXP = 14 )

      INTEGER               CK5RSZ
      PARAMETER           ( CK5RSZ = (CK5MXD + 1) * CK5MXP  +  CK5MET )


C
C     CK Type 6 parameters:
C
C
C     CK6DTP   CK data type 6 ID;
C
C     CK6MXD   maximum polynomial degree allowed in type 6
C              records.
C
C     CK6MET   number of additional DPs, which are not polynomial 
C              coefficients, located at the beginning of a type 6
C              CK record that passed between routines CKR06 and CKE06; 
C     
C     CK6MXP   maximum packet size for any subtype.  Subtype 2 
C              has the greatest packet size, since these packets
C              contain a quaternion, its derivative, an angular
C              velocity vector, and its derivative.  See ck06.inc
C              for a description of the subtypes.
C         
C     CK6RSZ   maximum size of type 6 CK record passed between CKR06
C              and CKE06; CK6RSZ is computed as follows:
C
C                 CK6RSZ = CK6MET + ( CK6MXD + 1 ) * ( CK6PS3 + 1 )
C
C              where CK6PS3 is equal to the parameter CK06PS3 defined
C              in ck06.inc. Note that the subtype having the largest
C              packet size (subtype 2) does not give rise to the
C              largest record size, because that type is Hermite and
C              requires half the window size used by subtype 3 for a
C              given polynomial degree.
C
      INTEGER               CK6DTP
      PARAMETER           ( CK6DTP =  6 )

      INTEGER               CK6MXD
      PARAMETER           ( CK6MXD = 23 )

      INTEGER               CK6MET
      PARAMETER           ( CK6MET =  4 )

C
C     The parameter CK6PS3 must be in sync with C06PS3 defined in
C     ck06.inc.
C
      INTEGER               CK6PS3
      PARAMETER           ( CK6PS3 = 7 )

      INTEGER               CK6RSZ
      PARAMETER           ( CK6RSZ =   ( CK6MXD + 1 ) * ( CK6PS3 + 1 ) 
     .                               +   CK6MET                        )

C
C
C     Maximum record size that can be handled by CKPFS. This value 
C     must be set to the maximum of all CKxRSZ parameters (currently
C     CK5RSZ.)
C
      INTEGER               CKMRSZ
      PARAMETER           ( CKMRSZ = CK5RSZ )

