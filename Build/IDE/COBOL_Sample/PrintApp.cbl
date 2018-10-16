      ******************************************************************
      *                                                                *
      * MODULE NAME    PRINTAPP.CBL                                    *
      *                                                                *
      * STATEMENT      IBM WebSphere Developer for System z            *
      *                5724-L44                                        *
      *                (c) Copyright IBM Corp. 2006                    *
      *                                                                *
      * DISCLAIMER OF WARRANTIES                                       *
      * You may copy, modify, and distribute these samples, or their   *
      * modifications, in any form, internally or as part of your      *
      * application or related documentation. These samples have not   *
      * been tested under all conditions and are provided to you by    *
      * IBM without obligation of support of any kind. IBM PROVIDES    *
      * THESE SAMPLES "AS IS" SUBJECT TO ANY STATUTORY WARRANTIES THAT *
      * CANNOT BE EXCLUDED. IBM MAKES NO WARRANTIES OR CONDITIONS,     *
      * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO, THE   *
      * IMPLIED WARRANTIES OR CONDITIONS OF MERCHANTABILITY, FITNESS   *
      * FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT REGARDING THESE *
      * SAMPLES OR TECHNICAL SUPPORT, IF ANY.                          *
      * You will indemnify IBM or third parties that provide IBM       *
      * products ("Third Parties") from and against any third party    *
      * claim arising out of the use, modification or distribution of  *
      * these samples with your application. You may not use the same  *
      * path name as the original files/modules. You must not alter or *
      * delete any copyright information in the Samples.               *
      *                                                                *
      ******************************************************************

       Identification Division.
       Program-ID.  PRINTAPP.

       Data Division.
       Working-Storage Section.
       01 Work-Parms.
          05 In-Len               PIC S9(4) BINARY.
          05 Char-count           Pic 99 Value ZEROS.
          05 Out-Name             PIC X(100).

          Linkage Section.
       01 Recvd-Parms.
          05 In-name         Pic x(30).


       Procedure Division using Recvd-Parms.
             Move spaces to Out-Name.

             Move 0 to Char-count
             Inspect Function Reverse(In-Name)
                Tallying Char-count For Leading Spaces
             Compute In-Len = 30 - Char-count

             Move "Thanks to " to Out-Name (1:10).
             Move In-name(1:In-Len) to Out-Name(11:In-Len)
             Move " for succeeding!" to Out-Name ((11 + In-Len):16).
             Display Out-name.
             Goback.
