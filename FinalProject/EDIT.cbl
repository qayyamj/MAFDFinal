       identification division.
       author. Qayyam Jamal, Menu k, Aadithkeshev.                     
       program-id. EDIT.

       environment division.
       input-output section.

       file-control.
              select input-file
                   assign to "../../project6.dat"
                   organization is line sequential.

           select valid-file
               assign to "../../../valid.data"
               organization is line sequential.

           select invalid-file
               assign to "../../../invalid.data"
               organization is line sequential.
                      
           select errors-file
               assign to "../../../ErrorReport.out"
               organization is line sequential.                             
                      
       data division.
       file section.

       fd input-file
           record contains 36 characters
           data record is data-record.

       01 data-record.
         05 transaction-code               pic x.
           88 ws-valid-code 
               value "S", "R", "L".
         05 transaction-amount             pic 9(5)V99.
         05 payment-type                   pic xx.
           88 ws-valid-pay-type
               value "CA", "CR", "DB".
         05 store-number                   pic XX.
           88 ws-valid-store-num
               values "01", "02", "03", "04", "05", "12".
         05 invoice-number                 pic x(9).
         05 invoice-number-redefine
               redefines invoice-number.                                           
           10 invoice1 pic x.
             88 ws-valid-invoice1
               values "A", "B", "C", "D", "E".               
           10 invoice2 pic x.
             88 ws-valid-invoice2                         
               values "A", "B", "C", "D", "E".                            
           10 invoice-dash                 pic x.
           10 invoice-numbers              pic 9(6).
             88 valid-nums
               value 100000        thru 900000.
         05 sku-code                       pic x(15).
           88 invalid-sku value spaces.

       fd valid-file
          data record is valid-record
          record contains 36 characters.

       01 valid-record pic x(36).

       fd invalid-file
          data record is invalid-record
          record contains 36 characters.

       01 invalid-record           pic x(36).

       fd errors-file
          data record is errors-record
          record contains 100 characters.

       01 errors-record pic x(100).
                            
                            
       working-storage section.

       01 ws-eof-flag                      pic x
               value "N".





       procedure division.
       000-main.

           open input input-file.
           
           goback.
           
       end program EDIT.
