       identification division.
       author. Qayyam Jamal, Menu k, Aadithkeshev.                     
       program-id. DATASPLITCOUNT.

       environment division.
       input-output section.

       file-control.
              select input-file
                   assign to "../../valid.out"
                   organization is line sequential.

           select sl-records-file
               assign to "../../../slrecord.data"
               organization is line sequential.

           select returns-file
               assign to "../../../returns.data"
               organization is line sequential.
                      
           select cc-totals-report
               assign to "../../../counts-controls-total.out"
               organization is line sequential.                             
                      
       data division.
       file section.

       fd input-file
           record contains 36 characters
           data record is data-record.

       01 data-record.
         05 dr-trans-code              pic x.
         05 dr-trans-amount            pic 9(5)V99.
         05 dr-trans-amount            pic 9(5)V99. 
         05 dr-pay-type                pic xx.
         05 dr-store-num               pic xx.
         05 dr-invoice-num             pic x(9).
         05 dr-sku-code                pic x(15).

       fd sl-records-file
          data record is slr-output
          record contains 36 characters.

       01 slr-output pic x(36).

       fd returns-file
          data record is returns-output
          record contains 36 characters.

       01 returns-output pic x(36).

       fd cc-totals-report
          data record is cc-total-output
          record contains 100 characters.

       01 cc-total-output           pic x(100).



                            
                            
       working-storage section.

       01 ws-eof-flag                      pic x
               value "N".

       01 ws-transaction-code                 pic x
               value spaces.
         88 ws-trans-sale
               value "S".
         88 ws-trans-layaway
               value "L".
         88 ws-trans-return
               value "R".


       procedure division.
       000-main.

           open input input-file.
           open output valid-file, invalid-file, errors-file.

           close input-file, valid-file, invalid-file, errors-file.
           goback.
           
       end program DATASPLITCOUNT.
