       identification division.
       program-id. EDIT.
       author. Qayyam Jamal, Menu k, Aadithkeshev.
      **************************************************************************
       environment division.
       input-output section.
       file-control.
           select input-file
               assign to "../../project6.dat"
               organization is line sequential.

           select valid-file
               assign to "../../valid.data"
               organization is line sequential.

           select invalid-file
               assign to "../../invalid.data"
               organization is line sequential.

           select errors-file
               assign to "../../ErrorReport.out"
               organization is line sequential.
      **************************************************************************
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
         05 store-number                   pic xx.
           88 ws-valid-store-num
               values "01", "02", "03", "04", "05", "12".
         05 invoice-number                 pic x(9).
         05 invoice-number-redefine
               redefines invoice-number.
           10 invoice1                     pic x.
             88 ws-valid-invoice1
               values "A", "B", "C", "D", "E".
           10 invoice2                     pic x.
             88 ws-valid-invoice2
               values "A", "B", "C", "D", "E".
           10 invoice-dash                 pic x.
           10 invoice-numbers              pic 9(6).
             88 valid-nums
               value 100000 thru 900000.
         05 sku-code                       pic x(15).
           88 invalid-sku
               value spaces.
      *________________________________________________________________
       fd valid-file
          data record is valid-record
          record contains 36 characters.

       01 valid-record                     pic x(36).
 
       fd invalid-file
          data record is invalid-record
          record contains 36 characters.

       01 invalid-record                   pic x(36).
       fd errors-file
          data record is errors-record
          record contains 100 characters.

       01 errors-record                    pic x(100).


       working-storage section.
       01 ws-eof-flag                      pic x
               value "N".
      *-----------------------------------------------------------------
       01 ws-detail-line.
         05 ws-trans-code                  pic x.
         05 ws-trans-amount                pic 9(5)V99.
         05 ws-pay-type                    pic xx.
         05 ws-store-num                   pic xx.
         05 ws-invoice-num                 pic x(9).
         05 ws-sku-code                    pic x(15).
      *-----------------------------------------------------------------
       01 ws-detail-invalid-line.
         05 ws-invalid-trans-code          pic x.
         05 ws-invalid-trans-amount        pic 9(5)V99.
         05 ws-invalid-pay-type            pic xx.
         05 ws-invalid-store-num           pic xx.
         05 ws-invalid-invoice-num         pic x(9).
         05 ws-invalid-sku-code            pic x(15).
      *-----------------------------------------------------------------
       01 ws-constants.
         05 ws-errors                      pic 99
               value 0.
         05 ws-errors-total                pic 99
               value 0.
         05 ws-invalid-total               pic 99
               value 0.
         05 ws-valid-total                 pic 99
               value 0.
         05 ws-transaction-error           pic x(24)
               value "INVALID TRANSACTION CODE".
         05 ws-transaction-num-error       pic x(36)
               value "TRANSACTION AMOUNT MUST BE NUMERIC".
         05 ws-payment-type-error          pic x(20)
               value "INVALID PAYMENT TYPE".
         05 ws-store-num-error             pic x(20)
               value "INVALID STORE NUMBER".
         05 ws-invoice-format-error         pic x(31)
               value "INVOICE NUMBER INVALID FORMAT".
         05 ws-invoice-alpha-error         pic x(33)
               value "INVOICE NUMBER MUST BE ALPHABETIC".
         05 ws-invoice-repeat-error         pic x(33)
               value "INVOICE NUMBER CANNOT BE REPEATED".
         05 ws-invoice-range-error         pic x(28)
               value "INVOICE NUMBER NOT IN RANGE".
         05 ws-sku-error                   pic x(26)
               value "SKU CODE IS NOT ALPHABETIC".


       01 ws-error-report1.
         05 filler                         pic x(6)
               value "ERRORS".
         05 filler                         pic x(5)
               value spaces.
         05 filler                         pic x(6)
               value "REPORT".
       01 ws-error-report2.
         05 filler                         pic x(17)
               value "-----------------".

       01 ws-error-report-num.
         05 filler                         pic x
               value spaces.
         05 filler                         pic x(5)
               value "Line ".
         05 ws-error-num                   pic ZZ9
               value 0.
         05 filler                         pic x(5)
               value spaces.
         05 ws-error-amount                pic x(40).
         05 ws-error-redef                 redefines
               ws-error-amount occurs 10 times.
           10 ws-error-total-amount        pic x(40).
      *-----------------------------------------------------------------

       01 ws-error-report-records.
         05 filler                         pic x(15)
               value "Total Records: ".
         05 filler                         pic x(5)
               value spaces.
         05 ws-error-report-total          pic ZZ9
               value 0.
      *-----------------------------------------------------------------
       01 ws-error-report-valid.
         05 filler                         pic x(21)
               value "Total Valid Records: ".
         05 filler                         pic x(5)
               value spaces.
         05 ws-error-valid-total           pic ZZ9
               value 0.
      *-----------------------------------------------------------------
       01 ws-error-report-invalid.
         05 filler                         pic x(23)
               value "Total Invalid Records: ".
         05 filler                         pic x(5)
               value spaces.
         05 ws-error-invalid-total         pic ZZ9
               value 0.

       01 ws-sub                           pic 99
               value 0.
      ******************************************************************
       procedure division.
       000-main.
      * Open files
           open input input-file, output valid-file, invalid-file,
           errors-file.

           read input-file
               at end
                   move "Y" to ws-eof-flag.

           perform 100-process-records until ws-eof-flag = "Y".
           perform 300-Write-files.

      * Close files andend program
           close input-file , valid-file , invalid-file , errors-file.
      * Exit program
           display "press enter key to exit...".
           accept return-code.

           goback.




      *-----------------------------------------------------------------
       100-process-records.

           perform 200-validating-data.
      *    stuff go here WIP

           read input-file
               at end
                   move "Y" to ws-eof-flag.

       200-validating-data.

           if not ws-valid-code then
               add 1 to ws-errors
               add 1 to ws-errors-total
               move ws-transaction-error to ws-error-redef(ws-errors)
           end-if.

           if transaction-amount not numeric then
               add 1 to ws-errors
               add 1 to ws-errors-total
               move ws-invalid-trans-amount to ws-error-redef(ws-errors)
           end-if.

           if not ws-valid-pay-type then
               add 1 to ws-errors
               add 1 to ws-errors-total
               move ws-payment-type-error to ws-error-redef(ws-errors)
           end-if.

           if not ws-valid-store-num then
               add 1 to ws-errors
               add 1 to ws-errors-total
               move ws-store-num-error to ws-error-redef(ws-errors)
           end-if.

           if not invoice1 = invoice2 then
               add 1 to ws-errors
               add 1 to ws-errors-total
               move ws-invoice-format-error to ws-error-redef(ws-errors)
           end-if.

           if not invoice-numbers is numeric then
               add 1 to ws-errors
               add 1 to ws-errors-total
               move ws-invoice-repeat-error to ws-error-redef(ws-errors)
           end-if.

           if not ws-valid-invoice1 then
               add 1 to ws-errors
               add 1 to ws-errors-total
               move ws-invoice-alpha-error to ws-error-redef(ws-errors)
           end-if.

           if not ws-valid-invoice2 then
               add 1 to ws-errors
               add 1 to ws-errors-total
               move ws-invoice-alpha-error to ws-error-redef(ws-errors)
           end-if.

           if not valid-nums then
               add 1 to ws-errors
               move ws-invoice-range-error to ws-error-redef(ws-errors)
           end-if.

           if not sku-code = spaces then
               add 1 to ws-errors
               move ws-sku-error to ws-error-redef(ws-errors)
           end-if.



           if ws-errors = 0 then
               add 1                   to ws-valid-total
               move transaction-code   to ws-trans-code
               move transaction-amount to ws-trans-amount
               move payment-type       to ws-pay-type
               move store-number       to ws-store-num
               move invoice-number     to ws-invoice-num
               move sku-code           to ws-sku-code
           else
               add 1 to ws-invalid-total
               move transaction-code   to ws-invalid-trans-code
               move transaction-amount to ws-invalid-trans-amount
               move payment-type       to ws-invalid-pay-type
               move store-number       to ws-invalid-store-num
               move invoice-number     to ws-invalid-invoice-num
               move sku-code           to ws-invalid-sku-code
           end-if.

       300-Write-files.
           move ws-valid-total         to ws-error-valid-total.
           move ws-invalid-total       to ws-error-invalid-total.
           write valid-record          from ws-detail-line.
           write invalid-record        from ws-detail-invalid-line.
           write errors-record         from ws-error-report1.
           write errors-record         from ws-error-report2
             after advancing 1 line.
           write errors-record         from ws-error-report-num
             after advancing 2 lines.
           write errors-record         from ws-error-report-records
             after advancing 1 line.
           write errors-record         from ws-error-report-valid
             after advancing 1 line.
           write errors-record         from ws-error-report-invalid
             after advancing 1 line.


       end program EDIT.
      ******************************************************************