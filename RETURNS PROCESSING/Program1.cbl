       identification division.
       program-id. Program1 as "RETURNS_PROCESSING.Program1".
       author. Aadithkeshev.
       date-written. 13/04/2001.

       environment division.
       input-output section.
       file-control.

           select input-file  
               assign to "../../data/returns.data"
               organization is line sequential.
                          
           select output-file 
               assign to "../../data/returns-report.out"
               organization is line sequential.

       data division.
       file section.

       fd input-file
           data record is input-record
           record contains 36 characters.

       01 input-record.
         05 ir-Trans-code              pic x.
         05 ir-Trans-amount            pic 9(5)v99.
         05 ir-payment-type            pic xx.
         05 ir-store-number            pic 99.
         05 ir-invoice-number          pic x(09).
         05 ir-sku-code                pic x(15).

       fd output-file
           data record is output-line
           record contains 100 characters.
       
       01 output-line                  pic x(100).

       working-storage section.

       01 ws-eof-flag                  pic x
           value "n".

       01 ws-title-heading.
         05 filler                    pic x(26)
            value spaces.
         05 filler                    pic x(31)
            value "PROGRAM #4 RETURNS PROCESSING".

       01     ws-valid-heading-1.
         05    filler                  pic x(11)
               value "TRANSACTION".
         05    filler                  pic x(02)
               value spaces.
         05    filler                  pic x(11)
               value "TRANSACTION".
         05    filler                  pic x(02)
               value spaces.
         05    filler                  pic x(07)
               value "PAYMENT".
         05    filler                  pic x(02)
               value spaces.
         05    filler                  pic x(05)
               value "STORE".
         05    filler                  pic x(04)
               value spaces.
         05    filler                  pic x(07)
               value "INVOICE".
         05    filler                  pic x(09)
               value spaces.
         05    filler                  pic x(03)
               value "SKU".
         05    filler                  pic x(10)
               value spaces.
         05    filler                  pic x(03)
               value "TAX".
      *-------------------------------------------------------------------------
       01     ws-valid-heading-2.
         05    filler                  pic x(03)
               value spaces.
         05    filler                  pic x(04)
               value "CODE".
         05    filler                  pic x(09)
               value spaces.
         05    filler                  pic x(06)
               value "AMOUNT".
         05    filler                  pic x(06)
               value spaces.
         05    filler                  pic x(04)
               value "TYPE".
         05    filler                  pic x(03)
               value spaces.
         05    filler                  pic x(06)
               value "NUMBER".
         05    filler                  pic x(04)
               value spaces.
         05    filler                  pic x(06)
               value "NUMBER".
         05    filler                  pic x(08)
               value spaces.
         05    filler                  pic x(04)
               value "CODE".
         05    filler                  pic x(09)
               value spaces.
         05    filler                  pic x(04)
               value "OWED".
      *-------------------------------------------------------------------------
       01     ws-valid-heading-3.
         05    filler                  pic x(11)
               value "-----------".
         05    filler                  pic x(02)
               value spaces.
         05    filler                  pic x(11)
               value "-----------".
         05    filler                  pic x(02)
               value spaces.
         05    filler                  pic x(07)
               value "-------".
         05    filler                  pic x(02)
               value spaces.
         05    filler                  pic x(06)
               value "------".
         05    filler                  pic x(02)
               value spaces.
         05    filler                  pic x(09)
               value "---------".
         05    filler                  pic x(02)
               value spaces.
         05    filler                  pic x(15)
               value "---------------".
         05    filler                  pic x(02)
               value spaces.
         05    filler                  pic x(07)
               value "-------".
      *_________________________________________________________________________
       01     ws-detail-line.
         05 filler                     pic x(04)
            value spaces.
         05 ws-Trans-code              pic x.
         05 filler                     pic x(09)
            value spaces.
         05 ws-Trans-amount            pic zzzz9.99.
         05 filler                     pic x(06)
            value spaces.
         05 ws-payment-type            pic xx.
         05 filler                     pic x(07)
            value spaces.
         05 ws-store-number            pic z9.
         05 filler                     pic x(04)
            value spaces.
         05 ws-invoice-number          pic x(09).
         05 filler                     pic x(02)
            value spaces.
         05 ws-sku-code                pic x(15).    
         05 filler                     pic x(02)
            value spaces.
         05 ws-tax-owed                pic zzz9.99.

      *_________________________________________________________________________
       01 ws-store-1-line.
         05 filler                     pic x(32)
            value "Processed returns from store 01:".
         05 filler                     pic x(01)
            value spaces.
         05 ws-store-1                 pic zz9.
         05 filler                     pic x(04)
            value spaces.
         05 filler                     pic x(13)
            value "Total Amount:".
         05 filler                     pic x(01)
            value spaces.
         05 ws-store-1-ttl-amt         pic zzzzz9.99.
      *------------------------------------------------------------------------- 
       01 ws-store-2-line.
         05 filler                     pic x(32)
            value "Processed returns from store 02:".
         05 filler                     pic x(01)
            value spaces.
         05 ws-store-2                 pic zz9.
         05 filler                     pic x(04)
            value spaces.
         05 filler                     pic x(13)
            value "Total Amount:".
         05 filler                     pic x(01)
            value spaces.
         05 ws-store-2-ttl-amt         pic zzzzz9.99.
      *-------------------------------------------------------------------------
       01 ws-store-3-line.
         05 filler                     pic x(32)
            value "Processed returns from store 03:".
         05 filler                     pic x(01)
            value spaces.
         05 ws-store-3                 pic zz9.
         05 filler                     pic x(04)
            value spaces.
         05 filler                     pic x(13)
            value "Total Amount:".
         05 filler                     pic x(01)
            value spaces.
         05 ws-store-3-ttl-amt         pic zzzzz9.99.
      *-------------------------------------------------------------------------
       01 ws-store-4-line.
         05 filler                     pic x(32)
            value "Processed returns from store 04:".
         05 filler                     pic x(01)
            value spaces.
         05 ws-store-4                 pic zz9.
         05 filler                     pic x(04)
            value spaces.
         05 filler                     pic x(13)
            value "Total Amount:".
         05 filler                     pic x(01)
            value spaces.
         05 ws-store-4-ttl-amt         pic zzzzz9.99.
      *-------------------------------------------------------------------------
       01 ws-store-5-line.
         05 filler                     pic x(32)
            value "Processed returns from store 05:".
         05 filler                     pic x(01)
            value spaces.
         05 ws-store-5                 pic zz9.
         05 filler                     pic x(04)
            value spaces.
         05 filler                     pic x(13)
            value "Total Amount:".
         05 filler                     pic x(01)
            value spaces.
         05 ws-store-5-ttl-amt         pic zzzzz9.99.
      *-------------------------------------------------------------------------
       01 ws-store-12-line.
         05 filler                     pic x(32)
            value "Processed returns from store 12:".
         05 filler                     pic x(01)
            value spaces.
         05 ws-store-12                 pic zz9.
         05 filler                     pic x(04)
            value spaces.
         05 filler                     pic x(13)
            value "Total Amount:".
         05 filler                     pic x(01)
            value spaces.
         05 ws-store-12-ttl-amt         pic zzzzz9.99.
      *-------------------------------------------------------------------------
       01 ws-return-line.
         05 filler                     pic x(08)
            value spaces.
         05 filler                     pic x(24)
            value "Total Processed returns:".
         05 filler                     pic x(01)
            value spaces.
         05 ws-total-returns           pic zz9.
         05 filler                     pic x(04)
            value spaces.
         05 filler                     pic x(13)
            value "Total Amount:".
         05 filler                     pic x(01)
            value spaces.
         05 ws-return-ttl-amt          pic zzzzz9.99.
      *-------------------------------------------------------------------------
       01 ws-owedtax-line.
         05 filler                     pic x(11)
            value spaces.
         05 filler                     pic x(21)
            value "Total Tax owed to us:".
         05 filler                     pic x(01)
            value spaces.
         05 ws-ttl-owed-tax            pic zz9.99.
      *_________________________________________________________________________
      * tax constant
       77 Tax-percent                  pic 9v99
           value 0.13.
      * paging variables
       77 ws-lines-per-page            pic 99
           value 20.
       77 ws-page-count                pic 99
           value 0.
       77 ws-line-count                pic 99
           value 0.
      * hold variables for processed returns
      * total return calc variables
       77  ws-return-calc              pic 99
           value 0.
       77  ws-return-ttl               pic 9(4)v99
           value 0.
       77  ws-return-calc-ttl          pic 9(6)v99
           value 0.
      * store 1 variables
       77  store-1                     pic 999
           value 0.
       77  store-1-tax                 pic 9(4)v99
           value 0.
       77  store-1-ttl                 pic 9(6)v99
           value 0.
      * store 2 variables
       77  store-2                     pic 999
           value 0.
       77  store-2-tax                 pic 9(4)v99
           value 0.
       77  store-2-ttl                 pic 9(6)v99
           value 0.
      * store 3 variables
       77  store-3                     pic 999
           value 0. 
       77  store-3-tax                 pic 9(4)v99
           value 0.
       77  store-3-ttl                 pic 9(6)v99
           value 0. 
      * store 4 variables
       77  store-4                     pic 999
           value 0.
       77  store-4-tax                 pic 9(4)v99
           value 0.
       77  store-4-ttl                 pic 9(6)v99
           value 0.
      * store 5 variables
       77  store-5                     pic 999
           value 0.
       77  store-5-tax                 pic 9(4)v99
           value 0.
       77  store-5-ttl                 pic 9(6)v99
           value 0.
      * store 12 variables
       77  store-12                    pic 999
           value 0.
       77  store-12-tax                pic 9(4)v99
           value 0.
       77  store-12-ttl                pic 9(6)v99
           value 0.
      * tax owed to us variables
       77  owed-tax-accumulate         pic 9(4)v99
           value 0.
      **************************************************************************
       procedure division.
           open input input-file.
           open output output-file.

      * Initial read of input file
           read input-file
               at end
                   move "y" to ws-eof-flag.

      * transaction file Processes
           perform 10-PAGE-HEADLINES until ws-eof-flag = "y".
           perform 200-summary-line-process.

      * Exit program
           display "press enter key to exit...".
           accept return-code.

      * Close files and end program
           close input-file.
           close output-file.
           goback.
      **************************************************************************
       10-PAGE-HEADLINES.
           add 1 to ws-page-count.
           move spaces to output-line.
           if ws-page-count > 1 then
               write output-line
                 after advancing page
               write output-line from ws-valid-heading-1
                 after advancing 1 line
               write output-line from ws-valid-heading-2
               write output-line from ws-valid-heading-3
           else
               write output-line from ws-title-heading
               write output-line from ws-valid-heading-1
                 after advancing 1 line
               write output-line from ws-valid-heading-2
               write output-line from ws-valid-heading-3
           end-if.

           perform 100-record-process
             varying ws-line-count from 1 by 1
             until (ws-line-count > ws-lines-per-page
             or ws-eof-flag = "y").

      **************************************************************************
       100-record-process.
      * calculating total owed tax for detail line
           multiply ir-Trans-amount by Tax-percent
             giving ws-tax-owed.

      *-------------------------------------------------------------------------
      * total processed returns calculations
           move 0 to ws-return-ttl.
           if ir-Trans-code = 'R'
               add 1 to ws-return-calc
               move ir-Trans-amount to ws-return-ttl
           end-if.

           add ws-return-ttl to ws-return-calc-ttl.
           move ws-return-calc-ttl to ws-return-ttl-amt.

      * total owed tax calculation
           move ws-return-ttl-amt to owed-tax-accumulate.
           multiply owed-tax-accumulate by Tax-percent
             giving ws-ttl-owed-tax.
      *-------------------------------------------------------------------------
      * store 1 calculations
           move 0 to store-1-tax.
           if ir-store-number = 1
               add 1 to store-1
               move ir-Trans-amount to store-1-tax
           end-if.

           add store-1-tax to store-1-ttl.
           move store-1-ttl to ws-store-1-ttl-amt.

      *-------------------------------------------------------------------------
      * store 2 calculations
           move 0 to store-2-tax.
           if ir-store-number = 2
               add 1 to store-2
               move ir-Trans-amount to store-2-tax
           end-if.

           add store-2-tax to store-2-ttl.
           move store-2-ttl to ws-store-2-ttl-amt.
      *-------------------------------------------------------------------------
      * store 3 calculations
           move 0 to store-3-tax.
           if ir-store-number = 3
               add 1 to store-3
               move ir-Trans-amount to store-3-tax
           end-if.

           add store-3-tax to store-3-ttl.
           move store-3-ttl to ws-store-3-ttl-amt.

      *-------------------------------------------------------------------------
      * store 4 calculations
           move 0 to store-4-tax.
           if ir-store-number = 4
               add 1 to store-4
               move ir-Trans-amount to store-4-tax
           end-if.

           add store-4-tax to store-4-ttl.
           move store-4-ttl to ws-store-4-ttl-amt.
      
      *-------------------------------------------------------------------------
      * store 5 calculations
           move 0 to store-5-tax.
           if ir-store-number = 5
               add 1 to store-5
               move ir-Trans-amount to store-5-tax
           end-if.

           add store-5-tax to store-5-ttl.
           move store-5-ttl to ws-store-5-ttl-amt.

      *-------------------------------------------------------------------------
      * store 12 calculations
           move 0 to store-12-tax.
           if ir-store-number = 12
               add 1 to store-12
               move ir-Trans-amount to store-12-tax
           end-if.

           add store-12-tax to store-12-ttl.
           move store-12-ttl to ws-store-12-ttl-amt.

      *-------------------------------------------------------------------------
      * move statements
           move ws-return-calc    to ws-total-returns.
           move store-1           to ws-store-1.
           move store-2           to ws-store-2.
           move store-3           to ws-store-3.
           move store-4           to ws-store-4.
           move store-5           to ws-store-5.
           move store-12          to ws-store-12.
           move ir-Trans-code     to ws-Trans-code.
           move ir-Trans-amount   to ws-trans-amount.
           move ir-payment-type   to ws-payment-type.
           move ir-store-number   to ws-store-number.
           move ir-invoice-number to ws-invoice-number.
           move ir-sku-code       to ws-sku-code.

           write output-line from ws-detail-line.

           read input-file
               at end
                   move "y" to ws-eof-flag.

      **************************************************************************
       200-summary-line-process.
      * writes page number
           write output-line from ws-page-count
           write output-line from ws-store-1-line
             after advancing 1 line.
           write output-line from ws-store-2-line.
           write output-line from ws-store-3-line.
           write output-line from ws-store-4-line.
           write output-line from ws-store-5-line.
           write output-line from ws-store-12-line.
           write output-line from ws-return-line.
           write output-line from ws-owedtax-line.

       end program Program1.
      **************************************************************************