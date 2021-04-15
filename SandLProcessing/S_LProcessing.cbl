       identification division.
       program-id. S_LProcessing.
       author. Qayyam Jamal.
       date-written. 2021-04-15.

       environment division.
       input-output section.
       file-control.

           select input-file
               assign to "../../slrecord.data"
               organization is line sequential.

           select output-file
           assign to "../../SandLReport.out"
           organization is line sequential.

       data division.
       file section.

       fd input-file
           record contains 36 characters
           data record is data-record.

       01 data-record.
         05 transaction-code       pic x.
         05 transaction-amount     pic 9(5)V99.
         05 payment-type           pic xx.
         05 store-number           pic 99.
         05 invoice-number         pic x(9).
         05 sku-code               pic x(15).

       fd output-file
           record contains 200 characters
           data record is output-record.

       01 output-record pic x(200).

       working-storage section.

       01 ws-flags.
         05 ws-eof-flag            pic x
           value "f".

       01 ws-constants.
         05 ws-lines-per-page      pic 99
           value 20.
         05 ws-pcnt-sign           pic x
           value "%".
         05 ws-tax                 pic 9V9(3)
           value 0.013.
         05 ws-s                   pic x
           value "S".
         05 ws-l                   pic x
           value "L".
         05 ws-credit              pic xx
           value "CR".
         05 ws-cash                pic xx
           value "CA".
         05 ws-debit               pic xx
           value "DB".
         05 ws-num-of-stores       pic 9
           value 6.

       01 ws-page-heading.
         05 filler                 pic x(191)
           value spaces.
         05 filler                 pic x(6)
           value "Page: ".
         05 page-num pic z9.

       01 ws-header-1.
         05 filler                 pic x(88)
           value spaces.
         05 filler                 pic x(23)
           value "SALES & LAYAWAYS REPORT".
         05 filler                 pic x(89)
           value spaces.

       01 ws-header-2.
         05 filler                 pic x(11)
           value "TRANSACTION".
         05 filler                 pic x(02)
           value spaces.
         05 filler                 pic x(11)
           value "TRANSACTION".
         05 filler                 pic x(02)
           value spaces.
         05 filler                 pic x(07)
           value "PAYMENT".
         05 filler                 pic x(02)
           value spaces.
         05 filler                 pic x(05)
           value "STORE".
         05 filler                 pic x(04)
           value spaces.
         05 filler                 pic x(07)
           value "INVOICE".
         05 filler                 pic x(09)
           value spaces.
         05 filler                 pic x(03)
           value "SKU".
         05 filler                 pic x(10)
           value spaces.
         05 filler                 pic x(03)
           value "TAX".

       01 ws-header-3.
         05 filler                 pic x(03)
           value spaces.
         05 filler                 pic x(04)
           value "CODE".
         05 filler                 pic x(09)
           value spaces.
         05 filler                 pic x(06)
           value "AMOUNT".
         05 filler                 pic x(06)
           value spaces.
         05 filler                 pic x(04)
           value "TYPE".
         05 filler                 pic x(03)
           value spaces.
         05 filler                 pic x(06)
           value "NUMBER".
         05 filler                 pic x(04)
           value spaces.
         05 filler                 pic x(06)
           value "NUMBER".
         05 filler                 pic x(08)
           value spaces.
         05 filler                 pic x(04)
           value "CODE".
         05 filler                 pic x(09)
           value spaces.
         05 filler                 pic x(05)
           value "OWING".

       01 ws-detail-line.
         05 filler                 pic x(04)
           value spaces.
         05 ws-Trans-code          pic x.
         05 filler                 pic x(09)
           value spaces.
         05 ws-Trans-amount        pic zzzz9.99.
         05 filler                 pic x(06)
           value spaces.
         05 ws-payment-type        pic xx.
         05 filler                 pic x(07)
           value spaces.
         05 ws-store-number        pic z9.
         05 filler                 pic x(04)
           value spaces.
         05 ws-invoice-number      pic x(09).
         05 filler                 pic x(02)
           value spaces.
         05 ws-sku-code            pic x(15).
         05 filler                 pic x(02)
           value spaces.
         05 ws-tax-owing           pic zzz9.99.

       01 ws-s-l-totals-line.
         05 filler                 pic x(29)
           value "TOTAL S&L RECORDS AND AMOUNT: ".
         05 filler                 pic x(7)
           value spaces.
         05 ws-s-l-records         pic zz9.
         05 filler                 pic x(7)
           value spaces.
         05 ws-s-l-total           pic zzz,zz9.99.

       01 ws-s-totals-line.
         05 filler                 pic x(27)
           value "TOTAL S RECORDS AND AMOUNT: ".
         05 filler                 pic x(9)
           value spaces.
         05 ws-s-records           pic zz9.
         05 filler                 pic x(7)
           value spaces.
         05 ws-s-total             pic zzz,zz9.99.

       01 ws-l-totals-line.
         05 filler                 pic x(27)
           value "TOTAL L RECORDS AND AMOUNT: ".
         05 filler                 pic x(9)
           value spaces.
         05 ws-l-records           pic zz9.
         05 filler                 pic x(7)
           value spaces.
         05 ws-l-total             pic zzz,zz9.99.

       01 ws-ca-pcnt-line.
         05 filler                 pic x(26)
           value "CASH PAYMENT PERCENTAGE:".
         05 filler                 pic x(5)
           value spaces.
         05 ws-ca-num              pic zz9.
         05 filler                 pic x(5)
           value spaces.
         05 ws-ca-pcnt             pic zz9.99.
         05 ws-ca-pcnt-sign        pic x.

       01 ws-cr-pcnt-line.
         05 filler                 pic x(26)
           value "CREDIT PAYMENT PERCENTAGE:".
         05 filler                 pic x(5)
           value spaces.
         05 ws-cr-num              pic zz9.
         05 filler                 pic x(5)
           value spaces.
         05 ws-cr-pcnt             pic zz9.99.
         05 ws-cr-pcnt-sign        pic x.

       01 ws-db-pcnt-line.
         05 filler                 pic x(26)
           value "DEBIT PAYMENT PERCENTAGE: ".
         05 filler                 pic x(5)
           value spaces.
         05 ws-db-num              pic zz9.
         05 filler                 pic x(5)
           value spaces.
         05 ws-db-pcnt             pic zz9.99.
         05 ws-db-pcnt-sign        pic x.

       01 ws-total-tax-line.
         05 filler                 pic x(16)
           value "TOTAL TAX OWING:".
         05 filler                 pic x(5)
           value spaces.
         05 ws-total-tax           pic zzz,zz9.99.

       01 ws-store-h-s-l-amount.
         05 filler                 pic x(52)
           value "STORE WITH THE HIGHEST S&L TOTAL TRANSACTION AMOUNT:".
         05 filler                 pic x(5)
           value spaces.
         05 ws-h-store-num         pic 99.

       01 ws-store-l-s-l-amount.
         05 filler pic x(52) value "STORE WITH THE LOWEST S&L TOTAL TRANSACTION AMOUNT:".
         05 filler pic x(5) value spaces.
         05 ws-l-store-num pic 99.

       01 ws-store-totals.
         10 store-size occurs 6 times pic 99.
         10 store-num occurs 6 times pic 99.
         10 store-trans occurs 6 times pic 9(6)V99.

       77 ws-tax-calc              pic 9(5)V99
           value 0.
       77 ws-total-tax-owing       pic 9(6)V99
           value 0.
       77 ws-s-l-count             pic 9(3)
           value 0.
       77 ws-s-l-total-calc        pic 9(6)V99
           value 0.
       77 ws-s-count               pic 9(3)
           value 0.
       77 ws-l-count               pic 9(3)
           value 0.
       77 ws-s-total-calc          pic 9(6)V99
           value 0.
       77 ws-l-total-calc          pic 9(6)V99
           value 0.
       77 ws-ca-count              pic 9(3)
           value 0.
       77 ws-ca-pcnt-calc          pic 9(3)V99
           value 0.
       77 ws-cr-count              pic 9(3) 
           value 0.
       77 ws-cr-pcnt-calc          pic 9(3)V99
           value 0.
       77 ws-db-count              pic 9(3)
           value 0.
       77 ws-db-pcnt-calc          pic 9(3)V99
           value 0.
       77 ws-sub                   pic 9
           value 0.
       77 ws-highest               pic 9(6)V99
           value 0.
       77 ws-lowest                pic 9(6)V99
           value 0.
       77 ws-line-count            pic 99
           value 0.
       77 ws-page-count pic 99 value 0.


       procedure division.

       000-Main.
           perform 100-Open-Files.
           perform 200-Read-Input.
           perform 400-Process-Pages
             until ws-eof-flag = "t".
           perform 500-Write.
           perform 600-Close-Files.
           goback.

       100-Open-Files.
           open input input-file,
                output output-file.

       200-Read-Input.
           read input-file
               at end
                   move "t" to ws-eof-flag.

       300-Write-Headings.
           write output-record from ws-header-1
           after advancing 1 line.
           write output-record from ws-header-2
               after advancing 2 lines.
           write output-record from ws-header-3.

       400-Process-Pages.
           perform 405-Page-Headings.
           perform 407-Process-Lines
             varying ws-line-count from 1 by 1
             until (ws-line-count > ws-lines-per-page
             OR ws-eof-flag = "t").


       405-Page-Headings.
           add 1 to ws-page-count.
           move ws-page-count to page-num.
           if (ws-page-count > 1)
               write output-record from ws-page-heading
                 after advancing page
               perform 300-Write-Headings
           else
               write output-record from ws-page-heading
               perform 300-Write-Headings
           end-if.

       407-Process-Lines.
           add 1 to ws-s-l-count.
           perform 410-Calculate-Tax-Owing.
           perform 415-Write-Details-Line.
           perform 420-S-L-Totals.
           perform 430-S-Totals.
           perform 440-L-Totals.
           perform 450-Cash.
           perform 460-Credit.
           perform 470-Debit.
           perform 480-Highest.
           perform 490-Lowest.
           perform 200-Read-Input.
       410-Calculate-Tax-Owing.
           compute ws-tax-calc rounded = transaction-amount * ws-tax.
           add ws-tax-calc to ws-total-tax-owing.
           move ws-tax-calc to ws-tax-owing.
           move ws-total-tax-owing to ws-total-tax.

       415-Write-Details-Line.
           move transaction-code to ws-Trans-code.
           move transaction-amount to ws-Trans-amount.
           move payment-type to ws-payment-type.
           move store-number to ws-store-number.
           move invoice-number to ws-invoice-number.
           move sku-code to ws-sku-code.

           write output-record from ws-detail-line.
       420-S-L-Totals.
           add transaction-amount to ws-s-l-total-calc.
           move ws-s-l-total-calc to ws-s-l-total.
           move ws-s-l-count to ws-s-l-records.
       430-S-Totals.
           if transaction-code = ws-s
               add 1 to ws-s-count
               add transaction-amount to ws-s-total-calc
           end-if.
           move ws-s-count to ws-s-records.
           move ws-s-total-calc to ws-s-total.

       440-L-Totals.
           if transaction-code = ws-l
               add 1 to ws-l-count
               add transaction-amount to ws-l-total-calc
           end-if.

           move ws-l-count to ws-l-records.
           move ws-l-total-calc to ws-l-total.

       450-Cash.
           if payment-type = ws-cash
               add 1 to ws-ca-count
           end-if.

           compute ws-ca-pcnt-calc rounded = (ws-ca-count / ws-s-l-count) * 100.
           move ws-ca-pcnt-calc to ws-ca-pcnt.
           move ws-pcnt-sign to ws-ca-pcnt-sign.
           move ws-ca-count to ws-ca-num.
       460-Credit.
           if payment-type = ws-credit
               add 1 to ws-cr-count
           end-if.

           compute ws-cr-pcnt-calc rounded = (ws-cr-count / ws-s-l-count) * 100.
           move ws-cr-pcnt-calc to ws-cr-pcnt.
           move ws-pcnt-sign to ws-cr-pcnt-sign.
           move ws-cr-count to ws-cr-num.

       470-Debit.
           if payment-type = ws-debit
               add 1 to ws-db-count
           end-if.

           compute ws-db-pcnt-calc rounded = (ws-db-count / ws-s-l-count) * 100.
           move ws-db-pcnt-calc to ws-db-pcnt.
           move ws-pcnt-sign to ws-db-pcnt-sign.
           move ws-db-count to ws-db-num.

       480-Highest.
           perform 485-Store-Totals.
           perform
             varying ws-sub from 1 by 1
             until ws-sub > ws-num-of-stores
               if store-trans(ws-sub) > ws-highest
                   move store-trans(ws-sub) to ws-highest
                   move store-num(ws-sub) to ws-h-store-num
               end-if
           end-perform.

       485-Store-Totals.
           perform
             varying ws-sub from 1 by 1
             until ws-sub > ws-num-of-stores
               move store-number to store-num(ws-sub)
               if store-number = store-num(ws-sub)
                   add transaction-amount to store-trans(ws-sub)
               end-if
           end-perform.
       490-Lowest.
           move ws-highest to ws-lowest.
           perform
             varying ws-sub from 1 by 1
             until ws-sub > ws-num-of-stores
               if store-trans(ws-sub) < ws-lowest
                   move store-trans(ws-sub) to ws-lowest
                   move store-num(ws-sub) to ws-l-store-num
               end-if
           end-perform.

       500-Write.
           write output-record from ws-s-l-totals-line
             after advancing 1 line.
           write output-record from ws-s-totals-line.
           write output-record from ws-l-totals-line.
           write output-record from ws-ca-pcnt-line
             after advancing 1 line.
           write output-record from ws-cr-pcnt-line.
           write output-record from ws-db-pcnt-line.
           write output-record from ws-total-tax-line
             after advancing 1 line.
           write output-record from ws-store-h-s-l-amount
             after advancing 2 lines.
           write output-record from ws-store-l-s-l-amount.

       600-Close-Files.
           close input-file, output-file.

       end program S_LProcessing.