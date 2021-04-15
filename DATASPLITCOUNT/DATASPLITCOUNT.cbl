       identification division.
       author. Qayyam Jamal, Menu k, Aadithkeshev.
       program-id. DATASPLITCOUNT.

       environment division.
       input-output section.

       file-control.
              select input-file
                   assign to "../FinalProject/valid.data"
                   organization is line sequential.

           select sl-records-file
               assign to "../FinalProject/slrecord.data"
               organization is line sequential.

           select returns-file
               assign to "../FinalProject/returns.data"
               organization is line sequential.

           select cc-totals-report
               assign to "../FinalProject/counts-controls-total.out"
               organization is line sequential.

       data division.
       file section.

       fd input-file
           record contains 36 characters
           data record is data-record.

       01 data-record.
         05 dr-trans-code pic x.
         05 dr-trans-amount pic 9(5)V99.
         05 dr-pay-type pic xx.
         05 dr-store-num pic xx.
         05 dr-invoice-num pic x(9).
         05 dr-sku-code pic x(15).

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
          record contains 49 characters.

       01 cc-total-output pic x(49).

       working-storage section.

       01 ws-eof-flag pic x value "N".

       01 ws-transaction-code  pic x value spaces.
         88 ws-trans-sale
                   value "S".
         88 ws-trans-layaway
                   value "L".
         88 ws-trans-return
                   value "R".

       01 ws-num-stores pic 9 value 6.
       01 ws-sub pic 9 value 0.

       01 ws-cc-heading.
         05 filler pic x(10) value space.
         05 filler pic x(27) value " COUNTS AND CONTROL TOTALS ".
         05 filler pic x(37) value
                   "-------------------------------------".

       01 ws-total-variables.
         05 ws-sl-tt pic 9(4) value 0.
         05 ws-sl-amount-tt pic 9(6)V99 value 0.
         05 ws-s-tt pic 9(4) value 0.
         05 ws-s-amount-tt pic 9(6)V99 value 0.
         05 ws-l-tt pic 9(4) value 0.
         05 ws-l-amount-tt pic 9(6)V99 value 0.
         05 ws-r-tt pic 9(4) value 0.
         05 ws-r-amount-tt pic 9(6)V99 value 0.
         05 ws-s-tt-percent pic 999V99 value 0.
         05 ws-l-tt-percent pic 999V99 value 0.
         05 ws-store-array-tt occurs 6 times.
           10 ws-store-sl-tt-amount pic 9(8)V99.
           10 ws-store-r-tt pic 9(4).
           10 ws-store-r-amount pic 9(8)V99.

       01 ws-sl-records-out.
         05 filler pic x value spaces.
         05 filler pic x(38) value
                   "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~".
         05 filler pic x(38) value
                   "Sales and Layaway Records Total:     ".
         05 ws-sl-records-in pic Z,ZZ9.

       01 ws-sl-amount-out.
         05 filler pic x value spaces.
         05 filler pic x(38) value
                   "Sales and Layaway Amount Total:     ".
         05 ws-sl-amount-in pic $,$$$,$$9.99.
         05 filler pic x(38) value
                   "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~".

       01 ws-s-records-out.
         05 filler pic x value spaces.
         05 filler pic x(29) value "~~~~~~~~~~~~~~~~~~~~~~~~~".
         05 filler pic x(25) value "Sales Records Total:     ".
         05 ws-s-records-in pic Z,ZZ9.

       01 ws-s-amount-out.
         05 filler pic x value spaces.
         05 filler pic x(25) value "Sales Amount Total:     ".
         05 ws-s-amount-in pic $,$$$,$$9.99.
         05 filler pic x(29) value "~~~~~~~~~~~~~~~~~~~~~~~~".

       01 ws-l-records-out.
         05 filler pic x value spaces.
         05 filler pic x(31) value "~~~~~~~~~~~~~~~~~~~~~~~~~~~".
         05 filler pic x(31) value "Layaway Records Total:     ".
         05 ws-l-records-in pic Z,ZZ9.

       01 ws-l-amount-out.
         05 filler pic x value spaces.
         05 filler pic x(31) value "Layaway Amounts Total:     ".
         05 ws-l-amount-in pic $,$$$,$$9.99.
         05 filler pic x(27) value "~~~~~~~~~~~~~~~~~~~~~~~~~~~".

       01 ws-r-records-out.
         05 filler pic x value spaces.
         05 filler pic x(31) value "~~~~~~~~~~~~~~~~~~~~~~~~~~~".
         05 filler pic x(31) value "Returns Records Total:     ".
         05 ws-r-records-in pic Z,ZZ9.

       01 ws-r-amount-out.
         05 filler pic x value spaces.
         05 filler pic x(31) value "Returns Amount Total:     ".
         05 ws-r-amount-in pic $,$$$,$$9.99.
         05 filler pic x(31) value "~~~~~~~~~~~~~~~~~~~~~~~~~~".

       01 ws-s-percent-out.
         05 filler pic x value spaces.
         05 filler pic x(26) value "~~~~~~~~~~~~~~~~~~~~~~".
         05 filler pic x(31) value "Sales Percentage:     ".
         05 ws-s-percent-in pic ZZ9.9.
         05 filler pic x value "%".

       01 ws-l-percent-out.
         05 filler pic x value spaces.
         05 filler pic x(31) value "Layaway Percentage:     ".
         05 ws-l-percent-in pic ZZ9.9.
         05 filler pic x value "%".
         05 filler pic x(29) value "~~~~~~~~~~~~~~~~~~~~~~~~".

       01 ws-store-heading-1.
         05 filler pic x value space.
         05 filler
                   value "Transactions and Returns by Store".
         05 filler pic x(33) value "=================================".

       01 ws-store-heading-2.
         05 filler pic x value space.
         05 filler pic x(9) value "Store #".
         05 filler pic x(5) value spaces.
         05 filler pic x(18) value "S&L Transactions".
         05 filler pic x(5) value spaces.
         05 filler pic x(7) value "Returns".
         05 filler pic x(5) value spaces.
         05 filler pic x(14) value "Returns Amount".

       01 ws-store-head2-in occurs 6 times.
         05 filler pic x(3) value spaces.
         05 ws-store-num pic 99.
         05 filler pic x(5) value spaces.
         05 ws-store-sl-amount pic $$,$$$,$$9.99.
         05 filler pic x(5) value spaces.
         05 ws-store-r-in pic Z,ZZ9.
         05 filler pic x(5) value spaces.
         05 ws-store-rec-amount pic $$,$$$,$$9.99.

       procedure division.
       000-main.

           open input input-file.
           open output sl-records-file, returns-file, cc-totals-report.

           read input-file
               at end
                   move "Y" to ws-eof-flag.

           perform 100-print-headings.
           perform 150-proccessing-totals until ws-eof-flag = "Y".
           perform 300-percentages.
           perform 400-totals.

           close input-file, sl-records-file, returns-file,
             cc-totals-report.

           goback.

       100-print-headings.
           write cc-total-output from ws-cc-heading
             before advancing 2 lines.

       150-proccessing-totals.
           move dr-trans-code to ws-transaction-code.

           perform 200-data-index.

           if (ws-trans-sale)
               add 1 to ws-sl-tt
               add dr-trans-amount to ws-sl-amount-tt

               add 1 to ws-s-tt
               add dr-trans-amount to ws-s-amount-tt

               add dr-trans-amount
                 to ws-store-sl-tt-amount(ws-sub)
           else
               if (ws-trans-layaway)
                   add 1 to ws-sl-tt
                   add dr-trans-amount to ws-sl-amount-tt

                   add 1 to ws-l-tt
                   add dr-trans-amount to ws-l-amount-tt

                   add dr-trans-amount
                     to ws-store-sl-tt-amount(ws-sub)
               else
                   if (ws-trans-return)
                       add 1 to ws-r-tt
                       add dr-trans-amount to ws-r-amount-tt

                       add 1 to ws-store-r-tt(ws-sub)
                       add dr-trans-amount
                         to ws-store-r-amount(ws-sub)
                   end-if
               end-if
           end-if.

           perform 250-print-data.

           read input-file
               at end
                   move "Y" to ws-eof-flag.

       200-data-index.
           if (dr-store-num = 1)
               move 1 to ws-sub
           else
               if (dr-store-num = 2)
                   move 2 to ws-sub
               else
                   if (dr-store-num = 3)
                       move 3 to ws-sub
                   else
                       if (dr-store-num = 4)
                           move 4 to ws-sub
                       else
                           if (dr-store-num = 5)
                               move 5 to ws-sub
                           else
                               if (dr-store-num = 12)
                                   move 12 to ws-sub
                               end-if
                           end-if
                       end-if
                   end-if
               end-if
           end-if.

       250-print-data.
           if (ws-trans-sale or ws-trans-layaway)
               write slr-output from data-record
           else
               write returns-output from data-record
           end-if.

       300-percentages.
           compute ws-s-tt-percent rounded = (ws-s-tt / ws-sl-tt) * 100.
           compute ws-l-tt-percent rounded = (ws-l-tt / ws-sl-tt) * 100.

       400-totals.
           move ws-sl-tt to ws-sl-records-in.
           move ws-sl-amount-tt to ws-sl-amount-in.

           move ws-s-tt to ws-s-records-in.
           move ws-s-amount-tt to ws-s-amount-in.

           move ws-l-tt to ws-l-records-in.
           move ws-l-amount-tt to ws-l-amount-in.

           move ws-r-tt to ws-r-records-in.
           move ws-r-amount-tt to ws-r-amount-in.

           move ws-s-tt-percent to ws-s-percent-in.
           move ws-l-tt-percent to ws-l-percent-in.

           perform 450-store-number.

           perform varying ws-sub from 1 by 1
             until ws-sub > ws-num-stores

               move ws-store-sl-tt-amount(ws-sub)
                 to ws-store-sl-amount(ws-sub)
               move ws-store-r-tt(ws-sub)
                 to ws-store-r-in(ws-sub)
               move ws-store-r-amount(ws-sub)
                 to ws-store-rec-amount(ws-sub)
           end-perform.

           write cc-total-output from ws-sl-records-out.
           write cc-total-output from ws-sl-amount-out.
           write cc-total-output from ws-s-records-out.
           write cc-total-output from ws-s-amount-out.
           write cc-total-output from ws-l-records-out.
           write cc-total-output from ws-l-amount-out.
           write cc-total-output from ws-r-records-out.
           write cc-total-output from ws-r-amount-out.
           write cc-total-output from ws-s-percent-out.
           write cc-total-output from ws-l-percent-out
             before advancing 2 lines.

           write cc-total-output from ws-store-heading-1.
           write cc-total-output from ws-store-heading-2.

           perform varying ws-sub from 1 by 1
             until ws-sub > ws-num-stores

               write cc-total-output from ws-store-head2-in(ws-sub)
           end-perform.

       450-store-number.
           move 1 to ws-store-num(1)
           move 2 to ws-store-num(2)
           move 3 to ws-store-num(3)
           move 4 to ws-store-num(4)
           move 5 to ws-store-num(5)
           move 12 to ws-store-num(6)

       end program DATASPLITCOUNT.
