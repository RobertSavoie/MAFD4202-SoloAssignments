       identification division.
       program-id. A3-SalesComm.
       author. Rob Savoie.
       date-written. Feb 13/2023.
      *Program Description:
      *
       environment division.
      *
       input-output section.
       file-control.
      *
           select sales-file
               assign to "../../../data/A3.dat"
               organization is line sequential.
      *
           select report-file
               assign to "../../../data/A3-SalesComm.out"
               organization is line sequential.
      *
       data division.
       file section.
       fd sales-file
           data record is sales-rec
           record contains 32 characters.
      *
       01 sales-rec.
           05 sr-sman-num              pic 999.
           05 sr-name                  pic x(8).
           05 sr-sales                 pic 9(6).
           05 sr-min                   pic 9(6).
           05 sr-max                   pic 9(6).
           05 sr-rate                  pic 99v9.
      *
       fd report-file
           data record is report-line
           record contains 90 characters.
      *
       01 report-line                  pic x(90).
      *
       working-storage section.
      *
       01 ws-heading-name-line.
           05 filler                   pic x(63) value spaces.
           05 ws_username              pic x(27) value
                                       "Robert Savoie, Assignment 3".
      *
       01 ws-heading-title.
           05 filler                   pic x(34) value spaces.
           05 filler                   pic x(23) value
                                       "SALES COMMISSION REPORT".
           05 filler                   pic x(33) value spaces.
      *
       01 ws-heading-columns.
           05 filler                   pic x(3)  value "NO.".
           05 filler                   pic x(5)  value spaces.
           05 filler                   pic x(4)  value "NAME".
           05 filler                   pic x(6)  value spaces.
           05 filler                   pic x(5)  value "SALES".
           05 filler                   pic x(5)  value spaces.
           05 filler                   pic x(3)  value "MIN".
           05 filler                   pic x(6)  value spaces.
           05 filler                   pic x(3)  value "MAX".
           05 filler                   pic x(4)  value spaces.
           05 filler                   pic x(4)  value "RATE".
           05 filler                   pic x(5)  value spaces.
           05 filler                   pic x(6)  value "EARNED".
           05 filler                   pic x(6)  value spaces.
           05 filler                   pic x(4)  value "PAID".
           05 filler                   pic x(5)  value spaces.
           05 filler                   pic x(14) value "BONUS/NO BONUS".
           05 filler                   pic x(2)  value spaces.
      *
       01 ws-heading-dashes.
           05 filler                   pic x(3)   value "---".
           05 filler                   pic x(3)   value spaces.
           05 filler                   pic x(8)   value "--------".
           05 filler                   pic x(3)   value spaces.
           05 filler                   pic x(7)   value "-------".
           05 filler                   pic x(2)   value spaces.
           05 filler                   pic x(7)   value "-------".
           05 filler                   pic x(2)   value spaces.
           05 filler                   pic x(7)   value "-------".
           05 filler                   pic x(2)   value spaces.
           05 filler                   pic x(5)   value "-----".
           05 filler                   pic x(1)   value spaces.
           05 filler                   pic x(10)  value "----------".
           05 filler                   pic x(2)   value spaces.
           05 filler                   pic x(10)  value "----------".
           05 filler                   pic x(2)   value spaces.
           05 filler                   pic x(16)  value
                                       "----------------".
      *
       01 ws-detail-line.
           05 ws-id                    pic x(3).
           05 filler                   pic x(3)   value spaces.
           05 ws-name                  pic x(8).
           05 filler                   pic x(3)   value spaces.
           05 ws-sales                 pic ZZZ,ZZ9.
           05 filler                   pic x(2)   value spaces.
           05 ws-min                   pic ZZZ,ZZ9.
           05 filler                   pic x(2)   value spaces.
           05 ws-max                   pic ZZZ,ZZ9.
           05 filler                   pic x(2)   value spaces.
           05 ws-rate                  pic Z9.9.
           05 filler                   pic x      value "%".
           05 filler                   pic x(2)   value spaces.
           05 ws-earned                pic Z,ZZZ,ZZ9.
           05 filler                   pic x(2)   value spaces.
           05 ws-paid                  pic $$,$$$,$$9.
           05 filler                   pic x(2)   value spaces.
           05 ws-bonus                 pic x(16).
      *
       01 ws-end-lines.
           05 ws-total-line.
               10 filler               pic x(41).
               10 filler               pic x(9)   value "Totals".
               10 ws-total-earned      pic $$,$$$,$$9 value 0.
               10 filler               pic xx     value spaces.
               10 ws-total-paid        pic $$,$$$,$$9 value 0.
               10 filler               pic x(18)  value spaces.
           05 ws-ft-bonus-over-max.
               10 filler               pic x(38)  value
                                   "NUMBER WITH BONUS MORE THAN MAX".
               10 ws-over              pic zz9    value 0.
               10 filler               pic x(49)  value spaces.
           05 ws-ft-bonus-under-min.
               10 filler               pic x(38)  value
                                   "NUMBER WITH NO BONUS LESS THAN MIN".
               10 ws-under             pic zz9    value 0.
               10 filler               pic x(49)  value spaces.
           05 ws-ft-sales-with-bonus.
               10 filler               pic x(38)  value
                                   "NUMBER OF SALESPEOPLE WITH BONUS".
               10 ws-with              pic zz9    value 0.
               10 filler               pic x(49)  value space.
           05 ws-ft-sales-without-bonus.
               10 filler               pic x(38)  value
                                   "NUMBER OF SALESPEOPLE WITHOUT BONUS".
               10 ws-without           pic zz9    value 0.
               10 filler               pic x(49)  value spaces.
           05 ws-ft-salespeople.
               10 filler               pic x(38)  value
                                   "NUMBER OF SALESPEOPLE".
               10 ws-salespeople       pic zz9    value 0.
               10 filler               pic x(49)  value spaces.
           05 ws-ft-paid-equal.
               10 filler               pic x(38)  value
                                   "NUMBER  WITH PAID EQUAL EARNED".
               10 ws-paid-equal        pic zz9    value 0.
               10 filler               pic x(49)  value spaces.
           05 ws-ft-percent-equal.
               10 filler               pic x(38)  value
                                   "PERCENT WITH PAID EQUAL EARNED".
               10 ws-percent-equal     pic zz9.99 value 0.
               10 filler               pic x      value "%".
               10 filler               pic x(50)  value spaces.
           05 ws-ft-high-bonus.
               10 filler               pic x(38)  value
                                   "PERCENT WITH BONUS     >300,000".
               10 ws-high-bonus        pic zz9.99 value 0.
               10 filler               pic x      value "%".
               10 filler               pic x(50)  value spaces.
           05 ws-ft-low-bonus.
               10 filler               pic x(38)  value
                                   "PERCENT WITHOUT BONUS <=300,000".
               10 ws-low-bonus         pic zz9.99 value 0.
               10 filler               pic x      value "%".
               10 filler               pic x(50)  value spaces.
      *
      *math storage
       01 ws-math-store.
           05 ws-math-sales            pic 9(6).
           05 ws-math-min              pic 9(6).
           05 ws-math-max              pic 9(6).
           05 ws-math-rate             pic v9(4).
           05 ws-math-earned           pic 9(7).
           05 ws-math-paid             pic 9(7).
           05 ws-math-above-earned     pic 9(7).
           05 ws-math-above            pic 9(7).
      *
      *counters
       01 ws-counters.
           05 ws-cntr-page             pic 99     value 1.
           05 ws-cntr-line             pic 99     value 0.
           05 ws-cntr-over-max         pic 99     value 0.
           05 ws-cntr-under-min        pic 99     value 0.
           05 ws-cntr-with             pic 99     value 0.
           05 ws-cntr-without          pic 99     value 0.
           05 ws-cntr-salespeople      pic 99     value 0.
           05 ws-cntr-number-equal     pic 99     value 0.
           05 ws-cntr-percent-equal    pic 99     value 0.
           05 ws-cntr-high             pic 99     value 0.
           05 ws-cntr-low              pic 99     value 0.
      *
      *eof constants
       77 ws-eof-flag                  pic x      value "n".
       77 ws-eof-Y                     pic x      value "y".
       77 ws-eof-N                     pic x      value "n".
      *
      *constants
       77 ws-lines-per-page            pic 99     value 10.
       77 ws-commission-cutoff         pic 9(6)   value 300000.
       77 ws-commission-rate           pic v9(4)  value 0.1525.
      *
       procedure division.
       000-main.
      *
           perform 100-open-files.
           perform 110-print-report-heading.
           perform 120-read-file.
           perform 200-process-pages
             until ws-eof-flag equals ws-eof-Y.
           perform 140-print-report-footer.
           perform 800-close-files.
           goback.
      *
       100-open-files.
      *
           open input sales-file.
           open output report-file.
           move ws-eof-N to ws-eof-flag.
      *
       110-print-report-heading.
      *
           write report-line
             from ws-heading-name-line
             before advancing 2 lines.
      *
       120-read-file.
      *
           read sales-file
               at end
                   move ws-eof-Y to ws-eof-flag.
      *
       130-print-headings.
      *
           if ws-cntr-page is greater than 0
               write report-line
                 from ws-heading-title
                 after advancing page
           else
               write report-line
                 from ws-heading-title
                 after advancing 2 lines
           end-if.
           write report-line
             from ws-heading-columns
             after advancing 2 line.
           write report-line
             from ws-heading-dashes
             after advancing 1 line.
      *
       140-print-report-footer.
      *
           perform 750-print-totals.
           perform 700-print-counters.
      *
       150-clear-artifacts.
      *
           move spaces to ws-detail-line.
           move spaces to ws-math-store.
      *
       200-process-pages.
      *
           perform 130-print-headings.
           perform 250-process-lines
             varying ws-cntr-line from 0 by 1
             until ws-cntr-line equals ws-lines-per-page
             or ws-eof-flag equals ws-eof-Y.
      *
       250-process-lines.
      *
           perform 150-clear-artifacts
           perform 300-calculations.
           perform 600-output-detail-line.
           perform 120-read-file.
           add 1 to ws-cntr-salespeople.
      *
       300-calculations.
      *    
           perform 400-calculate-earned-commission.
           perform 500-calculate-paid.
      *
       400-calculate-earned-commission.
      *
           divide sr-rate
               by 100
           giving ws-math-rate.

           if sr-sales is less than or equal to ws-commission-cutoff
             then
               multiply sr-sales
                     by ws-math-rate
                 giving ws-math-earned rounded
           else
               if sr-sales is greater than ws-commission-cutoff
                 then

                   subtract ws-commission-cutoff
                       from sr-sales
                     giving ws-math-above

                   multiply sr-sales
                         by ws-math-rate
                     giving ws-earned

                   multiply ws-math-above
                         by ws-commission-rate
                     giving ws-math-above-earned

                   add ws-math-above-earned
                     to ws-math-earned

           end-if.
      *
       500-calculate-paid.
      *
           perform 510-sales-over-cutoff.
           perform 520-sales-under-cutoff.
           perform 530-earned-over-max.
           perform 540-earned-under-min.
      *
       510-sales-over-cutoff.
      *
           if sr-sales is greater than ws-commission-cutoff
             then
               move ws-math-earned to ws-paid
           end-if.
      *
       520-sales-under-cutoff.
      *
           if sr-sales less than or equal to ws-commission-cutoff
             then
               move ws-math-earned to ws-paid
           end-if.
      *
       530-earned-over-max.
      *
           if sr-sales is greater than ws-commission-cutoff and
             ws-math-earned is greater than sr-max
             then
               move sr-max to ws-paid
               add 1 to ws-cntr-over-max
           end-if.
       540-earned-under-min.
      *
           if sr-sales less than or equal to ws-commission-cutoff and
             ws-math-earned is less than sr-min
             then
               move sr-min to ws-paid
               add 1 to ws-cntr-under-min
           end-if.

      *
       600-output-detail-line.
      *
           move sr-sman-num       to ws-id.
           move sr-name           to ws-name.
           move sr-sales          to ws-sales.
           move sr-min            to ws-min.
           move sr-max            to ws-max.
           move sr-rate           to ws-rate.
           move ws-math-earned    to ws-earned.
           write report-line
             from ws-detail-line
             before advancing 2 lines.
      *
       700-print-counters.
      *
           move ws-cntr-over-max      to ws-over.
           move ws-cntr-under-min     to ws-under.
           move ws-cntr-with          to ws-with.
           move ws-cntr-without       to ws-without.
           move ws-cntr-salespeople   to ws-salespeople.
           move ws-cntr-number-equal  to ws-paid-equal.
           move ws-cntr-percent-equal to ws-percent-equal.
           move ws-cntr-high          to ws-high-bonus.
           move ws-cntr-low           to ws-low-bonus.

           write report-line
             from ws-ft-bonus-over-max
             before advancing 1 line.
           write report-line
             from ws-ft-bonus-under-min
             before advancing 2 lines.

           write report-line
             from ws-ft-sales-with-bonus
             before advancing 1 line.
           write report-line
             from ws-ft-sales-without-bonus
             before advancing 1 line.
           write report-line
             from ws-ft-salespeople
             before advancing 2 lines.

           write report-line
             from ws-ft-paid-equal
             before advancing 1 line.
           write report-line
             from ws-ft-percent-equal
             before advancing 2 lines.

           write report-line
             from ws-ft-high-bonus
             before advancing 1 line.
           write report-line
             from ws-ft-low-bonus
             before advancing 1 line.


      *
       750-print-totals.
      *
           write report-line
             from ws-total-line
             before advancing 2 lines.
      *
       800-close-files.
      *
           close sales-file
             report-file.
      *
       end program A3-SalesComm.