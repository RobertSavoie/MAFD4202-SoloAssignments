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
           05 filler                   pic x(3)  value "---".
           05 filler                   pic x(3)  value spaces.
           05 filler                   pic x(8)  value "--------".
           05 filler                   pic x(3)  value spaces.
           05 filler                   pic x(7)  value "-------".
           05 filler                   pic x(2)  value spaces.
           05 filler                   pic x(7)  value "-------".
           05 filler                   pic x(2)  value spaces.
           05 filler                   pic x(7)  value "-------".
           05 filler                   pic x(2)  value spaces.
           05 filler                   pic x(5)  value "-----".
           05 filler                   pic x(1)  value spaces.
           05 filler                   pic x(10) value "----------".
           05 filler                   pic x(2)  value spaces.
           05 filler                   pic x(10) value "----------".
           05 filler                   pic x(2)  value spaces.
           05 filler                   pic x(16) value
                                       "----------------".
      *
       01 ws-detail-line.
           05 ws-id                    pic x(3).
           05 filler                   pic x(3)  value spaces.
           05 ws-name                  pic x(8).
           05 filler                   pic x(3)  value spaces.
           05 ws-sales                 pic ZZZ,ZZ9.
           05 filler                   pic x(2)  value spaces.
           05 ws-min                   pic ZZZ,ZZ9.
           05 filler                   pic x(2)  value spaces.
           05 ws-max                   pic ZZZ,ZZ9.
           05 filler                   pic x(2)  value spaces.
           05 ws-rate                  pic Z9.9.
           05 filler                   pic x     value "%".
           05 filler                   pic x(2)  value spaces.
           05 ws-earned                pic Z,ZZZ,ZZ9.
           05 filler                   pic x(2)  value spaces.
           05 ws-paid                  pic $$,$$$,$$9.
           05 filler                   pic x(2)  value spaces.
           05 ws-bonus                 pic x(16).
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
           05 ws-page-count            pic 99    value 1.
           05 ws-line-count            pic 99.
      *
      *eof constants
       77 ws-eof-flag                  pic x     value "n".
       77 ws-eof-Y                     pic x     value "y".
       77 ws-eof-N                     pic x     value "n".
      *
      *constants
       77 ws-lines-per-page            pic 99    value 10.
       77 ws-commission-cutoff         pic 9(6)  value 300000.
       77 ws-commission-rate           pic v9(4) value 0.1525.
      *
       procedure division.
       000-main.
      *
           perform 100-open-files.
           perform 110-print-report-heading.
           perform 120-read-file.
           perform 140-process-pages
             until ws-eof-flag equals ws-eof-Y.
      *    perform 160-print-report-footer.
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
       140-process-pages.
      *
           perform 150-print-headings.
           perform 200-process-lines
             varying ws-line-count from 0 by 1
             until ws-line-count equals ws-lines-per-page
             or ws-eof-flag equals ws-eof-Y.
      *    perform 160-print-footer.
      *
       150-print-headings.
      *
           if ws-page-count is greater than 0
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
       160-print-report-footer.
      *
      *    perform 500-print-totals.
      *
       170-clear-artifacts.
      *
           move spaces to ws-detail-line.
           move spaces to ws-math-store.
      *
       200-process-lines.
      *
           perform 170-clear-artifacts
           perform 300-calculate-earned-commission.
           perform 420-paid-under-max.
           perform 400-output-detail-line.
           perform 120-read-file.
      *
       300-calculate-earned-commission.
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
       400-calculate-paid.
      *
           perform 420-paid-under-max.
      *
       410-paid-under-minimum.
      *
           if sr-sales is greater than ws-commission-cutoff
             then
               multiply ws-math-earned
                     by ws-math-rate
                 giving ws-math-paid

               move ws-math-paid to ws-paid
           end-if.
      *
       420-paid-under-max.
      *
           if sr-sales is greater than ws-commission-cutoff and
             ws-math-earned is greater than sr-max
             then
               move sr-max to ws-paid
           end-if.
      *
       430-bonus-over-maximum.
      *

      *
       400-output-detail-line.
      *
           move sr-sman-num    to ws-id.
           move sr-name        to ws-name.
           move sr-sales       to ws-sales.
           move sr-min         to ws-min.
           move sr-max         to ws-max.
           move sr-rate        to ws-rate.
           move ws-math-earned to ws-earned.
           write report-line
             from ws-detail-line
             before advancing 2 lines.
      *
       500-print-totals.
      *

      *
       800-close-files.
      *
           close sales-file
             report-file.
      *
       end program A3-SalesComm.