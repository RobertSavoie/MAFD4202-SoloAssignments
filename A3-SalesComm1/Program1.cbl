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
       01 ws-heading-headings.
           05 filler                   pic x(3) value "NO.".
           05 filler                   pic x(5) value spaces.
           05 filler                   pic x(4) value "NAME".
           05 filler                   pic x(6) value spaces.
           05 filler                   pic x(5) value "SALES".
           05 filler                   pic x(5) value spaces.
           05 filler                   pic x(3) value "MIN".
           05 filler                   pic x(6) value spaces.
           05 filler                   pic x(3) value "MAX".
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(4) value "RATE".
           05 filler                   pic x(5) value spaces.
           05 filler                   pic x(6) value "EARNED".
           05 filler                   pic x(6) value spaces.
           05 filler                   pic x(4) value "PAID".
           05 filler                   pic x(5) value spaces.
           05 filler                   pic x(14) value "BONUS/NO BONUS".
           05 filler                   pic x(2) value spaces.
      *
       01 ws-heading-underlines.
           05 filler                   pic x(3) value "---".
           05 filler                   pic x(3) value spaces.
           05 filler                   pic x(8) value "--------".
           05 filler                   pic x(3) value spaces.
           05 filler                   pic x(7) value "-------".
           05 filler                   pic x(2) value spaces.
           05 filler                   pic x(7) value "-------".
           05 filler                   pic x(2) value spaces.
           05 filler                   pic x(7) value "-------".
           05 filler                   pic x(2) value spaces.
           05 filler                   pic x(5) value "-----".
           05 filler                   pic x(1) value spaces.
           05 filler                   pic x(10) value "----------".
           05 filler                   pic x(2) value spaces.
           05 filler                   pic x(10) value "----------".
           05 filler                   pic x(2) value spaces.
           05 filler                   pic x(16) value
                                       "----------------".
      *
      *counters
       01 ws-counters.
           05 ws-page-count            pic 99 value 1.
           05 ws-line-count            pic 99.
      *
      *eof constants
       77 ws-eof-flag                  pic x value "n".
       77 ws-eof-Y                     pic x value "y".
       77 ws-eof-N                     pic x value "n".
      *
      *constants
       77 ws-lines-per-page            pic 99 value 10.
      *
       procedure division.
       000-main.
      *
           perform 100-open-files.
           perform 110-print-report-heading.
           perform 120-read-file.
           perform 140-process-pages
             until ws-eof-flag equals ws-eof-Y.
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
             from ws-heading-headings
             after advancing 1 line.
           write report-line
             from ws-heading-underlines
             after advancing 1 line.
      *
       160-print-footer.
      *

      *
       200-process-lines.
      *
           perform 400-output-detail-line.
           perform 120-read-file.
      *
       300-bonus-greater-than.
      *

      *
       310-bonus-less-than.
      *

      *
       320-bonus-under-minimum.
      *

      *
       330-bonus-over-maximum.
      *

      *
       400-output-detail-line.
      *
           write report-line
             from ws-line-count
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