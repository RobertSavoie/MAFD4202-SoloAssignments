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
      * TODO: correct file paths if needed
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
           record contains 120 characters.
      *
       01 report-line                  pic x(120).
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
       01 ws-heading-underlines.
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
      *eof constants
           77 ws-eof-flag                  pic x value "n".
           77 ws-eof-Y                     pic x value "y".
           77 ws-eof-N                     pic x value "n".
      *
      *constants

      *
       procedure division.
       000-main.
      *

      *
       100-process-pages.
      *

      *
       200-print-headings.
      *

      *
       300-process-lines.
      *

      *
       350-print-totals.
      *

      *
       400-bonus-greater-than.
      *

      *
       500-bonus-less-than.
      *

      *
       600-bonus-under-minimum.
      *

      *
       700-bonus-over-maximum.
      *

      *
       800-print-totals.
      *

      *
       end program A3-SalesComm.