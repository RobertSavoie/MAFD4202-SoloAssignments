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
       01 ws-eof-flag                  pic x value 'n'.
      *
       01 ws-heading1-name-line.

      *
       01 ws-heading2-title.

      *
       01 ws-heading3-headings.

      *
       01 ws-heading4-underlines.

      *
       01 ws-report-detail-line.

      *
       01 ws-total-line.

      *
       01 ws-num-max-line.

      *
       01 ws-num-min-line.

      *

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
       300-print-totals.
      *

      *
       end program A3-SalesComm.