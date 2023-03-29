       identification division.
       program-id. A6-DataValidation.
      *
       environment division.
      *
       input-output section.
       file-control.
      *
           select input-file
               assign to "../../../data/A6.dat"
               organization is line sequential.
      *
           select output-file
               assign to "../../../data/A6-DataValidation.out"
               organization is line sequential.
      *
       data division.
      *
       file section.
       fd input-file
           data record is input-line
           record contains 24 characters.
      *
       01 input-line.
           05 il-prt-mnt-code          pic x.
               88 mnt-code-valid       value "A", "C", "D".
               88 mnt-code-a           value "A".
               88 mnt-code-c           value "C".
               88 mnt-code-d           value "D".
      *
           05 il-prt-num               pic 999.
      *
           05 il-prt-desc              pic x(10).
               88 desc-blank           value " ".
      *
           05 il-prt-price             pic 99v99.
               88 price-range          value 1.00 thru 50.00.
      *
           05 il-prt-vend-num          pic 9(6).
      *
       fd output-file
           data record is output-line
           record contains 50 characters.
      *
       01 output-line                  pic x(50)   value spaces.
      *
       working-storage section.
      *
      *name line
       01 ws-name-line.
           05 filler                   pic x(24)   value
                                       "ROB SAVOIE, ASSIGNMENT 6".
           05 filler                   pic x(26)   value spaces.
      *
      *page headings
       01 ws-error-report-line.
           05 filler                   pic x(19) value spaces.
           05 filler                   pic x(12) value "ERROR REPORT".
           05 filler                   pic x(19) value spaces.
       01 ws-data-heading-line-one.
           05 filler                   pic x(2) value spaces.
           05 filler                   pic x(6) value "Record".
           05 filler                   pic x(8) value spaces.
           05 filler                   pic x(24) value
                                       "--------Raw Data--------".
           05 filler                   pic x(10) value spaces.
       01 ws-data-heading-line-two.
           05 filler                   pic x(2) value spaces.
           05 filler                   pic x(6) value "Number".
           05 filler                   pic x(13) value spaces.
           05 filler                   pic x(14) value "Error Messages".
           05 filler                   pic x(15) value spaces.
      *
      *output line
       01 ws-error-line.
           05 filler                   pic x(3) value spaces.
           05 ws-record-number         pic z9.
           05 filler                   pic x(6) value spaces.
           05 ws-prt-mnt-code          pic x value spaces.
           05 ws-prt-mnt-err           pic x(2) value spaces.

           05 filler                   pic x value spaces.
           05 ws-prt-num               pic 999.
           05 ws-prt-num-err           pic x(2) value space.

           05 filler                   pic x value spaces.
           05 ws-prt-desc              pic x(10) value spaces.
           05 ws-prt-desc-err          pic x(2) value spaces.

           05 filler                   pic x value spaces.
           05 ws-prt-price             pic 99v99.
           05 ws-prt-price-err         pic x(2) value spaces.

           05 filler                   pic x value spaces.
           05 ws-prt-vend-num          pic 9(6).
           05 ws-prt-vend-err          pic x(2) value spaces.
      *
      *total lines
       01 ws-totals.
           05 filler                   pic x(2) value spaces.
           05 filler                   pic x(6) value "TOTALS".
           05 filler                   pic x(42) value spaces.
       01 ws-input.
           05 filler                   pic x(15) value
                                       "INPUT        - ".
           05 ws-number-of-records     pic z9.
       01 ws-good.
           05 filler                   pic x(15) value
                                       "GOOD         - ".
           05 ws-number-of-good        pic z9.
       01 ws-error.
           05 filler                   pic x(15) value
                                       "IN ERROR     - ".
           05 ws-number-of-error       pic z9.
       01 ws-good-adds.
           05 filler                   pic x(15) value
                                       "GOOD ADDS    - ".
           05 ws-number-of-adds        pic z9.
       01 ws-good-changes.
           05 filler                   pic x(15) value
                                       "GOOD CHANGES - ".
           05 ws-number-of-changes     pic z9.
       01 ws-good-deletes.
           05 filler                   pic x(15) value
                                       "GOOD DELETES - ".
           05 ws-number-of-deletes     pic z9.
      *
      *error descriptions
       01 ws-error-description-line.
           05 filler                   pic x(10) value spaces.
           05 ws-error-description     pic x(23) value spaces.
           05 filler                   pic x(16) value spaces.
      *
      *eof constants
       77 eof-flag                     pic x value "n".
       77 eof-Y                        pic x value "y".
      *
      *vendor table
       01 vend-num-tbl.
           05 ws-element-one           pic 9 occurs 6 times.
               88 valid-vend-num       value 1, 2, 3.
      *
      *counters
       01 ws-counters.
           05 ws-line-counter          pic 99 value 0.
           05 ws-page-counter          pic 99 value 0.
           05 ws-error-counter         pic 9 value 0.
           05 ws-total-error-lines     pic 999 value 0.
           05 ws-total-good            pic 99 value 0.
           05 ws-total-good-adds       pic 99 value 0.
           05 ws-total-good-changes    pic 99 value 0.
           05 ws-total-good-deletes    pic 99 value 0.
           05 ws-page-lines            pic 99 value 0.
      *
      *constants
       77 lines-per-page               pic 9 value 5.
       77 err-indicator                pic x(2) value "<-".
       77 mntcode-err                  pic x(19) value
                                       "WRONG MAINT CODE".
       77 prtnbr-err                   pic x(23) value
                                       "PART NO. NOT NUMERIC".
       77 price-err                    pic x(19) value
                                       "PRICE IN TROUBLE".
       77 desc-err-1                   pic x(22) value
                                       "DESCRIPTION MISSING".
       77 desc-err-2                   pic x(20) value
                                       "NON ALPHA IN DESC".
       77 vend-err                     pic x(22) value
                                       "WRONG VENDOR SERIES".
      *
       procedure division.
       000-main.
      *
           perform 25-open-files.
      *
           perform 50-read-input-file.
      *
           perform 200-process-pages
               until eof-flag = eof-Y.
      *
           perform 125-print-footers.
      *
           perform 75-close-files.
      *
           goback.
      *
       25-open-files.
      *open files
      *
           open input input-file.
           open output output-file.
      *
       50-read-input-file.
      *read input file
      *
           read input-file
               at end
                   move eof-Y to eof-flag.
      *
       75-close-files.
      *closes files
      *
           close input-file
             output-file.
      *
       80-clear-artifacts.
      *clears output-line
      *
           move spaces to ws-error-line.
           move spaces to output-line.
           move 0 to ws-error-counter.
           
      *
      *
       100-print-page-headings.
      *prints page heading
      *
           if ws-page-counter > 0
               add 1 to ws-page-counter
               write output-line
                 from ws-name-line
                 after advancing page
      *
           else
               add 1 to ws-page-counter
               write output-line
                 from ws-name-line
                 after advancing 0 lines
           end-if.
      *
           write output-line
             from ws-error-report-line
             after advancing 2 lines.
      *
           write output-line
             from ws-data-heading-line-one
             after advancing 2 lines.
      *
           write output-line
             from ws-data-heading-line-two
             before advancing 1 line.
      *
       125-print-footers.
      *print footers
      *
           write output-line
             from ws-totals
             after advancing 2 lines.
      *
           write output-line
             from ws-input
             after advancing 2 lines.
      *
           write output-line
             from ws-good
             after advancing 2 lines.
      *
           write output-line
             from ws-error
             after advancing 2 lines.
      *
           write output-line
             from ws-good-adds
             after advancing 2 lines.
      *
           write output-line
             from ws-good-changes
             after advancing 2 lines.
      *
           write output-line
             from ws-good-deletes
             after advancing 2 lines.
      *
       200-process-pages.
      *process pages
      *
           perform 100-print-page-headings.
           perform 250-process-lines
             until ws-page-lines = lines-per-page
             or eof-flag = eof-Y.
           move 0 to ws-page-lines.
      *
       250-process-lines.
      *process lines
      *
           perform 80-clear-artifacts.
           perform 300-process-invalid-output.
           perform 310-process-error-descriptions.
           perform 50-read-input-file.
      *
       300-process-invalid-output.
      *perform all validations
           add 1 to ws-line-counter.
           move ws-line-counter to ws-number-of-records.
      *
      *perform maintenence code validation
           if not mnt-code-valid
               add 1 to ws-error-counter
               move err-indicator to ws-prt-mnt-err
           else 
               
           end-if.
      *
      *perform part number validation
           if not mnt-code-d
               if il-prt-num is not numeric
                   add 1 to ws-error-counter
                   move err-indicator to ws-prt-num-err
               end-if
           end-if.
      *
      *perform part description validation
           if not mnt-code-d
               if desc-blank
                   add 1 to ws-error-counter
                   move err-indicator to ws-prt-desc-err
               end-if
               if il-prt-desc is not alphabetic
                   add 1 to ws-error-counter
                   move err-indicator to ws-prt-desc-err
               end-if
           end-if.
      *
      *perform unit price validation
           if not mnt-code-d
               if il-prt-price numeric
                   if not price-range
                       add 1 to ws-error-counter
                       move err-indicator to ws-prt-price-err
                   end-if
               else
                   add 1 to ws-error-counter
                   move err-indicator to ws-prt-price-err
               end-if
           end-if.
      *
      *perform vendor number validation
           if not mnt-code-d
               move il-prt-vend-num to vend-num-tbl
               if not valid-vend-num(1)
                   add 1 to ws-error-counter
                   move err-indicator to ws-prt-vend-err
               end-if
           end-if.
      *
      *creates the line to send to output
           if ws-error-counter > 0
               add 1 to ws-total-error-lines
               add 1 to ws-page-lines
      *
               move ws-total-error-lines to ws-number-of-error
               move ws-line-counter to ws-record-number
               move il-prt-mnt-code to ws-prt-mnt-code
               move il-prt-num to ws-prt-num
               move il-prt-desc to ws-prt-desc
               move il-prt-price to ws-prt-price
               move il-prt-vend-num to ws-prt-vend-num
      *
               write output-line
                 from ws-error-line
                 after advancing 1 line
      *
           else if mnt-code-a
               add 1 to ws-total-good-adds
               move ws-total-good-adds to ws-number-of-adds
      *
               add 1 to ws-total-good
               move ws-total-good to ws-number-of-good
           else if mnt-code-c
               add 1 to ws-total-good-changes
               move ws-total-good-changes to ws-number-of-changes
      *
               add 1 to ws-total-good
               move ws-total-good to ws-number-of-good
           else if mnt-code-d
               add 1 to ws-total-good-deletes
               move ws-total-good-deletes to ws-number-of-deletes
      *
               add 1 to ws-total-good
               move ws-total-good to ws-number-of-good
           end-if.
       310-process-error-descriptions.
      *perform all validations
      *
      *perform maintenence code validation
           if not mnt-code-valid
               add 1 to ws-error-counter
               move mntcode-err to ws-error-description
               write output-line from ws-error-description-line
           end-if.
      *
      *perform part number validation
           if not mnt-code-d
               if il-prt-num is not numeric
                   add 1 to ws-error-counter
                   move prtnbr-err to ws-error-description
                   write output-line from ws-error-description-line
               end-if
           end-if.
      *
      *perform part description validation
           if not mnt-code-d
               if desc-blank
                   add 1 to ws-error-counter
                   move desc-err-1 to ws-error-description
                   write output-line from ws-error-description-line
               end-if
               if il-prt-desc is not alphabetic
                   add 1 to ws-error-counter
                   move desc-err-2 to ws-error-description
                   write output-line from ws-error-description-line
               end-if
           end-if.
      *
      *perform unit price validation
           if not mnt-code-d
               if il-prt-price numeric
                   if not price-range
                       add 1 to ws-error-counter
                       move price-err to ws-error-description
                       write output-line from ws-error-description-line
                   end-if
               else
                   add 1 to ws-error-counter
                   move price-err to ws-error-description
                   write output-line from ws-error-description-line
               end-if
           end-if.
      *
      *perform vendor number validation
           if not mnt-code-d
               move il-prt-vend-num to vend-num-tbl
               if not valid-vend-num(1)
                   move vend-err to ws-error-description
                   write output-line from ws-error-description-line
               end-if
           end-if.
      *
       end program A6-DataValidation.