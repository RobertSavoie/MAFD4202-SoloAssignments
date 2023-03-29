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
               88 mnt-code-d           value "D".
      *
           05 il-prt-num               pic 999.
           05 il-prt-num-text
           redefines il-prt-num        pic xxx.
      *
           05 il-prt-desc              pic x(10).
               88 desc-blank           value " ".
      *
           05 il-prt-price        pic 99v99.
               88 price-range       value 1.00 thru 50.00.
           05 il-prt-price-text
           redefines il-prt-price pic x(4).
      *
           05 il-prt-vend-num          pic 9(6).
               88 il-vend-range        value 1 thru 3.
           05 il-prt-vend-txt
           redefines il-prt-vend-num   pic x(6).
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
           05 ws-prt-txt
           redefines ws-prt-num        pic xxx.
           05 ws-prt-num-err           pic x(2) value space.

           05 filler                   pic x value spaces.
           05 ws-prt-desc              pic x(10) value spaces.
           05 ws-prt-desc-err          pic x(2) value spaces.

           05 filler                   pic x value spaces.
           05 ws-prt-price             pic 99v99.
           05 ws-prt-price-txt
           redefines ws-prt-price      pic x(5).
           05 ws-prt-price-err         pic x(2) value spaces.

           05 filler                   pic x value spaces.
           05 ws-prt-vend-num          pic 9(6).
           05 ws-prt-vend-txt
           redefines ws-prt-vend-num   pic x(6).
           05 ws-prt-vend-err          pic x(2) value spaces.
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
       77 eof-N                        pic x value "n".
      *
      *counters
       01 ws-counters.
           05 ws-line-counter          pic 99 value 0.
           05 ws-page-counter          pic 99 value 0.
           05 ws-error-counter         pic 9 value 0.
           05 ws-line-error-counter    pic 999 value 0.
           05 ws-total-error-lines     pic 999 value 0.
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
           move 0 to ws-error-counter.
      *
      *
       100-print-page-headings.
      *prints page heading
      *
           if ws-page-counter > 0
               add 1 to ws-page-counter
      *        move ws-page-counter to ws-page-number
               write output-line
                 from ws-name-line
                 after advancing page
      *
           else
               add 1 to ws-page-counter
      *        move ws-page-counter to ws-page-number
               write output-line
                 from ws-name-line
                 before advancing 1 line
           end-if.
      *
           write output-line
             from ws-error-report-line
             after advancing 1 line.
      *
           write output-line
             from ws-data-heading-line-one
             after advancing 2 line.
      *
           write output-line
             from ws-data-heading-line-two
             before advancing 1 line.
      *
       125-print-footers.
      *print footers
      *

      *
       200-process-pages.
      *process pages
      *
           perform 100-print-page-headings.
           perform 250-process-lines
           until ws-error-counter = lines-per-page
             or eof-flag = eof-Y.
      *
       250-process-lines.
      *process lines
      *
           perform 80-clear-artifacts.
           add 1 to ws-line-counter.
           perform 300-process-invalid-output.
           perform 310-process-error-descriptions.
           perform 50-read-input-file.
      *
       300-process-invalid-output.
      *perform all validations
      *
      *perform maintenence code validation
           if not mnt-code-valid
               add 1 to ws-error-counter
               move err-indicator to ws-prt-mnt-err
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

           end-if.
      *
      *creates the line to send to output
           if ws-error-counter > 0
               add 1 to ws-line-error-counter
               add 1 to ws-total-error-lines
               move ws-line-counter to ws-record-number
               move il-prt-mnt-code to ws-prt-mnt-code
               move il-prt-num to ws-prt-num
               move il-prt-desc to ws-prt-desc
               move il-prt-price to ws-prt-price
               move il-prt-vend-num to ws-prt-vend-num
               write output-line
                 from ws-error-line
                 after advancing 1 line
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

           end-if.
      *
       end program A6-DataValidation.
