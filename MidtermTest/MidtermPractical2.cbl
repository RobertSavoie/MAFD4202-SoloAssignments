       IDENTIFICATION DIVISION.
       PROGRAM-ID. MidtermPractical2.
       AUTHOR. Rob Savoie.
       DATE-WRITTEN. Feb 22/2023.
      *
       environment division.
       input-output section.
       file-control.
      *
            select in-file
                assign to '../../../data/test2.dat'
                organization is line sequential.
      *
            select print-file
                assign to '../../../data/test2.out'
                organization is line sequential.
      *
       data division.
       file section.
       fd in-file
          data record is tax-record
          record contains 33 characters.
      *
       01 tax-record.
           05 tax-name                 pic x(20).
           05 province-code            pic x(2).
           05 gross-salary             pic 9(6).
           05 exemption-amount         pic 9(5).
      *
       fd print-file
           data record is print-line
           record contains 64 characters.
      *
       01 print-line                   pic x(64).
      *
       working-storage section.
      *
       01 ws-total-1.
           05 filler                   pic x(45)    value spaces.
           05 filler                   pic x(11)    value "Total Tax: ".
           05 ws-total-tot-tax         pic $$$$,$$9 value spaces.
      *
       01 ws-total-2.
           05 filler                   pic x(35)    value spaces.
           05 filler                   pic x(21)    value
                                       "Average Ontario Tax: ".
           05 ws-avg-ont-tax           pic $$$$,$$9 value space.
      *
       01 ws-heading1.
           05 filler                   pic x(2)     value spaces.
           05 filler                   pic x(4)     value "Name".
           05 filler                   pic x(15)    value spaces.
           05 filler                   pic x(8)     value "Province".
           05 filler                   pic x(4)     value spaces.
           05 filler                   pic x(7)     value "Federal".
           05 filler                   pic x(5)     value spaces.
           05 filler                   pic x(10)    value "Provincial".
           05 filler                   pic x(3)     value spaces.
           05 filler                   pic x(5)     value "Total".
           05 filler                   pic x        value spaces.
      *
       01 ws-heading2.
           05 filler                   pic x(35)    value spaces.
           05 filler                   pic x(3)     value "Tax".
           05 filler                   pic x(11)    value spaces.
           05 filler                   pic x(3)     value "Tax".
           05 filler                   pic x(7)     value spaces.
           05 filler                   pic x(3)     value "Tax".
           05 filler                   pic xx       value spaces.
      *
       01 ws-detail-line.
           05 filler                   pic x(2).
           05 ws-name                  pic x(20).
           05 filler                   pic x(2).
           05 ws-prov                  pic xx.
           05 filler                   pic x(6).
           05 ws-fed-tax               pic zz,zz9.99.
           05 filler                   pic x(6).
           05 ws-prov-tax              pic zz,zzz.
           05 filler                   pic x(5).
           05 ws-tot-tax               pic zz,zzz.
      *
      *end of flag constants
       77 ws-eof                       pic x        value "n".
       77 ws-eof-y                     pic x        value "y".
       77 ws-eof-n                     pic x        value "n".
      *
      *constants
       77 cnst-fed-tax                 pic 9v999    value 0.214.
       77 cnst-alb-tax                 pic 9v999    value 0.078.
       77 cnst-ont-tax                 pic 9v9999   value 0.1491.
       77 cnst-que-tax                 pic 9v9999   value 0.1970.
       77 cnst-man-tax                 pic 9v9      value 0.1.
       77 cnst-ab-code                 pic xx       value "AB".
       77 cnst-on-code                 pic xx       value "ON".
       77 cnst-qc-code                 pic xx       value "QC".
       77 cnst-mb-code                 pic xx       value "MB".
      *
      *math variables
       01 math-storage.
           05 math-taxable-income      pic 9(6)v99.
           05 math-fed-tax             pic 9(5)v99.
           05 math-prov-tax            pic 9(5).
           05 math-total-tax           pic 9(5).
       01 math-totals.
           05 math-total-tot-tax       pic 9(6).
           05 math-total-ont           pic 9(6).
       01 on-counter                   pic 99       value 0.
      *
       procedure division.
       000-main.
      *
           perform 25-open-files.
      *
           perform 80-read-file.
      *
           write print-line
             from ws-heading1 after advancing 1 lines.
           write print-line
             from ws-heading2 before advancing 1 lines.
      *
           perform 100-process-file
             until ws-eof = "y".
      *
           write print-line
             from ws-total-1 after advancing 2 lines.
           write print-line
             from ws-total-2 after advancing 1 lines.
      *
           perform 75-close-files.
      *
           goback.
      *
       25-open-files.
      *
           open input in-file,
             output print-file.
      *
       50-clear-artifacts.
      *clear artifacts from working storage
      *
           move spaces to ws-detail-line.
           move spaces to math-storage.
      *
       75-close-files.
      *
           close print-file,
             in-file.
      *
       80-read-file.
      *
           read in-file
               at end
                   move "y" to ws-eof.
      *
       100-process-file.
      *
           perform 50-clear-artifacts.
      *    
           move tax-name       to ws-name.
           move province-code  to ws-prov.
      *
           perform 200-calculate-taxable.
           perform 300-calculate-fed-tax.
           perform 400-calculate-prov-tax.
           perform 500-calculate-totals.
           perform 600-calculate-average.
      *
           perform 150-write-lines.
      *
           perform 80-read-file.
      *
       150-write-lines.
      *
           write print-line
             from ws-detail-line
             after advancing 1 line.
      *
       200-calculate-taxable.
      *subtract personal exemption from gross income to 
      *give taxable income amount
      *
           subtract exemption-amount
               from gross-salary
             giving math-taxable-income.
      *
       300-calculate-fed-tax.
      *multiply taxable income by federal tax rate then
      *move the rounded value to the appropriate detail line variable
      *    
           multiply math-taxable-income
                 by cnst-fed-tax
             giving math-fed-tax rounded.
      *
           move math-fed-tax       to ws-fed-tax.
      *
       400-calculate-prov-tax.
      *calculate provincial taxes by multiplying taxable income by
      *provincial tax amount. ontario gets added to a total and
      *increments a counter for later use.
      *
           if      province-code equals cnst-on-code
             then
               multiply math-taxable-income
                     by cnst-ont-tax
                 giving math-prov-tax rounded
      *
                    add math-prov-tax
                     to math-total-ont rounded
      *
                    add 1
                     to on-counter
      *
           else if province-code equals cnst-ab-code
             then
               multiply math-taxable-income
                     by cnst-alb-tax
                 giving math-prov-tax rounded
      *
           else if province-code equals cnst-qc-code
             then
               multiply math-taxable-income
                     by cnst-que-tax
                 giving math-prov-tax rounded
      *
           else if province-code equals cnst-mb-code
             then
               multiply math-taxable-income
                     by cnst-man-tax
                 giving math-prov-tax rounded
      *
           end-if.
      *
           move math-prov-tax to ws-prov-tax.
      *
       500-calculate-totals.
      *add up totals and move to total sections of detail line
      *
           add math-fed-tax
            to math-total-tot-tax rounded.
      *
           add math-prov-tax
            to math-total-tot-tax rounded.
      *
           add math-fed-tax
            to math-prov-tax
        giving ws-tot-tax rounded.
      *
           move math-total-tot-tax to ws-total-tot-tax.
      *
       600-calculate-average.
      *calculate ontario average tax using counter and total
      *
           divide math-total-ont
               by on-counter
           giving ws-avg-ont-tax rounded.
      *
       end program MidtermPractical2.