       identification division.
       program-id. A4-SalaryReport.
       author. Rob Savoie.
       date-written. Mar 08/2023.
      *
       environment division.
      *
       input-output section.
       file-control.
      *
           select input-file
               assign to "../../../data/A4.dat"
               organization is line sequential.
      *
           select output-file
               assign to "../../../data/A4-SalaryReport.out"
               organization is line sequential.
      *
       data division.
       file section.
       fd input-file
           data record is input-line
           record contains 28 characters.
      *
       01 input-line.
           05 il-emp-num               pic xxx.
           05 il-emp-name              pic x(15).
           05 il-emp-code              pic x.
           05 il-emp-years             pic 99.
           05 il-emp-sal               pic 9(5)v99.
      *
       fd output-file
           data record is output-line
           record contains 80 characters.
      *
       01 output-line                  pic x(80)   value spaces.
      *
       working-storage section.
      *
       01 ws-name-line.
           05 filler                   pic x(24)   value
                                       "Rob Savoie, Assignment 4".
           05 filler                   pic x(15)   value spaces.
           05 filler                   pic x(8)    value "20220111".
           05 filler                   pic x(26)   value spaces.
           05 filler                   pic x(7)    value "1951043".
      *
       01 ws-page-heading.
           05 filler                   pic x(30)   value spaces.
           05 filler                   pic x(23)   value
                                       "EMPLOYEE SALARY REPORT".
           05 filler                   pic x(14)   value spaces.
           05 filler                   pic x(4)    value "PAGE".
           05 filler                   pic x       value spaces.
           05 ws-page-number           pic z9.
           05 filler                   pic x(6).
      *
       01 ws-column-head-one.
           05 filler                   pic x       value spaces.
           05 filler                   pic xxx     value "EMP".
           05 filler                   pic xx      value spaces.
           05 filler                   pic xxx     value "EMP".
           05 filler                   pic x(28)   value spaces.
           05 filler                   pic x(7)    value "PRESENT".
           05 filler                   pic xx      value spaces.
           05 filler                   pic x(8)    value "INCREASE".
           05 filler                   pic x(5)    value spaces.
           05 filler                   pic xxx     value "PAY".
           05 filler                   pic x(11)   value spaces.
           05 filler                   pic xxx     value "NEW".
           05 filler                   pic x(4)    value spaces.
      *
       01 ws-column-head-two.
           05 filler                   pic x       value spaces.
           05 filler                   pic xxx     value "NUM".
           05 filler                   pic xx      value spaces.
           05 filler                   pic x(4)    value "NAME".
           05 filler                   pic x(10)   value spaces.
           05 filler                   pic x(5)    value "YEARS".
           05 filler                   pic x       value spaces.
           05 filler                   pic x(8)    value "POSITION".
           05 filler                   pic x(4)    value spaces.
           05 filler                   pic x(6)    value "SALARY".
           05 filler                   pic x(5)    value spaces.
           05 filler                   pic x       value "%".
           05 filler                   pic x(7)    value spaces.
           05 filler                   pic x(8)    value "INCREASE".
           05 filler                   pic x(7)    value spaces.
           05 filler                   pic x(6)    value "SALARY".
           05 filler                   pic xx      value spaces.
      *
       01 ws-print-line.
           05 filler                   pic x       value spaces.
           05 ws-emp-num               pic xxx     value spaces.
           05 filler                   pic x       value spaces.
           05 ws-emp-name              pic x(15)   value spaces.
           05 filler                   pic xx      value spaces.
           05 ws-emp-year              pic z9.
           05 filler                   pic xx      value spaces.
           05 ws-emp-position          pic x(8)    value spaces.
           05 filler                   pic xx      value spaces.
           05 ws-emp-pres-salary       pic zz,zz9.99.
           05 filler                   pic xx      value spaces.
           05 ws-emp-increase-perc     pic z9.9.
           05 filler                   pic x       value "%".
           05 filler                   pic xxx     value spaces.
           05 ws-emp-increase-pay      pic $$$,$$$.99.
           05 filler                   pic x       value "+".
           05 filler                   pic x       value spaces.
           05 filler                   pic x       value "$".
           05 filler                   pic xx      value spaces.
           05 ws-emp-new-salary        pic zzz,zz9.99.
      *
       01 ws-class-heading.
           05 filler                   pic x       value spaces.
           05 filler                   pic x(15)   value
                                       "EMPLOYEE CLASS:".
           05 filler                   pic x(8)    value spaces.
           05 filler                   pic x(7)    value "Analyst".
           05 filler                   pic x(4)    value spaces.
           05 filler                   pic x(8)    value "Sen Prog".
           05 filler                   pic x(4)    value spaces.
           05 filler                   pic x(4)    value "Prog".
           05 filler                   pic x(4)    value spaces.
           05 filler                   pic x(7)    value "Jr Prog".
           05 filler                   pic x(4)    value spaces.
           05 filler                   pic x(12)   value "Unclassified".
           05 filler                   pic x(2)    value spaces.
      *
       01 ws-class-totals.
           05 filler                   pic x       value spaces.
           05 filler                   pic x(15)   value
                                       "# ON THIS PAGE:".
           05 filler                   pic x(13)   value spaces.
           05 ws-analyst-total         pic z9      value 0.
           05 filler                   pic x(10)   value spaces.
           05 ws-sen-prog-total        pic z9      value 0.
           05 filler                   pic x(6)    value spaces.
           05 ws-prog-total            pic z9      value 0.
           05 filler                   pic x(9)    value spaces.
           05 ws-jrprog-total          pic z9      value 0.
           05 filler                   pic x(14)   value spaces.
           05 ws-unclassified-total    pic z9      value 0.
           05 filler                   pic xx      value spaces.
      *
       01 ws-math-store.
           05 ws-new-salary            pic 9(6).
      *
       01 ws-counters.
           05 ws-cntr-page             pic 99      value 0.
           05 ws-cntr-line             pic 99      value 0.
      *
      *eof constants
       77 ws-eof-flag                  pic x       value "n".
       77 ws-eof-Y                     pic x       value "y".
       77 ws-eof-N                     pic x       value "n".
      *
      *constants
       77 ws-lines-per-page            pic 99      value 10.
      *
       procedure division.
      *
       000-main.
      *
           perform 25-open-files.
      *
           perform 50-read-input-file.
      *
           perform 100-print-report-heading.
      *
           perform 200-process-pages
             until ws-eof-flag equals ws-eof-Y.
      *
           perform 75-close-files.
      *
           goback.
      *
       25-open-files.
      *
           open input input-file.
           open output output-file.
      *
       50-read-input-file.
      *
           read input-file
               at end
                   move ws-eof-Y to ws-eof-flag.
      *
       75-close-files.
      *
           close input-file
               output-file.
      *
       100-print-report-heading.
      *
           write output-line
             from ws-name-line
             after advancing 1 line.
      *
       125-print-page-headings.
      *
           if ws-cntr-page > 0
               add 1 to ws-cntr-page
               move ws-cntr-page to ws-page-number

               write output-line
                 from ws-page-heading
                 after advancing page
           else
               add 1 to ws-cntr-page
               move ws-cntr-page to ws-page-number

               write output-line
                 from ws-page-heading
                 after advancing 2 lines
           end-if.
           write output-line
             from ws-column-head-one
             after advancing 2 lines.
           write output-line
             from ws-column-head-two
             before advancing 2 lines.
      *
       150-clear-artifacts.
      *
           move spaces to ws-print-line.
           move spaces to ws-math-store.
      *
       200-process-pages.
      *
           perform 125-print-page-headings.
           perform 250-process-lines
             varying ws-cntr-line from 0 by 1
             until ws-cntr-line greater than ws-lines-per-page
             or ws-eof-flag equals ws-eof-Y.
           perform 650-print-totals.
      *
       250-process-lines.
      *
           perform 600-create-output-line.
           perform 50-read-input-file.
      *
       600-create-output-line.
      *
           move il-emp-num         to ws-emp-num.
           move il-emp-name        to ws-emp-name.
           move il-emp-years       to ws-emp-year.
           move il-emp-sal         to ws-emp-pres-salary.
           write output-line
             from ws-print-line
             before advancing 1 line.
      *
       650-print-totals.
      *
           write output-line
             from ws-class-heading
             after advancing 1 line.
           write output-line
             from ws-class-totals
             before advancing 2 lines.
      *
       end program A4-SalaryReport.
