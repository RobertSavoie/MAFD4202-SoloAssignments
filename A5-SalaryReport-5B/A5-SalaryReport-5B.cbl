       identification division.
       program-id. A5-SalaryReport-5B.
       author. Rob Savoie.
       date-written. Mar 12/2023.
      *
       environment division.
      *
       input-output section.
       file-control.
      *
           select input-file
               assign to "../../../../data/A5-SalaryData-NonGrad.dat"
               organization is line sequential.
      *
           select output-file
               assign to "../../../../data/A5-SalaryReport-5B.out"
               organization is line sequential.
      *
       data division.
       file section.
      *
       fd input-file
           data record is input-line
           record contains 36 characters.
      *
       01 input-line.
           05 il-emp-num               pic xxx.
           05 il-emp-name              pic x(15).
           05 il-emp-years             pic 99.
           05 il-emp-code              pic x.
           05 il-emp-sal               pic 9(5)v99.
           05 il-emp-budget-est        pic 9(6)v99.
      *
       fd output-file
           data record is output-line
           record contains 110 characters.
      *
       01 output-line                  pic x(110)   value spaces.
      *
       working-storage section.
      *
      *name line
       01 ws-name-line.
           05 filler                   pic x(24)   value
                                       "Rob Savoie, Assignment 5".
           05 filler                   pic x(86)   value spaces.
      *
      *page heading
       01 ws-page-heading.
           05 filler                   pic x(33)   value spaces.
           05 filler                   pic x(35)   value
                                  "NON-GRADUATE EMPLOYEE SALARY REPORT".
           05 filler                   pic x(8)    value spaces.
           05 filler                   pic x(4)    value "PAGE".
           05 filler                   pic x(11)   value spaces.
           05 ws-page-number           pic z9.
           05 filler                   pic x(17).
      *
      *top column header
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
           05 filler                   pic x(9)    value spaces.
           05 filler                   pic x(6)    value "BUDGET".
           05 filler                   pic x(7)    value spaces.
           05 filler                   pic x(6)    value "BUDGET".
           05 filler                   pic x(6)    value spaces.
      *
      *bottom column header
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
           05 filler                   pic x(6)    value spaces.
           05 filler                   pic x(8)    value "ESTIMATE".
           05 filler                   pic x(7)    value spaces.
           05 filler                   pic x(4)    value "DIFF".
           05 filler                   pic x(7)    value spaces.
      *
      *formatted print detail line
       01 ws-print-line.
           05 filler                   pic x       value spaces.
           05 ws-emp-num               pic xxx     value spaces.
           05 filler                   pic x       value spaces.
           05 ws-emp-name              pic x(15)   value spaces.
           05 filler                   pic xx      value spaces.
           05 ws-emp-year              pic z9      value 0.
           05 filler                   pic xx      value spaces.
           05 ws-emp-position          pic x(8)    value spaces.
           05 filler                   pic xx      value spaces.
           05 ws-emp-pres-salary       pic zz,zz9.99
                                                   value 0.
           05 filler                   pic xx      value spaces.
           05 ws-emp-increase-perc     pic x(5)    value space.
           05 filler                   pic xxx     value spaces.
           05 ws-emp-increase-pay      pic $$$,$$9.99
                                                   value 0.
           05 filler                   pic x       value spaces.
           05 ws-emp-new-salary        pic +$z,zzz,zz9.99
                                                   value 0.
           05 filler                   pic xx      value spaces.
           05 ws-emp-budget-est        pic $zzz,zz9.99
                                                   value 0.
           05 filler                   pic xx      value spaces.
           05 ws-emp-budget-diff       pic $$$$,$$9.99
                                                   value 0.
           05 filler                   pic x(4)    value spaces.
      *
      *employee class heading
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
      *employee class totals
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
      *first average line
       01 ws-increase-average-one.
           05 filler                   pic x       value spaces.
           05 filler                   pic x(18)   value
                                       "AVERAGE INCREASES:".
           05 filler                   pic xxx     value spaces.
           05 filler                   pic x(5)    value "PROG=".
           05 filler                   pic x(8)    value spaces.
           05 ws-prog-average          pic z,zz9.99
                                                   value 0.
           05 filler                   pic x(5)    value spaces.
           05 filler                   pic x(8)    value "JR PROG=".
           05 filler                   pic x(4)    value spaces.
           05 ws-jrprog-average        pic z,zz9.99
                                                   value 0.
           05 filler                   pic x(12)   value spaces.
      *
      *regular math variables
       01 ws-math-store.
           05 ws-math-increase-pay     pic 9(9)v99.
           05 ws-math-new-salary       pic 9(9)v99.
           05 ws-math-average          pic 9(9)v99.
           05 ws-math-percent          pic 9v999.
      *
      *totals used for math
       01 ws-math-totals.
           05 ws-math-prog-total       pic 9(7)v9(4).
           05 ws-math-jrprog-total     pic 9(7)v9(4).
      *
      *page specific counters
       01 ws-page-counters.
           05 ws-cntr-prog             pic 99      value 0.
           05 ws-cntr-jrprog           pic 99      value 0.
           05 ws-cntr-unclass          pic 99      value 0.
      *
      *global counters
       01 ws-global-counters.
           05 ws-global-cntr-page      pic 99      value 0.
           05 ws-global-cntr-line      pic 99      value 0.
           05 ws-global-cntr-prog      pic 99      value 0.
           05 ws-global-cntr-jrprog    pic 99      value 0.
      *
      *eof constants
       77 eof-flag                     pic x       value "n".
       77 eof-Y                        pic x       value "y".
       77 eof-N                        pic x       value "n".
      *
      *constants
       77 cnst-lines-per-page          pic 99      value 20.
       77 cnst-nongrad-prog-start      pic 99      value 10.
       77 cnst-nongrad-jr-start        pic 9       value 4.
       77 cnst-prog                    pic x(8)    value "    PROG".
       77 cnst-jrprog                  pic x(8)    value " JR PROG".
       77 cnst-prog-increase           pic x(5)    value " 6.7%".
       77 cnst-jrprog-increase         pic x(5)    value " 3.2%".
       77 cnst-unclass-increase        pic x(5)    value spaces.
       77 cnst-math-prog-increase      pic 9v9     value 6.7.
       77 cnst-math-jrprog-increase    pic 9v9     value 3.2.
       77 cnst-math-unclass-increase   pic 9       value 0.
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
             until eof-flag equals eof-Y.
      *
           perform 150-print-average-increases.
      *
           perform 75-close-files.
      *
           goback.
       25-open-files.
      *
      *open files
           open input input-file.
           open output output-file.
      *
       50-read-input-file.
      *
      *read input file
           read input-file
               at end
                   move eof-Y to eof-flag.
      *
       75-close-files.
      *
      *closes files
           close input-file
             output-file.
      *
       80-clear-artifacts.
      *
      *clears output-line and ws-math-store
           move spaces to output-line.
           move spaces to ws-math-store.
      *
       90-clear-page-counters.
      *
      *resets counters to 0 for each page
           move 0 to ws-cntr-prog.
           move 0 to ws-cntr-unclass.
      *
       100-print-report-heading.
      *
      *prints report heading
           write output-line
             from ws-name-line
             after advancing 1 line.
      *
       125-print-page-headings.
      *
      *prints page headings
           if ws-global-cntr-page > 0
               add 1 to ws-global-cntr-page
               move ws-global-cntr-page to ws-page-number
      *
               write output-line
                 from ws-page-heading
                 after advancing page
           else
               add 1 to ws-global-cntr-page
               move ws-global-cntr-page to ws-page-number
      *
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
       150-print-average-increases.
      *
      *prints average salary increase footers
           write output-line
             from ws-increase-average-one.
      *
       200-process-pages.
      *
      *processes each page until the counter goes over 10
           perform 90-clear-page-counters.
           perform 125-print-page-headings.
           perform 250-process-lines
             varying ws-global-cntr-line from 1 by 1
             until ws-global-cntr-line > cnst-lines-per-page
             or eof-flag = eof-Y.
           perform 650-print-totals.
      *
       250-process-lines.
      *
      *processes the lines for each page
           perform 80-clear-artifacts.
           perform 300-create-output-line.
           perform 50-read-input-file.
      *
       300-create-output-line.
      *
      *determines if an employee is a graduate
           if il-emp-years > cnst-nongrad-prog-start   then
               move cnst-prog      to ws-emp-position
               add 1               to ws-cntr-prog
               add 1               to ws-global-cntr-prog
           end-if
           if il-emp-years <= cnst-nongrad-prog-start  and
             il-emp-years  > cnst-nongrad-jr-start     then
               move cnst-jrprog    to ws-emp-position
               add 1               to ws-cntr-jrprog
               add 1               to ws-global-cntr-jrprog
           end-if
           if il-emp-years <= cnst-nongrad-jr-start    then
               move spaces         to ws-emp-position
               add 1               to ws-cntr-unclass
           end-if.
           perform 400-calculations.
           move ws-cntr-prog       to ws-prog-total.
           move ws-cntr-unclass    to ws-unclassified-total.
           move il-emp-num         to ws-emp-num.
           move il-emp-name        to ws-emp-name.
           move il-emp-years       to ws-emp-year.
           move il-emp-sal         to ws-emp-pres-salary.
           move il-emp-budget-est  to ws-emp-budget-est.
           write output-line
             from ws-print-line
             before advancing 1 line.
      *
       400-calculations.
      *
      *runs all calculation paragraphs
           perform 430-calculate-increase-prog.
           perform 440-calculate-increase-jrprog.
           perform 450-calculate-increase-unclass.
           perform 460-calculate-average-increases.
      *
       430-calculate-increase-prog.
      *
      *calculates pay increase for programmers
      *    creates usable number for multiplication
           divide cnst-math-prog-increase
               by 100
           giving ws-math-percent.
      *
      *    sends percentage string to print line
           if ws-emp-position = cnst-prog then
              move cnst-prog-increase   to ws-emp-increase-perc
      *
      *    multiplies salary by percent to get increase amount
               multiply il-emp-sal
                     by ws-math-percent
                 giving ws-math-increase-pay rounded
      *    adds increase pay to total and moves increase amount
      *    to print line
               add ws-math-increase-pay
                to ws-math-prog-total
              move ws-math-increase-pay to ws-emp-increase-pay
      *
      *    adds increase amount to base salary the moves it
      *    to the print line
               add ws-math-increase-pay
                to il-emp-sal
            giving ws-math-new-salary
              move ws-math-new-salary   to ws-emp-new-salary
           end-if.
      *
       440-calculate-increase-jrprog.
      *calculates pay increase for junior programmers
      *
      *    creates usable number for multiplication
           divide cnst-math-jrprog-increase
               by 100
           giving ws-math-percent.
      *
      *    sends percentage string to print line
           if ws-emp-position = cnst-jrprog then
               move cnst-jrprog-increase to ws-emp-increase-perc
      *
      *    multiplies salary by percent to get increase amount
               multiply il-emp-sal
                     by ws-math-percent
                 giving ws-math-increase-pay rounded
      *    adds increase pay to total and moves increase amount
      *    to print line
               add ws-math-increase-pay
                to ws-math-jrprog-total
              move ws-math-increase-pay  to ws-emp-increase-pay
      *
      *    adds increase amount to base salary the moves it
      *    to the print line
               add ws-math-increase-pay
                to il-emp-sal
            giving ws-math-new-salary
              move ws-math-new-salary    to ws-emp-new-salary
           end-if.
      *
       450-calculate-increase-unclass.
      *
      *calculates pay increase for junior programmers
           divide cnst-math-unclass-increase
               by 100
           giving ws-math-percent.
      *
      *    sends percentage string to print line
           if ws-emp-position = " " then
               move cnst-unclass-increase to ws-emp-increase-perc
      *
      *    multiplies salary by percent to get increase amount
           multiply il-emp-sal
                 by ws-math-percent
             giving ws-math-increase-pay rounded
               move ws-math-increase-pay  to ws-emp-increase-pay
      *
      *    adds increase amount to base salary the moves it
      *    to the print line
               add ws-math-increase-pay
                to il-emp-sal
            giving ws-math-new-salary
              move ws-math-new-salary     to ws-emp-new-salary
           end-if.
      *
       460-calculate-average-increases.
      *
      *calculates average salary increases for each position
           divide ws-math-prog-total
               by ws-global-cntr-prog
           giving ws-math-average rounded.
             move ws-math-average to ws-prog-average.
      *
           divide ws-math-jrprog-total
               by ws-global-cntr-jrprog
           giving ws-math-average rounded.
             move ws-math-average to ws-jrprog-average.
      *
       650-print-totals.
      *
      *print position totals
           write output-line
             from ws-class-heading
             after advancing 1 line.
           write output-line
             from ws-class-totals
             before advancing 2 lines.
      *
       end program A5-SalaryReport-5B.