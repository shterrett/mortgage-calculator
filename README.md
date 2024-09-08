# mortgage-calculator

Our mortgage processor, [Shellpoint](https://www.shellpointmtg.com/), has thus
far been incapable of correctly calculating our monthly payment. So I wrote this
tool to do the calculation as a way of tracking the amount we've overpaid and
showing what our expectations are.

## The Mortgage

The tool is specialized to our particular class of mortgage: an interest-only
loan where the interest is accrued daily based on the current principal amount.
This means that
1. The payments due each month are _only_ the interest accrued over the past
   month
2. the payment due each month can _decrease_ as the principal is paid.

N.B. that this is technically a variable-rate loan; however, the tool makes no
effort to account for a changing interest rate, because it's not intended to be
used over a many-year period. If we're still fighting Shellpoint in 7 years when
the interest rate changes, then I'll have plenty of time to refactor.

## The Calculation

Each day, from the given start date to end date, the following calculations
happen, in order:
1. If it is the first of the month, *accrued interest* is converted to *balance
   due*, and *accrued interest* is reset to 0.
2. Interest for the day is accrued, based on the current principal
3. Any payments are applied. An interest payment that is larger than the
   *balance due* (which only includes the previous month's accrued interest) can
   either 1. reduce the principal or 2. push the *balance due* negative for the
   next month. This is controlled by a command line option
4. If it is the first of the month or a payment was made (or `--show-all` was
   passed), and event is emitted with the current balances / payments.

## Usage

See `--help` for the command line options. For the `--payments` file path, a
text file that lists all payments should be passed. The file should be formatted

```
2024-07-01 p:100.00 i:1.00
2024-07-01 i:1.00
2024-07-01 p:100.00
```

where `p:100.00` is a principal payment, and `i:1.00` is an interest payment
(and the numbers are the actual amounts).

Each day should be represented at most once; if multiple separate payments were
made on a day, combine them.

There is no validation to this effect, but to make the most sense, the
`--start-date` should be the first of the month after the mortgage closed
(closed in July: `--start-date 2024-08-01`. This is the first day that the loan
accrues interest.

`--end-date` can be any date; if not passed, it defaults to the last payment
made. Setting it to the first of the following month is a reasonable thing to
do.
