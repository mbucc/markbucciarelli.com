---
title: Double Entry Accounting
date: July 31, 2022
tags: finance
---

The rules are deceptively simple.
==============================================

Rule #1: Assets = Liabilities + Equity
----------------------------------------------

This equality must hold after you close the period.


Rule #2: Debits = Credits for every transaction.
----------------------------------------------

This is why this is called "double-entry" accounting.  Every time
money moves, there is one entry for the account that is debited
and one entry for the account that is credited.

A double-entry transaction is represented as a "T chart",
with debits
on left and credits
on the right.

```
            Debits  | Credits
            --------+--------
                    |
                    |
                    |
```



Rule #3: A credit does not always increase an account&rsquo;s balance.
----------------------------------------------

Whether the balance goes up or down depends on the type of account.


| Account     | Increased By | Decreased By | Examples                                     |
|-------------|--------------|--------------|----------------------------------------------|
| Assets      | Debit        | Credit       | cash, checking account, real estate          |
| Expenses    | Debit        | Credit       | food, entertainment, interest payments, etc. |
| Liabilities | Credit       | Debit        | mortgage principle, credit card balance      |
| Equity      | Credit       | Debit        | your net worth                               |
| Revenue     | Credit       | Debit        | paycheck, interest earned, bond dividends    |



Book of Accounts (more commonly called &ldquo;the books&rdquo;)
==============================================

Let&rsquo;s walk through an example&mdash;on December 1, 2022 your
parents loan you $3,000 for first and last month&rsquo;s rent (which
you pay on the 20th) and on December 25, 2022 you get your first weekly paycheck
with the following amounts on your paystub:

       1,000      Gross wages
         120      Federal income tax
          60      State income tax
          15      Medicare tax
          62      Social Security tax
          50      401k savings
      ------
         693      Net pay

          50      401k employer matches your 401K contribution

All this stuff is automated with accounting software, but I find it
instructive to work through these old-school books.

The General Journal (the &ldquo;book of original entry&rdquo;)
----------------------------------------------

    Date         Accounts & Description                 Debit      Credit

    12/01/2022   Checking account                       3,000
                 Liabilities                                        3,000
                 Loan from parents for rent

    12/20/2022   Rent                                   3,000
                 Checking account                                   3,000
                 First and last month's rent

    12/25/2022   Checking account                         693
                 Federal income tax                       120
                 State tax                                 60
                 Social security tax                       62
                 Medicare tax                              15
                 401K Savings Account                     100
                 Income                                             1,000
                 Employer 401K match                                   50
                 My first paycheck!

The general journal records transactions in date order.  Each journal
entry is a T-chart with a date and an optional description.

The general journal is sometimes called the &ldquo;the book of original
entry.&rdquo;


The other journals that a business would keep are

  * cash receipt journal
  * cash disbursement journal
  * sales journal (for any sales made on credit)
  * purchase journal (for any purchases bought on credit)

The General Ledger (the &ldquo;book of final entry&rdquo;)
----------------------------------------------

    Account: Checking Account
    Date          Description          Debit     Credit    Balance
    12/01/2022    Loan                 3,000                 3,000
    12/20/2022    Rent                            3,000          0
    12/25/2022    Paycheck               693                   693

    Account: 401K
    Date          Description          Debit     Credit    Balance
    12/25/2022    Paycheck                50                    50
    12/25/2022    Employer match          50                   100

    Account: Liabilities
    Date          Description          Debit     Credit    Balance
    12/20/2022    Loan                            3,000      3,000

    Account: Income
    Date          Description          Debit     Credit    Balance
    12/25/2022    Paycheck                        1,000      1,000
    12/25/2022    Employer match (non-taxable)       50      1,050

    Account: Expenses
    Date          Description          Debit     Credit    Balance
    12/20/2022    Rent                 3,000                 3,000
    12/25/2022    Federal income tax     120                 3,120
    12/25/2022    State tax               60                 3,180
    12/25/2022    Social security tax     62                 3,242
    12/25/2022    Medicare tax            15                 3,257

The general ledger contains one or more pages per account.  It is
sometimes called the "book of final entry" because this is the book
from which the financial statements are created.

Speaking of financial statements ...


The Balance Sheet
----------------------------------------------

    Balance Sheet
    As of December 31, 2022

    Assets
      Checking               693
      401K                   100
                          -------
                             793

    Liabilities
      Loan                 3,000
                          -------
                           3,000

    Equity                (2,207)
                          =======


The balance sheet is a snapshot at a given point in time.

The Income Statement
----------------------------------------------

    Income Statement
    For the period from 12/1 through 12/31/2022

    Income                 1,050

    Expenses               3,257

    Net Income            (2,207)
                          =======

The income statement covers a period, in this example,
one month.

A basic financial control
----------------------------------------------


Given:

     equity_start    is the equity at a start of a period
     net_income      is the net income over that period
     equity_end      is the equity at the end of a period


     equity_end = equity_start + net_income

If not, someone is cooking the books.

In our example,

     equity_start    = 0
     net_income      = -2,207
     equity_end      = -2,207

Which matches our equity on 12/31.

Closing the year
----------------------------------------------

Income and revenue accounts are reset to zero at the beginning of
each year.  To reset them, you &ldquo;close&rdquo; the year.

We add a transaction to the general journal,

    Date         Accounts & Description                 Debit      Credit
    12/31/2022   Income                                 1,050
                 Equity                                 2,207
                 Expenses                                           3,257

update the income and expenses accounts in the general ledger,

    Account: Income
    Date          Description          Debit     Credit    Balance
    12/25/2022    Paycheck                        1,000      1,000
    12/25/2022    Employer match (non-taxable)       50      1,050
    12/31/2022    Close year           1,050                     0

    Account: Expenses
    Date          Description          Debit     Credit    Balance
    12/20/2022    Rent                 3,000                 3,000
    12/25/2022    Federal income tax     120                 3,120
    12/25/2022    State tax               60                 3,180
    12/25/2022    Social security tax     62                 3,242
    12/25/2022    Medicare tax            15                 3,257
    12/31/2022    Close year                      3,257          0

and add an Equity account page to the ledger.

    Account: Equity
    Date          Description          Debit     Credit    Balance
    12/31/2022    Close year           2,207                -2,207


History
============================

The earliest written account of double-entry accounting that I could
find a reference to was Luca Pacioli's *Summa de arithmetica,
geometria, proportioni et proportionalita* (Summary of arithmetic,
geometry, proportion and proportionality), published in Venice,
Italy in 1494.

This was a summary work of existing knowledge, so this system was
in use before this book was published.

An interesting side note is that after Venice, Luca Pacioli went
to Milan to publish his second book.  The illustrator for his book
was none other than Leonardo da Vinci, whom Luca had befriended in
Milan!

This taken from the [MacTutor History of Mathematics
Archive](https://mathshistory.st-andrews.ac.uk/Biographies/Pacioli/).
