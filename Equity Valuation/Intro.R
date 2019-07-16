# Equity Valuation in R
# There is a time value to money, we can discount cash flows to determine
# valuation.

# Market value balance sheet: Assets = Liabilities + Equity
# Free Cash Flow to Equity
#   - direct valuation of the value of equity
#   - Cost of Equity (CAPM)

# 1.0 Present Value ####

fv <- 100
r <- 0.05

# Calculate PV of $100 one year from now
pv_1 <- fv / (1 + r) ^ 1
pv_1

# Calculate PV of $100 two years from now
pv_2 <- 100 / (1 + r) ^ 2
pv_2

# Write a present value function
pv_fx = function(fv, r, t) {
  pv <- fv / (1 + r) ^ t
  print(pv)
}
pv_fx(100, 0.10, 2)


# 2.0 Free Cash Flow ####
# "Free Cash Flows" are cash flows after paying out: 
#   - suppliers, employees, lenders, taxes
#   - capital investments, additional working capital needs
#   - net of new borrowings and debt repayments
# cash flows that can be taken away from the firm and given to shareholders w/o
# affecting the operations and growth prospects of a firm. 

# After-Tax Income is calculated via:
# [1] Revenues (Sales)
# [2] Less: Cost of Goods Sold (COGS)
# ___________________________________
# [3] Gross Profit
# [4] Less: Operating Expenses
# ___________________________________
# [5] Operating Income or EBIT (Earnings Before Interest & Taxes)
# [6] Less: Interest Expense (Compensation to debtholders)
# ___________________________________
# [7] Pre-Tax Income
# [8] Less: Taxes 
# ___________________________________
# [9] After-Tax Income (Net Income)
# [10] Add: Depreciation and Amortization (Non-cash charge. Cash spent at purchase)
# [11] Less: Capital Expenditures (Cash spent on investments)
# [12] Less: Increases in Working Capital (cash spent on additional working capital needs)
# ___________________________________
# [13] Free Cash Flow to Equity 

# Terminal Value is the value of the cash flows beyond the forecast period.
# - Commonly estimated using the Perpetuity with Growth Model
#     TV = (FCFE_t+1) / (k_e - g) = (FCFE_t * (1+g)) / (k_e - g)
#       TV = terminal value
#       FCFE_t+1 = free cash flow to equity the year after the end of the forecast period 
#       k_e = cost of equity
#       g = Perpetuity Growth Rate


