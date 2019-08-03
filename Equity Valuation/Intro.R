library(ggplot2)
# Equity Valuation in R
# There is a time value to money, we can discount cash flows to determine
# valuation.

# Market value balance sheet: Assets = Liabilities + Equity
# Free Cash Flow to Equity
#   - direct valuation of the value of equity
#   - Cost of Equity (CAPM)

# 1.0. Present Value ####

fv <- 100
r <- 0.05

# 1.1. Calculate PV of $100 one year from now
pv_1 <- fv / (1 + r) ^ 1
pv_1

# 1.2. Calculate PV of $100 two years from now
pv_2 <- 100 / (1 + r) ^ 2
pv_2

# 1.3. Write a present value function
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

# 2.1. Example Calculation for FCFE ####
after_tax_income <- c(22.8, 24.0, 30.6, 38.4, 43.2)
depn_amort <- c(11, 12, 12, 14, 15)
capex <- c(11, 11, 12, 14, 15)
incr_wc <- c(16, 16, 14, 14, 14)
fcfe <- after_tax_income + depn_amort - capex - incr_wc
fcfe <- as.data.frame(fcfe)

# 2.2. Firm's have 'infinite' life, FCFE goes to five years. The value of FCFE from
# year 6 into perpituity is the "terminal value" and a common estimation approach
# is to use the 'Growth Model'. We start with the 2021 FCFE, the growth rate and
# the cost of equity.
fcfe_2021 <- 29.2
g <- 0.034
ke <- 0.105
tv_2021 <- fcfe_2021 * (1 + g) / (ke - g)

# 3.0. Calculating present value of free cash flow to equity. Discount fcfe to
# the present.

# 3.1. Add discount periods
fcfe$periods <- seq(1, 5, 1)
fcfe

# 3.2. # Calculate Present Value Factor
fcfe$pv_factor <- 1 / (1 + ke) ^ fcfe$periods
fcfe

# 3.3. Calculate Present Value of each Cash Flow
fcfe$pv <- fcfe$fcfe * fcfe$pv_factor
fcfe

# 3.4. Total Present Value
pv_fcfe <- sum(fcfe$pv)
pv_fcfe

# 3.5. Calculate Present Value
pv_tv <- tv_2021 / (1 + ke) ^ 5
pv_tv

# 3.6. Calculate Equity Value
eq_val <- pv_fcfe + pv_tv
eq_val

# 3.7. Calculate Equity Value Per Share
shares <- 10
eq_val_per_share <- eq_val / shares
eq_val_per_share

# 4.0. Visualizing revenue trends
hist_rev <- c(86.8,
              89.0,
              93.0,
              128.6,
              176.4,
              171.4,
              214.2,
              236.0,
              0.0,
              0.0,
              0.0,
              0.0,
              0.0)
rev_proj <- c(0.0,
              0.0,
              0.0,
              0.0,
              0.0,
              0.0,
              0.0,
              0.0,
              193.2,
              212.9,
              225.0,
              279.2,
              295.9)

# 4.1. Combine hist_rev and rev_proj
rev_split <- rbind(hist_rev, rev_proj)

# 4.2. Rename the column headers
colnames(rev_split) <- seq(2009, 2021, 1)
rev_split

# 4.3. Create a bar plot of the data
barplot(rev_split,
        col = c("red", "blue"),
        main = "Historical vs. Projected Revenues")
legend(
  "topleft",
  legend = c("Historical", "Projected"),
  fill = c("red", "blue")
)

# 5.0. Analyzing revenue trends using linear regression
rev_proj <-
  c(86.8,
    89.0,
    93.0,
    128.6,
    176.4,
    171.4,
    214.2,
    236.0,
    193.2,
    212.9,
    225.0,
    279.2,
    295.9)
rev_all <- as.data.frame(cbind(rev_proj))
# 5.1. Create a trend variable
rev_all$trend <- seq(1, 13, 1)
# 5.2. Create shift variable
rev_all$shift <- c(rep(0, 8), rep(1, 5))
# 5.3. Run regression
reg <- lm(rev_proj ~ trend + shift, data = rev_all)
# 5.4. Print regression summary
summary(reg)
# 5.5. View Regression
p_regression <- ggplot(rev_all, aes(trend + shift, rev_proj)) +
  geom_point()
p_regression
