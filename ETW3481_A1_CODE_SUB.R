# ETW3481_A1

# Packages
install.packages(c("readr","dplyr","tidyr","lubridate"), quiet = TRUE)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(fBasics)
library(tibble)
library(quantmod)
library(stats)
library(forecast)
library(tseries)


# CHAT GPT HAS BEEN USED FOR THIS CODING

# / / / / / / / / / / P A R T   1  : C L E A N   C R Y P T O / / / / / / / / /
# / / / / / / / / / / P A R T   1  : C L E A N   C R Y P T O / / / / / / / / /
# / / / / / / / / / / P A R T   1  : C L E A N   C R Y P T O / / / / / / / / /
# / / / / / / / / / / P A R T   1  : C L E A N   C R Y P T O / / / / / / / / /
# / / / / / / / / / / P A R T   1  : C L E A N   C R Y P T O / / / / / / / / /

# 1) list all txt files
cln_files <- list.files("Clean cryptos", pattern = "\\.txt$", full.names = TRUE)

# 2) reader for one file (whitespace-delimited: "Date <value>")
cln_read_one <- function(fp) {
  df <- readr::read_table(
    fp,
    col_types = cols(.default = col_character()),
    progress = FALSE
  )
  names(df) <- tolower(names(df))
  # pick date in first column or column named like "date"
  date_col <- if ("date" %in% names(df)) "date" else names(df)[1]
  # pick value column (second column)
  value_col <- setdiff(names(df), date_col)[1]
  
  tibble(
    date   = lubridate::dmy(df[[date_col]]),
    symbol = tools::file_path_sans_ext(basename(fp)),
    value  = suppressWarnings(as.numeric(df[[value_col]]))
  ) |>
    filter(!is.na(date))
}

# 3) bind all -> long panel (date, symbol, value)
cln_data_long <- lapply(cln_files, cln_read_one) |>
  bind_rows() |>
  arrange(symbol, date) |>
  group_by(symbol, date) |>
  summarize(value = last(value), .groups = "drop")

# 4) align by date and spread to wide (rows = time, cols = symbols)
cln_data_wide_full <- cln_data_long |>
  arrange(date, symbol) |>
  pivot_wider(names_from = symbol, values_from = value) |>
  arrange(date)

# 5) (optional) keep only dates shared by all symbols (inner intersection)
cln_data_wide_inner <- cln_data_wide_full |>
  tidyr::drop_na()

cln_price <- cln_data_wide_inner
cln_price

# FINDING LOG RETURN FOR CLEAN CRYPTO
clean_return <- cln_price %>%
  arrange(date) %>%
  transmute(
    date,
    across(
      -date,
      ~ 100 * log(.x / dplyr::lag(.x)),   # r_t = 100*ln(P_t / P_{t-1})
      .names = "{.col}"
    )
  ) %>%
  filter(date >= as.Date("2024-01-12"))

clean_return

cln_X <- select(clean_return, -date)

cln_stats <- basicStats(as.matrix(cln_X))

cln_coin_stats <- as.data.frame(t(cln_stats)) %>%
  rownames_to_column("coin") %>%
  as_tibble()

cln_coin_stats <- cln_coin_stats %>%
  select(coin, Mean, Stdev, Skewness, Kurtosis) %>%
  mutate(ExKurtosis = Kurtosis - 3)

# --- CLEAN PLOTS ---

# (a) Clean Mean
cln_plot_mean <- ggplot(cln_coin_stats, aes(x = reorder(coin, Mean), y = Mean)) +
  geom_col(fill = "blue", alpha = 0.7) +
  coord_flip() +
  labs(title = "Mean of Log Returns (Clean, per Coin)",
       x = "Coin", y = "Mean Return") +
  theme(
    plot.title   = element_text(size = 30),
    axis.text.x  = element_text(size = 25),
    axis.text.y  = element_text(size = 8)
  )
cln_plot_mean


# (b) Clean Std
cln_plot_sd <- ggplot(cln_coin_stats, aes(x = reorder(coin, Stdev), y = Stdev)) +
  geom_col(fill = "blue", alpha = 0.7) +
  coord_flip() +
  labs(title = "Standard Deviation of Log Returns (Clean, per Coin)", x = "Coin", y = "Std Dev")+
  theme(
    plot.title   = element_text(size = 30),
    axis.text.x  = element_text(size = 35),
    axis.text.y  = element_text(size = 8)
  )
cln_plot_sd

# (c) Clean Skewedness
cln_plot_skew <- ggplot(cln_coin_stats, aes(x = reorder(coin, Skewness), y = Skewness)) +
  geom_col(fill = "blue", alpha = 0.7) +
  coord_flip() +
  labs(title = "Skewness of Log Returns (Clean, per Coin)", x = "Coin", y = "Skewness")+
  theme(
    plot.title   = element_text(size = 30),
    axis.text.x  = element_text(size = 35),
    axis.text.y  = element_text(size = 8)
  )
cln_plot_skew

# (d) Clean excess
cln_plot_kurt <- ggplot(cln_coin_stats, aes(x = reorder(coin, ExKurtosis), y = ExKurtosis)) +
  geom_col(fill = "blue", alpha = 0.7) +
  coord_flip() +
  labs(title = "Excess Kurtosis of Log Returns (Clean, per Coin)", x = "Coin", y = "Excess Kurtosis")+
  theme(
    plot.title   = element_text(size = 30),
    axis.text.x  = element_text(size = 35),
    axis.text.y  = element_text(size = 8)
  )
cln_plot_kurt

# (e) Clean scatter
cln_plot_scatter <- ggplot(cln_coin_stats, aes(x = Mean, y = Stdev)) +
  geom_point(color = "blue", alpha = 0.7) +
  labs(title = "Scatter Plot: Mean vs Std Dev (Clean, per Coin)", 
       x = "Mean Return", y = "Standard Deviation") +
  theme_minimal()+
  theme(
    plot.title   = element_text(size = 30),
    axis.text.x  = element_text(size = 35),
    axis.text.y  = element_text(size = 8)
  )
cln_plot_scatter

# (f) Clean Skewd VS excess scatter
cln_plot_skew_vs_kurt <- ggplot(cln_coin_stats, aes(x = Skewness, y = ExKurtosis)) +
  geom_point(color = "blue",alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Skewness vs Excess Kurtosis (Clean, per Coin)",
    x = "Skewness",
    y = "Excess Kurtosis"
  ) +
  theme_minimal() +
  theme(
    plot.title   = element_text(size = 30),
    axis.text.x  = element_text(size = 35),
    axis.text.y  = element_text(size = 8)
  )
cln_plot_skew_vs_kurt


# (g) Clean crypto mean
cln_mean_rank <- clean_return %>%
  select(-date) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "coin", values_to = "mean_return") %>%
  arrange(desc(mean_return))
cln_mean_rank

# (h) Clean median crypto
cln_median_coin <- cln_mean_rank[ceiling(nrow(cln_mean_rank) / 2), ]
cln_mean_rank
cln_median_coin

# (i) Clean median imperical density plot
    # 0FLOW log return vector extraction (clean)
cln_x_0FLOW <- clean_return$`0FLOW`
cln_x_0FLOW <- cln_x_0FLOW[!is.na(cln_x_0FLOW)]

    # Empirical density (KDE) clean
cln_d1_0FLOW <- density(cln_x_0FLOW, na.rm = TRUE)

    # View the Plot
{
  plot(cln_d1_0FLOW$x, cln_d1_0FLOW$y, type = "l",
     xlab = "return", ylab = "density",
     main = "Empirical density of 0FLOW (clean log returns)",
     las = 1,
     cex.main = 2.0,
     cex.axis = 1.9)

    #Add the theoretical normal density plot to the existing empirical plot for comparison. 
cln_mu_0FLOW <- mean(cln_x_0FLOW)
cln_sd_0FLOW <- sd(cln_x_0FLOW)
cln_xx_0FLOW <- seq(min(cln_x_0FLOW), max(cln_x_0FLOW), length.out = 512)
cln_yy_0FLOW <- dnorm(cln_xx_0FLOW, mean = cln_mu_0FLOW, sd = cln_sd_0FLOW)
lines(cln_xx_0FLOW, cln_yy_0FLOW, lty = 2, col = "blue")

legend("topright",
       legend = c("Empirical KDE", "Normal(mu, sd)"),
       lty = c(1, 2), col = c("black", "blue"), bty = "n")
}

# (j) Normality Test : Clean
normalTest(clean_return$`0FLOW`,method="jb", na.rm=TRUE)

OFLOW_rets <- clean_return$`0FLOW`
OFLOW_rets

OFLOW_rets_ts = ts(OFLOW_rets)
OFLOW_rets_ts

qqnorm(OFLOW_rets_ts, main = "Normal Q-Q Plot of 0FLOW
returns", xlab = "Theoretical Quantiles", ylab = "Sample",
       cex.main = 2.0,
       cex.axis = 2.0,
       col = "blue")

qqline(OFLOW_rets_ts)



# / / / / / / / / / / P A R T   1  : D I R T Y  C R Y P T O / / / / / / / / /
# / / / / / / / / / / P A R T   1  : D I R T Y  C R Y P T O / / / / / / / / /
# / / / / / / / / / / P A R T   1  : D I R T Y  C R Y P T O / / / / / / / / /
# / / / / / / / / / / P A R T   1  : D I R T Y  C R Y P T O / / / / / / / / /
# / / / / / / / / / / P A R T   1  : D I R T Y  C R Y P T O / / / / / / / / /

# 1) list all txt files
dty_files <- list.files("Dirty cryptos", pattern = "\\.txt$", full.names = TRUE)

# 2) reader for one file (whitespace-delimited: "Date <value>")
dty_read_one <- function(fp) {
  df <- readr::read_table(
    fp,
    col_types = cols(.default = col_character()),
    progress = FALSE
  )
  names(df) <- tolower(names(df))
  # pick date in first column or column named like "date"
  date_col <- if ("date" %in% names(df)) "date" else names(df)[1]
  # pick value column (second column)
  value_col <- setdiff(names(df), date_col)[1]
  
  tibble(
    date   = lubridate::dmy(df[[date_col]]),
    symbol = tools::file_path_sans_ext(basename(fp)),
    value  = suppressWarnings(as.numeric(df[[value_col]]))
  ) |>
    filter(!is.na(date))
}

# 3) bind all -> long panel (date, symbol, value)
dty_data_long <- lapply(dty_files, dty_read_one) |>
  bind_rows() |>
  arrange(symbol, date) |>
  group_by(symbol, date) |>
  summarize(value = last(value), .groups = "drop")

# 4) align by date and spread to wide (rows = time, cols = symbols)
dty_data_wide_full <- dty_data_long |>
  arrange(date, symbol) |>
  pivot_wider(names_from = symbol, values_from = value) |>
  arrange(date)
dty_data_wide_full


# 5) (optional) keep only dates shared by all symbols : FAIL
dty_data_wide_inner <- dty_data_wide_full |>
  tidyr::drop_na()

dty_price <- dty_data_wide_inner
dty_price

# FINDING COMMON RANGE of clean data
cln_common_range <- cln_data_wide_full %>%
  drop_na() %>% 
  summarise(
    start = min(date),
    end   = max(date)
  )
cln_common_range


# Fitting data timezone with the clean data
dty_subset <- dty_data_wide_full %>%
  filter(date >= as.Date("2024-01-11"),
         date <= as.Date("2024-10-31"))

# Revewing coin price data only not date
dty_na_coins <- names(dty_subset)[
  sapply(dty_subset, function(x) any(is.na(x)))
]
dty_na_coins <- setdiff(dty_na_coins, "date")

# REPORT
if (length(dty_na_coins) > 0) {
  cat("NA included crypto:\n")
  print(dty_na_coins)
} else {
  cat("no NA value in this period.\n")
}

dty_data_wide_full <- dty_data_wide_full %>%
  select(-all_of(dty_na_coins))

# 5-1) (optional) keep only dates shared by all symbols : SUCCESS
dty_data_wide_inner <- dty_data_wide_full |>
  tidyr::drop_na()

dty_price <- dty_data_wide_inner
dty_price


# FINDING LOG RETURN FOR CLEAN CRYPTO
dirty_return <- dty_price %>%
  arrange(date) %>%
  transmute(
    date,
    across(
      -date,
      ~ 100 * log(.x / dplyr::lag(.x)),   # r_t = 100*ln(P_t / P_{t-1})
      .names = "{.col}"
    )
  ) %>%
  filter(date >= as.Date("2024-01-12"))

dirty_return

dty_X <- select(dirty_return, -date)

dty_stats <- basicStats(as.matrix(dty_X))

dty_coin_stats <- as.data.frame(t(dty_stats)) %>%
  rownames_to_column("coin") %>%
  as_tibble()

dty_coin_stats <- dty_coin_stats %>%
  select(coin, Mean, Stdev, Skewness, Kurtosis) %>%
  mutate(ExKurtosis = Kurtosis - 3)






# --- dirty PLOTS ---

# (a) dirty Mean
dty_plot_mean <- ggplot(dty_coin_stats, aes(x = reorder(coin, Mean), y = Mean)) +
  geom_col(fill = "red", alpha = 0.7) +
  coord_flip() +
  labs(title = "Mean of Log Returns (dirty, per Coin)", x = "Coin", y = "Mean Return") +
  theme(
    plot.title   = element_text(size = 30),
    axis.text.x  = element_text(size = 25),
    axis.text.y  = element_text(size = 8)
  )
dty_plot_mean

# (b) dirty Std
dty_plot_sd <- ggplot(dty_coin_stats, aes(x = reorder(coin, Stdev), y = Stdev)) +
  geom_col(fill = "red", alpha = 0.7) +
  coord_flip() +
  labs(title = "Standard Deviation of Log Returns (dirty, per Coin)", x = "Coin", y = "Std Dev") +
  theme(
    plot.title   = element_text(size = 30),
    axis.text.x  = element_text(size = 35),
    axis.text.y  = element_text(size = 8)
  )
dty_plot_sd

# (c) dirty Skewedness
dty_plot_skew <- ggplot(dty_coin_stats, aes(x = reorder(coin, Skewness), y = Skewness)) +
  geom_col(fill = "red", alpha = 0.7) +
  coord_flip() +
  labs(title = "Skewness of Log Returns (dirty, per Coin)", x = "Coin", y = "Skewness") +
  theme(
    plot.title   = element_text(size = 30),
    axis.text.x  = element_text(size = 35),
    axis.text.y  = element_text(size = 8)
  )
dty_plot_skew

# (d) dirty excess
dty_plot_kurt <- ggplot(dty_coin_stats, aes(x = reorder(coin, ExKurtosis), y = ExKurtosis)) +
  geom_col(fill = "red", alpha = 0.7) +
  coord_flip() +
  labs(title = "Excess Kurtosis of Log Returns (dirty, per Coin)", x = "Coin", y = "Excess Kurtosis") +
  theme(
    plot.title   = element_text(size = 30),
    axis.text.x  = element_text(size = 35),
    axis.text.y  = element_text(size = 8)
  )
dty_plot_kurt

# (e) dirty scatter
dty_plot_scatter <- ggplot(dty_coin_stats, aes(x = Mean, y = Stdev)) +
  geom_point(color = "red", alpha = 0.7) +
  labs(title = "Scatter Plot: Mean vs Std Dev (dirty, per Coin)", 
       x = "Mean Return", y = "Standard Deviation") +
  theme_minimal() +
  theme(
    plot.title   = element_text(size = 30),
    axis.text.x  = element_text(size = 35),
    axis.text.y  = element_text(size = 8)
  )
dty_plot_scatter

# (f) dirty Skewd VS excess scatter
dty_plot_skew_vs_kurt <- ggplot(dty_coin_stats, aes(x = Skewness, y = ExKurtosis)) +
  geom_point(color = "red",alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Skewness vs Excess Kurtosis (dirty, per Coin)",
    x = "Skewness",
    y = "Excess Kurtosis"
  ) +
  theme_minimal() +
  theme(
    plot.title   = element_text(size = 30),
    axis.text.x  = element_text(size = 35),
    axis.text.y  = element_text(size = 8)
  )
dty_plot_skew_vs_kurt


# (g) dirty crypto mean
dty_mean_rank <- dirty_return %>%
  select(-date) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "coin", values_to = "mean_return") %>%
  arrange(desc(mean_return))
dty_mean_rank

# (h) dirty median crypto
dty_median_coin <- dty_mean_rank[ceiling(nrow(dty_mean_rank) / 2), ]
dty_mean_rank
dty_median_coin

# (i) dirty median imperical density plot
# 0FLOW log return vector extraction (dirty)
dty_x_1ERG <- dirty_return$`1ERG`
dty_x_1ERG <- dty_x_1ERG[!is.na(dty_x_1ERG)]

# Empirical density (KDE) dirty
dty_d1_1ERG <- density(dty_x_1ERG, na.rm = TRUE)

# View the Plot
{plot(dty_d1_1ERG$x, dty_d1_1ERG$y, type = "l",
      xlab = "return", ylab = "density",
      main = "Empirical density of 1ERG (dirty log returns)",
      las = 1,
      cex.main = 2.0,
      cex.axis = 1.9)
  
  #Add the theoretical normal density plot to the existing empirical plot for comparison. 
  dty_mu_1ERG <- mean(dty_x_1ERG)
  dty_sd_1ERG <- sd(dty_x_1ERG)
  dty_xx_1ERG <- seq(min(dty_x_1ERG), max(dty_x_1ERG), length.out = 512)
  dty_yy_1ERG <- dnorm(dty_xx_1ERG, mean = dty_mu_1ERG, sd = dty_sd_1ERG)
  lines(dty_xx_1ERG, dty_yy_1ERG, lty = 2, col = "red")
  
  legend("topright",
         legend = c("Empirical KDE", "Normal(mu, sd)"),
         lty = c(1, 2), col = c("black", "red"), bty = "n")
  }

# (j) Normality Test : dirty
normalTest(dirty_return$`1ERG`,method="jb", na.rm=TRUE)

lERG_rets <- dirty_return$`1ERG`
lERG_rets

lERG_rets_ts = ts(lERG_rets)
lERG_rets_ts

qqnorm(lERG_rets_ts, main = "Normal Q-Q Plot of 1ERG
returns", xlab = "Theoretical Quantiles", ylab = "Sample",
       cex.main = 2.0,
       cex.axis = 2.0,
       col = "red")

qqline(lERG_rets_ts)


# / / / / / / / / / / P A R T   2  : C L E A N   C R Y P T O / / / / / / / / /
# / / / / / / / / / / P A R T   2  : C L E A N   C R Y P T O / / / / / / / / /
# / / / / / / / / / / P A R T   2  : C L E A N   C R Y P T O / / / / / / / / /
# / / / / / / / / / / P A R T   2  : C L E A N   C R Y P T O / / / / / / / / /
# / / / / / / / / / / P A R T   2  : C L E A N   C R Y P T O / / / / / / / / /

dty_data_wide_full

cln_data_wide_full

.get_runs <- function(df_wide, date_col = "date") {
  dates <- df_wide[[date_col]]
  cols  <- setdiff(names(df_wide), date_col)
  
  out <- lapply(cols, function(cn) {
    v <- df_wide[[cn]]
    mask <- !is.na(v)
    if (!any(mask)) return(tibble())
    
    r <- rle(mask)
    ends   <- cumsum(r$lengths)
    starts <- ends - r$lengths + 1
    keep   <- which(r$values)
    if (length(keep) == 0) return(tibble())
    
    tibble(
      coin       = cn,
      start_idx  = starts[keep],
      end_idx    = ends[keep],
      length_run = r$lengths[keep],
      start_date = dates[starts[keep]],
      end_date   = dates[ends[keep]]
    )
  })
  
  res <- bind_rows(out)
  if (nrow(res) == 0) {
    res <- tibble(
      coin       = character(),
      start_idx  = integer(),
      end_idx    = integer(),
      length_run = integer(),
      start_date = as.Date(character()),
      end_date   = as.Date(character())
    )
  }
  res
}

min_len <- 2000L

# ---- 1) DIRTY: pick coin(s) whose qualifying run ends most recently ----
dty_runs_all   <- .get_runs(dty_data_wide_full)
dty_runs_2000  <- dty_runs_all %>% filter(length_run >= min_len)

if (nrow(dty_runs_2000) == 0) {
  dty_best <- tibble()
} else {
  dty_max_end <- max(dty_runs_2000$end_date, na.rm = TRUE)
  dty_ties    <- dty_runs_2000 %>% filter(end_date == dty_max_end)
  dty_best_len <- max(dty_ties$length_run, na.rm = TRUE)
  dty_best <- dty_ties %>%
    filter(length_run == dty_best_len) %>%
    arrange(coin, start_date)
}
dty_best

# ---- 2) CLEAN: pick coin(s) whose qualifying run ends most recently ----
cln_runs_all   <- .get_runs(cln_data_wide_full)
cln_runs_2000  <- cln_runs_all %>% filter(length_run >= min_len)

if (nrow(cln_runs_2000) == 0) {
  cln_best <- tibble()
} else {
  cln_max_end <- max(cln_runs_2000$end_date, na.rm = TRUE)
  cln_ties    <- cln_runs_2000 %>% filter(end_date == cln_max_end)
  cln_best_len <- max(cln_ties$length_run, na.rm = TRUE)
  cln_best <- cln_ties %>%
    filter(length_run == cln_best_len) %>%
    arrange(coin, start_date)
}
cln_best

cln_best
dty_best


dty_1BTC_trim <- dty_data_wide_full %>%
  filter(date >= as.Date("2018-07-19"),
         date <= as.Date("2024-12-31")) %>%
  select(date, `1BTC`)
dty_1BTC_trim



dty_1LTC_trim <- dty_data_wide_full %>%
  filter(date >= as.Date("2018-07-19"),
         date <= as.Date("2024-12-31")) %>%
  select(date, `1LTC`)
dty_1LTC_trim



dty_1NMC_trim <- dty_data_wide_full %>%
  filter(date >= as.Date("2018-07-19"),
         date <= as.Date("2024-12-31")) %>%
  select(date, `1NMC`)
dty_1NMC_trim


dty_three <- dty_data_wide_full %>%
  filter(date >= as.Date("2018-07-19"),
         date <= as.Date("2024-12-31")) %>%
  select(date, `1BTC`, `1LTC`, `1NMC`)

# Calculating the log return : DIRTY
dty_three_ret <- dty_three %>%
  mutate(
    `1BTC` = 100 * log(`1BTC` / dplyr::lag(`1BTC`)),
    `1LTC` = 100 * log(`1LTC` / dplyr::lag(`1LTC`)),
    `1NMC` = 100 * log(`1NMC` / dplyr::lag(`1NMC`))
  )
dty_three

dty_three_ret <- dty_three_ret %>% drop_na()

dty_three_ret

# CLEAN CRYPTO LOG RETURN
cln_one <- cln_data_wide_full %>%
  filter(date >= as.Date("2018-07-19"),
         date <= as.Date("2024-12-31")) %>%
  select(date, `0RPL`)
cln_one

# Calculating the log return : DIRTY
cln_one_ret <- cln_one %>%
  mutate(
    `0RPL` = 100 * log(`0RPL` / dplyr::lag(`0RPL`)))

cln_one_ret <- cln_one_ret %>% drop_na()

cln_one_ret

log_return_2000 <- cbind(dty_three_ret,cln_one_ret$`0RPL`)
log_return_2000 <- log_return_2000 %>%
  rename(`0RPL` = "cln_one_ret$`0RPL`")
log_return_2000


# FINAL CHOICE OF CURRENCY
dty_BTC <- log_return_2000$`1BTC`
dty_BTC
cln_RPL <- log_return_2000$`0RPL`
cln_RPL





#  / / / / / / / / / / ACTUAL PREDICTION AND MSE & MAE(D)  / / / / / / / / / / 
#  / / / / / / / / / / / / / D I R T Y   C R Y P T O / / / / / / / / / / / / /
#  / / / / / / / / / / ACTUAL PREDICTION AND MSE & MAE(D)  / / / / / / / / / / 
#  / / / / / / / / / / / / / D I R T Y   C R Y P T O / / / / / / / / / / / / /
#  / / / / / / / / / / ACTUAL PREDICTION AND MSE & MAE(D)  / / / / / / / / / / 
#  / / / / / / / / / / / / / D I R T Y   C R Y P T O / / / / / / / / / / / / /
#  / / / / / / / / / / ACTUAL PREDICTION AND MSE & MAE(D)  / / / / / / / / / / 
#  / / / / / / / / / / / / / D I R T Y   C R Y P T O / / / / / / / / / / / / /
#  / / / / / / / / / / ACTUAL PREDICTION AND MSE & MAE(D)  / / / / / / / / / / 


# FORECASTING AND EXTRACT MSE & MAE DIRTY
    # DIRTY : AR1
AR1_dty_BTC_e <- tsCV(
  dty_BTC,
  forecastfunction = function(y, h) forecast(Arima(y, order = c(1,0,0)), h = h),
  h = 1
)
AR1_dty_BTC_mse <- mean(AR1_dty_BTC_e^2, na.rm = TRUE)
AR1_dty_BTC_mae <- mean(abs(AR1_dty_BTC_e), na.rm = TRUE)
AR1_dty_BTC_mse; AR1_dty_BTC_mae


# DIRTY : MA1
MA1_dty_BTC_e <- tsCV(
  dty_BTC,
  forecastfunction = function(y, h) forecast(Arima(y, order = c(0,0,1)), h = h),
  h = 1
)
MA1_dty_BTC_mse <- mean(MA1_dty_BTC_e^2, na.rm = TRUE)
MA1_dty_BTC_mae <- mean(abs(MA1_dty_BTC_e), na.rm = TRUE)
MA1_dty_BTC_mse; MA1_dty_BTC_mae


# DIRTY : ARIMA(1,0,1)
ARIMA_dty_BTC_e <- tsCV(
  dty_BTC,
  forecastfunction = function(y, h) forecast(Arima(y, order = c(1,0,1)), h = h),
  h = 1
)
ARIMA_dty_BTC_mse <- mean(ARIMA_dty_BTC_e^2, na.rm = TRUE)
ARIMA_dty_BTC_mae <- mean(abs(ARIMA_dty_BTC_e), na.rm = TRUE)
ARIMA_dty_BTC_mse; ARIMA_dty_BTC_mae


# DIRTY : Naive
e_naive_dty_BTC <- tsCV(dty_BTC,
                forecastfunction = function(y, h) naive(y, h = h),
                h = 1
)
mse_naive_dty_BTC_cv <- mean(e_naive_dty_BTC^2, na.rm = TRUE)
mae_naive_dty_BTC_cv <- mean(abs(e_naive_dty_BTC), na.rm = TRUE)
mse_naive_dty_BTC_cv; mae_naive_dty_BTC_cv



# DIRTY : Historical Mean Approach
hm_predict <- function(r) {
  n  <- length(r)
  cs <- cumsum(r)
  fc <- rep(NA_real_, n)
  fc[2:n] <- cs[1:(n-1)] / (1:(n-1))
  fc
}

hm_metrics <- function(r) {
  fc <- hm_predict(r)
  err <- r - fc
  err <- err[!is.na(err)]
  list(pred = fc,
       mse  = mean(err^2),
       mae  = mean(abs(err)))
}

resisual_dty_BTC <- hm_metrics(dty_BTC)
resisual_dty_BTC$mse
resisual_dty_BTC$mae



# DIRTY : Simple Moving Average m = 20
{
  ma_m_predict <- function(r, m = 20L) {
  r <- as.numeric(r); n <- length(r)
  cs <- c(0, cumsum(r))
  pred <- rep(NA_real_, n)
  if (n >= m + 1) {
    idx <- (m + 1):n
    pred[idx] <- (cs[idx] - cs[idx - m]) / m
  }
  pred
}

ma_m_metrics <- function(r, m = 20L) {
  pred <- ma_m_predict(r, m)
  err  <- (r - pred)[-(1:m)]
  list(pred = pred, MSE = mean(err^2), MAE = mean(abs(err)))
}

res <- ma_m_metrics(dty_BTC, m = 20)
dty_BTC_ma20_summary <- c(MSE = res$MSE, MAE = res$MAE)
dty_BTC_ma20_summary
}



# DIRTY : Simple Moving Average m = 60
{
  ma_m_predict <- function(r, m = 60L) {
    r <- as.numeric(r); n <- length(r)
    cs <- c(0, cumsum(r))
    pred <- rep(NA_real_, n)
    if (n >= m + 1) {
      idx <- (m + 1):n
      pred[idx] <- (cs[idx] - cs[idx - m]) / m
    }
    pred
  }
  
  ma_m_metrics <- function(r, m = 60L) {
    pred <- ma_m_predict(r, m)
    err  <- (r - pred)[-(1:m)]
    list(pred = pred, MSE = mean(err^2), MAE = mean(abs(err)))
  }
  
  res <- ma_m_metrics(dty_BTC, m = 60)
  dty_BTC_ma60_summary <- c(MSE = res$MSE, MAE = res$MAE)
  dty_BTC_ma60_summary
}



# DIRTY : Simple Moving Average m = 180
{
  ma_m_predict <- function(r, m = 180L) {
    r <- as.numeric(r); n <- length(r)
    cs <- c(0, cumsum(r))
    pred <- rep(NA_real_, n)
    if (n >= m + 1) {
      idx <- (m + 1):n
      pred[idx] <- (cs[idx] - cs[idx - m]) / m
    }
    pred
  }
  
  ma_m_metrics <- function(r, m = 180L) {
    pred <- ma_m_predict(r, m)
    err  <- (r - pred)[-(1:m)]
    list(pred = pred, MSE = mean(err^2), MAE = mean(abs(err)))
  }
  
  res <- ma_m_metrics(dty_BTC, m = 180)
  dty_BTC_ma180_summary <- c(MSE = res$MSE, MAE = res$MAE)
  dty_BTC_ma180_summary
}









#  / / / / / / / / / / ACTUAL PREDICTION AND MSE & MAE(D)  / / / / / / / / / / 
#  / / / / / / / / / / / / / C L E A N   C R Y P T O / / / / / / / / / / / / /
#  / / / / / / / / / / ACTUAL PREDICTION AND MSE & MAE(D)  / / / / / / / / / / 
#  / / / / / / / / / / / / / C L E A N   C R Y P T O / / / / / / / / / / / / /
#  / / / / / / / / / / ACTUAL PREDICTION AND MSE & MAE(D)  / / / / / / / / / / 
#  / / / / / / / / / / / / / C L E A N   C R Y P T O / / / / / / / / / / / / /
#  / / / / / / / / / / ACTUAL PREDICTION AND MSE & MAE(D)  / / / / / / / / / / 
#  / / / / / / / / / / / / / C L E A N   C R Y P T O / / / / / / / / / / / / /
#  / / / / / / / / / / ACTUAL PREDICTION AND MSE & MAE(D)  / / / / / / / / / / 


# FORECASTING AND EXTRACT MSE & MAE DIRTY
# DIRTY : AR1
AR1_cln_RPL_e <- tsCV(
  cln_RPL,
  forecastfunction = function(y, h) forecast(Arima(y, order = c(1,0,0)), h = h),
  h = 1
)
AR1_cln_RPL_mse <- mean(AR1_cln_RPL_e^2, na.rm = TRUE)
AR1_cln_RPL_mae <- mean(abs(AR1_cln_RPL_e), na.rm = TRUE)
AR1_cln_RPL_mse; AR1_cln_RPL_mae


# DIRTY : MA1
MA1_cln_RPL_e <- tsCV(
  cln_RPL,
  forecastfunction = function(y, h) forecast(Arima(y, order = c(0,0,1)), h = h),
  h = 1
)
MA1_cln_RPL_mse <- mean(MA1_cln_RPL_e^2, na.rm = TRUE)
MA1_cln_RPL_mae <- mean(abs(MA1_cln_RPL_e), na.rm = TRUE)
MA1_cln_RPL_mse; MA1_cln_RPL_mae


# DIRTY : ARIMA(1,0,1)
ARIMA_cln_RPL_e <- tsCV(
  cln_RPL,
  forecastfunction = function(y, h) forecast(Arima(y, order = c(1,0,1)), h = h),
  h = 1
)
ARIMA_cln_RPL_mse <- mean(ARIMA_cln_RPL_e^2, na.rm = TRUE)
ARIMA_cln_RPL_mae <- mean(abs(ARIMA_cln_RPL_e), na.rm = TRUE)
ARIMA_cln_RPL_mse; ARIMA_cln_RPL_mae


# DIRTY : Naive
e_naive_cln_RPL <- tsCV(cln_RPL,
                        forecastfunction = function(y, h) naive(y, h = h),
                        h = 1
)
mse_naive_cln_RPL_cv <- mean(e_naive_cln_RPL^2, na.rm = TRUE)
mae_naive_cln_RPL_cv <- mean(abs(e_naive_cln_RPL), na.rm = TRUE)
mse_naive_cln_RPL_cv; mae_naive_cln_RPL_cv



# DIRTY : Historical Mean Approach
hm_predict <- function(r) {
  n  <- length(r)
  cs <- cumsum(r)
  fc <- rep(NA_real_, n)
  fc[2:n] <- cs[1:(n-1)] / (1:(n-1))
  fc
}

hm_metrics <- function(r) {
  fc <- hm_predict(r)
  err <- r - fc
  err <- err[!is.na(err)]
  list(pred = fc,
       mse  = mean(err^2),
       mae  = mean(abs(err)))
}

resisual_cln_RPL <- hm_metrics(cln_RPL)
resisual_cln_RPL$mse
resisual_cln_RPL$mae



# DIRTY : Simple Moving Average m = 20
{
  ma_m_predict <- function(r, m = 20L) {
    r <- as.numeric(r); n <- length(r)
    cs <- c(0, cumsum(r))
    pred <- rep(NA_real_, n)
    if (n >= m + 1) {
      idx <- (m + 1):n
      pred[idx] <- (cs[idx] - cs[idx - m]) / m
    }
    pred
  }
  
  ma_m_metrics <- function(r, m = 20L) {
    pred <- ma_m_predict(r, m)
    err  <- (r - pred)[-(1:m)]
    list(pred = pred, MSE = mean(err^2), MAE = mean(abs(err)))
  }
  
  res <- ma_m_metrics(cln_RPL, m = 20)
  cln_RPL_ma20_summary <- c(MSE = res$MSE, MAE = res$MAE)
  cln_RPL_ma20_summary
}



# DIRTY : Simple Moving Average m = 60
{
  ma_m_predict <- function(r, m = 60L) {
    r <- as.numeric(r); n <- length(r)
    cs <- c(0, cumsum(r))
    pred <- rep(NA_real_, n)
    if (n >= m + 1) {
      idx <- (m + 1):n
      pred[idx] <- (cs[idx] - cs[idx - m]) / m
    }
    pred
  }
  
  ma_m_metrics <- function(r, m = 60L) {
    pred <- ma_m_predict(r, m)
    err  <- (r - pred)[-(1:m)]
    list(pred = pred, MSE = mean(err^2), MAE = mean(abs(err)))
  }
  
  res <- ma_m_metrics(cln_RPL, m = 60)
  cln_RPL_ma60_summary <- c(MSE = res$MSE, MAE = res$MAE)
  cln_RPL_ma60_summary
}



# DIRTY : Simple Moving Average m = 180
{
  ma_m_predict <- function(r, m = 180L) {
    r <- as.numeric(r); n <- length(r)
    cs <- c(0, cumsum(r))
    pred <- rep(NA_real_, n)
    if (n >= m + 1) {
      idx <- (m + 1):n
      pred[idx] <- (cs[idx] - cs[idx - m]) / m
    }
    pred
  }
  
  ma_m_metrics <- function(r, m = 180L) {
    pred <- ma_m_predict(r, m)
    err  <- (r - pred)[-(1:m)]
    list(pred = pred, MSE = mean(err^2), MAE = mean(abs(err)))
  }
  
  res <- ma_m_metrics(cln_RPL, m = 180)
  cln_RPL_ma180_summary <- c(MSE = res$MSE, MAE = res$MAE)
  cln_RPL_ma180_summary
}

