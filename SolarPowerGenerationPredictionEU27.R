# imports
library(readr) 
library(fpp3)
library(ggplot2)
library(urca)


# import the dataset
data <- read_delim("SolarPowerGenerationPrediction/Realisierte_Erzeugung_20150101_20230630_monthly.csv", delim = ";", locale = locale(decimal_mark = ","))

# view first few rows
print(data)

# column names
names(data)

#summary
summary(data)



# transform date
data$Time <- as.Date(data$Datum, "%d.%m.%y")

#transform tibble to tsibble
ts_data <- data %>%
  mutate(Month = yearmonth(Time)) %>%
  as_tsibble(index = Month)

# Scale the data from MWh to GWh
ts_data$GWh <- ts_data$MWh / 1000


plot(ts_data$GWh - ts_data$Month)

lambda <- ts_data %>% 
  features(GWh, features = guerrero) %>% 
  pull(lambda_guerrero)

ts_data %>% 
  autoplot(box_cox(GWh, lambda)) + labs

# Looking at the peaks we see an increasing trend in generated power by solar 
# Probably a minimal positive trend can be seen less clear in the local minimums over time
# Indicates seasonality, as expected as solar power generation is more useful in summer due to more sun

# Examine seasonality 1
ts_data %>%
  gg_season(GWh)
# Confirms seasonality in the summer months

# Examine seasonality 2
ts_data %>%
  gg_subseries(GWh)
# Confirms upwards trend, but trend is stronger in summer month than in winter month

# Examine lags 
ts_data %>%
  gg_lag(GWh, geom="point")

# Examine lags 
ts_data %>%
  gg_lag(GWh, geom="point", lags=26)

# Examine lags 
ts_data %>%
  gg_lag(GWh, geom="point", lags=52)

#Examine ACF
ts_data %>%
  ACF(GWh)

ts_data %>%
  ACF(GWh, lag_max = 18) %>%
  autoplot()

ts_data$Month

# Time Series Decomposition
ts_data %>% 
  model(STL(GWh ~ trend(window=15) + season(window= "periodic")))  %>% 
  components() %>% 
  autoplot()

ts_data %>% 
  model(STL(log(GWh) ~ trend(window=15) + season(window= "periodic")))  %>% 
  components() %>% 
  autoplot()




# BENCHMARK Model
# Set training data from W1 2015 to W52 2021
train <- ts_data %>%
  select(GWh) %>% 
  filter_index("2015 Jan" ~ "2021 Dec")

test <- ts_data %>%
  select(GWh) %>% 
  filter_index("2021 Dec" ~ .)

# Fit the models
bench_fit <- train %>%
  model(
    Mean = MEAN(log(GWh)),
    Naive = NAIVE(log(GWh)),
    SNaive = SNAIVE(log(GWh)),
    Drift = NAIVE(log(GWh) ~ drift())
  )

#TS Residuals
bench_fit[1] %>% 
  gg_tsresiduals()

#TS Residuals
bench_fit[2] %>% 
  gg_tsresiduals()

#TS Residuals
bench_fit[3] %>% 
  gg_tsresiduals()

#TS Residuals
bench_fit[4] %>% 
  gg_tsresiduals()

augment(bench_fit[3]) %>%
  features(.innov, box_pierce, lag = 10)

augment(bench_fit[3]) %>%
  features(.innov, ljung_box, lag = 10)

# Generate forecasts for 76 weeks
bench_fc <- bench_fit %>% forecast(h = 18)

# Plot forecasts against actual values
bench_fc %>% 
  autoplot(train, level = NULL) +
  autolayer(
    test,
    colour = "black"
  ) +
  labs(
    y = "GWh",
    title = "Forecasts for weekly Solar Power Generation"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

accuracy(bench_fc, ts_data)

test


# ETS
ets_fit <- train %>%
  model(ets_auto = ETS(log(GWh)),
        ets_guess = ETS(log(GWh) ~ error("A") + trend("A") + season("M"))
        )

ets_fit %>% 
  select(.model = "ets_auto") %>% 
  report()

ets_fit %>% 
  select(.model = "ets_guess") %>% 
  report()

report(ets_fit)

augment(bench_fit[3]) %>%
  features(.innov, box_pierce, lag = 10)

augment(bench_fit[3]) %>%
  features(.innov, ljung_box, lag = 10)





# Generate forecasts for 76 weeks
ets_fc <- ets_fit %>% forecast(h = 18)
  
# Plot forecasts against actual values
ets_fc %>% 
  autoplot(train, level = NULL) +
  autolayer(
    test,
    colour = "black"
  ) +
  labs(
    y = "GWh",
    title = "Forecasts for weekly Solar Power Generation"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

accuracy(ets_fc, ts_data)

train %>%
  model(ETS(log(GWh) ~ error("A") + trend("A") + season("M"))) %>%
  forecast(h = 17) %>%
  autoplot(train)


#ts_data$diffGWh <- diff(log(ts_data$GWh))
ts_data$diffGWh <- c(NA, diff(log(ts_data$GWh)))

# BENCHMARK Model
# Set training data from W1 2015 to W52 2021
train <- ts_data %>%
  select(diffGWh) %>% 
  filter_index("2015 Jan" ~ "2021 Dec")

test <- ts_data %>%
  select(diffGWh) %>% 
  filter_index("2021 Dec" ~ .)

# Fit the models
bench_fit <- train %>%
  model(
    Mean = MEAN(diffGWh),
    Naive = NAIVE(diffGWh),
    SNaive = SNAIVE(diffGWh),
    Drift = NAIVE(diffGWh ~ drift())
  )

#TS Residuals
bench_fit[1] %>% 
  gg_tsresiduals()

#TS Residuals
bench_fit[2] %>% 
  gg_tsresiduals()

#TS Residuals
bench_fit[3] %>% 
  gg_tsresiduals()

#TS Residuals
bench_fit[4] %>% 
  gg_tsresiduals()

augment(bench_fit[3]) %>%
  features(.innov, box_pierce, lag = 10)

augment(bench_fit[3]) %>%
  features(.innov, ljung_box, lag = 10)

# Generate forecasts for 76 weeks
bench_fc <- bench_fit %>% forecast(h = 18)

# Plot forecasts against actual values
bench_fc %>% 
  autoplot(train, level = NULL) +
  autolayer(
    test,
    colour = "black"
  ) +
  labs(
    y = "GWh",
    title = "Forecasts for weekly Solar Power Generation"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

accuracy(bench_fc, ts_data)

test


# ETS
ets_fit <- train %>%
  model(ets = ETS(diffGWh),
        ets_log = ETS(diffGWh),
        #ets_boxcox = ETS(box_cox(GWh, lambda)),
        #additive = ETS(GWh ~ error("A") + trend("Ad") + season("A")),
        #multiplicative = ETS(GWh ~ error("M") + trend("M") + season("M")),
        additive_log = ETS(diffGWh ~ error("A") + trend("A") + season("M")),
        #additive_box_cox = ETS(box_cox(GWh, lambda) ~ error("A") + trend("A") + season("M"))
        #multiplicative_log = ETS(log(GWh) ~ error("M") + trend("A") + season("M")),
  )

ets_fit %>% 
  select(.model = "ets_log") %>% 
  report()

report(ets_fit)

#TS Residuals
ets_fit[2] %>% 
  gg_tsresiduals()

#TS Residuals
ets_fit[3] %>% 
  gg_tsresiduals()

#TS Residuals
ets_fit[4] %>% 
  gg_tsresiduals()

# Generate forecasts for 76 weeks
ets_fc <- ets_fit %>% forecast(h = 18)

# Plot forecasts against actual values
ets_fc %>% 
  autoplot(train, level = NULL) +
  autolayer(
    test,
    colour = "black"
  ) +
  labs(
    y = "GWh",
    title = "Forecasts for weekly Solar Power Generation"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

accuracy(ets_fc, ts_data)

# plot the data
#ts_data %>%
#  autoplot(box_cox(MWh, 0))
# Looking at the peaks we see an increasing trend in generated power by solar 
# The increasing trend can be seen less clear in the local minimums over time
# Indicates seasonality, as expected as solar power generation is more useful in summer due to more sun

# Examine seasonality 1
#ts_data %>%
#  gg_season(box_cox(MWh, 0))
# Confirms seasonality in the summer months

# Examine seasonality 2
#ts_data %>%
#  gg_subseries(box_cox(MWh, 0))
# Confirms upwards trend, but trend is stronger in summer month than in winter month

# Examine lags 
#ts_data %>%
#  gg_lag(box_cox(MWh, 0))

#Examine ACF
#ts_data %>%
#  ACF(box_cox(MWh, 0))

#ts_data %>%
#  ACF(box_cox(MWh, 0), lag_max = 150) %>%
#  autoplot()

test









# Convert the data to a time series object
ts_object <- ts(log(ts_data$GWh, 0), frequency = 12) 

# Apply STL decomposition
decomposed <- stl(ts_object, s.window = "periodic")

# Extract the components
trend <- decomposed$time.series[, "trend"]
seasonal <- decomposed$time.series[, "seasonal"]
residual <- decomposed$time.series[, "remainder"]

# Plot the components
plot(decomposed)

# Perform the ADF test with trend
#adf_result <- ur.df(diff(log(ts_data$MWh)), type = "trend")
#summary(adf_result)

#ts_data %>%
#  (diff(log(MWh))) %>% autoplot()
