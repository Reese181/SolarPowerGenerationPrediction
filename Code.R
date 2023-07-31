# imports
library(readr) 
library(fpp3)
library(ggplot2)
library(urca)
library(strucchange)
library(gridExtra)

# import the dataset
data <- read_delim("SolarPowerGenerationPrediction/Realisierte_Erzeugung_20150101_20230630_monthly.csv", delim = ";", locale = locale(decimal_mark = ","))

# view first few rows
print(data)

# column names
names(data)

#summary
summary(data)

# transform date
data$Time <- as.Date(data$Datum, "%d.%m.%Y")

#transform tibble to tsibble
ts_data <- data %>%
  mutate(Month = yearmonth(Time)) %>%
  as_tsibble(index = Month)

ts_data <- ts_data %>% 
  select("Photovoltaik [MWh] Berechnete Auflösungen") %>% 
  rename("MWh" = "Photovoltaik [MWh] Berechnete Auflösungen")

# Scale the data from MWh to GWh
ts_data$GWh <- ts_data$MWh / 1000

#summary
summary(ts_data$GWh)

# plot the data
ts_data %>%
  autoplot(GWh) + 
  labs(y="Power Produced [GWh]", x="Month")


# Check linearity with Scatterplot
ggplot(ts_data, aes(x = Month, y = GWh)) +
  geom_point() +
  labs(x = "Month", y = "Power Produced [GWh]")


# Check Trend, Seasonality and Remainder with STL decomposition
ts_data %>% 
  model(STL(GWh))  %>% 
  components() %>% 
  autoplot()

# Seasonal Plot
ts_data %>%
  gg_season(GWh) +
  labs(x = "Month", y = "Power Produced [GWh]")


# Apply a log transformation to data
# plot the data
ts_data %>%
  autoplot(log(GWh)) + 
  labs(y="log(Power Produced [GWh])", x="Month")

# plot STL decomposition after transformation
ts_data %>% 
  model(STL(log(GWh)))  %>% 
  components() %>% 
  autoplot()

# Pull guerrero feature to compare with log
lambda <- ts_data %>% 
  features(GWh, features = guerrero) %>% 
  pull(lambda_guerrero)
print(lambda)


# Unit Root tests - No differencing
# ADF Trend
summary(ur.df(log(ts_data$GWh), type = "trend")) #potentially stationary

# ADF Drift
summary(ur.df(log(ts_data$GWh), type = "drift")) #potentially stationary

# ADF None
summary(ur.df(log(ts_data$GWh), type = "none")) #not stationary

# KPSS Trend
summary(ur.kpss(log(ts_data$GWh), type = "tau")) #potentially stationary

# KPSS None
summary(ur.kpss(log(ts_data$GWh), type = "mu")) #potentially stationary

# KPSS seasonal
summary(ur.kpss(log(ts_data$GWh), type = "tau", use.lag = 12)) # not stationary

# KPSS seasonal
summary(ur.kpss(log(ts_data$GWh), type = "mu", use.lag = 12)) # stationary

# Unit Root tests - Seasonal differencing
# ADF Trend
summary(ur.df(diff(log(ts_data$GWh),lag=12), type = "trend")) #potentially stationary

# ADF Drift
summary(ur.df(diff(log(ts_data$GWh),lag=12), type = "drift")) #potentially stationary

# ADF None
summary(ur.df(diff(log(ts_data$GWh),lag=12),type = "none")) #potentially stationary

# KPSS Trend
summary(ur.kpss(diff(log(ts_data$GWh),lag=12), type = "tau")) #potentially stationary

# KPSS None
summary(ur.kpss(diff(log(ts_data$GWh),lag=12), type = "mu")) #potentially stationary

# KPSS seasonal
summary(ur.kpss(diff(log(ts_data$GWh),lag=12), type = "tau", use.lag = 12)) # not stationary

# KPSS seasonal
summary(ur.kpss(diff(log(ts_data$GWh),lag=12), type = "mu", use.lag = 12)) # stationary


# Unit Root tests - first lag differencing
# ADF Trend
summary(ur.df(diff(log(ts_data$GWh)), type = "trend")) #potentially stationary

# ADF Drift
summary(ur.df(diff(log(ts_data$GWh)), type = "drift")) #potentially stationary

# ADF None
summary(ur.df(diff(log(ts_data$GWh)),type = "none")) #potentially stationary

# KPSS Trend
summary(ur.kpss(diff(log(ts_data$GWh)), type = "tau")) #potentially stationary

# KPSS None
summary(ur.kpss(diff(log(ts_data$GWh)), type = "mu")) #potentially stationary

# KPSS seasonal
summary(ur.kpss(diff(log(ts_data$GWh)), type = "tau", use.lag = 12)) # not stationary

# KPSS seasonal
summary(ur.kpss(diff(log(ts_data$GWh)), type = "mu", use.lag = 12)) # stationary


# Plot all transformations or differences
plot1 <- ts_data %>%
  autoplot(GWh) +
  labs(y = "Power Produced\n [GWh]", x = "Month")

# Plot log
plot2 <- ts_data %>%
  autoplot(log(GWh)) +
  labs(y = "Log\n [GWh]", x = "Month")

plot3 <- ts_data %>%
  autoplot(log(GWh) %>% difference()) +
  labs(y = "First lag differenced\nLog [GWh]", x = "Month")

# Take seasonal difference
plot4 <- ts_data %>%
  autoplot(log(GWh) %>% difference(lag = 12)) +
  labs(y = "Seasonal differenced\nLog [GWh]", x = "Month")

# Combine the plots
#combined_plot <- plot_grid(plot1, plot2, plot3, plot4, ncol = 1)

# Display the combined plot
#combined_plot


# QLR test
sb_data <- ts_data %>%
  mutate(
    Lag0 = GWh,
    Lag1 = lag(GWh),
    Lag12 = lag(GWh,12),
    Month = Month
  )

# QLR test with monthly lags
qlr1 <- Fstats(Lag0 ~ Lag1 + Lag12, data = sb_data, from = 0.2)
test1 <- sctest(qlr1, type = "supF")
test1

breakpoints(qlr1, alpha = 0.05)

plot(qlr1, alpha = 0.05)
lines(breakpoints(qlr1))
# indicating nor structural breaks in the data


# Train Test Split
train <- ts_data %>%
  select(GWh) %>% 
  filter_index(. ~ "2021 Dec")

test <- ts_data %>%
  select(GWh) %>% 
  filter_index("2022 Jan" ~ .)

len_test <- nrow(test)
len_train <- nrow(train)
len_ts <- nrow(ts_data)

print(len_test)
print(len_train)
print(len_ts)


# ETS
ets_fit <- train %>%
  model(ets_auto = ETS(log(GWh)),
        ets_guess = ETS(log(GWh) ~ error("A") + trend("A") + season("M"))
  )

report(ets_fit)

ets_fit %>% 
  select(.model = "ets_guess") %>% 
  report()


# Plot residuals from ETS model
ets_fit %>%
  select(ets_guess) %>%
  gg_tsresiduals(type = "innovation")

# Ljung-Box Test
augment(ets_fit[2]) %>%
  features(.innov, ljung_box, lag = 24)


# ARIMA
# Check ACF und PACF Plot for guess
gg_tsdisplay(train %>% mutate(GWh = difference(log(GWh),lag=12)) %>% drop_na(GWh), plot_type = "partial", lag=36)

# Closer look at ACF and PACF
train_diff <- train %>%
  mutate(GWh = difference(log(GWh),lag=12))%>%
  drop_na(GWh)

# One significant spike at the seasonal lag 12 in ACF
# decaying seasonla lags in pacfs --> GUESS (0,0,0)(0,1,1)
# According to Oreilly book 
arima_fit <- train %>%
  model(
    arima_guess = ARIMA(log(GWh) ~ pdq(0,0,0) + PDQ(0,1,1)),
    arima_auto = ARIMA(log(GWh))
  )

arima_fit %>%  pivot_longer(everything(), names_to = "Model name",
                    values_to = "Orders")

glance(arima_fit)
# We choose the auto model

arima_fit %>%  select(arima_auto) %>%  gg_tsresiduals()
# No significant autocorrelation
# Increasing variance in

# Ljung-Box Test
augment(arima_fit[2]) %>%
  features(.innov, ljung_box, lag = 24)
# Decision for ARIMA Auto


### FORECAST
# Forecast Auto ETS on test set
fit <- train %>% model(
  arima = ARIMA(log(GWh)),
  ets = ETS(log(GWh) ~ error("A") + trend("A") + season("M"))
)

# Create the ETS plot
fc_ets <- fit %>%
  select(ets) %>%
  forecast(test) %>%
  autoplot(ts_data) +
  labs(title = "ETS(A,A,M)")

# Create the ARIMA plot
fc_arima <- fit %>%
  select(arima) %>%
  forecast(test) %>%
  autoplot(ts_data) +
  labs(title = "ARIMA(0,0,0)(2,1,0)")

# Merge the two plots
merged_plot <- gridExtra::grid.arrange(fc_ets, fc_arima, nrow = 2)

# Display the merged plot
print(merged_plot)
# Plot Both
fc_both <- fit %>%
  select(ets,arima) %>%
  forecast(test) %>%
  autoplot(test,level=NULL) 
print(fc_both)

# Accuracy measures
fit %>%
  forecast(test) %>%
  accuracy(ts_data)

# According to Error metrics, ETS outperforms ARIMA 
ts_data %>% 
  model(ETS(log(GWh) ~ error("A") + trend("A") + season("M"))) %>% 
  forecast(h = 18) |>
  autoplot(ts_data) +
  labs(y = "Power Produced\n [GWh]")
  
