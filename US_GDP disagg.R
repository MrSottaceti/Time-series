library(tseries)
library(fredr)
library(tempdisagg)
library(forecast)
library(ggplot2)

source("fred_key.R")
fredr_set_key(fred_key) #YOU NEED A KEY TO USE FRED API

#GET GDP DATA (QUARTERLY) AND DISAGGREGATE TO MONTHLY
gdp_us <- fredr(series_id = "GDPC1", 
                frequency = "q", 
                observation_start = as.Date("2000-01-01"))

#CONVERT A DATAFRAME INTO A TIME SERIES 
gdp_ts <- ts(gdp_us$value, start = c(2000, 1), frequency = 4)

#GET INDUSTRIAL PRODUCTION DATA (AS A PROXY)
indpro_us <- fredr(series_id = "INDPRO",
                   frequency = "m",
                   observation_start = as.Date("2000-01-01"))

indpro <- ts(indpro_us$value,  start = c(2000, 1), end=c(2024, 12), frequency = 12)

#DISAGGREGATE USING TWO METHODS. THE RIGHT CONVERSION IS SUM
gdp_month_dent <- td(gdp_ts ~ 1, to = "monthly", 
                     method = "denton-cholette", 
                     conversion = "sum")

gdp_month_chow <- td(gdp_ts ~ indpro,
                     to = "monthly",
                     method = "chow-lin-maxlog",
                     conversion = "sum")

gdp_m_d <- predict(gdp_month_dent)
gdp_m_c <- predict(gdp_month_chow)

#CHECK: VISUALLY, MSE/RMSE, CORRELATION
ts.plot(cbind(predict(gdp_month_dent), predict(gdp_month_chow)), col = c("blue", "red"), lty = 1:2)
legend("topleft", legend = c("Denton", "Chow-Lin"), col = c("blue", "red"), lty = 1:2)

rmse <- sqrt(mean((predict(gdp_month_dent) - predict(gdp_month_chow))^2))
rmse_pct <- rmse/mean(gdp_m_c)*100  #0.3731223%
ratio_var <- (rmse^2)/var(gdp_ts)
ratio_var #7.140365e-05

cor(predict(gdp_month_dent), predict(gdp_month_chow), 
    use = "complete.obs") # 0.9996757


df_plot <- data.frame(Date = as.Date(time(gdp_m_d)),
  Denton = as.numeric(gdp_m_d),
  ChowLin = as.numeric(gdp_m_c))

#SEE: CL vs Denton.png
ggplot(df_plot, aes(x = Date)) +
  geom_line(aes(y = Denton, color = "Denton")) +
  geom_line(aes(y = ChowLin, color = "Chow-Lin")) +
  labs(title = "GDP DISAGGREGATION", y = "Gdp", x = "Time") +
  scale_color_manual(values = c("Denton" = "blue", "Chow-Lin" = "red"), name = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
