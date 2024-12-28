g_data <- read.csv("/Users/lixinzhu/Desktop/RData/R-timeseries analysis/gdp.csv")
# 加载数据并转换日期列
g_data$date <- as.Date(paste0(g_data$date, "-01-01"), format = "%Y-%m-%d")

# 创建时间序列
library(xts)
g_ts <- xts(g_data$GDP_INDEX, order.by = g_data$date)
plot(g_ts)

# 差分和平稳性检验
g_diff1 <- diff(g_ts, differences = 1)
plot(g_diff1)
g_diff1 <- na.omit(g_diff1)
adf_test1 <- adf.test(g_diff1)
# 图显示一阶差分提取了原序列中的部分长期趋势，但长期趋势信息提取不充分，仍然蕴含长期递增的趋势

#继续进行二阶拆分
g_diff <- diff(g_ts, differences = 2)
plot(g_diff)
g_diff <- na.omit(g_diff)
adf_test <- adf.test(g_diff)
print(adf_test)

# white-noise 检验-结果表明序列不是白噪声 可以进行后续的步骤
for(k in 1:4)print(Box.test(g_diff,lag=1*k))


# 检查 ACF 和 PACF
acf(g_diff, main = "ACF of GDPINDEX")
pacf(g_diff, main = "PACF of GDPINDEX")

# ARIMA 模型拟合-自动选择最佳模型
library(forecast)
arima_model <- auto.arima(g_ts)
summary(arima_model)

# 模型残差白噪声检验-残差的确是白噪声 模型提取的很充分了
checkresiduals(arima_model)

for(k in 1:4)
  print(Box.test(arima_model$residuals,lag=1*k))

# 预测未来 5 年
forecast_values <- forecast(arima_model, h = 5)
autoplot(forecast_values) +
  xlab("Year") +
  ylab("Pieces") +
  ggtitle("5-Year Forecast of GDPindex") +
  theme_minimal()
show(forecast_values)
#----------------------------------------------------------------------------
# 使用 d = 2 手动指定差分次数
mod <- arima(g_ts, order = c(1, 2, 1))  # 这里的 p 和 q 可以根据 ACF/PACF 图调整

# 检查模型
summary(mod)

# 模型诊断（白噪声检验等） 残差是白噪声 表明模型提取的很充分了
for(k in 1:2) print(Box.test(mod$residuals,lag=6*k))
checkresiduals(mod)

# 预测未来 5 年
forecast_values <- forecast(mod, h = 5)
autoplot(forecast_values) +
  xlab("Year") +
  ylab("GDP INDEX") +
  ggtitle("5-Year Forecast of GDPindex ") +
  theme_minimal()

#-------------------------------------------------------------------模型选择
AIC(arima_model)
AIC(mod)
BIC(arima_model)
BIC(mod)

Box.test(residuals(arima_model), lag = 6)
Box.test(residuals(mod), lag =6)