#Linear Trend Method
#Non-linear Trend Method
# 加载数据
d_data <- read.csv("/Users/lixinzhu/Desktop/RData/R-timeseries analysis/delivery.csv")


# 使用lm进行线性回归 一次+二次
x.fit1 <- lm(pieces~Date,data=d_data)
x.fit2<- lm(pieces~Date+I(Date^2),data=d_data)

# 查看线性回归模型摘要
summary(x.fit1)
summary(x.fit2)

# 绘制时间序列图
plot(d_data, main = "Delivery Pieces Over Time", xlab = "Date", ylab = "Pieces")

# 将回归结果的趋势线添加到图中
abline(x.fit1, col = 2)
lines(d_data$Date,fitted(x.fit1),col="blue")
lines(d_data$Date,fitted(x.fit2),col = "green")

#------------------------------------------------------------------------------------------
#Simple Moving Average, SMA
library(TTR)

# 读取数据
d_data <- read.csv("/Users/lixinzhu/Desktop/RData/R-timeseries analysis/delivery.csv")

# 假设 'Date' 列是年份，并且 'pieces' 列是快递数量
# 反转数据的顺序，使得年份从2004年到2024年
d_data <- d_data[rev(1:nrow(d_data)), ]

# 创建时间序列
x <- ts(d_data$pieces, start = 2004)  # 这里假设数据的起始年份是2004

# 绘制反转后的时间序列图
plot(x)


# 计算5期的简单移动平均
x.ma <- SMA(x, n = 5)

# 绘制原始数据和移动平均线
plot(x, main="Delivery Pieces with Simple Moving Average")
lines(x.ma, col = 2)

# 预测未来值 - 假设你想预测未来1年的数据，或更多期的数据
# 基于现有的移动平均线，预测接下来的值
future_values <- tail(x.ma, 1)  # 取最后一个移动平均值作为预测值
cat("未来预测值为：", future_values)


#--------------------------------------------------------------------------------
#Exponential Smoothing
library(forecast)
d_data <- read.csv("/Users/lixinzhu/Desktop/RData/R-timeseries analysis/delivery.csv")
d_data <- d_data[rev(1:nrow(d_data)), ]
# 创建时间序列
x <- ts(d_data$pieces, start = 2004)

# 使用 Holt 两参数指数平滑
hw_model <- HoltWinters(x,gamma=F)

# 预测未来 5 年
forecast_values <- forecast(hw_model, h = 5)

# 绘制预测图
plot(forecast_values,main = "Delivery Pieces Over Time (forcast)")
