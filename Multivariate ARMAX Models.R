# 加载必要的库
library(vars)

# 假设cmort, tempr, part是已经加载的数据集，分别代表心血管死亡率、温度和颗粒物水平
# 将这些数据组合成一个矩阵
x <- cbind(cmort, tempr, part)
summary(VAR(x, p=1, type='both'))

summary(fit <- VAR(x, p=2, type="both")) #
# 使用VAR模型拟合数据，选择最佳滞后阶数
selection <- VARselect(x, lag.max=10, type="both")
best_model <- selection$selection[which.min(selection$AIC)]

# 拟合VAR模型
fit <- VAR(x, p=best_model, type="both")

# 显示模型结果
summary(fit)

# 预测未来值
forecast <- predict(fit, n.ahead=24, ci=0.95)  # 预测未来24个时间点，95%置信区间
fanchart(forecast)  # 绘制预测图