library(dplyr)
library(ggplot2)
library(splines)

# 从插补对象中提取长格式数据
completed_long <- complete(imputed_object, action = "long", include = TRUE)

# 添加Participant_ID列（使用行号作为ID）
completed_long$Participant_ID <- rep(1:nrow(imputation_data), times = imputed_object$m + 1)

# 方法1：使用插补均值（减少数据量）
plot_data_mean <- completed_long %>%
  filter(.imp > 0) %>%
  group_by(Participant_ID) %>%
  summarise(
    avg_mvpa_hours = mean(overall_mvpa_hours, na.rm = TRUE),
    avg_cognitive_change = mean(cognitive_change, na.rm = TRUE),
    .groups = "drop"
  )

# 方法2：随机抽样进一步减少数据量（如果还是太大）
set.seed(123)
if (nrow(plot_data_mean) > 10000) {
  plot_data_mean <- plot_data_mean %>%
    sample_n(10000)  # 随机抽取10000个点
  print(paste("数据量过大，已随机抽样", nrow(plot_data_mean), "个点进行绘图"))
}

# 检查数据分布
print("MVPA数据分布：")
print(summary(plot_data_mean$avg_mvpa_hours))
print(paste("99%分位数：", quantile(plot_data_mean$avg_mvpa_hours, 0.99, na.rm = TRUE)))

# 方法3：使用更轻量的绘图方式
EDA2plot <- ggplot(plot_data_mean, aes(x = avg_mvpa_hours, y = avg_cognitive_change)) +
  geom_point(alpha = 0.3, size = 0.8) +  # 减小点的大小和透明度
  geom_smooth(method = "loess", se = TRUE, color = "#1f78b4", span = 0.3) +  # 增加span减少计算量
  geom_smooth(method = "lm",
              formula = y ~ splines::ns(x, df = 3),
              se = FALSE, color = "#33a02c", linetype = "dashed") +
  # 控制X轴范围的方法（选择其中一种）：
  # 方法A：使用99%分位数作为上限
  xlim(0, quantile(plot_data_mean$avg_mvpa_hours, 0.99, na.rm = TRUE)) +
  # 方法B：固定上限（比如2小时/天）
  # xlim(0, 2) +
  # 方法C：使用数据的中位数+3倍标准差
  # xlim(0, median(plot_data_mean$avg_mvpa_hours, na.rm = TRUE) + 3*sd(plot_data_mean$avg_mvpa_hours, na.rm = TRUE)) +
  labs(
    x = "MVPA 总量（小时/天，插补平均）",
    y = "认知变化分数（插补平均）",
    title = "MVPA 与认知变化的关系（插补均值，X轴限制在99%分位数）"
  ) +
  theme_minimal(base_size = 12)  # 减小字体

# 显示图形
print(EDA2plot)

# 如果还是太慢，可以使用这个更轻量的版本：
# EDA2plot_light <- ggplot(plot_data_mean, aes(x = avg_mvpa_hours, y = avg_cognitive_change)) +
#   geom_bin2d(bins = 50) +  # 使用密度图代替散点图
#   geom_smooth(method = "lm", se = TRUE, color = "red") +
#   labs(
#     x = "MVPA 总量（小时/天，插补平均）",
#     y = "认知变化分数（插补平均）",
#     title = "MVPA 与认知变化的关系（密度图）"
#   ) +
#   theme_minimal()
# print(EDA2plot_light)