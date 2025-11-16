# MVPA与认知变化的快速平滑图
# ==========================================
# 作者: AI Assistant
# 日期: 2024
# 描述: 绘制认知变化与MVPA（控制所有协变量）的快速平滑图

# 1. 准备工作
# ==========================================
# 加载必要的包
library(dplyr)
library(ggplot2)
library(mgcv)  # 用于GAM平滑
library(splines)  # 用于样条函数
library(tidyr)  # 用于gather函数
library(parallel)
library(doParallel)
source("paths.R")

# 2. 数据准备
# ==========================================
print("正在准备数据...")

# 加载数据（假设这些数据已经在环境中）
# 如果数据未加载，需要先运行相应的数据准备脚本

# 检查必要的数据是否存在
required_data <- c("Derived_accelerometry", "cognitive_scores", "baseline_table_step1", "final_dataset")
missing_data <- required_data[!sapply(required_data, exists)]

if (length(missing_data) > 0) {
  stop(paste("缺少必要的数据对象:", paste(missing_data, collapse = ", "), 
             "\n请先运行相应的数据准备脚本"))
}

# 步骤 A: 清理并准备MVPA和认知得分数据
print("步骤 A: 正在准备MVPA和认知得分数据...")
mvpa_clean <- Derived_accelerometry %>%
  transmute(
    Participant_ID = `Participant ID`,
    overall_mvpa_hours = `Moderate-Vigorous - Overall average | Instance 0` * 24  # 转换为每天MVPA小时数
  )

cognitive_score_clean <- cognitive_scores %>%
  select(
    Participant_ID,
    cognitive_score_0,
    cognitive_change
  )

# 步骤 B: 合并数据
print("步骤 B: 正在合并数据...")
baseline_table_step1_clean <- baseline_table_step1 %>%
  select(-any_of(c("cognitive_score_0")))

full_merged_table <- final_dataset %>%
  left_join(baseline_table_step1_clean, by = "Participant_ID") %>%
  left_join(mvpa_clean, by = "Participant_ID") %>%
  left_join(cognitive_score_clean, by = "Participant_ID")

# 3. 数据采样
# ==========================================
print("步骤 C: 正在进行数据采样...")

# 定义所有协变量
all_covariates <- c(
  "age_recruitment", "sex", "townsend_index", "bmi", "smoking_status",
  "alcohol_status", "education_level", "cvd_history_any", 
  "history_diabetes", "cognitive_score_0"
)

# 选择用于绘图的数据
plot_data_full <- full_merged_table %>%
  select(
    Participant_ID,
    cognitive_change,
    overall_mvpa_hours,
    all_of(all_covariates)
  ) %>%
  filter(!is.na(cognitive_change) & !is.na(overall_mvpa_hours))

print(paste("原始数据样本数:", nrow(plot_data_full)))

# 数据采样：5%采样，最低1000最大10000
set.seed(123)
sample_proportion <- 0.05
min_sample_size <- 1000
max_sample_size <- 10000

sample_size <- max(min_sample_size, 
                   min(max_sample_size, 
                       round(nrow(plot_data_full) * sample_proportion)))

# 如果数据量很小，使用全部数据
if (nrow(plot_data_full) <= min_sample_size) {
  plot_data <- plot_data_full
  print("数据量较小，使用全部数据进行绘图")
} else {
  plot_data <- plot_data_full[sample(nrow(plot_data_full), sample_size), ]
  print(paste("已采样", nrow(plot_data), "个样本进行绘图（原始数据:", nrow(plot_data_full), "）"))
}

# 4. 控制协变量的残差分析
# ==========================================
print("步骤 D: 正在计算控制协变量后的残差...")

# 首先检查并处理缺失值
print(paste("处理前数据行数:", nrow(plot_data)))

# 检查每个变量的缺失值情况
missing_summary <- plot_data %>%
  summarise_all(~sum(is.na(.))) %>%
  gather(variable, missing_count) %>%
  filter(missing_count > 0)

if (nrow(missing_summary) > 0) {
  print("发现缺失值:")
  print(missing_summary)
}

# 移除包含缺失值的行
plot_data_complete <- plot_data %>%
  filter(complete.cases(.))

print(paste("移除缺失值后数据行数:", nrow(plot_data_complete)))

# 如果数据太少，给出警告
if (nrow(plot_data_complete) < 100) {
  warning("完整数据太少，可能影响分析结果")
}

# 拟合控制所有协变量的线性模型
covariate_formula <- as.formula(
  paste("cognitive_change ~", paste(all_covariates, collapse = " + "))
)

# 拟合模型
control_model <- lm(covariate_formula, data = plot_data_complete)

# 计算残差（这些残差代表去除协变量影响后的认知变化）
plot_data_complete$residuals <- residuals(control_model)

# 更新plot_data为完整数据
plot_data <- plot_data_complete

# 5. 创建平滑图
# ==========================================
print("步骤 E: 正在创建平滑图...")

# 方法1: 使用GAM平滑
smooth_plot_gam <- ggplot(plot_data, aes(x = overall_mvpa_hours, y = residuals)) +
  geom_point(alpha = 0.4, size = 0.8, color = "steelblue") +
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cs", k = 10),
              se = TRUE, 
              color = "red", 
              size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.7) +
  labs(
    title = "MVPA与认知变化的关系（控制所有协变量）",
    subtitle = paste("样本数:", nrow(plot_data), "| 使用GAM平滑"),
    x = "MVPA (小时/天)",
    y = "认知变化残差（控制协变量后）",
    caption = "残差 = 实际认知变化 - 协变量预测值"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 11),
    plot.caption = element_text(size = 9, hjust = 0)
  ) +
  # 限制X轴范围到99%分位数，避免极端值影响可视化
  xlim(0, quantile(plot_data$overall_mvpa_hours, 0.99, na.rm = TRUE))

# 方法2: 使用LOESS平滑
smooth_plot_loess <- ggplot(plot_data, aes(x = overall_mvpa_hours, y = residuals)) +
  geom_point(alpha = 0.4, size = 0.8, color = "steelblue") +
  geom_smooth(method = "loess", 
              se = TRUE, 
              color = "darkgreen", 
              size = 1.2,
              span = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.7) +
  labs(
    title = "MVPA与认知变化的关系（控制所有协变量）",
    subtitle = paste("样本数:", nrow(plot_data), "| 使用LOESS平滑"),
    x = "MVPA (小时/天)",
    y = "认知变化残差（控制协变量后）",
    caption = "残差 = 实际认知变化 - 协变量预测值"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 11),
    plot.caption = element_text(size = 9, hjust = 0)
  ) +
  xlim(0, quantile(plot_data$overall_mvpa_hours, 0.99, na.rm = TRUE))

# 方法3: 使用样条平滑
smooth_plot_spline <- ggplot(plot_data, aes(x = overall_mvpa_hours, y = residuals)) +
  geom_point(alpha = 0.4, size = 0.8, color = "steelblue") +
  geom_smooth(method = "lm",
              formula = y ~ splines::ns(x, df = 4),
              se = TRUE, 
              color = "purple", 
              size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.7) +
  labs(
    title = "MVPA与认知变化的关系（控制所有协变量）",
    subtitle = paste("样本数:", nrow(plot_data), "| 使用样条平滑"),
    x = "MVPA (小时/天)",
    y = "认知变化残差（控制协变量后）",
    caption = "残差 = 实际认知变化 - 协变量预测值"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 11),
    plot.caption = element_text(size = 9, hjust = 0)
  ) +
  xlim(0, quantile(plot_data$overall_mvpa_hours, 0.99, na.rm = TRUE))

# 6. 组合图形
# ==========================================
print("步骤 F: 正在组合图形...")

# 使用gridExtra组合三个图形
library(gridExtra)
combined_plot <- grid.arrange(
  smooth_plot_gam,
  smooth_plot_loess,
  smooth_plot_spline,
  ncol = 1,
  nrow = 3
)

# 7. 保存图形
# ==========================================
print("步骤 G: 正在保存图形...")

# 保存组合图形
ggsave(
  filename = path_result("mvpa_cognitive_smooth_plots.png"),
  plot = combined_plot,
  width = 10, 
  height = 15, 
  dpi = 300
)

# 单独保存每个图形
ggsave(
  filename = path_result("mvpa_cognitive_gam.png"),
  plot = smooth_plot_gam,
  width = 10, 
  height = 6, 
  dpi = 300
)

ggsave(
  filename = path_result("mvpa_cognitive_loess.png"),
  plot = smooth_plot_loess,
  width = 10, 
  height = 6, 
  dpi = 300
)

ggsave(
  filename = path_result("mvpa_cognitive_spline.png"),
  plot = smooth_plot_spline,
  width = 10, 
  height = 6, 
  dpi = 300
)

# 8. 输出统计信息
# ==========================================
print("步骤 H: 正在计算统计信息...")

# 计算基本统计信息
mvpa_stats <- summary(plot_data$overall_mvpa_hours)
residual_stats <- summary(plot_data$residuals)

print("===== MVPA统计信息 =====")
print(mvpa_stats)

print("===== 认知变化残差统计信息 =====")
print(residual_stats)

# 计算相关性
correlation <- cor(plot_data$overall_mvpa_hours, plot_data$residuals, use = "complete.obs")
print(paste("MVPA与认知变化残差的相关系数:", round(correlation, 4)))

# 保存统计信息
stats_summary <- data.frame(
  Variable = c("MVPA_hours_per_day", "cognitive_change_residuals"),
  Mean = c(mean(plot_data$overall_mvpa_hours, na.rm = TRUE), 
           mean(plot_data$residuals, na.rm = TRUE)),
  SD = c(sd(plot_data$overall_mvpa_hours, na.rm = TRUE), 
         sd(plot_data$residuals, na.rm = TRUE)),
  Min = c(min(plot_data$overall_mvpa_hours, na.rm = TRUE), 
          min(plot_data$residuals, na.rm = TRUE)),
  Max = c(max(plot_data$overall_mvpa_hours, na.rm = TRUE), 
          max(plot_data$residuals, na.rm = TRUE)),
  Correlation = c(correlation, NA)
)

# 使用UTF-8编码保存CSV文件，避免中文字符显示问题
write.csv(stats_summary, 
          file = path_result("mvpa_cognitive_smooth_stats.csv"), 
          row.names = FALSE,
          fileEncoding = "UTF-8")

# 同时保存一个带中文标签的版本（如果系统支持）
stats_summary_chinese <- data.frame(
  Variable = c("MVPA_小时每天", "认知变化残差"),
  Mean = c(mean(plot_data$overall_mvpa_hours, na.rm = TRUE), 
           mean(plot_data$residuals, na.rm = TRUE)),
  SD = c(sd(plot_data$overall_mvpa_hours, na.rm = TRUE), 
         sd(plot_data$residuals, na.rm = TRUE)),
  Min = c(min(plot_data$overall_mvpa_hours, na.rm = TRUE), 
          min(plot_data$residuals, na.rm = TRUE)),
  Max = c(max(plot_data$overall_mvpa_hours, na.rm = TRUE), 
          max(plot_data$residuals, na.rm = TRUE)),
  Correlation = c(correlation, NA)
)

write.csv(stats_summary_chinese, 
          file = path_result("mvpa_cognitive_smooth_stats_chinese.csv"), 
          row.names = FALSE,
          fileEncoding = "UTF-8")

print("===== 完成 =====")
print("图形已保存到result文件夹中:")
print("- mvpa_cognitive_smooth_plots.png (组合图)")
print("- mvpa_cognitive_gam.png (GAM平滑)")
print("- mvpa_cognitive_loess.png (LOESS平滑)")
print("- mvpa_cognitive_spline.png (样条平滑)")
print("统计信息已保存到:")
print("- mvpa_cognitive_smooth_stats.csv (英文变量名)")
print("- mvpa_cognitive_smooth_stats_chinese.csv (中文变量名)")

# 显示图形
print("正在显示图形...")
print(combined_plot)
