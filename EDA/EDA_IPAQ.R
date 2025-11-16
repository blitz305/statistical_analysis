# IPAQ与认知得分变化的EDA分析
# ==========================================
# 目标：探索IPAQ活动组别与认知得分变化的关系，为模型选择提供依据

# 1. 准备工作
# ==========================================
library(dplyr)
library(ggplot2)
library(gridExtra)
library(car)  # 用于方差齐性检验
library(nortest)  # 用于正态性检验
library(scales)  # 用于百分比格式化
source("paths.R")

# 加载数据
print("正在加载数据...")
IPAQ <- readRDS("C:/Users/Dragon/Desktop/UKB数据/IPAQ.rds")
cognitive_scores <- readRDS(path_mid("cognitive_scores.rds"))

# 2. 数据提取和准备
# ==========================================
print("正在提取和准备数据...")

# 提取IPAQ的第一列（Participant ID）和第十二列（IPAQ activity group | Instance 1）
IPAQ_clean <- IPAQ %>%
  select(
    Participant_ID = `Participant ID`,
    ipaq_activity_group = `IPAQ activity group | Instance 2`
  ) %>%
  # 移除缺失值
  filter(!is.na(ipaq_activity_group))

# 提取认知得分变化
cognitive_clean <- cognitive_scores %>%
  select(
    Participant_ID,
    cognitive_change
  ) %>%
  filter(!is.na(cognitive_change))

# 合并数据
merged_data <- IPAQ_clean %>%
  inner_join(cognitive_clean, by = "Participant_ID") %>%
  mutate(
    # 确保ipaq_activity_group是数值型
    ipaq_activity_group = as.numeric(ipaq_activity_group),
    # 将IPAQ活动组别转换为因子，并添加标签
    ipaq_activity_group = factor(
      ipaq_activity_group,
      levels = c(0, 1, 2),
      labels = c("不活跃", "一般", "非常活跃")
    )
  ) %>%
  # 移除转换后可能的NA值
  filter(!is.na(ipaq_activity_group))

print(paste("合并后的数据行数:", nrow(merged_data)))
print(paste("各活动组别的人数分布:"))
print(table(merged_data$ipaq_activity_group))

# 3. 描述性统计
# ==========================================
print("\n===== 描述性统计 =====")

# 总体统计
overall_stats <- merged_data %>%
  summarise(
    n = n(),
    mean = mean(cognitive_change, na.rm = TRUE),
    sd = sd(cognitive_change, na.rm = TRUE),
    median = median(cognitive_change, na.rm = TRUE),
    q25 = quantile(cognitive_change, 0.25, na.rm = TRUE),
    q75 = quantile(cognitive_change, 0.75, na.rm = TRUE),
    min = min(cognitive_change, na.rm = TRUE),
    max = max(cognitive_change, na.rm = TRUE)
  )
print("总体认知得分变化统计:")
print(overall_stats)

# 按组别统计
group_stats <- merged_data %>%
  group_by(ipaq_activity_group) %>%
  summarise(
    n = n(),
    mean = mean(cognitive_change, na.rm = TRUE),
    sd = sd(cognitive_change, na.rm = TRUE),
    median = median(cognitive_change, na.rm = TRUE),
    q25 = quantile(cognitive_change, 0.25, na.rm = TRUE),
    q75 = quantile(cognitive_change, 0.75, na.rm = TRUE),
    se = sd / sqrt(n)
  )
print("\n按IPAQ活动组别分组的认知得分变化统计:")
print(group_stats)

# 4. 正态性检验
# ==========================================
print("\n===== 正态性检验 =====")

# 总体正态性检验
shapiro_test_overall <- shapiro.test(merged_data$cognitive_change)
print(paste("总体Shapiro-Wilk正态性检验: W =", round(shapiro_test_overall$statistic, 4), 
            ", p =", format(shapiro_test_overall$p.value, scientific = TRUE)))

# 各组别正态性检验
normality_by_group <- merged_data %>%
  group_by(ipaq_activity_group) %>%
  summarise(
    n = n(),
    shapiro_w = ifelse(n >= 3 & n <= 5000, {
      test <- shapiro.test(cognitive_change)
      round(test$statistic, 4)
    }, NA),
    shapiro_p = ifelse(n >= 3 & n <= 5000, {
      test <- shapiro.test(cognitive_change)
      format(test$p.value, scientific = TRUE)
    }, NA)
  )
print("\n各组别Shapiro-Wilk正态性检验:")
print(normality_by_group)

# 5. 方差齐性检验
# ==========================================
print("\n===== 方差齐性检验 =====")
levene_test <- car::leveneTest(cognitive_change ~ ipaq_activity_group, data = merged_data)
print("Levene方差齐性检验:")
print(levene_test)

# 6. 可视化分析
# ==========================================
print("\n正在生成可视化图表...")

# 图1: 箱线图
p1 <- ggplot(merged_data, aes(x = ipaq_activity_group, y = cognitive_change, fill = ipaq_activity_group)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
  labs(
    title = "IPAQ活动组别与认知得分变化的关系",
    x = "IPAQ活动组别",
    y = "认知得分变化",
    fill = "活动组别"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none"
  )

# 图2: 密度图
p2 <- ggplot(merged_data, aes(x = cognitive_change, fill = ipaq_activity_group)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "认知得分变化的分布（按活动组别）",
    x = "认知得分变化",
    y = "密度",
    fill = "活动组别"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

# 图3: 小提琴图
p3 <- ggplot(merged_data, aes(x = ipaq_activity_group, y = cognitive_change, fill = ipaq_activity_group)) +
  geom_violin(alpha = 0.7, trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.8) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
  labs(
    title = "认知得分变化的分布形态（小提琴图）",
    x = "IPAQ活动组别",
    y = "认知得分变化",
    fill = "活动组别"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none"
  )

# 图4: 带误差棒的均值图
p4 <- ggplot(group_stats, aes(x = ipaq_activity_group, y = mean, fill = ipaq_activity_group)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  labs(
    title = "各组别认知得分变化的均值（±标准误）",
    x = "IPAQ活动组别",
    y = "认知得分变化均值",
    fill = "活动组别"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none"
  )

# 图5: 认知得分变化的总体直方图
p5 <- ggplot(merged_data, aes(x = cognitive_change)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
  geom_vline(aes(xintercept = mean(cognitive_change)), 
             linetype = "dashed", color = "red", size = 1) +
  geom_vline(aes(xintercept = median(cognitive_change)), 
             linetype = "dashed", color = "darkgreen", size = 1) +
  labs(
    title = "认知得分变化的分布直方图",
    x = "认知得分变化",
    y = "频数",
    subtitle = "红色虚线=均值，绿色虚线=中位数"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )

# 图6: 按活动组别分组的直方图
p6 <- ggplot(merged_data, aes(x = cognitive_change, fill = ipaq_activity_group)) +
  geom_histogram(bins = 30, alpha = 0.7, color = "white", position = "identity") +
  facet_wrap(~ ipaq_activity_group, ncol = 3) +
  labs(
    title = "认知得分变化的分布直方图（按活动组别分组）",
    x = "认知得分变化",
    y = "频数",
    fill = "活动组别"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    strip.text = element_text(size = 11, face = "bold")
  )

# 保存图表
ggsave(path_result("IPAQ_cognitive_boxplot.png"), p1, width = 10, height = 6, dpi = 300)
ggsave(path_result("IPAQ_cognitive_density.png"), p2, width = 10, height = 6, dpi = 300)
ggsave(path_result("IPAQ_cognitive_violin.png"), p3, width = 10, height = 6, dpi = 300)
ggsave(path_result("IPAQ_cognitive_mean.png"), p4, width = 10, height = 6, dpi = 300)
ggsave(path_result("IPAQ_cognitive_histogram.png"), p5, width = 10, height = 6, dpi = 300)
ggsave(path_result("IPAQ_cognitive_histogram_by_group.png"), p6, width = 12, height = 5, dpi = 300)

print("图表已保存到result文件夹")

# 7. 初步统计检验（单因素方差分析）
# ==========================================
print("\n===== 单因素方差分析（ANOVA） =====")
anova_result <- aov(cognitive_change ~ ipaq_activity_group, data = merged_data)
print(summary(anova_result))

# 8. 模型选择建议
# ==========================================
print("\n===== 模型选择建议 =====")

# 判断条件
is_normal <- shapiro_test_overall$p.value > 0.05
is_homogeneous <- levene_test$`Pr(>F)`[1] > 0.05
n_groups <- length(unique(merged_data$ipaq_activity_group))

print(paste("数据正态性: ", ifelse(is_normal, "是", "否"), 
            "(p =", format(shapiro_test_overall$p.value, scientific = TRUE), ")"))
print(paste("方差齐性: ", ifelse(is_homogeneous, "是", "否"), 
            "(p =", format(levene_test$`Pr(>F)`[1], scientific = TRUE), ")"))

if (is_normal && is_homogeneous) {
  print("\n推荐模型: 线性回归模型 (Linear Regression)")
  print("  - 适用条件：数据满足正态性和方差齐性假设")
  print("  - 可以使用标准的线性模型分析组别间的差异")
} else if (!is_normal && is_homogeneous) {
  print("\n推荐模型: 广义线性模型 (GLM) 或 非参数检验")
  print("  - 数据不满足正态性，但方差齐性满足")
  print("  - 可以考虑使用Kruskal-Wallis检验（非参数）")
  print("  - 或使用GLM with appropriate distribution family")
} else if (is_normal && !is_homogeneous) {
  print("\n推荐模型: Welch's ANOVA 或 稳健回归")
  print("  - 数据满足正态性，但方差不齐")
  print("  - 可以使用Welch's ANOVA（oneway.test）")
  print("  - 或使用稳健标准误的回归模型")
} else {
  print("\n推荐模型: 非参数检验 或 稳健方法")
  print("  - 数据既不满足正态性，也不满足方差齐性")
  print("  - 强烈推荐使用Kruskal-Wallis检验")
  print("  - 或使用稳健回归方法（如robust regression）")
}

print("\n===== EDA分析完成（连续变量）=====\n\n")

# 9. 二分类结局变量的EDA分析
# ==========================================
print("===== 开始二分类结局变量的EDA分析 =====")

# 创建二分类变量：认知得分变化 >= 0 为提升(1)，< 0 为降低(0)
merged_data_binary <- merged_data %>%
  mutate(
    cognitive_change_binary = ifelse(cognitive_change >= 0, 1, 0),
    cognitive_change_binary_label = factor(
      cognitive_change_binary,
      levels = c(0, 1),
      labels = c("降低", "提升")
    )
  )

print(paste("二分类变量创建完成，数据行数:", nrow(merged_data_binary)))

# 9.1 描述性统计（二分类变量）
# ==========================================
print("\n===== 二分类结局变量的描述性统计 =====")

# 总体统计
overall_binary_stats <- merged_data_binary %>%
  summarise(
    n_total = n(),
    n_decrease = sum(cognitive_change_binary == 0),
    n_increase = sum(cognitive_change_binary == 1),
    prop_decrease = mean(cognitive_change_binary == 0),
    prop_increase = mean(cognitive_change_binary == 1)
  )
print("总体认知变化分布:")
print(overall_binary_stats)

# 按活动组别统计
group_binary_stats <- merged_data_binary %>%
  group_by(ipaq_activity_group) %>%
  summarise(
    n_total = n(),
    n_decrease = sum(cognitive_change_binary == 0),
    n_increase = sum(cognitive_change_binary == 1),
    prop_decrease = mean(cognitive_change_binary == 0),
    prop_increase = mean(cognitive_change_binary == 1),
    .groups = "drop"
  )
print("\n按IPAQ活动组别分组的认知变化分布:")
print(group_binary_stats)

# 交叉表
contingency_table <- table(
  merged_data_binary$ipaq_activity_group,
  merged_data_binary$cognitive_change_binary_label
)
print("\n交叉表（行=活动组别，列=认知变化）:")
print(contingency_table)
print("\n交叉表（百分比）:")
print(prop.table(contingency_table, margin = 1) * 100)

# 9.2 统计检验（卡方检验）
# ==========================================
print("\n===== 卡方检验 =====")
chi_square_test <- chisq.test(contingency_table)
print("卡方检验结果:")
print(chi_square_test)

# Fisher精确检验（如果样本量较小）
if (min(chi_square_test$expected) < 5) {
  print("\n注意：部分单元格期望频数 < 5，建议使用Fisher精确检验")
  fisher_test <- fisher.test(contingency_table)
  print("Fisher精确检验结果:")
  print(fisher_test)
}

# 9.3 可视化分析（二分类变量）
# ==========================================
print("\n正在生成二分类结局变量的可视化图表...")

# 图7: 堆叠条形图（显示各组别的提升/降低比例）
p7 <- ggplot(group_binary_stats, aes(x = ipaq_activity_group, y = prop_increase, fill = ipaq_activity_group)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_text(aes(label = paste0(round(prop_increase * 100, 1), "%")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.1)) +
  labs(
    title = "各组别认知提升的比例",
    x = "IPAQ活动组别",
    y = "认知提升比例",
    fill = "活动组别"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none"
  )

# 图8: 堆叠条形图（显示提升和降低的绝对数量）
p8 <- ggplot(merged_data_binary, aes(x = ipaq_activity_group, fill = cognitive_change_binary_label)) +
  geom_bar(position = "stack", alpha = 0.7) +
  geom_text(stat = "count", aes(label = after_stat(count)), 
            position = position_stack(vjust = 0.5), color = "white", fontface = "bold") +
  labs(
    title = "各组别认知变化的人数分布（堆叠条形图）",
    x = "IPAQ活动组别",
    y = "人数",
    fill = "认知变化"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

# 图9: 分组条形图（显示提升和降低的绝对数量）
p9 <- ggplot(merged_data_binary, aes(x = ipaq_activity_group, fill = cognitive_change_binary_label)) +
  geom_bar(position = "dodge", alpha = 0.7) +
  geom_text(stat = "count", aes(label = after_stat(count)), 
            position = position_dodge(width = 0.9), vjust = -0.5, fontface = "bold") +
  labs(
    title = "各组别认知变化的人数分布（分组条形图）",
    x = "IPAQ活动组别",
    y = "人数",
    fill = "认知变化"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

# 图10: 比例堆叠条形图（显示各组别的提升/降低比例）
# 准备比例数据用于标签
prop_data <- merged_data_binary %>%
  group_by(ipaq_activity_group, cognitive_change_binary_label) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(ipaq_activity_group) %>%
  mutate(
    total = sum(count),
    prop = count / total,
    prop_label = paste0(round(prop * 100, 1), "%"),
    # 计算累积比例用于定位标签
    cumsum_prop = cumsum(prop),
    y_pos = cumsum_prop - prop / 2
  ) %>%
  ungroup()

p10 <- ggplot(merged_data_binary, aes(x = ipaq_activity_group, fill = cognitive_change_binary_label)) +
  geom_bar(position = "fill", alpha = 0.7) +
  geom_text(data = prop_data, aes(y = y_pos, label = prop_label), 
            color = "white", fontface = "bold", size = 4) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "各组别认知变化的比例分布（堆叠比例图）",
    x = "IPAQ活动组别",
    y = "比例",
    fill = "认知变化"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

# 图11: 按组别分面的条形图
p11 <- ggplot(merged_data_binary, aes(x = cognitive_change_binary_label, fill = cognitive_change_binary_label)) +
  geom_bar(alpha = 0.7) +
  geom_text(stat = "count", aes(label = after_stat(count)), 
            vjust = -0.5, fontface = "bold") +
  facet_wrap(~ ipaq_activity_group, ncol = 3) +
  labs(
    title = "各组别认知变化的人数分布（分面图）",
    x = "认知变化",
    y = "人数",
    fill = "认知变化"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    strip.text = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(angle = 0)
  )

# 保存二分类变量的图表
ggsave(path_result("IPAQ_cognitive_binary_proportion.png"), p7, width = 10, height = 6, dpi = 300)
ggsave(path_result("IPAQ_cognitive_binary_stacked.png"), p8, width = 10, height = 6, dpi = 300)
ggsave(path_result("IPAQ_cognitive_binary_grouped.png"), p9, width = 10, height = 6, dpi = 300)
ggsave(path_result("IPAQ_cognitive_binary_proportion_stacked.png"), p10, width = 10, height = 6, dpi = 300)
ggsave(path_result("IPAQ_cognitive_binary_facet.png"), p11, width = 12, height = 5, dpi = 300)

print("二分类变量的图表已保存到result文件夹")

# 9.4 模型选择建议（二分类结局）
# ==========================================
print("\n===== 二分类结局变量的模型选择建议 =====")

print("结局变量类型: 二分类变量（0=降低，1=提升）")
print(paste("各活动组别样本量:", paste(group_binary_stats$ipaq_activity_group, "=", group_binary_stats$n_total, collapse = ", ")))

# 检查样本量是否足够
min_cell_count <- min(chi_square_test$expected)
if (min_cell_count < 5) {
  print(paste("\n注意：交叉表中最小期望频数为", round(min_cell_count, 2), "，小于5"))
  print("  - 建议使用Fisher精确检验")
  print("  - 或考虑合并某些组别")
} else {
  print(paste("\n交叉表中最小期望频数为", round(min_cell_count, 2), "，>= 5"))
  print("  - 卡方检验结果可靠")
}

print("\n推荐模型: 逻辑回归模型 (Logistic Regression)")
print("  - 适用条件：二分类结局变量")
print("  - 可以使用多项逻辑回归（Multinomial Logistic Regression）")
print("  - 或使用有序逻辑回归（如果组别有顺序关系）")
print("  - 也可以使用广义线性模型（GLM）with binomial family")

if (chi_square_test$p.value < 0.05) {
  print(paste("\n统计检验结果: 各组别间认知提升比例存在显著差异 (p =", 
              format(chi_square_test$p.value, scientific = TRUE), ")"))
} else {
  print(paste("\n统计检验结果: 各组别间认知提升比例无显著差异 (p =", 
              format(chi_square_test$p.value, scientific = TRUE), ")"))
}

print("\n===== 二分类结局变量的EDA分析完成 =====")
print("\n===== 全部EDA分析完成 =====")
