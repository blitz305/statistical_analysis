# 痴呆结局与认知变化的重叠分析
# ==========================================
# 描述: 分析痴呆结局和认知变化两个结局变量的重叠情况
#       查看痴呆结局的人里面，他们的认知变化是怎么样的

# 1. 准备工作
# ==========================================
library(dplyr)
library(ggplot2)
library(VennDiagram)
library(grid)
source("paths.R")

# 2. 数据准备
# ==========================================
print("正在准备数据...")

# 检查必要的数据是否存在
required_data <- c("final_dataset", "cognitive_scores")
missing_data <- required_data[!sapply(required_data, exists)]

if (length(missing_data) > 0) {
  stop(paste("缺少必要的数据对象:", paste(missing_data, collapse = ", "), 
             "\n请先运行相应的数据准备脚本"))
}

# 步骤 A: 提取两个结局变量的数据
print("步骤 A: 正在提取两个结局变量的数据...")

# 提取痴呆结局数据（来自final_dataset）
dementia_data <- final_dataset %>%
  select(
    Participant_ID,
    dementia_status,
    survival_time_days
  ) %>%
  filter(!is.na(dementia_status))

# 提取认知变化数据（来自cognitive_scores）
cognitive_data <- cognitive_scores %>%
  select(
    Participant_ID,
    cognitive_change,
    cognitive_score_0
  ) %>%
  filter(!is.na(cognitive_change))

print("步骤 A 完成。")

# 3. 样本量统计
# ==========================================
print("步骤 B: 正在统计样本量...")

# 统计痴呆结局的样本量
n_dementia_total <- nrow(dementia_data)
n_dementia_cases <- sum(dementia_data$dementia_status == 1, na.rm = TRUE)
n_dementia_non_cases <- sum(dementia_data$dementia_status == 0, na.rm = TRUE)

# 统计认知变化的样本量
n_cognitive_total <- nrow(cognitive_data)

# 合并前的诊断
print("\n===== 合并前数据诊断 =====")
print(paste("dementia_data中dementia_status=1的样本数:", sum(dementia_data$dementia_status == 1, na.rm = TRUE)))
print(paste("dementia_data中dementia_status=0的样本数:", sum(dementia_data$dementia_status == 0, na.rm = TRUE)))
print(paste("cognitive_data总样本数:", nrow(cognitive_data)))

# 检查dementia_status=1的样本是否在cognitive_data中
dementia_ids_1 <- dementia_data$Participant_ID[dementia_data$dementia_status == 1]
dementia_ids_1_in_cog <- intersect(dementia_ids_1, cognitive_data$Participant_ID)
print(paste("dementia_status=1的样本ID中，有多少在cognitive_data中:", length(dementia_ids_1_in_cog)))
print(paste("dementia_status=1的样本ID中，有多少不在cognitive_data中:", length(dementia_ids_1) - length(dementia_ids_1_in_cog)))
print("=========================================")

# 合并数据
merged_data <- dementia_data %>%
  inner_join(cognitive_data, by = "Participant_ID")

n_merged_total <- nrow(merged_data)
n_merged_dementia_cases <- sum(merged_data$dementia_status == 1, na.rm = TRUE)
n_merged_dementia_non_cases <- sum(merged_data$dementia_status == 0, na.rm = TRUE)

# 打印样本量统计
print("===== 样本量统计 =====")
print(paste("痴呆结局总样本数:", n_dementia_total))
print(paste("  - 痴呆病例数:", n_dementia_cases))
print(paste("  - 非痴呆病例数:", n_dementia_non_cases))
print(paste("认知变化总样本数:", n_cognitive_total))
print(paste("合并后总样本数:", n_merged_total))
print(paste("  - 合并后痴呆病例数:", n_merged_dementia_cases))
print(paste("  - 合并后非痴呆病例数:", n_merged_dementia_non_cases))
print("======================")

# 创建样本量统计表
sample_size_table <- data.frame(
  变量 = c("痴呆结局", "认知变化", "合并后"),
  总样本数 = c(n_dementia_total, n_cognitive_total, n_merged_total),
  痴呆病例数 = c(n_dementia_cases, NA, n_merged_dementia_cases),
  非痴呆病例数 = c(n_dementia_non_cases, NA, n_merged_dementia_non_cases)
)

print("\n样本量统计表:")
print(sample_size_table)

# 保存样本量统计表
write.csv(sample_size_table, 
          file = path_result("dementia_cognitive_overlap_sample_size.csv"), 
          row.names = FALSE)

# 4. 重叠分析 - Venn图
# ==========================================
print("步骤 C: 正在创建重叠分析图...")

# 获取有痴呆结局的ID
dementia_ids <- dementia_data$Participant_ID
# 获取有认知变化的ID
cognitive_ids <- cognitive_data$Participant_ID

# 计算重叠
both_ids <- intersect(dementia_ids, cognitive_ids)
only_dementia_ids <- setdiff(dementia_ids, cognitive_ids)
only_cognitive_ids <- setdiff(cognitive_ids, dementia_ids)

print(paste("同时有痴呆结局和认知变化的样本数:", length(both_ids)))
print(paste("只有痴呆结局的样本数:", length(only_dementia_ids)))
print(paste("只有认知变化的样本数:", length(only_cognitive_ids)))

# 创建Venn图
venn_data <- list(
  "痴呆结局" = dementia_ids,
  "认知变化" = cognitive_ids
)

# 保存Venn图
png(filename = path_result("dementia_cognitive_venn.png"), 
    width = 800, height = 600, res = 300)
venn.plot <- venn.diagram(
  venn_data,
  filename = NULL,
  fill = c("steelblue", "coral"),
  alpha = 0.5,
  cat.cex = 1.2,
  cex = 1.2,
  main = "痴呆结局与认知变化的重叠情况",
  main.cex = 1.3
)
grid.draw(venn.plot)
dev.off()

print("Venn图已保存到: dementia_cognitive_venn.png")

# 4.5. 验证：检查dementia_status=1的样本是否都有认知变化数据
# ==========================================
print("\n===== 验证：dementia_status=1的样本的认知变化数据情况 =====")

# 检查合并后数据中dementia_status=1的样本
dementia_cases_all <- merged_data %>%
  filter(dementia_status == 1)

print(paste("1. 合并后数据中dementia_status=1的总样本数:", nrow(dementia_cases_all)))

# 检查这些样本中有认知变化数据的数量
dementia_cases_with_cog <- dementia_cases_all %>%
  filter(!is.na(cognitive_change))

print(paste("2. dementia_status=1且认知变化数据不缺失的样本数:", nrow(dementia_cases_with_cog)))

# 检查缺失认知变化数据的数量
dementia_cases_missing_cog <- dementia_cases_all %>%
  filter(is.na(cognitive_change))

print(paste("3. dementia_status=1但认知变化数据缺失的样本数:", nrow(dementia_cases_missing_cog)))

# 计算缺失比例
if (nrow(dementia_cases_all) > 0) {
  missing_rate <- round(nrow(dementia_cases_missing_cog) / nrow(dementia_cases_all) * 100, 2)
  print(paste("4. 缺失率:", missing_rate, "%"))
} else {
  print("4. 缺失率: 无法计算（dementia_status=1的样本数为0）")
}

# 查看一些具体的ID示例（如果有缺失）
if (nrow(dementia_cases_missing_cog) > 0) {
  print("\n5. 部分缺失认知变化数据的dementia_status=1样本ID（前10个）:")
  print(head(dementia_cases_missing_cog$Participant_ID, 10))
} else {
  print("\n5. 所有dementia_status=1的样本都有认知变化数据！")
}

# 对比非痴呆病例的情况
dementia_non_cases_all <- merged_data %>%
  filter(dementia_status == 0)

print(paste("\n6. 对比：合并后数据中dementia_status=0的总样本数:", nrow(dementia_non_cases_all)))

dementia_non_cases_with_cog <- dementia_non_cases_all %>%
  filter(!is.na(cognitive_change))

print(paste("7. dementia_status=0且认知变化数据不缺失的样本数:", nrow(dementia_non_cases_with_cog)))

if (nrow(dementia_non_cases_all) > 0) {
  non_missing_rate <- round(nrow(dementia_non_cases_with_cog) / nrow(dementia_non_cases_all) * 100, 2)
  print(paste("8. dementia_status=0的样本中认知变化数据完整率:", non_missing_rate, "%"))
}

print("============================================================")

# 5. 痴呆病例的认知变化分析
# ==========================================
print("步骤 D: 正在分析痴呆病例的认知变化...")

# 分析合并数据中痴呆病例的认知变化
dementia_cases_cognitive <- merged_data %>%
  filter(dementia_status == 1)

dementia_non_cases_cognitive <- merged_data %>%
  filter(dementia_status == 0)

# 统计描述（添加安全检查，处理空数据情况）
cognitive_stats_dementia <- dementia_cases_cognitive %>%
  summarise(
    n = n(),
    mean_cognitive_change = if_else(n() > 0 && sum(!is.na(cognitive_change)) > 0, 
                                    mean(cognitive_change, na.rm = TRUE), NA_real_),
    sd_cognitive_change = if_else(n() > 0 && sum(!is.na(cognitive_change)) > 1, 
                                   sd(cognitive_change, na.rm = TRUE), NA_real_),
    median_cognitive_change = if_else(n() > 0 && sum(!is.na(cognitive_change)) > 0, 
                                       median(cognitive_change, na.rm = TRUE), NA_real_),
    q25_cognitive_change = if_else(n() > 0 && sum(!is.na(cognitive_change)) > 0, 
                                    quantile(cognitive_change, 0.25, na.rm = TRUE), NA_real_),
    q75_cognitive_change = if_else(n() > 0 && sum(!is.na(cognitive_change)) > 0, 
                                    quantile(cognitive_change, 0.75, na.rm = TRUE), NA_real_),
    min_cognitive_change = if_else(n() > 0 && sum(!is.na(cognitive_change)) > 0, 
                                    min(cognitive_change, na.rm = TRUE), NA_real_),
    max_cognitive_change = if_else(n() > 0 && sum(!is.na(cognitive_change)) > 0, 
                                    max(cognitive_change, na.rm = TRUE), NA_real_)
  ) %>%
  mutate(group = "痴呆病例")

cognitive_stats_non_dementia <- dementia_non_cases_cognitive %>%
  summarise(
    n = n(),
    mean_cognitive_change = if_else(n() > 0 && sum(!is.na(cognitive_change)) > 0, 
                                    mean(cognitive_change, na.rm = TRUE), NA_real_),
    sd_cognitive_change = if_else(n() > 0 && sum(!is.na(cognitive_change)) > 1, 
                                   sd(cognitive_change, na.rm = TRUE), NA_real_),
    median_cognitive_change = if_else(n() > 0 && sum(!is.na(cognitive_change)) > 0, 
                                      median(cognitive_change, na.rm = TRUE), NA_real_),
    q25_cognitive_change = if_else(n() > 0 && sum(!is.na(cognitive_change)) > 0, 
                                    quantile(cognitive_change, 0.25, na.rm = TRUE), NA_real_),
    q75_cognitive_change = if_else(n() > 0 && sum(!is.na(cognitive_change)) > 0, 
                                    quantile(cognitive_change, 0.75, na.rm = TRUE), NA_real_),
    min_cognitive_change = if_else(n() > 0 && sum(!is.na(cognitive_change)) > 0, 
                                    min(cognitive_change, na.rm = TRUE), NA_real_),
    max_cognitive_change = if_else(n() > 0 && sum(!is.na(cognitive_change)) > 0, 
                                    max(cognitive_change, na.rm = TRUE), NA_real_)
  ) %>%
  mutate(group = "非痴呆病例")

cognitive_stats_summary <- bind_rows(cognitive_stats_dementia, cognitive_stats_non_dementia)

print("\n===== 认知变化统计描述 =====")
print(cognitive_stats_summary)
print("============================")

# 保存统计描述表
write.csv(cognitive_stats_summary, 
          file = path_result("dementia_cognitive_change_stats.csv"), 
          row.names = FALSE)

# 6. 可视化分析
# ==========================================
print("步骤 E: 正在创建可视化图表...")

# 检查dementia_status的实际值
print("检查dementia_status的值:")
print(table(merged_data$dementia_status, useNA = "ifany"))

# 创建因子变量，确保正确处理
# 首先检查dementia_status的实际值
print("检查dementia_status的值:")
print(table(merged_data$dementia_status, useNA = "ifany"))

# 确保dementia_status的值是正确的
merged_data <- merged_data %>%
  mutate(
    dementia_status = as.integer(dementia_status))
  
# 创建因子变量
merged_data <- merged_data %>%
  mutate(
    dementia_status_factor = factor(
      dementia_status,
      levels = c(0, 1),
      labels = c("非痴呆", "痴呆")
    )
  )

# 过滤掉缺失值以确保绘图正常
plot_data <- merged_data %>%
  filter(!is.na(dementia_status) & !is.na(cognitive_change))

# 诊断：检查痴呆病例的认知变化数据情况
print("\n===== 数据诊断 =====")
print(paste("合并后总样本数:", n_merged_total))
print(paste("合并后痴呆病例数:", n_merged_dementia_cases))
print(paste("合并后非痴呆病例数:", n_merged_dementia_non_cases))
print(paste("绘图数据总样本数:", nrow(plot_data)))
print(paste("绘图数据中痴呆病例数:", sum(plot_data$dementia_status == 1, na.rm = TRUE)))
print(paste("绘图数据中非痴呆病例数:", sum(plot_data$dementia_status == 0, na.rm = TRUE)))

# 检查痴呆病例中缺失认知变化的比例
dementia_cases_with_cog <- merged_data %>%
  filter(dementia_status == 1 & !is.na(cognitive_change))
print(paste("痴呆病例中有认知变化数据的样本数:", nrow(dementia_cases_with_cog)))
print(paste("痴呆病例中缺失认知变化数据的样本数:", n_merged_dementia_cases - nrow(dementia_cases_with_cog)))
print("======================")

# 图1: 认知变化的分布对比（箱线图）
p1 <- ggplot(plot_data, aes(x = dementia_status_factor, 
                                y = cognitive_change, 
                                fill = dementia_status_factor)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_violin(alpha = 0.3, trim = FALSE) +
  scale_fill_manual(values = c("steelblue", "coral"), guide = "none") +
  labs(
    title = "痴呆结局与认知变化的分布对比",
    x = "痴呆状态",
    y = "认知变化",
    subtitle = paste0("痴呆病例: n=", n_merged_dementia_cases, 
                     ", 非痴呆病例: n=", n_merged_dementia_non_cases)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(
  filename = path_result("dementia_cognitive_boxplot.png"),
  plot = p1,
  width = 8,
  height = 6,
  dpi = 300
)

print("箱线图已保存到: dementia_cognitive_boxplot.png")

# 图2: 认知变化的直方图对比
p2 <- ggplot(plot_data, aes(x = cognitive_change, fill = dementia_status_factor)) +
  geom_histogram(alpha = 0.6, bins = 50, position = "identity") +
  scale_fill_manual(
    values = c("steelblue", "coral"),
    name = "痴呆状态"
  ) +
  labs(
    title = "痴呆病例与非痴呆病例的认知变化分布",
    x = "认知变化",
    y = "频数",
    subtitle = paste0("合并后总样本数: n=", n_merged_total)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title = element_text(size = 12),
    legend.position = "right"
  )

ggsave(
  filename = path_result("dementia_cognitive_histogram.png"),
  plot = p2,
  width = 10,
  height = 6,
  dpi = 300
)

print("直方图已保存到: dementia_cognitive_histogram.png")

# 图3: 认知变化的密度图对比
p3 <- ggplot(plot_data, aes(x = cognitive_change, color = dementia_status_factor)) +
  geom_density(size = 1.2, alpha = 0.7) +
  scale_color_manual(
    values = c("steelblue", "coral"),
    name = "痴呆状态"
  ) +
  labs(
    title = "痴呆病例与非痴呆病例的认知变化密度分布",
    x = "认知变化",
    y = "密度",
    subtitle = paste0("合并后总样本数: n=", n_merged_total)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title = element_text(size = 12),
    legend.position = "right"
  )

ggsave(
  filename = path_result("dementia_cognitive_density.png"),
  plot = p3,
  width = 10,
  height = 6,
  dpi = 300
)

print("密度图已保存到: dementia_cognitive_density.png")

# 7. 统计检验
# ==========================================
print("步骤 F: 正在进行统计检验...")

# 进行t检验（如果数据满足正态性）或Wilcoxon检验
# 先检查正态性（Shapiro-Wilk检验，样本量大时可能不适用）
if (n_merged_dementia_cases <= 5000 && n_merged_dementia_non_cases <= 5000) {
  shapiro_dementia <- shapiro.test(dementia_cases_cognitive$cognitive_change)
  shapiro_non_dementia <- shapiro.test(dementia_non_cases_cognitive$cognitive_change)
  
  print("\n正态性检验结果:")
  print(paste("痴呆病例Shapiro-Wilk检验 p值:", shapiro_dementia$p.value))
  print(paste("非痴呆病例Shapiro-Wilk检验 p值:", shapiro_non_dementia$p.value))
}

# 进行Wilcoxon秩和检验（非参数检验）
# 使用plot_data确保没有缺失值
wilcox_test <- wilcox.test(
  cognitive_change ~ dementia_status,
  data = plot_data
)

print("\n===== Wilcoxon秩和检验结果 =====")
print(wilcox_test)
print("================================")

# 进行t检验（参数检验）
# 使用plot_data确保没有缺失值
# 检查是否有足够的数据进行t检验
if (nrow(plot_data) > 0 && 
    sum(plot_data$dementia_status == 1, na.rm = TRUE) > 0 && 
    sum(plot_data$dementia_status == 0, na.rm = TRUE) > 0) {
  t_test <- t.test(
    cognitive_change ~ dementia_status,
    data = plot_data
  )
  
  print("\n===== t检验结果 =====")
  print(t_test)
  print("====================")
  
  # 保存检验结果
  test_results <- data.frame(
    检验方法 = c("Wilcoxon秩和检验", "t检验"),
    W统计量 = c(as.numeric(wilcox_test$statistic), NA),
    t统计量 = c(NA, as.numeric(t_test$statistic)),
    p值 = c(wilcox_test$p.value, t_test$p.value),
    痴呆病例均值 = c(NA, t_test$estimate[1]),
    非痴呆病例均值 = c(NA, t_test$estimate[2])
  )
} else {
  print("\n警告: 数据不足，无法进行t检验")
  t_test <- NULL
  test_results <- data.frame(
    检验方法 = c("Wilcoxon秩和检验", "t检验"),
    W统计量 = c(as.numeric(wilcox_test$statistic), NA),
    t统计量 = c(NA, NA),
    p值 = c(wilcox_test$p.value, NA),
    痴呆病例均值 = c(NA, NA),
    非痴呆病例均值 = c(NA, NA)
  )
}

write.csv(test_results, 
          file = path_result("dementia_cognitive_statistical_tests.csv"), 
          row.names = FALSE)

# 8. 总结报告
# ==========================================
print("\n===== 分析总结 =====")
print(paste("1. 痴呆结局样本数:", n_dementia_total, 
            "(病例:", n_dementia_cases, ", 非病例:", n_dementia_non_cases, ")"))
print(paste("2. 认知变化样本数:", n_cognitive_total))
print(paste("3. 合并后样本数:", n_merged_total, 
            "(病例:", n_merged_dementia_cases, ", 非病例:", n_merged_dementia_non_cases, ")"))
print(paste("4. 重叠样本数:", length(both_ids)))
print(paste("5. 痴呆病例的平均认知变化:", 
            round(cognitive_stats_dementia$mean_cognitive_change, 3)))
print(paste("6. 非痴呆病例的平均认知变化:", 
            round(cognitive_stats_non_dementia$mean_cognitive_change, 3)))
print(paste("7. 统计检验p值 (Wilcoxon):", round(wilcox_test$p.value, 6)))
if (!is.null(t_test)) {
  print(paste("8. 统计检验p值 (t检验):", round(t_test$p.value, 6)))
} else {
  print("8. 统计检验p值 (t检验): 数据不足，无法计算")
}
print("======================")

print("\n所有结果已保存到result文件夹中。")

