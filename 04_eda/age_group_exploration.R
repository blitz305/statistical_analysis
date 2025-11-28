# 年龄分组探索分析 - MVPA与认知变化
# ==========================================
# 目标：探索两种年龄分组方式下的数据分布情况，为后续分层分析选择合适的分组方式
# 分组方式1：以65为界限（≤65 vs >65）
# 分组方式2：四分组（≤54, 55-64, 65-74, >75）

# 1. 准备工作
# ==========================================
library(dplyr)
library(ggplot2)
source("paths.R")

print("==========================================")
print("年龄分组探索分析开始")
print("==========================================")

# 2. 数据整合
# ==========================================

# 步骤 A: 清理并准备需要新加入的数据框，统一ID列名
print("\n步骤 A: 正在准备MVPA和认知得分数据...")
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
print("步骤 A 完成。")

# 步骤 B: 将所有数据源合并
print("\n步骤 B: 正在合并所有数据框...")
baseline_table_step1_clean <- baseline_table_step1 %>%
  select(-any_of(c("cognitive_score_0")))

full_merged_table <- final_dataset %>%
  left_join(baseline_table_step1_clean, by = "Participant_ID") %>%
  left_join(mvpa_clean, by = "Participant_ID") %>%
  left_join(cognitive_score_clean, by = "Participant_ID")

print(paste("合并后总样本数:", nrow(full_merged_table)))
print("步骤 B 完成。")

# 步骤 C: 剔除结局变量和暴露变量的缺失值
print("\n步骤 C: 正在剔除结局变量和暴露变量的缺失值...")
n_before <- nrow(full_merged_table)

# 剔除 cognitive_change 和 overall_mvpa_hours 的缺失值
analysis_data <- full_merged_table %>%
  filter(!is.na(cognitive_change) & !is.na(overall_mvpa_hours))

n_after <- nrow(analysis_data)
n_excluded <- n_before - n_after

print(paste("剔除前样本数:", n_before))
print(paste("剔除后样本数:", n_after))
print(paste("剔除样本数:", n_excluded))
print(paste("剔除比例:", round(n_excluded/n_before * 100, 2), "%"))
print("步骤 C 完成。")

# 3. 创建年龄分组
# ==========================================
print("\n步骤 D: 正在创建年龄分组...")

# 分组方式1：以65为界限
analysis_data <- analysis_data %>%
  mutate(
    age_group_65 = case_when(
      age_recruitment <= 65 ~ "≤65",
      age_recruitment > 65 ~ ">65",
      TRUE ~ NA_character_
    ),
    age_group_65 = factor(age_group_65, levels = c("≤65", ">65"))
  )

# 分组方式2：四分组
analysis_data <- analysis_data %>%
  mutate(
    age_group_4 = case_when(
      age_recruitment <= 54 ~ "≤54",
      age_recruitment >= 55 & age_recruitment <= 64 ~ "55-64",
      age_recruitment >= 65 & age_recruitment <= 74 ~ "65-74",
      age_recruitment > 74 ~ ">75",
      TRUE ~ NA_character_
    ),
    age_group_4 = factor(age_group_4, levels = c("≤54", "55-64", "65-74", ">75"))
  )

print("步骤 D 完成。")

# 4. 统计年龄分布 - 分组方式1（65分界）
# ==========================================
print("\n步骤 E: 正在统计分组方式1（65分界）的分布...")

# 计算各年龄组的描述性统计
age_group_65_stats <- analysis_data %>%
  group_by(age_group_65) %>%
  summarise(
    N = n(),
    Percentage = round(n() / nrow(analysis_data) * 100, 2),
    Age_Mean = round(mean(age_recruitment, na.rm = TRUE), 2),
    Age_SD = round(sd(age_recruitment, na.rm = TRUE), 2),
    Age_Median = round(median(age_recruitment, na.rm = TRUE), 2),
    Age_Q1 = round(quantile(age_recruitment, 0.25, na.rm = TRUE), 2),
    Age_Q3 = round(quantile(age_recruitment, 0.75, na.rm = TRUE), 2),
    Age_Min = round(min(age_recruitment, na.rm = TRUE), 2),
    Age_Max = round(max(age_recruitment, na.rm = TRUE), 2),
    MVPA_Mean = round(mean(overall_mvpa_hours, na.rm = TRUE), 3),
    MVPA_SD = round(sd(overall_mvpa_hours, na.rm = TRUE), 3),
    MVPA_Median = round(median(overall_mvpa_hours, na.rm = TRUE), 3),
    Cognitive_Change_Mean = round(mean(cognitive_change, na.rm = TRUE), 3),
    Cognitive_Change_SD = round(sd(cognitive_change, na.rm = TRUE), 3),
    Cognitive_Change_Median = round(median(cognitive_change, na.rm = TRUE), 3),
    .groups = 'drop'
  ) %>%
  ungroup()

# 添加总计行
total_row_65 <- data.frame(
  age_group_65 = "Total",
  N = nrow(analysis_data),
  Percentage = 100.00,
  Age_Mean = round(mean(analysis_data$age_recruitment, na.rm = TRUE), 2),
  Age_SD = round(sd(analysis_data$age_recruitment, na.rm = TRUE), 2),
  Age_Median = round(median(analysis_data$age_recruitment, na.rm = TRUE), 2),
  Age_Q1 = round(quantile(analysis_data$age_recruitment, 0.25, na.rm = TRUE), 2),
  Age_Q3 = round(quantile(analysis_data$age_recruitment, 0.75, na.rm = TRUE), 2),
  Age_Min = round(min(analysis_data$age_recruitment, na.rm = TRUE), 2),
  Age_Max = round(max(analysis_data$age_recruitment, na.rm = TRUE), 2),
  MVPA_Mean = round(mean(analysis_data$overall_mvpa_hours, na.rm = TRUE), 3),
  MVPA_SD = round(sd(analysis_data$overall_mvpa_hours, na.rm = TRUE), 3),
  MVPA_Median = round(median(analysis_data$overall_mvpa_hours, na.rm = TRUE), 3),
  Cognitive_Change_Mean = round(mean(analysis_data$cognitive_change, na.rm = TRUE), 3),
  Cognitive_Change_SD = round(sd(analysis_data$cognitive_change, na.rm = TRUE), 3),
  Cognitive_Change_Median = round(median(analysis_data$cognitive_change, na.rm = TRUE), 3)
)

age_group_65_stats <- bind_rows(age_group_65_stats, total_row_65)

print("\n分组方式1（65分界）统计结果:")
print(age_group_65_stats)

# 5. 统计年龄分布 - 分组方式2（四分组）
# ==========================================
print("\n步骤 F: 正在统计分组方式2（四分组）的分布...")

# 计算各年龄组的描述性统计
age_group_4_stats <- analysis_data %>%
  group_by(age_group_4) %>%
  summarise(
    N = n(),
    Percentage = round(n() / nrow(analysis_data) * 100, 2),
    Age_Mean = round(mean(age_recruitment, na.rm = TRUE), 2),
    Age_SD = round(sd(age_recruitment, na.rm = TRUE), 2),
    Age_Median = round(median(age_recruitment, na.rm = TRUE), 2),
    Age_Q1 = round(quantile(age_recruitment, 0.25, na.rm = TRUE), 2),
    Age_Q3 = round(quantile(age_recruitment, 0.75, na.rm = TRUE), 2),
    Age_Min = round(min(age_recruitment, na.rm = TRUE), 2),
    Age_Max = round(max(age_recruitment, na.rm = TRUE), 2),
    MVPA_Mean = round(mean(overall_mvpa_hours, na.rm = TRUE), 3),
    MVPA_SD = round(sd(overall_mvpa_hours, na.rm = TRUE), 3),
    MVPA_Median = round(median(overall_mvpa_hours, na.rm = TRUE), 3),
    Cognitive_Change_Mean = round(mean(cognitive_change, na.rm = TRUE), 3),
    Cognitive_Change_SD = round(sd(cognitive_change, na.rm = TRUE), 3),
    Cognitive_Change_Median = round(median(cognitive_change, na.rm = TRUE), 3),
    .groups = 'drop'
  ) %>%
  ungroup()

# 添加总计行
total_row_4 <- data.frame(
  age_group_4 = "Total",
  N = nrow(analysis_data),
  Percentage = 100.00,
  Age_Mean = round(mean(analysis_data$age_recruitment, na.rm = TRUE), 2),
  Age_SD = round(sd(analysis_data$age_recruitment, na.rm = TRUE), 2),
  Age_Median = round(median(analysis_data$age_recruitment, na.rm = TRUE), 2),
  Age_Q1 = round(quantile(analysis_data$age_recruitment, 0.25, na.rm = TRUE), 2),
  Age_Q3 = round(quantile(analysis_data$age_recruitment, 0.75, na.rm = TRUE), 2),
  Age_Min = round(min(analysis_data$age_recruitment, na.rm = TRUE), 2),
  Age_Max = round(max(analysis_data$age_recruitment, na.rm = TRUE), 2),
  MVPA_Mean = round(mean(analysis_data$overall_mvpa_hours, na.rm = TRUE), 3),
  MVPA_SD = round(sd(analysis_data$overall_mvpa_hours, na.rm = TRUE), 3),
  MVPA_Median = round(median(analysis_data$overall_mvpa_hours, na.rm = TRUE), 3),
  Cognitive_Change_Mean = round(mean(analysis_data$cognitive_change, na.rm = TRUE), 3),
  Cognitive_Change_SD = round(sd(analysis_data$cognitive_change, na.rm = TRUE), 3),
  Cognitive_Change_Median = round(median(analysis_data$cognitive_change, na.rm = TRUE), 3)
)

age_group_4_stats <- bind_rows(age_group_4_stats, total_row_4)

print("\n分组方式2（四分组）统计结果:")
print(age_group_4_stats)

# 6. 保存结果
# ==========================================
print("\n步骤 G: 正在保存结果...")

# 保存分组方式1的统计表
write.csv(
  age_group_65_stats, 
  file = path_result("age_group_65_distribution.csv"), 
  row.names = FALSE
)
print(paste("分组方式1统计表已保存到:", path_result("age_group_65_distribution.csv")))

# 保存分组方式2的统计表
write.csv(
  age_group_4_stats, 
  file = path_result("age_group_4_distribution.csv"), 
  row.names = FALSE
)
print(paste("分组方式2统计表已保存到:", path_result("age_group_4_distribution.csv")))

print("步骤 G 完成。")

# 7. 总结
# ==========================================
print("\n==========================================")
print("年龄分组探索分析完成")
print("==========================================")
print("\n关键信息总结:")
print(paste("- 最终分析样本数:", n_after))
print(paste("- 分组方式1（65分界）各组的样本数:"))
print(table(analysis_data$age_group_65))
print(paste("- 分组方式2（四分组）各组的样本数:"))
print(table(analysis_data$age_group_4))
print("\n详细统计结果已保存到result文件夹中。")

