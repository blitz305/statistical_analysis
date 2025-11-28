# 1. 加载必要的包
# ==========================================
library(dplyr)

# 2. 定义分类函数 - 新版本：基于50%贡献度阈值
# ==========================================
classify_mvpa_pattern <- function(hourly_percent_string) {
  # 检查输入是否有效
  if (is.na(hourly_percent_string) || hourly_percent_string == "") {
    return(NA_character_)
  }
  
  # 稳健地解析字符串
  hourly_values <- suppressWarnings(as.numeric(unlist(strsplit(as.character(hourly_percent_string), ","))))
  if (length(hourly_values) != 24 || all(is.na(hourly_values))) {
    return(NA_character_)
  }
  
  # 计算24小时总MVPA
  total_mvpa <- sum(hourly_values, na.rm = TRUE)
  
  # 如果总MVPA为0，返回NA
  if (total_mvpa == 0) return(NA_character_)
  
  # 定义新的时段 (注意：R中索引从1开始，所以小时0对应索引1)
  # 早晨组：05:00-11:00 (索引6-12)
  # 中午-下午组：11:00-17:00 (索引12-18) 
  # 晚上组：17:00-24:00 (索引18-24)
  period_sums <- c(
    Morning = sum(hourly_values[6:12], na.rm = TRUE),      # 05:00-11:00
    Afternoon = sum(hourly_values[12:18], na.rm = TRUE),   # 11:00-17:00
    Evening = sum(hourly_values[18:24], na.rm = TRUE)      # 17:00-24:00
  )
  
  # 计算各时段贡献百分比
  period_percentages <- period_sums / total_mvpa * 100
  
  # 检查是否有任何时段贡献>50%
  dominant_periods <- names(period_percentages)[period_percentages > 50]
  
  # 根据结果返回分类
  if (length(dominant_periods) == 1) {
    return(paste(dominant_periods, "type"))
  } else {
    return("Mixed type") # 没有任何时段贡献>50%
  }
}

# 针对 Evening 组的细分函数（探索用）
classify_evening_subpattern <- function(hourly_percent_string) {
  if (is.na(hourly_percent_string) || hourly_percent_string == "") {
    return(NA_character_)
  }
  
  hourly_values <- suppressWarnings(as.numeric(unlist(strsplit(as.character(hourly_percent_string), ","))))
  if (length(hourly_values) != 24 || all(is.na(hourly_values))) {
    return(NA_character_)
  }
  
  evening_total <- sum(hourly_values[18:24], na.rm = TRUE)
  if (evening_total == 0) return(NA_character_)
  
  evening_sums <- c(
    Evening_17_20 = sum(hourly_values[18:20], na.rm = TRUE),
    Evening_20_22 = sum(hourly_values[21:22], na.rm = TRUE),
    Evening_22_24 = sum(hourly_values[23:24], na.rm = TRUE)
  )
  
  evening_percentages <- evening_sums / evening_total * 100
  dominant_evening <- names(evening_percentages)[evening_percentages > 50]
  
  if (length(dominant_evening) == 1) {
    return(paste(dominant_evening, "type"))
  } else {
    return("Mixed evening type")
  }
}

# 3. 应用函数并生成最终表格 (使用纯 dplyr 流程)
# ==========================================
print("步骤 A & B: 正在计算分类并生成最终表格...")

# 创建一个新的主表，并直接在上面操作
mvpa_time_pattern <- Derived_accelerometry %>%
  # 选择需要的列
  select(
    Participant_ID = `Participant ID`,
    hourly_mvpa_string = `Moderate-Vigorous - Day hour average | Instance 0`
  ) %>%
  # 使用 rowwise() 逐行应用我们的函数
  rowwise() %>%
  mutate(
    activity_time_pattern = classify_mvpa_pattern(hourly_mvpa_string)
  ) %>%
  # 结束逐行操作
  ungroup()

print("步骤 A & B 完成。")

# 4. 保留Mixed type，不转换为NA
# ==========================================
print("步骤 C: 保留Mixed type，不转换为NA...")
# 不需要转换，直接保留所有类型
print("步骤 C 完成。")

# 5. 汇总信息 - 显示四种类型的分布
# ==========================================
print("===== 四种运动时间模式分布 =====")
pattern_table <- table(mvpa_time_pattern$activity_time_pattern, useNA = "ifany")
print(pattern_table)

# 计算各类型的数量和百分比
pattern_summary <- mvpa_time_pattern %>%
  count(activity_time_pattern) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 2)
  ) %>%
  arrange(desc(n))

print("===== 详细统计信息 =====")
print(pattern_summary)

# 6. 重命名变量并抽取主暴露表供后续分析使用
# ==========================================
main_time_pattern_table <- mvpa_time_pattern %>%
  select(Participant_ID, activity_time_pattern)

# 7. Evening 细分探索表（不影响原流程）
# ==========================================
print("步骤 D: 针对 Evening type 进行更细分的探索...")

mvpa_time_pattern_eveningnew <- mvpa_time_pattern %>%
  rowwise() %>%
  mutate(
    evening_subpattern = if (!is.na(activity_time_pattern) && activity_time_pattern == "Evening type") {
      classify_evening_subpattern(hourly_mvpa_string)
    } else {
      activity_time_pattern
    }
  ) %>%
  ungroup() %>%
  select(Participant_ID, activity_time_pattern, evening_subpattern)

print("===== Evening 细分分布（仅限原 Evening type）=====")
evening_only <- mvpa_time_pattern_eveningnew %>%
  filter(activity_time_pattern == "Evening type")

if (nrow(evening_only) == 0) {
  print("没有 Evening type 个体可供细分探索。")
} else {
  evening_summary <- evening_only %>%
    count(evening_subpattern) %>%
    mutate(
      percentage = round(n / sum(n) * 100, 2)
    ) %>%
    arrange(desc(n))
  print(evening_summary)
}

print("步骤 D 完成。")