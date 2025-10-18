library(dplyr)
# 1. 定义分类函数 (与您的版本几乎一样，只简化了返回值)
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
  
  # 定义时段并求和
  period_sums <- c(
    Morning = sum(hourly_values[7:12], na.rm = TRUE),
    Afternoon = sum(hourly_values[13:18], na.rm = TRUE),
    Evening = sum(hourly_values[19:24], na.rm = TRUE)
  )
  
  # 如果所有时段运动量都为0，也视为NA (或可以定义为'Inactive')
  if (sum(period_sums) == 0) return(NA_character_)
  
  # 找出最大值和对应的时段
  max_value <- max(period_sums, na.rm = TRUE)
  max_periods <- names(period_sums)[period_sums == max_value]
  
  # 根据结果返回分类
  if (length(max_periods) == 1) {
    return(paste(max_periods, "type"))
  } else {
    return("Balanced/Tied") # <-- 建议：将并列情况定义为一个新类别
  }
}

# 2. 应用函数并生成最终表格 (使用纯 dplyr 流程)
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


# 3. 汇总信息 (与您的版本类似)
# ==========================================
print("===== 分类结果分布 =====")
print(table(mvpa_time_pattern$activity_time_pattern, useNA = "ifany"))

# 4. 抽取主暴露表供后续分析使用
# ==========================================
main_time_pattern_table <- mvpa_time_pattern %>%
  select(Participant_ID, activity_time_pattern)