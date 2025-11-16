# 探索 Mixed evening type 参与者在周内 MVPA 活动型态（B.classify）中的分布
# 该脚本独立运行：无需依赖已存在的中间对象，直接从 Derived_accelerometry 重新生成所需分类

# 1. 加载依赖 ---------------------------------------------------------------
library(dplyr)
library(stringr)

# 如果项目中使用 paths.R 管理输出路径，可按需取消注释
# source("paths.R")

if (!exists("Derived_accelerometry")) {
  stop("缺少数据框 `Derived_accelerometry`，请在执行脚本前加载。")
}


# 2. 复用 B 模块中的分类函数 -----------------------------------------------
# 2.1 24 小时分段分类（Morning/Afternoon/Evening/Mixed）
classify_mvpa_pattern <- function(hourly_percent_string) {
  if (is.na(hourly_percent_string) || hourly_percent_string == "") {
    return(NA_character_)
  }
  
  hourly_values <- suppressWarnings(as.numeric(unlist(strsplit(as.character(hourly_percent_string), ","))))
  if (length(hourly_values) != 24 || all(is.na(hourly_values))) {
    return(NA_character_)
  }
  
  total_mvpa <- sum(hourly_values, na.rm = TRUE)
  if (total_mvpa == 0) return(NA_character_)
  
  period_sums <- c(
    Morning = sum(hourly_values[6:12], na.rm = TRUE),      # 05:00-11:00
    Afternoon = sum(hourly_values[12:18], na.rm = TRUE),   # 11:00-17:00
    Evening = sum(hourly_values[18:24], na.rm = TRUE)      # 17:00-24:00
  )
  
  period_percentages <- period_sums / total_mvpa * 100
  dominant_periods <- names(period_percentages)[period_percentages > 50]
  
  if (length(dominant_periods) == 1) {
    paste(dominant_periods, "type")
  } else {
    "Mixed type"
  }
}

# 2.2 晚间细分类（17-20 / 20-22 / 22-24）
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
    paste(dominant_evening, "type")
  } else {
    "Mixed evening type"
  }
}

# 2.3 B.classify 中的周内聚集度分类
classify_activity_pattern <- function(daily_percent_string) {
  MINS_IN_DAY <- 1440
  WEEKLY_GUIDELINE_MINS <- 150
  CONCENTRATED_THRESHOLD <- 0.50
  
  daily_percents <- suppressWarnings(as.numeric(unlist(strsplit(daily_percent_string, ","))))
  if (all(is.na(daily_percents))) {
    return(NA_character_)
  }
  
  daily_minutes <- daily_percents * MINS_IN_DAY
  total_weekly_minutes <- sum(daily_minutes, na.rm = TRUE)
  
  if (total_weekly_minutes < WEEKLY_GUIDELINE_MINS) {
    "Inactive"
  } else {
    sorted_minutes <- sort(daily_minutes, decreasing = TRUE, na.last = TRUE)
    top_2_days_minutes <- sum(sorted_minutes[1:2], na.rm = TRUE)
    contribution_percent <- top_2_days_minutes / total_weekly_minutes
    
    if (contribution_percent >= CONCENTRATED_THRESHOLD) {
      "Concentrated Active"
    } else {
      "Regularly Active"
    }
  }
}


# 3. 生成 Evening 细分表 -----------------------------------------------------
print("步骤 1: 重新生成 mvpa_time_pattern_eveningnew ...")

mvpa_time_pattern_eveningnew <- Derived_accelerometry %>%
  select(
    Participant_ID = `Participant ID`,
    hourly_mvpa_string = `Moderate-Vigorous - Day hour average | Instance 0`
  ) %>%
  rowwise() %>%
  mutate(
    activity_time_pattern = classify_mvpa_pattern(hourly_mvpa_string),
    evening_subpattern = if (!is.na(activity_time_pattern) && activity_time_pattern == "Evening type") {
      classify_evening_subpattern(hourly_mvpa_string)
    } else {
      NA_character_
    }
  ) %>%
  ungroup()

mixed_evening_ids <- mvpa_time_pattern_eveningnew %>%
  filter(
    activity_time_pattern == "Evening type",
    evening_subpattern == "Mixed evening type"
  ) %>%
  distinct(Participant_ID)

print(paste("Mixed evening type 人数:", nrow(mixed_evening_ids)))
if (nrow(mixed_evening_ids) == 0) {
  stop("当前数据集中无 Mixed evening type 参与者，终止分析。")
}


# 4. 生成 B.classify 周内聚集度分类 -----------------------------------------
print("步骤 2: 生成 mvpa_activity_pattern ...")

mvpa_classify <- Derived_accelerometry %>%
  transmute(
    participant_id = `Participant ID`,
    mvpa_daily_string = `Moderate-Vigorous - Day average | Instance 0`,
    mvpa_activity_pattern = classify_activity_pattern(mvpa_daily_string)
  )


# 5. 关联并输出分布 ---------------------------------------------------------
print("步骤 3: 统计 Mixed evening type 在 B.classify 分类中的分布 ...")

mixed_evening_profile <- mixed_evening_ids %>%
  left_join(mvpa_classify, by = c("Participant_ID" = "participant_id"))

profile_summary <- mixed_evening_profile %>%
  count(mvpa_activity_pattern, name = "n") %>%
  mutate(
    percentage = round(n / sum(n) * 100, 2)
  ) %>%
  arrange(desc(n))

print("===== Mixed evening type 对应的周内活动模式分布 =====")
print(profile_summary)

# 可选：保存结果，按需启用
# output_path <- path_result("mixed_evening_type_activity_profile.csv")
# write.csv(profile_summary, output_path, row.names = FALSE)
# print(paste("结果已保存到:", output_path))

