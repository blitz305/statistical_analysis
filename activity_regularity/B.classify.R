# 1. 依赖加载
library(dplyr)
source("paths.R")

# 2. 定义分类函数
# 这个函数负责处理单个参与者的数据字符串
classify_activity_pattern <- function(daily_percent_string) {
  
  # --- 参数定义 ---
  MINS_IN_DAY <- 1440
  WEEKLY_GUIDELINE_MINS <- 150     # 指南推荐量 (分钟)
  CONCENTRATED_THRESHOLD <- 0.50  # 集中运动的贡献度阈值 (50%)
  
  # 将字符串解析为数值
  daily_percents <- as.numeric(unlist(strsplit(daily_percent_string, ",")))
  
  # 检查数据是否有效
  if (all(is.na(daily_percents))) {
    return(NA_character_)
  }
  
  # 将每日百分比转换为每日分钟数
  daily_minutes <- (daily_percents ) * MINS_IN_DAY
  total_weekly_minutes <- sum(daily_minutes, na.rm = TRUE)
  
  # 分类规则
  if (total_weekly_minutes < WEEKLY_GUIDELINE_MINS) {
    return("Inactive")
  } else {
    sorted_minutes <- sort(daily_minutes, decreasing = TRUE, na.last = TRUE)
    top_2_days_minutes <- sum(sorted_minutes[1:2], na.rm = TRUE)
    contribution_percent <- top_2_days_minutes / total_weekly_minutes
    
    if (contribution_percent >= CONCENTRATED_THRESHOLD) {
      return("Concentrated Active")
    } else {
      return("Regularly Active")
    }
  }
}

# 3. 应用到完整数据集并生成输出表
print("步骤 A: 正在根据每日MVPA字符串分类参与者...")

mvpa_daily_string <- Derived_accelerometry$"Moderate-Vigorous - Day average | Instance 0"
mvpa_activity_pattern <- vapply(
  mvpa_daily_string,
  classify_activity_pattern,
  character(1)
)

mvpa_classify <- tibble(
  participant_id = Derived_accelerometry$"Participant ID",
  mvpa_daily_string = mvpa_daily_string,
  mvpa_activity_pattern = mvpa_activity_pattern
)

# 4. 输出分布信息
print("===== MVPA 活动模式分布 =====")
pattern_summary <- mvpa_classify %>%
  count(mvpa_activity_pattern, name = "n") %>%
  mutate(percentage = round(n / sum(n) * 100, 2)) %>%
  arrange(desc(n))

print(pattern_summary)

# 5. 存储结果文件
#saveRDS(mvpa_classify, file = path_mid("mvpa_activity_pattern.rds"))
#write.csv(mvpa_classify, file = path_mid("mvpa_activity_pattern.csv"), row.names = FALSE)

#print("输出文件已保存: mvpa_activity_pattern.rds / .csv")
