# 1. 准备工作
# ==========================================
# 加载我们需要的包
library(dplyr)
library(tidyr) # 需要 separate 函数

# 确保原始的 Derived_accelerometry 数据框已经加载
# ...

# 2. 核心探索逻辑
# ==========================================

# 从原始数据框中提取出24小时数据
hourly_data <- Derived_accelerometry %>%
  select(
    Participant_ID = `Participant ID`,
    hourly_mvpa_str = `Moderate-Vigorous - Day hour average | Instance 0`
  ) %>%
  # 移除没有任何运动记录的行
  filter(!is.na(hourly_mvpa_str))

# 将24小时的字符串拆分成24个独立的列
# 我们需要先生成列名 "h1", "h2", ..., "h24"
hour_cols <- paste0("h", 1:24)
hourly_data_split <- hourly_data %>%
  separate(
    col = hourly_mvpa_str, 
    into = hour_cols, 
    sep = ",", 
    convert = TRUE # convert = TRUE 会自动将文本转换为数值
  )

# 计算每个时段的运动量
time_of_day_summary <- hourly_data_split %>%
  # 使用 rowwise() 让我们能对每一行进行操作
  rowwise() %>%
  mutate(
    # 上午 (06:00 - 11:59) -> 第7到12列
    morning_mvpa = sum(c_across(h7:h12), na.rm = TRUE),
    # 下午 (12:00 - 17:59) -> 第13到18列
    afternoon_mvpa = sum(c_across(h13:h18), na.rm = TRUE),
    # 晚上 (18:00 - 23:59) -> 第19到24列
    evening_mvpa = sum(c_across(h19:h24), na.rm = TRUE),
    
    # 计算三个时段的总量
    total_slot_mvpa = morning_mvpa + afternoon_mvpa + evening_mvpa
  ) %>%
  # ungroup() 结束行操作
  ungroup()


# 3. 执行并输出探索结果
# ==========================================

# 探索一：计算“运动集中度指数”并查看其分布
concentration_analysis <- time_of_day_summary %>%
  filter(total_slot_mvpa > 0) %>% # 只分析有运动的人
  rowwise() %>%
  mutate(
    max_slot_mvpa = max(morning_mvpa, afternoon_mvpa, evening_mvpa),
    concentration_ratio = max_slot_mvpa / total_slot_mvpa
  ) %>%
  ungroup()

print("===== 探索一：运动集中度指数的分布 =====")
print("（指数越高，代表运动越集中在某一个时段）")
print(summary(concentration_analysis$concentration_ratio))


# 探索二：计算简单的三分类人群分布
group_distribution <- time_of_day_summary %>%
  mutate(
    time_of_day_group = case_when(
      morning_mvpa > afternoon_mvpa & morning_mvpa > evening_mvpa ~ "Morning type",
      afternoon_mvpa > morning_mvpa & afternoon_mvpa > evening_mvpa ~ "Afternoon type",
      evening_mvpa > morning_mvpa & evening_mvpa > afternoon_mvpa ~ "Evening type",
      TRUE ~ "Balanced" # 对于运动量相等或总量为0的情况，暂时标记为均衡
    )
  )

print("===== 探索二：各运动时段偏好人群的数量分布 =====")
print(table(group_distribution$time_of_day_group))