# 1. 准备工作
# ==========================================

# 加载我们需要的包
library(dplyr)
library(tidyr)
library(lubridate) # 用于处理日期格式

dementia_codes <- c(
  "G30", "G300", "G301", "G308", "G309", "G310", "G311", 
  "G318", "F00", "F000", "F001", "F002", "F009", "F01", 
  "F010", "F011", "F012", "F013", "F018", "F019", "F02", 
  "F020", "F021", "F022", "F023", "F024", "F028", "F03", 
  "F051", "F106", "A810", "I673"
)
# 步骤 A: 找出所有与痴呆相关的死亡记录，并列出匹配的代码
dementia_death_details <- Death_register %>%
  select(
    Participant_ID = `Participant ID`, 
    starts_with("Underlying (primary) cause"),
    starts_with("Contributory (secondary) causes")
  ) %>%
  pivot_longer(
    cols = -Participant_ID,
    names_to = "cause_column",
    values_to = "cause_code",
    values_drop_na = TRUE
  ) %>%
  filter(cause_code %in% dementia_codes) %>%
  # 按ID分组，将所有匹配到的痴呆代码合并到一个单元格
  group_by(Participant_ID) %>%
  summarise(
    # 使用 paste 将所有不重复的代码用 ", " 连接成一个字符串
    matched_dementia_codes = paste(sort(unique(cause_code)), collapse = ", ")
  )

# 步骤 B: 创建包含所有死者和正确日期格式的基础表，并合并所有信息
final_death_summary <- Death_register %>%
  # 选择ID和死亡日期列
  select(
    Participant_ID = `Participant ID`,
    date_of_death_str = `Date of death | Instance 0`
  ) %>%
  # ===================================================================
  # === 日期 DEBUG 修正: 使用 as.Date 并明确指定格式 ===
  # ===================================================================
  mutate(
    date_of_death = as.Date(date_of_death_str, format = "%d/%m/%Y")
  ) %>%
  # 将步骤A中生成的痴呆诊断详情，通过左连接合并进来
  left_join(dementia_death_details, by = "Participant_ID") %>%
  # 创建最终的标记列，并只保留我们需要的列
  mutate(
    dementia_related_death = ifelse(is.na(matched_dementia_codes), 0, 1)
  ) %>%
  select(Participant_ID, date_of_death, dementia_related_death, matched_dementia_codes)


# 查看最终结果
print(final_death_summary)