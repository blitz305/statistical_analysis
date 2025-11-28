# 1. 准备工作
# ==========================================
# 确保已加载 dplyr 包
library(dplyr)

# 确保我们之前的两个成果数据框 `dementia_results_final` 和 `final_death_summary` 
# 已经加载到您的R环境中。

# 2. 核心逻辑：直接合并与条件更新
# ==========================================

# 为了让合并更清晰，我们先从死亡数据中选取我们需要的列
death_info_to_join <- final_death_summary %>%
  select(
    Participant_ID, 
    date_of_death, 
    dementia_related_death, 
    matched_dementia_codes_death = matched_dementia_codes # 重命名以免混淆
  )

# 将死亡信息左连接到我们的主数据框，并进行条件更新
fully_updated_results <- dementia_results_final %>%
  left_join(death_info_to_join, by = "Participant_ID") %>%
  
  # 使用 case_when 进行更清晰的条件判断和更新
  mutate(
    # 更新 first_dementia_date:
    # 当“住院诊断”为0 且“死亡诊断”为1时 (即冲突案例)，将首次诊断日期更新为死亡日期
    # 否则，保留原来的首次诊断日期
    first_dementia_date = case_when(
      dementia_status == 0 & dementia_related_death == 1 ~ date_of_death,
      TRUE ~ first_dementia_date
    ),
    
    # 更新 dementia_status:
    # 当“住院诊断”为0 且“死亡诊断”为1时 (即冲突案例)，将状态更新为1
    # 否则，保留原来的状态
    dementia_status = case_when(
      dementia_status == 0 & dementia_related_death == 1 ~ 1,
      TRUE ~ dementia_status
    )
  ) %>%
  # 为了整洁，我们可以移除用于判断的 dementia_related_death 辅助列
  # 如果您想保留它以供检查，可以注释掉下面这一行
  select(-dementia_related_death)


# 3. 查看结果
# ==========================================
print("更新后的最终数据框 (表头):")
print(head(fully_updated_results))


# 我们可以再次用一个冲突案例的ID来验证结果是否符合您的例子
# 首先需要找出至少一个冲突案例
conflict_check_id <- final_death_summary %>%
  filter(dementia_related_death == 1) %>%
  inner_join(
    dementia_results_final %>% filter(dementia_status == 0),
    by = "Participant_ID"
  ) %>%
  # 拉取第一个冲突ID
  pull(Participant_ID) %>%
  .[1]

if (!is.na(conflict_check_id)) {
  print("======================================================")
  print(paste("以冲突案例 ID:", conflict_check_id, "为例，验证最终结果："))
  print("可以看到'first_dementia_date'和'date_of_death'已统一为死亡日期，'dementia_status'已更新为1")
  fully_updated_results %>% filter(Participant_ID == conflict_check_id) %>% print()
} else {
  print("数据中没有找到需要更新的冲突案例。")
}