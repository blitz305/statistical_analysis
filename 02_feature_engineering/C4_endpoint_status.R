# 1. 准备工作
# ==========================================
# 加载 data.table 包
library(data.table)

# 确保我们之前的成果 Death_and_dementia 和原始的 data_lost_to_follow-up 
# 已经加载到您的R环境中。
lost_to_followup_df <- `data_lost_to_follow-up`

# 定义统一的研究截止日期
cutoff_date <- as.IDate("2023-10-01")


# 2. 转换为 data.table 对象
# ==========================================
setDT(Death_and_dementia)
setDT(lost_to_followup_df)


# 3. 核心逻辑：合并与分层更新
# ==========================================

# 步骤 A: 准备并合并失访数据 (这一步不再需要，因为我们不再使用个体失访日期)
# 我们只需要 Death_and_dementia 即可

# 步骤 B: 使用 data.table 的方式，按优先级顺序，分步创建终点列

# 1. 初始化：将所有人“乐观地”默认为在研究截止日期被删失 (状态0)
Death_and_dementia[, `:=` (
  Endpoint_Date = cutoff_date, 
  Endpoint_Status = 0
)]

# 2. 更新死亡者信息 (状态2) - 这会覆盖掉默认值，符合优先级
Death_and_dementia[!is.na(date_of_death), `:=` (
  Endpoint_Date = date_of_death,
  Endpoint_Status = 2
)]

# 3. 更新痴呆患者信息 (状态1) - 这是最高优先级，会覆盖之前的所有状态
Death_and_dementia[dementia_status == 1, `:=` (
  Endpoint_Date = first_dementia_date,
  Endpoint_Status = 1
)]


# 4. 查看结果
# ==========================================
print("最终生存数据框 (表头):")
print(head(Death_and_dementia))

print("======================================================")
print("最终结局状态分布统计:")
# table() 函数可以帮我们快速查看每个状态的人数
print(table(Death_and_dementia$Endpoint_Status))

outcome_variables <- Death_and_dementia

# (可选) 移除旧的变量名以保持环境整洁
rm(Death_and_dementia)
