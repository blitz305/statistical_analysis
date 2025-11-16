# 1. 准备工作
# ==========================================
# 加载 data.table 包
library(data.table)

# 确保我们之前的最终成果 outcome_variables 和原始的 data_of_attend 
# 已经加载到您的R环境中。

# 2. 核心逻辑
# ==========================================

# 步骤 A: 准备入组时间数据
# 从 data_of_attend 中提取ID和我们确认好的第9列日期
recruitment_info <- data.table(
  # 注意：这里的 `data_of_attend[[1]]` 和 `data_of_attend[[9]]`
  # 是为了确保无论原始列名多么复杂，我们都能准确取到第1列和第9列
  Participant_ID = date_attend[[1]],
  recruitment_date_str = date_attend[[9]]
)

# 将日期字符串转换为 data.table 的 IDate 格式
recruitment_info[, recruitment_date := as.IDate(as.Date(recruitment_date_str, format = "%d/%m/%Y"))]

# 移除临时的字符串日期列
recruitment_info[, recruitment_date_str := NULL]


# 步骤 B: 将入组日期合并到我们的主数据框中
# 我们使用 merge 执行左连接
final_dataset <- merge(
  outcome_variables,
  recruitment_info,
  by = "Participant_ID",
  all.x = TRUE # 确保 outcome_variables 中的每一行都被保留
)

# 步骤 C: 计算生存时间（天）
# 生存时间 = 终点日期 - 入组日期
final_dataset[, survival_time_days := as.numeric(Endpoint_Date - recruitment_date)]
final_dataset <- final_dataset[survival_time_days >= 0]

# 3. 查看最终成果
# ==========================================
print("最终成品数据框 (表头):")
# 我们将列的顺序调整一下，方便查看
setcolorder(final_dataset, c("Participant_ID", "recruitment_date", "Endpoint_Date", "Endpoint_Status", "survival_time_days"))
print(head(final_dataset))

# 做一个简单的检查，看看是否有异常值
print("======================================================")
print("生存时间（天）的简要统计:")
print(summary(final_dataset$survival_time_days))