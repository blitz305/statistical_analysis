# 1. 准备工作
# ==========================================
# 加载 data.table 包
library(data.table)

# 确保 dementia_results_final 和 final_death_summary 已经加载
# ...

# 2. 转换为 data.table 对象
# ==========================================
setDT(dementia_results_final)
setDT(final_death_summary)


# 3. 核心逻辑 (与之前相同)
# ==========================================

# 步骤 A: 重命名 final_death_summary 中的列
setnames(final_death_summary, "matched_dementia_codes", "matched_dementia_codes_death")

# 步骤 B: 使用 merge 执行左连接
Death_and_dementia <- merge(
  dementia_results_final,
  final_death_summary,
  by = "Participant_ID",
  all.x = TRUE
)

# 步骤 C: 找出需要更新的“冲突”行
conflict_rows <- Death_and_dementia[dementia_status == 0 & dementia_related_death == 1, which = TRUE]

# 步骤 D: 更新这些冲突行的数据
Death_and_dementia[conflict_rows, `:=` (
  dementia_status = 1,
  first_dementia_date = date_of_death
)]


# 4. 清理并查看结果 (已修正)
# ==========================================

# ===================================================================
# === 最终修正：只移除临时的辅助判断列，保留我们需要的死亡编码列 ===
# ===================================================================
Death_and_dementia[, dementia_related_death := NULL]


# 查看最终结果的表头
print("最终成品数据框 (表头):")
print(head(Death_and_dementia))

# 再次验证一个冲突案例
if (length(conflict_rows) > 0) {
  print("======================================================")
  print(paste("以第一个冲突案例 ID:", Death_and_dementia[conflict_rows[1], Participant_ID], "为例，验证最终结果："))
  print("现在可以看到新增了 `matched_dementia_codes_death` 这一列")
  print(Death_and_dementia[conflict_rows[1], ])
}