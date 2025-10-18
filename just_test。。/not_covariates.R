# 1. 准备工作 (不变)
# ==========================================
library(dplyr)
library(survival)
# ... (确保所有原始数据框已加载) ...


# 2. 最终数据整合 (最终修正版)
# ==========================================

# 步骤 A: 清理并准备独立的自变量数据框 (不变)
regularity_score_clean <- regularity_score %>%
  select(Participant_ID = participantid, regularity_score)
cognitive_score_clean <- cognitive_scores %>%
  select(Participant_ID, cognitive_score_0) # 我们暂时不用它，但先准备好

# ===================================================================
# === 关键修正：我们只定义一个“核心模型”所需的变量列表 ===
# ===================================================================
# 步骤 B: 定义“核心模型”需要的所有变量
core_model_vars <- c(
  "Participant_ID", 
  "survival_time_days", "dementia_status", # 结局变量
  "regularity_score", # 核心自变量
  "age_recruitment", "sex", "cvd_history_any" # 核心协变量
)

# 步骤 C: 合并所有数据，并只根据“核心模型”的变量来筛选
full_merged_table <- final_dataset %>%
  left_join(baseline_table_step1, by = "Participant_ID") %>%
  left_join(regularity_score_clean, by = "Participant_ID")
# 注意：我们暂时不合并 cognitive_score_clean，因为它缺失太多

# 创建最终的核心分析数据集
final_analytical_table_core <- full_merged_table %>%
  # 1. 只选择“核心模型”需要的变量
  select(all_of(core_model_vars)) %>%
  # 2. 然后，只对这个精简的子集进行缺失值移除
  na.omit()


# 3. 构建Cox回归模型 (现在将在一个更大的数据集上运行)
# ==========================================

# 步骤 D: 定义核心协变量列表 (现在它和筛选步骤完全同步了)
core_covariates <- c(
  "age_recruitment", "sex", "cvd_history_any"
)

# 步骤 E: 构建模型一 (单因素模型)
formula_unadjusted <- as.formula(
  paste("Surv(survival_time_days, dementia_status) ~ regularity_score")
)
# 注意：我们在新的、更大的数据集上运行
cox_model_unadjusted <- coxph(formula_unadjusted, data = final_analytical_table_core)


# 步骤 F: 构建模型二 (核心调整模型)
formula_adjusted_core <- as.formula(
  paste("Surv(survival_time_days, dementia_status) ~ regularity_score +", 
        paste(core_covariates, collapse = " + "))
)
# 注意：我们也在新的、更大的数据集上运行
cox_model_adjusted_core <- coxph(formula_adjusted_core, data = final_analytical_table_core)


# 4. 展示与解读结果
# ==========================================
print(paste("最终进入“核心模型”的样本量为:", nrow(final_analytical_table_core), "人"))
print("======================================================")
print("模型一：单因素Cox回归分析结果 (大样本)")
print(summary(cox_model_unadjusted))

print("======================================================")
print("模型二：核心调整Cox回归分析结果 (大样本)")
print(summary(cox_model_adjusted_core))