
# ==========================================
library(dplyr)
library(survival)

# 确保所有原始数据框已加载
# ...


# 2. 最终数据整合与缺失值审计 (已优化)
# ==========================================

# 步骤 A: 清理并准备独立的自变量数据框 (不变)
regularity_score_clean <- regularity_score %>%
  select(Participant_ID = participantid, regularity_score)
cognitive_score_clean <- cognitive_scores %>%
  select(Participant_ID, cognitive_score_0)

# 步骤 B: 定义模型需要的所有变量 (不变)
all_model_vars <- c(
  "Participant_ID", "survival_time_days", "dementia_status",
  "regularity_score", "age_recruitment", "sex", "townsend_index", "bmi", 
  "smoking_status", "alcohol_status", "education_level", "cvd_history_any", 
  "history_diabetes"
)

# 步骤 C: 合并所有数据，得到一个包含所有潜在缺失值的完整表格
full_merged_table <- final_dataset %>%
  left_join(baseline_table_step1, by = "Participant_ID") %>%
  left_join(regularity_score_clean, by = "Participant_ID") %>%
  left_join(cognitive_score_clean, by = "Participant_ID")


# ===================================================================
# === 新增功能：缺失值审计 ===
# ===================================================================

# 1. 先创建一个只包含模型所需变量的临时表
model_vars_table <- full_merged_table %>%
  select(all_of(all_model_vars))

# 2. 创建最终的分析数据集 (移除了含有NA的行)
final_analytical_table <- na.omit(model_vars_table)

# 3. 找出被移除的参与者
excluded_ids <- setdiff(model_vars_table$Participant_ID, final_analytical_table$Participant_ID)
excluded_table <- model_vars_table %>% filter(Participant_ID %in% excluded_ids)

# 4. 生成并打印审计报告
total_before <- nrow(model_vars_table)
total_after <- nrow(final_analytical_table)
total_excluded <- total_before - total_after
dementia_cases_excluded <- sum(excluded_table$dementia_status == 1, na.rm = TRUE)

print("===== 缺失值审计报告 =====")
print(paste("移除缺失值前，总样本量为:", total_before))
print(paste("移除缺失值后，最终分析样本量为:", total_after))
print(paste("因协变量缺失共剔除了:", total_excluded, "个样本"))
print(paste("在被剔除的样本中，有", dementia_cases_excluded, "个是痴呆结局(dementia_status = 1)的样本。"))
print("============================")


# 进阶建议：找出导致样本丢失的“罪魁祸首”
# 这部分代码会计算在被剔除的样本中，每个变量的缺失率
missing_summary <- excluded_table %>%
  select(-Participant_ID) %>% # 不计算ID的缺失
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  tidyr::pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
  mutate(missing_proportion = missing_count / total_excluded) %>%
  filter(missing_count > 0) %>%
  arrange(desc(missing_count))

print("在被剔除的样本中，各变量的缺失情况如下：")
print(missing_summary)
print("============================")




# 3. 构建Cox回归模型
# ==========================================

# 步骤 C: 定义我们的协变量列表
# 完全按照您的指示，包含了所有需要的变量
covariates <- c(
  "age_recruitment", "sex", "townsend_index", "bmi", "smoking_status",
  "alcohol_status", "education_level", "cvd_history_any", 
  "history_diabetes"
)

# 步骤 D: 构建模型一 (单因素模型)
# ~ 后面的 . 代表公式右侧的所有变量
# 我们先构建一个包含所有变量的公式
formula_unadjusted <- as.formula(
  paste("Surv(survival_time_days, dementia_status) ~ regularity_score")
)

cox_model_unadjusted <- coxph(formula_unadjusted, data = final_analytical_table)


# 步骤 E: 构建模型二 (多因素调整模型)
# 我们将核心自变量和所有协变量组合在一起
formula_adjusted <- as.formula(
  paste("Surv(survival_time_days, dementia_status) ~ regularity_score +", 
        paste(covariates, collapse = " + "))
)

cox_model_adjusted <- coxph(formula_adjusted, data = final_analytical_table)


# 4. 展示与解读结果
# ==========================================
print("======================================================")
print("模型一：单因素Cox回归分析结果")
print(summary(cox_model_unadjusted))

print("======================================================")
print("模型二：多因素调整Cox回归分析结果")
print(summary(cox_model_adjusted))