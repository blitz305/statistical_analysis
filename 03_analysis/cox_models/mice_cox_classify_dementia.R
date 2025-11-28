# 1. 准备工作
# ==========================================
# 加载我们需要的包
library(dplyr)
library(survival)
library(mice)
source("paths.R")

# 确保我们之前的所有成果都已加载:
# final_dataset, baseline_table_step1, regularity_score, cognitive_scores
# ...


# 2. 最终数据整合 (与之前相同，但为清晰起见保留)
# ==========================================

# 步骤 A: 清理并准备独立的自变量数据框
print("步骤 A: 正在准备运动得分和认知得分数据...")
regularity_score_clean <- regularity_score %>%
  select(Participant_ID = participantid, regularity_score)
cognitive_score_clean <- cognitive_scores %>%
  select(Participant_ID, cognitive_score_0)
print("步骤 A 完成。")

# 步骤 B: 将所有数据源合并
print("步骤 B: 正在合并所有数据框...")
baseline_table_step1_clean <- baseline_table_step1 %>%
  select(-any_of(c("cognitive_score_0")))
full_merged_table <- final_dataset %>%
  left_join(baseline_table_step1_clean, by = "Participant_ID") %>%
  left_join(regularity_score_clean, by = "Participant_ID") %>%
  left_join(cognitive_score_clean, by = "Participant_ID")
print("步骤 B 完成，`full_merged_table` 已创建。")


# 3. 多重插补 (Impute)
# ==========================================

# ===================================================================
# === 关键修改 1: 更新模型变量列表，用 exposure_group 替换 regularity_score ===
# ===================================================================
# 步骤 C: 定义我们最终模型需要的所有变量
all_model_vars_new <- c(
  "Participant_ID", "survival_time_days", "dementia_status",
  "exposure_group", # <-- 新的核心暴露变量
  "age_recruitment", "sex", "townsend_index", "bmi", 
  "smoking_status", "alcohol_status", "education_level", "cvd_history_any", 
  "history_diabetes", "cognitive_score_0"
)

# 步骤 D: 从完整数据中挑选用于插补的变量
imputation_data <- full_merged_table %>%
  select(all_of(all_model_vars_new))

# 在送入 mice 函数前，移除 Participant_ID
imputation_data_for_mice <- imputation_data %>%
  select(-Participant_ID)

# 步骤 E: 执行多重插补 (不变)
print("步骤 E: 开始执行多重插补，请耐心等待...")
imputed_object <- mice(imputation_data_for_mice, m = 10, seed = 123, maxit = 5, printFlag = FALSE)
print("步骤 E 完成，多重插补已生成！")


# 4. 在每个插补数据集上运行分层分析 (Analyze)
# ==========================================

# ===================================================================
# === 关键修改 2: 更新模型公式，使用新的暴露变量和参照组 ===
# ===================================================================
# 步骤 F: 定义我们三个模型的新公式

# 模型0: 单因素
formula_model0_new <- as.formula("Surv(survival_time_days, dementia_status) ~ exposure_group")

# 模型1: 调整人口学核心变量
formula_model1_new <- as.formula("Surv(survival_time_days, dementia_status) ~ exposure_group + age_recruitment + sex")

# 模型2: 完全调整模型
all_covariates <- c(
  "age_recruitment", "sex", "townsend_index", "bmi", "smoking_status",
  "alcohol_status", "education_level", "cvd_history_any", 
  "history_diabetes", "cognitive_score_0"
)
formula_model2_new <- as.formula(
  paste("Surv(survival_time_days, dementia_status) ~ exposure_group +", 
        paste(all_covariates, collapse = " + "))
)

# 步骤 G: 手动循环运行新模型 (已修正)
# ==========================================
print("步骤 G: 正在对插补数据集分别运行3个新模型...")
model0_list_new <- list()
model1_list_new <- list()
model2_list_new <- list()

for (i in 1:imputed_object$m) {
  # 提取第 i 个完整数据集
  completed_data <- complete(imputed_object, i)
  
  # ===================================================================
  # === 关键修正：在使用relevel前，先确保该列是因子类型 ===
  # ===================================================================
  # 1. 明确地将 exposure_group 转换为因子
  completed_data$exposure_group <- factor(completed_data$exposure_group)
  
  # 2. 现在可以安全地设置参照组了
  completed_data$exposure_group <- relevel(completed_data$exposure_group, ref = "Inactive")
  # ===================================================================
  
  # 在这个准备好的数据集上运行我们的三个模型
  model0_list_new[[i]] <- coxph(formula_model0_new, data = completed_data)
  model1_list_new[[i]] <- coxph(formula_model1_new, data = completed_data)
  model2_list_new[[i]] <- coxph(formula_model2_new, data = completed_data)
}
print("步骤 G 完成！")


# 5. 汇总所有分析结果 (Pool) 并展示
# ==========================================

# 步骤 H: 汇总新模型的结果
pooled_model0_new <- pool(model0_list_new)
pooled_model1_new <- pool(model1_list_new)
pooled_model2_new <- pool(model2_list_new)

# 1. 准备工作
# ==========================================


# 2. 核心逻辑
# ==========================================
model0_tidy <- tidy(pooled_model0_new, conf.int = TRUE, exponentiate = TRUE)
model1_tidy <- tidy(pooled_model1_new, conf.int = TRUE, exponentiate = TRUE)
model2_tidy <- tidy(pooled_model2_new, conf.int = TRUE, exponentiate = TRUE)

model0_tidy$model <- "Model 0 (Unadjusted)"
model1_tidy$model <- "Model 1 (Adjusted for Demographics)"
model2_tidy$model <- "Model 2 (Fully Adjusted)"

all_models_summary_cox_exp <- bind_rows(model0_tidy, model1_tidy, model2_tidy)

final_report_table_cox_exp <- all_models_summary_cox_exp %>%
  select(model, term, estimate, conf.low, conf.high, p.value) %>%
  mutate(
    across(c(estimate, conf.low, conf.high), ~round(.x, 3)),
    p.value = if_else(p.value < 0.001, "<0.001", as.character(round(p.value, 3)))
  ) %>%
  mutate(
    `HR (95% CI)` = paste0(estimate, " (", conf.low, " to ", conf.high, ")")
  ) %>%
  select(
    Model = model,
    Variable = term,
    `HR (95% CI)`,
    `P Value` = p.value
  )

# 3. 查看与导出
# ==========================================


write.csv(final_report_table_cox_exp, file = path_result("cox_model_group_results.csv"), row.names = FALSE)

# 美化输出结果表格
# ==========================================
print("步骤 H: 正在美化输出结果表格...")
source("beautify_results.R")
beautify_model_results(
  path_result("cox_model_group_results.csv"),
  path_result("cox_model_group_results_beautified.png")
)
print("步骤 H 完成，美化表格已生成。")
