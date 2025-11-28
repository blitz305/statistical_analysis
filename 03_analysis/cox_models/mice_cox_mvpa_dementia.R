# 1. 准备工作
# ==========================================
# 加载我们需要的包
library(dplyr)
library(survival)
library(mice)
source("paths.R")

# 2. 最终数据整合 (明确展示每一步)
# ==========================================

# 步骤 A: 清理并准备需要新加入的数据框，统一ID列名
print("步骤 A: 正在准备MVPA和认知得分数据...")
mvpa_clean <- Derived_accelerometry %>%
  transmute(
    Participant_ID = `Participant ID`,
    overall_mvpa_minutes = `Moderate-Vigorous - Overall average | Instance 0` * 24
    
  )

cognitive_score_clean <- cognitive_scores %>%
  select(
    Participant_ID,
    cognitive_score_0
  )
print("步骤 A 完成。")


# 步骤 B: 将所有数据源合并，创建包含所有潜在缺失值的最完整数据框
print("步骤 B: 正在合并所有数据框...")
baseline_table_step1_clean <- baseline_table_step1 %>%
  select(-any_of(c("cognitive_score_0")))
full_merged_table <- final_dataset %>%
  # 关键一步：合并所有协变量
  left_join(baseline_table_step1_clean, by = "Participant_ID") %>%
  # 合并MVPA数据
  left_join(mvpa_clean, by = "Participant_ID") %>%
  # 合并基线认知得分
  left_join(cognitive_score_clean, by = "Participant_ID")
print("步骤 B 完成，`full_merged_table` 已创建。")


# 3. 多重插补 (Impute)
# ==========================================

# 步骤 C: 定义我们最终模型需要的所有变量
all_model_vars <- c(
  "Participant_ID", "survival_time_days", "dementia_status",
  "overall_mvpa_minutes", "age_recruitment", "sex", "townsend_index", "bmi", 
  "smoking_status", "alcohol_status", "education_level", "cvd_history_any", 
  "history_diabetes", "cognitive_score_0"
)

# 步骤 D: 从完整数据中，只挑选出用于插补和模型分析的变量
imputation_data <- full_merged_table %>%
  select(all_of(all_model_vars))
imputation_data_for_mice <- imputation_data %>%
  select(-Participant_ID)

# 步骤 E: 执行多重插补 [警告：这一步可能非常耗时！]
print("步骤 E: 开始执行多重插补，请耐心等待...")
imputed_object <- mice(imputation_data_for_mice, m = 10, seed = 123, maxit = 5, printFlag = FALSE)
print("步骤 E 完成，多重插补已生成！")


# 4. 在每个插补数据集上运行分层分析 (Analyze)
# ==========================================

# 步骤 F: 定义我们三个模型的公式
formula_model0 <- as.formula("Surv(survival_time_days, dementia_status) ~ overall_mvpa_minutes")
formula_model1 <- as.formula("Surv(survival_time_days, dementia_status) ~ overall_mvpa_minutes + age_recruitment + sex")
all_covariates <- c(
  "age_recruitment", "sex", "townsend_index", "bmi", "smoking_status",
  "alcohol_status", "education_level", "cvd_history_any", 
  "history_diabetes", "cognitive_score_0"
)
formula_model2 <- as.formula(
  paste("Surv(survival_time_days, dementia_status) ~ overall_mvpa_minutes +", 
        paste(all_covariates, collapse = " + "))
)

# 步骤 G: 使用 with() 在每个插补数据集上运行这三个模型
print("步骤 G: 正在手动对10个插补数据集逐一运行Cox模型...")

# 创建空的列表来储存每次分析的结果
model0_list <- list()
model1_list <- list()
model2_list <- list()

for (i in 1:imputed_object$m) {
  # 使用 complete() 函数提取第 i 个被填补好的完整数据集
  # 这个函数会自动把原始的完整列(如survival_time_days)和被插补的列合并在一起
  completed_data <- complete(imputed_object, i)
  
  # 在这个完整的数据集上运行我们的三个模型
  model0_list[[i]] <- coxph(formula_model0, data = completed_data)
  model1_list[[i]] <- coxph(formula_model1, data = completed_data)
  model2_list[[i]] <- coxph(formula_model2, data = completed_data)
}
print("步骤 G 完成！")


# 5. 汇总所有分析结果 (Pool) 并展示
# ==========================================

# 步骤 H: 使用 pool() 函数分别汇总三个模型的结果
pooled_model0 <- pool(model0_list)
pooled_model1 <- pool(model1_list)
pooled_model2 <- pool(model2_list)

# 1. 准备工作
# ==========================================

# 2. 核心逻辑
# ==========================================
# tidy() 函数会自动提取 HR, CI 和 P值
# exponentiate = TRUE 是关键，它会直接给出风险比(HR)而不是log(HR)
model0_tidy <- tidy(pooled_model0, conf.int = TRUE, exponentiate = TRUE)
model1_tidy <- tidy(pooled_model1, conf.int = TRUE, exponentiate = TRUE)
model2_tidy <- tidy(pooled_model2, conf.int = TRUE, exponentiate = TRUE)

model0_tidy$model <- "Model 0 (Unadjusted)"
model1_tidy$model <- "Model 1 (Adjusted for Demographics)"
model2_tidy$model <- "Model 2 (Fully Adjusted)"

all_models_summary_cox_mvpa <- bind_rows(model0_tidy, model1_tidy, model2_tidy)

final_report_table_cox_mvpa <- all_models_summary_cox_mvpa %>%
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

print("===== 结果：overall_mvpa vs 痴呆风险 (Cox模型) =====")
print(as.data.frame(final_report_table_cox_mvpa))

write.csv(final_report_table_cox_mvpa, file = path_result("cox_model_mvpa_results.csv"), row.names = FALSE)

# 美化输出结果表格
# ==========================================
print("步骤 H: 正在美化输出结果表格...")
source("beautify_results.R")
beautify_model_results(
  path_result("cox_model_mvpa_results.csv"),
  path_result("cox_model_mvpa_results_beautified.png")
)
print("步骤 H 完成，美化表格已生成。")