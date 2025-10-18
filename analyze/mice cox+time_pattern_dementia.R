# 1. 准备工作
# ==========================================
# 加载我们需要的包
library(dplyr)
library(survival)
library(mice)
library(broom)
source("paths.R")

# 2. 最终数据整合 (明确展示每一步)
# ==========================================

# 步骤 A: 清理并准备需要新加入的数据框，统一ID列名
print("步骤 A: 正在准备MVPA时段模式和认知得分数据...")
activity_time_pattern_clean <- mvpa_time_pattern_new %>%
  select(
    Participant_ID,
    activity_time_pattern 
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
  left_join(baseline_table_step1_clean, by = "Participant_ID") %>%
  left_join(activity_time_pattern_clean, by = "Participant_ID") %>%
  left_join(cognitive_score_clean, by = "Participant_ID") %>%
  mutate(
    activity_time_pattern = factor(
      activity_time_pattern,
      levels = c("Morning type", "Afternoon type", "Evening type")
    )
  )
print("步骤 B 完成，`full_merged_table` 已创建。")


# 3. 多重插补 (Impute)
# ==========================================

# 步骤 C: 定义我们最终模型需要的所有变量
all_model_vars <- c(
  "Participant_ID", "survival_time_days", "dementia_status",
  "activity_time_pattern", "age_recruitment", "sex", "townsend_index", "bmi",
  "smoking_status", "alcohol_status", "education_level", "cvd_history_any",
  "history_diabetes", "cognitive_score_0"
)

# 步骤 D: 从完整数据中，只挑选出用于插补和模型分析的变量
imputation_data <- full_merged_table %>%
  select(all_of(all_model_vars))

imputation_data_for_mice <- imputation_data %>%
  mutate(
    activity_time_pattern = factor(
      activity_time_pattern,
      levels = c("Morning type", "Afternoon type", "Evening type")
    )
  ) %>%
  select(-Participant_ID)

# 步骤 E: 执行多重插补 [警告：这一步可能非常耗时！]
print("步骤 E: 开始执行多重插补，请耐心等待...")
imputed_object <- mice(imputation_data_for_mice, m = 10, seed = 123, maxit = 5, printFlag = FALSE)
print("步骤 E 完成，多重插补已生成！")


# 4. 在每个插补数据集上运行分层分析 (Analyze)
# ==========================================

# 步骤 F: 定义我们三个模型的公式
formula_model0 <- as.formula("Surv(survival_time_days, dementia_status) ~ activity_time_pattern")
formula_model1 <- as.formula("Surv(survival_time_days, dementia_status) ~ activity_time_pattern + age_recruitment + sex")
all_covariates <- c(
  "age_recruitment", "sex", "townsend_index", "bmi", "smoking_status",
  "alcohol_status", "education_level", "cvd_history_any",
  "history_diabetes", "cognitive_score_0"
)
formula_model2 <- as.formula(
  paste(
    "Surv(survival_time_days, dementia_status) ~ activity_time_pattern +",
    paste(all_covariates, collapse = " + ")
  )
)

# 步骤 G: 手动对每个插补数据集运行 Cox 模型
print("步骤 G: 正在对10个插补数据集逐一运行Cox模型...")
model0_list <- list()
model1_list <- list()
model2_list <- list()

for (i in 1:imputed_object$m) {
  completed_data <- complete(imputed_object, i)
  completed_data$activity_time_pattern <- factor(
    completed_data$activity_time_pattern,
    levels = c("Morning type", "Afternoon type", "Evening type")
  )
  completed_data$activity_time_pattern <- relevel(completed_data$activity_time_pattern, ref = "Afternoon type")
  
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

model0_tidy <- tidy(pooled_model0, conf.int = TRUE, exponentiate = TRUE)
model1_tidy <- tidy(pooled_model1, conf.int = TRUE, exponentiate = TRUE)
model2_tidy <- tidy(pooled_model2, conf.int = TRUE, exponentiate = TRUE)

model0_tidy$model <- "Model 0 (Unadjusted)"
model1_tidy$model <- "Model 1 (Adjusted for Demographics)"
model2_tidy$model <- "Model 2 (Fully Adjusted)"

all_models_summary_cox_time <- bind_rows(model0_tidy, model1_tidy, model2_tidy)

final_report_table_cox_time <- all_models_summary_cox_time %>%
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

print("===== 结果：活动时段类型 vs 痴呆风险 (Cox模型) =====")
print(as.data.frame(final_report_table_cox_time))

#write.csv(final_report_table_cox_time, file = path_result("cox_model_time_pattern_results.csv"), row.names = FALSE)
write.csv(final_report_table_cox_time, file = path_result("cox_model_time_pattern_results_FIX.csv"), row.names = FALSE)
