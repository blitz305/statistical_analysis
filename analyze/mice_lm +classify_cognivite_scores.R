# 1. 准备工作
# ==========================================
# 加载我们需要的包
library(dplyr)
library(mice)
source("paths.R")

# 2. 最终数据整合 (明确展示每一步)
# ==========================================

# 步骤 A: 清理并准备需要新加入的数据框，统一ID列名
print("步骤 A: 正在准备运动得分和认知得分数据...")
# (尽管这次分析不用regularity_score, 但为保持数据完整性我们先合并它)
regularity_score_clean <- regularity_score %>%
  select(
    Participant_ID = participantid,
    regularity_score
  )

cognitive_score_clean <- cognitive_scores %>%
  select(
    Participant_ID,
    cognitive_score_0,
    cognitive_change # <-- 我们这次的结局变量
  )
print("步骤 A 完成。")


# 步骤 B: 将所有数据源合并，创建包含所有潜在缺失值的最完整数据框
print("步骤 B: 正在合并所有数据框...")
baseline_table_step1_clean <- baseline_table_step1 %>%
  select(-any_of(c("cognitive_score_0")))
full_merged_table <- final_dataset %>%
  # 关键一步：合并所有协变量
  left_join(baseline_table_step1_clean, by = "Participant_ID") %>%
  # 合并运动规律性得分
  left_join(regularity_score_clean, by = "Participant_ID") %>%
  # 合并基线认知得分和认知变化
  left_join(cognitive_score_clean, by = "Participant_ID")
print("步骤 B 完成，`full_merged_table` 已创建。")


# 3. 多重插补 (Impute)
# ==========================================

# 步骤 C: 定义我们最终线性模型需要的所有变量
# 注意：这次的结局变量是 cognitive_change，自变量是 exposure_group
linear_model_vars <- c(
  "Participant_ID", 
  "cognitive_change",  # <--- 新的结局变量
  "exposure_group",    # <--- 新的核心自变量
  "age_recruitment", "sex", "townsend_index", "bmi", 
  "smoking_status", "alcohol_status", "education_level", "cvd_history_any", 
  "history_diabetes", "cognitive_score_0" # 协变量列表
)

# 步骤 D: 从完整数据中，只挑选出用于插补和模型分析的变量
imputation_data <- full_merged_table %>%
  select(all_of(linear_model_vars))

# 在送入 mice 函数前，移除 Participant_ID
imputation_data_for_mice <- imputation_data %>%
  select(-Participant_ID)

# 步骤 E: 执行多重插补 [警告：这一步可能非常耗时！]
print("步骤 E: 开始执行多重插补，请耐心等待...")
imputed_object_lm <- mice(imputation_data_for_mice, m = 10, seed = 123, maxit = 5, printFlag = FALSE)
print("步骤 E 完成，多重插补已生成！")


# 4. 在每个插补数据集上运行分层分析 (Analyze) - [最终修正版]
# ==========================================

# 步骤 F: 定义我们三个模型的公式 (简化版)
# 我们将 relevel() 操作从公式中移除
formula_lm0 <- as.formula("cognitive_change ~ exposure_group")
formula_lm1 <- as.formula("cognitive_change ~ exposure_group + age_recruitment + sex")
all_covariates <- c(
  "age_recruitment", "sex", "townsend_index", "bmi", "smoking_status",
  "alcohol_status", "education_level", "cvd_history_any", 
  "history_diabetes", "cognitive_score_0"
)
formula_lm2 <- as.formula(
  paste("cognitive_change ~ exposure_group +", 
        paste(all_covariates, collapse = " + "))
)

# 步骤 G: 手动循环，在循环内部准备数据并运行模型
print("步骤 G: 正在对插补数据集分别运行3个新模型...")
lm_model0_list <- list()
lm_model1_list <- list()
lm_model2_list <- list()

for (i in 1:imputed_object_lm$m) {
  # 提取第 i 个完整数据集
  completed_data <- complete(imputed_object_lm, i)
  
  # ===================================================================
  # === 关键修正：在这里准备 exposure_group 因子和参照组 ===
  # ===================================================================
  # 1. 明确地将 exposure_group 转换为因子
  completed_data$exposure_group <- factor(completed_data$exposure_group)
  
  # 2. 现在可以安全地设置参照组了
  completed_data$exposure_group <- relevel(completed_data$exposure_group, ref = "Inactive")
  # ===================================================================
  
  # 在这个准备好的数据集上运行我们的三个模型
  lm_model0_list[[i]] <- lm(formula_lm0, data = completed_data)
  lm_model1_list[[i]] <- lm(formula_lm1, data = completed_data)
  lm_model2_list[[i]] <- lm(formula_lm2, data = completed_data)
}
print("步骤 G 完成！")

# 5. 汇总所有分析结果 (Pool) 并展示
# ==========================================

# 步骤 H: 使用 pool() 函数分别汇总三个模型的结果
pooled_lm0 <- pool(lm_model0_list)
pooled_lm1 <- pool(lm_model1_list)
pooled_lm2 <- pool(lm_model2_list)

# 1. 准备工作


# 2. 核心逻辑
# ==========================================
# 对于线性模型，我们不需要 exponentiate = TRUE
model0_tidy <- tidy(pooled_lm0, conf.int = TRUE)
model1_tidy <- tidy(pooled_lm1, conf.int = TRUE)
model2_tidy <- tidy(pooled_lm2, conf.int = TRUE)

model0_tidy$model <- "Model 0 (Unadjusted)"
model1_tidy$model <- "Model 1 (Adjusted for Demographics)"
model2_tidy$model <- "Model 2 (Fully Adjusted)"

all_models_summary_lm_exp <- bind_rows(model0_tidy, model1_tidy, model2_tidy)

final_report_table_lm_exp <- all_models_summary_lm_exp %>%
  select(model, term, estimate, conf.low, conf.high, p.value) %>%
  mutate(
    across(c(estimate, conf.low, conf.high), ~round(.x, 3)),
    p.value = if_else(p.value < 0.001, "<0.001", as.character(round(p.value, 3)))
  ) %>%
  # 注意：这里我们使用的是 Beta (β) 而不是 HR
  mutate(
    `Beta (95% CI)` = paste0(estimate, " (", conf.low, " to ", conf.high, ")")
  ) %>%
  select(
    Model = model,
    Variable = term,
    `Beta (95% CI)`,
    `P Value` = p.value
  )

# 3. 查看与导出
# ==========================================
print("===== 结果：exposure_group vs 认知变化 (线性模型) =====")
print(as.data.frame(final_report_table_lm_exp))

write.csv(final_report_table_lm_exp, file = path_result("linear_model_group_resultt.csv"), row.names = FALSE)
