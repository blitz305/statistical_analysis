# 1. 准备工作
# ==========================================
# 加载我们需要的包
library(dplyr)
library(mice)
source("paths.R")

# 确保我们之前的所有成果都已加载:
# final_dataset, baseline_table_step1, regularity_score, cognitive_scores
# ...


# 2. 最终数据整合 (与之前相同)
# ==========================================

# 步骤 A: 清理并准备独立的自变量数据框
print("步骤 A: 正在准备运动得分和认知得分数据...")
regularity_score_clean <- regularity_score %>%
  select(
    Participant_ID = participantid,
    regularity_score
  )

cognitive_score_clean <- cognitive_scores %>%
  select(
    Participant_ID,
    cognitive_score_0,
    cognitive_change
  )
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

# 步骤 C: 定义我们最终模型需要的所有变量
# 注意：这次的自变量是 regularity_score
all_model_vars <- c(
  "Participant_ID", 
  "cognitive_change",  # <--- 结局变量
  "regularity_score",  # <--- 核心自变量
  "age_recruitment", "sex", "townsend_index", "bmi", 
  "smoking_status", "alcohol_status", "education_level", "cvd_history_any", 
  "history_diabetes", "cognitive_score_0" # 协变量列表
)

# 步骤 D: 从完整数据中挑选用于插补的变量
imputation_data <- full_merged_table %>%
  select(all_of(all_model_vars))

# 在送入 mice 函数前，移除 Participant_ID
imputation_data_for_mice <- imputation_data %>%
  select(-Participant_ID)

# 步骤 E: 执行多重插补 (不变)
print("步骤 E: 开始执行多重插补，请耐心等待...")
# 我将这个对象命名为_rs，以区别于上次的_lm，避免混淆
imputed_object_rs <- mice(imputation_data_for_mice, m = 10, seed = 123, maxit = 5, printFlag = FALSE)
print("步骤 E 完成，多重插补已生成！")


# 4. 在每个插补数据集上运行分层线性回归分析 (Analyze)
# ==========================================

# 步骤 F: 定义我们三个线性模型的新公式
# 这次自变量是 regularity_score
formula_lm0_rs <- as.formula("cognitive_change ~ regularity_score")
formula_lm1_rs <- as.formula("cognitive_change ~ regularity_score + age_recruitment + sex")
lm_covariates <- c(
  "age_recruitment", "sex", "townsend_index", "bmi", "smoking_status",
  "alcohol_status", "education_level", "cvd_history_any", 
  "history_diabetes", "cognitive_score_0"
)
formula_lm2_rs <- as.formula(
  paste("cognitive_change ~ regularity_score +", 
        paste(lm_covariates, collapse = " + "))
)

# 步骤 G: 手动循环，运行这三个线性模型
print("步骤 G: 正在对插补数据集分别运行3个线性模型...")
lm_model0_list_rs <- list()
lm_model1_list_rs <- list()
lm_model2_list_rs <- list()

for (i in 1:imputed_object_rs$m) {
  completed_data <- complete(imputed_object_rs, i)
  
  lm_model0_list_rs[[i]] <- lm(formula_lm0_rs, data = completed_data)
  lm_model1_list_rs[[i]] <- lm(formula_lm1_rs, data = completed_data)
  lm_model2_list_rs[[i]] <- lm(formula_lm2_rs, data = completed_data)
}
print("步骤 G 完成！")


# 5. 汇总所有分析结果 (Pool) 并展示
# ==========================================

# 步骤 H: 使用 pool() 函数分别汇总三个模型的结果
pooled_lm0_rs <- pool(lm_model0_list_rs)
pooled_lm1_rs <- pool(lm_model1_list_rs)
pooled_lm2_rs <- pool(lm_model2_list_rs)

# 1. 准备工作
# ==========================================

library(broom)
library(dplyr)



# 使用 broom::tidy() 函数来提取每个模型的结果
# tidy() 会将模型输出转换为一个整洁的数据框
model0_tidy <- tidy(pooled_lm0_rs, conf.int = TRUE)
model1_tidy <- tidy(pooled_lm1_rs, conf.int = TRUE)
model2_tidy <- tidy(pooled_lm2_rs, conf.int = TRUE)

# 为了方便区分，给每个结果表增加一列，标明模型名称
model0_tidy$model <- "Model 0 (Unadjusted)"
model1_tidy$model <- "Model 1 (Adjusted for Demographics)"
model2_tidy$model <- "Model 2 (Fully Adjusted)"

# 使用 dplyr::bind_rows() 将三个结果合并成一个大表
all_models_summary <- bind_rows(model0_tidy, model1_tidy, model2_tidy)

# 创建一个更适合报告的最终表格
# 我们将 beta系数 和 95% CI 合并成一列
# 并对数值进行四舍五入，保留3位小数
final_report_table <- all_models_summary %>%
  # 保留需要的列
  select(model, term, estimate, conf.low, conf.high, p.value) %>%
  # 对数值进行格式化
  mutate(
    # across() 可以对多列同时应用一个函数
    across(c(estimate, conf.low, conf.high), ~round(.x, 3)),
    # p.value 通常需要更精细的处理，比如 <0.001
    p.value = if_else(p.value < 0.001, "<0.001", as.character(round(p.value, 3)))
  ) %>%
  # 将 estimate 和置信区间合并成 "estimate (conf.low-conf.high)" 的格式
  mutate(
    `Beta (95% CI)` = paste0(estimate, " (", conf.low, " to ", conf.high, ")")
  ) %>%
  # 重新排列并选择最终的列
  select(
    Model = model,
    Variable = term,
    `Beta (95% CI)`,
    `P Value` = p.value
  )


# 3. 查看与导出
# ==========================================


write.csv(final_report_table, file = path_result("linear_model_regualr_results.csv"), row.names = FALSE)
