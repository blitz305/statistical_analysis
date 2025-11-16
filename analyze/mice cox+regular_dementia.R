# 1. 准备工作
# ==========================================
# 加载我们需要的包
library(dplyr)
library(survival)
library(mice)
library(miceadds)
library(broom)
library(ggplot2)
library(gridExtra)
library(parallel)
library(doParallel)
library(foreach)
library(tidyr)
library(writexl)
source("paths.R")
# 2. 最终数据整合 (明确展示每一步)
# ==========================================

# 步骤 A: 清理并准备需要新加入的数据框，统一ID列名
print("步骤 A: 正在准备运动得分、MVPA和认知得分数据...")
regularity_score_clean <- regularity_score %>%
  select(
    Participant_ID = participantid,
    regularity_score = regularity_score
  )

mvpa_clean <- Derived_accelerometry %>%
  transmute(
    Participant_ID = `Participant ID`,
    overall_mvpa_minutes = `Moderate-Vigorous - Overall average | Instance 0` * 24
  )

categ_mvpa <- mvpa_classify %>%
  select(
    Participant_ID = participant_id,
    mvpa_activity_pattern
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
  # 合并运动规律性得分
  left_join(regularity_score_clean, by = "Participant_ID") %>%
  # 合并MVPA数据
  left_join(mvpa_clean, by = "Participant_ID") %>%
  # 合并MVPA分类结果
  left_join(categ_mvpa, by = "Participant_ID") %>%
  # 合并基线认知得分
  left_join(cognitive_score_clean, by = "Participant_ID")
print("步骤 B 完成，`full_merged_table` 已创建。")


# 仅保留分类为“Concentrated Active”或“Regularly Active”的参与者
allowed_patterns <- c("Concentrated Active", "Regularly Active")
print("步骤 B+: 仅保留符合活动模式的参与者 (Concentrated Active / Regularly Active)...")
full_merged_table_filtered <- full_merged_table %>%
  filter(mvpa_activity_pattern %in% allowed_patterns)
print(paste(
  "筛选后样本量：",
  nrow(full_merged_table_filtered),
  "(原始样本量：",
  nrow(full_merged_table),
  ")"
))


# 3. 多重插补 (Impute)
# ==========================================

# 步骤 C: 定义我们最终模型需要的所有变量
all_model_vars <- c(
  "Participant_ID", "survival_time_days", "dementia_status",
  "regularity_score", "overall_mvpa_minutes", "age_recruitment", "sex", "townsend_index", "bmi", 
  "smoking_status", "alcohol_status", "education_level", "cvd_history_any", 
  "history_diabetes", "cognitive_score_0"
)

# 步骤 D: 从完整数据中，只挑选出用于插补和模型分析的变量
imputation_data <- full_merged_table_filtered %>%
  select(all_of(all_model_vars))

# 步骤 D1: 剔除结局变量和暴露变量的缺失值
print("步骤 D1: 正在剔除结局变量和暴露变量的缺失值...")
n_before_exclusion <- nrow(imputation_data)
n_missing_outcome1 <- sum(is.na(imputation_data$survival_time_days))
n_missing_outcome2 <- sum(is.na(imputation_data$dementia_status))
n_missing_exposure <- sum(is.na(imputation_data$regularity_score))
n_missing_either <- sum(is.na(imputation_data$survival_time_days) | 
                         is.na(imputation_data$dementia_status) | 
                         is.na(imputation_data$regularity_score))

print(paste("剔除前样本数:", n_before_exclusion))
print(paste("survival_time_days 缺失数:", n_missing_outcome1, 
            "(", round(n_missing_outcome1/n_before_exclusion * 100, 2), "%)"))
print(paste("dementia_status 缺失数:", n_missing_outcome2, 
            "(", round(n_missing_outcome2/n_before_exclusion * 100, 2), "%)"))
print(paste("regularity_score 缺失数:", n_missing_exposure, 
            "(", round(n_missing_exposure/n_before_exclusion * 100, 2), "%)"))
print(paste("至少一个缺失的数量:", n_missing_either, 
            "(", round(n_missing_either/n_before_exclusion * 100, 2), "%)"))

# 剔除缺失值
imputation_data <- imputation_data %>%
  filter(!is.na(survival_time_days) & 
         !is.na(dementia_status) & 
         !is.na(regularity_score))

n_after_exclusion <- nrow(imputation_data)
n_excluded <- n_before_exclusion - n_after_exclusion
print(paste("剔除后样本数:", n_after_exclusion))
print(paste("剔除样本数:", n_excluded, "(", round(n_excluded/n_before_exclusion * 100, 2), "%)"))
print("步骤 D1 完成。")

# 打印最终纳入分析的样本量
print("\n===== 最终纳入分析的样本量 =====")
print(paste("准备进行插补的样本数:", n_after_exclusion))

# 准备用于插补的数据（移除Participant_ID）
imputation_data_for_mice <- imputation_data %>%
  select(-Participant_ID)

# 步骤 E: 执行多重插补 [优化版本，支持并行处理]
print("\n步骤 E: 开始执行多重插补，请耐心等待...")
print("注意：survival_time_days、dementia_status 和 regularity_score 不参与插补（已剔除缺失值）")

n_cores <- detectCores() - 1
if (n_cores < 1) n_cores <- 1
print(paste("检测到", n_cores, "个CPU核心可用于并行处理"))

method_specification <- c(
  "survival_time_days" = "",
  "dementia_status" = "",
  "regularity_score" = "",
  "overall_mvpa_minutes" = "pmm",
  "age_recruitment" = "pmm",
  "sex" = "logreg",
  "townsend_index" = "pmm",
  "bmi" = "pmm",
  "smoking_status" = "polr",
  "alcohol_status" = "polr",
  "education_level" = "polr",
  "cvd_history_any" = "logreg",
  "history_diabetes" = "logreg",
  "cognitive_score_0" = "pmm"
)

imputed_object <- mice(
  imputation_data_for_mice,
  method = method_specification,
  m = 10,
  seed = 123,
  maxit = 10,
  printFlag = TRUE,
  n.core = n_cores,
  ridge = 1e-05,
  threshold = 1.0
)

print("步骤 E 完成，多重插补已生成！")

registerDoParallel(cores = n_cores)
print(paste("已启用", n_cores, "个CPU核心用于后续并行处理"))


# 4. 在每个插补数据集上运行分层分析 (Analyze)
# ==========================================

# 步骤 F: 定义我们三个模型的公式
formula_model0 <- as.formula("Surv(survival_time_days, dementia_status) ~ regularity_score")
formula_model1 <- as.formula("Surv(survival_time_days, dementia_status) ~ regularity_score + age_recruitment + sex")
all_covariates <- c(
  "overall_mvpa_minutes", "age_recruitment", "sex", "townsend_index", "bmi", "smoking_status",
  "alcohol_status", "education_level", "cvd_history_any", 
  "history_diabetes", "cognitive_score_0"
)
formula_model2 <- as.formula(
  paste("Surv(survival_time_days, dementia_status) ~ regularity_score +", 
        paste(all_covariates, collapse = " + "))
)

# 步骤 G: 使用 with() 在每个插补数据集上运行这三个模型
print("步骤 G: 正在并行对插补数据集运行Cox模型...")

fit_models_parallel <- function(i) {
  completed_data <- complete(imputed_object, i)

  list(
    model0 = coxph(formula_model0, data = completed_data),
    model1 = coxph(formula_model1, data = completed_data),
    model2 = coxph(formula_model2, data = completed_data)
  )
}

model_results <- foreach(i = 1:imputed_object$m, .packages = c("survival", "mice")) %dopar% {
  fit_models_parallel(i)
}

model0_list <- lapply(model_results, function(x) x$model0)
model1_list <- lapply(model_results, function(x) x$model1)
model2_list <- lapply(model_results, function(x) x$model2)

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

all_models_summary_cox_rs <- bind_rows(model0_tidy, model1_tidy, model2_tidy)

final_report_table_cox_rs <- all_models_summary_cox_rs %>%
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


write.csv(final_report_table_cox_rs, file = path_result("cox_model_regular_results.csv"), row.names = FALSE)


# 6. 模型诊断与验证
print("步骤 I: 进行比例风险假设检验与一致性评估...")

diagnostic_data_full <- complete(imputed_object, 1)

set.seed(123)
sample_proportion <- 0.05
min_sample_size <- 1000
max_sample_size <- 10000

sample_size <- max(min_sample_size,
                   min(max_sample_size,
                       round(nrow(diagnostic_data_full) * sample_proportion)))

if (nrow(diagnostic_data_full) <= min_sample_size) {
  diagnostic_data <- diagnostic_data_full
  print("数据量较小，使用全部数据进行诊断分析")
} else {
  diagnostic_data <- diagnostic_data_full[sample(nrow(diagnostic_data_full), sample_size), ]
}

print(paste("诊断使用样本量:", nrow(diagnostic_data), "（原始样本量:", nrow(diagnostic_data_full), "）"))

diagnostic_model0 <- coxph(formula_model0, data = diagnostic_data)
diagnostic_model1 <- coxph(formula_model1, data = diagnostic_data)
diagnostic_model2 <- coxph(formula_model2, data = diagnostic_data)

cox_zph0 <- cox.zph(diagnostic_model0)
cox_zph1 <- cox.zph(diagnostic_model1)
cox_zph2 <- cox.zph(diagnostic_model2)

zph_table <- list(
  `Model 0` = cox_zph0$table,
  `Model 1` = cox_zph1$table,
  `Model 2` = cox_zph2$table
) %>%
  lapply(function(tbl) {
    tbl_df <- as.data.frame(tbl)
    tbl_df$Variable <- rownames(tbl)
    tbl_df
  }) %>%
  bind_rows(.id = "Model") %>%
  relocate(Model, Variable)

write.csv(zph_table, file = path_result("cox_model_regular_ph_tests.csv"), row.names = FALSE)

create_ph_plot <- function(cox_zph_object, model_name) {
  y_mat <- as.data.frame(cox_zph_object$y)
  time_vec <- cox_zph_object$x
  df_list <- lapply(seq_along(y_mat), function(i) {
    data.frame(
      time = time_vec,
      residual = y_mat[[i]],
      variable = names(y_mat)[i]
    )
  })
  zph_df <- bind_rows(df_list)

  ggplot(zph_df, aes(x = time, y = residual)) +
    geom_point(alpha = 0.4, color = "steelblue") +
    geom_smooth(method = "loess", se = TRUE, color = "red") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    facet_wrap(~variable, scales = "free_y") +
    labs(
      title = paste(model_name, "- Schoenfeld 残差"),
      x = "随访时间 (天)",
      y = "缩放后的Schoenfeld残差"
    ) +
    theme_minimal()
}

ph_plot0 <- create_ph_plot(cox_zph0, "Model 0")
ph_plot1 <- create_ph_plot(cox_zph1, "Model 1")
ph_plot2 <- create_ph_plot(cox_zph2, "Model 2")

ph_combined_plot <- grid.arrange(ph_plot0, ph_plot1, ph_plot2, ncol = 1)

ggsave(
  filename = path_result("cox_model_regular_ph_plots.png"),
  plot = ph_combined_plot,
  width = 12,
  height = 18,
  dpi = 300
)

diagnostic_data <- diagnostic_data %>%
  mutate(
    lp_model0 = predict(diagnostic_model0, type = "lp"),
    lp_model1 = predict(diagnostic_model1, type = "lp"),
    lp_model2 = predict(diagnostic_model2, type = "lp")
  )

c_index_list <- list(
  `Model 0` = survConcordance(Surv(survival_time_days, dementia_status) ~ lp_model0, data = diagnostic_data),
  `Model 1` = survConcordance(Surv(survival_time_days, dementia_status) ~ lp_model1, data = diagnostic_data),
  `Model 2` = survConcordance(Surv(survival_time_days, dementia_status) ~ lp_model2, data = diagnostic_data)
)

c_index_results <- lapply(names(c_index_list), function(model_name) {
  res <- c_index_list[[model_name]]
  data.frame(
    Model = model_name,
    Concordance = res$concordance,
    StdError = res$std.err,
    Lower95 = res$concordance - 1.96 * res$std.err,
    Upper95 = res$concordance + 1.96 * res$std.err,
    N = res$n
  )
}) %>%
  bind_rows()

write.csv(c_index_results, file = path_result("cox_model_regular_c_index.csv"), row.names = FALSE)

# 美化输出结果表格
# ==========================================
print("步骤 J: 正在美化输出结果表格...")
source("beautify_results.R")
beautify_model_results(
  path_result("cox_model_regular_results.csv"),
  path_result("cox_model_regular_results_beautified.png")
)
print("步骤 J 完成，美化表格已生成。")
