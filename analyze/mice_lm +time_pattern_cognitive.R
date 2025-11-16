# 1. 准备工作
# ==========================================
# 加载我们需要的包
library(dplyr)
library(mice)
library(miceadds)
library(broom)
library(ggplot2)
library(gridExtra)
library(parallel)
library(doParallel)
library(foreach)
source("paths.R")

# 2. 最终数据整合 (明确展示每一步)
# ==========================================

# 步骤 A: 清理并准备需要新加入的数据框，统一ID列名
print("步骤 A: 正在准备MVPA时段模式和认知得分数据...")
activity_time_pattern_clean <- mvpa_time_pattern %>%
  select(
    Participant_ID,
    activity_time_pattern
  )

cognitive_score_clean <- cognitive_scores %>%
  select(
    Participant_ID,
    cognitive_score_0,
    cognitive_change
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
  "Participant_ID", 
  "cognitive_change",  # <--- 结局变量
  "activity_time_pattern",  # <--- 核心自变量
  "age_recruitment", "sex", "townsend_index", "bmi", 
  "smoking_status", "alcohol_status", "education_level", "cvd_history_any", 
  "history_diabetes", "cognitive_score_0" # 协变量列表
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

# 步骤 E: 执行多重插补 [优化并行版本]
print("步骤 E: 开始执行多重插补，请耐心等待...")

n_cores <- detectCores() - 1
if (n_cores < 1) n_cores <- 1

print(paste("检测到", n_cores, "个CPU核心可用于并行处理"))

method_specification <- c(
  "cognitive_change" = "pmm",
  "activity_time_pattern" = "polyreg",
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

# 步骤 F: 定义我们三个线性模型的公式
formula_lm0 <- as.formula("cognitive_change ~ activity_time_pattern")
formula_lm1 <- as.formula("cognitive_change ~ activity_time_pattern + age_recruitment + sex")
all_covariates <- c(
  "age_recruitment", "sex", "townsend_index", "bmi", "smoking_status",
  "alcohol_status", "education_level", "cvd_history_any", 
  "history_diabetes", "cognitive_score_0"
)
formula_lm2 <- as.formula(
  paste("cognitive_change ~ activity_time_pattern +", 
        paste(all_covariates, collapse = " + "))
)

# 步骤 G: 手动循环，运行这三个线性模型
print("步骤 G: 正在并行对插补数据集运行3个线性模型...")

fit_models_parallel <- function(i) {
  completed_data <- complete(imputed_object, i)
  completed_data$activity_time_pattern <- factor(
    completed_data$activity_time_pattern,
    levels = c("Morning type", "Afternoon type", "Evening type")
  )
  completed_data$activity_time_pattern <- relevel(completed_data$activity_time_pattern, ref = "Afternoon type")

  list(
    model0 = lm(formula_lm0, data = completed_data),
    model1 = lm(formula_lm1, data = completed_data),
    model2 = lm(formula_lm2, data = completed_data)
  )
}

model_results <- foreach(i = 1:imputed_object$m, .packages = c("mice", "stats")) %dopar% {
  fit_models_parallel(i)
}

lm_model0_list <- lapply(model_results, function(x) x$model0)
lm_model1_list <- lapply(model_results, function(x) x$model1)
lm_model2_list <- lapply(model_results, function(x) x$model2)

print("步骤 G 完成！")


# 5. 汇总所有分析结果 (Pool) 并展示
# ==========================================

# 步骤 H: 使用 pool() 函数分别汇总三个模型的结果
pooled_lm0 <- pool(lm_model0_list)
pooled_lm1 <- pool(lm_model1_list)
pooled_lm2 <- pool(lm_model2_list)

# 使用 broom::tidy() 函数来提取每个模型的结果
model0_tidy <- tidy(pooled_lm0, conf.int = TRUE)
model1_tidy <- tidy(pooled_lm1, conf.int = TRUE)
model2_tidy <- tidy(pooled_lm2, conf.int = TRUE)

model0_tidy$model <- "Model 0 (Unadjusted)"
model1_tidy$model <- "Model 1 (Adjusted for Demographics)"
model2_tidy$model <- "Model 2 (Fully Adjusted)"

all_models_summary_lm_time <- bind_rows(model0_tidy, model1_tidy, model2_tidy)

final_report_table_lm_time <- all_models_summary_lm_time %>%
  select(model, term, estimate, conf.low, conf.high, p.value) %>%
  mutate(
    across(c(estimate, conf.low, conf.high), ~round(.x, 3)),
    p.value = if_else(p.value < 0.001, "<0.001", as.character(round(p.value, 3)))
  ) %>%
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

print("===== 结果：活动时段类型 vs 认知变化 (线性模型) =====")
print(as.data.frame(final_report_table_lm_time))

write.csv(final_report_table_lm_time, file = path_result("linear_model_time_pattern_results.csv"), row.names = FALSE)

# 6. 模型诊断图 (Model Diagnostics)
print("步骤 I: 生成模型诊断图...")

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
  print("数据量较小，使用全部数据进行诊断图绘制")
} else {
  diagnostic_data <- diagnostic_data_full[sample(nrow(diagnostic_data_full), sample_size), ]
}

print(paste("诊断图使用采样数据，样本数:", nrow(diagnostic_data), "（原始数据:", nrow(diagnostic_data_full), "）"))

diagnostic_data$activity_time_pattern <- factor(
  diagnostic_data$activity_time_pattern,
  levels = c("Morning type", "Afternoon type", "Evening type")
)
diagnostic_data$activity_time_pattern <- relevel(diagnostic_data$activity_time_pattern, ref = "Afternoon type")

diagnostic_model <- lm(formula_lm2, data = diagnostic_data)

create_diagnostic_plots <- function(model, model_name) {
  residuals <- residuals(model)
  fitted_values <- fitted(model)

  residuals_vs_fitted <- ggplot(data.frame(fitted = fitted_values, residuals = residuals)) +
    geom_point(aes(x = fitted, y = residuals), alpha = 0.6, color = "steelblue") +
    geom_smooth(aes(x = fitted, y = residuals), method = "loess", se = TRUE, color = "red") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    labs(
      title = paste(model_name, "- Residuals vs Fitted"),
      x = "Fitted Values",
      y = "Residuals"
    ) +
    theme_minimal()

  qq_data <- data.frame(
    theoretical = qnorm(ppoints(length(residuals))),
    sample = sort(residuals)
  )

  qq_plot <- ggplot(qq_data) +
    geom_point(aes(x = .data$theoretical, y = .data$sample), alpha = 0.6, color = "steelblue") +
    geom_qq_line(aes(sample = .data$sample), color = "red", size = 1) +
    labs(
      title = paste(model_name, "- Q-Q Plot"),
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme_minimal()

  list(residuals_vs_fitted = residuals_vs_fitted, qq_plot = qq_plot)
}

create_diagnostic_plots_parallel <- function(formula, model_name, data) {
  model <- lm(formula, data = data)
  create_diagnostic_plots(model, model_name)
}

diagnostic_plots_results <- foreach(
  i = 1:3,
  .packages = c("ggplot2", "stats"),
  .export = c("create_diagnostic_plots", "formula_lm0", "formula_lm1", "formula_lm2", "diagnostic_data")
) %dopar% {
  switch(i,
    create_diagnostic_plots_parallel(formula_lm0, "Model 0", diagnostic_data),
    create_diagnostic_plots_parallel(formula_lm1, "Model 1", diagnostic_data),
    create_diagnostic_plots_parallel(formula_lm2, "Model 2", diagnostic_data)
  )
}

diagnostic_plots_model0 <- diagnostic_plots_results[[1]]
diagnostic_plots_model1 <- diagnostic_plots_results[[2]]
diagnostic_plots_model2 <- diagnostic_plots_results[[3]]

all_diagnostic_plots <- grid.arrange(
  diagnostic_plots_model0$residuals_vs_fitted,
  diagnostic_plots_model0$qq_plot,
  diagnostic_plots_model1$residuals_vs_fitted,
  diagnostic_plots_model1$qq_plot,
  diagnostic_plots_model2$residuals_vs_fitted,
  diagnostic_plots_model2$qq_plot,
  ncol = 2, nrow = 3
)

ggsave(
  filename = path_result("linear_model_time_pattern_diagnostics.png"),
  plot = all_diagnostic_plots,
  width = 12, height = 15, dpi = 300
)

print("诊断图已保存到: linear_model_time_pattern_diagnostics.png")

print("步骤 J: 计算模型诊断统计信息...")

diagnostic_stats <- data.frame(
  Model = c("Model 0", "Model 1", "Model 2"),
  R_squared = c(
    summary(lm(formula_lm0, data = diagnostic_data))$r.squared,
    summary(lm(formula_lm1, data = diagnostic_data))$r.squared,
    summary(diagnostic_model)$r.squared
  ),
  Adjusted_R_squared = c(
    summary(lm(formula_lm0, data = diagnostic_data))$adj.r.squared,
    summary(lm(formula_lm1, data = diagnostic_data))$adj.r.squared,
    summary(diagnostic_model)$adj.r.squared
  ),
  AIC = c(
    AIC(lm(formula_lm0, data = diagnostic_data)),
    AIC(lm(formula_lm1, data = diagnostic_data)),
    AIC(diagnostic_model)
  ),
  BIC = c(
    BIC(lm(formula_lm0, data = diagnostic_data)),
    BIC(lm(formula_lm1, data = diagnostic_data)),
    BIC(diagnostic_model)
  )
)

print("模型诊断统计信息:")
print(diagnostic_stats)

write.csv(diagnostic_stats, file = path_result("linear_model_time_pattern_diagnostic_stats.csv"), row.names = FALSE)

# 美化输出结果表格
# ==========================================
print("步骤 K: 正在美化输出结果表格...")
source("beautify_results.R")
beautify_model_results(
  path_result("linear_model_time_pattern_results.csv"),
  path_result("linear_model_time_pattern_results_beautified.png")
)
print("步骤 K 完成，美化表格已生成。")