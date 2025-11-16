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
source("paths.R")

# 2. 最终数据整合 (明确展示每一步)
# ==========================================

# 步骤 A: 清理并准备需要新加入的数据框，统一ID列名
print("步骤 A: 正在准备MVPA和认知得分数据...")
mvpa_clean <- Derived_accelerometry %>%
  transmute(
    Participant_ID = `Participant ID`,
    overall_mvpa_hours = `Moderate-Vigorous - Overall average | Instance 0` * 24  # 转换为每天MVPA小时数
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
  left_join(mvpa_clean, by = "Participant_ID") %>%
  left_join(cognitive_score_clean, by = "Participant_ID")
print("步骤 B 完成，`full_merged_table` 已创建。")

# 根据 overall_mvpa_hours 计算三分位并创建 Low / Medium / High 分组
mvpa_quantiles <- quantile(
  full_merged_table$overall_mvpa_hours,
  probs = c(1 / 3, 2 / 3),
  na.rm = TRUE,
  names = FALSE
)
print(paste0(
  "MVPA 三分位阈值：Low≤", round(mvpa_quantiles[1], 2),
  ", Medium≤", round(mvpa_quantiles[2], 2),
  ", High>阈值2"
))

full_merged_table <- full_merged_table %>%
  mutate(
    mvpa_group = case_when(
      is.na(overall_mvpa_hours) ~ NA_character_,
      overall_mvpa_hours <= mvpa_quantiles[1] ~ "Low",
      overall_mvpa_hours <= mvpa_quantiles[2] ~ "Medium",
      TRUE ~ "High"
    ),
    mvpa_group = factor(mvpa_group, levels = c("Low", "Medium", "High"))
  )


# 3. 多重插补 (Impute)
# ==========================================

# 步骤 C: 定义我们最终模型需要的所有变量
all_model_vars <- c(
  "Participant_ID", 
  "cognitive_change",  # <--- 结局变量
  "mvpa_group",        # <--- 三分组后的核心自变量
  "overall_mvpa_hours",  # <--- 核心自变量
  "age_recruitment", "sex", "townsend_index", "bmi", 
  "smoking_status", "alcohol_status", "education_level", "cvd_history_any", 
  "history_diabetes", "cognitive_score_0" # 协变量列表
)

# 步骤 D: 从完整数据中，只挑选出用于插补和模型分析的变量
imputation_data <- full_merged_table %>%
  select(all_of(all_model_vars))

# 步骤 D1: 仅统计缺失情况（不再剔除，由插补处理）
print("步骤 D1: 统计结局变量与 MVPA 三分组的缺失情况（不剔除，交由插补处理）")
n_before_exclusion <- nrow(imputation_data)
n_missing_outcome <- sum(is.na(imputation_data$cognitive_change))
n_missing_exposure_cont <- sum(is.na(imputation_data$overall_mvpa_hours))
n_missing_exposure_group <- sum(is.na(imputation_data$mvpa_group))
n_missing_either <- sum(is.na(imputation_data$cognitive_change) | is.na(imputation_data$mvpa_group))

print(paste("剔除前样本数:", n_before_exclusion))
print(paste("cognitive_change 缺失数:", n_missing_outcome, "(", round(n_missing_outcome/n_before_exclusion * 100, 2), "%)"))
print(paste("overall_mvpa_hours 缺失数:", n_missing_exposure_cont, "(", round(n_missing_exposure_cont/n_before_exclusion * 100, 2), "%)"))
print(paste("mvpa_group 缺失数:", n_missing_exposure_group, "(", round(n_missing_exposure_group/n_before_exclusion * 100, 2), "%)"))
print(paste("至少一个缺失的数量:", n_missing_either, "(", round(n_missing_either/n_before_exclusion * 100, 2), "%)"))
print("提示：不再剔除缺失值，后续将通过 MICE 对暴露及协变量进行插补。")

# 打印最终纳入分析的样本量
print("\n===== 最终纳入分析的样本量 =====")
print(paste("最终分析样本数:", n_before_exclusion, "(与原始样本一致)"))

# 准备用于插补的数据（移除Participant_ID）
imputation_data_for_mice <- imputation_data %>%
  mutate(
    mvpa_group = factor(mvpa_group, levels = c("Low", "Medium", "High"))
  ) %>%
  select(-Participant_ID)

# 步骤 E: 执行多重插补 [优化版本，支持并行处理]
print("步骤 E: 开始执行多重插补，请耐心等待...")
print("注意：仅结局变量不参与插补；MVPA 暴露与协变量均通过 MICE 进行插补")

# 检测可用CPU核心数，设置并行处理
n_cores <- detectCores() - 1  # 保留一个核心给系统
if (n_cores < 1) n_cores <- 1

print(paste("检测到", n_cores, "个CPU核心可用于并行处理"))

# 为每个变量明确指定插补方法
method_specification <- c(
  "cognitive_change" = "",      # 结局变量不插补
  "overall_mvpa_hours" = "pmm", # 连续暴露使用 pmm
  "mvpa_group" = "polyreg",     # 多分类暴露使用 polyreg
  "age_recruitment" = "pmm",
  "sex" = "logreg",
  "smoking_status" = "polr",
  "education_level" = "polr",
  "bmi" = "pmm",
  "townsend_index" = "pmm",
  "alcohol_status" = "polr",
  "cvd_history_any" = "logreg",
  "history_diabetes" = "logreg",
  "cognitive_score_0" = "pmm"
)

# 执行多重插补，使用指定的方法进行并行处理
imputed_object <- mice(
  imputation_data_for_mice, 
  method = method_specification,  # 使用命名向量，为每个变量指定合适的方法
  m = 10,           # 插补数据集数量
  seed = 123,       # 随机种子
  maxit = 10,       # 增加迭代次数（从5增加到10）
  printFlag = TRUE, # 显示进度
  n.core = n_cores, # 并行核心数
  ridge = 1e-05,    # 提高数值稳定性
  threshold = 1.0   # 优化收敛
)

print("步骤 E 完成，多重插补已生成！")

# 启用后续步骤的并行处理
registerDoParallel(cores = n_cores)
print(paste("已启用", n_cores, "个CPU核心用于后续并行处理"))

# 4. 在每个插补数据集上运行分层分析 (Analyze)
# ==========================================

# 步骤 F: 定义我们三个线性模型的公式（基于 MVPA 三分组）
formula_lm0 <- as.formula("cognitive_change ~ mvpa_group")
formula_lm1 <- as.formula("cognitive_change ~ mvpa_group + age_recruitment + sex")
all_covariates <- c(
  "age_recruitment", "sex", "townsend_index", "bmi", "smoking_status",
  "alcohol_status", "education_level", "cvd_history_any", 
  "history_diabetes", "cognitive_score_0"
)
formula_lm2 <- as.formula(
  paste("cognitive_change ~ mvpa_group +", 
        paste(all_covariates, collapse = " + "))
)

# 步骤 G: 并行运行这三个线性模型
print("步骤 G: 正在并行对插补数据集运行3个线性模型...")

# 定义并行拟合函数
fit_models_parallel <- function(i) {
  completed_data <- complete(imputed_object, i)
  completed_data$mvpa_group <- factor(completed_data$mvpa_group, levels = c("Low", "Medium", "High"))
  
  list(
    model0 = lm(formula_lm0, data = completed_data),
    model1 = lm(formula_lm1, data = completed_data),
    model2 = lm(formula_lm2, data = completed_data)
  )
}

# 使用foreach进行并行处理
library(foreach)
model_results <- foreach(i = 1:imputed_object$m, .packages = c("mice")) %dopar% {
  fit_models_parallel(i)
}

# 提取模型列表
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

all_models_summary_lm_mvpa <- bind_rows(model0_tidy, model1_tidy, model2_tidy)

final_report_table_lm_mvpa <- all_models_summary_lm_mvpa %>%
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

# 6. 模型诊断图 (Model Diagnostics)
# ==========================================
# 以下代码已注释，如需模型诊断可取消注释

# print("步骤 I: 生成模型诊断图...")

# # 选择第一个插补数据集进行诊断图绘制
# diagnostic_data_full <- complete(imputed_object, 1)
# # 添加回Participant_ID和age_group_65
# diagnostic_data_full <- diagnostic_data_full %>%
#   bind_cols(id_and_age_group)
# 
# # 为了加快诊断图生成速度，使用采样数据（5%抽样）
# set.seed(123)
# sample_proportion <- 0.05  # 使用5%的样本进行诊断图绘制
# min_sample_size <- 1000    # 最少1000个样本
# max_sample_size <- 10000   # 最多10000个样本
# 
# # 按年龄组分别采样
# diagnostic_data_age65 <- diagnostic_data_full %>%
#   filter(age_group_65 == "≤65")
# diagnostic_data_age65plus <- diagnostic_data_full %>%
#   filter(age_group_65 == ">65")
# 
# # 对≤65组采样
# if (nrow(diagnostic_data_age65) <= min_sample_size) {
#   diagnostic_data_age65_sampled <- diagnostic_data_age65
# } else {
#   sample_size_age65 <- max(min_sample_size, 
#                            min(max_sample_size, 
#                                round(nrow(diagnostic_data_age65) * sample_proportion)))
#   diagnostic_data_age65_sampled <- diagnostic_data_age65[sample(nrow(diagnostic_data_age65), sample_size_age65), ]
# }
# 
# # 对>65组采样
# if (nrow(diagnostic_data_age65plus) <= min_sample_size) {
#   diagnostic_data_age65plus_sampled <- diagnostic_data_age65plus
# } else {
#   sample_size_age65plus <- max(min_sample_size, 
#                                min(max_sample_size, 
#                                    round(nrow(diagnostic_data_age65plus) * sample_proportion)))
#   diagnostic_data_age65plus_sampled <- diagnostic_data_age65plus[sample(nrow(diagnostic_data_age65plus), sample_size_age65plus), ]
# }
# 
# print(paste("≤65组诊断图使用采样数据，样本数:", nrow(diagnostic_data_age65_sampled), "（原始数据:", nrow(diagnostic_data_age65), "）"))
# print(paste(">65组诊断图使用采样数据，样本数:", nrow(diagnostic_data_age65plus_sampled), "（原始数据:", nrow(diagnostic_data_age65plus), "）"))
# 
# # 创建诊断图函数
# create_diagnostic_plots <- function(model, model_name) {
#   # 获取残差和拟合值
#   residuals <- residuals(model)
#   fitted_values <- fitted(model)
#   
#   # 创建Residuals vs Fitted图
#   residuals_vs_fitted <- ggplot(data.frame(fitted = fitted_values, residuals = residuals)) +
#     geom_point(aes(x = fitted, y = residuals), alpha = 0.6, color = "steelblue") +
#     geom_smooth(aes(x = fitted, y = residuals), method = "loess", se = TRUE, color = "red") +
#     geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
#     labs(
#       title = paste(model_name, "- Residuals vs Fitted"),
#       x = "Fitted Values",
#       y = "Residuals"
#     ) +
#     theme_minimal() +
#     theme(
#       plot.title = element_text(size = 12, hjust = 0.5),
#       axis.title = element_text(size = 10)
#     )
#   
#   # 创建Q-Q图
#   qq_data <- data.frame(
#     theoretical = qnorm(ppoints(length(residuals))),
#     sample = sort(residuals)
#   )
#   
#   qq_plot <- ggplot(qq_data) +
#     geom_point(aes(x = .data$theoretical, y = .data$sample), alpha = 0.6, color = "steelblue") +
#     geom_qq_line(aes(sample = .data$sample), color = "red", size = 1) +
#     labs(
#       title = paste(model_name, "- Q-Q Plot"),
#       x = "Theoretical Quantiles",
#       y = "Sample Quantiles"
#     ) +
#     theme_minimal() +
#     theme(
#       plot.title = element_text(size = 12, hjust = 0.5),
#       axis.title = element_text(size = 10)
#     )
#   
#   return(list(residuals_vs_fitted = residuals_vs_fitted, qq_plot = qq_plot))
# }
# 
# # 定义并行诊断图创建函数
# create_diagnostic_plots_parallel <- function(formula, model_name, data) {
#   model <- lm(formula, data = data)
#   create_diagnostic_plots(model, model_name)
# }
# 
# # 并行创建诊断图 - ≤65组
# print("正在并行创建≤65组的诊断图...")
# diagnostic_plots_results_age65 <- foreach(
#   i = 1:3,
#   .packages = c("ggplot2", "dplyr"),
#   .export = c("create_diagnostic_plots", "formula_lm0", "formula_lm1", "formula_lm2", "diagnostic_data_age65_sampled")
# ) %dopar% {
#   switch(i,
#     create_diagnostic_plots_parallel(formula_lm0, "Model 0 (≤65)", diagnostic_data_age65_sampled),
#     create_diagnostic_plots_parallel(formula_lm1, "Model 1 (≤65)", diagnostic_data_age65_sampled),
#     create_diagnostic_plots_parallel(formula_lm2, "Model 2 (≤65)", diagnostic_data_age65_sampled)
#   )
# }
# 
# # 并行创建诊断图 - >65组
# print("正在并行创建>65组的诊断图...")
# diagnostic_plots_results_age65plus <- foreach(
#   i = 1:3,
#   .packages = c("ggplot2", "dplyr"),
#   .export = c("create_diagnostic_plots", "formula_lm0", "formula_lm1", "formula_lm2", "diagnostic_data_age65plus_sampled")
# ) %dopar% {
#   switch(i,
#     create_diagnostic_plots_parallel(formula_lm0, "Model 0 (>65)", diagnostic_data_age65plus_sampled),
#     create_diagnostic_plots_parallel(formula_lm1, "Model 1 (>65)", diagnostic_data_age65plus_sampled),
#     create_diagnostic_plots_parallel(formula_lm2, "Model 2 (>65)", diagnostic_data_age65plus_sampled)
#   )
# }
# 
# # 提取结果 - ≤65组
# diagnostic_plots_model0_age65 <- diagnostic_plots_results_age65[[1]]
# diagnostic_plots_model1_age65 <- diagnostic_plots_results_age65[[2]]
# diagnostic_plots_model2_age65 <- diagnostic_plots_results_age65[[3]]
# 
# # 提取结果 - >65组
# diagnostic_plots_model0_age65plus <- diagnostic_plots_results_age65plus[[1]]
# diagnostic_plots_model1_age65plus <- diagnostic_plots_results_age65plus[[2]]
# diagnostic_plots_model2_age65plus <- diagnostic_plots_results_age65plus[[3]]
# 
# # 组合所有诊断图（按年龄组分别保存）
# all_diagnostic_plots_age65 <- grid.arrange(
#   diagnostic_plots_model0_age65$residuals_vs_fitted,
#   diagnostic_plots_model0_age65$qq_plot,
#   diagnostic_plots_model1_age65$residuals_vs_fitted,
#   diagnostic_plots_model1_age65$qq_plot,
#   diagnostic_plots_model2_age65$residuals_vs_fitted,
#   diagnostic_plots_model2_age65$qq_plot,
#   ncol = 2, nrow = 3
# )
# 
# all_diagnostic_plots_age65plus <- grid.arrange(
#   diagnostic_plots_model0_age65plus$residuals_vs_fitted,
#   diagnostic_plots_model0_age65plus$qq_plot,
#   diagnostic_plots_model1_age65plus$residuals_vs_fitted,
#   diagnostic_plots_model1_age65plus$qq_plot,
#   diagnostic_plots_model2_age65plus$residuals_vs_fitted,
#   diagnostic_plots_model2_age65plus$qq_plot,
#   ncol = 2, nrow = 3
# )
# 
# # 保存诊断图
# ggsave(
#   filename = path_result("linear_model_mvpa_diagnostics_age65.png"),
#   plot = all_diagnostic_plots_age65,
#   width = 12, height = 15, dpi = 300
# )
# 
# ggsave(
#   filename = path_result("linear_model_mvpa_diagnostics_age65plus.png"),
#   plot = all_diagnostic_plots_age65plus,
#   width = 12, height = 15, dpi = 300
# )
# 
# print("诊断图已保存到: linear_model_mvpa_diagnostics_age65.png 和 linear_model_mvpa_diagnostics_age65plus.png")
# 
# # 7. 模型诊断统计信息
# # ==========================================
# 
# print("步骤 J: 计算模型诊断统计信息...")
# 
# # 计算诊断统计 - ≤65组
# diagnostic_stats_age65 <- data.frame(
#   Age_Group = "≤65",
#   Model = c("Model 0", "Model 1", "Model 2"),
#   R_squared = c(
#     summary(lm(formula_lm0, data = diagnostic_data_age65_sampled))$r.squared,
#     summary(lm(formula_lm1, data = diagnostic_data_age65_sampled))$r.squared,
#     summary(lm(formula_lm2, data = diagnostic_data_age65_sampled))$r.squared
#   ),
#   Adjusted_R_squared = c(
#     summary(lm(formula_lm0, data = diagnostic_data_age65_sampled))$adj.r.squared,
#     summary(lm(formula_lm1, data = diagnostic_data_age65_sampled))$adj.r.squared,
#     summary(lm(formula_lm2, data = diagnostic_data_age65_sampled))$adj.r.squared
#   ),
#   AIC = c(
#     AIC(lm(formula_lm0, data = diagnostic_data_age65_sampled)),
#     AIC(lm(formula_lm1, data = diagnostic_data_age65_sampled)),
#     AIC(lm(formula_lm2, data = diagnostic_data_age65_sampled))
#   ),
#   BIC = c(
#     BIC(lm(formula_lm0, data = diagnostic_data_age65_sampled)),
#     BIC(lm(formula_lm1, data = diagnostic_data_age65_sampled)),
#     BIC(lm(formula_lm2, data = diagnostic_data_age65_sampled))
#   )
# )
# 
# # 计算诊断统计 - >65组
# diagnostic_stats_age65plus <- data.frame(
#   Age_Group = ">65",
#   Model = c("Model 0", "Model 1", "Model 2"),
#   R_squared = c(
#     summary(lm(formula_lm0, data = diagnostic_data_age65plus_sampled))$r.squared,
#     summary(lm(formula_lm1, data = diagnostic_data_age65plus_sampled))$r.squared,
#     summary(lm(formula_lm2, data = diagnostic_data_age65plus_sampled))$r.squared
#   ),
#   Adjusted_R_squared = c(
#     summary(lm(formula_lm0, data = diagnostic_data_age65plus_sampled))$adj.r.squared,
#     summary(lm(formula_lm1, data = diagnostic_data_age65plus_sampled))$adj.r.squared,
#     summary(lm(formula_lm2, data = diagnostic_data_age65plus_sampled))$adj.r.squared
#   ),
#   AIC = c(
#     AIC(lm(formula_lm0, data = diagnostic_data_age65plus_sampled)),
#     AIC(lm(formula_lm1, data = diagnostic_data_age65plus_sampled)),
#     AIC(lm(formula_lm2, data = diagnostic_data_age65plus_sampled))
#   ),
#   BIC = c(
#     BIC(lm(formula_lm0, data = diagnostic_data_age65plus_sampled)),
#     BIC(lm(formula_lm1, data = diagnostic_data_age65plus_sampled)),
#     BIC(lm(formula_lm2, data = diagnostic_data_age65plus_sampled))
#   )
# )
# 
# # 合并两个年龄组的诊断统计
# diagnostic_stats <- bind_rows(diagnostic_stats_age65, diagnostic_stats_age65plus)
# 
# print("模型诊断统计信息:")
# print(diagnostic_stats)
# 
# # 保存诊断统计信息
# write.csv(diagnostic_stats, file = path_result("linear_model_mvpa_diagnostic_stats_age65.csv"), row.names = FALSE)

# 8. 查看与导出
# ==========================================

print("===== 结果：overall_mvpa_hours vs 认知变化 (线性模型) =====")
print(as.data.frame(final_report_table_lm_mvpa))

write.csv(final_report_table_lm_mvpa, file = path_result("linear_model_mvpa_results.csv"), row.names = FALSE)

print("所有结果已保存到result文件夹中。")

# 美化输出结果表格
# ==========================================
print("步骤 K: 正在美化输出结果表格...")
source("beautify_results.R")
beautify_model_results(
  path_result("linear_model_mvpa_results.csv"),
  path_result("linear_model_mvpa_results_beautified.png")
)
print("步骤 K 完成，美化表格已生成。")