# 睡眠总时长与认知变化的RCS（限制性立方样条）模型分析
# ==========================================
# 使用多重插补和Rubin法则汇总结果

# 1. 准备工作
# ==========================================
# 加载我们需要的包
library(dplyr)
library(mice)
library(miceadds)
library(broom)
library(ggplot2)
library(gridExtra)
library(parallel)  # 仅用于detectCores()检测CPU核心数（用于插补）
library(rms)  # 用于RCS模型
library(splines)  # 用于样条函数
library(emmeans)  # 用于估计边际均值
source("paths.R")

# 2. 最终数据整合
# ==========================================

# 步骤 A: 清理并准备睡眠、MVPA和认知得分数据
print("步骤 A: 正在准备睡眠、MVPA和认知得分数据...")
sleep_clean <- Derived_accelerometry %>%
  transmute(
    Participant_ID = `Participant ID`,
    sleep_hours_per_day = `Sleep - Overall average | Instance 0` * 24  # 将占比转换为每天睡眠小时数
  )

mvpa_clean <- Derived_accelerometry %>%
  transmute(
    Participant_ID = `Participant ID`,
    MVPA_hours_per_day = `Moderate-Vigorous - Overall average | Instance 0` * 24  # 转换为每天MVPA小时数
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
  left_join(sleep_clean, by = "Participant_ID") %>%
  left_join(mvpa_clean, by = "Participant_ID") %>%
  left_join(cognitive_score_clean, by = "Participant_ID")
print("步骤 B 完成，`full_merged_table` 已创建。")

# 3. 多重插补 (Impute)
# ==========================================

# 步骤 C: 定义我们最终模型需要的所有变量
all_model_vars <- c(
  "Participant_ID", 
  "cognitive_change",  # <--- 结局变量
  "sleep_hours_per_day",  # <--- 核心自变量1
  "MVPA_hours_per_day",  # <--- 核心自变量2（用于交互作用分析）
  "age_recruitment", "sex", "townsend_index", "bmi", 
  "smoking_status", "alcohol_status", "education_level", "cvd_history_any", 
  "history_diabetes", "cognitive_score_0" # 协变量列表
)

# 步骤 D: 从完整数据中，只挑选出用于插补和模型分析的变量
imputation_data <- full_merged_table %>%
  select(all_of(all_model_vars))

# 步骤 D1: 剔除结局变量和暴露变量的缺失值
print("步骤 D1: 正在剔除结局变量和暴露变量的缺失值...")
n_before_exclusion <- nrow(imputation_data)
n_missing_outcome <- sum(is.na(imputation_data$cognitive_change))
n_missing_sleep <- sum(is.na(imputation_data$sleep_hours_per_day))
n_missing_mvpa <- sum(is.na(imputation_data$MVPA_hours_per_day))
n_missing_either <- sum(is.na(imputation_data$cognitive_change) | 
                        is.na(imputation_data$sleep_hours_per_day) | 
                        is.na(imputation_data$MVPA_hours_per_day))

print(paste("剔除前样本数:", n_before_exclusion))
print(paste("cognitive_change 缺失数:", n_missing_outcome, "(", round(n_missing_outcome/n_before_exclusion * 100, 2), "%)"))
print(paste("sleep_hours_per_day 缺失数:", n_missing_sleep, "(", round(n_missing_sleep/n_before_exclusion * 100, 2), "%)"))
print(paste("MVPA_hours_per_day 缺失数:", n_missing_mvpa, "(", round(n_missing_mvpa/n_before_exclusion * 100, 2), "%)"))
print(paste("至少一个缺失的数量:", n_missing_either, "(", round(n_missing_either/n_before_exclusion * 100, 2), "%)"))

# 剔除缺失值
imputation_data <- imputation_data %>%
  filter(!is.na(cognitive_change) & 
         !is.na(sleep_hours_per_day) & 
         !is.na(MVPA_hours_per_day))

n_after_missing_exclusion <- nrow(imputation_data)
n_excluded_missing <- n_before_exclusion - n_after_missing_exclusion
print(paste("剔除缺失值后样本数:", n_after_missing_exclusion))
print(paste("剔除缺失值样本数:", n_excluded_missing, "(", round(n_excluded_missing/n_before_exclusion * 100, 2), "%)"))

# 步骤 D1.1: 剔除睡眠时间异常值（限定于3-10小时）
print("\n步骤 D1.1: 正在剔除睡眠时间异常值（限定于3-10小时）...")
n_before_sleep_filter <- nrow(imputation_data)
n_sleep_outlier_low <- sum(imputation_data$sleep_hours_per_day < 3, na.rm = TRUE)
n_sleep_outlier_high <- sum(imputation_data$sleep_hours_per_day > 10, na.rm = TRUE)
n_sleep_outlier_total <- n_sleep_outlier_low + n_sleep_outlier_high

print(paste("睡眠时间 < 3小时的数量:", n_sleep_outlier_low, "(", round(n_sleep_outlier_low/n_before_sleep_filter * 100, 2), "%)"))
print(paste("睡眠时间 > 10小时的数量:", n_sleep_outlier_high, "(", round(n_sleep_outlier_high/n_before_sleep_filter * 100, 2), "%)"))
print(paste("睡眠时间异常值总数:", n_sleep_outlier_total, "(", round(n_sleep_outlier_total/n_before_sleep_filter * 100, 2), "%)"))

# 剔除睡眠时间异常值（保留3-10小时范围内的值）
imputation_data <- imputation_data %>%
  filter(sleep_hours_per_day >= 3 & sleep_hours_per_day <= 10)

n_after_exclusion <- nrow(imputation_data)
n_excluded_sleep <- n_before_sleep_filter - n_after_exclusion
print(paste("剔除睡眠异常值后样本数:", n_after_exclusion))
print(paste("剔除睡眠异常值样本数:", n_excluded_sleep, "(", round(n_excluded_sleep/n_before_sleep_filter * 100, 2), "%)"))
print("步骤 D1.1 完成。")

n_excluded <- n_before_exclusion - n_after_exclusion
print(paste("\n总剔除样本数:", n_excluded, "(", round(n_excluded/n_before_exclusion * 100, 2), "%)"))
print("步骤 D1 完成。")

# 打印最终纳入分析的样本量
print("\n===== 最终纳入分析的样本量 =====")
print(paste("最终分析样本数:", n_after_exclusion))

# 检查睡眠时长区间分布
print("\n===== 睡眠时长区间分布统计 =====")
sleep_distribution <- imputation_data %>%
  summarise(
    total_n = n(),
    sleep_lt_7 = sum(sleep_hours_per_day < 7, na.rm = TRUE),
    sleep_7_to_9 = sum(sleep_hours_per_day >= 7 & sleep_hours_per_day <= 9, na.rm = TRUE),
    sleep_gt_9 = sum(sleep_hours_per_day > 9, na.rm = TRUE),
    sleep_lt_7_pct = round(sleep_lt_7 / total_n * 100, 2),
    sleep_7_to_9_pct = round(sleep_7_to_9 / total_n * 100, 2),
    sleep_gt_9_pct = round(sleep_gt_9 / total_n * 100, 2)
  )

print(paste("总样本数:", sleep_distribution$total_n))
print(paste("睡眠时长 < 7小时:", sleep_distribution$sleep_lt_7, "(", sleep_distribution$sleep_lt_7_pct, "%)"))
print(paste("睡眠时长 7-9小时:", sleep_distribution$sleep_7_to_9, "(", sleep_distribution$sleep_7_to_9_pct, "%)"))
print(paste("睡眠时长 > 9小时:", sleep_distribution$sleep_gt_9, "(", sleep_distribution$sleep_gt_9_pct, "%)"))

# 更详细的分布统计
print("\n睡眠时长详细分布:")
sleep_quantiles <- quantile(imputation_data$sleep_hours_per_day, 
                            probs = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1), 
                            na.rm = TRUE)
print(round(sleep_quantiles, 2))

# 准备用于插补的数据（移除Participant_ID）
imputation_data_for_mice <- imputation_data %>%
  select(-Participant_ID)

# 步骤 E: 执行多重插补 [优化版本，支持并行处理]
print("步骤 E: 开始执行多重插补，请耐心等待...")
print("注意：cognitive_change、sleep_hours_per_day 和 MVPA_hours_per_day 不参与插补（已剔除缺失值）")

# 检测可用CPU核心数，设置并行处理
n_cores <- detectCores() - 1  # 保留一个核心给系统
if (n_cores < 1) n_cores <- 1

print(paste("检测到", n_cores, "个CPU核心可用于并行处理"))

# 为每个变量明确指定插补方法
# 注意：cognitive_change、sleep_hours_per_day 和 MVPA_hours_per_day 设置为 ""，表示不插补
method_specification <- c(
  "cognitive_change" = "",  # 不插补，已剔除缺失值
  "sleep_hours_per_day" = "",  # 不插补，已剔除缺失值
  "MVPA_hours_per_day" = "",  # 不插补，已剔除缺失值
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
  maxit = 10,       # 迭代次数
  printFlag = TRUE, # 显示进度
  n.core = n_cores, # 并行核心数
  ridge = 1e-05,    # 提高数值稳定性
  threshold = 1.0   # 优化收敛
)

print("步骤 E 完成，多重插补已生成！")

# 4. 在每个插补数据集上运行RCS模型分析 (Analyze)
# ==========================================

# 步骤 F: 计算固定的样条结点位置（关键修复：确保所有插补数据集使用相同的结点）
# ==========================================
print("步骤 F: 正在计算固定的样条结点位置...")

# 使用插补前的完整数据集计算固定的结点位置
# 这样可以确保所有插补数据集使用相同的样条基函数，使Rubin合并合法
fixed_spline_basis <- ns(imputation_data$sleep_hours_per_day, df = 4)

# 提取结点位置（用于后续创建相同的样条基函数）
# ns()函数使用分位数作为内部结点，我们需要保存这些信息
# 通过创建一个临时样条对象，我们可以获取其属性
fixed_knots <- attr(fixed_spline_basis, "knots")
fixed_boundary_knots <- attr(fixed_spline_basis, "Boundary.knots")

print(paste("固定边界结点:", round(fixed_boundary_knots[1], 3), "和", round(fixed_boundary_knots[2], 3)))
print(paste("固定内部结点:", paste(round(fixed_knots, 3), collapse = ", ")))

# 创建一个函数，使用固定结点创建样条基函数
create_fixed_spline <- function(x) {
  ns(x, knots = fixed_knots, Boundary.knots = fixed_boundary_knots)
}

print("步骤 F 完成，固定结点位置已确定。")

# 步骤 F1: 定义RCS模型的公式
# ==========================================
# 注意：现在我们需要手动创建样条基函数列，而不是在公式中使用ns()
# 这样可以确保所有插补数据集使用相同的结点位置

all_covariates <- c(
  "age_recruitment", "sex", "townsend_index", "bmi", "smoking_status",
  "alcohol_status", "education_level", "cvd_history_any", 
  "history_diabetes", "cognitive_score_0"
)

# 定义模型：
# 第一部分：RCS主效应模型（原有分析）
# Model 0: 仅RCS暴露变量
# Model 1: RCS暴露变量 + 基本人口学变量
# Model 2: RCS暴露变量 + 所有协变量

# 第二部分：交互作用模型（新增分析）
# Model 3: RCS睡眠 × MVPA交互 + 所有协变量

# 步骤 G: 运行RCS模型和交互作用模型
print("步骤 G: 正在对插补数据集运行RCS模型和交互作用模型（共4个模型）...")
print("注意：所有插补数据集使用相同的固定样条结点位置，确保Rubin合并合法")

# 定义拟合函数（使用固定的样条基函数）
fit_rcs_models <- function(i) {
  completed_data <- complete(imputed_object, i)
  
  # 使用固定结点创建样条基函数列
  sleep_spline_basis <- create_fixed_spline(completed_data$sleep_hours_per_day)
  sleep_spline_cols <- as.data.frame(sleep_spline_basis)
  names(sleep_spline_cols) <- paste0("sleep_spline_", seq_len(ncol(sleep_spline_cols)))
  
  # 将样条列添加到数据中
  data_with_spline <- cbind(completed_data, sleep_spline_cols)
  
  # 构建模型公式（使用样条列而不是ns()函数）
  formula_rcs0 <- as.formula(
    paste("cognitive_change ~", paste(names(sleep_spline_cols), collapse = " + "))
  )
  
  formula_rcs1 <- as.formula(
    paste("cognitive_change ~", paste(names(sleep_spline_cols), collapse = " + "), 
          "+ age_recruitment + sex")
  )
  
  formula_rcs2 <- as.formula(
    paste("cognitive_change ~", paste(names(sleep_spline_cols), collapse = " + "), "+",
          paste(all_covariates, collapse = " + "))
  )
  
  # 交互作用模型：需要创建交互项
  interaction_terms <- paste(paste0(names(sleep_spline_cols), ":MVPA_hours_per_day"), collapse = " + ")
  formula_interaction <- as.formula(
    paste("cognitive_change ~", paste(names(sleep_spline_cols), collapse = " + "), 
          "+ MVPA_hours_per_day +", interaction_terms, "+",
          paste(all_covariates, collapse = " + "))
  )
  
  # 用于P-overall检验的模型：只有协变量，没有睡眠项
  formula_covariates_only <- as.formula(
    paste("cognitive_change ~", paste(all_covariates, collapse = " + "))
  )
  
  # 用于P-nonlinear检验的模型：协变量 + 睡眠线性项
  formula_linear_sleep <- as.formula(
    paste("cognitive_change ~ sleep_hours_per_day +", paste(all_covariates, collapse = " + "))
  )
  
  list(
    model0 = lm(formula_rcs0, data = data_with_spline),
    model1 = lm(formula_rcs1, data = data_with_spline),
    model2 = lm(formula_rcs2, data = data_with_spline),
    model_interaction = lm(formula_interaction, data = data_with_spline),  # 交互作用模型
    model_covariates_only = lm(formula_covariates_only, data = data_with_spline),  # 用于P-overall
    model_linear_sleep = lm(formula_linear_sleep, data = data_with_spline)  # 用于P-nonlinear
  )
}

# 使用lapply串行处理
model_results <- lapply(1:imputed_object$m, fit_rcs_models)

# 提取模型列表
rcs_model0_list <- lapply(model_results, function(x) x$model0)
rcs_model1_list <- lapply(model_results, function(x) x$model1)
rcs_model2_list <- lapply(model_results, function(x) x$model2)
interaction_model_list <- lapply(model_results, function(x) x$model_interaction)
covariates_only_list <- lapply(model_results, function(x) x$model_covariates_only)
linear_sleep_list <- lapply(model_results, function(x) x$model_linear_sleep)

print("步骤 G 完成！")

# 5. 汇总所有分析结果 (Pool) 并展示
# ==========================================

# 步骤 H: 使用 pool() 函数分别汇总三个模型的结果（Rubin法则）
print("步骤 H: 正在使用Rubin法则汇总RCS模型结果...")

pooled_rcs0 <- pool(rcs_model0_list)
pooled_rcs1 <- pool(rcs_model1_list)
pooled_rcs2 <- pool(rcs_model2_list)
pooled_interaction <- pool(interaction_model_list)

# 使用 broom::tidy() 函数来提取每个模型的结果
rcs0_tidy <- tidy(pooled_rcs0, conf.int = TRUE)
rcs1_tidy <- tidy(pooled_rcs1, conf.int = TRUE)
rcs2_tidy <- tidy(pooled_rcs2, conf.int = TRUE)
interaction_tidy <- tidy(pooled_interaction, conf.int = TRUE)

# 输出交互项的系数和符号
print("\n===== 交互项系数（Model 3: Sleep × MVPA Interaction）=====")
interaction_terms <- interaction_tidy %>%
  filter(grepl("sleep_spline_.*:MVPA_hours_per_day|MVPA_hours_per_day:sleep_spline_", term))

if (nrow(interaction_terms) > 0) {
  print("交互项系数详情:")
  print(interaction_terms %>%
    select(term, estimate, conf.low, conf.high, p.value) %>%
    mutate(
      across(c(estimate, conf.low, conf.high), ~round(.x, 4)),
      p.value = if_else(p.value < 0.001, "<0.001", as.character(round(p.value, 4)))
    ))
  
  print("\n交互项系数符号:")
  for (i in seq_len(nrow(interaction_terms))) {
    coef_sign <- ifelse(interaction_terms$estimate[i] > 0, "正", "负")
    coef_value <- round(interaction_terms$estimate[i], 4)
    p_val <- ifelse(interaction_terms$p.value[i] < 0.001, "<0.001", 
                    round(interaction_terms$p.value[i], 4))
    print(paste0("  ", interaction_terms$term[i], ": ", coef_sign, " (", coef_value, 
                 ", P=", p_val, ")"))
  }
} else {
  print("未找到交互项，请检查模型公式。")
  print("所有系数名称:")
  print(interaction_tidy$term)
}

rcs0_tidy$model <- "Model 0 (Unadjusted)"
rcs1_tidy$model <- "Model 1 (Adjusted for Demographics)"
rcs2_tidy$model <- "Model 2 (Fully Adjusted)"
interaction_tidy$model <- "Model 3 (Sleep × MVPA Interaction)"

all_models_summary_rcs_sleep <- bind_rows(rcs0_tidy, rcs1_tidy, rcs2_tidy, interaction_tidy)

final_report_table_rcs_sleep <- all_models_summary_rcs_sleep %>%
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

# 6. 查看与导出
# ==========================================

print("===== 结果：sleep_hours_per_day vs 认知变化 (RCS模型) =====")
print(as.data.frame(final_report_table_rcs_sleep))

write.csv(final_report_table_rcs_sleep, file = path_result("rcs_model_sleep_results.csv"), row.names = FALSE)

print("所有结果已保存到result文件夹中。")

# 步骤 H1: P-overall 和 P-nonlinear 检验
# ==========================================
print("\n步骤 H1: 正在进行P-overall和P-nonlinear检验...")

# 函数：计算似然比检验统计量（对于线性模型，使用F检验）
compute_lrt <- function(model_reduced, model_full) {
  # 对于线性模型，使用anova()进行F检验
  anova_result <- anova(model_reduced, model_full)
  
  # 提取F统计量和P值
  # anova()返回的结果中，第二行是完整模型相对于简化模型的检验
  if (nrow(anova_result) >= 2) {
    # 使用更稳健的方式提取列（处理列名中的特殊字符）
    f_stat <- anova_result[2, "F"]
    p_value <- anova_result[2, "Pr(>F)"]
    df_diff <- anova_result[2, "Df"]
    
    # 如果F统计量为NA，尝试手动计算F统计量
    if (is.na(f_stat) || is.na(p_value) || is.na(df_diff)) {
      # 使用残差平方和计算
      rss_reduced <- sum(residuals(model_reduced)^2)
      rss_full <- sum(residuals(model_full)^2)
      df_full <- df.residual(model_full)
      df_reduced <- df.residual(model_reduced)
      
      # 计算F统计量
      # F = [(RSS_reduced - RSS_full) / (df_reduced - df_full)] / [RSS_full / df_full]
      if (df_reduced > df_full && df_full > 0 && (rss_reduced - rss_full) > 0) {
        f_stat <- ((rss_reduced - rss_full) / (df_reduced - df_full)) / (rss_full / df_full)
        p_value <- pf(f_stat, df_reduced - df_full, df_full, lower.tail = FALSE)
        df_diff <- df_reduced - df_full
      } else {
        f_stat <- NA
        p_value <- NA
        df_diff <- NA
      }
    }
    
    return(list(
      f_stat = f_stat,
      p_value = p_value,
      df = df_diff
    ))
  } else {
    return(list(
      f_stat = NA,
      p_value = NA,
      df = NA
    ))
  }
}

# 对每个插补数据集进行P-overall检验
# 比较：模型A（只有协变量）vs 模型B（协变量 + 所有sleep_spline项）
print("正在进行P-overall检验（睡眠整体效应）...")
p_overall_results <- lapply(seq_along(covariates_only_list), function(i) {
  model_reduced <- covariates_only_list[[i]]
  model_full <- rcs_model2_list[[i]]
  lrt_result <- compute_lrt(model_reduced, model_full)
  return(lrt_result)
})

# 提取F统计量和P值
p_overall_f_stats <- sapply(p_overall_results, function(x) x$f_stat)
p_overall_p_values <- sapply(p_overall_results, function(x) x$p_value)
p_overall_df <- sapply(p_overall_results, function(x) x$df)

# 使用Rubin法则合并P-overall检验结果
# 对于F检验，使用Rubin法则合并F统计量
# 方法：计算within-imputation和between-imputation方差，然后合并
p_overall_f_mean <- mean(p_overall_f_stats, na.rm = TRUE)
p_overall_f_within_var <- var(p_overall_f_stats, na.rm = TRUE)  # 实际上这是总方差
p_overall_df_mean <- mean(p_overall_df, na.rm = TRUE)

# 对于F检验，使用更简单但有效的方法：
# 1. 平均F统计量
# 2. 使用平均自由度计算P值
# 注意：对于多重插补的F检验，更精确的方法需要考虑between-imputation方差
# 但这里使用平均F统计量是常见且合理的近似方法
if (!is.na(p_overall_f_mean) && !is.na(p_overall_df_mean) && p_overall_df_mean > 0) {
  # 获取残差自由度（使用第一个模型的自由度作为近似）
  df_residual <- df.residual(rcs_model2_list[[1]])
  # 使用F分布计算P值
  p_overall_pooled <- pf(p_overall_f_mean, p_overall_df_mean, df_residual, lower.tail = FALSE)
} else {
  # 如果F统计量计算失败，使用P值的平均值（备选方法）
  p_overall_pooled <- mean(p_overall_p_values, na.rm = TRUE)
}

# 对每个插补数据集进行P-nonlinear检验
# 比较：模型C（协变量 + 睡眠线性项）vs 模型D（协变量 + 睡眠样条项）
print("正在进行P-nonlinear检验（非线性效应）...")
p_nonlinear_results <- lapply(seq_along(linear_sleep_list), function(i) {
  model_reduced <- linear_sleep_list[[i]]
  model_full <- rcs_model2_list[[i]]
  lrt_result <- compute_lrt(model_reduced, model_full)
  return(lrt_result)
})

# 提取F统计量和P值
p_nonlinear_f_stats <- sapply(p_nonlinear_results, function(x) x$f_stat)
p_nonlinear_p_values <- sapply(p_nonlinear_results, function(x) x$p_value)
p_nonlinear_df <- sapply(p_nonlinear_results, function(x) x$df)

# 使用Rubin法则合并P-nonlinear检验结果
p_nonlinear_f_mean <- mean(p_nonlinear_f_stats, na.rm = TRUE)
p_nonlinear_df_mean <- mean(p_nonlinear_df, na.rm = TRUE)

# 计算合并后的P值
if (!is.na(p_nonlinear_f_mean) && !is.na(p_nonlinear_df_mean) && p_nonlinear_df_mean > 0) {
  # 获取残差自由度（使用第一个模型的自由度作为近似）
  df_residual <- df.residual(rcs_model2_list[[1]])
  # 使用F分布计算P值
  p_nonlinear_pooled <- pf(p_nonlinear_f_mean, p_nonlinear_df_mean, df_residual, lower.tail = FALSE)
} else {
  # 如果F统计量计算失败，使用P值的平均值（备选方法）
  p_nonlinear_pooled <- mean(p_nonlinear_p_values, na.rm = TRUE)
}

# 对每个插补数据集进行P-interaction检验
# 比较：模型2（没有交互项）vs 模型3（含交互项）
print("正在进行P-interaction检验（MVPA × 睡眠交互整体效应）...")
p_interaction_results <- lapply(seq_along(interaction_model_list), function(i) {
  model_reduced <- rcs_model2_list[[i]]       # 没有交互项
  model_full <- interaction_model_list[[i]]   # 含交互项
  lrt_result <- compute_lrt(model_reduced, model_full)
  return(lrt_result)
})

# 提取F统计量和P值
p_interaction_f_stats <- sapply(p_interaction_results, function(x) x$f_stat)
p_interaction_p_values <- sapply(p_interaction_results, function(x) x$p_value)
p_interaction_df <- sapply(p_interaction_results, function(x) x$df)

# 使用Rubin法则合并P-interaction检验结果
p_interaction_f_mean <- mean(p_interaction_f_stats, na.rm = TRUE)
p_interaction_df_mean <- mean(p_interaction_df, na.rm = TRUE)

# 计算合并后的P值
if (!is.na(p_interaction_f_mean) && !is.na(p_interaction_df_mean) && p_interaction_df_mean > 0) {
  # 获取残差自由度（使用交互模型的平均自由度）
  df_residual_interaction <- mean(sapply(interaction_model_list, df.residual), na.rm = TRUE)
  # 使用F分布计算P值
  p_interaction_pooled <- pf(p_interaction_f_mean, p_interaction_df_mean, df_residual_interaction, lower.tail = FALSE)
} else {
  # 如果F统计量计算失败，使用P值的平均值（备选方法）
  p_interaction_pooled <- mean(p_interaction_p_values, na.rm = TRUE)
}

# 输出结果
print("\n===== P-overall、P-nonlinear 和 P-interaction 检验结果 =====")
print(paste("P-overall (睡眠整体效应):", 
            ifelse(p_overall_pooled < 0.001, "<0.001", 
                   round(p_overall_pooled, 4)),
            "| F统计量:", round(p_overall_f_mean, 3),
            "| 自由度:", round(p_overall_df_mean, 1)))
print(paste("P-nonlinear (非线性效应):", 
            ifelse(p_nonlinear_pooled < 0.001, "<0.001", 
                   round(p_nonlinear_pooled, 4)),
            "| F统计量:", round(p_nonlinear_f_mean, 3),
            "| 自由度:", round(p_nonlinear_df_mean, 1)))
print(paste("P-interaction (MVPA × 睡眠交互效应):", 
            ifelse(p_interaction_pooled < 0.001, "<0.001", 
                   round(p_interaction_pooled, 4)),
            "| F统计量:", round(p_interaction_f_mean, 3),
            "| 自由度:", round(p_interaction_df_mean, 1)))

# 创建结果表格
p_test_results <- data.frame(
  Test = c("P-overall (睡眠整体效应)", "P-nonlinear (非线性效应)", "P-interaction (MVPA × 睡眠交互效应)"),
  F_Statistic = c(round(p_overall_f_mean, 3), round(p_nonlinear_f_mean, 3), round(p_interaction_f_mean, 3)),
  DF = c(round(p_overall_df_mean, 1), round(p_nonlinear_df_mean, 1), round(p_interaction_df_mean, 1)),
  P_Value = c(
    ifelse(p_overall_pooled < 0.001, "<0.001", round(p_overall_pooled, 4)),
    ifelse(p_nonlinear_pooled < 0.001, "<0.001", round(p_nonlinear_pooled, 4)),
    ifelse(p_interaction_pooled < 0.001, "<0.001", round(p_interaction_pooled, 4))
  ),
  Interpretation = c(
    ifelse(p_overall_pooled < 0.05, 
           "睡眠与认知变化显著相关", 
           "睡眠与认知变化无显著相关"),
    ifelse(p_nonlinear_pooled < 0.05, 
           "睡眠与认知变化的关系显著非线性", 
           "睡眠与认知变化的关系可能是线性的"),
    ifelse(p_interaction_pooled < 0.05, 
           "MVPA显著调节睡眠对认知变化的影响", 
           "MVPA不显著调节睡眠对认知变化的影响")
  )
)

print("\n检验结果表格:")
print(p_test_results)

# 保存结果
write.csv(p_test_results, 
          file = path_result("rcs_model_p_overall_nonlinear_interaction.csv"), 
          row.names = FALSE)

print("\nP-overall、P-nonlinear和P-interaction检验结果已保存到: rcs_model_p_overall_nonlinear_interaction.csv")
print("步骤 H1 完成。")

# 美化输出结果表格
# ==========================================
print("步骤 I: 正在美化输出结果表格...")
source("beautify_results.R")
beautify_model_results(
  path_result("rcs_model_sleep_results.csv"),
  path_result("rcs_model_sleep_results_beautified.png")
)
print("步骤 I 完成，美化表格已生成。")

# 7. RCS曲线可视化
# ==========================================
print("步骤 J: 正在生成RCS曲线可视化...")

# 获取第一个插补数据集用于计算参考点和预测值
reference_data <- complete(imputed_object, 1)

# 计算睡眠时长的参考点（通常使用中位数）
sleep_reference <- median(reference_data$sleep_hours_per_day, na.rm = TRUE)
print(paste("使用睡眠时长中位数作为参考点:", round(sleep_reference, 2), "小时/天"))

# 创建睡眠时长的序列（用于绘制曲线）
# 注意：数据已过滤到3-10小时范围内，所以分位数自然在此范围内
sleep_range <- seq(
  quantile(reference_data$sleep_hours_per_day, 0.01, na.rm = TRUE),
  quantile(reference_data$sleep_hours_per_day, 0.99, na.rm = TRUE),
  length.out = 200
)

# 函数：手动计算合并后的方差-协方差矩阵（使用Rubin法则）
compute_pooled_vcov <- function(model_list) {
  # 获取所有模型的系数和方差-协方差矩阵
  n_models <- length(model_list)
  n_coef <- length(coef(model_list[[1]]))
  coef_names <- names(coef(model_list[[1]]))
  
  # 存储每个模型的系数向量和方差-协方差矩阵
  coef_matrix <- matrix(NA, nrow = n_models, ncol = n_coef)
  vcov_list <- vector("list", n_models)
  
  for (i in seq_len(n_models)) {
    coef_matrix[i, ] <- coef(model_list[[i]])
    vcov_list[[i]] <- vcov(model_list[[i]])
  }
  
  # 计算 within-imputation 方差（每个插补数据集内方差的平均值）
  within_vcov <- Reduce("+", vcov_list) / n_models
  
  # 计算 between-imputation 方差（系数估计值之间的协方差）
  coef_mean <- colMeans(coef_matrix)
  between_vcov <- matrix(0, nrow = n_coef, ncol = n_coef)
  for (i in seq_len(n_models)) {
    coef_diff <- coef_matrix[i, ] - coef_mean
    between_vcov <- between_vcov + outer(coef_diff, coef_diff)
  }
  between_vcov <- between_vcov / (n_models - 1)
  
  # Rubin法则：合并后的方差-协方差矩阵
  # Total variance = within + (1 + 1/m) * between
  pooled_vcov <- within_vcov + (1 + 1/n_models) * between_vcov
  
  # 设置行名和列名
  rownames(pooled_vcov) <- coef_names
  colnames(pooled_vcov) <- coef_names
  
  return(pooled_vcov)
}

# 函数：从pooled模型提取系数并计算预测值（使用完整的方差-协方差矩阵）
predict_rcs_curve <- function(pooled_model, model_list, sleep_values, reference_value, fixed_knots, fixed_boundary_knots) {
  # 使用tidy()提取系数信息（格式更清晰）
  pooled_tidy <- tidy(pooled_model, conf.int = FALSE)
  
  # 获取系数名称和值
  coef_names <- pooled_tidy$term
  pooled_coef <- setNames(pooled_tidy$estimate, coef_names)
  
  # 关键修复：手动计算合并后的方差-协方差矩阵（使用Rubin法则）
  pooled_vcov_matrix <- compute_pooled_vcov(model_list)
  
  # 打印系数名称用于调试
  print("系数名称:")
  print(coef_names)
  
  # 找到样条项的索引 - 现在样条项名称是 sleep_spline_1, sleep_spline_2, 等
  spline_coef_indices <- grep("^sleep_spline_", coef_names)
  
  if (length(spline_coef_indices) == 0) {
    print("所有系数名称:")
    print(coef_names)
    stop("未找到样条项系数。请检查系数名称格式。")
  }
  
  print(paste("找到", length(spline_coef_indices), "个样条项系数:"))
  print(coef_names[spline_coef_indices])
  
  # 提取样条项的方差-协方差子矩阵
  spline_vcov <- pooled_vcov_matrix[spline_coef_indices, spline_coef_indices, drop = FALSE]
  
  # 使用固定结点创建样条基函数对象
  create_fixed_spline <- function(x) {
    ns(x, knots = fixed_knots, Boundary.knots = fixed_boundary_knots)
  }
  
  # 创建一个临时样条对象用于预测
  temp_basis <- create_fixed_spline(c(reference_value, sleep_values))
  
  # 计算参考点的样条基函数
  reference_basis_matrix <- predict(temp_basis, newx = reference_value)
  reference_basis <- as.numeric(reference_basis_matrix)
  
  # 确保样条项数量匹配
  if (length(spline_coef_indices) != length(reference_basis)) {
    warning(paste("样条项数量不匹配：找到", length(spline_coef_indices), "个系数，但样条基函数有", length(reference_basis), "列"))
    # 如果数量不匹配，尝试只使用前几个
    if (length(spline_coef_indices) > length(reference_basis)) {
      spline_coef_indices <- spline_coef_indices[seq_len(length(reference_basis))]
      spline_vcov <- spline_vcov[seq_len(length(reference_basis)), seq_len(length(reference_basis)), drop = FALSE]
    } else {
      reference_basis <- reference_basis[seq_len(length(spline_coef_indices))]
    }
  }
  
  # 提取样条项系数
  spline_coef_values <- pooled_coef[spline_coef_indices]
  
  # 计算预测值（相对于参考点）
  predictions <- numeric(length(sleep_values))
  se_values <- numeric(length(sleep_values))
  
  for (i in seq_along(sleep_values)) {
    # 计算当前睡眠时长的样条基函数
    current_basis_matrix <- predict(temp_basis, newx = sleep_values[i])
    current_basis <- as.numeric(current_basis_matrix)
    
    # 确保长度匹配
    if (length(current_basis) != length(reference_basis)) {
      if (length(current_basis) > length(reference_basis)) {
        current_basis <- current_basis[seq_len(length(reference_basis))]
      } else {
        reference_basis <- reference_basis[seq_len(length(current_basis))]
      }
    }
    
    # 计算样条项的差值
    spline_diff <- current_basis - reference_basis
    
    # 确保长度匹配
    if (length(spline_diff) != length(spline_coef_values)) {
      min_len <- min(length(spline_diff), length(spline_coef_values))
      spline_diff <- spline_diff[seq_len(min_len)]
      spline_coef_values <- spline_coef_values[seq_len(min_len)]
      if (nrow(spline_vcov) > min_len) {
        spline_vcov <- spline_vcov[seq_len(min_len), seq_len(min_len), drop = FALSE]
      }
    }
    
    # 计算预测值
    predictions[i] <- sum(spline_diff * spline_coef_values)
    
    # 关键修复：使用完整的方差-协方差矩阵计算标准误
    # Var(a' * beta) = a' * Var(beta) * a，其中a是样条基函数差值向量
    se_values[i] <- sqrt(as.numeric(t(spline_diff) %*% spline_vcov %*% spline_diff))
  }
  
  # 计算置信区间
  ci_lower <- predictions - 1.96 * se_values
  ci_upper <- predictions + 1.96 * se_values
  
  return(data.frame(
    sleep_hours = sleep_values,
    prediction = predictions,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  ))
}

# 为每个模型生成预测曲线
print("正在计算Model 2的RCS曲线...")
rcs_curve_model2 <- predict_rcs_curve(
  pooled_rcs2, 
  rcs_model2_list,  # 传入模型列表用于计算方差-协方差矩阵
  sleep_range, 
  sleep_reference, 
  fixed_knots,
  fixed_boundary_knots
)

# 检查置信区间的范围
print("置信区间统计:")
print(paste("预测值范围:", round(min(rcs_curve_model2$prediction, na.rm = TRUE), 3), 
            "到", round(max(rcs_curve_model2$prediction, na.rm = TRUE), 3)))
print(paste("置信区间宽度（平均）:", round(mean(rcs_curve_model2$ci_upper - rcs_curve_model2$ci_lower, na.rm = TRUE), 3)))
print(paste("置信区间宽度（最小）:", round(min(rcs_curve_model2$ci_upper - rcs_curve_model2$ci_lower, na.rm = TRUE), 3)))
print(paste("置信区间宽度（最大）:", round(max(rcs_curve_model2$ci_upper - rcs_curve_model2$ci_lower, na.rm = TRUE), 3)))

# 绘制RCS曲线图
rcs_plot <- ggplot(rcs_curve_model2, aes(x = sleep_hours)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
  # 阴影区域（95%置信区间）- 使用更明显的颜色和透明度
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
              alpha = 0.3, 
              fill = "lightblue", 
              color = NA) +
  # 主曲线
  geom_line(aes(y = prediction), color = "steelblue", size = 1.2) +
  # 参考点线
  geom_vline(xintercept = sleep_reference, linetype = "dashed", color = "red", alpha = 0.7) +
  labs(
    title = "睡眠总时长与认知变化的关系（RCS模型，Model 2）",
    subtitle = paste("参考点:", round(sleep_reference, 2), "小时/天 | 样本数:", n_after_exclusion),
    x = "睡眠总时长 (小时/天)",
    y = "认知变化预测值（相对于参考点）",
    caption = "阴影区域表示95%置信区间。参考点用红色虚线标示。"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 11),
    plot.caption = element_text(size = 9, hjust = 0)
  ) +
  xlim(quantile(reference_data$sleep_hours_per_day, 0.01, na.rm = TRUE),
       quantile(reference_data$sleep_hours_per_day, 0.99, na.rm = TRUE))

# 保存RCS曲线图
ggsave(
  filename = path_result("rcs_model_sleep_curve.png"),
  plot = rcs_plot,
  width = 10,
  height = 6,
  dpi = 300
)

print("RCS曲线图已保存到: rcs_model_sleep_curve.png")

# 显示图形
print(rcs_plot)

# 8. 交互作用可视化
# ==========================================
print("步骤 K: 正在生成交互作用可视化...")

# 计算MVPA的参考点和分位数
mvpa_reference <- median(reference_data$MVPA_hours_per_day, na.rm = TRUE)
mvpa_quantiles <- quantile(reference_data$MVPA_hours_per_day, c(0.25, 0.5, 0.75), na.rm = TRUE)
mvpa_low <- mvpa_quantiles[1]
mvpa_medium <- mvpa_quantiles[2]
mvpa_high <- mvpa_quantiles[3]

print(paste("MVPA参考点（中位数）:", round(mvpa_reference, 2), "小时/天"))
print(paste("MVPA低水平（25%分位数）:", round(mvpa_low, 2), "小时/天"))
print(paste("MVPA高水平（75%分位数）:", round(mvpa_high, 2), "小时/天"))

# 函数：计算在不同MVPA水平下，睡眠时长与认知变化的关系（使用完整的方差-协方差矩阵）
# 注意：只对齐睡眠时间，不对齐MVPA（使用MVPA的实际值）
predict_interaction_curve <- function(pooled_model, model_list, sleep_values, mvpa_value, reference_sleep, reference_mvpa, fixed_knots, fixed_boundary_knots) {
  # 使用tidy()提取系数信息
  pooled_tidy <- tidy(pooled_model, conf.int = FALSE)
  
  # 获取系数名称和值
  coef_names <- pooled_tidy$term
  pooled_coef <- setNames(pooled_tidy$estimate, coef_names)
  
  # 关键修复：手动计算合并后的方差-协方差矩阵（使用Rubin法则）
  pooled_vcov_matrix <- compute_pooled_vcov(model_list)
  
  # 找到样条项和交互项的索引
  # 样条项主效应（现在名称是 sleep_spline_1, sleep_spline_2, 等）
  spline_main_indices <- grep("^sleep_spline_", coef_names)
  
  mvpa_index <- which(coef_names == "MVPA_hours_per_day")
  # 交互项：匹配 sleep_spline_X:MVPA_hours_per_day 或 MVPA_hours_per_day:sleep_spline_X
  interaction_indices <- grep("sleep_spline_.*:MVPA_hours_per_day|MVPA_hours_per_day:sleep_spline_", coef_names)
  
  # 打印调试信息（只在第一次调用时）
  if (!exists("debug_printed")) {
    print("交互模型系数名称:")
    print(coef_names)
    print(paste("找到样条项主效应:", length(spline_main_indices), "个"))
    print(paste("找到MVPA主效应:", length(mvpa_index), "个"))
    print(paste("找到交互项:", length(interaction_indices), "个"))
    if (length(interaction_indices) > 0) {
      print("交互项名称:")
      print(coef_names[interaction_indices])
    }
    assign("debug_printed", TRUE, envir = .GlobalEnv)
  }
  
  # 使用固定结点创建样条基函数对象
  create_fixed_spline <- function(x) {
    ns(x, knots = fixed_knots, Boundary.knots = fixed_boundary_knots)
  }
  
  # 创建一个临时样条对象用于预测
  temp_basis <- create_fixed_spline(c(reference_sleep, sleep_values))
  reference_basis <- as.numeric(predict(temp_basis, newx = reference_sleep))
  
  # 确定样条项的数量（应该等于交互项的数量或主效应的数量）
  n_spline_basis <- length(reference_basis)  # 通常是3或4（取决于df）
  
  # 如果样条项主效应不存在，说明样条项只出现在交互项中
  # 这种情况下，我们需要确保交互项数量与样条基函数数量匹配
  if (length(spline_main_indices) == 0 && length(interaction_indices) > 0) {
    # 交互项应该对应n_spline_basis个样条基函数
    if (length(interaction_indices) != n_spline_basis) {
      warning(paste("交互项数量(", length(interaction_indices), ")与样条基函数数量(", n_spline_basis, ")不匹配"))
      # 只使用前n_spline_basis个交互项
      interaction_indices <- interaction_indices[seq_len(min(length(interaction_indices), n_spline_basis))]
    }
  }
  
  # 计算预测值
  predictions <- numeric(length(sleep_values))
  se_values <- numeric(length(sleep_values))
  
  for (i in seq_along(sleep_values)) {
    current_basis <- as.numeric(predict(temp_basis, newx = sleep_values[i]))
    basis_diff <- current_basis - reference_basis
    
    # 确保basis_diff长度为n_spline_basis
    if (length(basis_diff) != n_spline_basis) {
      if (length(basis_diff) > n_spline_basis) {
        basis_diff <- basis_diff[1:n_spline_basis]
      } else {
        # 如果长度不足，用0补齐
        basis_diff <- c(basis_diff, rep(0, n_spline_basis - length(basis_diff)))
      }
    }
    
    # 计算预测值（只对齐睡眠，不对齐MVPA）
    # 1. 睡眠时长RCS项的主效应（相对于参考点）
    sleep_effect <- 0
    if (length(spline_main_indices) > 0) {
      n_spline_main <- min(length(basis_diff), length(spline_main_indices))
      sleep_effect <- sum(basis_diff[1:n_spline_main] * pooled_coef[spline_main_indices[1:n_spline_main]], na.rm = TRUE)
    }
    
    # 2. MVPA的主效应（使用实际值，不对齐）
    mvpa_effect <- 0
    if (length(mvpa_index) > 0 && !is.na(pooled_coef[mvpa_index])) {
      mvpa_effect <- mvpa_value * pooled_coef[mvpa_index]
    }
    
    # 3. 交互项效应（使用MVPA实际值，不对齐）
    interaction_effect <- 0
    if (length(interaction_indices) > 0) {
      # 确保交互项数量与样条基函数数量匹配
      n_interaction <- min(length(basis_diff), length(interaction_indices))
      if (n_interaction > 0) {
        # 使用MVPA实际值，而不是差值
        for (j in seq_len(n_interaction)) {
          if (!is.na(pooled_coef[interaction_indices[j]])) {
            # 交互项 = 样条基函数差值 × MVPA实际值 × 交互项系数
            interaction_effect <- interaction_effect + 
              basis_diff[j] * mvpa_value * pooled_coef[interaction_indices[j]]
          }
        }
      }
    }
    
    predictions[i] <- sleep_effect + mvpa_effect + interaction_effect
    
    # 关键修复：使用完整的方差-协方差矩阵计算标准误
    # 需要构建包含所有相关项的系数向量和对应的方差-协方差矩阵
    # 注意：使用MVPA实际值，不对齐
    
    # 构建系数向量：包含样条主效应、MVPA主效应和交互项
    relevant_indices <- c()
    coeff_vector <- c()
    
    if (length(spline_main_indices) > 0) {
      relevant_indices <- c(relevant_indices, spline_main_indices)
      coeff_vector <- c(coeff_vector, basis_diff[seq_len(min(length(basis_diff), length(spline_main_indices)))])
    }
    
    if (length(mvpa_index) > 0) {
      relevant_indices <- c(relevant_indices, mvpa_index)
      coeff_vector <- c(coeff_vector, mvpa_value)  # 使用实际值，不对齐
    }
    
    if (length(interaction_indices) > 0) {
      n_interaction <- min(length(basis_diff), length(interaction_indices))
      relevant_indices <- c(relevant_indices, interaction_indices[1:n_interaction])
      coeff_vector <- c(coeff_vector, basis_diff[1:n_interaction] * mvpa_value)  # 使用实际值，不对齐
    }
    
    # 提取对应的方差-协方差子矩阵
    if (length(relevant_indices) > 0) {
      relevant_vcov <- pooled_vcov_matrix[relevant_indices, relevant_indices, drop = FALSE]
      
      # 确保系数向量长度与方差-协方差矩阵维度匹配
      if (length(coeff_vector) != nrow(relevant_vcov)) {
        min_len <- min(length(coeff_vector), nrow(relevant_vcov))
        coeff_vector <- coeff_vector[1:min_len]
        relevant_vcov <- relevant_vcov[1:min_len, 1:min_len, drop = FALSE]
      }
      
      # 计算标准误：Var(a' * beta) = a' * Var(beta) * a
      se_sq <- as.numeric(t(coeff_vector) %*% relevant_vcov %*% coeff_vector)
      se_values[i] <- ifelse(se_sq > 0, sqrt(se_sq), 0.001)
    } else {
      se_values[i] <- 0.001
    }
  }
  
  # 计算基准值（当sleep=sleep_reference时的预测值），使预测值相对于睡眠参考点
  # 基准值的计算：sleep_effect = 0（因为basis_diff = 0），只保留MVPA效应和交互项效应
  reference_basis_zero <- rep(0, n_spline_basis)  # 睡眠参考点的样条基函数差值为0
  
  baseline_sleep_effect <- 0
  if (length(spline_main_indices) > 0) {
    n_spline_main <- min(length(reference_basis_zero), length(spline_main_indices))
    baseline_sleep_effect <- sum(reference_basis_zero[1:n_spline_main] * pooled_coef[spline_main_indices[1:n_spline_main]], na.rm = TRUE)
  }
  
  baseline_mvpa_effect <- 0
  if (length(mvpa_index) > 0 && !is.na(pooled_coef[mvpa_index])) {
    baseline_mvpa_effect <- mvpa_value * pooled_coef[mvpa_index]
  }
  
  baseline_interaction_effect <- 0
  if (length(interaction_indices) > 0) {
    n_interaction <- min(length(reference_basis_zero), length(interaction_indices))
    if (n_interaction > 0) {
      for (j in seq_len(n_interaction)) {
        if (!is.na(pooled_coef[interaction_indices[j]])) {
          baseline_interaction_effect <- baseline_interaction_effect + 
            reference_basis_zero[j] * mvpa_value * pooled_coef[interaction_indices[j]]
        }
      }
    }
  }
  
  baseline_prediction <- baseline_sleep_effect + baseline_mvpa_effect + baseline_interaction_effect
  
  # 从所有预测值中减去基准值，使预测值相对于睡眠参考点
  predictions <- predictions - baseline_prediction
  
  # 检查是否有NA值
  if (any(is.na(predictions))) {
    warning(paste("发现", sum(is.na(predictions)), "个NA预测值，将被替换为0"))
    predictions[is.na(predictions)] <- 0
  }
  if (any(is.na(se_values))) {
    warning(paste("发现", sum(is.na(se_values)), "个NA标准误，将被替换为0.001"))
    se_values[is.na(se_values)] <- 0.001
  }
  
  # 计算置信区间
  ci_lower <- predictions - 1.96 * se_values
  ci_upper <- predictions + 1.96 * se_values
  
  # 打印统计信息（只在第一次调用时）
  if (!exists("stats_printed")) {
    print(paste("预测值范围:", round(min(predictions, na.rm = TRUE), 4), "到", round(max(predictions, na.rm = TRUE), 4)))
    print(paste("置信区间宽度（平均）:", round(mean(ci_upper - ci_lower, na.rm = TRUE), 4)))
    assign("stats_printed", TRUE, envir = .GlobalEnv)
  }
  
  return(data.frame(
    sleep_hours = sleep_values,
    prediction = predictions,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  ))
}

# 为不同MVPA水平生成预测曲线
print("正在计算不同MVPA水平下的睡眠-认知关系曲线...")

# 清除之前的调试标志
if (exists("debug_printed", envir = .GlobalEnv)) {
  rm("debug_printed", envir = .GlobalEnv)
}
if (exists("stats_printed", envir = .GlobalEnv)) {
  rm("stats_printed", envir = .GlobalEnv)
}

curve_low_mvpa <- predict_interaction_curve(
  pooled_interaction, interaction_model_list, sleep_range, mvpa_low, sleep_reference, mvpa_reference, fixed_knots, fixed_boundary_knots
)
curve_low_mvpa$mvpa_level <- "Low MVPA (25th percentile)"

curve_medium_mvpa <- predict_interaction_curve(
  pooled_interaction, interaction_model_list, sleep_range, mvpa_medium, sleep_reference, mvpa_reference, fixed_knots, fixed_boundary_knots
)
curve_medium_mvpa$mvpa_level <- "Medium MVPA (50th percentile)"

curve_high_mvpa <- predict_interaction_curve(
  pooled_interaction, interaction_model_list, sleep_range, mvpa_high, sleep_reference, mvpa_reference, fixed_knots, fixed_boundary_knots
)
curve_high_mvpa$mvpa_level <- "High MVPA (75th percentile)"

# 合并数据
all_curves <- bind_rows(curve_low_mvpa, curve_medium_mvpa, curve_high_mvpa)
all_curves$mvpa_level <- factor(all_curves$mvpa_level, 
                                 levels = c("Low MVPA (25th percentile)",
                                           "Medium MVPA (50th percentile)",
                                           "High MVPA (75th percentile)"))

# 检查并清理数据
print("检查数据质量...")
print(paste("总行数:", nrow(all_curves)))
print(paste("预测值NA数:", sum(is.na(all_curves$prediction))))
print(paste("置信区间下界NA数:", sum(is.na(all_curves$ci_lower))))
print(paste("置信区间上界NA数:", sum(is.na(all_curves$ci_upper))))

# 移除包含NA的行
all_curves_clean <- all_curves %>%
  filter(!is.na(prediction) & !is.na(ci_lower) & !is.na(ci_upper))

print(paste("清理后行数:", nrow(all_curves_clean)))
print(paste("预测值范围:", round(min(all_curves_clean$prediction, na.rm = TRUE), 4), 
            "到", round(max(all_curves_clean$prediction, na.rm = TRUE), 4)))

# 绘制交互作用图
interaction_plot <- ggplot(all_curves_clean, aes(x = sleep_hours, color = mvpa_level, fill = mvpa_level)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
  # 阴影区域
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
              alpha = 0.2, 
              color = NA) +
  # 主曲线
  geom_line(aes(y = prediction), linewidth = 1.2) +
  # 参考点线
  geom_vline(xintercept = sleep_reference, linetype = "dashed", color = "red", alpha = 0.7) +
  labs(
    title = "睡眠总时长与认知变化的关系：MVPA的调节作用",
    subtitle = paste("参考点: 睡眠", round(sleep_reference, 2), "小时/天 | 样本数:", n_after_exclusion),
    x = "睡眠总时长 (小时/天)",
    y = "认知变化预测值（相对于参考点）",
    color = "MVPA水平",
    fill = "MVPA水平",
    caption = "阴影区域表示95%置信区间。参考点用红色虚线标示。"
  ) +
  scale_color_manual(values = c("Low MVPA (25th percentile)" = "#e74c3c",
                                 "Medium MVPA (50th percentile)" = "#3498db",
                                 "High MVPA (75th percentile)" = "#2ecc71")) +
  scale_fill_manual(values = c("Low MVPA (25th percentile)" = "#e74c3c",
                                "Medium MVPA (50th percentile)" = "#3498db",
                                "High MVPA (75th percentile)" = "#2ecc71")) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 11),
    plot.caption = element_text(size = 9, hjust = 0),
    legend.position = "bottom"
  ) +
  xlim(quantile(reference_data$sleep_hours_per_day, 0.01, na.rm = TRUE),
       quantile(reference_data$sleep_hours_per_day, 0.99, na.rm = TRUE))

# 保存交互作用图
ggsave(
  filename = path_result("interaction_sleep_mvpa_curve.png"),
  plot = interaction_plot,
  width = 12,
  height = 7,
  dpi = 300
)

print("交互作用曲线图已保存到: interaction_sleep_mvpa_curve.png")

# 显示图形
print(interaction_plot)

print("\n===== 分析完成 =====")
print("RCS模型分析已完成，结果已保存。")
print("注意：RCS模型使用4个节点的自然样条来捕捉睡眠总时长与认知变化的非线性关系。")
print("RCS曲线图展示了睡眠时长与认知变化的非线性关系，参考点为睡眠时长的中位数。")
print("\n交互作用分析：")
print("- Model 3 包含睡眠时长RCS × MVPA的交互项")
print("- 如果交互项显著，说明MVPA可以调节睡眠时长对认知变化的影响")
print("- 如果高MVPA组的曲线在睡眠过长时更平缓或更高，说明MVPA可以改善睡眠过长带来的认知衰退")

print("\n===== 缓冲效应分析完成 =====")
print("所有结果已保存到result文件夹中。")

