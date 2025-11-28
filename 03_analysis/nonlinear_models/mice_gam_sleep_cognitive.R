# 睡眠总时长与认知变化的GAM（广义加性模型）分析
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
library(mgcv)  # 用于GAM模型
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

# 步骤 B1: 创建PA_level分组（基于MVPA百分位）
print("步骤 B1: 正在创建PA_level分组...")
# 计算MVPA的百分位数（使用完整数据，不缺失MVPA的样本）
mvpa_for_quantiles <- full_merged_table %>%
  filter(!is.na(MVPA_hours_per_day)) %>%
  pull(MVPA_hours_per_day)

mvpa_quantiles <- quantile(mvpa_for_quantiles, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE)
print(paste("MVPA百分位数 (0%, 33%, 67%, 100%):", paste(round(mvpa_quantiles, 3), collapse = ", ")))

# 创建PA_level分组
full_merged_table <- full_merged_table %>%
  mutate(
    PA_level = case_when(
      is.na(MVPA_hours_per_day) ~ NA_character_,
      MVPA_hours_per_day < mvpa_quantiles[2] ~ "Low PA",
      MVPA_hours_per_day < mvpa_quantiles[3] ~ "Medium PA",
      TRUE ~ "High PA"
    ),
    PA_level = factor(PA_level, levels = c("Medium PA", "Low PA", "High PA"))
  )

# 统计各组的样本量
pa_level_counts <- full_merged_table %>%
  filter(!is.na(PA_level)) %>%
  count(PA_level)
print("\nPA_level分组样本量:")
print(pa_level_counts)
print("步骤 B1 完成。")

# 3. 多重插补 (Impute)
# ==========================================

# 步骤 C: 定义我们最终模型需要的所有变量
all_model_vars <- c(
  "Participant_ID", 
  "cognitive_change",  # <--- 结局变量
  "sleep_hours_per_day",  # <--- 核心自变量
  "MVPA_hours_per_day",  # <--- MVPA变量（用于分组）
  "PA_level",  # <--- PA分组变量
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
n_missing_pa_level <- sum(is.na(imputation_data$PA_level))
n_missing_either <- sum(is.na(imputation_data$cognitive_change) | 
                        is.na(imputation_data$sleep_hours_per_day) |
                        is.na(imputation_data$MVPA_hours_per_day) |
                        is.na(imputation_data$PA_level))

print(paste("剔除前样本数:", n_before_exclusion))
print(paste("cognitive_change 缺失数:", n_missing_outcome, "(", round(n_missing_outcome/n_before_exclusion * 100, 2), "%)"))
print(paste("sleep_hours_per_day 缺失数:", n_missing_sleep, "(", round(n_missing_sleep/n_before_exclusion * 100, 2), "%)"))
print(paste("MVPA_hours_per_day 缺失数:", n_missing_mvpa, "(", round(n_missing_mvpa/n_before_exclusion * 100, 2), "%)"))
print(paste("PA_level 缺失数:", n_missing_pa_level, "(", round(n_missing_pa_level/n_before_exclusion * 100, 2), "%)"))
print(paste("至少一个缺失的数量:", n_missing_either, "(", round(n_missing_either/n_before_exclusion * 100, 2), "%)"))

# 剔除缺失值
imputation_data <- imputation_data %>%
  filter(!is.na(cognitive_change) & 
         !is.na(sleep_hours_per_day) &
         !is.na(MVPA_hours_per_day) &
         !is.na(PA_level))

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
print("注意：cognitive_change、sleep_hours_per_day、MVPA_hours_per_day 和 PA_level 不参与插补（已剔除缺失值）")

# 检测可用CPU核心数，设置并行处理
n_cores <- detectCores() - 1  # 保留一个核心给系统
if (n_cores < 1) n_cores <- 1

print(paste("检测到", n_cores, "个CPU核心可用于并行处理"))

# 为每个变量明确指定插补方法
# 注意：cognitive_change、sleep_hours_per_day、MVPA_hours_per_day 和 PA_level 设置为 ""，表示不插补
method_specification <- c(
  "cognitive_change" = "",  # 不插补，已剔除缺失值
  "sleep_hours_per_day" = "",  # 不插补，已剔除缺失值
  "MVPA_hours_per_day" = "",  # 不插补，已剔除缺失值
  "PA_level" = "",  # 不插补，已剔除缺失值
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

# 4. 在每个插补数据集上运行GAM模型分析 (Analyze)
# ==========================================

all_covariates <- c(
  "age_recruitment", "sex", "townsend_index", "bmi", "smoking_status",
  "alcohol_status", "education_level", "cvd_history_any", 
  "history_diabetes", "cognitive_score_0"
)

# 定义模型：
# Model 0: 仅GAM暴露变量（睡眠时长平滑项）
# Model 1: GAM暴露变量 + 基本人口学变量
# Model 2: GAM暴露变量 + 所有协变量
# Model 3: 分组平滑项模型（按PA_level分组）+ 所有协变量

# 步骤 G: 运行GAM模型
print("步骤 G: 正在对插补数据集运行GAM模型（共4个模型）...")

# 定义拟合函数
fit_gam_models <- function(i) {
  completed_data <- complete(imputed_object, i)
  
  # 确保PA_level是因子类型（Medium PA作为参考组）
  completed_data$PA_level <- factor(completed_data$PA_level, 
                                     levels = c("Medium PA", "Low PA", "High PA"))
  
  # Model 0: 仅睡眠时长平滑项
  formula_gam0 <- cognitive_change ~ s(sleep_hours_per_day, bs = "cs", k = 10)
  
  # Model 1: 睡眠时长平滑项 + 基本人口学变量
  formula_gam1 <- cognitive_change ~ s(sleep_hours_per_day, bs = "cs", k = 10) + 
                  age_recruitment + sex
  
  # Model 2: 睡眠时长平滑项 + 所有协变量
  formula_gam2 <- cognitive_change ~ s(sleep_hours_per_day, bs = "cs", k = 10) + 
                  age_recruitment + sex + townsend_index + bmi + 
                  smoking_status + alcohol_status + education_level + 
                  cvd_history_any + history_diabetes + cognitive_score_0
  
  # Model 3: 分组平滑项模型（按PA_level分组）+ 所有协变量
  # 使用by参数为每个PA_level创建不同的平滑曲线
  formula_gam3 <- cognitive_change ~ s(sleep_hours_per_day, by = PA_level, bs = "cs", k = 10) + 
                  PA_level +  # 添加PA_level作为参数项（主效应）
                  age_recruitment + sex + townsend_index + bmi + 
                  smoking_status + alcohol_status + education_level + 
                  cvd_history_any + history_diabetes + cognitive_score_0
  
  list(
    model0 = gam(formula_gam0, data = completed_data),
    model1 = gam(formula_gam1, data = completed_data),
    model2 = gam(formula_gam2, data = completed_data),
    model3 = gam(formula_gam3, data = completed_data)
  )
}

# 使用lapply串行处理
model_results <- lapply(1:imputed_object$m, fit_gam_models)

# 提取模型列表
gam_model0_list <- lapply(model_results, function(x) x$model0)
gam_model1_list <- lapply(model_results, function(x) x$model1)
gam_model2_list <- lapply(model_results, function(x) x$model2)
gam_model3_list <- lapply(model_results, function(x) x$model3)

print("步骤 G 完成！")

# 5. 汇总所有分析结果 (Pool) 并展示
# ==========================================

# 步骤 H: 使用 pool() 函数分别汇总三个模型的结果（Rubin法则）
print("步骤 H: 正在使用Rubin法则汇总GAM模型结果...")

# 注意：GAM模型不能直接使用pool()函数，因为pool()主要适用于lm/glm等模型
# 我们需要提取GAM模型的系数（固定效应部分）进行合并
# 对于平滑项，我们需要使用predict()函数来获取预测值

# 提取GAM模型的固定效应系数（如果有的话）
# 对于GAM模型，我们主要关注平滑项的预测效果

# 由于GAM模型的特殊性，我们需要使用不同的方法来汇总结果
# 方法1：提取每个模型的摘要统计信息
# 方法2：使用预测值来评估模型效果

print("注意：GAM模型使用平滑项，无法直接使用pool()函数汇总系数。")
print("我们将通过提取模型摘要信息和预测值来评估结果。")

# 提取每个插补数据集的模型摘要
extract_gam_summary <- function(model_list) {
  summaries <- lapply(model_list, function(m) {
    summary(m)
  })
  return(summaries)
}

gam0_summaries <- extract_gam_summary(gam_model0_list)
gam1_summaries <- extract_gam_summary(gam_model1_list)
gam2_summaries <- extract_gam_summary(gam_model2_list)
gam3_summaries <- extract_gam_summary(gam_model3_list)

# 提取平滑项的显著性（P值）
extract_smooth_pvalue <- function(summaries) {
  p_values <- sapply(summaries, function(s) {
    # GAM摘要中平滑项的P值在s.table中
    if ("s.table" %in% names(s)) {
      return(s$s.table[1, "p-value"])
    } else {
      return(NA)
    }
  })
  return(p_values)
}

gam0_smooth_p <- extract_smooth_pvalue(gam0_summaries)
gam1_smooth_p <- extract_smooth_pvalue(gam1_summaries)
gam2_smooth_p <- extract_smooth_pvalue(gam2_summaries)

print("\n===== 平滑项显著性检验（各插补数据集）=====")
print(paste("Model 0 平滑项P值范围:", round(min(gam0_smooth_p, na.rm = TRUE), 4), 
            "到", round(max(gam0_smooth_p, na.rm = TRUE), 4)))
print(paste("Model 1 平滑项P值范围:", round(min(gam1_smooth_p, na.rm = TRUE), 4), 
            "到", round(max(gam1_smooth_p, na.rm = TRUE), 4)))
print(paste("Model 2 平滑项P值范围:", round(min(gam2_smooth_p, na.rm = TRUE), 4), 
            "到", round(max(gam2_smooth_p, na.rm = TRUE), 4)))

# 计算平均P值（简单方法，更精确的方法需要考虑Rubin法则）
gam0_smooth_p_mean <- mean(gam0_smooth_p, na.rm = TRUE)
gam1_smooth_p_mean <- mean(gam1_smooth_p, na.rm = TRUE)
gam2_smooth_p_mean <- mean(gam2_smooth_p, na.rm = TRUE)

print("\n===== 平滑项显著性检验（合并后）=====")
print(paste("Model 0 平滑项平均P值:", round(gam0_smooth_p_mean, 4)))
print(paste("Model 1 平滑项平均P值:", round(gam1_smooth_p_mean, 4)))
print(paste("Model 2 平滑项平均P值:", round(gam2_smooth_p_mean, 4)))

# 步骤 H0.5: 提取Model 3分组平滑项的统计信息（EDF, p, F）
# ==========================================
print("\n步骤 H0.5: 正在提取Model 3分组平滑项的统计信息（EDF, p, F）...")

# 函数：提取分组平滑项的统计信息
extract_grouped_smooth_stats <- function(model_list, summaries) {
  # 提取所有插补数据集的平滑项统计信息
  stats_list <- lapply(seq_along(summaries), function(i) {
    s <- summaries[[i]]
    m <- model_list[[i]]
    
    if ("s.table" %in% names(s)) {
      s_table <- s$s.table
      smooth_names <- rownames(s_table)
      
      # 查找sleep_hours_per_day相关的平滑项
      sleep_smooth_indices <- grep("sleep_hours_per_day", smooth_names, ignore.case = TRUE)
      
      if (length(sleep_smooth_indices) >= 3) {
        # 如果有3个或更多sleep相关的平滑项，应该是分组平滑项
        # 检查模型对象，确认是否有by参数
        # 获取平滑项的详细信息
        smooth_info <- m$smooth
        result_list <- list()
        
        # 查找使用by参数的平滑项
        by_smooth_idx <- 0
        for (j in seq_along(smooth_info)) {
          if (!is.null(smooth_info[[j]]$by) && smooth_info[[j]]$by == "PA_level") {
            by_smooth_idx <- j
            break
          }
        }
        
        if (by_smooth_idx > 0) {
          # 找到了使用by参数的平滑项
          # 这个平滑项会为每个PA_level组创建独立的平滑项
          # 在s.table中，这些平滑项的顺序应该与因子水平的顺序一致
          pa_levels_order <- c("Medium PA", "Low PA", "High PA")
          
          # 提取每个组的统计信息
          # 注意：使用by参数时，每个组对应s.table中的一行
          for (k in seq_along(pa_levels_order)) {
            if (k <= length(sleep_smooth_indices)) {
              idx <- sleep_smooth_indices[k]
              result_list[[length(result_list) + 1]] <- data.frame(
                PA_level = pa_levels_order[k],
                smooth_term = smooth_names[idx],
                edf = s_table[idx, "edf"],
                ref.df = s_table[idx, "Ref.df"],
                F = s_table[idx, "F"],
                p.value = s_table[idx, "p-value"],
                stringsAsFactors = FALSE
              )
            }
          }
        } else {
          # 如果没有找到by参数，但有多于3个sleep相关的平滑项
          # 按顺序分配（假设前3个对应3个PA_level组）
          pa_levels_order <- c("Medium PA", "Low PA", "High PA")
          for (k in seq_along(pa_levels_order)) {
            if (k <= length(sleep_smooth_indices)) {
              idx <- sleep_smooth_indices[k]
              result_list[[length(result_list) + 1]] <- data.frame(
                PA_level = pa_levels_order[k],
                smooth_term = smooth_names[idx],
                edf = s_table[idx, "edf"],
                ref.df = s_table[idx, "Ref.df"],
                F = s_table[idx, "F"],
                p.value = s_table[idx, "p-value"],
                stringsAsFactors = FALSE
              )
            }
          }
        }
        
        if (length(result_list) > 0) {
          return(bind_rows(result_list))
        }
      }
      
      # 如果没有找到足够的分组平滑项，返回空数据框
      return(data.frame(
        PA_level = character(0),
        smooth_term = character(0),
        edf = numeric(0),
        ref.df = numeric(0),
        F = numeric(0),
        p.value = numeric(0),
        stringsAsFactors = FALSE
      ))
    } else {
      return(data.frame(
        PA_level = character(0),
        smooth_term = character(0),
        edf = numeric(0),
        ref.df = numeric(0),
        F = numeric(0),
        p.value = numeric(0),
        stringsAsFactors = FALSE
      ))
    }
  })
  
  return(stats_list)
}

# 提取Model 3的分组平滑项统计信息
gam3_grouped_stats_list <- extract_grouped_smooth_stats(gam_model3_list, gam3_summaries)

# 检查提取的结果
print("检查Model 3分组平滑项提取结果...")
if (length(gam3_grouped_stats_list) > 0 && nrow(gam3_grouped_stats_list[[1]]) > 0) {
  print("第一个插补数据集的平滑项:")
  print(gam3_grouped_stats_list[[1]])
  
  # 获取所有唯一的PA_level
  all_pa_levels <- unique(unlist(lapply(gam3_grouped_stats_list, function(x) x$PA_level)))
  print(paste("找到", length(all_pa_levels), "个PA_level组:", paste(all_pa_levels, collapse = ", ")))
  
  if (length(all_pa_levels) >= 3) {
    # 合并所有插补数据集的结果（按PA_level分组）
    pa_levels_order <- c("Medium PA", "Low PA", "High PA")
    
    pooled_grouped_stats <- lapply(pa_levels_order, function(pa_level) {
      # 提取该PA_level在所有插补数据集中的统计信息
      edf_values <- sapply(gam3_grouped_stats_list, function(x) {
        idx <- which(x$PA_level == pa_level)
        if (length(idx) > 0) {
          return(x$edf[idx[1]])
        } else {
          return(NA)
        }
      })
      
      ref_df_values <- sapply(gam3_grouped_stats_list, function(x) {
        idx <- which(x$PA_level == pa_level)
        if (length(idx) > 0) {
          return(x$ref.df[idx[1]])
        } else {
          return(NA)
        }
      })
      
      f_values <- sapply(gam3_grouped_stats_list, function(x) {
        idx <- which(x$PA_level == pa_level)
        if (length(idx) > 0) {
          return(x$F[idx[1]])
        } else {
          return(NA)
        }
      })
      
      p_values <- sapply(gam3_grouped_stats_list, function(x) {
        idx <- which(x$PA_level == pa_level)
        if (length(idx) > 0) {
          return(x$p.value[idx[1]])
        } else {
          return(NA)
        }
      })
      
      # 移除NA值
      valid_idx <- !is.na(edf_values) & !is.na(f_values) & !is.na(p_values)
      if (sum(valid_idx) == 0) {
        return(list(
          PA_level = pa_level,
          edf = NA,
          ref.df = NA,
          F = NA,
          p.value = NA
        ))
      }
      
      edf_values <- edf_values[valid_idx]
      ref_df_values <- ref_df_values[valid_idx]
      f_values <- f_values[valid_idx]
      p_values <- p_values[valid_idx]
      
      # 使用简单平均方法合并（对于EDF和F统计量）
      pooled_edf <- mean(edf_values, na.rm = TRUE)
      pooled_ref_df <- mean(ref_df_values, na.rm = TRUE)
      pooled_f <- mean(f_values, na.rm = TRUE)
      pooled_p <- mean(p_values, na.rm = TRUE)
      
      return(list(
        PA_level = pa_level,
        edf = pooled_edf,
        ref.df = pooled_ref_df,
        F = pooled_f,
        p.value = pooled_p
      ))
    })
    
    # 转换为数据框
    grouped_stats_table <- do.call(rbind, lapply(pooled_grouped_stats, function(x) {
      data.frame(
        PA_level = x$PA_level,
        EDF = round(x$edf, 3),
        Ref_df = round(x$ref.df, 3),
        F = round(x$F, 3),
        P_Value = ifelse(x$p.value < 0.001, "<0.001", round(x$p.value, 4)),
        stringsAsFactors = FALSE
      )
    }))
    
    print("\n===== Model 3分组平滑项统计信息（EDF, p, F）=====")
    print(grouped_stats_table)
    
    # 保存结果
    write.csv(grouped_stats_table, 
              file = path_result("gam_model3_grouped_smooth_stats.csv"), 
              row.names = FALSE)
    
    print("\n分组平滑项统计信息已保存到: gam_model3_grouped_smooth_stats.csv")
  } else {
    print("警告：未找到足够的PA_level组（期望3个，找到", length(all_pa_levels), "个）")
    print("找到的PA_level组:")
    print(all_pa_levels)
  }
} else {
  print("警告：未能提取Model 3的分组平滑项统计信息")
  print("尝试检查模型摘要结构...")
  if (length(gam3_summaries) > 0) {
    print("第一个模型的摘要结构:")
    print(names(gam3_summaries[[1]]))
    if ("s.table" %in% names(gam3_summaries[[1]])) {
      print("s.table内容:")
      print(gam3_summaries[[1]]$s.table)
    }
  }
  if (length(gam_model3_list) > 0) {
    print("\n第一个模型的平滑项信息:")
    print(gam_model3_list[[1]]$smooth)
  }
}

print("步骤 H0.5 完成。")

# 提取R²值
extract_r_squared <- function(model_list) {
  r_squared <- sapply(model_list, function(m) {
    summary(m)$r.sq
  })
  return(r_squared)
}

gam0_r2 <- extract_r_squared(gam_model0_list)
gam1_r2 <- extract_r_squared(gam_model1_list)
gam2_r2 <- extract_r_squared(gam_model2_list)

print("\n===== 模型拟合优度（R²）=====")
print(paste("Model 0 平均R²:", round(mean(gam0_r2, na.rm = TRUE), 4), 
            "(范围:", round(min(gam0_r2, na.rm = TRUE), 4), 
            "到", round(max(gam0_r2, na.rm = TRUE), 4), ")"))
print(paste("Model 1 平均R²:", round(mean(gam1_r2, na.rm = TRUE), 4), 
            "(范围:", round(min(gam1_r2, na.rm = TRUE), 4), 
            "到", round(max(gam1_r2, na.rm = TRUE), 4), ")"))
print(paste("Model 2 平均R²:", round(mean(gam2_r2, na.rm = TRUE), 4), 
            "(范围:", round(min(gam2_r2, na.rm = TRUE), 4), 
            "到", round(max(gam2_r2, na.rm = TRUE), 4), ")"))

# 创建结果摘要表
model_summary_table <- data.frame(
  Model = c("Model 0 (Unadjusted)", "Model 1 (Adjusted for Demographics)", "Model 2 (Fully Adjusted)"),
  Smooth_P_Value = c(
    ifelse(gam0_smooth_p_mean < 0.001, "<0.001", round(gam0_smooth_p_mean, 4)),
    ifelse(gam1_smooth_p_mean < 0.001, "<0.001", round(gam1_smooth_p_mean, 4)),
    ifelse(gam2_smooth_p_mean < 0.001, "<0.001", round(gam2_smooth_p_mean, 4))
  ),
  R_Squared_Mean = c(
    round(mean(gam0_r2, na.rm = TRUE), 4),
    round(mean(gam1_r2, na.rm = TRUE), 4),
    round(mean(gam2_r2, na.rm = TRUE), 4)
  )
)

print("\n===== GAM模型结果摘要 =====")
print(model_summary_table)

write.csv(model_summary_table, file = path_result("gam_model_sleep_results.csv"), row.names = FALSE)

# 步骤 H1: 提取并汇总参数项系数（β值）
# ==========================================
print("\n步骤 H1: 正在提取并汇总参数项系数（β值）...")

# 函数：提取GAM模型的参数项系数
extract_gam_coefficients <- function(model_list) {
  # 提取所有模型的系数
  coef_list <- lapply(model_list, function(m) {
    # 获取模型摘要
    model_summary <- summary(m)
    
    # 提取参数项系数（p.table包含参数项的系数）
    if ("p.table" %in% names(model_summary)) {
      p_table <- model_summary$p.table
      coef_df <- data.frame(
        term = rownames(p_table),
        estimate = p_table[, "Estimate"],
        std.error = p_table[, "Std. Error"],
        statistic = p_table[, "t value"],
        p.value = p_table[, "Pr(>|t|)"],
        stringsAsFactors = FALSE
      )
      return(coef_df)
    } else {
      # 如果没有p.table，尝试从coef()和vcov()提取
      coef_values <- coef(m)
      vcov_matrix <- vcov(m)
      
      # 只提取参数项（排除平滑项）
      # 平滑项通常以"s("开头，但保留截距项和其他参数项
      param_indices <- !grepl("^s\\(", names(coef_values))
      if (sum(param_indices) > 0) {
        param_coef <- coef_values[param_indices]
        param_se <- sqrt(diag(vcov_matrix)[param_indices])
        param_t <- param_coef / param_se
        param_p <- 2 * (1 - pnorm(abs(param_t)))
        
        coef_df <- data.frame(
          term = names(param_coef),
          estimate = as.numeric(param_coef),
          std.error = as.numeric(param_se),
          statistic = as.numeric(param_t),
          p.value = as.numeric(param_p),
          stringsAsFactors = FALSE
        )
        return(coef_df)
      } else {
        return(data.frame(
          term = character(0),
          estimate = numeric(0),
          std.error = numeric(0),
          statistic = numeric(0),
          p.value = numeric(0),
          stringsAsFactors = FALSE
        ))
      }
    }
  })
  
  return(coef_list)
}

# 函数：使用Rubin法则合并系数
pool_gam_coefficients <- function(coef_list) {
  if (length(coef_list) == 0) {
    return(data.frame())
  }
  
  # 获取所有唯一的系数名称
  all_terms <- unique(unlist(lapply(coef_list, function(x) x$term)))
  
  if (length(all_terms) == 0) {
    return(data.frame())
  }
  
  # 对每个系数进行合并
  pooled_coef <- lapply(all_terms, function(term) {
    # 提取该系数在所有插补数据集中的值
    estimates <- sapply(coef_list, function(x) {
      idx <- which(x$term == term)
      if (length(idx) > 0) {
        return(x$estimate[idx[1]])
      } else {
        return(NA)
      }
    })
    
    std_errors <- sapply(coef_list, function(x) {
      idx <- which(x$term == term)
      if (length(idx) > 0) {
        return(x$std.error[idx[1]])
      } else {
        return(NA)
      }
    })
    
    # 移除NA值
    valid_idx <- !is.na(estimates) & !is.na(std_errors)
    if (sum(valid_idx) == 0) {
      return(list(
        term = term,
        estimate = NA,
        std.error = NA,
        conf.low = NA,
        conf.high = NA,
        p.value = NA
      ))
    }
    
    estimates <- estimates[valid_idx]
    std_errors <- std_errors[valid_idx]
    
    # Rubin法则合并
    m <- length(estimates)
    
    # Within-imputation方差（平均标准误的平方）
    within_var <- mean(std_errors^2, na.rm = TRUE)
    
    # Between-imputation方差（系数估计值之间的方差）
    between_var <- stats::var(estimates, na.rm = TRUE)
    
    # 合并后的方差
    total_var <- within_var + (1 + 1/m) * between_var
    pooled_se <- sqrt(total_var)
    
    # 合并后的系数
    pooled_coef_value <- mean(estimates, na.rm = TRUE)
    
    # 计算95%置信区间
    conf_low <- pooled_coef_value - 1.96 * pooled_se
    conf_high <- pooled_coef_value + 1.96 * pooled_se
    
    # 计算P值
    t_stat <- pooled_coef_value / pooled_se
    p_value <- 2 * (1 - pnorm(abs(t_stat)))
    
    return(list(
      term = term,
      estimate = pooled_coef_value,
      std.error = pooled_se,
      conf.low = conf_low,
      conf.high = conf_high,
      p.value = p_value
    ))
  })
  
  # 转换为数据框
  result_df <- do.call(rbind, lapply(pooled_coef, function(x) {
    data.frame(
      term = x$term,
      estimate = x$estimate,
      std.error = x$std.error,
      conf.low = x$conf.low,
      conf.high = x$conf.high,
      p.value = x$p.value,
      stringsAsFactors = FALSE
    )
  }))
  
  return(result_df)
}

# 提取并合并每个模型的系数
print("正在提取Model 0的系数...")
coef_list_0 <- extract_gam_coefficients(gam_model0_list)
pooled_coef_0 <- pool_gam_coefficients(coef_list_0)
if (nrow(pooled_coef_0) > 0) {
  pooled_coef_0$model <- "Model 0 (Unadjusted)"
}

print("正在提取Model 1的系数...")
coef_list_1 <- extract_gam_coefficients(gam_model1_list)
pooled_coef_1 <- pool_gam_coefficients(coef_list_1)
if (nrow(pooled_coef_1) > 0) {
  pooled_coef_1$model <- "Model 1 (Adjusted for Demographics)"
}

print("正在提取Model 2的系数...")
coef_list_2 <- extract_gam_coefficients(gam_model2_list)
pooled_coef_2 <- pool_gam_coefficients(coef_list_2)
if (nrow(pooled_coef_2) > 0) {
  pooled_coef_2$model <- "Model 2 (Fully Adjusted)"
}

# 合并所有模型的结果
all_coef_results <- bind_rows(
  if (nrow(pooled_coef_0) > 0) pooled_coef_0 else NULL,
  if (nrow(pooled_coef_1) > 0) pooled_coef_1 else NULL,
  if (nrow(pooled_coef_2) > 0) pooled_coef_2 else NULL
)

# 创建格式化的结果表格
if (nrow(all_coef_results) > 0) {
  coef_results_table <- all_coef_results %>%
    mutate(
      across(c(estimate, std.error, conf.low, conf.high), ~round(.x, 4)),
      p.value = if_else(p.value < 0.001, "<0.001", as.character(round(p.value, 4)))
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
  
  print("\n===== GAM模型参数项系数（β值）=====")
  print(coef_results_table)
  
  # 保存系数结果
  write.csv(coef_results_table, 
            file = path_result("gam_model_coefficients.csv"), 
            row.names = FALSE)
  
  # 同时保存详细版本（包含标准误等）
  write.csv(all_coef_results, 
            file = path_result("gam_model_coefficients_detailed.csv"), 
            row.names = FALSE)
  
  print("\n系数结果已保存到:")
  print("- gam_model_coefficients.csv (格式化版本)")
  print("- gam_model_coefficients_detailed.csv (详细版本)")
} else {
  print("\n警告：未找到参数项系数。GAM模型可能只包含平滑项。")
}

print("步骤 H1 完成。")

print("所有结果已保存到result文件夹中。")

# 7. GAM曲线可视化
# ==========================================
print("步骤 I: 正在生成GAM曲线可视化...")

# 获取第一个插补数据集用于计算参考点和预测值
reference_data <- complete(imputed_object, 1)

# 计算睡眠时长的参考点（通常使用中位数）
sleep_reference <- median(reference_data$sleep_hours_per_day, na.rm = TRUE)
print(paste("使用睡眠时长中位数作为参考点:", round(sleep_reference, 2), "小时/天"))

# 创建睡眠时长的序列（用于绘制曲线）
sleep_range <- seq(
  quantile(reference_data$sleep_hours_per_day, 0.01, na.rm = TRUE),
  quantile(reference_data$sleep_hours_per_day, 0.99, na.rm = TRUE),
  length.out = 200
)

# 函数：从GAM模型提取预测值（使用所有插补数据集）
predict_gam_curve <- function(model_list, sleep_values, reference_value, reference_data) {
  # 对每个插补数据集进行预测
  predictions_list <- lapply(seq_along(model_list), function(i) {
    m <- model_list[[i]]
    
    # 创建预测数据框（使用参考数据的协变量均值）
    pred_data <- data.frame(
      sleep_hours_per_day = sleep_values,
      stringsAsFactors = FALSE
    )
    
    # 添加协变量（使用参考数据的均值或众数）
    if (length(all_covariates) > 0) {
      for (cov in all_covariates) {
        if (cov %in% names(reference_data)) {
          if (is.numeric(reference_data[[cov]])) {
            pred_data[[cov]] <- median(reference_data[[cov]], na.rm = TRUE)
          } else {
            # 对于分类变量，使用最常见的值
            cov_table <- table(reference_data[[cov]])
            if (length(cov_table) > 0) {
              pred_data[[cov]] <- names(sort(cov_table, decreasing = TRUE))[1]
            } else {
              pred_data[[cov]] <- reference_data[[cov]][1]
            }
          }
        }
      }
    }
    
    # 确保pred_data中的变量类型与reference_data匹配
    for (col_name in names(pred_data)) {
      if (col_name %in% names(reference_data)) {
        if (is.factor(reference_data[[col_name]])) {
          pred_data[[col_name]] <- factor(pred_data[[col_name]], 
                                          levels = levels(reference_data[[col_name]]))
        }
      }
    }
    
    # 进行预测
    tryCatch({
      pred_result <- predict(m, newdata = pred_data, se.fit = TRUE, type = "response")
      return(list(
        fit = as.numeric(pred_result$fit),
        se = as.numeric(pred_result$se.fit)
      ))
    }, error = function(e) {
      warning(paste("插补数据集", i, "预测时出错:", e$message))
      return(list(
        fit = rep(NA, length(sleep_values)),
        se = rep(NA, length(sleep_values))
      ))
    })
  })
  
  # 合并所有插补数据集的预测结果（使用Rubin法则）
  n_models <- length(predictions_list)
  n_points <- length(sleep_values)
  
  # 计算平均预测值
  pred_matrix <- matrix(NA, nrow = n_models, ncol = n_points)
  se_matrix <- matrix(NA, nrow = n_models, ncol = n_points)
  
  for (i in seq_len(n_models)) {
    pred_matrix[i, ] <- predictions_list[[i]]$fit
    se_matrix[i, ] <- predictions_list[[i]]$se
  }
  
  # Within-imputation方差（平均标准误的平方）
  within_var <- colMeans(se_matrix^2, na.rm = TRUE)
  
  # Between-imputation方差（预测值之间的方差）
  # 使用明确的函数调用避免名称冲突
  between_var <- apply(pred_matrix, 2, function(x) stats::var(x, na.rm = TRUE))
  
  # Rubin法则：合并后的方差
  total_var <- within_var + (1 + 1/n_models) * between_var
  
  # 合并后的预测值和标准误
  pooled_pred <- colMeans(pred_matrix, na.rm = TRUE)
  pooled_se <- sqrt(total_var)
  
  # 计算相对于参考点的预测值
  # 首先计算参考点的预测值
  ref_pred_list <- lapply(seq_along(model_list), function(i) {
    m <- model_list[[i]]
    ref_data <- data.frame(
      sleep_hours_per_day = reference_value,
      stringsAsFactors = FALSE
    )
    
    if (length(all_covariates) > 0) {
      for (cov in all_covariates) {
        if (cov %in% names(reference_data)) {
          if (is.numeric(reference_data[[cov]])) {
            ref_data[[cov]] <- median(reference_data[[cov]], na.rm = TRUE)
          } else {
            cov_table <- table(reference_data[[cov]])
            if (length(cov_table) > 0) {
              ref_data[[cov]] <- names(sort(cov_table, decreasing = TRUE))[1]
            } else {
              ref_data[[cov]] <- reference_data[[cov]][1]
            }
          }
        }
      }
    }
    
    # 确保ref_data中的变量类型与reference_data匹配
    for (col_name in names(ref_data)) {
      if (col_name %in% names(reference_data)) {
        if (is.factor(reference_data[[col_name]])) {
          ref_data[[col_name]] <- factor(ref_data[[col_name]], 
                                         levels = levels(reference_data[[col_name]]))
        }
      }
    }
    
    tryCatch({
      pred_ref <- predict(m, newdata = ref_data, se.fit = TRUE, type = "response")
      return(as.numeric(pred_ref$fit))
    }, error = function(e) {
      warning(paste("插补数据集", i, "参考点预测时出错:", e$message))
      return(NA)
    })
  })
  
  ref_pred_mean <- mean(unlist(ref_pred_list), na.rm = TRUE)
  
  # 相对于参考点的预测值
  predictions_relative <- pooled_pred - ref_pred_mean
  
  # 计算置信区间（注意：标准误不变，因为只是平移）
  ci_lower <- predictions_relative - 1.96 * pooled_se
  ci_upper <- predictions_relative + 1.96 * pooled_se
  
  return(data.frame(
    sleep_hours = sleep_values,
    prediction = predictions_relative,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  ))
}

# 为Model 2生成预测曲线
print("正在计算Model 2的GAM曲线...")
gam_curve_model2 <- predict_gam_curve(
  gam_model2_list, 
  sleep_range, 
  sleep_reference, 
  reference_data
)

# 检查置信区间的范围
print("置信区间统计:")
print(paste("预测值范围:", round(min(gam_curve_model2$prediction, na.rm = TRUE), 3), 
            "到", round(max(gam_curve_model2$prediction, na.rm = TRUE), 3)))
print(paste("置信区间宽度（平均）:", round(mean(gam_curve_model2$ci_upper - gam_curve_model2$ci_lower, na.rm = TRUE), 3)))

# 绘制GAM曲线图
gam_plot <- ggplot(gam_curve_model2, aes(x = sleep_hours)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
  # 阴影区域（95%置信区间）
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
              alpha = 0.3, 
              fill = "lightblue", 
              color = NA) +
  # 主曲线
  geom_line(aes(y = prediction), color = "steelblue", size = 1.2) +
  # 参考点线
  geom_vline(xintercept = sleep_reference, linetype = "dashed", color = "red", alpha = 0.7) +
  labs(
    title = "睡眠总时长与认知变化的关系（GAM模型，Model 2）",
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

# 保存GAM曲线图
ggsave(
  filename = path_result("gam_model_sleep_curve.png"),
  plot = gam_plot,
  width = 10,
  height = 6,
  dpi = 300
)

print("GAM曲线图已保存到: gam_model_sleep_curve.png")

# 显示图形
print(gam_plot)

# 8. 分组平滑项可视化（Model 3）
# ==========================================
print("步骤 J: 正在生成分组平滑项可视化（Model 3）...")

# 函数：为分组平滑项模型生成预测曲线（按PA_level分组）
predict_gam_curve_by_pa <- function(model_list, sleep_values, reference_value, reference_data, pa_level_value) {
  # 对每个插补数据集进行预测
  predictions_list <- lapply(seq_along(model_list), function(i) {
    m <- model_list[[i]]
    
    # 创建预测数据框（使用参考数据的协变量均值）
    pred_data <- data.frame(
      sleep_hours_per_day = sleep_values,
      PA_level = pa_level_value,
      stringsAsFactors = FALSE
    )
    
    # 确保PA_level是因子类型（Medium PA作为参考组）
    pred_data$PA_level <- factor(pred_data$PA_level, 
                                 levels = c("Medium PA", "Low PA", "High PA"))
    
    # 添加协变量（使用参考数据的均值或众数）
    if (length(all_covariates) > 0) {
      for (cov in all_covariates) {
        if (cov %in% names(reference_data)) {
          if (is.numeric(reference_data[[cov]])) {
            pred_data[[cov]] <- median(reference_data[[cov]], na.rm = TRUE)
          } else {
            cov_table <- table(reference_data[[cov]])
            if (length(cov_table) > 0) {
              pred_data[[cov]] <- names(sort(cov_table, decreasing = TRUE))[1]
            } else {
              pred_data[[cov]] <- reference_data[[cov]][1]
            }
          }
        }
      }
    }
    
    # 确保pred_data中的变量类型与reference_data匹配
    for (col_name in names(pred_data)) {
      if (col_name %in% names(reference_data) && col_name != "PA_level") {
        if (is.factor(reference_data[[col_name]])) {
          pred_data[[col_name]] <- factor(pred_data[[col_name]], 
                                          levels = levels(reference_data[[col_name]]))
        }
      }
    }
    
    # 进行预测
    tryCatch({
      pred_result <- predict(m, newdata = pred_data, se.fit = TRUE, type = "response")
      return(list(
        fit = as.numeric(pred_result$fit),
        se = as.numeric(pred_result$se.fit)
      ))
    }, error = function(e) {
      warning(paste("插补数据集", i, "PA_level", pa_level_value, "预测时出错:", e$message))
      return(list(
        fit = rep(NA, length(sleep_values)),
        se = rep(NA, length(sleep_values))
      ))
    })
  })
  
  # 合并所有插补数据集的预测结果（使用Rubin法则）
  n_models <- length(predictions_list)
  n_points <- length(sleep_values)
  
  # 计算平均预测值
  pred_matrix <- matrix(NA, nrow = n_models, ncol = n_points)
  se_matrix <- matrix(NA, nrow = n_models, ncol = n_points)
  
  for (i in seq_len(n_models)) {
    pred_matrix[i, ] <- predictions_list[[i]]$fit
    se_matrix[i, ] <- predictions_list[[i]]$se
  }
  
  # Within-imputation方差（平均标准误的平方）
  within_var <- colMeans(se_matrix^2, na.rm = TRUE)
  
  # Between-imputation方差（预测值之间的方差）
  between_var <- apply(pred_matrix, 2, function(x) stats::var(x, na.rm = TRUE))
  
  # Rubin法则：合并后的方差
  total_var <- within_var + (1 + 1/n_models) * between_var
  
  # 合并后的预测值和标准误
  pooled_pred <- colMeans(pred_matrix, na.rm = TRUE)
  pooled_se <- sqrt(total_var)
  
  # 计算相对于参考点的预测值
  # 所有组都相对于Medium PA组的参考点
  # 首先计算Medium PA组参考点的预测值
  ref_pred_list <- lapply(seq_along(model_list), function(i) {
    m <- model_list[[i]]
    ref_data <- data.frame(
      sleep_hours_per_day = reference_value,
      PA_level = "Medium PA",  # 固定使用Medium PA作为参考组
      stringsAsFactors = FALSE
    )
    
    ref_data$PA_level <- factor(ref_data$PA_level, 
                                levels = c("Medium PA", "Low PA", "High PA"))
    
    if (length(all_covariates) > 0) {
      for (cov in all_covariates) {
        if (cov %in% names(reference_data)) {
          if (is.numeric(reference_data[[cov]])) {
            ref_data[[cov]] <- median(reference_data[[cov]], na.rm = TRUE)
          } else {
            cov_table <- table(reference_data[[cov]])
            if (length(cov_table) > 0) {
              ref_data[[cov]] <- names(sort(cov_table, decreasing = TRUE))[1]
            } else {
              ref_data[[cov]] <- reference_data[[cov]][1]
            }
          }
        }
      }
    }
    
    # 确保ref_data中的变量类型与reference_data匹配
    for (col_name in names(ref_data)) {
      if (col_name %in% names(reference_data) && col_name != "PA_level") {
        if (is.factor(reference_data[[col_name]])) {
          ref_data[[col_name]] <- factor(ref_data[[col_name]], 
                                         levels = levels(reference_data[[col_name]]))
        }
      }
    }
    
    tryCatch({
      pred_ref <- predict(m, newdata = ref_data, se.fit = TRUE, type = "response")
      return(as.numeric(pred_ref$fit))
    }, error = function(e) {
      warning(paste("插补数据集", i, "PA_level", pa_level_value, "参考点预测时出错:", e$message))
      return(NA)
    })
  })
  
  ref_pred_mean <- mean(unlist(ref_pred_list), na.rm = TRUE)
  
  # 相对于参考点的预测值
  predictions_relative <- pooled_pred - ref_pred_mean
  
  # 计算置信区间
  ci_lower <- predictions_relative - 1.96 * pooled_se
  ci_upper <- predictions_relative + 1.96 * pooled_se
  
  return(data.frame(
    sleep_hours = sleep_values,
    prediction = predictions_relative,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    PA_level = pa_level_value
  ))
}

# 为每个PA_level生成预测曲线
print("正在计算各PA_level的GAM曲线...")
pa_levels <- c("Medium PA", "Low PA", "High PA")

all_pa_curves <- lapply(pa_levels, function(pa_level) {
  print(paste("正在计算", pa_level, "的曲线..."))
  predict_gam_curve_by_pa(
    gam_model3_list,
    sleep_range,
    sleep_reference,
    reference_data,
    pa_level
  )
})

# 合并所有PA_level的曲线
all_pa_curves_df <- bind_rows(all_pa_curves)
all_pa_curves_df$PA_level <- factor(all_pa_curves_df$PA_level, 
                                    levels = c("Medium PA", "Low PA", "High PA"))

# 检查数据质量
print("检查分组曲线数据质量...")
print(paste("总行数:", nrow(all_pa_curves_df)))
print(paste("预测值NA数:", sum(is.na(all_pa_curves_df$prediction))))

# 移除包含NA的行
all_pa_curves_df_clean <- all_pa_curves_df %>%
  filter(!is.na(prediction) & !is.na(ci_lower) & !is.na(ci_upper))

print(paste("清理后行数:", nrow(all_pa_curves_df_clean)))

# 绘制分组平滑项曲线图
gam_plot_by_pa <- ggplot(all_pa_curves_df_clean, 
                         aes(x = sleep_hours, color = PA_level, fill = PA_level)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
  # 阴影区域（95%置信区间）
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
              alpha = 0.2, 
              color = NA) +
  # 主曲线
  geom_line(aes(y = prediction), linewidth = 1.2) +
  # 参考点线
  geom_vline(xintercept = sleep_reference, linetype = "dashed", color = "red", alpha = 0.7) +
  labs(
    title = "睡眠总时长与认知变化的关系：按PA水平分组（GAM模型，Model 3）",
    subtitle = paste("参考点: 睡眠", round(sleep_reference, 2), "小时/天（Medium PA组）| 样本数:", n_after_exclusion),
    x = "睡眠总时长 (小时/天)",
    y = "认知变化预测值（相对于Medium PA组参考点）",
    color = "PA水平",
    fill = "PA水平",
    caption = "阴影区域表示95%置信区间。参考点用红色虚线标示。"
  ) +
  scale_color_manual(values = c("Medium PA" = "#3498db",
                                "Low PA" = "#e74c3c",
                                "High PA" = "#2ecc71")) +
  scale_fill_manual(values = c("Medium PA" = "#3498db",
                               "Low PA" = "#e74c3c",
                               "High PA" = "#2ecc71")) +
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

# 保存分组平滑项曲线图
ggsave(
  filename = path_result("gam_model_sleep_curve_by_pa.png"),
  plot = gam_plot_by_pa,
  width = 12,
  height = 7,
  dpi = 300
)

print("分组平滑项曲线图已保存到: gam_model_sleep_curve_by_pa.png")

# 显示图形
print(gam_plot_by_pa)

print("\n===== 分析完成 =====")
print("GAM模型分析已完成，结果已保存。")
print("注意：GAM模型使用平滑样条来捕捉睡眠总时长与认知变化的非线性关系。")
print("Model 2展示了整体睡眠时长与认知变化的关系。")
print("Model 3展示了不同PA水平下睡眠时长与认知变化的关系（分组平滑项）。")
print("\n所有结果已保存到result文件夹中。")

