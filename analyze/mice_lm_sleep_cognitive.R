# 睡眠总时长与认知变化的线性模型（LM）分析
# ==========================================
# 使用多重插补和Rubin法则汇总结果

# 1. 准备工作
# ==========================================
# 加载我们需要的包
library(dplyr)
library(mice)
library(miceadds)
library(broom)
library(parallel)
library(doParallel)
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

# 步骤 B1: 创建睡眠分组变量（6-8小时和8-12小时）
print("步骤 B1: 正在创建睡眠分组变量...")
full_merged_table <- full_merged_table %>%
  mutate(
    sleep_group = case_when(
      is.na(sleep_hours_per_day) ~ NA_character_,
      sleep_hours_per_day >= 6 & sleep_hours_per_day < 8 ~ "6-8 hours",
      sleep_hours_per_day >= 8 & sleep_hours_per_day <= 12 ~ "8-12 hours",
      TRUE ~ NA_character_  # 不在范围内的设为NA
    ),
    sleep_group = factor(sleep_group, levels = c("6-8 hours", "8-12 hours"))
  )

# 统计各组的样本量
sleep_group_counts <- full_merged_table %>%
  filter(!is.na(sleep_group)) %>%
  count(sleep_group)
print("\n睡眠分组样本量:")
print(sleep_group_counts)

# 统计被排除的样本（不在6-12小时范围内）
n_excluded_sleep_range <- sum(!is.na(full_merged_table$sleep_hours_per_day) & is.na(full_merged_table$sleep_group))
print(paste("睡眠时长不在6-12小时范围内的样本数:", n_excluded_sleep_range))
print("步骤 B1 完成。")

# 步骤 B2: 创建PA_level分组（基于MVPA三分位）
print("步骤 B2: 正在创建PA_level分组...")
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
    PA_level = factor(PA_level, levels = c("Low PA", "Medium PA", "High PA"))
  )

# 统计各组的样本量
pa_level_counts <- full_merged_table %>%
  filter(!is.na(PA_level)) %>%
  count(PA_level)
print("\nPA_level分组样本量:")
print(pa_level_counts)
print("步骤 B2 完成。")

# 3. 多重插补 (Impute)
# ==========================================

# 步骤 C: 定义我们最终模型需要的所有变量
all_model_vars <- c(
  "Participant_ID", 
  "cognitive_change",  # <--- 结局变量
  "sleep_group",  # <--- 核心自变量（分组变量）
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
n_missing_sleep <- sum(is.na(imputation_data$sleep_group))
n_missing_pa <- sum(is.na(imputation_data$PA_level))
n_missing_either <- sum(is.na(imputation_data$cognitive_change) | 
                        is.na(imputation_data$sleep_group) |
                        is.na(imputation_data$PA_level))

print(paste("剔除前样本数:", n_before_exclusion))
print(paste("cognitive_change 缺失数:", n_missing_outcome, "(", round(n_missing_outcome/n_before_exclusion * 100, 2), "%)"))
print(paste("sleep_group 缺失数:", n_missing_sleep, "(", round(n_missing_sleep/n_before_exclusion * 100, 2), "%)"))
print(paste("PA_level 缺失数:", n_missing_pa, "(", round(n_missing_pa/n_before_exclusion * 100, 2), "%)"))
print(paste("至少一个缺失的数量:", n_missing_either, "(", round(n_missing_either/n_before_exclusion * 100, 2), "%)"))

# 剔除缺失值
imputation_data <- imputation_data %>%
  filter(!is.na(cognitive_change) & !is.na(sleep_group) & !is.na(PA_level))

n_after_exclusion <- nrow(imputation_data)
n_excluded <- n_before_exclusion - n_after_exclusion
print(paste("剔除后样本数:", n_after_exclusion))
print(paste("剔除样本数:", n_excluded, "(", round(n_excluded/n_before_exclusion * 100, 2), "%)"))
print("步骤 D1 完成。")

# 打印最终纳入分析的样本量
print("\n===== 最终纳入分析的样本量 =====")
print(paste("最终分析样本数:", n_after_exclusion))

# 准备用于插补的数据（移除Participant_ID）
imputation_data_for_mice <- imputation_data %>%
  select(-Participant_ID)

# 步骤 E: 执行多重插补
print("步骤 E: 开始执行多重插补，请耐心等待...")
print("注意：cognitive_change、sleep_group 和 PA_level 不参与插补（已剔除缺失值）")

# 检测可用CPU核心数，设置并行处理
n_cores <- detectCores() - 1  # 保留一个核心给系统
if (n_cores < 1) n_cores <- 1

print(paste("检测到", n_cores, "个CPU核心可用于并行处理"))

# 为每个变量明确指定插补方法
# 注意：cognitive_change、sleep_group 和 PA_level 设置为 ""，表示不插补
method_specification <- c(
  "cognitive_change" = "",  # 不插补，已剔除缺失值
  "sleep_group" = "",  # 不插补，已剔除缺失值
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



# 4. 在每个插补数据集上运行线性模型分析 (Analyze)
# ==========================================

# 步骤 F: 定义我们三个线性模型的公式
# 使用sleep_group和PA_level的交互项（6-8小时和Low PA作为参考组）
formula_lm0 <- as.formula("cognitive_change ~ sleep_group * PA_level")
formula_lm1 <- as.formula("cognitive_change ~ sleep_group * PA_level + age_recruitment + sex")
all_covariates <- c(
  "age_recruitment", "sex", "townsend_index", "bmi", "smoking_status",
  "alcohol_status", "education_level", "cvd_history_any", 
  "history_diabetes", "cognitive_score_0"
)
formula_lm2 <- as.formula(
  paste("cognitive_change ~ sleep_group * PA_level +", 
        paste(all_covariates, collapse = " + "))
)

# 步骤 G: 并行运行这三个线性模型
print("步骤 G: 正在并行对插补数据集运行3个线性模型...")

# 定义并行拟合函数
fit_models_parallel <- function(i) {
  completed_data <- complete(imputed_object, i)
  
  # 确保sleep_group是因子类型（6-8 hours作为参考组）
  completed_data$sleep_group <- factor(completed_data$sleep_group, 
                                       levels = c("6-8 hours", "8-12 hours"))
  
  # 确保PA_level是因子类型（Low PA作为参考组）
  completed_data$PA_level <- factor(completed_data$PA_level, 
                                    levels = c("Low PA", "Medium PA", "High PA"))
  
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

all_models_summary_lm_sleep <- bind_rows(model0_tidy, model1_tidy, model2_tidy)

final_report_table_lm_sleep <- all_models_summary_lm_sleep %>%
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

print("===== 结果：sleep_group * PA_level vs 认知变化 (线性模型，含交互项) =====")
print(as.data.frame(final_report_table_lm_sleep))

write.csv(final_report_table_lm_sleep, file = path_result("linear_model_sleep_pa_interaction_results.csv"), row.names = FALSE)

print("所有结果已保存到result文件夹中。")

print("\n===== 分析完成 =====")
print("线性模型分析已完成，结果已保存。")
print("注意：")
print("- 睡眠分组变量中，'6-8 hours' 作为参考组")
print("- PA分组变量中，'Low PA' 作为参考组")
print("- 交互项系数表示不同PA水平下睡眠时长效应的差异")

