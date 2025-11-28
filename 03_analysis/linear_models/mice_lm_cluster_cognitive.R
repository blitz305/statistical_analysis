# 聚类暴露 (cluster_label) 对认知变化 (cognitive_change) 的线性模型分析
# 依赖：已运行 `mice cox+cluster_exposures_dementia.R` 并在当前环境中生成 `cluster_result`

# 1. 准备工作 ---------------------------------------------------------------
library(dplyr)
library(mice)
library(broom)
library(parallel)
library(doParallel)
library(foreach)
source("paths.R")

if (!exists("cluster_result")) {
  stop("缺少 `cluster_result` 对象，请先运行 mice cox+cluster_exposures_dementia.R 以生成聚类结果。")
}


# 2. 数据整合 ---------------------------------------------------------------
print("步骤 A: 正在准备聚类标签与协变量数据...")

cluster_labels <- cluster_result %>%
  select(Participant_ID, cluster_label)

baseline_table_step1_clean <- baseline_table_step1 %>%
  select(-any_of(c("cognitive_score_0")))

cognitive_score_clean <- cognitive_scores %>%
  select(
    Participant_ID,
    cognitive_score_0,
    cognitive_change
  )

full_merged_table <- final_dataset %>%
  left_join(baseline_table_step1_clean, by = "Participant_ID") %>%
  left_join(cluster_labels, by = "Participant_ID") %>%
  left_join(cognitive_score_clean, by = "Participant_ID") %>%
  mutate(
    cluster_label = factor(cluster_label, levels = c(4, 1, 2, 3))
  )

print("步骤 A 完成，`full_merged_table` 已创建。")


# 3. 多重插补准备 -----------------------------------------------------------
print("步骤 B: 选择模型变量并准备插补数据...")

all_model_vars <- c(
  "Participant_ID",
  "cognitive_change",          # 结局变量
  "cluster_label",             # 聚类暴露变量
  "age_recruitment", "sex", "townsend_index", "bmi",
  "smoking_status", "alcohol_status", "education_level",
  "cvd_history_any", "history_diabetes", "cognitive_score_0"
)

imputation_data <- full_merged_table %>%
  select(all_of(all_model_vars))

print("步骤 C: 统计结局或聚类标签的缺失情况（将被剔除）...")
n_before <- nrow(imputation_data)
n_missing_outcome <- sum(is.na(imputation_data$cognitive_change))
n_missing_exposure <- sum(is.na(imputation_data$cluster_label))
n_missing_either <- sum(is.na(imputation_data$cognitive_change) | is.na(imputation_data$cluster_label))

print(paste("样本总数:", n_before))
print(paste("cognitive_change 缺失:", n_missing_outcome))
print(paste("cluster_label 缺失:", n_missing_exposure))
print(paste("至少一个缺失:", n_missing_either))

imputation_data <- imputation_data %>%
  filter(
    !is.na(cognitive_change),
    !is.na(cluster_label)
  )

n_after <- nrow(imputation_data)
print(paste("剔除后样本数:", n_after, "| 剔除数量:", n_before - n_after))

imputation_data_for_mice <- imputation_data %>%
  mutate(
    cluster_label = factor(cluster_label, levels = c(4, 1, 2, 3))
  ) %>%
  select(-Participant_ID)


# 4. 执行多重插补 -----------------------------------------------------------
print("步骤 D: 开始执行多重插补，请稍候...")

n_cores <- detectCores() - 1
if (n_cores < 1) n_cores <- 1
print(paste("并行核心数:", n_cores))

method_specification <- c(
  "cognitive_change" = "",   # 不插补结局
  "cluster_label" = "",      # 不插补聚类标签
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

print("多重插补完成！")

registerDoParallel(cores = n_cores)
print(paste("已启用", n_cores, "个核心用于模型拟合"))


# 5. 线性模型拟合 -----------------------------------------------------------
print("步骤 E: 在每个插补数据集上运行线性模型...")

formula_lm0 <- as.formula("cognitive_change ~ cluster_label")
formula_lm1 <- as.formula("cognitive_change ~ cluster_label + age_recruitment + sex")
all_covariates <- c(
  "age_recruitment", "sex", "townsend_index", "bmi",
  "smoking_status", "alcohol_status", "education_level",
  "cvd_history_any", "history_diabetes", "cognitive_score_0"
)
formula_lm2 <- as.formula(
  paste("cognitive_change ~ cluster_label +", paste(all_covariates, collapse = " + "))
)

fit_models_parallel <- function(i) {
  completed_data <- complete(imputed_object, i)
  completed_data$cluster_label <- factor(completed_data$cluster_label, levels = c(4, 1, 2, 3))
  
  list(
    model0 = lm(formula_lm0, data = completed_data),
    model1 = lm(formula_lm1, data = completed_data),
    model2 = lm(formula_lm2, data = completed_data)
  )
}

model_results <- foreach(i = 1:imputed_object$m, .packages = c("mice")) %dopar% {
  fit_models_parallel(i)
}

lm_model0_list <- lapply(model_results, function(x) x$model0)
lm_model1_list <- lapply(model_results, function(x) x$model1)
lm_model2_list <- lapply(model_results, function(x) x$model2)

print("模型拟合完成！")


# 6. 汇总与导出 -------------------------------------------------------------
print("步骤 F: 汇总三个模型的结果...")

pooled_lm0 <- pool(lm_model0_list)
pooled_lm1 <- pool(lm_model1_list)
pooled_lm2 <- pool(lm_model2_list)

model0_tidy <- tidy(pooled_lm0, conf.int = TRUE)
model1_tidy <- tidy(pooled_lm1, conf.int = TRUE)
model2_tidy <- tidy(pooled_lm2, conf.int = TRUE)

model0_tidy$model <- "Model 0 (Unadjusted)"
model1_tidy$model <- "Model 1 (Adjusted for Demographics)"
model2_tidy$model <- "Model 2 (Fully Adjusted)"

all_models_summary_lm_cluster <- bind_rows(model0_tidy, model1_tidy, model2_tidy)

final_report_table_lm_cluster <- all_models_summary_lm_cluster %>%
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

print("===== 结果：聚类暴露 vs 认知变化 (线性模型) =====")
print(as.data.frame(final_report_table_lm_cluster))

output_csv <- path_result("linear_model_cluster_results.csv")
write.csv(final_report_table_lm_cluster, file = output_csv, row.names = FALSE)
print(paste("结果表已保存到:", output_csv))



print("全部流程完成。")

