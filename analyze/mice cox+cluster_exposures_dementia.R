# 四个暴露变量的聚类分析 + Cox回归
# ==========================================
# 描述: 整合四个暴露变量，进行标准化、Gower距离+PAM聚类，然后进行Cox回归分析

# 1. 准备工作
# ==========================================
library(dplyr)
library(survival)
library(mice)
library(broom)
library(parallel)
library(doParallel)
library(foreach)
source("paths.R")

# 若尚未生成聚类结果，自动调用共享脚本
if (!exists("cluster_result")) {
  print("未检测到 cluster_result，正在运行 B/B.clusterize_exposures.R ...")
  source("B/B.clusterize_exposures.R")
}

# 2. 数据准备与整合
# ==========================================
print("步骤 A: 正在整合聚类标签与协变量...")

cluster_labels <- cluster_result %>%
  select(Participant_ID, cluster_label)

baseline_table_step1_clean <- baseline_table_step1 %>%
  select(-any_of(c("cognitive_score_0")))

cognitive_score_clean <- cognitive_scores %>%
  select(
    Participant_ID,
    cognitive_score_0
  )

full_merged_table <- final_dataset %>%
  left_join(baseline_table_step1_clean, by = "Participant_ID") %>%
  left_join(cluster_labels, by = "Participant_ID") %>%
  left_join(cognitive_score_clean, by = "Participant_ID") %>%
  mutate(
    cluster_label = factor(cluster_label, levels = c(4, 1, 2, 3))
  )

print("步骤 A 完成，`full_merged_table` 已创建。")

# 9. 多重插补
# ==========================================
print("步骤 H: 开始执行多重插补...")

# 定义模型需要的所有变量
all_model_vars <- c(
  "Participant_ID", "survival_time_days", "dementia_status",
  "cluster_label",  # 聚类结果作为暴露变量
  "age_recruitment", "sex", "townsend_index", "bmi", 
  "smoking_status", "alcohol_status", "education_level", "cvd_history_any", 
  "history_diabetes", "cognitive_score_0"
)

# 选择用于插补的数据
imputation_data <- full_merged_table %>%
  select(all_of(all_model_vars))

# 先剔除暴露变量和结局变量有缺失的样本（不插补这些变量）
print("剔除暴露变量和结局变量有缺失的样本...")
n_before <- nrow(imputation_data)
imputation_data <- imputation_data %>%
  filter(
    !is.na(survival_time_days) &
    !is.na(dementia_status) &
    !is.na(cluster_label)
  )
n_after <- nrow(imputation_data)
print(paste("剔除前样本数:", n_before, "| 剔除后样本数:", n_after, "| 剔除样本数:", n_before - n_after))

imputation_data_for_mice <- imputation_data %>%
  select(-Participant_ID)

# 执行多重插补
n_cores <- detectCores() - 1
if (n_cores < 1) n_cores <- 1

print(paste("检测到", n_cores, "个CPU核心可用于并行处理"))

# 只对协变量进行插补，暴露变量和结局变量不插补（设置为空字符串）
method_specification <- c(
  "survival_time_days" = "",  # 不插补结局变量
  "dementia_status" = "",     # 不插补结局变量
  "cluster_label" = "",        # 不插补暴露变量
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
print(paste("已启用", n_cores, "个CPU核心用于后续并行处理"))

# 10. 在每个插补数据集上运行Cox回归分析
# ==========================================
print("步骤 I: 正在对插补数据集运行Cox模型...")

# 定义三个模型的公式
formula_model0 <- as.formula("Surv(survival_time_days, dementia_status) ~ cluster_label")
formula_model1 <- as.formula("Surv(survival_time_days, dementia_status) ~ cluster_label + age_recruitment + sex")
all_covariates <- c(
  "age_recruitment", "sex", "townsend_index", "bmi", "smoking_status",
  "alcohol_status", "education_level", "cvd_history_any", 
  "history_diabetes", "cognitive_score_0"
)
formula_model2 <- as.formula(
  paste("Surv(survival_time_days, dementia_status) ~ cluster_label +", 
        paste(all_covariates, collapse = " + "))
)

# 并行拟合模型
fit_models_parallel <- function(i) {
  completed_data <- complete(imputed_object, i)
  
  # 确保cluster_label是因子类型并设置参照组为Cluster 4
  completed_data$cluster_label <- factor(completed_data$cluster_label)
  # 设置Cluster 4作为参照组
  completed_data$cluster_label <- relevel(completed_data$cluster_label, ref = "4")
  
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

print("模型拟合完成！")

# 11. 汇总所有分析结果 (Pool)
# ==========================================
print("步骤 J: 正在汇总分析结果...")

pooled_model0 <- pool(model0_list)
pooled_model1 <- pool(model1_list)
pooled_model2 <- pool(model2_list)

model0_tidy <- tidy(pooled_model0, conf.int = TRUE, exponentiate = TRUE)
model1_tidy <- tidy(pooled_model1, conf.int = TRUE, exponentiate = TRUE)
model2_tidy <- tidy(pooled_model2, conf.int = TRUE, exponentiate = TRUE)

model0_tidy$model <- "Model 0 (Unadjusted)"
model1_tidy$model <- "Model 1 (Adjusted for Demographics)"
model2_tidy$model <- "Model 2 (Fully Adjusted)"

all_models_summary_cox_cluster <- bind_rows(model0_tidy, model1_tidy, model2_tidy)

final_report_table_cox_cluster <- all_models_summary_cox_cluster %>%
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

# 12. 查看与导出结果
# ==========================================
print("===== 结果：聚类结果 vs 痴呆风险 (Cox模型) =====")
print(as.data.frame(final_report_table_cox_cluster))

write.csv(final_report_table_cox_cluster, 
          file = path_result("cox_model_cluster_k4_results.csv"), 
          row.names = FALSE)

# 聚类信息（若已由 B/B.clusterize_exposures.R 生成，则直接沿用）
if (exists("cluster_info")) {
  print("\n聚类特征:")
  print(cluster_info)
} else {
  print("\n未检测到 cluster_info，可运行 B/B.clusterize_exposures.R 获取聚类特征摘要。")
}

# 保存聚类分配结果（若对象存在）
if (exists("cluster_result")) {
  write.csv(
    cluster_result,
    file = path_result("cluster_k4_assignments.csv"),
    row.names = FALSE
  )
}

# 保存聚类评估指标（由于直接使用k=4，评估指标不再计算）
# clustering_evaluation <- data.frame(
#   k = k_range,
#   silhouette_score = silhouette_scores,
#   within_cluster_sum_squares = withinss_scores
# )
# 
# write.csv(clustering_evaluation, 
#           file = path_result("clustering_k4_evaluation.csv"), 
#           row.names = FALSE)

print("\n所有结果已保存到result文件夹中。")

# 美化输出结果表格
# ==========================================
print("步骤 K: 正在美化输出结果表格...")
source("beautify_results.R")
beautify_model_results(
  path_result("cox_model_cluster_k4_results.csv"),
  path_result("cox_model_cluster_k4_results_beautified.png")
)
print("步骤 K 完成，美化表格已生成。")

