# B.clusterize_exposures.R
# ==========================================
# 目的：复用四个暴露变量（overall MVPA、regularity_score、activity_time_pattern、exposure_group），
#       执行 K-prototypes 聚类并产出 `cluster_result`、`cluster_info`（以及中间对象），供下游分析使用。
#
# 依赖：请先加载以下数据框
#   - Derived_accelerometry
#   - regularity_score
#   - mvpa_time_pattern
#   - mvpa_classify  (由 B.classify.R 生成)
#   - final_dataset
#   - baseline_table_step1
#   - cognitive_scores
#
# 输出（保存在全局环境，可视需要写入文件）：
#   - cluster_result: Participant_ID + cluster_label + 暴露变量
#   - cluster_info: 每个聚类的样本量与暴露特征摘要
#   - kproto_result: K-prototypes 模型对象

# 1. 加载依赖 ---------------------------------------------------------------
library(dplyr)
library(clustMixType)
source("paths.R")

required_objects <- c(
  "Derived_accelerometry",
  "regularity_score",
  "mvpa_time_pattern",
  "mvpa_classify",
  "final_dataset",
  "baseline_table_step1",
  "cognitive_scores"
)
missing_objects <- required_objects[!sapply(required_objects, exists)]
if (length(missing_objects) > 0) {
  stop(paste(
    "缺少以下数据框，无法运行聚类：",
    paste(missing_objects, collapse = ", ")
  ))
}


# 2. 整理暴露变量 -----------------------------------------------------------
print("聚类步骤 A: 正在准备四个暴露变量...")

mvpa_clean <- Derived_accelerometry %>%
  transmute(
    Participant_ID = `Participant ID`,
    overall_mvpa_minutes = `Moderate-Vigorous - Overall average | Instance 0` * 24
  )

regularity_score_clean <- regularity_score %>%
  transmute(
    Participant_ID = participantid,
    regularity_score = regularity_score * -1
  )

activity_time_pattern_clean <- mvpa_time_pattern %>%
  select(
    Participant_ID,
    activity_time_pattern
  )

exposure_group_clean <- mvpa_classify %>%
  select(
    Participant_ID = participant_id,
    exposure_group = mvpa_activity_pattern
  )

cognitive_score_clean <- cognitive_scores %>%
  select(
    Participant_ID,
    cognitive_score_0
  )

baseline_table_step1_clean <- baseline_table_step1 %>%
  select(-any_of(c("cognitive_score_0")))

full_merged_table <- final_dataset %>%
  left_join(baseline_table_step1_clean, by = "Participant_ID") %>%
  left_join(mvpa_clean, by = "Participant_ID") %>%
  left_join(regularity_score_clean, by = "Participant_ID") %>%
  left_join(activity_time_pattern_clean, by = "Participant_ID") %>%
  left_join(exposure_group_clean, by = "Participant_ID") %>%
  left_join(cognitive_score_clean, by = "Participant_ID")

print("聚类步骤 A 完成。")


# 3. 构建聚类数据集 ---------------------------------------------------------
print("聚类步骤 B: 构建用于聚类的数据集...")

cluster_data <- full_merged_table %>%
  select(
    Participant_ID,
    overall_mvpa_minutes,
    regularity_score,
    activity_time_pattern,
    exposure_group
  ) %>%
  filter(
    !is.na(overall_mvpa_minutes),
    !is.na(regularity_score),
    !is.na(activity_time_pattern),
    !is.na(exposure_group)
  )

print(paste("用于聚类的样本数:", nrow(cluster_data)))

cluster_data <- cluster_data %>%
  mutate(
    overall_mvpa_minutes_z = scale(overall_mvpa_minutes)[, 1],
    regularity_score_z = scale(regularity_score)[, 1],
    activity_time_pattern = factor(activity_time_pattern),
    exposure_group = factor(exposure_group)
  )

cluster_data_for_model <- cluster_data %>%
  select(
    overall_mvpa_minutes_z,
    regularity_score_z,
    activity_time_pattern,
    exposure_group
  )

print("聚类数据集准备完成。")


# 4. 执行 K-prototypes 聚类 -------------------------------------------------
print("聚类步骤 C: 执行 K-prototypes 聚类 (k = 4)...")

optimal_k <- 4
kproto_result <- kproto(
  cluster_data_for_model,
  k = optimal_k,
  nstart = 10,
  verbose = TRUE
)

cluster_data <- cluster_data %>%
  mutate(cluster_label = kproto_result$cluster)

print("聚类完成，分布如下：")
print(table(cluster_data$cluster_label, useNA = "ifany"))


# 5. 输出聚类结果 -----------------------------------------------------------
cluster_result <- cluster_data %>%
  select(
    Participant_ID,
    cluster_label,
    overall_mvpa_minutes,
    regularity_score,
    activity_time_pattern,
    exposure_group
  )

cluster_info <- cluster_data %>%
  group_by(cluster_label) %>%
  summarise(
    n = n(),
    mean_mvpa = mean(overall_mvpa_minutes, na.rm = TRUE),
    mean_regularity = mean(regularity_score, na.rm = TRUE),
    activity_time_pattern_dist = {
      pattern_table <- table(activity_time_pattern)
      pattern_levels <- names(pattern_table)
      paste(
        paste0(pattern_levels, ": ", as.numeric(pattern_table)),
        collapse = "; "
      )
    },
    exposure_group_dist = {
      group_table <- table(exposure_group)
      group_levels <- names(group_table)
      paste(
        paste0(group_levels, ": ", as.numeric(group_table)),
        collapse = "; "
      )
    },
    .groups = "drop"
  )

assign("cluster_result", cluster_result, envir = .GlobalEnv)
assign("cluster_info", cluster_info, envir = .GlobalEnv)
assign("kproto_result", kproto_result, envir = .GlobalEnv)

print("聚类结果已生成并写入全局环境：cluster_result / cluster_info / kproto_result")

# 可选：保存结果文件
assignments_path <- path_result("cluster_k4_assignments.csv")
info_path <- path_result("cluster_k4_characteristics.csv")
model_path <- path_result("cluster_k4_model.rds")

write.csv(cluster_result, file = assignments_path, row.names = FALSE)
write.csv(cluster_info, file = info_path, row.names = FALSE)
saveRDS(kproto_result, file = model_path)

print("聚类输出已保存到 result/ 目录。")


# 6. 从已保存的 CSV 恢复 cluster_result（若环境中缺失） ----------------------
if (!exists("cluster_result")) {
  print("未检测到 cluster_result，将尝试从已保存的 CSV 恢复...")
  recover_assignments_path <- path_result("cluster_k4_assignments.csv")
  if (file.exists(recover_assignments_path)) {
    restored_cluster_result <- read.csv(recover_assignments_path)
    assign("cluster_result", restored_cluster_result, envir = .GlobalEnv)
    print("cluster_result 已从 CSV 恢复。")
  } else {
    warning("未找到 cluster_k4_assignments.csv，无法恢复 cluster_result。")
  }
}

