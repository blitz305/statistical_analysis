# 基于聚类结果 (cluster_label) 生成 TableOne 摘要
# -------------------------------------------------
# 此脚本复用 table1/picture.R 的流程，只是将分层变量替换为聚类分组。

library(tableone)
library(dplyr)
library(knitr)
library(kableExtra)

# 确保 baseline_table_cluster 已经由 table1/table_cluster.R 创建
if (!exists("baseline_table_cluster")) {
  stop("缺少 baseline_table_cluster，请先运行 table1/table_cluster.R。")
}

# 1. 定义变量列表 ----------------------------------------------------------
myVars <- c(
  "age_recruitment",
  "sex",
  "townsend_index",
  "ethnicity",
  "education_level",
  "bmi",
  "bmi_group",
  "smoking_status",
  "alcohol_status",
  "sedentary_hours",
  "cognitive_score_0",
  "cvd_history_any",
  "history_diabetes"
)

catVars <- c(
  "sex", "ethnicity", "education_level", "bmi_group",
  "smoking_status", "alcohol_status", "cvd_history_any", "history_diabetes"
)

# 2. 创建 TableOne 对象 -----------------------------------------------------
tableOneCluster <- CreateTableOne(
  vars = myVars,
  strata = "cluster_label",
  data = baseline_table_cluster,
  factorVars = catVars
)

# 3. 打印结果（含 SMD） ----------------------------------------------------
print(
  tableOneCluster,
  smd = TRUE,
  showAllLevels = TRUE,
  quote = FALSE,
  noSpaces = TRUE,
  nonnormal = c("age_recruitment", "townsend_index", "bmi", "sedentary_hours", "cognitive_score_0"),
  contDigits = 1,
  catDigits = 1
)

# 4. 获取矩阵形式 -----------------------------------------------------------
tableOneClusterMatrix <- print(
  tableOneCluster,
  smd = TRUE,
  showAllLevels = TRUE,
  quote = FALSE,
  noSpaces = TRUE,
  printToggle = FALSE,
  nonnormal = c("age_recruitment", "townsend_index", "bmi", "sedentary_hours", "cognitive_score_0")
)

# 5. 使用 kable 美化 --------------------------------------------------------
kable(
  tableOneClusterMatrix,
  caption = "Table 1: Baseline Characteristics by Cluster Assignment"
) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  ) %>%
  save_kable(
    file = path_result("table_cluster_baseline.html")
  )

print(paste("已将聚类分层基线表保存为:", path_result("table_cluster_baseline.html")))

