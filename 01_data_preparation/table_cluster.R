# 1. 准备工作 ---------------------------------------------------------------
# 说明：该脚本仿照 table1/table.R 的清洗流程，但以聚类结果 (cluster_label)
#       为分组依据，最终生成 baseline_table_cluster 供 Table 1 / 其他分析使用。

library(dplyr)
library(stringr)
library(tidyr)
source("paths.R")

# 确保 cluster_result 存在；若无，则从 result/cluster_k4_assignments.csv 读取
if (!exists("cluster_result")) {
  cluster_csv_path <- path_result("cluster_k4_assignments.csv")
  if (!file.exists(cluster_csv_path)) {
    stop("缺少 cluster_result，且未找到 cluster_k4_assignments.csv，无法继续。")
  }
  cluster_result <- read.csv(cluster_csv_path)
}

cluster_assignments <- cluster_result %>%
  select(Participant_ID, cluster_label) %>%
  mutate(
    cluster_label = factor(
      cluster_label,
      levels = c(4, 1, 2, 3),
      labels = c("Cluster 4", "Cluster 1", "Cluster 2", "Cluster 3")
    )
  )


# 2. 核心逻辑 ---------------------------------------------------------------
# 目标：构建 baseline_table_cluster，包含所有协变量并附带 cluster_label

# 步骤 A: 清理并准备人口学数据框
demographics_clean <- demographic_charateristics %>%
  rename(
    Participant_ID = `Participant ID`,
    age_recruitment = `Age at recruitment`,
    birth_month = `Month of birth`,
    birth_year = `Year of birth`,
    sex = `Sex`,
    townsend_index = `Townsend deprivation index at recruitment`
  ) %>%
  mutate(
    birth_year_month = str_c(birth_year, str_pad(birth_month, 2, pad = "0"), sep = "-"),
    sex = factor(sex, levels = c(0, 1), labels = c("Female", "Male"))
  ) %>%
  select(
    Participant_ID,
    age_recruitment,
    birth_year_month,
    sex,
    townsend_index
  )

# 将人口学信息与聚类标签合并
baseline_table_cluster <- main_exposure_table %>%
  left_join(cluster_assignments, by = "Participant_ID") %>%
  left_join(demographics_clean, by = "Participant_ID")


###### 种族 ---------------------------------------------------------------
ethnicity_clean <- Ethnictiy %>%
  select(
    Participant_ID = `Participant ID`,
    ethnicity_code = `Ethnic background | Instance 0`
  ) %>%
  mutate(
    ethnicity_code = na_if(ethnicity_code, -1),
    ethnicity_code = na_if(ethnicity_code, -3),
    ethnicity_group = case_when(
      ethnicity_code %in% c(1, 1001, 1002, 1003) ~ "White",
      ethnicity_code %in% c(
        2, 2001, 2002, 2003, 2004,
        3, 3001, 3002, 3003, 3004,
        4, 4001, 4002, 4003,
        5, 6
      ) ~ "Non-white",
      TRUE ~ NA_character_
    ),
    ethnicity = factor(ethnicity_group, levels = c("White", "Non-white"))
  ) %>%
  select(Participant_ID, ethnicity)

baseline_table_cluster <- baseline_table_cluster %>%
  left_join(ethnicity_clean, by = "Participant_ID")


###### 学历 ---------------------------------------------------------------
education_clean <- Education %>%
  select(
    Participant_ID = `Participant ID`,
    qualifications_str = `Qualifications | Instance 2`
  ) %>%
  separate_rows(qualifications_str, sep = "\\|", convert = TRUE) %>%
  filter(qualifications_str != -3) %>%
  group_by(Participant_ID) %>%
  summarise(
    highest_qual_code = min(qualifications_str, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    education_level_text = case_when(
      highest_qual_code %in% c(1, 5, 6) ~ "College and above",
      highest_qual_code == 2 ~ "High school or equivalent",
      highest_qual_code %in% c(3, 4, -7) ~ "Below high school",
      TRUE ~ NA_character_
    ),
    education_level = factor(
      education_level_text,
      levels = c("Below high school", "High school or equivalent", "College and above"),
      ordered = TRUE
    )
  ) %>%
  select(Participant_ID, education_level)

baseline_table_cluster <- baseline_table_cluster %>%
  left_join(education_clean, by = "Participant_ID")


###### BMI ---------------------------------------------------------------
bmi_clean <- BMI %>%
  select(
    Participant_ID = `Participant ID`,
    bmi = `Body mass index (BMI) | Instance 2(Body size measures)`
  ) %>%
  mutate(
    bmi_group_text = case_when(
      bmi < 25 ~ "Normal or Underweight (<25)",
      bmi >= 25 & bmi < 30 ~ "Overweight (25-29.9)",
      bmi >= 30 ~ "Obese (>=30)",
      TRUE ~ NA_character_
    ),
    bmi_group = factor(
      bmi_group_text,
      levels = c("Normal or Underweight (<25)", "Overweight (25-29.9)", "Obese (>=30)"),
      ordered = TRUE
    )
  ) %>%
  select(Participant_ID, bmi, bmi_group)

baseline_table_cluster <- baseline_table_cluster %>%
  left_join(bmi_clean, by = "Participant_ID")


###### Smoking -----------------------------------------------------------
smoking_clean <- Smokeing %>%
  select(
    Participant_ID = `Participant ID`,
    current_smoking = `Current tobacco smoking | Instance 2`,
    past_smoking = `Past tobacco smoking | Instance 2`
  ) %>%
  mutate(
    smoking_status_text = case_when(
      current_smoking %in% c(1, 2) ~ "Current smoker",
      current_smoking == 0 & past_smoking %in% c(1, 2, 3) ~ "Former smoker",
      current_smoking == 0 & past_smoking == 4 ~ "Never smoker",
      current_smoking %in% c(-1, -3, -7) | past_smoking %in% c(-1, -3, -7) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    smoking_status = factor(
      smoking_status_text,
      levels = c("Never smoker", "Former smoker", "Current smoker"),
      ordered = TRUE
    )
  ) %>%
  select(Participant_ID, smoking_status)

baseline_table_cluster <- baseline_table_cluster %>%
  left_join(smoking_clean, by = "Participant_ID")


###### Alcohol -----------------------------------------------------------
alcohol_clean <- Alcohol %>%
  select(
    Participant_ID = `Participant ID`,
    alcohol_code = `Alcohol drinker status | Instance 2`
  ) %>%
  mutate(
    alcohol_code = na_if(alcohol_code, -3),
    alcohol_status_text = case_when(
      alcohol_code == 0 ~ "Never drinker",
      alcohol_code == 1 ~ "Former drinker",
      alcohol_code == 2 ~ "Current drinker",
      TRUE ~ NA_character_
    ),
    alcohol_status = factor(
      alcohol_status_text,
      levels = c("Never drinker", "Former drinker", "Current drinker"),
      ordered = TRUE
    )
  ) %>%
  select(Participant_ID, alcohol_status)

baseline_table_cluster <- baseline_table_cluster %>%
  left_join(alcohol_clean, by = "Participant_ID")


###### Sedentary ---------------------------------------------------------
sedentary_clean <- Derived_accelerometry %>%
  select(
    Participant_ID = `Participant ID`,
    sedentary_prop = `Sedentary - Overall average | Instance 0`
  ) %>%
  mutate(
    sedentary_hours = sedentary_prop * 24
  )

baseline_table_cluster <- baseline_table_cluster %>%
  left_join(sedentary_clean, by = "Participant_ID")


###### Cardiovascular history --------------------------------------------
cvd_clean <- Medical_conditions %>%
  select(
    Participant_ID = `Participant ID`,
    cvd_codes_str = `Vascular/heart problems diagnosed by doctor | Instance 2`
  ) %>%
  mutate(
    cvd_codes_str = na_if(cvd_codes_str, "-3"),
    history_heart_attack = as.integer(str_detect(cvd_codes_str, "1")),
    history_angina = as.integer(str_detect(cvd_codes_str, "2")),
    history_stroke = as.integer(str_detect(cvd_codes_str, "3")),
    history_high_bp = as.integer(str_detect(cvd_codes_str, "4")),
    cvd_history_any = as.integer(
      history_heart_attack == 1 |
        history_angina == 1 |
        history_stroke == 1 |
        history_high_bp == 1
    ),
    cvd_history_any = factor(cvd_history_any, levels = c(0, 1), labels = c("No", "Yes"))
  ) %>%
  select(
    Participant_ID,
    cvd_history_any,
    history_heart_attack,
    history_angina,
    history_stroke,
    history_high_bp
  )

baseline_table_cluster <- baseline_table_cluster %>%
  left_join(cvd_clean, by = "Participant_ID")


###### Diabetes ----------------------------------------------------------
diabetes_clean <- Medical_conditions %>%
  select(
    Participant_ID = `Participant ID`,
    diabetes_code = `Diabetes diagnosed by doctor | Instance 2`
  ) %>%
  mutate(
    diabetes_code = na_if(diabetes_code, -1),
    diabetes_code = na_if(diabetes_code, -3),
    history_diabetes = factor(
      diabetes_code,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  ) %>%
  select(Participant_ID, history_diabetes)

baseline_table_cluster <- baseline_table_cluster %>%
  left_join(diabetes_clean, by = "Participant_ID")


###### Cognitive score ---------------------------------------------------
if (!exists("cognitive_score_clean")) {
  cognitive_score_clean <- cognitive_scores %>%
    select(
      Participant_ID,
      cognitive_score_0
    )
}

baseline_table_cluster <- baseline_table_cluster %>%
  left_join(cognitive_score_clean %>% select(Participant_ID, cognitive_score_0),
            by = "Participant_ID")


print("baseline_table_cluster 已创建，可用于按聚类分组的 Table 1 分析。")


# ==============================================================================
# 3. 生成 TableOne 基线特征表 (按聚类分组)
# ==============================================================================

library(tableone)
library(knitr)
library(kableExtra)

# 定义变量列表
table_vars <- c(
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

cat_vars <- c(
  "sex", "ethnicity", "education_level", "bmi_group",
  "smoking_status", "alcohol_status", "cvd_history_any", "history_diabetes"
)

nonnormal_vars <- c(
  "age_recruitment", "townsend_index", "bmi",
  "sedentary_hours", "cognitive_score_0"
)

# 创建 TableOne 对象
table_one_cluster <- CreateTableOne(
  vars = table_vars,
  strata = "cluster_label",
  data = baseline_table_cluster,
  factorVars = cat_vars
)

# 打印结果（含 SMD）
print(
  table_one_cluster,
  smd = TRUE,
  showAllLevels = TRUE,
  quote = FALSE,
  noSpaces = TRUE,
  nonnormal = nonnormal_vars,
  contDigits = 1,
  catDigits = 1
)

# 获取矩阵形式
table_one_cluster_matrix <- print(
  table_one_cluster,
  smd = TRUE,
  showAllLevels = TRUE,
  quote = FALSE,
  noSpaces = TRUE,
  printToggle = FALSE,
  nonnormal = nonnormal_vars
)

# 使用 kable 美化并保存
kable(
  table_one_cluster_matrix,
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
