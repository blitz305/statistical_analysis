# 1. 准备工作
# ==========================================
# 确保已加载 dplyr 和 stringr 包
library(dplyr)
library(stringr) # 我们用它来补齐月份的0
library(tidyr) # separate_rows 函数需要它

# 确保我们之前创建的 main_exposure_table 已经加载
# ...

# 2. 核心逻辑
# ==========================================

# 步骤 A: 清理并准备人口学数据框
demographics_clean <- demographic_charateristics %>%
  # 选择我们需要的列，并一步到位地进行重命名
  rename(
    Participant_ID = `Participant ID`,
    age_recruitment = `Age at recruitment`,
    birth_month = `Month of birth`,
    birth_year = `Year of birth`,
    sex = `Sex`,
    townsend_index = `Townsend deprivation index at recruitment`
  ) %>%
  # 使用 mutate 创建新列和转换类型
  mutate(
    # 按照您的建议，整合年月。str_pad可以确保月份总是两位数（例如 9 -> "09"）
    birth_year_month = str_c(birth_year, str_pad(birth_month, 2, pad = "0"), sep = "-"),
    
    # 按照您的确认，将sex转换为带有标签的因子
    sex = factor(sex, 
                 levels = c(0, 1), 
                 labels = c("Female", "Male"))
  ) %>%
  # 选择最终需要的列，舍弃临时的 birth_year 和 birth_month
  select(
    Participant_ID,
    age_recruitment,
    birth_year_month,
    sex,
    townsend_index
  )

# 步骤 B: 将清理好的人口学数据，合并到主表中
baseline_table_step1 <- main_exposure_table %>%
  left_join(demographics_clean, by = "Participant_ID")

######种族##########
# 步骤 A: 清理并准备种族数据框
ethnicity_clean <- Ethnictiy %>%
  # 选择我们需要的列，并进行重命名
  select(
    Participant_ID = `Participant ID`,
    ethnicity_code = `Ethnic background | Instance 0`
  ) %>%
  # 使用 mutate 进行数据转换
  mutate(
    # 步骤 1: 将特殊值 (-1, -3) 转换成 NA
    ethnicity_code = na_if(ethnicity_code, -1),
    ethnicity_code = na_if(ethnicity_code, -3),
    
    # 步骤 2: 将所有详细编码归类到二分变量中
    ethnicity_group = case_when(
      ethnicity_code %in% c(1, 1001, 1002, 1003) ~ "White",
      ethnicity_code %in% c(2, 2001, 2002, 2003, 2004, 3, 3001, 3002, 3003, 3004, 
                           4, 4001, 4002, 4003, 5, 6) ~ "Non-white",
      TRUE ~ NA_character_ # 对于已经是NA的，保持为NA
    ),
    
    # 步骤 3: 将文本分组转换为二分因子
    ethnicity = factor(ethnicity_group, levels = c("White", "Non-white"))
  ) %>%
  # 只保留最终需要的ID和处理好的因子列
  select(Participant_ID, ethnicity)

# 步骤 B: 将清理好的种族数据，合并到主表中
baseline_table_step1 <- baseline_table_step1 %>%
  left_join(ethnicity_clean, by = "Participant_ID")

####学历######
# 步骤 A: 清理并准备学历数据框
education_clean <- Education %>%
  # 选择我们需要的列，并进行重命名
  select(
    Participant_ID = `Participant ID`,
    qualifications_str = `Qualifications | Instance 2`
  ) %>%
  # 将 "1|2|3" 这样的字符串拆分成多行
  separate_rows(qualifications_str, sep = "\\|", convert = TRUE) %>%
  
  # 步骤 1: 移除 -3 (拒绝回答)
  filter(qualifications_str != -3) %>%
  
  # 按ID分组，找出每个人的最高学历 (即最小的有效编码)
  group_by(Participant_ID) %>%
  summarise(
    highest_qual_code = min(qualifications_str, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  
  # 步骤 2 & 3: 使用 case_when 进行分组映射，并转换为有序因子
  mutate(
    education_level_text = case_when(
      highest_qual_code %in% c(1, 5, 6) ~ "College and above",
      highest_qual_code == 2 ~ "High school or equivalent",
      highest_qual_code %in% c(3, 4, -7) ~ "Below high school",
      TRUE ~ NA_character_
    ),
    
    education_level = factor(education_level_text, 
                             levels = c("Below high school", "High school or equivalent", "College and above"),
                             ordered = TRUE) # ordered = TRUE 声明这是一个有序分类
  ) %>%
  select(Participant_ID, education_level)


# 步骤 B: 将清理好的学历数据，合并到主表中
baseline_table_step1 <- baseline_table_step1 %>%
  left_join(education_clean, by = "Participant_ID")

###BMI#######
# 步骤 A: 清理并准备BMI数据框
bmi_clean <- BMI %>%
  # 选择我们需要的列，并进行重命名
  select(
    Participant_ID = `Participant ID`,
    bmi = `Body mass index (BMI) | Instance 2(Body size measures)`
  ) %>%
  # 使用 mutate 创建分组
  mutate(
    # 使用 case_when 根据您的标准进行分组
    bmi_group_text = case_when(
      bmi < 25            ~ "Normal or Underweight (<25)",
      bmi >= 25 & bmi < 30 ~ "Overweight (25-29.9)",
      bmi >= 30           ~ "Obese (>=30)",
      TRUE                ~ NA_character_ # 这会处理原始数据中的 NA 值
    ),
    
    # 将文本分组转换为有序因子
    bmi_group = factor(bmi_group_text, 
                       levels = c("Normal or Underweight (<25)", "Overweight (25-29.9)", "Obese (>=30)"),
                       ordered = TRUE)
  ) %>%
  # 我们保留原始的bmi数值和新的bmi_group分组，两者都很有用
  select(Participant_ID, bmi, bmi_group)

# 步骤 B: 将清理好的BMI数据，合并到主表中 (并覆盖原表)
baseline_table_step1 <- baseline_table_step1 %>%
  left_join(bmi_clean, by = "Participant_ID")


###Smokeing######

smoking_clean <- Smokeing %>%
  # 选择我们需要的列，并进行重命名
  select(
    Participant_ID = `Participant ID`,
    current_smoking = `Current tobacco smoking | Instance 2`,
    past_smoking = `Past tobacco smoking | Instance 2`
  ) %>%
  # 使用 mutate 和 case_when 将您的规则转换为代码
  mutate(
    smoking_status_text = case_when(
      # 规则: 当前吸烟者 - 如果个人目前吸烟=大多数日子（1）或偶尔（2）
      current_smoking %in% c(1, 2) ~ "Current smoker",
      
      # 规则: 曾经吸烟者 - 如果当前吸烟=不吸烟（0）且过去吸烟=大多数日子（1）或偶尔（2）或尝试过一两次（3）
      current_smoking == 0 & past_smoking %in% c(1, 2, 3) ~ "Former smoker",
      
      # 规则: 从不吸烟者 - 如果当前吸烟=不吸烟（0）且过去吸烟=从不吸烟（4）
      current_smoking == 0 & past_smoking == 4 ~ "Never smoker",
      
      # 规则: 将"不知道"（-1）、"不愿意回答"（-3）和"以上都不是"（-7）归为NA
      current_smoking %in% c(-1, -3, -7) | past_smoking %in% c(-1, -3, -7) ~ NA_character_,
      
      # 对于任何不符合以上规则的组合（例如原始数据就是NA），我们将其归为NA
      TRUE ~ NA_character_
    ),
    
    # 将文本分组转换为有序因子
    smoking_status = factor(smoking_status_text, 
                            levels = c("Never smoker", "Former smoker", "Current smoker"),
                            ordered = TRUE)
  ) %>%
  # 只保留最终需要的ID和处理好的因子列
  select(Participant_ID, smoking_status)


# 步骤 B: 将清理好的吸烟数据，合并到主表中
baseline_table_step1 <- baseline_table_step1 %>%
  left_join(smoking_clean, by = "Participant_ID")
####Alcohol######
# 步骤 A: 清理并准备饮酒数据框
alcohol_clean <- Alcohol %>%
  # 选择我们需要的列，并进行重命名
  select(
    Participant_ID = `Participant ID`,
    alcohol_code = `Alcohol drinker status | Instance 2`
  ) %>%
  # 使用 mutate 进行数据转换
  mutate(
    # 步骤 1: 将特殊值 (-3) 转换成 NA
    alcohol_code = na_if(alcohol_code, -3),
    
    # 步骤 2: 将编码映射到文本分组
    alcohol_status_text = case_when(
      alcohol_code == 0 ~ "Never drinker",
      alcohol_code == 1 ~ "Former drinker",
      alcohol_code == 2 ~ "Current drinker",
      TRUE ~ NA_character_ # 处理原始数据中的 NA 值
    ),
    
    # 步骤 3: 将文本分组转换为有序因子
    alcohol_status = factor(alcohol_status_text, 
                            levels = c("Never drinker", "Former drinker", "Current drinker"),
                            ordered = TRUE)
  ) %>%
  # 只保留最终需要的ID和处理好的因子列
  select(Participant_ID, alcohol_status)


# 步骤 B: 将清理好的饮酒数据，合并到主表中 (并覆盖原表)
baseline_table_step1 <- baseline_table_step1 %>%
  left_join(alcohol_clean, by = "Participant_ID")
####sedentary#####
# 2. 核心逻辑
# ==========================================

# 步骤 A: 清理并准备久坐时间数据
sedentary_clean <- Derived_accelerometry %>%
  # 选择我们需要的列，并进行重命名
  select(
    Participant_ID = `Participant ID`,
    sedentary_prop = `Sedentary - Overall average | Instance 0`
  ) %>%
  # 使用 mutate 计算每日久坐小时数
  mutate(
    sedentary_hours = sedentary_prop * 24
  )

# 步骤 B: 将清理好的久坐时间数据，合并到主表中 (并覆盖原表)
baseline_table_step1 <- baseline_table_step1 %>%
  left_join(sedentary_clean, by = "Participant_ID")

####medical conditions#########
# 步骤 A: 清理并准备心血管病史数据框
cvd_clean <- Medical_conditions %>%
  # 选择我们需要的列，并进行重命名
  select(
    Participant_ID = `Participant ID`,
    cvd_codes_str = `Vascular/heart problems diagnosed by doctor | Instance 2`
  ) %>%
  # 使用 mutate 创建所有新变量
  mutate(
    # 步骤 1: 将特殊值 (-3) 转换成 NA
    cvd_codes_str = na_if(cvd_codes_str, "-3"),
    
    # 步骤 2: 创建四个独立的二分类变量
    # str_detect() 会检查字符串中是否包含某个模式, 非常适合处理'1|2|3'这样的数据
    # 我们用 as.integer() 将 TRUE/FALSE 的结果转换为 1/0
    history_heart_attack = as.integer(str_detect(cvd_codes_str, "1")),
    history_angina       = as.integer(str_detect(cvd_codes_str, "2")),
    history_stroke       = as.integer(str_detect(cvd_codes_str, "3")),
    history_high_bp      = as.integer(str_detect(cvd_codes_str, "4")),
    
    # 步骤 3: 创建总览变量 cvd_history_any
    # 只要上面四个变量中任何一个为1，总览变量就为1
    cvd_history_any = as.integer(
      history_heart_attack == 1 | 
        history_angina == 1 | 
        history_stroke == 1 | 
        history_high_bp == 1
    ),
    
    # 步骤 4: 将 cvd_history_any 转换为因子以便在TableOne中正确显示
    cvd_history_any = factor(cvd_history_any, 
                            levels = c(0, 1), 
                            labels = c("No", "Yes"))
  ) %>%
  # 只保留最终需要的ID和我们新创建的变量
  select(
    Participant_ID,
    cvd_history_any,
    history_heart_attack,
    history_angina,
    history_stroke,
    history_high_bp
  )

# 步骤 B: 将清理好的数据，合并到主表中 (并覆盖原表)
baseline_table_step1 <- baseline_table_step1 %>%
  left_join(cvd_clean, by = "Participant_ID")
########diabetes diagnosis#######
# 2. 核心逻辑
# ==========================================

# 步骤 A: 清理并准备糖尿病史数据框
diabetes_clean <- Medical_conditions %>%
  # 选择我们需要的列，并进行重命名
  select(
    Participant_ID = `Participant ID`,
    diabetes_code = `Diabetes diagnosed by doctor | Instance 2`
  ) %>%
  # 使用 mutate 进行数据转换
  mutate(
    # 步骤 1: 将特殊值 (-1, -3) 转换成 NA
    diabetes_code = na_if(diabetes_code, -1),
    diabetes_code = na_if(diabetes_code, -3),
    
    # 步骤 2: 将 0/1 编码转换为带有标签的因子
    history_diabetes = factor(diabetes_code, 
                              levels = c(0, 1), 
                              labels = c("No", "Yes"))
  ) %>%
  # 只保留最终需要的ID和处理好的因子列
  select(Participant_ID, history_diabetes)


# 步骤 B: 将清理好的糖尿病史数据，合并到主表中 (并覆盖原表)
baseline_table_step1 <- baseline_table_step1 %>%
  left_join(diabetes_clean, by = "Participant_ID")

####cognitive_score######
# 步骤 A: 清理并准备认知得分数据框
cognitive_score_clean_table1 <- cognitive_score_clean %>%
  # 选择我们需要的列
  select(
    Participant_ID,
    cognitive_score_0
  )

# 步骤 B: 将清理好的认知得分数据，合并到主表中 (并覆盖原表)
baseline_table_step1 <- baseline_table_step1 %>%
  left_join(cognitive_score_clean_table1, by = "Participant_ID")


# ==============================================================================
# 3. 生成 TableOne 基线特征表
# ==============================================================================
# 注意：在运行此部分前，请先运行 normality_tests.R 来验证变量分布
# 这将确保 nonnormal 参数设置的科学性和严谨性

library(tableone)
library(knitr)
library(kableExtra)

# 步骤 A: 定义我们要纳入基线表的所有变量
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

# 从上面的列表中，挑出所有分类变量的名字
cat_vars <- c(
  "sex", "ethnicity", "education_level", "bmi_group", "smoking_status",
  "alcohol_status", "cvd_history_any", "history_diabetes"
)

# 注意：以下变量基于正态性检验结果标记为非正态分布
# 所有连续变量都未通过 Kolmogorov-Smirnov 检验 (p < 0.01)
# 详见 normality_tests.R 的检验结果
nonnormal_vars <- c(
  "age_recruitment", "townsend_index", "bmi",
  "sedentary_hours", "cognitive_score_0"
)

# 步骤 B: 创建 TableOne 对象
table_one_object <- CreateTableOne(
  vars = table_vars,
  strata = "exposure_group",
  data = baseline_table_step1,
  factorVars = cat_vars
)

# 步骤 C: 打印并美化表格
print(
  table_one_object,
  smd = TRUE,
  showAllLevels = TRUE,
  quote = FALSE,
  noSpaces = TRUE,
  nonnormal = nonnormal_vars,
  contDigits = 1,
  catDigits = 1
)

# 步骤 D: 创建一个可以被处理的矩阵
table_one_matrix <- print(
  table_one_object,
  smd = TRUE,
  showAllLevels = TRUE,
  quote = FALSE,
  noSpaces = TRUE,
  printToggle = FALSE,
  nonnormal = nonnormal_vars
)

# 步骤 E: 使用 kable() 函数来美化表格
kable(
  table_one_matrix,
  caption = "Table 1: Baseline Characteristics of Study Participants"
) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  )
