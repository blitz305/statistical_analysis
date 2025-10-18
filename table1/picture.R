# 1. 准备工作

library(tableone)
library(dplyr)



# 2. 核心逻辑 


# 步骤 A: 定义我们要纳入基线表的所有变量
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

# 从上面的列表中，挑出所有分类变量的名字
catVars <- c(
  "sex", "ethnicity", "education_level", "bmi_group", "smoking_status", 
  "alcohol_status", "cvd_history_any", "history_diabetes"
)

# 步骤 B: 创建TableOne对象

tableOneObject <- CreateTableOne(
  vars = myVars, 
  strata = "exposure_group", 
  data = baseline_table_step1, 
  factorVars = catVars
)

# 步骤 C: 打印并美化表格
print(
  tableOneObject, 
  smd = TRUE,               # 显示标准化均数差 (SMD)
  showAllLevels = TRUE,     # 显示所有分类的水平
  quote = FALSE,           
  noSpaces = TRUE,          # 压缩空格

  nonnormal = c("townsend_index", "sedentary_hours"),
  # 控制小数位数
  contDigits = 1,
  catDigits = 1
)


# 步骤 D:创建一个可以被处理的矩阵
tableOneMatrix <- print(
  tableOneObject, 
  smd = TRUE,
  showAllLevels = TRUE,
  quote = FALSE,
  noSpaces = TRUE,
  printToggle = FALSE
)

# 步骤 E: 使用 kable() 函数来美化表格

library(knitr)
library(kableExtra)

kable(tableOneMatrix, caption = "Table 1: Baseline Characteristics of Study Participants") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

