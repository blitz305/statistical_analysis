library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(data.table)

# (2) 打印开始信息
# ------------------------------------
cat("======================================================\n")
cat("C part star...\n")
cat("======================================================\n\n")


cat("--> 正在执行模块 1: 基于住院记录的痴呆分类...\n")
source("dementia_classify.R")

cat("--> 正在执行模块 2: 基于死亡登记的痴呆分类...\n")
source("death_register.R")

cat("--> 正在执行模块 3: 合并诊断与死亡记录...\n")
source("death_and_dementia_classify.R")

cat("--> 正在执行模块 4: 定义终点事件变量...\n")
source("outcome_variables.R")

cat("--> 正在执行模块 5: 计算生存时间...\n")
source("survival_time.R")


cat("\n======================================================\n")
cat("最终成果已保存至数据框: final_dataset\n")
cat("======================================================\n")

