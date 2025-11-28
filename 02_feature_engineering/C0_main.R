# ==============================================================================
# C0_main.R - 生存结局数据准备主入口
# ==============================================================================
#
# 【功能说明】
# 本脚本是 C 模块的主入口，用于构建痴呆发病与死亡的生存分析数据集。
# 运行后将依次执行以下子模块，最终生成可用于Cox回归的生存数据。
#
# 【执行流程】
# C1 -> C2 -> C3 -> C4 -> C5
#
# 【子模块说明】
# C1_dementia_icd10.R    : 从住院记录中提取ICD10痴呆诊断 (G30/F00/F01等)
# C2_death_dementia.R    : 从死亡登记中提取痴呆相关死因
# C3_dementia_merge.R    : 合并住院诊断与死亡记录，处理诊断冲突
# C4_endpoint_status.R   : 定义终点事件状态 (0=删失, 1=痴呆, 2=死亡)
# C5_survival_time.R     : 计算从入组到终点的生存时间 (天)
#
# 【输出数据】
# - final_dataset (数据框): 包含以下关键变量
#   - Participant_ID    : 参与者ID
#   - recruitment_date  : 入组日期
#   - Endpoint_Date     : 终点日期
#   - Endpoint_Status   : 终点状态 (0/1/2)
#   - survival_time_days: 生存时间 (天)
#   - first_dementia_date: 首次痴呆诊断日期
#   - matched_dementia_codes: 匹配的痴呆ICD10编码
#
# 【不包含在 main 中的脚本】
# C3b_dementia_conflict.R : 诊断冲突处理的备用/调试脚本 (功能已整合至C3)
# C6_competing_risk.R     : 竞争风险数据准备 (将痴呆细分为AD/VaD/其他亚型)
#                           用于Fine-Gray竞争风险回归，需单独运行
#
# ==============================================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(data.table)

cat("======================================================\n")
cat("C part start...\n")
cat("======================================================\n\n")

cat("--> 正在执行模块 1: 基于住院记录的痴呆分类...\n")
source("C1_dementia_icd10.R")

cat("--> 正在执行模块 2: 基于死亡登记的痴呆分类...\n")
source("C2_death_dementia.R")

cat("--> 正在执行模块 3: 合并诊断与死亡记录...\n")
source("C3_dementia_merge.R")

cat("--> 正在执行模块 4: 定义终点事件变量...\n")
source("C4_endpoint_status.R")

cat("--> 正在执行模块 5: 计算生存时间...\n")
source("C5_survival_time.R")

cat("\n======================================================\n")
cat("最终成果已保存至数据框: final_dataset\n")
cat("======================================================\n")
