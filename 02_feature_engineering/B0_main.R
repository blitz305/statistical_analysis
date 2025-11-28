# ==============================================================================
# B0_main.R - 活动规律性指标计算主入口
# ==============================================================================
#
# 【功能说明】
# 本脚本是 B 模块的主入口，用于计算参与者的身体活动规律性指标。
# 运行后将依次执行以下子模块，最终生成综合规律性得分。
#
# 【执行流程】
# B1 -> B2 -> B3 -> B4
#
# 【子模块说明】
# B1_diurnal_variability.R   : 计算日内时段活动变异性 (早/中/下午/傍晚四时段的SD)
# B2_intensity_variability.R : 计算日间MVPA强度变异性 (7天MVPA的SD和CV)
# B3_weekday_variability.R   : 计算周内各天活动变异性 (周一至周日加速度的SD和CV)
# B4_regularity_score_pca.R  : 使用PCA合成综合规律性得分 (基于上述三个指标)
#
# 【输出数据】
# - mid_result/diurnal_regularity.rds   : 日内变异性数据
# - mid_result/intensity_consistency.rds: 强度变异性数据
# - mid_result/temporal_regularity.rds  : 周内变异性数据
# - mid_result/regularity_score.rds     : 最终规律性得分 (用于下游分析)
#
# 【不包含在 main 中的脚本】
# B5_mvpa_weekly_pattern.R : 周MVPA模式分类 (活跃/集中活跃/不活跃)
# B6_mvpa_time_pattern.R   : 日内MVPA时段模式分类 (早晨型/下午型/傍晚型/混合型)
# B7_exposure_cluster.R    : K-prototypes聚类分析 (整合多个暴露变量进行聚类)
# 以上脚本为独立分析模块，需根据研究需要单独运行。
#
# ==============================================================================

library(writexl)
library(dplyr)

cat("===========================\n")
cat("B part start \n")

source("B1_diurnal_variability.R")
source("B2_intensity_variability.R")
source("B3_weekday_variability.R")
source("B4_regularity_score_pca.R")

cat("finish \n")
cat("===========================\n")
