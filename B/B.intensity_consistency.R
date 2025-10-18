library(writexl)
source("paths.R")

# 1. 定义计算函数（这部分与之前相同，保持不变）
calculate_intensity_stats <- function(mvpa_string) {
  # 将字符串按逗号分割，并转换为数值型向量
  daily_values <- as.numeric(unlist(strsplit(mvpa_string, ",")))
  
  # 如果数据无效，则返回NA
  if (all(is.na(daily_values))) {
    return(c(intensity_sd = NA, intensity_cv = NA))
  }
  
  # 计算均值和标准差
  mean_val <- mean(daily_values, na.rm = TRUE)
  sd_val <- sd(daily_values, na.rm = TRUE)
  
  # 计算变异系数 (CV)，并处理均值为0的情况
  if (is.na(mean_val) || mean_val == 0) {
    cv_val <- 0
  } else {
    cv_val <- sd_val / mean_val
  }
  
  # 返回一个命名的向量
  return(c(intensity_sd = sd_val, intensity_cv = cv_val))
}


# --- 生成您指定的最终数据框 ---

# 2. 从您的数据框中提取目标列，并应用函数
col_to_process <- Derived_accelerometry$"Moderate-Vigorous - Day average | Instance 0"
stats_results <- sapply(col_to_process, calculate_intensity_stats)

# 3. 将计算结果整理为临时数据框
stats_df <- as.data.frame(t(stats_results))

# 4. 创建您最终想要的、只包含三列的新数据框
#    并直接按您的要求重命名列名
intensity_consistency <- data.frame(
  participantid = Derived_accelerometry$"Participant ID",
  sd = stats_df$intensity_sd,
  cv = stats_df$intensity_cv  # 这里我使用了cv，如果确实是sv请修改
)
saveRDS(intensity_consistency, file = path_mid("intensity_consistency.rds"))
write_xlsx(list(Sheet1 = intensity_consistency), path = path_mid("intensity_consistency.xlsx"))
