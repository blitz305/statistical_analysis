# 1. 同样，先定义好我们要计算的7个列的名称
source("paths.R")
daily_cols <- c("Monday average acceleration", "Tuesday average acceleration", 
                "Wednesday average acceleration", "Thursday average acceleration", 
                "Friday average acceleration", "Saturday average acceleration", 
                "Sunday average acceleration")

# 2. 从原始数据框中只选取这7列
daily_data_subset <- Acceleration_averages[, daily_cols]

# 3. 使用 apply() 函数对每一行 (MARGIN = 1) 进行计算
#    我们先计算SD
temporal_sd <- apply(daily_data_subset, 1, sd, na.rm = TRUE)
#    再计算CV
temporal_cv <- apply(daily_data_subset, 1, function(row) sd(row, na.rm = TRUE) / mean(row, na.rm = TRUE))

# 4. 将计算结果与Participant ID合并成一个新的数据框
temporal_regularity_base_r <- data.frame(
  `Participant ID` = Acceleration_averages$`Participant ID`,
  temporal_sd = temporal_sd,
  temporal_cv = temporal_cv
)

saveRDS(temporal_regularity_base_r, file = path_mid("temporal_regularity.rds"))
write_xlsx(list(Sheet1 = temporal_regularity_base_r), path = path_mid("temporal_regularity.xlsx"))
