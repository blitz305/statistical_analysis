# --- 第1步：确保您的数据框已加载 ---
source("paths.R")
# 假设您的数据框名为 Acceleration_averages

# --- 第2步：定义四个时间段各自包含的列名 ---

# 早晨 (06:00 - 09:59)
morning_cols <- c("Average acceleration 06:00 - 06:59", "Average acceleration 07:00 - 07:59", 
                  "Average acceleration 08:00 - 08:59", "Average acceleration 09:00 - 09:59")

# 中午 (10:00 - 13:59)
noon_cols <- c("Average acceleration 10:00 - 10:59", "Average acceleration 11:00 - 11:59", 
               "Average acceleration 12:00 - 12:59", "Average acceleration 13:00 - 13:59")

# 下午 (14:00 - 17:59)
afternoon_cols <- c("Average acceleration 14:00 - 14:59", "Average acceleration 15:00 - 15:59", 
                    "Average acceleration 16:00 - 16:59", "Average acceleration 17:00 - 17:59")

# 傍晚 (18:00 - 21:59)
evening_cols <- c("Average acceleration 18:00 - 18:59", "Average acceleration 19:00 - 19:59", 
                  "Average acceleration 20:00 - 20:59", "Average acceleration 21:00 - 21:59")


# --- 第3步：计算每个时段的平均活动量 ---
# 使用高效的 rowMeans() 函数分别计算

mean_morning_activity <- rowMeans(Acceleration_averages[, morning_cols], na.rm = TRUE)
mean_noon_activity <- rowMeans(Acceleration_averages[, noon_cols], na.rm = TRUE)
mean_afternoon_activity <- rowMeans(Acceleration_averages[, afternoon_cols], na.rm = TRUE)
mean_evening_activity <- rowMeans(Acceleration_averages[, evening_cols], na.rm = TRUE)


# --- 第4步：计算这四个时段平均值之间的标准差 ---

# 首先，将这四个计算出的结果合并成一个新的矩阵
slot_means_matrix <- cbind(mean_morning_activity, mean_noon_activity, 
                           mean_afternoon_activity, mean_evening_activity)

# 然后，对这个新矩阵的每一行，计算标准差
diurnal_sd_values <- apply(slot_means_matrix, 1, sd, na.rm = TRUE)


# --- 第5步：将结果与ID合并成最终的数据框 ---
diurnal_regularity <- data.frame(
  `Participant ID` = Acceleration_averages$`Participant ID`,
  diurnal_sd = diurnal_sd_values
)

saveRDS(diurnal_regularity, file = path_mid("diurnal_regularity.rds"))
write_xlsx(list(Sheet1 = diurnal_regularity), path = path_mid("diurnal_regularity.xlsx"))
