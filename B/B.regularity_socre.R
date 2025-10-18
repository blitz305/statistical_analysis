# install.packages("dplyr") # 如果没有安装过dplyr
library(dplyr)
source("paths.R")

# --- 1. & 2. 数据加载、修正列名与合并 (与之前相同) ---

# 加载您的三个RDS文件
diurnal_reg <- readRDS(path_mid("diurnal_regularity.rds"))
intensity_con <- readRDS(path_mid("intensity_consistency.rds"))
temporal_reg <- readRDS(path_mid("temporal_regularity.rds"))

# 标准化ID列名并为intensity的指标添加前缀
diurnal_reg <- diurnal_reg %>% rename(participantid = "Participant.ID")
temporal_reg <- temporal_reg %>% rename(participantid = "Participant.ID")
intensity_con <- intensity_con %>% rename(intensity_sd = sd)

# 合并数据
pca_data <- diurnal_reg %>%
  select(participantid, diurnal_sd) %>%
  inner_join(intensity_con %>% select(participantid, intensity_sd), by = "participantid") %>%
  inner_join(temporal_reg %>% select(participantid, temporal_cv), by = "participantid")

# --- 3. 【核心修改】筛选完整且非零的数据 ---

# 从合并后的数据中，筛选出三个指标都没有NA且都不为0的行
pca_data_complete <- pca_data %>%
  filter(
    !is.na(diurnal_sd) & diurnal_sd != 0 &
      !is.na(intensity_sd) & intensity_sd != 0 &
      !is.na(temporal_cv) & temporal_cv != 0
  )

# --- 4. 准备数据并执行PCA (与之前相同) ---

# 从筛选后的完整数据中，提取用于PCA的指标列
pca_input <- pca_data_complete %>%
  select(diurnal_sd, intensity_sd, temporal_cv)

# 标准化数据
scaled_data <- scale(pca_input)

# 执行PCA
pca_results <- prcomp(scaled_data)

# --- 5. 将得分直接添加回数据框 (与之前相同) ---

# 将计算出的PC1得分和规律性得分，作为新列直接添加到 pca_data_complete 中
pca_data_complete$irregularity_score_PC1 <- pca_results$x[, "PC1"]

# 计算规律性得分：仅使用PC1，使得得分越高代表越规律
# PC1载荷为负值，所以直接使用PC1即可
pca_data_complete$regularity_score <- pca_data_complete$irregularity_score_PC1

# --- 6. 输出PC1贡献度信息 ---

# 计算并显示PC1的贡献度
pc1_variance_explained <- pca_results$sdev[1]^2 / sum(pca_results$sdev^2) * 100
pc1_cumulative_variance <- cumsum(pca_results$sdev^2 / sum(pca_results$sdev^2) * 100)[1]

# 显示PC1的载荷（loadings）
pc1_loadings <- pca_results$rotation[, "PC1"]

cat("=== PCA结果摘要 ===\n")
cat("PC1方差解释比例:", round(pc1_variance_explained, 2), "%\n")
cat("PC1累积方差解释比例:", round(pc1_cumulative_variance, 2), "%\n")
cat("规律性得分方向：得分越高代表越规律\n\n")

cat("=== PC1载荷 (Loadings) ===\n")
cat("diurnal_sd:", round(pc1_loadings["diurnal_sd"], 4), "\n")
cat("intensity_sd:", round(pc1_loadings["intensity_sd"], 4), "\n")
cat("temporal_cv:", round(pc1_loadings["temporal_cv"], 4), "\n\n")

cat("=== 所有主成分的方差解释比例 ===\n")
variance_explained <- pca_results$sdev^2 / sum(pca_results$sdev^2) * 100
for(i in 1:length(variance_explained)) {
  cat("PC", i, ":", round(variance_explained[i], 2), "%\n")
}

# --- 7. 生成最终结果 (与之前相同) ---

# 重新排列列的顺序，使其更清晰
regularity_score <- pca_data_complete %>%
  select(
    participantid,
    diurnal_sd,
    intensity_sd,
    temporal_cv,
    regularity_score
  )

saveRDS(regularity_score, file = path_mid("regularity_score.rds"))
write_xlsx(list(Sheet1 = regularity_score), path = path_mid("regularity_score.xlsx"))
