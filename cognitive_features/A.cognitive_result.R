library(dplyr)
library(FactoMineR)
library(writexl)
library(purrr) 
source("paths.R")



final_merged_baseline <- list(
  # 1. Fluid intelligence
  Fluid_intelligence %>%
    filter(!is.na(.[[4]])) %>%
    select(1,4) %>%
    rename(Participant_ID = 1, Fluid_intelligence_0 = 2),
  
  # 2. Prospective memory
  prospective_memory %>%
    filter(!is.na(.[[4]])) %>%
    select(1,4) %>%
    rename(Participant_ID = 1, Prospective_memory_0 = 2) %>%
    mutate(Prospective_memory_0 = if_else(Prospective_memory_0 == 2, 1, Prospective_memory_0)),
  
  # 3. Reaction time
  Reaction_time %>%
    filter(!is.na(.[[4]])) %>%
    select(1,4) %>%
    rename(Participant_ID = 1, Reaction_time_0 = 2),
  
  # 4. Pairs matching
  Pairs_matching %>%
    filter(!is.na(.[[8]]) | !is.na(.[[9]]) | !is.na(.[[10]])) %>%
    select(1,8,9,10) %>%
    rename(Participant_ID = 1, round1_errors = 2, round2_errors = 3, round3_errors = 4) %>%
    mutate(incorrect_match_0 = round1_errors + round2_errors + round3_errors) %>%
    select(Participant_ID, incorrect_match_0)
) %>% 
  reduce(full_join, by = "Participant_ID")



final_merged_followup <- list(
  # 1. Fluid intelligence
  Fluid_intelligence %>%
    filter(!is.na(.[[5]])) %>%
    select(1, 5) %>%
    rename(Participant_ID = 1, Fluid_intelligence_2 = 2),
  
  # 2. Prospective memory
  prospective_memory %>%
    filter(!is.na(.[[5]])) %>%
    select(1, 5) %>%
    rename(Participant_ID = 1, Prospective_memory_2 = 2) %>%
    mutate(Prospective_memory_2 = if_else(Prospective_memory_2 == 2, 1, Prospective_memory_2)),
  
  # 3. Reaction time
  Reaction_time %>%
    filter(!is.na(.[[5]])) %>%
    select(1, 5) %>%
    rename(Participant_ID = 1, Reaction_time_2 = 2),
  
  # 4. Pairs matching
  Pairs_matching %>%
    filter(!is.na(.[[11]]) | !is.na(.[[12]]) | !is.na(.[[13]])) %>%
    select(1, 11, 12,13) %>%
    rename(Participant_ID = 1, round1_errors = 2, round2_errors = 3 , round3_errors = 4) %>%
    mutate(incorrect_match_2 = round1_errors + round2_errors + round3_errors) %>%
    select(Participant_ID, incorrect_match_2)
) %>%
  reduce(full_join, by = "Participant_ID")


# --- 第5步：执行前瞻性FAMD分析 ---

famd_baseline_input <- final_merged_baseline %>%
  na.omit() %>%
  mutate(Prospective_memory_0 = as.factor(Prospective_memory_0))

famd_model <- FAMD(famd_baseline_input[, -1], graph = FALSE)

baseline_scores <- data.frame(Participant_ID = famd_baseline_input$Participant_ID,
                              cognitive_score_0 = famd_model$ind$coord[, 1])

# 5b. 准备随访数据并使用基线模型进行预测
followup_scores <- final_merged_followup %>%
  na.omit() %>%
  mutate(Prospective_memory_2 = as.factor(Prospective_memory_2)) %>%
  # 使用 R 的占位符 `.` 来在管道中继续操作
  {
    followup_for_pred <- .[, -1]
    colnames(followup_for_pred) <- colnames(famd_baseline_input[, -1])
    
    followup_pred <- predict.FAMD(famd_model, newdata = followup_for_pred)
    
    data.frame(Participant_ID = .$Participant_ID,
               cognitive_score_2 = followup_pred$coord[, 1])
  }



final_output_data <- full_join(final_merged_baseline, final_merged_followup, by = "Participant_ID") %>%
  left_join(
    full_join(baseline_scores, followup_scores, by = "Participant_ID") %>%
      mutate(
        cognitive_score_0 = cognitive_score_0 * -1,
        cognitive_score_2 = cognitive_score_2 * -1,
        cognitive_change = cognitive_score_2 - cognitive_score_0
      ),
    by = "Participant_ID"
  )


saveRDS(final_output_data, file = path_mid("cognitive_scores.rds"))
write_xlsx(list(Sheet1 = final_output_data), path = path_mid("cognitive_scores.xlsx"))








cat("--- FAMD模型结果解读 ---\n\n")

# 显示所有维度的方差解释百分比
cat("各维度解释的方差百分比:\n")
for(i in 1:nrow(famd_model$eig)) {
  cat(paste0("Dim.", i, ": ", round(famd_model$eig[i, 2], 2), "%\n"))
}

cat("\n累积方差解释百分比:\n")
cumulative_var <- cumsum(famd_model$eig[, 2])
for(i in 1:length(cumulative_var)) {
  cat(paste0("前", i, "个维度: ", round(cumulative_var[i], 2), "%\n"))
}

# 显示前3个维度的变量关系
for(dim in 1:min(3, ncol(famd_model$quanti.var$coord))) {
  cat(paste0("\n=== 第", dim, "维度 (解释", round(famd_model$eig[dim, 2], 2), "%方差) ===\n"))
  
  cat("\n连续型变量与第", dim, "维度的关系 (权重):\n")
  print(round(famd_model$quanti.var$coord[, dim, drop = FALSE], 4))
  
  cat("\n分类型变量与第", dim, "维度的关系 (权重):\n")
  print(round(famd_model$quali.var$coord[, dim, drop = FALSE], 4))
}

cat("\n--- 模型解读结束 ---\n\n")
# ---------------------------------------------