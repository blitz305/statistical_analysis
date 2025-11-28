  # 1. 准备工作
# ==========================================
# 加载需要的包
library(dplyr)
library(purrr)
library(tidyr)
library(survival)
library(cmprsk)
library(mice)
library(broom)
library(ggplot2)
library(parallel)
source("paths.R")

# 2. 前置检查
# ==========================================
required_objects <- c(
  "competing_risk_dataset",
  "baseline_table_step1",
  "cognitive_scores"
)

missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]
if (length(missing_objects) > 0) {
  stop(paste0("以下对象尚未加载，无法继续: ", paste(missing_objects, collapse = ", ")))
}


# 3. 数据整合
# ==========================================
print("步骤 A: 正在整合协变量和暴露信息...")

baseline_table_step1_clean <- baseline_table_step1 %>%
  select(-any_of(c("cognitive_score_0")))

cognitive_score_clean <- cognitive_scores %>%
  select(Participant_ID, cognitive_score_0)

analysis_dataset <- competing_risk_dataset %>%
  left_join(baseline_table_step1_clean, by = "Participant_ID") %>%
  left_join(cognitive_score_clean, by = "Participant_ID")

core_covariates <- c(
  "exposure_group", "age_recruitment", "sex", "townsend_index", "bmi",
  "smoking_status", "alcohol_status", "education_level",
  "cvd_history_any", "history_diabetes", "cognitive_score_0"
)

all_model_vars <- c("Participant_ID", "survival_time_days", "event_type_numeric", core_covariates)

imputation_data <- analysis_dataset %>%
  select(all_of(all_model_vars)) %>%
  # 排除非痴呆死亡（值4），只保留痴呆相关事件（0=截尾, 1=AD, 2=VaD, 3=Other dementia）
  filter(is.na(event_type_numeric) | event_type_numeric != 4)

# 将分类变量转换为因子，以便使用正确的插补方法
imputation_data$exposure_group <- factor(imputation_data$exposure_group)
imputation_data$event_type_numeric <- factor(imputation_data$event_type_numeric)

imputation_data_for_mice <- imputation_data %>%
  select(-Participant_ID)

print("步骤 A 完成。")


# 4. 多重插补
# ==========================================
print("步骤 B: 开始执行多重插补，请耐心等待...")

# 显式指定每个变量的插补方法
method_specification <- c(
  "survival_time_days" = "pmm",
  "event_type_numeric" = "polyreg",      # 多项逻辑回归用于无序分类变量（0=截尾, 1=AD, 2=VaD, 3=Other dementia，排除4=非痴呆死亡）
  "exposure_group" = "polyreg",      # 多项逻辑回归用于分类变量
  "age_recruitment" = "pmm",
  "sex" = "logreg",
  "townsend_index" = "pmm",
  "bmi" = "pmm",
  "smoking_status" = "polr",
  "alcohol_status" = "polr",
  "education_level" = "polr",
  "cvd_history_any" = "logreg",
  "history_diabetes" = "logreg",
  "cognitive_score_0" = "pmm"
)

# 检测CPU核心数用于并行插补
n_cores <- parallel::detectCores() - 1
if (n_cores < 1) n_cores <- 1

print(paste("检测到", n_cores, "个CPU核心可用于并行处理"))

imputed_object <- mice(
  imputation_data_for_mice,
  method = method_specification,
  m = 10,
  seed = 123,
  maxit = 5,
  printFlag = FALSE,
  n.core = n_cores,
  ridge = 1e-05,
  threshold = 1.0
)
print("步骤 B 完成，多重插补已生成！")


# 5. 辅助函数
# ==========================================
pool_crr_models <- function(fit_list, model_label) {
  if (length(fit_list) == 0) {
    return(tibble())
  }
  coef_names <- names(fit_list[[1]]$coef)
  map_dfr(seq_along(coef_names), function(j) {
    qhat <- vapply(fit_list, function(f) f$coef[j], numeric(1))
    uvals <- vapply(fit_list, function(f) f$var[j, j], numeric(1))
    pooled <- mice::pool.scalar(Q = qhat, U = uvals)
    se <- sqrt(pooled$t)
    df <- pooled$df
    crit <- qt(0.975, df)
    estimate_log <- pooled$qbar
    tibble(
      Model = model_label,
      term = coef_names[j],
      estimate = exp(estimate_log),
      conf.low = exp(estimate_log - crit * se),
      conf.high = exp(estimate_log + crit * se),
      p.value = 2 * pt(-abs(estimate_log / se), df)
    )
  })
}

format_results_table <- function(df, effect_label) {
  df %>%
    mutate(
      Variable = term,
      `Effect (95% CI)` = paste0(
        round(estimate, 3),
        " (",
        round(conf.low, 3),
        " to ",
        round(conf.high, 3),
        ")"
      ),
      `P Value` = if_else(p.value < 0.001, "<0.001", format(round(p.value, 3), nsmall = 3)),
      Model = paste0(Model, " (", effect_label, ")")
    ) %>%
    select(Model, Variable, `Effect (95% CI)`, `P Value`)
}


# 6. 在每个插补数据集上拟合模型
# ==========================================
print("步骤 C: 正在每个插补数据集上拟合竞争风险与原因特异模型...")

design_formula <- as.formula(paste("~", paste(core_covariates, collapse = " + ")))

model_indices <- seq_len(imputed_object$m)

if (length(model_indices) == 0) {
  stop("多重插补对象未包含任何数据集，无法拟合模型。")
}

fit_models_for_imputation <- function(index, imputed_object, design_formula) {
  completed_data <- mice::complete(imputed_object, index)
  completed_data$exposure_group <- factor(completed_data$exposure_group)
  completed_data$exposure_group <- stats::relevel(completed_data$exposure_group, ref = "Inactive")
  
  # 将 event_type_numeric 转换回数值型（crr函数需要数值型）
  completed_data$event_type_numeric <- as.numeric(as.character(completed_data$event_type_numeric))
  
  # 排除非痴呆死亡（值4），只分析痴呆相关事件（0=截尾, 1=AD, 2=VaD, 3=Other dementia）
  completed_data <- completed_data %>%
    filter(is.na(event_type_numeric) | event_type_numeric != 4)

  covariate_matrix <- stats::model.matrix(design_formula, data = completed_data)[, -1, drop = FALSE]

  list(
    fg_ad = cmprsk::crr(
      ftime = completed_data$survival_time_days,
      fstatus = completed_data$event_type_numeric,
      cov1 = covariate_matrix,
      failcode = 1,
      cencode = 0
    ),
    fg_vad = cmprsk::crr(
      ftime = completed_data$survival_time_days,
      fstatus = completed_data$event_type_numeric,
      cov1 = covariate_matrix,
      failcode = 2,
      cencode = 0
    ),
    fg_other = cmprsk::crr(
      ftime = completed_data$survival_time_days,
      fstatus = completed_data$event_type_numeric,
      cov1 = covariate_matrix,
      failcode = 3,
      cencode = 0
    ),
    cs_ad = survival::coxph(
      survival::Surv(survival_time_days, event_type_numeric == 1) ~ exposure_group + age_recruitment + sex + townsend_index +
        bmi + smoking_status + alcohol_status + education_level + cvd_history_any + history_diabetes + cognitive_score_0,
      data = completed_data
    ),
    cs_vad = survival::coxph(
      survival::Surv(survival_time_days, event_type_numeric == 2) ~ exposure_group + age_recruitment + sex + townsend_index +
        bmi + smoking_status + alcohol_status + education_level + cvd_history_any + history_diabetes + cognitive_score_0,
      data = completed_data
    ),
    cs_other = survival::coxph(
      survival::Surv(survival_time_days, event_type_numeric == 3) ~ exposure_group + age_recruitment + sex + townsend_index +
        bmi + smoking_status + alcohol_status + education_level + cvd_history_any + history_diabetes + cognitive_score_0,
      data = completed_data
    ),
    data = completed_data
  )
}

available_cores <- parallel::detectCores()
if (is.null(available_cores)) available_cores <- 1L
cluster_size <- min(max(available_cores - 1L, 1L), length(model_indices))
use_parallel <- length(model_indices) > 1L && cluster_size > 1L

if (use_parallel) {
  print(paste0("步骤 C: 已启用并行，使用 ", cluster_size, " 个核心处理 ", length(model_indices), " 个插补数据集。"))
  cluster <- parallel::makeCluster(cluster_size)
  parallel::clusterExport(
    cluster,
    varlist = c("imputed_object", "design_formula", "fit_models_for_imputation"),
    envir = environment()
  )
  parallel::clusterEvalQ(cluster, {
    library(cmprsk)
    library(survival)
    library(mice)
    library(dplyr)  # 添加dplyr，因为fit_models_for_imputation使用了 %>% 和 filter()
    NULL
  })
  model_results <- parallel::parLapply(
    cl = cluster,
    X = model_indices,
    fun = fit_models_for_imputation,
    imputed_object = imputed_object,
    design_formula = design_formula
  )
  parallel::stopCluster(cluster)
} else {
  print("步骤 C: 核心数有限，采用串行方式处理插补数据集。")
  model_results <- lapply(
    model_indices,
    fit_models_for_imputation,
    imputed_object = imputed_object,
    design_formula = design_formula
  )
}

fg_ad_models <- lapply(model_results, `[[`, "fg_ad")
fg_vad_models <- lapply(model_results, `[[`, "fg_vad")
fg_other_models <- lapply(model_results, `[[`, "fg_other")
cs_ad_models <- lapply(model_results, `[[`, "cs_ad")
cs_vad_models <- lapply(model_results, `[[`, "cs_vad")
cs_other_models <- lapply(model_results, `[[`, "cs_other")
completed_datasets <- lapply(model_results, `[[`, "data")

print("步骤 C 完成！")


# 7. 汇总 (Pool) 结果
# ==========================================
print("步骤 D: 正在汇总模型结果...")

finegray_results <- bind_rows(
  pool_crr_models(fg_ad_models, "Fine-Gray SHR - AD"),
  pool_crr_models(fg_vad_models, "Fine-Gray SHR - VaD"),
  pool_crr_models(fg_other_models, "Fine-Gray SHR - Other dementia")
)

cause_specific_results <- bind_rows(
  tidy(pool(cs_ad_models), conf.int = TRUE, exponentiate = TRUE) %>% mutate(Model = "Cause-specific HR - AD"),
  tidy(pool(cs_vad_models), conf.int = TRUE, exponentiate = TRUE) %>% mutate(Model = "Cause-specific HR - VaD"),
  tidy(pool(cs_other_models), conf.int = TRUE, exponentiate = TRUE) %>% mutate(Model = "Cause-specific HR - Other dementia")
) %>%
  filter(term != "(Intercept)")

finegray_table <- format_results_table(finegray_results, "SHR")
cause_specific_table <- cause_specific_results %>%
  mutate(
    Variable = term,
    `Effect (95% CI)` = paste0(
      round(estimate, 3),
      " (",
      round(conf.low, 3),
      " to ",
      round(conf.high, 3),
      ")"
    ),
    `P Value` = if_else(p.value < 0.001, "<0.001", format(round(p.value, 3), nsmall = 3)),
    Model = paste0(Model, " (HR)")
  ) %>%
  select(Model, Variable, `Effect (95% CI)`, `P Value`)

write.csv(finegray_table, file = path_result("competing_risk_finegray_exposure_group.csv"), row.names = FALSE)
write.csv(cause_specific_table, file = path_result("competing_risk_causespecific_exposure_group.csv"), row.names = FALSE)

print("步骤 D 完成，结果已写入 result/ 目录。")


# 8. 计算并绘制 CIF 曲线
# ==========================================
first_completed_dataset <- if (length(completed_datasets) >= 1) completed_datasets[[1]] else NULL
plot_data <- first_completed_dataset  # 直接使用全部数据，不抽样

if (!is.null(plot_data)) {
  print(paste0(
    "步骤 E: 正在使用全部数据 (样本量 ",
    nrow(plot_data),
    ") 计算并绘制不同运动模式的CIF曲线..."
  ))
} else {
  print("步骤 E: 未获取到用于绘图的样本，跳过 CIF 曲线绘制。")
}

if (!is.null(plot_data)) {
  # 确保 event_type_numeric 是数值型
  plot_data$event_type_numeric <- as.numeric(as.character(plot_data$event_type_numeric))
  
  # 排除非痴呆死亡（值4），只分析痴呆相关事件（0=截尾, 1=AD, 2=VaD, 3=Other dementia）
  plot_data_filtered <- plot_data %>%
    filter(is.na(event_type_numeric) | event_type_numeric != 4)
  
  # 明确指定 cencode = 0（截尾）
  cuminc_obj <- cuminc(
    ftime = plot_data_filtered$survival_time_days,
    fstatus = plot_data_filtered$event_type_numeric,
    group = plot_data_filtered$exposure_group,
    cencode = 0  # 明确指定截尾编码
  )

  cause_labels <- c(
    "1" = "Alzheimer's disease",
    "2" = "Vascular dementia",
    "3" = "Other dementia"
  )

  cuminc_df <- imap_dfr(cuminc_obj, function(curve, name) {
    if (!grepl(" ", name)) return(NULL)
    parts <- strsplit(name, " ")[[1]]
    cause_code <- tail(parts, 1)
    group_name <- paste(head(parts, -1), collapse = " ")
    if (!cause_code %in% names(cause_labels)) return(NULL)
    tibble(
      time = curve$time,
      estimate = curve$est,
      group = group_name,
      cause = cause_labels[[cause_code]]
    )
  })

  if (nrow(cuminc_df) > 0) {
    cif_plot <- ggplot(cuminc_df, aes(x = time, y = estimate, colour = group)) +
      geom_step(size = 0.9) +
      facet_wrap(~cause, scales = "free_y") +
      labs(
        x = "Follow-up time (days)",
        y = "Cumulative incidence",
        colour = "Exposure group",
        title = "Cumulative incidence of dementia subtypes by activity group"
      ) +
      theme_minimal()

    ggsave(filename = path_result("cif_exposure_group.png"), plot = cif_plot, width = 9, height = 5, dpi = 300)
    print("CIF 曲线已保存至 result/cif_exposure_group.png")
  } else {
    warning("无法生成 CIF 曲线：未能从 cuminc 对象提取所需信息。")
  }
} else {
  warning("无法生成 CIF 曲线：未找到用于绘图的样本数据。")
}

print("步骤 E 完成。")


# 9. 生成 Kaplan-Meier 生存曲线
# ==========================================
if (!is.null(plot_data)) {
  print(paste0(
    "步骤 F: 正在使用全部数据 (样本量 ",
    nrow(plot_data),
    ") 生成 Kaplan-Meier 生存曲线..."
  ))
} else {
  print("步骤 F: 未获取到用于绘图的样本，跳过 Kaplan-Meier 曲线绘制。")
}

create_km_plot <- function(data, event_code, filename, title) {
  surv_obj <- survfit(Surv(survival_time_days, event_type_numeric == event_code) ~ exposure_group, data = data)
  surv_df <- broom::tidy(surv_obj) %>%
    mutate(exposure_group = if ("strata" %in% names(.)) sub("^exposure_group=", "", strata) else unique(data$exposure_group)[1])

  plot <- ggplot(surv_df, aes(x = time, y = estimate, colour = exposure_group)) +
    geom_step(size = 0.9) +
    labs(
      x = "Follow-up time (days)",
      y = "Survival probability",
      colour = "Exposure group",
      title = title
    ) +
    theme_minimal()

  output_path <- path_result(filename)
  ggsave(filename = output_path, plot = plot, width = 9, height = 5, dpi = 300)
  print(paste0("Kaplan-Meier 曲线已保存至 ", output_path))
}

if (!is.null(plot_data)) {
  create_km_plot(
    data = plot_data,
    event_code = 1,
    filename = "km_exposure_group_ad.png",
    title = "Kaplan-Meier: Alzheimer's disease"
  )

  create_km_plot(
    data = plot_data,
    event_code = 2,
    filename = "km_exposure_group_vad.png",
    title = "Kaplan-Meier: Vascular dementia"
  )

  create_km_plot(
    data = plot_data,
    event_code = 3,
    filename = "km_exposure_group_other.png",
    title = "Kaplan-Meier: Other dementia"
  )
} else {
  warning("无法生成 Kaplan-Meier 曲线：未找到完成的插补数据集。")
}

print("竞争风险分析脚本已完成。")

# 10. 美化输出结果表格
# ==========================================
print("步骤 F: 正在美化输出结果表格...")
source("beautify_results.R")

# 美化Fine-Gray和Cause-specific结果
beautify_model_results(
  path_result("competing_risk_finegray_exposure_group.csv"),
  path_result("competing_risk_finegray_exposure_group_beautified.png")
)
beautify_model_results(
  path_result("competing_risk_causespecific_exposure_group.csv"),
  path_result("competing_risk_causespecific_exposure_group_beautified.png")
)

print("步骤 F 完成，美化表格已生成。")


