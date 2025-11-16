# 美化模型结果表格函数
# ==========================================
# 此函数用于美化CSV格式的模型结果文件，生成三线表格式的图片表格
# 只保留暴露变量，不保留协变量

library(dplyr)
library(readr)
library(gridExtra)
library(grid)
library(gtable)
library(ggplot2)
source("paths.R")

# 定义协变量列表（需要排除的变量）
covariates_list <- c(
  "(Intercept)",
  "age_recruitment",
  "sex", "sexMale", "sexFemale",
  "townsend_index",
  "bmi",
  "smoking_status", "smoking_status.L", "smoking_status.Q", "smoking_status.C",
  "alcohol_status", "alcohol_status.L", "alcohol_status.Q", "alcohol_status.C",
  "education_level", "education_level.L", "education_level.Q", "education_level.C",
  "cvd_history_any", "cvd_history_anyYes", "cvd_history_anyNo",
  "history_diabetes", "history_diabetesYes", "history_diabetesNo",
  "cognitive_score_0"
)

# 主函数：美化单个CSV文件
beautify_model_results <- function(csv_file_path, output_file_path = NULL) {
  # 读取CSV文件
  if (!file.exists(csv_file_path)) {
    warning(paste("文件不存在:", csv_file_path))
    return(NULL)
  }
  
  df <- read_csv(csv_file_path, show_col_types = FALSE)
  
  # 检查必要的列是否存在
  required_cols <- c("Model", "Variable")
  if (!all(required_cols %in% names(df))) {
    warning(paste("CSV文件缺少必要的列:", csv_file_path))
    return(NULL)
  }
  
  # 识别效应列（可能是"HR (95% CI)", "Beta (95% CI)", "Effect (95% CI)"等）
  effect_col <- names(df)[grepl("CI|Effect|HR|Beta|SHR", names(df), ignore.case = TRUE)][1]
  pvalue_col <- names(df)[grepl("P|p.value|p_value", names(df), ignore.case = TRUE)][1]
  
  if (is.na(effect_col) || is.na(pvalue_col)) {
    warning(paste("无法识别效应列或P值列:", csv_file_path))
    return(NULL)
  }
  
  # 检测是否是竞争风险格式（Model列包含Fine-Gray或Cause-specific）
  is_competing_risk <- any(grepl("Fine-Gray|Cause-specific", df$Model, ignore.case = TRUE))
  
  if (is_competing_risk) {
    # 竞争风险格式：不过滤Model，保留所有结果类型
    df_filtered <- df
    
    # 只保留暴露变量（exposure_group开头的变量）
    df_exposure <- df_filtered %>%
      filter(grepl("^exposure_group", Variable, ignore.case = TRUE))
    
    if (nrow(df_exposure) == 0) {
      warning(paste("未找到暴露变量:", csv_file_path))
      return(NULL)
    }
    
    # 创建竞争风险格式的表格（按结果类型分组）
    beautified_table <- create_competing_risk_table(df_exposure, effect_col, pvalue_col)
  } else {
    # 标准格式：只保留Model 0, 1, 2
    df_filtered <- df %>%
      filter(
        grepl("Model 0|Model 1|Model 2", Model, ignore.case = TRUE)
      )
    
    # 过滤掉协变量，只保留暴露变量
    # 暴露变量是不在协变量列表中的变量
    df_exposure <- df_filtered %>%
      filter(!Variable %in% covariates_list) %>%
      # 进一步过滤：排除以协变量开头的变量（如smoking_status.L等）
      filter(!grepl("^(age_recruitment|sex|townsend_index|bmi|smoking_status|alcohol_status|education_level|cvd_history|history_diabetes|cognitive_score)", Variable))
    
    if (nrow(df_exposure) == 0) {
      warning(paste("未找到暴露变量:", csv_file_path))
      return(NULL)
    }
    
    # 标准化Model名称
    df_exposure <- df_exposure %>%
      mutate(
        Model = case_when(
          grepl("Model 0|Unadjusted", Model, ignore.case = TRUE) ~ "Model 0",
          grepl("Model 1|Demographics", Model, ignore.case = TRUE) ~ "Model 1",
          grepl("Model 2|Fully Adjusted|Fully", Model, ignore.case = TRUE) ~ "Model 2",
          TRUE ~ Model
        )
      )
    
    # 创建美化后的表格（横排格式：每个暴露变量一行，Model 0/1/2作为列）
    beautified_table <- create_wide_table(df_exposure, effect_col, pvalue_col)
  }
  
  # 生成图片表格
  table_image <- generate_table_image(beautified_table, effect_col)
  
  # 保存结果
  if (is.null(output_file_path)) {
    base_name <- tools::file_path_sans_ext(basename(csv_file_path))
    output_file_path <- path_result(paste0(base_name, "_beautified.png"))
  } else {
    # 确保输出文件是PNG格式
    if (!grepl("\\.png$", output_file_path, ignore.case = TRUE)) {
      output_file_path <- gsub("\\.[^.]+$", ".png", output_file_path)
    }
  }
  
  # 保存图片 - 根据表格类型调整尺寸
  # 检测是否是竞争风险格式（列数较多）
  is_wide_table <- ncol(beautified_table) > 7
  
  if (is_wide_table) {
    # 竞争风险格式：更宽的表格
    width <- 18
    height <- max(8, 3 + nrow(beautified_table) * 0.7)
  } else {
    # 标准格式：正常宽度
    width <- 16
    height <- max(7, 2.5 + nrow(beautified_table) * 0.6)
  }
  
  ggsave(
    filename = output_file_path,
    plot = table_image,
    width = width,
    height = height,
    units = "in",
    dpi = 400,
    bg = "white"
  )
  
  # 同时保存为CSV格式（美化后的）
  csv_output_path <- gsub("\\.png$", "_beautified.csv", output_file_path, ignore.case = TRUE)
  write_csv(beautified_table, csv_output_path)
  
  cat(paste("美化表格图片已保存至:", output_file_path, "\n"))
  cat(paste("美化CSV已保存至:", csv_output_path, "\n"))
  
  return(list(table = beautified_table, plot = table_image))
}

# 创建横排格式的表格（每个暴露变量一行，Model 0/1/2作为列，HR和P值分开）
create_wide_table <- function(df, effect_col, pvalue_col) {
  # 获取唯一的暴露变量
  unique_vars <- unique(df$Variable)
  
  # 创建空的结果表 - 每个模型有两列：HR和P值
  result_table <- data.frame(
    Variable = character(),
    `Model 0\nHR (95% CI)` = character(),
    `Model 0\nP Value` = character(),
    `Model 1\nHR (95% CI)` = character(),
    `Model 1\nP Value` = character(),
    `Model 2\nHR (95% CI)` = character(),
    `Model 2\nP Value` = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  # 为每个暴露变量创建一行
  for (var in unique_vars) {
    var_clean <- gsub("_", " ", var)
    var_clean <- tools::toTitleCase(var_clean)
    
    # 获取三个模型的结果
    model0_row <- df %>% filter(Model == "Model 0", Variable == var)
    model1_row <- df %>% filter(Model == "Model 1", Variable == var)
    model2_row <- df %>% filter(Model == "Model 2", Variable == var)
    
    effect0 <- if (nrow(model0_row) > 0) model0_row[[effect_col]][1] else "---"
    effect1 <- if (nrow(model1_row) > 0) model1_row[[effect_col]][1] else "---"
    effect2 <- if (nrow(model2_row) > 0) model2_row[[effect_col]][1] else "---"
    
    p0 <- if (nrow(model0_row) > 0) model0_row[[pvalue_col]][1] else "---"
    p1 <- if (nrow(model1_row) > 0) model1_row[[pvalue_col]][1] else "---"
    p2 <- if (nrow(model2_row) > 0) model2_row[[pvalue_col]][1] else "---"
    
    result_table <- rbind(
      result_table,
      data.frame(
        Variable = var_clean,
        `Model 0\nHR (95% CI)` = effect0,
        `Model 0\nP Value` = p0,
        `Model 1\nHR (95% CI)` = effect1,
        `Model 1\nP Value` = p1,
        `Model 2\nHR (95% CI)` = effect2,
        `Model 2\nP Value` = p2,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    )
  }
  
  return(result_table)
}

# 创建竞争风险格式的表格（按结果类型分组，每个结果类型有两列：HR和P值）
create_competing_risk_table <- function(df, effect_col, pvalue_col) {
  # 获取唯一的结果类型（如"Fine-Gray SHR - AD (SHR)"）
  unique_models <- unique(df$Model)
  # 获取唯一的暴露变量
  unique_vars <- unique(df$Variable)
  
  # 创建空的结果表
  result_table <- data.frame(
    Variable = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  # 为每个结果类型添加列（HR和P值各一列）
  for (model in unique_models) {
    # 清理模型名称：提取简洁的结果类型名称
    # 例如："Fine-Gray SHR - AD (SHR)" -> "AD"
    #       "Cause-specific HR - VaD (HR)" -> "VaD"
    model_clean <- gsub(" \\(.*\\)$", "", model)  # 移除 (HR) 或 (SHR)
    model_clean <- gsub(".*- ", "", model_clean)  # 提取"- "后面的部分
    model_clean <- trimws(model_clean)
    
    # 进一步简化名称
    if (grepl("Other", model_clean, ignore.case = TRUE)) {
      model_clean <- "Other"
    } else if (grepl("VaD|Vascular", model_clean, ignore.case = TRUE)) {
      model_clean <- "VaD"
    } else if (grepl("AD|Alzheimer", model_clean, ignore.case = TRUE)) {
      model_clean <- "AD"
    }
    
    result_table[[paste0(model_clean, "\nHR (95% CI)")]] <- character()
    result_table[[paste0(model_clean, "\nP Value")]] <- character()
  }
  
  # 为每个暴露变量创建一行
  for (var in unique_vars) {
    # 清理变量名：移除exposure_group前缀，美化显示
    var_clean <- gsub("^exposure_group", "", var)
    var_clean <- gsub("_", " ", var_clean)
    var_clean <- tools::toTitleCase(var_clean)
    
    row_data <- list(Variable = var_clean)
    
    # 为每个结果类型获取数据
    for (model in unique_models) {
      model_row <- df %>% filter(Model == model, Variable == var)
      
      effect <- if (nrow(model_row) > 0) model_row[[effect_col]][1] else "---"
      p_val <- if (nrow(model_row) > 0) model_row[[pvalue_col]][1] else "---"
      
      # 使用相同的清理逻辑
      model_clean <- gsub(" \\(.*\\)$", "", model)
      model_clean <- gsub(".*- ", "", model_clean)
      model_clean <- trimws(model_clean)
      
      if (grepl("Other", model_clean, ignore.case = TRUE)) {
        model_clean <- "Other"
      } else if (grepl("VaD|Vascular", model_clean, ignore.case = TRUE)) {
        model_clean <- "VaD"
      } else if (grepl("AD|Alzheimer", model_clean, ignore.case = TRUE)) {
        model_clean <- "AD"
      }
      
      row_data[[paste0(model_clean, "\nHR (95% CI)")]] <- effect
      row_data[[paste0(model_clean, "\nP Value")]] <- p_val
    }
    
    result_table <- rbind(
      result_table,
      as.data.frame(row_data, stringsAsFactors = FALSE, check.names = FALSE)
    )
  }
  
  return(result_table)
}

# 生成表格图片
generate_table_image <- function(df, effect_col) {
  # 检测是否是竞争风险格式（列数较多）
  is_wide_table <- ncol(df) > 7
  
  # 根据表格类型调整字体大小
  if (is_wide_table) {
    # 竞争风险格式：稍小的字体以适应更多列
    core_fontsize <- 10.5
    header_fontsize <- 12
    rowhead_fontsize <- 10.5
  } else {
    # 标准格式：正常字体
    core_fontsize <- 11
    header_fontsize <- 13
    rowhead_fontsize <- 11
  }
  
  # 创建表格Grob，使用更大的字体和更好的样式
  table_grob <- tableGrob(
    df,
    rows = NULL,
    theme = ttheme_minimal(
      core = list(
        fg_params = list(fontsize = core_fontsize, hjust = 0.5, x = 0.5),
        bg_params = list(fill = c(rep(c("white", "gray96"), length.out = nrow(df))))
      ),
      colhead = list(
        fg_params = list(fontsize = header_fontsize, fontface = "bold", hjust = 0.5, x = 0.5),
        bg_params = list(fill = "gray75")
      ),
      rowhead = list(
        fg_params = list(fontsize = rowhead_fontsize, fontface = "bold", hjust = 1, x = 0.95),
        bg_params = list(fill = "gray90")
      )
    )
  )
  
  # 调整列宽 - 根据列数动态设置
  n_cols <- ncol(df)
  if (n_cols > 1) {
    # Variable列宽度（竞争风险格式需要更宽）
    col_widths <- c(3.0)
    # 其余列：每2列为一组（HR + P值）
    n_effect_cols <- (n_cols - 1) / 2  # 结果类型数量
    for (i in 1:n_effect_cols) {
      col_widths <- c(col_widths, 2.8, 1.4)  # HR列更宽 + P值列稍宽
    }
    # 如果列数不匹配（可能不是标准的HR+P格式），使用平均分配
    if (length(col_widths) != n_cols) {
      col_widths <- c(3.0, rep((18 - 3.0) / (n_cols - 1), n_cols - 1))
    }
    if (length(col_widths) == n_cols) {
      table_grob$widths <- unit(col_widths, "in")
    }
  }
  
  # 添加三线表样式的线条
  # 顶部粗线
  table_grob <- gtable_add_grob(
    table_grob,
    grobs = segmentsGrob(
      x0 = unit(0, "npc"),
      y0 = unit(1, "npc"),
      x1 = unit(1, "npc"),
      y1 = unit(1, "npc"),
      gp = gpar(lwd = 2, col = "black")
    ),
    t = 1,
    l = 1,
    r = ncol(table_grob),
    name = "toprule"
  )
  
  # 表头下方细线
  table_grob <- gtable_add_grob(
    table_grob,
    grobs = segmentsGrob(
      x0 = unit(0, "npc"),
      y0 = unit(0, "npc"),
      x1 = unit(1, "npc"),
      y1 = unit(0, "npc"),
      gp = gpar(lwd = 1, col = "black")
    ),
    t = 2,
    l = 1,
    r = ncol(table_grob),
    name = "midrule"
  )
  
  # 底部粗线
  table_grob <- gtable_add_grob(
    table_grob,
    grobs = segmentsGrob(
      x0 = unit(0, "npc"),
      y0 = unit(0, "npc"),
      x1 = unit(1, "npc"),
      y1 = unit(0, "npc"),
      gp = gpar(lwd = 2, col = "black")
    ),
    t = nrow(table_grob),
    l = 1,
    r = ncol(table_grob),
    name = "bottomrule"
  )
  
  # 添加标题
  title_grob <- textGrob(
    "Model Results for Exposure Variables",
    gp = gpar(fontsize = 16, fontface = "bold"),
    just = "center"
  )
  
  # 组合标题和表格
  final_grob <- arrangeGrob(
    title_grob,
    table_grob,
    ncol = 1,
    heights = unit(c(1, 1), "null"),
    padding = unit(0.4, "in")
  )
  
  # 转换为ggplot对象以便使用ggsave
  p <- ggplot() +
    annotation_custom(final_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    theme_void() +
    theme(plot.margin = margin(0.6, 0.6, 0.6, 0.6, "in"))
  
  return(p)
}

# 批量处理所有CSV结果文件
beautify_all_results <- function(result_dir = "result") {
  if (!dir.exists(result_dir)) {
    warning(paste("目录不存在:", result_dir))
    return(NULL)
  }
  
  # 查找所有CSV文件（排除已经美化过的）
  csv_files <- list.files(
    result_dir,
    pattern = "\\.csv$",
    full.names = TRUE
  )
  
  # 排除诊断统计文件和其他非模型结果文件
  exclude_patterns <- c(
    "diagnostic", "ph_test", "c_index", "characteristics", 
    "assignments", "evaluation", "overlap", "stats"
  )
  
  csv_files <- csv_files[!grepl(paste(exclude_patterns, collapse = "|"), basename(csv_files), ignore.case = TRUE)]
  
  # 排除已经美化过的文件
  csv_files <- csv_files[!grepl("beautified", basename(csv_files), ignore.case = TRUE)]
  
  cat(paste("找到", length(csv_files), "个CSV结果文件需要美化\n"))
  
  results <- list()
  for (csv_file in csv_files) {
    cat(paste("\n处理文件:", basename(csv_file), "\n"))
    tryCatch({
      result <- beautify_model_results(csv_file, gsub("\\.csv$", "_beautified.png", csv_file))
      if (!is.null(result)) {
        results[[basename(csv_file)]] <- result
      }
    }, error = function(e) {
      warning(paste("处理文件时出错:", csv_file, "-", e$message))
    })
  }
  
  cat("\n所有文件处理完成！\n")
  return(results)
}

