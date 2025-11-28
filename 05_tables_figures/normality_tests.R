# Table 1 所有连续变量的正态性检验
# ==========================================

library(dplyr)
library(ggplot2)
library(moments)

# 定义需要检验的连续变量
continuous_vars <- c("age_recruitment", "townsend_index", "bmi", "sedentary_hours", "cognitive_score_0")

# 1. 对所有连续变量进行正态性检验
# ==========================================

normality_results <- data.frame(
  variable = character(),
  test_type = character(),
  p_value = numeric(),
  is_normal = logical(),
  skewness = numeric(),
  kurtosis = numeric(),
  stringsAsFactors = FALSE
)

for(var in continuous_vars) {
  cat("\n=== 检验变量:", var, "===\n")
  
  # 获取非缺失值
  var_data <- baseline_table_step1[[var]]
  var_data <- var_data[!is.na(var_data)]
  
  if(length(var_data) == 0) {
    cat("变量", var, "没有有效数据\n")
    next
  }
  
  # 选择检验方法
  if(length(var_data) < 3000) {
    test_result <- shapiro.test(var_data)
    test_type <- "Shapiro-Wilk"
  } else {
    # 使用更稳健的参数估计方法进行KS检验
    test_result <- ks.test(var_data, "pnorm", 
                          mean = median(var_data),  # 使用中位数而非均值
                          sd = mad(var_data))       # 使用MAD而非标准差
    test_type <- "Kolmogorov-Smirnov"
  }
  
  # 计算偏度和峰度
  var_skew <- skewness(var_data)
  var_kurt <- kurtosis(var_data)
  
  # 判断是否正态分布
  # 对于大样本(n>3000)，使用更宽松的标准(p>0.01)
  # 对于小样本，使用传统标准(p>0.05)
  if(length(var_data) >= 3000) {
    is_normal <- test_result$p.value > 0.01
  } else {
    is_normal <- test_result$p.value > 0.05
  }
  
  # 输出结果
  cat("检验方法:", test_type, "\n")
  cat("p值:", round(test_result$p.value, 6), "\n")
  cat("显著性水平:", ifelse(length(var_data) >= 3000, "α=0.01", "α=0.05"), "\n")
  cat("偏度:", round(var_skew, 3), "\n")
  cat("峰度:", round(var_kurt, 3), "\n")
  cat("是否正态分布:", ifelse(is_normal, "是", "否"), "\n")
  
  # 保存结果
  normality_results <- rbind(normality_results, data.frame(
    variable = var,
    test_type = test_type,
    p_value = test_result$p.value,
    is_normal = is_normal,
    skewness = var_skew,
    kurtosis = var_kurt
  ))
}

# 2. 汇总结果
# ==========================================

cat("\n=== 正态性检验结果汇总 ===\n")
print(normality_results)

# 识别非正态分布的变量
nonnormal_vars <- normality_results$variable[!normality_results$is_normal]
normal_vars <- normality_results$variable[normality_results$is_normal]

cat("\n符合正态分布的变量:", paste(normal_vars, collapse = ", "), "\n")
cat("不符合正态分布的变量:", paste(nonnormal_vars, collapse = ", "), "\n")

# 3. 描述性统计
# ==========================================

cat("\n=== 描述性统计 ===\n")
for(var in continuous_vars) {
  var_data <- baseline_table_step1[[var]]
  var_data <- var_data[!is.na(var_data)]
  
  if(length(var_data) > 0) {
    cat("\n", var, ":\n")
    cat("  样本量:", length(var_data), "\n")
    cat("  均值:", round(mean(var_data), 3), "\n")
    cat("  中位数:", round(median(var_data), 3), "\n")
    cat("  标准差:", round(sd(var_data), 3), "\n")
    cat("  偏度:", round(skewness(var_data), 3), "\n")
    cat("  峰度:", round(kurtosis(var_data), 3), "\n")
  }
}

# 4. 可视化分布
# ==========================================

# 为每个连续变量创建直方图和Q-Q图
library(gridExtra)

plot_list <- list()
for(i in seq_along(continuous_vars)) {
  var <- continuous_vars[i]
  var_data <- baseline_table_step1[[var]]
  var_data <- var_data[!is.na(var_data)]
  
  if(length(var_data) > 0) {
    # 直方图
    p_hist <- ggplot(data.frame(x = var_data), aes(x = x)) +
      geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.7, fill = "blue") +
      geom_density(color = "red", size = 1) +
      stat_function(fun = dnorm, 
                    args = list(mean = mean(var_data), sd = sd(var_data)),
                    color = "green", size = 1) +
      labs(title = paste(var, "Distribution"), x = var, y = "Density")
    
    # Q-Q图
    p_qq <- ggplot(data.frame(x = var_data), aes(sample = x)) +
      stat_qq() +
      stat_qq_line() +
      labs(title = paste(var, "Q-Q Plot"))
    
    plot_list[[i*2-1]] <- p_hist
    plot_list[[i*2]] <- p_qq
  }
}

# 显示所有图形
if(length(plot_list) > 0) {
  do.call(grid.arrange, c(plot_list, ncol = 2))
}

# 5. 生成建议的 nonnormal 参数
# ==========================================

cat("\n=== 建议的 nonnormal 参数 ===\n")
if(length(nonnormal_vars) > 0) {
  cat("建议在 CreateTableOne() 中使用以下 nonnormal 参数:\n")
  cat("nonnormal = c(", paste(paste0('"', nonnormal_vars, '"'), collapse = ", "), ")\n")
} else {
  cat("所有连续变量都符合正态分布，可以省略 nonnormal 参数\n")
}

# 6. 生成更新后的代码建议
# ==========================================

cat("\n=== 更新 picture.R 的建议 ===\n")
if(length(nonnormal_vars) > 0) {
  cat("将 picture.R 中的 nonnormal 参数更新为:\n")
  cat("nonnormal = c(", paste(paste0('"', nonnormal_vars, '"'), collapse = ", "), "),\n")
} else {
  cat("可以删除 nonnormal 参数，因为所有变量都符合正态分布\n")
}
