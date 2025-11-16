# 1. 准备工作
# ==========================================
# 加载所需的包
library(dplyr)
library(stringr)
library(data.table)

# 2. 前置检查
# ==========================================
if (!exists("final_dataset")) {
  stop("未检测到 final_dataset。请先运行 C.main.R 或确保 final_dataset 已在环境中可用。")
}

required_cols <- c(
  "Participant_ID", "recruitment_date", "Endpoint_Date", "Endpoint_Status",
  "first_dementia_date", "matched_dementia_codes", "matched_dementia_codes_death",
  "date_of_death"
)
missing_cols <- setdiff(required_cols, names(final_dataset))
if (length(missing_cols) > 0) {
  stop(paste0("final_dataset 缺少以下列，无法继续: ", paste(missing_cols, collapse = ", ")))
}

# 创建 data.table 的安全副本并统一日期类型
final_dataset_dt <- data.table::copy(final_dataset)
final_dataset_dt[, c("recruitment_date", "Endpoint_Date", "first_dementia_date", "date_of_death") :=
                   lapply(.SD, function(x) as.Date(as.character(x))),
                 .SDcols = c("recruitment_date", "Endpoint_Date", "first_dementia_date", "date_of_death")]

# 3. 定义痴呆亚型编码
# ==========================================
ad_codes <- c("G30", "G300", "G301", "G308", "G309",
             "F00", "F000", "F001", "F002", "F009")
vad_codes <- c("F01", "F010", "F011", "F012", "F013", "F018", "F019")

# 4. 辅助函数：拆分编码字符串
# ==========================================
extract_codes <- function(code_string) {
  if (is.null(code_string) || length(code_string) == 0 || is.na(code_string)) {
    return(character(0))
  }
  codes <- str_split(code_string, pattern = "\\s*,\\s*", simplify = FALSE)[[1]]
  codes[codes != ""]
}

# 5. 创建竞争风险分析所需的数据框
# ==========================================
competing_risk_dataset <- final_dataset_dt %>%
  as_tibble() %>%
  mutate(across(c(matched_dementia_codes, matched_dementia_codes_death), ~if_else(.x == "", NA_character_, .x))) %>%
  rowwise() %>%
  mutate(
    combined_codes = list(unique(c(
      extract_codes(matched_dementia_codes),
      extract_codes(matched_dementia_codes_death)
    ))),
    has_ad_code = any(combined_codes %in% ad_codes),
    has_vad_code = any(combined_codes %in% vad_codes),
    dementia_subtype = case_when(
      Endpoint_Status != 1 ~ NA_character_,
      has_ad_code & !has_vad_code ~ "AD",
      has_vad_code & !has_ad_code ~ "VaD",
      (has_ad_code & has_vad_code) ~ "Other",
      TRUE ~ "Other"
    ),
    event_type_numeric = case_when(
      Endpoint_Status == 0 ~ 0L,                               # 截尾
      Endpoint_Status == 2 ~ 4L,                               # 非痴呆死亡
      Endpoint_Status == 1 & dementia_subtype == "AD" ~ 1L,    # AD 痴呆
      Endpoint_Status == 1 & dementia_subtype == "VaD" ~ 2L,   # VaD 痴呆
      Endpoint_Status == 1 ~ 3L,                               # 其他痴呆
      TRUE ~ NA_integer_
    ),
    event_date = case_when(
      Endpoint_Status == 1 ~ first_dementia_date,
      Endpoint_Status == 2 ~ date_of_death,
      TRUE ~ Endpoint_Date
    ),
    event_date = coalesce(event_date, Endpoint_Date),
    survival_time_days = as.numeric(event_date - recruitment_date)
  ) %>%
  ungroup() %>%
  mutate(
    event_type = factor(
      event_type_numeric,
      levels = c(0, 1, 2, 3, 4),
      labels = c("Censored", "AD", "VaD", "Other dementia", "Non-dementia death")
    )
  ) %>%
  select(
    Participant_ID,
    recruitment_date,
    event_date,
    survival_time_days,
    event_type_numeric,
    event_type,
    dementia_subtype,
    combined_codes,
    matched_dementia_codes,
    matched_dementia_codes_death,
    Endpoint_Status
  )

# 6. 输出检查
# ==========================================
print("竞争风险数据集 (前6行)：")
print(head(competing_risk_dataset))

print("======================================================")
print("事件类型分布 (event_type):")
print(table(competing_risk_dataset$event_type, useNA = "ifany"))

# 7. 可选：移除中间变量
drop_cols <- c("combined_codes")
competing_risk_dataset <- competing_risk_dataset %>%
  mutate(event_type_numeric = as.integer(event_type_numeric)) %>%
  select(-any_of(drop_cols))

print("竞争风险数据集已创建，存储于对象 competing_risk_dataset。")
