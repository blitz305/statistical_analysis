
library(dplyr)
library(tidyr)
library(stringr)

dementia_codes <- c(
  "G30", "G300", "G301", "G308", "G309", "G310", "G311", 
  "G318", "F00", "F000", "F001", "F002", "F009", "F01", 
  "F010", "F011", "F012", "F013", "F018", "F019", "F02", 
  "F020", "F021", "F022", "F023", "F024", "F028", "F03", 
  "F051", "F106", "A810", "I673"
)

colnames(Summary_diagnoses)[1] <- "Participant_ID"
colnames(Summary_diagnoses)[2] <- "ICD10_codes"
# 步骤 0: 筛选需要的列
Summary_diagnoses_icd10 <- Summary_diagnoses %>%
  select(
    Participant_ID, 
    ICD10_codes, 
    starts_with("Date of first in-patient diagnosis - ICD10 | Array")
  )

# 步骤 1: 拆分代码并创建安全索引 
codes_long <- Summary_diagnoses_icd10 %>%
  select(Participant_ID, ICD10_codes) %>%
  filter(!is.na(ICD10_codes) & ICD10_codes != "") %>%
  separate_longer_delim(ICD10_codes, delim = "|") %>%
  filter(ICD10_codes != "") %>% 
  group_by(Participant_ID) %>%
  mutate(code_index = row_number()) %>%
  ungroup()

# 步骤 2: 拆分日期并创建索引
dates_long <- Summary_diagnoses_icd10 %>%
  select(Participant_ID, starts_with("Date of first in-patient diagnosis - ICD10 | Array")) %>%
  pivot_longer(
    cols = -Participant_ID,
    names_to = "date_col_name",
    values_to = "diagnosis_date",
    values_drop_na = TRUE
  ) %>%
  mutate(
 
    code_index = as.integer(str_extract(date_col_name, "(?<=Array )\\d+")) + 1
  ) %>%
  select(Participant_ID, code_index, diagnosis_date)

# 步骤 3: 安全连接并计算结果 
dementia_summary <- codes_long %>%
  left_join(dates_long, by = c("Participant_ID", "code_index")) %>%
  group_by(Participant_ID) %>%
  summarise(
    matched_dementia_codes = paste(
      sort(unique(ICD10_codes[ICD10_codes %in% dementia_codes])),
      collapse = ", "
    ),
    first_dementia_date = {
      valid_dates <- diagnosis_date[ICD10_codes %in% dementia_codes & !is.na(diagnosis_date)]
      if (length(valid_dates) > 0) as.Date(min(valid_dates)) else as.Date(NA)
    }
  ) %>%
  mutate(
    matched_dementia_codes = if_else(matched_dementia_codes == "", NA_character_, matched_dementia_codes)
  )

# 步骤 4: 合并，生成最终数据框 
all_participants <- Summary_diagnoses %>% select(Participant_ID)
dementia_results_final <- all_participants %>%
  left_join(dementia_summary, by = "Participant_ID") %>%
  mutate(
    dementia_status = ifelse(is.na(matched_dementia_codes), 0, 1)
  )



