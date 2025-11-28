# UKB Statistical Analysis Toolkit ✨

Welcome aboard! This toolkit packages the end-to-end statistical workflows built on UK Biobank accelerometry and registry data. It walks you through cognitive feature processing, activity regularity & exposure clustering, plus survival/clinical outcome modeling.

## IMPORTANCE
It is not advised to copy and use it directly , as the reproducibility was not considered at the beginning!!!

## Repository Layout 🗂️

```
├── 01_data_preparation/              # 数据准备与清洗
│   ├── table.R                       # 基线表数据清洗 + TableOne生成
│   └── table_cluster.R               # 聚类分析基线表 + TableOne生成
│
├── 02_feature_engineering/           # 特征工程
│   │
│   ├── A1_cognitive_score_famd.R     # 认知得分计算 (FAMD)
│   │
│   ├── B0_main.R                     # B模块主入口 (运行B1-B4)
│   ├── B1_diurnal_variability.R      # 日内时段活动变异性
│   ├── B2_intensity_variability.R    # 日间MVPA强度变异性
│   ├── B3_weekday_variability.R      # 周内各天活动变异性
│   ├── B4_regularity_score_pca.R     # PCA合成规律性得分
│   ├── B5_mvpa_weekly_pattern.R      # 周MVPA模式分类 (独立运行)
│   ├── B6_mvpa_time_pattern.R        # 日内MVPA时段模式 (独立运行)
│   ├── B7_exposure_cluster.R         # K-prototypes暴露聚类 (独立运行)
│   │
│   ├── C0_main.R                     # C模块主入口 (运行C1-C5)
│   ├── C1_dementia_icd10.R           # ICD10住院记录痴呆诊断
│   ├── C2_death_dementia.R           # 死亡登记痴呆诊断
│   ├── C3_dementia_merge.R           # 合并住院与死亡记录
│   ├── C3b_dementia_conflict.R       # 诊断冲突处理 (备用)
│   ├── C4_endpoint_status.R          # 定义终点事件状态
│   ├── C5_survival_time.R            # 计算生存时间
│   └── C6_competing_risk.R           # 竞争风险数据准备 (独立运行)
│
├── 03_analysis/                      # 统计分析
│   ├── cox_models/                   # Cox回归分析（痴呆结局）
│   │   ├── mice_cox_classify_dementia.R
│   │   ├── mice_cox_cluster_dementia.R
│   │   ├── mice_cox_mvpa_dementia.R
│   │   ├── mice_cox_regular_dementia.R
│   │   ├── mice_cox_time_pattern_dementia.R
│   │   └── competing_risk_classify_dementia.R
│   │
│   ├── linear_models/                # 线性模型分析（认知结局）
│   │   ├── mice_lm_classify_cognitive.R
│   │   ├── mice_lm_cluster_cognitive.R
│   │   ├── mice_lm_mvpa_cognitive.R
│   │   ├── mice_lm_regular_cognitive.R
│   │   ├── mice_lm_time_pattern_cognitive.R
│   │   └── mice_lm_sleep_cognitive.R
│   │
│   └── nonlinear_models/             # 非线性模型分析
│       ├── mice_gam_sleep_cognitive.R
│       └── mice_rcs_sleep_cognitive.R
│
├── 04_eda/                           # 探索性数据分析
│   ├── EDA.R                         # 基础EDA
│   ├── EDA2.R                        # 扩展EDA
│   ├── EDA_IPAQ.R                    # IPAQ数据EDA
│   ├── age_group_exploration.R       # 年龄分组探索
│   ├── dementia_cognitive_overlap.R  # 痴呆与认知重叠分析
│   ├── mixed_evening_type_profile.R  # 混合/傍晚型特征分析
│   ├── mvpa_cognitive_smooth_plot.R  # MVPA-认知平滑图
│   └── sleep_cognitive_smooth_plot.R # 睡眠-认知平滑图
│
├── 05_tables_figures/                # 表格与图表生成
│   ├── beautify_results.R            # 结果美化
│   └── normality_tests.R             # 正态性检验
│
├── 06_validation/                    # 验证与快速检查
│   ├── not_covariates.R              # 协变量验证
│   └── regular_score_dementia_cox.R  # 规律性得分验证
│
├── data/                             # 数据文件
├── mid_result/                       # 中间结果
├── result/                           # 最终结果
├── renv/                             # R环境
├── paths.R                           # 路径配置
└── UKb.Rproj                         # RStudio项目文件
```

## Environment Setup ⚙️

1. **Install dependencies**: run `R -q -e "renv::restore()"` in the repo root
2. **Configure data paths**: edit `paths.R` to match your filesystem
3. **Match the R version**: use the version recorded by `renv` (typically 4.x)

## Workflow Overview 🚀

### 1. Data Preparation (01_data_preparation/)
```bash
Rscript 01_data_preparation/table.R          # 基线表 + TableOne
Rscript 01_data_preparation/table_cluster.R  # 聚类基线表 + TableOne
```

### 2. Feature Engineering (02_feature_engineering/)
```bash
# 认知特征
Rscript 02_feature_engineering/A1_cognitive_score_famd.R

# 活动规律性 (B0运行B1-B4)
Rscript 02_feature_engineering/B0_main.R

# 生存结局 (C0运行C1-C5)
Rscript 02_feature_engineering/C0_main.R

# 独立模块 (按需运行)
Rscript 02_feature_engineering/B5_mvpa_weekly_pattern.R   # 周模式分类
Rscript 02_feature_engineering/B6_mvpa_time_pattern.R     # 时段模式分类
Rscript 02_feature_engineering/B7_exposure_cluster.R      # 暴露聚类
Rscript 02_feature_engineering/C6_competing_risk.R        # 竞争风险
```

### 3. Statistical Analysis (03_analysis/)
```bash
# Cox回归（痴呆结局）
Rscript 03_analysis/cox_models/mice_cox_regular_dementia.R

# 线性模型（认知结局）
Rscript 03_analysis/linear_models/mice_lm_mvpa_cognitive.R

# 非线性模型
Rscript 03_analysis/nonlinear_models/mice_gam_sleep_cognitive.R
```

### 4. EDA & Visualization (04_eda/)
```bash
Rscript 04_eda/mvpa_cognitive_smooth_plot.R
Rscript 04_eda/sleep_cognitive_smooth_plot.R
```

## Coding Style 📝

- tidyverse style: 2-space indent, `<-` assignment, snake_case names
- Script naming: `{Module}{Number}_{description}.R`
- Keep scripts executable via `Rscript`
- Use `dplyr` pipelines over nested base-R
- Document non-obvious constants with inline comments

## Contribution Guidelines 🤝

- Conventional commit messages (e.g., `feat: add diurnal regularity scorer`)
- Ensure sensitive data remains untracked by Git
- Attach screenshots for UI/figure changes in PRs
