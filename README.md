# UKB Statistical Analysis Toolkit

本仓库封装了基于 UK Biobank 加速度计与临床登记数据的整套统计分析流程，覆盖认知特征加工（Phase A）、活动节律与暴露聚类（Phase B）、以及生存/临床结局建模（Phase C）。除主流程外，还提供了复现性环境、快速校验脚本与发表表格的生成代码。

## 目录一览
- `cognitive_features/`（原 `A/`）：整理原始认知测试与筛查信息，`A.cognitive_result.R` 为入口脚本。
- `activity_regularity/`（原 `B/`）：对加速度计特征进行分段、聚类、规律性评分等处理，`B.main.R` 会串联该目录下的模块脚本。
- `survival_outcomes/`（原 `C/`）：准备死亡与痴呆结局、冲突消解与生存时间，`C.main.R` 是 Phase C 的执行入口。
- `analyze/`：多种 MICE+cox/gam/lm 配置，命名约定为“模型类型+暴露/结局”。直接使用 `Rscript analyze/<script>.R` 运行。
- `quick_checks/`：轻量验证或探索性脚本（例如 `regular_score+demential_cox.R`）。修改后可借助 `Rscript quick_checks/not_covariates.R` 等命令快速查看结果。
- `EDA/`：探索性图表与临时分析。
- `table1/`：发表所需表格及导出脚本。
- `paths.R`：集中管理所有数据输入/输出路径、种子与缓存位置，任何新脚本应复用此处的常量。
- `renv/` 与 `renv.lock`：使用 `renv` 固定 R 依赖环境。
- `UKb.Rproj`：便于在 RStudio 中以项目方式打开仓库。

## 环境准备
1. **安装依赖**：在仓库根目录执行 `R -q -e "renv::restore()"`，自动同步 `renv.lock` 中记录的包版本。
2. **配置数据路径**：根据本地文件系统编辑 `paths.R`，确保所有阶段脚本均可找到输入数据与输出目录。
3. **R 版本**：建议使用与 `renv` 锁定文件一致的 R 版本（通常为 4.x），以免引入未测试的运行差异。

## 工作流
### 1. Phase A – 认知特征
```bash
Rscript cognitive_features/A.cognitive_result.R
```
- 汇总并清洗认知测验结果。
- 输出供活动节律与生存分析使用的中间表。

### 2. Phase B – 活动规律性
```bash
Rscript activity_regularity/B.main.R
```
`B.main.R` 会依次调用以下模块：
- `B.classify_time_of_day.R` / `B.classify.R`：切分时间段与活动情境。
- `B.clusterize_exposures.R`：聚合暴露模式。
- `B.diurnal_regularity.R`、`B.intensity_consistency.R`、`B.regularity_socre.R`、`B.Temporal Regularity.R`：计算规律性评分和强度一致性指标。
- 将结果写入 `paths.R` 中定义的输出表。

### 3. Phase C – 生存/结局准备
```bash
Rscript survival_outcomes/C.main.R
```
- `C.outcome_variables.R`、`C.death_register.R`、`C.dementia_classify.R` 等脚本会生成结局变量与随访时间。
- `C.conflict.R`、`C.competing_risk_preparation.R` 负责冲突检测和竞争风险结构化。

### 4. 分析与建模
- 进入 `analyze/` 选择合适的脚本，例如：
  - `Rscript analyze/mice\ cox+regular_dementia.R` 评估规律性得分对痴呆的影响。
  - `Rscript analyze/mice_lm\ +mvpa_cognitive.R` 探索 MVPA 与认知的线性关系。
- 请在脚本顶部设置随机种子与筛选条件，保持结果可复现。

### 5. 快速验证与质量控制
- 将一次性验证逻辑放入 `quick_checks/`，以免污染主流程。
- 示例：`Rscript quick_checks/regular_score+demential_cox.R`。
- 在提交前至少复现一个 `analyze/` 路径或对应 quick check，并保存关键指标（c-index、HR 等）的控制台输出。

## 贡献约定
- 遵循 tidyverse 风格（2 空格缩进、`<-` 赋值、snake_case 命名）。
- 任何新增数据输出请确保未将敏感 CSV/原始数据加入 Git 版本控制。
- 提交信息示例：`feat: add diurnal regularity scorer`。PR 说明需概括模型/数据改动，并在 UI/图形更新时附截图。

## 常见问题
- **包缺失或版本不匹配**：确认已运行 `renv::restore()`，必要时删除 `renv/library` 后重新安装。
- **脚本找不到数据**：检查 `paths.R` 中的目录是否存在，以及是否拥有相应读写权限。
- **长任务复用**：建议将耗时逻辑封装为函数，在 `B.main.R` 或 `C.main.R` 里按需调用，方便下游分析复用。

如需补充目录说明或添加新阶段，请在 README 中保持同样的结构与语言风格，方便协作成员快速上手。
