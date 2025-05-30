 
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Icare

<!-- badges: start -->
<!-- badges: end -->

🔍 Icare R包是一款专为医学检验组学数据分析设计的智能化工具集，旨在帮助医学检验人员高效、准确地处理与分析高维度、多样性的临床检验数据。
该包提供从数据预处理、疾病预测、亚型分型到预后建模的一站式分析流程，严格遵循国际标准），并整合了多种统计方法与机器学习算法。
通过用户友好的Shiny界面，Icare R包显著降低了使用门槛，提升了数据分析的透明性与可重复性。
其模块化设计与高效的数据处理能力，为精准医疗和个性化治疗提供了强有力的技术支持，有望推动医学检验数据分析领域的标准化与智能化发展。

It offers four main applications:

- ​数据预处理模块：通过多种方法（如缺失值填补、异常值处理、标准化）和可视化工具，确保数据质量与一致性，为后续分析奠定基础。
- 疾病预测模块：整合传统统计与机器学习算法，构建稳健的预测模型，支持特征筛选、模型评估与可解释性分析，助力疾病风险评估与诊断。
- 亚型分型模块：采用聚类与降维技术，识别疾病潜在亚型，结合可视化工具与分型评估方法，为个性化治疗方案提供科学依据。
- ​预后建模与生存分析模块：通过Cox回归、随机生存森林等方法，构建个性化预后模型，结合Kaplan-Meier曲线与森林图，精确预测患者生存风险并支持临床决策

(--------放一张总的图-----------)​<br>

​**最好新创建一个新的R-project**​<br>
​**✧ 新建 R 项目操作指南 ✧**​
​**⬇ 请按以下步骤创建新项目 ⬇**


1. ​**在 RStudio 中点击**​：  
   `File → New Project... → New Directory → New Project`

2. ​**设置项目参数**​：  
   ✓ 项目名称：`Icare_Analysis`  
   ✓ 存储路径：选择你的工作目录
   
​**可视化函数标准化参数配置说明**​<br>
所有可视化函数均支持以下标准化参数配置，用户可根据实际需求调整这些参数以自定义输出效果：<br>
使用wesanderson调色板:<br>
`palette_name = "AsteroidCity1"`<br>
自动保存图表:<br>
`save_plot = TRUE` <br> 
统一保存路径:<br>
`save_dir = here("PrognosiX", "results")`  <br>
图表宽度(英寸):<br>
`plot_width = 5` <br>
图表高度(英寸): <br>
`plot_height = 5`<br> 
基础字体大小:<br>
`base_size = 14 ` <br>

## 1.Install

You can install the development version of Icare like so:

``` r

# 安装devtools包（如果尚未安装）
if (!require("devtools")) install.packages("devtools")

# 从GitHub安装Icare包
devtools::install_github("OmicsLY/Icare")

# 加载包
library(Icare)
```

## 2.Quick Start

This is a basic example which shows you how to use `Icare`:

``` r
###加载示例数据
###要求输入的对象为一个数据框
###info.data和raw.data的行名是要一致的 
data(raw_data)
df<-raw_data
info <- as.data.frame(df[c("event", "time")])
df <- df[, !(names(df) %in% c("event", "time"))]
df<-df.frame(df)

```


### 1.数据预处理模块
#### 1.1 数据准备与变量类型注释
加载对象`Stat`
``` r
object_stat <- CreateStatObject(
  raw.data = df,   ###原始数据
  #clean.data = clean,  ###传入清洗后的数据
  info.data = info,  ###临床信息
  group_col = "group" ###标签信息、假设不存在group_col=NULL 
)
```

注释变量的类型（数值型、分类型、需要独热编码的变量）。<br>
如果一个分类变量的唯一值数量`max_unique_values = 5`超过 5，则认为其需要进行独热编码处理。<br>
如果输入是 Stat 对象，函数将注信息存储在`variable.types`槽中<br>
``` r
# 注释变量类型
object_stat <- stat_diagnose_variable_type(object_stat)
```

#### 1.2 缺失值处理
**变量缺失值的分布**<br>

如果输入是 Stat 对象，函数将缺失值信息存储在`processe.info`槽中
``` r

object_stat<-state_plot_missing_data(object_stat)
```
<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/combined_missing_data_plot.png" alt="Screenshot" width="500">
</div>

**缺失值处理**<br>
对缺失值进行处理，支持以下两种方法：<br>
- **mice**：多重插补法，适合数据缺失机制复杂的情况。<br>
- **median_mode**：中位数/众数填补法，适合数据缺失机制简单的情况。<br>

默认使用 mice 方法（impute_method = "mice"）。<br>
如果样本或特征的缺失值比例超过 20%（miss_threshold = 20），则自动删除该样本或特征。<br>
如果输入是 Stat 对象，清洗后的数据将在`clean.data`槽中更新
``` r

object_stat<-stat_miss_processed(object_stat,
                                 impute_method ="mice",
                                 miss_threshold = 20)
```

#### 1.3 异常值处理
**异常值检测**<br>
使用 `stat_detect_and_mark_outliers` 函数检测数据中的异常值，并对其进行标记并可视化。<br>
如果输入是 Stat 对象，函数将异常值信息存储在`outlier.marked`槽中
``` r

# 检测并标记异常值
object_stat <- stat_detect_and_mark_outliers(object_stat)
```
<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/boxplot_outliers_batch_60.png" alt="Screenshot" width="500">
</div>



**异常值处理**<br>
检测到异常值后，支持以下四种处理方式：
- ​**replace**：用中位数或四分位距（IQR）范围内的值替换异常值。
- ​**remove**：删除包含异常值的样本。
- **keep**：保留异常值，不做处理。
- ​**capping**：将异常值限制在指定范围内（如 IQR 的上下限）

默认使用`handle_method = "replace"`
如果输入是 Stat 对象，函数将清洗后的数据存储在`outlier.marked`槽中，并且对`clean.data`进行更新
``` r
# 处理异常值
object_stat <- stat_handle_outliers(object_stat, handle_method = "replace")

```

#### 1.4 生成基线表
您可以通过`gaze_method`参数选择组间均值比较的统计方法,默认`gaze_method=3`
- 1 forces analysis as normal-distributed
- 2 forces analysis as continuous non-normal
- 3 performs a Shapiro-Wilk test or nortest::ad.test to decide between normal or non-normal <br>

允许用户自定义公式 `formula` 或基于 `group_cols` 自动生成分析公式。<br>
分析结果可按 `digits`指定的小数位数进行四舍五入，并可选择是否显示 p 值 (show.p = TRUE/FALSE)。<br>
如果输入是 `Stat` 对象，函数将基线表信息存储在`baseline.table`槽中<br>
``` r
##生成基线表格
object_stat<-stat_gaze_analysis(object_stat,
                                show.p = TRUE,
                                gaze_method = 3)
```
<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/gaze_analysis.png" alt="Screenshot" width="500">
</div>


#### 1.5 描述性统计分析

**统计分析**<br>
`stat_compute_descriptive` 函数用于计算数据的描述性统计信息，包括数值型变量的均值、中位数、标准差等，以及分类变量的频数统计。<br>
此外，它还会对数值型变量进行正态性检验，并根据检验结果分别计算正态和非正态变量的统计量。<br>
如果输入是 Stat 对象，函数将统计结果存储在`compute.descriptive`槽中
``` r
object_stat <- stat_compute_descriptive(object_stat)

#> object_stat@compute.descriptive[["Group_Counts"]]
#  0   1 
#  200 260 
#> object_stat@compute.descriptive[["Count_Results"]][["ABO"]]
#  A  AB   B   O 
#134  37 125 164 
#> object_stat@compute.descriptive[["Num_Results"]][["Normal"]]
#           AGE        TT       MCV      MCHC     RDWSD       rct
#Mean 55.486957 16.651739 40.911087 30.392174 43.834565 4.3802391
#SD    9.485561  1.724771  4.279162  1.733919  3.160162 0.4429685
#> object_stat@compute.descriptive[["Num_Results"]][["Non_Normal"]]
#                 HBcAb        HCVAg PreS1antigenofHBV
#AD_p_value 5.930567e-24 6.577279e-21      1.412496e-24
#Median     6.700000e-01 3.300000e-01      1.700000e-01
#IQR        2.100250e+00 6.000000e-02      3.025000e-01
#> object_stat@compute.descriptive[["Normality_Test"]][["AGE"]]
#$p_value
#[1] 0.09958748
#$is_normal
#[1] TRUE
```
**对分类变量可视化**
``` r
object_stat<-plot_categorical_descriptive(object_stat)
```
<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/ABO_plot.png" alt="Screenshot" width="500">
</div>

**对数值型变量可视化**
- ​小提琴图：展示数值型变量的分布密度，适合观察数据的整体分布和集中趋势。
- ​山脊图：按组展示数值型变量的分布密度，适合比较不同组之间的分布差异。
plot_numeric_descriptive 函数用于对数值型变量进行可视化展示，支持两种`plot_type`展示形式："violin"（小提琴图）和 "ridge"（山脊图）。
默认使用`plot_type = "violin"`
``` r
###山脊图绘制
object_stat <- plot_numeric_descriptive(object_stat,plot_type = "ridge")
###小提琴图绘制
object_stat <- plot_numeric_descriptive(object_stat,plot_type = "violin")

```
<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/density_ridge_plot_part_50.png" alt="Screenshot" width="500">
</div>

<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/violin_plot_part_55.png" alt="Screenshot" width="500">
</div>


#### 1.6数据类型转换与独热编码
**数据类型转换**<br>
`stat_convert_variables`函数用于将数据中的变量转换为正确的数据类型。例如，将字符型变量转换为因子型，或将数值型变量保留为数值型。

``` r
# 将变量转换为正确的数据类型
object_stat <- stat_convert_variables(object_stat)
```
**独热编码**<br>
 `stat_onehot_encode`函数用于对分类变量进行独热编码（One-Hot Encoding），将其转换为二进制向量形式
``` r
# 对分类变量进行独热编码
object_stat <- stat_onehot_encode(object_stat)
```

#### 1.7 数据标准化
多种标准化方法： <br>
| 方法名称                 | 描述                                                                 |
|--------------------------|----------------------------------------------------------------------|
| `auto`                  | 自动选择合适的标准化方法（基于数据分布特征）                          |
| `log_transform`         | 对数变换，适用于右偏分布且全正值的数据                                 |
| `min_max_scale`         | 最小-最大缩放，将数据线性变换到 [0,1] 范围                             |
| `z_score_standardize`   | Z 分数标准化，转换为均值为 0、标准差为 1 的分布                        |
| `max_abs_scale`         | 最大绝对值缩放，将数据除以该特征的最大绝对值                           |
| `center_data`           | 中心化处理，使数据均值为 0 但不缩放方差                                |
| `scale_data`            | 缩放处理，调整数据尺度但不改变中心位置                                 |
| `boxcox_transform`      | Box-Cox 变换，适用于正数数据的幂变换                                  |
| `yeojohnson_transform`  | Yeo-Johnson 变换，适用于含零/负值数据的广义幂变换                     |


默认 `normalize_method = "auto" `的执行流程：<br>
>对每个数值型变量，首先进行正态性检验（Shapiro-Wilk 检验）。<br>
>根据变量的分布特征，自动选择最合适的归一化方法。<br>
>如果自动选择的方法应用失败，将自动退回使用 Z 分数标准化（z_score_standardize）作为补救措施。<br>
>整个处理过程中，会记录每一列数据的归一化方法及相关处理细节，便于后续追踪与复现。<br>

如果输入是 Stat 对象，函数会自动更新对象的 scale.data 槽位，存储标准化后的数据。
``` r
# 使用对数变换标准化数据
object_stat <- stat_normalize_process(object_stat, normalize_method = "auto")

# 使用最小-最大缩放标准化数据
object_stat <- stat_normalize_process(object_stat, normalize_method = "log_transform")
```
#### 1.8 相关性分析与交叉变量分析
**相关性分析**<br>
`stat_correlated_features` 函数用于分析数据集中的特征相关性，主要提供以下功能：
1.相关性计算：
- 计算变量间的相关性矩阵
- 识别高相关变量对（默认阈值correlation_threshold = 0.95）
- 按相关性值降序排序并输出前5组最高相关变量对
2.检测高度相关特征：
- 根据指定的相关性阈值（correlation_threshold）默认为 0.95，高于该阈值的特征对将被视为高度相关。<br>
- ​生成相关性热图：可视化特征之间的相关性矩阵<br>
- `data_type`数据类型，可选 "clean" 或 "scale"，默认为 "scale"<br>
如果输入是 Stat 对象，函数会自动更新对象的 corr.result槽位，存储相关性分析结果。
``` r
# 检测高度相关特征并生成相关性热图
object_stat <- stat_correlated_features(object_stat, data_type="scale",correlation_threshold = 0.95)

#> object_stat@corr.result[["top5_pairs"]]
#         drop_feature        corr_feature corr_value
#4           pre_RET_r Ret_mid_F_Intensity  0.9789148
#5 Ret_mid_F_Intensity           pre_RET_r  0.9789148
#1                  PT                 INR  0.9579065
#2                 INR                  PT  0.9579065
#3                 MCV                  hb  0.9563364
```

<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/correlation_heatmap.png" alt="Screenshot" width="500">
</div>


**相关性热图（Top N 特征）** <br>
`cor_top_correlations `函数用于生成相关性热图，展示前 N 个最相关的特征。

``` r
# 检测高度相关特征并生成相关性热图
# 生成前 15 个最相关特征的相关性热图
object_stat <- cor_top_correlations(object_stat,top_n = 15)
```
<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/top_correlations_plot.png" alt="Screenshot" width="500">
</div>

**交叉变量分析** <br>
`cross_plot_analysis` 函数用于对两个变量进行交叉分析，生成散点图并计算相关性。
``` r
# 对两个变量（如cl和cr）进行交叉分析
cross_plot_analysis(object_stat, vars = c("cl", "cr"))
```

<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/scatter_plot_cl_vs_cr_00.png" alt="Screenshot" width="500">
</div>

#### 1.9 差异分析与可视化
`stat_var_feature` 函数用于对数据进行差异分析，识别在不同组别之间显著变化的特征。<br>
⚠️ 注意 | 关键前提:
本功能必须在数据包含`group_col`指定的有效分组列时才能正常运行！使用前请确保：
如果输入是 Stat 对象，函数会自动更新对象的 var.result 槽位，存储差异分析结果。<br>
- Wilcoxon 检验：使用 Wilcoxon 秩和检验比较两组之间的差异。
​- 多重检验校正：对 p 值进行 Bonferroni 校正，控制假阳性率。
- ​差异特征筛选：根据 logFC 和 p 值筛选显著变化的特征。
- ​生物标志物筛选：差异分析可用于识别潜在的生物标志物。
- ​数据探索：可视化工具（如火山图、小提琴图）可用于探索数据分布和差异。
- 模型评估：ROC 曲线图可用于评估特征的分类性能。

**进行差异分析**<br>
  
``` r
# 进行差异分析
object_stat <- stat_var_feature(object_stat, data_type = "clean")
#> object_stat@var.result[["last_test_sig"]]
#                  id       W            p       mean_x     mean_y median_x median_y
#2                alp 48762.0 2.318873e-58 1.382423e+02  76.820000  129.000   74.000
#14               PLT 46322.5 7.060287e-47 3.243819e+02 175.544000  310.500  159.600
#1                alb 18289.0 4.879498e-08 3.886615e+01  41.236500   39.400   41.500
#11               L_r 19655.5 7.168725e-06 2.035346e+01  23.748000   19.550   23.400
#10               L_c 20400.0 7.431591e-05 1.434423e+00   1.622850    1.410    1.505
#       p.adjust       sd_0       sd_1        logFC change
#2  4.405858e-57   22.88034   38.45866  0.847645337     Up
#14 1.341455e-45   71.13009  106.62522  0.885860738     Up
#1  9.271046e-07   3.772383   4.823576 -0.085407540 Stable
#11 1.362058e-04   7.815845   7.917520 -0.222531842 Stable
#10 1.412002e-03  0.4801972  0.4426622 -0.178059054 Stable  

```
**雷达图可视化** <br>
`VarFeature_radarchart` 函数用于生成雷达图，展示显著变化特征的分布。
``` r
# 生成雷达图
object_var <- VarFeature_radarchart(object_var)
```
<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/radar_chart.png" alt="Screenshot" width="500">
</div>

**火山图可视化** <br>
`VarFeature_volcano`函数用于生成火山图，展示差异分析结果。
``` r
# 生成火山图
object_stat <- VarFeature_volcano(object_stat)
```

<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/volcano_plot.png" alt="Screenshot" width="500">
</div>


**小提琴图可视化** <br>
`VarFeature_violinplot` 函数用于生成小提琴图，展示显著变化特征的分布。
``` r
# 生成小提琴图
object_stat <- VarFeature_violinplot(object_stat)
```

<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/violinplot.png" alt="Screenshot" width="500">
</div>

**ROC 曲线图可视化** <br>
`VarFeature_ROC` 函数用于生成 ROC 曲线图，评估显著变化特征的分类性能。
``` r
# 生成 ROC 曲线图
object_stat <- VarFeature_ROC(object_stat)
```
<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/roc_plot.png" alt="Screenshot" width="500">
</div>


#### 1.10 保存结果 
保存清洗后的 Stat 对象<br>
在完成数据清洗后，清洗后的 `Stat` 对象可以保存为 .RData 文件。您可以使用`readRDS`函数加载该对象，以便在后续分析中使用。
``` r
###保存S4对象
saveRDS(object_stat, file = "object_stat.RDS")
###读取S4对象
object_stat <-readRDS(file = "object_stat.RData")
```

### 2.疾病预测模块
#### 2.1 数据加载与简单清洗
在疾病预测模块中，您可以使用 `Stat` 对象或清洗后的数据来创建 `Model` 对象。<br>
`PrepareData`函数用于对数据进行简单处理，特别是将因子变量（factor 或 character 类型）转换为二进制或哑变量（dummy variables），以便数据符合模型的输入要求。
``` r
# 使用 Stat 对象创建 Model 对象
object_model <- CreateModelObject(
  object = object_stat,  # 加载的 Stat 对象
  group_col = "group"    # 分组列名
)


# 使用清洗后的数据创建 Model 对象
object_model <- CreateModelObject(
  clean.data = data,  # 清洗后的数据框
  group_col = "group" # 分组列名
)

# 使用 Model_data 对象进行数据处理
object_model <- PrepareData(object_model)
```

#### 2.2 数据平衡处理
`BalanceData`函数用于处理数据中的类别不平衡问题，支持过采样（over）、欠采样（under）或两者结合（both）的方法。默认使用 both 方法`method = "both"`。<br>
该函数根据类别不平衡情况自动选择是否进行平衡处理，并提供了可视化功能，用于展示平衡前后的类别分布。<br>
如果类别不平衡比例低于 默认`imbalance_threshold=0.15` 或样本大小超过 默认`sample_size_threshold=1500`，则不会进行平衡处理，除非`force_balance=TRUE`<br>
如果输入是 `Model_data` 对象，函数会自动更新对象的`clean.data`槽位，存储相关性分析结果。

``` r

# 使用 Model_data 对象进行数据平衡处理
object_model <- BalanceData(object_model,
                         imbalance_threshold = 0.15,
                         sample_size_threshold = 1500,
                         force_balance = FALSE,
                         method = "both")

####采取强制平衡处理
object_model <- BalanceData(object_model,
                        force_balance =T)

```

<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/class_distribution_balance.png" alt="Screenshot" width="500">
</div>

#### 2.2 数据集划分
`SplitDatModel`函数用于将数据按比例拆分为训练集和测试集。基于分组列`group_col`进行分层拆分，以确保训练集和测试集中的类别分布一致。<br>
默认训练集`train_ratio=0.7`测试集`test_ratio=0.3`<br>
如果输入是 `Model_data 对象`，函数会自动更新对象的`split.data`槽位，存储数据划分结果。<br>

``` r
 
# 将数据拆分为训练集和测试集
object_model<-SplitDatModel(object_model,
                         train_ratio = 0.7,
                         test_ratio = 0.3)
```

#### 2.3 数据标准化
`NormalizeData` 函数对数据进行标准化处理，并将标准化方法应用于测试集。支持多种标准化方法同1.7
如果输入是 `Model_data 对象`，函数会自动更新对象的`split.sacle.data`槽位，存储数据标准化处理结果。<br>

``` r
# 对数据进行标准化处理
object_model<-NormalizeData(object_model,
                         normalize_method = "auto")
```

#### 2.4 特征筛选与特征子集过滤
`SelFeatureSet` 函数用于从数据集中选择最优特征子集，支持基于以下方法的特征选择：
- 信息值（IV）​：评估特征与目标变量之间的关联强度。<br>
- ​最大信息系数（MIC）​：衡量特征与目标变量之间的非线性关系。<br>
- ​互信息（MI）​：量化特征与目标变量之间的信息共享程度。<br>

该函数通过计算不同特征数量下的 AUC（Area Under Curve）值，选择最优特征子集，并可视化 AUC 随特征数量的变化趋势。<br>
- `AUC_change_threshold `是用于判断特征选择过程中 AUC（Area Under Curve）值变化的阈值。当增加特征数量时，如果 AUC 的提升幅度小于该阈值，则认为继续增加特征数量对模型性能的提升不再显著，从而停止特征选择。默认值`AUC_change_threshold=0.01`，即 AUC 变化小于 1% 时，选择当前特征数量为最优。<br>
- `max_feature`为筛选得到最大特征子集数量，默认`max_feature=NULL`，即使用所有特征。<br>
- data_type用于指定进行特征筛选的数据类型，可选值为 "clean"（清洗后的数据）或 "scale"（标准化后的数据）默认`data_type = "clean"`<br>

`FilterDataFeatures` 函数用于根据特征选择结果或直接使用完整数据集，过滤训练集和测试集，保留最优特征子集或全部特征。<br>
该函数支持从清洗后或标准化后的数据中进行过滤，并更新 `Model_data` 对象的 `filtered.set` 槽位。<br>

``` r

# 从数据集中选择最优特征子集
object_model <- SelFeatureSet(object_model,
                           AUC_change_threshold=0.01,
                           max_feature=NULL,
                           data_type = "clean")
#> object_model@feature.result[["best_features_subset"]]
# [1] "PLT"                 "alp"                 "alb"                 "L_c"                 "FDP"              


# 过滤特征子集
#也可以不经过特征筛选直接执行下面这一步,对数据进行过滤
object_model <- FilterDataFeatures(object_model)
```
<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/Combined_AUC_vs_Features.png" alt="Screenshot" width="500">
</div>


#### 2.5 模型训练与分析

**训练模型**<br>


`ModelTrainAnalysis` 函数用于训练多个机器学习模型，评估其性能，并生成 ROC 曲线和性能指标。支持多种模型（如 GBM、随机森林、SVM 等），并允许自定义超参数调优和交叉验证设置。<br>
`methods`:模型名称列表，默认为 `methods=c("gbm", "rf", "svmLinear", "svmRadial", "glmnet")`<br>
`tune_grids`:模型超参数调优网格，默认为预定义的调优网格<br>
`classProbs`：是否计算类别概率，默认为 TRUE。<br>
`verboseIter`：是否显示训练过程的详细信息，默认为 FALSE。<br>
`allowParallel`：是否启用并行计算，默认为 TRUE。<br>
如果输入是 Model_data 对象，函数会自动更新其 all.results 槽位（用于存储所有模型在训练集上的性能分析结果）和 train.models 槽位（用于存储训练完成的所有模型），并返回更新后的对象。<br>


``` r
# 训练模型并分析性能
object_model<-ModelTrainAnalysis(object_model,
                                 methods = c("gbm", "rf", "svmLinear", "svmRadial", "glmnet"),
                                 control = list(method = "repeatedcv", number = 10, 
                                                repeats = 5),
                                 tune_grids = list(
                                   gbm = expand.grid(
                                     n.trees = c(50, 100),
                                     interaction.depth = c(2, 3),
                                     shrinkage = c(0.001, 0.01),
                                     n.minobsinnode = c(10, 20)
                                   ),
                                   rf = expand.grid(
                                     mtry = c(2, 3, 4, 5)
                                   ),
                                   svmLinear = expand.grid(
                                     C = c(0.01, 0.1, 1)
                                   ),
                                   svmRadial = expand.grid(
                                     sigma = c(0.01, 0.05, 0.1),
                                     C = c(0.1, 1)
                                   ),
                                   glmnet = expand.grid(
                                     alpha = c(0.1, 0.5, 0.9),
                                     lambda = 10^seq(-4, -1, 1)
                                   )
                                 ),
                                 classProbs = TRUE, 
                                 verboseIter = FALSE,
                                 allowParallel = TRUE,seed=1234)

#> object_model@all.results
#              Model Sensitivity Specificity Positive_predictive_value Negative_predictive_value accuracy_score
#rf               rf   1.0000000   1.0000000                 100.00000                 100.00000      100.00000
#svmRadial svmRadial   0.9863946   0.9942857                  99.31507                  98.86364       99.06832
#svmLinear svmLinear   0.9795918   0.9657143                  96.00000                  98.25581       97.20497
#glmnet       glmnet   0.9659864   0.9714286                  96.59864                  97.14286       96.89441
#gbm             gbm   0.8843537   0.9485714                  93.52518                  90.71038       91.92547
#          Precision f1_score recall_score       auc
#rf        100.00000 1.980198    100.00000 1.0000000
#svmRadial  99.31507 1.953388     98.63946 0.9987949
#svmLinear  96.00000 1.939394     97.95918 0.9919534
#glmnet     96.59864 1.912844     96.59864 0.9913314
#gbm        93.52518 1.752140     88.43537 0.9861613
```

<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/roc_curves.png" alt="Screenshot" width=500">
</div>

**提取最佳模型结果**<br>

`ModelBestRoc` 函数用于从 `Model_data` 对象中提取性能最佳模型，并在训练集和测试集上绘制 ROC 曲线。支持自定义性能指标（如 AUC、准确率等）作为最佳模型选择指标默认`metric="auc"`<br>
如果输入是 Model_data 对象，函数会自动更新其 best.model.result 槽位存储最佳模型及其在测试集上的性能分析结果<br>

``` r
# 绘制最佳模型的 ROC 曲线
object_model <- ModelBestRoc(object_model,
                          metric="auc")
#> object_model@best.model.result[["test_result"]]
#  Model Sensitivity Specificity Positive_predictive_value Negative_predictive_value accuracy_score Precision f1_score
#1    rf   0.8679245   0.9529412                        92                  92.04545       92.02899        92 1.719626
#  recall_score       auc
#1     86.79245 0.9651498

```

<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/best_model_roc_plot.png" alt="Screenshot" width=500">
</div>

**最佳模型混淆矩阵生成**<br>
`ModelBestCM`函数用于从 `Model_data`对象中提取性能最佳模型，并在测试集上生成混淆矩阵及其可视化图表。<br>
如果输入是 Model_data 对象，函数会自动更新其 best.model.result 槽位<br>

``` r

# 生成最佳模型的混淆矩阵
object_model <- ModelBestCM(object_model)
```
<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/confusion_matrix_plot.png" alt="Screenshot" width=500">
</div>

**特征重要性分析**<br>
`FeatureImportance` 函数用于从 `Model_data` 对象中提取最佳模型，并计算其特征重要性。<br>
支持自定义显示前 top_n 个重要特征默`top_n = 15`，并生成可视化图表。<br>
如果输入是 Model_data 对象，函数会自动更新其 best.model.result 槽位<br>

``` r
# 计算特征重要性并生成可视化图表
object_model<-FeatureImportance(object_model,
                             top_n =15)
```
<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/Feature_Importance.png" alt="Screenshot" width=500">
</div>

**SHAP 值分析模块**<br>
`ModelShap` 函数用于从` Model_data `对象中提取最佳模型，并生成 SHAP（SHapley Additive exPlanations）值分析的可视化图表。<br>
支持生成 Beeswarm 图、Force 图和 Waterfall 图<br>
如果输入是 Model_data 对象，函数会自动更新其 shap.result 槽位<br>

``` r

# 生成 SHAP 值分析的可视化图表
object_model <- ModelShap(object_model)
```
<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/shap_beeswarm_plot.png" alt="Screenshot" width=500">
</div>

<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/shap_force_plot.png" alt="Screenshot" width=500">
</div>

<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/shap_waterfall_plot.png" alt="Screenshot" width=500">
</div>



**添加外部验证集**<br>
`Extract_external_validata` 函数用于将外部验证数据集添加到 Model_data 对象中。<br>
支持从 Stat 对象或直接提供的数据框中提取验证数据<br>
如果输入是 Model_data 对象，函数会自动更新其filtered.set槽位存储外部验证集<br>

``` r

# 直接提供验证数据并更新 `Model_data` 对象
data(val_data)
validation_data <- val_data
object_model <- Extract_external_validata(data = validation_data, object_model = object_model)

# 从 `Stat` 对象中提取验证数据并更新 `Model_data` 对象
object_model <- Extract_external_validata(object_stats = object_val, object_model = object_model)

```
**模型外部验证**<br>
`ModelValidation`函数用于对 `Model_data` 对象中的最佳模型进行外部验证。<br>
支持在独立验证集上评估模型性能，生成 ROC 曲线<br>
如果输入是 Model_data 对象，函数会自动更新其 best.model.result 槽位<br>

``` r
# 进行模型外部验证
object_model <- ModelValidation(object_model)
```
<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/validation_roc_plot.png" alt="Screenshot" width=500">
</div>

#### 2.5 模型临床应用 

**模型阈值选择**
`ModelThreshold` 函数用于从 `Model_data` 对象中提取最佳模型，并在测试集上选择最佳阈值。<br>
支持基于最大准确率、最接近 0.95 的 PPV 或 NPV 来选择阈值，并生成相应的可视化图表。<br>
如果输入是 Model_data 对象，函数会自动更新其best.model.result槽位存储最优阈值<br>


``` r
# 选择最佳阈值并生成可视化图表
object_model<-ModelThreshold(object_model,
                          method_threshold="max_accuracy")
```
<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/accuracy_vs_threshold_curve.png" alt="Screenshot" width=500">
</div>

**临床预测**<br>
`ModelClinicalPrediction`函数用于使用`Model_data`对象中的最佳模型对新临床数据进行预测。<br>
支持基于最佳阈值生成预测结果，并可视化预测概率和分类。<br>
可根据上述函数`ModelThreshold`得到的结果也可以自定义，这里选取通用的0.5

``` r
# 对新临床数据进行预测
# 使用 ModelClinicalPrediction 进行预测（自动提取最佳阈值）
Clinical_results <- ModelClinicalPrediction(object = object_model, new_data = new_data)

# 使用自定义阈值进行预测
Clinical_results <- ModelClinicalPrediction(object = object_model, new_data = new_data, best_threshold = 0.5)
```
<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/prediction_visualization.png" alt="Screenshot" width=500">
</div>



