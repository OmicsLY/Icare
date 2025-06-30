 
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Icare

<!-- badges: start -->
<!-- badges: end -->

🔍 Icare R包是一款专为医学检验组学数据分析设计的智能化工具集，旨在帮助医学检验人员高效、准确地处理与分析高维度、多样性的临床检验数据。
其模块化设计与高效的数据处理能力，为精准医疗和个性化治疗提供了强有力的技术支持，有望推动医学检验数据分析领域的标准化与智能化发展。


​**最好新创建一个新的R-project**​<br>
​**✧ 新建 R 项目操作指南 ✧**​
​**⬇ 请按以下步骤创建新项目 ⬇**

1. ​**在 RStudio 中点击**​：  
   `File → New Project... → New Directory → New Project`

2. ​**设置项目参数**​：  
   ✓ 项目名称：`Icare_Analysis`  
   ✓ 存储路径：选择你的工作目录
   
### 可视化函数标准化参数配置说明
所有可视化函数均支持以下标准化参数配置，用户可根据实际需求调整这些参数以自定义输出效果：<br>
使用wesanderson调色板:<br>
`palette_name = "AsteroidCity1"`<br>
自动保存图表:<br>
`save_plot = TRUE` <br> 
统一保存路径:<br>
`save_dir = here("ModelData")`  <br>
图表宽度(英寸):<br>
`plot_width = 5` <br>
图表高度(英寸): <br>
`plot_height = 5`<br> 
基础字体大小:<br>
`base_size = 14 ` <br>
自动保存数据:<br>
`save_data = TRUE` <br> 

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

## 1.Quick Start

This is a basic example which shows you how to use `Icare`:

``` r
###加载示例数据
data("raw_data")
data("valid_data")
```

### 1.疾病预测模块
#### 1.1 数据加载与简单清洗
在疾病预测模块中，您可以使用数据来创建 `Train_Model` 对象。<br>
`PrepareData`函数用于对数据进行简单处理，特别是将因子变量（factor 或 character 类型）转换为二进制或哑变量（dummy variables），以便数据符合模型的输入要求。
``` r
# 创建 Train_Model 对象
object_model <- CreateModelObject(
  data = raw_data,  # 加载数据
  group_col = "group"    # 分组列名
)

# 使用 Train_Model 对象进行数据处理
object_model <- PrepareData(object_model)
```
#### 1.2 数据集划分
`SplitDatModel`函数用于将数据按比例拆分为训练集和测试集。<br>
基于分组列`group_col`进行分层拆分，以确保训练集和测试集中的类别分布一致。<br>
默认训练集`train_ratio=0.7`测试集，`test_ratio=0.3`<br>
如果输入是 `Train_Model 对象`，函数会自动更新对象的`split.data`槽位，存储数据划分结果。<br>
`Extract_validata`是一个​验证集提取函数，主要用于从新数据中分离出验证集并与现有模型对象整合。<br>
`Extract_external_validation`用于提取外部验证集。<br>
``` r
# 将数据拆分为训练集和测试集
object_model<-SplitDatModel(object_model,
                         train_ratio = 0.7,
                         test_ratio = 0.3)
# 提取验证集
object_model <- Extract_validata(data = new_data, object_model = object_model)
# 提取外部验证集
object_model <- Extract_external_validation(data = new_data, object_model = object_model)

```
#### 1.3 数据清洗
**数据缺失值删除**<br>
`ModelRemoveMiss`是一个专门处理机器学习数据集中的缺失值问题。<br>
该函数自动识别并移除高缺失率的变量和样本，同时确保训练集、测试集、验证集和外部验证集保持一致的变量集合。<br>
`miss_threshold`为缺失率阈值(%)，超过此值的变量/样本将被移除<br>

``` r
# 使用Train_Model对象
object_model <- ModelRemoveMiss(object_model, 
                               miss_threshold = 30)
                               
```
**数据缺失值填补**<br>
`ModelApplyMiss`该函数自动识别并填补训练集、测试集、验证集和外部验证集中的缺失值，支持多种填补方法（如mice、均值、中位数等），并确保各数据集间填补策略的一致性。<br>
如果输入是 Train_Model 对象，函数会自动对象的split.data槽位（包含填补后的数据集）、process.info槽位（包含填补详情）。<br>
``` r
object_model<-ModelApplyMiss(object_model, 
              impute_method = "mice",
              m = 10,
              save_data = TRUE)
```

**异常值检测**<br>
`ModelDetectOutliers`提供智能异常值检测功能，基于训练数据建立正常值范围并自动应用于测试集、验证集和外部验证集，支持可视化分析和变量级自定义阈值设置。<br>
如果输入是 Train_Model 对象，函数会自动对象的process.info槽位（包含异常值信息）。<br>
``` r
object_model <- ModelDetectOutliers(object_model,
                                    custom_ranges = list("AGE" = c(0, 100)))
```
**异常值处理**<br>
`ModelHandleOutliers`实现端到端异常值处理流程，自动将训练集处理策略应用于测试集/验证集，并完整记录处理参数和结果。<br>
检测到异常值后，支持以下四种处理方式：<br>
- ​**replace**：用中位数或四分位距（IQR）范围内的值替换异常值。<br>
- ​**remove**：删除包含异常值的样本。<br>
- **keep**：保留异常值，不做处理。<br>
- ​**capping**：将异常值限制在指定范围内（如 IQR 的上下限）<br>
默认使用`handle_method = "replace"`
``` r
object_model <- ModelHandleOutliers(object_model, 
                                    handle_method = "replace")
```
**数据平衡处理**<br>
`BalanceData`函数用于处理数据中的类别不平衡问题，支持过采样（over）、欠采样（under）或两者结合（both）的方法。默认使用 both 方法`method = "both"`。<br>
该函数根据类别不平衡情况自动选择是否进行平衡处理，并提供了可视化功能，用于展示平衡前后的类别分布。<br>
如果类别不平衡比例低于 默认`imbalance_threshold=0.15` 或样本大小超过 默认`sample_size_threshold=1500`，则不会进行平衡处理，除非`force_balance=TRUE`<br>

``` r
# 使用 Train_Model 对象进行数据平衡处理
object_model <- BalanceData(object_model,
                         imbalance_threshold = 0.15,
                         sample_size_threshold = 1500,
                         force_balance = FALSE,
                         method = "both")

```

<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/class_distribution_balance.png" alt="Screenshot" width="500">
</div>

**数据标准化**<br>
`NormalizeData` 提供端到端数据标准化解决方案，自动选择最优方法并确保多数据集标准化一致性，支持结果导出，也可用参数`normalize_method`指定选择想要的数据标准化方式。
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

``` r
# 对数据进行标准化处理
object_model<-NormalizeData(object_model,
                         normalize_method = "auto")
```

####1.4 特征筛选与特征子集过滤
`SelFeatureSet` 函数用于从数据集中选择最优特征子集，支持基于以下方法的特征选择：
- 信息值（IV）​：评估特征与目标变量之间的关联强度。<br>
- ​最大信息系数（MIC）​：衡量特征与目标变量之间的非线性关系。<br>
- ​互信息（MI）​：量化特征与目标变量之间的信息共享程度。<br>

该函数通过计算不同特征数量下的 AUC（Area Under Curve）值，选择最优特征子集，并可视化 AUC 随特征数量的变化趋势。<br>
- `AUC_change_threshold `是用于判断特征选择过程中 AUC（Area Under Curve）值变化的阈值。当增加特征数量时，如果 AUC 的提升幅度小于该阈值，则认为继续增加特征数量对模型性能的提升不再显著，从而停止特征选择。默认值`AUC_change_threshold=0.01`，即 AUC 变化小于 1% 时，选择当前特征数量为最优。<br>
- `max_feature`为筛选得到最大特征子集数量，默认`max_feature=NULL`，即使用所有特征。<br>
- data_type用于指定进行特征筛选的数据类型，可选值为 "clean"（清洗后的数据）或 "scale"（标准化后的数据）默认`data_type = "clean"`<br>

`FilterDataFeatures` 函数用于根据特征选择结果或直接使用完整数据集，过滤训练集和测试集，保留最优特征子集或全部特征。<br>
该函数支持从清洗后或标准化后的数据中进行过滤，并更新 `Train_Model` 对象的 `filtered.set` 槽位。<br>

``` r

# 从数据集中选择最优特征子集
object_model <- SelFeatureSet(object_model,
                           AUC_change_threshold=0.01,
                           max_feature=NULL,
                           data_type = "clean")


# 过滤特征子集
#也可以不经过特征筛选直接执行下面这一步,对数据进行过滤
object_model <- FilterDataFeatures(object_model)
```
<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/Combined_AUC_vs_Features.png" alt="Screenshot" width="500">
</div>


####1.5 模型训练与分析

**训练模型**<br>


`ModelTrainAnalysis` 函数用于训练多个机器学习模型，评估其性能，并生成 ROC 曲线和性能指标。支持多种模型（如 GBM、随机森林、SVM 等），并允许自定义超参数调优和交叉验证设置。<br>
`methods`:模型名称列表，默认为 `methods=c("gbm", "rf", "svmLinear", "svmRadial", "glmnet")`<br>
`tune_grids`:模型超参数调优网格，默认为预定义的调优网格<br>
`classProbs`：是否计算类别概率，默认为 TRUE。<br>
`allowParallel`：是否启用并行计算，默认为 TRUE。<br>
` loocv_threshold = 100`当样本量<100时自动切换为留一法(LOOCV)
如果输入是 Train_Model 对象，函数会自动更新其 all.results 槽位（用于存储所有模型在训练集上的性能分析结果）和 train.models 槽位（用于存储训练完成的所有模型），并返回更新后的对象。<br>


``` r
# 训练模型并分析性能
object_model<-ModelTrainAnalysis(object_model,
                                 methods = c("glm", "rpart", "naive_bayes", "bayesglm", "rf",
                                           "xgbTree", "svmRadial", "svmLinear", "gbm", "earth", "glmnet"),
                                 control = list(method = "repeatedcv", number = 10, 
                                                repeats = 5),
                                 tune_grids = list(
                                 glm = NULL,
                                 rpart = expand.grid(cp = seq(0.0001, 0.01, length.out = 10)),
                                 naive_bayes = NULL,
                                 bayesglm = NULL,
                                 rf = expand.grid(mtry = 1:5),
                                 xgbTree = expand.grid(
                                   nrounds = 100,
                                   max_depth = c(2, 4, 6),
                                   eta = c(0.01, 0.1),
                                   gamma = 0,
                                   colsample_bytree = 1,
                                   min_child_weight = 1,
                                   subsample = 1
                                 ),
                                 svmRadial = expand.grid(sigma = 0.01, C = 2^(-1:2)),
                                 svmLinear = expand.grid(C = c(0.01, 0.1, 1)),
                                 gbm = expand.grid(
                                   n.trees = c(50, 100),
                                   interaction.depth = c(2, 3),
                                   shrinkage = c(0.001, 0.01),
                                   n.minobsinnode = c(10, 20)
                                 ),
                                 earth = expand.grid(degree = 1:2, nprune = 2:10),
                                 glmnet = expand.grid(
                                   alpha = c(0.1, 0.5, 0.9),
                                   lambda = 10^seq(-4, -1, 1)
                                 )
                               ),
                                 loocv_threshold = 100,
                                 classProbs = TRUE, 
                                 allowParallel = TRUE,seed=1234)


```

<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/roc_curves.png" alt="Screenshot" width=500">
</div>

**提取最佳模型结果**<br>

`SelectBestModel ` 函数用于从 `Train_Model` 对象中提取性能最佳模型，支持自定义性能指标（如 AUC、准确率等）作为最佳模型选择指标默认`metric="auc"`<br>
并且支持基于指定评估指标自动选择或用户手动指定模型。
``` r
# 自动选择AUC最高的模型
object_model <- SelectBestModel(object_model, metric = "auc")

# 手动指定使用随机森林模型
object_model <- SelectBestModel(object_model, custom_selection = "rf")
```
**超参数调整**<br>
`​ModelHyperparameterPlot​`可视化模型超参数调优过程，支持多种绘图风格并自动保存结果到模型对象。<br>
`​ModelTuneSuggestion`基于当前最优模型智能生成超参数调优建议，采用局部搜索策略提升调优效率。<br>
基于当前最优参数值，按`expand_factor`系数智能扩展搜索范围,用户可通过`custom_tune`手动指定调参网格
`​ModelTuneComparison​`通过ROC曲线和AUC指标对比调优前后模型性能，生成专业可视化图表并保存比较结果。<br>

``` r
##可视化当前模型的超参数调优结果
object_model <- ModelHyperparameterPlot(object_model)


##智能生成并执行参数调优
object_model <- ModelTuneSuggestion(object_model)

##对比调优前后的模型性能
object_model <- ModelTuneComparison(object_model)
```




**最佳模型混淆矩阵生成**<br>
`ModelBestCM`函数用于从 `Train_Model`对象中提取性能最佳模型，并在测试集上生成混淆矩阵及其可视化图表。<br>
如果输入是 Train_Model 对象，函数会自动更新其 best.model.result 槽位<br>

``` r

# 生成最佳模型的混淆矩阵
object_model <- ModelBestCM(object_model)
```
<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/confusion_matrix_plot.png" alt="Screenshot" width=500">
</div>

**特征重要性分析**<br>
`FeatureImportance` 函数用于从 `Train_Model` 对象中提取最佳模型，并计算其特征重要性。<br>
支持自定义显示前 top_n 个重要特征默`top_n = 15`，并生成可视化图表。<br>
如果输入是 Train_Model 对象，函数会自动更新其 best.model.result 槽位<br>

``` r
# 计算特征重要性并生成可视化图表
object_model<-FeatureImportance(object_model,
                             top_n =15)
```
<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/Feature_Importance.png" alt="Screenshot" width=500">
</div>

**SHAP 值分析模块**<br>
`ModelShap` 函数用于从` Train_Model `对象中提取最佳模型，并生成 SHAP（SHapley Additive exPlanations）值分析的可视化图表。<br>
支持生成 Beeswarm 图、Force 图和 Waterfall 图<br>
如果输入是 Train_Model 对象，函数会自动更新其 shap.result 槽位<br>

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
`Extract_external_validata` 函数用于将外部验证数据集添加到 Train_Model 对象中。<br>
支持从 Stat 对象或直接提供的数据框中提取验证数据<br>
如果输入是 Train_Model 对象，函数会自动更新其filtered.set槽位存储外部验证集<br>

``` r

# 直接提供验证数据并更新 `Train_Model` 对象
data(val_data)
validation_data <- val_data
object_model <- Extract_external_validata(data = validation_data, object_model = object_model)

# 从 `Stat` 对象中提取验证数据并更新 `Train_Model` 对象
object_model <- Extract_external_validata(object_stats = object_val, object_model = object_model)

```
**模型外部验证**<br>
`ModelValidation`函数用于对 `Train_Model` 对象中的最佳模型进行外部验证。<br>
支持在独立验证集上评估模型性能，生成 ROC 曲线<br>
如果输入是 Train_Model 对象，函数会自动更新其 best.model.result 槽位<br>

``` r
# 进行模型外部验证
object_model <- ModelValidation(object_model)
```
<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/validation_roc_plot.png" alt="Screenshot" width=500">
</div>

####1.5 模型临床应用 

**模型阈值选择**
`ModelThreshold` 函数用于从 `Train_Model` 对象中提取最佳模型，并在测试集上选择最佳阈值。<br>
支持基于最大准确率、最接近 0.95 的 PPV 或 NPV 来选择阈值，并生成相应的可视化图表。<br>
如果输入是 Train_Model 对象，函数会自动更新其best.model.result槽位存储最优阈值<br>


``` r
# 选择最佳阈值并生成可视化图表
object_model<-ModelThreshold(object_model,
                          method_threshold="max_accuracy")
```
<div align="center">
<img src="https://github.com/OmicsLY/Icare/blob/master/fig/accuracy_vs_threshold_curve.png" alt="Screenshot" width=500">
</div>

**临床预测**<br>
`ModelClinicalPrediction`函数用于使用`Train_Model`对象中的最佳模型对新临床数据进行预测。<br>
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



