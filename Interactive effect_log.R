library(robustbase)   # 用于稳健回归分析
library(car)           # 用于方差膨胀因子计算
library(piecewiseSEM)  # 结构方程模型
library(openxlsx)
library(dplyr)
library(broom)
library(officer)
library(flextable)

# ========== 数据读取与预处理 ==========
data_all1 <- read.xlsx("D:/SHDI-RWI(SPEI)/Statistical Analysis/Regression_Q25.xlsx", sheet = 1)

# 过滤异常值
for (var in c("Rt", "Rs_pnas")) {
  m <- mean(data_all1[[var]], na.rm = TRUE)
  s <- sd(data_all1[[var]], na.rm = TRUE)
  data_all1 <- data_all1[data_all1[[var]] >= (m - 3 * s) & data_all1[[var]] <= (m + 3 * s), ]
}

# 筛选并标准化部分变量
data_all <- data_all1 %>%
  filter(
    shdi    >= 0.5,
    Rt      >= 1,
    Rs_pnas > 0,
    Rs_pnas <= 1
  )

cols_to_scale <- 10:19
data_all[, cols_to_scale] <- scale(data_all[, cols_to_scale])

# 分类数据集
data_short      <- subset(data_all1, Drought == 1)
data_prolonged  <- subset(data_all1, Drought == 2)

# 过滤异常值
for (var in c("Rt", "Rs_pnas")) {
  m <- mean(data_short[[var]], na.rm = TRUE)
  s <- sd(data_short[[var]], na.rm = TRUE)
  data_short <- data_short[data_short[[var]] >= (m - 3 * s) & data_short[[var]] <= (m + 3 * s), ]
}

# 筛选并标准化部分变量
data_short <- data_short %>%
  filter(
    shdi    >= 0.5,
    Rt      >= 1,
    Rs_pnas > 0,
    Rs_pnas <= 1
  )

cols_to_scale <- 10:19
data_short[, cols_to_scale] <- scale(data_short[, cols_to_scale])

# 过滤异常值
for (var in c("Rt", "Rs_pnas")) {
  m <- mean(data_prolonged[[var]], na.rm = TRUE)
  s <- sd(data_prolonged[[var]], na.rm = TRUE)
  data_prolonged <- data_prolonged[data_prolonged[[var]] >= (m - 3 * s) & data_prolonged[[var]] <= (m + 3 * s), ]
}

# 筛选并标准化部分变量
data_prolonged <- data_prolonged %>%
  filter(
    shdi    >= 0.5,
    Rt      >= 1,
    Rs_pnas > 0,
    Rs_pnas <= 1
  )

cols_to_scale <- 10:19
data_prolonged[, cols_to_scale] <- scale(data_prolonged[, cols_to_scale])

# ========== 定义函数：获取 RIC 分组 ==========
get_RIC_levels <- function(data) {
  m <- mean(data$RIC, na.rm = TRUE)
  s <- sd(data$RIC, na.rm = TRUE)
  return(c("Low1" = m - s, "Mid" = m, "High1" = m + s))
}

# ========== 定义函数：预测值与置信区间（原始尺度） ==========
get_prediction_df <- function(model, data, model_name) {
  shdi_seq <- seq(min(data$shdi, na.rm = TRUE), max(data$shdi, na.rm = TRUE), length.out = 100)
  levels_RIC <- get_RIC_levels(data)
  
  pred_df_list <- list()
  
  for (group_name in names(levels_RIC)) {
    RIC_val <- levels_RIC[group_name]
    
    new_data <- data.frame(
      shdi = shdi_seq,
      RIC   = RIC_val,
      DI  = mean(data$DI, na.rm = TRUE),
      TMP  = mean(data$TMP, na.rm = TRUE),
      CTI  = mean(data$CTI, na.rm = TRUE),
      FCC  = mean(data$FCC, na.rm = TRUE)
    )
    new_data$`shdi:RIC` <- new_data$shdi * new_data$RIC
    
    pred <- predict(model, newdata = new_data, interval = "confidence", level = 0.95)
    
    result <- data.frame(
      shdi = new_data$shdi,
      RIC   = new_data$RIC,
      Group = group_name,
      fit  = exp(pred[, "fit"]),
      lwr  = exp(pred[, "lwr"]),
      upr  = exp(pred[, "upr"])
    )
    
    result_unique <- result %>%
      group_by(shdi, Group) %>%
      summarise(across(c(fit, lwr, upr), mean), .groups = "drop")
    
    pred_df_list[[group_name]] <- result_unique
  }
  
  do.call(rbind, pred_df_list)
}

# ========== 拟合模型 ==========
fit_all_Rt <- lmrob(log(Rt) ~ shdi + DI + TMP + CTI + FCC + shdi:RIC, data = data_all)
print(summary(fit_all_Rt))
fit_all_Rs <- lmrob(log(Rs_pnas) ~ shdi + DI + TMP + CTI + FCC + shdi:RIC, data = data_all)
print(summary(fit_all_Rs))

fit_short_Rt <- lmrob(log(Rt) ~ shdi + DI + TMP + CTI + FCC + shdi:RIC, data = data_short)
print(summary(fit_short_Rt))
fit_short_Rs <- lmrob(log(Rs_pnas) ~ shdi + DI + TMP + CTI + FCC + shdi:RIC, data = data_short)
print(summary(fit_short_Rs))

fit_pro_Rt <- lmrob(log(Rt) ~ shdi + DI + TMP + CTI + FCC + shdi:RIC, data = data_prolonged)
print(summary(fit_pro_Rt))
fit_pro_Rs <- lmrob(log(Rs_pnas) ~ shdi + DI + TMP + CTI + FCC + shdi:RIC, data = data_prolonged)
print(summary(fit_pro_Rs))

# ========== 生成预测数据框 ==========
df1 <- get_prediction_df(fit_all_Rt, data_all, "Rt_all")
df2 <- get_prediction_df(fit_all_Rs, data_all, "Rs_all")
df3 <- get_prediction_df(fit_short_Rt, data_short, "Rt_short")
df4 <- get_prediction_df(fit_short_Rs, data_short, "Rs_short")
df5 <- get_prediction_df(fit_pro_Rt, data_prolonged, "Rt_prolonged")
df6 <- get_prediction_df(fit_pro_Rs, data_prolonged, "Rs_prolonged")

# ========== 写入 Excel ==========
write.xlsx(
  list(
    "Rt_all"        = df1,
    "Rs_all"        = df2,
    "Rt_short"      = df3,
    "Rs_short"      = df4,
    "Rt_prolonged"  = df5,
    "Rs_prolonged"  = df6
  ),
  file = "D:/SHDI-RWI(SPEI)/Statistical Analysis/SHDI_Prediction_Interval_RIC5_new3.xlsx",
  rowNames = FALSE
)

cat("\n 有分组预测结果已写入 Excel。\n")


########  首先，为每个数据集创建分组变量
# 首先，为每个数据集创建分组变量
create_ric_groups <- function(data) {
  levels <- get_RIC_levels(data)
  # 创建分割点，确保包含所有数据范围
  breaks <- c(-Inf, levels["Low1"], levels["Mid"], levels["High1"], levels["High2"], Inf)
  data$RIC_Group <- cut(data$RIC, 
                        breaks = breaks,
                        labels = c("Low2", "Low1", "Mid", "High1", "High2"),
                        include.lowest = TRUE)
  return(data)
}

# 应用函数到您的数据集
data_all <- create_ric_groups(data_all)
data_short <- create_ric_groups(data_short)
data_prolonged <- create_ric_groups(data_prolonged)

# 方法1: 使用dplyr的count()函数[2,8](@ref)
library(dplyr)

# 查看data_prolonged中各组的样本数
sample_counts_prolonged <- data_prolonged %>% count(RIC_Group)
print(sample_counts_prolonged)

# 方法2: 使用基础函数table()
table(data_prolonged$RIC_Group)












# 多元稳健回归分析

fit_short_Rt <- lmrob(log(Rt) ~ shdi + DI + TMP + CTI + FCC + shdi:RIC, data = data_short)
print(summary(fit_short_Rt))
fit_short_Rs <- lmrob(log(Rs_pnas) ~ shdi + DI + TMP + CTI + FCC + shdi:RIC, data = data_short)
print(summary(fit_short_Rs))

fit_pro_Rt <- lmrob(log(Rt) ~ shdi + DI + TMP + CTI + FCC + shdi:RIC, data = data_prolonged)
print(summary(fit_pro_Rt))
fit_pro_Rs <- lmrob(log(Rs_pnas) ~ shdi + DI + TMP + CTI + FCC + shdi:RIC, data = data_prolonged)
print(summary(fit_pro_Rs))

# 提取 summary 和 confint
summary_short_Rt_df <- as.data.frame(summary(fit_short_Rt)$coefficients)
confint_short_Rt_df <- as.data.frame(confint(fit_short_Rt, level = 0.95))
summary_short_Rs_df <- as.data.frame(summary(fit_short_Rs)$coefficients)
confint_short_Rs_df <- as.data.frame(confint(fit_short_Rs, level = 0.95))
summary_pro_Rt_df <- as.data.frame(summary(fit_pro_Rt)$coefficients)
confint_pro_Rt_df <- as.data.frame(confint(fit_pro_Rt, level = 0.95))
summary_pro_Rs_df <- as.data.frame(summary(fit_pro_Rs)$coefficients)
confint_pro_Rs_df <- as.data.frame(confint(fit_pro_Rs, level = 0.95))
# 保留 10 位小数
summary_short_Rt_df[] <- lapply(summary_short_Rt_df, function(x) round(x, 10))
confint_short_Rt_df[] <- lapply(confint_short_Rt_df, function(x) round(x, 10))
summary_short_Rs_df[] <- lapply(summary_short_Rs_df, function(x) round(x, 10))
confint_short_Rs_df[] <- lapply(confint_short_Rs_df, function(x) round(x, 10))
summary_pro_Rt_df[] <- lapply(summary_pro_Rt_df, function(x) round(x, 10))
confint_pro_Rt_df[] <- lapply(confint_pro_Rt_df, function(x) round(x, 10))
summary_pro_Rs_df[] <- lapply(summary_pro_Rs_df, function(x) round(x, 10))
confint_pro_Rs_df[] <- lapply(confint_pro_Rs_df, function(x) round(x, 10))
# 添加变量列（来自行名）
summary_short_Rt_df$Variable <- rownames(summary_short_Rt_df)
confint_short_Rt_df$Variable <- rownames(confint_short_Rt_df)
summary_short_Rs_df$Variable <- rownames(summary_short_Rs_df)
confint_short_Rs_df$Variable <- rownames(confint_short_Rs_df)
summary_pro_Rt_df$Variable <- rownames(summary_pro_Rt_df)
confint_pro_Rt_df$Variable <- rownames(confint_pro_Rt_df)
summary_pro_Rs_df$Variable <- rownames(summary_pro_Rs_df)
confint_pro_Rs_df$Variable <- rownames(confint_pro_Rs_df)
# 创建 Word 文档
doc <- read_docx()

# 插入模型结果
doc <- doc %>%
  body_add_par("稳健回归模型 (Resistance )-short 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_short_Rt_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_short_Rt_df)) %>%
  body_add_par("稳健回归模型 (Resilience)-short 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_short_Rs_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_short_Rs_df)) %>%
  body_add_par("稳健回归模型 (Resistance )-prolonged 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_pro_Rt_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_pro_Rt_df)) %>%
  body_add_par("稳健回归模型 (Resilience)-prolonged 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_pro_Rs_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_pro_Rs_df))

# 保存文档
print(doc, target = "Regression_Result_interactive_NEW.docx")









short_lowDEM <- subset(data_short, DEM <= 1000)
short_highDEM <- subset(data_short, DEM > 1000)

# ---------- ALL数据Potential mechanisms causing the shdi effect  ----------
fit_CEC <- lmrob(CEC ~ shdi + DI + TMP + PET + CTI + FCC, 
                 data = short_highDEM)
cat("稳健回归模型 (CEC) 结果：\n")
print(summary(fit_CEC))
fit_AWC <- lmrob(AWC ~ shdi + DI + TMP + PET + CTI + FCC, 
                 data = short_highDEM)
cat("稳健回归模型 (AWC) 结果：\n")
print(summary(fit_AWC))
fit_SLA <- lmrob(SLA ~ shdi + DI + TMP + PET + CTI + FCC, 
                 data = short_highDEM)
cat("稳健回归模型 (SLA) 结果：\n")
print(summary(fit_SLA))
fit_WD <- lmrob(WD ~ shdi + DI + TMP + PET + CTI + FCC, 
                data = short_highDEM)
cat("稳健回归模型 (WD) 结果：\n")
print(summary(fit_WD))
fit_RIC <- lmrob(RIC ~ shdi + DI + TMP + PET + CTI + FCC, 
                 data = short_highDEM)
cat("稳健回归模型 (RIC) 结果：\n")
print(summary(fit_RIC))

# 提取 summary 和 confint
summary_fit_CEC_df <- as.data.frame(summary(fit_CEC)$coefficients)
confint_fit_CEC_df <- as.data.frame(confint(fit_CEC, level = 0.95))
summary_fit_AWC_df <- as.data.frame(summary(fit_AWC)$coefficients)
confint_fit_AWC_df <- as.data.frame(confint(fit_AWC, level = 0.95))
summary_fit_SLA_df <- as.data.frame(summary(fit_SLA)$coefficients)
confint_fit_SLA_df <- as.data.frame(confint(fit_SLA, level = 0.95))
summary_fit_WD_df <- as.data.frame(summary(fit_WD)$coefficients)
confint_fit_WD_df <- as.data.frame(confint(fit_WD, level = 0.95))
summary_fit_RIC_df <- as.data.frame(summary(fit_RIC)$coefficients)
confint_fit_RIC_df <- as.data.frame(confint(fit_RIC, level = 0.95))

# 保留 10 位小数
summary_fit_CEC_df[] <- lapply(summary_fit_CEC_df, function(x) round(x, 10))
confint_fit_CEC_df[] <- lapply(confint_fit_CEC_df, function(x) round(x, 10))
summary_fit_AWC_df[] <- lapply(summary_fit_AWC_df, function(x) round(x, 10))
confint_fit_AWC_df[] <- lapply(confint_fit_AWC_df, function(x) round(x, 10))
summary_fit_SLA_df[] <- lapply(summary_fit_SLA_df, function(x) round(x, 10))
confint_fit_SLA_df[] <- lapply(confint_fit_SLA_df, function(x) round(x, 10))
summary_fit_WD_df[] <- lapply(summary_fit_WD_df, function(x) round(x, 10))
confint_fit_WD_df[] <- lapply(confint_fit_WD_df, function(x) round(x, 10))
summary_fit_RIC_df[] <- lapply(summary_fit_RIC_df, function(x) round(x, 10))
confint_fit_RIC_df[] <- lapply(confint_fit_RIC_df, function(x) round(x, 10))

# 添加变量列（来自行名）
summary_fit_CEC_df$Variable <- rownames(summary_fit_CEC_df)
confint_fit_CEC_df$Variable <- rownames(confint_fit_CEC_df)
summary_fit_AWC_df$Variable <- rownames(summary_fit_AWC_df)
confint_fit_AWC_df$Variable <- rownames(confint_fit_AWC_df)
summary_fit_SLA_df$Variable <- rownames(summary_fit_SLA_df)
confint_fit_SLA_df$Variable <- rownames(confint_fit_SLA_df)
summary_fit_WD_df$Variable <- rownames(summary_fit_WD_df)
confint_fit_WD_df$Variable <- rownames(confint_fit_WD_df)
summary_fit_RIC_df$Variable <- rownames(summary_fit_RIC_df)
confint_fit_RIC_df$Variable <- rownames(confint_fit_RIC_df)

# 创建 Word 文档
doc <- read_docx()

# 插入模型结果
doc <- doc %>%
  body_add_par("稳健回归模型 SHDI-cec (Short) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_fit_CEC_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_fit_CEC_df)) %>%
  body_add_par("稳健回归模型 (SHDI-AWC (Short) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_fit_AWC_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_fit_AWC_df)) %>%
  body_add_par("稳健回归模型 SHDI-SLA (Short) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_fit_SLA_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_fit_SLA_df)) %>%
  body_add_par("稳健回归模型 SHDI-WD (Short) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_fit_WD_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_fit_WD_df)) %>%
  body_add_par("稳健回归模型 SHDI-RIC (Short) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_fit_RIC_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_fit_RIC_df))


# 保存文档
print(doc, target = "D:/SHDI-RWI(SPEI)/Statistical Analysis/Pathway1000_Short_RIC.docx")



long_lowDEM <- subset(data_prolonged, DEM <= 1000)
long_highDEM <- subset(data_prolonged, DEM > 1000)

# ---------- ALL数据Potential mechanisms causing the shdi effect  ----------
fit_CEC <- lmrob(CEC ~ shdi + DI + TMP + PET + CTI + FCC, 
                 data = long_highDEM)
cat("稳健回归模型 (CEC) 结果：\n")
print(summary(fit_CEC))
fit_AWC <- lmrob(AWC ~ shdi + DI + TMP + PET + CTI + FCC, 
                 data = long_highDEM)
cat("稳健回归模型 (AWC) 结果：\n")
print(summary(fit_AWC))
fit_SLA <- lmrob(SLA ~ shdi + DI + TMP + PET + CTI + FCC, 
                 data = long_highDEM)
cat("稳健回归模型 (SLA) 结果：\n")
print(summary(fit_SLA))
fit_WD <- lmrob(WD ~ shdi + DI + TMP + PET + CTI + FCC, 
                data = long_highDEM)
cat("稳健回归模型 (WD) 结果：\n")
print(summary(fit_WD))
fit_RIC <- lmrob(RIC ~ shdi + DI + TMP + PET + CTI + FCC, 
                data = long_highDEM)
cat("稳健回归模型 (RIC) 结果：\n")
print(summary(fit_RIC))

# 提取 summary 和 confint
summary_fit_CEC_df <- as.data.frame(summary(fit_CEC)$coefficients)
confint_fit_CEC_df <- as.data.frame(confint(fit_CEC, level = 0.95))
summary_fit_AWC_df <- as.data.frame(summary(fit_AWC)$coefficients)
confint_fit_AWC_df <- as.data.frame(confint(fit_AWC, level = 0.95))
summary_fit_SLA_df <- as.data.frame(summary(fit_SLA)$coefficients)
confint_fit_SLA_df <- as.data.frame(confint(fit_SLA, level = 0.95))
summary_fit_WD_df <- as.data.frame(summary(fit_WD)$coefficients)
confint_fit_WD_df <- as.data.frame(confint(fit_WD, level = 0.95))
summary_fit_RIC_df <- as.data.frame(summary(fit_RIC)$coefficients)
confint_fit_RIC_df <- as.data.frame(confint(fit_RIC, level = 0.95))

# 保留 10 位小数
summary_fit_CEC_df[] <- lapply(summary_fit_CEC_df, function(x) round(x, 10))
confint_fit_CEC_df[] <- lapply(confint_fit_CEC_df, function(x) round(x, 10))
summary_fit_AWC_df[] <- lapply(summary_fit_AWC_df, function(x) round(x, 10))
confint_fit_AWC_df[] <- lapply(confint_fit_AWC_df, function(x) round(x, 10))
summary_fit_SLA_df[] <- lapply(summary_fit_SLA_df, function(x) round(x, 10))
confint_fit_SLA_df[] <- lapply(confint_fit_SLA_df, function(x) round(x, 10))
summary_fit_WD_df[] <- lapply(summary_fit_WD_df, function(x) round(x, 10))
confint_fit_WD_df[] <- lapply(confint_fit_WD_df, function(x) round(x, 10))
summary_fit_RIC_df[] <- lapply(summary_fit_RIC_df, function(x) round(x, 10))
confint_fit_RIC_df[] <- lapply(confint_fit_RIC_df, function(x) round(x, 10))

# 添加变量列（来自行名）
summary_fit_CEC_df$Variable <- rownames(summary_fit_CEC_df)
confint_fit_CEC_df$Variable <- rownames(confint_fit_CEC_df)
summary_fit_AWC_df$Variable <- rownames(summary_fit_AWC_df)
confint_fit_AWC_df$Variable <- rownames(confint_fit_AWC_df)
summary_fit_SLA_df$Variable <- rownames(summary_fit_SLA_df)
confint_fit_SLA_df$Variable <- rownames(confint_fit_SLA_df)
summary_fit_WD_df$Variable <- rownames(summary_fit_WD_df)
confint_fit_WD_df$Variable <- rownames(confint_fit_WD_df)
summary_fit_RIC_df$Variable <- rownames(summary_fit_RIC_df)
confint_fit_RIC_df$Variable <- rownames(confint_fit_RIC_df)

# 创建 Word 文档
doc <- read_docx()

# 插入模型结果
doc <- doc %>%
  body_add_par("稳健回归模型 SHDI-cec (Prolonged) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_fit_CEC_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_fit_CEC_df)) %>%
  body_add_par("稳健回归模型 (SHDI-AWC (Prolonged) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_fit_AWC_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_fit_AWC_df)) %>%
  body_add_par("稳健回归模型 SHDI-SLA (Prolonged) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_fit_SLA_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_fit_SLA_df)) %>%
  body_add_par("稳健回归模型 SHDI-WD (Prolonged) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_fit_WD_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_fit_WD_df)) %>%
  body_add_par("稳健回归模型 SHDI-RIC (Prolonged) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_fit_RIC_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_fit_RIC_df))


# 保存文档
print(doc, target = "D:/SHDI-RWI(SPEI)/Statistical Analysis/Pathway1000_Prolonged_RIC.docx")


#---------------中介变量

fit_Short_Rt <- lmrob(log(Rt) ~ shdi + DI + TMP + CTI + FCC + CEC + AWC + SLA + WD + RIC, 
                      data = short_highDEM)
cat("稳健回归模型 (WD) 结果：\n")
print(summary(fit_Short_Rt))
fit_Short_Rs <- lmrob(log(Rs_pnas) ~ shdi + DI + TMP + CTI + FCC + CEC + AWC + SLA + WD + RIC, 
                      data = short_highDEM)
cat("稳健回归模型 (WD) 结果：\n")
print(summary(fit_Short_Rs))
fit_Prolonged_Rt <- lmrob(log(Rt) ~ shdi + DI + TMP + CTI + FCC + CEC + AWC + SLA + WD + RIC, 
                          data = long_highDEM)
cat("稳健回归模型 (WD) 结果：\n")
print(summary(fit_Prolonged_Rt))
fit_Prolonged_Rs <- lmrob(log(Rs_pnas) ~ shdi + DI + TMP + CTI + FCC + CEC + AWC + SLA + WD + RIC, 
                          data = long_highDEM)
cat("稳健回归模型 (WD) 结果：\n")
print(summary(fit_Prolonged_Rs))

# 提取 summary 和 confint
summary_Short_Rt_df <- as.data.frame(summary(fit_Short_Rt)$coefficients)
confint_Short_Rt_df <- as.data.frame(confint(fit_Short_Rt, level = 0.95))
summary_Short_Rs_df <- as.data.frame(summary(fit_Short_Rs)$coefficients)
confint_Short_Rs_df <- as.data.frame(confint(fit_Short_Rs, level = 0.95))
summary_Prolonged_Rt_df <- as.data.frame(summary(fit_Prolonged_Rt)$coefficients)
confint_Prolonged_Rt_df <- as.data.frame(confint(fit_Prolonged_Rt, level = 0.95))
summary_Prolonged_Rs_df <- as.data.frame(summary(fit_Prolonged_Rs)$coefficients)
confint_Prolonged_Rs_df <- as.data.frame(confint(fit_Prolonged_Rs, level = 0.95))
# 保留 10 位小数
summary_Short_Rt_df[] <- lapply(summary_Short_Rt_df, function(x) round(x, 10))
confint_Short_Rt_df[] <- lapply(confint_Short_Rt_df, function(x) round(x, 10))
summary_Prolonged_Rt_df[] <- lapply(summary_Prolonged_Rt_df, function(x) round(x, 10))
confint_Prolonged_Rt_df[] <- lapply(confint_Prolonged_Rt_df, function(x) round(x, 10))
summary_Short_Rs_df[] <- lapply(summary_Short_Rs_df, function(x) round(x, 10))
confint_Short_Rs_df[] <- lapply(confint_Short_Rs_df, function(x) round(x, 10))
summary_Prolonged_Rs_df[] <- lapply(summary_Prolonged_Rs_df, function(x) round(x, 10))
confint_Prolonged_Rs_df[] <- lapply(confint_Prolonged_Rs_df, function(x) round(x, 10))
# 添加变量列（来自行名）
summary_Short_Rt_df$Variable <- rownames(summary_Short_Rt_df)
confint_Short_Rt_df$Variable <- rownames(confint_Short_Rt_df)
summary_Prolonged_Rt_df$Variable <- rownames(summary_Prolonged_Rt_df)
confint_Prolonged_Rt_df$Variable <- rownames(confint_Prolonged_Rt_df)
summary_Short_Rs_df$Variable <- rownames(summary_Short_Rs_df)
confint_Short_Rs_df$Variable <- rownames(confint_Short_Rs_df)
summary_Prolonged_Rs_df$Variable <- rownames(summary_Prolonged_Rs_df)
confint_Prolonged_Rs_df$Variable <- rownames(confint_Prolonged_Rs_df)
# 创建 Word 文档
doc <- read_docx()

# 插入模型结果
doc <- doc %>%
  body_add_par("稳健回归模型 (Resistance) - Short 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_Short_Rt_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_Short_Rt_df)) %>%
  body_add_par("稳健回归模型 (Resistance) - Prolonged  结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_Prolonged_Rt_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_Prolonged_Rt_df)) %>%
  body_add_par("稳健回归模型 (Resilience) - Short 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_Short_Rs_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_Short_Rs_df)) %>%
  body_add_par("稳健回归模型 ((Resilience) - Prolonged：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_Prolonged_Rs_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_Prolonged_Rs_df))

# 保存文档
print(doc, target = "D:/SHDI-RWI(SPEI)/Statistical Analysis/Regression_Pathway_two.docx")