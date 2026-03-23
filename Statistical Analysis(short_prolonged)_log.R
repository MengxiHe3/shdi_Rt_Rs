library(robustbase)   # 用于稳健回归分析
library(car)           # 用于方差膨胀因子计算
library(piecewiseSEM)  # 结构方程模型
library(openxlsx)
library(dplyr)
library(broom)
library(officer)
library(flextable)


data_all1 <- read.xlsx("D:/SHDI-RWI(SPEI)/Statistical Analysis/Regression_Q25.xlsx", sheet = 1)
data_all1 <- read.xlsx("D:/SHDI-RWI(SPEI)/Statistical Analysis/Regression_Q20.xlsx", sheet = 1)
data_all1 <- read.xlsx("D:/SHDI-RWI(SPEI)/Statistical Analysis/Regression_Q30.xlsx", sheet = 1)


# ---------- 分短期干旱 & 长期干旱 ----------

# 假设 data_short 和 data_long 分别是短期和长期干旱的子数据集
data_short <- subset(data_all1, Drought == 1)  # 短期干旱的样本
data_long <- subset(data_all1, Drought == 2)  # 长期干旱的样本

# 剔除 Resistance 和 Resilience 超出均值±3SD范围的观测值
for(var in c("Rt", "Rs_pnas")){
  m <- mean(data_short[[var]], na.rm = TRUE)
  s <- sd(data_short[[var]], na.rm = TRUE)
  data_short <- data_short[data_short[[var]] >= (m - 3*s) & data_short[[var]] <= (m + 3*s), ]
}
for(var in c("Rt", "Rs_pnas")){
  m <- mean(data_long[[var]], na.rm = TRUE)
  s <- sd(data_long[[var]], na.rm = TRUE)
  data_long <- data_long[data_long[[var]] >= (m - 3*s) & data_long[[var]] <= (m + 3*s), ]
}

# 数据过滤：
# 2. 保留 SHDI >= 0.5 且 Rt >= 1 且 0 < Rs <= 1
data_short <- data_short %>%
  filter(
    shdi      >= 0.5,
    Rt        >= 1,
    Rs_pnas   >  0,
    Rs_pnas   <= 1
  )
data_long <- data_long %>%
  filter(
    shdi      >= 0.5,
    Rt        >= 1,
    Rs_pnas   >  0,
    Rs_pnas   <= 1
  )


# --- 标准化环境和功能性状变量 ---
# 设定需要标准化的变量列，例如第15至24列（根据你的数据调整）
cols_to_scale <- c(10:18)
data_short[, cols_to_scale] <- scale(data_short[, cols_to_scale])
summary(data_short)
data_long[, cols_to_scale] <- scale(data_long[, cols_to_scale])
summary(data_long)


# 多元稳健回归分析
# Short
fit_full_Rt <- lmrob(log(Rt) ~ shdi + DI + TMP + PET + CTI + FCC, 
                     data = data_short)
cat("稳健回归模型 (Resistance) 结果：\n")
print(summary(fit_full_Rt))

fit_full_Rs <- lmrob(log(Rs_pnas) ~ shdi + DI + TMP + PET + CTI + FCC, 
                     data = data_short)
cat("稳健回归模型 (Resilience) 结果：\n")
print(summary(fit_full_Rs))

# 提取 summary 和 confint
summary_Rt_df <- as.data.frame(summary(fit_full_Rt)$coefficients)
confint_Rt_df <- as.data.frame(confint(fit_full_Rt, level = 0.95))
summary_Rs_df <- as.data.frame(summary(fit_full_Rs)$coefficients)
confint_Rs_df <- as.data.frame(confint(fit_full_Rs, level = 0.95))
# 保留 10 位小数
summary_Rt_df[] <- lapply(summary_Rt_df, function(x) round(x, 10))
confint_Rt_df[] <- lapply(confint_Rt_df, function(x) round(x, 10))
summary_Rs_df[] <- lapply(summary_Rs_df, function(x) round(x, 10))
confint_Rs_df[] <- lapply(confint_Rs_df, function(x) round(x, 10))
# 添加变量列（来自行名）
summary_Rt_df$Variable <- rownames(summary_Rt_df)
confint_Rt_df$Variable <- rownames(confint_Rt_df)
summary_Rs_df$Variable <- rownames(summary_Rs_df)
confint_Rs_df$Variable <- rownames(confint_Rs_df)
# 创建 Word 文档
doc <- read_docx()

# 插入模型结果
doc <- doc %>%
  body_add_par("稳健回归模型 (Resistance ) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_Rt_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_Rt_df)) %>%
  body_add_par("稳健回归模型 (Resilience) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_Rs_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_Rs_df))

# 保存文档
print(doc, target = "Regression_Result_Short_Q10.docx")

# ---------- 第一类序贯回归模型 ----------
# 第一步：气候异质性 (shdi) 回归：以其他变量为自变量
fit_sr <- lmrob(
  shdi ~ DI + TMP + PET + CTI + FCC, 
  data = data_short,
  na.action = na.exclude
)
data_short$resid_sr <- residuals(fit_sr)

# 第二步：将物种丰富度的残差与其他变量一起回归抵抗力
fit_seq1_Rt <- lmrob(
  log(Rt) ~ resid_sr + DI + TMP + PET + CTI + FCC, 
  data = data_short
)
cat("第一类序贯回归模型 (Resistance) 结果：\n")
print(summary(fit_seq1_Rt))

# 同样地，对恢复力进行回归
fit_seq1_Rs <- lmrob(
  log(Rs_pnas) ~ resid_sr + DI + TMP + PET + CTI + FCC, 
  data = data_short
)
cat("第一类序贯回归模型 (Resilience) 结果：\n")
print(summary(fit_seq1_Rs))

# 提取 summary 和 confint
summary_Rt_df <- as.data.frame(summary(fit_seq1_Rt)$coefficients)
confint_Rt_df <- as.data.frame(confint(fit_seq1_Rt, level = 0.95))
summary_Rs_df <- as.data.frame(summary(fit_seq1_Rs)$coefficients)
confint_Rs_df <- as.data.frame(confint(fit_seq1_Rs, level = 0.95))
# 保留 10 位小数
summary_Rt_df[] <- lapply(summary_Rt_df, function(x) round(x, 10))
confint_Rt_df[] <- lapply(confint_Rt_df, function(x) round(x, 10))
summary_Rs_df[] <- lapply(summary_Rs_df, function(x) round(x, 10))
confint_Rs_df[] <- lapply(confint_Rs_df, function(x) round(x, 10))
# 添加变量列（来自行名）
summary_Rt_df$Variable <- rownames(summary_Rt_df)
confint_Rt_df$Variable <- rownames(confint_Rt_df)
summary_Rs_df$Variable <- rownames(summary_Rs_df)
confint_Rs_df$Variable <- rownames(confint_Rs_df)
# 创建 Word 文档
doc <- read_docx()

# 插入模型结果
doc <- doc %>%
  body_add_par("第一类序贯回归模型 (Resistance) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_Rt_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_Rt_df)) %>%
  body_add_par("第一类序贯回归模型 (Resilience) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_Rs_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_Rs_df))

# 保存文档
print(doc, target = "Regression_Seq1_Result_Short.docx")


# ---------- 第二类序贯回归模型 ----------
# 第一步：环境和功能性状变量对 Resistance 的回归
fit_env_Rt <- lmrob(
  log(Rt) ~  DI + TMP + PET + CTI + FCC, 
  data = data_short,
  na.action = na.exclude
)
resid_Rt <- residuals(fit_env_Rt)
# 第二步：用物种丰富度预测 Resistance 的残差
fit_seq2_Rt <- lmrob(
  resid_Rt ~ shdi, 
  data = data_short
)
cat("第二类序贯回归模型 (Resistance) 结果：\n")
print(summary(fit_seq2_Rt))

# 同样对 Resilience 进行
fit_env_Rs <- lmrob(
  log(Rs_pnas) ~  DI + TMP + PET + CTI + FCC, 
  data = data_short,
  na.action = na.exclude
)
resid_Rs <- residuals(fit_env_Rs)
fit_seq2_Rs <- lmrob(
  resid_Rs ~ shdi, 
  data = data_short
)
cat("第二类序贯回归模型 (Resilience) 结果：\n")
print(summary(fit_seq2_Rs))

# 提取 summary 和 confint
summary_Rt_df <- as.data.frame(summary(fit_seq2_Rt)$coefficients)
confint_Rt_df <- as.data.frame(confint(fit_seq2_Rt, level = 0.95))
summary_Rs_df <- as.data.frame(summary(fit_seq2_Rs)$coefficients)
confint_Rs_df <- as.data.frame(confint(fit_seq2_Rs, level = 0.95))
# 保留 10 位小数
summary_Rt_df[] <- lapply(summary_Rt_df, function(x) round(x, 10))
confint_Rt_df[] <- lapply(confint_Rt_df, function(x) round(x, 10))
summary_Rs_df[] <- lapply(summary_Rs_df, function(x) round(x, 10))
confint_Rs_df[] <- lapply(confint_Rs_df, function(x) round(x, 10))
# 添加变量列（来自行名）
summary_Rt_df$Variable <- rownames(summary_Rt_df)
confint_Rt_df$Variable <- rownames(confint_Rt_df)
summary_Rs_df$Variable <- rownames(summary_Rs_df)
confint_Rs_df$Variable <- rownames(confint_Rs_df)
# 创建 Word 文档
doc <- read_docx()

# 插入模型结果
doc <- doc %>%
  body_add_par("第二类序贯回归模型 (Resistance) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_Rt_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_Rt_df)) %>%
  body_add_par("第二类序贯回归模型 (Resilience) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_Rs_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_Rs_df))

# 保存文档
print(doc, target = "Regression_Seq2_Result_Short.docx")

# ---------- 短期干旱分组 ----------
# ----------     分组     ----------
# 计算 DEM 的中位数
short_dem_median <- median(data_short$DEM)
short_lowDEM <- subset(data_short, DEM <= 1000)
short_highDEM <- subset(data_short, DEM > 1000)

# Resilience
# 稳健回归 - DEM <= 1000
fit_low_Rt <- lmrob(log(Rt) ~ shdi + DI + TMP + PET + CTI + FCC, 
                    data = short_lowDEM)
cat("稳健回归模型 (Resistance) - DEM <= 中位数 结果：\n")
print(summary(fit_low_Rt))
# 稳健回归 - DEM > 1000
fit_high_Rt <- lmrob(log(Rt) ~ shdi + DI + TMP + PET + CTI + FCC, 
                     data = short_highDEM)
cat("稳健回归模型 (Resistance) - DEM > 中位数 结果：\n")
print(summary(fit_high_Rt))
# Resilience
# 稳健回归 - DEM <= 1000
fit_low_Rs <- lmrob(log(Rs_pnas) ~ shdi + DI + TMP + PET + CTI + FCC, 
                    data = short_lowDEM)
cat("稳健回归模型 (Resilience) - DEM <= 中位数 结果：\n")
print(summary(fit_low_Rs))
# 稳健回归 - DEM > 1000
fit_high_Rs <- lmrob(log(Rs_pnas) ~ shdi + DI + TMP + PET + CTI + FCC, 
                     data = short_highDEM)
cat("稳健回归模型 (Resilience) - DEM > 中位数 结果：\n")
print(summary(fit_high_Rs))

# 提取 summary 和 confint
summary_low_Rt_df <- as.data.frame(summary(fit_low_Rt)$coefficients)
confint_low_Rt_df <- as.data.frame(confint(fit_low_Rt, level = 0.95))
summary_high_Rt_df <- as.data.frame(summary(fit_high_Rt)$coefficients)
confint_high_Rt_df <- as.data.frame(confint(fit_high_Rt, level = 0.95))
summary_low_Rs_df <- as.data.frame(summary(fit_low_Rs)$coefficients)
confint_low_Rs_df <- as.data.frame(confint(fit_low_Rs, level = 0.95))
summary_high_Rs_df <- as.data.frame(summary(fit_high_Rs)$coefficients)
confint_high_Rs_df <- as.data.frame(confint(fit_high_Rs, level = 0.95))
# 保留 10 位小数
summary_low_Rt_df[] <- lapply(summary_low_Rt_df, function(x) round(x, 10))
confint_low_Rt_df[] <- lapply(confint_low_Rt_df, function(x) round(x, 10))
summary_high_Rt_df[] <- lapply(summary_high_Rt_df, function(x) round(x, 10))
confint_high_Rt_df[] <- lapply(confint_high_Rt_df, function(x) round(x, 10))
summary_low_Rs_df[] <- lapply(summary_low_Rs_df, function(x) round(x, 10))
confint_low_Rs_df[] <- lapply(confint_low_Rs_df, function(x) round(x, 10))
summary_high_Rs_df[] <- lapply(summary_high_Rs_df, function(x) round(x, 10))
confint_high_Rs_df[] <- lapply(confint_high_Rs_df, function(x) round(x, 10))
# 添加变量列（来自行名）
summary_low_Rt_df$Variable <- rownames(summary_low_Rt_df)
confint_low_Rt_df$Variable <- rownames(confint_low_Rt_df)
summary_high_Rt_df$Variable <- rownames(summary_high_Rt_df)
confint_high_Rt_df$Variable <- rownames(confint_high_Rt_df)
summary_low_Rs_df$Variable <- rownames(summary_low_Rs_df)
confint_low_Rs_df$Variable <- rownames(confint_low_Rs_df)
summary_high_Rs_df$Variable <- rownames(summary_high_Rs_df)
confint_high_Rs_df$Variable <- rownames(confint_high_Rs_df)
# 创建 Word 文档
doc <- read_docx()

# 插入模型结果
doc <- doc %>%
  body_add_par("稳健回归模型 (Resistance) - DEM <= 1000 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_low_Rt_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_low_Rt_df)) %>%
  body_add_par("稳健回归模型 (Resistance) - DEM > 1000  结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_high_Rt_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_high_Rt_df)) %>%
  body_add_par("稳健回归模型 (Resilience) - DEM <= 1000 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_low_Rs_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_low_Rs_df)) %>%
  body_add_par("稳健回归模型 ((Resilience) - DEM > 1000  结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_high_Rs_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_high_Rs_df))

# 保存文档
print(doc, target = "Regression_Group1000_Result_Short.docx")


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

# 提取 summary 和 confint
summary_fit_CEC_df <- as.data.frame(summary(fit_CEC)$coefficients)
confint_fit_CEC_df <- as.data.frame(confint(fit_CEC, level = 0.95))
summary_fit_AWC_df <- as.data.frame(summary(fit_AWC)$coefficients)
confint_fit_AWC_df <- as.data.frame(confint(fit_AWC, level = 0.95))
summary_fit_SLA_df <- as.data.frame(summary(fit_SLA)$coefficients)
confint_fit_SLA_df <- as.data.frame(confint(fit_SLA, level = 0.95))
summary_fit_WD_df <- as.data.frame(summary(fit_WD)$coefficients)
confint_fit_WD_df <- as.data.frame(confint(fit_WD, level = 0.95))

# 保留 10 位小数
summary_fit_CEC_df[] <- lapply(summary_fit_CEC_df, function(x) round(x, 10))
confint_fit_CEC_df[] <- lapply(confint_fit_CEC_df, function(x) round(x, 10))
summary_fit_AWC_df[] <- lapply(summary_fit_AWC_df, function(x) round(x, 10))
confint_fit_AWC_df[] <- lapply(confint_fit_AWC_df, function(x) round(x, 10))
summary_fit_SLA_df[] <- lapply(summary_fit_SLA_df, function(x) round(x, 10))
confint_fit_SLA_df[] <- lapply(confint_fit_SLA_df, function(x) round(x, 10))
summary_fit_WD_df[] <- lapply(summary_fit_WD_df, function(x) round(x, 10))
confint_fit_WD_df[] <- lapply(confint_fit_WD_df, function(x) round(x, 10))

# 添加变量列（来自行名）
summary_fit_CEC_df$Variable <- rownames(summary_fit_CEC_df)
confint_fit_CEC_df$Variable <- rownames(confint_fit_CEC_df)
summary_fit_AWC_df$Variable <- rownames(summary_fit_AWC_df)
confint_fit_AWC_df$Variable <- rownames(confint_fit_AWC_df)
summary_fit_SLA_df$Variable <- rownames(summary_fit_SLA_df)
confint_fit_SLA_df$Variable <- rownames(confint_fit_SLA_df)
summary_fit_WD_df$Variable <- rownames(summary_fit_WD_df)
confint_fit_WD_df$Variable <- rownames(confint_fit_WD_df)

# 创建 Word 文档
doc <- read_docx()

# 插入模型结果
doc <- doc %>%
  body_add_par("稳健回归模型 SHDI-cec (All) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_fit_CEC_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_fit_CEC_df)) %>%
  body_add_par("稳健回归模型 (SHDI-AWC (All) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_fit_AWC_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_fit_AWC_df)) %>%
  body_add_par("稳健回归模型 SHDI-SLA (All) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_fit_SLA_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_fit_SLA_df)) %>%
  body_add_par("稳健回归模型 SHDI-WD (All) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_fit_WD_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_fit_WD_df))

# 保存文档
print(doc, target = "D:/SHDI-RWI(SPEI)/Statistical Analysis/Pathway1000_Short.docx")



fit_Rt_CEC <- lmrob(log(Rt) ~ shdi + DI + TMP + PET + CTI + FCC + CEC, 
                    data = short_highDEM)
cat("稳健回归模型 (WD) 结果：\n")
print(summary(fit_Rt_CEC))
fit_Rt_AWC <- lmrob(log(Rt) ~ shdi + DI + TMP + PET + CTI + FCC + AWC, 
                    data = short_highDEM)
cat("稳健回归模型 (WD) 结果：\n")
print(summary(fit_Rt_AWC))
fit_Rt_SLA <- lmrob(log(Rt) ~ shdi + DI + TMP + PET + CTI + FCC + SLA, 
                    data = short_highDEM)
cat("稳健回归模型 (WD) 结果：\n")
print(summary(fit_Rt_SLA))
fit_Rt_WD<- lmrob(log(Rt) ~ shdi + DI + TMP + PET + CTI + FCC + WD, 
                  data = short_highDEM)
cat("稳健回归模型 (WD) 结果：\n")
print(summary(fit_Rt_WD))

# 提取 summary 和 confint
summary_fit_CEC_df <- as.data.frame(summary(fit_Rt_CEC)$coefficients)
confint_fit_CEC_df <- as.data.frame(confint(fit_Rt_CEC, level = 0.95))
summary_fit_AWC_df <- as.data.frame(summary(fit_Rt_AWC)$coefficients)
confint_fit_AWC_df <- as.data.frame(confint(fit_Rt_AWC, level = 0.95))
summary_fit_SLA_df <- as.data.frame(summary(fit_Rt_SLA)$coefficients)
confint_fit_SLA_df <- as.data.frame(confint(fit_Rt_SLA, level = 0.95))
summary_fit_WD_df <- as.data.frame(summary(fit_Rt_WD)$coefficients)
confint_fit_WD_df <- as.data.frame(confint(fit_Rt_WD, level = 0.95))

# 保留 10 位小数
summary_fit_CEC_df[] <- lapply(summary_fit_CEC_df, function(x) round(x, 10))
confint_fit_CEC_df[] <- lapply(confint_fit_CEC_df, function(x) round(x, 10))
summary_fit_AWC_df[] <- lapply(summary_fit_AWC_df, function(x) round(x, 10))
confint_fit_AWC_df[] <- lapply(confint_fit_AWC_df, function(x) round(x, 10))
summary_fit_SLA_df[] <- lapply(summary_fit_SLA_df, function(x) round(x, 10))
confint_fit_SLA_df[] <- lapply(confint_fit_SLA_df, function(x) round(x, 10))
summary_fit_WD_df[] <- lapply(summary_fit_WD_df, function(x) round(x, 10))
confint_fit_WD_df[] <- lapply(confint_fit_WD_df, function(x) round(x, 10))

# 添加变量列（来自行名）
summary_fit_CEC_df$Variable <- rownames(summary_fit_CEC_df)
confint_fit_CEC_df$Variable <- rownames(confint_fit_CEC_df)
summary_fit_AWC_df$Variable <- rownames(summary_fit_AWC_df)
confint_fit_AWC_df$Variable <- rownames(confint_fit_AWC_df)
summary_fit_SLA_df$Variable <- rownames(summary_fit_SLA_df)
confint_fit_SLA_df$Variable <- rownames(confint_fit_SLA_df)
summary_fit_WD_df$Variable <- rownames(summary_fit_WD_df)
confint_fit_WD_df$Variable <- rownames(confint_fit_WD_df)

# 创建 Word 文档
doc <- read_docx()

# 插入模型结果
doc <- doc %>%
  body_add_par("稳健回归模型 SHDI-cec (All) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_fit_CEC_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_fit_CEC_df)) %>%
  body_add_par("稳健回归模型 (SHDI-AWC (All) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_fit_AWC_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_fit_AWC_df)) %>%
  body_add_par("稳健回归模型 SHDI-SLA (All) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_fit_SLA_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_fit_SLA_df)) %>%
  body_add_par("稳健回归模型 SHDI-WD (All) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_fit_WD_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_fit_WD_df))

# 保存文档
print(doc, target = "D:/SHDI-RWI(SPEI)/Statistical Analysis/Pathway1000_Short_Rt.docx")




# ------------------------------- Long -------------------------------
# 多元稳健回归分析
fit_full_Rt <- lmrob(log(Rt) ~ shdi + DI + TMP + PET + CTI + FCC, 
                     data = data_long)
cat("稳健回归模型 (Resistance) 结果：\n")
print(summary(fit_full_Rt))

fit_full_Rs <- lmrob(log(Rs_pnas) ~ shdi + DI + TMP + PET + CTI + FCC, 
                     data = data_long)
cat("稳健回归模型 (Resilience) 结果：\n")
print(summary(fit_full_Rs))
# 提取 summary 和 confint
summary_Rt_df <- as.data.frame(summary(fit_full_Rt)$coefficients)
confint_Rt_df <- as.data.frame(confint(fit_full_Rt, level = 0.95))
summary_Rs_df <- as.data.frame(summary(fit_full_Rs)$coefficients)
confint_Rs_df <- as.data.frame(confint(fit_full_Rs, level = 0.95))
# 保留 10 位小数
summary_Rt_df[] <- lapply(summary_Rt_df, function(x) round(x, 10))
confint_Rt_df[] <- lapply(confint_Rt_df, function(x) round(x, 10))
summary_Rs_df[] <- lapply(summary_Rs_df, function(x) round(x, 10))
confint_Rs_df[] <- lapply(confint_Rs_df, function(x) round(x, 10))
# 添加变量列（来自行名）
summary_Rt_df$Variable <- rownames(summary_Rt_df)
confint_Rt_df$Variable <- rownames(confint_Rt_df)
summary_Rs_df$Variable <- rownames(summary_Rs_df)
confint_Rs_df$Variable <- rownames(confint_Rs_df)
# 创建 Word 文档
doc <- read_docx()

# 插入模型结果
doc <- doc %>%
  body_add_par("稳健回归模型 (Resistance ) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_Rt_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_Rt_df)) %>%
  body_add_par("稳健回归模型 (Resilience) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_Rs_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_Rs_df))

# 保存文档
print(doc, target = "Regression_Result_Prolonged_Q10.docx")


# ---------- 第一类序贯回归模型 ----------
# 第一步：气候异质性 (shdi) 回归：以其他变量为自变量
fit_sr <- lmrob(
  shdi ~ DI + TMP + PET + CTI + FCC, 
  data = data_long,
  na.action = na.exclude
)
data_long$resid_sr <- residuals(fit_sr)

# 第二步：将物种丰富度的残差与其他变量一起回归抵抗力
fit_seq1_Rt <- lmrob(
  log(Rt) ~ resid_sr + DI + TMP + PET + CTI + FCC, 
  data = data_long
)
cat("第一类序贯回归模型 (Resistance) 结果：\n")
print(summary(fit_seq1_Rt))

# 同样地，对恢复力进行回归
fit_seq1_Rs <- lmrob(
  log(Rs_pnas) ~ resid_sr + DI + TMP + PET + CTI + FCC, 
  data = data_long
)
cat("第一类序贯回归模型 (Resilience) 结果：\n")
print(summary(fit_seq1_Rs))

# 提取 summary 和 confint
summary_Rt_df <- as.data.frame(summary(fit_seq1_Rt)$coefficients)
confint_Rt_df <- as.data.frame(confint(fit_seq1_Rt, level = 0.95))
summary_Rs_df <- as.data.frame(summary(fit_seq1_Rs)$coefficients)
confint_Rs_df <- as.data.frame(confint(fit_seq1_Rs, level = 0.95))
# 保留 10 位小数
summary_Rt_df[] <- lapply(summary_Rt_df, function(x) round(x, 10))
confint_Rt_df[] <- lapply(confint_Rt_df, function(x) round(x, 10))
summary_Rs_df[] <- lapply(summary_Rs_df, function(x) round(x, 10))
confint_Rs_df[] <- lapply(confint_Rs_df, function(x) round(x, 10))
# 添加变量列（来自行名）
summary_Rt_df$Variable <- rownames(summary_Rt_df)
confint_Rt_df$Variable <- rownames(confint_Rt_df)
summary_Rs_df$Variable <- rownames(summary_Rs_df)
confint_Rs_df$Variable <- rownames(confint_Rs_df)
# 创建 Word 文档
doc <- read_docx()

# 插入模型结果
doc <- doc %>%
  body_add_par("第一类序贯回归模型 (Resistance) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_Rt_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_Rt_df)) %>%
  body_add_par("第一类序贯回归模型 (Resilience) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_Rs_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_Rs_df))

# 保存文档
print(doc, target = "Regression_Seq1_Result_Prolonged.docx")

# ---------- 第二类序贯回归模型 ----------
# 第一步：环境和功能性状变量对 Resistance 的回归
fit_env_Rt <- lmrob(
  log(Rt) ~  DI + TMP + PET + CTI + FCC, 
  data = data_long,
  na.action = na.exclude
)
resid_Rt <- residuals(fit_env_Rt)
# 第二步：用物种丰富度预测 Resistance 的残差
fit_seq2_Rt <- lmrob(
  resid_Rt ~ shdi, 
  data = data_long
)
cat("第二类序贯回归模型 (Resistance) 结果：\n")
print(summary(fit_seq2_Rt))

# 同样对 Resilience 进行
fit_env_Rs <- lmrob(
  log(Rs_pnas) ~  DI + TMP + PET + CTI + FCC, 
  data = data_long,
  na.action = na.exclude
)
resid_Rs <- residuals(fit_env_Rs)
fit_seq2_Rs <- lmrob(
  resid_Rs ~ shdi, 
  data = data_long
)
cat("第二类序贯回归模型 (Resilience) 结果：\n")
print(summary(fit_seq2_Rs))

# 提取 summary 和 confint
summary_Rt_df <- as.data.frame(summary(fit_seq2_Rt)$coefficients)
confint_Rt_df <- as.data.frame(confint(fit_seq2_Rt, level = 0.95))
summary_Rs_df <- as.data.frame(summary(fit_seq2_Rs)$coefficients)
confint_Rs_df <- as.data.frame(confint(fit_seq2_Rs, level = 0.95))
# 保留 10 位小数
summary_Rt_df[] <- lapply(summary_Rt_df, function(x) round(x, 10))
confint_Rt_df[] <- lapply(confint_Rt_df, function(x) round(x, 10))
summary_Rs_df[] <- lapply(summary_Rs_df, function(x) round(x, 10))
confint_Rs_df[] <- lapply(confint_Rs_df, function(x) round(x, 10))
# 添加变量列（来自行名）
summary_Rt_df$Variable <- rownames(summary_Rt_df)
confint_Rt_df$Variable <- rownames(confint_Rt_df)
summary_Rs_df$Variable <- rownames(summary_Rs_df)
confint_Rs_df$Variable <- rownames(confint_Rs_df)
# 创建 Word 文档
doc <- read_docx()

# 插入模型结果
doc <- doc %>%
  body_add_par("第二类序贯回归模型 (Resistance) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_Rt_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_Rt_df)) %>%
  body_add_par("第二类序贯回归模型 (Resilience) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_Rs_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_Rs_df))

# 保存文档
print(doc, target = "Regression_Seq2_Result_Prolonged.docx")


# 长期干旱分组
long_lowDEM <- subset(data_long, DEM <= 1000)
long_highDEM <- subset(data_long, DEM > 1000)


# ---------- 分组 ----------
# 计算 DEM 的中位数
long_dem_median <- median(data_long$DEM)

long_lowDEM <- subset(data_long, DEM <= 1000)
long_highDEM <- subset(data_long, DEM > 1000)

# Resilience
# 稳健回归 - DEM <= 1000
fit_low_Rt <- lmrob(log(Rt) ~ shdi + DI + TMP + PET + CTI + FCC, 
                    data = long_lowDEM)
cat("稳健回归模型 (Resistance) - DEM <= 中位数 结果：\n")
print(summary(fit_low_Rt))
# 稳健回归 - DEM > 1000
fit_high_Rt <- lmrob(log(Rt) ~ shdi + DI + TMP + PET + CTI + FCC, 
                     data = long_highDEM)
cat("稳健回归模型 (Resistance) - DEM > 中位数 结果：\n")
print(summary(fit_high_Rt))
# Resilience
# 稳健回归 - DEM <= 1000
fit_low_Rs <- lmrob(log(Rs_pnas) ~ shdi + DI + TMP + PET + CTI + FCC, 
                    data = long_lowDEM)
cat("稳健回归模型 (Resilience) - DEM <= 中位数 结果：\n")
print(summary(fit_low_Rs))
# 稳健回归 - DEM > 1000
fit_high_Rs <- lmrob(log(Rs_pnas) ~ shdi + DI + TMP + PET + CTI + FCC, 
                     data = long_highDEM)
cat("稳健回归模型 (Resilience) - DEM > 中位数 结果：\n")
print(summary(fit_high_Rs))

# 提取 summary 和 confint
summary_low_Rt_df <- as.data.frame(summary(fit_low_Rt)$coefficients)
confint_low_Rt_df <- as.data.frame(confint(fit_low_Rt, level = 0.95))
summary_high_Rt_df <- as.data.frame(summary(fit_high_Rt)$coefficients)
confint_high_Rt_df <- as.data.frame(confint(fit_high_Rt, level = 0.95))
summary_low_Rs_df <- as.data.frame(summary(fit_low_Rs)$coefficients)
confint_low_Rs_df <- as.data.frame(confint(fit_low_Rs, level = 0.95))
summary_high_Rs_df <- as.data.frame(summary(fit_high_Rs)$coefficients)
confint_high_Rs_df <- as.data.frame(confint(fit_high_Rs, level = 0.95))
# 保留 10 位小数
summary_low_Rt_df[] <- lapply(summary_low_Rt_df, function(x) round(x, 10))
confint_low_Rt_df[] <- lapply(confint_low_Rt_df, function(x) round(x, 10))
summary_high_Rt_df[] <- lapply(summary_high_Rt_df, function(x) round(x, 10))
confint_high_Rt_df[] <- lapply(confint_high_Rt_df, function(x) round(x, 10))
summary_low_Rs_df[] <- lapply(summary_low_Rs_df, function(x) round(x, 10))
confint_low_Rs_df[] <- lapply(confint_low_Rs_df, function(x) round(x, 10))
summary_high_Rs_df[] <- lapply(summary_high_Rs_df, function(x) round(x, 10))
confint_high_Rs_df[] <- lapply(confint_high_Rs_df, function(x) round(x, 10))
# 添加变量列（来自行名）
summary_low_Rt_df$Variable <- rownames(summary_low_Rt_df)
confint_low_Rt_df$Variable <- rownames(confint_low_Rt_df)
summary_high_Rt_df$Variable <- rownames(summary_high_Rt_df)
confint_high_Rt_df$Variable <- rownames(confint_high_Rt_df)
summary_low_Rs_df$Variable <- rownames(summary_low_Rs_df)
confint_low_Rs_df$Variable <- rownames(confint_low_Rs_df)
summary_high_Rs_df$Variable <- rownames(summary_high_Rs_df)
confint_high_Rs_df$Variable <- rownames(confint_high_Rs_df)
# 创建 Word 文档
doc <- read_docx()

# 插入模型结果
doc <- doc %>%
  body_add_par("稳健回归模型 (Resistance) - DEM <= 1000 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_low_Rt_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_low_Rt_df)) %>%
  body_add_par("稳健回归模型 (Resistance) - DEM > 1000  结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_high_Rt_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_high_Rt_df)) %>%
  body_add_par("稳健回归模型 (Resilience) - DEM <= 1000 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_low_Rs_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_low_Rs_df)) %>%
  body_add_par("稳健回归模型 ((Resilience) - DEM > 1000  结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_high_Rs_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_high_Rs_df))

# 保存文档
print(doc, target = "Regression_Group1000_Result_Prolonged.docx")



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

# 提取 summary 和 confint
summary_fit_CEC_df <- as.data.frame(summary(fit_CEC)$coefficients)
confint_fit_CEC_df <- as.data.frame(confint(fit_CEC, level = 0.95))
summary_fit_AWC_df <- as.data.frame(summary(fit_AWC)$coefficients)
confint_fit_AWC_df <- as.data.frame(confint(fit_AWC, level = 0.95))
summary_fit_SLA_df <- as.data.frame(summary(fit_SLA)$coefficients)
confint_fit_SLA_df <- as.data.frame(confint(fit_SLA, level = 0.95))
summary_fit_WD_df <- as.data.frame(summary(fit_WD)$coefficients)
confint_fit_WD_df <- as.data.frame(confint(fit_WD, level = 0.95))

# 保留 10 位小数
summary_fit_CEC_df[] <- lapply(summary_fit_CEC_df, function(x) round(x, 10))
confint_fit_CEC_df[] <- lapply(confint_fit_CEC_df, function(x) round(x, 10))
summary_fit_AWC_df[] <- lapply(summary_fit_AWC_df, function(x) round(x, 10))
confint_fit_AWC_df[] <- lapply(confint_fit_AWC_df, function(x) round(x, 10))
summary_fit_SLA_df[] <- lapply(summary_fit_SLA_df, function(x) round(x, 10))
confint_fit_SLA_df[] <- lapply(confint_fit_SLA_df, function(x) round(x, 10))
summary_fit_WD_df[] <- lapply(summary_fit_WD_df, function(x) round(x, 10))
confint_fit_WD_df[] <- lapply(confint_fit_WD_df, function(x) round(x, 10))

# 添加变量列（来自行名）
summary_fit_CEC_df$Variable <- rownames(summary_fit_CEC_df)
confint_fit_CEC_df$Variable <- rownames(confint_fit_CEC_df)
summary_fit_AWC_df$Variable <- rownames(summary_fit_AWC_df)
confint_fit_AWC_df$Variable <- rownames(confint_fit_AWC_df)
summary_fit_SLA_df$Variable <- rownames(summary_fit_SLA_df)
confint_fit_SLA_df$Variable <- rownames(confint_fit_SLA_df)
summary_fit_WD_df$Variable <- rownames(summary_fit_WD_df)
confint_fit_WD_df$Variable <- rownames(confint_fit_WD_df)

# 创建 Word 文档
doc <- read_docx()

# 插入模型结果
doc <- doc %>%
  body_add_par("稳健回归模型 SHDI-cec (All) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_fit_CEC_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_fit_CEC_df)) %>%
  body_add_par("稳健回归模型 (SHDI-AWC (All) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_fit_AWC_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_fit_AWC_df)) %>%
  body_add_par("稳健回归模型 SHDI-SLA (All) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_fit_SLA_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_fit_SLA_df)) %>%
  body_add_par("稳健回归模型 SHDI-WD (All) 结果：", style = "heading 2") %>%
  body_add_flextable(flextable(summary_fit_WD_df)) %>%
  body_add_par("95% 置信区间：", style = "heading 2") %>%
  body_add_flextable(flextable(confint_fit_WD_df))

# 保存文档
print(doc, target = "D:/SHDI-RWI(SPEI)/Statistical Analysis/Pathway1000_Prolonged.docx")


#---------------中介变量

fit_Short_Rt <- lmrob(log(Rt) ~ shdi + DI + TMP + CTI + FCC + CEC + AWC + SLA + WD, 
                      data = short_highDEM)
cat("稳健回归模型 (WD) 结果：\n")
print(summary(fit_Short_Rt))
fit_Short_Rs <- lmrob(log(Rs_pnas) ~ shdi + DI + TMP + CTI + FCC + CEC + AWC + SLA + WD, 
                      data = short_highDEM)
cat("稳健回归模型 (WD) 结果：\n")
print(summary(fit_Short_Rs))
fit_Prolonged_Rt <- lmrob(log(Rt) ~ shdi + DI + TMP + CTI + FCC + CEC + AWC + SLA + WD, 
                          data = long_highDEM)
cat("稳健回归模型 (WD) 结果：\n")
print(summary(fit_Prolonged_Rt))
fit_Prolonged_Rs <- lmrob(log(Rs_pnas) ~ shdi + DI + TMP + CTI + FCC + CEC + AWC + SLA + WD, 
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



#----------------------------------------------------------
#--------------------------变量R2--------------------------
#----------------------------------------------------------

# 变量分类（已修正语法错误）
var_groups <- list(
  SHDI = c("shdi"),
  DI = c("DI"),
  Climate = c("TMP", "PET"),
  CTI = c("CTI"),
  FCC = c("FCC")
)

# 创建空数据框保存结果
r2_results <- data.frame(
  Category = character(),
  Response = character(),
  R2 = numeric(),
  stringsAsFactors = FALSE
)

# 遍历每类变量（已修正逻辑错误）
for (group_name in names(var_groups)) {
  predictors <- var_groups[[group_name]]
  
  # 为不同响应变量创建公式
  formula_Rt <- as.formula(paste0("log(Rt) ~ ", paste(predictors, collapse = " + ")))
  formula_Rs <- as.formula(paste0("log(Rs_pnas) ~ ", paste(predictors, collapse = " + ")))
  
  # 稳健回归分析（每个数据集使用正确的公式）
  tryCatch({
    # 短期数据集分析
    fit_short_Rt <- lmrob(formula_Rt, data = data_short)
    fit_short_Rs <- lmrob(formula_Rs, data = data_short)
    
    # 长期数据集分析（假设data_long中存在相同变量）
    fit_prolonged_Rt <- lmrob(formula_Rt, data = data_long)  # 使用相同的公式结构
    fit_prolonged_Rs <- lmrob(formula_Rs, data = data_long)  # 使用相同的公式结构
    
    # 提取 R² 值
    r2_results <- rbind(
      r2_results,
      data.frame(Category = group_name, Response = "Short_Resistance", R2 = summary(fit_short_Rt)$r.squared),
      data.frame(Category = group_name, Response = "Short_Resilience", R2 = summary(fit_short_Rs)$r.squared),
      data.frame(Category = group_name, Response = "Prolonged_Resistance", R2 = summary(fit_prolonged_Rt)$r.squared),
      data.frame(Category = group_name, Response = "Prolonged_Resilience", R2 = summary(fit_prolonged_Rs)$r.squared)    
    )
  }, error = function(e) {
    message(sprintf("处理 %s 时出错: %s", group_name, e$message))
  })
}

# 显示结果
print(r2_results)

# 输出到 Excel（已更新路径分隔符为平台无关格式）
dir_path <- "D:/SHDI-RWI(SPEI)/Statistical Analysis"
if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)

output_file <- file.path(dir_path, "R2_by_variable2.xlsx")
write.xlsx(r2_results, output_file)



# ========== 定义函数：预测值与置信区间（原始尺度） ==========
#==============================================================
# =========================拟合值绘图==========================
#==============================================================
# 辅助函数：计算拟合值和置信区间（转为原始尺度）
get_prediction_df <- function(model, data, name) {
  shdi_seq <- seq(min(data$shdi, na.rm = TRUE), max(data$shdi, na.rm = TRUE), length.out = 100)
  new_data <- data.frame(
    shdi = shdi_seq,
    DI   = mean(data$DI, na.rm = TRUE),
    TMP  = mean(data$TMP, na.rm = TRUE),
    PET  = mean(data$PET, na.rm = TRUE),
    CTI  = mean(data$CTI, na.rm = TRUE),
    FCC  = mean(data$FCC, na.rm = TRUE)
  )
  
  # 预测并转为 data.frame
  pred <- predict(model, newdata = new_data, interval = "confidence", level = 0.95)
  result <- data.frame(
    shdi = new_data$shdi,
    fit = exp(pred[, "fit"]),
    lwr = exp(pred[, "lwr"]),
    upr = exp(pred[, "upr"])
  )
  
  # 按唯一 shdi 分组并取平均（或第一条）
  result_unique <- aggregate(. ~ shdi, data = result, FUN = mean)
  
  return(result_unique)
}


# ========== 模型拟合 ==========
fit_full_Rt_short <- lmrob(log(Rt) ~ shdi + DI + TMP + PET + CTI + FCC, data = data_short)
fit_full_Rs_short <- lmrob(log(Rs_pnas) ~ shdi + DI + TMP + PET + CTI + FCC, data = data_short)
fit_full_Rt_long  <- lmrob(log(Rt) ~ shdi + DI + TMP + PET + CTI + FCC, data = data_long)
fit_full_Rs_long  <- lmrob(log(Rs_pnas) ~ shdi + DI + TMP + PET + CTI + FCC, data = data_long)

# ========== 构造数据框 ==========
df3 <- get_prediction_df(fit_full_Rt_short, data_short, "Rt_short")
df4 <- get_prediction_df(fit_full_Rs_short, data_short, "Rs_short")
df5 <- get_prediction_df(fit_full_Rt_long, data_long, "Rt_long")
df6 <- get_prediction_df(fit_full_Rs_long, data_long, "Rs_long")

# ========== 写入 Excel 多工作表 ==========
write.xlsx(
  list(
    "Rt_short" = df3,
    "Rs_short" = df4,
    "Rt_long" = df5,
    "Rs_long" = df6
  ),
  file = "SHDI_Prediction_Interval_ln_withoutIntere.xlsx",
  rowNames = FALSE
)


#------------------------------
# 定义标准化系数计算函数
#------------------------------
# 加载必要的包
library(robustbase)
library(officer)
library(flextable)

# ========== 1. 定义标准化系数计算函数 ==========
calculate_standardized_coef <- function(model, data) {
  # 提取原始系数（去除截距）
  raw_coef <- coef(model)[-1]
  
  # 获取置信区间（去除截距）
  ci_raw <- confint(model, level = 0.95)[-1, , drop = FALSE]
  
  # 获取因变量名称（处理log转换）
  y_var <- all.vars(formula(model))[1]
  if (grepl("log\\(", y_var)) {
    y_values <- log(data[[sub("\\)$", "", sub("^log\\(", "", y_var))]])
  } else {
    y_values <- data[[y_var]]
  }
  
  # 计算因变量标准差
  sd_y <- sd(y_values, na.rm = TRUE)
  
  # 计算自变量标准差
  x_vars <- names(raw_coef)
  sd_x <- sapply(x_vars, function(var) {
    if (var %in% colnames(data)) {
      sd(data[[var]], na.rm = TRUE)
    } else if (grepl(":", var)) { # 处理交互项
      vars <- unlist(strsplit(var, ":"))
      sd(apply(data[, vars], 1, prod), na.rm = TRUE)
    } else {
      NA_real_
    }
  })
  
  # 计算标准化系数
  beta_coef <- signif(raw_coef * (sd_x / sd_y), digits = 10)
  
  # 计算标准化置信区间
  beta_lower <- signif(ci_raw[, 1] * (sd_x / sd_y), digits = 10)
  beta_upper <- signif(ci_raw[, 2] * (sd_x / sd_y), digits = 10)
  
  # 获取p值
  p_values <- summary(model)$coefficients[-1, 4]
  
  # 构建结果数据框
  data.frame(
    Variable = x_vars,
    Raw_Coefficient = round(raw_coef, 10),
    Standardized_Beta = round(beta_coef, 10),
    Std_Lower_CI = round(beta_lower, 10),
    Std_Upper_CI = round(beta_upper, 10),
    Significance = round(p_values, 3),
    stringsAsFactors = FALSE
  )
}

# ========== 2. 计算所有模型的标准化系数 ==========
# 假设已拟合以下模型：
fit_short_Rt <- lmrob(log(Rt) ~ shdi + DI + TMP + PET + CTI + FCC, data = data_short)
fit_short_Rs <- lmrob(log(Rs_pnas) ~ shdi + DI + TMP + PET + CTI + FCC, data = data_short)
fit_prolonged_Rt <- lmrob(log(Rt) ~ shdi + DI + TMP + PET + CTI + FCC, data = data_long)
fit_prolonged_Rs <- lmrob(log(Rs_pnas) ~ shdi + DI + TMP + PET + CTI + FCC, data = data_long)

# 计算各模型的标准化系数
results_short_Rt <- calculate_standardized_coef(fit_short_Rt, data_short)
results_short_Rs <- calculate_standardized_coef(fit_short_Rs, data_short)
results_pro_Rt <- calculate_standardized_coef(fit_prolonged_Rt, data_long)
results_pro_Rs <- calculate_standardized_coef(fit_prolonged_Rs, data_long)

# 添加模型标识列
results_short_Rt$Model <- "Short-Term Resistance"
results_short_Rs$Model <- "Short-Term Resilience"
results_pro_Rt$Model <- "Prolonged Resistance"
results_pro_Rs$Model <- "Prolonged Resilience"

# 合并所有结果
all_results <- rbind(
  results_short_Rt,
  results_short_Rs,
  results_pro_Rt,
  results_pro_Rs
)

# ========== 3. 创建专业Word文档 ==========
# 初始化Word文档
doc <- read_docx()

# 添加主标题
doc <- doc %>%
  body_add_par("STANDARDIZED REGRESSION COEFFICIENTS", style = "heading 1") %>%
  body_add_par("Robust MM-Estimator Results for Groundwater Response Models",
               style = "heading 2") %>%
  body_add_par(" ", style = "Normal") # 空行

# 按模型类型分节输出表格
model_names <- c("Short-Term Resistance", "Short-Term Resilience",
                 "Prolonged Resistance", "Prolonged Resilience")

for (model_name in model_names) {
  # 提取当前模型结果
  model_data <- all_results[all_results$Model == model_name, ]
  
  # 创建三线表（排除Model列）
  ft <- flextable(model_data[, !names(model_data) %in% "Model"]) %>%
    set_caption(paste("Model:", model_name)) %>%
    theme_booktabs() %>% # 专业三线表格式
    align(align = "center", part = "all") %>%
    fontsize(size = 10, part = "all") %>%
    set_header_labels(
      Variable = "Predictor",
      Raw_Coefficient = "Raw Coef.",
      Standardized_Beta = "Std. Beta",
      Std_Lower_CI = "Lower 95% CI",
      Std_Upper_CI = "Upper 95% CI",
      Significance = "p-value"
    ) %>%
    bg(j = "Significance", bg = function(x) ifelse(x < 0.05, "#FFEEEE", "white")) %>%
    autofit()
  
  # 添加表格到文档
  doc <- doc %>%
    body_add_par(model_name, style = "heading 3") %>%
    body_add_flextable(ft) %>%
    body_add_par(" ", style = "Normal") # 空行分隔
}

# ========== 4. 添加方法说明 ==========
doc <- doc %>%
  body_add_par("Methodological Notes", style = "heading 2") %>%
  body_add_par("1. Model Specification:", style = "Normal") %>%
  body_add_par(" - Response variables: log(Rt) for Resistance, log(Rs_pnas) for Resilience",
               style = "Normal") %>%
  body_add_par(" - Predictors: shdi (biodiversity), DI (drought index), TMP (temperature), PET (potential evapotranspiration), CTI (terrain index), FCC (forest cover)",
               style = "Normal") %>%
  body_add_par("2. Statistical Methods:", style = "Normal") %>%
  body_add_par(" - Robust regression using MM-estimation with Tukey's biweight function",
               style = "Normal") %>%
  body_add_par(" - Standardized coefficients calculated as: β = b × (σ_x / σ_y)",
               style = "Normal") %>%
  body_add_par(" - 95% confidence intervals for standardized coefficients: CI_β = CI_b × (σ_x / σ_y)",
               style = "Normal") %>%
  body_add_par(" - p-values reported to three decimal places; significance threshold at α = 0.05",
               style = "Normal")

# ========== 5. 保存最终文档 ==========
output_file <- "Robust_Regression_Standardized_Coefficients.docx"
print(doc, target = output_file)

# 完成提示
message(paste0("Analysis complete. Word document saved to:\n",
               normalizePath(output_file)))



# 模型诊断
cat("Variance Inflation Factors:\n")
print(sqrt(vif(fit_full)))