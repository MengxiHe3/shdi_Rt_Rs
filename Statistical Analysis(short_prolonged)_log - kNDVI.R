library(robustbase)   # 用于稳健回归分析
library(car)           # 用于方差膨胀因子计算
library(piecewiseSEM)  # 结构方程模型
library(openxlsx)
library(dplyr)
library(broom)
library(officer)
library(flextable)


data_all1 <- read.xlsx("D:/SHDI-RWI(SPEI)/Statistical Analysis/kNDVI/kNDVI_reg25_withRtRs.xlsx", sheet = 1)



# ---------- 分短期干旱 & 长期干旱 ----------

# 假设 data_short 和 data_long 分别是短期和长期干旱的子数据集
data_short <- subset(data_all1, Drought == 1)  # 短期干旱的样本
data_long <- subset(data_all1, Drought == 2)  # 长期干旱的样本

# 剔除 Resistance 和 Resilience 超出均值±3SD范围的观测值
for(var in c("Rt", "Rs")){
  m <- mean(data_short[[var]], na.rm = TRUE)
  s <- sd(data_short[[var]], na.rm = TRUE)
  data_short <- data_short[data_short[[var]] >= (m - 3*s) & data_short[[var]] <= (m + 3*s), ]
}
for(var in c("Rt", "Rs")){
  m <- mean(data_long[[var]], na.rm = TRUE)
  s <- sd(data_long[[var]], na.rm = TRUE)
  data_long <- data_long[data_long[[var]] >= (m - 3*s) & data_long[[var]] <= (m + 3*s), ]
}


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

fit_full_Rs <- lmrob(log(Rs) ~ shdi + DI + TMP + PET + CTI + FCC, 
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
print(doc, target = "Regression_Result_Short_kNDVI.docx")



# ------------------------------- Long -------------------------------
# 多元稳健回归分析
fit_full_Rt <- lmrob(log(Rt) ~ shdi + DI + TMP + PET + CTI + FCC, 
                     data = data_long)
cat("稳健回归模型 (Resistance) 结果：\n")
print(summary(fit_full_Rt))

fit_full_Rs <- lmrob(log(Rs) ~ shdi + DI + TMP + PET + CTI + FCC, 
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
print(doc, target = "Regression_Result_Prolonged_kNDVI.docx")



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
fit_full_Rs_short <- lmrob(log(Rs) ~ shdi + DI + TMP + PET + CTI + FCC, data = data_short)
fit_full_Rt_long  <- lmrob(log(Rt) ~ shdi + DI + TMP + PET + CTI + FCC, data = data_long)
fit_full_Rs_long  <- lmrob(log(Rs) ~ shdi + DI + TMP + PET + CTI + FCC, data = data_long)

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
  file = "SHDI_Prediction_kNDVI.xlsx",
  rowNames = FALSE
)




# 模型诊断
cat("Variance Inflation Factors:\n")
print(sqrt(vif(fit_full_Rt)))