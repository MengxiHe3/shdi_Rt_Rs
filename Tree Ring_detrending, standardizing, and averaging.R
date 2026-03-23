library(dplR)
library(tidyverse)
library(writexl)
library(readxl)

# —— 载入元数据 ——  
meta_df <- read_excel(
  "D:/SHDI-RWI(SPEI)/data/SHDI data/point_with_shdi.xlsx",
  col_types = rep("guess", 12)
) %>% 
  select(
    Collection_Name, Latitude, Longitude,
    Common_Name, Tree_Species_Code, shdi1, shdi2
  ) %>% 
  distinct(Collection_Name, .keep_all = TRUE)

# —— Python 风格的“最长窗口内连续段+向前延伸”函数 ——  
longest_in_window_then_extend <- function(valid_mask, window_mask, min_len=15) {
  n <- length(valid_mask)
  best_start <- NA; best_len <- 0; current_start <- NA
  
  # 1) 在窗口内找所有连续段
  for(i in seq_len(n)) {
    if(valid_mask[i] && window_mask[i]) {
      if(is.na(current_start)) current_start <- i
    } else if(!is.na(current_start)) {
      len_seg <- i - current_start
      if(len_seg >= min_len && len_seg > best_len) {
        best_len   <- len_seg
        best_start <- current_start
      }
      current_start <- NA
    }
  }
  # 尾段检查
  if(!is.na(current_start)) {
    len_seg <- n + 1 - current_start
    if(len_seg >= min_len && len_seg > best_len) {
      best_len   <- len_seg
      best_start <- current_start
    }
  }
  # 如果没找到合格段
  if(is.na(best_start)) return(rep(FALSE, n))
  
  seg_end <- best_start + best_len - 1
  
  # 2) 向前延伸
  ext_start <- best_start
  if(best_start > 1) {
    for(j in (best_start-1):1) {
      if(valid_mask[j]) {
        ext_start <- j
      } else {
        break
      }
    }
  }
  
  # 返回掩码
  mask <- rep(FALSE, n)
  mask[ext_start:seg_end] <- TRUE
  mask
}

# —— 核心处理函数 ——  
process_rwi_hybrid <- function(input_file) {
  collection_id <- tools::file_path_sans_ext(basename(input_file))
  
  # 读表并清哨值
  df <- read_excel(input_file) %>%
    rename(Year = 1) %>%
    mutate(
      Year = as.integer(Year),
      across(-Year, ~ ifelse(.x %in% c(999, -9999, 9999), NA, as.numeric(.x)))
    ) %>%
    arrange(Year) %>%
    distinct(Year, .keep_all = TRUE)
  
  years <- df$Year
  window_mask <- years >= 1960 & years <= 2023
  
  vals <- as.matrix(df %>% select(-Year))
  n <- nrow(vals); p <- ncol(vals)
  
  mask_mat <- matrix(FALSE, nrow=n, ncol=p)
  for(j in seq_len(p)) {
    valid_mask <- !is.na(vals[,j])
    mask_mat[,j] <- longest_in_window_then_extend(valid_mask, window_mask, min_len=15)
  }
  
  masked_vals <- vals
  masked_vals[!mask_mat] <- NA
  
  keep_row <- rowSums(!is.na(masked_vals)) >= 5
  final_vals <- masked_vals[keep_row, , drop=FALSE]
  final_years <- years[keep_row]
  
  if(length(final_years) < 15) return(NULL)
  
  # —— 新增：剔除全为 NA 的列 ——  
  has_data <- colSums(!is.na(final_vals)) > 0
  final_vals <- final_vals[, has_data, drop=FALSE]
  if(ncol(final_vals) == 0) return(NULL)
  
  # 转为 rwl 对象
  df_rwl <- as.data.frame(final_vals)
  rownames(df_rwl) <- final_years
  rwl_obj <- as.rwl(df_rwl)
  
  # dplR 去趋势 & biweight 聚合
  detrended <- detrend(rwl_obj, method="Spline", nyrs=30, f=0.5)
  chrono_df <- chron(detrended, biweight=TRUE, prewhiten=FALSE)
  
  std_rwi <- chrono_df[, "std"]
  out_years <- as.integer(rownames(chrono_df))
  
  tibble(
    Collection_Name = collection_id,
    Year            = out_years,
    Std_RWI         = std_rwi
  ) %>%
    left_join(meta_df, by="Collection_Name") %>%
    select(
      Collection_Name, Latitude, Longitude,
      Common_Name, Tree_Species_Code,
      Year, shdi1, shdi2, Std_RWI
    )
}

# —— 批量处理 ——  
files <- list.files(
  "D:/SHDI-RWI(SPEI)/data/AGE/Tree ring data/1",
  pattern="\\.xlsx$", full.names=TRUE
)

result_list <- map(files, process_rwi_hybrid) %>% compact()
final_df <- bind_rows(result_list) %>%
  filter(Year >= 1960, Year <= 2023) %>%
  arrange(Collection_Name, Year)

# —— 输出 ——  
write_xlsx(final_df, "D:/SHDI-RWI(SPEI)/data/final_output_hybrid.xlsx")
cat("处理完成，共", nrow(final_df), "条记录\n")