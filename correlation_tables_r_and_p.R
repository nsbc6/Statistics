library(dplyr)
library(DataExplorer)
library(tidyverse)
library(tibble)

where_save <- "~/Diabetes probi/Ana_complete/Ana_complete/ANÁLISIS_DEFINITIVO_FACS_NOV2021/RESELECCIÓN_TR1/PROBI/NORM_BY_BLOOD/"

data_r <- 
  read.xlsx("~/Diabetes probi/Ana_complete/Ana_complete/ANÁLISIS_DEFINITIVO_FACS_NOV2021/RESELECCIÓN_TR1/PROBI/NORM_BY_BLOOD/corr_table_Probi_noIMP_nodiet_noTR1_N33_spearman.xlsx",
            sheet = 1)
#data_r <- data_r[, c(29, (1:ncol(data_r))[-29])]
data_p <- 
  read.xlsx("~/Diabetes probi/Ana_complete/Ana_complete/ANÁLISIS_DEFINITIVO_FACS_NOV2021/RESELECCIÓN_TR1/PROBI/NORM_BY_BLOOD/corr_table_Probi_noIMP_nodiet_noTR1_N33_spearman.xlsx",
            sheet = 2)
#data_p <- data_p[, c(29, (1:ncol(data_p))[-29])]

name <- "prep_to_paper_corr_probi_noIMP_nodiet_noTR1"

df_corrs <- as.data.frame(matrix(ncol = 1 + nrow(data_p)*2, nrow = nrow(data_p) + 1))
colnames(df_corrs)[1] <- "Variables"
colnames(df_corrs)[seq(2,ncol(df_corrs),by = 2)] <- data_p$Variables
colnames(df_corrs)[seq(3,ncol(df_corrs),by = 2)] <- data_p$Variables

 
df_corrs[1,] <- c("Numbers", rep(c("r", "p"), nrow(data_p)))
df_corrs[2:nrow(df_corrs),1] <- data_p$Variables

vuelta_col <- 2
for ( val in 2:ncol(data_p)){
  df_corrs[2:nrow(df_corrs),vuelta_col] <- data_r[,val]
  df_corrs[2:nrow(df_corrs),vuelta_col+1] <- data_p[,val]
  vuelta_col <- vuelta_col + 2
}

df_corrs[, -1] <- as.numeric(apply(df_corrs[, -1],2, as.numeric))
#df_corrs[1,] <- c("Numbers", rep(c("r", "p"), nrow(data_p)))


write.xlsx(df_corrs, paste0(where_save, name, ".xlsx"))
