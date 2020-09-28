CY15_FDR005 <- read.table("~/Nakano_RNAseq/PCC/new/base/CY15_FDR005.txt", header=T, sep="\t", row.names = 1)
CY16_FDR005 <- read.table("~/Nakano_RNAseq/PCC/new/base/CY16_FDR005.txt", header=T, sep="\t", row.names = 1)
CY20_FDR005 <- read.table("~/Nakano_RNAseq/PCC/new/base/CY20_FDR005.txt", header=T, sep="\t", row.names = 1)
####timedata_FDR005####
#CY15_time
CY15_1h_FDR005 <- CY15_FDR005[CY15_FDR005$CY15_1h_q_value < 0.05, ]
CY15_3h_FDR005 <- CY15_FDR005[CY15_FDR005$CY15_3h_q_value < 0.05, ]
CY15_12h_FDR005 <- CY15_FDR005[CY15_FDR005$CY15_12h_q_value < 0.05, ]
CY15_24h_FDR005 <- CY15_FDR005[CY15_FDR005$CY15_24h_q_value < 0.05, ]
CY15_48h_FDR005 <- CY15_FDR005[CY15_FDR005$CY15_48h_q_value < 0.05, ]
#CY16_time
CY16_1h_FDR005 <- CY16_FDR005[CY16_FDR005$CY16_1h_q_value < 0.05, ]
CY16_3h_FDR005 <- CY16_FDR005[CY16_FDR005$CY16_3h_q_value < 0.05, ]
CY16_12h_FDR005 <- CY16_FDR005[CY16_FDR005$CY16_12h_q_value < 0.05, ]
CY16_24h_FDR005 <- CY16_FDR005[CY16_FDR005$CY16_24h_q_value < 0.05, ]
CY16_48h_FDR005 <- CY16_FDR005[CY16_FDR005$CY16_48h_q_value < 0.05, ]
#CY20_time
CY20_1h_FDR005 <- CY20_FDR005[CY20_FDR005$CY20_1h_q_value < 0.05, ]
CY20_3h_FDR005 <- CY20_FDR005[CY20_FDR005$CY20_3h_q_value < 0.05, ]
CY20_12h_FDR005 <- CY20_FDR005[CY20_FDR005$CY20_12h_q_value < 0.05, ]
CY20_24h_FDR005 <- CY20_FDR005[CY20_FDR005$CY20_24h_q_value < 0.05, ]
CY20_48h_FDR005 <- CY20_FDR005[CY20_FDR005$CY20_48h_q_value < 0.05, ]
####Venn Diagram####
#CY15
library(gplots)
CY15_data <- list(CY15_1h = rownames(CY15_1h_FDR005), CY15_3h = rownames(CY15_3h_FDR005), CY15_12h = rownames(CY15_12h_FDR005), CY15_24h = rownames(CY15_24h_FDR005), CY15_48h = rownames(CY15_48h_FDR005))
CY15_Venn <- venn(CY15_data)
CY15_Venn <- unlist(attr(CY15_Venn,"intersections"))

temp_all <- strsplit(names(CY15_Venn), "h")  #文字列の分割#overlapしている数が複数だとその数字が表示されてしまうので除きたい。例CY15_1h53　この53を除くために以下の作業
CY15_all <- c()
n <- 1
for(n in 1:length(temp_all)){
  temp <- temp_all[[n]]
  temp_nChar <- nchar(temp)#nchar文字列の長さを調べる
  CY15_all <- c(CY15_all, paste0(paste(temp[temp_nChar >= 4], collapse="h"), "h"))#>=4の理由はCY15_1hの文字列がヒットするのとoverlapしている数がmax3桁であるから。
}
names(CY15_Venn) <- CY15_all
#attribute_file#
n <- 1
T_AGI <- c()
T_attribute <- c()
for(n in 1:length(unique(CY15_all))){
  T_AGI <- c(T_AGI, CY15_Venn[names(CY15_Venn) == unique(CY15_all)[n]])
  temp <- switch(unique(CY15_all)[n],
                 "CY15_1h:CY15_3h" = 5,
                 "CY15_1h:CY15_12h" = 6,
                 "CY15_1h:CY15_24h" = 7,
                 "CY15_1h:CY15_48h" = 8,
                 "CY15_3h:CY15_12h" = 9,
                 "CY15_3h:CY15_24h" = 10,
                 "CY15_3h:CY15_48h" = 11,
                 "CY15_12h:CY15_24h" = 12,
                 "CY15_24h:CY15_48h" = 13,
                 "CY15_1h:CY15_3h:CY15_12h" = 14,
                 "CY15_1h:CY15_3h:CY15_24h" = 15,
                 "CY15_1h:CY15_3h:CY15_48h" = 16,
                 "CY15_1h:CY15_12h:CY15_24h" = 17,
                 "CY15_3h:CY15_12h:CY15_24h" = 18,
                 "CY15_3h:CY15_12h:CY15_48h" = 19,
                 "CY15_3h:CY15_24h:CY15_48h" = 20,
                 "CY15_12h:CY15_24h:CY15_48h" = 21,
                 "CY15_1h:CY15_3h:CY15_12h:CY15_24h" = 22,
                 "CY15_1h:CY15_3h:CY15_12h:CY15_48h" = 23,
                 "CY15_1h:CY15_12h:CY15_24h:CY15_48h" = 24,
                 "CY15_3h:CY15_12h:CY15_24h:CY15_48h" = 25,
                 "CY15_1h:CY15_3h:CY15_12h:CY15_24h:CY15_48h" = 26,
                 "CY15_1h" = 1,
                 "CY15_3h" = 2,
                 "CY15_12h" = 3,
                 "CY15_24h" = 4
                 )
  T_attribute <- c(T_attribute, rep(temp, times=length(CY15_Venn[names(CY15_Venn) == unique(CY15_all)[n]])))
  print(n)
  n <- n+1
}

CY15_FDR005_time <- data.frame(AGI=T_AGI,
                                       attribute=T_attribute)
write.table(CY15_FDR005_time, "~/Nakano_RNAseq/PCC/new/Venn Diagram/CY15_FDR005_time.txt", append=F, quote = F, sep = "\t", row.names = F)

#CY16
CY16_data <- list(CY16_1h = rownames(CY16_1h_FDR005), CY16_3h = rownames(CY16_3h_FDR005), CY16_12h = rownames(CY16_12h_FDR005), CY16_24h = rownames(CY16_24h_FDR005), CY16_48h = rownames(CY16_48h_FDR005))
CY16_Venn <- venn(CY16_data)
CY16_Venn <- unlist(attr(CY16_Venn,"intersections"))
temp_all <- strsplit(names(CY16_Venn), "h")#文字列の分割#overlapしている数が複数だとその数字が表示されてしまうので除きたい。例CY16_1h53　この53を除くために以下の作業
CY16_all <- c()
n <- 1
for(n in 1:length(temp_all)){
  temp <- temp_all[[n]]
  temp_nChar <- nchar(temp)#nchar文字列の長さを調べる
  CY16_all <- c(CY16_all, paste0(paste(temp[temp_nChar >= 4], collapse="h"), "h"))#>=4の理由はCY16_1hの文字列がヒットするのとoverlapしている数がmax3桁であるから。
}
names(CY16_Venn) <- CY16_all
#attribute_file#
n <- 1
T_AGI <- c()
T_attribute <- c()
for(n in 1:length(unique(CY16_all))){
  T_AGI <- c(T_AGI, CY16_Venn[names(CY16_Venn) == unique(CY16_all)[n]])
  temp <- switch(unique(CY16_all)[n],
                 "CY16_1h:CY16_3h" = 5,
                 "CY16_1h:CY16_12h" = 6,
                 "CY16_1h:CY16_24h" = 7,
                 "CY16_1h:CY16_48h" = 8,
                 "CY16_3h:CY16_12h" = 9,
                 "CY16_3h:CY16_24h" = 10,
                 "CY16_3h:CY16_48h" = 11,
                 "CY16_12h:CY16_24h" = 12,
                 "CY16_12h:CY16_48h" = 13,
                 "CY16_24h:CY16_48h" = 14,
                 "CY16_1h:CY16_3h:CY16_12h" = 15,
                 "CY16_1h:CY16_3h:CY16_24h" = 16,
                 "CY16_1h:CY16_3h:CY16_48h" = 17,
                 "CY16_1h:CY16_12h:CY16_24h" = 18,
                 "CY16_1h:CY16_24h:CY16_48h" = 19,
                 "CY16_3h:CY16_12h:CY16_24h" = 20,
                 "CY16_3h:CY16_24h:CY16_48h" = 21,
                 "CY16_12h:CY16_24h:CY16_48h" = 22,
                 "CY16_1h:CY16_3h:CY16_12h:CY16_24h" = 22,
                 "CY16_1h:CY16_12h:CY16_24h:CY16_48h" = 24,
                 "CY16_3h:CY16_12h:CY16_24h:CY16_48h" = 25,
                 "CY16_1h:CY16_3h:CY16_12h:CY16_24h:CY16_48h" = 26,
                 "CY16_1h" = 1,
                 "CY16_3h" = 2,
                 "CY16_12h" = 3,
                 "CY16_24h" = 4)
  T_attribute <- c(T_attribute, rep(temp, times=length(CY16_Venn[names(CY16_Venn) == unique(CY16_all)[n]])))
  print(n)
  n <- n+1
}

CY16_FDR005_time <- data.frame(AGI=T_AGI,
                                       attribute=T_attribute)
write.table(CY16_FDR005_time, "~/Nakano_RNAseq/PCC/new/Venn Diagram/CY16_FDR005_time.txt", append=F, quote = F, sep = "\t", row.names = F)

#CY20
CY20_data <- list(CY20_1h = rownames(CY20_1h_FDR005), CY20_3h = rownames(CY20_3h_FDR005), CY20_12h = rownames(CY20_12h_FDR005), CY20_24h = rownames(CY20_24h_FDR005), CY20_48h = rownames(CY20_48h_FDR005))
CY20_Venn <- venn(CY20_data)
CY20_Venn <- unlist(attr(CY20_Venn,"intersections"))


temp_all <- strsplit(names(CY20_Venn), "h")#文字列の分割#overlapしている数が複数だとその数字が表示されてしまうので除きたい。例CY20_1h53　この53を除くために以下の作業
CY20_all <- c()
n <- 1
for(n in 1:length(temp_all)){
  temp <- temp_all[[n]]
  temp_nChar <- nchar(temp)#nchar文字列の長さを調べる
  CY20_all <- c(CY20_all, paste0(paste(temp[temp_nChar >= 4], collapse="h"), "h"))#>=4の理由はCY20_1hの文字列がヒットするのとoverlapしている数がmax3桁であるから。
}
names(CY20_Venn) <- CY20_all
#attribute_file#
T_AGI <- c()
T_attribute <- c()
for(n in 1:length(unique(CY20_all))){
  T_AGI <- c(T_AGI, CY20_Venn[names(CY20_Venn) == unique(CY20_all)[n]])
  temp <- switch(unique(CY20_all)[n],
                 "CY20_1h:CY20_3h" = 6,
                 "CY20_1h:CY20_12h" = 7,
                 "CY20_1h:CY20_24h" = 8,
                 "CY20_1h:CY20_48h" = 9,
                 "CY20_3h:CY20_12h" = 10,
                 "CY20_3h:CY20_24h" = 11,
                 "CY20_3h:CY20_48h" = 12,
                 "CY20_12h:CY20_24h" = 13,
                 "CY20_12h:CY20_48h" = 14,
                 "CY20_24h:CY20_48h" = 15,
                 "CY20_1h:CY20_3h:CY20_12h" = 16,
                 "CY20_1h:CY20_3h:CY20_24h" = 17,
                 "CY20_1h:CY20_3h:CY20_48h" = 18,
                 "CY20_1h:CY20_12h:CY20_24h" = 19,
                 "CY20_1h:CY20_24h:CY20_48h" = 20,
                 "CY20_3h:CY20_12h:CY20_24h" = 21,
                 "CY20_3h:CY20_12h:CY20_48h" = 22,
                 "CY20_3h:CY20_24h:CY20_48h" = 23,
                 "CY20_12h:CY20_24h:CY20_48h" = 24,
                 "CY20_1h:CY20_3h:CY20_12h:CY20_24h" = 25,
                 "CY20_1h:CY20_3h:CY20_12h:CY20_48h" = 26,
                 "CY20_1h:CY20_3h:CY20_24h:CY20_48h" = 27,
                 "CY20_1h:CY20_12h:CY20_24h:CY20_48h" = 28,
                 "CY20_3h:CY20_12h:CY20_24h:CY20_48h" = 29,
                 "CY20_1h:CY20_3h:CY20_12h:CY20_24h:CY20_48h" = 30,
                 "CY20_1h" = 1,
                 "CY20_3h" = 2,
                 "CY20_12h" = 3,
                 "CY20_24h" = 4,
                 "CY20_48h" = 5)
  T_attribute <- c(T_attribute, rep(temp, times=length(CY20_Venn[names(CY20_Venn) == unique(CY20_all)[n]])))
}

CY20_FDR005_time <- data.frame(AGI=T_AGI,
                                       attribute=T_attribute)
write.table(CY20_FDR005_time, "~/Nakano_RNAseq/PCC/new/Venn Diagram/CY20_FDR005_time.txt", append=F, quote = F, sep = "\t", row.names = F)
