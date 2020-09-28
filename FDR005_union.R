#read data
CYall_FDR005 <- rownames(allRNASeq[allRNASeq$CYall_1h_q_value < 0.05 | allRNASeq$CYall_3h_q_value < 0.05 | allRNASeq$CYall_12h_q_value < 0.05 | allRNASeq$CYall_24h_q_value < 0.05 | allRNASeq$CYall_48h_q_value < 0.05, ])
CY16_FDR005 <- rownames(allRNASeq[allRNASeq$CY16_1h_q_value < 0.05 | allRNASeq$CY16_3h_q_value < 0.05 | allRNASeq$CY16_12h_q_value < 0.05 | allRNASeq$CY16_24h_q_value < 0.05 | allRNASeq$CY16_48h_q_value < 0.05, ])
CY20_FDR005 <- rownames(allRNASeq[allRNASeq$CY20_1h_q_value < 0.05 | allRNASeq$CY20_3h_q_value < 0.05 | allRNASeq$CY20_12h_q_value < 0.05 | allRNASeq$CY20_24h_q_value < 0.05 | allRNASeq$CY20_48h_q_value < 0.05, ])
#Venn
library(gplots)
CYall_data <- list(CY15 = CY15_FDR005, CY16 = CY16_FDR005, CY20 = CY20_FDR005)
CYall_Venn <- venn(CYall_data)
CYall_Venn <- unlist(attr(CYall_Venn,"intersections"))

CYall_FDR005 <- data.frame(CYall_Venn)
write.table(CYall_FDR005, file = "~/Nakano_RNAseq/network_analysis/base/genes_set/CYall_FDR005.txt", append=F, quote = F, sep = "\t")