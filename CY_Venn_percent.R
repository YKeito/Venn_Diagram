subcluster1_node <- read.table("~/Nakano_RNAseq/network_analysis/base/Subcluster_Information/subcluster1nodetable.csv", sep = ",", header = T)
subcluster1_node <- subcluster1_node[order(subcluster1_node$Degree, decreasing=T), ]
sum(subcluster1_node$Degree[1:81])/sum(subcluster1_node$Degree)#上位10%でハブ同定
unique(subcluster1_node[1:81, ]$X__mclCluster)#subcluster1のみであった。
subcluster1_hub <- subcluster1_node[1:81, ]$name#hub候補遺伝子のAGI出力
#read
CY151620_FDR005 <- read.table("~/Nakano_RNAseq/network_analysis/base/genes_set/CYall_FDR005.txt", sep = "\t", header = T)
subcluster1_hub <- as.character(unlist(subcluster1_hub))
subcluster1_node <- iterations14[iterations14$X__mclCluster == 1, ]$name
subcluster1_node <- as.character(unlist(subcluster1_node))
#CY151620_common
CY151620_common <- CY151620_FDR005[CY151620_FDR005$CYall_Venn == "CY15:CY16:CY20", ]$AGI
CY151620_common <- as.character(unlist(CY151620_common))
subcluster1_CY151620_common <- intersect(CY151620_common, subcluster1_node)
CY151620 <- length(intersect(subcluster1_hub, subcluster1_CY151620_common))/length(subcluster1_hub)
#CY15:CY16
CY1516_common <- CY151620_FDR005[CY151620_FDR005$CYall_Venn == "CY15:CY16", ]$AGI
CY1516_common <- as.character(unlist(CY1516_common))
subcluster1_CY1516_common <- intersect(CY1516_common, subcluster1_node)
CY1516 <- length(intersect(subcluster1_hub, subcluster1_CY1516_common))/length(subcluster1_hub)
#CY1520
CY1520_common <- CY151620_FDR005[CY151620_FDR005$CYall_Venn == "CY15:CY20", ]$AGI
CY1520_common <- as.character(unlist(CY1520_common))
subcluster1_CY1520_common <- intersect(CY1520_common, subcluster1_node)
CY1520 <- length(intersect(subcluster1_hub, subcluster1_CY1520_common))/length(subcluster1_hub)
#CY1620
aCY1620_common <- CY151620_FDR005[CY151620_FDR005$CYall_Venn == "CY16:CY20", ]$AGI
CY1620_common <- as.character(unlist(CY1620_common))
subcluster1_CY1620_common <- intersect(CY1620_common, subcluster1_node)
CY1620 <- length(intersect(subcluster1_hub, subcluster1_CY1620_common))/length(subcluster1_hub)
#CY15
CY15_common <- CY151620_FDR005[CY151620_FDR005$CYall_Venn == "CY15", ]$AGI
CY15_common <- as.character(unlist(CY15_common))
subcluster1_CY15_common <- intersect(CY15_common, subcluster1_node)
CY15 <- length(intersect(subcluster1_hub, subcluster1_CY15_common))/length(subcluster1_hub)
#CY16
CY16_common <- CY151620_FDR005[CY151620_FDR005$CYall_Venn == "CY16", ]$AGI
CY16_common <- as.character(unlist(CY16_common))
subcluster1_CY16_common <- intersect(CY16_common, subcluster1_node)
CY16 <- length(intersect(subcluster1_hub, subcluster1_CY16_common))/length(subcluster1_hub)
#CY20
CY20_common <- CY151620_FDR005[CY151620_FDR005$CYall_Venn == "CY20", ]$AGI
CY20_common <- as.character(unlist(CY20_common))
subcluster1_CY20_common <- intersect(CY20_common, subcluster1_node)
CY20 <- length(intersect(subcluster1_hub, subcluster1_CY20_common))/length(subcluster1_hub)


test <- data.frame(CY = c("CY15", "CY16", "CY20", "CY1516", "CY1520", "CY1620", "CY151620"),
                   value = c(CY15*100, CY16*100, CY20*100, CY1516*100, CY1520*100, CY1620*100, CY151620*100)
                   )


p <- ggplot(test, aes(x=1, y=value, fill=CY)) + geom_bar(stat="identity")
p <- p + coord_polar(theta='y')
p <- p + guides(fill=guide_legend(override.aes=list(colour=NA)))
p <- p + geom_bar(stat="identity", color='black')
p <- p + theme(axis.ticks=element_blank(),  #縦軸の-を消す 
               axis.title=element_blank(),  #縦軸のラベル消す
               axis.text.y=element_blank()) #縦軸の数値を消す
print(p)