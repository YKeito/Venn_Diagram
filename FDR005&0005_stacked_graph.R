data <- allRNASeq[match(subcluster1_CY151620_common, rownames(allRNASeq)), ]
CY15_1h_FDR005unique_up <- sum(data[data$CY15_1h_q_value < 0.05 & data$CY15_1h_q_value >= 0.005, ]$CY15_1h > 0)
CY15_3h_FDR005unique_up <- sum(data[data$CY15_3h_q_value < 0.05 & data$CY15_3h_q_value >= 0.005, ]$CY15_3h > 0)
CY15_12h_FDR005unique_up <- sum(data[data$CY15_12h_q_value < 0.05 & data$CY15_12h_q_value >= 0.005, ]$CY15_12h > 0)
CY15_24h_FDR005unique_up <- sum(data[data$CY15_24h_q_value < 0.05 & data$CY15_24h_q_value >= 0.005, ]$CY15_24h > 0)
CY15_48h_FDR005unique_up <- sum(data[data$CY15_48h_q_value < 0.05 & data$CY15_48h_q_value >= 0.005, ]$CY15_48h > 0)
CY16_1h_FDR005unique_up <- sum(data[data$CY16_1h_q_value < 0.05 & data$CY16_1h_q_value >= 0.005, ]$CY16_1h > 0)
CY16_3h_FDR005unique_up <- sum(data[data$CY16_3h_q_value < 0.05 & data$CY16_3h_q_value >= 0.005, ]$CY16_3h > 0)
CY16_12h_FDR005unique_up <- sum(data[data$CY16_12h_q_value < 0.05 & data$CY16_12h_q_value >= 0.005, ]$CY16_12h > 0)
CY16_24h_FDR005unique_up <- sum(data[data$CY16_24h_q_value < 0.05 & data$CY16_24h_q_value >= 0.005, ]$CY16_24h > 0)
CY16_48h_FDR005unique_up <- sum(data[data$CY16_48h_q_value < 0.05 & data$CY16_48h_q_value >= 0.005, ]$CY16_48h > 0)
CY20_1h_FDR005unique_up <- sum(data[data$CY20_1h_q_value < 0.05 & data$CY20_1h_q_value >= 0.005, ]$CY20_1h > 0)
CY20_3h_FDR005unique_up <- sum(data[data$CY20_3h_q_value < 0.05 & data$CY20_3h_q_value >= 0.005, ]$CY20_3h > 0)
CY20_12h_FDR005unique_up <- sum(data[data$CY20_12h_q_value < 0.05 & data$CY20_12h_q_value >= 0.005, ]$CY20_12h > 0)
CY20_24h_FDR005unique_up <- sum(data[data$CY20_24h_q_value < 0.05 & data$CY20_24h_q_value >= 0.005, ]$CY20_24h > 0)
CY20_48h_FDR005unique_up <- sum(data[data$CY20_48h_q_value < 0.05 & data$CY20_48h_q_value >= 0.005, ]$CY20_48h > 0)

#down
CY15_1h_FDR005unique_down <- sum(data[data$CY15_1h_q_value < 0.05 & data$CY15_1h_q_value >= 0.005, ]$CY15_1h < 0)
CY15_3h_FDR005unique_down <- sum(data[data$CY15_3h_q_value < 0.05 & data$CY15_3h_q_value >= 0.005, ]$CY15_3h < 0)
CY15_12h_FDR005unique_down <- sum(data[data$CY15_12h_q_value < 0.05 & data$CY15_12h_q_value >= 0.005, ]$CY15_12h < 0)
CY15_24h_FDR005unique_down <- sum(data[data$CY15_24h_q_value < 0.05 & data$CY15_24h_q_value >= 0.005, ]$CY15_24h < 0)
CY15_48h_FDR005unique_down <- sum(data[data$CY15_48h_q_value < 0.05 & data$CY15_48h_q_value >= 0.005, ]$CY15_48h < 0)
CY16_1h_FDR005unique_down <- sum(data[data$CY16_1h_q_value < 0.05 & data$CY16_1h_q_value >= 0.005, ]$CY16_1h < 0)
CY16_3h_FDR005unique_down <- sum(data[data$CY16_3h_q_value < 0.05 & data$CY16_3h_q_value >= 0.005, ]$CY16_3h < 0)
CY16_12h_FDR005unique_down <- sum(data[data$CY16_12h_q_value < 0.05 & data$CY16_12h_q_value >= 0.005, ]$CY16_12h < 0)
CY16_24h_FDR005unique_down <- sum(data[data$CY16_24h_q_value < 0.05 & data$CY16_24h_q_value >= 0.005, ]$CY16_24h < 0)
CY16_48h_FDR005unique_down <- sum(data[data$CY16_48h_q_value < 0.05 & data$CY16_48h_q_value >= 0.005, ]$CY16_48h < 0)
CY20_1h_FDR005unique_down <- sum(data[data$CY20_1h_q_value < 0.05 & data$CY20_1h_q_value >= 0.005, ]$CY20_1h < 0)
CY20_3h_FDR005unique_down <- sum(data[data$CY20_3h_q_value < 0.05 & data$CY20_3h_q_value >= 0.005, ]$CY20_3h < 0)
CY20_12h_FDR005unique_down <- sum(data[data$CY20_12h_q_value < 0.05 & data$CY20_12h_q_value >= 0.005, ]$CY20_12h < 0)
CY20_24h_FDR005unique_down <- sum(data[data$CY20_24h_q_value < 0.05 & data$CY20_24h_q_value >= 0.005, ]$CY20_24h < 0)
CY20_48h_FDR005unique_down <- sum(data[data$CY20_48h_q_value < 0.05 & data$CY20_48h_q_value >= 0.005, ]$CY20_48h < 0)
#########
#CYall FDR005
df <- data.frame(expression_change = rep(c("01up", "02down"), times = 15),
                 Numgenes = c(CY15_1h_FDR005unique_up, CY15_1h_FDR005unique_down,
                              CY15_3h_FDR005unique_up, CY15_3h_FDR005unique_down,
                              CY15_12h_FDR005unique_up, CY15_12h_FDR005unique_down,
                              CY15_24h_FDR005unique_up, CY15_24h_FDR005unique_down,
                              CY15_48h_FDR005unique_up, CY15_48h_FDR005unique_down,
                              CY16_1h_FDR005unique_up, CY16_1h_FDR005unique_down,
                              CY16_3h_FDR005unique_up, CY16_3h_FDR005unique_down,
                              CY16_12h_FDR005unique_up, CY16_12h_FDR005unique_down,
                              CY16_24h_FDR005unique_up, CY16_24h_FDR005unique_down,
                              CY16_48h_FDR005unique_up, CY16_48h_FDR005unique_down,
                              CY20_1h_FDR005unique_up, CY20_1h_FDR005unique_down,
                              CY20_3h_FDR005unique_up, CY20_3h_FDR005unique_down,
                              CY20_12h_FDR005unique_up, CY20_12h_FDR005unique_down,
                              CY20_24h_FDR005unique_up, CY20_24h_FDR005unique_down,
                              CY20_48h_FDR005unique_up, CY20_48h_FDR005unique_down),
                 Category = rep(rep(c("011h", "023h", "0312h", "0424h", "0548h"), each = 2), times = 3),
                 CY = rep(c("CY15", "CY16", "CY20"), each = 10)
)
library(ggplot2)
library(reshape2)
#install.packages("RColorBrewer", dependencies = TRUE)
#library(RColorBrewer)
#display.brewer.all()

levels(df$Category) <- c("1h", "3h", "12h", "24h", "48h")
levels(df$expression_change) <- c("up", "down")
g <- ggplot(
  df,
  aes (
    x = Category,             # 遺伝子別でグルーピング
    y = Numgenes,
    fill = expression_change       # 縦軸を生物種で fill up
  )
)
g <- g + geom_bar(stat = "identity")
g <- g +  theme_bw()
g <- g + scale_colour_manual(values = "Set1")
g <- g + ylab("Number of DEGs")
g <- g + xlab("time-course")
g <- g + theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))
g <- g + theme_bw(base_size = 20)
CYall <- g + facet_grid(CY ~ .)
plot(CYall)
ggsave(file = "~/Nakano_RNAseq/network_analysis/results_Fig/common_network/FDR0005=<fig>FDR005.png", plot = CYall, dpi = 100)