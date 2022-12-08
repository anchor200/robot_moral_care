library(lavaan)
library(ggplot2)
data_resh<-read.delim("data.txt", sep='\t', header=T, stringsAsFactor=FALSE)
quant <- data_resh[c("MR", "RHAB", "AN", "AC", "RATE_WITCH", "SBS", "AM", "IRI_EC", "IRI_PT", "EXP", "cul","Age", "Gender", "EDU")]
colnames(quant) <- c("MR", "RHAB", "AN", "AC", "RATE_WITCH", "SBS", "AM", "IRI_EC", "IRI_PT", "EXP", "cul","Age", "Gender", "EDU")
edu_tiers <- rownames(table(quant$EDU))
data_by_item_JP<-read.delim("JP_DATA.txt", sep='\t', header=T, stringsAsFactor=FALSE)
data_by_item_JP$EDU[data_by_item_JP$EDU==edu_tiers[1]] <- 1
data_by_item_JP$EDU[data_by_item_JP$EDU==edu_tiers[2]] <- 6
data_by_item_JP$EDU[data_by_item_JP$EDU==edu_tiers[3]] <- 2
data_by_item_JP$EDU[data_by_item_JP$EDU==edu_tiers[4]] <- 0
data_by_item_JP$EDU[data_by_item_JP$EDU==edu_tiers[5]] <- 5
data_by_item_JP$EDU[data_by_item_JP$EDU==edu_tiers[6]] <- 3.5
data_by_item_JP$EDU[data_by_item_JP$EDU==edu_tiers[7]] <- 0
data_by_item_JP$EDU[data_by_item_JP$EDU==edu_tiers[8]] <- 3
data_by_item_JP$EDU[data_by_item_JP$EDU==edu_tiers[9]] <- 1
data_by_item_JP$EDU[data_by_item_JP$EDU==edu_tiers[10]] <- 4
data_by_item_JP$EDU[data_by_item_JP$EDU==edu_tiers[11]] <- 5
data_by_item_JP$EDU[data_by_item_JP$EDU==edu_tiers[12]] <- 6
data_by_item_JP$EDU[data_by_item_JP$EDU==edu_tiers[13]] <- 2
data_by_item_JP$EDU <- as.numeric(data_by_item_JP$EDU)
data_by_item_JP$EDU[data_by_item_JP$EDU < 5] <- 0
data_by_item_JP$EDU[data_by_item_JP$EDU >= 5] <- 1
table(data_by_item_JP$Gender)
data_by_item_JP$Gender[data_by_item_JP$Gender=="その他"] <- "2"
data_by_item_JP <- subset(data_by_item_JP, data_by_item_JP$Gender != "2")
#data_by_item_JP$Gender <- as.factor(data_by_item_JP$Gender)
data_by_item_US<-read.delim("US_DATA.txt", sep='\t', header=T, stringsAsFactor=FALSE)
data_by_item_US$EDU[data_by_item_US$EDU==edu_tiers[1]] <- 1
data_by_item_US$EDU[data_by_item_US$EDU==edu_tiers[2]] <- 6
data_by_item_US$EDU[data_by_item_US$EDU==edu_tiers[3]] <- 2
data_by_item_US$EDU[data_by_item_US$EDU==edu_tiers[4]] <- 0
data_by_item_US$EDU[data_by_item_US$EDU==edu_tiers[5]] <- 5
data_by_item_US$EDU[data_by_item_US$EDU==edu_tiers[6]] <- 3.5
data_by_item_US$EDU[data_by_item_US$EDU==edu_tiers[7]] <- 0
data_by_item_US$EDU[data_by_item_US$EDU==edu_tiers[8]] <- 3
data_by_item_US$EDU[data_by_item_US$EDU==edu_tiers[9]] <- 1
data_by_item_US$EDU[data_by_item_US$EDU==edu_tiers[10]] <- 4
data_by_item_US$EDU[data_by_item_US$EDU==edu_tiers[11]] <- 5
data_by_item_US$EDU[data_by_item_US$EDU==edu_tiers[12]] <- 6
data_by_item_US$EDU[data_by_item_US$EDU==edu_tiers[13]] <- 2
data_by_item_US$EDU <- as.numeric(data_by_item_US$EDU)
data_by_item_US$EDU[data_by_item_US$EDU < 5] <- 0
data_by_item_US$EDU[data_by_item_US$EDU >= 5] <- 1
table(data_by_item_US$Gender)
data_by_item_US$Gender[data_by_item_US$Gender=="Other"] <- "2"
data_by_item_US <- subset(data_by_item_US, data_by_item_US$Gender != "2")
data_by_item_US$cul <- "US"
joint_data <- rbind(data_by_item_JP, data_by_item_US)

source('sem_model.r')

model_result <- cfa(model, joint_data, group = "cul", std.lv=TRUE)
fitMeasures(fit_preRegi_FULL_multi)

# write.table(summary(fit_preRegi_FULL_multi, standardized=F)[1], "coefs_with_anam.tsv", quote=F, sep = "\t", col.names=T, append=F)
