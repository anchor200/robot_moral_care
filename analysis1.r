library(effsize)
library(MuMIn)
options(na.action = "na.fail")

data_resh<-read.delim("data.txt", sep='\t', header=T, stringsAsFactor=FALSE)

quant <- data_resh[c("MR", "RHAB", "AN", "AC", "RATE_WITCH", "SBS", "AM", "IRI_EC", "IRI_PT_norev", "EXP", "cul","Age", "Gender", "EDU")]
colnames(quant) <- c("MR", "RHAB", "AN", "AC", "RATE_WITCH", "SBS", "AM", "IRI_EC", "IRI_PT", "EXP", "cul","Age", "Gender", "EDU")

quant$cul[quant$cul=="US"] <- 0
quant$cul[quant$cul=="JP"] <- 1
quant$Gender[quant$Gender=="その他"] <- "2"
quant$Gender[quant$Gender=="Other"] <- "2"
quant$Gender <- as.factor(quant$Gender)
quant$Age <- as.numeric(quant$Age)
edu_tiers <- rownames(table(quant$EDU))
quant$EDU[quant$EDU==edu_tiers[1]] <- 1
quant$EDU[quant$EDU==edu_tiers[2]] <- 6
quant$EDU[quant$EDU==edu_tiers[3]] <- 2
quant$EDU[quant$EDU==edu_tiers[4]] <- 0
quant$EDU[quant$EDU==edu_tiers[5]] <- 5
quant$EDU[quant$EDU==edu_tiers[6]] <- 3.5
quant$EDU[quant$EDU==edu_tiers[7]] <- 0
quant$EDU[quant$EDU==edu_tiers[8]] <- 3
quant$EDU[quant$EDU==edu_tiers[9]] <- 1
quant$EDU[quant$EDU==edu_tiers[10]] <- 4
quant$EDU[quant$EDU==edu_tiers[11]] <- 5
quant$EDU[quant$EDU==edu_tiers[12]] <- 6
quant$EDU[quant$EDU==edu_tiers[13]] <- 2
quant$EDU <- as.numeric(quant$EDU)
colnames(quant)[5] <- "TYP"
quant$Gender1[quant$Gender == 0] <- 0
quant$Gender2[quant$Gender == 0] <- 0
quant$Gender1[quant$Gender == 1] <- 1
quant$Gender2[quant$Gender == 1] <- 0
quant$Gender1[quant$Gender == 2] <- 0
quant$Gender2[quant$Gender == 2] <- 1


normali <- function(quant){
    quant$MR <- (quant$MR - mean(quant$MR))/sd(quant$MR)
    quant$AN <- (quant$AN- mean(quant$AN))/sd(quant$AN)
    quant$AC <- (quant$AC- mean(quant$AC))/sd(quant$AC)
    quant$SBS <- (quant$SBS- mean(quant$SBS))/sd(quant$SBS)
    quant$AM <- (quant$AM- mean(quant$AM))/sd(quant$AM)
    quant$IRI_EC <- (quant$IRI_EC- mean(quant$IRI_EC))/sd(quant$IRI_EC)
    quant$IRI_PT <- (quant$IRI_PT- mean(quant$IRI_PT))/sd(quant$IRI_PT)
    quant$EXP <- (quant$EXP- mean(quant$EXP))/sd(quant$EXP)
    quant$RHAB <- (quant$RHAB- mean(quant$RHAB))/sd(quant$RHAB)
    quant$Age <- (quant$Age- mean(quant$Age,na.rm=T))/sd(quant$Age,na.rm=T)

    temp.edu <- quant$EDU
    table(temp.edu)
    temp.edu[temp.edu >= 5] <- 1000
    temp.edu[temp.edu < 5] <- -1000
    temp.edu[temp.edu ==1000] <- 1
    temp.edu[temp.edu ==-1000] <- 0
    table(temp.edu)
    quant$EDU <- temp.edu
    return(quant)
}

temp_norm <- normali(quant[quant$cul=="0", ])
temp_norm$AN_AM <- temp_norm$AN + temp_norm$AM
temp_norm$AN_AM <- (temp_norm$AN_AM - mean(temp_norm$AN_AM)) / sd(temp_norm$AN_AM)
data_us <- temp_norm

temp_norm <- normali(quant[quant$cul=="1", ])
temp_norm$AN_AM <- temp_norm$AN + temp_norm$AM
temp_norm$AN_AM <- (temp_norm$AN_AM - mean(temp_norm$AN_AM)) / sd(temp_norm$AN_AM)
data_jp <- temp_norm

temp_norm <- normali(quant)
temp_norm$AN_AM <- temp_norm$AN + temp_norm$AM
temp_norm$AN_AM <- (temp_norm$AN_AM - mean(temp_norm$AN_AM)) / sd(temp_norm$AN_AM)

data_sum <- temp_norm
us_subdata <- data_sum[data_sum$cul=="0",]
jp_subdata <- data_sum[data_sum$cul=="1",]

Diff_resh <- function(jp, us){

    jp_m <- mean(jp)
    us_m <- mean(us)
    jp_sd <- sd(jp)
    us_sd <- sd(us)
    dd <- cohen.d(us, jp)
    ddcu <- dd$conf.int[[1]]
    ddcl <- dd$conf.int[[2]]
    ddd <- dd$estimate
        AB <- data.frame("US_Mean"=us_m, "US_SD"=us_sd, "JP_Mean"=jp_m, "JP_SD"=jp_sd, "Cohen's_d"=ddd, "d_upper_95CI"=ddcu, "d_lower_95CI"=ddcl)
    return(AB)
}

coef_resh <- function(x, method, country_){
    coef_s <- as.data.frame(summary(x)$coefficients)
    coef_s$variable <- rownames(coef_s)
    coef_s$variable[coef_s$variable=="RHAB"] <- "Religious attendance"
    coef_s$variable[coef_s$variable=="SBS"] <- "Religious beliefs"
    coef_s$variable[coef_s$variable=="AC"] <- "Anthropocentrism"
    coef_s$variable[coef_s$variable=="AN"] <- "Animism"
    coef_s$variable[coef_s$variable=="AM"] <- "Anthropomorphism"
    coef_s$variable[coef_s$variable=="AN_AM"] <- "merged Animism"
    coef_s$variable[coef_s$variable=="cul1"] <- "culture"
    coef_s$variable[coef_s$variable=="cul1:RHAB"] <- "culture*Religious attendance"
    coef_s$variable[coef_s$variable=="cul1:SBS"] <- "culture*Religious beliefs"
    coef_s$variable[coef_s$variable=="cul1:AC"] <- "culture*Anthropocentrism"
    coef_s$variable[coef_s$variable=="cul1:AN"] <- "culture*Animism"
    coef_s$variable[coef_s$variable=="cul1:AM"] <- "culture*Anthropomorphism"
    coef_s$variable[coef_s$variable=="cul1:AN_AM"] <- "culture*merged Animism"
    coef_s$variable <- factor(coef_s$variable, 
                              levels=rev(c("(Intercept)","culture", "Religious attendance","Religious beliefs","Anthropocentrism", "merged Animism","Animism","Anthropomorphism", 
                                           "culture*Religious attendance","culture*Religious beliefs", "culture*Anthropocentrism", "culture*merged Animism", "culture*Animism", "culture*Anthropomorphism", 
                                           "IRI_EC", "IRI_PT", "EXP","TYP", "EDU", "Age", "Gender1", "Gender2")))
    coef_s$model <- method
    coef_s$country <- country_
    coef_s$id <- rownames(coef_s)
    colnames(coef_s)[2] <- "Std"
    temp <- data.frame(confint(x))
    colnames(temp) <- c("lower", "upper")
    
    temp$id <- rownames(temp)
    
    coef_s <- merge(coef_s, temp, by="id", all=F)
    coef_s <- coef_s[, c(8,7,6, 2,3,9,10,4,5)]
    return(coef_s)
}


### descriptive statistics ###
rbind(Diff_resh(jp_subdata$MR, us_subdata$MR),
Diff_resh(jp_subdata$RHAB, us_subdata$RHAB),
Diff_resh(jp_subdata$SBS, us_subdata$SBS),
Diff_resh(jp_subdata$AC, us_subdata$AC),
Diff_resh(jp_subdata$AN_AM, us_subdata$AN_AM),
Diff_resh(jp_subdata$IRI_EC, us_subdata$IRI_EC),
Diff_resh(jp_subdata$IRI_PT, us_subdata$IRI_PT),
Diff_resh(jp_subdata$EXP, us_subdata$EXP))



### correlation ###
for_cor_us <- data_us
for_cor_us_ <- for_cor_us[, c("MR", "RHAB", "SBS","AC", "AN_AM", "IRI_EC", "IRI_PT", "TYP", "EXP", "Age", "Gender1", "Gender2", "EDU")]
colnames(for_cor_us_) <- c("Moral care for robots (MR)", 
                           "Religious attendance (RA)", 
                           "Religious beliefs (RB)",
                           "Anthropocentrism (AC)", 
                           "Animism (AN)",
                           "Empathic concern (EC)",
                           "Perspective taking (PT)",
                           "Typical image of robots (TYP)",
                           "Exposure to robot related experience (EXP)",
                           "Age",
                           "Gender1",
                           "Gender2",
                           "Education")
cor_us__ <- as.data.frame(cor(for_cor_us_))
colnames(cor_us__) <-  c("MR", 
                           "RA", 
                           "RB",
                           "AC", 
                           "AN",
                           "EC",
                           "PT",
                           "TYP",
                           "EXP",
                           "Age",
                           "Gender1",
                           "Gender2",
                           "Edu")
cor_us__



for_cor_jp <- data_jp
for_cor_jp_ <- for_cor_jp[, c("MR", "RHAB", "SBS","AC", "AN_AM", "IRI_EC", "IRI_PT", "TYP", "EXP", "Age", "Gender1", "Gender2", "EDU")]
colnames(for_cor_jp_) <- c("Moral care for robots (MR)", 
                           "Religious attendance (RA)", 
                           "Religious beliefs (RB)",
                           "Anthropocentrism (AC)", 
                           "Animism (AN)",
                           "Empathic concern (EC)",
                           "Perspective taking (PT)",
                           "Typical image of robots (TYP)",
                           "Exposure to robot related experience (EXP)",
                           "Age",
                           "Gender1",
                           "Gender2",
                           "Education")
cor_jp__ <- as.data.frame(cor(for_cor_jp_))
colnames(cor_jp__) <-  c("MR", 
                           "RA", 
                           "RB",
                           "AC", 
                           "AN",
                           "EC",
                           "PT",
                           "TYP",
                           "EXP",
                           "Age",
                           "Gender1",
                           "Gender2",
                           "Edu")
cor_jp__

fit_re <- function(x, method, country){

rrr1 <- data.frame(x)
rrr1$country <- country
rrr1$model <- method
rrr1$var <- rownames(rrr1)
rrr1 <- rrr1[, c(2,3,4,1)]
colnames(rrr1)[4] <- "value"

return(rrr1)
}

### regression ###
#### US ####
rel_us <- lm(MR ~ RHAB + SBS + IRI_PT + IRI_EC +TYP + EXP + Age+EDU + Gender1 + Gender2, data=data_us)
cof1 <- coef_resh(rel_us, "religion", "US")
rrr1 <- t(data.frame("R-squared"=summary(rel_us)["adj.r.squared"][[1]], "AIC"=AIC(rel_us)))
rrr1 <- fit_re(rrr1, "religion", "US")
write.table(cof1, "out/religion_us_coef.tsv", sep="\t", row.names=F)
write.table(rrr1, "out/religion_us_fit.tsv", sep="\t", row.names=F)

val_us <- lm(MR ~ AC + AN_AM + IRI_PT + IRI_EC +TYP + EXP + Age+EDU + Gender1 + Gender2, data=data_us)
cof2 <- coef_resh(val_us, "value", "US")
rrr2 <- t(data.frame("R-squared"=summary(val_us)["adj.r.squared"][[1]], "AIC"=AIC(val_us)))
rrr2 <- fit_re(rrr2, "value", "US")
write.table(cof2, "out/value_us_coef.tsv", sep="\t", row.names=F)
write.table(rrr2, "out/value_us_fit.tsv", sep="\t", row.names=F)

both_us <- lm(MR ~ RHAB + SBS + AC + AN_AM + IRI_PT + IRI_EC +TYP + EXP + Age+EDU + Gender1 + Gender2, data=data_us)
coef_resh(both_us, "both", "US")
(data.frame("R-squared"=summary(both_us)["adj.r.squared"][[1]], "AIC"=AIC(both_us)))


sep_us <- lm(MR ~ AC + AN+AM + IRI_PT + IRI_EC +TYP + EXP + Age+EDU + Gender1 + Gender2, data=data_us)
coef_resh(sep_us, "separate", "US")
t(data.frame("R-squared"=summary(sep_us)["adj.r.squared"][[1]], "AIC"=AIC(sep_us)))

##### model selection #####
US_full_model <- lm(MR ~ RHAB + SBS + AC + AN_AM + IRI_PT  + IRI_EC+TYP + EXP + Age + EDU + Gender1 + Gender2, data=data_us)
US_kekka.AIC <- dredge(US_full_model, rank="AIC")
US_kekka.AIC

best_us <- lm(MR ~ AC + AN_AM + IRI_PT  + IRI_EC + EXP+ Gender1 , data=data_us)
cof3 <- coef_resh(best_us, "best", "US")
rrr3 <- t(data.frame("R-squared"=summary(best_us)["adj.r.squared"][[1]], "AIC"=AIC(best_us)))
rrr3 <- fit_re(rrr3, "best", "US")
write.table(cof3, "out/best_us_coef.tsv", sep="\t", row.names=F)
write.table(rrr3, "out/best_us_fit.tsv", sep="\t", row.names=F)

#### JP ####
rel_jp <- lm(MR ~ RHAB + SBS + IRI_PT + IRI_EC +TYP + EXP + Age+EDU + Gender1 + Gender2, data=data_jp)
cof4 <- coef_resh(rel_jp, "religion", "JP")
rrr4 <- t(data.frame("R-squared"=summary(rel_jp)["adj.r.squared"][[1]], "AIC"=AIC(rel_jp)))
rrr4 <- fit_re(rrr4, "religion", "JP")
write.table(cof4, "out/religion_jp_coef.tsv", sep="\t", row.names=F)
write.table(rrr4, "out/religion_jp_fit.tsv", sep="\t", row.names=F)

val_jp <- lm(MR ~ AC + AN_AM + IRI_PT + IRI_EC +TYP + EXP + Age+EDU + Gender1 + Gender2, data=data_jp)
cof5 <- coef_resh(val_jp, "value", "JP")
rrr5 <- t(data.frame("R-squared"=summary(val_jp)["adj.r.squared"][[1]], "AIC"=AIC(val_jp)))
rrr5 <- fit_re(rrr5, "value", "JP")
write.table(cof5, "out/value_jp_coef.tsv", sep="\t", row.names=F)
write.table(rrr5, "out/value_jp_fit.tsv", sep="\t", row.names=F)

both_jp <- lm(MR ~ RHAB + SBS + AC + AN_AM + IRI_PT + IRI_EC +TYP + EXP + Age+EDU + Gender1 + Gender2, data=data_jp)
coef_resh(both_jp, "both", "JP")
t(data.frame("R-squared"=summary(both_jp)["adj.r.squared"][[1]], "AIC"=AIC(both_jp)))

sep_jp <- lm(MR ~ AC + AN+AM + IRI_PT + IRI_EC +TYP + EXP + Age+EDU + Gender1 + Gender2, data=data_jp)
coef_resh(sep_jp, "separate", "JP")
t(data.frame("R-squared"=summary(sep_jp)["adj.r.squared"][[1]], "AIC"=AIC(sep_jp)))

##### model selection #####
JP_full_model <- lm(MR ~ RHAB + SBS + AC + AN_AM + IRI_PT  + IRI_EC+TYP + EXP + Age + EDU + Gender1 + Gender2, data=data_jp)
JP_kekka.AIC <- dredge(JP_full_model, rank="AIC")
JP_kekka.AIC 

best_jp <- lm(MR ~ AC + Age + EDU+EXP+TYP +AN_AM + IRI_PT  + IRI_EC + Gender1 , data=data_jp)
cof6 <- coef_resh(best_jp, "best", "JP")
rrr6 <- t(data.frame("R-squared"=summary(best_jp)["adj.r.squared"][[1]], "AIC"=AIC(best_jp)))
rrr6 <- fit_re(rrr6, "best", "JP")
write.table(cof6, "out/best_jp_coef.tsv", sep="\t", row.names=F)
write.table(rrr6, "out/best_jp_fit.tsv", sep="\t", row.names=F)


#### INTERACTION ####
data_ <- rbind(data_us, data_jp)
inter <- lm(MR ~ cul*(RHAB + SBS + AC + AN_AM) + IRI_PT + IRI_EC +TYP + EXP + Age+EDU + Gender1 + Gender2 , data=data_)
cof7 <- coef_resh(inter, "interaction_all", "interaction")
rrr7 <- t(data.frame("R-squared"=summary(inter)["adj.r.squared"][[1]], "AIC"=AIC(inter)))
rrr7 <- fit_re(rrr7, "interaction_all", "interaction")
write.table(cof7, "out/int_all_coef.tsv", sep="\t", row.names=F)
write.table(rrr7, "out/int_all_fit.tsv", sep="\t", row.names=F)

int_full_model <- inter
kekka.AIC <- dredge(int_full_model, rank="AIC")
write.table(kekka.AIC, "out/int_select.tsv", sep="\t", row.names=F)

inter_best <- lm(MR ~ cul*(AC + AN_AM) + IRI_PT + IRI_EC +TYP + EXP + Age+EDU + Gender1 , data=data_)
cof8 <- coef_resh(inter_best, "interaction_best", "interaction")
rrr8 <- t(data.frame("R-squared"=summary(inter_best)["adj.r.squared"][[1]], "AIC"=AIC(inter_best)))
rrr8 <- fit_re(rrr8, "interaction_best", "interaction")
write.table(cof8, "out/int_best_coef.tsv", sep="\t", row.names=F)
write.table(rrr8, "out/int_best_fit.tsv", sep="\t", row.names=F)

write.table(rbind(cof1,cof2,cof3,cof4,cof5,cof6,cof7,cof8), "out/coef.tsv", sep="\t", row.names=F)
write.table(rbind(rrr1,rrr2,rrr3,rrr4,rrr5,rrr6,rrr7,rrr8), "out/fit.tsv", sep="\t", row.names=F)




