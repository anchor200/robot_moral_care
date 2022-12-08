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

coef_resh <- function(x, method){
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
    colnames(coef_s)[2] <- "Std"
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


### regression ###
#### US ####
rel_us <- lm(MR ~ RHAB + SBS + IRI_PT + IRI_EC +TYP + EXP + Age+EDU + Gender1 + Gender2, data=data_us)
coef_resh(rel_us, "religion")
t(data.frame("R-squared"=summary(rel_us)["adj.r.squared"][[1]], "AIC"=AIC(rel_us)))

val_us <- lm(MR ~ AC + AN_AM + IRI_PT + IRI_EC +TYP + EXP + Age+EDU + Gender1 + Gender2, data=data_us)
coef_resh(val_us, "value")
t(data.frame("R-squared"=summary(val_us)["adj.r.squared"][[1]], "AIC"=AIC(val_us)))

both_us <- lm(MR ~ RHAB + SBS + AC + AN_AM + IRI_PT + IRI_EC +TYP + EXP + Age+EDU + Gender1 + Gender2, data=data_us)
coef_resh(both_us, "both")
t(data.frame("R-squared"=summary(both_us)["adj.r.squared"][[1]], "AIC"=AIC(both_us)))

sep_us <- lm(MR ~ AC + AN+AM + IRI_PT + IRI_EC +TYP + EXP + Age+EDU + Gender1 + Gender2, data=data_us)
coef_resh(sep_us, "separate")
t(data.frame("R-squared"=summary(sep_us)["adj.r.squared"][[1]], "AIC"=AIC(sep_us)))

##### model selection #####
US_full_model <- lm(MR ~ RHAB + SBS + AC + AN_AM + IRI_PT  + IRI_EC+TYP + EXP + Age + EDU + Gender1 + Gender2, data=data_us)
US_kekka.AIC <- dredge(US_full_model, rank="AIC")
US_kekka.AIC

best_us <- lm(MR ~ AC + AN_AM + IRI_PT  + IRI_EC + EXP+ Gender1 , data=data_us)
coef_resh(best_us, "best")
t(data.frame("R-squared"=summary(best_us)["adj.r.squared"][[1]], "AIC"=AIC(best_us)))


#### JP ####
rel_jp <- lm(MR ~ RHAB + SBS + IRI_PT + IRI_EC +TYP + EXP + Age+EDU + Gender1 + Gender2, data=data_jp)
coef_resh(rel_jp, "religion")
t(data.frame("R-squared"=summary(rel_jp)["adj.r.squared"][[1]], "AIC"=AIC(rel_jp)))

val_jp <- lm(MR ~ AC + AN_AM + IRI_PT + IRI_EC +TYP + EXP + Age+EDU + Gender1 + Gender2, data=data_jp)
coef_resh(val_jp, "value")
t(data.frame("R-squared"=summary(val_jp)["adj.r.squared"][[1]], "AIC"=AIC(val_jp)))

both_jp <- lm(MR ~ RHAB + SBS + AC + AN_AM + IRI_PT + IRI_EC +TYP + EXP + Age+EDU + Gender1 + Gender2, data=data_jp)
coef_resh(both_jp, "both")
t(data.frame("R-squared"=summary(both_jp)["adj.r.squared"][[1]], "AIC"=AIC(both_jp)))

sep_jp <- lm(MR ~ AC + AN+AM + IRI_PT + IRI_EC +TYP + EXP + Age+EDU + Gender1 + Gender2, data=data_jp)
coef_resh(sep_jp, "separate")
t(data.frame("R-squared"=summary(sep_jp)["adj.r.squared"][[1]], "AIC"=AIC(sep_jp)))

##### model selection #####
JP_full_model <- lm(MR ~ RHAB + SBS + AC + AN_AM + IRI_PT  + IRI_EC+TYP + EXP + Age + EDU + Gender1 + Gender2, data=data_jp)
JP_kekka.AIC <- dredge(JP_full_model, rank="AIC")
JP_kekka.AIC 

best_jp <- lm(MR ~ AC + Age + EDU+EXP+TYP +AN_AM + IRI_PT  + IRI_EC + Gender1 , data=data_jp)
coef_resh(best_jp, "best")
t(data.frame("R-squared"=summary(best_jp)["adj.r.squared"][[1]], "AIC"=AIC(best_jp)))


