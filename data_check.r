library(ggplot2)

data_jp<-read.delim("redundant/jp_data.tsv", sep='\t', header=T, stringsAsFactor=FALSE)
data_jp$id <- paste("JP", rownames(data_jp), sep="")
data_en<-read.delim("redundant/us_data.tsv", sep='\t', header=T, stringsAsFactor=FALSE)
data_en$id <- paste("EN", rownames(data_en), sep="")
data_en$FNL1 <- c(NA)
data_en$FNL2 <- c(NA)
data_jp$Gender[data_jp$Gender=="女性"] = 1
data_jp$Gender[data_jp$Gender=="男性"] = 0
data_en$Gender[data_en$Gender=="Female"] = 1
data_en$Gender[data_en$Gender=="Male"] = 0
table(data_jp$Gender) 
table(data_en$Gender)
table(data_jp$Gender)  / nrow(data_jp)
table(data_en$Gender) / nrow(data_en)
data_jp$Age <- as.numeric(data_jp$Age)
data_en$Age <- as.numeric(data_en$Age)



### data summary ###
hist(data_jp$Age)
hist(data_en$Age)

sum(is.na(data_jp$Age))
mean(data_jp$Age)
sd(data_jp$Age)
min(data_jp$Age)
max(data_jp$Age)

sum(is.na(data_en$Age))
mean(data_en$Age)
sd(data_en$Age)
min(data_en$Age)
max(data_en$Age)


### data check and extract ###

#### US ####
AN_en <- data_en[str_detect(colnames(data_en), pattern="AN_")]
AN_en$id <- data_en$id
AC_en <- data_en[str_detect(colnames(data_en), pattern="AC_")]
AC_en$id <- data_en$id
AC_en$AC_6. <- 8 - AC_en$AC_6.
AC_en$AC_9. <- 8 - AC_en$AC_9.
RATE_en <- data_en[str_detect(colnames(data_en), pattern="RATE_WITCH")]
RATE_en$id <- data_en$id
MR_en <- 8 - data_en[str_detect(colnames(data_en), pattern="MR_")]
MR_en$id <- data_en$id
SBS_en <- data_en[str_detect(colnames(data_en), pattern="SBS_")]
SBS_en$id <- data_en$id
AM_en <- data_en[str_detect(colnames(data_en), pattern="AM_")]
AM_en$id <- data_en$id
IRI_en <- data_en[str_detect(colnames(data_en), pattern="IRI_")]
IRI_en$id <- data_en$id
IRI_en$IRI_3.PT. <- 8 - IRI_en$IRI_3.PT.
IRI_en$IRI_4.EC. <- 8 - IRI_en$IRI_4.EC.
IRI_en$IRI_14.EC. <- 8 - IRI_en$IRI_14.EC.
IRI_en$IRI_15.PT. <- 8 - IRI_en$IRI_15.PT.
IRI_en$IRI_18.EC. <- 8 - IRI_en$IRI_18.EC.
IRI_en <- IRI_en[c(1,3,5,7,9,10,12, 2,4,6,8,11,13,14, 15)]
EXP_en <- data_en[str_detect(colnames(data_en), pattern="EXP")]
EXP_en$id <- data_en$id
RHAB_en <- data_en[c("RHAB", "FNL1", "FNL2", "id")]
DEMO_en <- data_en[c("id", "Gender", "Age", "Region", "REL")]
DEMO2_en <- cbind(data_en[str_detect(colnames(data_en), pattern="NAT")], data_en[str_detect(colnames(data_en), pattern="LNG")])
DEMO2_en <- cbind(DEMO2_en, data_en["EDU"])
DEMO2_en$id <- data_en$id

#### JP ####
AN_jp <- data_jp[str_detect(colnames(data_jp), pattern="AN_")]
AN_jp$id <- data_jp$id
AC_jp <- data_jp[str_detect(colnames(data_jp), pattern="AC_")]
AC_jp$id <- data_jp$id
AC_jp$AC_6. <- 8 - AC_jp$AC_6.
AC_jp$AC_9. <- 8 - AC_jp$AC_9.
RATE_jp <- data_jp[str_detect(colnames(data_jp), pattern="RATE_WITCH")]
RATE_jp$id <- data_jp$id
MR_jp <- 8 - data_jp[str_detect(colnames(data_jp), pattern="MR_")]
MR_jp$id <- data_jp$id
SBS_jp <- data_jp[str_detect(colnames(data_jp), pattern="SBS_")]
SBS_jp$id <- data_jp$id
AM_jp <- data_jp[str_detect(colnames(data_jp), pattern="AM_")]
AM_jp$id <- data_jp$id
IRI_jp <- data_jp[str_detect(colnames(data_jp), pattern="IRI_")]
IRI_jp$id <- data_jp$id
IRI_jp$IRI_3.PT. <- 8 - IRI_jp$IRI_3.PT.
IRI_jp$IRI_4.EC. <- 8 - IRI_jp$IRI_4.EC.
IRI_jp$IRI_14.EC. <- 8 - IRI_jp$IRI_14.EC.
IRI_jp$IRI_15.PT. <- 8 - IRI_jp$IRI_15.PT.
IRI_jp$IRI_18.EC. <- 8 - IRI_jp$IRI_18.EC.
IRI_jp <- IRI_jp[c(1,3,5,7,9,10,12, 2,4,6,8,11,13,14, 15)]
EXP_jp <- data_jp[str_detect(colnames(data_jp), pattern="EXP")]
EXP_jp$id <- data_jp$id
RHAB_jp <- data_jp[c("RHAB", "FNL1", "FNL2", "id")]
DEMO_jp <- data_jp[c("id", "Gender", "Age", "Region", "REL")]
DEMO2_jp <- cbind(data_jp[str_detect(colnames(data_jp), pattern="NAT")], data_jp[str_detect(colnames(data_jp), pattern="LNG")])
DEMO2_jp <- cbind(DEMO2_jp, data_jp["EDU"])
DEMO2_jp$id <- data_jp$id
DEMO2_jp

Cor_check <- function(data){
    M <- data.frame(AV=apply(data,1,mean))
    data_M <- cbind(data, M)
    
    data_alpha <- psych::alpha(data)
    print(data_alpha)
    corrplot::corrplot(cor(data_M))
}

Diff_check <- function(data_jp, data_en, min, max){
    AB_jp <- data_jp
        AB_en <- data_en
    print(summary(apply(AB_jp[-ncol(AB_jp)], 1,mean)))
    print(summary(apply(AB_en[-ncol(AB_en)], 1,mean)))
    print(sd(apply(AB_jp[-ncol(AB_jp)], 1,mean)))
    print(sd(apply(AB_en[-ncol(AB_en)], 1,mean)))
    print(t.test(apply(AB_jp[-ncol(AB_jp)], 1,mean), apply(AB_en[-ncol(AB_en)], 1,mean)))
    print(mean(apply(AB_en[-ncol(AB_jp)], 1,mean)) - mean(apply(AB_jp[-ncol(AB_jp)], 1,mean)))
    print(cohen.d(apply(AB_en[-ncol(AB_en)], 1,mean), apply(AB_jp[-ncol(AB_jp)], 1,mean)))
    temp <- data.frame(apply(AB_jp[-ncol(AB_jp)], 1,mean))
    colnames(temp) <- c("value")
    temp$cul <- "JP"
    temp$id <- AB_jp$id
    temp2 <- data.frame(apply(AB_en[-ncol(AB_en)], 1,mean))
    colnames(temp2) <- c("value")
    temp2$cul <- "US"
    temp2$id <- AB_en$id
    AB <- rbind(temp, temp2)
    
    g <- ggplot(AB, aes(x=cul, y=value)) + geom_boxplot()  +
    stat_summary(fun=mean, geom="point", shape=20, size=9, color="blue", fill="red")+ 
    scale_y_continuous(breaks=c(min:max))+
    theme(axis.title.x=element_blank(), 
          axis.title.y=element_blank(),
          axis.text=element_text(size=18)
         )
    print(g)
    return(AB)
}

#### by variables ####
##### moral care for robots #####
Cor_check(MR_jp[-ncol(MR_jp)])
Cor_check(MR_en[-ncol(MR_en)])
MR <- Diff_check(MR_jp, MR_en, 1, 7)

##### animism #####
Cor_check(AN_jp[-ncol(AN_jp)])
Cor_check(AN_en[-ncol(AN_en)])
AN <- Diff_check(AN_jp, AN_en, 1, 5)

Cor_check(cbind(AN_jp[-ncol(AN_jp)], AM_jp[-ncol(AM_jp)]))
Cor_check(cbind(AN_en[-ncol(AN_en)], AM_en[-ncol(AM_en)]))
AN <- Diff_check(AN_jp, AN_en, 1, 5)

##### anthropomorphism #####
Cor_check(AM_jp[-ncol(AM_jp)])
Cor_check(AM_en[-ncol(AM_en)])
AM <- Diff_check(AM_jp, AM_en, 0, 10)

##### anthropocentrism #####
Cor_check(AC_jp[-ncol(AC_jp)])
Cor_check(AC_en[-ncol(AC_en)])
AC <- Diff_check(AC_jp, AC_en, 1, 7)

##### SBS #####
Cor_check(SBS_jp[-ncol(SBS_jp)])
Cor_check(SBS_en[-ncol(SBS_en)])
SBS <- Diff_check(SBS_jp, SBS_en, -4, 4)

hist(apply(SBS_jp[-ncol(SBS_jp)], 1,mean), main="Religious beliefs (JP)", xlab="")
hist(apply(SBS_en[-ncol(SBS_jp)], 1,mean), main="Religious beliefs (US)", xlab="")

##### IRI #####
Cor_check(IRI_jp[c(1,2,3,4,5,6,7)])
Cor_check(IRI_en[c(1,2,3,4,5,6,7)])
IRI <- Diff_check(IRI_jp, IRI_en, 1, 7)
IRI_EC <- Diff_check(IRI_jp[c(1,2,3,4,5,6,7,15)], IRI_en[c(1,2,3,4,5,6,7,15)], 1, 7)
IRI_PT <- Diff_check(IRI_jp[c(8,9,10,11,12,13,14,15)], IRI_en[c(8,9,10,11,12,13,14,15)], 1, 7)

Cor_check(IRI_jp_ec_nonrev)
Cor_check(IRI_en_ec_nonrev)
IRI_EC_norev <- Diff_check(IRI_jp[, c(1,3,6,7)], IRI_en[, c(1,3,6,7)], 1, 7)

Cor_check(IRI_jp[c(9,10,12,13,14)])
Cor_check(IRI_en[c(9,10,12,13,14)])
IRI_PT_norev <- Diff_check(IRI_jp[c(9,10,12,13,14)], IRI_en[c(9,10,12,13,14)], 1, 7)

##### media effect #####
Cor_check(EXP_jp[-ncol(EXP_jp)])
Cor_check(EXP_en[-ncol(EXP_en)])
EXP <- Diff_check(EXP_jp, EXP_en, 1,8)


#### robot image ####
robo_rate <- rbind(
table(RATE_jp[1]),
table(RATE_en[1])
    )
robo_rate
table(RATE_jp[1]) / sum(table(RATE_jp[1]))
table(RATE_en[1]) / sum(table(RATE_en[1]))
chisq.test(robo_rate)


#### religiosity ####
hist(RHAB_jp$RHAB, breaks=seq(0,10,1), main="Religious attendance (JP)", xlab="")
hist(RHAB_en$RHAB, main="Religious attendance (US)", xlab="")
table(RHAB_jp$RHAB)
table(RHAB_en$RHAB)
t.test(RHAB_jp$RHAB, RHAB_en$RHAB)

temphab <- data.frame(RHAB_jp$RHAB)
colnames(temphab) <- c("value")
temphab$cul <- "Japan"
temphab$id <- RHAB_jp$id
temp2hab <- data.frame(RHAB_en$RHAB)
colnames(temp2hab) <- c("value")
temp2hab$cul <- "US"
temp2hab$id <- RHAB_en$id

RHAB <- rbind(temphab, temp2hab)
g <- ggplot(RHAB, aes(x=cul, y=value)) + geom_boxplot()  +
stat_summary(fun=mean, geom="point", shape=20, size=9, color="blue", fill="red")+
    theme(axis.title.x=element_blank(), 
          axis.title.y=element_blank(),
          axis.text=element_text(size=18)
         )
print(g)

sd(RHAB_en$RHAB)
sd(RHAB_jp$RHAB)
cohen.d(RHAB_en$RHAB, RHAB_jp$RHAB)


#### demographics ####
table(data_jp$Region) / nrow(data_jp)
table(data_en$Region) / nrow(data_en)
table(data_jp$REL)
table(data_en$REL)
table(data_jp$FNL1)
table(data_jp$FNL2)



### data resh ###
colnames(AN) <- c("AN", "cul", "id")
colnames(AC) <- c("AC", "cul", "id")
colnames(MR) <- c("MR", "cul", "id")
colnames(SBS) <- c("SBS", "cul", "id")
colnames(AM) <- c("AM", "cul", "id")
colnames(IRI) <- c("IRI", "cul", "id")
colnames(EXP) <- c("EXP", "cul", "id")
colnames(IRI_EC) <- c("IRI_EC", "cul", "id")
colnames(IRI_PT) <- c("IRI_PT", "cul", "id")
IRI_EC_norev$id <- IRI_EC$id
IRI_PT_norev$id <- IRI_PT$id
colnames(IRI_EC_norev) <- c("IRI_EC_norev", "cul", "id")
colnames(IRI_PT_norev) <- c("IRI_PT_norev", "cul", "id")

RATE_jp$cul <- "JP"
RATE_en$cul <- "US"
RATE <- rbind(RATE_jp, RATE_en)
RATE$RATE_WITCH[RATE$RATE_WITCH=="A"] = 1
RATE$RATE_WITCH[RATE$RATE_WITCH=="B"] = 0
DEMO_jp$cul <- "JP"
DEMO_en$cul <- "US"
DEMO <- rbind(DEMO_jp, DEMO_en)
DEMO2_jp$cul <- "JP"
DEMO2_en$cul <- "US"
colnames(DEMO2_jp) <- c("NAT_1", "NAT_2", "NAT_3", "NAT_4", "LNG_1", "LNG_2", "EDU", "id", "cul")
colnames(DEMO2_en) <- c("NAT_1", "NAT_2", "NAT_3", "NAT_4", "LNG_1", "LNG_2", "EDU", "id", "cul")
DEMO2_jp$cul <- "JP"
DEMO2_en$cul <- "US"
DEMO2 <- rbind(DEMO2_jp, DEMO2_en)
RHAB_jp$cul <- "JP"
RHAB_en$cul <- "US"
RHAB <- rbind(RHAB_jp, RHAB_en)


merge2 <- function(dfs, ...)
{
　 # res <- merge2(list(Df1, Df2, Df3), by="ID")
　 base <- dfs[1]
　 lapply(dfs[-1], function(i) base <<- merge(base, i, ...)) # [1]
  return(base)
}

full_data <- merge2(list(DEMO, DEMO2, RHAB, AN, AC, RATE, MR, SBS, AM, IRI_EC, IRI_PT, EXP, IRI_EC_norev, IRI_PT_norev), by=c("id", "cul"))
# write.table(full_data, "data.txt", quote=F, col.names=T, append=F, sep="\t")