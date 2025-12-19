library(ggplot2)

crcdata <- read.csv("./data/crc_data_ES2.csv")


#set YN to 01
require(dplyr)
crcdata <- crcdata %>%
  mutate(solution_proposed_YN = ifelse(solution_proposed_YN == "N",0,1))

require(dplyr)
crcdata <- crcdata %>%
  mutate(solution_implemented_YN = ifelse(solution_implemented_YN == "N",0,1))

require(dplyr)
crcdata <- crcdata %>%
  mutate(S_stakeholder_engagment_YN = ifelse(S_stakeholder_engagment_YN == "N",0,1))

#fix colnames
names(crcdata)[names(crcdata) == 'STE_IAP2_NA'] <- 'STE_IAP2_data_gathering'
names(crcdata)[names(crcdata) == 'G_na'] <- 'G_nogeography'

par(mar = c(8,4,4,4))
barplot(colSums(crcdata[,4:10]), las=2)


#year
par(mar = c(4,4,4,4))
barplot(table(crcdata$solution_proposed_YN))

nrow(subset(crcdata, solution_proposed_YN == "1"))/nrow(subset(crcdata, solution_proposed_YN == "0"))

nrow(subset(crcdata, solution_implemented_YN == "1"))/nrow(subset(crcdata, solution_implemented_YN == "0"))


#stakeholder
par(mar = c(4,4,4,4))
barplot(table(crcdata$S_stakeholder_engagment_YN))

#year
barplot(table(crcdata$solution_implemented_YN))

#year
barplot(table(crcdata$year))

#researcher
df <- t(t(colSums(crcdata[,12:17])))
df <- df[order(df[,1], decreasing = TRUE),]
par(mar = c(6,4,4,4))
barplot(df, las=2)

#stakeholder types
df <- t(t(colSums(crcdata[,23:32])))
df <- df[order(df[,1], decreasing = TRUE),]
par(mar = c(11,4,4,4))
barplot(df, las=2)

#ghodsvali scale
df <- t(t(colSums(crcdata[,33:36])))
df <- df[order(df[,1], decreasing = TRUE),]
barplot(df, las=2)

#IAP2 scale
df <- t(t(colSums(crcdata[,37:43])))
df <- df[order(df[,1], decreasing = TRUE),]
par(mar = c(12,4,4,4))
barplot(df, las=2)


#local scale
df <- t(t(colSums(crcdata[,44:51])))
df <- df[order(df[,1], decreasing = TRUE),]
par(mar = c(9,4,4,4))
barplot(df, las=2)


#geography
df <- t(t(colSums(crcdata[,52:57])))
df <- df[order(df[,1], decreasing = TRUE),]
par(mar = c(8,4,4,4))
barplot(df, las=2)


ggplot(crcdata,aes(x=solution_proposed_YN, fill=year)) + 
  geom_bar(stat = "count", position = "dodge", fill="gray", color="black") + theme_minimal()

ggplot(crcdata,aes(x=solution_implemented_YN, fill=year)) + 
  geom_bar(stat = "count", position = "dodge", fill="gray", color="black") + theme_minimal()

ggplot(crcdata,aes(x=S_model_YN, fill=year)) + 
  geom_bar(stat = "count", position = "dodge", fill="gray", color="black") + theme_minimal()
  
# 
# 
# ggplot(as.data.frame(crcdata), aes(factor(year), Freq, fill = S_social)) +     
#   geom_col(position = 'dodge')
# 
# 
# ggplot(data=crcdata, aes(x=S_tech)) +
#   geom_bar()
# 
# ggplot(data=crcdata, aes(x=S_policy)) +
#   geom_bar()
# 
# ggplot(data=crcdata, aes(x=S_institutional)) +
#   geom_bar()
# 
# ggplot(data=crcdata, aes(x=S_social)) +
#   geom_bar()
# 
# ggplot(data=crcdata, aes(x=S_economic)) +
#   geom_bar()
# 
# ggplot(data=crcdata, aes(x=S_ecological)) +
#   geom_bar()
# 
# ggplot(data=crcdata, aes(x=S_educational)) +
