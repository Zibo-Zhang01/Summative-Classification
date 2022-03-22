#Summarize_DMEF 

setwd("C:\\work\\DMEF04")
DMEF <- read.csv("DMEF04.csv", head = T, sep = ",",stringsAsFactors = F)



#Preparation--------------------------------------------------------------------------------------
library(mice)
md.pattern(DMEF) 



##RFM model
#Recently
#The first purchase interval (as of June 1992, unit: month), recorded as date1
sum(is.na(DMEF$DATEFP6))  

class(DMEF$DATEFP6)       
head(DMEF$DATEFP6)
DMEF$DATEFP6 <- as.Date(DMEF$DATEFP6, "%m/%d/%Y")
a <- as.Date("92-09-01")
DMEF$date1 <- difftime(a, DMEF$DATEFP6, units = "days")
DMEF$date1 <- DMEF$date1/30
head(DMEF$date1)
hist(as.numeric(DMEF$date1), breaks = 150, xlab = "(Month)", main = "Time interval since first purchase")      


#The first and last purchase interval, recorded as date2
sum(is.na(DMEF$DATELP6))  

DMEF$DATELP6 <- as.Date(DMEF$DATELP6, "%m/%d/%Y")
DMEF$date2 <- difftime(DMEF$DATELP6, DMEF$DATEFP6, units = "days")
DMEF$date2 <- DMEF$date2/30
hist(as.numeric(DMEF$date2), breaks = 200)        
head(DMEF$date2)                                 

#Recency
opar <- par(no.readonly = T)
par(mfrow=c(1,2))
hist(as.numeric(DMEF$date1), breaks = 150, xlab = "(Month)", main = "Time interval since first purchase") 
hist(as.numeric(DMEF$date2), breaks = 200, xlab = "(Month)", main = "First and last purchase interval") 
par(opar)


DMEF$custype1[DMEF$date2 == 0] <- 0
DMEF$custype1[DMEF$date2 >  0] <- 1               #dummy£¬0 - Purchase only once, 1 - Purchase more than once


#The most recent purchase interval, recorded as date3
a <- as.Date("92-09-01")
DMEF$DATELP6 <- as.Date(DMEF$DATELP6, "%m/%d/%Y")
DMEF$date3 <- difftime(a, DMEF$DATEFP6, units = "days")
DMEF$date3 <- as.numeric(DMEF$date2/30)                           
hist(DMEF$date3)                                 


#Frequency
opar <- par(no.readonly = T)
par(mfrow=c(2,3))
hist(DMEF$ORDTYR, main = "Purchases this year")
hist(DMEF$ORDLYR, main = "Purchases last year")
hist(DMEF$ORD2AGO, main = "Purchases 2 years ago")
hist(DMEF$ORD3AGO, main = "Purchases 3 years ago")
hist(DMEF$ORD4AGO, main = "Purchases 4 years ago")
par(opar)

##Model Building---------------------------------------------------------------------------------
#Test set, training set preparation
DMEF$y1 <- 0
DMEF$y1[DMEF$TARGORD > 0] <- 1
 
set.seed(0821)
a <- sample(nrow(DMEF), 0.7*nrow(DMEF))
train    <- DMEF[a, ] 
validate <- DMEF[-a,]
table(train$y1)
table(validate$y1)


#LOGIT
fit.logit <- glm(y1 ~ date1 + date2 + ORDTYR + ORDLYR +ORD2AGO + ORD3AGO
                 + ORD4AGO, data = train, family = binomial())
fit.logit <- step(fit.logit)                                  

prob <- predict(fit.logit, validate, type="response")
logit.pred <- factor(prob > 0.5, levels = c(FALSE, TRUE),
                     labels = c("0", "1"))
logit.perf <- table(validate$y1, logit.pred,
                    dnn = c("Actual", "Predicted"))
logit.perf

#decision-making tree 1
library(rpart)
set.seed(0821)
dtree <- rpart(y1 ~ date1 + date2 + ORDTYR + ORDLYR + ORD2AGO + ORD3AGO
               + ORD4AGO, data = train, method = "class",
               parms=list(split="information"))
dtree$cptable
plotcp(dtree)           #CV

dtree.pred <- predict(dtree, validate, type = "class")
dtree.perf <- table(validate$y1, dtree.pred,
                    dnn = c("Actual", "Predicted"))
dtree.perf
library(rpart.plot)
prp(dtree,type=2, extra = 104)   #decision-making tree PLOT


#Decision-making Tree (pruning)
dtree.pruned <- prune(dtree, cp=0.0125)
dtree2.pred <- predict(dtree.pruned, validate, type = "class")
dtree2.perf <- table(validate$y1, dtree2.pred,
                    dnn = c("Actual", "Predicted"))
dtree2.perf
library(rpart.plot)
prp(dtree.pruned,type=2, extra = 104)   

opar <- par(no.readonly = T)
par(mfrow=c(1,2))
prp(dtree,type=2, extra = 104, main = "Decision-making Tree", tweak=1.2)
prp(dtree.pruned,type=2, extra = 104, main = "Decision-making Tree (pruning)", tweak=1.2) 
par(opar)

#random forest
library(randomForest)
set.seed(0821)
fit.forest <- randomForest(y1 ~ date1 + date2 + ORDTYR + ORDLYR + ORD2AGO + ORD3AGO
               + ORD4AGO, data = train, importance=T)

fit.forest
importance(fit.forest,type=2)

forest.pred <- predict(fit.forest, validate)
forest.perf <- table(validate$y1, round(forest.pred),
                     dnn = c("Actual", "Predicted"))
forest.perf


#CV-------------------------------------------------------------------------------------
performance <- function(table, n=2) {
  if (!all(dim(table) == c(2,2)))
    stop("Must be a 2X2 table")
  tn = table[1,1]
  fp = table[1,2]
  fn = table[2,1]
  tp = table[2,2]
  sensitivity = tp/(tp+fn)
  specificity = tn/(tn+fp)
  ppp = tp/(tp+fp)
  npp = tn/(tn+fn)
  hitrate = (tp+tn)/(tp+tn+fp+fn)
  result <- paste("Sensitivity = ", round(sensitivity, n),
                  "\nSpecificity = ", round(specificity, n),
                  "\nPositive Predictive Value = ", round(ppp, n),
                  "\nNegative Predictive Value = ", round(npp, n),
                  "\nAccuracy = ", round(hitrate, n), "\n", sep=" ")
  cat(result)
}


performance(logit.perf)
performance(dtree.perf)
performance(dtree2.perf)
performance(forest.perf)





