
install.packages("sfsmisc")
install.packages("fmsb")
install.packages("dplyr")


myPallete <- c("#f94144","#f3722c","#f8961e","#f9844a","#f9c74f","#90be6d","#43aa8b","#4d908e","#577590","#277da1",
               "#f72585","#b5179e","#7209b7","#560bad","#480ca8","#3a0ca3","#3f37c9","#4361ee","#4895ef","#4cc9f0",
               "#8ecae6","#219ebc","#023047","#ffb703","#fb8500",
               "#335c67","#fff3b0","#e09f3e","#9e2a2b","#540b0e",
               "#4f000b","#720026","#ce4257","#ff7f51","#ff9b54")

################################ general functions 

addVectorToList <- function (lst,v){
  lst <- c(lst, list(v))
  return(lst)
}

sd.p<-function(x){sd(x)*sqrt((length(x)-1)/length(x))}

rsq <- function (x, y) cor(x, y) ^ 2
##################################  code:

setwd("D:/PB/workingDir")
getwd()    


################################
## Figure 3:
################################ 

PlotPDFwithLogBins <- function(dataList,title, yTitle, dataNameVec){ # remove print
  myColors<- c("#c9061d", "black", "#2f99d6");
  mylLineTypes <-c(2,1,6,7,0);
  
  n<-length(dataList);
  
  ################## histo remove zero and log bins
  
  xAxis<- list();
  densityList<- list();
  vectorOfMaxX <- c(1)
  vectorOfMaxXAboveThreshold <- c(1)
  vectorOfMaxValues <- c(1)
  bins <- 100
  for(i in 1:n){
    vBefore <- unlist(dataList[i])
    vBefore <-sort(vBefore, decreasing = TRUE)
    pos <- which(vBefore > 0)
    lastInx <-max(pos)
    v<- vBefore[1:lastInx]
    h <- hist(v, breaks = exp(seq(log(min(v)), log(max(v)), len = bins)), plot=FALSE)
    densityList<- addVectorToList(densityList, h$density)
    xAxis<- addVectorToList(xAxis, h$mids)
    vectorOfMaxX[i] <- max(v);
    vectorOfMaxValues[i] <- max(h$density);
  }
  maxY <- max(vectorOfMaxValues);
  ################## scatter:
  
  expResVec <- NULL
  coefResVec <- NULL
  xValuesMax <- NULL
  legendNames <- NULL
  vectorOfMinValues <- NULL
  R2 <- NULL
  R3 <- NULL
  
  i<-1
  for(i in 1:n){
    a <- unlist(xAxis[i]);
    b <- unlist(densityList[i])
    inds <- which(b %in% 0)
    a=a[-inds]
    b=b[-inds]
    vectorOfMinValues[i] = min(b)
    dat = data.frame(x=a, y = b)
    dat = transform(dat, logx = log(x), logy = log(y))
    mod = lm(logy ~ logx, data = dat)
    expResVec[i] = exp(coef(mod)[1])
    coefResVec[i] = coef(mod)[2]
    vecy<- NULL;
    for( p in 1: length(a)){
      vecy[p]<-expResVec[i]* (a[p])^coefResVec[i]
    }
    R2[i] <- summary(mod)$r.squared;
    R3[i] <- cor(log(a), log(b))^2
    
    xValuesMax[i] = max(unlist(xAxis[i]));
  }
  minY <- min(vectorOfMinValues)
  
  maxX = max(xValuesMax);
  r2 = sprintf("%.2f", R2[1]);
  betaV = sprintf("%.2f", coefResVec[1]);
  legendNames[1] <- as.expression(bquote(beta==.(betaV) ~ ". " ~ R^2 ==.(r2)))
  
  fileName<- sprintf("%s discrete PDF log-log with trend.pdf",title)
  pdf(fileName)
  
  fileTitleA <- sprintf("D:/PBDATA/PDF/%s.csv", title)
  printMe <- data.frame(unlist(xAxis[1]), unlist(densityList[1]), unlist(xAxis[2]), unlist(densityList[2]), unlist(xAxis[3]), unlist(densityList[3]))
  write.csv(printMe, fileTitleA, row.names = FALSE)
  
  plot(unlist(xAxis[1]), unlist(densityList[1]), log="xy", xlab="",ylab="", pch = mylLineTypes[1],
       col=myColors[1], las=1,xaxt="n",yaxt="n", cex.axis = 1.2, xlim = c(1,maxX*1.1), ylim = c(minY / 1.1, maxY * 1.1));
  curve(expResVec[1]*x^(coefResVec[1]), from=1, to=(maxX*1.1), col = myColors[1], lty=2, add=TRUE)
  mtext(title, 1,line=2.5,cex=1.2)
  mtext("Probability Density",2,line=3,cex=1.2)
  for(i in 2:n){
    points(unlist(xAxis[i]), unlist(densityList[i]), col=myColors[i], pch = mylLineTypes[i])
    curve(expResVec[i]*x^(coefResVec[i]), from=1, to=maxX, col = myColors[i], lty=2, add=TRUE)
    r2 = sprintf("%.2f", R2[i]);
    betaV = sprintf("%.2f", coefResVec[i]);
    legendNames[i] <- as.expression(bquote(beta==.(betaV) ~ ". " ~ R^2 ==.(r2)))
  }
  eaxis(1,at=c(0.0001,0.001,0.01,0.1,1, 10, 100, 1000, 10000, 100000, 1000000),cex.axis=1.2)
  eaxis(2,at=c(10^-7, 10^-6, 10^-5,10^-4,0.001,0.01,0.1,1, 10, 100, 1000, 10000),cex.axis=1.2)
  legend("topright",inset=.02,legend=dataNameVec, col=myColors, pch = mylLineTypes, cex = 1.2)
  legend("bottomleft",inset=.02, legend=legendNames, col=myColors,lty=2, cex = 1.2)
  dev.off()
}

PlotPDFwithLogBinsNoTrend <- function(dataList,title, yTitle, dataNameVec){ # remove print
  myColors<- c("#c9061d", "black", "#2f99d6");
  mylLineTypes <-c(2,1,6,7,0);
  
  n<-length(dataList);
  
  ################## histo remove zero and log bins
  
  xAxis<- list();
  densityList<- list();
  vectorOfMaxX <- c(1)
  vectorOfMaxXAboveThreshold <- c(1)
  vectorOfMaxValues <- c(1)
  bins <- 100
  for(i in 1:n){
    vBefore <- unlist(dataList[i])
    vBefore <-sort(vBefore, decreasing = TRUE)
    pos <- which(vBefore > 0)
    lastInx <-max(pos)
    v<- vBefore[1:lastInx]
    h <- hist(v, breaks = exp(seq(log(min(v)), log(max(v)), len = bins)), plot=FALSE)
    densityList<- addVectorToList(densityList, h$density)
    xAxis<- addVectorToList(xAxis, h$mids)
    vectorOfMaxX[i] <- max(v);
    vectorOfMaxValues[i] <- max(h$density);
  }
  maxY <- max(vectorOfMaxValues);
  ################## scatter:
  
  xValuesMax <- NULL
  legendNames <- NULL
  vectorOfMinValues <- NULL

  
  i<-1
  for(i in 1:n){
    a <- unlist(xAxis[i]);
    b <- unlist(densityList[i])
    inds <- which(b %in% 0)
    a=a[-inds]
    b=b[-inds]
    vectorOfMinValues[i] = min(b)
    xValuesMax[i] = max(unlist(xAxis[i]));
  }
  minY <- min(vectorOfMinValues)
  maxX = max(xValuesMax);

  fileName<- sprintf("%s discrete PDF log-log no trend.pdf",title)
  pdf(fileName)
  
  fileTitleA <- sprintf("D:/PBDATA/PDF/%s.csv", title)
  printMe <- data.frame(unlist(xAxis[1]), unlist(densityList[1]), unlist(xAxis[2]), unlist(densityList[2]), unlist(xAxis[3]), unlist(densityList[3]))
  write.csv(printMe, fileTitleA, row.names = FALSE)
  
  plot(unlist(xAxis[1]), unlist(densityList[1]), log="xy", xlab="",ylab="", pch = mylLineTypes[1],
       col=myColors[1], las=1,xaxt="n",yaxt="n", cex.axis = 1.2, xlim = c(1,maxX*1.1), ylim = c(minY / 1.1, maxY * 1.1));
  mtext(title, 1,line=2.5,cex=1.2)
  mtext("Probability Density",2,line=3,cex=1.2)
  for(i in 2:n){
    points(unlist(xAxis[i]), unlist(densityList[i]), col=myColors[i], pch = mylLineTypes[i])
  }
  eaxis(1,at=c(0.0001,0.001,0.01,0.1,1, 10, 100, 1000, 10000, 100000, 1000000),cex.axis=1.2)
  eaxis(2,at=c(10^-7, 10^-6, 10^-5,10^-4,0.001,0.01,0.1,1, 10, 100, 1000, 10000),cex.axis=1.2)
  legend("topright",inset=.02,legend=dataNameVec, col=myColors, pch = mylLineTypes, cex = 1.2)
  dev.off()
}

PlotPDFwithLogBinsTrunc <- function(dataList, title, yTitle, dataNameVec, minX, maxX){
  myColors<- c("#c9061d", "black", "#2f99d6");
  mylLineTypes <-c(2,1,6,7,0);
  
  n<-length(dataList);
  vectorOfMaxValues <- c(1)
  
  ################## histo remove zero and log bins
  
  xAxis<- list();
  densityList<- list();
  vectorOfMaxX <- c(1)
  vectorOfMaxXAboveThreshold <- c(1)
  vectorOfMaxValues <- c(1)
  bins <- 100
  for(i in 1:n){
    vBefore <- unlist(dataList[i])
    vBefore <-sort(vBefore, decreasing = TRUE)
    pos <- which(vBefore > minX - 1)
    lastInx <-max(pos)
    pos <- which(vBefore < maxX + 1)
    firstInx <-min(pos)
    v<- vBefore[firstInx:lastInx]
    h <- hist(v, breaks = exp(seq(log(min(v)), log(max(v)), len = bins)), plot=FALSE)
    densityList<- addVectorToList(densityList, h$density)
    xAxis<- addVectorToList(xAxis, h$mids*15)
    vectorOfMaxX[i] <- max(v);
    vectorOfMaxValues <- max(h$density);
  }
  maxY = max(vectorOfMaxValues);
  
  ################## scatter:
  
  expResVec <- NULL
  coefResVec <- NULL
  xValuesMax <- NULL
  legendNames <- NULL
  R2 <- NULL
  
  for(i in 1:n){
    a <- unlist(xAxis[i]);
    b <- unlist(densityList[i])
    inds <- which(b %in% 0)
    a=a[-inds]
    b=b[-inds]
    dat = data.frame(x=a, y = b)
    dat = transform(dat, logx = log(x), logy = log(y))
    mod = lm(logy ~ logx, data = dat)
    expResVec[i] = exp(coef(mod)[1])
    coefResVec[i] = coef(mod)[2]
    R2[i] <- summary(mod)$r.squared;
    xValuesMax[i] = max(unlist(xAxis[i]));
  }
  
  maxX = max(xValuesMax);
  r2 = sprintf("%.2f", R2[1]);
  betaV = sprintf("%.2f", coefResVec[1]);
  legendNames[1] <- as.expression(bquote(beta==.(betaV) ~ ", " ~ R^2 ==.(r2) ~ " "))
  
  fileName<- sprintf("%s discrete PDF log-log with trend.pdf",title)
  pdf(fileName)
  
  plot(unlist(xAxis[1]), unlist(densityList[1]), log="xy", xlab="",ylab="", pch = 0,
       col=myColors[1], las=1,xaxt="n",yaxt="n")
  curve(expResVec[1]*x^(coefResVec[1]), from=1, to=maxX, col = myColors[1], lty=2, add=TRUE)
  mtext(title, 1,line=2.5,cex=1.2)
  mtext("Probability Density",2,line=3,cex=1.2)
  for(i in 2:n){
    points(unlist(xAxis[i]), unlist(densityList[i]), col=myColors[i], pch = i-1)
    curve(expResVec[i]*x^(coefResVec[i]), from=1, to=maxX, col = myColors[i], lty=2, add=TRUE)
    r2 = sprintf("%.2f", R2[i]);
    betaV = sprintf("%.2f", coefResVec[i]);
    legendNames[i] <- as.expression(bquote(beta==.(betaV) ~ ", " ~ R^2 ==.(r2)))
  }
  eaxis(1,at=c(0.0001,0.001,0.01,0.1,1, 10, 100, 1000, 10000, 100000, 1000000),cex.axis=1.2)
  eaxis(2,at=c(0.0001,0.001,0.01,0.1,1, 10, 100, 1000, 10000),cex.axis=1.2)
  legend("topright",inset=.02,legend=dataNameVec, col=myColors, pch = c(0,1,2), cex = 1.2)
  legend("bottomleft",inset=.02, legend=legendNames, col=myColors,lty=2, cex = 1.2)
  
  dev.off()
}

TLVTrunk <- read.csv("TLV_TrunkOverTime.csv", header = TRUE)
TLVignoreTrunk <- read.csv("TLVC_TrunkOverTime.csv", header = TRUE)
LondonTrunk <- read.csv("London_TrunkOverTime.csv", header = TRUE)

LondonTrunk <- transform(LondonTrunk, AvgCost = LondonTrunk$totalCost / LondonTrunk$count)
TLVTrunk <- transform(TLVTrunk, AvgCost = TLVTrunk$totalCost / TLVTrunk$count)
TLVignoreTrunk <- transform(TLVignoreTrunk, AvgCost = TLVignoreTrunk$totalCost / TLVignoreTrunk$count)


library (sfsmisc)
warnings()

dataNameVec<-c("London", "TLV", "TLV Center"); 

dataList<-list(A=LondonTrunk$AvgCost, B=TLVTrunk$AvgCost, C=TLVignoreTrunk$AvgCost)
PlotPDFwithLogBins(dataList,"Average total cost [HH]", "Cost [Human hours]", dataNameVec)
PlotPDFwithLogBinsNoTrend(dataList,"Average total cost [HH]", "Cost [Human hours]", dataNameVec)

dataList<-list(A=LondonTrunk$maxCost/60, B=TLVTrunk$maxCost/60, C=TLVignoreTrunk$maxCost/60)
PlotPDFwithLogBins(dataList, "Max momentary cost [HH]", "Cost [Human minutes]", dataNameVec)
PlotPDFwithLogBinsNoTrend(dataList, "Max momentary cost [HH]", "Cost [Human minutes]", dataNameVec)

dataList<-list(A=LondonTrunk$totalIterations, B=TLVTrunk$totalIterations, C=TLVignoreTrunk$totalIterations)
PlotPDFwithLogBinsTrunc(dataList,"Duration [minutes]",  "Iterations", dataNameVec, 2, 96)


################################
## Figures 4 a-c and SI 3 a-c
################################ 

samplesExample<-data

newDaysCostCorrelationBsideLegend <- function(samplesExample, city){
  numberOfRoads = length(samplesExample[,1]);
  samplesExample$trunkId<-NULL
  
  m <- matrix(0, ncol = 7, nrow = numberOfRoads)
  samplesCount <- data.frame(m)
  res <- matrix(0, ncol = 5, nrow = 6)
  
  for(i in 1:5){
    for(roadInx in 1:numberOfRoads){
      if(samplesExample[roadInx, i] >= 60){
        if(samplesExample[roadInx, i] <= 180){
          samplesCount[roadInx, 1] = samplesCount[roadInx, 1] + 1;
        } else if(samplesExample[roadInx,i] <= 600){
          samplesCount[roadInx, 2] = samplesCount[roadInx, 2] + 1;
        } else if(samplesExample[roadInx, i] <= 1800){
          samplesCount[roadInx, 3] = samplesCount[roadInx, 3] + 1;
        } else if(samplesExample[roadInx, i] <= 6000){
          samplesCount[roadInx, 4] = samplesCount[roadInx, 4] + 1;
        } else {
          samplesCount[roadInx, 5] = samplesCount[roadInx, 5] + 1;
        }
      } else {
        samplesCount[roadInx, 6] = samplesCount[roadInx, 6] + 1;
      }
    } 
  }
  
  samplesCount[,7] = 5- samplesCount[,6]
  
  res<-data.frame(matrix(0, nrow=6, ncol=6))
  
  for(i in 1:5){
    x<-as.data.frame(table(samplesCount[,i]))
    for (j in 1: length(x$Var1)){
      ag<-x$Var1[j]
      res[as.numeric(x$Var1[j]),i+1] <- x$Freq[j]
    }
  }
  res$X1 <- c(0,1,2,3,4,5)
  res <- res[-1, ]
  colnames(res) <- c("days", "H1", "H3", "H10", "H30", "H100")

  my6Colors = c("#7ef542", "#d1f542", "#f5f242", "#f58a42", "#f56042", "#2e9fdb");
  
  maxH = max(sum(res[,2]), sum(res[,3]), sum(res[,4]), sum(res[,5]), sum(res[,6]))
  yMax = ceiling(maxH / 500) * 500
  
  legendNames <- c("1 day ", "2 days", "3 days", "4 days", "5 days")
  cleanRes<-data.frame(res$H1, res$H3, res$H10, res$H30, res$H100)
  cleanRes<-as.matrix(cleanRes)
  counts <- as.numeric(colSums(cleanRes))
  mainTitle = sprintf("Cost correlations count-sideLegend - %s", city)
  
  fileName<- sprintf("%s.pdf", mainTitle)
  pdf(fileName)
  
  cleanResScaled <- scale(cleanRes, FALSE, colSums(cleanRes)) * 100
  par(mar=c(5.1, 4.1, 4.1, 6.6), xpd=TRUE)
  xx<-barplot(cleanResScaled, names.arg=c('1-3', ' 3-10', '10-30', '30-100', '>100'), ylim=c(0,110), col=my6Colors, ann=FALSE,axes=FALSE,
              main=mainTitle, border="black", xlab = "Number of returns", ylab = "Cost of Bottleneck [Human Hours]", cex.axis =1.2, cex.names = 1.1)
  axis(2,at=seq(0,100,20), cex.axis =1.2)
  text(x = xx, y = 105, label = counts, pos = 3, cex = 1.25)
  mtext("Bottlenecks breakdown by cost [HH]", 1,line=2.5,cex=1.2)
  mtext("Percentage of return days",2,line=2.5,cex=1.2)
 
  legend("right", legend = legendNames, inset = -0.23, fill = my6Colors, cex=1.15)
  
  dev.off()
}

newDaysCostCorrelationC <- function(samplesExample, city){
  numberOfRoads = length(samplesExample[,1]);
  samplesExample$trunkId<-NULL
  
  m <- matrix(0, ncol = 2, nrow = numberOfRoads)
  samplesCount <- data.frame(m)
  res <- matrix(0, ncol = 5, nrow = 6)
  
  for(i in 1:5){
    for(roadInx in 1:numberOfRoads){
      if(samplesExample[roadInx, i] >= 600){
        samplesCount[roadInx, 1] = samplesCount[roadInx, 1] + 1;
      }
      if(samplesExample[roadInx, i] >= 1800){
        samplesCount[roadInx, 2] = samplesCount[roadInx, 1] + 1;
      }
    } 
  }
  
  x1<-as.data.frame(table(samplesCount$X1))
  x2<-as.data.frame(table(samplesCount$X2))
  
  if(length(x1$Var1)<6){
    de<-data.frame(5,0)
    names(de)<-c("Var1","Freq")
    x1 <- rbind(x1, de)
  }
  
  res<-data.frame(x1$Var1, x1$Freq, x2$Freq)
  res <- res[-1, ]
  colnames(res) <- c("days", "H10", "H30")
  
  ###############
  
  my6Colors = c("#7ef542", "#d1f542", "#f5f242", "#f58a42", "#f56042", "#2e9fdb");
  
  maxH = max(sum(res[,2]), sum(res[,3]))
  yMax = ceiling(maxH / 500) * 500
  
  legendNames <- c("1 day", "2 days", "3 days", "4 days", "5 days")
  cleanRes<-data.frame(res$H10,res$H30)
  cleanRes<-as.matrix(cleanRes)
  counts <- as.numeric(colSums(cleanRes))
  mainTitle = sprintf("Cost correlations count - %s", city)
  
  fileName<- sprintf("aggregation %s.pdf", mainTitle)
  pdf(fileName)
  
  cleanResScaled <- scale(cleanRes, FALSE, colSums(cleanRes)) * 100
  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  xx<-barplot(cleanResScaled, names.arg=c( '>10 HH', '>30 HH'), ylim=c(0,110), col=my6Colors, ann=FALSE,axes=FALSE, cex.axis =1.2, cex.names =1.2,
              main=mainTitle, border="black", xlab = "Number of returns", ylab = "Cost of Bottleneck [Human Hours]")
  #axis(2,at=seq(0,yMax + 500,500))
  axis(2,at=seq(0,100,20))
  text(x = xx, y = 105, label = counts, pos = 3, cex = 1.15)
  mtext("Bottlenecks breakdown by cost", 1,line=2.5,cex=1.2)
  mtext("Percentage of return days",2,line=2.5,cex=1.2)
  
  legend("right", legend = legendNames, inset = -0.27, fill = my6Colors, cex=1.15)
  
  dev.off()
}

data <- read.csv("London_TrunksOTpDay_totalCost.csv", header = TRUE)
city<-'London'
newDaysCostCorrelationC(data, city)
newDaysCostCorrelationBsideLegend(data, city)

data <- read.csv("TLV_TrunksOTpDay_totalCost.csv", header = TRUE)
city<-'TLV'
newDaysCostCorrelationC(data, city)
newDaysCostCorrelationBsideLegend(data, city)

data <- read.csv("TLVC_TrunksOTpDay_totalCost.csv", header = TRUE)
city<-'TLV Center'
newDaysCostCorrelationC(data, city)
newDaysCostCorrelationBsideLegend(data, city)


################################
## Figures 4 d-f and SI 6
################################ 

library(fmsb)

printRadar <- function(data, city, isHours){
  
  divideBy <- 60
  if(isHours){
    divideBy <- 1
  }
  
  data$trunkId <- NULL
  data$totalCost <- NULL
  sumVec<-rowSums(data)
  data$sum <- sumVec
  data <- data[order(-data$sum),,drop=FALSE]
  data$sum <- NULL
  data <- head(data, n=10)
  
  data_for_radar<-data.frame(day1=data$day1, day5=data$day5, day4=data$day4, day3=data$day3, day2=data$day2)
  colnames(data_for_radar) <- c("day 1", "day 5", "day 4", "day 3", "day 2")
  maxValue <- max(data_for_radar)
  seg <- 5
  data_for_radar <- rbind(rep(maxValue, 7), rep(0, 7), data_for_radar)
  
  fileName<- sprintf("Radar of top 10 B - %s.pdf",city)
  pdf(fileName)
  
  radarchart(data_for_radar, 
             axistype = 1,
             axislabcol = "black",
             caxislabels = as.integer(seq(from = 0, to = maxValue/divideBy, by = (maxValue/seg/divideBy))),
             seg = seg,  # Number of axis segments
             pcol = myPallete,
             plty = 1, vlcex = 1.15, calcex = 1.15,
             plwd = 1)
  dev.off()
}

getDurationRes <- function (minHour, maxHour, allcurrentTrees, city){
  
  maxTrunkNum = 20000
  allcurrentTrees$day <- (floor(allcurrentTrees$iteration /96))+1
  allcurrentTrees$iteration <- allcurrentTrees$iteration %% 96
  allcurrentTrees$iteration <- floor(allcurrentTrees$iteration /4)
  cost <- allcurrentTrees$costOfThisIterOnly[(allcurrentTrees$iteration >= minHour) & (allcurrentTrees$iteration <= maxHour)]
  trunk <- allcurrentTrees$trunkId[(allcurrentTrees$iteration >= minHour) & (allcurrentTrees$iteration <= maxHour)]
  day<- allcurrentTrees$day[(allcurrentTrees$iteration >= minHour) & (allcurrentTrees$iteration <= maxHour)]
  trunkId<-trunk + day*maxTrunkNum
  df <- data.frame(cost, trunkId)
  groups <- split.data.frame(df,df$trunkId)
  res <- lapply(groups, "[[", "cost")
  costs <- sapply(res, sum)
  costs <- as.numeric(round(costs/60))
  names <- as.numeric(names(groups))
  trunkId <- names %% maxTrunkNum
  day <- floor(names/maxTrunkNum)
  
  m <- matrix(0, ncol = 7, nrow = max(trunkId))
  indices<-which(costs > 0)
  
  dfB <- data.frame(costs[indices], trunkId[indices], day[indices])
  for(i in 1:length(trunkId)){
    tr<-dfB$trunkId[i]
    d<-dfB$day[i]
    cost<-dfB$costs[i]
    m[tr,d] = cost
    m[tr,6] = tr
    m[tr,7] = m[tr,7]+cost
  }
  
  res<-m[m[,7] != 0, ]
  max(res[,7])
  return (res)
}

isHours <- FALSE
data <- read.csv("London_TrunksOTpDay_totalCost.csv", header = TRUE)
city<-' London'
printRadar(data, city, isHours)

data <- read.csv("TLV_TrunksOTpDay_totalCost.csv", header = TRUE)
city<-' TLV'
printRadar(data, city, isHours)

data <- read.csv("TLVC_TrunksOTpDay_totalCost.csv", header = TRUE)
city<-' TLV Center'
printRadar(data, city, isHours)

isHours <- TRUE;


allcurrentTrees <- read.csv("London_currentTrees_G.csv", header = TRUE)

data<- as.data.frame(getDurationRes(7, 8, allcurrentTrees, city))
city<-'London7-9'
data$V6<- NULL
colnames(data) <- c("day1", "day2", "day3", "day4", "day5", "totalCost")
printRadar(data, city, isHours)

data <- as.data.frame(getDurationRes(17, 20, allcurrentTrees, city))
city<-'London17-21'
data$V6<- NULL
colnames(data) <- c("day1", "day2", "day3", "day4", "day5", "totalCost")
printRadar(data, city, isHours)



allcurrentTrees <- read.csv("TLV_currentTrees_G.csv", header = TRUE)

data <- as.data.frame(getDurationRes(7, 8, allcurrentTrees, city))
city<-'TLV7-9'
data$V6<- NULL
colnames(data) <- c("day1", "day2", "day3", "day4", "day5", "totalCost")
printRadar(data, city, isHours)

data <- as.data.frame(getDurationRes(17, 20, allcurrentTrees, city))
city<-'TLV17-21'
data$V6<- NULL
colnames(data) <- c("day1", "day2", "day3", "day4", "day5", "totalCost")
printRadar(data, city, isHours)



allcurrentTrees <- read.csv("TLVC_currentTrees_G.csv", header = TRUE)

data <- as.data.frame(getDurationRes(7, 8, allcurrentTrees, city))
city<-'TLVCenter7-9'
data$V6<- NULL
colnames(data) <- c("day1", "day2", "day3", "day4", "day5", "totalCost")
printRadar(data, city, isHours)

data <- as.data.frame(getDurationRes(17, 20, allcurrentTrees, city))
city<-'TLVcenter17-21'
data$V6<- NULL
colnames(data) <- c("day1", "day2", "day3", "day4", "day5", "totalCost")
printRadar(data, city, isHours)


################################
## Figures 5 a-c 
################################

printRegression <-function(data, city){
  my4Colors <- c("#26a642", "#f2e018", "#f5a511", "#b80707", "black");
  
  data<-data[order(data$sumOfOwnCost),]
  
  ctrl<-data.frame(data$maxSimDist, data$medianSimDist, data$upTo100Count)
  #length(ctrl)
  groups <- split.data.frame(ctrl,ctrl$data.upTo100Count)
  resMax <- lapply(groups, "[[", "data.maxSimDist")
  ctrlMax <- sapply(resMax, mean)
  ctrlMax <- as.numeric(round(ctrlMax))
  ctrlMaxSD <- sapply(resMax, sd.p)
  ctrlMaxSD <- as.numeric(round(ctrlMaxSD))
  
  resMedian <- lapply(groups, "[[", "data.medianSimDist")
  ctrlMedian <- sapply(resMedian, mean)
  ctrlMedian <- as.numeric(round(ctrlMedian))
  ctrlMedianSD <- sapply(resMedian, sd.p)
  ctrlMedianSD <- as.numeric(round(ctrlMedianSD))
  
  ctrlX<-as.numeric(names(resMax))
  
  res<-cutToFour(data);
  
  ################
  ctrlMaxSDWONA<-ctrlMaxSD
  ctrlMaxSDWONA[is.na(ctrlMaxSDWONA)] <- 0
  maxY<-max(max(data$maxDist), max(ctrlMaxSDWONA+ ctrlMax))
  maxX<-max(data$upTo100Count)
  
  fileName<- sprintf("max distance To Bottlenecks Num-unifiedBank-%s.pdf", city)
  pdf(fileName)
  
  plot(y = ctrlMax/1000 , x= ctrlX, xlim=c(1,maxX*1.1), ylim=c(0,maxY*1.2/1000),las = 1, 
       xlab="",ylab="", pch = 1, cex = 1.2, cex.axis = 1.2)
  
  mtext("number of bottlenecks", 1,line=2.5,cex=1.2)
  mtext("max distance between bottlenecks [km]",2,line=2.5,cex=1.2)
  arrows(ctrlX, (ctrlMax+ctrlMaxSD)/1000, ctrlX, (ctrlMax-ctrlMaxSD)/1000, length=0.05, angle=90, code=3)
  
  for(i in 1:4){
    df<-res[[i]]
    points(y = df$maxDist/1000 , x= df$upTo100Count, col = my4Colors[i], pch = 1)
  }
  
  legend("topleft",inset=.02, legend=c("0%-50%", "50%-75%", "75%-90%", "90%-100%", "control"), col=my4Colors, pch = 1, cex = 1.2)
  dev.off()
  
  ########################################
  ctrlMedianSDWONA<-ctrlMedianSD
  ctrlMedianSDWONA[is.na(ctrlMedianSDWONA)] <- 0
  maxY<-max(max(data$medianDist), max(ctrlMedianSDWONA + ctrlMedian))
  maxX<-max(data$upTo100Count)
  
  fileName<- sprintf("median distance To Bottlenecks Num-unifiedBank-%s.pdf", city)
  pdf(fileName)
  
  plot(y = ctrlMedian/1000 , x= ctrlX, xlim=c(1,maxX*1.1), ylim=c(0,maxY*1.3/1000),las = 1, 
       xlab="",ylab="", pch = 1, cex = 1.2, cex.axis = 1.2)
  
  mtext("Number of bottlenecks for a link", 1,line=2.5,cex=1.2)
  mtext("Medain distance between bottlenecks [km]",2,line=2.5,cex=1.2)
  arrows(ctrlX, (ctrlMedian+ctrlMedianSD)/1000, ctrlX, (ctrlMedian-ctrlMedianSD)/1000, length=0.05, angle=90, code=3)
  
  for(i in 1:4){
    df<-res[[i]]
    points(y = df$medianDist/1000 , x= df$upTo100Count, col = my4Colors[i], pch = 1)
  }
  
  legend("topleft",inset=.02, legend=c("0%-50%", "50%-75%", "75%-90%", "90%-100%", "control"), col=my4Colors, pch = 1, cex = 1.2)
  dev.off()
}

cutToFour <- function (data){
  data$linkId<-NULL
  data$sumCostOfTrees <-NULL
  sizeOfChunk<-ceiling(length(data$maxDist)/20)
  seriesList <-list()
  
  seriesList[[1]]<-data[1:sizeOfChunk*10,]
  seriesList[[2]]<-data[(sizeOfChunk*10+1):(sizeOfChunk*15),]
  seriesList[[3]]<-data[(sizeOfChunk*15+1):(sizeOfChunk*18),]
  seriesList[[4]]<-data[(sizeOfChunk*18+1):length(data$maxDist),]
  return (seriesList)
}

data<- read.csv("pb-linksLondon.csv", header = TRUE)
city<-'London'
printRegression(data, city)

data<- read.csv("pb-linksTLV.csv", header = TRUE)
city<-'TLV'
printRegression(data, city)

data<- read.csv("pb-linksTLVC.csv", header = TRUE)
city<-'TLV center'
printRegression(data, city)

################################
## Figures 5 d-f 
################################

printProbHist <- function(data, param){
  
  my4Colors <- c("#FF0000", "#FF9900", "#FFE500", "#26a642");
  mylLineTypes <-c(2,1,6,7,0);
  
  colnames(data) <- c("values","prob")
  
  sizeOfChunk<-ceiling(length(data$prob)/20)
  
  vecTop<-data$prob[1:(sizeOfChunk*2)]
  VecSec<-data$prob[(sizeOfChunk*2+1):(sizeOfChunk*5)]
  VecTir<-data$prob[(sizeOfChunk*5+1):(sizeOfChunk*10)]
  vecLast<-data$prob[(sizeOfChunk*10+1):length(data$data.wtCost)]
  
  breaks <- c(0,0.2,0.4,0.6,0.8,1)
  
  resA<-hist(vecTop, breaks = breaks, plot = FALSE)
  resB<-hist(VecSec, breaks = breaks, plot = FALSE)
  resC<-hist(VecTir, breaks = breaks, plot = FALSE)
  resD<-hist(vecLast, breaks = breaks, plot = FALSE)
  
  counts<-rbind(resA$counts /length(vecTop), resB$counts /length(VecSec), resC$counts /length(VecTir), resD$counts /length(vecLast))
  
  fileName<- sprintf("Bottlenecks %s - evolution prob histo-%s.pdf", param, city)
  pdf(fileName)
  
  yText <- sprintf("Relative  %s", param)
  
  barplot(counts, col=my4Colors, cex.axis = 1.2, cex.names = 1.2,
          beside=TRUE, ylim=c(0,1), names.arg=c("0-0.2", "0.2-0.4", "0.4-0.6", "0.6-0.8","0.8-1"))
  mtext('Frequency ', 1,line=2.5,cex=1.2)
  mtext(yText,2,line=2.5,cex=1.2)
  axis(2, cex.axis =1.2)
  legend("topleft",inset=.02, legend=c( "90%-100%","75%-90%","50%-75%","0%-50%"), fill =my4Colors, cex = 1.2)
  
  dev.off()
}

data<- read.csv("trunk-evo-prob - London.csv", header = TRUE)
city<-'London'

dataTime <- data.frame(data$maxTime, data$timeProb)
dataTime<-dataTime[order(-dataTime$data.maxTime),]
dataCost <- data.frame(data$maxCost, data$costProb)
dataCost<-dataCost[order(-dataCost$data.maxCost),]

printProbHist(dataCost,'Cost')
printProbHist(dataTime,'Time')


data<- read.csv("trunk-evo-prob - TLV.csv", header = TRUE)
city<-'TLV'

dataTime <- data.frame(data$maxTime, data$timeProb)
dataTime<-dataTime[order(-dataTime$data.maxTime),]

dataCost <- data.frame(data$maxCost, data$costProb)
dataCost<-dataCost[order(-dataCost$data.maxCost),]

printProbHist(dataCost,'Cost')
printProbHist(dataTime,'Time')


data<- read.csv("trunk-evo-prob - TLVC.csv", header = TRUE)
city<-'TLV Center'

dataTime <- data.frame(data$maxTime, data$timeProb)
dataTime<-dataTime[order(-dataTime$data.maxTime),]

dataCost <- data.frame(data$maxCost, data$costProb)
dataCost<-dataCost[order(-dataCost$data.maxCost),]


printProbHist(dataCost,'Cost')
printProbHist(dataTime,'Time')

#####################################################################

                                # SI #

#####################################################################

################################
## Figures SI2 a-c 
################################

PlotPDFwithLogBins5Colors <- function(dataList,title, yTitle, dataNameVec){
  myColors<- c("#be0000", "black", "#2f99d6", "#ff0066", "#ff6600");
  mylLineTypes <-c(2,1,6,7,0);
  
  n<-length(dataList);
  vectorOfMaxValues <- c(1)
  
  ################## histo remove zero and log bins
  
  xAxis<- list();
  densityList<- list();
  vectorOfMaxX <- c(1)
  vectorOfMaxXAboveThreshold <- c(1)
  vectorOfMaxValues <- c(1)
  bins <- 100
  for(i in 1:n){
    vBefore <- unlist(dataList[i])
    vBefore <-sort(vBefore, decreasing = TRUE)
    pos <- which(vBefore > 0)
    lastInx <-max(pos)
    v<- vBefore[1:lastInx]
    h <- hist(v, breaks = exp(seq(log(min(v)), log(max(v)), len = bins)), plot=FALSE)
    densityList<- addVectorToList(densityList, h$density)
    xAxis<- addVectorToList(xAxis, h$mids)
    vectorOfMaxX[i] <- max(v);
    vectorOfMaxValues <- max(h$density);
  }
  maxY = max(vectorOfMaxValues);
  
  ################## scatter:
  
  expResVec <- NULL
  coefResVec <- NULL
  xValuesMax <- NULL
  legendNames <- NULL
  R2 <- NULL
  
  for(i in 1:n){
    a <- unlist(xAxis[i]);
    b <- unlist(densityList[i])
    inds <- which(b %in% 0)
    a=a[-inds]
    b=b[-inds]
    dat = data.frame(x=a, y = b)
    dat = transform(dat, logx = log(x), logy = log(y))
    mod = lm(logy ~ logx, data = dat)
    expResVec[i] = exp(coef(mod)[1])
    coefResVec[i] = coef(mod)[2]
    R2[i] <- summary(mod)$r.squared;
    xValuesMax[i] = max(unlist(xAxis[i]));
  }
  
  maxX = max(xValuesMax);
  r2 = sprintf("%.2f", R2[1]);
  betaV = sprintf("%.2f", coefResVec[1]);
  legendNames[1] <- as.expression(bquote(beta==.(betaV) ~ ", " ~ R^2 ==.(r2)))
  
  fileName<- sprintf("%s discrete PDF log-log with trend.pdf",title)
  pdf(fileName)
  
  plot(unlist(xAxis[1]), unlist(densityList[1]), log="xy", xlab="",ylab="", pch = mylLineTypes[1],
       col=myColors[1], las=1,xaxt="n",yaxt="n", cex.axis = 1.2)
  curve(expResVec[1]*x^(coefResVec[1]), from=1, to=maxX, col = myColors[1], lty=2, add=TRUE)
  mtext(yTitle, 1,line=3,cex=1.2)
  mtext("Probability density",2,line=3,cex=1.2)
  for(i in 2:n){
    points(unlist(xAxis[i]), unlist(densityList[i]), col=myColors[i], pch = mylLineTypes[i])
    curve(expResVec[i]*x^(coefResVec[i]), from=1, to=maxX, col = myColors[i], lty=2, add=TRUE)
    r2 = sprintf("%.2f", R2[i]);
    betaV = sprintf("%.2f", coefResVec[i]);
    legendNames[i] <- as.expression(bquote(beta==.(betaV) ~ ", " ~ R^2 ==.(r2)))
  }
  eaxis(1,at=c(10^-7, 10^-6, 10^-5,10^-4,0.001,0.01,0.1,1, 10, 100, 1000, 10000, 100000, 1000000),cex.axis=1.2)
  eaxis(2,at=c(10^-7, 10^-6, 10^-5,10^-4,0.001,0.01,0.1,1, 10, 100, 1000, 10000),cex.axis=1.2)
  legend("topright",inset=.02,legend=dataNameVec, col=myColors, pch = mylLineTypes, cex = 1.2)
  legend("bottomleft",inset=.02, legend=legendNames, col=myColors,lty=2, cex = 1.2)
  dev.off()
}

dataNameVec<-c("Day 1", "Day 2", "Day 3","Day 4", "Day 5"); 


data <- read.csv("London_TrunksOTpDay_maxCost.csv", header = TRUE)
dataList<-list(A=data$day1/60, B=data$day2/60, C=data$day3/60,
               D=data$day4/60, E=data$day5/60)
PlotPDFwithLogBins5Colors(dataList,'Trunk max cost per day London2','Max momentary cost [Human hours]', dataNameVec)


data <- read.csv("TLV_TrunksOTpDay_maxCost.csv", header = TRUE)
dataList<-list(A=data$day1/60, B=data$day2/60, C=data$day3/60,
               D=data$day4/60, E=data$day5/60)
PlotPDFwithLogBins5Colors(dataList,'Trunk Max Cost per Day TLV','Max momentary cost [Human hours]', dataNameVec)

data <- read.csv("TLVC_TrunksOTpDay_maxCost.csv", header = TRUE)
dataList<-list(A=data$day1/60, B=data$day2/60, C=data$day3/60,
               D=data$day4/60, E=data$day5/60)
PlotPDFwithLogBins5Colors(dataList,'Trunk Max Cost per Day TLV Part','Max momentary cost [Human hours]', dataNameVec)


################################
## Figures SI2 d-f
################################

library (dplyr)

GeometricSequence<-function(initial.value, discount.factor, max){
  
  vec<-integer()
  vec[1]<-initial.value
  i=1
  while (vec[length(vec)] < max) {
    vec[i+1]<-vec[i]*discount.factor
    i = i+1
  }
  return (vec)
}

getDurationResB <- function (minHour, maxHour, allcurrentTrees){
  
  maxTrunkNum = 20000
  allcurrentTrees$day <- (floor(allcurrentTrees$iteration /96))+1
  allcurrentTrees$iteration <- allcurrentTrees$iteration %% 96
  allcurrentTrees$iteration <- floor(allcurrentTrees$iteration /4)
  cost <- allcurrentTrees$costOfThisIterOnly[(allcurrentTrees$iteration >= minHour) & (allcurrentTrees$iteration <= maxHour)]
  trunk <- allcurrentTrees$trunkId[(allcurrentTrees$iteration >= minHour) & (allcurrentTrees$iteration <= maxHour)]
  day<- allcurrentTrees$day[(allcurrentTrees$iteration >= minHour) & (allcurrentTrees$iteration <= maxHour)]
  trunkId<-trunk + day*maxTrunkNum
  df <- data.frame(cost, trunkId)
  groups <- split.data.frame(df,df$trunkId)
  res <- lapply(groups, "[[", "cost")
  costs <- sapply(res, sum)
  costs <- as.numeric(round(costs/60))
  sortedCost <- sort(costs, decreasing = TRUE)
  indices<-which(sortedCost > 0)
  sortedCost<-sortedCost[indices]
  return (sortedCost)
}

printDurationsCost <- function (allcurrentTrees, city){
  
  mylLineTypes <-c(2,1,6,7,0, 13);
  my7Colors<-c("#f94144", "#f8961e", "#90be6d", "#43aa8b", "#277da1","#7209b7")
  allcurrentTrees$wTCost <- NULL
  allcurrentTrees$branchesCount  <- NULL
  allcurrentTrees$splits  <- NULL
  allcurrentTrees$loadedFor <- NULL
  allcurrentTrees$avgK <- NULL

  allcurrentTrees <- allcurrentTrees %>% filter(costOfThisIterOnly >= 60)
  head(allcurrentTrees, n=5)
  
  
  hours <- c('07:00-09:00', '09:00-12:00', '12:00-17:00', '17:00-21:00', '21:00-01:00', '01:00-07:00')
  hoursNum <-c(2,3,5,4,4,6)
  
  morningRushRes <- getDurationResB(7,8, allcurrentTrees)
  morningRes <- getDurationResB(9,11, allcurrentTrees)
  noonRes <- getDurationResB(12,16, allcurrentTrees)
  evningRes <- getDurationResB(17,20, allcurrentTrees)
  lateEvningRes <- getDurationResB(21,24, allcurrentTrees)
  nightRes <- getDurationResB(0,6, allcurrentTrees)
  
  seriesList <-list()
  seriesList<-addVectorToList(seriesList, morningRushRes)
  seriesList<-addVectorToList(seriesList, morningRes)
  seriesList<-addVectorToList(seriesList, noonRes)
  seriesList<-addVectorToList(seriesList, evningRes)
  seriesList<-addVectorToList(seriesList, lateEvningRes)
  seriesList<-addVectorToList(seriesList, nightRes)
  
  expResVec <- NULL
  coefResVec <- NULL
  xValuesMax <- NULL
  legendNames <- NULL
  xList <- list()
  yList <- list()
  maxX <- 0
  maxY <- 0
  minY <- 1
  R2 <- NULL
  
  for(i in 1:6){
    resData <- unlist(seriesList[i])
    breaks<- GeometricSequence(initial.value = 1, discount.factor = 2, max = max(resData*10))
    breaksMids<-breaks -0.5
    histo <- hist(resData, breaks = breaksMids, plot = FALSE)
    resY <- histo$density * hoursNum[i]
    resX <- histo$mids / hoursNum[i]
    indices<-which(resY > 0)
    resY<-resY[indices]
    resX<-resX[indices]
    
    yList<- addVectorToList(yList, resY)
    xList<- addVectorToList(xList, resX )
    maxX <- max(maxX, resX)
    maxY <- max(maxY, resY)
    minY <- min(minY, resY)
    
    dat = data.frame(x=resX, y = resY)
    dat = transform(dat, logx = log(x), logy = log(y))
    mod = lm(logy ~ logx, data = dat)
    expResVec[i] = exp(coef(mod)[1])
    coefResVec[i] = coef(mod)[2]
    R2[i] <- summary(mod)$r.squared;
  }
  
  mainTitle<- "Density per hour"
  fileName<- sprintf("%s %s (log-log).pdf",city, mainTitle)
  pdf(fileName)
  
  plot(y = unlist(yList[1]), x = unlist(xList[1]), xlim = c(0.1, maxX*2), ylim = c(minY/2, maxY*2),
       xaxt="n",yaxt="n", las = 1, log = "xy",
       xaxs="i", yaxs="i",
       xlab="",ylab="", col = my7Colors[1],  pch = mylLineTypes[1]
  )
  curve(expResVec[1]*x^(coefResVec[1]), from=min( unlist(xList[1])), to=max( unlist(xList[1])), col = my7Colors[1], lty=2, add=TRUE)
  eaxis(1,at=c(10^-3,10^-2,10^-1, 1,10,10^2,10^3, 10^4),cex.axis=1.2, outer.at = FALSE)
  eaxis(2,at=c(10^-6, 10^-5, 10^-4, 10^-3,10^-2,10^-1,1,10,10^2,10^3, 10^4),cex.axis=1.2, outer.at = FALSE)
  mtext("Human Hours per hour(bins)", 1,line=2.5,cex=1.2)
  mtext("Density",2,line=3,cex=1.2)
  r2 = sprintf("%.2f", R2[1]);
  betaV = sprintf("%.2f", coefResVec[1])
  legendNames[1] <- as.expression(bquote(.(hours[1])~","~beta==.(betaV) ~ ", " ~ R^2 ==.(r2)))
  for(i in 2:6){
    points(x=unlist(xList[i]), y=unlist(yList[i]), col = my7Colors[i], pch = mylLineTypes[i])
    curve(expResVec[i]*x^(coefResVec[i]), from=min( unlist(xList[i])), to=max( unlist(xList[i])), col = my7Colors[i], lty=2, add=TRUE)
    r2 = sprintf("%.2f", R2[i]);
    betaV = sprintf("%.2f", coefResVec[i])
    legendNames[i] <- as.expression(bquote(.(hours[i])~","~beta==.(betaV) ~ ", " ~ R^2 ==.(r2)))
  }
  legend("topright",inset=.02, legend=legendNames, col=my7Colors,lty=2) #lty=2
  dev.off()
}

data <- read.csv("London_currentTrees_G.csv", header = TRUE)
city<-'London'
printDurationsCost(data, city)

data <- read.csv("TLV_currentTrees_G.csv", header = TRUE)
city<-'TLV'
printDurationsCost(data, city)

data <- read.csv("TLVC_currentTrees_G.csv", header = TRUE)
city<-'TLV Part'
printDurationsCost(data, city)


################################
## Figures SI3
################################

samplesExample<- data

daysDurationCorrelationBsideLegend <- function(samplesExample, city){
  numberOfRoads = length(samplesExample[,1]);
  samplesExample$trunkId<-NULL
  
  m <- matrix(0, ncol = 6, nrow = numberOfRoads)
  samplesCount <- data.frame(m)
  res <- matrix(0, ncol = 5, nrow = 6)
  
  for(i in 1:5){
    for(roadInx in 1:numberOfRoads){
      if(samplesExample[roadInx, i] >= 4){
        if(samplesExample[roadInx, i] <= 12){
          samplesCount[roadInx, 1] = samplesCount[roadInx, 1] + 1;
        } else if(samplesExample[roadInx,i] <= 20){
          samplesCount[roadInx, 2] = samplesCount[roadInx, 2] + 1;
        } else if(samplesExample[roadInx, i] <= 28){
          samplesCount[roadInx, 3] = samplesCount[roadInx, 3] + 1;
        } else {
          samplesCount[roadInx, 4] = samplesCount[roadInx, 4] + 1;
        }
      } else {
        samplesCount[roadInx, 5] = samplesCount[roadInx, 5] + 1;
      }
    } 
  }
  
  samplesCount[,6] = 5- samplesCount[,5]
  
  head(samplesCount, n=5)
#########################################
  
  m <- matrix(0, ncol = 5, nrow = 7)

  m[,1] <- c(0,1,2,3,4,5,6)
  for(i in 1:4){
    vec<-hist(samplesCount[,i], breaks=c(-1,0,1,2,3,4,5,6))
    m[,i+1] <- vec$counts
  }
  
  m <- m[-c(1,7),]
  m <- m[,-c(1)]
  
  cleanRes <- t(m)
  sums <- colSums (cleanRes, dims = 1)
  
  for(i in 1:5){
    cleanRes[,i] <- cleanRes[,i]/sums[i]
  }
  
  
  mainTitle = sprintf("Bottleneck Duration - %s", city)
  fileName<- sprintf("%s.pdf", mainTitle)
  pdf(fileName)
  
  my6Colors = c("#7ef542", "#d1f542", "#f58a42", "#b50e0e");
  legendNames <- c("1-3H", "3-5H", "5-7H", " >7H")
  
  par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
  xx<-barplot(cleanRes, names.arg=c('1 day', '2 days', '3 days', '4 days', '5 days'), ylim=c(0,1.1), col=my6Colors, ann=FALSE,axes=FALSE,
              main=mainTitle, border="black", cex.axis =1.2, cex.names =1.2, bty='L')
  axis(2,at=seq(0,1,0.2), cex.axis =1.2)
  text(x = xx, y = 1.05, label = sums, pos = 3, cex = 1.25)
  mtext("Bottlenecks breakdown by duration", 1,line=2.5,cex=1.3)
  mtext("Percenatge of the JT duration",2,line=2.5,cex=1.3)
  
  temp <- legend("right", legend = c(" "," "," "," "), inset = -0.23, bg = 'white',
                 text.width = strwidth("300000"), fill = my6Colors, xjust = 1, yjust = 1,
  )
  text(temp$rect$left + temp$rect$w, temp$text$y,
       legendNames, pos = 2, cex=1.2)
  
  dev.off()
}


data<- read.csv("London_TrunksOTpDay_totalIterations.csv", header = TRUE)
city<-'London'
daysDurationCorrelationBsideLegend(data, city)

data<- read.csv("TLV_TrunksOTpDay_totalIterations.csv", header = TRUE)
city<-'TLV'
daysDurationCorrelationBsideLegend(data, city)

data<- read.csv("TLVC_TrunksOTpDay_totalIterations.csv", header = TRUE)
city<-'TLVCenter'
daysDurationCorrelationBsideLegend(data, city)


################################
## Figures SI 4-5
################################

calcHeadCorVecSpearman<- function(cityDF, headPercent){
  
  cityDF$trunkId <- NULL
  cityDF$totalCost <- NULL
  sd <- double()
  y <- double()
  x <- double()
  l<- length(cityDF$day1)
  n<- ceiling(l * headPercent / 100)
  binSize = 25;
  bins <- ceiling(n/binSize)
  
  for(i in 1:bins){
    df <- head(cityDF, n=binSize*i)
    corsVec <- double()
    inx = 1;
    for(m in 1:4){
      for(n in (m+1):5){
        corsVec[inx] = cor(df[,m], df[,n], method = "spearman")
        inx = inx + 1
      }
    }
    sd[i] = sd.p(corsVec)
    y[i] = mean(corsVec)
    x[i] = round(binSize*i/l*100)
  }
  
  return (df <- data.frame(x, y, sd))
}

calcHeadCorVecSpearmanNoD3<- function(cityDF, headPercent){
  
  cityDF$trunkId <- NULL
  cityDF$totalCost <- NULL
  cityDF$day3 <- NULL
  sd <- double()
  y <- double()
  x <- double()
  l<- length(cityDF$day1)
  n<- ceiling(l * headPercent / 100)
  binSize = 25;
  bins <- ceiling(n/binSize)
  
  for(i in 1:bins){
    df <- head(cityDF, n=binSize*i)
    corsVec <- double()
    inx = 1;
    for(m in 1:3){
      for(n in (m+1):4){
        corsVec[inx] = cor(df[,m], df[,n], method = "spearman")
        inx = inx + 1
      }
    }
    sd[i] = sd.p(corsVec)
    y[i] = mean(corsVec)
    x[i] = round(binSize*i/l*100)
  }
  
  return (df <- data.frame(x, y, sd))
} # day 3 is Feb 14 - looks different

calcTailCorVecPearson<- function(cityDF, tailPercent){
  
  cityDF$trunkId <- NULL
  cityDF$totalCost <- NULL
  y <- double()
  x <- double()
  sd <- double()
  l<- length(cityDF$day1)
  n<- ceiling(l * tailPercent / 100)
  binSize = 25;
  bins <- ceiling(n/binSize)
  
  for(i in 1:bins){
    df <- tail(cityDF, n=binSize*i)
    corsVec <- double()
    inx = 1;
    for(m in 1:4){
      for(n in (m+1):5){
        corsVec[inx] = cor(df[,m], df[,n], method = "pearson")
        inx = inx + 1
      }
    }
    sd[i] = sd.p(corsVec)
    y[i] = mean(corsVec)
    x[i] = round(binSize*i/l*100)
  }
  
  return (df <- data.frame(x,y, sd))
}

calcTailCorVecPearsonNoD3<- function(cityDF, tailPercent){
  
  cityDF$trunkId <- NULL
  cityDF$totalCost <- NULL
  cityDF$day3 <- NULL
  y <- double()
  x <- double()
  sd <- double()
  l<- length(cityDF$day1)
  n<- ceiling(l * tailPercent / 100)
  binSize = 25;
  bins <- ceiling(n/binSize)
  
  for(i in 1:bins){
    df <- tail(cityDF, n=binSize*i)
    corsVec <- double()
    inx = 1;
    for(m in 1:3){
      for(n in (m+1):4){
        corsVec[inx] = cor(df[,m], df[,n], method = "pearson")
        inx = inx + 1
      }
    }
    sd[i] = sd.p(corsVec)
    y[i] = mean(corsVec)
    x[i] = round(binSize*i/l*100)
  }
  
  return (df <- data.frame(x,y, sd))
} # day 3 is Feb 14 - looks different

calcHeadCorVecPearson<- function(cityDF, headPercent){
  
  cityDF$trunkId <- NULL
  cityDF$totalCost <- NULL
  y <- double()
  x <- double()
  sd <- double()
  l<- length(cityDF$day1)
  n<- ceiling(l * headPercent / 100)
  binSize = 25;
  bins <- ceiling(n/binSize)
  
  for(i in 1:bins){
    df <- head(cityDF, n=binSize*i)
    corsVec <- double()
    inx = 1;
    for(m in 1:4){
      for(n in (m+1):5){
        corsVec[inx] = cor(df[,m], df[,n], method = "pearson")
        inx = inx + 1
      }
    }
    sd[i] = sd.p(corsVec)
    y[i] = mean(corsVec)
    x[i] = binSize*i/l*100
  }
  
  return (df <- data.frame(x,y, sd))
}

calcHeadCorVecPearsonNoD3<- function(cityDF, headPercent){
  
  cityDF$trunkId <- NULL
  cityDF$totalCost <- NULL
  cityDF$day3 <- NULL
  y <- double()
  x <- double()
  sd <- double()
  l<- length(cityDF$day1)
  n<- ceiling(l * headPercent / 100)
  binSize = 25;
  bins <- ceiling(n/binSize)
  
  for(i in 1:bins){
    df <- head(cityDF, n=binSize*i)
    corsVec <- double()
    inx = 1;
    for(m in 1:3){
      for(n in (m+1):4){
        corsVec[inx] = cor(df[,m], df[,n], method = "pearson")
        inx = inx + 1
      }
    }
    sd[i] = sd.p(corsVec)
    y[i] = mean(corsVec)
    x[i] = binSize*i/l*100
  }
  
  return (df <- data.frame(x,y, sd))
} # day 3 is Feb 14 - looks different

calcPearsonByMyForm <- function(cityDF){
  
  l<- length(cityDF$day1)
  cityDF$trunkId <- NULL
  cityDF$totalCost <- NULL
  y <- double()
  x <- double()
  ylim = c(-1,1)
  maxX = l
  for(i in 1:maxX){
    
    per <- i / (i+1)
    n <- ceiling(l*per)
    dfTail <- tail(cityDF, n=n)
    cTail<-cor(dfTail, method = "pearson")
    sTail<-sum(cTail)
    s2Tail<-(sTail-5)/ 20
    x[i]<-per*100
    y[i] = s2Tail
  }
  return (df <- data.frame(x,y))
  
}

printHeadOfOneSeriesSpearman<- function(trunkPerDay, city){
  percent <- 100
  dfA <- calcHeadCorVecSpearman(trunkPerDay, percent);
  fileName<- sprintf("Spearman correlation of top data by bins - %s.pdf",city)
  pdf(fileName)
  sdPlus <- dfA$y+dfA$sd
  sdMinus <- dfA$y-dfA$sd
  
  
  plot(x=dfA$x, y=dfA$y, xlab="",ylab="",log="x", pch = mylLineTypes[1], col=myColors[1], las=1, ylim = c(0.02,0.7), cex.axis = 1.2)
  mtext("Head percent", 1,line=2.5,cex=1.2)
  mtext("Spearman correlation +/- sd",2,line=3,cex=1.2)
  arrows(dfA$x, sdPlus, dfA$x, sdMinus, length=0.05, angle=90, code=3, col = myColors[1])
  #legend("topright",inset=.02,legend=c("London",  "TLV", "TLV Center"),col=myColors, pch = mylLineTypes)
  dev.off()
}

printHeadOfOneSeriesSpearmanNoD3<- function(trunkPerDay, city){
  percent <- 100
  dfA <- calcHeadCorVecSpearmanNoD3(trunkPerDay, percent);
  fileName<- sprintf("Spearman correlation of top data by bins - %s.pdf",city)
  pdf(fileName)
  sdPlus <- dfA$y+dfA$sd
  sdMinus <- dfA$y-dfA$sd
  
  
  plot(x=dfA$x, y=dfA$y, xlab="",ylab="",log="x", pch = mylLineTypes[1], col=myColors[1], las=1, ylim = c(0.02,0.7), cex.axis = 1.2)
  mtext("Head percent", 1,line=2.5,cex=1.2)
  mtext("Spearman correlation +/- sd",2,line=3,cex=1.2)
  arrows(dfA$x, sdPlus, dfA$x, sdMinus, length=0.05, angle=90, code=3, col = myColors[1])
  #legend("topright",inset=.02,legend=c("London",  "TLV", "TLV Center"),col=myColors, pch = mylLineTypes)
  dev.off()
} # day 3 is Feb 14 - looks different

printHeadAllPearson <- function(percent){
  
  myColors<- c("#be0000", "black", "#03adfc", "#ff0066", "#ff6600");
  mylLineTypes <-c(2,1,6,7,0);
  
  dfA <- calcHeadCorVecPearson(LondonTrunkPerDay, percent);
  dfB <- calcHeadCorVecPearson(TLVTrunkPerDay, percent);
  dfC <- calcHeadCorVecPearsonNoD3(TLVCenterTrunkPerDay, percent);
  
  fileName<- sprintf("Pearson correlation of top %s data by bins.pdf",percent)
  pdf(fileName)
  
  plot(x=dfA$x, y=dfA$y,xlab="",ylab="", pch = mylLineTypes[1], col=myColors[1], las=1, ylim = c(0.1,0.9), cex.axis = 1.2)
  mtext("Head percent", 1,line=2.5,cex=1.2)
  mtext("Pearson correlation",2,line=3,cex=1.2)
  #arrows(dfA$x, dfA$y+dfA$sd, dfA$x, dfA$y-dfA$sd, length=0.05, angle=90, code=3, col = myColors[1])
  points(dfB,xlab="",ylab="", pch = mylLineTypes[2],
         col=myColors[2], las=1)
  #arrows(dfB$x, dfB$y+dfB$sd, dfB$x, dfB$y-dfB$sd, length=0.05, angle=90, code=3, col = myColors[2])
  points(dfC,xlab="",ylab="", pch = mylLineTypes[3],
         col=myColors[3], las=1)
  #arrows(dfC$x, dfC$y+dfC$sd, dfC$x, dfC$y-dfC$sd, length=0.05, angle=90, code=3, col = myColors[3])
  legend("bottomright",inset=.02,legend=c("London",  "TLV", "TLV Center"),col=myColors, pch = mylLineTypes, cex = 1.2)
  dev.off()
}

printTailPearson <- function(trunkPerDay, city){
  
  myColors<- c("#be0000", "black", "#122878", "#ff0066", "#ff6600");
  mylLineTypes <-c(2,1,6,7,0);
  
  dfA <- calcTailCorVecPearson(trunkPerDay, 100);
  
  
  fileName<- sprintf("Pearson correlation of tail percent data by bins - %s.pdf", city)
  pdf(fileName)
  
  plot(x=dfA$x, y= dfA$y, xlab="",ylab="", pch = mylLineTypes[1], col=myColors[1], las=1, ylim = c(-0.4,1), cex = 1.2)
  mtext("Tail percent", 1,line=2.5,cex=1.2)
  mtext("Pearson correlation",2,line=3,cex=1.2)
  arrows(dfA$x, dfA$y+dfA$sd, dfA$x, dfA$y-dfA$sd, length=0.05, angle=90, code=3, col = myColors[1])
  abline(h = 0.5, lty = 3, col = "gray")
  dev.off()
}

printTailPearsonNoD3 <- function(trunkPerDay, city){
  
  myColors<- c("#be0000", "black", "#122878", "#ff0066", "#ff6600");
  mylLineTypes <-c(2,1,6,7,0);
  
  dfA <- calcTailCorVecPearsonNoD3(trunkPerDay, 100);
  
  
  fileName<- sprintf("Pearson correlation of tail percent data by bins - %s.pdf", city)
  pdf(fileName)
  
  plot(x=dfA$x, y= dfA$y, xlab="",ylab="", pch = mylLineTypes[1], col=myColors[1], las=1, ylim = c(-0.4,1), cex = 1.2)
  mtext("Tail percent", 1,line=2.5,cex=1.2)
  mtext("Pearson correlation",2,line=3,cex=1.2)
  arrows(dfA$x, dfA$y+dfA$sd, dfA$x, dfA$y-dfA$sd, length=0.05, angle=90, code=3, col = myColors[1])
  dev.off()
} # day 3 is Feb 14 - looks different

printHeadAllSparman <- function(percent){
  
  myColors<- c("#be0000", "black", "#03b6fc", "#ff0066", "#ff6600");
  mylLineTypes <-c(2,1,6,7,0);
  
  dfA <- calcHeadCorVecSpearman(LondonTrunkPerDay, percent);
  dfB <- calcHeadCorVecSpearman(TLVTrunkPerDay, percent);
  dfC <- calcHeadCorVecSpearmanNoD3(TLVCenterTrunkPerDay, percent);
  
  fileName<- sprintf("Spearman correlation of top %s data.pdf",percent)
  pdf(fileName)
  
  plot(x=dfA$x, y=dfA$y, xlab="",ylab="", pch = mylLineTypes[1], col=myColors[1], las=1, ylim = c(0.02,0.7), cex.axis = 1.2)
  mtext("Head percent", 1,line=2.5,cex=1.2)
  mtext("Spearman correlation",2,line= 3,cex=1.2)
  #arrows(dfB$x, dfB$y-dfB$sd, dfB$x, dfB$y+dfB$sd, length=0.05, angle=90, code=3, col = myColors[1])
  points(x=dfB$x, y=dfB$y,xlab="",ylab="", pch = mylLineTypes[2],
         col=myColors[2], las=1)
  #arrows(dfB$x, dfB$y-dfB$sd, dfB$x, dfB$y+dfB$sd, length=0.05, angle=90, code=3, col = myColors[2])
  points(x=dfC$x, y=dfC$y,xlab="",ylab="", pch = mylLineTypes[3],
         col=myColors[3], las=1)
  #arrows(dfC$x, dfC$y-dfC$sd, dfC$x, dfC$y+dfC$sd, length=0.05, angle=90, code=3, col = myColors[3])
  #dfD<-head(dfC, n=2)
  #arrows(dfD$x, dfD$y-dfD$sd, dfD$x, dfD$y+dfD$sd, length=0.05, angle=90, code=3, col = myColors[3])
  legend("topright",inset=.02,legend=c("London",  "TLV", "TLV Center"),col=myColors, pch = mylLineTypes, cex = 1.2)
  dev.off()
}

printTailAll <- function(percent){
  
  myColors<- c("#be0000", "black", "#03b6fc", "#ff0066", "#ff6600");
  mylLineTypes <-c(2,1,6,7,0);
  
  dfA <- calcTailCorVecPearson(LondonTrunkPerDay, percent);
  dfB <- calcTailCorVecPearson(TLVTrunkPerDay, percent);
  dfC <- calcTailCorVecPearsonNoD3(TLVCenterTrunkPerDay, percent);
  
  fileName<- sprintf("Pearson correlation of tail percent data by bins.pdf")
  pdf(fileName)
  
  plot(x=dfA$x, y= dfA$y, xlab="",ylab="", pch = mylLineTypes[1], col=myColors[1], las=1, ylim = c(-0.4,1), cex.axis = 1.2)
  mtext("Tail percent", 1,line=2.5,cex=1.2)
  mtext("Pearson correlation",2,line=3,cex=1.2)
  # arrows(dfA$x, dfA$y+dfA$sd, dfA$x, dfA$y-dfA$sd, length=0.05, angle=90, code=3, col = myColors[1])
  points(dfB,xlab="",ylab="", pch = mylLineTypes[2],
         col=myColors[2], las=1)
  # arrows(dfB$x, dfB$y+dfB$sd, dfB$x, dfB$y-dfB$sd, length=0.05, angle=90, code=3, col = myColors[2])
  points(dfC,xlab="",ylab="", pch = mylLineTypes[3],
         col=myColors[3], las=1)
  legend("topleft",inset=.02,legend=c("London",  "TLV", "TLV Center"),col=myColors, pch = mylLineTypes, cex = 1.2)
  # arrows(dfC$x, dfC$y+dfC$sd, dfC$x, dfC$y-dfC$sd, length=0.05, angle=90, code=3, col = myColors[3])
  dev.off()
}

printPearsonAllMyForm <- function(percent){
  
  myColors<- c("#be0000", "black", "#122878", "#ff0066", "#ff6600");
  mylLineTypes <-c(2,1,6,7,0);
  
  dfA <- calcPearsonByMyForm(LondonTrunkPerDay);
  dfB <- calcPearsonByMyForm(TLVTrunkPerDay);
  dfC <- calcPearsonByMyForm(TLVCenterTrunkPerDay);
  
  fileName<- sprintf("Pearson correlation of tail data by i-(i+1).pdf")
  pdf(fileName)
  
  plot(dfA, xlab="",ylab="", pch = mylLineTypes[1], col=myColors[1], las=1, ylim = c(-0.4,1))
  mtext("tail percent", 1,line=2.5,cex=1)
  mtext("Pearson correlation",2,line=2.5,cex=1)
  points(dfB,xlab="",ylab="", pch = mylLineTypes[2],
         col=myColors[2], las=1)
  points(dfC,xlab="",ylab="", pch = mylLineTypes[3],
         col=myColors[3], las=1)
  legend("topleft",inset=.02,legend=c("London",  "TLV", "TLV Center"),col=myColors, pch = mylLineTypes)
  dev.off()
  
  fileName<- sprintf("Pearson correlation of tail data by i-(i+1) log-log.pdf")
  pdf(fileName)
  
  plot(dfA, xlab="",ylab="", pch = mylLineTypes[1], col=myColors[1], las=1, log = "xy", xlim = c(60,100))
  mtext("tail percent", 1,line=2.5,cex=1)
  mtext("Pearson correlation",2,line=2.5,cex=1)
  points(dfB,xlab="",ylab="", pch = mylLineTypes[2], log = "xy", col=myColors[2], las=1)
  points(dfC,xlab="",ylab="", pch = mylLineTypes[3], log = "xy", col=myColors[3], las=1)
  legend("topleft",inset=.02,legend=c("London",  "TLV", "TLV Center"),col=myColors, pch = mylLineTypes)
  dev.off()
}


mylLineTypes <-c(2,1,6,7,0, 13);
myColors<- c("#c9061d", "black", "#2f99d6");


LondonTrunkPerDay <- read.csv("London_TrunksOTpDay_totalCost.csv", header = TRUE)
LondonTrunkPerDay$totalCost <- rowSums(LondonTrunkPerDay[1:5])
LondonTrunkPerDay<- LondonTrunkPerDay[order(-LondonTrunkPerDay$totalCost),,drop=FALSE]
city<-"London"
printHeadOfOneSeriesSpearman(LondonTrunkPerDay, city)
printTailPearson(LondonTrunkPerDay, city)


TLVTrunkPerDay <- read.csv("TLV_TrunksOTpDay_totalCost.csv", header = TRUE)
TLVTrunkPerDay$totalCost <- rowSums(TLVTrunkPerDay[1:5])
TLVTrunkPerDay<- TLVTrunkPerDay[order(-TLVTrunkPerDay$totalCost),,drop=FALSE]
city<-"TLV"
printHeadOfOneSeriesSpearman(TLVTrunkPerDay, city)
printTailPearson(TLVTrunkPerDay, city)

TLVCenterTrunkPerDay <- read.csv("TLVC_TrunksOTpDay_totalCost.csv", header = TRUE)
TLVCenterTrunkPerDay$totalCost <- rowSums(TLVCenterTrunkPerDay[1:5])
TLVCenterTrunkPerDay<- TLVCenterTrunkPerDay[order(-TLVCenterTrunkPerDay$totalCost),,drop=FALSE]
city<-"TLV Center"
printHeadOfOneSeriesSpearmanNoD3(TLVCenterTrunkPerDay, city)
printTailPearsonNoD3(TLVCenterTrunkPerDay, city)


printHeadAllPearson(100)
printTailAll(100)
printHeadAllSparman(100)
########################################### End
