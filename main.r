####################
#load data
dn = list(paste("Y", as.character(1949:1960), sep = ""), month.abb)
airmat = matrix(AirPassengers, 12, byrow = TRUE, dimnames = dn)
air = as.data.frame(t(airmat))
##erwtisi 1 #mean
print(mean(air$Y1951)) 
##erwtisi 2 #maximum]
range(air['Jan',])
range(air['Feb',])
#air$max<-apply(air, 1, max)
### erwtisi 3 correlation
cor(air$Y1952,air$Y1951)
cor(air$Y1953,air$Y1951)
cor(air$Y1954,air$Y1951)
## erwtisi 4 correlation IDK
cor(as.numeric(air['Jan',]),as.numeric(air['Nov',]))
cor(as.numeric(air['Feb',]),as.numeric(air['Nov',]))
## erwtisi 5
sums = colSums(air[,-1])
plot(sums)

####################
setwd('D:\\Auth\\DDPMS\\1ο εξάμηνο\\Big Data')
df = read.csv('car_insurance.csv', stringsAsFactors = TRUE)
#GINI INDEXES IOU
data = read.csv('car_insurance.csv', header = TRUE, stringsAsFactors = TRUE)
absfreqB = table(data[, c(4, 5)])
freqb = prop.table(absfreqB, 1)
freqSumB = rowSums(prop.table(absfreqB))
Gini_low = 1 - freqb["Low","No"]^2 - freqb["Low","Yes"]^2
Gini_High = 1 - freqb["High","No"]^2 - freqb["High","Yes"]^2
Gini_Medium = 1 - freqb["Medium","No"]^2 - freqb["Medium","Yes"]^2
Gini_VeryHigh = 1 - freqb["VeryHigh","No"]^2 - freqb["VeryHigh","Yes"]^2
Gini_Budget = freqSumB["Low"]*Gini_low + freqSumB["High"]*Gini_High + freqSumB["Medium"]*Gini_Medium +freqSumB["VeryHigh"]*Gini_VeryHigh
Gini_Budget
#TREES
#erwtisi 1
library(rpart)
library(rpart.plot)
model <- rpart(Insurance ~ Sex+CarType+Budget, method = "class", data =df, minsplit = 1, minbucket = 1,cp = -1)
rpart.plot(model, extra = 104, nn = TRUE)
#erwtisi 2
model1 <- rpart(Insurance ~ CustomerID+Sex+CarType+Budget, method = "class", data =df, minsplit = 1, minbucket = 1,cp = -1)
model2 <- rpart(Insurance ~ Sex+CarType+Budget, method = "class", data =df, minsplit = 1, minbucket = 1,cp = -1)
rpart.plot(model2, extra = 104, nn = TRUE)
rpart.plot(model1, extra = 104, nn = TRUE)

###########
#Naives Bayes Classifier
library('e1071')
traffic = read.csv('traffic.csv', stringsAsFactors = TRUE)
#erwtima 1
model <- naiveBayes(as.factor(HighTraffic) ~ ., data = traffic)
trvalue <- data.frame(Weather = factor("Cold",levels(as.factor(traffic$Weather))), Day = factor("Vacation",levels(as.factor(traffic$Day))))
predict(model, trvalue, type = "raw")
#erwtima 2
model <- naiveBayes(as.factor(HighTraffic) ~ ., data = traffic,laplace = 1)
trvalue <- data.frame(Weather = factor("Cold",levels(as.factor(traffic$Weather))), Day = factor("Vacation",levels(as.factor(traffic$Day))))
predict(model, trvalue, type = "raw")

##########
#knn
library(class)
X1 = c(-2.0, -2.0, -1.8, -1.4, -1.2, 1.2, 1.3, 1.3, 2.0, 2.0,-0.9, -0.5, -0.2, 0.0, 0.0, 0.3, 0.4, 0.5, 0.8, 1.0)
X2 = c(-2.0, 1.0, -1.0, 2.0, 1.2, 1.0, -1.0, 2.0, 0.0, -2.0,0.0, -1.0, 1.5, 0.0, -0.5, 1.0, 0.0, -1.5, 1.5, 0.0)
Y = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
alldata = data.frame(X1, X2, Y)
X_train = alldata[,c("X1","X2")]
Y_train = alldata$Y
#erwtima 1
plot(X_train, col = Y_train, pch = c("o","+")[Y_train])
#erwtima 2
knn(X_train, c(1.5, -0.5), Y_train, k = 3)
#erwtima 3
knn(X_train, c(-1, 1), Y_train, k = 5, prob = TRUE)



##########
#kmeans
library(cluster)
kdata = read.csv('sdata.csv', stringsAsFactors = TRUE)
ctrns = matrix(data = c(-4, 10, 0, 0, 4, 10), nrow = 3, byrow = TRUE)
ctrns
model = kmeans(kdata, centers = ctrns)
#erwtima 1
cohesion = model$tot.withinss
cohesion
#erwtima 2
separation = model$betweenss
separation
#erwtima 3
model_silhouette = silhouette(model$cluster, dist(kdata))
mean(model_silhouette[, 3])



