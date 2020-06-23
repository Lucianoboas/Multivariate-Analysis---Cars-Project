install.packages("readr")
library(readr)
data <- read_csv("~/00LUCIANO/Data Science/08_Multivariate Analysis/0_Project/imports-85.data.csv")
head(data)

data <- read.csv("~/0LUCIANO/Data Science/08_Multivariate Analysis/0_Project/imports-85.data.csv")
data

#create header vector
#files pulled from the website contained a description of the data
#these were stored in a .txt file to be used as headers


headers <- readLines("~/0LUCIANO/Data Science/08_Multivariate Analysis/0_Project/Data and Report_Group 14/headers.txt")

#add headers 
colnames(data) <- headers

#create vector with numerical only data
idata2 <- data[,c(1,3)] #Use to make correltion betwenn risky (symboling column and make)
idata <- data[,c(10:14, 17, 19:26 )]

#replace all ? with NA
idx <- idata == "?"
is.na(idata) <- idx

#data without na
icdata <- na.omit(idata)

#check data types
typeof(icdata)

#create matrix
mdata <- data.matrix(icdata)

#coveriance
cov(mdata)
#correlation
mdata.cor <- cor(mdata)

#color scatterplots for visual analysis
plot(icdata[,1:7], col = icdata[,1])
label = levels(icdata[,1])

plot(icdata[,8:14], col = icdata[,1])
label = levels(icdata[,1])

plot(icdata[,1:14], col = icdata[,1])
label = levels(icdata[,1])

#create a grid of plots
plot(icdata$length, icdata$cityMpg)


#Plot "kdepairs" Matrix:
install.packages("ResourceSelection")
library(ResourceSelection)
kdepairs(mdata)

#Heatmap
install.packages("gplots")
library("gplots")
heatmap(icdata)


#Heatmap
install.packages("gplots")
library("gplots")
heatmap.2(cor.pc,scale = "none", col = bluered(100), 
          trace = "none", density.info = "none")


### PROJECT UPDATE 2 ###

# Run PCA Analysis: Result shows that we got 70% for the first two Components.
cars.pca <- princomp(mdata, cor =T)
summary(cars.pca, loading = T)

# Barchart to vizualize PCA cumulative percentuals:
install.packages("factoextra")
library(factoextra)
fviz_screeplot(cars.pca, main="Barchart of Cumulative Porportion",ncp=50)

# Plot biplot - Two Principal Components:
fviz_pca_biplot(cars.pca, invisible = "ind", habillage ="none", geom = "text", labelsize=5) + theme_minimal()
# PS: I'm not sure how to get rid of the numbers that are showing #col.ind="darkgray"

# Other Options
fviz_pca_biplot(cars.pca, label ="var", col.ind="cos2") + theme_minimal()

fviz_pca_biplot(cars.pca, col.var="contrib")+
  scale_color_gradient2(low="white", mid="blue",
                        high="red", midpoint=96) +
  theme_minimal()



# Check standardized scaled values
options(digits = 2)
scale(mdata)



# RUN EFA Analysis

# Find the number (range) of potential factors for analysis - PLOT Parallel Scree Plot:
install.packages('psych')
library(psych)

parallel.c <- fa.parallel(mdata, fm = 'minres', fa = 'fa')
parallel.c

# Run EFA:
#car.fa <- factanal(mdata, factors=6) # Six is the minimun
#car.fa

car.fa4 <- factanal(mdata, factors=4) # Four should be enough for this set
car.fa4 

car.fa4$loadings

#P-value is 1.08, which is sufficient to accept the null-hypothesis based on 4 factors.













# Run MDS Analysis:

# Create a scaled distance matrix for observations.
cmd <- cmdscale(dist(scale(mdata)), k= 7, eig = T)
cmd
# Check eigenvalues coordinates (cumulative):
cumsum(cmd$eig)/sum(cmd$eig)

#Plot eigenvalue:
eign <- cmd$eig
plot(eign, main="Eigen Plot")
cumsum(eign[1:7])/sum(eign[1:7])
# 3 dimensions represents more than 83% of the overral eigenvalues of the data.

# Perform graphical MDS analysis:
plot(cmd$points[,1:2], pch=".", main="MDS Analysis on Cars Dataset")
text(cmd$points[,1:2], labels=rownames(mdata), cex=0.1)



### Attemptins for Update 2

#Breaking dataset to focus in just 7 variables"
head(mdata)
pcdata <- mdata[,c(2:3,6,10,12:14)]
head(pcdata)

#coveriance
cov(pcdata)
#correlation
cor.pc <- cor(pcdata)
cor.pc


#Plot "kdepairs" Matrix:
install.packages("ResourceSelection")
library(ResourceSelection)
kdepairs(cor.pc)





# CA Analysis on Make and Price :
cac <- data[,c(3,10:14, 17, 19:26 )]

tbl = table(data$make, data$price)
tbl


#EFA Analysis:
# Check thedataset to see what are the reasonable number of factors we can analysebased in this dataset:
install.packages('psych')
library(psych)
install.packages('GPArotation')
# Find the number (range) of potential factors for analysis:
parallel <-fa.parallel(tbl, fm = 'minres', fa = 'fa')
parallel


efa <- factanal(mdata, factors=3)
efa





## Cluster ###

# Check classs:
class(newdata)

# Change the class from data frame to distance:
d = dist(newdata) 
class(d)

# Plot Dendogram
ee <- hclust(d, "complete")
plot(ee, main = "Complete Linkage EE Dendogram")
#abline(h=25)



### Bringing MAKE ###

#create df with make column
newdata <- data.frame(idata, data$make)

#replace all ? with NA
idx <- newdata == "?"
is.na(newdata) <- idx

#data without na
newdata <- na.omit(newdata)


install.packages("mclust")
library(mclust)

# Get optimanl number of clusters
mc <- Mclust(newdata)
mc

# Determining which countries are in which groups:
mc$classification

# Check number of items on each group:
table(mc$classification)


#i) Use "plot" on your fitted mclust object, and report the "uncertainty" plot
#for variables #(SER, SPS), which is dimens = c(6,8). Explain the grouping of
#what country is more #uncertain with what probability of uncertainty.

# ANSWER: Malta is the most uncertain variable with 0.04 probability of uncertainty.

# Plot fitted mclust:
plot(mc, what="classification")
plot(mc, what="uncertainty")



# Report uncertainty for curbWeight and Price
plot(mc, what="uncertainty", dimens = c(5,14))
text(mc$newdata[,c(5,14)], labels = abbreviate(colnames(newdata)), col=mc$classification)


# Check probability of uncertainty:
clust.data = cbind(rownames(mdata), mc$classification, mc$uncertainty)
clust.data[order(mc$uncertainty),]




### RISK Analysis ###
idata2 <- data[,c(1,3)] #Use to make correltion betwenn risky (symboling column and make)

car_risk <- corr(idata2)

plot(idata2)


install.packages("MVA")
library(MVA)



#### RUNNINF CFA ####


# RUN EFA Analysis First with 2 Factors

# Find the number (range) of potential factors for analysis - PLOT Parallel Scree Plot:
install.packages('psych')
library(psych)

parallel.c <- fa.parallel(mdata, fm = 'minres', fa = 'fa')
parallel.c

# Get Correlation:
mdata.cor

# Run EFA:
car.fa2 <- factanal(mdata, factors=2)
car.fa2 

car.fa2$loadings


install.packages("sem")
library("sem")
install.packages("semPlot")
library(semPlot)


#Bring txt file model:
car_txt <- read.table("~/0LUCIANO/Data Science/08_Multivariate Analysis/0_Project/car_project.txt")
cars_model <- specifyModel(file = "~/0LUCIANO/Data Science/08_Multivariate Analysis/0_Project/car_project.txt")

#check # of rolls?
nrow(mdata)

is.na(mdata)

# Run CFA model (correlation plus model text file plu # of rows)
cars_sem <- sem(cars_model, mdata.cor, 194)

summary(cars_sem)


















