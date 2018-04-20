##PCA Using R

##Read data

pc <- read.csv(file.choose())
str(pc)
summary(pc)
newpc <- pc[,-c(1,2)]
str(newpc)##FactoMiner is used for Multivariate  EDA
##if (!require('FactoMineR')) install.packages('FactoMineR') 
library(FactoMineR)
##for pca only numeric variables are to be selected
##create a df for having only numerical factors
attach(pc)
##numeric_predictors=data.frame('X.age.Black ','X.age.Hispanic','X.age.Asian','Median.Age',
                     'Unemployment.rate','Per.capita.income.000.s.')
##Data_for_PCA = pc[,numeric_predictors]
##head(numeric_predictors,top =5)

pca = PCA(newpc)
pca$eig


Correlation_Matrix=as.data.frame(round(cor(newpc,pca$ind$coord)^2*100,0))
Correlation_Matrix[with(Correlation_Matrix, order(-Correlation_Matrix[,1])),]
##From the matrix it is evident that Median Age,Per Capita Income and Unemployement rate are important.

plot(pca)
pc.cr <- princomp( Correlation_Matrix)
library(graphics)
screeplot(pc.cr)
##scree plot shows first three components are significant.
