library(keras)
require(abind)
# install_keras()
# library()
# install_tensorflow(gpu = TRUE)

setwd("/Users/Yuki/Dropbox/sokouo1/jiseki")
load("testdata.RData")
mis_list = read.csv("list_mistake.csv")

set.seed(0)
slist = unique(rpois(1000, lambda = 500))
rep = sample(slist, 100, replace = FALSE)
i = 1

set.seed(rep[i])
nr = nrow(data$figure) #1185
sprit = sample(nr, replace = F, nr*0.85)
nr*0.85 #1007

data3 = list(data$figure[sprit,,,, drop=F], data$age[sprit, ])
data2 = list(data$figure[-sprit,,,, drop=F], data$age[-sprit, ])
names(data2) = c("figure", "age")
names(data3) = c("figure", "age")

x_train <- data3$figure[,,,, drop=F]
y_train <- data3$age[,]
x_test <- data2$figure[,,,, drop=F]
y_test <- data2$age[,]

mis = mis_list %>% filter(times == i)
mis = mis$mistake
vector_to_image <- function(img)
{
  img <- t(apply(img, 1, rev))
  par(mar=c(0,0,0,0))
  image(img, col=gray.colors(128), axes=F)
}

par(mfrow=c(3,3))
for(j in mis){
  vector_to_image(x_test[j,,,])
}

y_test2 = c()
for(j in mis){
  data = y_test[j, ]
  y_test2 = rbind(y_test2, data)
}
