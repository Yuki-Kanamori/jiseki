library(keras)
# install_keras()
# library()
# install_tensorflow(gpu = TRUE)

setwd("/Users/Yuki/Dropbox/sokouo1/jiseki")
load("testdata.RData")

set.seed(0)
nr = nrow(data$figure) #1185
sprit = sample(nr, replace = F, nr*0.85)
nr*0.85 #1007

data2 = list(data$figure[sprit,,,, drop=F], data$age[sprit, ])

train = data$figure[sprit,,,, drop=F]
test = data$figure[-sprit,,,, drop=F]

