library(keras)
require(abind)
# install_keras()
# library()
# install_tensorflow(gpu = TRUE)

setwd("/Users/Yuki/Dropbox/sokouo1/jiseki")
load("testdata.RData")
# mis_list = read.csv("list_mistake.csv")

fig <- data$figure[,,,, drop=F]
age <- data$age[,] %>% data.frame()
age$tag = rep(1:nrow(age))
list = age %>% filter(age1 == 1)

vector_to_image <- function(img)
{
  img <- t(apply(img, 1, rev))
  par(mar=c(0,0,0,0))
  image(img, col=gray.colors(128), axes=F)
}

list_age1 = list$tag
par(mfrow=c(3,3))
for(j in list_age1){
  vector_to_image(fig[j,,,])
}
