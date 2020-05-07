library(keras)
require(abind)
# install_keras()
# library()
# install_tensorflow(gpu = TRUE)

setwd("/Users/Yuki/Dropbox/sokouo1/jiseki")
load("testdata.RData")
# mis_list = read.csv("list_mistake.csv")
set.seed(0)

fig <- data$figure[,,,, drop=F]
age <- data$age[,] %>% data.frame()
age$tag = rep(1:nrow(age))
dim(fig)

vector_to_image <- function(img)
{
  img <- t(apply(img, 1, rev))
  par(mar=c(0,0,0,0))
  image(img, col=gray.colors(128), axes=F)
}


# age 1 ---------------------------------------------------------
list1 = age %>% filter(age1 == 1)
list_age1 = list1$tag
par(mfrow=c(3,3))
list = sample(list_age1, 9)
for(j in list){
  vector_to_image(fig[j,,,])
}


# age 5 ---------------------------------------------------------
list5 = age %>% filter(age5 == 1)
list_age5 = list5$tag
par(mfrow=c(3,3))
list = sample(list_age5, 9)
for(j in list){
  vector_to_image(fig[j,,,])
}

# age 10 ---------------------------------------------------------
list10 = age %>% filter(age10 == 1)
list_age10 = list10$tag
par(mfrow=c(3,3))
list = sample(list_age10, 9)
for(j in list){
  vector_to_image(fig[j,,,])
}
