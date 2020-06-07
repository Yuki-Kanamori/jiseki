library(keras)
require(abind)
# install_keras()
# library()
# install_tensorflow(gpu = TRUE)
require(tidyverse)

setwd("/Users/Yuki/Dropbox/sokouo1/jiseki")
load("testdata.RData")

age_list = data$age[,] %>% data.frame() %>% gather(key = class, value = freq)
age_list = age_list %>% filter(freq > 0)
unique(age_list$class)
summary(age_list)

tag = data_frame(class = c("age1", "age2", "age3", "age4", "age5", "age6", "age7", "age8", "age9", "age10", "age10."), age = c(1,2,3,4,5,6,7,8,9,10,11))
age_list = left_join(age_list, tag, by = "class")
age_list = age_list %>% mutate(step = ifelse(age_list$age < 6, "small", "large"))

s1 = age_list %>% select(step)
step1 = model.matrix(as.formula(~0+step), data = s1)
colnames(step1) = c("6to10+", "1to5")

s2 = age_list %>% select(class)
step2 = model.matrix(as.formula(~0+class), data = s2)
colnames(step2)
colnames(step2) = c("age1", "age10", "age10+", "age2", "age3", "age4", "age5", "age6", "age7", "age8", "age9")

img = data$figure[,,,, drop=F]
dim(img)

data = list(img, step1, step2)
names(data) = c("figure", "small", "age")
save(data, file = "testdata2.RData")


