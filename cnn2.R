library(keras)
require(abind)
# install_keras()
# library()
# install_tensorflow(gpu = TRUE)

setwd("/Users/Yuki/Dropbox/sokouo1/jiseki")
load("testdata.RData")

batch_size <- 256
epochs <- 20
validation_split <- 0.1
img_size <- dim(data$figure)[-1]
rep <- 1

set.seed(0)
slist = unique(rpois(1000, lambda = 500))
rep = sample(slist, 100, replace = FALSE)

list_acc = c()
list_mistake = c()
all_test = c()
summ = c()

for(i in 1:length(rep)){
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
  
  
  model_age_f <- keras_model_sequential()
  model_age_f %>% 
    layer_conv_2d(
      filter = 32, kernel_size = c(3,3), padding = "same", 
      input_shape = img_size
    ) %>%
    layer_activation("relu") %>%
    layer_conv_2d(filter = 32, kernel_size = c(3,3)) %>%
    layer_activation("relu") %>%
    layer_max_pooling_2d(pool_size = c(2,2)) %>%
    layer_conv_2d(filter = 64, kernel_size = c(3,3), padding = "same") %>%
    layer_activation("relu") %>%
    layer_conv_2d(filter = 64, kernel_size = c(3,3)) %>%
    layer_activation("relu") %>%
    layer_max_pooling_2d(pool_size = c(2,2)) %>%
    layer_flatten() %>%
    layer_dense(512) %>%
    layer_activation("relu") %>%
    layer_dropout(0.5) %>%
    layer_dense(dim(y_train)[2]) %>%
    layer_activation("softmax")
  
  model_age_f %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy')
  )
  
  history <- model_age_f %>% fit(
    x_train, y_train,
    batch_size = batch_size,
    epochs = epochs,
    verbose = 1,
    callbacks = callback_tensorboard(log_dir = "logs/run_b"),
    validation_split = validation_split,
    shuffle = T
  )
  
  result <- model_age_f %>% predict(x_test)
  y_pred <- round(result, 0)
  mistake <- which(y_pred[,1]-y_test[,1]!=0)
  dfmist = data.frame(mistake = mistake, times = paste0(i))
  list_mistake = rbind(list_mistake, dfmist)
  acc <- 1 - length(mistake)/nrow(y_test)
  list_acc = rbind(list_acc, acc)
  
  all_test = abind(all_test, x_test[])
  
  file = paste0("/Users/Yuki/Dropbox/sokouo1/jiseki/model_age_f_", i, ".hdf5")
  save_model_hdf5(model_age_f, file = file)
  
  t = y_test %>% data.frame() %>% gather(key = age, value = dam) %>% group_by(age) %>% summarize(count = sum(dam)) %>% mutate(times = 1, type = "test_data")
  t2 = y_test[mistake, ] %>% data.frame() %>% gather(key = age, value = dam) %>% group_by(age) %>% summarize(count = sum(dam)) %>% mutate(times = i, type = "mistake")
  t3 = rbind(t, t2)
  summ =  rbind(summ, t3)
  
}

nrow(list_mistake)
nrow(summ)
unique(summ$age)
summ = summ %>% mutate(times = rep(1:10, each = 22), age = ifelse(age == "age10.", "age10+", summ$age))

write.csv(summ, "summ.csv")
write.csv(list_acc, "list_acc.csv")
write.csv(list_mistake, "list_mistake.csv")
save(all_test, file = "all_test.RData")


setwd("/Users/Yuki/Dropbox/sokouo1/jiseki")
summ = read.csv("summ.csv")
age = read.csv("age.csv", fileEncoding = "CP932")
age_check = read.csv("age_check.csv")

require(tidyverse)
require(plyr)

# summary of mistake --------------------------------------------
summ2 = ddply(summ, .(age, type), summarize, mean = mean(count), sd = sd(count))
summ2 = summ2[-1, ]
unique(summ2$age)
summ2$age2 = ifelse(summ2$age == "age10.", "10+", paste0(str_sub(summ2$age, 4, 5)))
summ2$age2 = factor(summ2$age2, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "10+"))
summ2$cate = ifelse(summ2$age2 == "10+", "10+", "1-10")
summ3 = summ2 %>% mutate(cate = "all")
summ4 = rbind(summ2, summ3)
summ4$cate = factor(summ4$cate, levels = c("all", "1-10", "10+"))

g = ggplot(summ4, aes(x = age2, y = mean, fill = type), stat = "identity")
# g = ggplot(tai_miya2, aes(x = taityo, y = mean), stat = "identity")
b = geom_bar(stat = "identity", position = "dodge", width = 0.5)
e = geom_errorbar(aes(ymin = summ4$mean-summ4$sd, ymax = summ4$mean+summ4$sd), stat = "identity", position = position_dodge(0.5), size = 0.3, width = 0.5)
f = facet_wrap(~ cate, ncol = 1, scales = 'free')
labs = labs(x = "Age", y = "Numbers (mean ± sd)", title = "Test data")
g+b+e+f+labs+theme_bw()



# summary of age ---------------------------------------------------
summary(age)
colnames(age)[2] = "age2"
age = merge(age, age_check, by = "age2")

age_sum = na.omit(age)
age_sum = age_sum %>% group_by(age_cate) %>% dplyr::summarize(count = n())
age_sum$age_cate = factor(age_sum$age_cate, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "10+"))

g = ggplot(age_sum, aes(x = age_cate, y = count), stat = "identity")
# g = ggplot(tai_miya2, aes(x = taityo, y = mean), stat = "identity")
b = geom_bar(stat = "identity", position = "dodge")
labs = labs(x = "Age", y = "Numbers", title = "Raw data")
g+b+labs+theme_bw()

