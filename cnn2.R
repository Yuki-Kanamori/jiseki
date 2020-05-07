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

require(ggplot2)
g = ggplot(summ, aes(x = age, y = count, fill = type), stat = "identity")
# g = ggplot(tai_miya2, aes(x = taityo, y = mean), stat = "identity")
b = geom_bar(stat = "identity", position = "dodge")
f = facet_wrap(~ times, ncol = 3, scales = 'free')
labs = labs(x = "Age", y = "Numbers", title = "Test data")
g+b+f+labs+theme_bw()