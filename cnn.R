library(keras)
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
nr = nrow(data$figure) #1185
sprit = sample(nr, replace = F, nr*0.85)
nr*0.85 #1007

# data3 = list(data$figure[sprit,,,, drop=F], data$age[sprit, ])
# data2 = list(data$figure[-sprit,,,, drop=F], data$age[-sprit, ])

x_train = data$figure[sprit,,,, drop=F]
y_train = data$age
y_train2 = y_train[sprit, drop = F]
y_train = data$age[sprit, drop=F]
x_test = data$figure[-sprit,,,, drop=F]
y_test = data$age[-sprit,,,, drop=F]

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
acc <- 1 - length(mistake)/nrow(y_test)