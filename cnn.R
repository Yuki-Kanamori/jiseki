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

data3 = list(data$figure[sprit,,,, drop=F], data$age[sprit, ])
data2 = list(data$figure[-sprit,,,, drop=F], data$age[-sprit, ])
names(data2) = c("figure", "age")
names(data3) = c("figure", "age")

# x_train = data$figure[sprit,,,, drop=F]
# y_train = data$age
# y_train2 = y_train[sprit, drop=F]
# y_train = data$age[sprit, drop=F]
# x_test = data$figure[-sprit,,,, drop=F]
# y_test = data$age[-sprit,,,, drop=F]

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
acc <- 1 - length(mistake)/nrow(y_test)

vector_to_image <- function(img)
{
  img <- t(apply(img, 1, rev))
  par(mar=c(0,0,0,0))
  image(img, col=gray.colors(128), axes=F)
}

if(1){
  par(mfrow=c(3,3))
  for(i in mistake)
  {
    vector_to_image(x_test[i,,,])
  }
}

max_val <- c()
min_val <- c()
for(i in 1:nrow(result))
{
  max_val <- c(max_val, max(result[i,]))  
  min_val <- c(min_val, min(result[i,]))  
}
max_err <- c()
min_err <- c()
for(i in mistake)
{
  max_err <- c(max_err, max(result[i,]))  
  min_err <- c(min_err, min(result[i,]))  
}

mean(max_val)
sd(max_val)
mean(max_err)
sd(max_err)
mean(min_val)
mean(min_err)

y_test[mistake,]
#result[mistake,]
acc_list <- c(acc_list, acc)

acc_list
mean(acc_list)
sd(acc_list)
