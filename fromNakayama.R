library("imager")
library("abind")
folder_img <- "/Users/shin-ichironakayama/Dropbox/otolith_backup/img_fishery_all"
master <- read.csv("/Users/shin-ichironakayama/Dropbox/otolith_backup/master_fishery_all.csv")
file_output <- "/Users/shin-ichironakayama/Dropbox/samma_analysis/jiseki/input_fishery_20.rda"
proportion <- 20
gray_scale = T 
invert_h = F 
invert_v = T
invert_hv = F
plot = T



image_to_vector <- function(img, proportion, gray_scale=T, invert_h=F, invert_v=F, plot=F)
{
  if(gray_scale)
  {
    img <- grayscale(img)
  }
  if(invert_h)
  {
    img <- mirror(img, "x")
  }
  if(invert_v)
  {
    img <- img <- mirror(img, "y")
  }
  img <- resize(img, round(width(img)/proportion),round(height(img)/proportion))
  if(plot)
  {
    par(mar = c(0,0,0,0))
    plot(img, axes = FALSE, xlab = "", ylab = "" )
  }
  img <- (img-mean(img))/sd(img)
  img <- as.array(img)
  dim(img) <- c(1, dim(img)[c(1,2,4)])
  img
}

make_exp_var <- function(folder_img, master, proportion, gray_scale=T, invert_h=F, invert_v=F, invert_hv=F, plot=F)
{
  list_img_files <- c()
  for(i in 1:nrow(master))
  {
    path <- paste(folder_img, "/", master$file[i], sep="")
    list_img_files <- c(list_img_files, path)
  }
  num <- 1
  exp_var <- c()
  data_num <- length(list_img_files) * (invert_h + invert_v + invert_hv + 1)
  par(mfrow = c(5,5))
  for(filename in list_img_files)
  {
    img <- load.image(filename)
    if(plot)
    {
      exp_var <- abind(exp_var, image_to_vector(img, proportion, gray_scale, F, F, T), along=1)
    }else
    {
      exp_var <- abind(exp_var, image_to_vector(img, proportion, gray_scale, F, F, F), along=1)
    }
    print(paste("Proccessing file No.", num, " / ", data_num))
    num <- num + 1
  }
  if(invert_h)
  {
    for(filename in list_img_files)
    {
      img <- load.image(filename)
      if(plot)
      {
        exp_var <- abind(exp_var, image_to_vector(img, proportion, gray_scale, T, F, T), along=1)
      }else
      {
        exp_var <- abind(exp_var, image_to_vector(img, proportion, gray_scale, T, F, F), along=1)
      }
      print(paste("Proccessing file No.", num, " / ", data_num))
      num <- num + 1
    }
  }
  if(invert_v)
  {
    for(filename in list_img_files)
    {
      img <- load.image(filename)
      if(plot)
      {
        exp_var <- abind(exp_var, image_to_vector(img, proportion, gray_scale, F, T, T), along=1)
      }else
      {
        exp_var <- abind(exp_var, image_to_vector(img, proportion, gray_scale, F, T, F), along=1)
      }
      print(paste("Proccessing file No.", num, " / ", data_num))
      num <- num + 1
    }
  }
  if(invert_hv)
  {
    for(filename in list_img_files)
    {
      img <- load.image(filename)
      if(plot)
      {
        exp_var <- abind(exp_var, image_to_vector(img, proportion, gray_scale, T, T, T), along=1)
      }else
      {
        exp_var <- abind(exp_var, image_to_vector(img, proportion, gray_scale, T, T, F), along=1)
      }
      print(paste("Proccessing file No.", num, " / ", data_num))
      num <- num + 1
    }
  }
  exp_var
}

make_res_age <- function(master, invert_h, invert_v, invert_hv)
{
  type <- as.character(master$type)
  for(i in 1:length(type))
  {
    if(type[i]==1)
    {
      type[i] <- 0
    }else if(type[i]==2|type[i]==3|type[i]==4)
    {
      type[i] <- 1
    }else
    {
      type[i] <- NA
    }
  }
  type <- as.factor(type)
  #input <- read.csv(file_res, header=F)
  classes <- sort(na.omit(unique(type)))
  res_var <- matrix(0, nrow=length(type), ncol=length(classes))
  colnames(res_var) <- classes
  for(i in 1:nrow(res_var))
  {
    if(is.na(type[i]))
    {
      res_var[i,] <- rep(NA, ncol(res_var))
    }else
    {
      res_var[i,colnames(res_var)==type[i]] <- 1
    }
  }
  multiplier <- invert_h + invert_v + invert_hv
  res_temp <- res_var
  if(multiplier)
  {
    for(i in 1:multiplier)
    {
      res_var <- rbind(res_var, res_temp)
    }
  }
  res_var
}

make_res_radius <- function(master, invert_h, invert_v, invert_hv)
{
  res_var <- master$radius
  multiplier <- invert_h + invert_v + invert_hv
  res_temp <- res_var
  if(multiplier)
  {
    for(i in 1:multiplier)
    {
      res_var <- c(res_var, res_temp)
    }
  }
  res_var
}

make_res_odd <- function(master, invert_h, invert_v, invert_hv)
{
  type <- master$odd
  #input <- read.csv(file_res, header=F)
  classes <- sort(na.omit(unique(type)))
  res_var <- matrix(0, nrow=length(type), ncol=length(classes))
  colnames(res_var) <- classes
  for(i in 1:nrow(res_var))
  {
    if(is.na(type[i]))
    {
      res_var[i,] <- rep(NA, ncol(res_var))
    }else
    {
      res_var[i,colnames(res_var)==type[i]] <- 1
    }
  }
  multiplier <- invert_h + invert_v + invert_hv
  res_temp <- res_var
  if(multiplier)
  {
    for(i in 1:multiplier)
    {
      res_var <- rbind(res_var, res_temp)
    }
  }
  res_var
}

make_res_flipped <- function(master, invert_h, invert_v, invert_hv)
{
  res_var <- cbind(rep(1, nrow(master)), rep(0, nrow(master)))
  colnames(res_var) <- c("normal", "flipped")
  multiplier <- invert_h + invert_v + invert_hv
  res_temp <- cbind(rep(0, nrow(master)), rep(1, nrow(master)))
  if(multiplier)
  {
    for(i in 1:multiplier)
    {
      res_var <- rbind(res_var, res_temp)
    }
  }
  res_var
}

make_input_data <- function(folder_img, master, proportion, gray_scale=T, invert_h=F, invert_v=F, invert_hv=F, plot=F)
{
  list_img_files <- list.files(folder_img, full.name=T)
  figure <- make_exp_var(folder_img, master, proportion, gray_scale, invert_h, invert_v, invert_hv, plot)
  age <- make_res_age(master, invert_h, invert_v, invert_hv)
  radius <- make_res_radius(master, invert_h, invert_v, invert_hv)
  odd <- make_res_odd(master, invert_h, invert_v, invert_hv)
  flipped <- make_res_flipped(master, invert_h, invert_v, invert_hv)
  data <- list(figure, age, radius, odd, flipped)
  names(data) <- c("figure", "age", "radius", "odd", "flipped")
  data
}

data <- make_input_data(folder_img, master, proportion, gray_scale, invert_h, invert_v, invert_hv, plot)
save(data, file=file_output)

