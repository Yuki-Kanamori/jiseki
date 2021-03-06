
# setting directory ---------------------------------------------
# setwd("/Users/Yuki/Dropbox/jiseki")
# load("input_20_mat.rda")
setwd("/Users/Yuki/Dropbox/sokouo1/jiseki/2016/No3")

# packages ------------------------------------------------------
require(imager) #need XQuartz
require(abind)
require(tidyverse)

devtools::install_github("rstudio/keras")
devtools::install_github("rstudio/tensorflow")

# test for check the images (https://htsuda.net/archives/1985) -------------------------------------
# https://note.com/hanaori/n/ne7124ba5e3ca
img = load.image( "A510-3(内) 19.0 0.71x 251.jpg" )
# img = load.image("A450-46(内) 18.6 0.71x 243.jpg")
plot(img) #with axes
plot(img, axes = FALSE, xlab = "", ylab = "") #remove the axes from img and plot
img # show information of img (colour channel = 3), class = cimg
grayimg = grayscale(img)
plot(grayimg)
grayimg #colour channel = 1
dim(grayimg) #4-dims data [x,y,z(depth), colour channel] (4th dim = 1 when grayscale, and = 3 when colour)
dim(img)
grayimg[,,1,1] #??
grayimg[1,1,1,1] #pixcel value of the coordinate
grayimg[ , , 1, 1 ][1:5,1:5] #1 is white, and 0 is black
head(as.data.frame(img))
plot(imlist(light = img^(1/3), original = img, dark = img^3), layout = "row") #change pixel and plot
plot(imlist(light = grayscale(img) + .4, original = grayscale(img)), layout = "row") #increase pixel (i.e. bright) using add
layout(1) #delete the setting of plot
gimg = grayscale(img)
gimg[gimg > .5] = 1
gimg[gimg <= .5 ] = 0
plot(gimg) #monochrome

layout(t(1:4))
hist(grayscale(boats), main = "Luminance")
hist(R(boats), main = "Red")
hist(G(boats), main = "Green")
hist(B(boats), main = "Blue")

layout(1) #delete the setting of plot
imsub(img, x < 1100) %>% plot #trimming

parmin(grayimg) %>% plot #focus on low pixel
parmax(grayimg) %>% plot #focus on high pixel

par(mar = c(0,0,0,0)) #setting of plot (delete the margin)
par(mfrow = c(5,5))
mirror(grayimg, "x") %>% plot 
mirror(grayimg, "y") %>% plot 

exp_var = c()
trgimg = imsub(grayimg, x < 1100)
trgimg = img
dim(trgimg)
trgimg2 = (trgimg - mean(trgimg))/sd(trgimg) 
trgimg2 = as.array(trgimg2)
dim(trgimg2) = c(1, dim(trgimg2)[c(1,2,4)])
par(mfrow = c(5,5))
exp_var = abind(exp_var, trgimg2)



# check imgs ------------------------------------------------------
setwd("/Users/Yuki/Dropbox/sokouo1/jiseki")
wd = "/Users/Yuki/Dropbox/sokouo1/jiseki"
year = 2016

list_img = c()
for(year in year){
  if(year < 2016){
   for(no in 1:9){
     path = paste(wd, "/", year, "/No", no, sep = "")
     list = list.files(path, pattern = "jpg")
     setwd(path)
     for(l in 1:length(list)){
       img = load.image(list[l])
       img = grayscale(img)
       img = imsub(img, x < 1100)
       list_img = list(list_img, img) 
     }
   } 
  }else{
    for(no in 1:5){
      path = paste(wd, "/", year, "/No", no, sep = "")
      list = list.files(path, pattern = "jpg")
      setwd(path)
      for(l in 1:length(list)){
        # data = list[l]
        img = load.image(list[l])
        img = grayscale(img)
        img = imsub(img, x < 1100)
        list_img = list(list_img, img)  
      }
    }
  }
}


# retry ---------------------------------------------------------
setwd("/Users/Yuki/Dropbox/sokouo1/jiseki")
wd = "/Users/Yuki/Dropbox/sokouo1/jiseki"
year = 2015

list_img = c()
par(mfrow = c(10, 10))
par(mar = c(0,0,0,0))

for(year in year){
  if(year < 2016){
    for(no in 1:9){
      path = paste(wd, "/", year, "/No", no, sep = "")
      list = list.files(path, pattern = "jpg")
      setwd(path)
      for(l in 1:length(list)){
        img = load.image(list[l])
        img = grayscale(img)
        img = imsub(img, x < 1090)
        # list_img = list(list_img, img) 
        plot(img, axes = FALSE, xlab = "", ylab = "")  
      }
    } 
  }else{
    for(no in 1:5){
      path = paste(wd, "/", year, "/No", no, sep = "")
      list = list.files(path, pattern = "jpg")
      setwd(path)
      for(l in 1:length(list)){
        # data = list[l]
        img = load.image(list[l])
        img = grayscale(img)
        img = imsub(img, x < 1090)
        # list_img = list(list_img, img)
        # par(mfrow = c(10,10))
        plot(img)  
      }
    }
  }
}

par(mfrow = c(10, 10))
img = load.image(list[2])
img = grayscale(img)
img = imsub(img, x < 1100)
par(mar = c(0,0,0,0))
# list_img = list(list_img, img)
# par(mfrow = c(10,10))
plot(img)  


# resize --------------------------------------------------------

# setting directory ---------------------------------------------
# setwd("/Users/Yuki/Dropbox/jiseki")
# load("input_20_mat.rda")
setwd("/Users/Yuki/Dropbox/sokouo1/jiseki/2016/No3")

# packages ------------------------------------------------------
require(imager) #need XQuartz
require(abind)
require(tidyverse)

# test for check the images (https://htsuda.net/archives/1985) -------------------------------------
# https://note.com/hanaori/n/ne7124ba5e3ca
img = load.image( "A510-3(内) 19.0 0.71x 251.jpg" )
# img = load.image("A450-46(内) 18.6 0.71x 243.jpg")
plot(img) #with axes
img # show information of img (colour channel = 3), class = cimg
grayimg = grayscale(img)
dim(grayimg) #2048, 1536, 1, 1
plot(grayimg)
t = imsub(grayimg, x < 1090)
dim(t) #1089,1536,1,1
plot(t)
prop = 10
t2 = resize(t, round(width(t)/prop), round(height(t)/prop))
dim(t2) #218,307,1,1
plot(t2)



# check nakayama data -------------------------------------------
setwd("~/Dropbox/otolith_ps")
img = load.image("B16-001-01.bmp") %>% grayscale()
plot(img)
r_img = resize(img, round(width(img))/20, round(height(img))/20)
plot(r_img)
dim(img) #1280, 1024, 1, 1
dim(r_img) #64, 51, 1, 1




# check the size ------------------------------------------------
# filelist
setwd("/Users/Yuki/Dropbox/sokouo1/jiseki")
master = read.csv("img_list.csv", fileEncoding = "CP932")
no = master$no
no[is.na(no)] = 2

er = master$error 
er[is.na(er)] = 0
mir = master$mirror
mir[is.na(mir)] = 0

master$no = no
master$error = er
master$mirror = mir
summary(master)

master2 = master %>% filter(no == 2)
# master2 = master %>% filter(no == 2 & year != 2017)
summary(master2)

age = read.csv("age.csv", fileEncoding = "CP932") %>% na.omit()
summary(age)
age$age2 = as.character(age$age)
# check = unique(age$age2) %>% data.frame()
# write.csv(check, "age_check.csv")
check = read.csv("age_check.csv")
head(age)
head(check)
age = merge(age, check, by = "age2")

head(master2)
head(age)
master2 = merge(master2, age, by = c("year", "file"))
master3 = master2 %>% select(year, box, rawname, file, age_cate,)
summary(master3)
# master3 = master3 %>% filter(rawname != "C410-12(外) 6.6 0.71x 16.jpg")"C510-16(外) 6.5 0.71x 12.jpg"


wd = "/Users/Yuki/Dropbox/sokouo1/jiseki"
prop = 10
size = matrix(NA, ncol = 2, nrow = nrow(master3))
size1 = c()
size2 = c()
age_list = c()
for(i in 1:nrow(master3)){
  list = master3[i, ]
  path = paste(wd, "/", list$year, "/No", list$box, sep = "")
  setwd(path)
  data = paste(list$rawname)
  img = load.image(data)
  img = grayscale(img)
  img = imsub(img, x < 1090)
  img = resize(img, round(width(img)/prop), round(height(img)/prop))
  d_size1 = paste(dim(img)[1])
  d_size2 = paste(dim(img)[2])
  # size1 = abind(size, d_size1)
  # size2 = abind(size, d_size2)
  size[i,1] = d_size1
  size[i,2] = d_size2

  age_data = paste(list$age_cate)
  age_list = abind(age_list, age_data)
}
summary(size)
size = data.frame(size)
master3 = cbind(master3, size)
summary(master3)
master4 = master3 %>% na.omit() %>% filter(X2 != 120)



# load the data -------------------------------------------------
wd = "/Users/Yuki/Dropbox/sokouo1/jiseki"
prop = 10
exp_var = c()
age_list = c()
for(i in 1:nrow(master4)){
  #i = 1
  list = master4[i, ]
  path = paste(wd, "/", list$year, "/No", list$box, sep = "")
  setwd(path)
  data = paste(list$rawname)
  img = load.image(data)
  img = grayscale(img)
  img = imsub(img, x < 1090)
  img = resize(img, round(width(img)/prop), round(height(img)/prop))
  
  img = (img - mean(img))/sd(img)
  img = as.array(img)
  dim(img) = c(1, dim(img)[c(1,2,4)])
  exp_var = abind(exp_var, img, along = 1)
  
  age_data = paste(list$age_cate)
  age_list = abind(age_list, age_data)
}

dim(img)
dim(exp)
setwd("/Users/Yuki/Dropbox/sokouo1/jiseki")
save(exp_var, file = "exp_var.Rdata")
save(age_list, file = "age_list.Rdata")

age_list2 = age_list %>% data.frame() %>% mutate(freq = 1)
colnames(age_list2)[1] = "age"
age_list3 =  model.matrix(as.formula(~0+age), data = age_list2)

data = list(exp_var, age_list3)  
names(data) = c("figure", "age")
save(data, file = "testdata.RData")  
  