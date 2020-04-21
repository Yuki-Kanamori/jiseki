
# setting directory ---------------------------------------------
# setwd("/Users/Yuki/Dropbox/サンマ耳石")
# load("input_20_mat.rda")
setwd("/Users/Yuki/Dropbox/sokouo1/キチジ耳石写真/201610若鷹丸キチジ耳石/201610No.1")

# packages ------------------------------------------------------
require(imager) #need XQuartz
require(abind)
require(tidyverse)

# test for check the images (https://htsuda.net/archives/1985) -------------------------------------
# https://note.com/hanaori/n/ne7124ba5e3ca
img = load.image( "A510-30(内) 10.0 0.71x 71.jpg" )
plot(img) #with axes
plot(img, axes = FALSE, xlab = "", ylab = "") #remove the axes from img and plot
img # show information of img (colour channel = 3), class = cimg
grayimg = grayscale(img)
plot(grayimg)
grayimg #colour channel = 1
dim(grayimg) #4-dims data [x,y,z(depth), colour channel] (4th dim = 1 when grayscale, and = 3 when colour)
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
par(mar = c(5,5))
mirror(grayimg, "x") %>% plot 
mirror(grayimg, "y") %>% plot 

exp_var = c()
trgimg = imsub(grayimg, x < 1100)
trgimg = img
dim(trgimg)
trgimg2 = (trgimg - mean(trgimg))/sd(trgimg) 
trgimg2 = as.array(trgimg2)
dim(trgimg2) = c(1, dim(trgimg2))[c(1,2,4)]
par(mfrow = c(5,5))
exp_var = abind(exp_var, trgimg2)
