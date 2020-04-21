
# setting directory ---------------------------------------------
# setwd("/Users/Yuki/Dropbox/サンマ耳石")
# load("input_20_mat.rda")
setwd("/Users/Yuki/Dropbox/sokouo1/キチジ耳石写真/201610若鷹丸キチジ耳石/201610No.1")

# packages ------------------------------------------------------
require(imager) #need XQuartz
require(abind)


# test for check the images (https://htsuda.net/archives/1985) -------------------------------------
img = load.image( "A510-30(内) 10.0 0.71x 71.jpg" )
# plot( img ) #with axes
plot(img, axes = FALSE, xlab = "", ylab = "") #remove the axes from img and plot
img # show information of img (colour channel = 3), class = cimg
grayimg = grayscale(img)
plot(grayimg)
grayimg #colour channel = 1
dim(grayimg) #4-dims data [x,y,z(depth), colour channel] (4th dim = 1 when grayscale, and = 3 when colour)
grayimg[,,1,1] #??
grayimg[1,1,1,1] #pixcel value of the coordinate
plot(imlist(light = img^(1/3), original = img, dark = img^3), layout = "row") #change pixel and plot
plot(imlist(light = grayscale(img) + .4, original = grayscale(img)), layout = "row") #increase pixel (i.e. bright) using add
gimg = grayscale(img)
gimg[gimg > .5] = 1
gimg[gimg <= .5 ] = 0
plot(gimg) #monochrome

layout(t(1:4))
hist(grayscale(boats), main = "Luminance")
hist(R(boats), main = "Red")
hist(G(boats), main = "Green")
hist(B(boats), main = "Blue")

