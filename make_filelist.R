require(tidyverse)

setwd("/Users/Yuki/Dropbox/sokouo1/キチジ耳石写真")
wd = "/Users/Yuki/Dropbox/sokouo1/キチジ耳石写真"

img_list = c()
for(year in 2014:2018){
  if(year < 2016){
    for(no in 1:9){
      path = paste(wd, "/", year, "/No", no, sep = "")
      list = list.files(path, pattern = "jpg") %>% data.frame() %>% mutate(year = paste(year), box = paste(no))
      img_list = rbind(img_list, list)
    }
  }else{
    for(no in 1:5){
      path = paste(wd, "/", year, "/No", no, sep = "")
      list = list.files(path, pattern = "jpg") %>% data.frame() %>% mutate(year = paste(year), box = paste(no))
      img_list = rbind(img_list, list)
    }
  }
}
img_list = img_list %>% mutate(test = str_trim(img_list$.)) %>% mutate(tag = str_extract(img_list$., pattern = "0.71")) %>% mutate(file = ifelse(img_list$year < 2016, str_sub(img_list$., -19, -10), str_sub(img_list$., 1, 10)))
test = data.frame(str_split(img_list$file, "，", simplify=T)) 
img_list = cbind(img_list, test)
img_list2 = img_list %>% na.omit()
setwd("/Users/Yuki/Dropbox/sokouo1/キチジ耳石写真")
write.csv(img_list2, "img_list2.csv", fileEncoding = "CP932")



# combine filename with agedata ---------------------------------
setwd("/Users/Yuki/Dropbox/sokouo1/キチジ耳石写真")
age = read.csv("age.csv", fileEncoding = "CP932") #1515
filename = read.csv("master.csv", fileEncoding = "CP932") #2155

filename = filename %>% select(year, box, file)
summary(filename)
head(filename)
head(age)
filename2 = merge(filename, age, by = c("file", "year"), all = F) #1198
# filename = merge(filename, age, by = c("file", "year"), all = T)
