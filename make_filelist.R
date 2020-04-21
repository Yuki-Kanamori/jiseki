setwd("/Users/Yuki/Dropbox/sokouo1/キチジ耳石写真")
img_list = c()
for(year in 2014:2018){
  if(year < 2016){
    for(no in 1:9){
      path = paste(year, "/No", no)
      list = list.files(path, pattern = "jpg") %>% data.frame() %>% mutate(year = paste(year), box = paste(no))
      img_list = rbind(img_list, list)
    }
  }else{
    for(no in 1:5){
      path = paste(year, "/No", no)
      list = list.files(path, pattern = "jpg") %>% data.frame() %>% mutate(year = paste(year), box = paste(no))
      img_list = rbind(img_list, list)
    }
  }
}
