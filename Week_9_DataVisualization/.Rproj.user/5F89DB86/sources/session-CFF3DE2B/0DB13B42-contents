library(esquisse)
library(dplyr)
library(ggplot2)
library(wordcloud2) 
library(tidyverse)
library(plotly)
library(nord)
library(RColorBrewer)
library(htmlwidgets)
library(jsonlite)
library(yaml)
library(base64enc)
library(devtools)
library(htmltools)
library(tm)
library(showtext)
library(devtools)
devtools::install_github("lchiffon/wordcloud2")



esquisse::esquisser()
windows(width = 7, height = 5)

# DF로딩
platformsDF <- read.csv(file = './Kdrama_data/platforms.csv')
platformsDF
imdbDF <- read.csv(file = './Kdrama_data/imdb.csv')
imdbDF

# 결측치 확인 및 제거
platformsDF[platformsDF == ''] <- NA
dim(platformsDF)
VIM::aggr(platformsDF)
platformsDF <- na.omit(platformsDF)
dim(platformsDF)
VIM::aggr(platformsDF)

# Unique값 확인
unique(platformsDF$platform)

# 공백제거 후 재확인
platformsDF$platform <- platformsDF$platform %>% str_trim()
platformsDF[platformsDF == ''] <- NA
platformsDF <- na.omit(platformsDF)
unique(platformsDF$platform)

# 상위 20개 데이터 추출
word_data <- platformsDF %>% group_by(platform) %>% summarise(count = n())
word_data_top20 <- word_data[order(-word_data$count), ]
word_data_top20 <- subset(word_data_top20, count >= 5)
word_data_top20

# 정제된 데이터 저장
write.csv(platformsDF,file="./Kdrama_data/platforms_edit.csv")
write.csv(word_data_top20,file="./Kdrama_data/platforms_edit_top20.csv")
write.csv(imdbDF_word,file="./Kdrama_data/imdbDF_word.csv")



# TOP20 OTT 플랫폼 원형 바 그래프==============================================
word_data_top20$rno <- c(1:20)
word_data_top20$order <- c(20:1)
word_data_top20$order <- word_data_top20$order * 20

lb <- word_data_top20
angle <-  90-(360*(lb$rno-0.5)/nrow(word_data_top20))
lb$hjust<-ifelse(angle < -90, 1, 0)
lb$angle<-ifelse(angle < -90, angle+180, angle)
color_1 <- c('#edf5fa','#e0eef6','#d3e7f2','#c5e0ee','#b8d9eb',
             '#abd2e7','#9ecbe3','#91c4df','#83bddc','#76b6d8',
             '#69afd4','#5ca8d0','#4fa1cc','#419ac9','#3792c2',
             '#3488b4','#307ea7','#2c749a','#286a8d','#256080')
color_1 <- rev(color_1)

ggplot(lb, aes(x=as.factor(rno), y=order+100))+
  geom_bar(stat='identity', fill=color_1)+
  ylim(-100,600)+
  theme_void()+
  coord_polar(start=0)+
  geom_text(data=lb, 
            aes(x=rno, y=order+150, label=platform, hjust=hjust),
            color='black', size=8,
            fontface = 'bold',
            angle=lb$angle, inherit.aes=FALSE,
            family="my1")

# imdb유저수 1000명 이상인 것만 필터링
imdbDF$imdb_users <- gsub(",", "", imdbDF$imdb_users)
imdbDF$imdb_users <- as.integer(imdbDF$imdb_users)
dim(imdbDF)
imdbDF <- subset(imdbDF, imdb_users >= 1000 & imdb_users <= 1000000)
imdbDF <- subset(imdbDF, imdb_rating >= 4.5)
dim(imdbDF)
str(imdbDF)

# 유저 대비 평점 산점도=========================================================
color_2 <- brewer.pal(5,"Set3")

rating <- ggplot(imdbDF, aes(x = log10(imdb_users), 
                             y = imdb_rating, 
                             colour = log(imdb_users), 
                             size = imdb_users,
                             text = paste(' Drama Name:', kdrama_name, '<br>',
                                          'Rating:', imdb_rating, '<br>',
                                          'Users:', imdb_users, '<br>'))) +
  geom_point(shape = 'circle small') +
  scale_color_distiller(palette = 'Pastel1', direction = 1) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position='none',
        axis.text.x=element_blank())+
  xlab('Users')+
  ylab('Rating')

ggplotly(rating, tooltip = 'text')

# 드라마 인기별 word cloud======================================================
imdbDF_word <- imdbDF[,c(1, 3)]
imdbDF_word <- rename(imdbDF_word, "word" = "kdrama_name")
imdbDF_word <- rename(imdbDF_word, "freq" = "imdb_users")
imdbDF_word$freq <- sqrt(imdbDF_word$freq)
imdbDF_word

pal <- brewer.pal(n = 8, name = 'Set2')



font_add(family = "my1", regular = "etc\\DynaPuff-Regular.ttf")

wordcloud2(imdbDF_word, figPath = "etc\\TV.png", size = 0.7, 
           backgroundColor="white", fontFamily = "mono",
           color = pal)
