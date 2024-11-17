library(shiny)
library(ggplot2)
library(esquisse)
library(dplyr)
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
library(colorspace)
library(extrafont)
Sys.setlocale('LC_ALL','C')
# ==================================================================================================================

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

font_add(family = "my2", regular = "etc/DynaPuff-Regular.ttf")

font_add_google(family = 'test1', 'Padauk')

font_families_google()

font_families()

ggplot(lb, aes(x=as.factor(rno), y=order+100))+
  geom_bar(stat='identity', fill=color_1)+
  ylim(-100,600)+
  theme_void()+
  coord_polar(start=0)+
  geom_text(data=lb, 
            aes(x=rno, y=order+150, label=platform, hjust=hjust),
            size=6,
            fontface = 'bold',
            angle=lb$angle, inherit.aes=FALSE,
            family="mono")

warnings()

word_data_top20_view <- word_data_top20[,c(1,2,3)]
word_data_top20_view <- rename(word_data_top20_view, "ranking" = "rno")

# imdb유저수 1000명 이상인 것만 필터링
imdbDF$imdb_users <- gsub(",", "", imdbDF$imdb_users)
imdbDF$imdb_users <- as.integer(imdbDF$imdb_users)
dim(imdbDF)
imdbDF <- subset(imdbDF, imdb_users >= 1000 & imdb_users <= 1000000)
imdbDF <- subset(imdbDF, imdb_rating >= 4.5)
dim(imdbDF)
str(imdbDF)

# 유저 대비 평점 산점도=========================================================
rating <- ggplot(imdbDF, aes(x = log10(imdb_users), 
                             y = imdb_rating, 
                             customdata = imdb_description,
                             colour = sqrt(imdb_users), 
                             size = imdb_users,
                             text = paste(' Drama Name:', kdrama_name, '<br>',
                                          'Rating:', imdb_rating, '<br>',
                                          'Users:', imdb_users, '<br>'))) +
  geom_point(shape = 'circle small') +
  # scale_color_continuous_sequential(palette='Heat2') + 
  scale_color_distiller(palette = 'Accent', direction = 1) +
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

wordcloud2(imdbDF_word, figPath = "etc/TV.png", size = 0.7, 
           backgroundColor="white", fontFamily = "mono",
           color = pal)

# ==================================================================================================================

server <- function(input, output, session){
  
  # output$graph_1 <- renderPlot({
  # ggplot(lb, aes(x=as.factor(rno), y=order+100))+
  #   geom_bar(stat='identity', fill=color_1)+
  #   ylim(-100,600)+
  #   theme_void()+
  #   coord_polar(start=0)+
  #   geom_text(data=lb, 
  #             aes(x=rno, y=order+150, label=platform, hjust=hjust),
  #             color='black', size=7,
  #             fontface = 'bold',
  #             angle=lb$angle, inherit.aes=FALSE,
  #             family="my1")
  # })

  
  output$graph_2 <- renderPlotly({
    ggplotly(rating, tooltip = 'text', source = "Plot1")
  })
  
  
  output$pic_1 <- renderImage(deleteFile = F,{
    width  <- session$clientData$output_pic_1_width
    height <- session$clientData$output_pic_1_height
    list(src = "etc/WordCloud.png",
         width = 1200,
         height = 900)
  })
  
  output$pic_2 <- renderImage(deleteFile = F,{
    width  <- session$clientData$output_pic_1_width
    height <- session$clientData$output_pic_1_height
    list(src = "etc/CircleBarPlot.png",
         width = 600,
         height = 600)
  })

  output$click <- renderUI({
    d <- event_data("plotly_click", source = 'Plot1')
    if (!is.null(d)) 
      {
      HTML('<span style=" font: italic bold 2em/1.5em Comic Sans MS, serif ;">', 
           '[Description]',
           '</span>',
           '<br>',
           '<br>',
           '<span style=" font: italic 1.5em/1.5em Comic Sans MS, serif ;">', 
           paste(d$customdata), 
           '</span>')
    }
  })
  
  output$bartitle <- renderUI({
    HTML('<br>',
         '<div calss="center">',
         '<span style=" font: italic bold 2em/1.5em Comic Sans MS, serif ;">', 
         '[ TOP 20 Platforms ]',
         '</span>',
         '</div>')
  })
  
  output$scattertitle <- renderUI({
    HTML('<br>',
         '<div calss="center">',
         '<span style=" font: italic bold 2em/1.5em Comic Sans MS, serif ;">', 
         '[ Users & Rating ]',
         '</span>',
         '</div>')
  })
  
  output$Cloudtitle <- renderUI({
    HTML('<br>',
         '<div calss="center">',
         '<span style=" font: italic bold 2em/1.5em Comic Sans MS, serif ;">', 
         '[ Drama Cloud ]',
         '</span>',
         '</div>')
  })
  
  output$DF <- renderTable({
    word_data_top20_view
  })
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #pic_2 img {
        max-width: 100%; 
        height: auto; 
      }
    "))
  ),
  titlePanel(
    h1(HTML('<span style=" font: italic bold 1em Comic Sans MS, serif ;">',
       '&nbsp&nbspK-Drama Data Visualization',
       '</span>'))
  ),
  # sidebarPanel(width = 3,
  #   checkboxGroupInput('showvars',
  #                      HTML('<span style=" font: italic bold 1em Comic Sans MS, serif ;">',
  #                           'Choose OTT Platform',
  #                           '</span>'),
  #                      # names(penguins),
  #                      # selected = names(penguins)
  #                      ),
  # ),
  fluidPage(
    tabsetPanel(
      tabPanel(HTML('<span style=" font: italic bold 1em Comic Sans MS, serif ;">',
                    'Platforms Ranking',
                    '</span>'),
               
        fluidRow(
          htmlOutput("bartitle"),
          splitLayout(cellWidths = c("70%", "30%"), 
                      imageOutput('pic_2'), 
                      tableOutput('DF')))
        
),
      tabPanel(HTML('<span style=" font: italic bold 1em Comic Sans MS, serif ;">',
                    'Users & Rating',
                    '</span>'),
               htmlOutput("scattertitle"),
               fluidPage(plotlyOutput('graph_2'), 
                         htmlOutput("click")
                         )
               ),
      tabPanel(HTML('<span style=" font: italic bold 1em Comic Sans MS, serif ;">',
                    'Drama Cloud',
                    '</span>'),
               htmlOutput("Cloudtitle"),
               imageOutput('pic_1'))
      
      
    )
  )
)

shinyApp(ui, server)