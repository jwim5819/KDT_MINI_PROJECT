library(plotly)
library(gapminder)

df <- data.frame(
  x = c(1,2,1), 
  y = c(1,2,1), 
  f = c(1,2,3)
)

fig <- df %>%
  plot_ly(
    x = ~x,
    y = ~y,
    frame = ~f,
    type = 'scatter',
    mode = 'markers',
    showlegend = F
  )

fig

# x축은 gdpPercap, y축은 lifeExp
# hover는 국가, 인구가 나타나도록
str(gapminder)
gapminder %>%
  plot_ly(x = ~ gdpPercap,
          y = ~lifeExp,
          name = ~ continent,
          hovertext = ~ country,
          hoverinfo = 'pop')
