# graficos apoio
# 
# 
library(plotly)

widget_file_size <- function(p,filename="grafico") {
  d <- tempdir()
  withr::with_dir(d, htmlwidgets::saveWidget(p, paste0(filename,".html")))
  f <- file.path(d, paste0(filename,".html"))
  mb <- round(file.info(f)$size / 1e6, 3)
  message("File is: ", mb," MB")
  cat(f)
  fs::file_move(f,paste0(getwd(),"/",filename,".html"))
}
#widget_file_size(p)
#widget_file_size(partial_bundle(p))


month <- c('January', 'February', 'March', 'April', 'May', 'June', 'July',
           'August', 'September', 'October', 'November', 'December')
high_2014 <- c(28.8, 28.5, 37.0, 56.8, 69.7, 79.7, 78.5, 77.8, 74.1, 62.6, 45.3, 39.9)
low_2014 <- c(12.7, 14.3, 18.6, 35.5, 49.9, 58.0, 60.0, 58.6, 51.7, 45.2, 32.2, 29.1)
data <- data.frame(month, high_2014, low_2014)
data$average_2014 <- rowMeans(data[,c("high_2014", "low_2014")])

#The default order will be alphabetized unless specified as below:
data$month <- factor(data$month, levels = data[["month"]])

fig <- plot_ly(data, x = ~month, y = ~high_2014, type = 'scatter', mode = 'lines',
               line = list(color = 'transparent'),
               showlegend = FALSE, name = 'High 2014') 
fig <- fig %>% add_trace(x = ~month, y = ~average_2014, type = 'scatter', mode = 'lines',
                         line = list(color='rgb(0,100,80)'),
                         name = 'Average') 
fig <- fig %>% layout(title = "Average, High and Low Temperatures in New York",
                      paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                      xaxis = list(title = "Months",
                                   gridcolor = 'rgb(255,255,255)',
                                   showgrid = TRUE,
                                   showline = FALSE,
                                   showticklabels = TRUE,
                                   tickcolor = 'rgb(127,127,127)',
                                   ticks = 'outside',
                                   zeroline = FALSE),
                      yaxis = list(title = "Temperature (degrees F)",
                                   gridcolor = 'rgb(255,255,255)',
                                   showgrid = TRUE,
                                   showline = FALSE,
                                   showticklabels = TRUE,
                                   tickcolor = 'rgb(127,127,127)',
                                   ticks = 'outside',
                                   zeroline = FALSE))

fig




## do trabalho
## 
data = base_completa$total_brasil

require(dplyr)
data = data %>% 
  mutate(total = as.numeric(vinculos_executivo_estadual)) %>% 
  select(ano, total)


fig <- plot_ly(data, x = ~ano, y = ~total, type = 'scatter', mode = 'lines',
               line = list(color = 'transparent'),
               showlegend = FALSE, name = '') 
fig <- fig %>% add_trace(x = ~ano, y = ~total, type = 'scatter', mode = 'lines',
                         line = list(color='rgb(0,100,80)'),
                         name = 'Valor') 

limites = list(0,1.1*as.numeric(max(data$total)))
fig <- fig %>% layout(title = "Brasil - Vínculos públicos no poder Executivo no nível estadual",
                      paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                      xaxis = list(title = "",
                                   gridcolor = 'rgb(255,255,255)',
                                   showgrid = TRUE,
                                   showline = FALSE,
                                   showticklabels = TRUE,
                                   tickcolor = 'rgb(127,127,127)',
                                   ticks = 'outside',
                                   zeroline = FALSE),
                      yaxis = list(title = "Vínculos",
                                   gridcolor = 'rgb(255,255,255)',
                                   showgrid = TRUE,
                                   showline = FALSE,
                                   showticklabels = TRUE,
                                   tickcolor = 'rgb(127,127,127)',
                                   ticks = 'outside',
                                   zeroline = TRUE,
                                   range = limites))

fig


# range = c(-4,4))


data = base_completa$total_brasil %>% 
  mutate(total = as.numeric(vinculos_executivo_estadual)) %>% 
  select(ano, total)


fig <- plot_ly(data, x = ~ano, y = ~total, type = 'scatter', mode = 'lines',
               line = list(color = 'transparent'),
               showlegend = FALSE, name = '') 
fig <- fig %>% add_trace(x = ~ano, y = ~total, type = 'scatter', mode = 'lines',
                         line = list(color='rgb(0,100,80)'),
                         name = 'Valor') 

limites = list(0,1.1*as.numeric(max(data$total)))
fig <- fig %>% layout(title = "Brasil - Vínculos públicos no poder Executivo no nível estadual",
                      paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                      xaxis = list(title = "",
                                   gridcolor = 'rgb(255,255,255)',
                                   showgrid = TRUE,
                                   showline = FALSE,
                                   showticklabels = TRUE,
                                   tickcolor = 'rgb(127,127,127)',
                                   ticks = 'outside',
                                   zeroline = FALSE),
                      yaxis = list(title = "Vínculos",
                                   gridcolor = 'rgb(255,255,255)',
                                   showgrid = TRUE,
                                   showline = FALSE,
                                   showticklabels = TRUE,
                                   tickcolor = 'rgb(127,127,127)',
                                   ticks = 'outside',
                                   zeroline = TRUE,
                                   range = limites))

fig

widget_file_size(fig,"v2")
