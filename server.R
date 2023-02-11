# https://data-sci-books.shinyapps.io/shinyapp/

if(!require(packcircles)) install.packages("packcircles", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")

library(shinyjs)
library(packcircles)
library(ggplot2)
library(viridis)
library(ggiraph)
library(tidyverse)
library(shiny)
library(DT)
library(leaflet)
library(plotly)
library(shinydashboard)
library(ggrepel)


function(input, output){
  
  # Andmed
  df <- read.csv("final_book_dataset_kaggle.csv", header=TRUE)
  
  # Jäta ainult vajalikud väljad
  df = df %>%  select(publisher, n_reviews, avg_reviews, title, star5, author, complete_link) %>% drop_na()
  
  # Eralda date ja publisher
  df = df %>% separate(publisher, c("publisher_orig", "published_date"), sep = "[()]")%>% drop_na()
  
  # Eralda kuupäev kuuks, päevaks, aastaks
  df = df %>% separate(published_date, c("month", "day","year"), sep = "[ ]")%>%  drop_na()
  
  # Eralda edition publisher-ist. Edition võib jääda NA-ks, seda ei kasuta.
  df = df %>% separate(publisher_orig, c("publisher","edition"), sep = ";")
  
  #Eemalda autoritest nurksulud
  df = df %>% mutate(author = str_remove(df$author, "[\\[]"))
  df = df %>% mutate(author = str_remove(df$author, "[\\]]"))
  df = df %>% mutate(author = gsub(",([A-Za-z])", ", \\1", df$author))
  df = df %>% mutate(publisher = str_trim(publisher, side = c("right")))
  
  
  # Circle packing graafik
  output$plot = renderGirafe({
    
    # Mulligraafiku andmed
    df = df  %>% 
         filter(n_reviews >= input$hinnangute_arv[1] & 
                n_reviews <= input$hinnangute_arv[2] & 
                avg_reviews >= input$kesk_hinnang[1] & 
                avg_reviews <= input$kesk_hinnang[2] & 
                year >= input$aasta[1] & 
                year <= input$aasta[2]) %>%
         filter(if(input$text  != "") {str_detect(title, regex(input$text, ignore_case = T))} else{TRUE})
    
    validate(
      need(nrow(df) != 0, "Selliseid filtritele vastavaid raamatuid pole. No books match these filter conditions.")
    )
    
    # Tekst, mida iga mull näitab
    df$text <- paste("<b>",df$title, "</b>\n",
                     "Autor:", df$author, "\n",
                     "Väljaanne: ", df$publisher, "\n",
                     "Avaldamisaasta: ", df$year, "\n",
                     "Hinnangute arv:", df$n_reviews, "\n",
                     "Keskmine hinnang: ", df$avg_reviews
    )
    
    # Layout
    packing <- circleProgressiveLayout(df$n_reviews, sizetype='area')
    data <- cbind(df, packing)
    dat.gg <- circleLayoutVertices(packing, npoints=50)
    
    # Javascript Amazoni lingi avamiseks
    df$onclick <- sprintf("window.open(\"%s\")", as.character(df$complete_link))
    
    # Plot
    p <- ggplot() + 
      geom_polygon_interactive(data = dat.gg,
                               aes(x, y, 
                                   group = id, 
                                   fill=df$avg_reviews[id],
                                   tooltip = df$text[id], 
                                   data_id = id, 
                                   onclick = df$onclick[id]), 
                               colour = 'black', 
                               alpha = 0.6) +
      scale_fill_viridis(option = 'D') +
      geom_text(data = data, 
                aes(x, y, label = gsub("Average reviwes:", "", avg_reviews)), 
                size=2, 
                color="black") +
      theme_void() + 
      theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
      coord_equal()
    
    # Interaktiivseks ggiraph-iga
    widg <- ggiraph(ggobj = p, width_svg = 7, height_svg = 7,options = list(
      opts_hover_inv(css = "opacity:0.5;") ) )
    
    widg
    
  })
  
  # Andmetabel, mis sõltub circle packing graafiku filtritest
  output$table_main = DT::renderDataTable({
    
    # Andmed
    df = df %>%
         filter(year >= input$aasta[1] & year <= input$aasta[2]) %>%
         filter(n_reviews >= input$hinnangute_arv[1] & 
                n_reviews <= input$hinnangute_arv[2] & 
                avg_reviews >= input$kesk_hinnang[1] & 
                avg_reviews <= input$kesk_hinnang[2])%>%
         filter(if(input$text  != "") {str_detect(title, regex(input$text, ignore_case = T))} else{TRUE})
    
    validate(
      need(nrow(df) != 0, "Selliseid filtritele vastavaid raamatuid pole. No books match these filter conditions.")
    )
    
    df = df %>%
         select(title, author, publisher, year, n_reviews, avg_reviews, star5) %>%
         relocate(any_of(c("title", "author", "publisher", "year", "n_reviews","avg_reviews","star5")))
    
    datatable(
              df,
              options = list(paginate = F, scrollY = 500,selection = 'single'),
              colnames = c("Pealkiri"="title",
                           "Autor"="author",
                           "Väljaandja"="publisher",
                           "Aasta"="year",
                           "Arvustuste arv"="n_reviews",
                           "Keskmine arvustus"="avg_reviews",
                           "5-tärni arvustuste osakaal"="star5")
              )
    })
  
  
  # Mitu raamatut filtritele vastab
  output$raamatuid <- renderText({ 
    
    df = df  %>% 
         filter(year >= input$aasta[1] & year <= input$aasta[2]) %>%
         filter(n_reviews >= input$hinnangute_arv[1] & 
                n_reviews <= input$hinnangute_arv[2] & 
                avg_reviews >= input$kesk_hinnang[1] & 
                avg_reviews <= input$kesk_hinnang[2]) %>%
         filter(if(input$text  != "") {str_detect(title, regex(input$text, ignore_case = T))} else{TRUE})
    
    value = df %>% summarise(n())
    paste(as.character(value[1]), " raamatut vastab nendele filtritele.")
  })
  
  
  # Kui tabeli reale klikitakse, ava vastava raamatu Amazoni link
  observeEvent(input$table_main_rows_selected, {
    
    df = df  %>% 
         filter(year >= input$aasta[1] & year <= input$aasta[2]) %>%
         filter(n_reviews >= input$hinnangute_arv[1] & 
                n_reviews <= input$hinnangute_arv[2] & 
                avg_reviews >= input$kesk_hinnang[1] & 
                avg_reviews <= input$kesk_hinnang[2]) %>%
         filter(if(input$text  != "") {str_detect(title, regex(input$text, ignore_case = T))} else{TRUE})
    
    runjs(sprintf('window.open("%s");',df$complete_link[input$table_main_row_last_clicked]))
    print(input$table_main_row_last_clicked)
    
  })
  
  
  # Igas aastas avaldatud raamatute graafik
  output$plot2 = renderPlotly({
    xaxis <- list(title = 'Avaldamisaasta',
                  showline = TRUE,
                  showgrid = FALSE,
                  showticklabels = TRUE,
                  linecolor = 'rgb(204, 204, 204)',
                  linewidth = 2,
                  autotick = FALSE,
                  ticks = 'outside',
                  tickcolor = 'rgb(204, 204, 204)',
                  tickwidth = 2,
                  ticklen = 5,
                  tickfont = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgb(82, 82, 82)'))
    
    yaxis <- list(title = 'Raamatute arv',
                  showgrid = FALSE,
                  zeroline = FALSE,
                  showline = FALSE,
                  showticklabels = FALSE)
    
    fig <-
      plot_ly(data = df %>% 
                     filter(year <2022)%>%
                     group_by(year) %>%
                     summarise(n = n()),
              x = ~year,
              text = ~n,
              hoverinfo = 'text',
              hovertext = ~paste('Aasta: ', year,
                                 '<br> Raamatute arv: ',n),
              showlegend = FALSE) %>% 
              add_trace(y = ~n, 
                        type = 'scatter', 
                        mode = 'lines+markers+text', 
                        line = list(color = 'rgba(67,67,67,1)', 
                                    width = 2),
                        textposition = "top left",
                marker = list(color = 'rgb(242,142,43)', 
                              size = 8)) %>%  
              layout(title = "Andmeteaduse raamatuid avaldatakse järjest rohkem",
                     yaxis = yaxis, 
                     xaxis = xaxis, 
                     showlegend = FALSE)
    
    fig
    
  })
  
  # Väljaandjate graafik
  output$plot3 = renderPlotly({
    fig <- plot_ly(df %>% group_by(publisher) %>% summarise(n = n())%>% filter(n >10),
                   x = ~n, 
                   y = ~publisher, 
                   type = 'bar',
                   orientation = 'h', 
                   text = ~n,
                   marker = list(color = c('rgba(204,204,204,1)', 'rgba(204,204,204,1)',
                                           'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
                                           'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
                                           'rgba(242,142,43,1)', 'rgba(204,204,204,1)',
                                           'rgba(204,204,204,1)', 'rgba(242,142,43,1)',
                                           'rgba(242,142,43,1)', 'rgba(204,204,204,1)',
                                           'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
                                           'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
                                           'rgba(204,204,204,1)', 'rgba(204,204,204,1')),
                   hoverinfo = 'text',
                   hovertext = ~paste('Väljaandja: ', publisher,
                                      '<br> Raamatute arv: ',n))
    
    fig <- fig %>% layout(title = "Kõige rohkem andmeteaduse raamatuid on välja andnud Packt Publishing",
                          xaxis = list(title = "",
                                       zeroline = FALSE,
                                       showline = FALSE,
                                       showticklabels = FALSE,
                                       showgrid = FALSE),
                          yaxis = list(title = "",
                                       categoryorder = "total ascending"))
    
    fig
    
  })
  
  # Koguandmete tabel
  output$table = DT::renderDataTable({
    
    df = df %>% 
         select(title, author, publisher, year, n_reviews, avg_reviews, star5, complete_link) %>% 
         relocate(any_of(c("title", "author", "publisher", "year", "n_reviews","avg_reviews","star5","complete_link")))
    
    datatable(
      df,
      options = list(paginate = F, searching = T, scrollY = 500),
      colnames = c(
        "Pealkiri"="title",
        "Autor"="author",
        "Väljaandja"="publisher",
        "Aasta"="year",
        "Arvustuste arv"="n_reviews",
        "Keskmine arvustus"="avg_reviews",
        "%5 tärni"="star5",
        "link"="complete_link")
    ) 
  })
  
}