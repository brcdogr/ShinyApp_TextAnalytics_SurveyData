### Dashboard Team 6 ###

## app.R ##
library(shiny)
library(shinydashboard)
library(wordcloud2)
## install.packages("wordcloud2")
library(wordcloud2)
library(DT)
library(wordcloud) # visualize the keyword as a word cloud
library(ggplot2)   # visualization tool
library(igraph)    # for bigram network
library(ggraph)    # for bigram network
library(widyr)
library(plot)
install.packages("plot")

ui <- dashboardPage( skin = "blue",
                     dashboardHeader(title = "Beer Superbowl Promotion Analysis"),
                     dashboardSidebar(
                         sidebarMenu(
                             id = "tabs",
                             
                             menuItem("Token Frequency", tabName = "frequency", icon = icon("chart-bar")),
                             menuItem("Token Correlogram", tabName = "Correlogram", icon = icon("chart-bar")),
                             menuItem("TF-IDF - Unique Words Analysis", tabName = "unique_words", icon = icon("chart-bar")),
                             menuItem("Sentiment - Word Clouds", tabName = "sentiment", icon = icon("cloud")),
                             menuItem("Sentiment Analysis", tabName = "yes_no_sent", icon = icon("chart-bar")),
                             menuItem("Sentiment Analysis type", tabName = "afin_bing_nrc", icon = icon("chart-bar")),
                             menuItem("Sentiment Analysis Words (Bing)", tabName = "positive_negative", icon = icon("chart-bar")),
                             menuItem("Sentiment Analysis Words (NRC)", tabName = "nrc_words", icon = icon("chart-bar")),
                             menuItem("TF-IDF - Term Frequency", tabName = "TF", icon = icon("line-chart")),
                             menuItem("Bigram & N-gram ", tabName = "networks", icon = icon("cloud")),  
                             menuItem("Word Correlation Network", tabName = "word_networks", icon = icon("cloud"))
                         ),
                         
                         textOutput("res")
                         
                     ),
                     
                     dashboardBody(
                         tabItems(
                             
                             #frequency histograms
                             tabItem(tabName = "frequency",h2("Token Frequency in Responses"),
                                     
                                     fluidRow(
                                         box(
                                             width = 6,
                                             selectInput("freq_select","Select Success/Failure",selected = 1,choices = c("Success","Failure"))
                                         ),
                                         box(
                                             width = 12,
                                             title = "Frequency Histograms",
                                             plotly::plotlyOutput("hist"),
                                             
                                         )
                                     )
                             ),
                             
                             
                             #sentiment analysis plots - Word cloud - BING
                             tabItem(tabName = "sentiment", h2("Sentiment Analysis"),
                                     
                                     fluidRow(
                                         box(
                                             width = 6,
                                             selectInput("sent_select","Select Outcome",selected = 1,choices = c("Success","Failure"))
                                         ),
                                         box(
                                             width = 12,
                                             title = "Word Clouds - Bing and NRC",
                                             ## wordcloud2Output(outputId = "bing_cloud")
                                             ## renderWordcloud2("bing_cloud"),
                                             ## renderWordcloud2("nrc_cloud")
                                             plotOutput("plot"),
                                             plotOutput("nrc_plot")
                                         )
                                     )
                             ),
                             
                             #sentiment analysis plots - Positive - Negative
                             tabItem(tabName = "yes_no_sent", h2("Positive - Negetive"),
                                     
                                     fluidRow(
                                         box(
                                             width = 6,
                                             selectInput("positive_negative", "Select Sentiment type", c("Bing", "NRC"))
                                         ),
                                         box(
                                             width = 12,
                                             title = "positive and negative sentiments",
                                             plotOutput("type"),
                                             
                                         )
                                     )
                             ),
                             #sentiment analysis Types - Afin, Bing and NRC
                             tabItem(tabName = "afin_bing_nrc", h2("Types of Sentiment based on outcome"),
                                     
                                     fluidRow(
                                         box(
                                             width = 6,
                                             selectInput("select_out", "Select Business Outcome", c("Success", "Failure"))
                                         ),
                                         box(
                                             width = 12,
                                             title = "Types of Sentiments",
                                             plotOutput("sentiment_type"),
                                             
                                         )
                                     )
                             ),
                             
        
                             
                             ## Networks
                             tabItem(tabName = "networks", h2("Word correlation network"),
                                     
                                     fluidRow(
                                         box(
                                             width = 6,
                                             selectInput("select_output", "Select Business Outcome", c("Success", "Failure"))
                                         ),
                                         box(
                                             width = 12,
                                             title = "Networks",
                                             plotOutput("network"),
                                             
                                         )
                                     )
                             ),
                             ## Correlogram
                             tabItem(tabName = "Correlogram", h2("Correlation Among Words"),
                                     
                                     fluidRow(
                                         
                                         box(
                                             width = 12,
                                             title = "Correlagram",
                                             plotOutput("Correlagram"),
                                             
                                         )
                                     )
                             ),
                             
                             ## Unique Words TF-IDF
                             tabItem(tabName = "unique_words", h2("TF - IDF: Unique Words"),
                                     
                                     fluidRow(
                                         
                                         box(
                                             width = 12,
                                             title = "Unique Words Analysis",
                                             plotOutput("unique"),
                                             
                                         )
                                     )
                             ),
                             
                             ## Most often Positive and Negative words used (BING)
                             tabItem(tabName = "positive_negative", h2("Most common positive and negative words in Outcome"),
                                     
                                     fluidRow(
                                         box(
                                             width = 6,
                                             selectInput("select_options", "Select Business Outcome", c("Success", "Failure"))
                                         ),
                                         box(
                                             width = 12,
                                             title = "Most Common Words",
                                             plotOutput("common"),
                                             
                                         )
                                     )
                             ),
                             
                             # Most often Positive and Negative words used (NRC)
                             tabItem(tabName = "nrc_words", h2("Most common NRC words in Outcome"),
                                     
                                     fluidRow(
                                         box(
                                             width = 6,
                                             selectInput("select_words", "Select Business Outcome", c("Success", "Failure"))
                                         ),
                                         box(
                                             width = 12,
                                             title = "Most Common NRC Words",
                                             plotOutput("nrc_words"),
                                             
                                         )
                                     )
                             ),
                             
                             
                             ## Term Frequency
                             tabItem(tabName = "TF", h2("TF-IDF: Term Frequency Analysis"),
                                     
                                     fluidRow(
                                         
                                         box(
                                             width = 12,
                                             title = "TF-IDF: Term Frequency",
                                             plotOutput("TF"),
                                             
                                         )
                                     )
                             ),
                             ## Word Networks
                             tabItem(tabName = "word_networks", h2("Word correlation network"),
                                     
                                     fluidRow(
                                         box(
                                             width = 6,
                                             selectInput("select_networks", "Select Business Outcome", c("Success", "Failure"))
                                         ),
                                         box(
                                             width = 12,
                                             title = "Word Networks",
                                             plotOutput("word_network"),
                                             
                                         )
                                     )
                             )
                         )
                     )
)

server <- function(input, output) {
    
    output$res <- renderText({
        paste("You've selected:", input$tabs)
    })
    
    
    hist <- function() {
        if(input$freq_select=='Success'){
            tidy_success %>%
                filter(n > 7) %>%
                mutate(word = reorder(word, n)) %>%
                ggplot(aes(word, n)) +
                geom_col(fill="orange") +
                xlab(NULL) +
                coord_flip()}
        else 
            tidy_failure %>%
            filter(n > 4) %>%
            mutate(word = reorder(word, n)) %>%
            ggplot(aes(word, n)) +
            geom_col(fill="cyan4") +
            xlab(NULL) +
            coord_flip()
        
    }
    
    output$hist <- plotly::renderPlotly({hist()})
    
    plot <- function(){
        if(input$sent_select=='Success'){
            business_success_tokens %>%
                anti_join(stop_words, by="word") %>%
                inner_join(get_sentiments("bing"), by="word") %>% # binary
                count(word, sentiment, sort=TRUE) %>%
                acast(word ~sentiment, value.var="n", fill=0) %>%
                comparison.cloud(colors = c("red", "darkgreen"),scale = c(5,0.5),
                                 max.words=100,random.order=FALSE,rot.per=0.35,user.r.layout=FALSE)}
        
        
        else 
            business_failure_tokens %>%  
            anti_join(stop_words, by="word") %>%
            inner_join(get_sentiments("bing"), by="word") %>% # binary
            count(word, sentiment, sort=TRUE) %>%
            acast(word ~sentiment, value.var="n", fill=0) %>%
            comparison.cloud(colors = c("grey20", "lightblue"),scale=c(5,0.5),
                             max.words=100,random.order=FALSE,rot.per=0.35,user.r.layout=FALSE)
        
    }
    output$plot <- renderPlot({plot()})
    
    nrc_plot <- function(){
        if(input$sent_select=='Success'){
            business_success_tokens %>%
                anti_join(stop_words, by="word") %>%
                inner_join(get_sentiments("nrc"), by="word") %>% # flavor
                count(word, sentiment, sort=TRUE) %>%
                acast(word ~sentiment, value.var="n", fill=0) %>%
                comparison.cloud(colors = c("grey20", "gray80"),
                                 max.words=100, scale=c(1.2, 0.8),
                                 fixed.asp=TRUE, title.size=0.9)
        }
        else business_failure_tokens %>%
            anti_join(stop_words, by="word") %>%
            inner_join(get_sentiments("nrc"), by="word") %>% # flavor
            count(word, sentiment, sort=TRUE) %>%
            acast(word ~sentiment, value.var="n", fill=0) %>%
            comparison.cloud(colors = c("grey20", "gray80"),
                             max.words=100, scale=c(1.2, 0,8),
                             fixed.asp=TRUE, title.size=0.9)
        
        
    }
    output$nrc_plot <- renderPlot({nrc_plot()})
    
    type <- function(){
        if(input$positive_negative=='Bing'){
            ggplot(superbowl_sentiment, aes(index, sentiment, fill = superbowl)) +
                geom_col(show.legend = FALSE) +
                facet_wrap(~superbowl, ncol = 2, scales = "free_y")
        }
        else 
            ggplot(superbowl_sentiment, aes(index, sentiment, fill = superbowl)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~superbowl, ncol = 2, scales = "free_x")}
    
    output$type <- renderPlot({type()})
 
sentiment_type <- function(){
    if(input$select_out=='Success'){
        bind_rows(afinn, 
                  bing_and_nrc) %>%
            ggplot(aes(index, sentiment, fill = method)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~method, ncol = 1, scales = "fixed")
    }
    else 
        bind_rows(afinn, 
                  bing_and_nrc) %>%
        ggplot(aes(index, sentiment, fill = method)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~method, ncol = 1, scales = "fixed")
}
    output$sentiment_type <- renderPlot({sentiment_type()})
    
network <- function(){
    if(input$select_output=='Success'){
        ggraph(business_success_bigram_graph, layout = "fr") +
            geom_edge_link()+
            geom_node_point()+
            geom_node_text(aes(label=name), vjust =1, hjust=1)
    }
    else 
        ggraph(business_success_bigram_graph, layout = "fr") +
        geom_edge_link()+
        geom_node_point()+
        geom_node_text(aes(label=name), vjust =1, hjust=1)
}    
      output$network <- renderPlot(({network()})) 
      
      Correlagram <- function(){
          ggplot(frequency, aes(x=proportion, y=`No`, 
                                color = abs(`No`- proportion)))+
              geom_abline(color="grey40", lty=2)+
              geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
              geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
              scale_x_log10(labels = percent_format())+
              scale_y_log10(labels= percent_format())+
              scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
              facet_wrap(~superbowl, ncol=2)+
              theme(legend.position = "none")+
              labs(y= "No", x=NULL)
          
      }
      output$Correlagram <- renderPlot(({Correlagram()}))
      

unique <- function(){
        ggplot(superbowl_words, aes(n/total, fill = superbowl))+
        geom_histogram(show.legend=FALSE,na.rm = TRUE)+
        xlim(NA, 0.1) +
        facet_wrap(~superbowl, ncol=2, scales="free_y")
    
    
}
    output$unique <- renderPlot(({unique()}))


common <- function(){
    
    if(input$select_options=='Success'){
        Success_bing_counts %>%
            group_by(sentiment) %>%
            top_n(10) %>%
            ungroup() %>%
            mutate(word=reorder(word, n)) %>%
            ggplot(aes(word, n, fill=sentiment)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~sentiment, scales = "free_y")+
            labs(y="Contribution to sentiment", x=NULL)+
            coord_flip()
 }
    
    else
        Failure_bing_counts %>%
        group_by(sentiment) %>%
        top_n(10) %>%
        ungroup() %>%
        mutate(word=reorder(word, n)) %>%
        ggplot(aes(word, n, fill=sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y")+
        labs(y="Contribution to sentiment", x=NULL)+
        coord_flip()
    
} 

      output$common <- renderPlot(({common()}))
      
      nrc_words <- function(){
          
          if(input$select_words=='Success'){
              Success_nrc_counts %>%
                  group_by(sentiment) %>%
                  top_n(10) %>%
                  ungroup() %>%
                  mutate(word=reorder(word, n)) %>%
                  ggplot(aes(word, n, fill=sentiment)) +
                  geom_col(show.legend = FALSE) +
                  facet_wrap(~sentiment, scales = "free_y")+
                  labs(y="Contribution to sentiment", x=NULL)+
                  coord_flip()}
              
          
          else
              Failure_nrc_counts %>%
                  group_by(sentiment) %>%
                  top_n(10) %>%
                  ungroup() %>%
                  mutate(word=reorder(word, n)) %>%
                  ggplot(aes(word, n, fill=sentiment)) +
                  geom_col(show.legend = FALSE) +
                  facet_wrap(~sentiment, scales = "free_y")+
                  labs(y="Contribution to sentiment", x=NULL)+
                  coord_flip()
              
      } 
      
      output$nrc_words <- renderPlot(({nrc_words()})) 
      
      
      TF <- function(){
          freq_by_rank %>%
              ggplot(aes(rank, `term frequency`, color=superbowl))+
              geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
              geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
              scale_x_log10()+
              scale_y_log10()
         
      }
      output$TF <- renderPlot(({TF()}))
      
      word_network <- function(){
          if(input$select_networks=='Success'){
              business_success_corrs %>%
                  filter(correlation >0.7) %>%
                  graph_from_data_frame() %>%
                  ggraph(layout = "fr") +
                  geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
                  geom_node_point(color = "lightgreen", size=6)+
                  geom_node_text(aes(label=name), repel=T)+
                  theme_void()
          }
          else 
              business_failure_corrs %>%
              filter(correlation >0.9) %>%
              graph_from_data_frame() %>%
              ggraph(layout = "fr") +
              geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
              geom_node_point(color = "lightgreen", size=8)+
              geom_node_text(aes(label=name), repel=T)+
              theme_void()
      }    
      output$word_network <- renderPlot(({word_network()})) 
}    
      
shinyApp(ui, server)




