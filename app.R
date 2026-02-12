library(shiny)
library(ggplot2)
library(dplyr)


summary_df <- readRDS("summary_df.rds")
fc_df <- readRDS("fc_df.rds")

gene_choices <- summary_df %>%
  distinct(WBID, gene_name) %>%
  mutate(
    label = ifelse(
      is.na(gene_name) | gene_name == "",
      WBID,
      paste0(gene_name)
    )
  )


ui <- fluidPage(
  titlePanel(
    HTML("Gene expression fold-change throughout desiccation and recovery in <i>Caenorhabditis elegans</i>")
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "gene",
        label = "Search and select gene(s):",
        choices = NULL,
        multiple = TRUE
      ),
      
      checkboxInput(
        inputId = "showReplicates",
        label = "Show individual replicates",
        value = FALSE
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Plot",
          plotOutput("distPlot"),
          downloadButton("downloadPlot", "Download Plot")
        ),
        tabPanel(
          "Experimental summary and citation",
          htmlOutput("hello")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  updateSelectizeInput(
    session,
    "gene",
    choices = setNames(gene_choices$WBID, gene_choices$label),
    server = TRUE
  )
  

  plotInput <- function(selected_wbid, show_reps = FALSE) {
    
    df_summary <- summary_df %>% filter(WBID %in% selected_wbid)
    df_reps <- fc_df %>% filter(WBID %in% selected_wbid)
    
    p <- ggplot(
      df_summary,
      aes(x = stage, y = fc_mean, group = WBID, color = gene_name)
    ) +
      geom_line(linewidth = 1) +
      geom_point(size = 3)
    
    if (show_reps) {
      p <- p +
        geom_point(
          data = df_reps,
          aes(x = stage, y = fc),
          position = position_jitter(width = 0.15),
          alpha = 0.5,
          size = 1.5
        )
    }
    
    p +
      scale_y_continuous(trans = "log2") +
      labs(
        x = "Experimental Stage",
        y = "Fold change vs control (log2)",
        title = "Gene expression recovery after desiccation",
        color = "Gene"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1)
      )
  }
  

  output$distPlot <- renderPlot({
    req(input$gene)
    plotInput(input$gene, input$showReplicates)
  })
  

  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("gene_foldchange_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(
        file,
        plot = plotInput(input$gene, input$showReplicates),
        width = 8,
        height = 5,
        dpi = 300,
        bg = "white"
      )
    }
  )
  

  output$hello <- renderUI({
    HTML("Fill in later")
  })
}


shinyApp(ui = ui, server = server)
