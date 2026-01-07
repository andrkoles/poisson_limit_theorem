library(shiny)
library(ggplot2)
library(tibble)
library(bslib)

# https://forum.posit.co/t/adding-manual-legend-to-ggplot2/41651/2
colors <- c("Poisson" = "blue3", "Binomial" = "red3")

ui <- page_fillable(
  h3("Poisson Limit Theorem"),
  layout_columns(
    card(
      card(
        sliderInput("n", "n", min = 10, max = 1000, value = 10, animate = TRUE, 
                    step = 5),
      ),
      card(
        markdown("The binomial distribution is well approximated by the
                 Poisson distribution under certain conditions. Specifically
                 if: "),
        withMathJax("$$n\\rightarrow\\infty
                    \\space and
                    \\space p\\rightarrow0$$"),
        markdown("while:"),
        withMathJax("$$np = \\lambda$$"),
        markdown("remains fixed. Then for $$k = 0, 1, 2,...$$"),
        withMathJax("$$\\binom{n}{k}p^{k}(1-p)^{n-k}
                    \\rightarrow\\frac{\\lambda^{k}}{k!}e^{-\\lambda}$$"),
        markdown("
          **Reference**
           
           Exploring University Mathematics with Python (2023), Siri Chongchitnan
           (page 393)
           
           DOI: https://doi.org/10.1007/978-3-031-46270-2
        ")
      )
    ),
    card(
      plotOutput("plot")
    ),
    col_widths = c(5, 7)
  )
)

server <- function(input, output, session) {
  proba <- reactive({
    5 / input$n
  })
  
  data <- reactive({
    df <- tibble(
      k = 0:10,
      pois = dpois(x = 0:10, lambda = 5),
      binom = dbinom(x = 0:10, size = input$n, prob = proba())
    ) 
  })
  
  output$plot <- renderPlot({
    data() |> ggplot(aes(x = k)) +
      geom_point(aes(y = pois, color = "Poisson"), size = 3) +
      geom_line(aes(y = pois), color = "black", linetype = "dotted") +
      geom_point(aes(y = binom, color = "Binomial"), size = 4, shape = 5) +
      geom_line(aes(y = binom), color = "red3", linetype = "dashed") +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      ylab("Probability") +
      ylim(0, 0.25) +
      scale_x_continuous(breaks = 0:10) +
      labs(color = "Distribution") +
      scale_color_manual(values = colors) +
      theme_minimal()
  })
}

shinyApp(ui, server)