library(shiny)
library(ggplot2)
library(tibble)
library(bslib)

ui <- page_fillable(
  h4("Poisson Limit Theorem"),
  theme = bs_theme(
    version = 5, 
    base_font = font_google("Roboto"),
    bootswatch = "litera",
    heading_font = font_google("Montserrat")
  ),
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
          *Reference:*
           
           Exploring University Mathematics with Python, Siri Chongchitnan,
           Springer, 2023, p. 393
           
           DOI: https://doi.org/10.1007/978-3-031-46270-2
        ")
      )
    ),
    card(
      plotOutput("plot")
    ),
    col_widths = c(3, 9)
  )
)

server <- function(input, output, session) {
  proba <- reactive({
    5 / input$n
  })
  
  data <- reactive({
    df <- tibble(
      k = rep(0:10, 12),
      prob = c(
        rep(dpois(x = 0:10, lambda = 5), 6),
        rep(dbinom(x = 0:10, size = input$n, prob = proba()), 6)
      ),
      dist = c(rep("Poisson", 66), rep("Binomial", 66))
    ) 
  })
  
  output$plot <- renderPlot({
    data() |> ggplot(aes(x = k, y = prob, color = dist)) +
      geom_point(size = 4) +
      geom_line(linewidth = 2) +
      ylab("PROBABILITY") +
      ylim(0, 0.25) +
      scale_color_brewer(palette = "Set2") +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      labs(color = "DISTRIBUTION") +
      scale_x_continuous(breaks = 0:10) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        axis.title.x = element_text(size = 20, ),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 19)
      )
  })
}

shinyApp(ui, server)