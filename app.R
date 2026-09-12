library(shiny)
library(ggplot2)
library(tibble)
library(bslib)
library(tidyr)

ui <- page_sidebar(
  title = "Poisson Limit Theorem",
  theme = bs_theme(
    bootswatch = "minty",
  ),
  sidebar = sidebar(
    width = 350,
    card(
      card_header(
        "Number of Trials"
      ),
      card_body(
        sliderInput("n", "n", min = 10, max = 2000, value = 10, animate = TRUE,
                    step = 1),
      )
    ),
    card(
      full_screen = TRUE,
      card_header(
        "The Theorem"
      ),
      card_body(
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
    )
  ),
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Number of Trials (n)",
      p("Number of independent trials in Binomial Distribution"),
      value = textOutput("trials"),
      theme = "indigo"
    ),
    value_box(
      title = markdown("Probability of Success (p = λ / n)"),
      p("The value of λ is fixed (in our case to 5)"),
      value = textOutput("success"),
      theme = "teal"
    ),
    value_box(
      title = "Total Variation Distance",
      p("Distance between Poisson and Binomial distributions"),
      value = textOutput("distance"),
      theme = "light"
    )
  ),
  card(
    card_header(
      ""
    ),
    card_body(
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  proba <- reactive({
    5 / input$n
  })

  data <- reactive({
    df <- tibble(
      k = rep(0:10, 2),
      prob = c(
        rep(dpois(x = 0:10, lambda = 5), 1),
        rep(dbinom(x = 0:10, size = input$n, prob = proba()), 1)
      ),
      dist = c(rep("Poisson", 11), rep("Binomial", 11))
    )
  })
  
  wide_data <- reactive({
    data() |> 
      pivot_wider(
        names_from = dist,
        values_from = prob
      )
  })

  output$plot <- renderPlot({
    ggplot(data = data()) +
      geom_point(aes(x = k, y = prob, color = dist), size = 4) +
      geom_line(aes(x = k, y = prob, color = dist), linewidth = 2) +
      geom_segment(
        data = wide_data(),
        aes(x = k, y = Poisson, xend = k, yend = Binomial),
        linetype = "dashed",
        colour = "black"
      ) +
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
    
  output$trials <- renderText({
    input$n
  })
  
  output$success <- renderText({
    round(proba(), 5)
  })
  
  total_vd <- reactive({
    P <- wide_data()$Poisson
    B <- wide_data()$Binomial
    
    0.5 * sum(abs(P - B)) |> 
      round(5)
  })
  
  output$distance <- renderText({
    total_vd()
  })
}

shinyApp(ui, server)