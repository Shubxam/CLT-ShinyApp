rlnorm
rbeta
# Load packages ----------------------------------------------------------------

library(shiny)
library(tidyverse)
library(gridExtra)

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  # Title ----
  titlePanel("Central Limit Theorem for Means", windowTitle = "CLT for means"),
  
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        # Select distribution ----
        radioButtons("dist", "Parent distribution (population):",
                     c("Normal" = "rnorm",
                          "Uniform" = "runif"),
                     selected = "rnorm"),
        # hr(),
        
        # Distribution parameters / features ----
        uiOutput("mu"),
        uiOutput("sd"),
        uiOutput("minmax"),
        
        # Select sample size ----
        sliderInput("n",
                    "Sample size:", 
                    value = 30,
                    min = 2,
                    max = 500),
        br(),
        
        # Number of samples ----
        sliderInput("k",
                    "Number of samples:",
                    value = 200,
                    min = 10,
                    max = 1000)
      )
      


    ),
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
          # First tab ----
          tabPanel(
            title = "Population Distribution",
            # Population plot ----
            plotOutput("pop.dist", height = "500px"),
            br()
          ),
          # Second tab ----
          tabPanel(
            title = "Samples",
            # Sample plots ----
            br(),
            plotOutput("sample.dist", height = "600px"),
            #  Number of samples text ----
            div(h3(textOutput("num.samples")), align = "center"),
            br()
          )
          # Third tab ----
        )
      )
    )
  )

# Define server function --------------------------------------------

seed <- as.numeric(Sys.time())

server <- function(input, output, session) {
  
  # Mean slider for Normal distribution ----
  output$mu = renderUI(
    {
      if (input$dist == "rnorm")
      {
        sliderInput("mu",
                    "Mean:",
                    value = 0,
                    min = -40,
                    max = 50)
      }
    })
  
  # SD slider for Normal distribution ----
  output$sd = renderUI(
    {
      if (input$dist == "rnorm")
      {
        sliderInput("sd",
                    "Standard deviation:",
                    value = 20,
                    min = 1,
                    max = 30)
      }
    })
  
  # Minmax slider for Uniform distribution ----
  output$minmax = renderUI(
    {

      if (input$dist == "runif")
      {
        sliderInput("minmax",
                    "Lower and Upper Bounds",
                    value = c(5, 15),
                    min = 0,
                    max = 20)
      }
    })
  
  # Making sure range for uniform distribution != 0 ----
  observeEvent(input$minmax, {
    
    req(input$minmax)
    
    if (input$minmax[1] == input$minmax[2]){
      if (input$minmax[1] == 0){
        updateSliderInput(session, "minmax", value = c(0, 1))
      } else if (input$minmax[2] == 20){
        updateSliderInput(session, "minmax", value = c(19, 20))
      } else {
        updateSliderInput(session, "minmax", value = c(input$minmax[2], input$minmax[2] + 1))
      }
    }
  })
  


  # generating random samples ----
  rand_draw <- function(dist, n, mu, sd, min, max){
    
    vals = NULL
    
    
    if (dist == "rnorm"){
      req(mu, sd)
      vals = do.call(dist, list(n=n, mean=mu, sd=sd))
    }

    
    else if (dist == "runif"){
      req(min, max)
      vals = do.call(dist, list(n=n, min=min, max=max))
    }
    return(vals)
  }
  
  rep_rand_draw = repeatable(rand_draw)
  
  # Defining some reactive variables to use later ----
  parent = reactive({
    
    n_sample = 1e5
    
    return(rep_rand_draw(input$dist, n_sample, input$mu, input$sd,
                         input$minmax[1], input$minmax[2]))
  })
  
  samples = reactive({
    
    pop = parent()
    n = input$n
    k = input$k
    
    return(replicate(k, sample(pop, n, replace=TRUE)))
  })
  
  u_min = reactive({
    req(input$minmax)
    return(input$minmax[1])
  })
  
  u_max = reactive({
    req(input$minmax)
    return(input$minmax[2])
  })
  
  # plot 1 a) ----
  output$pop.dist = renderPlot({
    
    distname = switch(input$dist,
                      rnorm = "Population distribution: Normal",
                      runif = "Population distribution: Uniform")
    
    pop = parent()
    
    m_pop =  round(mean(pop), 2)
    sd_pop = round(sd(pop), 2)
    
    pop = tibble(samples = pop)
    pdens = density(pop$samples)

    x_range = max(pop$samples) - min(pop$samples)
    y_pos = max(pdens$y) - 0.2*max(pdens$y)
    
    if (input$dist == "rnorm"){
      
      req(input$mu)
      mu = input$mu
      
      x_pos = ifelse(mu > 0, min(-100, min(pop$samples)) + 20,
                     max(100, max(pop$samples)) - 20)
      
      ggplot(data = pop, aes(x = samples, y = ..density..)) + 
        geom_histogram(bins = 45, color = "white", fill = "#195190") +
        # geom_density() + draws a weird baseline. using stat_density() instead.
        stat_density(geom="line", color = "#195190", size = 1) +
        scale_x_continuous(limits = c(min(-100, pop$samples), max(100, pop$samples))) +
        labs(title = distname, x = "x") +
        annotate("text", x = x_pos, y = y_pos,
                 label = paste("mean of x", "=", bquote(.(m_pop)),
                               "\n", "SD of x", "=", bquote(.(sd_pop))),
                 color = "black", size = 5) +
        theme_light(base_size = 19) + # better than doing title sizes inside theme().
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
    } else if (input$dist == "runif"){
      
      if (u_min() == u_max()){
        "  " # this is to temporarily prevent graph from displaying while 
        # observeEvent is fixing the range.
      } else {
      
        x_pos = max(pop$samples) - 0.1*x_range
  
        ggplot(data = pop, aes(x = samples, y = ..density..)) +
          geom_histogram(bins = 45, color = "white", fill = "#195190") +
          stat_density(geom = "line", color = "#195190", size = 1) +
          scale_y_continuous(expand = expand_scale(mult = c(0, .3))) +
          labs(title = distname, x = "x") +
          annotate("text", x = x_pos, y = y_pos + 0.5*max(pdens$y),
                   label = paste("mean of x", "=", bquote(.(m_pop)),
                                 "\n", "SD of x", "=", bquote(.(sd_pop))),
                   color = "black", size = 5) +
          theme_light(base_size = 10) +
          theme(plot.title = element_text(hjust = 0.5),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())}
    
    } 
  })
  
  # plot 1 b) ----
  # this is the population plot in the third tab. it is plot 1 a) with a few
  # changes to make it fit the smaller pace in the tab. (apparently not possible to use the
  # same output id in different tabs.)
  
  output$pop.dist.two = renderPlot({
    
    distname = switch(input$dist,
                      rnorm = "Population distribution: Normal",
                      runif = "Population distribution: Uniform")
    
    pop = parent()
    
    m_pop =  round(mean(pop),2)
    sd_pop = round(sd(pop),2)
    
    pop = tibble(samples = pop)
    pdens = density(pop$samples)
    
    x_range = max(pop$samples) - min(pop$samples)
    y_pos = max(pdens$y) - 0.2*max(pdens$y)
    
    if (input$dist == "rnorm"){
      
      req(input$mu)
      mu = input$mu
      
      x_pos = ifelse(mu > 0, min(-100, min(pop$samples)) + 27,
                     max(100, max(pop$samples)) - 27)
      
      ggplot(data = pop, aes(x = samples, y = ..density..)) + 
        geom_histogram(bins = 45, color = "white", fill = "#195190") +
        stat_density(geom="line", color = "#195190", size = 1) +
        scale_x_continuous(limits = c(min(-100, pop$samples), max(100, pop$samples))) +
        labs(title = distname, x = "x") +
        annotate("text", x = x_pos, y = y_pos,
                 label = paste("mean of x", "=", bquote(.(m_pop)),
                               "\n", "SD of x", "=", bquote(.(sd_pop))),
                 color = "black", size = 3) +
        theme_light(base_size = 10) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
    } else if (input$dist == "runif"){
      
      if (u_min() == u_max()){
        " "
      } else {
  
        x_pos = max(pop$samples) - 0.1*x_range
        
        ggplot(data = pop, aes(x = samples, y = ..density..)) +
          geom_histogram(bins = 45, color = "white", fill = "#195190") +
          stat_density(geom = "line", color = "#195190", size = 1) +
          scale_y_continuous(expand = expand_scale(mult = c(0, .3))) +
          labs(title = distname, x = "x") +
          annotate("text", x = x_pos, y = y_pos + 0.5*max(pdens$y),
                   label = paste("mean of x", "=", bquote(.(m_pop)),
                                 "\n", "SD of x", "=", bquote(.(sd_pop))),
                   color = "black", size = 3) +
          theme_light(base_size = 10) +
          theme(plot.title = element_text(hjust = 0.5),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())}
      
    }
  })
  
  # plot 2 ----
  output$sample.dist = renderPlot({
    
    y = samples()
    x = samples() %>% as_tibble()
    
    plots = list(rep(NA, 8))

    for(i in 1:8){
      
      mean = round(mean(y[,i]), 2)
      sd = round(sd(y[,i]), 2)
      
      x_range = max(y[,i]) - min(y[,i])
      pdens = density(y[,i])
      
      x_pos = ifelse(input$dist == "rbeta", min(y[,i]) + 0.1*x_range, 
                     max(y[,i]) - 0.1*x_range)
      
      plots[[i]] = ggplot(x, aes_string(x = paste0("V", i))) +
        geom_dotplot(alpha = 0.8, dotsize = 0.7) +
        labs(title = paste("Sample", i), x = "", y = "") +
        theme_light(base_size = 13) +
        annotate("text", x = x_pos, y = 1.8,
                 label = paste("x_bar", "=", bquote(.(mean)),
                               "\n", "SD", "=", bquote(.(sd))),
                 color = "black", size = 3) +
        scale_y_continuous(limits = c(0,2), breaks = NULL) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    }
    
    grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]],
      plots[[6]], plots[[7]], plots[[8]], ncol = 4)
  })
  
  
  
  # text for sample plots ----
  output$num.samples = renderText({

    k = input$k
    paste0("... continuing to Sample ",k,".")

  })

  # plot 3 ----
  

  # description for sampling distribution plot ----


  # description for CLT ----

}
# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)
