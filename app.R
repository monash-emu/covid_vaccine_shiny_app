#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

user <- unname(Sys.info()["user"])
#if (user == "shiny") {
  
  # Set library locations
#  .libPaths(c(C:/Program Files/R/R-4.1.2/library")
#  )
#}


library(shiny)
library(shinyWidgets)

library(tidyverse)
library(stringi)
source('./master_utils.R')


library(markdown)

HR_STYLE = "border-top: 2px solid #000000;"

countrypops <- list.dirs('./data/prem_2020',full.names=FALSE, recursive=FALSE)

vaccDF <- readxl::read_xlsx("./data/strain_specific_vaccine_efficacy.xlsx", sheet = 1) 
vacc_types = stri_enc_toascii(vaccDF$vaccine)

#ages <- age_groups
ages <- read_csv('./data/ages.csv')

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Prediction of COVID-19 epidemic size under various scenarios of pre-existing immunity"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel( style = "background-color: #FEF4E8;", width = 4,

      h2("Epidemiological Situation"),
      
      selectInput("strain",
                  "Strain:",
                  c('Delta','wild-type')),
      
      fluidRow(
        column(
          11,
          sliderInput('R0',
                      "Basic Reproduction number (R0):",
                      0,10,4.5,step=0.1)
        ),
        column(
          1,
          make_info_button("r_zero")
        )
      ),
      
      fluidRow(
        column(
          11,
          sliderInput('mobility',
                      "Inter-personnal interactions (relative to pre-COVID era):",
                      0,100,80,step=5,post  = " %")
        ),
        column(
          1,
          make_info_button("mobility")
        )
      ),
      
      
      fluidRow(
        column(
          11,
          sliderInput('microdistancing',
                      "Relative transmission risk per contact (reduced by individual preventive measures such as masks or hand washing). 100% = no preventive measure",
                      0,100,80,step=5,post  = " %")
        ),
        column(
          1,
          make_info_button("microdistancing")
        )
      ),
      
      fluidRow(
        column(
          11,
          h3("Pre-existing infection-induced immunity")
        ),
        column(
          1,
          make_info_button("immunity")
        )
      ),
      
      sliderInput('seroprevalence_0_14',
                  "0-14 years old:",
                  0,100,0,post  = " %"),
      sliderInput('seroprevalence_15_59',
                  "15-59 years old:",
                  0,100,0,post  = " %"),
      sliderInput('seroprevalence_60_over',
                  "60 years old and over:",
                  0,100,0,post  = " %"),
      
      fluidRow(
        column(
          11,
          h3("Modelled effective reproduction number:")
          
        ),
        column(
          1,
          make_info_button("r_eff")
        )
      ),
      span(textOutput("r_eff"), style="font-weight: bold; font-size: large;"),
      
      ##############################################################
      hr(style = HR_STYLE),
      
      fluidRow(
        column(
          11,
          h2("Vaccination Program Characteristics")
          
        ),
        column(
          1,
          make_info_button("vacc_program")
        )
      ),
      
      #Dynamic number of vaccine inputs
      sliderInput(
        "n_vaccines",
        "N vaccine types",
        min = 1, max=length(vacc_types),value=1,step=1
      ),
      
      fluidRow(
        column(6,
               mainPanel(uiOutput("vacc_type_ui"))      
        ),
        column(6,
               mainPanel(uiOutput("vacc_perc_ui"))      
        ),
      ),
      
      span(textOutput("sum_vacc_percs"), style="font-size: medium; color: red;"),
      
      # Old code
      #selectInput("vacc_1",
      #            "Vaccine used:",
      #            vacc_types,
      #            vacc_types[1]),
      #selectInput("vacc_2",
      #            "Vaccine 2 (ages >cutoff):",
      #            stri_enc_toascii(vaccDF$vaccine),
      #            stri_enc_toascii(vaccDF$vaccine)[2]),
      # selectInput("cutoff_between",
      #            "Age cutoff between vaccines",
      #            c(30,40,50,60),60),
      selectInput('eligible_age',
                  'Minimum age for vaccine eligibility?',
                  c(5,10,15),15),
      # sliderInput('uptake',
      #             'Vaccine uptake (proportion):',
      #             value = 0.9, step=0.05,min=0,max=1),
      
      ##########################################################
      hr(style = HR_STYLE),
      fluidRow(
        column(
          11,
          h2("Coverage Scenarios")
          
        ),
        column(
          1,
          make_info_button("coverage")
        )
      ),
      
      sliderInput('coverage_1',
                  'Coverage Scenario 1',
                  value = 0, step=5,min=0,max=100, post=" %"),
      sliderInput('coverage_2',
                'Coverage Scenario 2:',
                 value = 60, step=5,min=0,max=100, post=" %")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      navbarPage("",
                 tabPanel('Overall results',
                          plotOutput("overallPlot", width = 900, height = 700)
                 ),
                 tabPanel("Results by age group",
                          plotOutput("agePlot", width = 900, height = 700)
                          #tableOutput("ptext")
                 ),
                 
                 tabPanel("Epidemic size against vaccine coverage",
                          plotOutput('coveragePlot', width = 800, height = 700)),
                 
                 tabPanel("Country information",
                          plotOutput('countryDemoPlot', width = 600, height = 500),
                          plotOutput('countryContactPlot')),
                 
                 tabPanel("Explanatory notes",
                          includeMarkdown("notes.Rmd")
                 ),
                 
                 tabPanel("Reference",
                          includeMarkdown("info.Rmd")
                 )
      )
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #FIXME dynamic number of vaccine inputs

  vacc_type_selectors <- reactive({
    n <- input$n_vaccines
    
    lapply(seq_len(n), function(i) {
        selectInput(
          paste0("vaccine", i),
          paste0("Vaccine ", i),
          vacc_types,
          selected = vacc_types[i]
        )
      })
  
  })
  
  vacc_perc_selectors<- reactive({
    n <- input$n_vaccines
    
    lapply(seq_len(n), function(i) {
      numericInput(
        paste0("vacc_perc ", i),
        paste0("% Vaccine ", i),
        value=get_default_vacc_perc(n,i),
        min=0,
        max=100,
        step=1
      )
    })
    
  })
  
  sum_of_vacc_percs <- reactive({
    sum = 0
    for (i in 1:input$n_vaccines){
      input_name = paste0("vacc_perc ", i)
      sum = sum + input[[input_name]]
    }
    return(sum)
  })
  
  output$sum_vacc_percs <- renderText({
    s = sum_of_vacc_percs()
    display = ""
    if (length(s) == 0 || s != 100){
      display = paste0("Sum of vaccine percentages should be 100. Current sum: ", s, "%")
    }  
    return(display)
    }
  )

  vacc_program <- reactive({
    vacc_types = c()
    vacc_percs = c()
    for (i in 1:input$n_vaccines){
      type_input_name = paste0("vaccine", i)
      vacc_types = c(vacc_types, input[[type_input_name]])
      
      perc_input_name = paste0("vacc_perc ", i)
      vacc_percs = c(vacc_percs, input[[perc_input_name]])
      
    }
    
    return(list("vacc_types"=vacc_types, "vacc_percs"=vacc_percs))
  })
  
  output$vacc_type_ui <- renderUI({ vacc_type_selectors() })
  output$vacc_perc_ui <- renderUI({ vacc_perc_selectors() })
  
  
  c1 <- reactive(get_params(country = "Philippines",
                            vacc_program=vacc_program(),
                            strain=input$strain,
                            R0 = input$R0 * (input$mobility /100.) * (input$microdistancing /100.),
                            seropositivity = c(
                              rep(input$seroprevalence_0_14/100.0,3),
                              rep(input$seroprevalence_15_59/100.0, 9),
                              rep(input$seroprevalence_60_over/100.0, 4)
                            )
                              )
                 )
  
  p1 <- reactive(
    calc_targets_ages(c1(),
                      target_coverages = c(input$coverage_1/100., input$coverage_2/100., input$coverage_2/100.),
                      uptake=1.0, #input$uptake,
                      cutoff = 60, # strtoi(input$cutoff_between),
                      eligibility_cutoff = input$eligible_age,
                      vacc_program = vacc_program()
    )
   )
  p2 <- reactive(calc_targets(c1(),
                              target_coverages = seq(0,1,by=0.05),
                              uptake=1.0, #input$uptake,
                              cutoff = 60., # strtoi(input$cutoff_between),
                              eligibility_cutoff = input$eligible_age,
                              vacc_program = vacc_program()
  ))
  
  output$r_eff <- renderText({
    round(calc_Reff(
      c1()
    ), 1)
  })
  
  output$ptext <- renderTable({print(p1())})
  output$agePlot <- renderPlot({
    p1() %>% pivot_longer(cols = 3:5,
                          names_to = 'event',
                          values_to = 'value') %>%
      group_by(age_ind,interval,coverage,strategy,event,vaccine) %>%
      summarise(all_val = sum(value),
                all_pop = sum(pop_size)) %>%
      ungroup() %>%
      group_by(interval,coverage,strategy,event,vaccine) %>%
      mutate(rate = all_val*1e5 / sum(all_pop)) %>%
      filter(interval == 'mean') %>%
      # filter(!event == 'hospitalizations') %>%
      left_join(ages) %>%
      ggplot( aes(x=forcats::fct_reorder(age,age_ind, min),y = rate, col = strategy,group=strategy))+
      geom_point()+
      geom_line()+
      theme_bw(base_size = 18)+
      labs(y = 'rate per 100,000 population',
           x = 'Age group')+
      #facet_grid(vaccine+event~coverage,scales='free_y')+
      facet_grid(event~coverage,scales='free_y', labeller=facet_labeller)+
      
      theme(axis.text.x = element_text(angle = -90),
            legend.position = 'top')+
      expand_limits(y = c(0,100))
    # generate bins based on input$bins from ui.R
    # p1() %>% 
    #     pivot_longer(cols = 5:6,
    #                  names_to = 'measure',
    #                  values_to = 'value') %>%
    #     filter(coverage <= 1.0) %>%
    #     ggplot(aes(x=forcats::fct_reorder(age,age_ind, min), y=value, col = strategy,group = strategy))+
    #     geom_line(aes(lty = strategy))+
    #     geom_point()+
    #     ylim(c(0,NA)) +
    #     theme_bw(base_size = 18)+
    #     facet_grid(coverage~measure,
    #                                         labeller = labeller(measure=label_value,coverage = label_both))+
    #     theme(axis.text.x = element_text(angle = -90),
    #           legend.position = 'top')+
    #     labs(y = 'rate per 100,000 population',
    #          x = 'Age group')
  })
  output$overallPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    p1() %>% pivot_longer(cols = 3:5,
                          names_to = 'event',
                          values_to = 'value') %>%
      group_by(age_ind,interval,coverage,strategy,event,vaccine) %>%
      summarise(all_val = sum(value),
                all_pop = sum(pop_size)) %>%
      ungroup() %>%
      group_by(interval,coverage,strategy,event,vaccine) %>%
      mutate(rate = all_val*1e5 / sum(all_pop)) %>%
      filter(interval == 'mean') %>%
      # filter(!event == 'hospitalizations') %>%
      left_join(ages) %>%
      ggplot( aes(x=strategy,y = rate, fill = forcats::fct_reorder(age_group_p,age_group_ind2, min)))+
      geom_col(col = 'black',position = position_stack(reverse = TRUE))+
      coord_flip()+
      # facet_grid(vaccine+coverage~event,scales='free_x')+
      facet_grid(coverage~event,scales='free_x',  labeller=facet_labeller)+
      
      ylim(c(0,NA)) +
      theme_bw(base_size = 18)+
      labs(y = 'rate per 100,000 population',
           x = 'Vaccination strategy',
           fill = 'Age group ')+
      theme(axis.text.x = element_text(angle=-90, vjust = 0.5))+
      expand_limits(y = c(0,100))
    
  })
  
  output$countryContactPlot <- renderPlot({
    reshape2::melt(c1()$contact_matrix) %>%
      ggplot(aes(x=Var1, y=Var2, fill = value))+
      geom_raster()+ labs(x='Age group',y='Age group',
                          fill = 'Mean contacts\n per person\n per day')+
      theme_bw(base_size = 18)+
      theme(axis.text.x = element_text(angle = -90, vjust = 0.5))+
      ggtitle('Country contact matrix')+
      scale_fill_viridis_c()
  })
  output$countryDemoPlot <- renderPlot({
    c1()$population_size %>% as_tibble_row() %>%pivot_longer(cols=1:16,names_to = 'ageGroup',values_to = 'population') %>%
      mutate(age_ind = 1:16)%>%
      ggplot(aes(x=forcats::fct_reorder(ageGroup,age_ind,min), y=population))+
      geom_col(fill = 'darkblue')+
      coord_flip()+ labs(x='Age group',y='Population')+
      theme_bw(base_size = 18)+
      ggtitle('Country population age structure')
  })
  output$countryTable <- renderTable({
    c1()$population_size
  })
  
  output$coveragePlot <- renderPlot({
    p2() %>% plot_targets(l1 = input$coverage_1/100.,
                          l2 = input$coverage_2/100.)
    # generate bins based on input$bins from ui.R
    # p2() %>% 
    #   pivot_longer(cols = 5:6,
    #                names_to = 'measure',
    #                values_to = 'value') %>%
    #   filter(coverage <= 1.0) %>%
    #   group_by(coverage, strategy, measure) %>%
    #   summarise(tot_val = sum(value)) %>%
    #   ggplot(aes(x=coverage, y=tot_val))+
    #   geom_line()+
    #   geom_point()+
    #   #ylim(c(0,NA)) +
    #   theme_bw(base_size = 18)+
    #   facet_grid(strategy~measure,
    #              labeller = labeller(measure=label_value,strategy = label_both))+
    #   theme(axis.text.x = element_text(angle = -90),
    #         legend.position = 'top')+
    #   labs(y = 'rate per 100,000 population',
    #        x = 'Target coverage')+
    #   geom_vline(xintercept=input$coverage_1,col='red')+
    #   geom_vline(xintercept=input$coverage_2,col='red')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
