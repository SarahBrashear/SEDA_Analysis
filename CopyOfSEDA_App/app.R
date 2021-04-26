#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(shinythemes)

data <- read_csv("raw_data/SEDA19.csv",
                 col_types = cols(
                     leaid = col_double(),
                     leanm = col_character(),
                     stateabb = col_character(),
                     mn_asmts = col_double(),
                     meanavg = col_double(),
                     sesavgall = col_double(),
                     fips = col_character(),
                     tot_asmts = col_double(),
                     cellcount = col_double(),
                     mn_grd_eb = col_double(),
                     mn_coh_eb = col_double(),
                     mn_mth_eb = col_double()
                 )
)

###############################################################################
###############################################################################

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("sandstone"),
    
    navbarPage("Student Achievement and SES in U.S. Public Schools",
               tabPanel("Data",
                        h1("How are student achievement and SES correlated in each state?"),
                        p("Based on the Stanford Education Data Archive (SEDA)", 
                          style = "font-size:20px;"),
                        br(),
                        
                        # Main Panel
                        mainPanel(
                            selectInput(inputId = "selected_state",
                                        label = "Choose a state",
                                        choices = data$stateabb,
                                        selected = "AL"),
                            
                            plotOutput("state_plot"),
                            br(),
                            p("Select a state from the dropdown list above in order
                              to see the relationship between student achievement
                              and socioeconomic status for each school district
                              in that state. Each bubble represents one public
                              school district, and the size of the district represents
                              the number of students enrolled in the district. Along
                              the X axis, you can see a school district's average
                              socioeconomic status, with zero representing average
                              SES. Along the Y axis, you see student achievement, 
                              measured by the average test scores of
                              5th graders in that district.")),
                        
                       
                        h2("About the Data"),
                        p("The Educational Opportunity Project at Stanford University
                          collects and publishes ", 
                          a(href="https://edopportunity.org/", 
                            "The Stanford Education Data Archive (SEDA),"), 
                          "which is the first national database of academic
                          performance in all public school districts across the
                          United States. Some of the covariates used in this analysis
                          come from publically accessible files from the ",
                          a(href="https://nces.ed.gov/",
                            "Center for Education Statistics (NCES)"),
                          ". See below for more information on the main predictor
                          and outcome variables used in this analysis."
                          ),
                        
                        
                        sidebarPanel(
                            h3(""),
                            h4("Average Student Achievement"),
                            p("The data for this variable comes from the standardized
                              accountability tests in math and Reading Language
                              Arts that all public-school students in grades 3-8
                              took from the 2008-09 school year through the 2017-18
                              school year. In total, this comes out to roughly 
                              450 million test scores. SEDA researchers use these
                              scores along with information from the",
                              a(href="https://nces.ed.gov/nationsreportcard/",
                                "National Assessment of Educational Progress (NAEP)"),
                              "in order to calculate each school district's average 
                              student achievement. Since this variable measures difference
                              in",
                              strong("average test scores,"),
                              "it can be interpreted as
                              the educational opportunities available
                              in a given school district." 
                              ),
                            h4("Average Family Socioeconomic Status"),
                            p("For each school district, SEDA researchers have 
                              estimated average socioeconomic status (SES) 
                              using data from the Census Bureau's ",
                              a(href="https://www.census.gov/programs-surveys/acs",
                                "American Community Survey (ACS)"),
                              ". The variable is a composite
                              measure which includes median income, educational 
                              attainment, poverty rate, SNAP benefits, single 
                              mother households, and employment rate. The measure
                              is also standardized, which means that the value of 
                              zero represents the SES of the average school district
                              in the U.S., and each one unit difference in SES 
                              corresponds to one standard deviation difference.")
                            
                            )
                        
                        
               ),
               
               # Panel 2
               tabPanel("Models",
                        h3("How would we expect students in the \"typical\" school district to perform on future assessments?"),
                        p("Public school districts vary dramatically in size, 
                          location, demographics, and available funding. All of
                          these factors shape the learning outcomes of the students
                          who attend them. While no two school districts are the 
                          same, the SEDA data allows us to explore with great
                          depth what the learning experiences at various school
                          districts are like for students. Now more than ever,
                          as districts across the country prepare for a post-pandemic
                          educational landscape, it is imperative that leaders and
                          policy-makers understand how educational outcomes vary
                          across the country. "), 
                        br(),
                        p("I designed a predictive model to explore the ", 
                          strong("predicted student achievement"),
                          "for each school district in the nation, based on their
                          average socio-economic status. I decided to control for
                          the percentage of students who are English Language 
                          Learners and the percentage of students who receive 
                          special education services, based on extensive literature
                          that shows that traditional assessments do not accurately
                          measure achievement outcomes for these groups of students.
                          I also tested a few other models-one controlling for 
                          racial differences between school districts, and one
                          controlling for various component parts of the SES index.
                          Using the Loo Compare analysis, I chose the strongest 
                          model, and it is illustrated below."),
                          

                        br(),
                        p(strong("Regression Model Equation:")),
                        withMathJax('$$ meanavg_i = \\beta_0 + \\beta_1sesavgall_i + 
                        \\beta_2sesavgall_i*perell_i + \\beta_3sesavgall_i*perspeced_i +
                           \\varepsilon_i $$'),
                        
                        h4("The Model, by State:"),
                        img(src = "ca_table.png", height = "40%", width = "40%",
                            style = "display: block; margin-left: auto; margin-right: auto;"),
                        img(src = "tx_table.png", height = "42%", width = "42%",
                            style = "display: block; margin-left: auto; margin-right: auto;"),
                        img(src = "ma_table.png", height = "35%", width = "35%",
                            style = "display: block; margin-left: auto; margin-right: auto;"),
                        
                        br(),
                        h4("Visualizing the Differences Between States"), 
                        img(src = "low_ses_image.png", height = "80%", width = "80%",
                            style = "display: block; margin-left: auto; margin-right: auto;"),
                        br(),
                        img(src = "mean_ses_image.png", height = "80%", width = "80%",
                            style = "display: block; margin-left: auto; margin-right: auto;"),
                        br(),
                        img(src = "high_ses_image.png", height = "80%", width = "80%",
                            style = "display: block; margin-left: auto; margin-right: auto;"),
                        br(),
                        
                        ## data table output
                        p(em("Source: SEDA", 
                             style = "font-size:10px;")),
                        p("* small comment at the bottom.", 
                          style = "font-size:10px;")
                        
               ),         
               
               
               # Panel 3
               
               tabPanel("About",
                        mainPanel(
                            h3("About the Project"),
                            h4("Summary"),
                            p("This project focuses on...", 
                              strong("bold text,"), 
                              "more talking about all the things here. Talk talk."),
                            h4("Motivation"),
                            p("The idea for this project stems from my interest in the", 
                              strong("bold text here."), 
                              "More text will go here"),
                            h4("Data"),
                            p("I'll talk about my data here. The 
                 original datasets can be found in the Raw_data folder of my", 
                              a(href="https://github.com/SarahBrashear",
                                "GitHub repository."), 
                              "In the repo, you can also find my Shiny app and data wrangling code.")),
                        br(),
                        br(),
                        br(),
                        sidebarPanel(
                                     h3("About Me"),
                                     h4("Sarah Brashear, Ed.M. Candidate at Harvard GSE"),
                                     p("This is where I'll talk about myself."))
    )))


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$state_plot <- renderPlot({
        
        data %>%
            
            filter(stateabb == input$selected_state) %>%
            
            drop_na(sesavgall, meanavg) %>%
            ggplot(aes(x = sesavgall, 
                       y = meanavg,
                       size = tot_asmts,
                       # color = ifelse(stateabb == "CA", "blue", "black")
            )) +
            geom_point(alpha = .4,
                       color = 'royalblue4') +
            geom_hline(yintercept = 5, linetype = "dashed") +
            geom_smooth(method = "lm", formula = y ~ poly(x, 3), color = "grey15", se = FALSE) +
            labs(title = "Correlation Between SES and Academic Acheivement",
                 subtitle = "Each Bubble is a Public School District",
                 x = "Average Family Socioeconomic Status",
                 y = "Average Student Achievement of 5th Graders",
                 caption = "Source: Stanford Educational Data Archive (SEDA)") +
            theme_classic() +
            theme(legend.position = "none") +
            scale_size_continuous(range = c(1, 10))
        

        
    })
    
    output$low_ses_image <- renderImage({ 
        
        readRDS("SEDA_App/low_ses_plot.rds")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
