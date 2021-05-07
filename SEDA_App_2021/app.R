
# One of my goals was to keep this app as efficient and streamlined as possible
# so that it would run quickly, so I did as much work as possible in a separate
# Rmd file. Because of this, I don't need to library a bunch of packages here.
# But, here are some of the packages that I did play with during the project / 
# analysis, but do not have to library within the app itself.

# library(rstanarm)
# library(tidycensus)
# library(ggthemes)
# library(gt)
# library(gtsummary)
# library(patchwork)
# library(ggdist)

library(tidyverse)
library(shiny)
library(shinythemes)

# I also only have to read part of my data directly into the app. I had covariates
# in a separate data table, but I don't need to call them in the actual app
# because the interactive part of my app does not use them. They only show up
# in the static images that I created in the separate Rmd file. This improves
# the speed / efficiency of the app since the covariates file is pretty large.

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

# Because this core portion of the SEDA data was already quite clean, I am 
# loading it directly from the raw_data folder. Also, setting the col_types
# avoids an error message.

###############################################################################
###############################################################################


ui <- fluidPage(theme = shinytheme("sandstone"),
                
                # I found the Sandstone theme from a link Beau sent me during
                # recitations. I was also inspired by a previous semester's 
                # project that Jessica shared on Slack that used this theme
                # with cool side panels.
                
                navbarPage("Student Achievement and SES in U.S. Public Schools",
                           
                           # Panel 1
                           tabPanel("Data",
                                    h1("How are student achievement and SES 
                                       correlated in each state?"),
                                    p("Based on the Stanford Education Data 
                                      Archive (SEDA)", 
                                      style = "font-size:20px;"),
                                    
                                    # I wish the titles didn't run off the edge
                                    # but in previous PSets, teaching team has
                                    # said that's okay because indendenting 
                                    # causes an unwanted line break.
                                    
                                    br(),
                                    
                                    # Main Panel
                                    # This is where the interactive portion of 
                                    # my app is.
                                    
                                    mainPanel(
                                        selectInput(inputId = "selected_state",
                                                    label = "Choose a state",
                                                    choices = data$stateabb,
                                                    selected = data$stateabb == "AL"),
                                        
                                        # I got hung up on the selected argument
                                        # for awhile. At first I just set it equal
                                        # to "AL", but it still came out blank in
                                        # the app. finally found a similar
                                        # problem on a discussion board, and realized
                                        # I neeed to specifiy where the "AL" was
                                        # coming from, with data$stateabb.
                                        
                                        
                                        
                                        plotOutput("state_plot"),
                                        br(),
                                        
                                        p("Select a state from the dropdown list
                                          above in order to see the relationship 
                                          between student achievement and 
                                          socioeconomic status for each school 
                                          district in that state. Each bubble 
                                          represents one public school district, 
                                          and the size of the district represents
                                          the number of students enrolled in the 
                                          district. Along the X axis, you can 
                                          see a school district's average 
                                          socioeconomic status, with zero 
                                          representing the national average SES. 
                                          Along the Y axis, you see student 
                                          achievement, measured by the average 
                                          test scores of 5th graders in that 
                                          district.")),
                                    
                                    
                                    h2("About the Data"),
                                    p("The Educational Opportunity Project at 
                                      Stanford University collects and publishes ", 
                                      a(href="https://edopportunity.org/", 
                                        "The Stanford Education Data Archive (SEDA),"), 
                                      "which is the first national database of 
                                      academic performance in all public school 
                                      districts across the United States. Some 
                                      of the covariates used in this analysis
                                      come from publically accessible files from 
                                      the ",
                                      a(href="https://nces.ed.gov/",
                                        "Center for Education Statistics (NCES)"),
                                      ". See below for more information on the 
                                      main predictor and outcome variables used 
                                      in this analysis."),
                                    
                                    
                                    # In the Sandstone theme, this creates a 
                                    # cool, accented side bar on the right side
                                    # that's a tan color.
                                    
                                    sidebarPanel(
                                        h4("Average Student Achievement"),
                                        p("The data for this variable comes from 
                                          the standardized accountability tests 
                                          in math and Reading Language Arts that 
                                          all public-school students in grades 3-8
                                          took from the 2008-09 school year through 
                                          the 2017-18 school year. In total, this 
                                          comes out to roughly 450 million test 
                                          scores. SEDA researchers use these
                                          scores along with information from the",
                                          a(href="https://nces.ed.gov/nationsreportcard/",
                                            "National Assessment of Educational 
                                            Progress (NAEP)"),
                                          "in order to calculate each school 
                                          district's average student achievement. 
                                          Since this variable measures difference
                                          in",
                                          strong("average test scores,"),
                                          "it can be interpreted as the educational 
                                          opportunities available in a given school 
                                          district."),
                                        h4("Average Family Socioeconomic Status"),
                                        p("For each school district, SEDA researchers 
                                          have estimated average socioeconomic status 
                                          (SES) using data from the Census Bureau's ",
                                          a(href="https://www.census.gov/programs-surveys/acs",
                                            "American Community Survey (ACS)"),
                                          ". The variable is a composite measure 
                                          which includes median income, educational 
                                          attainment, poverty rate, SNAP benefits, 
                                          single mother households, and employment 
                                          rate. The measure is also standardized, 
                                          which means that the value of zero represents 
                                          the SES of the average school district
                                          in the U.S., and each one unit difference 
                                          in SES corresponds to one standard 
                                          deviation difference.")
                                        
                                    )
                                    
                                    
                           ),
                           
                           # Panel 2
                           tabPanel("Model",
                                    h2("How would we expect students in the 
                                       \"typical\" school district to perform on 
                                       future assessments?"),
                                    p("Modeling Predicted Student Achievement", 
                                      style = "font-size:20px;"),
                                    p("Public school districts vary dramatically 
                                      in size, location, student body demographics, 
                                      and available funding. All of these factors 
                                      shape the learning outcomes of the students 
                                      who attend them. While no two school districts 
                                      are exactly the same, the SEDA data allows 
                                      us to compare student outcomes across time 
                                      and place in order to better understand and 
                                      explain the variance in student achievement.
                                      Now more than ever,as districts across the 
                                      country prepare for a post-pandemic educational 
                                      landscape, it is imperative that education 
                                      leaders and policy-makers understand how 
                                      educational outcomes vary across the country."), 
                                    br(),
                                    p("I designed a predictive model to explore 
                                      the ", 
                                      strong("predicted student achievement"),
                                      "for the average school district, based 
                                      on socio-economic status. With a lens towards 
                                      equity, I wanted to explore whether \"demography 
                                      is destiny\" for students in U.S. public schools, 
                                      or if students who are born into low-SES communities 
                                      are provided educational opportunities that 
                                      allow them to achieve as highly as their more 
                                      affluent peers. I decided to control for
                                      the percentage of students who are English 
                                      Language Learners and the percentage of students 
                                      who receive special education services based 
                                      on extensive literature that shows that 
                                      traditional assessments do not accurately
                                      measure achievement outcomes for these groups 
                                      of students. I also tested a few other 
                                      models-one controlling for racial differences 
                                      between school districts, and one controlling 
                                      for various component parts of the SES index,
                                      such as the percentage of students receiving 
                                      free or reduced-price lunch (a proxy for 
                                      parental income). Using the Loo Compare analysis, 
                                      I found that the racial demographic variables 
                                      were not statistically significant. Further, 
                                      the model that accounted for state, district's 
                                      percentage of English Language Learners, and 
                                      students receiving special education services 
                                      was the strongest. It is illustrated below."),
                                    
                                    
                                    br(),
                                    p(strong("Regression Model Equation:")),
                                    
                                    # the withMathJax function is the easiest way
                                    # to write equations in a shiny app. I learned
                                    # about this from peers on Demo Day.
                                    
                                    withMathJax('$$ meanavg_i = \\beta_0 + 
                                        \\beta_1sesavgall_i + 
                                        \\beta_2sesavgall_i*state_i + 
                                        \\beta_3perell_i +
                                        \\beta_4perspeced_i +
                                        \\varepsilon_i $$'),
                                    
                                    br(),
                                    br(),
                                    
                                    p("Interpretation of Findings:", 
                                      style = "font-size:20px;"),
                                    br(),
                                    
                                    # Loading the full data into the
                                    # app and running the regression, and then
                                    # building the plot in the app would have 
                                    # slowed it down significantly. Also, lots
                                    # of classmates tried to use plotly and had
                                    # issues with it. I opted to load in all my 
                                    # posterior plots as static images, following
                                    # the code from Beau's app as an example. 
                                    
                                    img(src = "posterior.png", 
                                        height = "80%", 
                                        width = "80%",
                                        style = "display: block; margin-left: 
                                                 auto; margin-right: auto;"),
                                    
                                    # Also learned from a peer's code comments
                                    # that images have to be housed in a folder
                                    # called "www" in order to be read in. 
                                    # Upon further Googling, I learned that 
                                    # the www directory also has to be at the
                                    # same level as the UI and Server. So, in
                                    # the app's main folder. 
                                    
                                    br(),
                                    
                                    p("Based on this model,", 
                                      strong("we would expect 5th 
                                      grade students in an average school district 
                                      in the United States to perform slightly 
                                      above grade level"),
                                      "if they have national average
                                      socio-economic status, and just 10% of students 
                                      receive Special Education services, and 10% 
                                      are English Language Learners, regardless 
                                      of the state they are in. The plot above 
                                      shows the range of potential outcomes for 
                                      student achievement. Since each unit represents
                                      one grade level (5 indicating mastery of 
                                      knowledge and skills on par with a 5th grader, 
                                      6 indicating 6th grade, and so on), this 
                                      model estimates that 5th graders are very 
                                      likely to perform between 5.37 and 5.47. While 
                                      it is reassuring to see that America's middle
                                      class 5th graders are performing slightly 
                                      above 5th grade-level, on average, this finding 
                                      alone was not particularly insightful. The 
                                      more I dug into the data, though, the more 
                                      I realized that there are wide discrepencies 
                                      between the predicted achievement outcomes 
                                      between different states. Click on the 'Differences 
                                      Between States' tab to learn more.") 
                                    
                                    # My biggest error when editing this app is
                                    # having commas and parentheses slightly off
                                    
                           ),         
                           
                           
                           # Panel 3
                           tabPanel("Differences Between States",
                                    h1("How much does predicted achievement vary 
                                       between states?"),
                                    p("Modeling Predicted Student Achievement in 
                                      Three Prototypical States", 
                                      style = "font-size:20px;"),
                                    br(),
                                    
                                    mainPanel(
                                        p("The American education system runs on 
                                          a long-standing tradition of local 
                                          decision-making. While there are federal 
                                          laws that impact the student experience, 
                                          the vast majority of curriculum, policy, 
                                          and funding decisions are left up to the 
                                          states. Despite recent efforts to align 
                                          student learning objectives (such as the 
                                          Common Core curriculum, for example), 
                                          educational opportunity still varies 
                                          significantly from state to state. Because 
                                          of this, the previous model, which
                                          makes predictions based on nation-wide 
                                          data, is not necessarily the most informative 
                                          for education leaders or policy-makers 
                                          who are interested in improving outcomes 
                                          for students at the state-level. So, I 
                                          extended my analysis to explore how much 
                                          variance there is between states in terms 
                                          of the relationship between socio-economic 
                                          status and student achievement."),
                                        
                                        br(),
                                        
                                        p("In order to do this, I focused on 
                                          three of the most influential
                                          states in education policy: Texas, 
                                          California, and Massachusetts. See the 
                                          posterior below to compare
                                          the expected student achievement for 
                                          students in average SES districts, in 
                                          each of the three states, when percentages
                                          of English Language Learners and Special 
                                          Education students are held constant at
                                          10%."),
                                        
                                        br(),
                                        
                                        # Again, loading in the images below
                                        # from the www folder.
                                        
                                        h4("Visualizing the Differences Between 
                                           States"), 
                                        br(),
                                        
                                        
                                        # The Average SES, Comparative Posterior
                                        img(src = "states_posterior.png", 
                                            height = "100%", 
                                            width = "100%",
                                            style = "display: block; margin-left: 
                                            auto; margin-right: auto;"),
                                        

                                        p("As you can see, predicted achievement
                                          can vary considerably between states.
                                          Among the \"Big Three\" in education,
                                          it is likely that there is about", 
                                          strong("half of a grade level of variation 
                                          between predicted outcomes for students"), 
                                          "in Texas compared to students in Massachusetts.")),
                                
                           ),
                           
                           
                           
                           # Panel 4
                           
                           tabPanel("About",
                                    mainPanel(
                                        h3("About the Project"),
                                        h4("Summary"),
                                        p("This project explores the relationship 
                                          between socio-economic status and student 
                                          achievement for each of the roughly 
                                          13,000 school districts in the country. 
                                          I grouped districts by their states 
                                          since the majority of education policy 
                                          decision-making happens at the state 
                                          level. In each state, I found a positive 
                                          correlation between SES and student 
                                          achievement; however, the magnitude of 
                                          correlations vary. I then built a 
                                          predictive model that estimates that
                                          students from a typical school 
                                          district will perform slightly above 
                                          grade-level on future assessments under 
                                          a certain set of assumptions. Because 
                                          I was interested in the variance between 
                                          states, I also analyzed the posterior
                                          distributions for three of the most 
                                          influential states. Ultimately the variance 
                                          between these states could be used to 
                                          inform policy decisions in the future 
                                          as state education agencies strategically 
                                          allocate funding and support for post-pandemic 
                                          recovery."), 

                                        br(),
                                        
                                        h4("Limitations of This Analysis"),
                                        p("The main limitation of these models is 
                                          that they hope to answer questions 
                                          about future academic performance, and 
                                          yet the Covid-19 pandemic has dramatically
                                          changed the learning experiences students 
                                          had over the last year. In other words, 
                                          I am concerned about the external 
                                          validity of this data, since the learning
                                          conditions during the pandemic looked 
                                          so different from state to state, and 
                                          district to district over the last year.
                                          Another limitation of this analysis is 
                                          that my models use 5th grade test scores 
                                          to represent student achievement for 
                                          the school districts. While the scores
                                          are averaged across several years, a 
                                          stronger model may make use of more 
                                          grade levels, or examine differences 
                                          between grade levels or within grade 
                                          levels, over time."),
                                        
                                        br(),
                                        
                                        h4("Data"),
                                        p("I was inspired by my Harvard Graduate 
                                          School of Education professor, Andrew 
                                          Ho, to use the SEDA data for my analysis.
                                          I learned about this data set in his 
                                          course, Intermediate and Advanced 
                                          Statistical Methods for Applied Education
                                          Research, and was excited by the 
                                          opportunity to use it in my own original 
                                          analysis for Gov 1005. The datasets and 
                                          the reproduceable code for this analysis 
                                          can be found in my", 
                                          a(href="https://github.com/SarahBrashear",
                                            "GitHub repository."), 
                                          "For more information about the data 
                                          itself, click the Data tab at the top 
                                          of the page.")),
                                    
                                    # Here's the link to my Github repo, which 
                                    # is one of the required components of the 
                                    # project.
                                    
                                    br(),
                                    br(),
                                    br(),
                                    
                                    sidebarPanel(
                                        img(src = "IMG_6688.JPG", 
                                            height = "60%", 
                                            width = "60%",
                                            style = "display: block; margin-left: 
                                            auto; margin-right: auto;"),
                                        h3("About Me"),
                                        h4("Sarah Brashear, Ed.M. Candidate"),
                                        h4("Harvard Graduate School of Education"),
                                        p("I am a master's student studying Education
                                          Policy. As a former high school English 
                                          teacher, I think of data analysis as 
                                          another language through which to tell 
                                          compelling stories. After I leave Harvard 
                                          this spring, I hope to  use my analytical 
                                          skills and knowledge of the US education 
                                          landscape to improve the lives of 
                                          students on a much larger scale than I 
                                          could when my sphere of influence 
                                          stopped at my classroom doors."))
                           )))


# Define server logic

server <- function(input, output) {
    
    # This output is my interactive plot that's on the first tab of my app.
    # You select the state from the drop down list, and then it spits out
    # the ggplot with the data filtered for just that state.
    # I kind of got lucky with this. I guessed that the filtering part would
    # work, and then it did! On the first try!!
    
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
            
            # The alpha value here is to make the dots more transparent since
            # for some states there are a lot of dots that overlap. Solves the
            # problem of "overplotting"
            # I just picked this color because I thought it looked nice with 
            # the Sandstone theme.
            
            geom_hline(yintercept = 5, linetype = "dashed") +
            
            # The dashed line at y = 5 is a reference line. Since 5th graders
            # should be performing at a 5th grade level, this shows that if a 
            # dot is above the line, students in that district are achieving
            # above grade-level, and vice versa if it's below the line.
            
            geom_smooth(method = "lm", 
                        formula = y ~ poly(x, 3), 
                        color = "grey15", 
                        se = FALSE) +
            
            # This adds a curved line of best fit. I chose a curved line rather 
            # than a straight line because in some states the relationship is
            # very definitely NOT linear, though they are all positively 
            # correlated.
            
            labs(title = "Correlation Between SES and Academic Acheivement",
                 subtitle = "Each Bubble is a Public School District",
                 x = "Average Family Socioeconomic Status",
                 y = "Average Student Achievement of 5th Graders",
                 caption = "Source: Stanford Educational Data Archive (SEDA)") +
            theme_classic() +
            theme(legend.position = "none") +
            
            # I hid the legend because I didn't need it to display the alpha
            # value, and I had already explained that the size of the dot 
            # represented the enrollment size of the district.
            
            scale_size_continuous(range = c(1, 10))
        
        
        
    })
    
   
}

# Run the application 
shinyApp(ui = ui, server = server)
