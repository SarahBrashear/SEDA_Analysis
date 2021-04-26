
# Panel 2
# I use this panel to show my data. It mostly consists of two dt
# tables.

tabPanel("Data",
         h3("How Well Does Sexual Education Predict Health 
                         Outcomes Compared with Other Indicators?"),
         p("In the following tab, I will be making models to 
                        determine how well sexual education scores can", 
           strong("predict a selection of health outcomes.")), 
         p("For this purpose, I collected data on health outcomes 
                        per country, as well as on various predictors that may have a", 
           strong("stronger predictive value on the outcomes.")),
         p("You can take a look at all the data here!"),
         br(),
         h4("Health Outcomes"),
         dataTableOutput('health'),
         p(em("Sources: UN Stats, World Health Organization, 
                           Institute for Health Metrics and Evaluation, 
                           World Development Indicators, 
                           United Nations Development Programme", 
              style = "font-size:10px;")),
         h4("Predictors"),
         dataTableOutput('predictors'),
         p(em("Sources: UN Stats, World Development Indicators, 
                           Center for Systemic Peace, UN Population Division, 
                           United Nations Development Programme", 
              style = "font-size:10px;")),
         p("* The Democracy and Autocracy Scores are calculated 
                        on a scale from 0 to 10. The Polity Score ranges from 
                        -10 to 10.", 
           style = "font-size:10px;")
         
),

# Panel 3
# Here are my two models, including a plot for the first one and a
# regression table for both.

tabPanel("Models",
         mainPanel(
           h3("Model 1: Simple Regression"),
           p("This first model regresses a chosen 
                           health outcome on all four SDG scores."),
           p("Choose an outcome below to visualize 
                           its predicted variation as access to sexual 
                           education increases."),
           selectInput(inputId = "selected_outcome",
                       label = "Choose a health outcome:",
                       choices = c("Teen Pregnancy Rate" = "pregnancy_rate", 
                                   "Contraception Prevalence" = "contracept_prev", 
                                   "Youth HIV Rate" = "youth_hiv", 
                                   "Maternal Mortality" = "matern_mort", 
                                   "Total Life Expectancy" = "total_life_exp", 
                                   "Female Life Expectancy" = "female_life_exp"),
                       selected = "matern_mort"),
           plotOutput("model_1_plot"),
           br(),
           
           p("This plot shows us the predicted change in our chosen health 
                           outcome as the SDG 5.6.2. Total score increases. All other scores are
                           held constant at their median value."),
           p("In addition to a regression line, this plot also prints out the 
                         standard error for our predictions. As you can see, it is very wide! 
                         In the regression table, you can find the", strong("95% confidence 
                         interval"), "for each coefficient. These depict the ", 
             strong("incertainty"), "around each value. Large confidence 
                           intervals in this model suggest that the results are not very precise. 
                           This is why, in the next model, we will be including other indicators, 
                           in an attempt to determine", strong("more precise predictors.")),
           br(),
           br(),
           br(),
           
           h3("Model 2: Predictors"),
           selectInput(inputId = "selected_outcome2",
                       label = "Choose a health outcome:",
                       choices = c("Teen Pregnancy Rate" = 
                                     "pregnancy_rate", 
                                   "Contraception Prevalence" = 
                                     "contracept_prev", 
                                   "Youth HIV Rate" = 
                                     "youth_hiv", 
                                   "Maternal Mortality" = 
                                     "matern_mort", 
                                   "Total Life Expectancy" = 
                                     "total_life_exp", 
                                   "Female Life Expectancy" = 
                                     "female_life_exp"),
                       selected = "matern_mort"),
           p("For this next model, we will add a number of related indicators 
                          to our model to establish which ones most strongly predict a 
                           country’s health outcomes. By examining the coefficients for
                           each predictor, we will compare the magnitude and precision
                           with which they can accurately predict our chosen  outcome,
                           using the same logic as above. Select any variables that you think  
                           may be strong predictors of health outcomes for women and girls."),
           selectInput(inputId = "selected_predictors",
                       label = "Choose your predictors:",
                       choices = c("Urbanization Rate" = 
                                     "percent_urban_pop",
                                   "Median Age" = 
                                     "med_age",
                                   "HDI" = 
                                     "human_devel_index",
                                   "Inequality" = 
                                     "inequality_coef",
                                   "GDP per Capita" = 
                                     "gdp_capita",
                                   "School Enrollment Rate (girls)" = 
                                     "school_enroll_girls",
                                   "Gender Inequality Index" = 
                                     "gend_ineq_index",
                                   "Youth Dependency Ratio" = 
                                     "youth_depend_ratio",
                                   "Primary Healthcare Expenditures" = 
                                     "phc_expend",
                                   "Total Government Expenditures" = 
                                     "gov_expend",
                                   "Expenditures on Reproductive Health" = 
                                     "reprod_health_expend",
                                   "Expenditures on Family Planning" = 
                                     "family_plann_expend",
                                   "State Fragility Index" = 
                                     "state_frag_index",
                                   "Democracy Score" = 
                                     "democ",
                                   "Autocracy Score" = 
                                     "autoc",
                                   "Polity Score" = 
                                     "polity",
                                   "World Region" = 
                                     "region",
                                   "Total Population" = 
                                     "pop_count",
                                   "Youth Literacy Rate" = 
                                     "youth_literacy_rate",
                                   "Poverty Rate" = 
                                     "poverty_rate",
                                   "Income Group" = 
                                     "income_group"),
                       multiple = TRUE,
                       selected = c("school_enroll_girls", 
                                    "inequality_coef")),
           gt_output("model2"),
           br(),
           br()
         ),
         br(),
         br(),
         withMathJax(),
         
         # withMathJax() will allow me to write my function in LaTex.
         
         gt_output("model1"),
         p('$$ outcome_i = \\beta_0 + \\beta_1total_i + 
                          \\beta_2plan_i + \\beta_3curric_i +
                          \\beta_4laws_i + \\epsilon_i $$'),
         p("The coefficients in this regression table 
                          (under the Beta column) represent the", 
           strong("predicted change in the chosen health outcome 
                          for a unit increase of the value of each SDG score,"), 
           "holding all other scores constant at zero. A unit increase
                          here is an increase of 1 in the specific SDG score, which ranges 
                          from 0 to 100. In other words, these coefficients represent the 
                          difference between two hypothetical groups: one where all
                          scores are held constant at zero (this is our Intercept value), 
                          and another where", strong("just one of the scores increases."),
           "For example, the predicted maternal mortality rate is 492 (the 
                          Intercept of our model) for a country with scores of zero on each 
                          indicator, and it is predicted to", em("decrease by -7.5 for each unit
                          increase in the total SDG score."), "This works for other scores too:", 
           em("each increase of one"), "on the family_planning or curriculum score 
                          is estimated to", em("increase or decrease the value of the chosen health 
                          indicator"), "by the coefficient listed in the Beta column."),
         p("The mathematical formula above depicts this relationship: each 
                          coefficient in our table represents the median of the posterior 
                          for the Beta values in our above formula. The Intercept 
                          (\\(\\beta_0\\)) is the", strong("value of the health outcome
                          when all predictors are held constant"), "at zero. When multiplied
                          by the value of each corresponding SDG score, coefficients  
                          can be added up to the Intercept (\\(\\beta_0\\)) to calculate 
                          \\(outcome_i\\), the", strong("predicted value of the health outcome"),  
           "based on SDG scores. \\(\\epsilon_i\\) is the error term; it accounts 
                          for the uncertainty of our predictions."),
         br(),
         br(),
         br(),
         br(),
         br(),
         br(),    
         br(),
         br(),
         br(),
         br(),
         br(),
         h4("Interpretation:"),
         p("Looking at this regression table, the Total SDG Score for 
                        sexual education seems to be a", 
           strong("pretty poor predictor of health outcomes for women 
                                 and girls."),
           "The ", em("95% Confidence Interval"), 
           "is often huge, and the coefficient is", 
           em("relatively insignificant"), 
           "compared to other predictors. Inequality, for example, seems 
                          to be a much stronger predictor for many health outcomes."),
         p("These results suggest that", 
           strong("sexual education alone may not be sufficient for 
                          improving health outcomes"), 
           "for women and girls. The predictors with
                          the highest coefficients and lowest uncertainty, 
                          such as School Enrollment Rates for Girls or Gender 
                          Inequality Index, are those that impact health the most. 
                          These results suggest that such issues should be addressed
                          in priority, before sexual education laws can be effective.")
),


)))