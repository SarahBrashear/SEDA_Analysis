# Student Achievement and SES in American Public Schools
Analyzing the relationship between student achievement and SES in the 13,000 public school districts in the United States. Data from the Stanford Education Data Archive.

Check out my Shiny App: https://sarahbrashear.shinyapps.io/SEDA_App_2021/

## Summary
This project explores the question of whether demography is destiny in American public school districts. I started by examining the relationship between socio-economic status and student achievement for each of the roughly 13,000 school districts in the country. I grouped districts by their states since the majority of education policy decision-making happens at the state level. In each state, there is a positive correlation between SES and student achievement; however, the magnitude of correlations vary. I then built a predictive model that draws on all 450 million test scores from students in grades 3-8 from 2008-2018, that predicts that 5th graders from a typical school district will perform slightly above grade-level on future assessments under a certain set of assumptions. Because I was interested in the variance between states, I also ran the same model on subsets of the data (a Texas model, a Califonia model, and a Massachusetts model) under three different sets of assumptions (low SES, average SES, and high SES). Ultimately the variance between these states could be used to inform policy decisions in the future as state education agencies strategically allocate funding and support for post-pandemic recovery.

## Motivation
I was inspired by my Harvard Graduate School of Education professor, Andrew Ho, to use the SEDA data for my analysis. I learned about this data set in his course, Intermediate and Advanced Statistical Methods for Applied Education Research, and was excited by the opportunity to use it in my own analysis.
