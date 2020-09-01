library(tidyverse)
library(magrittr)

# load our custom functions
source(here::here("analysis/functions.R"))
load_survey_data()

# levels are low (strongly disagree) to high (strongly agree)
mylevels <- c('Strongly Disagree', 'Disagree', 'Neutral', 'Agree', 'Strongly Agree')

short_principles <- 
  c("Stewardship",
    "Accountability",
    "Commercialization",
    "Public Education",
    "Intellectual Property",
    "Public Reporting",
    "Records and Preservation",
    "Training and Resources",
    "Safe Educational and Workplace"
  )


x1 <- 
survey_data %>% 
  # get demographic data
  mutate(work_setting = as.factor(case_when(
    str_detect(`What is your primary work setting? - Selected Choice`, "Academic") ~ "Academic",
    str_detect(`What is your primary work setting? - Selected Choice`, "CRM") ~ "CRM", 
    str_detect(`What is your primary work setting? - Selected Choice`, "Gov") ~ "Government",
    TRUE ~ "other")
  ),
  years = `How many years have you worked in/with archaeology?`,
  member = `Are you a current SAA member?`,
  member = ifelse(str_detect("Yes", member), "yes", 'no'),
  rpa = `Are you a member of the RPA?`,
  age = `What is your age?`,
  gender = `What is your gender identity? - Selected Choice`,
  lgbqtia = `Do you consider yourself a member of the LGBTQIA+ community?`,
  ethnicity = `Please indicate your ethnicity: - Selected Choice`
  ) %>% 
  # get likert data for Principles
  mutate_at(vars(!!likert_q), ~factor(., levels=mylevels))  %>% 
  dplyr::select(!!likert_q,
         work_setting,
         years,
         member,
         rpa,
         age,
         #gender,
         #lgbqtia,
         #ethnicity
         ) %>% 
  drop_na() 


# run a regression for each of the nine principles
# store the output

output_lst <- vector("list", length(likert_q))

for(i in 1:length(output_lst)){

x1$pr <- x1[[names(survey_data)[likert_q[i]]]]

tbl <- table(x1$pr)

# Ordered logit model
library(MASS)
polr_out <- polr(pr ~ work_setting + years + member + rpa, 
             na.action = na.omit,
             method = "logistic",
             data = x1)

polr_out_summary <- summary(polr_out)

polr_out_aov <- car::Anova(polr_out)

#  parallel regression assumption
library(brant)
# Run the Brant test on the model
polr_out_brant <- brant(polr_out)

# get the percent change in the odds for a 
# one unit increase in the independent variable.
polr_out_coef <- (exp(polr_out$coefficients)-1)*100

polr_out_ll <- logLik(polr_out)

# pseudo-R-squared - McFadden
library(pscl)
polr_out_pr2 <- pR2(polr_out)

# confidence intervals
polr_out_ci <- confint(polr_out, level = 0.95)

# no p-value, t-value more than |2| indicates significant
# likelihood of disagreeing 
# 

output_lst[[i]] <- 
  list(p = names(survey_data)[likert_q[i]],
       tbl = tbl,
       polr_out = polr_out,
       polr_out_aov = polr_out_aov, 
       polr_out_summary= polr_out_summary,
       polr_out_brant = polr_out_brant,
       polr_out_coef = polr_out_coef,
       polr_out_ll = polr_out_ll,
       polr_out_ci = polr_out_ci)

}

names(output_lst) <- short_principles

# guide to interpretation:
# https://rpubs.com/rslbliss/r_logistic_ws

# look at anova only, this allows us to see the p-values
# for significant variables in each model
output_lst_aov_tbl <- 
map_df(output_lst, 
       ~broom::tidy(.x$polr_out_aov),
       .id = "model")

# Plotting
library(effects)
plot(Effect(focal.predictors = c("years","member","rpa","work_setting"), 
            output_lst[[8]]$polr_out),
     rug = FALSE)
        