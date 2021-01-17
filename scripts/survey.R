library(readxl)
library(dplyr)
library(tidyverse)
library(stringr)
survey_df <- read_xlsx(here::here("data/survey-thesis.xlsx"), 
                       sheet = 1) %>% 
  mutate(gender = as.factor(gender), education = as.factor(education),
         age = as.factor(age), eco = as.factor(eco), identifier = as.factor(identifier),
         certainty = as.numeric(certainty))

count(survey_df$identifier)

survey_df <- survey_df[!survey_df$name %in% c("test", "Steph", "tan", "li  jing"),]

survey_df <- survey_df %>% group_by(identifier, plot_name) %>% 
  slice(1) %>% ungroup() %>% 
  arrange(identifier, plor_order)

# gender
survey_df %>% group_by(gender) %>% dplyr::summarise(n = n()/12)

# age
survey_df %>% group_by(age) %>% dplyr::summarise(n = n()/12)

# education
survey_df %>% group_by(education) %>% dplyr::summarise(n = n()/12)

# eco
survey_df %>% group_by(eco) %>% dplyr::summarise(n = n()/12)

count(survey_df$identifier)

correct_response <- c("aut_v11_1.png" = 7, "aut_v12_1.png" = 7, 
                      "aut_v13_1.png" = 13, "aut_v14_1.png" = 13,
                      "lin_v11_1.png" = 7, "lin_v12_1.png" = 7,
                      "lin_v13_1.png" = 13, "lin_v14_1.png" = 13,
                      "slp_v11_1.png" = 7, "slp_v12_1.png" = 7,
                      "slp_v13_1.png" = 13, "slp_v14_1.png" = 13,
                      
                      "aut_v11_2.png" = 7, "aut_v12_2.png" = 7, 
                      "aut_v13_2.png" = 13, "aut_v14_2.png" = 13,
                      "lin_v11_2.png" = 7, "lin_v12_2.png" = 7,
                      "lin_v13_2.png" = 13, "lin_v14_2.png" = 13,
                      "slp_v11_2.png" = 7, "slp_v12_2.png" = 7,
                      "slp_v13_2.png" = 13, "slp_v14_2.png" = 13,
                      
                      "aut_v11_3.png" = 9, "aut_v12_3.png" = 9, 
                      "aut_v13_3.png" = 4, "aut_v14_3.png" = 4,
                      "lin_v11_3.png" = 9, "lin_v12_3.png" = 9,
                      "lin_v13_3.png" = 4, "lin_v14_3.png" = 4,
                      "slp_v11_3.png" = 9, "slp_v12_3.png" = 9,
                      "slp_v13_3.png" = 4, "slp_v14_3.png" = 4,
                      
                      "aut_v11_4.png" = 9, "aut_v12_4.png" = 9, 
                      "aut_v13_4.png" = 4, "aut_v14_4.png" = 4,
                      "lin_v11_4.png" = 9, "lin_v12_4.png" = 9,
                      "lin_v13_4.png" = 4, "lin_v14_4.png" = 4,
                      "slp_v11_4.png" = 9, "slp_v12_4.png" = 9,
                      "slp_v13_4.png" = 4, "slp_v14_4.png" = 4,
                      
                      "aut_v21_1.png" = 5, "aut_v22_1.png" = 5, 
                      "aut_v23_1.png" = 15, "aut_v24_1.png" = 15,
                      "lin_v21_1.png" = 5, "lin_v22_1.png" = 5,
                      "lin_v23_1.png" = 15, "lin_v24_1.png" = 15,
                      "slp_v21_1.png" = 5, "slp_v22_1.png" = 5,
                      "slp_v23_1.png" = 15, "slp_v24_1.png" = 15,
                      
                      "aut_v21_2.png" = 5, "aut_v22_2.png" = 5, 
                      "aut_v23_2.png" = 15, "aut_v24_2.png" = 15,
                      "lin_v21_2.png" = 5, "lin_v22_2.png" = 5,
                      "lin_v23_2.png" = 15, "lin_v24_2.png" = 15,
                      "slp_v21_2.png" = 5, "slp_v22_2.png" = 5,
                      "slp_v23_2.png" = 15, "slp_v24_2.png" = 15,
                      
                      "aut_v21_3.png" = 8, "aut_v22_3.png" = 8, 
                      "aut_v23_3.png" = 20, "aut_v24_3.png" = 20,
                      "lin_v21_3.png" = 8, "lin_v22_3.png" = 8,
                      "lin_v23_3.png" = 20, "lin_v24_3.png" = 20,
                      "slp_v21_3.png" = 8, "slp_v22_3.png" = 8,
                      "slp_v23_3.png" = 20, "slp_v24_3.png" = 20,
                      
                      "aut_v21_4.png" = 8, "aut_v22_4.png" = 8, 
                      "aut_v23_4.png" = 20, "aut_v24_4.png" = 20,
                      "lin_v21_4.png" = 8, "lin_v22_4.png" = 8,
                      "lin_v23_4.png" = 20, "lin_v24_4.png" = 20,
                      "slp_v21_4.png" = 8, "slp_v22_4.png" = 8,
                      "slp_v23_4.png" = 20, "slp_v24_4.png" = 20,
                      
                      "aut_v31_1.png" = 15, "aut_v32_1.png" = 15, 
                      "aut_v33_1.png" = 2, "aut_v34_1.png" = 2,
                      "lin_v31_1.png" = 15, "lin_v32_1.png" = 15,
                      "lin_v33_1.png" = 2, "lin_v34_1.png" = 2,
                      "slp_v31_1.png" = 15, "slp_v32_1.png" = 15,
                      "slp_v33_1.png" = 2, "slp_v34_1.png" = 2,
                      
                      "aut_v31_2.png" = 15, "aut_v32_2.png" = 15, 
                      "aut_v33_2.png" = 2, "aut_v34_2.png" = 2,
                      "lin_v31_2.png" = 15, "lin_v32_2.png" = 15,
                      "lin_v33_2.png" = 2, "lin_v34_2.png" = 2,
                      "slp_v31_2.png" = 15, "slp_v32_2.png" = 15,
                      "slp_v33_2.png" = 2, "slp_v34_2.png" = 2,
                      
                      "aut_v31_3.png" = 11, "aut_v32_3.png" = 11, 
                      "aut_v33_3.png" = 6, "aut_v34_3.png" = 6,
                      "lin_v31_3.png" = 11, "lin_v32_3.png" = 11,
                      "lin_v33_3.png" = 6, "lin_v34_3.png" = 6,
                      "slp_v31_3.png" = 11, "slp_v32_3.png" = 11,
                      "slp_v33_3.png" = 6, "slp_v34_3.png" = 6,
                      
                      "aut_v31_4.png" = 11, "aut_v32_4.png" = 11, 
                      "aut_v33_4.png" = 6, "aut_v34_4.png" = 6,
                      "lin_v31_4.png" = 11, "lin_v32_4.png" = 11,
                      "lin_v33_4.png" = 6, "lin_v34_4.png" = 6,
                      "slp_v31_4.png" = 11, "slp_v32_4.png" = 11,
                      "slp_v33_4.png" = 6, "slp_v34_4.png" = 6)


result <- survey_df %>%  group_by(plot_name) %>%  
  # filter(responses == (correct_response[plot_name]))
  dplyr::summarise(n = n(), correct = sum(responses==as.numeric(correct_response[plot_name])))

# Marginal residuals
## V1
result %>% filter(str_detect(plot_name, '1._1')) %>% summarise(sum(n), sum(correct))
## V2
result %>% filter(str_detect(plot_name, '2._1')) %>% summarise(sum(n), sum(correct))
## V3
result %>% filter(str_detect(plot_name, '3._1')) %>% summarise(sum(n), sum(correct))

# Conditional residuals
## V1
result %>% filter(str_detect(plot_name, '1._2')) %>% summarise(sum(n), sum(correct))
## V2
result %>% filter(str_detect(plot_name, '2._2')) %>% summarise(sum(n), sum(correct))
## V3
result %>% filter(str_detect(plot_name, '3._2')) %>% summarise(sum(n), sum(correct))

# Conditional residuals Normality
## V1
result %>% filter(str_detect(plot_name, '1._3')) %>% summarise(sum(n), sum(correct))
## V2
result %>% filter(str_detect(plot_name, '2._3')) %>% summarise(sum(n), sum(correct))
## V3
result %>% filter(str_detect(plot_name, '3._3')) %>% summarise(sum(n), sum(correct))

# Least confounded residual normality
## V1
result %>% filter(str_detect(plot_name, '1._4')) %>% summarise(sum(n), sum(correct))
## V2
result %>% filter(str_detect(plot_name, '2._4')) %>% summarise(sum(n), sum(correct))
## V3
result %>% filter(str_detect(plot_name, '3._4')) %>% summarise(sum(n), sum(correct))

## Aut
result %>% filter(str_detect(plot_name, 'aut_.3._1')) %>% summarise(sum(n), sum(correct))
result %>% filter(str_detect(plot_name, 'aut_.3._2')) %>% summarise(sum(n), sum(correct))
result %>% filter(str_detect(plot_name, 'aut_.3._3')) %>% summarise(sum(n), sum(correct))
result %>% filter(str_detect(plot_name, 'aut_.3._4')) %>% summarise(sum(n), sum(correct))

# Lin
result %>% filter(str_detect(plot_name, 'lin_.3._1')) %>% summarise(sum(n), sum(correct))
result %>% filter(str_detect(plot_name, 'lin_.3._2')) %>% summarise(sum(n), sum(correct))
result %>% filter(str_detect(plot_name, 'lin_.3._3')) %>% summarise(sum(n), sum(correct))
result %>% filter(str_detect(plot_name, 'lin_.3._4')) %>% summarise(sum(n), sum(correct))

# slp
result %>% filter(str_detect(plot_name, 'slp_.3._1')) %>% summarise(sum(n), sum(correct))
result %>% filter(str_detect(plot_name, 'slp_.3._2')) %>% summarise(sum(n), sum(correct))
result %>% filter(str_detect(plot_name, 'slp_.3._3')) %>% summarise(sum(n), sum(correct))
result %>% filter(str_detect(plot_name, 'slp_.3._4')) %>% summarise(sum(n), sum(correct))


# Certainty
certainty <- survey_df %>%
  mutate(detect = ifelse(responses == correct_response[plot_name], 1, 0)) %>% 
  mutate(detect_f = factor(detect, levels = c(0, 1), 
                           labels = c("Detected? No", "Detected? Yes"))) %>% 
  mutate(Detected = ifelse(detect_f == "Detected? Yes", "Yes", "No")) %>% 
  mutate(vers = case_when(
    str_detect(plot_name, 'v1') ~ "Version 1",
    str_detect(plot_name, 'v2') ~ "Version 2",
    str_detect(plot_name, 'v3') ~ "Version 3"
  )) %>% 
  mutate(type = case_when(
    str_detect(plot_name, 'aut') ~ "Mixed",
    str_detect(plot_name, 'lin') ~ "Categorical",
    str_detect(plot_name, 'slp') ~ "Numerical"
  )) %>% 
  group_by(vers, type) %>%
  ggplot(aes(certainty, fill = Detected)) + geom_bar() + 
  facet_grid(type ~ vers) +
  theme(legend.position = "bottom") + 
  xlab("Level of certainty")


survey_df %>% filter(str_detect(plot_name, "v3")) %>% tally() #223
survey_df %>% filter(str_detect(plot_name, "v2")) %>% tally() #194
survey_df %>% filter(str_detect(plot_name, "v1")) %>% tally() #183

version <- survey_df %>% 
  mutate(detect = ifelse(responses == correct_response[plot_name], 1, 0)) %>% 
  mutate(detect_f = factor(detect, levels = c(0, 1), 
                           labels = c("Detected? No", "Detected? Yes"))) %>% 
  mutate(Detected = ifelse(detect_f == "Detected? Yes", "Yes", "No")) %>% 
  mutate(vers = case_when(
  str_detect(plot_name, 'v1') ~ "Version 1",
  str_detect(plot_name, 'v2') ~ "Version 2",
  str_detect(plot_name, 'v3') ~ "Version 3"
)) %>% 
  group_by(vers) %>% tally()
  ggplot(aes(vers, fill = Detected)) + geom_bar() + theme(legend.position = "bottom") + 
  xlab("Version")
