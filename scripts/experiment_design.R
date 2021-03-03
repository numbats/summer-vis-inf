library(edibble)


set.seed(1)
expdf <- start_design("Residual diagnostic for multi-level models") %>%
  set_context(data_source = "lme4::sleepstudy",
              data_info = "18 subjects measured over 10 days on reaction time",
              noise = "20% of the mean added to ~33% of participants (=6 participants) for (random) 2 days") %>%
  set_units(person0 = 100,
            question = nested_in(person0, 8)) %>%
  set_trts(resid = c("Conditional", "Least-confounded"),
           noise = c("no", "yes"),
           plot = c("QQ-plot", "residual plot"),
           data = c("rep1", "rep2", "rep3", "rep4", "rep5")) %>%
  allocate_trts(resid:noise:plot ~ question,
                data ~ person0) %>%
  randomise_trts() %>%
  serve_table()


###

library(tidyverse)
expdf <- expdf %>%
  tsibble::tibble()%>%
  mutate(
    plot_num=case_when(
      str_detect(noise,"no")~1,
      str_detect(noise,"yes")~2

    ))
expdf$version <- regmatches(expdf$data,regexpr(".$", expdf$data))
expdf$p_id <- as.numeric(stringi::stri_sub(expdf$person0,7))
expdf <- expdf%>%
  mutate(
    plot_order=case_when(
      str_detect(resid,"Conditional") &str_detect(plot,"residual plot")~1,
      str_detect(resid,"Conditional") & str_detect(plot,"QQ-plot")~2,
      str_detect(resid,"Least-confounded") & str_detect(plot,"QQ-plot")~3,
      str_detect(resid,"Least-confounded") & str_detect(plot,"residual plot")~4
    ))

expdf <- expdf %>%
  mutate(image_list=paste0("www/images/sleepstudy/v",plot_num,version ,"_" , plot_order ,".png"))

expdf$unique_id <- 0
expdf$unique_id <- as.character(expdf$unique_id)


write.csv(expdf,file = "experiment/data/experiment.csv")

#googlesheets4::sheet_write(df)

ssid <- as_sheets_id("https://docs.google.com/spreadsheets/d/1HekgyCloy5uXZzriqHO_EKFvDXqDV4jAse3DEPfow7A/edit#gid=1756982656")
class(ssid)
unclass(ssid)

id <- as_sheets_id("https://docs.google.com/spreadsheets/d/1X84LWPw0JiH9KslBzCEF4cNGe1hFsmrGJ7IM4PwJIC4/edit#gid=0")
unclass(id)
