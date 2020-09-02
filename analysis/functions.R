

load_survey_data <- function(){
  library(tidyverse)
survey_data <- 
  readr::read_csv("data/raw-data/Data Pull 06-10-20 CSV.csv") %>% 
  select(-starts_with("X"))

# names from the first data pull only
# saveRDS(names(survey_data), here::here("data/derived-data/column-names.rds"))

# get col names from first data pull
saved_names <- readRDS(here::here("data/derived-data/column-names.rds"))

# only keep cols that are in the first data pull
survey_data <<- 
  survey_data %>% 
  select(saved_names)

# to easily browse the questions:
survey_questions_tbl <<- tibble(questions = names(survey_data))
survey_questions_vec <<- names(survey_data)
single_response <<- c(4, 5, 35, 44, 46, 47, 49, 50, 52, 53, 55)
multiple_response <<- c(6, 7, 42, 54)
likert_q <<- str_which(survey_questions_vec, "^Principle")
likert_general <<- str_which(survey_questions_vec, "^The SAA Principles of Ethics")
}


# decode demographic variables
decode_demographics <- function(){
  
  key_sheets <- readxl::excel_sheets(here::here("data/raw-data/SAA Ethics TF 2 Survey - 09-01-20 - Key.xlsx"))
  key_tbl <- map(key_sheets[-1], 
                 ~readxl::read_excel(here::here("data/raw-data/SAA Ethics TF 2 Survey - 09-01-20 - Key.xlsx"),
                                    sheet = .x) %>% 
                   rename_all(tolower)) 
  
  demographic_variables <<-
    survey_questions_vec[c(4, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56)]
  
  # to match with the key
  demographic_variables_key <- demographic_variables[c(4, 11, 10, 8, 7, 5)]
  
  names(key_tbl) <- demographic_variables_key
  
# rename columns so we can join easily 
  key_tbl_nm <- 
  map2(key_tbl,
       demographic_variables_key,
       ~.x %>% rename(!!.y := names(.x)[2] ))
  
  # demographic question responses only 
  survey_data_demographics <- 
  survey_data %>%
    dplyr::select(!!demographic_variables_key)
  
  survey_data_demographics %>% 
   select(!!demographic_variables_key[3]) %>% 
    left_join(key_tbl_nm[["Geographical area of origin:"]]) %>% View
    
  
zzz <-  
map(key_tbl_nm, 
    ~ survey_data %>%
      dplyr::select(!!demographic_variables_key) %>% 
    left_join(.x) %>% 
  dplyr::select(-c(!!demographic_variables_key)))
 

 zzz$`What is your age?`                                             # 1,661 
 # Age - 62 matches both '70 and over' and 'Prefer not to answer' 
 
 zzz$`Geographical area of origin:`                                 # 1,658
 # GeoOrigin - 33 matches both Eastern Asia & Latin America and the Caribbean
 
 zzz$`Current place of residence:`                                   # 2,500
 # Residency - 90 matches both Polynesia and Eastern Europe 
 
 zzz$`Please indicate your ethnicity: - Selected Choice`             # 1,542 
 zzz$`Do you consider yourself a member of the LGBTQIA+ community?`  # 1,542 
 zzz$`What is your gender identity? - Selected Choice`               # 1,542 
 

}



# general purpose function for single option questions

plot_single_option_question <- function(x, 
                                        wrap = 20, 
                                        title_wrap = 60,
                                        base_size = 10,
                                        sort_bars = FALSE){
  
  output <- 
    survey_data %>% 
    group_by(!!as.name(x)) %>% 
    tally() %>% 
    drop_na() 
  
  if(sort_bars) {
    
    ggplot(output, 
           aes(reorder(str_wrap(!!as.name(x), wrap), n),
               n)) +
      geom_col() +
      coord_flip() +
      xlab("") +
      scale_x_discrete(labels = function(x) str_wrap(x, width = wrap)) +
      ggtitle(str_wrap(paste0(names(output)[1], 
                              " (", sum(output$n), 
                              " valid responses)"),
                       title_wrap)) +
      theme_bw(base_size = base_size)
    
    
  } else {
    
    ggplot(output, 
           aes(!!as.name(x), # str_wrap(!!as.name(x), wrap), 
               n)) +
      geom_col() +
      coord_flip() +
      xlab("") +
      ggtitle(str_wrap(paste0(names(output)[1], 
                              " (", sum(output$n), 
                              " valid responses)"),
                       title_wrap)) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = wrap)) +
      theme_bw(base_size = base_size)
  }
}


plot_single_option_question_by_membership <- function(x, 
                                                      wrap = 20, 
                                                      title_wrap = 60,
                                                      base_size = 10,
                                                      sort_bars = FALSE){
  
  output <- 
    survey_data %>% 
    mutate(saa_member = `Are you a current SAA member?`) %>% 
    group_by(!!as.name(x), 
             saa_member) %>% 
    tally() %>% 
    drop_na() 
  
  if(sort_bars) {
    
    ggplot(output, 
           aes(reorder(str_wrap(!!as.name(x), wrap), n),
               fill = saa_member,
               n)) +
      geom_col(position = position_fill()) +
      coord_flip() +
      xlab("") +
      ggtitle(str_wrap(names(output)[1],
                       title_wrap)) +
      theme_bw(base_size = base_size) +
      scale_fill_viridis_d(name = "SAA Membership Status")
    
    
  } else {
    
    ggplot(output, 
           aes(str_wrap(!!as.name(x), wrap), 
               n,
               fill = saa_member)) +
      geom_col(position = position_fill()) +
      coord_flip() +
      xlab("") +
      ggtitle(str_wrap(names(output)[1],
                       title_wrap)) +
      theme_bw(base_size = base_size) +
      scale_fill_viridis_d(name = "SAA Membership Status")
  }
}

plot_single_option_question_by_workplace <- function(x, 
                                                     wrap = 20, 
                                                     title_wrap = 60,
                                                     base_size = 10,
                                                     sort_bars = FALSE){
  
  output <- 
    survey_data %>% 
    filter(str_detect(`What is your primary work setting? - Selected Choice`, 
                      "Academic - 4-year institution with a graduate program|CRM")) %>% 
    mutate(work_setting = ifelse(str_detect(`What is your primary work setting? - Selected Choice`, 
                                            "CRM"), "CRM", "Univeristy")) %>% 
    group_by(!!as.name(x), 
             work_setting) %>% 
    tally() %>% 
    drop_na() 
  
  if(sort_bars) {
    
    ggplot(output, 
           aes(reorder(str_wrap(!!as.name(x), wrap), n),
               fill = work_setting,
               n)) +
      geom_col(position = position_fill()) +
      coord_flip() +
      xlab("") +
      ggtitle(str_wrap(names(output)[1],
                       title_wrap)) +
      theme_bw(base_size = base_size) +
      scale_fill_viridis_d(name = "Work setting")
    
    
  } else {
    
    ggplot(output, 
           aes(str_wrap(!!as.name(x), wrap), 
               n,
               fill = work_setting)) +
      geom_col(position = position_fill()) +
      coord_flip() +
      xlab("") +
      ggtitle(str_wrap(names(output)[1],
                       title_wrap)) +
      theme_bw(base_size = base_size) +
      scale_fill_viridis_d(name = "Work setting")
  }
}


plot_multiple_option_question <- function(x, wrap = 30, 
                                          truncn = 50,
                                          title_wrap = 60,
                                          base_size = 10){
  
  x_tbl <- 
    survey_data %>% 
    select(x) 
  
  options <-  
    x_tbl %>%  # split on word,Word
    separate_rows(names(x_tbl), sep = ",(?=[[:upper:]])") %>% 
    distinct() %>% 
    drop_na() %>% 
    pull
  
  cross_tab <-   
    x_tbl %>% 
    mutate(x = !!as.name(x)) %>% 
    select(x) %>% 
    separate(x, 
             paste0("v", 1:length(options)), 
             sep = ",(?=[[:upper:]])", remove=F) %>% 
    select(starts_with("v")) %>% 
    pivot_longer(cols = v2:paste0("v", length(options)),
                 names_to = "q",
                 values_to = "val")  %>% 
    drop_na %>% 
    widyr::pairwise_count(v1, val) %>% 
    spread(item2, n, fill = 0)
  
  # Separate rownames
  y <- cross_tab
  row_name <- y$item1
  y <- y[, colnames(y) != "item1"]
  
  # Remove lower matrix
  y[lower.tri(y)] <- NA
  
  # Reappend rownames
  new_col <- tibble::tibble(row_name)
  names(new_col) <- "."
  new_df <- c(new_col, y)
  z <- 
    dplyr::as_tibble(new_df) %>% 
    mutate_all(as.character) %>% 
    replace(is.na(.), "")
  
  # count of each 
  x_tbl_tally <- 
    x_tbl %>% 
    mutate(x = !!as.name(x)) %>% 
    mutate(x = str_squish(x)) %>% 
    select(x) %>% 
    separate_rows(x, 
                  sep = ",(?=[[:upper:]])") %>% 
    mutate(x = str_squish(x)) %>% 
    group_by(x) %>% 
    tally() %>% 
    drop_na() %>% 
    arrange(-n)
  
  plot <- 
    ggplot(x_tbl_tally, 
           aes(reorder(str_wrap(x, wrap), n),
               n)) +
    geom_col() +
    coord_flip() +
    xlab("") +
    ggtitle(str_wrap(paste0(str_trunc(x, truncn), 
                            " (", sum(x_tbl_tally$n), 
                            " valid responses)"), 
                     title_wrap)) +
    theme_bw(base_size = base_size)
  
  
  upset_tbl <- 
    x_tbl %>% 
    mutate(x = !!as.name(x)) %>% 
    mutate(id = row_number()) %>% 
    separate_rows(x, sep = ",(?=[[:upper:]])") %>% 
    drop_na(x) %>% 
    mutate(y = 1) %>%
    spread(x, y, fill = 0)  
  
  upset_plot <- 
    upset_tbl %>% 
    as.data.frame() %>% 
    upset(sets.bar.color = "gray20",
          main.bar.color = "gray20",
          order.by = "freq",
          nsets = length(options)) 
  
  upset_plot_caption <-  
    paste0(str_trunc(x, truncn), 
           " (", 
           sum(x_tbl_tally$n), 
           " selections in ",
           nrow(x_tbl),
           " responses)")
  
  return(list(plot = plot, 
              crosstab = z,
              tally = x_tbl_tally,
              upset = upset_tbl,
              upset_plot = upset_plot,
              upset_plot_caption = upset_plot_caption))
}


load_free_text <- function(){
free_text <<- 
  survey_data %>% 
  select("How have you used the SAA Principles of Ethics? Please check all that apply. - Other - Text",
         "Please feel free to elaborate on your answer here:" ,
         "What do you see as being the primary ethical concerns in the field today?",
         "Please elaborate here:",       # Principle 1: Stewardship
         "Please elaborate here:_1",     # Principle 2: Accountability
         "Please elaborate here:_2",     # Principle 3: Commercialization
         "Please elaborate here:_3",     # Principle 4: Public Education
         "Please elaborate here:_4",     # Principle 5: Intellectual Property
         "Please elaborate here:_5",     # Principle 6: Public Reporting 
         "Please elaborate here:_6",     # Principle 7: Records and Preservation
         "Please elaborate here:_7",     # Principle 8: Training and Resources
         "Please elaborate here:_8",     # Principle 9: Safe Educational and Workplace Environments
         "Please elaborate here:_9",     #  satisfactorily addresses ethical situations
         "Please elaborate here:_10",    #  addresses ethical concerns in the country in which I work
         "Please elaborate here:_11",    #  addresses ethical concerns in the sector in which I work
         "Please elaborate here:_12",    #  additional ethical issues?
         "Feel free to comment on these formats or suggest additional formats here:" # ethical documents
  ) 

free_text_short <<- 
  stack(free_text) %>% 
  as_tibble() %>% 
  mutate(
    question = case_when(
      ind == "How have you used the SAA Principles of Ethics? Please check all that apply. - Other - Text" ~ "how used",
      ind == "Please feel free to elaborate on your answer here:" ~ "other codes",
      ind == "What do you see as being the primary ethical concerns in the field today?" ~ "primary concerns",
      ind == "Please elaborate here:" ~ "Principle 1: Stewardship",
      ind == "Please elaborate here:_1"  ~ "Principle 2: Accountability",
      ind == "Please elaborate here:_2"  ~ "Principle 3: Commercialization",
      ind == "Please elaborate here:_3"  ~ "Principle 4: Public Education",
      ind == "Please elaborate here:_4"  ~ "Principle 5: Intellectual Property",
      ind == "Please elaborate here:_5"  ~ "Principle 6: Public Reporting ",
      ind == "Please elaborate here:_6"  ~ "Principle 7: Records and Preservation",
      ind == "Please elaborate here:_7"  ~ "Principle 8: Training and Resources",
      ind == "Please elaborate here:_8"  ~ "Principle 9: Safe Environments",
      ind == "Please elaborate here:_9"  ~ "addresses ethical situations",
      ind == "Please elaborate here:_10" ~ "addresses ethical concerns in country",
      ind == "Please elaborate here:_11" ~ "addresses ethical concerns in  sector",
      ind == "Please elaborate here:_12" ~ "additional ethical issues?",
      ind == "Feel free to comment on these formats or suggest additional formats here:" ~  "ethical documents",
      TRUE ~ "other"
    ))
}

load_tagged_text <- function(){
  tagged_files <- list.files(here::here("data/tagged-text"), 
                             full.names = TRUE,
                             # only use the most recent export
                             pattern = "3")
  library(tidyverse)
  library(rvest)
  library(qdapRegex)
  
  tidy_tags <- function(x){
    a1 <- 
      read_lines(x ) %>% 
      str_squish() %>% 
      paste0(collapse = ", ") %>% 
      strsplit("<hr>", fixed=TRUE)
    
    txt <- ex_between(a1, "<p>", "<strong>")[[1]] %>% 
      str_remove_all("</p>, <p>,")
    doc <- ex_between(a1, "<strong>Document:</strong>", "<strong>Tags:")[[1]]
    tag <- ex_between(a1, "<strong>Tags:</strong>", "</p>")[[1]] %>% 
      str_replace_all(",+", ",") %>% 
      str_remove_all( ",\\s*$") %>% 
      str_remove_all( "^,")  %>% 
      str_remove_all( ", ,") %>% 
      str_remove_all( ", $") %>% 
      str_squish()
    
    tibble(txt = txt,
           tag = tag,
           doc = doc)
  }
  
  out_all <<- 
    map_df(tagged_files, 
           tidy_tags)
  
  principle_tags <<- 
    out_all %>% 
    filter(str_detect(doc, "Principle")) %>%
    separate_rows(tag, sep = ", ") %>% 
    mutate(tag = str_squish(tag)) %>% 
    mutate(doc = str_remove_all(doc, "_|\\.txt|,|\\(1\\)"))
}

