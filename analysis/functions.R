

load_survey_data <- function(){
  library(tidyverse)
survey_data <- 
  readr::read_csv(here::here("data/private-data/Data Pull 06-10-20 CSV.csv")) %>% 
  # readr::read_csv(here::here("data/raw-data/Data Pull 06-10-20 CSV.csv")) %>% 
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
  library(tidyverse)
  key_sheets <- 
    readxl::excel_sheets(here::here("data/private-data/SAA Ethics TF 2 Survey - 09-01-20 - Key.xlsx"))
  key_tbl <- map(key_sheets[-1], 
                 ~readxl::read_excel(here::here("data/private-data/SAA Ethics TF 2 Survey - 09-01-20 - Key.xlsx"),
                                    sheet = .x) %>% 
                   rename_all(tolower)) 
  # but he made a mistake and updated some of the variables...
  key_sheets_1 <- readxl::excel_sheets(here::here("data/private-data/SAA Ethics TF 2 Survey - 09-02-20 - Key Correction Data.xlsx"))
  key_tbl_1 <- map(key_sheets_1[-1], 
                 ~readxl::read_excel(here::here("data/private-data/SAA Ethics TF 2 Survey - 09-02-20 - Key Correction Data.xlsx"),
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
  
  # we are just going to do some of the demo vars here, because the others need
  # to be treated completely differently
  key_tbl_nm_ok  <- key_tbl_nm[c(-1, -2, -3)]
  key_tbl_nm_not_ok  <- key_tbl_nm[c(1, 2, 3)]
  
  # demographic question responses only 
  survey_data_demographics <- 
  survey_data %>%
    dplyr::select(!!demographic_variables_key)
 
zzz <-  
map(key_tbl_nm_ok, 
    ~ survey_data %>%
      dplyr::select(!!demographic_variables_key) %>% 
    left_join(.x) %>% 
  dplyr::select(-c(!!demographic_variables_key))) %>% 
  bind_cols()

# now that's one set of demo vars done

# now for the set he botched
aaa <- 
survey_data %>% 
  mutate(`Geographical area of origin:` = ifelse(`Geographical area of origin:` == "GeoOrigin - 29", 
         NA, `Geographical area of origin:`)) %>% 
  left_join(key_tbl_1[[1]],
            by = c("Response ID" = "response id")) %>% 
  left_join(key_tbl_1[[2]],
            by = c("Response ID" = "response id")) %>% 
  left_join(key_tbl_1[[3]],
            by = c("Response ID" = "response id")) %>% 
  dplyr::select(!!demographic_variables_key,
                `what is your age?`,
                `geographical area of origin:`,
                `current place of residence:`) %>% 
  mutate(`What is your age?` = ifelse(is.na(`what is your age?`),
                      `What is your age?`,
                      `what is your age?`)) %>% 
  mutate(`Current place of residence:` = ifelse(is.na(`current place of residence:`),
                      `Current place of residence:`,
                      `current place of residence:`))  %>% 
  mutate(`Geographical area of origin:` = ifelse(is.na(`geographical area of origin:`),
                            `Geographical area of origin:`,
                            `geographical area of origin:`))

demo_cols <- c("What is your age?", 
              "Current place of residence:", 
              "Geographical area of origin:")

zzz1 <-  
  map2(key_tbl_nm_not_ok, 
       demo_cols,
      ~ aaa %>%
        dplyr::select(.y) %>% 
        left_join(.x) ) %>% 
  bind_cols() %>% 
  # combine cols
  mutate(`What is your age?` = ifelse(is.na(age), 
                                      `What is your age?`, 
                                      age)) %>% 
  mutate(`Current place of residence:` = ifelse(is.na(residency), 
                                                `Current place of residence:`, 
                                                residency)) %>% 
  mutate(`Geographical area of origin:` = ifelse(is.na(geoorigin), 
                                                `Geographical area of origin:`, 
                                                geoorigin))  %>% 
  select(age = `What is your age?` ,
         residence = `Current place of residence:`,
         geoorigin = `Geographical area of origin:` )

# combine all demo vars
survey_data_demographics <<-bind_cols(zzz, zzz1)

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
                                          base_size = 10,
                                          text.scale = 1,
                                          show.numbers = FALSE, 
                                          mb.ratio = c(0.5, 0.5)){
  
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
          nsets = length(options),
          text.scale = text.scale,
          show.numbers = show.numbers,
          mb.ratio = mb.ratio
          ) 
  
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


iclust.diagram.custom <- 
function (ic, 
          labels = NULL, 
          short = FALSE, 
          digits = 2,
          cex = NULL,
          cex.variables = NULL,
          cex.in.circle = NULL,
          cex.arrow.digits = NULL,
          min.size = NULL, 
          e.size = 1, 
          colors = c("black", "blue"), 
          main = "", 
          cluster.names = NULL, 
          marg = c(0.5,  0.5, 1.5, 0.5)
          ) 
{
  old.par <- par(mar = marg)
  on.exit(par(old.par))
  clusters <- ic$results
  num <- nrow(clusters)
  num.var <- num + 1
  if (is.null(cex)) 
    cex <- min(16/num.var, 1)
  if (is.null(labels)) {
    var.labels <- rownames(ic$loadings)
  }
  else {
    var.labels = labels
  }
  if (short) {
    var.labels <- paste("V", 1:num.var, sep = "")
  }
  if (is.null(var.labels)) {
    var.labels <- paste("V", 1:num.var, sep = "")
  }
  fixed <- psych:::fix.names(ic, var.labels)
  clusters <- fixed$ic$results
  max.len <- max(nchar((var.labels)))
  if (is.null(cluster.names)) 
    cluster.names <- rownames(clusters)
  names(cluster.names) <- rownames(clusters)
  length.labels <- max(max.len * 0.15 * cex, 0.25 * cex)
  nc <- length(ic$size)
  nvar <- sum(ic$size)
  last <- dim(clusters)[1]
  max.size <- max(ic$size)
  limx <- c(-length.labels, nvar + 2)
  limy <- c(0, nvar + 1)
  if (nvar < 12) 
    e.size <- e.size * 0.7
  if (is.null(min.size)) 
    min.size <- 0.1 * nvar
  plot(0, type = "n", xlim = limx, ylim = limy, frame.plot = FALSE, 
       axes = FALSE, ylab = "", xlab = "", main = main)
  new.max.len <- max(strwidth(var.labels, units = "user"))
  if (new.max.len > max.len) {
    limx <- c(-new.max.len/2, nvar + 2)
    plot(0, type = "n", xlim = limx, ylim = limy, frame.plot = FALSE, 
         axes = FALSE, ylab = "", xlab = "", main = main)
  }
  top <- num.var
  done <- 0
  rect.list <- list()
  arrow.list <- list()
  cluster.list <- list()
  if (nc == 1) {
    head <- num
    size <- num.var
    y.loc <- clusters[head, "size2"]
    v.loc <- psych:::down(clusters, head, size, y.loc, old.head = NULL, 
                  old.loc = NULL, min.size = min.size, e.size = e.size, 
                  digits = digits, cex = cex, limx = limx, limy = limy, 
                  colors = colors, cluster.names = cluster.names, rect.list = rect.list, 
                  arrow.list = arrow.list, cluster.list = cluster.list)
    rect.list <- c(rect.list$rect.list, v.loc$rect.list)
    cluster.list <- v.loc$cluster.list
    arrow.list <- v.loc$arrow.list
  }
  else {
    for (clust in 1:nc) {
      size <- sum(abs(ic$clusters[, clust]))
      if (substr(colnames(ic$clusters)[clust], 1, 1) == 
          "C") {
        head <- which(rownames(clusters) == colnames(ic$clusters)[clust])
        cluster <- clusters[head, ]
        y.loc <- clusters[head, "size2"] + done
        v.loc <- down(clusters, head, size, y.loc, old.head = NULL, 
                      old.loc = NULL, min.size = min.size, e.size = e.size, 
                      digits = digits, cex = cex.arrow.digits, limx = limx, limy = limy, 
                      colors = colors, cluster.names = cluster.names, 
                      rect.list = rect.list, arrow.list = arrow.list, 
                      cluster.list = cluster.list)
        rect.list <- v.loc$rect.list
        cluster.list <- v.loc$cluster.list
        arrow.list <- v.loc$arrow.list
      }
      else {
        v.name <- names(which(ic$clusters[, clust] == 
                                1))
        v.loc <- dia.rect(0, done + 0.5, v.name, xlim = limx, 
                          ylim = limy, cex = cex.arrow.digits, draw = FALSE)
        rect.list <- c(rect.list, v.loc, v.name)
      }
      done <- done + size
    }
  }
  rect.mat <- matrix(unlist(rect.list), ncol = 12, byrow = TRUE)
  rect.df <- as.data.frame(rect.mat, stringsAsFactors = FALSE)
  colnames(rect.df) <- c("left", "y", "right", "right.y", "topx", 
                         "topy", "xbott", "botty", "centerx", "centery", "radius", 
                         "lab")
  text(as.numeric(rect.df$centerx), 
       as.numeric(rect.df$centery), 
       rect.df$lab, 
       cex = cex.variables)
 # don't draw rectangles around variable names
 # rect(as.numeric(rect.df$left), as.numeric(rect.df$botty), 
 #      as.numeric(rect.df$right), as.numeric(rect.df$topy))
  cluster.mat <- matrix(unlist(cluster.list), ncol = 15, byrow = TRUE)
  cluster.df <- data.frame(cluster.mat, stringsAsFactors = FALSE)
  cluster.df[c(1:12, 14:15)] <- nchar2numeric(cluster.df[c(1:12, 
                                                           14:15)])
  colnames(cluster.df) <- c("left", "yl", "right", "yr", "topx", 
                            "topy", "xbott", "botty", "centerx", "centery", "link", 
                            "radius", "lab", "alpha", "beta")
  rownames(cluster.df) <- cluster.df$lab
  psych:::dia.cluster1(cluster.df, cex = cex.in.circle, e.size = e.size, digits = digits)
  arrow.mat <- matrix(unlist(arrow.list), ncol = 21, byrow = TRUE)
  arrow.df <- data.frame(arrow.mat, stringsAsFactors = FALSE)
  arrow.df[c(1:19, 21)] <- nchar2numeric(arrow.df[c(1:19, 21)])
  tv <- arrow.df
  text(tv[, 1], tv[, 2], tv[, 3], cex = cex.arrow.digits)
  arrows(x0 = tv[, 6], 
         y0 = tv[, 7], 
         x1 = tv[, 8], 
         y1 = tv[, 9], 
         length = tv[1, 10], 
         angle = tv[1, 11], 
         code = 1, 
         col = tv[, 20], 
         lty = tv[, 21])
  # these are the arrows to the variable labels
  arrows(x0 = tv[, 13], 
         y0 = tv[, 14], 
         x1 = tv[, 15], 
         y1 = tv[, 16], 
         length = tv[1, 17], 
         angle = tv[1, 18], 
         code = 2, 
         col = tv[, 20], 
         lty = tv[, 21])
}

