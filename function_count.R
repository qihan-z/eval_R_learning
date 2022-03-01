library(tidyverse)
library(fs)



#Takes a file path and reads it as an output give a dataframe that has the file name, 
#list column with the name of the functions, 
#regular numeric functions with number of function

get_functions_w_path <- function(path){
  x = read_lines(path)
  function_list = x[x %>% str_detect("([a-z]|[A-Z])+[(]")] %>%
    #get all that satisfy function constraints, alphabetic character followed by an open parenthesis
    str_extract_all("([a-z]|[A-Z]|[.])+[(]+?", simplify = TRUE) %>%
    #make into vector to run unique
    as.vector() %>%
    #get all unique functions
    unique() %>%
    # remove the open parenthesis
    str_remove("[(]") %>% 
    #make into list
    list() %>%
    #get rid of all that have an empty string 
    lapply(function(x) x[nchar(x) >= 1]) %>%
    #save as character vector so each path has one character string of functions
    as.character()
  #find the amount of functions
  amount = length(str_split(function_list, ",")[[1]])
  #cutoff path up to the class
  update_path = substring(path, 67, nchar(path))
  #then just need to count the items in the list once I can get function_list to a list
  return(data.frame("path" = update_path, "functions" = function_list, "length" = amount))
}


#column with number of different functions
#give me all the functions from the list that aren't part of the tidyverse...to get from the tidyverse,

#see if the number of functions is different by syntax first


# work just with one class because this class only has R scripts 
sample_class <- as.vector(dir_ls(path = "~/Documents/Publications/tidylearn/student-projects/", recursive = TRUE))
#need to find all that don't go exactly to a file, i.e 2013-fall-2/
fixed_class<-sample_class[which(sample_class != "/Users/benfeder/Documents/Publications/tidylearn/student-projects/16-spring-2-01"& 
        sample_class != "/Users/benfeder/Documents/Publications/tidylearn/student-projects/15-spring-2"&
        sample_class != "/Users/benfeder/Documents/Publications/tidylearn/student-projects/13-fall-2"&
        sample_class != "/Users/benfeder/Documents/Publications/tidylearn/student-projects/14-fall-2"&
        sample_class != "/Users/benfeder/Documents/Publications/tidylearn/student-projects/15-fall-2-01"&
        sample_class != "/Users/benfeder/Documents/Publications/tidylearn/student-projects/15-fall-2-06"&
        sample_class != "/Users/benfeder/Documents/Publications/tidylearn/student-projects/16-spring-2-002"&
          #for now
        sample_class != "/Users/benfeder/Documents/Publications/tidylearn/student-projects/13-fall-2/SupaHotFireProject2.Rmd"  
          )]

#steps right now: purl all non R scripts
#re-run the code we already have
#use mine's code

total_df<-map_dfr(fixed_class, get_functions_w_path)

#now just need to break it down by tidyverse and base R
#assign to base r or tidyverse
total_df <- total_df %>%
  mutate(tidyverse = case_when(
    total_df$path %>% str_detect("14-fall|13-fall|15-spring") ~ "base R",
    TRUE ~ "tidyverse"
    )
  )

#break down by tidyverse
total_df %>%
  group_by(tidyverse) %>%
  summarize(means = mean(length), sd = sd(length))

#assign to class
total_df <- total_df %>%
  mutate(class = case_when(
    total_df$path %>% str_detect("13-fall") ~ "Fall 2013",
    total_df$path %>% str_detect("14-fall") ~ "Fall 2014",
    total_df$path %>% str_detect("15-fall-2-01") ~ "Fall 2015-01",
    total_df$path %>% str_detect("15-fall-2-06") ~ "Fall 2015-06",
    total_df$path %>% str_detect("15-spring") ~ "Spring 2015",
    total_df$path %>% str_detect("16-spring-2-002") ~ "Spring 2016-002",
    total_df$path %>% str_detect("16-spring-2-01") ~ "Spring 2016-01"
  )
)

#break down by class
total_df %>%
  group_by(class) %>%
  summarize(means = mean(length), sd = sd(length))


#Mine's example code...need to implement into function_count but first should test on some 
#projects
text <- "
library(ggplot2)
ggplot(df, aes()) + geom_point()
"

#test it on mjrj
expr <- parse(text = mjrj)
parsed <- getParseData(expr)
functions <- parsed$text[parsed$token == "SYMBOL_FUNCTION_CALL"]
filt_functions = unique(functions)


library(rlang)
pkg_lookup <- function(x){
  environmentName(environment(get(x)))
}
map_chr(filt_functions, pkg_lookup)

#see with total_df$functions[1] if we can use regex to separate to make it exactly like 
#filt_functions
filt_functions
total_df$functions[1]


Building up to this:

#an example of how to find all functions used for one project submission
mjrj <- read_lines("student-projects/13-fall-2/MJRJ_Project2.R")


#get all lines that contain at least one function inside mjr  
mjrj %>%
  str_extract_all("([a-z]|[A-Z]|[.])+[(]+?") %>% 
  unlist() %>% 
  unique() %>% 
  str_remove("\\(") %>% 
  list()

#here's the actual function with x being a file already used by read_lines
get_functions <- function(x){
  function_list <- x %>%
    str_extract_all("([a-z]|[A-Z]|[.])+[(]+?") %>% 
    unlist() %>% 
    unique() %>% 
    str_remove("\\(") %>% 
    list()
  return(function_list)
}



