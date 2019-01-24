library(tidyverse)
setwd('/Users/aritrabiswas/Desktop/census2011')

# Number of state: 35
# Number of district : 640
# Number of sub-district: 
# Number of village: 751486


data <- data.frame()
meta_data <- data.frame()
TotalNumberOfHouseHold <- c()

for(index in seq_along(1:10)){
  
  url <- paste0("http://www.censusindia.gov.in/pca/SearchDetails.aspx?Id=",index)
  pop <- read_html(url)
  

  data <- bind_rows(data,
                     tryCatch(
                       pop %>% 
                         html_nodes(xpath = '//*[@id="gvPopulation"]') %>% 
                         html_table() %>% 
                         data.frame() %>% 
                         mutate(Population = as.character(Population),
                                Persons = as.character(Persons),
                                Males = as.character(Males),
                                Females = as.character(Females),
                                Index = index),
                       error=function(e) 
                         data.frame(
                           Population = as.character(index),
                           Persons = as.character(index),
                           Males = as.character(index),
                           Females = as.character(index),
                           Index = index
                         )
                     )
                     
              )

  meta_data <-  bind_rows(meta_data,
                          tryCatch(
                            pop %>% 
                              html_nodes(xpath = '//*[@id="grd1"]')%>% 
                              html_table() %>% 
                              data.frame() %>% 
                              mutate(
                                `State.Name` = as.character(`State.Name`),
                                `District.Name` = as.character(`District.Name`),
                                Name = as.character(Name),
                                Level = as.character(Level),
                                Index = index
                                
                              )
                            ,error=function(e) 
                              data.frame(
                                `State.Name` = as.character(index),
                                `District.Name` = as.character(index),
                                 Name = as.character(index),
                                 Level = as.character(index),
                                 Index = index
                              )
                          )

                    )

  TotalNumberOfHouseHold <- c(TotalNumberOfHouseHold,
                              pop %>% 
                                html_nodes(xpath = '//*[@id="Label1"]') %>% 
                                html_text() %>% 
                                as.character()
  )
  print(index)
}

write_csv(data,"Population_Data.csv")
write_csv(meta_data,"Population_Meta_Data.csv")
write_csv(data.frame(TotalNumberOfHouseHold),"TotalNumberOfHouseHold.csv")