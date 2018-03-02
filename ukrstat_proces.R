require(rvest)
require(stringr)
require(purrr)
require(tidyr)
require(dplyr)
require(magrittr)
require(lubridate)

r <- read_html("http://www.ukrstat.gov.ua/operativ/operativ2008/ds/kn/kn_u/kn0408_u.html") %>% html_nodes(".MsoNormalTable") %>% html_table(fill = T)

r03 <- read_html("http://www.ukrstat.gov.ua/operativ/operativ2003/ds/kn/kn_u/012003.html") %>% html_nodes("table") %>% html_table(fill = T)


r03_12 <- read_html("http://www.ukrstat.gov.ua/operativ/operativ2003/ds/kn/kn_u/112003.html") %>% html_nodes("table") %>% html_table(fill = T)
people_proc <- function(data) {
  if("Вінницька" %in% data[[1]]) names(data)[1] = "Області"
  
  data <- data[data$Області != "області", ]
  
  
  names(data)[2:ncol(data)] <- map_chr(2:ncol(data), function(x) paste0(data[[x]][1:(min(which(data[[1]] != "")-1))], collapse = ": ")) %>% str_remove_all("\\\r") %>% str_remove_all("\\\n") %>% str_remove_all("\\\t")
  data <- data[which(data[[1]] != ""), ]
  
  data %>% gather("Дата", "Значення", 2:ncol(data)) %>% mutate(
    Області = Області %>% str_remove_all("\\\r") %>% str_remove_all("\\\n") %>% str_remove_all("\\\t"),
    Значення = as.numeric(Значення)
  ) %>% as_data_frame() %>% mutate(date = Дата %>%  str_replace_all(" січ(ень|ня) ","-01-") %>% 
                                     str_replace_all(" лют(ий|ого) ","-02-") %>% 
                                     str_replace_all(" берез(ень|ня) ","-03-") %>% 
                                     str_replace_all(" квіт(ень|ня) ","-04-") %>% 
                                     str_replace_all(" трав(ень|ня) ","-05-") %>%
                                     str_replace_all(" черв(ень|ня) ","-06-") %>% 
                                     str_replace_all(" лип(ень|ня) ","-07-") %>% 
                                     str_replace_all(" серп(ень|ня) ","-08-") %>% 
                                     str_replace_all(" верес(ень|ня) ","-09-") %>% 
                                     str_replace_all(" жовт(ень|ня) ","-10-") %>% 
                                     str_replace_all(" листопад(|а) ","-11-") %>% 
                                     str_replace_all(" груд(ень|ня) ","-12-") %>% str_remove_all("[:letter:]") %>% dmy,
                                   Тип = case_when(str_detect(Дата, "(В|в)се населення") ~ "все",
                                                   str_detect(Дата, "сільське") ~ "село",
                                                   str_detect(Дата, "міське") ~ "місто"
                                   )) %>% filter(!str_detect(Дата, "Середня")) %>% select(Області, date, Тип, Значення) %>% rename(Дата = date)
}

people_proc(r[[1]])
