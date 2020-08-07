#####READING AND SUMMARIZING DATA#####
library(tidyverse)
#####Respuesta 1#####
base_2 %>% select(`Parameter Name`,
                  `State Name`,
                  `Arithmetic Mean`) %>% 
    filter(`State Name`=='Wisconsin',
           `Parameter Name`=='Bromine PM2.5 LC') %>% 
    summarise(Media=mean(`Arithmetic Mean`,na.rm=TRUE)
#####Respuesta 2#####
base_2 %>% group_by(`Parameter Name`) %>% 
    summarise(max_mean=mean(`Arithmetic Mean`,na.rm=TRUE)) %>% 
    arrange(-mean)
#####Respuesta 3#####
base_2 %>% filter(`Parameter Name`=='Sulfate PM2.5 LC') %>% 
    group_by(`State Code`,`County Code`,`Site Num`) %>% 
    summarise(media=mean(`Arithmetic Mean`,na.rm=TRUE)) %>% 
#####Respuesta 4#####
arreglo_1=base_2 %>% filter(`State Name` %in% c('California','Arizona'),
                  `Parameter Name`=='EC PM2.5 LC TOR') %>%
    group_by(`State Name`) %>% 
    summarise(media=mean(`Arithmetic Mean`,na.rm=TRUE))
arreglo_1$media[2]-arreglo_1$media[1]
#####Respuesta 5#####
base_2 %>% filter(`Parameter Name`=='OC PM2.5 LC TOR'&
                        Longitude<=-100) %>% 
    summarise(mediana=median(`Arithmetic Mean`,na.rm = TRUE))
#####Respuesta 6#####
base_1 %>% filter(`Land Use`=='RESIDENTIAL' &
                     `Location Setting`=='SUBURBAN') %>%
    count()
#####Respuesta 7#####
base_2 %>% 
    filter(`State Name` %in%  filter(base_1,`Land Use`=='RESIDENTIAL'&
                       `Location Setting`=='SUBURBAN')$`State Name` &
        `Parameter Name`=='EC PM2.5 LC TOR'&
            Longitude>=-100) %>% 
    summarise(mediana=median(`Arithmetic Mean`,na.rm=TRUE))
#####Respuesta 8#####
base_2 %>% filter(`State Name` %in%
                      filter(base_1,`Land Use`=='COMMERCIAL')$`State Name`&
                      `Parameter Name`=="Sulfate PM2.5 LC") %>% 
    mutate(mes=format(`Date Local`,'%m')) %>% 
    group_by(mes) %>% 
    summarise(media=mean(`Arithmetic Mean`,na.rm = TRUE)) %>% 
        arrange(-media)
#####Respuesta 9#####
base_2 %>% 
    filter(`State Code`=='06',
                  `County Code`=='065',
                  `Site Num`=='8001',
                  `Parameter Name` %in% c('Sulfate PM2.5 LC','Total Nitrate PM2.5 LC')) %>% 
    group_by(`Date Local`,`Parameter Name`) %>% 
    summarise(media=mean(`Arithmetic Mean`,na.rm = TRUE)) %>% 
    group_by(`Date Local`) %>% 
    summarise(suma=sum(media)) %>% 
    filter(suma>10) %>% 
    count()
#####Respuesta 10#####
base_2 %>% 
    filter(`Parameter Name` %in% c('Sulfate PM2.5 LC','Total Nitrate PM2.5 LC')) %>%
    group_by(`Parameter Name`,`State Code`,`County Code`,`Site Num`,`Date Local`) %>% 
    summarise(media=mean(`Arithmetic Mean`)) %>% 
    spread(`Parameter Name`,media) %>% 
    group_by(`State Code`,`County Code`,`Site Num`) %>% 
    summarise(correlacion=cor(`Sulfate PM2.5 LC`,`Total Nitrate PM2.5 LC`)) %>% 
    arrange(-correlacion)


    
    
    
    
    

                  
                  
        
        
    

                  
                  
    
    
