## Read in the data
library(readr)
library(magrittr)
library(tidyverse)
source("oop_code.R")
## Load any other packages that you may need to execute your code

data = read_csv("MIE.csv")

##Function
make_LD=function(data){
    data_1=data %>% nest(-id)
    structure(data_1,class=c('LongitudinalData'))
}
print(class(make_LD(data)))

data_2=make_LD(data)

print.LongitudinalData=function(data_2){
    cat("Longitudinal dataset with", length(unique(data_2[['id']])), "subjects")
}
print(data_2)
## Subject 10 doesn't exist

## OTHER FUNCTION and METHODS
subject = function(data_2, id) UseMethod("subject")

visit = function(subject, visit_id) UseMethod("visit")

room = function(visit, room_id) UseMethod("room")

subject.LongitudinalData = function(data_2,id){
    if (id %in% c(data_2[['id']])){
        data_3=data_2[['data']][[which(data_2[['id']]==id)]]}
    else {
        return(NULL)
    }
     structure(list(id_3=id,data_4=data_3),class=c('SUBJECT_1'))
}
print.SUBJECT_1=function(object){
        cat('Subject ID:',object$id_3)
}

out = subject(data_2, 10)
print(out)

out = subject(data_2, 14)
print(out)

summary.SUBJECT_1=function(object_1){
    object_2=object_1$data_4 %>% 
        group_by(room,visit) %>% 
        summarise(MEAN=mean(value)) %>% 
        spread(room,MEAN) %>% 
        as.data.frame()
    structure(list(id=object_1$id_3,data_5=object_2),class=c('summary'))
}
print.summary=function(object_3){
    cat('ID:',object_3$id)
    object_3$data_5 %>% knitr::kable()
}

out = subject(data_2, 54) %>% summary
print(out)

out = subject(data_2, 14) %>% summary
print(out)

visit.SUBJECT_1=function(subject,visit_id){
    data_6 <- subject$data_4 %>% 
        filter(visit == visit_id) %>% 
        select(-visit)
    structure(list(id = subject$id_3,
                   visit_id = visit_id,
                   data_7 = data_6), class = c("VISIT_1"))
}
room.VISIT_1=function(visit,room_id){
        data_8 <- visit$data %>% 
            filter(room == room_id) %>% 
            select(-room)
        structure(list(id = visit$id,
                       visit = visit$visit_id,
                       room = room_id,
                       data_9 = data_8), class = c("ROOM_1"))
}
print.ROOM_1=function(object_4){
    cat("ID:", object_4$id, "\n")
    cat("Visit:", object_4$visit, "\n")
    cat("Room:", object_4$room)
}
## Show a summary of the pollutant values
out = subject(data_2, 44) %>% visit(0) %>% room("bedroom")
print(out)

summary.ROOM_1 <- function(object_5) {
    output <- summary(object_5[["data_9"]][["value"]])
    structure(list(id = object_5[["id"]],
                   output = output), class = "Summary")
}
print.Summary <- function(variable) {
    cat("ID:", variable[[1]], "\n")
    print(variable[[2]])
    invisible(variable)
}


out = subject(data_2, 44) %>% visit(1) %>% room("living room") %>% summary
print(out)
