library(dplyr)
library(stringr)

react_angular_commits<-read.csv("react_angular_commits.csv")  %>% 
  mutate(message=str_replace_all(message, "\n", ""), 
         message_len=str_length(message), 
         name=str_sub(repo_name, start=regexpr('/', repo_name) + 1))
boxplot(message_len ~ name, react_angular_commits, outline=FALSE, horizontal=TRUE, las=2, 
        col=c("#d20013", "#53d2fa"), main="Len of the commits messages", par(mar=c(5,6,4,6)))