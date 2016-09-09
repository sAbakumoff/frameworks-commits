library(dplyr)
library(stringr)
library(tidytext)
data("stop_words")

react_angular_commits<-read.csv("react_angular_commits.csv")  %>% 
  mutate(message=str_replace_all(message, "\n", ""), 
         message_len=str_length(message), 
         name=str_sub(repo_name, start=regexpr('/', repo_name) + 1))

boxplot(message_len ~ name, react_angular_commits, outline=FALSE, horizontal=TRUE, las=2, 
        col=c("#d20013", "#53d2fa"), main="Len of the commits messages", par(mar=c(5,6,4,6)))

tidy_commits <- react_angular_commits %>% 
  select(name, committer_date, message) %>% 
  unnest_tokens(word, message) %>% 
  anti_join(stop_words)

tidy_commits %>% filter(name=="angular.js") %>% count(word, sort = TRUE) %>%
       head(10) %>%
       mutate(word = reorder(word, n)) %>%
       ggplot(aes(word, n)) +
       geom_bar(stat = "identity", fill=I("#d20013")) +
       labs(y="Occurrences", title="Most Common Words in Angular Commit Messages") +
       coord_flip()

tidy_commits %>% filter(name=="react") %>% count(word, sort = TRUE) %>%
  head(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity", fill=I("#53d2fa")) +
  labs(y="Occurrences", title="Most Common Words in React Commit Messages") +
  coord_flip()