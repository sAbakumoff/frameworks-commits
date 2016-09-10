library(dplyr)
library(stringr)
library(tidytext)
library(wordcloud)
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

angular_commits_type_scope <- react_angular_commits %>% 
  filter(name=="angular.js", str_detect(message, "^(feat|fix|docs|style|refactor|test|chore)\\s*\\(")) %>%
  mutate(type=str_sub(message, start=0, end=regexpr( "\\(", message ) - 1), 
         scope=str_sub(message, start=regexpr( "\\(", message ) + 1, end=regexpr( "\\)", message ) - 1)) %>%
  select(type, scope) #%>%
  #count(type, scope, sort=TRUE) %>%
  #filter(n>=30)

angular_commits_type_scope %>% count(scope) %>% with(wordcloud(scope, n, max.words = 50))

top_scope<-angular_commits_type_scope %>% count(scope, sort=TRUE) %>% head(10)

tt <- angular_commits_type_scope %>% inner_join(top_scope)

ggplot(tt, aes(type, n, fill = scope)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Number of commits", fill = "")

bing <- sentiments %>% filter(lexicon == "bing") %>% select(-score)

angular_word_counts<-tidy_commits %>% 
  filter(name=="angular.js") %>% 
  inner_join(bing) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  ungroup()
angular_word_counts %>% filter(n > 50) %>%
       mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
       mutate(word = reorder(word, n)) %>%
       ggplot(aes(word, n, fill = sentiment)) +
       geom_bar(stat = "identity") +
       ylab("Contribution to sentiment") + 
       coord_flip()