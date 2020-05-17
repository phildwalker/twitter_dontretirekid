library(rtweet)
library(tidyverse)
library(tidygraph)
library(ggraph)

drk <- search_tweets("no homo", n = 1000)

drk %>% 
  summarise(
    min = min(created_at, na.rm=TRUE),
    max = max(created_at, na.rm=TRUE)
  )


users <- users_data(drk) %>% 
  count(user_id, screen_name) %>% 
  mutate(FreqFlag = ifelse(n > 1, "MoreThanOnce", "OnlyOnce"))

ggplot(users, aes(n))+
  geom_bar()+
  theme_bw()

FreqIDs <- 
  users  %>% 
  filter(n > 1) %>% 
  pull(user_id)

## create from-to data frame representing retweet/mention/reply connections
drk_net_data <- network_data(drk, "retweet,mention,reply")

drk_tidy <- 
  drk_net_data %>% 
  as_tbl_graph()

drk_tidy <- drk_tidy %>%
  activate(nodes) %>%
  right_join(., users, by = c("name" = "user_id")) %>% #right join to remove ids that are no longer valid 
  mutate(
    importance = centrality_betweenness()
  ) 


drk_tidy

# drk_tidy %>%
#   activate(nodes) %>% 
#   filter(is.na(screen_name))

thm <- theme_minimal() +
  theme(
    # legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major = element_blank()
  ) 

theme_set(thm)

library(viridisLite)

drk_tidy %>%
  ggraph(layout = "kk") +
  geom_node_text(aes(label = screen_name, color=FreqFlag, size=n)) +
  geom_edge_diagonal(color = "gray30", alpha = 0.3, arrow = arrow(type = "closed", length = unit(.1, "cm"))) +
  labs(title = "Network graph of users interacting with tweets related to: no homo",
       subtitle = glue::glue("As run on {Sys.Date()}"))+
  scale_color_viridis_d(begin = 0, end = .5)










