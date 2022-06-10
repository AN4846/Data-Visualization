# Check if packages necessary to run code are installed. If not, automatically install them.
packages <- c("friends", "tidyverse", "extrafont", "ggh4x", "rvest", "scales", "statebins", "devtools")
install.packages(setdiff(packages, rownames(installed.packages())))
if(!'ggthemr' %in% installed.packages()){
  devtools::install_github('Mikata-Project/ggthemr')


library(friends)
library(tidyverse)
library(extrafont)
library(ggh4x)
library(rvest)
library(scales)
library(statebins)
library(ggthemr)


# ----------------- Dataset tables --------------------------------------
# The following dataframes from the friends library were used:
# 1) friends_info: contains general information about the show per season and episode
# 2) friends: contains character transcript information
# 3) friends_emotions: contains emotional related data for each character until season 4.


# ----------------- Section 1 -------------------------------------------
# ----------------- Visualization 1: Viewership Plot --------------------

viewers_df <- friends_info %>%
  arrange(season, episode) %>%
  mutate(panel_title = str_c('Season ', season))

seasonal_avg <- viewers_df %>%
  group_by(panel_title) %>%
  summarize('Season Average' = mean(us_views_millions))

viewers_df <- viewers_df %>%
  inner_join(seasonal_avg, by = 'panel_title')

facet_text <- data.frame(
  label = c("'The One After the \nSuperbowl' (Parts 1 and 2)", "'The One with\nthe Vows'","'The Last One'\n(Parts 1 and 2)"),
  panel_title = c("Season 2", "Season 7", "Season 10"),
  x = c(13, 19, 15.5),
  y = c(54.9*10^6, 14*10^6, 54.5*10^6)
)
facet_text2 <- data.frame(
  label = c("Season\nAVG"),
  panel_title = c("Season 1"),
  x = c(21.5),
  y = c(23.5*10^6)
)

viewership_plot <- ggplot(viewers_df, aes(x = episode, y = us_views_millions * 10^6)) +
  geom_line(color = "#8C182D") + 
  geom_point(shape = 21, fill = "white",color = "#8C182D", size = 2.3) +
  geom_point(data=viewers_df[viewers_df$us_views_millions == min(viewers_df$us_views_millions),],
             pch=21, fill=NA, size=4, colour="#0D0D0D", stroke=1) +
  geom_point(data=viewers_df[viewers_df$us_views_millions == max(viewers_df$us_views_millions),][2,],
             mapping = aes(x = 12.5, y = 52.9*10^6),
             pch=21, fill=NA, size=5, colour="#0D0D0D", stroke=1) +
  geom_point(data=viewers_df[viewers_df$title == 'The Last One',][2,],
             mapping = aes(x = 17.5, y = 52.5*10^6),
             pch=21, fill=NA, size=5, colour="#0D0D0D", stroke=1) +
  geom_segment(aes(x=-Inf,xend=Inf,y=`Season Average`*10^6, yend = `Season Average`*10^6), 
               linetype="dashed", color = "#146229") +
  ggtitle("US Viewership of Friends Sitcom") +
  geom_text(
    data    = facet_text,
    mapping = aes(x = x, y = y, label = label),
    size = 3,
    color = '#0D0D0D',
    family = 'Open Sans'
  ) +
  geom_text(
    data    = facet_text2,
    mapping = aes(x = x, y = y, label = label),
    size = 3,
    color = '#146229',
    family = 'Open Sans'
  ) +
  scale_y_continuous(limits = c(10*10^6, max(viewers_df$us_views_millions)*10^6 + 4*10^6),
                     labels = unit_format(unit = "M", scale = 1e-6), expand = c(0,0),
                     breaks = c(10*10^6, 20*10^6, 30*10^6, 40*10^6, 50*10^6)) +
  facet_grid(~factor(panel_title, levels = unique(viewers_df$panel_title))) + 
  theme(text = element_text(family = "Open Sans"),
        panel.grid.major = element_blank(),
        plot.title = element_text(face = "bold", family = 'Open Sans', size = 20),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(),
        panel.background = element_rect(fill="#f0f0f0"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.spacing.x = unit(0.5, "lines"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(color = '#5E5E5E', family = 'Open Sans'),
        legend.position = "none",
        axis.text.x = element_blank(),
        plot.title.position = "plot",
        strip.text = element_text(color = "White", face = "bold", size = 12, family = 'Open Sans'),
        strip.background = element_rect(fill = '#8C182D')) +
  force_panelsizes(cols = unit(1.5, "in"))

# ggsave('viewership2.svg', viewership_plot, width = 16.4, height = 6.81) # save to svg and continue work in inkscape


# Get awards statistics from wikipedia. Friends package loaded above does not include any information
# regarding this.
url <- "https://en.wikipedia.org/wiki/List_of_awards_and_nominations_received_by_Friends"

awards_nomination <- read_html(url) %>%
  html_node('table.wikitable') %>% 
  html_table() %>%
  mutate(Result = case_when(str_detect(Result, 'Won') ~ 'Win',
                            !str_detect(Result, 'Won') ~ 'Nomination')) %>%
  group_by(`Year[b]`, Result) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  spread(Result, n, fill = 0) %>%
  mutate(Nomination = Nomination + Win) %>%
  gather(Result, Count, -1) %>%
  filter(`Year[b]` %in% seq(1995,2004)) %>%
  mutate(Season = case_when(`Year[b]` == 1995 ~ 'Season 1',
                            `Year[b]` == 1996 ~ 'Season 2',
                            `Year[b]` == 1997 ~ 'Season 3',
                            `Year[b]` == 1998 ~ 'Season 4',
                            `Year[b]` == 1999 ~ 'Season 5',
                            `Year[b]` == 2000 ~ 'Season 6',
                            `Year[b]` == 2001 ~ 'Season 7',
                            `Year[b]` == 2002 ~ 'Season 8',
                            `Year[b]` == 2003 ~ 'Season 9',
                            `Year[b]` == 2004 ~ 'Season 10'))

names(awards_nomination) <- c('Year', 'Result', 'Count', 'Season')

count_awards <- awards_nomination %>%
  group_by(Result) %>%
  summarize(n = sum(Count))

awards_by_season_plot <- ggplot(awards_nomination, aes(x = Result, y = Count, fill = Result)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = Count, color = Result), vjust = 0) +
  facet_grid(~factor(Season, levels = unique(awards_nomination$Season))) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,40)) +
  scale_fill_manual(values = c('#ff7f2a', '#b3b3b3')) +
  scale_color_manual(values = c('#ff7f2a', '#b3b3b3')) +
  theme(text = element_text(family = "Open Sans"),
        panel.grid.major = element_blank(),
        plot.title = element_text(face = "bold", family = 'Open Sans', size = 20),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(fill="#f0f0f0"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.spacing.x = unit(0.5, "lines"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        legend.text = element_text(family = "Open Sans", size = 15),
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x = element_line(size = 1.1),
        plot.title.position = "plot",
        strip.text = element_blank(),
        strip.background = element_blank()) +
  force_panelsizes(cols = unit(1.5, "in"),
                   row = unit(1.5, "in"))


# ggsave('viewership3.svg', awards_by_season_plot, width = 16.4, height = 6.81) # save to svg and continue work in inkscape


# ----------------- Section 2 ----------------------------------------------
# ----------------- Visualization 2: Sentiment Analysis --------------------

overall_ep_nu <- friends_info %>%
  filter(season <= 4) %>%
  mutate(Episode = row_number()) %>%
  select(season,episode,Episode)

viewers_df <- viewers_df %>%
  mutate(Episode = row_number()) 

sentiment_score <- friends %>%
  filter(speaker %in% c("Monica Geller", "Joey Tribbiani", "Chandler Bing","Phoebe Buffay",
                        "Ross Geller", "Rachel Green")) %>%
  inner_join(friends_emotions, by = c("season","episode","scene","utterance")) %>%
  select(speaker, season, episode, emotion) %>%
  mutate(sentiment = case_when(emotion %in% c("Mad", "Scared", "Sad") ~ "Negative",
                               emotion %in% c("Joyful", "Powerful", "Peaceful") ~ "Positive")) %>%
  filter(emotion != "Neutral") %>%
  group_by(speaker, season, episode, sentiment) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = Positive - Negative) %>%
  select(speaker, season, episode, sentiment) %>%
  mutate(Category = ifelse(sentiment < 0, "Negative", "Positive")) %>%
  filter(sentiment != 0) %>%
  inner_join(overall_ep_nu, by = c("season", "episode"))

arrange_mean_sent <- sentiment_score %>% # Get average sentiment for each character
  group_by(speaker) %>%
  summarize(avg_sentiment = mean(sentiment)) %>%
  arrange(desc(avg_sentiment))

arrange_mean_sent_by_season <- sentiment_score %>% # Get average seasonal sentiment for each character
  group_by(speaker,season) %>%
  summarize(avg_sentiment = mean(sentiment)) %>%
  arrange(desc(avg_sentiment))

arrange_count_sent <- sentiment_score %>% # Get count of positive episodes for each character
  filter(Category == "Positive") %>%
  group_by(speaker) %>%
  summarize(count_sentiment = n()) %>%
  arrange(desc(count_sentiment))

min_sentiment_by_char <- sentiment_score %>% # Get episodes with lowest sentiment for each character
  group_by(speaker) %>%
  summarize(sentiment = min(sentiment)) %>%
  inner_join(sentiment_score, by = c("speaker","sentiment"))

max_sentiment_by_char <- sentiment_score %>% # Get episodes with highest sentiment for each character
  group_by(speaker) %>%
  summarize(sentiment = max(sentiment)) %>%
  inner_join(sentiment_score, by = c("speaker","sentiment"))

try_rects <- sentiment_score %>% # filler code to add season number rectangles to facets
  group_by(season) %>%
  summarize(range = list(c(min(Episode), max(Episode)))) %>%
  unnest_wider(range, names_sep = c("min", "max"))

dust_theme <- ggthemr('dust', type = "outer", set_theme = F)

sentiment_plot <- ggplot(sentiment_score, aes(Episode, sentiment, fill = Category)) +
  geom_segment(aes(x = Episode, xend = Episode, y = 0, yend = sentiment, color = Category),
               arrow = arrow(length = unit(0.05, "in")))  +
  statebins:::geom_rrect(data = try_rects, aes(xmin=rangemin1, xmax=rangemax2, ymin=21.5,
                                               ymax=Inf), 
                         alpha=0.1,inherit.aes = F, size = 0.2) +
  geom_text(data = try_rects, aes(x = (rangemin1 + rangemax2)/2, y = 24, 
                                  label =str_c('Season ',season)), inherit.aes = F) +
  dust_theme$theme +
  scale_color_manual(values = c("#e84646", "#2179d2ff")) +
  scale_x_continuous("Episode", expand = c(0,0),limits = c(0,
                                                           max(sentiment_score$Episode) + 2)) +
  scale_y_continuous("Net Sentiment") +
  facet_wrap(~factor(speaker, levels = arrange_mean_sent$speaker), 
             ncol = 3, scales = "free_x") +
  theme(text = element_text(family = "Open Sans"),
        panel.spacing.x = unit(4, "lines"),
        panel.spacing.y = unit(7, "lines"),
        legend.position = "none",
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) 

# ggsave("sentiment333_mod.svg", sentiment_plot, width = 17.7, height = 8.92) # save to svg and continue work in inkscape