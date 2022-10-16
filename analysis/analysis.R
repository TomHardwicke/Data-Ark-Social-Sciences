library(tidyverse)
library(here)

red <- "#fdd2ce"
yellow <- "#f2d15a"
green <- "#b8e6c0"

d <- read_csv(here('data','data.csv')) 

# add higher level data status classification layer
d <- d %>%
  mutate(dataStatusHigh = case_when(
    dataStatus %in% c(10,1,8) ~ "Available, unrestricted",
    dataStatus %in% c(2,3,9) ~ "Available, restricted",
    dataStatus %in% c(0,4,5,6) ~ "Not available",
    TRUE ~ "CLASSIFICATION ERROR"
  ))

# recode data status numbers to text labels
d <- d %>%
  mutate(dataStatusLabel = case_when(
    dataStatus == 0 ~ "No response",
    dataStatus == 1 ~ "Data Ark, unrestricted",
    dataStatus == 2 ~ "Data Ark, restricted",
    dataStatus == 3 ~ "METRICS only",
    dataStatus == 4 ~ "Will not share",
    dataStatus == 5 ~ "Authors locating/preparing data",
    dataStatus == 6 ~ "Authors considering request",
    dataStatus == 7 ~ "Authors want more information",
    dataStatus == 8 ~ "Data sharing system already in place (unrestricted)",
    dataStatus == 9 ~ "Data sharing system already in place (restricted)",
    dataStatus == 10 ~ "Data sharing statement says data are available",
    TRUE ~ "CLASSIFICATION ERROR"
  ))

# add higher level publication year classification layer
d <- d %>%
  mutate(timeFrame = case_when(
    publicationDate %in% seq(2008, 2013) ~ "2008-2013",
    publicationDate %in% seq(2016, 2018) ~ "2016-2018",
    TRUE ~ "CLASSIFICATION ERROR"
  ))

# convert to factors and set order
d <- d %>%
  mutate(timeFrame = factor(timeFrame),
         dataStatusHigh = factor(dataStatusHigh, levels = c("Available, unrestricted","Available, restricted","Not available")),
         field = factor(field))

# regular bar
d %>% 
  count(field, dataStatusHigh) %>%
  ggplot(aes(dataStatusHigh, n)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  facet_wrap(~ field)

facets <- data_frame(facetNames = unique(d$dataStatusHigh),1)

d %>% 
  count(field, dataStatusHigh, timeFrame, .drop = F) %>%
  mutate(lineGroup = as.numeric((factor(paste0(field,'_',dataStatusHigh))))) %>%
  ggplot(aes(x = timeFrame, y = n, shape = field, group = field)) +
  geom_rect(aes(fill = dataStatusHigh),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 0.1) +
  geom_line(aes(group = lineGroup)) +
  geom_point(size = 5, alpha = 1, colour ='black', aes(fill = dataStatusHigh)) +
  facet_wrap(~ dataStatusHigh) +
  scale_fill_manual(values = c(green, yellow, red)) +
  scale_shape_manual(name = 'domain', values = c(21,24)) +
  xlab('time period') +
  guides(fill = 'none') +
  theme_minimal(base_size = 18) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


d %>% 
  count(dataStatusHigh, .drop = F)

d %>% 
  count(dataStatusLabel, .drop = F)

d %>%
  filter(dataStatusLabel == "Data sharing system already in place (restricted)") %>%
  count(requirements)

d %>%
  count(reasons) %>%
  filter(reasons != 'NA') 

# citations

d %>%
  group_by(field, timeFrame) %>%
  summarise(median = median(citations),
            IQR_low = quantile(citations, .25),
            IQR_high = quantile(citations, .75))

