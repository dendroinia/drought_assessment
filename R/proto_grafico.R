
library(data.table)
library(tidyverse)
source("R/drought_indicators.R")
d <- read_csv("data/datos_spei_sitio_seco.csv")
da12<- drought_indicators(d, vname = "SPEI12", -1.28)
da06 <- drought_indicators(d, vname = "SPEI6", -1.28)


custom_year <- 2001 

plot12 <- da12$drought_assessment |> 
  separate(rangeDate, into = c("start_month", "end_month"), remove = FALSE) |> 
  unite("start", c("minyear", "start_month"), remove = FALSE, sep = "-") |> 
  unite("end", c("maxyear", "end_month"), remove = FALSE, sep = "-") |> 
  mutate(plot_start = ifelse(minyear == maxyear,
                             paste(as.character(custom_year), start_month, "01", sep = "-"), 
                             paste(as.character(custom_year-1), start_month, "01", sep = "-"))) |> 
  mutate(plot_start = as.Date(plot_start,format = "%Y-%b-%d")) |> 
  mutate(plot_end = as.Date(paste(as.character(custom_year), end_month, "01", sep = "-"), format = "%Y-%b-%d")) |> 
  unite("date_range", c("start", "end"), sep = " / ") |> 
  ggplot(aes(y = date_range)) +
  geom_segment(aes(x = plot_start, xend = plot_end, yend = date_range, colour = d_intensity), 
               size = 9, lineend = "butt") +
  scale_x_date(date_labels = "%b", breaks = "1 month") +
  theme_bw() +
  xlab("") + ylab("Severe drought events") +
  theme(
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom"
  ) +
  scale_colour_gradient(low = "#662506", high = "#fee391") +
  ggtitle("SPEI12")




plot06 <- da06$drought_assessment |> 
separate(rangeDate, into = c("start_month", "end_month"), remove = FALSE) |> 
  unite("start", c("minyear", "start_month"), remove = FALSE, sep = "-") |> 
  unite("end", c("maxyear", "end_month"), remove = FALSE, sep = "-") |> 
  mutate(plot_start = ifelse(minyear == maxyear,
                             paste(as.character(custom_year), start_month, "01", sep = "-"), 
                             paste(as.character(custom_year-1), start_month, "01", sep = "-"))) |> 
  mutate(plot_start = as.Date(plot_start,format = "%Y-%b-%d")) |> 
  mutate(plot_end = as.Date(paste(as.character(custom_year), end_month, "01", sep = "-"), format = "%Y-%b-%d")) |> 
  unite("date_range", c("start", "end"), sep = " / ") |> 
  ggplot(aes(y = date_range)) +
  geom_segment(aes(x = plot_start, xend = plot_end, yend = date_range, colour = d_intensity), 
               size = 9, lineend = "butt") +
  scale_x_date(date_labels = "%b", breaks = "1 month") +
  theme_bw() +
  xlab("") + ylab("Severe drought events") + 
  theme(
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom"
  ) +
  scale_colour_gradient(low = "#662506", high = "#fee391") +
  ggtitle("SPEI06")
  
  

ggsave(
  plot06, 
  file = "data/grafico06_mariana.pdf"
)

ggsave(
  plot12, 
  file = "data/grafico12_mariana.pdf"
)

                              