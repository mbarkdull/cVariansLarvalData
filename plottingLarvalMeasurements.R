library(plyr)
library(tidyverse)
library(broom)
library(cowplot)
library(scales)
library(ggpubr)
library(rlang)
library(gganimate)
library(ggrepel)
library(MetBrewer)
library(gifski)
library(ggExtra)

# Read in larval measurements:
keyenceMeasurements <- read_csv(file = "keyenceData.csv") %>%
  separate(col = color,
           into = c("rValue",
                    "gValue",
                    "bValue"),
    sep = ",",
    remove = TRUE)

keyenceMeasurements$rValue <- gsub(".*:",
                                   "",
                                   keyenceMeasurements$rValue)
keyenceMeasurements$gValue <- gsub(".*:",
                                   "",
                                   keyenceMeasurements$gValue)
keyenceMeasurements$bValue <- gsub(".*:",
                                   "",
                                   keyenceMeasurements$bValue)
possiblyRGB <- purrr::possibly(rgb,
                               otherwise = "NA")
keyenceMeasurements$hexColor <- purrr::pmap(.l = list(keyenceMeasurements$rValue,
                                                   keyenceMeasurements$gValue,
                                                   keyenceMeasurements$bValue),
                                            possiblyRGB,
                                           maxColorValue = 255)

# Make colony a categorical variable:
keyenceMeasurements$colony <- as.character(keyenceMeasurements$colony)

# Get means for key life stages:
soldiers <- filter(keyenceMeasurements, collectingTrip == "Aug-23", lifeStage == "soldierPupa")
soldiersMean <- mean(soldiers$body)
eggs <- filter(keyenceMeasurements, collectingTrip == "Aug-23", lifeStage == "egg")
eggsMean <- mean(eggs$body)
workers <- filter(keyenceMeasurements, collectingTrip == "Aug-23", lifeStage == "workerPupa")
workersMean <- mean(workers$body)
queen <- filter(keyenceMeasurements, collectingTrip == "Aug-23", lifeStage == "queenPupa")
queenMean <- mean(queen$body)
male <- filter(keyenceMeasurements, collectingTrip == "Aug-23", lifeStage == "malePupa")
maleMean <- mean(male$body)

prepupae <- filter(keyenceMeasurements, collectingTrip == "Aug-23", lifeStage == "prepupa")
prepupaeMean <- mean(prepupae$head)

# Plot to see Dyer's rule:
plot <- ggplot(data = filter(keyenceMeasurements,
                            collectingTrip == "Aug-23"),
              mapping = aes(y = body, 
                            x = head,
                            shape = lifeStage,
                            colour = mouthpartMelanization)) +
  geom_point(size = 3,
             alpha = 0.75) +
  coord_trans(x="log10", 
              y="log10") + 
  geom_hline(yintercept = workersMean) +
  geom_hline(yintercept = eggsMean) +
  geom_hline(yintercept = soldiersMean) +
  geom_vline(xintercept = prepupaeMean) +
  ggtitle("Head width to body length") +
  xlab("Head width (log10, μm)") + 
  ylab("Body length (log10, μm)") + 
  theme_bw() +
  geom_text(mapping = aes(label = filename), 
            check_overlap = TRUE, 
            vjust = 1.5,
            size = 2.5)

plot

ggMarginal(plot, 
           type = "histogram",
           bins = 100)

# Plot mandible measurements:
ggplot(data = filter(keyenceMeasurements,
                     collectingTrip == "Aug-23"),
       mapping = aes(y = body, 
                     x = fullMouthparts,
                     shape = lifeStage,
                     colour = hexColor)) +
  geom_point(size = 3,
             alpha = 0.75) +
  coord_trans(x="log10", 
              y="log10") + 
  geom_hline(yintercept = workersMean) +
  geom_hline(yintercept = eggsMean) +
  geom_hline(yintercept = soldiersMean) +
  geom_vline(xintercept = prepupaeMean) +
  ggtitle("Head width to body length") +
  xlab("Mandible width (log10, μm)") + 
  ylab("Body length (log10, μm)") + 
  theme_bw() +
  geom_text(mapping = aes(label = filename), 
            check_overlap = TRUE, 
            vjust = 1.5,
            size = 2.5)

# Plot mandible measurements:
topMandible <- ggplot(data = filter(keyenceMeasurements,
                                    collectingTrip == "Aug-23"),
                      mapping = aes(y = body, 
                                    x = topOfMandibles,
                                    shape = lifeStage,
                                    colour = hexColor)) +
  geom_point(size = 3,
             alpha = 0.75) +
  coord_trans(x="log10", 
              y="log10") + 
  geom_hline(yintercept = workersMean) +
  geom_hline(yintercept = eggsMean) +
  geom_hline(yintercept = soldiersMean) +
  geom_hline(yintercept = queenMean) +
  geom_hline(yintercept = maleMean) +
  geom_vline(xintercept = prepupaeMean) +
  ggtitle("Head width to body length") +
  xlab("Mandible width (log10, μm)") + 
  ylab("Body length (log10, μm)") + 
  theme_bw() +
  geom_text(mapping = aes(label = filename), 
            check_overlap = TRUE, 
            vjust = 1.5,
            size = 2.5) 
#+ theme(panel.background = element_rect(fill = "#2A2930"))

topMandible
ggMarginal(topMandible, 
           type = "histogram",
           bins = 100)

# Try a density plot
ggplot(data = filter(keyenceMeasurements,
                     collectingTrip == "Aug-23"),
       aes(x = fullMouthparts,
           y = body)) +
  stat_density_2d(aes(fill = ..level..), 
                  geom = "polygon", 
                  colour="white") + 
  geom_hline(yintercept = workersMean) +
  geom_hline(yintercept = eggsMean) +
  geom_hline(yintercept = soldiersMean) +
  geom_hline(yintercept = queenMean) +
  geom_hline(yintercept = maleMean) +
  geom_vline(xintercept = prepupaeMean)

# Plot with larvae of concern:
ggplot(data = filter(keyenceMeasurements),
       mapping = aes(y = body, 
                     x = head,
                     shape = lifeStage,
                     colour = concerns)) +
  geom_point(size = 3) +
  coord_trans(x="log10", 
              y="log10") + 
  ggtitle("Head width to body length") +
  xlab("Head width (log10, μm)") + 
  ylab("Body length (log10, μm)") + 
  theme_bw()




# Read in larval measurements:
larvalMeasurements2022 <- read_csv(file = "larvalMeasurements.csv")
larvalMeasurements2022$year <- "2022"
larvalMeasurements2023 <- read_csv(file = "larvalMeasurements2023.csv")
larvalMeasurements2023$year <- "2023"
larvalMeasurements <- rbind(larvalMeasurements2022,
                            larvalMeasurements2023)

# Pivot them wider:
larvalMeasurementsWider <- select(larvalMeasurements, 
                                  c(measurement, 
                                    Length, 
                                    specimen, 
                                    observations, 
                                    pigmentation, 
                                    lifeStage,
                                    year)) %>%
  pivot_wider(names_from = c(measurement),
              values_from = c(Length))

groupedData <- plyr::ddply(larvalMeasurementsWider,
                           .(observations),
                           summarise, 
                           bodySD = sd(body),
                           headSD = sd(head),
                           body = mean(body),
                           head = mean(head))

# Plot to see Dyer's rule:
ggplot(data = larvalMeasurementsWider,
       mapping = aes(y = body, 
                     x = head,
                     colour = lifeStage,
                     shape = year)) +
  geom_point(size = 3) +
  coord_trans(x="log10", 
              y="log10") + 
  ggtitle("Head width to body length") +
  xlab("Head width (log10, mm)") + 
  ylab("Body length (log10, mm)") + 
  theme_bw()

# Plot measurements
plot <- ggplot(data = larvalMeasurementsWider,
               mapping = aes(y = body, 
                             x = head,
                             colour = observations)) +
  geom_point(size = 3) +
  #stat_ellipse() +
  geom_errorbarh(data = groupedData,
                 aes(xmin = body - bodySD,
                     xmax = body + bodySD,
                     y = head,
                     colour = observations,
                     height = 0.01)) + 
  geom_errorbar(data = groupedData,
                aes(ymin = head - headSD,
                    ymax = head + headSD,
                    x = body,
                    colour = observations)) +
  #ggrepel::geom_label_repel(mapping = aes(x = body,
                                          #y = head,
                                          #label = specimen,
                                          #color = observations),
                            #size = 2) +
  scale_color_manual(values = met.brewer("Redon", 8)) + 
  coord_trans(x="log10", 
              y="log10") + 
  ggtitle("Head width to body length") +
  xlab("Head width (log10, mm)") + 
  ylab("Body length (log10, mm)") + 
  theme_bw()
  
plot
ggsave(filename = "allLarvalMeasurementsScatter.png", 
       width = 7, 
       height = 5)

# Plot head widths as a bar plot, following method of Solis et al. 2010:
ggplot(data = larvalMeasurementsWider) +
  geom_histogram(mapping = aes(x = head,
                               fill = observations),
                 binwidth = 0.008)

# Plot body lengths as a bar plot:
ggplot(data = larvalMeasurementsWider) +
  geom_histogram(mapping = aes(x = body),
                 binwidth = 0.1)

# Run a PCA:
pca_fit <- larvalMeasurementsWider %>% 
  select(where(is.numeric)) %>% 
  prcomp(scale = TRUE) 
# Plot the PCA:
PCAplot <- pca_fit %>%
  broom::augment(larvalMeasurementsWider) %>% # add original dataset back in
  ggplot(aes(x = .fittedPC1, 
             y = .fittedPC2, 
             color = observations)) + 
  geom_point(size = 5) +
  stat_ellipse() +
  theme_half_open(12) + 
  scale_color_manual(values = met.brewer("Redon", 8)) + 
  background_grid()

PCAplot


############## Longitudinal measurements

# Read in larval measurements:
longitudinalMeasurements <- read_csv(file = "longitudinalMeasurements.csv") %>%
  drop_na(specimen)

# Pivot them wider:
longitudinalMeasurementsWider <- select(longitudinalMeasurements, 
                                  c(measurement, Length, specimen, observations, date)) %>%
  pivot_wider(names_from = c(measurement),
              values_from = c(Length))

longitudinalMeasurementsWider$date <- as.Date(longitudinalMeasurementsWider$date,
                                              format = "%m/%d/%y")

longitudinalSD <- plyr::ddply(longitudinalMeasurementsWider,
                           .(observations, date),
                           summarise, 
                           bodySD = sd(body),
                           headSD = sd(head),
                           bodyMean = mean(body),
                           headMean = mean(head))
longitudinalSD[is.na(longitudinalSD)] <- 0

longitudinalPlot <- ggplot(data = longitudinalMeasurementsWider) +
  geom_point(mapping = aes(x = body, 
                           y = head,
                           colour = observations,
                           group = date),
             size = 3) +
  geom_errorbarh(data = longitudinalSD,
                 aes(xmin = bodyMean - bodySD,
                     xmax = bodyMean + bodySD,
                     y = headMean,
                     colour = observations,
                     height = 0.01)) + 
  geom_errorbar(data = longitudinalSD,
                aes(ymin = headMean - headSD,
                    ymax = headMean + headSD,
                    x = bodyMean,
                    colour = observations)) +
  #ggrepel::geom_label_repel(mapping = aes(x = body,
  #y = head,
  #label = specimen,
  #color = observations),
  #size = 2) +
  scale_color_manual(values=met.brewer("Redon", 2)) + 
  coord_trans(x="log10", 
              y="log10") + 
  xlab("Body length (log10, mm)") + 
  ylab("Head width (log10, mm)") + 
  transition_states(date,
                    transition_length = 1, 
                    state_length = 1, 
                    wrap = FALSE) + 
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out') +
  labs(title = 'Head width to body length: {closest_state}') + 
  theme_bw()

longitudinalPlot

anim_save("longitudinalPlot.gif", longitudinalPlot)

ggplot(data = longitudinalSD) +
  geom_point(mapping = aes(x = date, 
                           y = (headMean/bodyMean),
                           color = as.character(observations)),
             size = 3) +
  geom_line(mapping = aes(x = date, 
                           y = (headMean/bodyMean),
                           color = as.character(observations))) +
  scale_color_manual(values=met.brewer("Archambault", 2))


wideLongitudinalSD <- longitudinalSD %>% pivot_longer(cols = c(bodyMean, headMean),
                                        names_to = "Measurement")
wideLongitudinalSD <- tidyr::unite(wideLongitudinalSD, 
                                   "observationMeasurement", 
                                   observations, 
                                   Measurement,
                                   remove = F)

ggplot(data = wideLongitudinalSD) +
  geom_point(mapping = aes(x = date, 
                           y = value,
                           color = observationMeasurement),
             size = 3) +
  geom_line(mapping = aes(x = date, 
                           y = value,
                           color = observationMeasurement)) +
  scale_color_manual(values=met.brewer("Archambault", 4)) +
  theme_bw()
