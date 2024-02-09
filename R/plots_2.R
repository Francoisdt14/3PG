library(tidyverse)
library(patchwork)
library(RColorBrewer)


a <- data.frame(
    x = c("a", "b", "c", "d", "e"),
    y = c(10, 20, 30, 450, 500)
)

a <- fao_df %>%
    filter(Study_Area == 'M_18S') %>%
    dplyr::select(Study_Area, Scenario, total_biomass)  %>%
    mutate(total_biomass = round(total_biomass, 0))


lower <- a %>%
    ggplot(aes(x = Scenario, y = total_biomass)) +
    geom_bar(stat = "identity", aes(fill = Scenario),
             width = 0.7, color = "black") +
    geom_text(aes(label = total_biomass), vjust = -0.2) +
    scale_fill_manual(values = brewer.pal(8, "Set2")) +
    theme_classic() +
    theme(legend.position = "none",
          axis.title.y = element_text(hjust =1 )) +
    coord_cartesian(ylim = c(0, 10))

upper1 <- a %>%
    ggplot(aes(x = Scenario, y = total_biomass)) +
    geom_bar(stat = "identity", aes(fill = Scenario),
             width = 0.7, color = "black") +
    geom_text(aes(label = total_biomass), vjust = -0.2) +
    scale_fill_manual(values = brewer.pal(8, "Set2")) +
    labs(x = NULL,
         y = NULL) +
    theme_classic() +
    theme(legend.position = "none",
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) +
    coord_cartesian(ylim = c(1600000000, 1750000000))


broken1 <- wrap_plots(upper1, lower, nrow = 2) &
    theme(axis.text = element_text(color = "black"),
          text = element_text(size = 14))

broken1

#ggsave("../Results/Broken_axis.svg", height = 4, width = 6, bg = "white")
#ggsave("../Results/Broken_axis.png", height = 4, width = 6, bg = "white")


b <- filtered_df5 %>%
    filter(Scenario == 's3') %>%
    dplyr::select(Study_Area, total_biomass)
######################################################################################

# Plotting the information
combined_plot <- filtered_df5 %>%
    ggplot(aes(x = Scenario, y = total_biomass, fill = Study_Area)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    geom_text(aes(label = total_biomass), vjust = -0.2, position = position_dodge(width = 0.8), size = 3) +
    scale_fill_manual(values = brewer.pal(5, "Set2")) +
    theme_classic() +
    labs(title = "Total Biomass by Study Area and Scenario",
         x = "Scenario",
         y = "Total Biomass") +
    coord_cartesian(ylim = c(0, max(filtered_df5$total_biomass) * 1.1))

# Print the combined plot
print(combined_plot)

b <-filtered_df5 %>%
    mutate(total_biomass = round(total_biomass, 0))%>%
    dplyr::select(Study_Area, Scenario, total_biomass)


lower2 <- filtered_df5 %>%
    ggplot(aes(x = Scenario, y = total_biomass, fill = Study_Area)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    geom_text(aes(label = total_biomass), vjust = -0.2, position = position_dodge(width = 0.8), size = 3) +
    scale_fill_manual(values = brewer.pal(5, "Set2")) +
    theme_classic() +
    theme(legend.position = "none",
          axis.title.y = element_text(hjust =1 )) +
    coord_cartesian(ylim = c(0, 1000000))

upper2 <- filtered_df5 %>%
    ggplot(aes(x = Scenario, y = total_biomass, fill = Study_Area)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    geom_text(aes(label = total_biomass), vjust = -0.2, position = position_dodge(width = 0.8), size = 3) +
    scale_fill_manual(values = brewer.pal(5, "Set2")) +
    labs(x = NULL,
         y = NULL) +
    theme_classic() +
    theme(legend.position = "none",
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) +
    coord_cartesian(ylim = c(1200000, 1310000))


broken2 <- wrap_plots(upper1, lower, nrow = 2) &
    theme(axis.text = element_text(color = "black"),
          text = element_text(size = 14))

broken2




lower <- a %>%
    ggplot(aes(x = Scenario, y = total_biomass)) +
    geom_bar(stat = "identity", aes(fill = Scenario),
             width = 0.7, color = "black") +
    geom_text(aes(label = total_biomass), vjust = -0.2) +
    scale_fill_manual(values = brewer.pal(8, "Set2")) +
    theme_classic() +
    theme(legend.position = "none",
          axis.title.y = element_text(hjust =1 )) +
    coord_cartesian(ylim = c(1600000000, 1750000000))

