##===========This function is for custom breaks for minor ticks

every_nth <- function(x, nth, empty = TRUE, inverse = FALSE) {
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if(empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##===========Single Habitat Length Frequency Tables

# This function produces a Brown trout LF with X Y labels, and habitat title
Brown_trout_single <- function (habitat, label.name) {       # set up function for model

  binwidth = 10                  # set the bin width of the length frequency
  
  fish.stats <- habitat %>% 
    filter(Species=='Salmo trutta') %>%
    mutate(bin = cut(Length_mm, breaks=seq(0,450, binwidth), 
                     labels= seq(0+binwidth,450, binwidth)-(binwidth/2)),
           n = n()) %>%
    group_by(Species, bin) %>%
    summarise(p = n()/n[1]) %>%
    ungroup() %>%
    mutate(bin = as.numeric(as.character(bin)))
  
  n.count <- nrow(filter(habitat, Species=='Salmo trutta'))

  x_breaks <- seq(45,435,10)
  binlibs <- seq(40,430,10)
  x_labls <- paste(binlibs, '-', binlibs+9)
  
  y_breaks <- seq(0, .275, 0.025)
  y_labls <- paste(y_breaks*100)
  
  ggplot(fish.stats, aes(x = bin, y = p)) +  
    LF_single +
    geom_col(position='dodge',fill='darkred') + 
    xlab('Length [mm]') +   # x-axis label
    ylab('% catch') +    # y-axis label
    scale_x_continuous(breaks = x_breaks,      # sets scale limits and breaks
                       labels = every_nth(x_labls, 2, inverse = TRUE)) + 
    scale_y_continuous(breaks = y_breaks, limits = c(0,.275),  # sets scale limits and breaks
                       labels = every_nth(y_labls, 2, inverse = TRUE)) +
    annotate('text',size=3, x = 396, y = .2475, label = paste('n =', n.count)) +
    ggsave('images/brown_single.pdf', width = 4.5, height = 3) 
}

# This function produces a Rainbow trout LF with X Y labels, and habitat title
Rainbow_trout_single <- function (habitat, label.name) {       # set up function for model

  binwidth = 10                  # set the bin width of the length frequency
  
  fish.stats <- habitat %>% 
    filter(Species=='Onchorhynchus mykiss') %>%
    mutate(bin = cut(Length_mm, breaks=seq(0,400, binwidth), 
                     labels= seq(0+binwidth,400, binwidth)-(binwidth/2)),
           n = n()) %>%
    group_by(Species, bin) %>%
    summarise(p = n()/n[1]) %>%
    ungroup() %>%
    mutate(bin = as.numeric(as.character(bin)))
  
  n.count <- nrow(filter(habitat, Species=='Onchorhynchus mykiss'))

  x_breaks <- seq(55,405,10)
  binlibs <- seq(50,400,10)
  x_labls <- paste(binlibs, '-', binlibs+9)
  
  y_breaks <- seq(0, .2, 0.025)
  y_labls <- paste(y_breaks*100)

  ggplot(fish.stats, aes(x = bin, y = p)) +  
    LF_single +
    geom_col(position='dodge',fill='steelblue') + 
    xlab('Length [mm]') +   # x-axis label
    ylab('% catch') +    # y-axis label

    scale_x_continuous(breaks = x_breaks,      # sets scale limits and breaks
                       labels = every_nth(x_labls, 2, inverse = TRUE)) + 
    scale_y_continuous(breaks = y_breaks, limits = c(0,.20),  # sets scale limits and breaks
                       labels = every_nth(y_labls, 2, inverse = TRUE)) +
    annotate('text',size=3, x = 365, y = .18, label = paste('n =', n.count)) +
    ggsave('images/rain_single.pdf', width = 4.5, height = 3) 
}

##===========River Section Comparison Length Frequency Tables

# This function produces a Brown trout LF with X Y labels,
# Ois River title, Divided by section
Brown_trout_section <- function (habitat, label.name) {       # set up function for model

  binwidth = 10                  # set the bin width of the length frequency
  
  fish.stats <- habitat %>% 
    filter(Species=='Salmo trutta') %>%
    group_by(Section) %>%
    mutate(bin = cut(Length_mm, breaks=seq(0,450, binwidth), 
                     labels= seq(0+binwidth,450, binwidth)-(binwidth/2)),
           n = n()) %>%
    group_by(Section, bin) %>%
    summarise(p = n()/n[1]) %>%
    ungroup() %>%
    mutate(bin = as.numeric(as.character(bin)))
  
  x_breaks <- seq(45,435,10)
  binlibs <- seq(40,430,10)
  x_labls <- paste(binlibs, '-', binlibs+9)
  
  y_breaks <- seq(0, .3, 0.05)
  y_labls <- paste(y_breaks*100)
  
  ggplot(fish.stats, aes(x = bin, y = p, fill = Section)) +
    LF_section +
    facet_rep_grid(~Section) +
    geom_col(position='dodge',fill='darkred') + 
    xlab('Length [mm]') +   # x-axis label
    ylab('% catch') +    # y-axis label
    scale_x_continuous(breaks = x_breaks,      # sets scale limits and breaks
                       labels = every_nth(x_labls, 3, inverse = TRUE)) + 
    scale_y_continuous(breaks = y_breaks, limits = c(0,.30),  # sets scale limits and breaks
                       labels = every_nth(y_labls, 1, inverse = TRUE)) +
    ggsave('images/brown_section.pdf', width = 5, height = 2.75) 
}

# This function produces a Rainbow trout LF with X Y labels,
# Ois River title, Divided by section
Rainbow_trout_section <- function (habitat, label.name) {       # set up function for model

  binwidth = 10                  # set the bin width of the length frequency
  
  fish.stats <- habitat %>% 
    filter(Species=='Onchorhynchus mykiss') %>%
    group_by(Section) %>%
    mutate(bin = cut(Length_mm, breaks=seq(0,400, binwidth), 
                     labels= seq(0+binwidth,400, binwidth)-(binwidth/2)),
           n = n()) %>%
    group_by(Section, bin) %>%
    summarise(p = n()/n[1]) %>%
    ungroup() %>%
    mutate(bin = as.numeric(as.character(bin)))

  x_breaks <- seq(55,405,10)
  binlibs <- seq(50,400,10)
  x_labls <- paste(binlibs, '-', binlibs+9)
  
  y_breaks <- seq(0, .25, 0.05)
  y_labls <- paste(y_breaks*100)
  
  ggplot(fish.stats, aes(x = bin, y = p, fill = Section)) +
    LF_section +
    facet_rep_grid(~Section) +
    geom_col(position='dodge',fill='steelblue') + 
    xlab('Length [mm]') +   # x-axis label
    ylab('% catch') +    # y-axis label
    
    scale_x_continuous(breaks = x_breaks,      # sets scale limits and breaks
                       labels = every_nth(x_labls, 3, inverse = TRUE)) + 
    scale_y_continuous(breaks = y_breaks, limits = c(0,.27),  # sets scale limits and breaks
                       labels = every_nth(y_labls, 1, inverse = TRUE)) +
    ggsave('images/rain_section.pdf', width = 5, height = 2.75) 
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##==========Mesohabitat Comparison Length Frequency Tables

# This function produces a Brown trout LF with X Y labels,
# Ois River title, Divided by section
Brown_trout_mesohabitat <- function (habitat, label.name) {       # set up function for model
  
  binwidth = 10                  # set the bin width of the length frequency
  
  fish.stats <- habitat %>% 
    filter(Species=='Salmo trutta') %>%
    group_by(Mesohabitat) %>%
    mutate(bin = cut(Length_mm, breaks=seq(0,450, binwidth), 
                     labels= seq(0+binwidth,450, binwidth)-(binwidth/2)),
           n = n()) %>%
    group_by(Mesohabitat, bin) %>%
    summarise(p = n()/n[1]) %>%
    ungroup() %>%
    mutate(bin = as.numeric(as.character(bin)))
  
  x_breaks <- seq(45,445,10)
  binlibs <- seq(40,440,10)
  x_labls <- paste(binlibs, '-', binlibs+9)
  
  y_breaks <- seq(0, .3, 0.05)
  y_labls <- paste(y_breaks*100)
  
  ggplot(fish.stats, aes(x = bin, y = p, fill = Section)) +
    LF_mesohabitat +
    facet_rep_grid(~Mesohabitat) +    # divides into collums and repeat grid
    geom_col(position='dodge',fill='darkred') + 
    xlab('Length [mm]') +   # x-axis label
    ylab('% catch') +    # y-axis label
    scale_x_continuous(breaks = x_breaks,      # sets scale limits and breaks
                       labels = every_nth(x_labls, 4, inverse = TRUE)) + 
    scale_y_continuous(breaks = y_breaks, limits = c(0,.33),  # sets scale limits and breaks
                       labels = every_nth(y_labls, 1, inverse = TRUE)) +
    ggsave('images/brown_meso.pdf', width = 7, height = 2.5) 
}

# This function produces a Rainbow trout LF with X Y labels,
# Ois River title, Divided by mesohabitat
Rainbow_trout_mesohabitat <- function (habitat, label.name) {       # set up function for model

  binwidth = 10                  # set the bin width of the length frequency
  
  fish.stats <- habitat %>% 
    filter(Species=='Onchorhynchus mykiss') %>%
    group_by(Mesohabitat) %>%
    mutate(bin = cut(Length_mm, breaks=seq(0,400, binwidth), 
                     labels= seq(0+binwidth,400, binwidth)-(binwidth/2)),
           n = n()) %>%
    group_by(Mesohabitat, bin) %>%
    summarise(p = n()/n[1]) %>%
    ungroup() %>%
    mutate(bin = as.numeric(as.character(bin)))

  x_breaks <- seq(55,405,10)
  binlibs <- seq(50,400,10)
  x_labls <- paste(binlibs, '-', binlibs+9)
  
  y_breaks <- seq(0, .25, 0.05)
  y_labls <- paste(y_breaks*100)

  ggplot(fish.stats, aes(x = bin, y = p, fill = Section)) +
    LF_mesohabitat +
    facet_rep_grid(~Mesohabitat) +
    geom_col(position='dodge',fill='steelblue') + 
    xlab('Length [mm]') +   # x-axis label
    ylab('% catch') +    # y-axis label
    
    scale_x_continuous(breaks = x_breaks,      # sets scale limits and breaks
                       labels = every_nth(x_labls, 3, inverse = TRUE)) + 
    scale_y_continuous(breaks = y_breaks, limits = c(0,.27),  # sets scale limits and breaks
                       labels = every_nth(y_labls, 1, inverse = TRUE)) +
    ggsave('images/rain_meso.pdf', width = 7, height = 2.5) 
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##==========Mix grid Comparison Length Frequency Tables

# This function produces a Brown trout LF with X Y labels,
# Ois River title, 3 x 3 matrix of site and mesohabitat
Brown_trout_mix <- function (habitat, label.name) {       # set up function for model

  binwidth = 10                  # set the bin width of the length frequency
  
  fish.stats <- habitat %>% 
    filter(Species=='Salmo trutta') %>%
    group_by(Lower.Upper.site, Mesohabitat) %>%
    mutate(bin = cut(Length_mm, breaks=seq(0,450, binwidth), 
                     labels= seq(0+binwidth,450, binwidth)-(binwidth/2)),
           n = n()) %>%
    group_by(Mesohabitat, Lower.Upper.site, bin) %>%
    summarise(p = n()/n[1]) %>%
    ungroup() %>%
    mutate(bin = as.numeric(as.character(bin)))
  
  x_breaks <- seq(45,445,10)
  binlibs <- seq(40,440,10)
  x_labls <- paste(binlibs, '-', binlibs+9)
  
  y_breaks <- seq(0, .35, 0.05)
  y_labls <- paste(y_breaks*100)
  
  ggplot(fish.stats, aes(x = bin, y = p)) +
    LF_mix +
    facet_rep_grid(Mesohabitat ~ Lower.Upper.site) +
    geom_col(position='dodge',fill='darkred') + 
    xlab('Length [mm]') +   # x-axis label
    ylab('% catch') +    # y-axis label
    scale_x_continuous(breaks = x_breaks,      # sets scale limits and breaks
                       labels = every_nth(x_labls, 4, inverse = TRUE)) + 
    scale_y_continuous(breaks = y_breaks, limits = c(0,.37),  # sets scale limits and breaks
                       labels = every_nth(y_labls, 1, inverse = TRUE)) +
    ggsave('images/brown_mix.pdf', width = 7, height = 5) 
}

# This function produces a Rainbow trout LF with X Y labels,
# Ois River title, 3 x 3 matrix of site and mesohabitat
Rainbow_trout_mix <- function (habitat, label.name) {       # set up function for model
  
  binwidth = 10                  # set the bin width of the length frequency
  
  fish.stats <- habitat %>% 
    filter(Species=='Onchorhynchus mykiss') %>%
    group_by(Lower.Upper.site, Mesohabitat) %>%
    mutate(bin = cut(Length_mm, breaks=seq(0,400, binwidth), 
                     labels= seq(0+binwidth,400, binwidth)-(binwidth/2)),
           n = n()) %>%
    group_by(Mesohabitat, Lower.Upper.site, bin) %>%
    summarise(p = n()/n[1]) %>%
    ungroup() %>%
    mutate(bin = as.numeric(as.character(bin)))

  x_breaks <- seq(55,405,10)
  binlibs <- seq(50,400,10)
  x_labls <- paste(binlibs, '-', binlibs+9)
  
  y_breaks <- seq(0, .4, 0.05)
  y_labls <- paste(y_breaks*100)

  ggplot(fish.stats, aes(x = bin, y = p)) +
    LF_mix +
    facet_rep_grid(Mesohabitat ~ Lower.Upper.site) +
    geom_col(position='dodge',fill='steelblue') + 
    xlab('Length [mm]') +   # x-axis label
    ylab('% catch') +    # y-axis label
    
    scale_x_continuous(breaks = x_breaks,      # sets scale limits and breaks
                       labels = every_nth(x_labls, 3, inverse = TRUE)) + 
    scale_y_continuous(breaks = y_breaks, limits = c(0,.40),  # sets scale limits and breaks
                       labels = every_nth(y_labls, 2, inverse = TRUE)) +
   ggsave('images/rain_mix.pdf', width = 7, height = 5) 
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##==============Combined histogram of abundance biomass

#this function calculates abundance and biomass for all brown trout
All_species <- function (River, label.name) {       # set up function for model
  
  fish.stats <- River %>% 
    filter(Species=='All Species')# %>%
  
  abundance <- 
    ggplot(fish.stats, aes(x = Mesohabitat, y = Ind_ha_dis, fill=Section)) +
    AB_theme +
    geom_col(width = .6, position = position_dodge(.7)) + 
    scale_y_continuous(breaks = seq(0, 1400, 200), limits = c(0,1350)) +
    scale_fill_manual(values=c('green4','green3')) +
    ylab('Ind. / ha')   # y-axis label
  
  biomass <-
    ggplot(fish.stats, aes(x = Mesohabitat, y = kg_ha_dis, fill=Section)) +
    AB_theme +
    geom_col(width = .6, position = position_dodge(.7)) + 
    scale_fill_manual(values=c('green4','green3')) +
    scale_y_continuous(breaks = seq(0, 70, 10), limits = c(0,70)) +
    ylab('kg / ha')    # y-axis label
  
  AB_graph <- 
    ggarrange(abundance, biomass, labels = c("Abundance", "Biomass"),
              label.x = .4,
              font.label = list(size = 11, face = "plain"),
              common.legend = TRUE, legend = "bottom")
  
  ggexport(AB_graph, filename = "images/all_species.pdf", width = 9, height = 3.5)
  
  AB_graph
  
}

#this function calculates abundance and biomass for all brown trout
Brown_trout <- function (River, label.name) {       # set up function for model
  
  fish.stats <- River %>% 
    filter(Species=='Salmo trutta')# %>%
  
  abundance <- 
    ggplot(fish.stats, aes(x = Mesohabitat, y = Ind_ha_dis, fill=Section)) +
    AB_theme +
    geom_col(width = .6, position = position_dodge(.7)) + 
    scale_fill_manual(values=c('darkred','firebrick2')) +
    scale_y_continuous(breaks = seq(0, 1200, 200), limits = c(0,1200)) +
    ylab('Ind. / ha')   # y-axis label
  
  biomass <-
    ggplot(fish.stats, aes(x = Mesohabitat, y = kg_ha_dis, fill=Section)) +
    AB_theme +
    geom_col(width = .6, position = position_dodge(.7)) + 
    scale_fill_manual(values=c('darkred','firebrick2')) +
    scale_y_continuous(breaks = seq(0, 50, 10), limits = c(0,50)) +
    ylab('kg / ha')     # y-axis label
  
  AB_graph <- 
    ggarrange(abundance, biomass, labels = c("Abundance", "Biomass"),
              label.x = .4,
              font.label = list(size = 11, face = "plain"),
              common.legend = TRUE, legend = "bottom")
  
  ggexport(AB_graph, filename = "images/brown_trout.pdf", width = 9, height = 3.5)
  
  AB_graph
  
}

#this function calculates abundance and biomass for all brown trout
Rainbow_trout <- function (River, label.name) {       # set up function for model
  
  fish.stats <- River %>% 
    filter(Species=='Onchorhynchus mykiss')# %>%
  
  abundance <- 
    ggplot(fish.stats, aes(x = Mesohabitat, y = Ind_ha_dis, fill=Section)) +
    AB_theme +
    geom_col(width = .6, position = position_dodge(.7)) + 
    scale_fill_manual(values=c('steelblue','steelblue2')) +
    scale_y_continuous(breaks = seq(0, 250, 50), limits = c(0,240)) +
    ylab('Ind. / ha')     # y-axis label
  
  biomass <-
    ggplot(fish.stats, aes(x = Mesohabitat, y = kg_ha_dis, fill=Section)) +
    AB_theme +
    geom_col(width = .6, position = position_dodge(.7)) + 
    scale_fill_manual(values=c('steelblue','steelblue2')) +
    scale_y_continuous(breaks = seq(0, 25, 5), limits = c(0,25)) +
    ylab('kg / ha')     # y-axis label
  
  AB_graph <- 
    ggarrange(abundance, biomass, labels = c("Abundance", "Biomass"),
              label.x = .4,
              font.label = list(size = 11, face = "plain"),
              common.legend = TRUE, legend = "bottom")
  
  ggexport(AB_graph, filename = "images/rainbow_trout.pdf", width = 9, height = 3.5)
  
  AB_graph
  
}

#this function calculates abundance and biomass for all brown trout
Grayling <- function (River, label.name) {       # set up function for model
  
  fish.stats <- River %>% 
    filter(Species=='Thymallus thymallus')# %>%
  
  abundance <- 
    ggplot(fish.stats, aes(x = Mesohabitat, y = Ind_ha_dis, fill=Section)) +
    AB_theme +
    geom_col(width = .6, position = position_dodge(.7)) + 
    scale_fill_manual(values=c('snow4','snow3')) +
    ylab('Ind. / ha')     # y-axis label
  
  biomass <-
    ggplot(fish.stats, aes(x = Mesohabitat, y = kg_ha_dis, fill=Section)) +
    AB_theme +
    geom_col(width = .6, position = position_dodge(.7)) + 
    scale_fill_manual(values=c('snow4','snow3')) +
    ylab('kg / ha')     # y-axis label
  
  AB_graph <- 
    ggarrange(abundance, biomass, labels = c("Abundance", "Biomass"),
              label.x = .4,
              font.label = list(size = 11, face = "plain"),
              common.legend = TRUE, legend = "bottom")
  
  ggexport(AB_graph, filename = "images/grayling.pdf", width = 9, height = 3.5)
  
  AB_graph
  
}

#this function calculates abundance and biomass for all brown trout
Meso_AB <- function (River, label.name) {       # set up function for model
  
  fish.stats <- River %>% 
    filter(Species !='All Species')# %>%
  
  abundance <- 
    ggplot(fish.stats, aes(x = Mesohabitat, y = Ind_ha_dis, fill=Species)) +
    AB_theme +
    geom_col(width = .6, position = position_dodge(.7)) + 
    scale_fill_manual(values=c('darkred','steelblue','snow4'),
                      labels = c('Brown trout', 'Rainbow trout', 'Grayling')) +
    scale_y_continuous(breaks = seq(0, 1200, 200), limits = c(0,1200)) +
    ylab('Ind. / ha')     # y-axis label
  
  biomass <-
    ggplot(fish.stats, aes(x = Mesohabitat, y = kg_ha_dis, fill=Species)) +
    AB_theme +
    geom_col(width = .6, position = position_dodge(.7)) + 
    scale_fill_manual(values=c('darkred','steelblue','snow4'),
                      labels = c('Brown trout', 'Rainbow trout', 'Grayling')) +
    ylab('kg / ha')    # y-axis label
  
  AB_graph <- 
    ggarrange(abundance, biomass, labels = c("Abundance", "Biomass"),
              label.x = .4,
              font.label = list(size = 11, face = "plain"),
              common.legend = TRUE, legend = "bottom")
  
  ggexport(AB_graph, filename = "images/mesohabitat.pdf", width = 9, height = 3.5)
  
  AB_graph
  
}

#this function calculates abundance and biomass for all brown trout
Section_AB <- function (River, label.name) {       # set up function for model
  
  fish.stats <- River %>% 
    filter(Species !='All Species')# %>%
  
  abundance <- 
    ggplot(fish.stats, aes(x = Section, y = Ind_ha_dis, fill=Species)) +
    AB_theme +
    geom_col(width = .6, position = position_dodge(.7)) + 
    scale_fill_manual(values=c('darkred','steelblue','snow4'),
                      labels = c('Brown trout', 'Rainbow trout', 'Grayling')) +
    scale_y_continuous(breaks = seq(0, 1200, 200), limits = c(0,1200)) +
    ylab('Ind. / ha')     # y-axis label
  
  biomass <-
    ggplot(fish.stats, aes(x = Section, y = kg_ha_dis, fill=Species)) +
    AB_theme +
    geom_col(width = .6, position = position_dodge(.7)) + 
    scale_fill_manual(values=c('darkred','steelblue','snow4'),
                      labels = c('Brown trout', 'Rainbow trout', 'Grayling')) +
    ylab('kg / ha')     # y-axis label
  
  AB_graph <- 
    ggarrange(abundance, biomass, labels = c("Abundance", "Biomass"),
              label.x = .4,
              font.label = list(size = 11, face = "plain"),
              common.legend = TRUE, legend = "bottom")
  
  ggexport(AB_graph, filename = "images/section.pdf", width = 9, height = 3.5)
  
  AB_graph
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#==================Fish average length function

#this function calculates abundance and biomass for all brown trout
Avg_length <- function (River, label.name) {       # set up function for model
  
  fish.stats <- River %>% 
    filter(Length_mm >120)# %>%
  
  levels(fish.stats$Species) <- c('Brown trout', 'Rainbow trout')  
  
  ggplot(fish.stats, aes(Mesohabitat, Length_mm, color=Species)) +
    Boxplot_theme +    
    geom_point(size = -1, aes(fill = Species)) + 
    geom_boxplot(color='black', aes(fill = Species), show.legend = FALSE) +
    scale_y_continuous(breaks = seq(100 ,450, 50), limits = c(100,450)) +
    scale_color_manual(values = c("black", "black")) +
    scale_fill_manual(values = c("darkred", "steelblue")) +
    guides(colour = guide_legend(keywidth = unit(.1, "cm"),
                                 keyheight = unit(.1, "cm"),
                                 override.aes = list(shape = 22,
                                                     size = 5))) +
    facet_grid(.~Section) +    #Seperates the groups into columns
    ylab('Length [mm]') +
  
  ggsave('images/avg_length.pdf', width = 6, height = 3.5) 
  
}



########################################################################
########################################################################
########################################################################


##This function calculates the average length by species
Avg_calc <- function (habitat) {
  
  fish.avg <- round(c(
    mean(habitat$Length_mm[habitat$Species=='Salmo trutta'], na.rm = T),
    mean(habitat$Length_mm[habitat$Species=='Onchorhynchus mykiss'], na.rm = T),
    mean(habitat$Length_mm, na.rm = T) 
    ))
 
  name.avg <- paste(substitute(habitat),'.avg',sep='')
  
  assign(name.avg, fish.avg, envir = globalenv())
  
}


## These functions calculate the abundance of the River
# This function calculats abundance for all species
All_abundance <- function (River, label.name) {       # set up function for model

  fish.stats <- River %>% 
    filter(Species=='All Species')# %>%
  
  ggplot(fish.stats, aes(x = Mesohabitat, y = Ind_ha_dis, fill=Section)) +
    AB_theme +
    geom_col(width = .6, position = position_dodge(.7)) + 
    scale_y_continuous(breaks = seq(0, 1400, 200), limits = c(0,1350)) +
    scale_fill_manual(values=c('green4','green3')) +
    ylab('Ind. / ha') +    # y-axis label
    ggsave('images/all_abundance.pdf', width = 5, height = 3.5) 
}

#this function calculates abundance for all brown trout
Brown_abundance <- function (River, label.name) {       # set up function for model

  fish.stats <- River %>% 
    filter(Species=='Salmo trutta')# %>%
  
  ggplot(fish.stats, aes(x = Mesohabitat, y = Ind_ha_dis, fill=Section)) +
    AB_theme +
    geom_col(width = .6, position = position_dodge(.7)) + 
    scale_fill_manual(values=c('darkred','firebrick2')) +
    scale_y_continuous(breaks = seq(0, 1200, 200), limits = c(0,1200)) +
    ylab('Ind. / ha') +    # y-axis label
    ggsave('images/brown_abundance.pdf', width = 5, height = 3.5) 
}

#this function calculates abundance for all rainbow trout
Rainbow_abundance <- function (River, label.name) {       # set up function for model

  fish.stats <- River %>% 
    filter(Species=='Onchorhynchus mykiss')# %>%
  
  ggplot(fish.stats, aes(x = Mesohabitat, y = Ind_ha_dis, fill=Section)) +
    AB_theme +
    geom_col(width = .6, position = position_dodge(.7)) + 
    scale_fill_manual(values=c('steelblue','steelblue2')) +
    scale_y_continuous(breaks = seq(0, 250, 50), limits = c(0,240)) +
    ylab('Ind. / ha') +    # y-axis label
    ggsave('images/rain_abundance.pdf', width = 5, height = 3.5) 
}

#This function calculates grayling abundance
Grayling_abundance <- function (River, label.name) {       # set up function for model

  fish.stats <- River %>% 
    filter(Species=='Thymallus thymallus')# %>%
  
  ggplot(fish.stats, aes(x = Mesohabitat, y = Ind_ha_dis, fill=Section)) +
    AB_theme +
    geom_col(width = .6, position = position_dodge(.7)) + 
    scale_fill_manual(values=c('snow4','snow3')) +
    #    xlab('Mesohabitat') +   # x-axis label
    ylab('Ind. / ha') +    # y-axis label
    ggsave('images/gray_abundance.pdf', width = 5, height = 3.5) 
}

#this function calculates the mix abundance in the mesohabitats
Meso_abundance <- function (River, label.name) {       # set up function for model
  
  fish.stats <- River %>% 
    filter(Species !='All Species')# %>%
  
  ggplot(fish.stats, aes(x = Mesohabitat, y = Ind_ha_dis, fill=Species)) +
    AB_theme +
    geom_col(width = .6, position = position_dodge(.7)) + 
    scale_fill_manual(values=c('darkred','steelblue','snow4'),
                      labels = c('Brown trout', 'Rainbow trout', 'Grayling')) +
    scale_y_continuous(breaks = seq(0, 1200, 200), limits = c(0,1200)) +
    ylab('Ind. / ha') +    # y-axis label
    ggsave('images/meso_abundance.pdf', width = 5, height = 3.5) 
}

#this function calculates the mix abundance in the sections
Sec_abundance <- function (River, label.name) {       # set up function for model
  
  fish.stats <- River %>% 
    filter(Species !='All Species')# %>%
  
  ggplot(fish.stats, aes(x = Section, y = Ind_ha_dis, fill=Species)) +
    AB_theme +
    geom_col(width = .6, position = position_dodge(.7)) + 
    scale_fill_manual(values=c('darkred','steelblue','snow4'),
                      labels = c('Brown trout', 'Rainbow trout', 'Grayling')) +
    scale_y_continuous(breaks = seq(0, 1200, 200), limits = c(0,1200)) +
    ylab('Ind. / ha') +    # y-axis label
    ggsave('images/sec_abundance.pdf', width = 5, height = 3.5) 
}




## These functions calculate the biomassof the River
# This function calculats biomassfor all species
All_biomass<- function (River, label.name) {       # set up function for model

    fish.stats <- River %>% 
    filter(Species=='All Species')# %>%
  
  ggplot(fish.stats, aes(x = Mesohabitat, y = kg_ha_dis, fill=Section)) +
    AB_theme +
    geom_col(width = .6, position = position_dodge(.7)) + 
    scale_fill_manual(values=c('green4','green3')) +
    scale_y_continuous(breaks = seq(0, 70, 10), limits = c(0,70)) +
    ylab('kg / ha') +    # y-axis label
    ggsave('images/all_biomass.pdf', width = 5, height = 3.5) 
}


#this function calculates biomassfor all brown trout
Brown_biomass<- function (River, label.name) {       # set up function for model

  fish.stats <- River %>% 
    filter(Species=='Salmo trutta')# %>%
  
  ggplot(fish.stats, aes(x = Mesohabitat, y = kg_ha_dis, fill=Section)) +
    AB_theme +
    geom_col(width = .6, position = position_dodge(.7)) + 
    scale_fill_manual(values=c('darkred','firebrick2')) +
    scale_y_continuous(breaks = seq(0, 50, 10), limits = c(0,50)) +
    ylab('kg / ha') +    # y-axis label
    ggsave('images/brown_biomass.pdf', width = 5, height = 3.5) 
}

#this function calculates biomassfor all rainbow trout
Rainbow_biomass<- function (River, label.name) {       # set up function for model

  fish.stats <- River %>% 
    filter(Species=='Onchorhynchus mykiss')# %>%
  
  ggplot(fish.stats, aes(x = Mesohabitat, y = kg_ha_dis, fill=Section)) +
    AB_theme +
    geom_col(width = .6, position = position_dodge(.7)) + 
    scale_fill_manual(values=c('steelblue','steelblue2')) +
    #    xlab('Mesohabitat') +   # x-axis label
    ylab('kg / ha') +    # y-axis label
    ggsave('images/rain_biomass.pdf', width = 5, height = 3.5) 
}

#This function calculates grayling abundance
Grayling_biomass<- function (River, label.name) {       # set up function for model

  fish.stats <- River %>% 
    filter(Species=='Thymallus thymallus')# %>%
  
  ggplot(fish.stats, aes(x = Mesohabitat, y = kg_ha_dis, fill=Section)) +
    AB_theme +
    geom_col(width = .6, position = position_dodge(.7)) + 
    scale_fill_manual(values=c('snow4','snow3')) +
    #    xlab('Mesohabitat') +   # x-axis label
    ylab('kg / ha') +    # y-axis label
    ggsave('images/gray_biomass.pdf', width = 5, height = 3.5) 
}

#This function calculates the mix biomass in the mesohabitats
Meso_biomass <- function (River, label.name) {       # set up function for model
  
  fish.stats <- River %>% 
    filter(Species !='All Species')# %>%
  
  ggplot(fish.stats, aes(x = Mesohabitat, y = kg_ha_dis, fill=Species)) +
    AB_theme +
    geom_col(width = .6, position = position_dodge(.7)) + 
    scale_fill_manual(values=c('darkred','steelblue','snow4'),
                      labels = c('Brown trout', 'Rainbow trout', 'Grayling')) +
    #    xlab('Mesohabitat') +   # x-axis label
    ylab('kg / ha') +    # y-axis label
    ggsave('images/meso_biomass.pdf', width = 5, height = 3.5) 
}

#This function calculates the mix biomass in the mesohabitats
Sec_biomass <- function (River, label.name) {       # set up function for model
  
  fish.stats <- River %>% 
    filter(Species !='All Species')# %>%
  
  ggplot(fish.stats, aes(x = Section, y = kg_ha_dis, fill=Species)) +
    AB_theme +
    geom_col(width = .6, position = position_dodge(.7)) + 
    scale_fill_manual(values=c('darkred','steelblue','snow4'),
                      labels = c('Brown trout', 'Rainbow trout', 'Grayling')) +
    ylab('kg / ha') +    # y-axis label
    ggsave('images/sec_biomass.pdf', width = 5, height = 3.5) 
}


##############################################################



# This function produces a Brown trout LF with highlighted age classes
Highlightedageclasses <- function (habitat, label.name) {       # set up function for model
  
  binwidth = 10                  # set the bin width of the length frequency
  
  fish.stats <- habitat %>% 
    filter(Species=='Salmo trutta') %>%
    mutate(bin = cut(Length_mm, breaks=seq(0,450, binwidth), 
                     labels= seq(0+binwidth,450, binwidth)-(binwidth/2)),
           n = n()) %>%
    group_by(Species, bin) %>%
    summarise(p = n()/n[1]) %>%
    ungroup() %>%
    mutate(bin = as.numeric(as.character(bin)))
  
  n.count <- nrow(filter(habitat, Species=='Salmo trutta'))
  
  x_breaks <- seq(45,435,10)
  binlibs <- seq(40,430,10)
  x_labls <- paste(binlibs, '-', binlibs+9)
  
  y_breaks <- seq(0, .25, 0.025)
  y_labls <- paste(y_breaks*100)
  
  ggplot(fish.stats, aes(x = bin, y = p)) +  
    LF_single +
    geom_col(position='dodge',fill='darkred') + 
    xlab('Length [mm]') +   # x-axis label
    ylab('% catch') +    # y-axis label
    scale_x_continuous(breaks = x_breaks,      # sets scale limits and breaks
                       labels = every_nth(x_labls, 2, inverse = TRUE)) + 
    scale_y_continuous(breaks = y_breaks, limits = c(0,.27),  # sets scale limits and breaks
                       labels = every_nth(y_labls, 2, inverse = TRUE)) +

    annotate("rect", xmin = 40, xmax = 119, ymin = 0, ymax = .27, alpha = .2) +
    annotate("rect", xmin = 220, xmax = 300, ymin = 0, ymax = .27, alpha = .2) +
       
    annotate('text',size=3, x = 391, y = .243, label = paste('n =', n.count)) +
    ggsave('images/brown_single.pdf', width = 4.5, height = 3) 
}




