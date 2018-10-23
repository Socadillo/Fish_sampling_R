#---------------setup boxplot charts
Boxplot_theme <- theme(plot.background = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank(),
                       strip.text = element_text(face="bold", size=11),
                       strip.background = element_blank(),
                       
                       panel.spacing.x = unit(.5, "cm"),
                       
                       strip.placement = 'inside',
                       
                       axis.line = element_line(color = 'black'),
                       axis.title.x = element_blank(),
                       axis.title.y = element_text(size = 10, color = 'black'),
                       axis.text.x = element_text(size = 10, color = 'black'),
                       axis.text.y = element_text(size = 8, color = 'black'),
                       legend.text = element_text(size = 11,
                                                  margin = margin(l = 3, r = 6)),
                       legend.title = element_blank(),
                       legend.margin=margin(t = -.25, unit='cm'),
#                       legend.background = element_rect(color = 'black', size = .5, linetype = 'solid'),
                       legend.position = 'bottom')



#---------------Abundance theme
AB_theme <- theme(plot.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(color = 'black'),
                  axis.title.x = element_blank(),
                  axis.title.y = element_text(size = 10, color = 'black'),
                  axis.text.x = element_text(size = 10, color = 'black'),
                  axis.text.y = element_text(size = 9, color = 'black'),
                  legend.text = element_text(size = 11,
                  margin = margin(l = 3, r = 6)),
                  legend.margin=margin(t = 0, unit='cm'),
                  legend.title = element_blank())


#---------------Fish length frequency theme
LF_single <- theme(plot.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(color = 'black'),
                   axis.title.x = element_text(size = 10, color = 'black'),
                   axis.title.y = element_text(size = 10, color = 'black',
                                               margin = margin(r=5)),
                   axis.text.x = element_text(size = 8, color = 'black',
                                              angle = 60, hjust = 1),
                   axis.text.y = element_text(size = 8, color = 'black'))

#---------------Fish length frequency theme
LF_section <- theme(plot.background = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    panel.background = element_blank(),
                    panel.spacing.x = unit(-.1, "lines"),
                    strip.text = element_text(face="bold", size=11),
                    strip.background = element_blank(),
                    axis.line = element_line(color = 'black'),
                    axis.title.x = element_text(size = 10, color = 'black'),
                    axis.title.y = element_text(size = 10, color = 'black', margin = margin(r=5)),
                    axis.text.x = element_text(size = 8, color = 'black',
                                               angle = 60, hjust = 1),
                    axis.text.y = element_text(size = 10, color = 'black'))


#---------------Fish length frequency theme
LF_mesohabitat <- theme(plot.background = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.border = element_blank(),
                        panel.background = element_blank(),
                        panel.spacing.x = unit(-.1, "lines"),
                        strip.text = element_text(face="bold", size = 11),
                        strip.background = element_blank(),
                        axis.line = element_line(color = 'black'),
                        axis.title.x = element_text(size = 10, color = 'black'),
                        axis.title.y = element_text(size = 10, color = 'black', margin = margin(r=5)),
                        axis.text.x = element_text(size = 8, color = 'black',
                                                   angle = 60, hjust = 1),
                        axis.text.y = element_text(size = 8, color = 'black'))

#---------------Fish length frequency theme
LF_mix <- theme(plot.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.spacing.x = unit(-.1, "lines"),
                panel.spacing.y = unit(-.1, "lines"),
                strip.text = element_text(face="bold", size = 11),
                strip.background = element_blank(),
                axis.line = element_line(color = 'black'),
                axis.title.x = element_text(size = 10, color = 'black'),
                axis.title.y = element_text(size = 10, color = 'black', margin = margin(r=5)),
                axis.text.x = element_text(size = 8, color = 'black',
                                           angle = 60, hjust = 1),
                axis.text.y = element_text(size = 8, color = 'black'))
