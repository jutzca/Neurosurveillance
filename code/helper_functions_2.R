# -----------------------------------------------------------------------------------
# Building a Shiny app for visualisation of neurological outcomes in SCI
# Helper functions
# 
# July 15, 2020
# L. Bourguignon
# -----------------------------------------------------------------------------------


plot_base_emsci <- function(data_emsci, score, time){
  data_transformed <- data_emsci
  data_transformed <- data_transformed[data_transformed$ExamStage %in% unlist(time, use.names=FALSE), ] # filter time points
  colors <- c("#D7191C", "#FDAE61", "#ABD9E9", "#2C7BB6")
  
  labels=c("A" = "AIS A", "B" = "AIS B", "C" = "AIS C", 'D' = 'AIS D')

  p1 <- ggplot(data_transformed, aes(x=ExamStage, y=get(score))) +
    geom_boxplot(outlier.shape = NA) +  # remove NAs, and set the whisker length to all included points
    geom_jitter(shape=21, colour="grey20", width = 0.3, alpha = 0.3) +
    #theme_economist(horizontal = FALSE) +
    theme(axis.text.x = element_text(size=16, angle = 30, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size=16),
          axis.title.x = element_text(size=18, face="bold", vjust=-0.5),
          axis.title.y = element_text(size=18, face="bold"),
          plot.background = element_rect(fill = "#FFFFFF")) +
    ylab(score) +
    xlab('Exam Stage')
  
  if(is.element(score, c('UEMS', 'LEMS', 'RMS', 'LMS'))){
    p1 <- p1 + ylim(0, 50)
  } else if (is.element(score, c('RUEMS', 'LUEMS', 'RLEMS', 'LLEMS'))){
    p1 <- p1 + ylim(0, 25)
  } else if (is.element(score, c('TMS'))){
    p1 <- p1 + ylim(0, 100)
  }
  
  return(p1)
}

plot_base_Sygen <- function(data_sygen, score, time){
  data_transformed <- data_sygen
  data_transformed <- data_transformed[data_transformed$Time %in% unlist(time, use.names=FALSE), ] # filter time points
  colors <- c("#D7191C", "#FDAE61", "#ABD9E9", "#2C7BB6")
  
  labels=c("A" = "AIS A", "B" = "AIS B", "C" = "AIS C", 'D' = 'AIS D')
  
  p1 <- ggplot(data_transformed, aes(x=Time, y=get(score))) +
    geom_boxplot(outlier.shape = NA) +  # remove NAs, and set the whisker length to all included points
    geom_jitter(shape=21, colour="grey20", width = 0.3, alpha = 0.3) +
    theme_economist(horizontal = FALSE) +
    theme(axis.text.x = element_text(size=16, angle = 30, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size=16),
          axis.title.x = element_text(size=18, face="bold", vjust=-0.5),
          axis.title.y = element_text(size=18, face="bold"),
          plot.background = element_rect(fill = "#FFFFFF")) +
    ylab(score) +
    xlab('Exam Stage')
  
    if(is.element(score, c('UEMS', 'LEMS', 'RMS', 'LMS'))){
      p1 <- p1 + ylim(0, 50)
    } else if (is.element(score, c('RUEMS', 'LUEMS', 'RLEMS', 'LLEMS'))){
      p1 <- p1 + ylim(0, 25)
    } else if (is.element(score, c('TMS'))){
      p1 <- p1 + ylim(0, 100)
    }
      
  return(p1)
}

plot_base_All <- function(data_All, score){
  data_transformed <- data_All
  colors <- c("#D7191C", "#FDAE61", "#ABD9E9", "#2C7BB6")
  
  labels=c("A" = "AIS A", "B" = "AIS B", "C" = "AIS C", 'D' = 'AIS D')
  
  p1 <- ggplot(data_transformed, aes(x=Dataset, y=get(score))) +
    geom_boxplot(outlier.shape = NA) +  # remove NAs, and set the whisker length to all included points
    geom_jitter(shape=21, colour="grey20", width = 0.3, alpha = 0.3) +
    theme_economist(horizontal = FALSE) +
    theme(axis.text.x = element_text(size=16, angle = 30, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size=16),
          axis.title.x = element_text(size=18, face="bold", vjust=-0.5),
          axis.title.y = element_text(size=18, face="bold"),
          plot.background = element_rect(fill = "#FFFFFF")) +
    ylab(score) +
    xlab('Exam Stage')
  
  if(is.element(score, c('UEMS', 'LEMS', 'RMS', 'LMS'))){
    p1 <- p1 + ylim(0, 50)
  } else if (is.element(score, c('RUEMS', 'LUEMS', 'RLEMS', 'LLEMS'))){
    p1 <- p1 + ylim(0, 25)
  } else if (is.element(score, c('TMS'))){
    p1 <- p1 + ylim(0, 100)
  }
  
  return(p1)
}

plot_error <- function(){
  rects <- data.frame(x = 1:1,
                      colors = c("white"),
                      text = "Please choose 2 filters")
  p <- ggplot(rects, aes(x, y = 0, fill = colors, label = text)) +
    geom_tile(width = .25, height = .1) + # make square tiles
    geom_text(color = "black") + # add white text in the middle
    scale_fill_identity(guide = "none") + # color the tiles with the colors in the data frame
    coord_fixed() + # make sure tiles are square
    theme_void() # remove any axis markings
  return(p)
  
}

plot_filters_emsci <- function(data_emsci, score, time, filter1, filter2, cat1, cat2){
  
  data_transformed <- data_emsci
  data_transformed <- data_transformed[data_transformed[ ,which(names(data_transformed) == 'ExamStage')] %in% unlist(time, use.names=FALSE), ] # filter time points
  data_transformed <- data_transformed[data_transformed[ ,which(names(data_transformed) == filter1)] %in% unlist(cat1, use.names=FALSE), ]
  data_transformed <- data_transformed[data_transformed[ ,which(names(data_transformed) == filter2)] %in% unlist(cat2, use.names=FALSE), ]

  colors <- c("#D7191C", "#FDAE61", "#ABD9E9", "#2C7BB6")

  labels=c("A" = "AIS A", "B" = "AIS B", "C" = "AIS C", 'D' = 'AIS D')

  p1 <- data_transformed %>%
    dplyr::group_by_at(vars(filter2,filter1)) %>%
    #dplyr::mutate(value2 = filter_lims(get(score))) %>%  # new variable (value2) so as not to displace first one)
    ggplot(aes(x=ExamStage, y=get(score))) +
    geom_boxplot(outlier.shape = NA) +  # remove NAs, and set the whisker length to all included points
    geom_jitter(shape=21, colour="grey20", width = 0.3, alpha = 0.3) +
    facet_grid(reformulate(filter2,filter1), scales="free") + 
    #theme_economist(horizontal = FALSE) +
    theme(axis.text.x = element_text(size=16, angle = 30, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size=16),
          axis.title.x = element_text(size=18, face="bold", vjust=-0.5),
          axis.title.y = element_text(size=18, face="bold"),
          plot.background = element_rect(fill = "#FFFFFF"),
          strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14)) +
    ylab(score) +
    xlab('Exam Stage')
  
  if(is.element(score, c('UEMS', 'LEMS', 'RMS', 'LMS'))){
    p1 <- p1 + ylim(0, 50)
  } else if (is.element(score, c('RUEMS', 'LUEMS', 'RLEMS', 'LLEMS'))){
    p1 <- p1 + ylim(0, 25)
  } else if (is.element(score, c('TMS'))){
    p1 <- p1 + ylim(0, 100)
  }
  
  return(p1)

}

plot_filters_emsci_2 <- function(data_emsci, score, time, filter1, filter2, cat1, cat2){
  
  data_transformed <- data_emsci
  data_transformed <- data_transformed[data_transformed[ ,which(names(data_transformed) == 'ExamStage')] %in% unlist(time, use.names=FALSE), ] # filter time points
  data_transformed <- data_transformed[data_transformed[ ,which(names(data_transformed) == filter1)] %in% unlist(cat1, use.names=FALSE), ]
  data_transformed <- data_transformed[data_transformed[ ,which(names(data_transformed) == filter2)] %in% unlist(cat2, use.names=FALSE), ]
  
  colors <- c("#D7191C", "#FDAE61", "#ABD9E9", "#2C7BB6")
  
  labels=c("A" = "AIS A", "B" = "AIS B", "C" = "AIS C", 'D' = 'AIS D')
  
  p1 <- data_transformed %>%
    #dplyr::group_by_at(vars(filter2,filter1)) %>%
    dplyr::group_by_at(vars(filter2,filter1)) %>%
    #dplyr::mutate(value2 = filter_lims(get(score))) %>%  # new variable (value2) so as not to displace first one)
    ggplot(aes(x=ExamStage, y=get(score))) +
    geom_boxplot(outlier.shape = NA) +  # remove NAs, and set the whisker length to all included points
    geom_jitter(shape=21, colour="grey20", width = 0.3, alpha = 0.3) +
    facet_grid(reformulate(cat1,cat2), scales="free") + 
    #theme_economist(horizontal = FALSE) +
    theme(axis.text.x = element_text(size=16, angle = 30, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size=16),
          axis.title.x = element_text(size=18, face="bold", vjust=-0.5),
          axis.title.y = element_text(size=18, face="bold"),
          plot.background = element_rect(fill = "#FFFFFF"),
          strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14)) +
    ylab(score) +
    xlab('Exam Stage')
  
  if(is.element(score, c('UEMS', 'LEMS', 'RMS', 'LMS'))){
    p1 <- p1 + ylim(0, 50)
  } else if (is.element(score, c('RUEMS', 'LUEMS', 'RLEMS', 'LLEMS'))){
    p1 <- p1 + ylim(0, 25)
  } else if (is.element(score, c('TMS'))){
    p1 <- p1 + ylim(0, 100)
  }
  
  return(p1)
  
}

filter_lims <- function(x){
  l <- boxplot.stats(x)$stats[1]
  u <- boxplot.stats(x)$stats[5]
  
  for (i in 1:length(x)){
    x[i] <- ifelse(x[i]>l & x[i]<u, x[i], NA)
  }
  return(x)
}

plot_filters_Sygen <- function(data_sygen, score, time, filter1, filter2, cat1, cat2){
  data_transformed <- data_sygen
  data_transformed <- data_transformed[data_transformed[ ,which(names(data_transformed) == 'Time')] %in% unlist(time, use.names=FALSE), ] # filter time points
  data_transformed <- data_transformed[data_transformed[ ,which(names(data_transformed) == filter1)] %in% unlist(cat1, use.names=FALSE), ]
  data_transformed <- data_transformed[data_transformed[ ,which(names(data_transformed) == filter2)] %in% unlist(cat2, use.names=FALSE), ]

  colors <- c("#D7191C", "#FDAE61", "#ABD9E9", "#2C7BB6")
  
  labels=c("A" = "AIS A", "B" = "AIS B", "C" = "AIS C", 'D' = 'AIS D')
  
  p1 <- data_transformed %>%
    dplyr::group_by_at(vars(filter2,filter1)) %>%
    #dplyr::mutate(value2 = filter_lims(get(score))) %>%  # new variable (value2) so as not to displace first one)
    ggplot(aes(x=Time, y=get(score))) +
    geom_boxplot(outlier.shape = NA) +  # remove NAs, and set the whisker length to all included points
    geom_jitter(shape=21, colour="grey20", width = 0.3, alpha = 0.3) +
    facet_grid(reformulate(filter2,filter1), scales="free") + 
    #theme_economist(horizontal = FALSE) +
    theme(axis.text.x = element_text(size=16, angle = 30, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size=16),
          axis.title.x = element_text(size=18, face="bold", vjust=-0.5),
          axis.title.y = element_text(size=18, face="bold"),
          plot.background = element_rect(fill = "#FFFFFF"),
          strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14)) +
    ylab(score) +
    xlab('Exam Stage')
  
  if(is.element(score, c('UEMS', 'LEMS', 'RMS', 'LMS'))){
    p1 <- p1 + ylim(0, 50)
  } else if (is.element(score, c('RUEMS', 'LUEMS', 'RLEMS', 'LLEMS'))){
    p1 <- p1 + ylim(0, 25)
  } else if (is.element(score, c('TMS'))){
    p1 <- p1 + ylim(0, 100)
  }
  
  return(p1)
  
}


plot_filters_All <- function(data_all, score, filter1, filter2, cat1, cat2){
  data <- data_all
  data <- data[data[ ,which(names(data) == filter1)] %in% unlist(cat1, use.names=FALSE), ]
  data <- data[data[ ,which(names(data) == filter2)] %in% unlist(cat2, use.names=FALSE), ]

  vars_sygen <- names(data) %in% c(score, filter1, filter2, 'Dataset')
  data_transformed <- data[vars_sygen]
  
  data_transformed = data_transformed[complete.cases(data_transformed),]
  
  colors <- c("#D7191C", "#FDAE61", "#ABD9E9", "#2C7BB6")
  
  labels=c("A" = "AIS A", "B" = "AIS B", "C" = "AIS C", 'D' = 'AIS D')
  
  p1 <- data_transformed %>%
    dplyr::group_by_at(vars(filter2,filter1)) %>%
    #dplyr::mutate(value2 = filter_lims(get(score))) %>%  # new variable (value2) so as not to displace first one)
    ggplot(aes(x=Dataset, y=get(score))) +
    geom_boxplot(outlier.shape = NA) +  # remove NAs, and set the whisker length to all included points
    geom_jitter(shape=21, colour="grey20", width = 0.3, alpha = 0.3) +
    facet_grid(reformulate(filter2,filter1), scales="free") + 
    #theme_economist(horizontal = FALSE) +
    theme(axis.text.x = element_text(size=16, angle = 30, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size=16),
          axis.title.x = element_text(size=18, face="bold", vjust=-0.6),
          axis.title.y = element_text(size=18, face="bold"),
          plot.background = element_rect(fill = "#FFFFFF"),
          strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14)) +
    ylab(score) +
    xlab('Exam Stage')
  
  if(is.element(score, c('UEMS', 'LEMS', 'RMS', 'LMS'))){
    p1 <- p1 + ylim(0, 50)
  } else if (is.element(score, c('RUEMS', 'LUEMS', 'RLEMS', 'LLEMS'))){
    p1 <- p1 + ylim(0, 25)
  } else if (is.element(score, c('TMS'))){
    p1 <- p1 + ylim(0, 100)
  }
  
  return(p1)
  
}

plot_base_Age_EMSCI <- function(data,title){
  
  age_overall <- ggplot(data,
                        aes(y = as.factor(YEARDOI_cat) , x = AgeAtDOI)) +
    geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) +
    scale_fill_gradientn(colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
                         name = "Age [years]") +
    labs(title = title) +
    facet_grid(.~data$Sex) +
    xlab("Age at Injury") +
    ylab("Year of Injury") +
    theme_economist(horizontal = FALSE) +
    theme(text = element_text(color = "#3A3F4A"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
          plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
          plot.subtitle = element_text(size = 12, margin = margin(b = 20), hjust = 0.030),
          plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
          axis.text.y = element_text(size = 10, color = "#5D646F"),
          strip.text = element_text(color = "#5D646F", size = 14, face = "bold", hjust = 0.030),
          plot.background = element_rect(fill = "#FFFFFF"),
          legend.spacing  = unit(0.1, "lines"),
          legend.text  = element_text(size = 10),
          legend.title = element_text(size = 10),
          legend.text.align = 0,
          strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))

  return(age_overall)
}

plot_base_Age_Sygen <- function(data,title){
  
  age_overall.sygen <- ggplot(
    data, 
    aes(y = as.factor(YEARDOI_cat) , x = age)
  ) + geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 3, size = 0.3
  ) + scale_fill_gradientn(
    colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"),
    name = "Age [years]"
  )+
    labs(title = title) +
    facet_grid(.~data$sexcd) +
    xlab("Age at Injury") +
    ylab("Year of Injury") +
    theme_economist(horizontal = FALSE) +
    theme(text = element_text(color = "#3A3F4A"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
          plot.title = element_text(face = "bold", size = 12, margin = margin(b = 10), hjust = 0.5),
          plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
          plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
          axis.text.y = element_text(size = 10, color = "#5D646F"),
          strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.030),
          plot.background = element_rect(fill = "#FFFFFF"),
          legend.spacing  = unit(0.1, "lines"),
          legend.text  = element_text(size = 10),
          legend.title = element_text(size = 10),
          legend.text.align = 0,
          strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  
  return(age_overall.sygen)
}

plot_base_Age_SCI_rehab <- function(data,title){
  
  age_overall.SCI_rehab <- ggplot(data, aes(x = Age)) + 
    geom_bar(aes(y = ..prop.., group = 1), stat = "count") + 
    scale_y_continuous(limits=c(0,0.3),labels = scales::percent)+ 
    labs(title = title) +
    facet_grid(data$YEARDOI~data$Sex) +
    xlab("Age at Injury") +
    ylab("Proportion") +
    theme_economist(horizontal = FALSE) +
    theme(panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
          strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.50),
          plot.background = element_rect(fill = "#FFFFFF"),
          axis.text.x = element_text(angle = 45),
          strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14))
  
  return(age_overall.SCI_rehab)
}

plot_base_Sex_EMSCI <- function(data, title){
  emsci.sex.long <- data %>%
    dplyr::count(Sex,YEARDOI_cat) %>%
    dplyr::group_by(YEARDOI_cat) %>%
    dplyr::mutate(frequency = (n/sum(n))*100)

  #------Plot population pyramide for year and color by sex - OVERALL ----
  #Plot data for the male patients
  gg.male <- ggplot(data = subset(emsci.sex.long, Sex=='Male'),
                    mapping = aes(x = as.factor(YEARDOI_cat),
                                  y = frequency,
                                  fill = Sex,
                                  label=paste(round(frequency, 0), "% (", 'n = ', n, ')', sep=""))) +
    geom_bar(stat = "identity") +
    scale_y_continuous('Frequency [%]', limits = c(0, 100)) +
    scale_fill_manual(values = as.vector("#3E606F"))+
    geom_text(hjust=(1.1), size=3.5, colour="#FFFFFF") +
    theme_economist(horizontal = FALSE) +
    theme(text = element_text(color = "#3A3F4A"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
          axis.title = element_blank(),
          plot.title = element_text(face = "bold", size = 14, margin = margin(b = 10), hjust = 0.030),
          plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
          plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
          axis.text.y = element_text(size = 10, color = "#5D646F"),
          axis.text.x = element_text(size = 10, color = "#5D646F"),
          strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.5),
          plot.background = element_rect(fill = "#FFFFFF"),
          plot.margin=unit(c(0.1,0.1,0.1,0.05),"cm"),
          legend.position = "none",
          legend.spacing  = unit(0.1, "lines"),
          legend.text  = element_text(size = 10),
          legend.text.align = 0,
          axis.ticks = element_blank())+
    ggtitle("Male") +
    coord_flip()

  ##Plot data for the female patients
  gg.female <-  ggplot(data = subset(emsci.sex.long, Sex=='Female'),
                       mapping = aes(x = as.factor(YEARDOI_cat),
                                     y = frequency,
                                     fill = Sex,
                                     label=paste(round(frequency, 0), "% (", 'n = ', n, ')', sep=""))) +
    geom_bar(stat = "identity") +
    geom_text(hjust=(1), size=3.5, colour="#5D646F") +
    scale_y_continuous('Frequency [%]', limits = c(100, 0), trans = 'reverse') +
    scale_fill_manual(values=as.vector("#8C3F4D")) +
    theme_economist(horizontal = FALSE) +
    theme(text = element_text(color = "#3A3F4A"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
          axis.title = element_blank(),
          plot.title = element_text(face = "bold", size = 14, margin = margin(b = 10), hjust = 0.95),
          plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
          plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 10, color = "#5D646F"),
          strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.5),
          plot.background = element_rect(fill = "#FFFFFF"),
          plot.margin=unit(c(0.1,0.1,0.1,0.05),"cm"),
          legend.position = "none",
          legend.spacing  = unit(0.1, "lines"),
          legend.text  = element_text(size = 10),
          legend.text.align = 0,
          axis.ticks = element_blank()) +
    ggtitle("Female") +
    coord_flip()

  ## Plutting the graphs together together
  # sex.overall <- grid.draw(gtable::gtable_add_grob(arrangeGrob(g, g, ncol=2),
  #                                   rectGrob(gp=gpar(lwd=5, fill=NA)), 1, 1, 1, 2))
  sex.overall <- grid.arrange(gg.female,
                              gg.male,
                              widths=c(0.4,0.5),
                              ncol=2,
                              top = textGrob(title,gp=gpar(fontsize=14)))
  return(sex.overall)
  #return(plot)
}


plot_base_Sex_Sygen <- function(data, title){
  Sygen.sex.long <- data %>%
    dplyr::count(sexcd,YEARDOI_cat) %>%
    dplyr::group_by(YEARDOI_cat) %>%
    dplyr::mutate(frequency = (n/sum(n))*100)

  #------Plot population pyramide for year and color by sex - OVERALL ----
  #Plot data for the male patients
  gg.male <- ggplot(data = subset(Sygen.sex.long, sexcd=='Male'),
                    mapping = aes(
                      x = as.factor(YEARDOI_cat),
                      y = frequency,
                      fill = sexcd,
                      label = paste(round(frequency, 0), "% (", 'n = ', n, ')', sep="")
                    )) +
    geom_bar(stat = "identity") +
    scale_y_continuous('Frequency [%]', limits = c(0, 100)) +
    scale_fill_manual(values=as.vector("#3E606F"))+
    geom_text(hjust=(1.1), size=3.5, colour="#FFFFFF") +
    theme_economist(horizontal = FALSE) +
    theme(text = element_text(color = "#3A3F4A"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
          axis.title = element_blank(),
          plot.title = element_text(face = "bold", size = 14, margin = margin(b = 10), hjust = 0.030),
          plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
          plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
          axis.text.y = element_text(size = 10, color = "#5D646F"),
          axis.text.x = element_text(size = 10, color = "#5D646F"),
          strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.5),
          plot.background = element_rect(fill = "#FFFFFF"),
          plot.margin=unit(c(0.1,0.1,0.1,0.05),"cm"),
          legend.position = "none",
          legend.spacing  = unit(0.1, "lines"),
          legend.text  = element_text(size = 14),
          legend.text.align = 0,
          axis.ticks = element_blank()) +
    ggtitle("Male") +
    coord_flip()
  
  ##Plot data for the female patients
  gg.female <-  ggplot(data = subset(Sygen.sex.long, sexcd=='Female'),
                       mapping = aes(
                         x = as.factor(YEARDOI_cat),
                         y = frequency,
                         fill = sexcd,
                         label=paste(round(frequency, 0), "% (", 'n = ', n, ')', sep="")
                       )) +
    geom_bar(stat = "identity") +
    geom_text(hjust=(1), size=3.5, colour="#5D646F") +
    scale_y_continuous('Frequency [%]', limits = c(100, 0), trans = 'reverse') +
    scale_fill_manual(values=as.vector("#8C3F4D"))+
    theme_economist(horizontal = FALSE) +
    theme(text = element_text(color = "#3A3F4A"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
          axis.title = element_blank(),
          plot.title = element_text(face = "bold", size = 14, margin = margin(b = 10), hjust = 0.95),
          plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
          plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 10, color = "#5D646F"),
          strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.5),
          plot.background = element_rect(fill = "#FFFFFF"),
          plot.margin=unit(c(0.1,0.1,0.1,0.05),"cm"),
          legend.position = "none",
          legend.spacing  = unit(0.1, "lines"),
          legend.text  = element_text(size = 10),
          legend.text.align = 0,
          axis.ticks = element_blank()) +
    ggtitle("Female") +
    coord_flip()
  
  ## Plutting the graphs together together
  sex.overall <- grid.arrange(gg.female,
                              gg.male,
                              widths=c(0.4,0.5),
                              ncol=2,
                              top = textGrob(title,gp=gpar(fontsize=14)))
  return(sex.overall)
  #return(plot)
}


plot_base_Sex_SCI_rehab <- function(data, title){
  SCI_rehab.sex.long <- data %>%
    dplyr::count(Sex,YEARDOI) %>%
    dplyr::group_by(YEARDOI) %>%
    dplyr::mutate(frequency = (n/sum(n))*100)
  
  #------Plot population pyramide for year and color by sex - OVERALL ----
  #Plot data for the male patients
  gg.male <- ggplot(data = subset(SCI_rehab.sex.long, Sex=='Male'),
                    mapping = aes(
                      x = as.factor(YEARDOI),
                      y = frequency,
                      fill = Sex,
                      label = paste(round(frequency, 0), "% (", 'n = ', n, ')', sep="")
                    )) +
    geom_bar(stat = "identity") +
    scale_y_continuous('Frequency [%]', limits = c(0, 100)) +
    scale_fill_manual(values=as.vector("#3E606F"))+
    geom_text(hjust=(1.1), size=3.5, colour="#FFFFFF") +
    theme_economist(horizontal = FALSE) +
    theme(text = element_text(color = "#3A3F4A"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
          axis.title = element_blank(),
          plot.title = element_text(face = "bold", size = 14, margin = margin(b = 10), hjust = 0.030),
          plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
          plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
          axis.text.y = element_text(size = 10, color = "#5D646F"),
          axis.text.x = element_text(size = 10, color = "#5D646F"),
          strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.5),
          plot.background = element_rect(fill = "#FFFFFF"),
          plot.margin=unit(c(0.1,0.1,0.1,0.05),"cm"),
          legend.position = "none",
          legend.spacing  = unit(0.1, "lines"),
          legend.text  = element_text(size = 14),
          legend.text.align = 0,
          axis.ticks = element_blank()) +
    ggtitle("Male") +
    coord_flip()
  
  ##Plot data for the female patients
  gg.female <-  ggplot(data = subset(SCI_rehab.sex.long, Sex=='Female'),
                       mapping = aes(
                         x = as.factor(YEARDOI),
                         y = frequency,
                         fill = Sex,
                         label=paste(round(frequency, 0), "% (", 'n = ', n, ')', sep="")
                       )) +
    geom_bar(stat = "identity") +
    geom_text(hjust=(1), size=3.5, colour="#5D646F") +
    scale_y_continuous('Frequency [%]', limits = c(100, 0), trans = 'reverse') +
    scale_fill_manual(values=as.vector("#8C3F4D"))+
    theme_economist(horizontal = FALSE) +
    theme(text = element_text(color = "#3A3F4A"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
          axis.title = element_blank(),
          plot.title = element_text(face = "bold", size = 14, margin = margin(b = 10), hjust = 0.95),
          plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
          plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 10, color = "#5D646F"),
          strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.5),
          plot.background = element_rect(fill = "#FFFFFF"),
          plot.margin=unit(c(0.1,0.1,0.1,0.05),"cm"),
          legend.position = "none",
          legend.spacing  = unit(0.1, "lines"),
          legend.text  = element_text(size = 10),
          legend.text.align = 0,
          axis.ticks = element_blank()) +
    ggtitle("Female") +
    coord_flip()
  
  ## Plutting the graphs together together
  sex.overall <- grid.arrange(gg.female,
                              gg.male,
                              widths=c(0.4,0.5),
                              ncol=2,
                              top = textGrob(title,gp=gpar(fontsize=14)))
  return(sex.overall)
  #return(plot)
}



plot_base_Sex_EMSCI_paralysis <- function(data, title){
  emsci.sex.long <- data %>%
    dplyr::count(Sex, YEARDOI_cat)%>%
    dplyr::group_by(YEARDOI_cat)%>%
    dplyr::mutate(frequency = (n / sum(n))*100)
  
  #------Plot population pyramide for year and color by sex - OVERALL ----
  ##Plot data for the male patients
  gg.male <- ggplot(data = subset(emsci.sex.long,Sex=='Male'), 
                    mapping = aes(
                      x = as.factor(YEARDOI_cat), 
                      y = frequency, 
                      fill = Sex,
                      label=paste(round(frequency, 0), "% (", 'n = ', n, ')', sep="")
                    )) +
    geom_bar(stat = "identity") +
    scale_y_continuous('Frequency [%]', limits = c(0, 100)) + 
    scale_fill_manual(values=as.vector("#3E606F"))+
    geom_text(hjust=(1.1), size=3.5, colour="#FFFFFF") +
    theme_economist(horizontal = FALSE) +
    theme(text = element_text(color = "#3A3F4A"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
          axis.title = element_blank(),
          plot.title = element_text(face = "bold", size = 14, margin = margin(b = 10), hjust = 0.030),
          plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
          plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
          axis.text.y = element_text(size = 10, color = "#5D646F"),
          axis.text.x = element_text(size = 10, color = "#5D646F"),
          strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.5),
          plot.background = element_rect(fill = "#FFFFFF"),
          plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
          legend.position = "none",
          legend.spacing  = unit(0.1, "lines"),
          legend.text  = element_text(size = 14),
          legend.text.align = 0,
          axis.ticks = element_blank()) +
    ggtitle("Male") + 
    coord_flip()    
  
  ##Plot data for the female patients
  gg.female <-  ggplot(data = subset(emsci.sex.long,Sex=='Female'), 
                       mapping = aes(
                         x = as.factor(YEARDOI_cat), 
                         y = frequency, 
                         fill = Sex,
                         label=paste(round(frequency, 0), "% (", 'n = ', n, ')', sep="")
                       )) +
    geom_bar(stat = "identity") +
    geom_text(hjust=(1), size=3.5, colour="#5D646F") +
    scale_y_continuous('Frequency [%]', limits = c(100, 0), trans = 'reverse') + 
    scale_fill_manual(values=as.vector("#8C3F4D"))+
    theme_economist(horizontal = FALSE) +
    theme(text = element_text(color = "#3A3F4A"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
          axis.title = element_blank(),
          plot.title = element_text(face = "bold", size = 14, margin = margin(b = 10), hjust = 0.95),
          plot.subtitle = element_text(size = 10, margin = margin(b = 20), hjust = 0.030),
          plot.caption = element_text(size = 10, margin = margin(b = 10, t = 50), color = "#5D646F"),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 10, color = "#5D646F"),
          strip.text = element_text(color = "#5D646F", size = 10, face = "bold", hjust = 0.5),
          plot.background = element_rect(fill = "#FFFFFF"),
          plot.margin=unit(c(0.1,0.1,0.1,0.05),"cm"),
          legend.position = "none",
          legend.spacing  = unit(0.1, "lines"),
          legend.text  = element_text(size = 10),
          legend.text.align = 0,
          axis.ticks = element_blank()) +
    ggtitle("Female") + 
    coord_flip()
  
  ## Plutting the graphs together together
  sex.overall <- grid.arrange(gg.female,
                              gg.male,
                              widths=c(0.4,0.4),
                              ncol=2,
                              top = textGrob(title,gp=gpar(fontsize=14)))
  return(sex.overall)
}

plot_base_AIS_EMSCI <- function(data, title){
  emsci.ais.proportions = data %>%
    dplyr::count(YEARDOI_cat,AIS,Sex) %>%
    dplyr::group_by(YEARDOI_cat, Sex)%>% 
    dplyr::mutate(frequency = (n/sum(n))*100)
  
  #----Plot the population pyramide 'Baseline Injury Severity' - OVERALL----
  emsci.ais.plot <-ggplot(data = emsci.ais.proportions, aes(x = YEARDOI_cat, y = frequency, fill = AIS)) +
    geom_bar(data = emsci.ais.proportions %>% filter(Sex == "Male") %>% arrange(rev(YEARDOI_cat)),
             stat = "identity")+
    geom_bar(data = emsci.ais.proportions %>% filter(Sex == "Female") %>% arrange(rev(YEARDOI_cat)),
             stat = "identity",
             mapping = aes(y = -frequency)) +
    coord_flip(clip = "off") +
    scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 10), expand = c(0,0)) +
    #scale_x_discrete(labels = abs, limits = c(2000, 2020), breaks = seq(2001, 2019, 1), expand = c(0,0))+ 
    geom_hline(yintercept = 0) +
    theme_economist(horizontal = FALSE) +
    scale_fill_economist() +
    labs(fill = "", x = "Year of Injury", y = "Proportion of Patients [%]")+ ggtitle(title)+
    theme(axis.title = element_text(size = 12, face = 'bold'), 
          axis.text = element_text(size = 10),
          legend.text = element_text(size=10),
          plot.title = element_text(hjust = 0.5, size = 12),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          plot.background = element_rect(fill = "#FFFFFF"))+
    annotation_custom(grob = textGrob(label = 'Female', gp=gpar(fontsize=10, fontface="bold")), xmin=length(levels(emsci.ais.proportions$YEARDOI_cat)), xmax=length(levels(emsci.ais.proportions$YEARDOI_cat)), ymin=-85, ymax=-85) + 
    annotation_custom(grob = textGrob(label = 'Male', gp=gpar(fontsize=10, fontface="bold")), xmin=length(levels(emsci.ais.proportions$YEARDOI_cat)), xmax=length(levels(emsci.ais.proportions$YEARDOI_cat)), ymin=90, ymax=90)
  return(emsci.ais.plot)
}

plot_base_AIS_Sygen <- function(data, title){
  Sygen.ais.proportions = data %>%
    dplyr::count(YEARDOI_cat, ais1, sexcd) %>%
    dplyr::group_by(YEARDOI_cat, sexcd)%>% 
    dplyr::mutate(frequency = (n / sum(n))*100)
  
  #----Plot the population pyramide 'Baseline Injury Severity' - OVERALL----
  Sygen.ais.plot <-ggplot(data = Sygen.ais.proportions, aes(x = YEARDOI_cat, y = frequency, fill = ais1)) +
    geom_bar(data = Sygen.ais.proportions %>% filter(sexcd == "Male") %>% arrange(rev(YEARDOI_cat)),
             stat = "identity")+
    geom_bar(data = Sygen.ais.proportions %>% filter(sexcd == "Female") %>% arrange(rev(YEARDOI_cat)),
             stat = "identity",
             mapping = aes(y = -frequency)) +
    coord_flip(clip = "off") +
    scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 10), expand = c(0,0)) +
    #scale_x_discrete(labels = abs, limits = c(2000, 2020), breaks = seq(2001, 2019, 1), expand = c(0,0))+ 
    geom_hline(yintercept = 0) +
    theme_economist(horizontal = FALSE) +
    scale_fill_economist() +
    labs(fill = "", x = "Year of Injury", y = "Proportion of Patients [%]")+ ggtitle(title)+
    theme(axis.title = element_text(size = 12, face = 'bold'), 
          axis.text = element_text(size = 10),
          legend.text = element_text(size=10),
          plot.title = element_text(hjust = 0.5, size = 12),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          plot.background = element_rect(fill = "#FFFFFF"))+
    annotation_custom(grob = textGrob(label = 'Female', gp=gpar(fontsize=10, fontface="bold")), xmin=length(levels(Sygen.ais.proportions$YEARDOI_cat)), xmax=length(levels(Sygen.ais.proportions$YEARDOI_cat)), ymin=-85, ymax=-85) + 
    annotation_custom(grob = textGrob(label = 'Male', gp=gpar(fontsize=10, fontface="bold")), xmin=length(levels(Sygen.ais.proportions$YEARDOI_cat)), xmax=length(levels(Sygen.ais.proportions$YEARDOI_cat)), ymin=90, ymax=90)
  return(Sygen.ais.plot)
}

plot_base_AIS_SCI_rehab <- function(data, title){
  data_sub = data[!is.na(data$AIS),]
  SCI_rehab.ais.proportions = data_sub %>%
    dplyr::count(YEARDOI, AIS, Sex) %>%
    dplyr::group_by(YEARDOI, Sex)%>% 
    dplyr::mutate(frequency = (n / sum(n))*100)
  
  #----Plot the population pyramide 'Baseline Injury Severity' - OVERALL----
  SCI_rehab.ais.plot <-ggplot(data = SCI_rehab.ais.proportions, aes(x = as.factor(YEARDOI), y = frequency, fill = AIS)) +
    geom_bar(data = SCI_rehab.ais.proportions %>% filter(Sex == "Male") %>% arrange(rev(as.factor(YEARDOI))),
             stat = "identity")+
    geom_bar(data = SCI_rehab.ais.proportions %>% filter(Sex == "Female") %>% arrange(rev(as.factor(YEARDOI))),
             stat = "identity",
             mapping = aes(y = -frequency)) +
    coord_flip(clip = "off") +
    scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 10), expand = c(0,0)) +
    #scale_x_discrete(labels = abs, limits = c(2000, 2020), breaks = seq(2001, 2019, 1), expand = c(0,0))+ 
    geom_hline(yintercept = 0) +
    theme_economist(horizontal = FALSE) +
    scale_fill_economist() +
    labs(fill = "", x = "Year of Injury", y = "Proportion of Patients [%]")+ ggtitle(title)+
    theme(axis.title = element_text(size = 12, face = 'bold'), 
          axis.text = element_text(size = 10),
          legend.text = element_text(size=10),
          plot.title = element_text(hjust = 0.5, size = 12),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          plot.background = element_rect(fill = "#FFFFFF"))+
    annotation_custom(grob = textGrob(label = 'Female', gp=gpar(fontsize=10, fontface="bold")), xmin=length(levels(SCI_rehab.ais.proportions$YEARDOI)), xmax=length(levels(SCI_rehab.ais.proportions$YEARDOI)), ymin=-85, ymax=-85) + 
    annotation_custom(grob = textGrob(label = 'Male', gp=gpar(fontsize=10, fontface="bold")), xmin=length(levels(SCI_rehab.ais.proportions$YEARDOI)), xmax=length(levels(SCI_rehab.ais.proportions$YEARDOI)), ymin=90, ymax=90)
  return(SCI_rehab.ais.plot)
}


plot_base_NLI_EMSCI <- function(data, title){
  emsci.nli.proportions = data %>%
    dplyr::count(YEARDOI_cat,NLI,Sex) %>%
    dplyr::group_by(YEARDOI_cat,Sex)%>%
    dplyr::mutate(frequency = (n / sum(n))*100)
  
  #Reorder levels
  emsci.nli.proportions$NLI <- factor(emsci.nli.proportions$NLI, levels = c("C1","C2","C3","C4","C5","C6","C7","C8",
                                                                            "T1","T2","T3","T4","T5","T6","T7","T8","T9","T10","T11","T12",
                                                                            "L1","L2","L3","L4","L5"))
  
  myColors <- c('#820400', '#A30500', '#CC0600', '#FF0800', '#FF3933', '#FF3933', '#FF615C', '#FF817D',
                '#D5D5FF', '#CBCAFF', '#BEBDFF', '#AEACFF', '#9A97FF', '#817DFF', '#615CFF', '#3933FF', '#0800FF', '#0600CC', '#0500A3', '#030068',
                '#7E8200', '#9EA300', '#C6CC00', '#F7FF00', '#FCFF97')
  names(myColors) <- levels(emsci.nli.proportions$NLI)
  colScale <- scale_fill_manual(name = "NLI",values = myColors)
  
  
  emsci.nli.plot <- ggplot(data = emsci.nli.proportions, aes(x = YEARDOI_cat, y = frequency, fill = NLI)) +
    geom_bar(data = emsci.nli.proportions %>% filter(Sex == "Male") %>% arrange(rev(YEARDOI_cat)),
             stat = "identity", colour="white")+
    geom_bar(data = emsci.nli.proportions %>% filter(Sex == "Female") %>% arrange(rev(YEARDOI_cat)),
             stat = "identity",
             mapping = aes(y = -frequency), colour="white") +
    coord_flip(clip = "off") +
    scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 20), expand = c(0,0)) +
    #scale_x_continuous(labels = abs, limits = c(2000, 2020), breaks = seq(2001, 2019, 1), expand = c(0,0))+ 
    geom_hline(yintercept = 0) +
    theme_economist(horizontal = FALSE) +
    colScale +
    labs(fill = "", x = "Year of Injury", y = "Proportion of Patients [%]")+ ggtitle(title)+
    theme(axis.title = element_text(size = 12, face = 'bold'), 
          axis.text = element_text(size = 10),
          legend.text = element_text(size=10),
          plot.title = element_text(hjust = 0.5, size = 12),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          plot.background = element_rect(fill = "#FFFFFF"))+
    #guides(colour=guide_legend(nrow=3, ncol=length(levels(emsci.nli.proportions$NLI))%%3, byrow=TRUE)) +
    guides(fill = guide_legend(nrow=3)) +
    #scale_shape_manual(guide = guide_legend(nrow=3)) + 
    annotation_custom(grob = textGrob(label = 'Female', gp=gpar(fontsize=10, fontface="bold")), xmin=length(levels(emsci.nli.proportions$YEARDOI_cat)), xmax=length(levels(emsci.nli.proportions$YEARDOI_cat)), ymin=-85, ymax=-85) + 
    annotation_custom(grob = textGrob(label = 'Male', gp=gpar(fontsize=10, fontface="bold")), xmin=length(levels(emsci.nli.proportions$YEARDOI_cat)), xmax=length(levels(emsci.nli.proportions$YEARDOI_cat)), ymin=90, ymax=90)
  return(emsci.nli.plot)
}

plot_base_NLI_Sygen <- function(data, title){
  Sygen.nli.proportions = data %>%
    dplyr::count(YEARDOI_cat,splvl,sexcd) %>%
    dplyr::group_by(YEARDOI_cat,sexcd)%>%
    dplyr::mutate(frequency = (n / sum(n))*100)
  
  #Reorder levels
  Sygen.nli.proportions$splvl <- factor(Sygen.nli.proportions$splvl, levels = c("C01","C02","C03","C04","C05",
                                                                                "C06","C07","C08",
                                                                                "T01","T02","T03","T04","T05","T06",
                                                                                "T07","T08","T09","T10","T11"))
  myColors <- c('#820400', '#A30500', '#CC0600', '#FF0800', '#FF3933', '#FF3933', '#FF615C', '#FF817D',
                '#D5D5FF', '#CBCAFF', '#BEBDFF', '#AEACFF', '#9A97FF', '#817DFF', '#615CFF', '#3933FF', '#0800FF', '#0600CC', '#0500A3')
  names(myColors) <- levels(Sygen.nli.proportions$splvl)
  colScale <- scale_colour_manual("splvl", values = myColors)
  
  
  Sygen.nli.plot <- ggplot(data = Sygen.nli.proportions, aes(x = YEARDOI_cat, y = frequency, fill = splvl)) +
    geom_bar(data = Sygen.nli.proportions %>% filter(sexcd == "Male") %>% arrange(rev(YEARDOI_cat)),
             stat = "identity", colour="white")+
    geom_bar(data = Sygen.nli.proportions %>% filter(sexcd == "Female") %>% arrange(rev(YEARDOI_cat)),
             stat = "identity",
             mapping = aes(y = -frequency), colour="white") +
    coord_flip(clip = "off") +
    scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 20), expand = c(0,0)) +
    #scale_x_continuous(labels = abs, limits = c(2000, 2020), breaks = seq(2001, 2019, 1), expand = c(0,0))+ 
    geom_hline(yintercept = 0) +
    theme_economist(horizontal = FALSE) +
    scale_fill_manual(values = myColors) +
    labs(fill = "", x = "Year of Injury", y = "Proportion of Patients [%]") + 
    ggtitle(title) +
    theme(axis.title = element_text(size = 12, face = 'bold'), 
          axis.text = element_text(size = 10),
          legend.text = element_text(size=10),
          plot.title = element_text(hjust = 0.5, size = 12),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          plot.background = element_rect(fill = "#FFFFFF")) +
    annotation_custom(grob = textGrob(label = 'Female', gp=gpar(fontsize=10, fontface="bold")), xmin=length(levels(Sygen.nli.proportions$YEARDOI_cat)), xmax=length(levels(Sygen.nli.proportions$YEARDOI_cat)), ymin=-85, ymax=-85) + 
    annotation_custom(grob = textGrob(label = 'Male', gp=gpar(fontsize=10, fontface="bold")), xmin=length(levels(Sygen.nli.proportions$YEARDOI_cat)), xmax=length(levels(Sygen.nli.proportions$YEARDOI_cat)), ymin=90, ymax=90)
  return(Sygen.nli.plot)
}

plot_base_NLI_SCI_rehab <- function(data, title){
  data_sub = data[!is.na(data$NLI),]
  SCI_rehab.nli.proportions = data_sub %>%
    dplyr::count(YEARDOI,NLI,Sex) %>%
    dplyr::group_by(YEARDOI,Sex)%>%
    dplyr::mutate(frequency = (n / sum(n))*100)
  
  #Reorder levels
  SCI_rehab.nli.proportions$NLI <- factor(SCI_rehab.nli.proportions$NLI, levels = c("C01","C02","C03","C04","C05","C06","C07","C08",
                                                                                    "T01","T02","T03","T04","T05","T06","T07","T08","T09","T10","T11", "T12",
                                                                                    "L01", "L02", "L03", "L04", "L05"))
  myColors <- c('#820400', '#A30500', '#CC0600', '#FF0800', '#FF3933', '#FF3933', '#FF615C', '#FF817D',
                '#D5D5FF', '#CBCAFF', '#BEBDFF', '#AEACFF', '#9A97FF', '#817DFF', '#615CFF', '#3933FF', '#0800FF', '#0600CC', '#0500A3', '#030068',
                '#7E8200', '#9EA300', '#C6CC00', '#F7FF00', '#FCFF97')
  names(myColors) <- levels(SCI_rehab.nli.proportions$NLI)
  colScale <- scale_colour_manual("NLI", values = myColors)
  
  
  SCI_rehab.nli.plot <- ggplot(data = SCI_rehab.nli.proportions, aes(x = as.factor(YEARDOI), y = frequency, fill = NLI)) +
    geom_bar(data = SCI_rehab.nli.proportions %>% filter(Sex == "Male") %>% arrange(rev(as.factor(YEARDOI))),
             stat = "identity", colour="white")+
    geom_bar(data = SCI_rehab.nli.proportions %>% filter(Sex == "Female") %>% arrange(rev(as.factor(YEARDOI))),
             stat = "identity",
             mapping = aes(y = -frequency), colour="white") +
    coord_flip(clip = "off") +
    scale_y_continuous(labels = abs, limits = c(-101, 101), breaks = seq(-100, 100, 20), expand = c(0,0)) +
    #scale_x_continuous(labels = abs, limits = c(2000, 2020), breaks = seq(2001, 2019, 1), expand = c(0,0))+ 
    geom_hline(yintercept = 0) +
    theme_economist(horizontal = FALSE) +
    scale_fill_manual(values = myColors) +
    labs(fill = "", x = "Year of Injury", y = "Proportion of Patients [%]") + 
    ggtitle(title) +
    theme(axis.title = element_text(size = 12, face = 'bold'), 
          axis.text = element_text(size = 10),
          legend.text = element_text(size=10),
          plot.title = element_text(hjust = 0.5, size = 12),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          plot.background = element_rect(fill = "#FFFFFF")) +
    annotation_custom(grob = textGrob(label = 'Female', gp=gpar(fontsize=10, fontface="bold")), xmin=length(levels(SCI_rehab.nli.proportions$YEARDOI)), xmax=length(levels(SCI_rehab.nli.proportions$YEARDOI)), ymin=-85, ymax=-85) + 
    annotation_custom(grob = textGrob(label = 'Male', gp=gpar(fontsize=10, fontface="bold")), xmin=length(levels(SCI_rehab.nli.proportions$YEARDOI)), xmax=length(levels(SCI_rehab.nli.proportions$YEARDOI)), ymin=90, ymax=90)
  
  return(SCI_rehab.nli.plot)
}

plot_predict_emsci <- function(data, score){
  
  completeVec <- complete.cases(data[, "ExamStage"])
  data <- data[completeVec, ]
  
  class <- data %>%
    dplyr::count(ExamStage) %>% 
    dplyr::mutate(label = paste0("n = ", n))
  
  plot <- ggplot(data, aes_string(x="ExamStage", y=score)) + 
    geom_jitter() +
    stat_summary(aes_string(y=score, group=1), fun.data = mean_sd, geom = "ribbon", fill = "pink", alpha = 0.6) +
    stat_summary(aes_string(y=score, group=1), fun=mean, colour="red", geom="line", group=1, size = 1) +
    geom_text(data = class, aes(y = 53, label = label)) + 
    ylim(0, 55) +
    theme_economist() +
    theme(axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16),
          axis.title.x = element_text(size=18, face="bold"),
          axis.title.y = element_text(size=18, face="bold"),
          plot.background = element_rect(fill = "#FFFFFF")) +
    xlab('Exam Stage')
  
  return(plot)
}

plot_predict_emsci_NN <- function(data, score, value){
  
  completeVec <- complete.cases(data[, "ExamStage"])
  data <- data[completeVec, ]
  
  down = value-5
  up = value+5
  
  df_temp = data %>%
    filter(
      .data[[score]] %in% c(down:up),
      .data[["ExamStage"]] == "very acute"
    )
  
  ID <- df_temp[['Patientennummer']]
  
  df = data %>%
    filter(
      data[["Patientennummer"]] %in% ID
    )
  
  class <- df %>%
    dplyr::count(ExamStage) %>% 
    dplyr::mutate(label = paste0("n = ", n))
  
  plot <- ggplot(data, aes_string(x="ExamStage", y=score)) + 
    geom_text(data = class, aes(y = 53, label = label), colour = 'blue') + 
    ylim(0, 55) +
    theme_economist() +
    theme(axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16),
          axis.title.x = element_text(size=18, face="bold", vjust=-0.5),
          axis.title.y = element_text(size=18, face="bold"),
          plot.background = element_rect(fill = "#FFFFFF"),
          legend.position = "none") +
    xlab('Exam Stage')
  
  plot_final <- add_line_points(plot, score, df)
  #plot_final2 <- plot_final + 
                 #theme(plot.background = element_rect(fill = "#FFFFFF"))
  if (dim(df)[1] == 0){
    plot_final <- plot_error_line()
  }
  return(plot_final)
}

add_line_points <- function(plot, score, df) {
  plot + 
    geom_jitter(alpha = 0.3) +
    geom_line(aes_string(x="ExamStage", y=score, group='Patientennummer'), colour = 'blue', data = df, show.legend=TRUE) +
   
    stat_summary(aes_string(y=score, group=1), fun.data = mean_sd, geom = "ribbon", fill = "skyblue2", alpha = 0.3, data = df) +
    stat_summary(aes_string(y=score, group=1), fun=mean, colour="blue4", geom="line", group=1, size = 1, data = df)
}

plot_error_data <- function(){
  rects <- data.frame(x = 1:1,
                      colors = c("white"),
                      text = "No data fit all criteria")
  p <- ggplot(rects, aes(x, y = 0, fill = colors, label = text)) +
    geom_tile(width = .25, height = .1) + # make square tiles
    geom_text(color = "black") + # add white text in the middle
    scale_fill_identity(guide = "none") + # color the tiles with the colors in the data frame
    coord_fixed() + # make sure tiles are square
    theme_void() # remove any axis markings
  return(p)
  
}

plot_error_value <- function(){
  rects <- data.frame(x = 1:1,
                      colors = c("white"),
                      text = "Please enter a valid value for patient's score, between 0 and 50")
  p <- ggplot(rects, aes(x, y = 0, fill = colors, label = text)) +
    geom_tile(width = .25, height = .1) + # make square tiles
    geom_text(color = "black") + # add white text in the middle
    scale_fill_identity(guide = "none") + # color the tiles with the colors in the data frame
    coord_fixed() + # make sure tiles are square
    theme_void() # remove any axis markings
  return(p)
  
}

plot_error_line <- function(){
  rects <- data.frame(x = 1:1,
                      colors = c("white"),
                      text = "No patient fit all criteria in our current dataset")
  p <- ggplot(rects, aes(x, y = 0, fill = colors, label = text)) +
    geom_tile(width = .25, height = .1) + # make square tiles
    geom_text(color = "black") + # add white text in the middle
    scale_fill_identity(guide = "none") + # color the tiles with the colors in the data frame
    coord_fixed() + # make sure tiles are square
    theme_void() # remove any axis markings
  return(p)
  
}



