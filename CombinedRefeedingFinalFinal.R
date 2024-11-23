# Refeeding (model) comparisons
{
  # Get ratios and graph
  {
    # Import and curate the data
    ds2<-RefeedingTimesFinal3
    library(plyr)
    library(dplyr)
    ds<-mutate(ds2,Treatment=paste(Line,Water,sep="_"))
    summary(ds)
    attach(ds)
    ds$Date<-as.factor(ds$Date)
    ds$Researcher<-as.factor(ds$Researcher)
    ds$Room<-as.factor(ds$Room)
    ds$Water<-as.factor(ds$Water)
    ds$Line<-as.factor(ds$Line)
    dsfinal<-ds[ds$Minute == '15',]
    dsfinal2<-dsfinal[dsfinal$Day == '4',]
    mean_per_alive <- dsfinal2 %>% group_by(Treatment) %>% 
      dplyr::summarise(Mean=mean(TotalProp), Median=median(TotalProp))
    mean_per_alive
    write.csv(mean_per_alive, "Mutant_Line_Proportion_Survival_Means.csv", quote=FALSE)
    
    # Group by mean
    library(tidyr)
    mean_tbl <- dsfinal2 %>% group_by(Treatment) %>% 
      dplyr::summarise(n = n(), mean=mean(TotalFed),sd=sd(TotalFed))
    mean_tbl
    mean_tbl2 <- mean_tbl %>% as.data.frame()
    mean_tbl2$se <- mean_tbl2[["sd"]]/sqrt(mean_tbl2[["n"]])
    mean_tbl2
    
    # Convert to df
    df2 <- mean_tbl %>% as.data.frame()
    df2 <- df2 %>% 
      separate(Treatment, into = c("Line", "Water"), sep="_")
    df2
    
    library(reshape2)
    df3 <- dcast(df2, Line ~ Water, value.var="mean")
    df3$ratio <- df3[["No"]]/df3[["Yes"]]
    df3$ratiosum <- df3[["No"]]/(df3[["No"]] + df3[["Yes"]])
    df3
    
    df4 <- dcast(df2, Line ~ Water, value.var="sd")
    df4 <- df4 %>% 
      dplyr::rename("sdNo" = "No", "sdYes" = "Yes")
    df4  
    
    df5 <- dcast(df2, Line ~ Water, value.var="n")
    df5 <- df5 %>% 
      dplyr::rename("nNo" = "No", "nYes" = "Yes")
    df5
    
    df6 <- cbind(df3,df4,df5)
    df6$seNo <- df6[["sdNo"]]/sqrt(df6[["nNo"]])
    df6$seYes <- df6[["sdYes"]]/sqrt(df6[["nYes"]])
    attach(df6)
    
    # To propagate ratio error -> (sqrt((seNo/MeanNo)^2+(seYes/MeanYes)^2))*(MeanNo/MeanYes)
    df6$propagatederror <- (sqrt((df6[["seNo"]]/df6[["No"]])^2+(df6[["seYes"]]/df6[["Yes"]])^2)*(df6[["No"]]/df6[["Yes"]]))
    df6 <- df6[, !duplicated(colnames(df6), fromFirst = TRUE)] 
    
    # To generate 95% confidence intervals, when n >20, multiply the standard (propagated) error by 1.96
    df6$uci <- (df6[["ratio"]] + (1.96 * df6[["propagatederror"]]))
    df6$lci <- (df6[["ratio"]] - (1.96 * df6[["propagatederror"]]))
    df6
    write.csv(df6, "Refeeding_Ratios_and_Propagated_Error.csv", quote=FALSE)
    
    # Here is the figure for ratios of means that were not normalized to death
    library(ggplot2)
    FigRat<-ggplot(data=df6, aes(x=reorder(Line, -ratio), y=ratio, group=Line, fill=Line)) +
      geom_point(aes(size = 5, fill = Line)) +
      geom_errorbar(aes(ymin=lci, ymax=uci),width=0.2)+
      xlab("\nMosquito Line") +
      ylab("Mosquito Bite Ratio (Dry Mean/Wet Mean)\n") +
      theme_classic() +
      theme(legend.position = c(1.05, 1.95)) +
      scale_y_continuous(position = "right", expand = c(0,0), 
                         breaks = c(0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.1), 
                         limits = c(0.49, 2.11)) +
      theme(axis.text.x=element_text(size=14,color="black"), axis.title=element_text(size=18,color="black")) +
      theme(axis.text.y=element_text(size=14,color="black"), axis.title=element_text(size=18,color="black")) +
      theme(panel.background=element_rect(fill="white",color="white")) +
      theme(axis.ticks=(element_line(color="black")),axis.line=(element_line(color="black")))  
    FigRat
    ggsave(filename="Refeeding_Ratios_For_Combining_Final.png",units=c("in"),width=11,height=6,plot=FigRat)
    ggsave(filename="Refeeding_Ratios_For_Combining_Final.pdf",units=c("in"),width=11,height=6,plot=FigRat)
    
    # The figure to compare overall feeding per alive mosquitoes compounded daily
    
    # Compare groups
    ratio_order <- c('WT_Yes','WT_No','IR93a-YFP_Yes','IR93a-YFP_No',
                     'ORCO16_Yes','ORCO16_No','IR8d_Yes','IR8d_No',
                     'IR93a-RFP_Yes','IR93a-RFP_No','ORCO5_Yes','ORCO5_No',
                     'IR8a_Yes','IR8a_No','ORCO2_Yes','ORCO2_No',
                     'GR3-4_Yes','GR3-4_No','GR3-ECFP_Yes','GR3-ECFP_No')
    
    FigTotProp<-ggplot() +
      geom_boxplot(data=dsfinal2, 
                   aes(x=factor(Treatment, level=ratio_order), 
                       y=TotalProp, fill=Line), outlier.shape = NA) +
      geom_jitter(data=dsfinal2, 
                  aes(x=factor(Treatment, level=ratio_order), 
                      y=TotalProp, fill=Line), width = 0.1) +
      xlab("\nMosquito Line") +
      ylab("Total bites/total alive mosquitoes (accrued daily)\n") +
      theme_classic() +
      theme(legend.position = c(1.05, 1.95)) +
      scale_y_continuous(expand = c(0,0), 
                         limits = c(0,1.05),
                         breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
      theme(axis.text.x=element_text(hjust= 1.2,angle=45,size=14,color="black"), 
            axis.title=element_text(size=18,color="black")) +
      theme(axis.text.y=element_text(size=14,color="black"), 
            axis.title=element_text(size=18,color="black")) +
      theme(panel.background=element_rect(fill="white",color="white")) +
      theme(axis.ticks=(element_line(color="black")),
            axis.line=(element_line(color="black")))  
    FigTotProp
    ggsave(filename="Refeeding_TotalProp_Bar_For_Combining_Final.png",units=c("in"),width=11,height=7,plot=FigTotProp)
    ggsave(filename="Refeeding_TotalProp_Bar_For_Combining_Final.pdf",units=c("in"),width=11,height=7,plot=FigTotProp)
  }
  
  # Get models and compare
  {
    # Create the GLMER model
    library(lme4)
    library(emmeans)
    model9 <- glm(TotalProp ~ Treatment + Day, data=dsfinal, family = "quasibinomial")
    plot(fitted(model9), residuals(model9, type = "response"))
    plot(fitted(model9), residuals(model9, type = "pearson"))
    qqnorm(residuals(model9))
    qqline(residuals(model9))
    options(max.print = 10000)
    outdaily<-emmeans(model9, pairwise ~ Treatment)
    show(outdaily)
    print
    
    #Save emmeans and all contrasts to default directory
    write.csv(outdaily$emmeans, "Refeeding_daily_emmeans_final.csv", quote=FALSE)
    write.csv(outdaily$contrasts, "Refeeding_daily_contrasts_final.csv", quote=FALSE)
  }
}

# Medium cage bite and egg comparisons
{
  # Import and manage datasets
  ds <- RefeedingBigFinal
  attach(ds)
  
  #Set factor and numeric
  ds$Exposure<-as.factor(ds$Exposure)
  ds$Factor<-as.factor(ds$Factor)
  ds$Amount<-as.numeric(ds$Amount)
  ds
  
  #Split data set into factor groups
  ds2<-dlply(ds,.(Factor))
  dsB<-ds2$'Bites'
  dsB
  dsR<-ds2$'Refeed'
  dsR
  dsEB<-ds2$'EggsBite'
  dsEB
  dsE<-ds2$'Eggs'
  dsE
  dsEMBM<-ds2$'EMBM'
  dsEMBM
  dsBiteMos<-ds2$'BiteMos'
  dsBiteMos
  dsRefeedMos<-ds2$'RefeedMos'
  dsRefeedMos
  dsEMRM<-ds2$'EMRM'
  dsEMRM
  
  #If you only have 2 samples, use this, otherwise, use aov below
  dsBt <- t.test(Amount ~ Exposure, var.equal=TRUE, data = dsB)
  dsBt
  dsRt <- t.test(Amount ~ Exposure, var.equal=TRUE, data = dsR)
  dsRt
  dsEBt <- t.test(Amount ~ Exposure, var.equal=TRUE, data = dsEB)
  dsEBt
  dsEt <- t.test(Amount ~ Exposure, var.equal=TRUE, data = dsE)
  dsEt
  
  #Use datasheets to run ANOVAs and export Tukey HSDs
  {
    dsBaov <- aov(Amount ~ Exposure, data = dsB)
    summary(dsBaov)
    dsRaov <- aov(Amount ~ Exposure, data = dsR)
    summary(dsRaov)
    dsEBaov <- aov(Amount ~ Exposure, data = dsEB)
    summary(dsEBaov)
    dsEaov <- aov(Amount ~ Exposure, data = dsE)
    summary(dsEaov)
    dsEMBMaov <- aov(Amount ~ Exposure, data = dsEMBM)
    summary(dsEMBMaov)
    dsBiteMosaov <- aov(Amount ~ Exposure, data = dsBiteMos)
    summary(dsBiteMosaov)
    dsRefeedMosaov <- aov(Amount ~ Exposure, data = dsRefeedMos)
    summary(dsRefeedMosaov)
    dsEMRMaov <- aov(Amount ~ Exposure, data = dsEMRM)
    summary(dsEMRMaov)
    
    #Finish with Tukey Post-Hoc Analyses
    TB<-TukeyHSD(dsBaov)
    TR<-TukeyHSD(dsRaov)
    TEB<-TukeyHSD(dsEBaov)
    TE<-TukeyHSD(dsEaov)
    TEMBM<-TukeyHSD(dsEMBMaov)
    TBM<-TukeyHSD(dsBiteMosaov)
    TRM<-TukeyHSD(dsRefeedMosaov)
    TEMRM<-TukeyHSD(dsEMRMaov)
    
    #Save all Tukeys to default directory
    write.csv(TB$Exposure, "THSDBite.csv", quote=FALSE)
    write.csv(TR$Exposure, "THSDRefeed.csv", quote=FALSE)
    write.csv(TEB$Exposure, "THSDEggsPerTotalBite.csv", quote=FALSE)
    write.csv(TE$Exposure, "THSDEggs.csv", quote=FALSE)
    write.csv(TEMBM$Exposure, "THSDEggsPerMosBite.csv", quote=FALSE)
    write.csv(TBM$Exposure, "THSDBitePerMos.csv", quote=FALSE)
    write.csv(TRM$Exposure, "THSDRefeedPerMos.csv", quote=FALSE)
    write.csv(TEMRM$Exposure, "THSDEggsPerRefedMos.csv", quote=FALSE)
  }
  
  #Create feeding, refeeding, and egglaying plots using timecourse data
  {
    #Import feeding and egglaying data
    dsRef <- RefeedingBigTotalBites
    dsEgg <- RefeedingEggs
    attach(dsRef)
    
    #Set factor and numeric
    dsRef$RH<-as.factor(dsRef$RH)
    dsRef$H2O<-as.factor(dsRef$H2O)
    dsRef$Fed<-as.numeric(dsRef$Fed)
    dsRef
    
    dsEgg$RH<-as.factor(dsEgg$RH)
    dsEgg$H2O<-as.factor(dsEgg$H2O)
    dsEgg$Eggs<-as.numeric(dsEgg$Eggs)
    dsEgg
    
    TotRef <- dsRef %>% mutate(dsRef,Treatment = paste(RH,H2O,sep="_"))
    TotRef$Treatment <- as.factor(TotRef$Treatment) 
    TotRef
    summary(TotRef)
    
    TotEgg <- dsEgg %>% mutate(dsEgg,Treatment = paste(RH,H2O,sep="_"))
    TotEgg$Treatment <- as.factor(TotEgg$Treatment) 
    TotEgg
    summary(TotEgg)
    
    #Geom_smooth uses stats::loess() for < 1,000 observations; otherwise mgcv::gam()
    DS1<-ggplot(TotRef, aes(x=Time, y=Fed, group=Treatment)) +
      geom_smooth(aes(x=Time, y=Fed))
    DS1
    
    DS2<-ggplot(TotRef, aes(x=Time, y=Refed, group=Treatment)) +
      geom_smooth(aes(x=Time, y=Refed))
    DS2
    
    DS3<-ggplot(TotEgg, aes(x=Time, y=Eggs, group=Treatment)) +
      geom_smooth(aes(x=Time, y=Eggs))
    DS3
    
    #Make a color palette
    TreatColor = c('33_No'='grey30','33_Yes'='grey50','75_No'='grey70','75_Yes'='grey90')
    
    G1<-ggplot(TotRef,aes(x=Time, y=Fed, color=Treatment)) +
      geom_smooth(aes(x=Time, y=Fed)) +
      xlab("\nTime (hours)") +
      ylab("Mosquitoes Fed\n") +
      theme_classic() +
      theme(legend.position = c(0.25, 0.8)) +
      scale_x_continuous(expand = c(0,0), breaks = seq(0, 72, by = 12)) +
      scale_y_continuous(expand = c(0,0)) +
      coord_cartesian(ylim=c(0,30),xlim=c(0,73)) +
      theme(axis.text.x=element_text(size=14,color="black"), axis.title=element_text(size=18,color="black")) +
      theme(axis.text.y=element_text(size=14,color="black"), axis.title=element_text(size=18,color="black")) +
      guides(guide_legend(title="Treatment", override.aes = list(size=.2))) +
      theme(panel.background=element_rect(fill="white",color="white")) +
      theme(axis.ticks=(element_line(color="black")),axis.line=(element_line(color="black"))) + 
      scale_color_manual(values=TreatColor) 
    G1
    ggsave(filename="Feeding_Smooth.png",units=c("in"),width=9,height=7,plot=G1)
    ggsave(filename="Feeding_Smooth.pdf",units=c("in"),width=9,height=7,plot=G1)   
  }
}

# Weight comparisons 
{
  my_dataset <- Weight
  attach(my_dataset)
  my_dataset
  
  #Separating into Anopheles and Aedes
  CP<-dlply(my_dataset,.(Species))
  Anopheles<-CP$'Anopheles'
  Aedes<-CP$'Aedes'
  
  Anopheles1<-mutate(Anopheles,Treatment = paste(Treatment,Sep="_",Day))
  Aedes1<-mutate(Aedes,Treatment = paste(Treatment,Sep="_",Day))
  
  #ANOVA weight comparisons between groups
  anopheles.aov <- aov(Weight ~ Treatment, data = Anopheles1)
  aedes.aov <- aov(Weight ~ Treatment, data = Aedes1)
  
  #Summary of the analyses
  summary(anopheles.aov)
  summary(aedes.aov)
  
  #Tukey post-hoc for all comparisons
  TAn<-TukeyHSD(anopheles.aov)
  TAe<-TukeyHSD(aedes.aov)
  TAn
  TAe
  write.csv(TAn$Treatment, "Refeeding_Weight_Anopheles.csv", quote=FALSE)
  write.csv(TAe$Treatment, "Refeeding_Weight_Aedes.csv", quote=FALSE)
  
  # Plot
  level_order <- c('None','Wet','Dry','Dry_Blood','Blood')
  TreatColor <- c("None"="grey20","Wet"="grey40","Dry"="grey60","Dry_Blood"="grey80","Blood"="grey100")
  
  Plot1<-ggplot(Aedes,aes(x=Treatment, y=Weight)) +
    geom_boxplot(aes(x=factor(Treatment, level=level_order),fill=Treatment)) +
    geom_jitter(width=0.1) + 
    facet_grid(. ~ Day, drop = TRUE, scales = "free", space = "free_x") +
    xlab("\nTreatment") +
    ylab("Mosquito Weight (mg)\n") +
    theme_classic() +
    scale_y_continuous(expand = c(0,0), limits = c(0,6), breaks = c(0,1,2,3,4,5,6)) +
    theme(strip.text = element_text(size = 14)) +
    theme(axis.text.y=element_text(size=14,color="black"), axis.title=element_text(size=18,color="black")) +
    theme(axis.text.x=element_text(size=14,color="black",angle=45,hjust=1), axis.title=element_text(size=15,color="black")) +
    theme(panel.background=element_rect(fill="white",color="white"))+
    theme(legend.text=(element_text(size=10)))+
    guides(fill=FALSE) + #this line removes the legend completely
    theme(axis.text=(element_text(size=12,color="black")),axis.title=(element_text(size=14,color="black")))+
    theme(axis.ticks=(element_line(color="black")),axis.line=(element_line(color="black"))) +
    scale_fill_manual(values=TreatColor)
  Plot1
  ggsave(filename="Refeeding_Weights_Aedes.png",units=c("in"),width=11,height=7,plot=Plot1)
  ggsave(filename="Refeeding_Weights_Aedes.pdf",units=c("in"),width=11,height=7,plot=Plot1)
  
  Plot2<-ggplot(Anopheles,aes(x=Treatment, y=Weight)) +
    geom_boxplot(aes(x=factor(Treatment, level=level_order),fill=Treatment)) +
    geom_jitter(width=0.1) + 
    facet_grid(. ~ Day, drop = TRUE, scales = "free", space = "free_x") +
    xlab("\nTreatment") +
    ylab("Mosquito Weight (mg)\n") +
    theme_classic() +
    scale_y_continuous(expand = c(0,0), limits = c(0,6), breaks = c(0,1,2,3,4,5,6)) +
    theme(strip.text = element_text(size = 14)) +
    theme(axis.text.y=element_text(size=14,color="black"), axis.title=element_text(size=18,color="black")) +
    theme(axis.text.x=element_text(size=14,color="black",angle=45,hjust=1), axis.title=element_text(size=15,color="black")) +
    theme(panel.background=element_rect(fill="white",color="white"))+
    theme(legend.text=(element_text(size=10)))+
    guides(fill=FALSE) + #this line removes the legend completely
    theme(axis.text=(element_text(size=12,color="black")),axis.title=(element_text(size=14,color="black")))+
    theme(axis.ticks=(element_line(color="black")),axis.line=(element_line(color="black"))) +
    scale_fill_manual(values=TreatColor)
  Plot2
  ggsave(filename="Refeeding_Weights_Anopheles.png",units=c("in"),width=11,height=7,plot=Plot2)
  ggsave(filename="Refeeding_Weights_Anopheles.pdf",units=c("in"),width=11,height=7,plot=Plot2)
}

# Ovary width comparisons
{
  my_dataset <- OvaryWidth
  attach(my_dataset)
  my_dataset
  
  #Separating into Anopheles and Aedes
  CP<-dlply(my_dataset,.(Species))
  Anopheles<-CP$'Anopheles'
  Aedes<-CP$'Aedes'
  
  Anopheles1<-mutate(Anopheles,Treatment = paste(Treatment,Sep="_",Day))
  Aedes1<-mutate(Aedes,Treatment = paste(Treatment,Sep="_",Day))
  
  #ANOVA ovary size comparisons between groups
  anopheles.aov <- aov(Width ~ Treatment, data = Anopheles1)
  aedes.aov <- aov(Width ~ Treatment, data = Aedes1)
  
  #Summary of the analyses
  summary(anopheles.aov)
  summary(aedes.aov)
  
  #Tukey post-hoc for all comparisons
  TAn<-TukeyHSD(anopheles.aov)
  TAe<-TukeyHSD(aedes.aov)
  TAn
  TAe
  write.csv(TAn$Treatment, "Refeeding_Ovary_Width_Anopheles.csv", quote=FALSE)
  write.csv(TAe$Treatment, "Refeeding_Ovary_Width_Aedes.csv", quote=FALSE)
  
  # Plot
  level_order <- c('None','Wet','Dry','Dry_Blood','Dry_Day2_Blood')
  TreatColor <- c("None"="grey30","Wet"="grey50","Dry"="grey70","Dry_Blood"="grey90","Dry_Day2_Blood"="grey100")
  
  Plot1<-ggplot(Aedes,aes(x=Treatment, y=Width)) +
    geom_boxplot(aes(x=factor(Treatment, level=level_order),fill=Treatment)) +
    geom_jitter(width=0.1) +
    facet_grid(. ~ Day, drop = TRUE, scales = "free", space = "free_x") +
    xlab("\nTreatment") +
    ylab("Ovary Width (mm)\n") +
    theme_classic() +
    scale_y_continuous(expand = c(0,0), limits = c(0,1.0), breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)) +
    theme(strip.text = element_text(size = 14)) +
    theme(axis.text.y=element_text(size=14,color="black"), axis.title=element_text(size=18,color="black")) +
    theme(axis.text.x=element_text(size=14,color="black",angle=45,hjust=1), axis.title=element_text(size=15,color="black")) +
    theme(panel.background=element_rect(fill="white",color="white"))+
    theme(legend.text=(element_text(size=10)))+
    guides(fill=FALSE) + #this line removes the legend completely
    theme(axis.text=(element_text(size=12,color="black")),axis.title=(element_text(size=14,color="black")))+
    theme(axis.ticks=(element_line(color="black")),axis.line=(element_line(color="black"))) +
    scale_fill_manual(values=TreatColor)
  Plot1
  ggsave(filename="Refeeding_Ovary_Width_Aedes.png",units=c("in"),width=11,height=7,plot=Plot1)
  ggsave(filename="Refeeding_Ovary_Width_Aedes.pdf",units=c("in"),width=11,height=7,plot=Plot1)
  
  Plot2<-ggplot(Anopheles,aes(x=Treatment, y=Width)) +
    geom_boxplot(aes(x=factor(Treatment, level=level_order),fill=Treatment)) +
    geom_jitter(width=0.1) +
    facet_grid(. ~ Day, drop = TRUE, scales = "free", space = "free_x") +
    xlab("\nTreatment") +
    ylab("Ovary Width (mm)\n") +
    theme_classic() +
    scale_y_continuous(expand = c(0,0), limits = c(0,1.0), breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)) +
    theme(strip.text = element_text(size = 14)) +
    theme(axis.text.y=element_text(size=14,color="black"), axis.title=element_text(size=18,color="black")) +
    theme(axis.text.x=element_text(size=14,color="black",angle=45,hjust=1), axis.title=element_text(size=15,color="black")) +
    theme(panel.background=element_rect(fill="white",color="white"))+
    theme(legend.text=(element_text(size=10)))+
    guides(fill=FALSE) + #this line removes the legend completely
    theme(axis.text=(element_text(size=12,color="black")),axis.title=(element_text(size=14,color="black")))+
    theme(axis.ticks=(element_line(color="black")),axis.line=(element_line(color="black"))) +
    scale_fill_manual(values=TreatColor)
  Plot2
  ggsave(filename="Refeeding_Ovary_Width_Anopheles.png",units=c("in"),width=11,height=7,plot=Plot2)  
  ggsave(filename="Refeeding_Ovary_Width_Anopheles.pdf",units=c("in"),width=11,height=7,plot=Plot2)
}

# Ovary count comparisons
{
  my_dataset <- OvaryCount
  attach(my_dataset)
  my_dataset
  
  #Separating into Anopheles and Aedes
  CP<-dlply(my_dataset,.(Species))
  Anopheles<-CP$'Anopheles'
  Aedes<-CP$'Aedes'
  
  Anopheles1<-mutate(Anopheles,Treatment = paste(Treatment,Sep="_",Day))
  Aedes1<-mutate(Aedes,Treatment = paste(Treatment,Sep="_",Day))
  
  #ANOVA ovary size comparisons between groups
  anopheles.aov <- aov(Count ~ Treatment, data = Anopheles1)
  aedes.aov <- aov(Count ~ Treatment, data = Aedes1)
  
  #Summary of the analyses
  summary(anopheles.aov)
  summary(aedes.aov)
  
  #Tukey post-hoc for all comparisons
  TAn<-TukeyHSD(anopheles.aov)
  TAe<-TukeyHSD(aedes.aov)
  TAn
  TAe
  write.csv(TAn$Treatment, "Refeeding_Ovary_Count_Anopheles.csv", quote=FALSE)
  write.csv(TAe$Treatment, "Refeeding_Ovary_Count_Aedes.csv", quote=FALSE)
  
  # Plot
  level_order <- c('None','Wet','Dry','Dry_Blood','Dry_Day2_Blood')
  TreatColor <- c("None"="grey30","Wet"="grey50","Dry"="grey70","Dry_Blood"="grey90","Dry_Day2_Blood"="grey100")
  
  Plot1<-ggplot(Aedes,aes(x=Treatment, y=Count)) +
    geom_boxplot(aes(x=factor(Treatment, level=level_order),fill=Treatment)) +
    geom_jitter(width=0.1) +
    facet_grid(. ~ Day, drop = TRUE, scales = "free", space = "free_x") +
    xlab("\nTreatment") +
    ylab("Ovary Count\n") +
    theme_classic() +
    scale_y_continuous(expand = c(0,0), limits = c(0,80), breaks = c(0,10,20,30,40,50,60,70,80)) +
    theme(strip.text = element_text(size = 14)) +
    theme(axis.text.y=element_text(size=14,color="black"), axis.title=element_text(size=18,color="black")) +
    theme(axis.text.x=element_text(size=14,color="black",angle=45,hjust=1), axis.title=element_text(size=15,color="black")) +
    theme(panel.background=element_rect(fill="white",color="white"))+
    theme(legend.text=(element_text(size=10)))+
    guides(fill=FALSE) + #this line removes the legend completely
    theme(axis.text=(element_text(size=12,color="black")),axis.title=(element_text(size=14,color="black")))+
    theme(axis.ticks=(element_line(color="black")),axis.line=(element_line(color="black"))) +
    scale_fill_manual(values=TreatColor)
  Plot1
  ggsave(filename="Refeeding_Ovary_Count_Aedes.png",units=c("in"),width=11,height=7,plot=Plot1)
  ggsave(filename="Refeeding_Ovary_Count_Aedes.pdf",units=c("in"),width=11,height=7,plot=Plot1)
  
  Plot2<-ggplot(Anopheles,aes(x=Treatment, y=Count)) +
    geom_boxplot(aes(x=factor(Treatment, level=level_order),fill=Treatment)) +
    geom_jitter(width=0.1) +
    facet_grid(. ~ Day, drop = TRUE, scales = "free", space = "free_x") +
    xlab("\nTreatment") +
    ylab("Ovary Count\n") +
    theme_classic() +
    scale_y_continuous(expand = c(0,0), limits = c(0,80), breaks = c(0,10,20,30,40,50,60,70,80)) +
    theme(strip.text = element_text(size = 14)) +
    theme(axis.text.y=element_text(size=14,color="black"), axis.title=element_text(size=18,color="black")) +
    theme(axis.text.x=element_text(size=14,color="black",angle=45,hjust=1), axis.title=element_text(size=15,color="black")) +
    theme(panel.background=element_rect(fill="white",color="white"))+
    theme(legend.text=(element_text(size=10)))+
    guides(fill=FALSE) + #this line removes the legend completely
    theme(axis.text=(element_text(size=12,color="black")),axis.title=(element_text(size=14,color="black")))+
    theme(axis.ticks=(element_line(color="black")),axis.line=(element_line(color="black"))) +
    scale_fill_manual(values=TreatColor)
  Plot2
  ggsave(filename="Refeeding_Ovary_Count_Anopheles.png",units=c("in"),width=11,height=7,plot=Plot2)
  ggsave(filename="Refeeding_Ovary_Count_Anopheles.pdf",units=c("in"),width=11,height=7,plot=Plot2)
}

# Three-box choice assays
{
  # Chi square
  dst<-ThreeBox
  datchi<-ThreeBoxChi
  dst
  # Separating to only look at daily totals (=30 mins)
  dst2<-mutate(dst,Combined=paste(Line,Sep="_",Treatment,Sep="_",Host))
  dst3<-dst2[dst2$Time == '30',]
  
  datchi2<-mutate(datchi,Combined=paste(Line,Sep="_",Treatment,Sep="_",Host))
  write.csv(datchi2, "Three_Box_ChiSquared_Aedes.csv", quote=FALSE)
  
  # Compare groups
  combined_order <- c('WT _ Control _ Human','WT _ Dry _ Human','GR34 _ Control _ Human','GR34 _ Dry _ Human',
                      'GR3ecfp _ Control _ Human','GR3ecfp _ Dry _ Human',
                      'WT _ Control _ Hemotek_no_CO2','WT _ Dry _ Hemotek_no_CO2',
                      'WT _ Control _ Hemotek_CO2','WT _ Dry _ Hemotek_CO2',
                      'WT _ Control _ Hemotek_CO2_butanal','WT _ Dry _ Hemotek_CO2_butanal')
  
  P1<-ggplot() +
    geom_boxplot(data=dst3,aes(fill=Host,x=factor(Combined, level=combined_order),y=Proportion)) + 
    geom_dotplot(data=dst3,binaxis="y",stackdir="center",dotsize=.4,aes(x=factor(Combined, level=combined_order),y=Proportion)) +
    xlab("\nTreatment") +
    ylab("Refeeding Proportion\n") +
    theme_classic() +
    scale_y_continuous(expand = c(0,0), limits = c(0,1.01), breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0)) +
    theme(strip.text = element_text(size = 14)) +
    theme(axis.text.y=element_text(size=14,color="black"), axis.title=element_text(size=18,color="black")) +
    theme(axis.text.x=element_text(size=14,color="black",angle=45,hjust=1), axis.title=element_text(size=15,color="black")) +
    theme(panel.background=element_rect(fill="white",color="white"))+
    theme(legend.text=(element_text(size=10)))+
    guides(fill=FALSE) + #this line removes the legend completely
    theme(axis.text=(element_text(size=12,color="black")),axis.title=(element_text(size=14,color="black")))+
    theme(axis.ticks=(element_line(color="black")),axis.line=(element_line(color="black"))) 
  P1
  ggsave(filename="ThreeBox_Refeeding_Aedes_Final.png",units=c("in"),width=11,height=7,plot=P1)
  ggsave(filename="ThreeBox_Refeeding_Aedes_Final.pdf",units=c("in"),width=11,height=7,plot=P1)  
  
  # Now complete Chi-Square analysis
  datchi3<-matrix(c(6,30,8,10,9,11,5,15,4,6,1,5,35,12,26,32,20,32,22,17,26,24,22,25),nrow=12,ncol=2,
                  dimnames = list(c('WT _ Control _ Human','WT _ Dry _ Human','GR34 _ Control _ Human','GR34 _ Dry _ Human',
                                    'GR3ecfp _ Control _ Human','GR3ecfp _ Dry _ Human',
                                    'WT _ Control _ Hemotek_CO2','WT _ Dry _ Hemotek_CO2',
                                    'WT _ Control _ Hemotek_no_CO2','WT _ Dry _ Hemotek_no_CO2',
                                    'WT _ Control _ Hemotek_CO2_butanal','WT _ Dry _ Hemotek_CO2_butanal'),
                                  c("Refed", "NonRefed")))
  datchi3
  chisq<-chisq.test(datchi3)
  chisq
  library(rcompanion)
  pwchisq<-pairwiseNominalIndependence(datchi3,
                                       compare = "row",
                                       fisher = FALSE,
                                       gtest  = FALSE,
                                       chisq  = TRUE,
                                       method = "fdr",
                                       digits = 3)
  pwchisq
  write.csv(pwchisq, "Threebox_Refeeding_Pairwise_ChiSquared.csv", quote=FALSE)
}

# Small cage refeeding comparisons
{
  # Chi square again
  dsts<-SmallCage
  datschi<-SmallCageChi
  
  # Separating into Anopheles and Aedes
  dsts2<-dlply(dsts,.(Species))
  SmAnopheles<-dsts2$'Anopheles'
  SmAedes<-dsts2$'Aedes'
  SmAedes2<-mutate(SmAedes,Combined=paste(Line,Sep="_",Treatment,Sep="_",Day))
  SmAnopheles2<-mutate(SmAnopheles,Combined=paste(Line,Sep="_",Treatment,Sep="_",Day))
  datschi2<-dlply(datschi,.(Species))
  SmChiAno<-datschi2$'Anopheles'
  SmChiAed<-datschi2$'Aedes'
  datschiano2<-mutate(SmChiAno,Combined=paste(Line,Sep="_",Treatment,Sep="_",Day))
  datschiaed2<-mutate(SmChiAed,Combined=paste(Line,Sep="_",Treatment,Sep="_",Day))
  write.csv(datschiaed2, "Small_Cage_ChiSquared_Aedes.csv", quote=FALSE)
  write.csv(datschiano2, "Small_Cage_ChiSquared_Anopheles.csv", quote=FALSE)
  
  # Compare groups
  combined_order1 <- c('WT _ Wet _ 1','WT _ Dry _ 1','WT _ Wet _ 2','WT _ Dry _ 2','WT _ Wet _ 3','WT _ Dry _ 3')
  
  PAe<-ggplot() +
    geom_col(data=datschiaed2,aes(fill=Day,x=factor(Combined, level=combined_order1),y=Proportion)) + 
    xlab("\nTreatment") +
    ylab("Refeeding Proportion\n") +
    theme_classic() +
    scale_y_continuous(expand = c(0,0), limits = c(0,1), breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0)) +
    theme(strip.text = element_text(size = 14)) +
    theme(axis.text.y=element_text(size=14,color="black"), axis.title=element_text(size=18,color="black")) +
    theme(axis.text.x=element_text(size=14,color="black",angle=45,hjust=1), axis.title=element_text(size=15,color="black")) +
    theme(panel.background=element_rect(fill="white",color="white"))+
    theme(legend.text=(element_text(size=10)))+
    guides(fill=FALSE) + #this line removes the legend completely
    theme(axis.text=(element_text(size=12,color="black")),axis.title=(element_text(size=14,color="black")))+
    theme(axis.ticks=(element_line(color="black")),axis.line=(element_line(color="black"))) 
  PAe
  ggsave(filename="Small_Cage_Refeeding_Aedes.png",units=c("in"),width=11,height=7,plot=PAe)
  ggsave(filename="Small_Cage_Refeeding_Aedes.pdf",units=c("in"),width=11,height=7,plot=PAe)  
  
  # For Anopheles
  PAn<-ggplot() +
    geom_col(data=datschiano2,aes(fill=Day,x=factor(Combined, level=combined_order1),y=Proportion)) + 
    xlab("\nTreatment") +
    ylab("Refeeding Proportion\n") +
    theme_classic() +
    scale_y_continuous(expand = c(0,0), limits = c(0,1), breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0)) +
    theme(strip.text = element_text(size = 14)) +
    theme(axis.text.y=element_text(size=14,color="black"), axis.title=element_text(size=18,color="black")) +
    theme(axis.text.x=element_text(size=14,color="black",angle=45,hjust=1), axis.title=element_text(size=15,color="black")) +
    theme(panel.background=element_rect(fill="white",color="white"))+
    theme(legend.text=(element_text(size=10)))+
    guides(fill=FALSE) + #this line removes the legend completely
    theme(axis.text=(element_text(size=12,color="black")),axis.title=(element_text(size=14,color="black")))+
    theme(axis.ticks=(element_line(color="black")),axis.line=(element_line(color="black"))) 
  PAn
  ggsave(filename="Small_Cage_Refeeding_Anopheles.png",units=c("in"),width=11,height=7,plot=PAn)
  ggsave(filename="Small_Cage_Refeeding_Anopheles.pdf",units=c("in"),width=11,height=7,plot=PAn)
  
  # Now complete Chi-Square analysis
  datschiaed3<-matrix(c(2,6,4,34,1,16,30,30,28,10,21,10),nrow=6,ncol=2,
                      dimnames = list(c('WT _ Wet _ 1','WT _ Dry _ 1','WT _ Wet _ 2','WT _ Dry _ 2','WT _ Wet _ 3','WT _ Dry _ 3'),
                                      c("Refed", "NonRefed")))
  datschiaed3
  chisqaed<-chisq.test(datschiaed3)
  chisqaed
  pwchisqaed<-pairwiseNominalIndependence(datschiaed3,
                                          compare = "row",
                                          fisher = FALSE,
                                          gtest  = FALSE,
                                          chisq  = TRUE,
                                          method = "fdr",
                                          digits = 3)
  pwchisqaed
  write.csv(pwchisqaed, "Small_Cage_Refeeding_Pairwise_ChiSquared_Aedes.csv", quote=FALSE)
  
  # For Anopheles
  datschiano3<-matrix(c(1,4,3,17,2,17,25,22,17,7,28,17),nrow=6,ncol=2,
                      dimnames = list(c('WT _ Wet _ 1','WT _ Dry _ 1','WT _ Wet _ 2','WT _ Dry _ 2','WT _ Wet _ 3','WT _ Dry _ 3'),
                                      c("Refed", "NonRefed")))
  datschiano3
  chisqano<-chisq.test(datschiano3)
  chisqano
  pwchisqano<-pairwiseNominalIndependence(datschiano3,
                                          compare = "row",
                                          fisher = FALSE,
                                          gtest  = FALSE,
                                          chisq  = TRUE,
                                          method = "fdr",
                                          digits = 3)
  pwchisqano
  write.csv(pwchisqano, "Small_Cage_Refeeding_Pairwise_ChiSquared_Anopheles.csv", quote=FALSE)
}

# Prolonged survival comparisons
{
  # Cox Proportional Hazards Model for prolonged survival
  {
    # Import and create datasheet
    library(plyr)
    dash<-mutate(RefeedingSurvival,Treatment=paste(Order,Treatment,Water,sep="_"))
    summary(dash)
    attach(dash)
    
    # Compare to the group specified by the csv-specified order
    library(survival)
    library(survminer)
    cox1 <- coxph(Surv(Day, Status) ~ Treatment, data = dash)
    summary(cox1)
    
    # Make plot
    fit1 = survfit(Surv(Day, Status) ~ Treatment, data = dash)
    summary(fit1)
    splot<-ggsurvplot(fit1, 
                      conf.int = FALSE, 
                      risk.table = FALSE, 
                      font.main = c(16, "bold", "black"),
                      font.x = c(14, "bold", "black"),
                      font.y = c(14, "bold", "black"),
                      font.tickslab = c(14, "black"),
                      legend = c(0.9, 0.22), 
                      legend.title = "Treatment",
                      xlab = "Days",
                      palette = c("blue3", "yellow3", "red3", "yellow4", "blue4", "red4"),
                      legend.labs = c("Sugar and Water", "Water Only", "Blood and Water", 
                                      "No Sugar nor Water", "Sugar Only", "Blood Only"))
    splot
    grid.draw.ggsurvplot <- function(x) survminer:::print.ggsurvplot(x, newpage = FALSE)
    ggsave("Prolonged_Survival_SurvPlot_Final.png", width=11,height=7, splot, bg = "transparent")
    ggsave("Prolonged_Survival_SurvPlot_Final.pdf", width=11,height=7, splot, bg = "transparent")
    
    splot2<-ggsurvplot(fit1, 
                       conf.int = TRUE, 
                       risk.table = FALSE, 
                       font.main = c(16, "bold", "black"),
                       font.x = c(14, "bold", "black"),
                       font.y = c(14, "bold", "black"),
                       font.tickslab = c(14, "black"),
                       legend = c(0.9, 0.22), 
                       legend.title = "Treatment",
                       xlab = "Days",
                       palette = c("grey", "grey", "grey", "grey", "grey", "grey"),
                       legend.labs = c("Sugar and Water", "Water Only", "Blood and Water", 
                                       "No Sugar nor Water", "Sugar Only", "Blood Only"))
    splot2
    grid.draw.ggsurvplot <- function(x) survminer:::print.ggsurvplot(x, newpage = FALSE)
    ggsave("Prolonged_Survival_SurvPlot_CIs_Final.png", width=11,height=7, splot2, bg = "transparent")
    ggsave("Prolonged_Survival_SurvPlot_CIs_Final.pdf", width=11,height=7, splot2, bg = "transparent")
    
  }
  
  # GAM figures and emmeans comparisons
  {
    # Prolonged survival curves for Aedes
    ds<-ProlongedSurvival
    attach(ds)
    
    # Split the dataset into appropriate groups
    library(plyr)
    library(dplyr)
    ds2<-dlply(ds,.(Water))
    dsNo<-ds2$'No'
    dsYes<-ds2$'Yes'
    
    ds3<-dlply(dsNo,.(Treatment))
    dsNoNone<-ds3$'None'
    dsNoSugar<-ds3$'Sugar'
    dsNoBlood<-ds3$"Blood"
    
    ds4<-dlply(dsYes,.(Treatment))
    dsYesNone<-ds4$'None'
    dsYesSugar<-ds4$'Sugar'
    dsYesBlood<-ds4$"Blood"
    
    Totds<-rbind(dsNoNone,dsNoSugar,dsNoBlood,dsYesNone,dsYesSugar,dsYesBlood)
    Totds
    attach(Totds)
    
    Totds2<-mutate(Totds,Mutated=paste(Water,Treatment,sep="_"))
    attach(Totds2)
    summary(Totds2)
    
    CTotds<-Totds2 %>% mutate(Treatments = factor(Mutated, levels = c('No_None',
                                                                      'No_Sugar',
                                                                      'No_Blood',
                                                                      'Yes_None',
                                                                      'Yes_Sugar',
                                                                      'Yes_Blood')))
    summary(CTotds)
    
    #SummarySE Function
    library(Rmisc)
    SumTotds<-summarySE(Totds2, measurevar="Alive",groupvars=c("Water","Treatment","Day"),conf.interval=0.95,.drop=TRUE)
    SumTotds
    
    #Geom_smooth uses stats::loess() for < 1,000 observations; otherwise mgcv::gam()
    library(ggplot2)
    DS1<-ggplot(dsNoNone, aes(x=Day, y=Alive)) +
      geom_smooth(aes(x=Day, y=Alive),method=mgcv::gam,formula = y ~ s(x, bs = "cs"))
    DS1
    
    #Will generate the same formula, check 'ggplot' against 'plot' graph for verification
    cstat1<-mgcv::gam(data=dsNoNone,formula = Alive ~ s(Day, bs = "cs"))
    cstat1
    plot(cstat1)
    
    DS2<-ggplot(dsNoSugar, aes(x=Day, y=Alive)) +
      geom_smooth(aes(x=Day, y=Alive),method=mgcv::gam,formula = y ~ s(x, bs = "cs"))
    DS2
    cstat2<-mgcv::gam(data=dsNoSugar,formula = Alive ~ s(Day, bs = "cs"))
    cstat2
    plot(cstat2)
    
    DS3<-ggplot(dsNoBlood, aes(x=Day, y=Alive)) +
      geom_smooth(aes(x=Day, y=Alive),method=mgcv::gam,formula = y ~ s(x, bs = "cs"))
    DS3
    cstat3<-mgcv::gam(data=dsNoBlood,formula = Alive ~ s(Day, bs = "cs"))
    cstat3
    plot(cstat3)
    
    DS4<-ggplot(dsYesNone, aes(x=Day, y=Alive)) +
      geom_smooth(aes(x=Day, y=Alive),method=mgcv::gam,formula = y ~ s(x, bs = "cs"))
    DS4
    cstat4<-mgcv::gam(data=dsYesNone,formula = Alive ~ s(Day, bs = "cs"))
    cstat4
    plot(cstat4)
    
    DS5<-ggplot(dsYesSugar, aes(x=Day, y=Alive)) +
      geom_smooth(aes(x=Day, y=Alive),method=mgcv::gam,formula = y ~ s(x, bs = "cs"))
    DS5
    cstat5<-mgcv::gam(data=dsYesSugar,formula = Alive ~ s(Day, bs = "cs"))
    cstat5
    plot(cstat5)
    
    DS6<-ggplot(dsYesBlood, aes(x=Day, y=Alive)) +
      geom_smooth(aes(x=Day, y=Alive),method=mgcv::gam,formula = y ~ s(x, bs = "cs"))
    DS6
    cstat6<-mgcv::gam(data=dsYesBlood,formula = Alive ~ s(Day, bs = "cs"))
    cstat6
    plot(cstat6)
    
    # Determine survival for vectorial capacity calculations
    ASE1<-emmeans(cstat1, ~ Day,
                  at=list(Day=c(0,0.75,1.75,2.75,3.75,1:20)))
    write.csv(ASE1, "NoNone_Survival.csv",row.names=TRUE)
    ASE2<-emmeans(cstat2, ~ Day,
                  at=list(Day=c(0,0.75,1.75,2.75,3.75,1:20)))
    write.csv(ASE2, "NoSugar_Survival.csv",row.names=TRUE)
    ASE3<-emmeans(cstat3, ~ Day,
                  at=list(Day=c(0,0.75,1.75,2.75,3.75,1:20)))
    write.csv(ASE3, "NoBlood_Survival.csv",row.names=TRUE)
    ASE4<-emmeans(cstat4, ~ Day,
                  at=list(Day=c(0,0.75,1.75,2.75,3.75,1:20)))
    write.csv(ASE4, "YesNone_Survival.csv",row.names=TRUE)
    ASE5<-emmeans(cstat5, ~ Day,
                  at=list(Day=c(0,0.75,1.75,2.75,3.75,1:20)))
    write.csv(ASE5, "YesSugar_Survival.csv",row.names=TRUE)
    ASE6<-emmeans(cstat6, ~ Day,
                  at=list(Day=c(0,0.75,1.75,2.75,3.75,1:20)))
    write.csv(ASE6, "YesBlood_Survival.csv",row.names=TRUE)
    
    # Complete figures, first glm, then gam
    #    DS7<-ggplot(Totds2,aes(x=Day, y=Alive, color=Mutated)) +
    #      geom_smooth(method=glm) +
    #      xlab("\nTime (days)") +
    #      ylab("Proportion Survivng\n") +
    #      theme_classic() +
    #      theme(legend.position = c(0.75, 0.3)) +
    #      scale_x_continuous(expand = c(0,0)) +
    #      scale_y_continuous(expand = c(0,0)) +
    #      coord_cartesian(ylim=c(0,1.01),xlim=c(0,21)) +
    #      theme(axis.text.x=element_text(size=14,color="black"), axis.title=element_text(size=18,color="black")) +
    #      theme(axis.text.y=element_text(size=14,color="black"), axis.title=element_text(size=18,color="black")) +
    #      guides(guide_legend(title="Treatment", override.aes = list(size=.2))) +
    #      theme(panel.background=element_rect(fill="white",color="white")) +
    #      theme(axis.ticks=(element_line(color="black")),axis.line=(element_line(color="black")))
    #    DS7
    #    ggsave(filename="Prolonged_Survival.png",units=c("in"),width=11,height=7,plot=DS7)
    
    DS8<-ggplot(Totds2,aes(x=Day, y=Alive, color=Mutated)) +
      geom_smooth(method=mgcv::gam,formula = y ~ s(x, bs = "cs")) +
      xlab("\nTime (days)") +
      ylab("Proportion Survivng\n") +
      theme_classic() +
      theme(legend.position = c(0.75, 0.3)) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      coord_cartesian(ylim=c(0,1.01),xlim=c(0,21)) +
      theme(axis.text.x=element_text(size=14,color="black"), axis.title=element_text(size=18,color="black")) +
      theme(axis.text.y=element_text(size=14,color="black"), axis.title=element_text(size=18,color="black")) +
      guides(guide_legend(title="Treatment", override.aes = list(size=.2))) +
      theme(panel.background=element_rect(fill="white",color="white")) +
      theme(axis.ticks=(element_line(color="black")),axis.line=(element_line(color="black")))
    DS8
    ggsave(filename="Prolonged_Survival_gam_Final.png",units=c("in"),width=11,height=7,plot=DS8)
    ggsave(filename="Prolonged_Survival_gam_Final.pdf",units=c("in"),width=11,height=7,plot=DS8)    
    
    # Stats to compare overall similarities/differences between GAMs
    library(emmeans)
    CComp <- mgcv::gam(data=CTotds,formula = Alive ~ s(Day, bs = "cs") + Treatments, by = Treatments, family = binomial)
    anova(CComp)
    summary(CComp)
    
    Cemmeans=emmeans(CComp,pairwise~Treatments,adjust="tukey")
    Cemmeans
    write.csv(Cemmeans$emmeans,"Prolonged_Survival_Emmeans.csv",row.names=TRUE)
    write.csv(Cemmeans$contrasts,"Prolonged_Survival_Contrasts.csv",row.names=TRUE)
  } 
}

# Time to feeding comparisons
{
  # Import and make datasheets
  {
    library(plyr)
    ttf<-mutate(RefeedingTimeToFeeding,Treatment=paste(Line,Water,sep="_"))
    summary(ttf)
    ttf
    ttf2<-mutate(ttf,Total=paste(Line,Water,State,sep="_"))
    summary(ttf2)
    ttf2
    attach(ttf)
    
    # Compare groups
    fig_order <- c('WT_Yes','WT_No','IR93a-YFP_Yes','IR93a-YFP_No',
                   'ORCO16_Yes','ORCO16_No','IR8d_Yes','IR8d_No',
                   'IR93a-RFP_Yes','IR93a-RFP_No','ORCO5_Yes','ORCO5_No',
                   'IR8a_Yes','IR8a_No','ORCO2_Yes','ORCO2_No',
                   'GR3-4_Yes','GR3-4_No','GR3-ECFP_Yes','GR3-ECFP_No')
  }
  
  # Create figures
  {
    FigTimeToFeed<-ggplot() +
      geom_boxplot(data=ttf, 
                   aes(x=factor(Treatment, level=fig_order), 
                       y=TimeToFeed, fill=Treatment), outlier.shape = NA) +
      geom_jitter(data=ttf, 
                  aes(x=factor(Treatment, level=fig_order), 
                      y=TimeToFeed, fill=Treatment), width = 0.1) +
      xlab("\nMosquito Line") +
      ylab("Time until feeding (seconds)\n") +
      theme_classic() +
      theme(legend.position = c(1.05, 1.95)) +
      scale_y_continuous(expand = c(0,0), 
                         limits = c(0,1010),
                         breaks = c(0,100,200,300,400,500,600,700,800,900,1000)) +
      theme(axis.text.x=element_text(hjust= 1.2,angle=45,size=14,color="black"), 
            axis.title=element_text(size=18,color="black")) +
      theme(axis.text.y=element_text(size=14,color="black"), 
            axis.title=element_text(size=18,color="black")) +
      theme(panel.background=element_rect(fill="white",color="white")) +
      theme(axis.ticks=(element_line(color="black")),
            axis.line=(element_line(color="black")))  
    FigTimeToFeed
    ggsave(filename="Refeeding_Time_To_Feeding_Final.png",units=c("in"),width=11,height=7,plot=FigTimeToFeed)
    ggsave(filename="Refeeding_Time_To_Feeding_Final.pdf",units=c("in"),width=11,height=7,plot=FigTimeToFeed)    
    
    FigTimeToRefeed<-ggplot() +
      geom_boxplot(data=ttf, 
                   aes(x=factor(Treatment, level=fig_order), 
                       y=TimeToRefeed, fill=Treatment), outlier.shape = NA) +
      geom_jitter(data=ttf, 
                  aes(x=factor(Treatment, level=fig_order), 
                      y=TimeToRefeed, fill=Treatment), width = 0.1) +
      xlab("\nMosquito Line") +
      ylab("Time until refeeding (seconds)\n") +
      theme_classic() +
      theme(legend.position = c(1.05, 1.95)) +
      scale_y_continuous(expand = c(0,0), 
                         limits = c(0,1010),
                         breaks = c(0,100,200,300,400,500,600,700,800,900,1000)) +
      theme(axis.text.x=element_text(hjust= 1.2,angle=45,size=14,color="black"), 
            axis.title=element_text(size=18,color="black")) +
      theme(axis.text.y=element_text(size=14,color="black"), 
            axis.title=element_text(size=18,color="black")) +
      theme(panel.background=element_rect(fill="white",color="white")) +
      theme(axis.ticks=(element_line(color="black")),
            axis.line=(element_line(color="black")))  
    FigTimeToRefeed
    ggsave(filename="Refeeding_Time_To_Refeeding_Final.png",units=c("in"),width=11,height=7,plot=FigTimeToRefeed)
    ggsave(filename="Refeeding_Time_To_Refeeding_Final.pdf",units=c("in"),width=11,height=7,plot=FigTimeToRefeed)
    
    FigTimeToTotal<-ggplot() +
      geom_boxplot(data=ttf2, 
                   aes(x=Total, y=Time, fill=Treatment), outlier.shape = NA) +
      geom_jitter(data=ttf2, 
                  aes(x=Total, y=Time, fill=Treatment), width = 0.1) +
      xlab("\nMosquito Line") +
      ylab("Time until feeding (seconds)\n") +
      theme_classic() +
      theme(legend.position = c(1.05, 1.95)) +
      scale_y_continuous(expand = c(0,0), 
                         limits = c(0,1010),
                         breaks = c(0,100,200,300,400,500,600,700,800,900,1000)) +
      theme(axis.text.x=element_text(hjust= 1.2,angle=45,size=14,color="black"), 
            axis.title=element_text(size=18,color="black")) +
      theme(axis.text.y=element_text(size=14,color="black"), 
            axis.title=element_text(size=18,color="black")) +
      theme(panel.background=element_rect(fill="white",color="white")) +
      theme(axis.ticks=(element_line(color="black")),
            axis.line=(element_line(color="black")))  
    FigTimeToTotal
    ggsave(filename="Refeeding_Time_To_Total_Final.png",units=c("in"),width=11,height=7,plot=FigTimeToTotal)
    ggsave(filename="Refeeding_Time_To_Total_Final.pdf",units=c("in"),width=11,height=7,plot=FigTimeToTotal)
    
  }
  
  # ANOVAs with Tukey post-hoc analyses
  {
    #ANOVA weight comparisons between groups
    feeding.aov <- aov(TimeToFeed ~ Treatment, data = ttf)
    refeeding.aov <- aov(TimeToRefeed ~ Treatment, data = ttf)
    total.aov <- aov(Time ~ Total, data = ttf2)
    
    #Summary of the analyses
    summary(feeding.aov)
    summary(refeeding.aov)
    summary(total.aov)
    
    #Tukey post-hoc for all comparisons
    TuF<-TukeyHSD(feeding.aov)
    TuR<-TukeyHSD(refeeding.aov)
    TuT<-TukeyHSD(total.aov)
    TuF
    TuR
    TuT
    write.csv(TuF$Treatment, "Time_To_Feeding_Tukey_Final.csv", quote=FALSE)
    write.csv(TuR$Treatment, "Time_To_Refeeding_Tukey_Final.csv", quote=FALSE)
    write.csv(TuT$Total, "Time_To_Total_Tukey_Final.csv", quote=FALSE)
  }
  
  # Feeding to refeeding time ratio comparisons
  {
    library(tidyr)
    ttf
    ttf_table <- ttf %>% group_by(Treatment) %>% 
      dplyr::summarise(nF = n_distinct(TimeToFeed, na.rm = TRUE), nR = n_distinct(TimeToRefeed, na.rm = TRUE),
                       meanF=mean(TimeToFeed, na.rm = TRUE),meanR=mean(TimeToRefeed, na.rm = TRUE),
                       sdF=sd(TimeToFeed, na.rm = TRUE), sdR=sd(TimeToRefeed, na.rm = TRUE))
    ttf_table
    
    ttfdf <- ttf_table %>% as.data.frame()
    ttfdf$seF <- ttfdf[["sdF"]]/sqrt(ttfdf[["nF"]])
    ttfdf$seR <- ttfdf[["sdR"]]/sqrt(ttfdf[["nR"]])
    ttfdf
    
    # Convert to df
    ttfdf2 <- ttfdf %>% as.data.frame()
    ttfdf2
    
    library(reshape2)
    ttfdf2$ratio <- ttfdf2[["meanR"]]/ttfdf2[["meanF"]]
    ttfdf2$ratiosum <- ttfdf2[["meanR"]]/(ttfdf2[["meanR"]] + ttfdf2[["meanF"]])
    ttfdf2
    
    # To propagate ratio error -> (sqrt((seNo/MeanNo)^2+(seYes/MeanYes)^2))*(MeanNo/MeanYes)
    ttfdf2$propagatederror <- (sqrt((ttfdf2[["seR"]]/ttfdf2[["meanR"]])^2+(ttfdf2[["seF"]]/ttfdf2[["meanF"]])^2)*(ttfdf2[["meanR"]]/ttfdf2[["meanF"]]))
    ttfdf2
    attach(ttfdf2)
    
    # To generate 95% confidence intervals, when n >20, multiply the standard (propagated) error by 1.96
    ttfdf2$uci <- (ratio + (1.96 * propagatederror))
    ttfdf2$lci <- (ratio - (1.96 * propagatederror))
    ttfdf2
    write.csv(ttfdf2, "Time_To_Feed_Ratios_and_Propagated_Error.csv", quote=FALSE)
    
    # Here is the figure for ratios of means that were not normalized to death
    library(ggplot2)
    FigFeedRat<-ggplot(data=ttfdf2, aes(x=reorder(Treatment, -ratio), y=ratio, group=Treatment, fill=Treatment)) +
      geom_point(aes(size = 5, fill = Treatment)) +
      geom_errorbar(aes(ymin=lci, ymax=uci),width=0.2)+
      xlab("\nMosquito Line") +
      ylab("Mosquito Feeding/Refeeding Ratio\n") +
      theme_classic() +
      theme(legend.position = c(1.05, 1.95)) +
      scale_y_continuous(position = "right", expand = c(0,0),
                         breaks = c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5),
                         limits = c(0,4.53)) +
      theme(axis.text.x=element_text(size=14,color="black",angle=45,hjust=1), axis.title=element_text(size=18,color="black")) +
      theme(axis.text.y=element_text(size=14,color="black"), axis.title=element_text(size=18,color="black")) +
      theme(panel.background=element_rect(fill="white",color="white")) +
      theme(axis.ticks=(element_line(color="black")),axis.line=(element_line(color="black")))  
    FigFeedRat
    ggsave(filename="Time_To_Feeding_Ratios_Final.png",units=c("in"),width=11,height=6,plot=FigFeedRat)
    ggsave(filename="Time_To_Feeding_Ratios_Final.pdf",units=c("in"),width=11,height=6,plot=FigFeedRat)
    
  } 
}

# Activity and sleep comparisons (by Dr. Oluwaseun Ajayi)
{
  ####################################Figure 3B###########################################################
  ####Install and load the following packages
  library(damr)
  library(behavr)
  library(ggetho)
  library(sleepr)
  library(scopr)
  library(zeitgebr)
  
  #set the working directory to where the files can be found
  DATA_DIR <- "folder directory" #use the folder path where the metadata and DAMS files are located
  
  setwd(DATA_DIR) #setting a working directory; final step
  
  #setting the sleep cut-off to 2 hours (7200 seconds) i.e. min_time_immobile = 7200. The default is 300 seconds for Drosophila.
  sleep_dam_annotation <- function(data,
                                   min_time_immobile =7200){
    
    asleep = moving = activity = duration = .SD = . = NULL
    wrapped <- function(d){
      if(! all(c("activity", "t") %in% names(d)))
        stop("data from DAM should have a column named `activity` and one named `t`")
      
      out <- data.table::copy(d)
      col_order <- c(colnames(d),"moving", "asleep")
      out[, moving := activity > 0]
      bdt <- bout_analysis(moving, out)
      bdt[, asleep := duration >= min_time_immobile & !moving]
      out <- bdt[,.(t, asleep)][out, on = "t", roll=TRUE]
      data.table::setcolorder(out, col_order)
      out
    }
    
    if(is.null(key(data)))
      return(wrapped(data))
    data[,
         wrapped(.SD),
         by=key(data)]
  }
  ######################
  #reading your metadata; this is for the period of 3 days
  metadata <- fread("metadata_all.csv")
  
  #After reading the metadata, you have to link the metadata with the DAMS files
  metadata <- link_dam_metadata(metadata, result_dir = DATA_DIR) 
  metadata
  
  #Loading the DAMS files for activity rhythms
  dt <- load_dam(metadata[status=="alive"]) #only mosquitoes that survived throughout the experiment periods were included.
  summary(dt)
  
  #Loading the DAMS files for sleep profiles
  dt <- load_dam(metadata[status=="alive"], FUN = sleep_dam_annotation) #only mosquitoes that survived throughout the experiment periods were included.
  dt
  
  #To generate plot of activity rhythm for each group/category across multiple days
  pl <- ggetho(dt, aes(x=t, y=moving, colour=group)) + 
    stat_pop_etho() + stat_ld_annotations()
  pl
  
  ######Our analyses showed no difference between wet and dry groups for both Day 1 and Day 2 after blood-feeding.
  #####So we are showing lines of code for analyses done for Day 3
  
  
  ####################################Figures 3C & 3D###########################################################
  
  #####Generation of day 3 data
  dt3 <- dt[t %between% days(c(2, 3))]
  
  #number of beam crosses
  #Average beam counts - Total
  stat_dt3 <- dt3[,
                  .(mean_acti = mean(activity)),
                  by='id']
  stat_dt3
  
  #dividing into the light phase and dark phase
  dt3[, phase := ifelse(t %% hours(24) < hours(12), "L", "D")]
  
  
  #In our metadata set-up, we made our start time (18:00:00) which the program automatically called the light-on
  #But based on our light set-up, that is the beginning of the dark phase
  #As a result L in this case is the dark phase and D is the light phase
  
  sum_stat_dt3 <- 
    rejoin(dt3[,
               .(
                 mean_acti_all = mean(activity),
                 mean_acti_dark = mean(activity[phase == "L"]),
                 mean_acti_light = mean(activity[phase == "D"])
               ),
               ,by=id])
  sum_stat_dt3
  
  #To save a usable file for downstream analysis
  Group <- sum_stat_dt3$group
  Beam_counts_T <- sum_stat_dt3$mean_acti_all
  Beam_counts_D <- sum_stat_dt3$mean_acti_dark
  Beam_counts_L <- sum_stat_dt3$mean_acti_light
  
  Day3_beam <- data.frame(Group, Beam_counts_T, Beam_counts_D, Beam_counts_L)
  
  write.csv(Day3_beam, 'file_name.csv') ####file name of choice
  
  
  ###Sleep fraction for each individual
  #Sleep fraction - Total
  sleep_dt3 <- 
    rejoin(dt3[,
               .(sleep_fraction = mean(asleep)),
               by=id])
  sleep_dt3
  
  #dividing into the light phase and dark phase
  dt3[, phase := ifelse(t %% hours(24) < hours(12), "L", "D")]
  
  #In our metadata set-up, we made our start time (18:00:00) which the program automatically called the light-on
  #But based on our light set-up, that is the beginning of the dark phase
  #As a result L in this case is the dark phase and D is the light phase
  
  sleep_dt3 <- 
    rejoin(dt3[,
               .(
                 sleep_fraction_all = mean(asleep),
                 sleep_fraction_dark = mean(asleep[phase == "L"]),
                 sleep_fraction_light = mean(asleep[phase == "D"])
               ),
               ,by=id])
  sleep_dt3
  
  #To save a usable file for downstream analysis
  Group <- sleep_dt3$group
  sleep_fraction_T <- sleep_dt3$sleep_fraction_all
  sleep_fraction_D <- sleep_dt3$sleep_fraction_dark
  sleep_fraction_L <- sleep_dt3$sleep_fraction_light
  
  Day3_sleep <- data.frame(Group, sleep_fraction_T, sleep_fraction_D, sleep_fraction_L)
  
  write.csv(Day3_sleep, 'file_name.csv')####file name of choice
  
  
  #####plots and statistical tests
  ####Install and load the following packages
  library(tidyverse)
  library(hrbrthemes)
  library(viridis)
  library(lattice)
  library(car)
  
  theme_set(
    theme_minimal() +
      theme(legend.position = "right")
  )
  
  setwd("folder directory") ###where associated files are found
  
  #Total beam#
  
  total_beam_day3 <- read.delim("total_beam_day3.txt", header = TRUE) #for statistics, all individuals were used
  total_beam_day3$Group <- as.factor(total_beam_day3$Group)
  
  total_beam_day3_plot <- read.delim("total_beam_day3_removed.txt", header = TRUE)#for graphical purpose, one individual from the wet group with extreme value was removed
  total_beam_day3_plot$Group <- as.factor(total_beam_day3_plot$Group)
  
  #Boxplot
  ggplot(total_beam_day3_plot, aes(x=Group, y=Avg_beam, fill=Group)) + 
    geom_boxplot(outlier.colour = "red", color="black") +
    scale_fill_brewer(palette="Blues") +
    #stat_summary(fun=mean, geom="point", shape=23, size=4)
    scale_x_discrete(limits=c("Dry", "Wet")) +
    geom_jitter(shape=16, size=2, color="black", position=position_jitter(width=0.21)) +
    theme_classic(base_size=10)
  
  #test for normality and equality of variances
  histogram(data=total_beam_day3, ~Avg_beam|Group)
  
  leveneTest(total_beam_day3$Avg_beam,total_beam_day3$Group,center=median)
  
  #normality was not obeyed, so Wilcoxon rank sum test was used
  
  wilcox.test(data=total_beam_day3,Avg_beam~Group,
              var.equal = TRUE,
              paired = FALSE)
  
  
  #Phase beam - combination of both dark phase and light phase on a single plot
  phase_beam_day3 <- read.delim("phase_beam_day3.txt", header = TRUE) #all individuals used
  phase_beam_day3$Group <- as.factor(phase_beam_day3$Group)
  phase_beam_day3$Phase <- as.factor(phase_beam_day3$Phase)
  
  phase_beam_day3_plot <- read.delim("phase_beam_day3_removed.txt", header = TRUE)#for graphical purpose, one individual from the wet group in the dark phase with extreme value was removed
  phase_beam_day3_plot$Group <- as.factor(phase_beam_day3_plot$Group)
  phase_beam_day3_plot$Phase <- as.factor(phase_beam_day3_plot$Phase)
  
  
  #Boxplot
  ggplot(phase_beam_day3_plot, aes(x=Phase, y=Avg_beam, fill=Group)) + 
    geom_boxplot(outlier.colour = "red", color="black") +
    scale_fill_brewer(palette="Blues") +
    #stat_summary(fun=mean, geom="point", shape=23, size=4)
    scale_x_discrete(limits=c("Night", "Day")) +
    geom_jitter(shape=16, size=2, color="black", position=position_jitter(width=0.21)) +
    theme_classic(base_size=10)
  
  #Night beam - during the dark phase
  night_beam_day3 <- read.delim("night_beam_day3.txt", header = TRUE)
  
  histogram(data=night_beam_day3, ~Avg_beam|Group)
  
  leveneTest(night_beam_day3$Avg_beam,night_beam_day3$Group,center=median)
  
  #normality was not obeyed, so Wilcoxon rank sum test was used
  
  wilcox.test(data=night_beam_day3,Avg_beam~Group,
              var.equal = TRUE,
              paired = FALSE)
  
  #Day beam - during the light phase
  day_beam_day3 <- read.delim("day_beam_day3.txt", header = TRUE)
  
  histogram(data=day_beam_day3, ~Avg_beam|Group)
  
  leveneTest(day_beam_day3$Avg_beam,day_beam_day3$Group,center=median)
  
  #normality was not obeyed, so Wilcoxon rank sum test was used
  
  wilcox.test(data=day_beam_day3,Avg_beam~Group,
              var.equal = TRUE,
              paired = FALSE)
  
  
  #Total sleep#
  
  total_sleep_day3 <- read.delim("total_sleep_day3.txt", header = TRUE)
  total_sleep_day3$Group <- as.factor(total_sleep_day3$Group)
  
  #Boxplot
  ggplot(total_sleep_day3, aes(x=Group, y=sleep_min, fill=Group)) + 
    geom_boxplot(outlier.colour = "red", color="black") +
    scale_fill_brewer(palette="Blues") +
    #stat_summary(fun=mean, geom="point", shape=23, size=4)
    scale_x_discrete(limits=c("Dry", "Wet")) +
    geom_jitter(shape=16, size=2, color="black", position=position_jitter(width=0.21)) +
    theme_classic(base_size=10)
  
  
  #test for normality and equality of variances
  histogram(data=total_sleep_day3, ~sleep_min|Group)
  
  leveneTest(total_sleep_day3$sleep_min,total_sleep_day3$Group,center=median)
  
  #normality was not obeyed, so Wilcoxon rank sum test was used
  
  wilcox.test(data=total_sleep_day3,sleep_min~Group,
              var.equal = TRUE,
              paired = FALSE)
  
  
  #Phase sleep - combination of both dark phase and light phase on a single plot
  phase_sleep_day3 <- read.delim("phase_sleep_day3.txt", header = TRUE)
  phase_sleep_day3$Group <- as.factor(phase_sleep_day3$Group)
  phase_sleep_day3$Phase <- as.factor(phase_sleep_day3$Phase)
  
  #Boxplot
  ggplot(phase_sleep_day3, aes(x=Phase, y=sleep_min, fill=Group)) + 
    geom_boxplot(outlier.colour = "red", color="black") +
    scale_fill_brewer(palette="Blues") +
    #stat_summary(fun=mean, geom="point", shape=23, size=4)
    scale_x_discrete(limits=c("Night", "Day")) +
    geom_jitter(shape=16, size=2, color="black", position=position_jitter(width=0.21)) +
    theme_classic(base_size=10)
  
  
  #Night sleep - during the dark phase
  night_sleep_day3 <- read.delim("night_sleep_day3.txt", header = TRUE)
  
  histogram(data=night_sleep_day3, ~sleep_min|Group)
  
  leveneTest(night_sleep_day3$sleep_min,night_sleep_day3$Group,center=median)
  
  #normality was not obeyed, so Wilcoxon rank sum test was used
  
  wilcox.test(data=night_sleep_day3,sleep_min~Group,
              var.equal = TRUE,
              paired = FALSE)
  
  
  #Day sleep - during the light phase
  day_sleep_day3 <- read.delim("day_sleep_day3.txt", header = TRUE)
  
  histogram(data=day_sleep_day3, ~sleep_min|Group)
  
  leveneTest(day_sleep_day3$sleep_min,day_sleep_day3$Group,center=median)
  
  #normality and homoscedasticity were not obeyed, so Wilcoxon rank sum test was used
  
  wilcox.test(data=day_sleep_day3,sleep_min~Group,
              var.equal = FALSE,
              paired = FALSE)
}