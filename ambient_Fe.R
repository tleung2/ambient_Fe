   ### LOAD PACKAGES
library(tidyverse)
library(GGally)
library(viridis)
library(RColorBrewer)
library(Hmisc)
library(corrplot)
library(ggpmisc)
library(lme4)

   ### Type in packages needed
load_pkg <- rlang::quos(tidyverse, GGally, RColorBrewer, Hmisc, 
                        corrplot, ggpubr, ggpmisc,lme4)
   ### Load these packages
invisible(lapply(lapply(load_pkg, rlang::quo_name),
                 library,
                 character.only = TRUE))


   ### LOAD DATA chase_lakes_final and ambient_Fe
   ### Data named as ambient_Fe = ambient_Fe
   ### Data names as chase_Fe = chase_lakes_final
   ### Data names as all.2018 = all_lakes_2018_Fe

   ### Convert columns to numeric
chase_Fe[,c(11:33)] <-as.numeric(chase_Fe[,c(11:33)])
########################################################################
   ######################   CHECK NORMALITY   ######################
ggplot(all.2018, aes(x = Fe)) +
  geom_histogram()
   ### Shapiro test
   ### if P > 0.05, then normally distributed
shapiro.test(all.2018$Fe)  ### failed normal distribution test

   ### log Fe values to improve distribution
all.2018$log_avgFe <-log(all.2018$avg_Fe)

   ### Use Shapiro to check distribution of logged values
shapiro.test(all.2018$log_avgFe)

   ### Check distribution
   ### Still not normal distribution but better than earlier
ggplot(all.2018, aes(x = log_avgFe)) +
  geom_histogram()


#######################################################################
   ######################   BOXPLOTS: Fe   ######################

   ### Subset 2018 data since this is most consistent
   ### and n is largest for this year
chase.2018<-subset(chase_Fe, Year == 2018)
chase.2019<-subset(chase_Fe, Year == 2019 & Fe > 0)
chase_Fe2<-chase_Fe %>%
  subset(!Site %in% c("Honey Creek Resort", "Union Grove", 
                    "Crandall's Beach", "Lake Wapello") & Fe > 0)


   ### make boxplots to look at the variance
   ### plot by order of median using reorder() and FUN = median
ambient_avgFe %>%
  na.omit() %>%
ggplot(aes(y = mean, x = reorder(Waterbody2, mean, FUN = median, .desc = FALSE, na.rm = TRUE))) +
  geom_boxplot(aes(fill = Waterbody2), na.rm = TRUE) +
  #stat_summary(fun.y=mean, geom="point", shape=4, size=2) +
  scale_fill_brewer(palette = "BrBG") +
  labs(y = expression(paste('Average Total Dissolved Fe (', mu, 'mol/L)'))) +
  coord_flip() +
  facet_wrap(.~Waterbody2, ncol = 1, scale = "free") +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 16, color = "black", angle = 90, hjust = 1, vjust = 0.5),
        axis.title.x = element_text(size = 16, color = "black"),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 16),
        legend.position = "N/A",
        legend.key = element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_blank())


#######################################################################
   #################    DESCRIPTIVE STATISTICS   #################
ambient_avgFe %>%
  subset(Waterbody2 == "Artificial Lakes") %>%
  summarise(quant = quantile(TDFe, probs = c(0.25,0.5,0.75)),
            mean = mean(TDFe),
            min = min(TDFe))
             
summary(all.2018$avg_Fe)
avg.fe.2018<-all.2018 %>%
  na.omit() %>%
#ambient_avgFe <- ambient_Fe %>%
  select(c(4,5,7,8,13)) %>%
  group_by(Site2) %>%
  #mutate(mean = mean(avg_Fe))#%>%
  #ungroup()
  summarise(mean = mean(avg_Fe))
write.csv(ambient_avgFe, "ambient_avgFe.csv")

ambient_avgFe %>%
  subset(Waterbody2 == "Artificial Lakes") %>%
  count()

#######################################################################
   #################    CORRELATION ANALYSIS     #################
set.seed(123)  ## ensures repetition

   ##################  1) Group lakes together   ####################
   ### based on previous boxplot and pivot_wider so each lake is a column
   ### Lakes with similar Fe distribution need to be together
   
   ### group 1
group1<- chase_Fe2 %>% 
  select(-c(1,3,6,8:12,14:34)) %>%
  subset(Site2 %in% c("Lake Darling","Prairie Rose",
                                       "Lake Mananwa", "Lake Keomah",
                                       "Lake of Three Fires")) %>%
  pivot_wider(names_from = "Site2", values_from = "Fe")

   ### group 2
group2<- chase_Fe2 %>%
  select(-c(1,3,7,8:12,14:34)) %>%
  subset(Site %in% c("North Twin","Viking Lake", "Big Creek",
                                       "Clear Lake","Rock Creek","Brushy Creek",
                                       "Black Hawk","Green Valley")) %>%
  pivot_wider(names_from = "Site", values_from = "Fe")

   ### group 3
group3<- chase_Fe2 %>%
  select(-c(1,3,6,8:12,14:34)) %>%
  subset(Site2 %in% c("Beeds Lake", "Lower Pine Lake")) %>%
  pivot_wider(names_from = "Site2", values_from = "Fe")

   
   #################   2) ggpairs: correlation analysis  #################
   ### Run correlation for each group
ggpairs(group1[,c(4:7)])
ggpairs(group2[c(1:15),c(4:7)]) + 
  labs(y = expression(paste('Total Dissolved Fe (', mu, 'mol/L)')),
       x = expression(paste('Total Dissolved Fe (', mu, 'mol/L)'))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

group2 %>%
  subset(Year == 2019) %>%
  select(-c(1:3,6,7)) %>%
  ggcorr(method = c("pairwise", "spearman")) +
  #ggpairs(group2[c(1:15),c(4:7)]) + 
  labs(y = expression(paste('Total Dissolved Fe (', mu, 'mol/L)')),
       x = expression(paste('Total Dissolved Fe (', mu, 'mol/L)'))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


pivot.2018b %>%
  select(-c(1:2)) %>%
  ggpairs(method = c("pairwise", "spearman")) +
  #ggpairs(group2[c(1:15),c(4:7)]) + 
  labs(y = expression(paste('Total Dissolved Fe (', mu, 'mol/L)')),
       x = expression(paste('Total Dissolved Fe (', mu, 'mol/L)'))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#######################################################################
   ##################    CORRELATION ANALYSIS    ##################

   ### Pivot wider 2018 Fe data so each lake is a column
pivot.2018<-all.2018 %>%
  na.omit() %>%
  select(c(4,5,7,13)) %>%
  pivot_wider(names_from = "Site2", values_from = "avg_Fe")

   ### Spearman correlation matrix
res<-rcorr(as.matrix(pivot.2018[,c(3:32)], type = "spearman"))
res
#######################################################################
   ##################   VISUALIZING CORRELATION   #################

   ### Be careful of where to put pvalue and r value
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(res$r, method = "color", 
         type = "upper", 
         order = "hclust", 
         tl.col = "black",
         col = brewer.pal(n = 11, name = "RdYlBu"),
         p.mat = res$P, sig.level = 0.05, 
         insig = "blank")


######################################################################
   ###############   SAVE CORRELATION ANALYSIS   #################

   ############   1) Flatten Function   ###############
   ### Create function to flatten correlation output
   ### cormat : matrix of the correlation coefficients
   ### pmat : matrix of the correlation p-values
   ### Make sure Hmisc package is turned on
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
   
   ############   2) Flatten correlation matrix   #################
   ### assign matrix as a dataframe
res.table<-flattenCorrMatrix(res$r, res$P)
   ############   3) Save correlation matrix   #################
   ### save inro R data
save(res.table, file = "spearman_corr_all_lakes.rda")
   ### save as .csv file
write.csv(res$P, file = "spearman_p_all_lakes.csv")

######################################################################
   ################   TIDYING CORRELATION RESULTS   ##############
res.table2<-subset(res.table, p < 5e-2)


########################################################################
   ################   PLOT nMDS WITH GGPLOT   #####################
## -------------- Plot nMDS of references only  -----------------
## hulling only default refs, alpha = transparency w/ 0 being transparent
p1<-ggplot() +
  geom_point(data = mds.scores, aes(x=NMDS1, y=NMDS2, color = end_fe), size = 5) +
  #geom_text(data = grp.default, aes(x = NMDS1, y = NMDS2), size = 7, nudge_x = 0.1) +
  scale_color_viridis_c() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=18), 
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 18), 
        legend.key=element_rect(fill='white'),
        legend.title = element_blank())
p1


##########################################################################
   ####################   SIGNIFICANCE TESTING   ####################

   ### Kruskal Willis test
   ### Is the range in Fe different between lakes?
   ### kruskal.test(dependent~independent)
kruskal.test(all.2018$avg_Fe~all.2018$Site2)
kruskal.test(ambient_Fe$TDFe~ambient_Fe$Waterbody2)
kruskal.test(ambient_Fe$TDFe~ambient_Fe$Site_Name)

   ### For pairwise comparison, number of observations must be the same
   ### Removing week 1 and 2 and other NA sites
all.2018b <-all.2018 %>%
  select(c(4,5,7,13)) %>%
  subset(!(Week %in% c(1,2)) & !(Site2 %in% "George Wyth")) %>%
  na.omit()


pair.res<-pairwise.wilcox.test(all.2018b$avg_Fe, all.2018b$Site2, paired = TRUE,
                     p.adjust.method = "BH")
write.csv(pair.res$p.value, "pairwise_results_2018.csv")
   ### Compare median between 2 groups
   ### Significant difference if pvalue < 0.05
wilcox.test(ambient_avgFe$mean~ ambient_avgFe$Waterbody2)
kruskal.test(lakes_fe$TDFe, lakes_fe$landform, correct=FALSE, na.rm = TRUE)

#######################################################################
   ###################    LINEAR REGRESSION   #####################
mob1<-lm(all.2018$avg_Fe~all.2018$Week)
summary(mob1)

   ### Linear regression for week 1 to 8
wk1_8<-all.2018 %>%
  na.omit() %>%
  subset(Week >= 1 & Week <=8)

   ### Plot Linear Regression
   ### Smooth moethod = loess means locally weighted regression
wk1_8 %>%
  #subset(!Site %in% c("Denison", "McIntosh Woods", "North Twin Lake West")) %>%
ggplot(aes(x=Week, y=avg_Fe)) + 
  geom_point(colour="black", size = 2) + 
  stat_smooth(method = 'loess', aes(color = 'linear'), se = TRUE, formula = y ~ x) + ## Turns on confidence intervals
  stat_poly_eq(aes(label = ..eq.label..), formula = y ~ x, parse = TRUE, size = 3) +                                 ## Turns on equation
  stat_cor(label.x.npc = "center", label.y.npc = "top", size = 3) + ## Turns on r value
  labs(x = 'Week',
       y = expression(paste('Total Dissolved Fe (', mu, 'mol/L)'))) +
  #scale_y_continuous(position = "right") +  ## places y scale on right
  facet_wrap(~Site2, scales = "free", ncol = 5) +
  theme_classic() +
  theme(axis.text.y.left = element_text(size=11, color = "black"), 
        axis.text.x.bottom = element_text(size=11, color = "black"),
        axis.title.x = element_text(size=11),
        axis.title.y = element_text(size=11),
        strip.text = element_text(size = 11))


   ###########  Plot Linear Regression: Slope vs Volume  #############
   ### LR of Week vs Fe show decline Fe from Week 1-8
   ### Pulled slope from this LR 
   ### Run LR for slope vs lake volume
   
chase_slope %>%
  #subset(!Site %in% c("Denison", "McIntosh Woods", "North Twin Lake West")) %>%
  ggplot(aes(x=reorder(Site2, slope), y=slope)) + 
  geom_bar(stat="identity", fill = "steelblue") +
  geom_text(stat = "identity", label = round(chase_slope$volume,0), nudge_y = -0.1) +
  #stat_smooth(method = 'loess', aes(color = 'linear'), se = TRUE, formula = y ~ x) + ## Turns on confidence intervals
  #stat_poly_eq(aes(label = ..eq.label..), formula = y ~ x, parse = TRUE, size = 3) +                                 ## Turns on equation
  #stat_cor(label.x.npc = "center", label.y.npc = "top", size = 3) + ## Turns on r value
  labs(y = expression(paste('Change in Fe (', mu, 'g/L) per week'))) +
  #scale_y_continuous(position = "right") +  ## places y scale on right
  #facet_wrap(~Site2, scales = "free", ncol = 5) +
  theme_classic() +
  theme(axis.text.y = element_text(size=12, color = "black"), 
        axis.text.x = element_text(size=12, color = "black", 
                                   angle = 90, vjust = 0.5, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=12),
        strip.text = element_text(size = 12))


   ###########  Plot Linear Regression: Fe vs. DOC  #############
   ### This is for browning....

all.2018 %>%
  subset(!Site %in% c("Denison", "McIntosh Woods", "North Twin Lake West")) %>%
  ggplot(aes(x=DOC, y=avg_Fe)) + 
  geom_point(stat="identity") +
  #geom_text(stat = "identity", label = round(chase_slope$volume,0), nudge_y = -0.1) +
  #stat_smooth(method = 'loess', aes(color = 'linear'), se = TRUE, formula = y ~ x) + ## Turns on confidence intervals
  #stat_poly_eq(aes(label = ..eq.label..), formula = y ~ x, parse = TRUE, size = 3) +                                 ## Turns on equation
  #stat_cor(label.x.npc = "center", label.y.npc = "top", size = 3) + ## Turns on r value
  labs(y = expression(paste('Dissolved Fe (', mu, 'g/L) per week')),
       x = 'Dissolved organic carbon (mg/L)') +
  #scale_y_continuous(position = "right") +  ## places y scale on right
  #facet_wrap(~Site2, scales = "free", ncol = 5) +
  theme_classic() +
  theme(axis.text.y = element_text(size=12, color = "black"), 
        axis.text.x = element_text(size=12, color = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size=12),
        strip.text = element_text(size = 12))

   ############  Linear Regression: Fe slope for sites  ##############
pivot.slope<-wk1_8 %>%
  pivot_wider(values_from = avg_Fe, names_from = Site2) %>%
  select(-c(1:4,6:18)) 
res.2<-rcorr(as.matrix(pivot.2018[,c(3:32)], type = "spearman"))
res
  ggpairs(method = c("pairwise", "spearman")) +
  #ggpairs(group2[c(1:15),c(4:7)]) + 
  labs(y = expression(paste('Total Dissolved Fe (', mu, 'mol/L)')),
       x = expression(paste('Total Dissolved Fe (', mu, 'mol/L)'))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  
