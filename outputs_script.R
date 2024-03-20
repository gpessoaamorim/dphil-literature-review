# Literature review summary outputs

# Libraries and system settings -----

library(flextable)
library(gtsummary)
library(tidyverse)
library(readxl)
library(crosstable)
library(beepr)
library(ggpubr)
library(magrittr)

setwd("C:/Users/guilhermep/OneDrive - Nexus365/DPhil/Thesis/Chapter 2 - Literature review/Literature review")

# save.image("literature.review.Rdata")
# load("literature.review.Rdata")


# plotting settings
theme_update(text=element_text(family="Mulish"))
update_geom_defaults("text", list(family="Mulish",
                                  size=3))


# Oxpop theme for powerpoint
oxpop_blue_panel<- (
  # dark_mode(
  #theme_fivethirtyeight(base_size = 20))+
  theme(plot.background = element_rect(fill = "transparent", color=NA), # the color argument removes white margin
        panel.background = element_rect(fill = "transparent"), # for ppt BG color use #0d1d41
        
        panel.grid.major = element_line(color = "grey90", size=0.1, linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey90", size=0.1, linetype = "dashed"),
        
        strip.background = element_blank(),
        legend.background = element_blank(),
        panel.spacing = unit(2, "lines"),        
        axis.title.y = element_text(family="Mulish",
                                    color = "white"),
        axis.title.x = element_text(family="Mulish",
                                    color = "white"),
        axis.text.y = element_text(family="Mulish",
                                   color = "white"),
        axis.text.x = element_text(family="Mulish",
                                   color = "white"),
        plot.subtitle = element_text(hjust=0),
        plot.caption = element_text(hjust=0),
        
        strip.text = element_text(size=20, color="white"),
        
        axis.ticks = element_line(color="white"),
        
        text = element_text(family="Mulish",color = "White", size=20),
        panel.border = element_blank()
  )
)





# Load data ------
results<-read_excel("Aggregate outputs/literature review outputs.xlsx", sheet = "Drug exposure estimates")%>%
  rename(Sn=Sensitivity,
         Sp=Specificity)


studies <- read_excel("Aggregate outputs/literature review outputs.xlsx", sheet = "Study level characteristics")%>%
  left_join( read_excel("Aggregate outputs/literature review outputs.xlsx", 
                          sheet = "Comparators"))%>%
  mutate(`Study type`=if_else(`Study type`=="Clinical trial", "RCT", "Observational study"))%>%
  mutate(`Study type`=factor(`Study type`, levels=c("RCT", "Observational study")))


# General description -----
studies%>%distinct(`Study reference`, `Study type`)%>%View()
  # 18 RCTs, 174 observational studies


# Tables ----

## table 1 - study size, location, population, medication -----
table1<-
  tbl_stack(
  list(
    
    # size
    studies%>%
      distinct(`Study reference`,
              # `Study number`,
               Size,
               `Study type`)%>%
      
      select(-`Study reference`)%>%
      mutate(Size=as.integer(Size))%>%
      tbl_summary(by="Study type",
                  missing = "no"),
    
  # countries
  studies%>%
  distinct(Country, 
           `Study type`, 
           # `Study reference`, 
           `Study number`
           )%>%
    select(-`Study number`)%>%
  tbl_summary(by="Study type",
              missing = "no",
              sort=list(everything()~"frequency"),
              statistic = list(all_categorical() ~ "{n} ({p}%)")
              ),
  

  
  # population group
  studies%>%
    distinct(`Study number`,
             `Population group`,
             `Study type`)%>%
    
    select(-`Study number`)%>%
    tbl_summary(by="Study type",
                sort=list(everything()~"frequency"),
                missing = "no"),
  
  # medication group
  results%>%
    left_join(studies%>%
                select(`Study reference`,
                       `Study type`))%>%
    distinct(`Study reference`,
             `Study type`,
             `Medication group`)%>%
    select(-`Study reference`)%>%
    tbl_summary(by="Study type",
                sort=list(everything()~"frequency"),
                missing = "no")))%>%
  as_flex_table()

table1

save_as_docx(table1,
               path="C:/Users/guilhermep/OneDrive - Nexus365/DPhil/Thesis/Chapter 2 - Literature review/Literature review/Outputs/literature_review_results_table1.docx")






## table 2 - RCD source, comparator, reference -----

table2<-
  tbl_stack(
  list(
  # RCD source 
  studies%>%
    distinct(`Study number`,
             `RCD source`,
             `Study type`)%>%
    
    select(-`Study number`)%>%
    tbl_summary(by="Study type",
                sort=list(everything()~"frequency"),
                missing = "no"),
  
  
  # comparator used
  studies%>%
    distinct(`Study number`,
             Comparator,
             `Study type`)%>%
    
    select(-`Study number`)%>%
    tbl_summary(by="Study type",
                sort=list(everything()~"frequency"),
                missing = "no"),
  
  
  
  # reference used
  studies%>%
    distinct(`Study number`,
             `Reference standard`,
             `Study type`)%>%
    
    select(-`Study number`)%>%
    tbl_summary(by="Study type",
                sort=list(everything()~"frequency"),
                missing = "no")))%>%
  as_flex_table()

table2

save_as_docx(table2,
             path="C:/Users/guilhermep/OneDrive - Nexus365/DPhil/Thesis/Chapter 2 - Literature review/Literature review/Outputs/literature_review_results_table2.docx")
    


## table 3 - drug exposure, method, agreement  -----

table3<-
  tbl_stack(
  list(
    
    # drug exposure, method
results%>%
  left_join(studies%>%
              select(`Study reference`,
                     `Study type`))%>%
  distinct(`Study reference`,
           `Drug exposure`,
           `Method`,
           `Study type`)%>%
  rename(`Drug exposure derivation method` = Method)%>%
  
  select(-`Study reference`)%>%
  tbl_summary(by="Study type",
              sort=list(everything()~"frequency"),
              missing = "no"),

# agreement metric
studies%>%
    distinct(`Study reference`,
           `Direct comparison (counts/proportions)`,
           `Agreement`,
           `Kappa`,
           `PABAK`,
           `Sensitivity and specificity`,
           `Predictive values`,
           `Correlation`,
           `Study type`)%>%
  select(-`Study reference`)%>%
  pivot_longer(-`Study type`, names_to="Agreement metric", values_to="Value")%>%
  filter(!is.na(Value))%>%
  select(-Value)%>%
  tbl_summary(by="Study type",
              sort=list(everything()~"frequency"),
              missing = "no"),
  
  # agreeement results
  studies%>%
  mutate(Conclusion=if_else(Conclusion=="Poor agreement", "Negative", Conclusion))%>%
  distinct(`Study type`, Conclusion, `Study reference`)%>%
  select(-`Study reference`)%>%
  tbl_summary(by="Study type",
              sort=list(everything()~"frequency"),
              missing = "no")))%>%
  
  
#   modify_header(stat_by = NULL)%>%
as_flex_table()%>%
  set_caption("Drug exposure and agreement metrics")

table3

save_as_docx(table3,
             path="C:/Users/guilhermep/OneDrive - Nexus365/DPhil/Thesis/Chapter 2 - Literature review/Literature review/Outputs/literature_review_results_table3.docx")



# plots -----

## study characteristics ------

### size ------

#### slides -----
studies%>%
  select(`Study type`, Size)%>%
  mutate(Size=as.integer(Size))%>%
  
  ggplot(aes(x=Size, fill=`Study type`))+
  geom_density(alpha=0.5,color="white")+
  oxpop_blue_panel+
  theme(legend.position = "bottom")+
  labs(title="Size of included studies (by study type)")

ggsave("Outputs/Slides/study_size_plot.png",
       dpi="retina",
       width=40,
       units = "cm",
       height=30)

#### thesis ------



studies%>%
  select(`Study type`, Size)%>%
  mutate(Size=as.integer(Size))%>%
  
  ggplot(aes(x=Size, fill=`Study type`))+
  geom_density(alpha=0.5)+
  theme(legend.position = "bottom",
        text=element_text(family="Mulish",
                          size=20))+
  labs(title="Size of included studies (by study type)",
       y="Density")

ggsave("Outputs/study_size_plot.png",
       dpi="retina",
       width=40,
       units = "cm",
       height=30)

### country -----

#### slides ------

studies%>%
  select(`Study type`, Country, `Study reference`)%>%
  group_by(`Study type`, Country)%>%
  summarise(n=n())%>%
  group_by(Country)%>%
  mutate(n_total=sum(n))%>%
  mutate(Country=as.factor(Country))%>%
  mutate(Country=reorder(Country, n_total))%>%
  arrange(n_total, Country)%>%
  ggplot(aes(x=reorder(Country, n_total), y=n,fill=`Study type`))+
  geom_col(
    # stat="count",
           position=position_dodge())+
  geom_text(aes(label=n),
            position = position_dodge(width=0.9),
            vjust=-1,
            color="white")+
  scale_x_discrete(limits=rev)+
  # scale_x_reverse()+
  oxpop_blue_panel+
  theme(legend.position="bottom")+
  labs(x="Country",
       y="Number of studies")+
  theme(axis.text.x = element_text(angle=30,
                                   hjust=1,
                                   vjust=1))

ggsave("Outputs/Slides/countries_plot.png",
       dpi="retina",
       width=60,
       units = "cm",
       height=30)


#### thesis ------

studies%>%
  select(`Study type`, Country, `Study reference`)%>%
  group_by(`Study type`, Country)%>%
  summarise(n=n())%>%
  group_by(Country)%>%
  mutate(n_total=sum(n))%>%
  mutate(Country=as.factor(Country))%>%
  mutate(Country=reorder(Country, n_total))%>%
  arrange(n_total, Country)%>%
  ggplot(aes(x=reorder(Country, n_total), y=n,fill=`Study type`))+
  geom_col(
    # stat="count",
    position=position_dodge())+
  geom_text(aes(label=n),
            position = position_dodge(width=0.9),
            vjust=-1)+
  scale_x_discrete(limits=rev)+
  # scale_x_reverse()+
  theme(legend.position="bottom",
        text=element_text(family="Mulish",
                          size=20),
        axis.text.x = element_text(angle=30,
                                   hjust=1,
                                   vjust=1))+
  labs(x="Country",
       y="Number of studies")

ggsave("Outputs/countries_plot.png",
       dpi="retina",
       width=60,
       units = "cm",
       height=30)

### population ------

#### slides ------

studies%>%
  select(`Study type`, `Population group`, `Study reference`)%>%
  group_by(`Study type`, `Population group`)%>%
  summarise(n=n())%>%
  group_by(`Population group`)%>%
  mutate(n_total=sum(n))%>%
  mutate(`Population group`=as.factor(`Population group`))%>%
  mutate(`Population group`=reorder(`Population group`, n_total))%>%
  arrange(n_total, `Population group`)%>%
  ggplot(aes(x=reorder(`Population group`, n_total), y=n,fill=`Study type`))+
  geom_col(
    # stat="count",
    position=position_dodge())+
  geom_text(aes(label=n),
            position = position_dodge(width=0.9),
            vjust=-1,
            color="white")+
  scale_x_discrete(limits=rev)+
  # scale_x_reverse()+
  oxpop_blue_panel+
  theme(legend.position="bottom")+
  labs(x="Population group",
       y="Number of studies")+
  theme(axis.text.x = element_text(angle=30,
                                   hjust=1,
                                   vjust=1),
        plot.margin = margin(0,0,0,2, "cm"))

ggsave("Outputs/Slides/populations_plot.png",
       dpi="retina",
       width=60,
       units = "cm",
       height=30)


#### thesis ------

studies%>%
  select(`Study type`, `Population group`, `Study reference`)%>%
  group_by(`Study type`, `Population group`)%>%
  summarise(n=n())%>%
  group_by(`Population group`)%>%
  mutate(n_total=sum(n))%>%
  mutate(`Population group`=as.factor(`Population group`))%>%
  mutate(`Population group`=reorder(`Population group`, n_total))%>%
  arrange(n_total, `Population group`)%>%
  ggplot(aes(x=reorder(`Population group`, n_total), y=n,fill=`Study type`))+
  geom_col(
    # stat="count",
    position=position_dodge())+
  geom_text(aes(label=n),
            position = position_dodge(width=0.9),
            vjust=-1)+
  scale_x_discrete(limits=rev)+
  # scale_x_reverse()+
  labs(x="Population group",
       y="Number of studies")+
  theme(axis.text.x = element_text(angle=30,
                                   hjust=1,
                                   vjust=1),
        plot.margin = margin(0,0,0,2, "cm"),
        text=element_text(family="Mulish",
                          size=20),
        legend.position="bottom")

ggsave("Outputs/populations_plot.png",
       dpi="retina",
       width=60,
       units = "cm",
       height=30)


### medications ------

#### slides -----

studies%>%
  select(`Study type`, `Study reference`)%>%
  left_join(results%>%select(`Study reference`, `Medication group`))%>%
  group_by(`Study type`, `Medication group`)%>%
  summarise(n=n())%>%
  group_by(`Medication group`)%>%
  mutate(n_total=sum(n))%>%
  mutate(`Medication group`=as.factor(`Medication group`))%>%
  mutate(`Medication group`=reorder(`Medication group`, n_total))%>%
  arrange(n_total, `Medication group`)%>%
  ggplot(aes(x=reorder(`Medication group`, n_total), y=n,fill=`Study type`))+
  geom_col(
    # stat="count",
    position=position_dodge())+
  geom_text(aes(label=n),
            position = position_dodge(width=0.9),
            vjust=-1,
            color="white")+
  scale_x_discrete(limits=rev)+
  # scale_x_reverse()+
  oxpop_blue_panel+
  theme(legend.position="bottom")+
  labs(x="Medication group",
       y="Number of studies")+
  theme(axis.text.x = element_text(angle=30,
                                   hjust=1,
                                   vjust=1),
        plot.margin = margin(2,0,0,2, "cm"))

ggsave("Outputs/Slides/medications_plot.png",
       dpi="retina",
       width=60,
       units = "cm",
       height=30)

#### thesis ------


studies%>%
  select(`Study type`, `Study reference`)%>%
  left_join(results%>%select(`Study reference`, `Medication group`))%>%
  group_by(`Study type`, `Medication group`)%>%
  summarise(n=n())%>%
  group_by(`Medication group`)%>%
  mutate(n_total=sum(n))%>%
  mutate(`Medication group`=as.factor(`Medication group`))%>%
  mutate(`Medication group`=reorder(`Medication group`, n_total))%>%
  arrange(n_total, `Medication group`)%>%
  ggplot(aes(x=reorder(`Medication group`, n_total), y=n,fill=`Study type`))+
  geom_col(
    # stat="count",
    position=position_dodge())+
  geom_text(aes(label=n),
            position = position_dodge(width=0.9),
            vjust=-1)+
  scale_x_discrete(limits=rev)+
  # scale_x_reverse()+
  labs(x="Medication group",
       y="Number of studies")+
  theme(axis.text.x = element_text(angle=30,
                                   hjust=1,
                                   vjust=1),
        plot.margin = margin(2,0,0,2, "cm"),
        text=element_text(family="Mulish",
                          size=20),
        legend.position="bottom")

ggsave("Outputs/medications_plot.png",
       dpi="retina",
       width=60,
       units = "cm",
       height=30)
  
## agreement results ----

### overall ----

results%>%
  select(`Study reference`,
         `Percent agreement`= `Agreement`,
         `Kappa`,
         `PABAK`,
         `Sp`,
         `Sn`,
         `PPV`,
         `NPV`,
         `Correlation`
  )%>%
  left_join(studies%>%select(`Study reference`, `Study type`))%>%
  pivot_longer(-c(`Study reference`, `Study type`), names_to="Metric", values_to="Value")%>%
  mutate(Metric=factor(Metric, c("Percent agreement", "Kappa", "PABAK", "Correlation", "Sn","Sp", "PPV", "NPV")))%>%
  arrange(Metric)%>%
  filter(!is.na(Value))%>%
  group_by(Metric)%>%
  mutate(labels_updated = as.factor(paste0(Metric,"\n(n = ",n_distinct(`Study reference`), "; comparisons = ", n(), ")")))%>%
  mutate(labels_updated = factor(labels_updated, levels=c("Percent agreement\n(n = 58; comparisons = 368)",
                                                          "Kappa\n(n = 89; comparisons = 897)",
                                                          "PABAK\n(n = 7; comparisons = 134)",
                                                          "Sn\n(n = 90; comparisons = 761)",
                                                          "Sp\n(n = 67; comparisons = 572)",
                                                          "PPV\n(n = 71; comparisons = 656)",
                                                          "NPV\n(n = 46; comparisons = 380)",
                                                          "Correlation\n(n = 18; comparisons = 62)")))->plot_data

plot_data%<>%
  rbind(
    results%>%
      select(`Study reference`,
             `Percent agreement`= `Agreement`,
             `Kappa`,
             `PABAK`,
             `Sp`,
             `Sn`,
             `PPV`,
             `NPV`,
             `Correlation`
      )%>%
      left_join(studies%>%select(`Study reference`, `Study type`))%>%
      pivot_longer(-c(`Study reference`, `Study type`), names_to="Metric", values_to="Value")%>%
      mutate(Metric=factor(Metric, c("Percent agreement", "Kappa", "PABAK", "Correlation", "Sn","Sp", "PPV", "NPV")))%>%
      arrange(Metric)%>%
      filter(!is.na(Value))%>%
      group_by(Metric)%>%
      mutate(labels_updated = as.factor(paste0(Metric,"\n(n = ",n_distinct(`Study reference`), "; comparisons = ", n(), ")")))%>%
      mutate(labels_updated = factor(labels_updated, levels=c("Percent agreement\n(n = 58; comparisons = 368)",
                                                              "Kappa\n(n = 89; comparisons = 897)",
                                                              "PABAK\n(n = 7; comparisons = 134)",
                                                              "Sn\n(n = 90; comparisons = 761)",
                                                              "Sp\n(n = 67; comparisons = 572)",
                                                              "PPV\n(n = 71; comparisons = 656)",
                                                              "NPV\n(n = 46; comparisons = 380)",
                                                              "Correlation\n(n = 18; comparisons = 62)")))%>%
      mutate(`Study type`="All studies\ncombined")
    
    
    
  )
  


#### thesis -----

plot_data%>%
  mutate(`Study type`=if_else(`Study type`=="Observational study","Observational\nstudy", `Study type`))%>%
  mutate(`Study type`=factor(`Study type`, c("All studies\ncombined", "RCT", "Observational\nstudy")))%>%
  ggplot(aes(x=`Study type`, 
             y=Value, 
             # fill=`Study type`
             ))+
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
  #             # aes(color=NA)
  #             )+
  geom_jitter(aes(color=`Study type`,
                  group=`Study type`),
              position = position_jitterdodge(dodge.width=0.9),
              alpha=0.3)+
  geom_boxplot(aes(fill=`Study type`,
                   group=`Study type`),
               outlier.colour = NA,
               # width=0.2,
               position = position_dodge(width=0.9))+
  facet_wrap(~labels_updated,
             scales="free_x",
             nrow=1,
                #labeller = label_wrap_gen(multi_line = TRUE)
             )+
  geom_text(
    data=.%>%group_by(Metric, labels_updated, `Study type`)%>%
      summarise(median=median(Value),
                Q1 = quantile(Value,0.25),
                Q3 = quantile (Value,0.75)),
    aes(x=`Study type`,
        y=0,
        group=`Study type`,
        label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")),
    size=4,
    position = position_dodge(width=0.9))+
  theme(legend.position = "bottom",
        text=element_text(family="Mulish",
                          size=19),
        # axis.text.x=element_text(size=10)
        axis.text.x=element_blank(),
        legend.title = element_blank(),
        # axis.title.x=element_blank(),
        # axis.ticks.x=element_blank()
        )+
  labs(x="Study type")

ggsave("Outputs/agreement_overall.png",
       dpi="retina",
       units = "cm",
       width = 60,
       height = 30)  


#### slides -----

plot_data%>%
  ggplot(aes(Metric, Value, fill=`Study type`, color=`Study type`
             # color=`Study type`, shap
  ))+
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
  #             # aes(color=NA)
  #             )+
  geom_jitter(alpha=0.4,
              color="white",
              shape=21,
              position=position_jitterdodge())+
  geom_boxplot(outlier.colour = NA,
               color="white",
               width=0.3,
               position=position_dodge(width=0.8))+
  facet_wrap(~labels_updated,
             ncol=4,
             scales="free_x")+
  geom_text(
    data=.%>%group_by(labels_updated,Metric,`Study type`)%>%
      summarise(median=median(Value),
                Q1 = quantile(Value,0.25),
                Q3 = quantile (Value,0.75)),
    color="white",
    size=4.5,
    position = position_jitterdodge(),
    aes(x=Metric,
        y=0,
        label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")))+
  oxpop_blue_panel+
  theme(axis.text.x = element_blank(),
        text=element_text(size=20),
        axis.title.x = element_blank(),
        legend.position = "bottom")

ggsave("Outputs/Slides/agreement_overall.png",
       dpi="retina",
       units = "cm",
       width = 36,
       height = 36)  




### agreement by drug -----
results%>%
  select(`Study reference`,
           `Agreement`,
           `Kappa`,
           `PABAK`,
         `Sp`,
         `Sn`,
           `PPV`,
         `NPV`,
           `Correlation`,
         `Medication group`
         )%>%
  
  left_join(studies%>%
              select(`Study reference`,
                     `Study type`))%>%
  # mutate(across(everything(), ~as.character(.)))%>%
  # select(-`Study reference`)%>%
  pivot_longer(-c(`Study type`, `Medication group`, `Study reference`), names_to="Metric", values_to="Value")%>%
  mutate(Metric=factor(Metric, c("Agreement", "Kappa", "PABAK", "Sn","Sp", "PPV", "NPV", "Correlation")))%>%
  filter(!is.na(Value))%>%
  group_by(`Medication group`)%>%
  mutate(labels_updated = paste0(`Medication group`,"\n(n = ",n_distinct(`Study reference`), "; comparisons = ", n(), ")"))->plot_data

#### thesis -----

plot_data%>%
  ggplot(aes(Metric, Value, 
             # color=`Study type`, shap
             ))+
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
  #             # aes(color=NA)
  #             )+
  geom_boxplot(outlier.colour = NA)+
  geom_jitter(alpha=0.1)+
  facet_wrap(~labels_updated,
             ncol=4)+
  geom_text(
    data=.%>%group_by(labels_updated,Metric)%>%
      summarise(median=median(Value),
             Q1 = quantile(Value,0.25),
             Q3 = quantile (Value,0.75)),
    aes(x=Metric,
        y=0,
        label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")),
    size=3)+
  theme(text=element_text(family="Mulish",
                          size=20),
        axis.text.x=element_text(angle=60,
                                 hjust=1,
                                 vjust=1))+
  labs(x="Agreement metric")

ggsave("Outputs/agreement_by_drug.png",
       dpi="retina",
       units = "cm",
       width = 50,
       height = 50)  


    

#### slides -----

plot_data%>%
  ggplot(aes(Metric, Value, 
             # color=`Study type`, shap
  ))+
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
  #             # aes(color=NA)
  #             )+
  geom_boxplot(outlier.colour = NA)+
  geom_jitter(alpha=0.1,
              color="white")+
  facet_wrap(~labels_updated,
             ncol=4)+
  geom_text(
    data=.%>%group_by(labels_updated,Metric)%>%
      summarise(median=median(Value),
                Q1 = quantile(Value,0.25),
                Q3 = quantile (Value,0.75)),
    color="white",
    size=3,
    aes(x=Metric,
        y=0,
        label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")))+
  oxpop_blue_panel+
  theme(axis.text.x = element_text(size=15,
                                   angle=30,
                                   hjust=1,
                                   vjust=1))+
  labs(x="Agreement metric")

ggsave("Outputs/Slides/agreement_by_drug.png",
       dpi="retina",
       units = "cm",
       width = 60,
       height = 30)  

#### alternative plot ------

results%>%
  select(`Study reference`,
         `Percent agreement` = `Agreement`,
         `Kappa`,
         `PABAK`,
         `Sp`,
         `Sn`,
         `PPV`,
         `NPV`,
         `Correlation`,
         `Medication group`
  )%>%
  
  left_join(studies%>%
              select(`Study reference`,
                     `Study type`))%>%
  # mutate(across(everything(), ~as.character(.)))%>%
  # select(-`Study reference`)%>%
  pivot_longer(-c(`Study type`, `Medication group`, `Study reference`), names_to="Metric", values_to="Value")%>%
  mutate(Metric=factor(Metric, c("Percent agreement", "Kappa", "PABAK", "Sn","Sp", "PPV", "NPV", "Correlation")))%>%
  filter(!is.na(Value))%>%
  group_by(Metric)%>%
  mutate(labels_updated = as.factor(paste0(Metric," (n = ",n_distinct(`Study reference`), "; comparisons = ", n(), ")")))%>%
  mutate(labels_updated = factor(labels_updated, levels=c("Percent agreement (n = 58; comparisons = 368)",
                                                          "Kappa (n = 89; comparisons = 897)",
                                                          "PABAK (n = 7; comparisons = 134)",
                                                          "Sn (n = 90; comparisons = 761)",
                                                          "Sp (n = 67; comparisons = 572)",
                                                          "PPV (n = 71; comparisons = 656)",
                                                          "NPV (n = 46; comparisons = 380)",
                                                          "Correlation (n = 18; comparisons = 62)")))->plot_data


plot_data%>%
  ggplot(aes(`Medication group`, Value, 
             # color=`Study type`, shap
  ))+
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
  #             # aes(color=NA)
  #             )+
  geom_boxplot(outlier.colour = NA)+
  geom_jitter(alpha=0.1)+
  facet_wrap(~labels_updated,
             nrow=8,
             )+
  geom_text(
    data=.%>%group_by(`Medication group`,labels_updated)%>%
      summarise(median=median(Value),
                Q1 = quantile(Value,0.25),
                Q3 = quantile (Value,0.75)),
    aes(x=`Medication group`,
        y=0,
        label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")),
    size=4)+
  theme(text=element_text(family="Mulish",
                          size=20),
        plot.margin = margin(0, 0, 0, 3, "cm"),
        axis.text.x=element_text(angle=60,
                                 hjust=1,
                                 vjust=1))+
  labs(x="Medication group")

ggsave("Outputs/agreement_by_drug.png",
       dpi="retina",
       units = "cm",
       width = 50,
       height = 50)  


### agreement by country -----
results%>%
  select(`Study reference`,
         `Agreement`,
         `Kappa`,
         `PABAK`,
         `Sp`,
         `Sn`,
         `PPV`,
         `NPV`,
         `Correlation`
         
  )%>%
  
  left_join(studies%>%
              select(`Study reference`,
                     Country))%>%
  pivot_longer(-c(Country, `Study reference`), names_to="Metric", values_to="Value")%>%
  mutate(Metric=factor(Metric, c("Agreement", "Kappa", "PABAK", "Sn","Sp", "PPV", "NPV", "Correlation")))%>%
  filter(!is.na(Value))%>%
  group_by(Country)%>%
  mutate(labels_updated = paste0(Country," (n = ",n_distinct(`Study reference`), "; comparisons = ", n(), ")"))->plot_data

#### thesis -----

plot_data%>%
  ggplot(aes(Metric, Value, 
             # color=`Study type`, shap
  ))+
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
  #             # aes(color=NA)
  #             )+
  geom_boxplot(outlier.colour = NA)+
  geom_jitter(alpha=0.1)+
  facet_wrap(~labels_updated,
             ncol=4)+
  geom_text(
    data=.%>%group_by(labels_updated,Metric)%>%
      summarise(median=median(Value),
                Q1 = quantile(Value,0.25),
                Q3 = quantile (Value,0.75)),
    size=3,
    aes(x=Metric,
        y=0,
        label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")))+
  labs(x="Agreement metric")+
  theme(text=element_text(family="Mulish",
                          size=20),
        axis.text.x=element_text(angle=60,
                                 hjust=1,
                                 vjust=1))

ggsave("Outputs/agreement_by_country.png",
       dpi="retina",
       units = "cm",
       width = 50,
       height = 50)  
    

#### slides -----

plot_data%>%
  ggplot(aes(Metric, Value, 
             # color=`Study type`, shap
  ))+
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
  #             # aes(color=NA)
  #             )+
  geom_boxplot(outlier.colour = NA)+
  geom_jitter(alpha=0.1,
              colour="white")+
  facet_wrap(~labels_updated,
             ncol=4)+
  geom_text(
    data=.%>%group_by(labels_updated,Metric)%>%
      summarise(median=median(Value),
                Q1 = quantile(Value,0.25),
                Q3 = quantile (Value,0.75)),
    
    colour="white",
    size=3,
    aes(x=Metric,
        y=0,
        label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")))+
  oxpop_blue_panel+
  theme(axis.text.x = element_text(size=15,
                                                    angle=30,
                                                    hjust=1,
                                                    vjust=1))+
  labs(x="Geographical location")

ggsave("Outputs/Slides/agreement_by_country.png",
       dpi="retina",
       units = "cm",
       width = 60,
       height = 30)  

#### alternative plot -------

results%>%
  select(`Study reference`,
         `Percent agreement`  = `Agreement`,
         `Kappa`,
         `PABAK`,
         `Sp`,
         `Sn`,
         `PPV`,
         `NPV`,
         `Correlation`
         
  )%>%
  
  left_join(studies%>%
              select(`Study reference`,
                     Country))%>%
  pivot_longer(-c(Country, `Study reference`), names_to="Metric", values_to="Value")%>%
  mutate(Metric=factor(Metric, c("Percent agreement", "Kappa", "PABAK", "Sn","Sp", "PPV", "NPV", "Correlation")))%>%
  filter(!is.na(Value))%>%
  group_by(Metric)%>%
  mutate(labels_updated = as.factor(paste0(Metric," (n = ",n_distinct(`Study reference`), "; comparisons = ", n(), ")")))%>%
  mutate(labels_updated = factor(labels_updated, levels=c("Percent agreement (n = 58; comparisons = 368)",
                                                          "Kappa (n = 89; comparisons = 897)",
                                                          "PABAK (n = 7; comparisons = 134)",
                                                          "Sn (n = 90; comparisons = 761)",
                                                          "Sp (n = 67; comparisons = 572)",
                                                          "PPV (n = 71; comparisons = 656)",
                                                          "NPV (n = 46; comparisons = 380)",
                                                          "Correlation (n = 18; comparisons = 62)")))->plot_data


plot_data%>%
  ggplot(aes(Country, Value, 
             # color=`Study type`, shap
  ))+
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
  #             # aes(color=NA)
  #             )+
  geom_boxplot(outlier.colour = NA)+
  geom_jitter(alpha=0.1)+
  facet_wrap(~labels_updated,
             ncol=1)+
  geom_text(
    data=.%>%group_by(Country,labels_updated)%>%
      summarise(median=median(Value),
                Q1 = quantile(Value,0.25),
                Q3 = quantile (Value,0.75)),
    size=5,
    aes(x=Country,
        y=0,
        label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")))+
  labs(x="Geographical location")+
  theme(text=element_text(family="Mulish",
                          size=30),
        axis.text.x=element_text(angle=60,
                                 hjust=1,
                                 vjust=1))

ggsave("Outputs/agreement_by_country.png",
       dpi="retina",
       units = "cm",
       width = 60,
       height = 60)  


### agreement by population group -----
results%>%
  select(`Study reference`,
         `Agreement`,
         `Kappa`,
         `PABAK`,
         `Sp`,
         `Sn`,
         `PPV`,
         `NPV`,
         `Correlation`
         
  )%>%
  
  left_join(studies%>%
              select(`Study reference`,
                     `Population group`))%>%
  pivot_longer(-c(`Population group`,`Study reference`), names_to="Metric", values_to="Value")%>%
  mutate(Metric=factor(Metric, c("Agreement", "Kappa", "PABAK", "Sn","Sp", "PPV", "NPV", "Correlation")))%>%
  filter(!is.na(Value))%>%
  group_by(`Population group`)%>%
  mutate(labels_updated = paste0(`Population group`,"\n(n = ",n_distinct(`Study reference`), "; comparisons = ", n(), ")"))->plot_data

#### thesis ----

plot_data%>%
  ggplot(aes(Metric, Value, 
             # color=`Study type`, shap
  ))+
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
  #             # aes(color=NA)
  #             )+
  geom_boxplot(outlier.colour = NA)+
  geom_jitter(alpha=0.1)+
  facet_wrap(~labels_updated,
             ncol=4)+
  geom_text(
    data=.%>%group_by(labels_updated,Metric)%>%
      summarise(median=median(Value),
                Q1 = quantile(Value,0.25),
                Q3 = quantile (Value,0.75)),
    size=3,
    aes(x=Metric,
        y=0,
        label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")))+
  labs(x="Agreement metric")+
  theme(text=element_text(family="Mulish",
                          size=20),
        axis.text.x=element_text(angle=60,
                                 hjust=1,
                                 vjust=1))

ggsave("Outputs/agreement_by_population_group.png",
       dpi="retina",
       units = "cm",
       width = 50,
       height = 50)  

#### slides ----

plot_data%>%
  ggplot(aes(Metric, Value, 
             # color=`Study type`, shap
  ))+
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
  #             # aes(color=NA)
  #             )+
  geom_boxplot(outlier.colour = NA)+
  geom_jitter(alpha=0.1,
              color="white")+
  facet_wrap(~labels_updated,
             ncol=4,
             # labeller = label_wrap_gen(width = 30),
             )+
  geom_text(
    data=.%>%group_by(labels_updated,Metric)%>%
      summarise(median=median(Value),
                Q1 = quantile(Value,0.25),
                Q3 = quantile (Value,0.75)),
    color="white",
    size=3.5,
    aes(x=Metric,
        y=0,
        label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")))+
  oxpop_blue_panel+
  theme(axis.text.x = element_text(size=15,
                                   angle=30,
                                   hjust=1,
                                   vjust=1))+
  labs(x="Agreement metric")

ggsave("Outputs/Slides/agreement_by_population_group.png",
       dpi="retina",
       units = "cm",
       width = 60,
       height = 30)  


#### alternative plot ----



results%>%
  select(`Study reference`,
         `Percent agreement` = `Agreement`,
         `Kappa`,
         `PABAK`,
         `Sp`,
         `Sn`,
         `PPV`,
         `NPV`,
         `Correlation`
         
  )%>%
  
  left_join(studies%>%
              select(`Study reference`,
                     `Population group`))%>%
  pivot_longer(-c(`Population group`,`Study reference`), names_to="Metric", values_to="Value")%>%
  mutate(Metric=factor(Metric, c("Percent agreement", "Kappa", "PABAK", "Sn","Sp", "PPV", "NPV", "Correlation")))%>%
  filter(!is.na(Value))%>%
  group_by(Metric)%>%
  mutate(labels_updated = as.factor(paste0(Metric," (n = ",n_distinct(`Study reference`), "; comparisons = ", n(), ")")))%>%
  mutate(labels_updated = factor(labels_updated, levels=c("Percent agreement (n = 58; comparisons = 368)",
                                                          "Kappa (n = 89; comparisons = 897)",
                                                          "PABAK (n = 7; comparisons = 134)",
                                                          "Sn (n = 90; comparisons = 761)",
                                                          "Sp (n = 67; comparisons = 572)",
                                                          "PPV (n = 71; comparisons = 656)",
                                                          "NPV (n = 46; comparisons = 380)",
                                                          "Correlation (n = 18; comparisons = 62)")))->plot_data

plot_data%>%
  ggplot(aes(`Population group`, Value, 
             # color=`Study type`, shap
  ))+
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
  #             # aes(color=NA)
  #             )+
  geom_boxplot(outlier.colour = NA)+
  geom_jitter(alpha=0.1)+
  facet_wrap(~labels_updated,
             ncol=1)+
  geom_text(
    data=.%>%group_by(`Population group`,labels_updated)%>%
      summarise(median=median(Value),
                Q1 = quantile(Value,0.25),
                Q3 = quantile (Value,0.75)),
    size=4,
    aes(x=`Population group`,
        y=0,
        label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")))+
  labs(x="Population group")+
  theme(text=element_text(family="Mulish",
                          size=20),
        axis.text.x=element_text(angle=60,
                                 hjust=1,
                                 vjust=1))

ggsave("Outputs/agreement_by_population_group.png",
       dpi="retina",
       units = "cm",
       width = 50,
       height = 50)  

### agreement by RCD source -----
results%>%
  select(`Study reference`,
         `Agreement`,
         `Kappa`,
         `PABAK`,
         `Sp`,
         `Sn`,
         `PPV`,
         `NPV`,
         `Correlation`
         
  )%>%
  
  left_join(studies%>%
              select(`Study reference`,
                     `RCD source`))%>%
  pivot_longer(-c(`RCD source`, `Study reference`), names_to="Metric", values_to="Value")%>%
  mutate(Metric=factor(Metric, c("Agreement", "Kappa", "PABAK", "Sn","Sp", "PPV", "NPV","Correlation")))%>%
  filter(!is.na(Value))%>%
  group_by(`RCD source`)%>%
  mutate(labels_updated = paste0(`RCD source`,"\n(n = ",n_distinct(`Study reference`), "; comparisons = ", n(), ")"))%>%
  
  ->plot_data

#### thesis -----

plot_data%>%
  ggplot(aes(Metric, Value, 
             # color=`Study type`, shap
  ))+
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
  #             # aes(color=NA)
  #             )+
  geom_boxplot(outlier.colour = NA)+
  geom_jitter(alpha=0.1)+
  facet_wrap(~labels_updated,
             ncol=3,
             #labeller = label_wrap_gen(width = 25)
             )+
  geom_text(
    data=.%>%group_by(labels_updated,Metric)%>%
      summarise(median=median(Value),
                Q1 = quantile(Value,0.25),
                Q3 = quantile (Value,0.75)),
    size=3,
    aes(x=Metric,
        y=0,
        label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")))+
  labs(x="Agreement metric")+
  theme(text=element_text(family="Mulish",
                          size=20),
        axis.text.x=element_text(angle=30,
                                 hjust=1,
                                 vjust=1)
        )

ggsave("Outputs/agreement_by_RCD_source.png",
       dpi="retina",
       units = "cm",
       width = 40,
       height = 40)


#### slides -----


plot_data%>%
  ggplot(aes(Metric, Value, 
             # color=`Study type`, shap
  ))+
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
  #             # aes(color=NA)
  #             )+
  geom_boxplot(outlier.colour = NA)+
  geom_jitter(alpha=0.1,
              color="white")+
  facet_wrap(~labels_updated,
             ncol=3)+
  geom_text(
    data=.%>%group_by(labels_updated,Metric)%>%
      summarise(median=median(Value),
                Q1 = quantile(Value,0.25),
                Q3 = quantile (Value,0.75)),
    color="white",
    size=4,
    aes(x=Metric,
        y=0,
        label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")))+
  oxpop_blue_panel+
  labs(x="Agreement metric")

ggsave("Outputs/Slides/agreement_by_RCD_source.png",
       dpi="retina",
       units = "cm",
       width = 60,
       height = 30)

#### alternative plot -----


results%>%
  select(`Study reference`,
         `Percent agreement` = `Agreement`,
         `Kappa`,
         `PABAK`,
         `Sp`,
         `Sn`,
         `PPV`,
         `NPV`,
         `Correlation`
         
  )%>%
  
  left_join(studies%>%
              select(`Study reference`,
                     `RCD source`))%>%
  pivot_longer(-c(`RCD source`, `Study reference`), names_to="Metric", values_to="Value")%>%
  mutate(Metric=factor(Metric, c("Percent agreement", "Kappa", "PABAK", "Sn","Sp", "PPV", "NPV","Correlation")))%>%
  filter(!is.na(Value))%>%
  group_by(Metric)%>%
  mutate(labels_updated = as.factor(paste0(Metric," (n = ",n_distinct(`Study reference`), "; comparisons = ", n(), ")")))%>%
  mutate(labels_updated = factor(labels_updated, levels=c("Percent agreement (n = 58; comparisons = 368)",
                                                          "Kappa (n = 89; comparisons = 897)",
                                                          "PABAK (n = 7; comparisons = 134)",
                                                          "Sn (n = 90; comparisons = 761)",
                                                          "Sp (n = 67; comparisons = 572)",
                                                          "PPV (n = 71; comparisons = 656)",
                                                          "NPV (n = 46; comparisons = 380)",
                                                          "Correlation (n = 18; comparisons = 62)")))->plot_data


plot_data%>%
  ggplot(aes(`RCD source`, Value, 
             # color=`Study type`, shap
  ))+
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
  #             # aes(color=NA)
  #             )+
  geom_boxplot(outlier.colour = NA,
               width=0.3)+
  geom_jitter(alpha=0.1,
              width=0.3)+
  facet_wrap(~labels_updated,
             ncol=1,
             #labeller = label_wrap_gen(width = 25)
  )+
  geom_text(
    data=.%>%group_by(`RCD source`,labels_updated)%>%
      summarise(median=median(Value),
                Q1 = quantile(Value,0.25),
                Q3 = quantile (Value,0.75)),
    size=5,
    aes(x=`RCD source`,
        y=0,
        label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")))+
  labs(x="RCD source")+
  theme(text=element_text(family="Mulish",
                          size=20),
        axis.text.x=element_text(angle=30,
                                 hjust=1,
                                 vjust=1)
  )

ggsave("Outputs/agreement_by_RCD_source.png",
       dpi="retina",
       units = "cm",
       width = 40,
       height = 40)

### agreement by comparator -----
results%>%
  select(`Study reference`,
         `Agreement`,
         `Kappa`,
         `PABAK`,
         `Sp`,
         `Sn`,
         `PPV`,
         `NPV`,
         `Correlation`
           )%>%
  
  left_join(studies%>%
              select(`Study reference`,
                     Comparator))%>%
  pivot_longer(-c(Comparator,`Study reference`), names_to="Metric", values_to="Value")%>%
  mutate(Metric=factor(Metric, c("Agreement", "Kappa", "PABAK", "Sn","Sp", "PPV", "NPV", "Correlation")))%>%
  filter(!is.na(Value))%>%
  group_by(Comparator)%>%
  mutate(labels_updated = paste0(Comparator," (n = ",n_distinct(`Study reference`), "; comparisons = ", n(), ")"))->plot_data

#### thesis ------

plot_data%>%
  ggplot(aes(Metric, Value, 
             # color=`Study type`, shap
  ))+
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
  #             # aes(color=NA)
  #             )+
  geom_boxplot(outlier.colour = NA)+
  geom_jitter(alpha=0.1)+
  facet_wrap(~labels_updated,
             ncol=3)+
  geom_text(
    data=.%>%group_by(labels_updated,Metric)%>%
      summarise(median=median(Value),
                Q1 = quantile(Value,0.25),
                Q3 = quantile (Value,0.75)),
    size=4,
    aes(x=Metric,
        y=0,
        label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")))+
  labs(x="Agreement metric")+
  theme(text=element_text(family="Mulish",
                          size=20),
        axis.text.x=element_text(angle=60,
                                 hjust=1,
                                 vjust=1))

ggsave("Outputs/agreement_by_comparator.png",
       dpi="retina",
       units = "cm",
       width = 60,
       height = 60)  


#### slides ------

plot_data%>%
  ggplot(aes(Metric, Value, 
             # color=`Study type`, shap
  ))+
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
  #             # aes(color=NA)
  #             )+
  geom_boxplot(outlier.colour = NA)+
  geom_jitter(alpha=0.1,
              color="white")+
  facet_wrap(~labels_updated,
             ncol=3)+
  geom_text(
    data=.%>%group_by(labels_updated,Metric)%>%
      summarise(median=median(Value),
                Q1 = quantile(Value,0.25),
                Q3 = quantile (Value,0.75)),
    size=4,
    color="white",
    aes(x=Metric,
        y=0,
        label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")))+
  oxpop_blue_panel+
  labs(x="Agreement metric")

ggsave("Outputs/Slides/agreement_by_comparator.png",
       dpi="retina",
       units = "cm",
       width = 60,
       height = 30)  

#### alternative plot -----

results%>%
  select(`Study reference`,
         `Percent agreement` = `Agreement`,
         `Kappa`,
         `PABAK`,
         `Sp`,
         `Sn`,
         `PPV`,
         `NPV`,
         `Correlation`
  )%>%
  
  left_join(studies%>%
              select(`Study reference`,
                     Comparator))%>%
  pivot_longer(-c(Comparator,`Study reference`), names_to="Metric", values_to="Value")%>%
  mutate(Metric=factor(Metric, c("Percent agreement", "Kappa", "PABAK", "Sn","Sp", "PPV", "NPV", "Correlation")))%>%
  filter(!is.na(Value))%>%
  group_by(Metric)%>%
  mutate(labels_updated = as.factor(paste0(Metric," (n = ",n_distinct(`Study reference`), "; comparisons = ", n(), ")")))%>%
  mutate(labels_updated = factor(labels_updated, levels=c("Percent agreement (n = 58; comparisons = 368)",
                                                          "Kappa (n = 89; comparisons = 897)",
                                                          "PABAK (n = 7; comparisons = 134)",
                                                          "Sn (n = 90; comparisons = 761)",
                                                          "Sp (n = 67; comparisons = 572)",
                                                          "PPV (n = 71; comparisons = 656)",
                                                          "NPV (n = 46; comparisons = 380)",
                                                          "Correlation (n = 18; comparisons = 62)")))->plot_data


plot_data%>%
  ggplot(aes(Comparator, Value, 
             # color=`Study type`, shap
  ))+
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
  #             # aes(color=NA)
  #             )+
  geom_boxplot(outlier.colour = NA)+
  geom_jitter(alpha=0.1)+
  facet_wrap(~labels_updated,
             ncol=1)+
  geom_text(
    data=.%>%group_by(Comparator,labels_updated)%>%
      summarise(median=median(Value),
                Q1 = quantile(Value,0.25),
                Q3 = quantile (Value,0.75)),
    size=5,
    aes(x=Comparator,
        y=0,
        label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")))+
  labs(x="Comparator")+
  theme(text=element_text(family="Mulish",
                          size=30),
        axis.text.x=element_text(angle=60,
                                 hjust=1,
                                 vjust=1))

ggsave("Outputs/agreement_by_comparator.png",
       dpi="retina",
       units = "cm",
       width = 60,
       height = 60)  

### agreement by method and outcome type -----
results%>%
  select(
    `Study reference`,
         `Agreement`,
         `Kappa`,
         `PABAK`,
    `Sp`,
    `Sn`,
         `PPV`,
         `NPV`,
         Method,
         `Drug exposure`,
    `Correlation`
         
  )%>%
  filter(Method %in% c("Fixed window", "Legend-time"),
         `Drug exposure` %in% c("Current use", "Past use" ,"Ever use", "Adherence"))%>%
  pivot_longer(-c(Method,`Drug exposure`, `Study reference`), names_to="Metric", values_to="Value")%>%
  mutate(Metric=factor(Metric, c("Agreement", "Kappa", "PABAK", "Sn","Sp", "PPV", "NPV", "Correlation")))%>%
  filter(!is.na(Value))%>%
  group_by(Method,`Drug exposure`)%>%
  mutate(labels_updated = paste0(`Drug exposure`, " / ", Method," (n = ",n_distinct(`Study reference`), "; comparisons = ", n(), ")"))->plot_data

#### thesis -----

# # alternative plot
# plot_data%>%
#   ggplot(aes(Metric, Value, 
#              # color=`Study type`, shap
#   ))+
#   # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
#   #             # aes(color=NA)
#   #             )+
#   geom_boxplot(outlier.colour = NA)+
#   geom_jitter(alpha=0.1)+
#   # facet_grid(rows = vars(`Drug exposure`),
#   #            cols=vars(Method),
#   #            switch = "y")+
#   facet_wrap(~labels_updated,ncol = 2)+
#   # geom_text(
#   #   data=.%>%group_by(`Drug exposure`, Method,Metric)%>%
#   #     summarise(median=median(Value),
#   #               Q1 = quantile(Value,0.25),
#   #               Q3 = quantile (Value,0.75)),
#   #   aes(x=Metric,
#   #       y=0,
#   #       label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")))+
#   geom_text(
#     data=.%>%group_by(labels_updated,Metric)%>%
#       summarise(median=median(Value),
#                 Q1 = quantile(Value,0.25),
#                 Q3 = quantile (Value,0.75)),
#     aes(x=Metric,
#         y=0,
#         label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")))+
#   labs(x="Agreement metric")

plot_data%>%
  ggplot(aes(Metric, Value, fill= Method
             # color=`Study type`, shap
  ))+
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
  #             # aes(color=NA)
  #             )+
  geom_boxplot(outlier.colour = NA,
               position = "dodge")+
  geom_jitter(alpha=0.2,
              position = position_jitterdodge(),
              # aes(color=Method),
              shape=21,
              color="black")+
  # facet_grid(rows = vars(`Drug exposure`),
  #            cols=vars(Method),
  #            switch = "y")+
  facet_wrap(~`Drug exposure`,ncol = 1)+
  # geom_text(
  #   data=.%>%group_by(`Drug exposure`, Method,Metric)%>%
  #     summarise(median=median(Value),
  #               Q1 = quantile(Value,0.25),
  #               Q3 = quantile (Value,0.75)),
  #   aes(x=Metric,
  #       y=0,
  #       label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")))+
  geom_text(
    data=.%>%group_by(`Drug exposure`, Method,Metric)%>%
      summarise(median=median(Value),
                Q1 = quantile(Value,0.25),
                Q3 = quantile (Value,0.75)),
    position = position_dodge(width = .9),
    size=5,
    aes(x=Metric,
        group=Method,
        y=0,
        label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")))+
  labs(x="Agreement metric")+
  theme(legend.position = "bottom",
        text=element_text(family="Mulish",
                          size=30),
        # axis.text.x=element_text(angle=60,
        #                          hjust=1,
        #                          vjust=1)
        )

ggsave("Outputs/agreement_by_method_outcome.png",
       dpi="retina",
       units = "cm",
       width = 60,
       height = 60)  

#### slides -----

plot_data%>%
  ggplot(aes(Metric, Value, fill= Method
             # color=`Study type`, shap
  ))+
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
  #             # aes(color=NA)
  #             )+
  geom_boxplot(outlier.colour = NA,
               position = "dodge")+
  geom_jitter(alpha=0.3,
              color="black",
              shape=21,
              position=position_jitterdodge())+
  # facet_grid(rows = vars(`Drug exposure`),
  #            cols=vars(Method),
  #            switch = "y")+
  facet_wrap(~`Drug exposure`,ncol = 1)+
  # geom_text(
  #   data=.%>%group_by(`Drug exposure`, Method,Metric)%>%
  #     summarise(median=median(Value),
  #               Q1 = quantile(Value,0.25),
  #               Q3 = quantile (Value,0.75)),
  #   aes(x=Metric,
  #       y=0,
  #       label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")))+
  geom_text(
    data=.%>%group_by(`Drug exposure`, Method,Metric)%>%
      summarise(median=median(Value),
                Q1 = quantile(Value,0.25),
                Q3 = quantile (Value,0.75)),
    position = position_dodge(width = .9),
    size=4,
    color="white",
    aes(x=Metric,
        group=Method,
        y=0,
        label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")))+
  oxpop_blue_panel+
  labs(x="Agreement metric")+
  theme(legend.position = "bottom")

ggsave("Outputs/Slides/agreement_by_method_outcome.png",
       dpi="retina",
       units = "cm",
       width = 60,
       height = 30)  

#### alternative plot ------

results%>%
  select(
    `Study reference`,
         `Percent agreement` = `Agreement`,
         `Kappa`,
         `PABAK`,
    `Sp`,
    `Sn`,
         `PPV`,
         `NPV`,
         Method,
         `Drug exposure`,
    `Correlation`
         
  )%>%
  filter(Method %in% c("Fixed window", "Legend-time"),
         `Drug exposure` %in% c("Current use", "Past use" ,"Ever use", "Adherence"))%>%
  pivot_longer(-c(Method,`Drug exposure`, `Study reference`), names_to="Metric", values_to="Value")%>%
  mutate(Metric=factor(Metric, c("Percent agreement", "Kappa", "PABAK", "Sn","Sp", "PPV", "NPV", "Correlation")))%>%
  filter(!is.na(Value))%>%
  group_by(Metric)%>%
  mutate(labels_updated = as.factor(paste0(Metric," (n = ",n_distinct(`Study reference`), "; comparisons = ", n(), ")")))%>%
  mutate(labels_updated = factor(labels_updated, levels=c("Percent agreement (n = 36; comparisons = 235)",
                                                          "Kappa (n = 70; comparisons = 670)",
                                                          "PABAK (n = 3; comparisons = 57)",
                                                          "Sn (n = 71; comparisons = 594)",
                                                          "Sp (n = 54; comparisons = 435)",
                                                          "PPV (n = 57; comparisons = 522)",
                                                          "NPV (n = 37; comparisons = 293)",
                                                          "Correlation (n = 14; comparisons = 44)")))->plot_data




plot_data%>%
  ggplot(aes(`Drug exposure`, Value, fill= Method
             # color=`Study type`, shap
  ))+
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
  #             # aes(color=NA)
  #             )+
  geom_boxplot(outlier.colour = NA,
               position = position_dodge(width = .9))+
  geom_jitter(alpha=0.2,
              position = position_jitterdodge(),
              # aes(color=Method),
              shape=21,
              color="black")+
  # facet_grid(rows = vars(`Drug exposure`),
  #            cols=vars(Method),
  #            switch = "y")+
  facet_wrap(~labels_updated,
             ncol = 1)+
  # geom_text(
  #   data=.%>%group_by(`Drug exposure`, Method,Metric)%>%
  #     summarise(median=median(Value),
  #               Q1 = quantile(Value,0.25),
  #               Q3 = quantile (Value,0.75)),
  #   aes(x=Metric,
  #       y=0,
  #       label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")))+
  geom_text(
    data=.%>%group_by(`Drug exposure`, Method,labels_updated)%>%
      summarise(median=median(Value),
                Q1 = quantile(Value,0.25),
                Q3 = quantile (Value,0.75)),
    position = position_dodge(width = .9),
    size=6,
    aes(x=`Drug exposure`,
        group=Method,
        y=0,
        label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")))+
  labs(x="Drug exposure")+
  theme(legend.position = "bottom",
        text=element_text(family="Mulish",
                          size=30),
        # axis.text.x=element_text(angle=60,
        #                          hjust=1,
        #                          vjust=1)
  )

ggsave("Outputs/agreement_by_method_outcome.png",
       dpi="retina",
       units = "cm",
       width = 60,
       height = 60)  









### agreement by size ------

results%>%
  select(`Study reference`,
         `Percent agreement`= `Agreement`,
         `Kappa`,
         `PABAK`,
         `Sp`,
         `Sn`,
         `PPV`,
         `NPV`,
         `Correlation`
  )%>%
  
  left_join(studies%>%
              select(`Study reference`,
                     Size,
                     #Country
                     ))%>%
  pivot_longer(-c(Size,`Study reference`), names_to="Metric", values_to="Value")%>%
  mutate(Metric=factor(Metric, c("Percent agreement", "Kappa", "PABAK", "Sn","Sp", "PPV", "NPV","Correlation")))%>%
  filter(!is.na(Value))%>%
  group_by(Metric)%>%
  mutate(labels_updated = as.factor(paste0(Metric,"\n(n = ",n_distinct(`Study reference`), "; comparisons = ", n(), ")")))%>%
  mutate(labels_updated = factor(labels_updated, levels=c("Percent agreement\n(n = 58; comparisons = 368)",
                                                          "Kappa\n(n = 89; comparisons = 897)",
                                                          "PABAK\n(n = 7; comparisons = 134)",
                                                          "Sn\n(n = 90; comparisons = 761)",
                                                          "Sp\n(n = 67; comparisons = 572)",
                                                          "PPV\n(n = 71; comparisons = 656)",
                                                          "NPV\n(n = 46; comparisons = 380)",
                                                          "Correlation\n(n = 18; comparisons = 62)")))->plot_data


#### thesis ------

plot_data%>%
  mutate(Size=as.integer(Size))%>%
  ggplot(aes(Size, Value, 
             # color=`Study type`, shap
  ))+
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
  #             # aes(color=NA)
  #             )+
  # geom_boxplot(outlier.colour = NA)+
  geom_smooth(method="lm")+
  stat_cor(method="pearson",
           size=8,
           label.x.npc = 0.5,
           label.y.npc=0.5)+
  geom_point(alpha=0.1)+
  facet_wrap(~labels_updated,
             ncol=3,
             scales = "free"
             # scales = "free_x"
             )+
  # geom_text(
  #   data=.%>%group_by(labels_updated,Metric)%>%
  #     summarise(median=median(Value),
  #               Q1 = quantile(Value,0.25),
  #               Q3 = quantile (Value,0.75)),
  #   aes(x=Metric,
  #       y=0,
  #       label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")))+
  labs(x="Study size")+
  theme(legend.position = "bottom",
        text=element_text(family="Mulish",
                          size=30))

ggsave("Outputs/agreement_by_size.png",
       dpi="retina",
       units = "cm",
       width = 60,
       height = 60)  


#### slides ------

plot_data%>%
  mutate(Size=as.integer(Size))%>%
  ggplot(aes(Size, Value, 
             # color=`Study type`, shap
  ))+
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
  #             # aes(color=NA)
  #             )+
  # geom_boxplot(outlier.colour = NA)+
  geom_point(alpha=0.2,
             fill="orange",
             color="black",
             shape=21)+
  facet_wrap(~Metric,
             ncol=3,
             scales = "free_x"
  )+
  oxpop_blue_panel+
  # geom_text(
  #   data=.%>%group_by(labels_updated,Metric)%>%
  #     summarise(median=median(Value),
  #               Q1 = quantile(Value,0.25),
  #               Q3 = quantile (Value,0.75)),
  #   aes(x=Metric,
  #       y=0,
  #       label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")))+
  labs(x="Study size")

ggsave("Outputs/Slides/agreement_by_size.png",
       dpi="retina",
       units = "cm",
       width = 60,
       height = 30)  


### agreement by interpretation ------


results%>%
  select(`Study reference`,
         `Percent agreement`= `Agreement`,
         `Kappa`,
         `PABAK`,
         `Sp`,
         `Sn`,
         `PPV`,
         `NPV`,
         `Correlation`
  )%>%
  
  left_join(studies%>%
              select(`Study reference`,
                     Conclusion))%>%
  pivot_longer(-c(Conclusion,`Study reference`), names_to="Metric", values_to="Value")%>%
  mutate(Metric=factor(Metric, c("Percent agreement", "Kappa", "PABAK", "Sn","Sp", "PPV", "NPV", "Correlation")))%>%
  filter(!is.na(Value))%>%
  group_by(Metric)%>%
    mutate(labels_updated = as.factor(paste0(Metric,"\n(n = ",n_distinct(`Study reference`), "; comparisons = ", n(), ")")))%>%
  mutate(labels_updated = factor(labels_updated, levels=c("Percent agreement\n(n = 58; comparisons = 368)",
                                                          "Kappa\n(n = 89; comparisons = 897)",
                                                          "PABAK\n(n = 7; comparisons = 134)",
                                                          "Sn\n(n = 90; comparisons = 761)",
                                                          "Sp\n(n = 67; comparisons = 572)",
                                                          "PPV\n(n = 71; comparisons = 656)",
                                                          "NPV\n(n = 46; comparisons = 380)",
                                                          "Correlation\n(n = 18; comparisons = 62)")))->plot_data

#### thesis ------
plot_data%>%
  mutate(Conclusion=factor(Conclusion, c("Positive", "Mixed","Negative")))%>%
  ggplot(aes(Conclusion, Value, fill=Conclusion,
             # color=`Study type`, shap
  ))+
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
  #             # aes(color=NA)
  #             )+
  geom_boxplot(outlier.colour = NA)+
  geom_jitter(alpha=0.2,
              aes(fill=Conclusion),
              color="black",
              shape=21,
              position = position_jitterdodge())+
  # facet_wrap(~labels_updated,
  #            ncol=3,
  #            # scales = "free_x"
  # )+
  geom_text(
    data=.%>%group_by(Conclusion,labels_updated)%>%
      summarise(median=median(Value),
                Q1 = quantile(Value,0.25),
                Q3 = quantile (Value,0.75)),
    position=position_dodge(width=0.9),
    size=3.5,
    aes(x=Conclusion,
        y=0,
        label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")))+
  facet_wrap(~labels_updated,
             scales="free_x",
             ncol=8)+
  scale_fill_manual(values=c("#00BA38", "grey", "#F8766D"))+
  labs(x="Agreement metric",
       colour="Agreement interpretation (by study authors)",
       fill="Agreement interpretation (by study authors)")+
  theme(legend.position = "bottom",
      text=element_text(family="Mulish",
                        size=20),
      axis.text.x=element_blank(),
      axis.title.x=element_blank(),
      axis.ticks.x=element_blank())



ggsave("Outputs/agreement_by_conclusion.png",
       dpi="retina",
       units = "cm",
       width = 60,
       height = 30)  

#### slides ----
plot_data%>%
  mutate(Conclusion=factor(Conclusion, c("Positive", "Mixed","Negative")))%>%
  ggplot(aes(Metric, Value, fill=Conclusion,
             # color=`Study type`, shap
  ))+
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
  #             # aes(color=NA)
  #             )+
  geom_boxplot(outlier.colour = NA)+
  geom_jitter(alpha=0.2,
              aes(fill=Conclusion),
              color="black",
              shape=21,
              position = position_jitterdodge())+
  # facet_wrap(~labels_updated,
  #            ncol=3,
  #            # scales = "free_x"
  # )+
  geom_text(
    data=.%>%group_by(Conclusion,Metric)%>%
      summarise(median=median(Value),
                Q1 = quantile(Value,0.25),
                Q3 = quantile (Value,0.75)),
    color="white",
    size=4,
    position=position_dodge(width=0.9),
    aes(x=Metric,
        y=0,
        label=paste0(round(median,2), "\n(", round(Q1,2), "-", round(Q3,2), ")")))+
  scale_fill_manual(values=c("#00BA38", "grey", "#F8766D"))+
  labs(x="Agreement metric",
       colour="Agreement interpretation (by study authors)",
       fill="Agreement interpretation (by study authors)")+
  oxpop_blue_panel+
  theme(legend.position = "bottom")



ggsave("Outputs/Slides/agreement_by_conclusion.png",
       dpi="retina",
       units = "cm",
       width = 60,
       height = 30)  
