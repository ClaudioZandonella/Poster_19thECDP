####################################################
#######  Plot preparation Poster 19thECDP  #########
####################################################


############ settings   ##############


rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(ggplot2)
library(kableExtra)
library(brms)
library(rstan)
library(ggcorrplot)
library(tidyr)
library(dplyr)
library(extrafont)
library(tikzDevice)

library(tidybayes)

#library(HDInterval)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

## ggplot settings
theme_set(theme_classic()+
            theme(text = element_text(size=12)))

## Data directory
data_directory<-"Rdata/"

load(paste0(data_directory,"d.finale.rda"))

# select valriabels of interest
d<-select(d.finale,id, classe, genere, age_years,fas.tot, ssm, ssp, 
          emo, con, hyp, peer, pro, int, ext, diff,ssm.M, ssp.M, emo.M, con.M,
          hyp.M, peer.M, pro.M, int.M, ext.M, diff.M)

#############    Compute WAIC weights    ###############

load(paste0(data_directory,"ext~base.rda"))
load(paste0(data_directory,"ext~ssm.rda"))
load(paste0(data_directory,"ext~ssp.rda"))
load(paste0(data_directory,"ext~ssm+ssp.rda"))
load(paste0(data_directory,"ext~ssm_ssp.rda"))


ext_WAIC<-data.frame(model=c(paste0("m.",1:5)),
                     WAIC=c(m1$waic$estimates[3,1],m2$waic$estimates[3,1],
                            m3$waic$estimates[3,1],m4$waic$estimates[3,1],
                            m5$waic$estimates[3,1]))

ext_WAIC<-ext_WAIC%>%
  mutate(diff_WAIC=max(WAIC)-WAIC,         # Compute difference
         rel_lik=exp(diff_WAIC/2),         # Compute relative likelihood
         weights=rel_lik/sum(rel_lik))     # Compute weights


load(paste0(data_directory,"int~base.rda"))
load(paste0(data_directory,"int~ssm.rda"))
load(paste0(data_directory,"int~ssp.rda"))
load(paste0(data_directory,"int~ssm+ssp.rda"))
load(paste0(data_directory,"int~ssm_ssp.rda"))


int_WAIC<-data.frame(model=c(paste0("m.",6:10)),
                     WAIC=c(m6$waic$estimates[3,1],m7$waic$estimates[3,1],
                            m8$waic$estimates[3,1],m9$waic$estimates[3,1],
                            m10$waic$estimates[3,1]))

int_WAIC<-int_WAIC%>%
  mutate(diff_WAIC=max(WAIC)-WAIC,         # Compute difference
         rel_lik=exp(diff_WAIC/2),         # Compute relative likelihood
         weights=rel_lik/sum(rel_lik))     # Compute weights


#############     Model Weights Table    #########

model_comparison<-data.frame(Model_name=c("Base","Mother","Father","Additive","Interaction"),
                             Independent=c("Gender+Grade+FAS","Base+SS mother","Base+SS father",
                                           "Base+SS mother+SS father", "Base+(SS mother*SS father)"),
                             Internalizing=c(0.00,0.00,0.34,0.03,0.63),
                             Externalizing=format(c(0.0,0.9,0.0,0.1,0.0), nsmall = 2), stringsAsFactors = F)


#############     Model Weights Plot     ########


cor_Weights<-data.frame(Int=rev(model_comparison$Internalizing),
                        Ext=rev(as.numeric(model_comparison$Externalizing)))


tikz('figure/Models_Weights.tex',standAlone=TRUE,
     width = 7, height = 7)

ggcorrplot(cor_Weights, method = "circle")+
  coord_flip()+
  scale_x_discrete(expand=expand_scale(mult = c(.07,.13)))+
  scale_y_discrete(expand=expand_scale(mult = c(.5,4)))+
  scale_fill_gradient2(limit=c(0,1), name="weigths")+
  scale_size(range = c(5,30))+
  annotate("text", x = 6.25, y = 1, label = expression(Int), size=10,family="Times")+
  annotate("text", x = 6.25, y = 2, label = expression(Ext),size=10,family="Times")+
  theme_classic()+
  theme(panel.grid = element_blank(), 
        panel.border = element_blank(),
        panel.spacing = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.key.size = unit(2, "cm"),
        legend.text =element_text(size=20),
        legend.position = c(.5,.4),
        text=element_text(size=25, family = "Times"))  

dev.off()

tools::texi2dvi('figure/Models_Weights.tex',pdf=T,clean=T)

#############       Externalizing        #########

load(paste0(data_directory,"m2.poisson.trunc.rda"))


####  Gender_effect
Gender_effect<-m2.poisson.trunc%>%
  emmeans::emmeans( ~ genere) %>%
  gather_emmeans_draws()%>%
  mutate(.value=exp(.value),
         Gender=recode(genere, "M"="Males","F"="Females"))%>%
  ggplot()+
  ggridges::geom_density_ridges(aes(x = .value, y = Gender,fill=Gender), alpha=.9)+
  stat_pointintervalh(aes(x = .value, y=Gender), .width = c( .95, .8, .5), position = position_nudge(y = -0.1))+
  xlab("Externalizing Score")+
  ggtitle("Gender Effect")+
  scale_x_continuous(breaks = seq(from=.5, to=3.5, by=.5))+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.position = c(.75,.8))

##### Grade_effect

Grade_effect<-m2.poisson.trunc%>%
  emmeans::emmeans( ~ classe) %>%
  gather_emmeans_draws()%>%
  mutate(.value=exp(.value),
         Grade=recode(classe, "3°"="3rd","4°"="4th"))%>%
  ggplot()+
  ggridges::geom_density_ridges(aes(x = .value, y = Grade,fill=Grade), alpha=.9)+
  stat_pointintervalh(aes(x = .value, y=Grade), .width =c( .95, .8, .5), position = position_nudge(y = -0.1))+
  xlab("Externalizing Score")+
  ggtitle("Grade Effect")+
  scale_x_continuous(breaks = seq(from=.5, to=3.5, by=.5))+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.position = c(.75,.8))


#### ssm.M_effect


ssm.M_prediction<-d %>%
 modelr::data_grid(ssm.M = modelr::seq_range(ssm.M, n = 201),
                   genere=c("M","F"),
                   classe=c("3°","4°"),
                   fas.tot=6.23) %>%
  add_predicted_draws(m2.poisson.trunc) %>%
  ungroup()%>%
  group_by(ssm.M,.draw)%>%
  summarise(mean_prediction=mean(.prediction))


ssm.M_effect<-ssm.M_prediction%>%
  ggplot(aes(x = ssm.M, y = mean_prediction)) +
  stat_lineribbon(aes(y = mean_prediction),
                  point_interval = mean_qi,
                  .width = c( .95, .8, .5), 
                  color = RColorBrewer::brewer.pal(5, "Blues")[[5]]) +
  scale_fill_brewer()+
  labs(fill = "HDI")+
  ggtitle("Mother Security Effect")+
  xlab("Mother Security")+
  ylab("Externalizing Score")+
  theme(legend.position = c(.75,.8),
        plot.title = element_text(hjust = 0.5))

#### Put all plots toghether

tikz('figure/ext_effects.tex',standAlone=TRUE,
     width = 7, height = 5)

plot <- gridExtra::arrangeGrob(Gender_effect,Grade_effect,ssm.M_effect,
                    nrow=4,ncol=7,
                    layout_matrix = rbind(c( 1, 1,1,NA,2,2,2),
                                          c( 1, 1,1,NA,2,2,2),
                                          c(NA,3,3,3,3,3,NA),
                                          c(NA,3,3,3,3,3,NA)))
# Add labels to the arranged plots
plot<-ggpubr::as_ggplot(plot) +
  cowplot::draw_plot_label(label = c("C)", "D)", "E)"), size = 15,
                  x = c(0, 0.55, 0.15), y = c(1, 1, 0.5)) # Add labels
plot
dev.off()

tools::texi2dvi('figure/ext_effects.tex',pdf=T,clean=T)

#############       Internalizing        ##########
load(paste0(data_directory,"m10.poisson.trunc.rda"))


####  Gender_effect
Gender_effect<-m10.poisson.trunc%>%
  emmeans::emmeans( ~ genere) %>%
  gather_emmeans_draws()%>%
  mutate(.value=exp(.value),
         Gender=recode(genere, "M"="Males","F"="Females"))%>%
  ggplot()+
  ggridges::geom_density_ridges(aes(x = .value, y = Gender,fill=Gender), alpha=.9)+
  stat_pointintervalh(aes(x = .value, y=Gender), .width = c( .95, .8, .5), position = position_nudge(y = -0.1))+
  xlab("Internalizing Score")+
  ggtitle("Gender Effect")+
  scale_x_continuous(breaks = seq(from=.5, to=3.5, by=.5))+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.position = c(.75,.8))

#### Interaction effect  


interaction_prediction<-d %>%
  modelr::data_grid(ssp.M = modelr::seq_range(ssp.M, n = 201),
                    ssm.M=c(2.5,3,3.5),
                    genere=c("M","F"),
                    classe=c("3°","4°"),
                    fas.tot=6.23) %>%
  add_predicted_draws(m10.poisson.trunc) %>%
  ungroup()%>%
  group_by(ssm.M,ssp.M,.draw)%>%
  summarise(mean_prediction=mean(.prediction))


interaction_effect<-interaction_prediction%>%
  ggplot(aes(x = ssp.M, y = mean_prediction, color=as.factor(ssm.M),fill=as.factor(ssm.M))) +
  stat_lineribbon(aes(y = mean_prediction),
                  point_interval = mean_qi,
                  .width = c( .8), alpha=.4)+
  scale_fill_discrete(name="Mother Security",
                      guide=guide_legend(direction = "horizontal",title.position = "top")) +
  scale_color_discrete(name = "Mother Security",
                       guide = guide_legend(direction = "horizontal",title.position = "top"))+
  ggtitle("Interaction Effect")+
  xlab("Father Security")+
  ylab("Internalizing Score")+
  theme(legend.position = c(.75,.75),
        legend.direction = "horizontal",
        plot.title = element_text(hjust = 0.5))


#### Put all plots toghether

tikz('figure/Int_effects.tex',standAlone=TRUE,
     width = 7*3/2, height =7/2 )

plot <- gridExtra::arrangeGrob(Gender_effect,interaction_effect,
                               nrow=1,ncol=9,
                               layout_matrix = rbind(c( 1, 1,1,NA,2,2,2,2,2)))

# Add labels to the arranged plots
plot<-ggpubr::as_ggplot(plot) +
  cowplot::draw_plot_label(label = c("A)", "B)"), size = 15,
                           x = c(0, 1/9*4), y = c(1, 1)) # Add labels
plot
dev.off()

tools::texi2dvi('figure/Int_effects.tex',pdf=T,clean=T)

#############   3d plot                  ######


ssp.list=seq(from=1.5, to=4, length.out = 20)
ssm.list=seq(from=1.5, to=4, length.out = 20)

new_data<- expand.grid(ssp.M=ssp.list,
                       ssm.M=ssm.list,
                       genere=c("M","F"),
                       classe=c("3°","4°"),
                       fas.tot=6)

post_predict<-predict(m10.poisson.trunc, newdata=new_data) # predicted values


new_data$prediction=post_predict[,"Estimate"]

new_data<-new_data %>%
  group_by(ssp.M,ssm.M) %>%
  summarise(mean_prediction=mean(prediction))
  

matrix_predicted<-matrix(new_data$mean_prediction, nrow=20, ncol=20)

plotly::plot_ly(x=ssp.list, y=ssm.list, z=matrix_predicted,
                colorscale = list(c(0, 15))) %>% 
  plotly::layout(scene = list(xaxis = list(title = 'Father Security'),
                 yaxis = list(title = 'Mother Security'),
                 zaxis = list(title = 'Internalizing Score')))%>%
  plotly::add_surface() # Grafico 3D
  



################


