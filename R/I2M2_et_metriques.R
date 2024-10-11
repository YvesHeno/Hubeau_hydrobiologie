# exploration des métriques de l'I2M2 à partir de Hub'eau


library(hubeau)
library(dplyr)
library(stringr) #pour str_extract
library(ggplot2)
library(tidyverse)
library(sf)
library(dplyr)
library(purrr)
library(cowplot)
library(DT)
library(httr)#utiliser POST pour calcul i2m2 à partir du SEEE
library(trend)

source(file="../Hubeau_hydrobiologie/R/Fonctions_unitaires_appel_API.R")
source(file = "../Hubeau_hydrobiologie/R/mk_st_by_group.R")
source(file = "../Hubeau_hydrobiologie/R/Mann_kendall_div.r")
source(file = "../Hubeau_hydrobiologie/R/Seee_to_df.R")
source(file = "../Hubeau_hydrobiologie/R/Calcule_I2M2_métriques.R")



#################################################################################
#                       import des métriques I2M2                               #
#################################################################################

dep <- c('22','29','35','56') # choix des numéros de département

## I2M2 et métriques
##l'interrogation de l'API est couteuse
# aussi , on le fait une fois et on enregistre les donnees dans le fichier Multi_indice_minv
if (file.exists("../Hubeau_hydrobiologie/Data/Multi_indice_minv.Rdata"))
{
  load("../Hubeau_hydrobiologie/Data/Multi_indice_minv.Rdata")
} else
{
   Multi_indice_minv <- map_df(dep,f_get_minv_departement)%>%
    mutate(annee=year(date_prelevement)) %>%
    select("code_station_hydrobio","libelle_station_hydrobio","date_prelevement","code_indice","resultat_indice","latitude","longitude","code_departement","annee") %>% 
    arrange(code_station_hydrobio,annee)
  
  save(Multi_indice_minv,file="../Hubeau_hydrobiologie/Data/Multi_indice_minv_all.Rdata")
}

## rajout des résultats de l'année (non qualifiés)
#on checke avant si on a de la données dans Fichier_SEEE/
Liste_fichiers <-  list.files("../Fichiers_SEEE//",pattern="*.txt")
if (length(Liste_fichiers!=0)){
  data_entree <- Seee_to_df("../Fichiers_SEEE/")
  donnees_metriques_complementaires <- calcule_SEEE_I2M2(data_entree) %>% 
    rename(code_station_hydrobio=CODE_STATION) %>%
    rename(code_indice=CODE_PAR) %>% 
    rename(resultat_indice=RESULTAT) %>% 
    filter(!is.na(resultat_indice)) %>% select(2,3,4,6) %>% 
    rename(date_prelevement=DATE) %>% 
    mutate(annee=str_sub(date_prelevement,start=7,end=10))#le sul moyen que j'ai trouvé pour recupérer l'année !
    Stations <- select(Multi_indice_minv,code_station_hydrobio,libelle_station_hydrobio,latitude,longitude,code_departement)%>%
      unique()
  
      donnees_metriques_complementaires <- left_join(donnees_metriques_complementaires,Stations,by='code_station_hydrobio') %>% 
        select(1,6,2,3,4,7,8,9,5)
      #on amende le DF initial
      Multi_indice_minv <- rbind(Multi_indice_minv,donnees_metriques_complementaires) %>%   arrange(code_station_hydrobio,annee)
}else {
  print ("pas de données complémentaires trouvées dans le dossier Data")
}

##

## Stations non retenues (donnéees < 4 ans)

#on vire s'il y a peu de données par paramètre (au moins 4 donnees/station/paramètre)
#on suppose que pour caque indice/metrique on a le même nb d'analyses
#on va donc compter pour i2m2 et virer les stations "pauvres" en données
comptemulti <- count(Multi_indice_minv,code_station_hydrobio,code_indice) %>%  filter(n>3) %>% filter(code_indice==7613) %>%   select("code_station_hydrobio")
Non_retenu <- count(Multi_indice_minv,code_station_hydrobio,code_indice,libelle_station_hydrobio) %>%  filter(n<=3) %>% filter(code_indice==7613) %>% select("code_station_hydrobio","libelle_station_hydrobio","n") %>% rename("Nb prélèvements"=n)
datatable(Non_retenu,class = 'cell-border stripe',options =
            list( iDisplayLength=10,
                  bLengthChange=TRUE,                       
                  bFilter=TRUE ,                                   
                  bInfo=TRUE,
                  rowid = FALSE,
                  autoWidth = FALSE,
                  ordering = TRUE,
                  scrollX = TRUE,
                  borders = TRUE,
                  columnDefs = list(list(className = 'dt-center', targets ="_all"))
            ),rownames=FALSE#enlever le numero des lignes
)


## Tracé des chroniques   

#Stations_a_garder <- unique(comptemulti$code_station_hydrobio)
Multi_indice_minv_s <- filter(Multi_indice_minv,Multi_indice_minv$code_station_hydrobio%in%comptemulti$code_station_hydrobio) %>% arrange(code_station_hydrobio,annee) #normalement la tri est deja fait plus haut mais je me méfie !
# pour calculer les tendances il faut ordonner le DF par station, annee



Tendances_multi <-mk_st_by_group(Multi_indice_minv_s,resultat_indice,code_indice,code_indice,code_station_hydrobio)
Tendance_i2m2 <- filter(Tendances_multi, code_indice==7613) %>% select(code_station_hydrobio,trend,sens_slope,mk_pvalue)
#

#on fait une selection des données 

#calcul des moyuennes
m_i2m2 <- filter(Multi_indice_minv_s,code_indice==7613) %>% 
  group_by(annee) %>% summarise(moy=mean(resultat_indice))
m_aspt <- filter(Multi_indice_minv_s,code_indice==8057) %>% 
  group_by(annee) %>% summarise(moy=mean(resultat_indice))
m_polyv <- filter(Multi_indice_minv_s,code_indice==8056)%>% 
  group_by(annee) %>% summarise(moy=mean(resultat_indice))
m_ovov <- filter(Multi_indice_minv_s,code_indice==8055)%>% 
  group_by(annee) %>% summarise(moy=mean(resultat_indice))

#chargement des seuils i2m2 +classes de qualité
# travail à faire ##############################
#load("../data/Classe_i2m2.Rda")#donne un DF Classe_i2m2

ploti2m2 <- ggplot(m_i2m2)+ aes(x=annee,y=moy)  +
  geom_point(colour='blue')+geom_line(aes(x = annee, y =moy),colour='blue')+
  ggtitle("Moyenne I2M2")+coord_cartesian(ylim=c(0,1))


plotaspt <- ggplot(m_aspt)+ aes(x=annee,y=moy)  +
  geom_point(colour='green')+geom_line(aes(x = annee, y =moy),colour='green')+
  ggtitle("Moyenne ASPT")+coord_cartesian(ylim=c(0,1))
plotpolyv <- ggplot(m_polyv)+ aes(x=annee,y=moy)  +
  geom_point(colour='orange')+geom_line(aes(x = annee, y =moy),colour='orange')+
  ggtitle("Moyenne polyvoltinisme")+coord_cartesian(ylim=c(0,1))
plotovov <- ggplot(m_ovov)+ aes(x=annee,y=moy)  +
  geom_point(colour='purple')+geom_line(aes(x = annee, y =moy),colour='purple')+
  ggtitle("Moyenne ovoviparité")+coord_cartesian(ylim=c(0,1))

plot_grid(ploti2m2,plotaspt,plotpolyv,plotovov)



## Chroniques par départements

# faire une procedure pour automatiser les visualisations





#on selectionne les i2m2 par dep pour faire plus loin le calcul de la taille ideale des facet

#on vire les 

nbcol <- 3#nb de collonnes du facet"

tailleligne <- 1.2 #tres empirique !
i2m2_22 <- filter(filter(Multi_indice_minv_s,code_indice==7613),code_departement==22)

nbsta <- length(unique(i2m2_22$code_station_hydrobio))
hauteurfacet22 <-  ceiling(nbsta/nbcol)*tailleligne

i2m2_29 <- filter(filter(Multi_indice_minv_s,code_indice==7613),code_departement==29)
nbsta <- length(unique(i2m2_29$code_station_hydrobio))
hauteurfacet29 <-  ceiling(nbsta/nbcol)*tailleligne

i2m2_35 <- filter(filter(Multi_indice_minv_s,code_indice==7613),code_departement==35)
nbsta <- length(unique(i2m2_35$code_station_hydrobio))
hauteurfacet35 <-  ceiling(nbsta/nbcol)*tailleligne

i2m2_56 <- filter(filter(Multi_indice_minv_s,code_indice==7613),code_departement==56)
nbsta <- length(unique(i2m2_56$code_station_hydrobio))
hauteurfacet56 <-  ceiling(nbsta/nbcol)*tailleligne






### Cotes d'armor



#I2M2 par département
ggplot(i2m2_22) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))





### Finistère

ggplot(i2m2_29) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))


### Ille et Vilaine

#I2M2 par département
ggplot(i2m2_35) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))



### Morbihan


#I2M2 par département
ggplot(i2m2_56) +  geom_point(aes(x = annee, color=libelle_station_hydrobio,y =resultat_indice,legend="false"))+geom_line(aes(x = annee, y =resultat_indice))+ facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y") + theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                                                                                                                                                                                     axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))


### Tendances remarquables

i2m2 <- filter(Multi_indice_minv_s,code_indice==7613)

i2m2_et_trend <- left_join(i2m2,Tendance_i2m2,by="code_station_hydrobio")

"tendance à la baisse" 


ggplot(filter(i2m2_et_trend,str_detect(trend,'Decrease'))) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

print("tendance a la hausse")

ggplot(filter(i2m2_et_trend,str_detect(trend,'Increase'))) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +    theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                            axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))


