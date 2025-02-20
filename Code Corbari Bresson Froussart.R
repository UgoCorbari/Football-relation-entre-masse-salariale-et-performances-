#########################################
#
# Econométrie Dossier de TD             #
# Groupe                                #
#
#########################################


# Cette étude portera sur le lien entre les dépenses salariales des clubs de football et leurs performances. Pour des résultats plus précis et 
# significatifs nous avons choisis d'étudier ce phénomène sur une période de 10 ans, allant de la saison 2011/2012 jusqu'à la saison 2021/202, et 
# pour les 34 équipes ayant évolués au moins 1 an dans le championnat.
# Ici l'indicateur de la performance des clubs sera la moyenne de points obtenus par saisons en "Ligue 1" sur la période 2011/2012- 2021/2022
# Les données concernant la masse salariale des clubs proviennent des rapports annuels de la DNCG et apparaissent sous le nom de 
# "Rémunération du personnel chargée". Ces valeurs sont exprimées en milliers d'euros et regroupent les salaires des joueurs (qui représentent la majeur partie),
# mais également les salaires des équipes de préparateurs, recrutement, équipes de jeunes, dirigent...

# Nous feront alors une hypothèse majeure pour notre étude, le salaire d'un joueur, d'un préparateur ou autre individu apparaissant dans la masse 
# salariale d'un club suit la loi d'offre et de demande du travail, c’est-à-dire qu’à l’équilibre : w = PmL. P avec P=prix. 
# En d’autres termes, le salaire réel (w/P) doit être égal à la productivité marginale « physique » du travail (PmL) fournit par le personnel chargé.
# On posera alors que les salaires sont égaux aux niveaux de performances et de compétences réels du personnel quels que soit leurs fonctions au sein du club.
# Ainsi dans le cas des joueurs nous n'aurons pas de contrainte vis à vis de joueurs surpayées par rapport à leurs niveaux de compétences, cependant nous prendront 
# tout de même en compte ce biais dans notre conclusion.


#. 1 Étapes préliminaires.  #

# Avant tout il faut commencer par installer les packages nécessaires 

install.packages("ggplot2")
install.packages("dplyr")
install.packages("readxl")
install.packages("stargazer")
install.packages("tidyr")

# Il faut maintenant charger les packages 

library("ggplot2")
library("readxl")
library("dplyr")
library("tidyr")

#.  2 Chargement des données, modifications et visualisation. #


# Je charge maintenant les données que j'ai préalablement choisis et préparées sur excel 

Data <- read_excel("~/Desktop/Licence (cours)/L3/Semestre 6/Econométrie /Data.xlsx")
View(Data)

# Par soucis de lisibilité et pour faciliter les calculs ultérieurs, nous procédons à quelques modifications 

Data <- rename(Data,NS_L1 = Saisons.en.Ligue1.sur.la.période)
Data <- rename(Data,NS_L2 = Saisons.en.Ligue2.sur.la.période) 
Data <- rename(Data,NS_CN = Saisons.en.Championnat.national.sur.la.période)

Data <- rename(Data,POINTS_TOT = Nombre.cumulé.de.points.en.ligue.1.sur.la.période)
Data <- rename(Data,MEAN_POINTS = Moyenne.de.points.par.saisons.passées.en.Ligue1)

Data <- rename(Data,TOT_WAGE = Masse.salariale.cumulée.des.clubs.sur.la.période)
# ces données, et toutes les données liées aux masses salariales sont éxprimées en milliers d'euros 




# une fois ces modifications faites nous pouvons créer la variable qui mesure la moyenne de la masse salariale sur la période 
# cependant il faut prendre en compte le fait que les données concernant la masse salariale des clubs en championnat national ne sont 
# pas disponibles ainsi nous pondéreront la variable "masse salariale cumulée" par le nombre de saisons passées en ligue 1 et ligue 2 

Data <- mutate(Data, MEAN_WAGE = TOT_WAGE/(NS_L1 + NS_L2))
summary(Data$MEAN_WAGE)
#cette visualisation permet d'observer que les données liées à la variable MEAN_WAGE sont très dispersées
# nous remarquons que 50% des clubs ont une masse salariale moyenne supérieur à 19 663 K€ et que les autres 50% des clubs ont une 
# masse salariale moyenne inferieur à 19 663 K€

# On créer à présent des variables qui permettent de classer les équipes par moyenne de points et par moyenne de masse salariale 
# Classement par moyenne de points :

Data <- Data %>% arrange(desc(MEAN_POINTS))
Data <- Data %>% mutate(CLASS_MEAN_POINTS = row_number())

# Classement par moyenne de masse salariale :

Data <- Data %>% arrange(desc(MEAN_WAGE))
Data <- Data %>% mutate(CLASS_MEAN_WAGE = row_number())


#.  3 Visualisation des données.  #

### Visualisations simples des variables ###


## Visualisation de la masse salariale des clubs ##

ggplot(Data, aes(x = Clubs, y = MEAN_WAGE)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  labs(title = "Masse salariale moyenne des clubs en millions d'euros sur la période 2011/2012 - 2021/2021",
       x = "Clubs",
       y = "Masse salariale moyenne en millions d'euros") 

# pour une visualisation plus claire et explicite nous allons créer la variable "Masse salariale Moyenne exprimée en millions d'euros" :
Data = mutate(Data, MEAN_WAGE_MILLIONS = MEAN_WAGE/1000)


ggplot(Data, aes(x = Clubs, y = MEAN_WAGE_MILLIONS)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  labs(title = "Masse salariale moyenne des clubs en millions d'euros sur la période 2011/2012 - 2021/2021",
       x = "Clubs",
       y = "Masse salariale moyenne en millions d'euros") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## Visualisation de la moyenne de points des clubs ##

ggplot(Data, aes(x = Clubs, y = MEAN_POINTS)) +
  geom_bar(stat = "identity", fill = "purple", width = 0.5) +
  labs(title = "Moyenne de points des clubs par saisons passées en Ligue1 sur la période 2011/2012 - 2021/2021",
       x = "Clubs",
       y = "Nombre moyen de points par saisons en Ligue1") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



## Comparaison entre le classement budgétaire et le classement moyen sur la période ##

# On commence par trier les clubs par ordre de moyenne de points 
Data <- Data[order(Data$CLASS_MEAN_POINTS), ]

# Jusqu'en 2022/2023 une saison de ligue 1 compte 20 clubs, nous créons alors un data frame dans lequel apparait 
# les 20 clubs ayant obtenu la moyenne de points la plus élevé par saisons en ligue 1 
Data2 <- head(Data, 20)
Data2 <- rename( Data2, Classement.par.moyenne.de.masse.salariale = CLASS_MEAN_WAGE)
Data2 <- rename( Data2, Classement.par.moyenne.de.points = CLASS_MEAN_POINTS)
# On converti le dataframe en format long afin de pouvoir representer les variables cotes à cotes 
Data_long <- tidyr::pivot_longer(Data2, cols = c(Classement.par.moyenne.de.masse.salariale, Classement.par.moyenne.de.points), names_to = "Variable", values_to = "Rang")

# On crée le graphique représentant cotes à cotes les classements par masse salariale et les classements par moyenne de 
# points pour chaque club

ggplot(Data_long, aes(x = Clubs, y = Rang, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.4) +
  labs(title = "Classement des 20 premiers clubs par moyenne de masse salariale et par moyenne de points") +
  scale_fill_manual(values = c("Classement.par.moyenne.de.masse.salariale" = "blue", "Classement.par.moyenne.de.points" = "skyblue")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Cette représentation permet de montrer que pour la majorité des équipes, il n'y a pas d'écart trop important entre le classement 
# par moyenne de points et le classement par moyenne de masse salariale.
# Cependant nous remarquons que pour des clubs comme le Toulouse FC et le SC Bastia il y a un écart important,
# en effet le SC Bastia à la 27ème masse salariale mais est classé 14ème en ce qui concerne la moyenne de points obtenu en ligue 1,
# en ce qui concerne le Toulouse FC, nous remarquons que le club possède la 12ème masse salariale la plus importante de l'échantillon mais n'est 
# classé que 20ème si on observe la moyenne de points obtenu par saisons en Ligue 1.
# Cette "surperformance" du SC Bastia peut s'expliquer par le fait que le club ait passé 5 saisons en Championnat National ainsi les données concernant sa masse salariale
# ne sont pas disponibles sur ces 5 années, de plus le club ayant fait "l'ascenseur" entre Ligue 1 et Championnat National durant la période ce dernier à eu forcément moins de 
# revenus et donc était moins disposé à avoir une masse salariale élevé.
# Dans le cas du Toulouse FC le club fait partie des 12 clubs ayant passé le plus de saisons en ligue 1, ici 9 années, ainsi le club a pu amasser des revenus plus importants que ses 
# concurrents qui oscillent entre Ligue 1 et Ligue 2 et donc allouer un budget plus important à la masse salariale.
# En ce qui concerne les performances sportives de ces deux clubs, cela peut s'expliquer au travers de plusieurs facteur que nous ne développerons pas dans notre analyse tels que 
# la qualité du centre de formation, la qualité du recrutement ou encore les blessures ou non de certains joueurs clefs.

### Visualisations de la relation entre les variables ###

plot( Data$MEAN_WAGE_MILLIONS, Data$MEAN_POINTS,
      xlab = "Masse salariale moyenne sur la période en millions d'euros", 
      ylab = "Nombre de points moyens obtennus en Ligue 1 sur la période",
      col= "orange",
      main = " Relation entre la masse salariale des clubs et leurs moyenne de points en ligue 1")

# on remarque très vite que la relation entre la masse salariale moyenne des clubs et leurs moyenne de points est une relation positive
# comme nous pouvons le verifier :
cov(Data$MEAN_POINTS,Data$MEAN_WAGE)  # supérieur à 0
cor(Data$MEAN_POINTS,Data$MEAN_WAGE)  # les variables sont fortement correlées 


#. 4 Création et résolution des modèles  .#


# Tout d'abord nous allons mettre en place un modèle MCO simple:


MCO <- lm(MEAN_POINTS~MEAN_WAGE_MILLIONS, data = Data)
summary(MCO)

plot( Data$MEAN_WAGE_MILLIONS, Data$MEAN_POINTS,
      xlab = "Masse salariale moyenne sur la période en millions d'euros", 
      ylab = "Nombre de points moyens obtennus en Ligue 1 sur la période",
      col= "orange",
      main = " Relation entre la masse salariale des clubs et leurs moyenne de points en ligue 1")
abline( MCO , col= "purple")

# cette régression permet de montrer qu'une hausse de 1 million d'euro de masse salariale moyenne permet d'obtenir en moyenne 0,16860 points de plus.
# Dans ce modèle, le R-carré multiple est d'environ 0.7433, ce qui signifie que le modèle explique environ 74.33% de la variance dans la variable dépendante.


# Nous mettons maintenant en place un modèle en termes d'interaction,


# Considérons la variable "Produit.d'exploitation.totaux.par.club" jusqu'alors laissé de coté, nous allons utiliser cette variable au sein de notre
# modèle en termes d'interaction, mais pour ce faire procédons à quelques modifications :

Data <- rename(Data, PROD_EXPLOIT = Produits.exploitation.totaux.par.clubs)
Data <- mutate(Data, MEAN_PROD_EXPLOIT = PROD_EXPLOIT/(NS_L1 + NS_L2)) #cette variable représente la moyenne des produits d'exploitations des clubs pondérée par le nompre de saisons passées en Ligue1 et Ligue2
Data <- mutate(Data, MEAN_PROD_EXPLOIT_MILLIONS = MEAN_PROD_EXPLOIT/1000) #par soucis de cohérence vis à vis de notre modèle nous éxprimons cette variable en millions d'euros 
summary( Data$MEAN_PROD_EXPLOIT_MILLIONS)

# Création du modèle en tèrmes d'intéraction :


MODEL.TI <- lm(MEAN_POINTS ~ MEAN_WAGE_MILLIONS * MEAN_PROD_EXPLOIT_MILLIONS, data = Data)
summary(MODEL.TI)

plot( Data$MEAN_WAGE_MILLIONS, Data$MEAN_POINTS,
      xlab = "Masse salariale moyenne sur la période en millions d'euros", 
      ylab = "Nombre de points moyens obtennus en Ligue 1 sur la période",
      col= "orange",
      main = " Relation entre la masse salariale des clubs et leurs moyenne de points en ligue 1")
abline( MODEL.TI , col= "darkblue")

# visuelement cette droite de régression semnble "fiter mieux" la corrélation entre nombre de points moyens et moyenne de mase salariale,
# de plus économétriquement nous remarquons que le modèle possède un R2 de 0,9024 et explique ainsi 90% de la variance dans la variable dépendante,







