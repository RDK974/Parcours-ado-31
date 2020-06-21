####### Quantitative assessment of an innovative departmental system for dealing 
####### with the psycho-behavioural crisis in adolescence
####### DER KASBARIAN Raphaël - M2R TOULOUSE

## working directory
setwd("D:\\Médecine\\Année recherche\\Mémoire\\Bases")
library(readxl)
ado <- read_excel("PARCOURS ADO _ ECHANTILLON_RETENU_et_ANONYMISE.xlsx")

## Detecting missing values (checked)
ls.str()
table(is.na(ado$"N°DOSSIER")) # No missing value
table(is.na(ado$"SU_he/ad")) # No missing value
table(is.na(ado$DELAI_RETOUR)) # 4899 missing values but it makes sense because only some patients return to the ER
table(is.na(ado$"N°_PATIENT")) # No missing value
table(is.na(ado$AGE)) # No missing value
table(is.na(ado$Sexe)) # 2 missing values
table(is.na(ado$CODE_GEO)) # 28 missing values
table(is.na(ado$ANNEE)) # No missing value
table(is.na(ado$MOIS)) # No missing value
table(is.na(ado$JSEM)) # No missing value
table(is.na(ado$HEURE)) # No missing value
table(is.na(ado$MOTIF)) # 865 missing values
table(is.na(ado$DP)) # 2312 missing values
table(is.na(ado$DA1)) # 7108 missing values
table(is.na(ado$DA2)) # 7425 missing values
table(is.na(ado$DA3)) # 7478 missing values
table(is.na(ado$DA4)) # 7485 missing values
table(is.na(ado$DA5)) # 7487 missing values
table(is.na(ado$PSYDP)) # 5024 missing values
table(is.na(ado$PSYDA1)) # 6408 missing values
table(is.na(ado$PSYDA2)) # 7017 missing values
table(is.na(ado$PSYDA3)) # 7305 missing values
table(is.na(ado$PSYDA4)) # 7413 missing values
table(is.na(ado$PSYDA5)) # 7456 missing values
table(is.na(ado$DEVENIR_POST_URG)) # 248 missing values
table(is.na(ado$SEL_MOTIF)) # no missing value
table(is.na(ado$SEL_CCMU_P)) # no missing value
table(is.na(ado$SEL_DP)) # 2312 missing values
table(is.na(ado$SEL_DA1)) # 7108 missing values
table(is.na(ado$SEL_DA2)) # 7425 missing values
table(is.na(ado$SEL_DA3)) # 7478 missing values
table(is.na(ado$SEL_DA4)) # 7485 missing values
table(is.na(ado$SEL_DA5)) # 7487 missing values
table(is.na(ado$SEL_DIAG_PSY_URQUAL)) # 5024 missing values

### Cleaning and tidying the database

  # Changing name for SU_he/ad
  ado$SU_he_ad<-ado$'SU_he/ad'
  table(ado$'SU_he/ad')
  table(ado$SU_he_ad,ado$ANNEE)
  prop.table(table(ado$SU_he_ad))
  prop.table(table(ado$SU_he_ad,ado$ANNEE),margin=2)
  
  #Changing names
  ado$N_PATIENT<-ado$"N°_PATIENT"
  ado$N_DOSSIER<-ado$`N°DOSSIER`
  
  # Identifying repeated visits at the ER
  xtabs(~DELAI_RETOUR=="NA",ado)
  ado$passage_répété<-ifelse(ado$DELAI_RETOUR=="NA",0,1) # create new variable establishing a repeated visit or not
  ado$passage_répété[is.na(ado$passage_répété)] <- 0 # replace na by 0
  
  table(ado$DELAI_RETOUR==0)
  xtabs(~passage_répété+ANNEE,ado)
  prop.table(xtabs(~passage_répété+ANNEE,ado),margin=2)

  # Tidying geo codes
  ado$Secteur <- ado$CODE_GEO
  ado$Secteur[ado$Secteur %in% c("31140","31150","31180","31240","31330","31340","31380","31480","31620","31660","31700","31770","31780","31790","31840")]<-"1"
  ado$Secteur[ado$Secteur %in% c("31110","31120","31160","31170","31190","31210","31220","31230","31260","31270","31310","31350","31360","31370","31390","31410","31420","31420","31430","31440","31470","31490","31510","31530","31550","31560","31600","31800","31810","31820","31830","31860","31870","31880")]<-"2"
  ado$Secteur[ado$Secteur %in% c("31130","31250","31280","31290","31320","31450","31460","31520","31540","31570","31590","31650","31670","31750","31850")]<-"3"
  ado$Secteur[ado$Secteur %in% c("31000")]<-"Toulouse"
  ado$Secteur[ado$Secteur %in% c("01220","02140","02840","06000", "07260", "09000", "09100", "09120", "09130", "09160", "09190", "09200", "09210", "09230", "09240", "09270", "09290", "09300", "09320", "09350", "09400", "09420" ,"09500", "09600", "09700", "10000", "11000", "11150", "11160", "11170", "11270", "11300","11320", "11330", "11400", "11420", "11500", "11600", "11610", "11800", "12100", "12160", "12240", "12270", "12310", "12370", "12400" ,"12C05","13011", "13014", "13110", "13150", "13270", "13610", "15C02", "16000", "19100", "22000", "23000", "23C03", "24000", "24100", "29480", "29860","30000", "30132", "30660","32000", "32120", "32130", "32140", "32190", "32200", "32220", "32310", "32350", "32380", "32420", "32430", "32490", "32500", "32600", "32800", "33000", "33290", "33350", "33400", "33470", "33580", "33600", "34000", "34160", "34220", "34300", "34600", "34C02","34C03", "35000", "35510", "38080", "40000", "40140", "41100", "42C07", "43700", "46000", "46090", "46100", "46130", "46140", "46230", "46320",  "46500", "46600", "46700", "47190", "47230", "47240", "47300", "47310", "54120", "57000", "62260", "62300", "63000", "63160", "64000", "64140","64200", "64250", "64C02", "65000", "65100", "65190", "65230", "65290", "65300", "65500", "65C03", "66000", "66120", "66400", "66550", "66720","69100", "69360", "71200", "72350", "73150", "74930", "75009", "75016", "75017", "75018", "77200", "77420", "78000", "78C03", "81000", "81100", "81120", "81150", "81190", "81200", "81240", "81290", "81300", "81310", "81320", "81350", "81370", "81390", "81400", "81500", "81600", "81630", "81700", "81800", "81990", "82000", "82100", "82170", "82200", "82230", "82240", "82290", "82300", "82350", "82370", "82400", "82410", "82500", "82600", "82700", "82800", "82C01", "84000", "87000", "87350", "91530", "92000", "92130", "95160", "95530", "99132", "99216", "99352", "99405", "99422", "9A080", "9C000")]<-"Outside of department"
  ado$Secteur[ado$Secteur %in% c(NA)]<-"missing"
  
  xtabs(~Secteur,ado)
  prop.table(xtabs(~Secteur,ado))
  
    #Population
    xtabs(~ado$Secteur[which(ado$passage_répété==0)],ado)
    prop.table(xtabs(~ado$Secteur[which(ado$passage_répété==0)],ado))
  
  # Removing same visits and odd situations and non-psychiatric cases
  ado<-ado[-c(779,780,1096,1448,1449,1673,1684,2040,2048,2304,2305,2951,2952,3503,3504,3505,3660,3661,4005,4006,4278,4279,4286,4287,4421,5286,5397,5650,5707,6077,6264,6406,6487,6659,6660,6727,7124,7377,7378,7384,7385,7490,7491,7513,7514,7515,7523,7524,7550,7551,7729,7730,7872,7873,7874,7875,7882,7883,7887,7888,8168,8169,365,643,647,792,1136,1269,1270,1398,1409,1487,1506,1518,1598,1622,1764,1773,1815,1931,1932,2014,2016,2042,2075,2470,2594,2602,2630,2643,2656,2777,2804,2816,3002,3026,3053,3056,3089,3092,3120,3162,3179,3224,3235,3280,3283,3419,3429,3527,3542,3765,3882,3937,3970,3993,4096,4097,4194,4213,4215,4222,4433,4491,4515,4712,4795,4796,4910,4919,4930,4974,4978,5045,5100,5101,5103,5106,5171,5172,5214,5215,5229,5272,5281,5295,5313,5315,5316,5322,5342,5343,5401,5402,5403,5409,5418,5518,5584,5608,5816,5853,5877,5914,5927,5945,5946,5948,5949,5952,5955,6043,6048,6057,6075,6085,6090,6158,6232,6260,6274,6290,6345,6384,6395,6417,6447,6449,6501,6514,6544,6557,6609,6610,6611,6612,6618,6620,6637,6720,6732,6736,6764,6769,6770,6773,6789,6792,6832,6833,6835,6886,6927,6971,7054,7058,7059,7062,7063,7071,7072,7135,7198,7212,7224,7234,7256,7272,7343,7417,7420,7448,7459,7499,7564,7583,7623,7629,7631,7632,7637,7696,7699,7700,7702,7712,7760,7802,7826,7896,7897,7900,7910,7932,7947,7979,7989,7993,7994,7996,8023,8082,8106,8113,8114,8139,8142,8143,8186,8199,8233,8236,8249,8250,8277,8281,8284,8297,8521),]
  length(unique(ado$N_PATIENT))
  
  # Removing patients with sex missing
  xtabs(~complete.cases(Sexe),ado)
  ado<-ado[complete.cases(ado$Sexe),]
  length(unique(ado$N_PATIENT))
  xtabs(~Sexe,ado)

          # Removing patients with GEO CODE missing
          #xtabs(~complete.cases(CODE_GEO),ado)
          #ado<-ado[complete.cases(ado$CODE_GEO),]
  
  # Removing patients with GEO CODE outside of department
  xtabs(~Secteur,ado)
  ado<-ado[!(ado$Secteur=="Outside of department"),]
  length(unique(ado$N_PATIENT))

  # Identifying all patients whose age is over 18
  xtabs(~AGE,ado)
  ado$AGE_OF_INTEREST <- (ado$AGE>17)
  xtabs(~AGE_OF_INTEREST,ado)
  ado2<-ado
  # Removing them
  ado<-ado[!(ado$AGE_OF_INTEREST==T),]
  
# Assembling similar motives :
  # Alcool intoxication
  ado$MOTIF[ado$MOTIF %in% c("Intox Ethylique")]<-"Intoxication Ethylique" 

  # Agitation, behaviour disorder
  ado$MOTIF[ado$MOTIF %in% c("Tr du comportement", "Trbles comportement", "Trouble du Comportement")]<-"Agitation, trouble de personnalité et du comportement"
  table((ado$MOTIF %in% c("Agitation modérée", "Agitation", "Agitation violente")), ado$ANNEE)
  table((ado$MOTIF %in% c("Agitation, trouble de personnalité et du comportement","Agitation modérée", "Agitation", "Agitation violente")), ado$ANNEE)

  # Assault, hetero-aggressiveness
  ado$MOTIF[ado$MOTIF %in% c("Agression", "Agression physique (bagarre, rixe)", "Hétéro-agressivité")]<-"Agression, hétéro-agressivité" 
  table((ado$MOTIF %in% c("Agression, hétéro-agressivité")), ado$ANNEE)

  # Alteration of general condition
  ado$MOTIF[ado$MOTIF %in% c("AEG", "Asthénie")]<-"Altération de l`état général" 
  
  # Anxiety or somatoform disorder
  ado$MOTIF[ado$MOTIF %in% c("Angoisse, stress, trouble névrotique ou somatoforme", "Etat anxieux", "Trbles anxieux")]<-"Trouble anxieux et/ou somatoforme" 
  
  # Self harm
  ado$MOTIF[ado$MOTIF %in% c("", "")]<-"Auto-agressivité" # Check with Alexis
  
  # Convulsions
  ado$MOTIF[ado$MOTIF %in% c("Convulsions en cours", "Convulsions fébriles", "Convulsions récentes", "Convulsions SAI ou NCA")]<-"Convulsions"
  
  # Panic Attack
  ado$MOTIF[ado$MOTIF %in% c("Crise d`angoisse")]<-"Crise d'angoisse" 
  
  # Motor deficit
  ado$MOTIF[ado$MOTIF %in% c("Déficit moteur < 2h")]<-"Déficit moteur"
 
  # Sensorial deficit
  ado$MOTIF[ado$MOTIF %in% c("Déficit sensoriel","Déficit sensoriel autre", "Tr de la Sensibilité", "Tr Sensibilité Cuisse G","Trouble de la vision")]<-"Déficit sensoriel ou trouble de la sensibilité"
  
  # Help inquiry
  ado$MOTIF[ado$MOTIF %in% c("Demande d'aide","Demande d'avis médical","Demande de Conseils")]<-"Demande d'aide, de conseil ou d'avis médical"
  
  # Placement inquiry
  ado$MOTIF[ado$MOTIF %in% c("Demande placement")]<-"Demande de placement"
  
  # Depression
  ado$MOTIF[ado$MOTIF %in% c("Dépression et troubles de l'humeur","Etat dépressif")]<-"Trouble ou symptômes dépressifs"
  
  #Abdominal pain
  ado$MOTIF[ado$MOTIF %in% c("Douleur Abdominale", "Douleurs abdominales")]<-"Douleur abdominale"
  
  # Chest pain
  ado$MOTIF[ado$MOTIF %in% c("Doul.thoraciques", "Douleur Thoracique")]<-"Douleur thoracique"
  
  # Limb pain
  ado$MOTIF[ado$MOTIF %in% c("Douleur d'un membre", "Douleur de Membre")]<-"Douleur de membre"
  
  # Dental pain
  ado$MOTIF[ado$MOTIF %in% c("Douleur Dentaire")]<-"Douleur dentaire"
  
  # Cervical pain
  ado$MOTIF[ado$MOTIF %in% c("Douleur Rachis cervical","Douleur Cervicale","Cervicalgie","Douleur Cou")]<-"Douleur cervicale"
  
  # Back pain
  ado$MOTIF[ado$MOTIF %in% c("Douleur Dorsale","Douleur Rachis dorsal")]<-"Douleur dorsale"
  
  # Low back pain
  ado$MOTIF[ado$MOTIF %in% c("Douleur Lombaire","Lombalgie basse")]<-"Douleur lombaire"
  
  # Pelvic pain
  ado$MOTIF[ado$MOTIF %in% c("Douleur Pelvienne")]<-"Douleur pelvienne"
  
  # Suicidal thoughts
  ado$MOTIF[ado$MOTIF %in% c("Idées Suicidaires")]<-"Idées suicidaires"
  
  # Intoxication
  ado$MOTIF[ado$MOTIF %in% c("Intox (Autre)")]<-"Intoxication autre"
  
  # Intox Cannabis
  ado$MOTIF[ado$MOTIF %in% c("Intox au Cannabis")]<-"Intoxication au Cannabis"
  
  # Faintness
  ado$MOTIF[ado$MOTIF %in% c("Malaise", "Malaise, fatigue")]<-"Malaise et/ou fatigue"
  
  # Faintness with loss of consciousness
  ado$MOTIF[ado$MOTIF %in% c("Malaise avec PCI", "Malaise + PCI")]<-"Malaise avec perte de conscience initiale"
  
  # Nausea or vomiting
  ado$MOTIF[ado$MOTIF %in% c("Nausées", "Nausées-Vomissements")]<-"Nausées ou vomissements"
  
  # Interruption of psychiatric treatment
  ado$MOTIF[ado$MOTIF %in% c("Rupture Traitement Psy", "Rupture trait. psy")]<-"Rupture de traitement psychiatrique"
  
  # Cranial trauma
  ado$MOTIF[ado$MOTIF %in% c("TC sans PC", "TC - PC", "TC SANS PC")]<-"TC - PC"
  
  # Consciousness
  ado$MOTIF[ado$MOTIF %in% c("Tr de la conscience", "Tr de la Conscience")]<-"Trouble de conscience"
  
  # Dizziness
  ado$MOTIF[ado$MOTIF %in% c("Vertiges", "Vertiges, étourdissements")]<-"Vertiges et/ou étourdissements"
  
  # Psychotic disorder
  ado$MOTIF[ado$MOTIF %in% c("Troubles psychotiques")]<-"Trouble psychotique"
  
## GROUPING motives
  # Psychiatric/Physical
  ado$NATURE_MOTIF<-ifelse(ado$MOTIF %in% c("Agitation","Agitation modérée","Agitation violente","Agitation,trouble de personnalité et du comportement","Agression, hétéro-agressivité","Anorexie","Compatibilité garde-à-vue", "Crise d'angoisse","Demande d'aide, de conseil ou d'avis médical","Demande de placement","Demande HDT","Examen à des fins administratives /certificat/ réquisitions","Hallucinations","Idées suicidaires","Insomnie / troubles du sommeil","Intox à des Psychotropes","Intoxication au Cannabis","Intox au Paracétamol","Intox Médicament. Volontaire","Intox Médicament sans précision","Intoxication autre","Intoxication Ethylique","Intoxication Fumées, Emanations","Polytoxicomanie","Possibles sévices physiques","Problème Médicolégal","Problème Social","Ralentissement psychomoteur","Retard du développement","Rupture de traitement psychiatrique","Schizophrénie, délire, hallucinations","Situation parentale atypique","Tentative d'autolyse","Tétanie","Toxicomanie aiguë","Trouble anxieux et/ou somatoforme","Troubles ou symptômes dépressifs","Trouble psychotique"),"Psychiatric","Physical")
  ado$NATURE_MOTIF<-ifelse(ado$MOTIF=="NA",NA,ado$NATURE_MOTIF)
  table(ado$NATURE_MOTIF,ado$ANNEE,useNA="always")
  table(ado$MOTIF=="NA",ado$ANNEE)
xtabs(~MOTIF,ado)
xtabs(~MOTIF+ANNEE,ado)
xtabs(~ANNEE,ado)

barplot(table(ado$NATURE_MOTIF,ado$ANNEE), main="Counts of psychiatric (grey) and physical(dark grey) motives at the ER", xlab="Years",ylab="Count")  
barplot(prop.table(table(ado$NATURE_MOTIF,ado$ANNEE),margin=2), main="Proportions of psychiatric (grey) and physical(dark grey) motives at the ER", xlab="Years",ylab="Count")  

  # Categories
  #Psy
  ado$MOTIF_CAT<-ado$MOTIF
  ado$MOTIF_CAT[ado$MOTIF_CAT %in% c("Agitation","Agitation modérée","Agitation, trouble de personnalité et du comportement","Agitation violente")]<-"Agitation and/or behaviour disorders"
  ado$MOTIF_CAT[ado$MOTIF_CAT %in% c("Hallucinations","Schizophrénie, délire, hallucinations", "Trouble psychotique")]<-"Psychotic disorders"
  ado$MOTIF_CAT[ado$MOTIF_CAT %in% c("Intoxication autre")]<-"Other Intoxication"
  ado$MOTIF_CAT[ado$MOTIF_CAT %in% c("Crise d'angoisse","Insomnie / troubles du sommeil","Trouble anxieux et/ou somatoforme")]<-"Anxiety disorders"
  ado$MOTIF_CAT[ado$MOTIF_CAT %in% c("Compatibilité garde à vue","Demande d'aide, de conseil ou d'avis médical", "Demande de placement","Demande HDT","Examen à des fins administratives /certificat / réquisitions","Possibles sévices physiques", "Problème Médicolégal","Problème Social","Ralentissement psychomoteur","Retard du développement","Situation parentale atypique","Tétanie")]<-"Other psychiatric complaints"
  ado$MOTIF_CAT[ado$MOTIF_CAT %in% c("Intoxication Ethylique","Intoxication au Cannabis","Polytoxicomanie", "Toxicomanie aiguë","Etat de manque")]<-"Drug use or abuse"
  ado$MOTIF_CAT[ado$MOTIF_CAT %in% c("Intox à des Psychotropes","Intox au Paracétamol","Intox Médicam. Volontaire","Intox Médicament sans précision")]<-"Tentative d'autolyse"
  
  #Non-psy
  ado$MOTIF_CAT[ado$MOTIF_CAT %in% c("Accident de la route","Accident de sport, loisirs","Accident domestique","Accident scolaire","Accident type non précisé")]<-"Accident"
  ado$MOTIF_CAT[ado$MOTIF_CAT %in% c("Adénopathies","Altération de l`état général","Anomalie de résultat biologique","Asthme","Ataxie","Brûlures mictionnelles","CE dans les voies digestives","Coma","Complication post opératoire","Corps étranger","Corps Etranger Ingéré","Crise d'Asthme","Crise d'asthme","Déficit moteur","Déficit moteur autre","Déficit sensoriel ou trouble de la sensibilité","Demande d'examen complémentaire","Démarche paralytique", "Détresse respiratoire","Diabète et Tr Glycémie","Drépanocytose","Dysménorrhée","Dyspnée","Dysurie","Epistaxis","Eruption","Fièvre","Hématome Oeil","Hémoptysie","Hyperglycémie","Ictère","Ictus amnésique","Infection cutanée","Mvt anormal","Nausées ou vomissements","Odynophagie","Oedeme","Oedème Langue","Otalgie","Palpitations","Pb alimentaires NN","Pb génital","Pb Ophtalmo","Perte d'appétit","Perte de poids anormale","Prurit","Rétention d'urine","Soins de contrôle chirurgie","Soins de contrôle orthopédique","Synd.paralytiques","Toux","Tr de la marche","Tr Neuro autre","Trble de la coagulation","Trble du rythme cardiaque","Tremblements","Trouble de conscience","Intoxication Fumées, Emanations")]<-"Other physical complaints"
  ado$MOTIF_CAT[ado$MOTIF_CAT %in% c("Céphalée progressive","Céphalées")]<-"Headache"
  ado$MOTIF_CAT[ado$MOTIF_CAT %in% c("Constipation","Diarrhée et GEA","Dysphagie","Hématémèse","Nausées ou vomissements","Odynophagie","Perte d'appétit","Perte de poids anormale","Synd.abdo.aigü","Vomissements")]<-"Digestive disorder"
  ado$MOTIF_CAT[ado$MOTIF_CAT %in% c("Contusion Cou","Contusion Rachis cervical","Dermabrasion Avant-Bras D","Dermabrasion Cuisse D","Dermabrasion Main G","Plaie Superf   AVB","Plaie Superf   BRAS","Plaie Superf   POIGNET","Plaie superficielle Abdomen","Plaie superficielle Avant-Bras D","Plaie superficielle Avant-Bras G","Plaie superficielle Bras G","Plaie superficielle Cou","Plaie superficielle Coude D","Plaie superficielle Cuir chevelu","Plaie superficielle Cuisse G","Plaie superficielle Doigt D","Plaie superficielle Doigt G","Plaie superficielle Front","Plaie superficielle Jambe G","Plaie superficielle Main G","Plaie superficielle Oreille","Plaie superficielle Poignet D","Plaie superficielle Poignet G", "Plaie superficielle Thorax")]<-"Bruises, dermabrasions and superficial wounds"
  ado$MOTIF_CAT[ado$MOTIF_CAT %in% c("Douleur abdominale","Douleur articulaire","Douleur cervicale","Douleur Côtes","Douleur Cuir chevelu","Douleur de membre","Douleur dentaire","Douleur des OGE","Douleur dorsale","Douleur Epigastrique","Douleur Genou D","Douleur Genou G","Douleur lombaire","Douleur Main D","Douleur pelvienne","Douleur Pied D","Douleur Poignet D","Douleur thoracique")]<-"Pain"
  ado$MOTIF_CAT[ado$MOTIF_CAT %in% c("TC + PC","TC + PC avec confusion","TC - PC")]<-"Cranial trauma"
  ado$MOTIF_CAT[ado$MOTIF_CAT %in% c("Trauma  COU","Trauma  DOIGT","Trauma  EPAULE","Trauma  PIED","Trauma MACHOIRE","Traumatisme Cheville G","Traumatisme Cou","Traumatisme Coude G","Traumatisme Doigt D","Traumatisme Epaule D","Traumatisme Genou D","Traumatisme Genou G","Traumatisme Machoire","Traumatisme Main D","Traumatisme Main G","Traumatisme Nez","Traumatisme Poignet D","Traumatisme Pommette","Plaie délabrante Jambe D","Plaie profonde Avant-Bras D","Plaie profonde Avant-Bras G","Plaie profonde Bras G","Plaie profonde Nez","Plaie profonde Pied D","Plaie profonde Poignet D")]<-"Other trauma"
  ado$MOTIF_CAT[ado$MOTIF_CAT %in% c("Malaise et/ou fatigue","Malaise sans PCI","Trouble de conscience","Vertiges et/ou étourdissements","Syncope et collapsus")]<-"Dizziness and/or disorder of consciousness"
  
  # Traducing remaining categories 
  ado$MOTIF_CAT[ado$MOTIF_CAT %in% c("Agitation violente")]<-"Agitation (violent)"
  ado$MOTIF_CAT[ado$MOTIF_CAT %in% c("Agression, hétéro-agressivité")]<-"Aggression, hetero-aggressivity"
  ado$MOTIF_CAT[ado$MOTIF_CAT %in% c("Anorexie")]<-"Anorexia"
  ado$MOTIF_CAT[ado$MOTIF_CAT %in% c("Auto-agressivité")]<-"Auto-aggressivity"
  ado$MOTIF_CAT[ado$MOTIF_CAT %in% c("Idées suicidaires")]<-"Suicidal thoughts"
  ado$MOTIF_CAT[ado$MOTIF_CAT %in% c("Malaise avec perte de conscience initiale")]<-"Dizziness with initial loss of consciousness"
  ado$MOTIF_CAT[ado$MOTIF_CAT %in% c("Rupture de traitement psychiatrique")]<-"Stopped psychiatric treatment"
  ado$MOTIF_CAT[ado$MOTIF_CAT %in% c("Tentative d'autolyse")]<-"Suicidal attempt"
  ado$MOTIF_CAT[ado$MOTIF_CAT %in% c("Trouble anxieux et/ou somatoforme")]<-"Anxious and/or somatoform disorder"
  ado$MOTIF_CAT[ado$MOTIF_CAT %in% c("Trouble ou symptômes dépressifs")]<-"Depressive symptoms or disorder"
  
  #Graph
  xtabs(~MOTIF_CAT+ANNEE,ado,addNA = T)
  library("openxlsx")
  write.xlsx(xtabs(~MOTIF_CAT+ANNEE,ado,addNA = T), file = "MOTIF_CAT+ANNEE.xlsx",
             sheetName = "MOTIF_CAT+ANNEE", append = FALSE)
  
                prop.table(xtabs(~MOTIF_CAT+ANNEE,ado,addNA = T),margin=2) 
                library("openxlsx")
                write.xlsx((prop.table(xtabs(~MOTIF_CAT+ANNEE,ado,addNA = T),margin=2)*100), file = "MOTIF_CAT+ANNEE1.xlsx",
                           sheetName = "MOTIF_CAT+ANNEE1", append = FALSE)

library(ggplot2)
library(extrafont)
p<-ggplot(data=ado, aes(x=factor(MOTIF_CAT))) +
  geom_bar(stat="count")+theme(axis.text.x = element_text(angle = 55, hjust = 1,size=12))
p

  # Additionnal grouping
  ado$MOTIF_CAT2<-ado$MOTIF_CAT
  ado$MOTIF_CAT2[ado$MOTIF_CAT2 %in% c("Accident","Other trauma","Cranial trauma","Deep wound")]<-"Accident and trauma"
  ado$MOTIF_CAT2[ado$MOTIF_CAT2 %in% c("Other physical complaints","Digestive disorder","Headache", "Convulsions","Bruises, dermabrasions and superficial wounds","Dizziness and/or disorder of consciousness","Dizziness with initial loss of consciousness","Pain")]<-"Somatic complaints"
  ado$MOTIF_CAT2[ado$MOTIF_CAT2 %in% c("Auto-aggressivity", "Suicidal attempt")]<-"Suicidal attempts and Non Suicidal Self Injury"
  ado$MOTIF_CAT2[ado$MOTIF_CAT2 %in% c("Anorexia","Other Intoxication","Compatibility for police custody","Examination for administrative purposes/certificates/requisitioning")]<- "Other psychiatric complaints"
  ado$MOTIF_CAT2[ado$MOTIF_CAT2 %in% c("Stopped psychiatric treatment")]<-"Other psychiatric complaints"
  ado$MOTIF_CAT2[is.na(ado$MOTIF_CAT2)] <- 0 # replace na by 0
  
  # Graph
  xtabs(~MOTIF_CAT2+ANNEE,ado,addNA = T)
  options(digits=1)
  prop.table(xtabs(~MOTIF_CAT2+ANNEE,ado,addNA = T),margin=2)*100
  library("openxlsx")
  write.xlsx(xtabs(~MOTIF_CAT2+ANNEE,ado,addNA = T), file = "MOTIF_CAT2+ANNEE.xlsx",
             sheetName = "MOTIF_CAT2+ANNEE", append = FALSE)
  
              prop.table(xtabs(~MOTIF_CAT2+ANNEE,ado,addNA = T),margin=2) 
              library("openxlsx")
              write.xlsx((prop.table(xtabs(~MOTIF_CAT2+ANNEE,ado,addNA = T),margin=2)*100), file = "MOTIF_CAT2+ANNEE1.xlsx",
                         sheetName = "MOTIF_CAT2+ANNEE1", append = FALSE)
  
  p2<-ggplot(data=ado, aes(x=factor(MOTIF_CAT2))) +
    geom_bar(stat="count")+theme(axis.text.x = element_text(angle = 55, hjust = 1,size=12))
  p2
  
      # p-tests
      prop.test(xtabs(~ANNEE+(MOTIF_CAT=="Agitation and/or behaviour disorders"),ado))
      prop.test(xtabs(~ANNEE+(MOTIF_CAT=="Suicidal attempt"),ado))
      prop.test(xtabs(~ANNEE+(MOTIF_CAT=="Drug use or abuse"),ado))
      prop.test(xtabs(~ANNEE+(MOTIF_CAT=="Anxiety disorders"),ado))
      prop.test(xtabs(~ANNEE+(MOTIF_CAT=="Suicidal thoughts"),ado))
      prop.test(xtabs(~ANNEE+(MOTIF_CAT=="Depressive symptoms or disorder"),ado))
      prop.test(xtabs(~ANNEE+(MOTIF_CAT=="Psychotic disorders"),ado))
  
  #Analysis of diagnosis
      # matching diagnoses
      xtabs(~DP+PSYDP,ado)
      library("openxlsx")
      write.xlsx(xtabs(~DP+PSYDP,ado), file = "DP+PSYDP.xlsx",
                 sheetName = "DP+PSYDP", append = FALSE)
      
  
      xtabs(~is.na(DP)&is.na(DA1)&is.na(DA2)&is.na(DA3)&is.na(DA4)&is.na(DA5)&is.na(PSYDP)&is.na(PSYDA1)&is.na(PSYDA2)&is.na(PSYDA3)&is.na(PSYDA4)&is.na(PSYDA5),ado)
      ado$diagnosis_retained<-ado$DP
      table(ado$diagnosis_retained,useNA="always")
      ado$diagnosis_retained<-with(ado, ifelse(is.na(diagnosis_retained), DA1, diagnosis_retained))
      ado$diagnosis_retained<-with(ado, ifelse(is.na(diagnosis_retained), DA2, diagnosis_retained))
      ado$diagnosis_retained<-with(ado, ifelse(is.na(diagnosis_retained), DA3, diagnosis_retained))
      ado$diagnosis_retained<-with(ado, ifelse(is.na(diagnosis_retained), DA4, diagnosis_retained))
      ado$diagnosis_retained<-with(ado, ifelse(is.na(diagnosis_retained), DA5, diagnosis_retained))
      ado$diagnosis_retained<-with(ado, ifelse(is.na(diagnosis_retained), PSYDP, diagnosis_retained))
      ado$diagnosis_retained<-with(ado, ifelse(is.na(diagnosis_retained), PSYDA1, diagnosis_retained))
      ado$diagnosis_retained<-with(ado, ifelse(is.na(diagnosis_retained), PSYDA2, diagnosis_retained))
      ado$diagnosis_retained<-with(ado, ifelse(is.na(diagnosis_retained), PSYDA3, diagnosis_retained))
      ado$diagnosis_retained<-with(ado, ifelse(is.na(diagnosis_retained), PSYDA4, diagnosis_retained))
      ado$diagnosis_retained<-with(ado, ifelse(is.na(diagnosis_retained), PSYDA5, diagnosis_retained))

      xtabs(~diagnosis_retained,ado)
      
      #ado$diagnosis_retained<-with(ado, ifelse(is.na(diagnosis_retained), MOTIF, diagnosis_retained))
      
      xtabs(~diagnosis_retained,ado)
      table(ado$diagnosis_retained,ado$ANNEE,useNA="always")
      
      library("openxlsx")
      write.xlsx(xtabs(~diagnosis_retained+ANNEE,ado), file = "diagnosis_retained.xlsx",
                 sheetName = "diagnosis_retained", append = FALSE)
      
      # creating categories
      ado$diagnosis_retained[ado$diagnosis_retained %in% c("A86","B341","B349","C950","E106","E109","E162","E86","G403","G408","G409","G419","G432","G433","G439","G442","G443","G454","G478","G479","H651","J00","J038","J039","J101","J111","J180","J340","J450","J459","J939","J960","K088","K291","K297","K359","K529","K719","K802","K859","K914","L509","M2550","M5429","M543","M544","M545","M62890","M765","M796","M7964","M7966","N10","N300","N390","N62","R000","R001","R05","R060","R064","R072","R073","R074","R104","R11","R17","R251","R258","R262","R290","R509","R51","R520","R5218","R522","R53","R53+0","R53+1","R53+2","R55","R568","R688","R739","R410","T182","T185","T621","T670","Z000","Z016","Z208","Z639","Z940")]<-"Physical complain"
      ado$diagnosis_retained[ado$diagnosis_retained %in% c("F061","F063","F0632","F064","F066","F068","F069","F072","F078","F079","F09","F44","F441","F444","F447","F448","F4482","F449","F510","F513","F519","F520","F54","F59","F638","F680","F681","F700","F701","F708","F710","F711","F721","F780","F781","F791","F798","F813","F83","F840","F845","F848","F849","F88","F89","F99","R400","R452","R454","R457","R458","R468","T749","Z004","Z022","Z032","Z133","Z04880","Z532","Z539","Z597","Z601","Z604","Z608","Z609","Z612","Z622","Z636","Z659","Z711","Z719","Z725","Z751","Z818","Z865","Z914")]<-"Other psychiatric disorders"
      ado$diagnosis_retained[ado$diagnosis_retained %in% c("F100","F1000","F1005","F1007","101","F102","F108","F109","F110","F1100","F1105","F1107","F113","F115","F120","F1200","F1202","F1207","F121","F122","F123","F125","F128","F129","F130","F1300","F1302","F1304","F1307","F1330","F140","F142","F149","F1504","F160","F165","F1670","F190","F1900","F1902","F1907","F191","F192","F195","F199","R780","R510","R518","R519","Y150","Y159","Y906","Y909","Y911","Z715","Z8783","T510","T518","T519","F101")]<-"Substance use disorder"
      ado$diagnosis_retained[ado$diagnosis_retained %in% c("F200","F201","F203","F209","F21","F220","F229","F230","F231","F232","F233","F238","F239","F2391","F252","F28","F29","F531","F538","R413","R440","R441","R443","R448")]<-"Psychotic disorder or symptoms"
      ado$diagnosis_retained[ado$diagnosis_retained %in% c("F302","F3130","F316","F319","F348","F349","F380","F388","F39")]<-"Other affective disorder"
      ado$diagnosis_retained[ado$diagnosis_retained %in% c("F320","F3200","F321","F3211","F322","F323","F328","F329","F332","F339")]<-"Depressive disorder"
      ado$diagnosis_retained[ado$diagnosis_retained %in% c("F400","F401","F402","F408","F409","F410","F4100","F4101","F411","F412","F413","F418","F419","F420","F421","R466")]<-"Anxious disorder"
      ado$diagnosis_retained[ado$diagnosis_retained %in% c("F430","F4300","F4301","F431","F432","F4322","F4323","F4324","F438","F439")]<-"Reaction to stress and adaptation disorder"
      ado$diagnosis_retained[ado$diagnosis_retained %in% c("F450","F451","F454","F458","F459")]<-"Somatoform disorder"
      ado$diagnosis_retained[ado$diagnosis_retained %in% c("F500","F501","F502","F505","F508","F509","F982","R630")]<-"Eating disorder"
      ado$diagnosis_retained[ado$diagnosis_retained %in% c("F600","F602","F603","F6031","F604","F606","F607","F608","F609")]<-"Personality disorder"
      ado$diagnosis_retained[ado$diagnosis_retained %in% c("F688","F69","F988","F989","R451","R462","R456")]<-"Behaviour disorder and/or Agitation"
      ado$diagnosis_retained[ado$diagnosis_retained %in% c("F901","F908","F910","F911","F912","F913","F918","F919","F920","F928","F929","F930","F931","F932","F938","F9380","F939","F940","F941","F948","F949","F958")]<-"Hyperactivity or Conduct, Emotional or Social functioning Disorder"
      ado$diagnosis_retained[ado$diagnosis_retained %in% c("T362","T381","T383","T388","T389","T390","T391","T393","T398","T399","T402","T407","T424","T426","T427","T430","T431","T432","T433","T435","T436","T438","T439","T447","T450","T455","T461","T464","T465","T469","T475","T478","T479","T481","T483","T486","T490","T492","T498","T509","T529","T539","T543","T549","T71","Y110","Z036")]<-"Acute medication poisonning and other suicide attempts"
      ado$diagnosis_retained[ado$diagnosis_retained %in% c("Y0589","Y355","Z027","Z028","Z044","Z046","Z614","Z615","Z616")]<-"Legal reasons, agressions, abuses"
      ado$diagnosis_retained[ado$diagnosis_retained %in% c("T794","V9542","Z045","S003","S007","S008","S011","S015","S0220","S0221","S0260","S060","S0600","S065","S069","S109","S203","S311","S318","S3230","S360","S400","S411","S460","S508","S509","S510","S517","S518","S519","S5250","S602","S608","S609","S610","S617","S618","S619","S623","S6230","S6260","S636","S711","S800","S809","S810","S8270","S903","S913","S920","S934","T042")]<-"Accident, trauma and wound"
      
      xtabs(~diagnosis_retained+ANNEE,ado)
      
# Assembling similar outcomes : à vérifier attention
  # DEVENIR_POST_URG
  ado$DEVENIR_POST_URG[ado$DEVENIR_POST_URG %in% c("Domicile")]<-"DOMICILE" # Home
  ado$DEVENIR_POST_URG[ado$DEVENIR_POST_URG %in% c("Fugue")]<-"FUGUE" # Runaway
  ado$DEVENIR_POST_URG[ado$DEVENIR_POST_URG %in% c("DEVENIR_NR")]<-"NR" # unknown
  
  xtabs(~DEVENIR_POST_URG+ANNEE,ado)
  
  # Create classification
  ado$DEVENIR_CAT <- ado$DEVENIR_POST_URG
  xtabs(~is.na(ado$DEVENIR_POST_URG)+ANNEE,ado)
  ado$DEVENIR_CAT[is.na(ado$DEVENIR_CAT)] <- 0
 
  xtabs(~DEVENIR_CAT+ ANNEE,ado)
  options(digits=1)
  prop.table(xtabs(~DEVENIR_CAT+ ANNEE,ado),margin=2)*100
  # ado$DEVENIR_CAT[ado$DEVENIR_CAT %in% c("CHU : Chir Générale, Digestive", "CHU : Gastro-entérologie", "CHU : Médecine interne (hors PUM)","CHU : Médecine pénitentiaire","CHU : Neurochirurgie","CHU : Neurologie ","CHU : ORL","CHU : Pneumologie", "CHU : Purpan NR", "CHU : Rangueil NR","CHU : Réanimation","CHU : Traumatologie","CHU : Urgences Gynéco PDV","TREXT : CH ARIEGE COUSERANS","TREXT : CH LANNEMEZAN","TREXT : LIMOUX-QUILLAN","TREXT : CHU LA COLOMBIERE MONTPELLIER","TREXT : CLINIQUE ARAGOU LES TILLEULS","TREXT : CLINIQUE DES CEDRES","TREXT : HOPITAL JOSEPH DUCUING","TREXT : HU-SAINT LOUIS SITE SAINT LOUIS","TREXT : POLYCLINIQUE JEAN VILLAR","TREXT : SSR PARIS SUD")]<-"Hospitalisation somatique" 
  # ado$DEVENIR_CAT[ado$DEVENIR_CAT %in% c("DOMICILE", "DOMICILE : CENTRE DEP ENFANCE ET FAMILLE", "Sortie")]<-"Domicile"
  # ado$DEVENIR_CAT[ado$DEVENIR_CAT %in% c("Mutation","Transfert")]<-"Hospitalisation sans précision" 
  # ado$DEVENIR_CAT[ado$DEVENIR_CAT %in% c("CHU : Psychiatrie","CHU : Casselardit NR","TREXT : CHS MARCHANT (psy)","TREXT : CLINIQUE AUFRERY (psy)","TREXT : CLINIQUE CASTELVIEL (psy)","TREXT : CLINIQUE DE BEAUPUY (psy)","TREXT : CH LEON JEAN GREGORY (psy)","TREXT : CLINIQUE DE MONTBERON (psy)","TREXT : CLINIQUE DU PAYS D'OC (psy)","TREXT : CLINIQUE MARIGNY (psy)","TREXT : IME VAL FLEURI (psy)","TREXT : NR (psy)")]<-"Hospitalisation psychiatrique" 
  
  ado$DEVENIR_CAT[ado$DEVENIR_CAT %in% c("CHU : Chir Générale, Digestive", "CHU : Gastro-entérologie", "CHU : Médecine interne (hors PUM)","CHU : Médecine Pénitentiaire","CHU : Neurochirurgie","CHU : Neurologie","CHU : ORL","CHU : Pneumologie", "CHU : Purpan NR", "CHU : Rangueil NR","CHU : Réanimation","CHU : Traumatologie","CHU : Urgences Gynéco PDV","CHU : Réa-SMC HE","CHU : Urgences PU")]<-"Hospitalisation somatique CHU"
  ado$DEVENIR_CAT[ado$DEVENIR_CAT %in% c("TREXT : CH ARIEGE COUSERANS","TREXT : CH LANNEMEZAN","TREXT : CH LIMOUX-QUILLAN","TREXT : CHU LA COLOMBIERE MONTPELLIER","TREXT : CLINIQUE ARAGOU LES TILLEULS","TREXT : CLINIQUE DES CEDRES","TREXT : HOPITAL JOSEPH DUCUING","TREXT : HU-SAINT LOUIS SITE SAINT LOUI","TREXT : POLYCLINIQUE JEAN VILLAR","TREXT : SSR PARIS SUD")]<-"Hospitalisation somatique hors CHU" 
  ado$DEVENIR_CAT[ado$DEVENIR_CAT %in% c("DOMICILE", "DOMICILE : CTRE DEP. ENFANCE ET FAMILLE", "Sortie","9292","PSA","SCAM","91","98","sortie","TREXT : IME VAL FLEURI")]<-"Domicile"
  ado$DEVENIR_CAT[ado$DEVENIR_CAT %in% c("Mutation","Transfert","1296","2420","B111","B117","UHCD")]<-"Hospitalisation sans précision" 
  ado$DEVENIR_CAT[ado$DEVENIR_CAT %in% c("CHU : Psychiatrie","CHU : Casselardit NR","TREXT : CHS MARCHANT (psy)", "CHU : Chirurgie HE","CHU : Enfants Brûlés HE","CHU : Hémato-Oncologie HE","CHU : Hosp. Jour Chir. HE","CHU : Méd-Chir HE","CHU : Medecine HE","CHU : Péd. Générale M-C. HE","CHU : Pédiatrie PP Riquet HE","CHU : Secteur protégé HE","CHU : UA PPR HE","CHU : Unité Relais HE")]<-"Hospitalisation psychiatrique CHU"
  ado$DEVENIR_CAT[ado$DEVENIR_CAT %in% c("TREXT : CLINIQUE AUFRERY (psy)","TREXT : CLINIQUE CASTELVIEL (psy)","TREXT : CLINIQUE DE BEAUPUY (psy)","TREXT : CH LEON JEAN GREGORY (psy)","TREXT : CLINIQUE DE MONTBERON (psy)","TREXT : CLINIQUE DU PAYS D'OC (psy)","TREXT : CLINIQUE MARIGNY (psy)","TREXT : IME VAL FLEURI (psy)","TREXT : NR (psy)")]<-"Hospitalisation psychiatrique hors CHU" 
  ado$DEVENIR_CAT[ado$DEVENIR_CAT %in% c("0","DEVENIR_NR","DOMICILE : EHPAD L'ALBERGUE","DOMICILE : EHPAD LES ARCADES","NR","TREXT : NR")]<-"Devenir inconnu"
  
  length(unique(ado$N_PATIENT[which(ado$DEVENIR_CAT=="Devenir inconnu"&ado$ANNEE==2019)]))
  

  
 

  
### Descriptive analysis
# Quantitative variables

summary(ado) # Global description of all quantitative variables in data


  # DELAI_RETOUR (time before return visit, in days) :
  summary(ado$DELAI_RETOUR)
  sd(ado$DELAI_RETOUR, na.rm=T)
  var(ado$DELAI_RETOUR, na.rm=T)
  hist(ado$DELAI_RETOUR, main = "Delay of return visit (days)", xlab = "Time before return visit (days)", ylab="Count",ylim=c(0,2500), las=1,col="grey")
  boxplot(ado$DELAI_RETOUR)

  table(ado$DELAI_RETOUR<1) # 174 values under 1
  
    # Analysis of repeated visits
    xtabs(~passage_répété,ado)
    length(ado$N_PATIENT[which(duplicated(ado$N_PATIENT)==F)]) # counts patients
    length(ado$N_PATIENT[which(duplicated(ado$N_PATIENT)==T)]) # counts patients
    
    length(unique(ado$N_PATIENT)) # same
    
                  PR_data<-as.data.frame(xtabs(~N_PATIENT[which(ado$passage_répété==1)],ado))
                  names(PR_data) <- c("N_PATIENT2","Freq")
                  View(PR_data)
                  summary(PR_data$Freq)
                  hist(PR_data$Freq)
              
                  # creating the new dataframe
                  
                  library(dplyr)
                  dplyr::count(ado,N_PATIENT)
                  
                  ado$N_DOSSIER<-ado$`N°DOSSIER`
                  
                  visits_number = ado  %>% 
                    group_by(N_PATIENT,Sexe,Secteur) %>% 
                    summarise(visits_number=n_distinct((N_DOSSIER)))
                    
                  
                  VN_data <- as.data.frame(visits_number)
                  summary(VN_data)
                  
                  # analyse multiple visits 
                  
                  # globally
                  VN_data$multiple_visits<-ifelse(VN_data$visits_number>1,1,0)
                  xtabs(~multiple_visits,VN_data)
                  chisq.test(xtabs(~multiple_visits,VN_data))$expected # all over 5 so we can do the test
                  chisq.test(xtabs(~multiple_visits,VN_data))
                  
                  # yearly
                  options(digits=3)
                  length(VN_data$N_PATIENT)
                  xtabs(~Sexe,VN_data)
                  prop.table(xtabs(~Sexe,VN_data))
                  xtabs(~Secteur, VN_data)
                  prop.table(xtabs(~Secteur, VN_data))
                  
                  VN_data_1 <- (VN_data %>% filter(visits_number>1))
                  summary(VN_data_1)
                  sd (VN_data_1$visits_number)
                  var (VN_data_1$visits_number)
                  hist(VN_data_1$visits_number)
                  length(VN_data_1$N_PATIENT)
                  

                  
  # AGE
  summary(ado$AGE)
  sd(ado$AGE, na.rm=T)
  var(ado$AGE, na.rm=T)
  hist(ado$AGE, main = "Age of patients (years)", xlab = "Age (years)", ylab="Count",ylim=c(0,2000), las=1,col="grey")
  boxplot(ado$AGE)
  
  median(ado$AGE[which(ado$passage_répété==0)])
  median(ado$AGE[which(ado$passage_répété==0&ado$ANNEE==2014)])
  median(ado$AGE[which(ado$passage_répété==0&ado$ANNEE==2015)])
  median(ado$AGE[which(ado$passage_répété==0&ado$ANNEE==2016)])
  median(ado$AGE[which(ado$passage_répété==0&ado$ANNEE==2017)])
  median(ado$AGE[which(ado$passage_répété==0&ado$ANNEE==2018)])
  median(ado$AGE[which(ado$passage_répété==0&ado$ANNEE==2019)])
  
  xtabs(~AGE+ANNEE,ado)
  ado$AGE2<-ifelse(ado$AGE<15,1,2)
  xtabs(~AGE2+ANNEE,ado) # Yearly
  options(digits=3)
  prop.table(xtabs(~AGE2+ANNEE,ado),margin=2)*100
  chisq.test(xtabs(~AGE2+ANNEE,ado))$expected # all above 5 so test is ok
  chisq.test(xtabs(~AGE2+ANNEE,ado))
  
  
  # First age per year analysis
    library(dplyr)
    first_age = ado  %>% 
    group_by(N_PATIENT,ANNEE,Sexe) %>% 
    summarise(first_age=min(AGE))
    
    FA <- as.data.frame(first_age)
    xtabs(~ANNEE+first_age+Sexe,FA)
    xtabs(~ANNEE+(first_age>14)+Sexe,FA)
    prop.table(xtabs(~ANNEE+(first_age>14),FA),margin=1)*100
    
    xtabs(~ANNEE+(AGE>14)+Sexe,ado)
  
# Variables that could be considered quantitatives or qualitatives
  
  # MOIS (Month)
  summary(ado$MOIS)
  sd(ado$MOIS, na.rm=T)
  var(ado$MOIS, na.rm=T)
  
  xtabs(~MOIS,ado)
  prop.table(xtabs(~MOIS,ado))
  barplot(xtabs(~as.factor(ado$MOIS)),main = "Month of visit", xlab = "Month of the year", ylab="Count", ylim=c(0,1000))
  
  # HEURE (Hour)
  summary(ado$HEURE)
  sd(ado$HEURE, na.rm=T)
  var(ado$HEURE, na.rm=T)
  barplot(xtabs(~as.factor(ado$HEURE)),main = "Hour of visit", xlab = "Hour", ylab="Count", ylim=c(0,1000))

# Qualititative variables
  #SU_he_ad
  xtabs(~SU_he_ad,ado)
  prop.table(xtabs(~SU_he_ad,ado))
  
    # first visit
    xtabs(~SU_he_ad[which(ado$passage_répété==0)],ado)
    prop.table(xtabs(~SU_he_ad[which(ado$passage_répété==0)],ado))
    chisq.test(xtabs(~SU_he_ad[which(ado$passage_répété==0)],ado))$expected # Theorical counts, all above 5 so we can use the test
    chisq.test(xtabs(~SU_he_ad[which(ado$passage_répété==0)],ado))
    
    # first visit yearly
    xtabs(~SU_he_ad[which(ado$passage_répété==0)]+ANNEE[which(ado$passage_répété==0)],ado)
    prop.table(xtabs(~SU_he_ad[which(ado$passage_répété==0)]+ANNEE[which(ado$passage_répété==0)],ado,),margin=2)
    chisq.test(xtabs(~SU_he_ad[which(ado$passage_répété==0)]+ANNEE[which(ado$passage_répété==0)],ado))$expected # Theorical counts, all above 5 so we can use the test
    chisq.test(xtabs(~SU_he_ad[which(ado$passage_répété==0)]+ANNEE[which(ado$passage_répété==0)],ado))
  
  xtabs(~SU_he_ad+ANNEE,ado)
  prop.table(xtabs(~SU_he_ad+ANNEE,ado),margin=2)
  
  barplot(xtabs(~SU_he_ad+ANNEE,ado), ylim=c(0,2000),main="Female (dark grey) and male (grey) visitors at the ER for psychiatric complaints", xlab="Years",ylab="Count")
  
  chisq.test(xtabs(~SU_he_ad,ado))$observed # Observed counts for chi2 test
  chisq.test(xtabs(~SU_he_ad,ado))$expected # Theorical counts, all above 5 so we can use the test
  chisq.test(xtabs(~SU_he_ad,ado))
   
  chisq.test(xtabs(~SU_he_ad+ANNEE,ado))$expected # Theorical counts, all above 5 so we can use the test
  chisq.test(xtabs(~SU_he_ad+ANNEE,ado))
  
  # Sexe (SEX)
  options(digits=6)
  xtabs(~Sexe,ado)
  prop.table(xtabs(~Sexe,ado))*100
  xtabs(~Sexe+ANNEE,ado)
  prop.table(xtabs(~Sexe+ANNEE,ado),margin=2)*100
  
  barplot(xtabs(~Sexe+ANNEE,ado), ylim=c(0,2000),main="Female (dark grey) and male (grey) visitors at the ER for psychiatric complaints", xlab="Years",ylab="Count")
  
  chisq.test(xtabs(~Sexe,ado))$observed # Observed counts for chi2 test
  chisq.test(xtabs(~Sexe,ado))$expected # Theorical counts, all above 5 so we can use the test
  chisq.test(xtabs(~Sexe,ado))
  
  chisq.test(xtabs(~Sexe+ANNEE,ado))$observed # Observed counts for chi2 test
  chisq.test(xtabs(~Sexe+ANNEE,ado))$expected # Theorical counts, all above 5 so we can use the test
  chisq.test(ado$Sexe,ado$ANNEE,correct=T)
      
      # Population
      # globally
      length(unique(ado$N_PATIENT[which(ado$Sexe=="F")]))
      length(unique(ado$N_PATIENT[which(ado$Sexe=="M")]))
      # Count of girls and boys yearly
      length(unique(ado$N_PATIENT[which(ado$Sexe=="F"&ado$ANNEE==2014)]))
      length(unique(ado$N_PATIENT[which(ado$Sexe=="M"&ado$ANNEE==2014)]))
      length(unique(ado$N_PATIENT[which(ado$Sexe=="F"&ado$ANNEE==2015)]))
      length(unique(ado$N_PATIENT[which(ado$Sexe=="M"&ado$ANNEE==2015)]))
      length(unique(ado$N_PATIENT[which(ado$Sexe=="F"&ado$ANNEE==2016)]))
      length(unique(ado$N_PATIENT[which(ado$Sexe=="M"&ado$ANNEE==2016)]))
      length(unique(ado$N_PATIENT[which(ado$Sexe=="F"&ado$ANNEE==2017)]))
      length(unique(ado$N_PATIENT[which(ado$Sexe=="M"&ado$ANNEE==2017)]))
      length(unique(ado$N_PATIENT[which(ado$Sexe=="F"&ado$ANNEE==2018)]))
      length(unique(ado$N_PATIENT[which(ado$Sexe=="M"&ado$ANNEE==2018)]))
      length(unique(ado$N_PATIENT[which(ado$Sexe=="F"&ado$ANNEE==2019)]))
      length(unique(ado$N_PATIENT[which(ado$Sexe=="M"&ado$ANNEE==2019)]))

      # First visit Globally
      xtabs(~Sexe[which(ado$passage_répété==0)],ado)
      prop.table( xtabs(~Sexe[which(ado$passage_répété==0)],ado))
      chisq.test(xtabs(~Sexe[which(ado$passage_répété==0)],ado))$expected # all over 5 so we can do the test
      chisq.test(xtabs(~Sexe[which(ado$passage_répété==0)],ado))
      # First visit Yearly
      xtabs(~Sexe[which(ado$passage_répété==0)]+ANNEE[which(ado$passage_répété==0)],ado)
      prop.table(xtabs(~Sexe[which(ado$passage_répété==0)]+ANNEE[which(ado$passage_répété==0)],ado,),margin=2)
      chisq.test(xtabs(~Sexe[which(ado$passage_répété==0)]+ANNEE[which(ado$passage_répété==0)],ado))$expected # Theorical counts, all above 5 so we can use the test
      chisq.test(xtabs(~Sexe[which(ado$passage_répété==0)]+ANNEE[which(ado$passage_répété==0)],ado))
      
  # ANNEE (Year)
  xtabs(~ANNEE,ado)
  prop.table(xtabs(~ANNEE,ado))
  barplot(xtabs(~as.factor(ado$ANNEE)),main = "Year of visit", xlab = "Year", ylab="Count", ylim=c(0,2000))
  
      # population
      length(unique(ado$N_PATIENT[which(ado$ANNEE==2014)]))
      length(unique(ado$N_PATIENT[which(ado$ANNEE==2015)]))
      length(unique(ado$N_PATIENT[which(ado$ANNEE==2016)]))
      length(unique(ado$N_PATIENT[which(ado$ANNEE==2017)]))
      length(unique(ado$N_PATIENT[which(ado$ANNEE==2018)]))
      length(unique(ado$N_PATIENT[which(ado$ANNEE==2019)]))
      
  # JSEM (Day in the week)
  levels(as.factor(ado$JSEM))
  ado$JSEM_FACTOR <- factor(ado$JSEM,levels=c("Lundi","Mardi","Mercredi","Jeudi", "Vendredi", "Samedi", "Dimanche"))
  levels(JSEM_FACTOR)
  xtabs(~JSEM_FACTOR,ado)
  prop.table(xtabs(~JSEM_FACTOR,ado))
  barplot(xtabs(~JSEM_FACTOR),main = "Day of visit", xlab = "Day of the week", ylab="Count", ylim=c(0,2000))
  
  chisq.test(xtabs(~JSEM_FACTOR,ado),correct=T)$expected # all above 5
  chisq.test(xtabs(~JSEM_FACTOR,ado),correct=T)

  ado$weekend<-ifelse(ado$JSEM==c("Samedi","Dimanche"),1,0)
  xtabs(~weekend,ado)
  prop.table(xtabs(~weekend,ado))
  chisq.test(xtabs(~weekend,ado),correct=T)$expected # all above 5
  chisq.test(xtabs(~weekend,ado),correct=T)
  
  t.test(xtabs(~weekend,ado))
  
  # MOTIF (Reason for consultation)
  xtabs(~MOTIF+ANNEE,ado)
  barplot(xtabs(~MOTIF,ado),main = "Day of visit", xlab = "Day of the week", ylab="Count", ylim=c(0,2000))
  
  library("openxlsx")
  write.xlsx(xtabs(~MOTIF+ANNEE,ado), file = "MOTIF.xlsx",
             sheetName = "MOTIF", append = FALSE)

      # Population for categories
        xtabs(~MOTIF_CAT2+Sexe+ANNEE,ado)
    
        library("openxlsx")
        write.xlsx(xtabs(~MOTIF_CAT2+Sexe+ANNEE,ado), file = "MOTIF+Sexe+année.xlsx",
                   sheetName = "MOTIF+Sexe+Année", append = FALSE)
        
        xtabs(~MOTIF_CAT2+AGE2+ANNEE,ado)
        library("openxlsx")
        write.xlsx(xtabs(~MOTIF_CAT2+AGE2+ANNEE,ado), file = "MOTIF+AGE2+année.xlsx",
                   sheetName = "MOTIF+AGE2+Année", append = FALSE)
      
  # GEO CODE
  options(digits=1)
  xtabs(~Secteur,ado)
  prop.table(xtabs(~Secteur,ado))
  xtabs(~Secteur+ANNEE,ado)
  prop.table(xtabs(~Secteur+ANNEE,ado),margin=2)*100
  
    # Population
    #Globally
    length(unique(ado$N_PATIENT[which(ado$Secteur=="Toulouse")]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="1")]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="2")]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="3")]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="missing")]))

    #Yearly
    #2014
    length(unique(ado$N_PATIENT[which(ado$Secteur=="Toulouse"&ado$ANNEE==2014)]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="1"&ado$ANNEE==2014)]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="2"&ado$ANNEE==2014)]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="3"&ado$ANNEE==2014)]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="missing"&ado$ANNEE==2014)]))
    
    #2015
    length(unique(ado$N_PATIENT[which(ado$Secteur=="Toulouse"&ado$ANNEE==2015)]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="1"&ado$ANNEE==2015)]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="2"&ado$ANNEE==2015)]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="3"&ado$ANNEE==2015)]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="missing"&ado$ANNEE==2015)]))
    
    # 2016
    length(unique(ado$N_PATIENT[which(ado$Secteur=="Toulouse"&ado$ANNEE==2016)]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="1"&ado$ANNEE==2016)]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="2"&ado$ANNEE==2016)]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="3"&ado$ANNEE==2016)]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="missing"&ado$ANNEE==2016)]))
    # 2017
    length(unique(ado$N_PATIENT[which(ado$Secteur=="Toulouse"&ado$ANNEE==2017)]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="1"&ado$ANNEE==2017)]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="2"&ado$ANNEE==2017)]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="3"&ado$ANNEE==2017)]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="missing"&ado$ANNEE==2017)]))
    # 2018
    length(unique(ado$N_PATIENT[which(ado$Secteur=="Toulouse"&ado$ANNEE==2018)]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="1"&ado$ANNEE==2018)]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="2"&ado$ANNEE==2018)]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="3"&ado$ANNEE==2018)]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="missing"&ado$ANNEE==2018)]))
    # 2019
    length(unique(ado$N_PATIENT[which(ado$Secteur=="Toulouse"&ado$ANNEE==2019)]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="1"&ado$ANNEE==2019)]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="2"&ado$ANNEE==2019)]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="3"&ado$ANNEE==2019)]))
    length(unique(ado$N_PATIENT[which(ado$Secteur=="missing"&ado$ANNEE==2019)]))
    
    # first visits
    xtabs(~Secteur[which(ado$passage_répété==0)]+ANNEE[which(ado$passage_répété==0)],ado)
    options(digits=1) 
    prop.table(xtabs(~Secteur[which(ado$passage_répété==0)]+ANNEE[which(ado$passage_répété==0)],ado),margin=2)*100
    chisq.test(xtabs(~Secteur[which(ado$passage_répété==0)]+ANNEE[which(ado$passage_répété==0)],ado,exclude=c("missing")))$expected # all over 5 so we can do the test
    chisq.test(xtabs(~Secteur[which(ado$passage_répété==0)]+ANNEE[which(ado$passage_répété==0)],ado,exclude=c("missing")))
    
    
  #DP and DAs
  library("openxlsx")
  write.xlsx(xtabs(~DP,ado), file = "DP.xlsx",
             sheetName = "DP", append = FALSE)
  library("openxlsx")
  write.xlsx(xtabs(~DA1,ado), file = "DA1.xlsx",
             sheetName = "DA1", append = FALSE)
  library("openxlsx")
  write.xlsx(xtabs(~DA2,ado), file = "DA2.xlsx",
             sheetName = "DA2", append = FALSE)
  library("openxlsx")
  write.xlsx(xtabs(~DA3,ado), file = "DA3.xlsx",
             sheetName = "DA3", append = FALSE)
  library("openxlsx")
  write.xlsx(xtabs(~DA4,ado), file = "DA4.xlsx",
             sheetName = "DA4", append = FALSE)
  library("openxlsx")
  write.xlsx(xtabs(~DA5,ado), file = "DA5.xlsx",
             sheetName = "DA5", append = FALSE)
  library("openxlsx")
  write.xlsx(xtabs(~PSYDP,ado), file = "PSYDP.xlsx",
             sheetName = "PSYDP", append = FALSE)
  library("openxlsx")
  write.xlsx(xtabs(~PSYDA1,ado), file = "PSYDA1.xlsx",
             sheetName = "PSYDA1", append = FALSE)
  library("openxlsx")
  write.xlsx(xtabs(~PSYDA2,ado), file = "PSYDA2.xlsx",
             sheetName = "PSYDA2", append = FALSE)
  library("openxlsx")
  write.xlsx(xtabs(~PSYDA3,ado), file = "PSYDA3.xlsx",
             sheetName = "PSYDA3", append = FALSE)
  library("openxlsx")
  write.xlsx(xtabs(~PSYDA4,ado), file = "PSYDA4.xlsx",
             sheetName = "PSYDA4", append = FALSE)
  library("openxlsx")
  write.xlsx(xtabs(~PSYDA5,ado), file = "PSYDA5.xlsx",
             sheetName = "PSYDA5", append = FALSE)
  
  #PSYDP and PSYDAs
  
  #DEVENIR_POST_URG (aftermath)
  xtabs(~DEVENIR_POST_URG,ado)
  xtabs(~DEVENIR_POST_URG+ANNEE,ado)
  prop.table(xtabs(~DEVENIR_POST_URG+ANNEE,ado))*100
  
  
  ado$N_DOSSIER<-ado$`N°DOSSIER`
  
  xtabs(~DEVENIR_POST_URG+ANNEE,ado)
  ado$DEVENIR_CAT2<-ifelse(ado$DEVENIR_POST_URG %in% c(NA,"DEVENIR_NR","DOMICILE : EHPAD L'ALBERGUE","DOMICILE : EHPAD LES ARCADES","NR","TREXT : NR","Mutation","Transfert","1296","2420","B111","B117","UHCD"),1,0)
  xtabs(~DEVENIR_CAT+ ANNEE,ado)
  xtabs(~DEVENIR_CAT2+ ANNEE,ado)
  xtabs(~DEVENIR_CAT2,ado)
  
  library(dplyr)
  dplyr::count(ado,N_PATIENT)
  aftermath = ado  %>% 
  group_by(N_PATIENT,DEVENIR_CAT2, ANNEE, Sexe, AGE) %>% 
    summarise(visits_number=n_distinct((N_DOSSIER)))
  
  aftermath_data<-as.data.frame(aftermath)
  length(unique(aftermath_data$N_PATIENT))
  length(unique(aftermath_data$N_PATIENT[which(aftermath_data$ANNEE==2014&aftermath_data$DEVENIR_CAT2==0)]))
  length(unique(aftermath_data$N_PATIENT[which(aftermath_data$ANNEE==2015&aftermath_data$DEVENIR_CAT2==0)]))
  length(unique(aftermath_data$N_PATIENT[which(aftermath_data$ANNEE==2016&aftermath_data$DEVENIR_CAT2==0)]))
  length(unique(aftermath_data$N_PATIENT[which(aftermath_data$ANNEE==2017&aftermath_data$DEVENIR_CAT2==0)]))
  length(unique(aftermath_data$N_PATIENT[which(aftermath_data$ANNEE==2018&aftermath_data$DEVENIR_CAT2==0)]))
  length(unique(aftermath_data$N_PATIENT[which(aftermath_data$ANNEE==2019&aftermath_data$DEVENIR_CAT2==0)]))
  
  length(unique(aftermath_data$N_PATIENT[which(aftermath_data$ANNEE==2014&aftermath_data$DEVENIR_CAT2==0&aftermath_data$Sexe=="F")]))
  length(unique(aftermath_data$N_PATIENT[which(aftermath_data$ANNEE==2015&aftermath_data$DEVENIR_CAT2==0&aftermath_data$Sexe=="F")]))
  length(unique(aftermath_data$N_PATIENT[which(aftermath_data$ANNEE==2016&aftermath_data$DEVENIR_CAT2==0&aftermath_data$Sexe=="F")]))
  length(unique(aftermath_data$N_PATIENT[which(aftermath_data$ANNEE==2017&aftermath_data$DEVENIR_CAT2==0&aftermath_data$Sexe=="F")]))
  length(unique(aftermath_data$N_PATIENT[which(aftermath_data$ANNEE==2018&aftermath_data$DEVENIR_CAT2==0&aftermath_data$Sexe=="F")]))
  length(unique(aftermath_data$N_PATIENT[which(aftermath_data$ANNEE==2019&aftermath_data$DEVENIR_CAT2==0&aftermath_data$Sexe=="F")]))
  
  length(unique(aftermath_data$N_PATIENT[which(aftermath_data$ANNEE==2014&aftermath_data$DEVENIR_CAT2==0&aftermath_data$Sexe=="M")]))
  length(unique(aftermath_data$N_PATIENT[which(aftermath_data$ANNEE==2015&aftermath_data$DEVENIR_CAT2==0&aftermath_data$Sexe=="M")]))
  length(unique(aftermath_data$N_PATIENT[which(aftermath_data$ANNEE==2016&aftermath_data$DEVENIR_CAT2==0&aftermath_data$Sexe=="M")]))
  length(unique(aftermath_data$N_PATIENT[which(aftermath_data$ANNEE==2017&aftermath_data$DEVENIR_CAT2==0&aftermath_data$Sexe=="M")]))
  length(unique(aftermath_data$N_PATIENT[which(aftermath_data$ANNEE==2018&aftermath_data$DEVENIR_CAT2==0&aftermath_data$Sexe=="M")]))
  length(unique(aftermath_data$N_PATIENT[which(aftermath_data$ANNEE==2019&aftermath_data$DEVENIR_CAT2==0&aftermath_data$Sexe=="M")]))
  
  first_age2 = ado  %>% 
    group_by(N_PATIENT,ANNEE,DEVENIR_CAT2) %>% 
    summarise(first_age2=min(AGE))
  after_first_age_data<-as.data.frame(first_age2)
  after_first_age_data$AGE2<-ifelse(after_first_age_data$first_age2<15,1,2)
  length(unique(after_first_age_data$N_PATIENT[which(after_first_age_data$ANNEE==2014&after_first_age_data$DEVENIR_CAT2==0&after_first_age_data$AGE2==1)]))
  length(unique(after_first_age_data$N_PATIENT[which(after_first_age_data$ANNEE==2015&after_first_age_data$DEVENIR_CAT2==0&after_first_age_data$AGE2==1)]))
  length(unique(after_first_age_data$N_PATIENT[which(after_first_age_data$ANNEE==2016&after_first_age_data$DEVENIR_CAT2==0&after_first_age_data$AGE2==1)]))
  length(unique(after_first_age_data$N_PATIENT[which(after_first_age_data$ANNEE==2017&after_first_age_data$DEVENIR_CAT2==0&after_first_age_data$AGE2==1)]))
  length(unique(after_first_age_data$N_PATIENT[which(after_first_age_data$ANNEE==2018&after_first_age_data$DEVENIR_CAT2==0&after_first_age_data$AGE2==1)]))
  length(unique(after_first_age_data$N_PATIENT[which(after_first_age_data$ANNEE==2019&after_first_age_data$DEVENIR_CAT2==0&after_first_age_data$AGE2==1)]))
  
  length(unique(after_first_age_data$N_PATIENT[which(after_first_age_data$ANNEE==2014&after_first_age_data$DEVENIR_CAT2==0&after_first_age_data$AGE2==2)]))
  length(unique(after_first_age_data$N_PATIENT[which(after_first_age_data$ANNEE==2015&after_first_age_data$DEVENIR_CAT2==0&after_first_age_data$AGE2==2)]))
  length(unique(after_first_age_data$N_PATIENT[which(after_first_age_data$ANNEE==2016&after_first_age_data$DEVENIR_CAT2==0&after_first_age_data$AGE2==2)]))
  length(unique(after_first_age_data$N_PATIENT[which(after_first_age_data$ANNEE==2017&after_first_age_data$DEVENIR_CAT2==0&after_first_age_data$AGE2==2)]))
  length(unique(after_first_age_data$N_PATIENT[which(after_first_age_data$ANNEE==2018&after_first_age_data$DEVENIR_CAT2==0&after_first_age_data$AGE2==2)]))
  length(unique(after_first_age_data$N_PATIENT[which(after_first_age_data$ANNEE==2019&after_first_age_data$DEVENIR_CAT2==0&after_first_age_data$AGE2==2)]))
  
  
  
        # Analysis by sex
    options(digits=3)
    xtabs(~DEVENIR_CAT+Sexe+ ANNEE,ado)
    library("openxlsx")
    write.xlsx(xtabs(~DEVENIR_CAT+Sexe+ ANNEE,ado,addNA = T), file = "DEVENIR_CAT+Sexe+ ANNEE.xlsx",
               sheetName = "DEVENIR_CAT+Sexe+ ANNEE", append = FALSE)
        # Analysis by age
    options(digits=3)
    xtabs(~DEVENIR_CAT+AGE2+ ANNEE,ado)
    library("openxlsx")
    write.xlsx(xtabs(~DEVENIR_CAT+AGE2+ ANNEE,ado), file = "DEVENIR_CAT+AGE2+ ANNEE.xlsx",
               sheetName = "DEVENIR_CAT+AGE2+ ANNEE", append = FALSE)
    
  
### Bivariate analysis (Not utilised in the final results)
  
  qqnorm(ado$AGE);qqline(ado$AGE) # Graphical verification of normality
  shapiro.test(ado$AGE) # unusable above 5000 observations
  ks.test(ado$AGE,"pnorm",mean(ado$AGE),sd(ado$AGE)) # Too many equal values
  
  # Link between SU_he_ad and AGE :
  aggregate(AGE~SU_he_ad,ado,mean)
  var.test(ado$AGE~ado$SU_he_ad) #Vérification of equality of variances by F test
  t.test(ado$AGE~ado$SU_he_ad) #Résults of Welch's test (done instead of t test because the variances are different)
  wilcox.test(ado$AGE~ado$SU_he_ad) # if conditions for student's test are not verified
  
  # Link between SU_he_ad and Sexe (Sex) :
  
  chisq.test(xtabs(~ado$Sexe+SU_he_ad,ado))$observed # Observed counts for chi2 test
  chisq.test(xtabs(~ado$Sexe+SU_he_ad,ado))$expected # Theorical counts, all above 5 so we can use the test
  chisq.test(ado$Sexe,ado$SU_he_ad,correct=T)
  
  # Link between SU_he_ad and CODE_GEO :
  
  
  # Link between SU_he_ad and ANNEE (Year) :
  
  chisq.test(xtabs(~ado$ANNEE+SU_he_ad,ado))$observed # Observed counts for chi2 test
  chisq.test(xtabs(~ado$ANNEE+SU_he_ad,ado))$expected # Theorical counts, all above 5 so we can use the test
  chisq.test(ado$ANNEE,ado$SU_he_ad,correct=T)
  
  # Link between SU_he_ad and MOIS (Month) :
  
  chisq.test(xtabs(~ado$MOIS+SU_he_ad,ado))$observed # Observed counts for chi2 test
  chisq.test(xtabs(~ado$MOIS+SU_he_ad,ado))$expected # Theorical counts, all above 5 so we can use the test
  chisq.test(ado$MOIS,ado$SU_he_ad,correct=T)
  
  # Link between SU_he_ad and JSEM (Day in the week) :
  chisq.test(xtabs(~ado$JSEM+SU_he_ad,ado))$observed # Observed counts for chi2 test
  chisq.test(xtabs(~ado$JSEM+SU_he_ad,ado))$expected # Theorical counts, all above 5 so we can use the test
  chisq.test(ado$JSEM,ado$SU_he_ad,correct=T)
  
  # Link between SU_he_ad and HEURE (Hour) :
  chisq.test(xtabs(~ado$HEURE+SU_he_ad,ado))$observed # Observed counts for chi2 test
  chisq.test(xtabs(~ado$HEURE+SU_he_ad,ado))$expected # Theorical counts, all above 5 so we can use the test
  chisq.test(ado$HEURE,ado$SU_he_ad,correct=T)
  
  # Link between SU_he_ad and MOTIF (Motive) ATTENTION :
  
    # NATURE_MOTIF
    chisq.test(xtabs(~ado$NATURE_MOTIF+SU_he_ad,ado))$observed # Observed counts for chi2 test
    chisq.test(xtabs(~ado$NATURE_MOTIF+SU_he_ad,ado))$expected # Theorical counts, all above 5 so we can use the test
    chisq.test(ado$NATURE_MOTIF,ado$SU_he_ad,correct=T)
  
    #MOTIF_CAT

  # Link between DELAI_RETOUR (time before return visit, in days) and AGE :
    boxplot(ado$DELAI_RETOUR ~ ado$AGE)
    plot(jitter(ado$AGE),jitter(ado$DELAI_RETOUR,factor=4), main="DELAI_RETOUR = f(AGE)")
    abline(lm(ado$DELAI_RETOUR~ado$AGE),lwd=2)
    summary(lm(ado$DELAI_RETOUR~ado$AGE)) # positive corrélation between age and the delay before return visit
    
  # Link between DELAI_RETOUR (time before return visit, in days) and Sexe (Sex) :
    boxplot(ado$DELAI_RETOUR ~ ado$Sexe)
    aggregate(DELAI_RETOUR~Sexe,ado,mean)
    var.test(ado$DELAI_RETOUR~ado$Sexe) #Vérification of equality of variances by F test
    t.test(ado$DELAI_RETOUR~ado$Sexe) #Résults of Welch's test (done instead of t test because the variances are different)
    wilcox.test(ado$DELAI_RETOUR~ado$Sexe) # if conditions for student's test are not verified
  
  # Link between DELAI_RETOUR (time before return visit, in days) and CODE_GEO :
  
  # Link between DELAI_RETOUR (time before return visit, in days) and ANNEE (Year) :
    boxplot(ado$DELAI_RETOUR ~ ado$ANNEE)
    aggregate(DELAI_RETOUR~ANNEE,ado,mean)
    summary((glm(ado$DELAI_RETOUR~as.factor(ado$ANNEE))))
    
  # Link between DELAI_RETOUR (time before return visit, in days) and MOIS (Month) :
    boxplot(ado$DELAI_RETOUR ~ ado$MOIS)
    aggregate(DELAI_RETOUR~MOIS,ado,mean)
    summary((glm(ado$DELAI_RETOUR~as.factor(ado$MOIS))))
    
  # Link between DELAI_RETOUR (time before return visit, in days) and JSEM (Day in the week) :
    boxplot(ado$DELAI_RETOUR ~ ado$JSEM)
    aggregate(DELAI_RETOUR~JSEM,ado,mean)
    summary((glm(ado$DELAI_RETOUR~ado$JSEM_FACTOR)))
    
  # Link between DELAI_RETOUR (time before return visit, in days) and HEURE (Hour) :
    boxplot(ado$DELAI_RETOUR ~ ado$HEURE)
    aggregate(DELAI_RETOUR~HEURE,ado,mean)
    summary((glm(ado$DELAI_RETOUR~as.factor(ado$HEURE))))
    
  # Link between DELAI_RETOUR (time before return visit, in days) and MOTIF (Motive) ATTENTION :
    # NATURE MOTIF
    boxplot(ado$DELAI_RETOUR ~ ado$NATURE_MOTIF)
    aggregate(DELAI_RETOUR~NATURE_MOTIF,ado,mean)
    var.test(ado$DELAI_RETOUR~ado$NATURE_MOTIF) #Vérification of equality of variances by F test
    t.test(ado$DELAI_RETOUR~ado$NATURE_MOTIF) #Résults of Welch's test (done instead of t test because the variances are different)
    wilcox.test(ado$DELAI_RETOUR~ado$NATURE_MOTIF) # if conditions for student's test are not verified
    
    # MOTIF_CAT
    
  # Link between AGE and Sexe (Sex)
  aggregate(AGE~Sexe,ado,mean)
  qqnorm(ado$AGE);qqline(ado$AGE) # Graphical verification of normality
  shapiro.test(ado$AGE) # unusable above 5000 observations
  ks.test(ado$AGE,"pnorm",mean(ado$AGE),sd(ado$AGE)) # Too many equal values
  var.test(ado$AGE~ado$Sexe) #Vérification of equality of variances by F test
  
  t.test(ado$AGE~ado$Sexe) #Résults of Welch's test (done instead of t test because the variances are different)
  wilcox.test(ado$AGE~ado$Sexe) # if conditions for student's test are not verified
  
  # Link between AGE and CODE_GEO :
  

  # Link between AGE and ANNEE (Year) :
  boxplot(ado$AGE ~ ado$ANNEE)
  aggregate(AGE~ANNEE,ado,mean)
  summary(glm(ado$AGE~as.factor(ado$ANNEE)))
  
  # Link between AGE and MOIS (Month) :
  boxplot(ado$AGE ~ ado$MOIS)
  aggregate(AGE~MOIS,ado,mean)
  summary(glm(ado$AGE~as.factor(ado$MOIS)))
  
  # Link between AGE and JSEM (Day in the week) :
  boxplot(ado$AGE ~ ado$JSEM)
  aggregate(AGE~JSEM,ado,mean)
  summary(glm(ado$AGE~ado$JSEM_FACTOR))
  
  # Link between AGE and HEURE (Hour) :
  boxplot(ado$AGE ~ ado$HEURE)
  aggregate(AGE~HEURE,ado,mean)
  summary(glm(ado$AGE~as.factor(ado$HEURE)))
  
  # Link between AGE and MOTIF (Motive) ATTTENTION :
  
    # NATURE_MOTIF
    boxplot(ado$AGE ~ ado$NATURE_MOTIF)
    aggregate(AGE~NATURE_MOTIF,ado,mean)
    summary(glm(ado$AGE~as.factor(ado$NATURE_MOTIF)))
    t.test(ado$AGE~ado$NATURE_MOTIF) #Résults of Welch's test (done instead of t test because the variances are different)
    wilcox.test(ado$AGE~ado$NATURE_MOTIF) # if conditions for student's test are not verified
  
  
  # Link between Sexe (Sex) and CODE_GEO :
  
  # Link between Sexe (Sex) and ANNEE (Year) :
  chisq.test(xtabs(~ado$Sexe+ado$ANNEE))$observed # Observed counts for chi2 test
  chisq.test(xtabs(~ado$Sexe+ado$ANNEE,ado))$expected # Theorical counts, all above 5 so we can use the test
  chisq.test(ado$Sexe,ado$ANNEE,correct=T)
  
  # Link between Sexe (Sex) and MOIS (Month) :
  chisq.test(xtabs(~ado$Sexe+ado$MOIS))$observed # Observed counts for chi2 test
  chisq.test(xtabs(~ado$Sexe+ado$MOIS,ado))$expected # Theorical counts, all above 5 so we can use the test
  chisq.test(ado$Sexe,ado$MOIS,correct=T)
  
  # Link between Sexe (Sex) and JSEM (Day in the week) :
  chisq.test(xtabs(~ado$Sexe+ado$JSEM))$observed # Observed counts for chi2 test
  chisq.test(xtabs(~ado$Sexe+ado$JSEM,ado))$expected # Theorical counts, all above 5 so we can use the test
  chisq.test(ado$Sexe,ado$JSEM,correct=T)
  
  # Link between Sexe (Sex) and HEURE (Hour) :
  chisq.test(xtabs(~ado$Sexe+ado$HEURE))$observed # Observed counts for chi2 test
  chisq.test(xtabs(~ado$Sexe+ado$HEURE,ado))$expected # Theorical counts, all above 5 so we can use the test
  chisq.test(ado$Sexe,ado$HEURE,correct=T)
  
  # Link between Sexe (Sex) and MOTIF (Motive) ATTENTION:
  
    #NATURE MOTIF
    chisq.test(xtabs(~ado$Sexe+ado$NATURE_MOTIF))$observed # Observed counts for chi2 test
    chisq.test(xtabs(~ado$Sexe+ado$NATURE_MOTIF,ado))$expected # Theorical counts, all above 5 so we can use the test
    chisq.test(ado$Sexe,ado$NATURE_MOTIF,correct=T)
  
  # Link between CODE_GEO and ANNEE (Year) :
  
  # Link between CODE_GEO and MOIS (Month) :
  
  # Link between CODE_GEO and JSEM (Day in the week) :
  
  # Link between CODE_GEO and HEURE (Hour) :
  
  # Link between CODE_GEO and MOTIF (Motive) :
  
  # Link between ANNEE (Year) and MOIS (Month) :
    chisq.test(xtabs(~ado$ANNEE+ado$MOIS))$observed # Observed counts for chi2 test
    chisq.test(xtabs(~ado$ANNEE+ado$MOIS,ado))$expected # Theorical counts, all above 5 so we can use the test
    chisq.test(ado$ANNEE,ado$MOIS,correct=T)
    
  # Link between ANNEE (Year) and JSEM (Day in the week) :
    chisq.test(xtabs(~ado$ANNEE+ado$JSEM))$observed # Observed counts for chi2 test
    chisq.test(xtabs(~ado$ANNEE+ado$JSEM,ado))$expected # Theorical counts, all above 5 so we can use the test
    chisq.test(ado$ANNEE,ado$JSEM,correct=T)
    
  # Link between ANNEE (Year) and HEURE (Hour) :
    chisq.test(xtabs(~ado$ANNEE+ado$HEURE))$observed # Observed counts for chi2 test
    chisq.test(xtabs(~ado$ANNEE+ado$HEURE,ado))$expected # Theorical counts, all above 5 so we can use the test
    chisq.test(ado$ANNEE,ado$HEURE,correct=T)
    
  # Link between ANNEE (Year) and MOTIF (Motive) ATTENTION :
      # NATURE_MOTIF
      chisq.test(xtabs(~ado$ANNEE+ado$NATURE_MOTIF))$observed # Observed counts for chi2 test
      chisq.test(xtabs(~ado$ANNEE+ado$NATURE_MOTIF,ado))$expected # Theorical counts, all above 5 so we can use the test
      chisq.test(ado$ANNEE,ado$NATURE_MOTIF,correct=T)

  # Link between MOIS (Month) and JSEM (Day in the week) :
    chisq.test(xtabs(~ado$MOIS+ado$JSEM))$observed # Observed counts for chi2 test
    chisq.test(xtabs(~ado$MOIS+ado$JSEM,ado))$expected # Theorical counts, all above 5 so we can use the test
    chisq.test(ado$MOIS,ado$JSEM,correct=T)
    
  # Link between MOIS (Month) and HEURE (Hour) :
    chisq.test(xtabs(~ado$MOIS+ado$HEURE))$observed # Observed counts for chi2 test
    chisq.test(xtabs(~ado$MOIS+ado$HEURE,ado))$expected # Theorical counts, all above 5 so we can use the test
    chisq.test(ado$MOIS,ado$HEURE,correct=T)
    
  # Link between MOIS (Month) and MOTIF (Motive) ATTENTION :
      # NATURE_MOTIF
      chisq.test(xtabs(~ado$MOIS+ado$NATURE_MOTIF))$observed # Observed counts for chi2 test
      chisq.test(xtabs(~ado$MOIS+ado$NATURE_MOTIF,ado))$expected # Theorical counts, all above 5 so we can use the test
      chisq.test(ado$MOIS,ado$NATURE_MOTIF,correct=T)
    
  # Link between JSEM (Day in the week) and HEURE (Hour) :
    chisq.test(xtabs(~ado$JSEM+ado$HEURE))$observed # Observed counts for chi2 test
    chisq.test(xtabs(~ado$JSEM+ado$HEURE,ado))$expected # Theorical counts, all above 5 so we can use the test
    chisq.test(ado$JSEM,ado$HEURE,correct=T)
      
  # Link between JSEM (Day in the week) and MOTIF (Motive) :
    chisq.test(xtabs(~ado$JSEM+ado$NATURE_MOTIF))$observed # Observed counts for chi2 test
    chisq.test(xtabs(~ado$JSEM+ado$NATURE_MOTIF,ado))$expected # Theorical counts, all above 5 so we can use the test
    chisq.test(ado$JSEM,ado$NATURE_MOTIF,correct=T)
    
  # Link between HEURE (Hour) and MOTIF (Motive) ATENTION :
      # NATURE_MOTIF
      chisq.test(xtabs(~ado$HEURE+ado$NATURE_MOTIF))$observed # Observed counts for chi2 test
      chisq.test(xtabs(~ado$HEURE+ado$NATURE_MOTIF,ado))$expected # Theorical counts, all above 5 so we can use the test
      chisq.test(ado$HEURE,ado$NATURE_MOTIF,correct=T)
          
  
### Main criterion analysis
  

# Incidence rate
  # Total per year of all adolescents visiting the ER
  # And incidence rates of adolescents visiting the ER for psychiatric motives compared to all adolescents
    # All incidence rates were calculated on a separate script, using the following commands :
      ado2014<-ado[(ado$ANNEE==2014),]
      length(unique(ado2014$N_PATIENT))
      n <- 1058
      d <- 794
      fit <- glm(n ~ offset(log(d)), family=poisson)
      summary(fit)
      exp(confint(fit))
      exp(coefficients(fit))
  
  # Repeated visits
  chisq.test(xtabs(~ado$passage_répété+ANNEE,ado))$observed # Observed counts for chi2 test
  chisq.test(xtabs(~ado$passage_répété+ANNEE,ado))$expected # Theorical counts, all above 5 so we can use the test
  chisq.test(ado$passage_répété,ado$ANNEE,correct=T)
  barplot(xtabs(~ado$passage_répété+ANNEE,ado))
  barplot(prop.table(xtabs(~ado$passage_répété+ANNEE,ado),margin=2))
  
#### Regression
  # For Hospitalisations in psychiatry
  # Logistic regression
  # Creating variable for hospitalisation in psychiatry
  xtabs(~DEVENIR_CAT,ado)
  ado$DEVENIR_CAT3<-ifelse(ado$DEVENIR_CAT %in% c("Hospitalisation psychiatrique CHU","Hospitalisation psychiatrique hors CHU"),1,0)
  ado$DEVENIR_CAT3<-ifelse(ado$DEVENIR_CAT=="Devenir inconnu",NA,ado$DEVENIR_CAT3)
  table(ado$DEVENIR_CAT3,useNA="always")
  
  # Creating variable for weekends
  table(ado$JSEM,useNA="always")
  ado$weekend<-ifelse(ado$JSEM %in% c("Samedi","Dimanche"),1,0)
  xtabs(~JSEM+ANNEE,ado)
  xtabs(~weekend+ANNEE,ado)
  
  table(ado$JSEM,useNA="always")
  table(ado$ANNEE,useNA="always")
  table(ado$AGE2,useNA="always")
  table(ado$Sexe,useNA="always")
  
  ado$Secteur_reg <- ifelse(ado$Secteur=="missing",NA,ado$Secteur)
  table(ado$Secteur_reg,useNA="always")
  
  
  ado$DEVENIR_CAT3<-ifelse(ado$DEVENIR_CAT=="Devenir inconnu",NA,ado$DEVENIR_CAT3)
  ado$HOSPIT_PSY <- ado$DEVENIR_CAT3
  table(ado$HOSPIT_PSY,ado$ANNEE,useNA="always")
  xtabs(~HOSPIT_PSY+ANNEE,ado)
  
  
  # univariate regression
  # ANNEE
  library(lme4)
  model <- glmer(HOSPIT_PSY ~ as.factor(ANNEE) +
                   (1 | N_PATIENT), data = ado, family = binomial, control = glmerControl(optimizer = "bobyqa"),na.action = na.omit)
  print(model)
  se <- sqrt(diag(vcov(model)))
  (tab <- cbind(Est = fixef(model), LL = fixef(model) - 1.96 * se, UL = fixef(model) + 1.96 *
                  se))
  exp(tab)
  summary(model)
  
  # AGE2
  library(lme4)
  model <- glmer(HOSPIT_PSY ~ as.factor(AGE2) +
                   (1 | N_PATIENT), data = ado, family = binomial, control = glmerControl(optimizer = "bobyqa"),na.action = na.omit)
  print(model)
  se <- sqrt(diag(vcov(model)))
  (tab <- cbind(Est = fixef(model), LL = fixef(model) - 1.96 * se, UL = fixef(model) + 1.96 *
                  se))
  exp(tab)
  summary(model)
  
  # Sexe
  library(lme4)
  model <- glmer(HOSPIT_PSY ~ as.factor(Sexe) +
                   (1 | N_PATIENT), data = ado, family = binomial, control = glmerControl(optimizer = "bobyqa"),na.action = na.omit)
  print(model)
  se <- sqrt(diag(vcov(model)))
  (tab <- cbind(Est = fixef(model), LL = fixef(model) - 1.96 * se, UL = fixef(model) + 1.96 *
                  se))
  exp(tab)
  summary(model)
  
  # Secteur_reg
  ado$Secteur_reg<-as.factor(ado$Secteur_reg)
  ado$Secteur_reg<-relevel(ado$Secteur_reg,ref="Toulouse")
  
  library(lme4)
  model <- glmer(HOSPIT_PSY ~ as.factor(Secteur_reg) +
                   (1 | N_PATIENT), data = ado, family = binomial, control = glmerControl(optimizer = "bobyqa"),na.action = na.omit)
  print(model)
  se <- sqrt(diag(vcov(model)))
  (tab <- cbind(Est = fixef(model), LL = fixef(model) - 1.96 * se, UL = fixef(model) + 1.96 *
                  se))
  exp(tab)
  summary(model)
  
  # weekend
  library(lme4)
  model <- glmer(HOSPIT_PSY ~ as.factor(weekend) +
                   (1 | N_PATIENT), data = ado, family = binomial, control = glmerControl(optimizer = "bobyqa"),na.action = na.omit)
  print(model)
  se <- sqrt(diag(vcov(model)))
  (tab <- cbind(Est = fixef(model), LL = fixef(model) - 1.96 * se, UL = fixef(model) + 1.96 *
                  se))
  exp(tab)
  summary(model)
  
  # multivariate regression
  library(lme4)
  model2 <- glmer(HOSPIT_PSY ~ as.factor(ANNEE) + as.factor(AGE2) + as.factor(Sexe) + as.factor(Secteur_reg) + as.factor(weekend) +
                   (1 | N_PATIENT), data = ado, family = binomial, control = glmerControl(optimizer = "bobyqa"),na.action=na.exclude)
  print(model2)
  se <- sqrt(diag(vcov(model2)))
  (tab2 <- cbind(Est = fixef(model2), LL = fixef(model2) - 1.96 * se, UL = fixef(model2) + 1.96 *
                  se))
  exp(tab2)
  summary(model2)
  
  # testing for interactions
  options(digits=8)
  library(lme4)
  model3 <- glmer(HOSPIT_PSY ~ as.factor(ANNEE) +as.factor(AGE2)*as.factor(Sexe) + as.factor(Secteur_reg) + as.factor(weekend) +
                    (1 | N_PATIENT), data = ado, family = binomial, control = glmerControl(optimizer = "bobyqa"),na.action=na.exclude)
  print(model3)
  se <- sqrt(diag(vcov(model3)))
  (tab3 <- cbind(Est = fixef(model3), LL = fixef(model3) - 1.96 * se, UL = fixef(model3) + 1.96 *
                   se))
  exp(tab3)
  summary(model3)