

### INSTALLATION ET CHARGEMENT DES PACKAGES NECESSAIRES 

requiredPackages = c('RMySQL','DBI','rstudioapi','cyphr', 'dplyr',"shiny","shinydashboard","wordcloud","tm",'tidyverse','visNetwork','igraph','ggraph','tidyr','tidytext','wordcloud','tm','stringi','stringr','RWeka','ggplot2','plotrix','qdap', 'RMySQL','DBI','rstudioapi','cyphr', 'dplyr', 'plotly','highcharter','LDAvis','lda')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

options(warn=-1)
### CONNEXION AUX DONNEES AFIN DE POUVOIR CREER LES LISTES NECESSAIRE A L INTERFACE (pour les inputs)

db = dbConnect(RMySQL::MySQL(), host ="localhost", port = 3306, user = "root", password = "", dbname="employment")

sql <- "SELECT job.Content,job.Contract,job.Mean_Salary,job.Date,city.name_city,department.name_dep,region.name_region FROM job,city,department,region where job.id_city=city.id_city and city.id_department=department.id_department and department.id_region=region.id_region"
data <- dbGetQuery(db, sql)

for (col in c("Content","Contract","name_city","name_dep","name_region")){
  Encoding(data[[col]]) <- "UTF-8"}

regions = c("Tout",sort(unique(data$name_region)))
contracts = c("Tout",sort(unique(data$Contract)))
dates = c("Tout",sort(unique(data$Date)))




### UI POUR L APPLICATION SHINY

ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "Text Analysis"),

                    ### DIFFERENTS ONGLETS AVEC ICONES
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Wordcloud",tabName = "wordcloud",icon = icon("cloud")),
                        menuItem("Mots populaires",tabName = "barplot",icon = icon("chart-bar")),
                        menuItem("Ou sont les offres ?",tabName = "carte",icon = icon("globe-europe")),
                        menuItem("Mots co-occurants",tabName = "cooccurant",icon = icon("project-diagram")),
                        menuItem("N Grammes",tabName = "ngram",icon = icon("envelope-open-text"))
                      )),
                    
                    ### CONTENU DE L APPLI POUR CHAQUE ONGLET COMPORTANT INPUTS ET SORTIES 
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "wordcloud",
                                fluidRow(
                                  box(
                                    helpText(paste("Permet de creer un wordcloud selon certains parametres. Il y a egalement la possibilite de l'enregistrer avec un format image de son choix.")),
                                    br(),
                                    sliderInput("freq","Frequence minimum:",min = 1,  max = 500, value = 10),
                                    sliderInput("max","Nombre maximal de mot:",min = 1,  max = 500,  value = 25),
                                    selectInput(inputId = "pal",label = "Couleur",choices = c("Dark"="Dark2","Pastel One"="Pastel1","Pastel Two"="Pastel2","Set One"="Set1",
                                                                                                  "Set Two"="Set2","Set Three"="Set3"),selected = "Dark2"),
                                    downloadButton("download1","Telecharger le wordcloud"),
                                    selectInput(inputId = "download3",label = "Choix du format du wordcloud",choices = list("png","pdf","bmp","jpeg"))),
                                  box(plotOutput("plot")))),
                        
                        tabItem(tabName = "barplot",
                                fluidRow(
                                  box(
                                    helpText(paste("Permet de creer un barplot montrant les mots les plus utilises. Il peut être filtré mais egalement modifié selon certains parametres.")),
                                    br(),
                                    sliderInput("barmax","Nombre maximal de mot:",min = 5,  max = 15,  value = 10),
                                    selectInput("barcontract", "Filtrer par contrat :",contracts),
                                    br(),
                                    selectInput("barregion", "Filtrer par region :",regions)
                                  ),
                                  box(plotlyOutput("barplot")))),
                        tabItem(tabName = "carte",
                                fluidRow(
                                  box(
                                    helpText(paste("Carte interactive permettant de visualiser des donnees telles que le nombre d'offres ou le salaire moyen par région.")),
                                    br(),
                                    radioButtons("mesure", "Choisir la mesure :",choices = list("Nombre d'offres" = "nombre", "Salaire moyen" = "salaire"),selected = "nombre"),
                                    br(),
                                    selectInput("mapcontract", "Filtrer par contrat :",contracts),
                                    selectInput("mapdate", "Filtrer par date :",dates)
                                  ),
                                  box(highchartOutput("map")))),
                        tabItem(tabName = "cooccurant",
                                fluidRow(
                                  column(4,
                                         sliderInput("slider", label = h4("Taille des liens"), min = 1, 
                                                     max = 182, value = c(20, 50)),
                                  column(8.5,
                                         sliderInput("degree", label = h4("Profondeur des noeuds  a colorier"), min = 1, 
                                                            max = 10, value = 2))
                                  ),
                                visNetworkOutput("graphs",width = "100%",height = "800px"))),
                        tabItem(tabName = "ngram",
                                fluidRow(
                                  box(
                                    helpText(paste("Permet de creer un N-gramme montrant les mots les plus utilises. Il peut être filtré mais egalement modifié selon certains parametres.")),
                                    br(),
                                    sliderInput("nb_gram","Nombre maximal de mot:",min = 2,  max = 5,  value = 2),
                                    selectInput("ncontract", "Filtrer par contrat :",contracts),
                                    br(),
                                    selectInput("nregion", "Filtrer par region :",regions)
                                  ),
                                  box(plotlyOutput("ngrams"))))
)))



### SERVEUR DE L APPLICATION SHINY
server <- function(input, output) {
  
  
  ### CREATION DE LA MATRICE DOCUMENTS TERMES ET GESTION DES ACCENTS etc
  text = data$Content
  vsdocs <-VectorSource(text)
  corpus <- Corpus(vsdocs)
  accent <- function(x) stri_trans_general(x, "Latin-ASCII")
  stopwords_fr <- sapply(stopwords("french"),accent)
  corpus <- tm_map(corpus, removeWords, stopwords_fr)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, c("ans", "selon","h","f","diplome",'etat','er'))
  mdt <- DocumentTermMatrix(corpus)
  mdt <- as.matrix(mdt)
  freq <- apply(mdt,2,sum)
  dbDisconnect(db)
  
  ### GENERATION DU WORDCLOUD
  output$plot <- renderPlot({
    
    withProgress(message = 'Creating WordCloud',
                 value = 0, {
                   for (i in 1:10) {
                     incProgress(1/10)
                     Sys.sleep(0.05)
                   }
                 },env = parent.frame(n=1))
    ##Wordcloud code
    set.seed(1234)
    
    wordcloud(names(freq), freq, min.freq = input$freq, max.words=input$max,colors=brewer.pal(8, input$pal))
  })
  
  ### POUR ENREGISTRER LE WORDCLOUD GENERE
  
  output$download1 <- downloadHandler(
    filename = function() { paste("WordCloud",input$download3,sep = ".") },
    content = function(file) {
      if(input$download3=="png")
        png(file)
      else if (input$download3=="jpeg")
        jpeg(file)
      else if (input$download3=="bmp")
        bmp(file)
      else if (input$download3=="pdf")
        pdf(file)
      wordcloud(names(freq), freq, min.freq = input$freq, max.words=input$max,colors=brewer.pal(8, input$pal))
      dev.off()
    })
  
  ### GENERATION DU BARPLOT
  
  output$barplot <- renderPlotly({
    
    withProgress(message = 'Creating Barplot',
                 value = 0, {
                   for (i in 1:10) {
                     incProgress(1/10)
                     Sys.sleep(0.05)
                   }
                 },env = parent.frame(n=1))
    
    
    
    #Permet de filtrer les donnees grace aux inputs
    bar = data %>% filter(if(input$barcontract!="Tout") Contract == input$barcontract else TRUE) %>% filter(if(input$barregion !="Tout") name_region == input$barregion else TRUE)


    text = bar$Content
    vsdocs <-VectorSource(text)
    corpus <- Corpus(vsdocs)
    
    dtm <- TermDocumentMatrix(corpus)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    
    
    g <- ggplot(head(d,input$barmax), aes(x = reorder(word,-freq), y = freq, text = paste(
      "Mot : ", {word},
      "\nFrequence : ", freq))) +
      geom_bar(stat="identity",width = .7, fill = 'steelblue')+
      xlab("Mots") + ylab("Frequence") +
      ggtitle("Mot les plus frequents") + theme_classic() +
      theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))
    
    gg = ggplotly(g, dynamicTicks = TRUE, tooltip = "text")
    gg
  })
  
  ### GENERATION DU GRAPH
  output$graphs <- renderVisNetwork({
    
    withProgress(message = 'Creating Graph',
                 value = 0, {
                   for (i in 1:10) {
                     incProgress(1/10)
                     Sys.sleep(0.05)
                   }
                 },env = parent.frame(n=5))
    
    # Creation des bi-grammes
    pdfdataframe <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = F)
    new_gram <- pdfdataframe %>% unnest_tokens(bigram, text, token = 'ngrams', n = 2)
    bigram_sep <- new_gram %>% separate(bigram, c('word1','word2'), sep = " ")
    # Enlever les stopwords
    bigram_filt <- bigram_sep %>% filter(!word1 %in% stopwords_fr) %>% filter(!word2 %in% stopwords_fr)
    # Nombre d'apparition de chaque bi-gramme
    bigram_count <- bigram_filt %>% count(word1, word2, sort = T)
    
    # Selection en fonction des inputs
    selection = bigram_count[bigram_count$n>input$slider[1] & bigram_count$n<input$slider[2],]
    
    # Gestion des noeuds
    sources <- selection %>%
      distinct(word1) %>%
      rename(label = word1)
    
    destinations <- selection %>%
      distinct(word2) %>%
      rename(label = word2)
    
    nodes <- sources %>% 
      full_join( destinations, by = "label")%>% 
      rowid_to_column("id")
    
    # Gestion des liens
    per_word <- tibble(selection)
    colnames(per_word) <- c('source','destination','value')
    
    edges <- per_word %>% 
      left_join(nodes, by = c("source" = "label")) %>% 
      rename(from = id)
    
    edges <- edges %>% 
      left_join(nodes, by = c("destination" = "label")) %>% 
      rename(to = id)
    
    edges <- select(edges, from, to, value)
    
    
    visNetwork(nodes = nodes, edges = edges, height = "300px", width="100%", main="Graphe") %>%
      visInteraction(hideEdgesOnDrag = TRUE) %>%
      visPhysics(stabilization = FALSE) %>%
      visEdges(smooth = FALSE) %>%
      visOptions(nodesIdSelection=TRUE, highlightNearest = list(enabled = TRUE, # "enabled" variable: highlight nearest nodes and edges by clicking on a node
                                                                degree = input$degree, hover = T)) %>% 
      visInteraction(navigationButtons = TRUE,keyboard = TRUE, tooltipDelay = 0) #keyboard : enable keyboard manipulation rather than mouse (click on network before)

    
    
    
    
  })
  
  ### GENERATION DU BARPLOT DES NGRAMMES
  output$ngrams <- renderPlotly({
    
    withProgress(message = 'Creating Ngrammes',
                 value = 0, {
                   for (i in 1:10) {
                     incProgress(1/10)
                     Sys.sleep(0.05)
                   }
                 },env = parent.frame(n=1))
    
    #Permet de filtrer les donnees grace aux inputs
    ngr = data %>% filter(if(input$ncontract!="Tout") Contract == input$ncontract else TRUE) %>% filter(if(input$nregion !="Tout") name_region == input$nregion else TRUE) 
    
    
    text = ngr$Content
    vsdocs <-VectorSource(text)

    corpus_v = VCorpus(vsdocs)

    
    nb_gram = input$nb_gram

    BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min =nb_gram, max = nb_gram))
    tdm.bigram = TermDocumentMatrix(corpus_v,
                                    control = list(tokenize = BigramTokenizer))
    freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
    freq.df = data.frame(word=names(freq), freq=freq)
    
    p <- ggplot(head(freq.df,10), aes(x = reorder(word,freq), y = freq, text = paste(
      "N-gramme : ", {word},
      "\nFrequence : ", freq))) +
      geom_bar(stat = "identity", width = .7, fill = 'steelblue') + coord_flip() +
      xlab("N-grammes") + ylab("Frequence") +
      ggtitle("N-grammes les plus frequents") + theme_classic() +
      theme(plot.title = element_text(hjust = 0.5))
    
    
    ggplotly(p, dynamicTicks = TRUE, tooltip = "text")
    
  })
  
  ### GENERATION DE LA CARTE INTERACTIVE
  output$map <- renderHighchart({
    
    withProgress(message = 'Creating Map',
                 value = 0, {
                   for (i in 1:10) {
                     incProgress(1/10)
                     Sys.sleep(0.05)
                   }
                 },env = parent.frame(n=1))
    

    
    code_region <- data.frame(
      code = c('fr-cor', 'fr-bre', 'fr-pdl', 'fr-pac', 'fr-occ', 'fr-naq', 'fr-bfc', 'fr-cvl', 'fr-idf', 'fr-hdf'
               , 'fr-ara', 'fr-ges', 'fr-nor', 'fr-lre', 'fr-may', 'fr-gf', 'fr-mq','fr-gua'),
      name_region = c('Corse', 'Bretagne', 'Pays de la Loire', "Provence-Alpes-Côte d'Azur", 'Occitanie',
                      'Nouvelle-Aquitaine', 'Bourgogne-Franche-Comté', 'Centre-Val de Loire', 'Île-de-France',
                      'Hauts-de-France', 'Auvergne-Rhône-Alpes', 'Grand Est', 'Normandie', 'La Réunion',
                      'Mayotte', 'Guyane française', 'La Martinique', 'Guadeloupe'))
    
    data_carte <- data  %>% filter(if(input$mapcontract!="Tout") Contract == input$mapcontract else TRUE) %>% filter(if(input$mapdate!="Tout") Date == input$mapdate else TRUE)


    sample_table <- merge(data_carte,code_region, by=c('name_region'))
    nb_offres <- sample_table %>% count(code)

    
    #Pour afficher même les régions qui n'ont personne
    for (i in 1:nrow(code_region)){
      if(length(grep(code_region$code[i], nb_offres$code))==0){
        nb_offres[nrow(nb_offres) + 1,] = c(code_region$code[i],0)
      }
    }
    

    
    salary <- sample_table %>% filter(!is.na(Mean_Salary)) %>% group_by(code) %>% summarise(Salaire = mean(Mean_Salary))  

    for (c in code_region$code){
      if (!(c %in% salary$code)){
        salary <- rbind(salary,c(c,0))
      }
    }
    #Creation de final qui est un df avec le code region pour la carte, le nombre d offre et le salaire moyen
    #Pour merge il fallait les 18 regions donc on a ajouter des 0 pour les valeurs grace aux boucles precedentes
    final <- merge(nb_offres,salary, by=c('code'))
    final$n <- as.integer(final$n) #ajout d'une ligne transform tout en charact donc reconverti
    final$Salaire <- as.double(final$Salaire)
    names(final) <- c("hc-key",'nombre',"salaire") #obligé renommé key car sinon reconnait pas ds carte
    #Display map
    
    #Affichage de la carte avec choix de la metrique grace a un input
    highcharter::hcmap(
      "countries/fr/fr-all.js", 
      data = final,
      name = ifelse(input$mesure=="nombre","Nombre d'offres :","Salaire moyen :"), 
      value = input$mesure,
      joinBy = "hc-key"
    )
    
  })
}


shinyApp(ui = ui, server = server)



