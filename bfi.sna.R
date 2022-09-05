#Loading Data & Packages #########
#names.women.df <- read.csv(file.choose()) #as of 2/9/22 using names_unarmed_women
#names.men.df <- read.csv(file.choose()) #as of 10/5/21 using names_unarmed_men
#bfi.sna <- read.csv(file.choose()) #as of 11/22/2021, using all_years_SNA_noposs_nospacetest_standardized

names.women.df <- 
  read.csv("C:\\Users\\Ian Davis\\Box Sync\\Studies\\BFI - Black Female Invisibility\\Data Analysis\\SNA\\names_women.csv")
#as of 2/9/22 using names_unarmed_women

names.men.df <- 
  read.csv("C:\\Users\\Ian Davis\\Box Sync\\Studies\\BFI - Black Female Invisibility\\Data Analysis\\SNA\\names_unarmed_men.csv")
#as of 10/5/21 using names_unarmed_men

bfi.sna <- 
  read.csv("C:\\Users\\Ian Davis\\Box Sync\\Studies\\BFI - Black Female Invisibility\\Data Analysis\\SNA\\csv by year for sna\\all years_SNA_noposs_spacetest_standardized.csv") 
#as of 11/22/2021, using all_years_SNA_noposs_nospacetest_standardized

#Loading Packages
bfi_lib<-c("lme4", "readr", "tidyr", "effects", "ggplot2", "psych",
      "MASS", "Rmisc", "plyr", "dplyr", "lmerTest", "ggthemes",
      "lsmeans", "pastecs", "sjstats", "car","irr", "reshape2",
      "gridExtra", "modelr", "tidytext", "tibble", "broom",
        "igraph", "RColorBrewer", "ggraph", "widyr", "stringr",
      "visNetwork", "htmlwidgets", "purr")

lapply(bfi_lib,require,character.only=TRUE)

#Cleaning####
colnames(bfi.sna) <- c("col", "line", "article", "year") #rename columns
bfi.sna <- bfi.sna %>% filter(article != "") #filter out empty rows
bfi.sna$col <- as.character(bfi.sna$col) 

#Dataframes by year
bfi.2014 <- filter(bfi.sna, year == "2014")
bfi.2015 <- filter(bfi.sna, year == "2015")
bfi.2016 <- filter(bfi.sna, year == "2016")
bfi.2017 <- filter(bfi.sna, year == "2017")
bfi.2018 <- filter(bfi.sna, year == "2018")
bfi.2019 <- filter(bfi.sna, year == "2019")
bfi.2020 <- filter(bfi.sna, year == "2020")

#n.articles <- bfi.sna %>% group_by(article) %>%
#                          summarize(year = mean(year)) %>%
#                          group_by(year) %>%
#                          summarize(n = n())
#write.csv(n.articles, file = "sna.include.csv") 
table(bfi.sna$year)

#Cleaning Names dfs####
names.men.v <- as.vector(names.men.df$all.names)
#Possessive permutations of names (e.g. Brown's, Gurns')
names.men.poss1.v <- as.vector(paste0(names.men.v, after = paste("'s")))
names.men.poss2.v <- as.vector(paste0(names.men.v, after = paste("'")))

all.names.men.v <- c(names.men.v, names.men.poss1.v, names.men.poss2.v)
all.names.men.colname.v <- make.names(all.names.men.v)

names.women.v <- as.vector(names.women.df$all.names)
#Possessive permutations of names (e.g. Brown's, Gurns')
names.women.poss1.v <- as.vector(paste0(names.women.v, after = paste("'s")))
names.women.poss2.v <- as.vector(paste0(names.women.v, after = paste("'")))

all.names.women.v <- c(names.women.v, names.women.poss1.v, names.women.poss2.v)
all.names.women.colname.v <- make.names(all.names.women.v)

all.names.v <- c(all.names.women.v, all.names.men.v)

#Bigram Analysis#####
#Break whole corpus into overlapping two-word chunks, 'bigrams'
bfi.sna.bigram <- bfi.sna  %>%
      unnest_tokens(bigram, col, token = "ngrams", n = 2)

#(Tagged out) Break whole corpus into individual words, 'unigrams'
#bfi.sna.unigram <- bfi.sna  %>%
#  unnest_tokens(bigram, col, token = "ngrams", n = 1)

#(Tagged out) Mean and SD of # of words in each line of corpus in initial CSV
#df.summary <- bfi.sna.unigram %>% group_by(line) %>%   
#                 summarise(words = n()) %>%
#                  summarise(words.mean = mean(words),
#                            words.sd = sd(words))

##2014
#Bigram analysis
bfi.2014.bigram <- bfi.sna.bigram %>% filter(year == "2014")

#Count names in 2014  
bigrams_unsep.2014 <- bfi.2014.bigram %>%
  group_by(article) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  #filter(!word1 %in% stop_words$word) %>%
  #filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  filter(bigram %in% all.names.v) %>%
  spread(article, n) %>%
  column_to_rownames(var = "bigram")

#bigrams_unsep %>% unite()  #sum possessives and non-possessives

#bigrams.2014.long <- as.data.frame(t(bigrams_unsep.2014))



## 2015 

bfi.2015.bigram <- bfi.sna.bigram %>% filter(year == "2015")

#Count names in 2015
bigrams_unsep.2015 <- bfi.2015.bigram %>%
  group_by(article) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  #filter(!word1 %in% stop_words$word) %>%
  #filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  filter(bigram %in% all.names.v) %>%
  spread(article, n) %>%
  column_to_rownames(var = "bigram")

#bigrams_unsep %>% unite()  #sum possessives and non-possessives

bigrams.2015.long <- as.data.frame(t(bigrams_unsep.2015))

## 2016 

bfi.2016.bigram <- bfi.sna.bigram %>% filter(year == "2016")

#Count names in 2016
bigrams_unsep.2016 <- bfi.2016.bigram %>%
  group_by(article) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  #filter(!word1 %in% stop_words$word) %>%
  #filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  filter(bigram %in% all.names.v) %>%
  spread(article, n) %>%
  column_to_rownames(var = "bigram")

## 2017 

bfi.2017.bigram <- bfi.sna.bigram %>% filter(year == "2017")

#Count names in 2017
bigrams_unsep.2017 <- bfi.2017.bigram %>%
  group_by(article) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  #filter(!word1 %in% stop_words$word) %>%
  #filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  filter(bigram %in% all.names.v)%>%
  spread(article, n) %>%
  column_to_rownames(var = "bigram")

## 2018 

bfi.2018.bigram <- bfi.sna.bigram %>% filter(year == "2018")

#Count names in 2018
bigrams_unsep.2018 <- bfi.2018.bigram %>%
  group_by(article) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  #filter(!word1 %in% stop_words$word) %>%
  #filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  filter(bigram %in% all.names.v) %>%
  spread(article, n) %>%
  column_to_rownames(var = "bigram")


## 2019 

bfi.2019.bigram <- bfi.sna.bigram %>% filter(year == "2019")

#Count names in 2019
bigrams_unsep.2019 <- bfi.2019.bigram %>%
  group_by(article) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  #filter(!word1 %in% stop_words$word) %>%
  #filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  filter(bigram %in% all.names.v) %>%
  spread(article, n) %>%
  column_to_rownames(var = "bigram")

## 2020 

bfi.2020.bigram <- bfi.sna.bigram %>% filter(year == "2020")

#Count names in 2020
bigrams_unsep.2020 <- bfi.2020.bigram %>%
  group_by(article) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  #filter(!word1 %in% stop_words$word) %>%
  #filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  filter(bigram %in% all.names.v) %>%
  spread(article, n) %>%
  column_to_rownames(var = "bigram")


# Annual sums #######

bigrams_unsep.2014$sum.2014 <- rowSums(bigrams_unsep.2014, na.rm = TRUE)
sums.2014 <- bigrams_unsep.2014 %>% select(sum.2014) %>%
                                    rownames_to_column()
bigrams_unsep.2015$sum.2015 <- rowSums(bigrams_unsep.2015, na.rm = TRUE)
sums.2015 <- bigrams_unsep.2015 %>% select(sum.2015)%>%
                                    rownames_to_column()
bigrams_unsep.2016$sum.2016 <- rowSums(bigrams_unsep.2016, na.rm = TRUE)
sums.2016 <- bigrams_unsep.2016 %>% select(sum.2016)%>%
                                    rownames_to_column()
bigrams_unsep.2017$sum.2017 <- rowSums(bigrams_unsep.2017, na.rm = TRUE)
sums.2017 <- bigrams_unsep.2017 %>% select(sum.2017)%>%
                                    rownames_to_column()
bigrams_unsep.2018$sum.2018 <- rowSums(bigrams_unsep.2018, na.rm = TRUE)
sums.2018 <- bigrams_unsep.2018 %>% select(sum.2018)%>%
                                    rownames_to_column()
bigrams_unsep.2019$sum.2019 <- rowSums(bigrams_unsep.2019, na.rm = TRUE)
sums.2019 <- bigrams_unsep.2019 %>% select(sum.2019)%>%
                                    rownames_to_column()
bigrams_unsep.2020$sum.2020 <- rowSums(bigrams_unsep.2020, na.rm = TRUE)
sums.2020 <- bigrams_unsep.2020 %>% select(sum.2020)%>%
                                    rownames_to_column()

#Join annual sums into one dataframe
year.to.year.sums <- full_join(sums.2014, sums.2015, by = "rowname")
year.to.year.sums <- full_join(year.to.year.sums, sums.2016, by = "rowname")
year.to.year.sums <- full_join(year.to.year.sums, sums.2017, by = "rowname")
year.to.year.sums <- full_join(year.to.year.sums, sums.2018, by = "rowname")
year.to.year.sums <- full_join(year.to.year.sums, sums.2019, by = "rowname")
year.to.year.sums <- full_join(year.to.year.sums, sums.2020, by = "rowname")
row.names(year.to.year.sums) <- make.names(year.to.year.sums$rowname)
year.to.year.sums <- year.to.year.sums %>% select(-rowname)

#Compute total mentions across years
total.sums <- year.to.year.sums %>%
                mutate(total = as.numeric(rowSums(year.to.year.sums, na.rm = TRUE))) %>%
                mutate(annual.mean = total/7)
total.sums.space <- rownames_to_column(total.sums)
colnames(total.sums.space) <- c("name", "2014", "2015", "2016", "2017", "2018", "2019", "2020",
                                "ment", "annual.mean")
total.sums.space$name <- str_to_title(gsub(".", " ", total.sums.space$name, fixed = T))
total.sums.space <- total.sums.space %>% mutate(gender = ifelse(total.sums.space$name %in% str_to_title(all.names.women.v),
                                                                1, 0),
                                                name2 = name)

#Pull summary statistics of mentions by gender
totals.gender.summary <- total.sums.space %>% group_by(gender) %>%
                            summarise(n = n(),
                                      total = sum(ment),
                                      sd = sd(ment),
                                      median = median(ment),
                                      mean = mean(ment),
                                      annual.mean.sd = sd(annual.mean, na.rm = T),
                                      annual.mean = mean(annual.mean))
totals.gender.summary <- bind_rows(totals.gender.summary, total.sums.space %>% 
                                    summarise(n = n(),
                                              total = sum(ment),
                                              sd = sd(ment),
                                              median = median(ment),
                                              mean = mean(ment),
                                              annual.mean.sd = sd(annual.mean, na.rm = T),
                                              annual.mean = mean(annual.mean)))


total.sums.space %>% group_by(gender) %>%
  summarise(sd = sd(annual.mean, na.rm = T))

#Clean summary dataframes for visualization
t.year.to.year.sums <- as.data.frame(t(year.to.year.sums))
t.year.to.year.sums <- rownames_to_column(t.year.to.year.sums, var = "year")

year.to.year.sums.women <- filter(year.to.year.sums, rownames(year.to.year.sums) %in% all.names.women.colname.v)
t.year.to.year.sums.women <- as.data.frame(t(year.to.year.sums.women)) %>%
                              rownames_to_column(var = "year") %>%
                              na_if(0)
melt.women <- melt(t.year.to.year.sums.women, id.vars = 'year', variable.name = 'name') %>%
              arrange(desc(value)) %>%
              mutate(name2 = factor(name, unique(name)))
melt.women.zero <- melt.women %>% replace_na(list(value = 0))
melt.women.no.na <- melt.women %>% na.omit()
                                  
                                                                    
year.to.year.sums.men <- filter(year.to.year.sums, rownames(year.to.year.sums) %in% all.names.men.colname.v)
t.year.to.year.sums.men <- as.data.frame(t(year.to.year.sums.men))%>%
                            rownames_to_column(var = "year")%>%
                            na_if(0)
melt.men <- melt(t.year.to.year.sums.men, id.vars = 'year', variable.name = 'name')%>% 
            arrange(desc(value)) %>%
            mutate(name2 = factor(name, unique(name)))
melt.men.no.na <- melt.men %>% na.omit()
melt.men.zero <- melt.men %>% replace_na(list(value = 0))

#Limit to just high visibility (often-mentioned) men
melt.men.hivis.list <- melt.men.no.na %>% group_by(name2) %>%
                                      summarise(avg = mean(value)) %>%
                                      filter(avg > 10) %>%
                                      ungroup()
melt.men.hivis <- melt.men.no.na %>% filter(name2 %in% melt.men.hivis.list$name2)

#Limit to just low visibility (infrequently mentioned) men
melt.men.lovis.list <- melt.men.no.na %>% group_by(name2) %>%
                                      summarise(avg = mean(value)) %>%
                                      filter(avg <= 10) %>%
                                      ungroup()
melt.men.lovis <- melt.men.no.na %>% filter(name2 %in% melt.men.lovis.list$name2)


###Year to year name frequency plot ####
n <- 60
qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))


year.to.year.plot.men.line <- ggplot(melt.men.zero, aes(year, value, group = name2)) +
  geom_line(aes(colour = name2), size = 1) +
  scale_color_manual(values = col_vector)
year.to.year.plot.men.line

year.to.year.plot.men.area <- ggplot(melt.men.zero, aes(year, value, group = name2)) +
  geom_area(aes(colour = name2, fill = name2), alpha = 0.1) +
  scale_color_manual(values = col_vector)+
  scale_fill_manual(values = col_vector)+
  guides(colour = guide_legend())
year.to.year.plot.men.area

year.to.year.plot.men.col <- ggplot() +
  geom_col(data = melt.men.no.na, aes(x = year, y = value, 
                      group = name2, fill = name2), 
                     position = position_dodge2(width = 1.1, preserve = "single"),
                                                width = 1.05) +
                    scale_fill_manual(values = col_vector) +
                    scale_y_continuous(breaks = seq(0,235, 10))+
                    labs(x = "Year",
                         y = "Count")
year.to.year.plot.men.col

year.to.year.plot.men.hivis <- ggplot() +
  geom_col(data = melt.men.hivis, 
           aes(x = year, y = value, group = name2, fill = name2), 
           position = position_stack(reverse = TRUE), width = 0.75) +
  scale_fill_manual(values = col_vector) +
  scale_y_continuous(breaks = seq(0,500, 20))+
  labs(x = "Year",
       y = "Count")
year.to.year.plot.men.hivis

year.to.year.plot.men.lovis <- ggplot() +
  geom_col(data = melt.men.lovis, 
           aes(x = year, y = value, group = name2, fill = name2), 
           position = position_stack(reverse = TRUE), width = 0.75) +
  scale_fill_manual(values = col_vector) +
  scale_y_continuous(breaks = seq(0, 90, 5))+
  labs(x = "Year",
       y = "Count")
year.to.year.plot.men.lovis

year.to.year.plot.men.stack <- ggplot() +
  geom_col(data = melt.men.no.na, 
           aes(x = year, y = value, group = name2, fill = name2), 
           position = position_stack(reverse = TRUE), width = 0.75) +
  scale_fill_manual(values = col_vector) +
  scale_y_continuous(breaks = seq(0,500, 20))+
  labs(x = "Year",
       y = "Count")
year.to.year.plot.men.stack


year.to.year.plot.women.col <- ggplot() +
  geom_col(data = melt.women.no.na, aes(x = year, y = value, 
                                        group = name2, fill = name2), 
                                        position = position_dodge2(width = 0.8, preserve = "single"),
                                        width = 0.75) +
  scale_fill_manual(values = col_vector) +
  scale_y_continuous(breaks = seq(0,36, 2))+
  labs(x = "Year",
       y = "Count")
year.to.year.plot.women.col

year.to.year.plot.women.stack <- ggplot() +
  geom_col(data = melt.women.no.na, 
           aes(x = year, y = value, group = name2, fill = name2), 
           position = position_stack(reverse = TRUE), width = 0.75) +
  scale_fill_manual(values = col_vector) +
  scale_y_continuous(breaks = seq(0,50, 2))+
  labs(x = "Year",
       y = "Count")
year.to.year.plot.women.stack

year.to.year.plot.women.area <- ggplot(melt.women.zero, aes(year, value, group = name2)) +
  geom_area(aes(colour = name2, fill = name2), alpha = 0.1) +
  scale_color_manual(values = col_vector)+
  scale_fill_manual(values = col_vector)+
  guides(colour = guide_legend())


year.to.year.plot.women.area2 <- ggplot(melt.women, aes(year, value, group = name2)) +
  geom_area(aes(colour = name2, fill = name2), alpha = 0.1) +
  scale_fill_gradientn(colors = col_vector) +
  guides(colour = guide_legend())

year.to.year.plot.women.point <- ggplot(melt.women, aes(year, value)) +
  geom_point(aes(colour = name))

year.to.year.plot.women.line <- ggplot(melt.women.zero, aes(year, value, group = name)) +
  geom_line(aes(colour = name))



#Names per Article######
names.by.article <- bfi.sna.bigram %>%
  filter(bigram %in% all.names.v) %>%
  mutate(year.article = paste0(year, ".", article))%>%
  select(-line, -year, -article) %>%
  pivot_wider(names_from = year.article, values_from = bigram) %>%
  t()%>%
  as.data.frame()%>%
  rename(name = V1)%>%
  rownames_to_column(var = "year.article")


names.by.article$name <- gsub("c(", "", names.by.article$name, fixed = T)
names.by.article$name <- gsub(")", "", names.by.article$name, fixed = T)
names.by.article$name <- gsub(' "', "", names.by.article$name, fixed = T)
names.by.article$name <- gsub('"', "", names.by.article$name, fixed = T)
names.by.article <- names.by.article %>% separate(name, into = c("name1","name2","name3",
                                                                 "name4","name5","name6",
                                                                 "name7","name8","name9",
                                                                 "name10","name11","name12",
                                                                 "name13","name14","name15",
                                                                 "name16","name17","name18",
                                                                 "name19","name20","name21",
                                                                 "name22","name23","name24",
                                                                 "name25","name26","name27",
                                                                 "name28","name29","name30",
                                                                 "name31","name32","name33",
                                                                 "name34","name35","name36",
                                                                 "name37","name38","name39",
                                                                 "name40","name41","name42",
                                                                 "name43","name44","name45"),
                                                  sep =",")
#names.by.article <- separate(names.by.article, year.article, into = c("year", "article","c"), sep = ".", remove = F)

name.gender.by.article <- names.by.article %>%
  mutate(across(starts_with("name"), ~ifelse( .x %in% all.names.women.v, 1,
                                              ifelse(.x %in% all.names.men.v, 0, NA))))
name.gender.by.article <- as.data.frame(lapply(name.gender.by.article, as.numeric))

by.article.summary <- name.gender.by.article %>% mutate(n.women = rowSums(name.gender.by.article==1, na.rm =T),
                                                        n.men = rowSums(name.gender.by.article==0, na.rm =T),
                                                        n.total.names = rowSums((name.gender.by.article==0 
                                                                                 | name.gender.by.article==1), na.rm = T)) %>%
                                                  select(year.article, n.women, n.men, n.total.names)
by.article.super.summary <- by.article.summary %>% summarise(women.median = median(n.women),
                                                             women.mean = mean(n.women),
                                                             women.sd = sd(n.women),
                                                             women.total = sum(n.women),
                                                             men.median = median(n.men),
                                                             men.mean = mean(n.men),
                                                             men.sd = sd(n.men),
                                                             men.total = sum(n.men),
                                                             pooled.median = median(n.total.names),
                                                             pooled.mean = mean(n.total.names),
                                                             pooled.sd = sd(n.total.names),
                                                             pooled.total = sum(n.total.names))
# Social Network Analysis (SNA) ##########
#Pair Counts
bfi.sna.bigram$article <- as.numeric(as.character(bfi.sna.bigram$article))

pair.count.all <- bfi.sna.bigram %>%
  filter(bigram %in% all.names.v) %>%
  #group_by(article) %>% 
  widyr::pairwise_count(bigram, article, sort = TRUE) %>%
  mutate(item1 = str_to_title(item1),
         item2 = str_to_title(item2))

net <- pair.count.all %>%
  graph_from_data_frame()

adj.matrix <- as.matrix(as_adjacency_matrix(net))
n.edges.matrix <- as.data.frame(rowSums(adj.matrix)) %>%
                    rownames_to_column()
colnames(n.edges.matrix) <- c("name", "edges")
n.edges <- sum(n.edges.matrix$edges)
density <- n.edges / (50*(50-1))
adj.matrix <- as.data.frame(adj.matrix)
women.women.adj.matrix <- adj.matrix %>% filter(rownames(adj.matrix) %in% str_to_title(all.names.women.v)) %>%
                                          select_if(colnames(adj.matrix) %in% str_to_title(all.names.women.v))
wwam.summary <- women.women.adj.matrix %>% 
                mutate(women.women.edges = rowSums(women.women.adj.matrix))%>%
                select(women.women.edges)
women.men.adj.matrix <- adj.matrix %>% filter(rownames(adj.matrix) %in% str_to_title(all.names.women.v)) %>%
  select_if(colnames(adj.matrix) %in% str_to_title(all.names.men.v))
wmam.summary <- women.men.adj.matrix %>% 
  mutate(women.men.edges = rowSums(women.men.adj.matrix))%>%
  select(women.men.edges)
men.women.adj.matrix <- adj.matrix %>% filter(rownames(adj.matrix) %in% str_to_title(all.names.men.v)) %>%
  select_if(colnames(adj.matrix) %in% str_to_title(all.names.women.v))
mwam.summary <- men.women.adj.matrix %>% 
  mutate(men.women.edges = rowSums(men.women.adj.matrix))%>%
  select(men.women.edges)
men.men.adj.matrix <- adj.matrix %>% filter(rownames(adj.matrix) %in% str_to_title(all.names.men.v)) %>%
  select_if(colnames(adj.matrix) %in% str_to_title(all.names.men.v))
mmam.summary <- men.men.adj.matrix %>% 
  mutate(men.men.edges = rowSums(men.men.adj.matrix))%>%
  select(men.men.edges)
edge.women.summary <- bind_cols(wwam.summary, wmam.summary) %>%
  mutate(total = women.men.edges + women.women.edges)
edge.men.summary <- bind_cols(mwam.summary, mmam.summary)%>%
  mutate(total = men.women.edges + men.men.edges)
edge.gender.super.summary <- edge.women.summary %>% summarise(median.ww = median(women.women.edges),
                                                              mean.ww = mean(women.women.edges),
                                                              sd.ww = sd(women.women.edges),
                                                              total.ww = sum(women.women.edges),
                                                              median.wm = median(women.men.edges),
                                                              mean.wm = mean(women.men.edges),
                                                              sd.wm = sd(women.men.edges),
                                                              total.wm = sum(women.men.edges)) %>%
                            bind_rows(edge.men.summary %>% summarise(median.mm = median(men.men.edges),
                                                                     mean.mm = mean(men.men.edges),
                                                                     sd.mm = sd(men.men.edges),
                                                                     total.mm = sum(men.men.edges),
                                                                     median.mw = median(men.women.edges),
                                                                     mean.mw = mean(men.women.edges),
                                                                     sd.mw = sd(men.women.edges),
                                                                     total.mw = sum(men.women.edges)))

#net.neigh <- connect.neighborhood(net, 50)


vert.attr <- as.data.frame(V(net)$name)
colnames(vert.attr) <- "name"
vert.attr <- left_join(vert.attr, total.sums.space, by = "name")%>%
              select(-name2) %>%
              mutate(color = ifelse(gender == 1, "brown1","mediumpurple3"))
V(net)$ment <- as.numeric(vert.attr$ment)
V(net)$gender <- as.numeric(vert.attr$gender)
V(net)$color <- vert.attr$color

edge.attr <- as.data.frame(get.edgelist(net)) %>%
              rename(name = V1, name2 = V2) %>%
              mutate(n = E(net)$n, 
                      width = E(net)$width)
edge.attr <- left_join(edge.attr, select(total.sums.space, c(name, gender)), by = "name")
edge.attr <- left_join(edge.attr, select(total.sums.space, c(name2, gender)), by = "name2")%>%
              mutate(gendercombo = gender.x + gender.y)
E(net)$gendercombo <- as.numeric(edge.attr$gendercombo)

edge.attr.summary.gender <- edge.attr %>% filter(gender.x ==1) %>%
                                      group_by(gendercombo) %>%
                                      summarise(size.median = median(n),
                                                size.mean= mean(n),
                                                size.sd = sd(n), 
                                                size.total = sum(n)) %>%
                          bind_rows(edge.attr.summary.men <- edge.attr %>% filter(gender.x ==0) %>%
                                      group_by(gendercombo) %>%
                                      summarise(size.median = median(n),
                                                size.mean= mean(n),
                                                size.sd = sd(n),
                                                size.total = sum(n)))

degree.df <- data.frame(deg = degree(net, mode = "out")) %>%
  rownames_to_column(var = "name")
between <- as.data.frame(betweenness(net))
between <- rownames_to_column(between)
colnames(between) <- c("name", "btw")
between$btw <- as.numeric(as.character(between$btw))

pair.count.summary <- pair.count.all %>% 
  rename(name = item1) %>% 
  group_by(name) %>% 
  summarise(total = sum(n), mean = mean(n)) %>% #Total gives total times the name co-occurs with other names
  left_join(total.sums.space, by = "name")%>% #Mean gives the name's typical frequency of co-occurring with any other given name
  left_join(degree.df, by = "name")%>% #Degree gives the number of other names that co-occur with the name in question
  select(-name2)%>% #Mentions gives the total number of mentions (irrespective of co-occurrences)
  left_join(between, by = "name")


pair.count.super.summary <- pair.count.summary %>%  
  summarise(ment2 = sum(ment),
            ment.sd = sd(ment),
            ment.mean = mean(ment),
            ment.median = median(ment),
            total.co = sum(total),
            total.mean = mean(total),
            total.median = median(total),
            total.sd = sd(total),
            degree =sum(deg),
            deg.median = median(deg),
            deg.mean = mean(deg),
            deg.sd = sd(deg),
           # co.occur.per.ment = total/ment, 
            between.sd = sd(btw),
            between = mean(btw, na.rm= T)) %>%
  ungroup() 

pair.count.summary.gender <- pair.count.summary %>% group_by(gender) %>% 
                      summarise(ment2 = sum(ment),
                                ment.sd = sd(ment),
                                ment.mean = mean(ment),
                                ment.median = median(ment),
                                total.co = sum(total),
                                total.mean = mean(total),
                                total.median = median(total),
                                total.sd = sd(total),
                                degree =sum(deg),
                                deg.median = median(deg),
                                deg.mean = mean(deg),
                                deg.sd = sd(deg),
                                #co.occur.per.ment = total/ment, 
                                between.sd = sd(btw),
                                between = mean(btw, na.rm= T)) %>%
                      ungroup() 
pair.count.summary.gender <- pair.count.summary.gender %>% bind_rows(pair.count.super.summary)


subset(total.sums.space,  !(total.sums.space$name %in% n.edges.matrix$name))

 
# SNA Visualization ######
#Full network#######

l <- layout_with_fr(net,dim = 2)
l <- as.data.frame(l)
l.save <- l
l <- l %>% mutate(d = V1^2 + V2^2)
l.small <- l %>% filter(d <= quantile(d, 0.1)) %>%
  select(V1, V2)
l.small <- as.matrix(l.small)
l.mid1 <- l %>% filter((d > quantile(d, 0.1) & d <= quantile(d, 0.2))) %>%
  select(V1, V2)
l.mid1 <- as.matrix(l.mid1)
l.mid2 <- l %>% filter((d > quantile(d, 0.2) & d <= quantile(d, 0.3))) %>%
  select(V1, V2)
l.mid2 <- as.matrix(l.mid2)
l.mid3 <- l %>% filter((d > quantile(d, 0.3) & d <= quantile(d, 0.4))) %>%
  select(V1, V2)
l.mid3 <- as.matrix(l.mid3)
l.mid4 <- l %>% filter((d > quantile(d, 0.4) & d <= quantile(d, 0.5))) %>%
  select(V1, V2)
l.mid4 <- as.matrix(l.mid4)
l.mid5 <- l %>% filter((d > quantile(d, 0.5) & d <= quantile(d, 0.6))) %>%
  select(V1, V2)
l.mid5 <- as.matrix(l.mid5)
l.mid6 <- l %>% filter((d > quantile(d, 0.6) & d <= quantile(d, 0.7))) %>%
  select(V1, V2)
l.mid6 <- as.matrix(l.mid6)
l.mid7 <- l %>% filter((d > quantile(d, 0.7) & d <= quantile(d, 0.8))) %>%
  select(V1, V2)
l.mid7 <- as.matrix(l.mid7)
l.mid8 <- l %>% filter((d > quantile(d, 0.8) & d <= quantile(d, 0.95))) %>%
  select(V1, V2)
l.mid8 <- as.matrix(l.mid8)
l.big <- l %>% filter(d >= quantile(d, 0.95)) %>%
  select(V1, V2)
l.big <- as.matrix(l.big)
l <- rbind(0.5*l.small, 0.6*l.mid1,0.5*l.mid2,0.6*l.mid3,0.6*l.mid4,
           0.6*l.mid5,0.5*l.mid6,0.5*l.mid7,0.4*l.mid8,0.2*l.big)
l <- norm_coords(l, ymin=-1.2, ymax=1.2, xmin=-2.1, xmax=2.1)
l.d <- as.data.frame(l)
l.d <- l.d %>% mutate(d = V1^2 + V2^2, d.x = rep(1:50)) %>%
  mutate(pi = atan(V2/V1))
l.save <- bind_cols(l.save, l.d)
write.csv(l.save, "g.fr.all2.csv")
mean(l.d$pi)

g.l.d <- ggplot(l.d) +
  geom_point(aes(x = d.x, y = d)) +
  geom_smooth(aes(x = d.x, y = d))
g.l.d

#Plot
V(net)$size <- 0.075*V(net)$ment
E(net)$width <- 1.1*(E(net)$n)
E(net)$color <- ifelse(E(net)$gendercombo == 0, "mediumpurple1",
                        ifelse(E(net)$gendercombo == 1,"springgreen", "red"))
col <- adjustcolor(E(net)$color, alpha.f = 0.4)

png(filename = "g.all.fr.png", width = 1920, height = 1080, units = "px", pointsize = 19, res = 125)
plot(net, rescale=F, margin = -2, xlim = c(-1.1,1.1), ylim = c(-1.1,1.1),
     layout=l, 
     vertex.shape = "circle",
     vertex.color = (c( "brown1", "mediumpurple3")[1+(V(net)$gender=="0")]),
     vertex.frame.color = (c( "brown1", "mediumpurple3")[1+(V(net)$gender=="0")]),
     vertex.label.color="black", vertex.label.dist=3,
     vertex.label.degree = 2*pi,
     vertex.label.family="Arial", vertex.label.cex = 0.5,
     edge.arrow.size=.0001, 
     edge.color = col,
     remove.multiple = T, remove.loops = T,
     edge.attr.comb=c(weight="sum"))
legend(x=-1.5, y=-1.4, c("Women","Men"), pch = 21,
       col="#777777", pt.bg=c("brown1", "mediumpurple3"), pt.cex=1.5, cex=.8, bty="n", ncol=1, y.intersp = 1.1)
dev.off()
file.show("D:\\rwd\\g.all.fr.png")


####################################
V(net)$size <- (V(net)$ment)*0.05
E(net)$width <- ((E(net)$n))
#color.scale <- colorRampPalette(c("purple", "aquamarine"))
#E(net)$color <- ifelse(E(net)$gendercombo < 1, "mediumpurple", "darkorange2") #color edges by gender
color.scale <- c("mediumpurple", "aquamarine", "violetred4")
E(net)$color <- ifelse(E(net)$gendercombo < 1, "mediumpurple", "darkorange2")
col <- adjustcolor(E(net)$color, alpha.f = 0.3)

l <- layout_with_fr(net,dim = 2)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1.25, xmax=1.25)
#par(mar=c(0.01,0.01,0.01,0.01))
png(filename = "g.fr7.png", width = 1820, height = 980, units = "px", pointsize = 10, res = 200)
plot(net, rescale=F, asp = 0.6, margin = -0.5,
     layout=l*1.2, 
     vertex.color= c("brown1", "lightskyblue")[1+(V(net)$gender=="0")],
     vertex.shape = "circle",
     vertex.frame.color = (c( "brown1", "lightskyblue")[1+(V(net)$gender=="0")]),
     vertex.label.color="black", vertex.label.dist=2,
     vertex.label.degree = pi/10,
     vertex.label.family="Arial", vertex.label.cex = 0.7,
     edge.arrow.size=.0001, 
     edge.color = col,
     remove.multiple = T, remove.loops = T,
     edge.attr.comb=c(weight="sum"))
legend(x=-1.5, y=-1, c("Women","Men"), pch = 21,
       col="#777777", pt.bg=c("brown1", "lightskyblue"), pt.cex=1.5, cex=.8, bty="n", ncol=1, y.intersp = 1.1)
dev.off()
file.show("D:\\rwd\\g.fr7.png")

#Previous layouts
#l <- layout_as_tree(net, circular = T, flip.y = T)
#l <- layout_with_kk(net)
#l <- layout_with_graphopt(net, charge = 1.2)

#Just frequently co-occurring individuals######
co.occur.df2 <- pair.count.summary %>% filter(total < 3)
co.occur.v2 <- as.vector(str_to_title(co.occur.df2$name))
net.occur2 <- delete_vertices(net, V(net)$name %in% co.occur.v2)

l <- layout_with_fr(net.occur2,dim = 2)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1.25, xmax=1.25)
#par(mar=c(0.01,0.01,0.01,0.01))
png(filename = "g.occur2.fr.png", width = 1820, height = 980, units = "px", pointsize = 10, res = 200)
plot(net.occur2, rescale=F, asp = 0.6, margin = -0.5,
     layout=l*1.2, 
     vertex.color= c("brown1", "lightskyblue")[1+(V(net.occur2)$gender=="0")],
     vertex.shape = "circle",
     vertex.frame.color = (c( "brown1", "lightskyblue")[1+(V(net.occur2)$gender=="0")]),
     vertex.label.color="black", vertex.label.dist=2,
     vertex.label.degree = pi/10,
     vertex.label.family="Arial", vertex.label.cex = 0.7,
     edge.arrow.size=.0001, 
     edge.color = col,
     remove.multiple = T, remove.loops = T,
     edge.attr.comb=c(weight="sum"))
legend(x=-1.5, y=-1, c("Women","Men"), pch = 21,
       col="#777777", pt.bg=c("brown1", "lightskyblue"), pt.cex=1.5, cex=.8, bty="n", ncol=1, y.intersp = 1.1)
dev.off()
file.show("D:\\rwd\\g.occur2.fr.png")


#Just high visibility individuals######
mention1 <- total.sums %>% filter(total==1)
mention2 <- total.sums %>% filter(total<3)

mention1 <- as.data.frame(gsub(".", " ", rownames(mention1), fixed = T))
colnames(mention1) <- "col"
mention1 <- mention1 %>% filter(col %in% all.names.men.v)
mention2 <- as.data.frame(gsub(".", " ", rownames(mention2), fixed = T))
colnames(mention2) <- "col"
mention2 <- mention2 %>% filter(col %in% all.names.men.v)

net.bign <- delete_vertices(net, V(net)$name %in% str_to_title(mention2))
net.bign <- delete_vertices(net, V(net)$name %in% str_to_title(mention2))
#V(net.bign)$name <- gsub(" ", "\n", V(net.bign)$name, fixed = T)

#V(net.bign)$size <- (E(net.bign)$n)+0.25
color.scale <- colorRampPalette(c("aquamarine", "mediumpurple1"))
#color.bign <- color.scale(degree(net.bign, mode="all"))

V(net.bign)$size <- 0.08*(V(net.bign)$ment)
E(net.bign)$width <- 1.2*(E(net.bign)$n)
#color.scale <- colorRampPalette(c("purple", "aquamarine"))
color.scale <- c("mediumpurple1", "aquamarine", "violetred4")
E(net.bign)$color <- ifelse(E(net.bign)$gendercombo < 1, "mediumpurple1", "darkorange2")
col <- adjustcolor(E(net.bign)$color, alpha.f = 0.5)
#net.bign <- delete_edges(net.bign, which(E(net.bign)$n<2))


#Layout####
#l <- layout_with_graphopt(net.bign, charge = 1.2)

l <- layout_with_fr(net.bign,dim = 2)
l <- as.data.frame(l)
l <- l %>% mutate(d = V1^2 + V2^2)
l.small <- l %>% filter(d <= quantile(d, 0.1)) %>%
  select(V1, V2)
l.small <- as.matrix(l.small)
l.mid1 <- l %>% filter((d > quantile(d, 0.1) & d <= quantile(d, 0.2))) %>%
  select(V1, V2)
l.mid1 <- as.matrix(l.mid1)
l.mid2 <- l %>% filter((d > quantile(d, 0.2) & d <= quantile(d, 0.3))) %>%
  select(V1, V2)
l.mid2 <- as.matrix(l.mid2)
l.mid3 <- l %>% filter((d > quantile(d, 0.3) & d <= quantile(d, 0.4))) %>%
  select(V1, V2)
l.mid3 <- as.matrix(l.mid3)
l.mid4 <- l %>% filter((d > quantile(d, 0.4) & d <= quantile(d, 0.5))) %>%
  select(V1, V2)
l.mid4 <- as.matrix(l.mid4)
l.mid5 <- l %>% filter((d > quantile(d, 0.5) & d <= quantile(d, 0.6))) %>%
  select(V1, V2)
l.mid5 <- as.matrix(l.mid5)
l.mid6 <- l %>% filter((d > quantile(d, 0.6) & d <= quantile(d, 0.7))) %>%
  select(V1, V2)
l.mid6 <- as.matrix(l.mid6)
l.mid7 <- l %>% filter((d > quantile(d, 0.7) & d <= quantile(d, 0.8))) %>%
  select(V1, V2)
l.mid7 <- as.matrix(l.mid7)
l.mid8 <- l %>% filter((d > quantile(d, 0.8) & d <= quantile(d, 0.9))) %>%
  select(V1, V2)
l.mid8 <- as.matrix(l.mid8)
l.big <- l %>% filter(d >= quantile(d, 0.9)) %>%
  select(V1, V2)
l.big <- as.matrix(l.big)
l <- rbind(0.6*l.small, 0.6*l.mid1,0.6*l.mid2,0.6*l.mid3,0.6*l.mid4,
           0.6*l.mid5,0.6*l.mid6,0.5*l.mid7,0.3*l.mid8,0.2*l.big)
l <- norm_coords(l, ymin=-1.1, ymax=1.1, xmin=-2, xmax=2)
l.d <- as.data.frame(l)
l.d <- l.d %>% mutate(d = V1^2 + V2^2, d.x = rep(1:50)) %>%
                mutate(pi = atan(V2/V1))
mean(l.d$pi)

g.l.d <- ggplot(l.d) +
          geom_point(aes(x = d.x, y = d)) +
          geom_smooth(aes(x = d.x, y = d))
g.l.d

#Plot####
png(filename = "g.bign.fr.n19.png", width = 1920, height = 1080, units = "px", pointsize = 22, res = 100)
plot(net.bign, rescale=F, margin = -2,
     layout=l, 
     vertex.color= c("brown1", "mediumpurple3")[1+(V(net.bign)$gender=="0")],
     vertex.shape = "circle",
     vertex.frame.color = (c( "brown1", "mediumpurple3")[1+(V(net.bign)$gender=="0")]),
     vertex.label.color="black", vertex.label.dist=2,
     vertex.label.degree = pi/10,
     vertex.label.family="Arial", vertex.label.cex = 0.7,
     edge.arrow.size=.0001, 
     edge.color = col,
     remove.multiple = T, remove.loops = T,
     edge.attr.comb=c(weight="sum"))
legend(x=-1.5, y=-1.2, c("Women","Men"), pch = 21,
       col="#777777", pt.bg=c("brown1", "mediumpurple3"), pt.cex=1.5, cex=.8, bty="n", ncol=1, y.intersp = 1.1)
dev.off()
file.show("D:\\rwd\\g.bign.fr.n19.png")

#Natural FR Layout####
mention2 <- total.sums %>% filter(total<3)
mention2 <- as.data.frame(gsub(".", " ", rownames(mention2), fixed = T))
colnames(mention2) <- "col"
net.bign.2 <- delete_vertices(net, V(net)$name %in% str_to_title(mention2$col))


#V(net.bign)$size <- 0.5*(V(net.bign)$ment)
V(net.bign.2)$size <- 0.08*V(net.bign.2)$ment
E(net.bign.2)$width <- 1.25*(E(net.bign.2)$n)
#color.scale <- colorRampPalette(c("purple", "aquamarine"))
color.scale <- c("mediumpurple1", "aquamarine", "violetred4")
E(net.bign.2)$color <- ifelse(E(net.bign.2)$gendercombo < 1, "mediumpurple1", "darkorange2")
col <- adjustcolor(E(net.bign.2)$color, alpha.f = 0.4)

#l <- layout_with_graphopt(net.bign.2, charge = 1.2)

l <- layout_with_fr(net.bign.2,dim = 2)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1.5, xmax=1.5)
png(filename = "g.bign.2.fr.nat.png", width = 1820, height = 980, units = "px", pointsize = 25)
plot(net.bign.2, rescale=F, layout=l*1.2, 
     vertex.color= c("brown1", "mediumpurple3")[1+(V(net.bign.2)$gender=="0")],
     vertex.shape = "circle",
     vertex.frame.color = (c( "brown1", "mediumpurple3")[1+(V(net.bign.2)$gender=="0")]),
     vertex.label.color="black", vertex.label.dist=2,
     vertex.label.degree = pi/10,
     vertex.label.family="Arial", vertex.label.cex = 0.7,
     edge.arrow.size=.0001, 
     edge.color = col,
     remove.multiple = T, remove.loops = T,
     edge.attr.comb=c(weight="sum"))
legend(x=-1.5, y=-1, c("Women","Men"), pch = 21,
       col="#777777", pt.bg=c("brown1", "mediumpurple3"), pt.cex=1.5, cex=.8, bty="n", ncol=1, y.intersp = 1.1)
dev.off()
file.show("D:\\rwd\\g.bign.2.fr.nat.png")

#BigN Top 5 Ment (Natural FR)####
l <- layout_with_fr(net.top5,dim = 2)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

#Top 10 Mentioned Women and Men####
#l <- layout_with_graphopt(net.bign, charge = 1.2)
top10.men.v <- pair.count.summary %>% 
              filter(gender==0) %>%            
              mutate(ment.rank = 37 - (ntile(ment,36))) %>%
              filter(ment.rank < 11) %>%
              select(name)
top10.men.v <- str_to_title(as.vector(top10.men.v$name))
              
top10.women.v <- pair.count.summary %>% 
      filter(gender==1) %>%            
      mutate(ment.rank = 14-(ntile(ment,13))) %>%
      filter(ment.rank < 11) %>%
      select(name)
top10.women.v <- str_to_title(as.vector(top10.women.v$name))

net.top10 <- delete_vertices(net, !((V(net)$name %in% top10.men.v) 
                                   | (V(net)$name %in% top10.women.v)))    

l <- layout_with_fr(net.top10,dim = 2)
l <- norm_coords(l, ymin=-1, ymax=1.6, xmin=-2.2, xmax=2.2)
l <- as.data.frame(l)
l.save2 <- l
l <- l %>% mutate(d = V1^2 + V2^2)
l <- l %>% mutate(d.rank = ntile(d, 20))
#l.small <- l %>% filter(d.rank <= 2) %>%
 # select(V1, V2)
l.small <- l %>% filter(d <= quantile(d, 0.1)) %>%
  select(V1, V2)
l.small <- as.matrix(l.small)
l.mid1 <- l %>% filter((d > quantile(d, 0.1) & d <= quantile(d, 0.2))) %>%
  select(V1, V2)
l.mid1 <- as.matrix(l.mid1)
l.mid2 <- l %>% filter((d > quantile(d, 0.2) & d <= quantile(d, 0.3))) %>%
  select(V1, V2)
l.mid2 <- as.matrix(l.mid2)
l.mid3 <- l %>% filter((d > quantile(d, 0.3) & d <= quantile(d, 0.4))) %>%
  select(V1, V2)
l.mid3 <- as.matrix(l.mid3)
l.mid4 <- l %>% filter((d > quantile(d, 0.4) & d <= quantile(d, 0.5))) %>%
  select(V1, V2)
l.mid4 <- as.matrix(l.mid4)
l.mid5 <- l %>% filter((d > quantile(d, 0.5) & d <= quantile(d, 0.6))) %>%
  select(V1, V2)
l.mid5 <- as.matrix(l.mid5)
l.mid6 <- l %>% filter((d > quantile(d, 0.6) & d <= quantile(d, 0.7))) %>%
  select(V1, V2)
l.mid6 <- as.matrix(l.mid6)
l.mid7 <- l %>% filter((d > quantile(d, 0.7) & d <= quantile(d, 0.8))) %>%
  select(V1, V2)
l.mid7 <- as.matrix(l.mid7)
l.mid8 <- l %>% filter((d > quantile(d, 0.8) & d <= quantile(d, 0.9))) %>%
  select(V1, V2)
l.mid8 <- as.matrix(l.mid8)
l.big <- l %>% filter(d > quantile(d, 0.9) & d < quantile(d, 0.95)) %>%
  select(V1, V2)
l.big <- as.matrix(l.big)
l.big2 <- l %>% filter(d >= quantile(d, 0.95)) %>%
  select(V1, V2)
l.big2 <- as.matrix(l.big2)
l <- rbind(1*l.small, 1.2*l.mid1,1.2*l.mid2,1.2*l.mid3,1.2*l.mid4,
           1.2*l.mid5,1.1*l.mid6,1*l.mid7,.9*l.mid8,0.8*l.big, 0.7*l.big2)
#l <- rbind(0.4*l.small, 0.5*l.mid1,0.625*l.mid2,0.625*l.mid3,0.625*l.mid4,
 #          0.625*l.mid5,0.625*l.mid6,0.625*l.mid7,0.5*l.mid8,0.425*l.big, 0.3*l.big2)

l.d <- as.data.frame(l)
#l.d <- l.d %>% mutate(V1 = V1-0.5, V2 = V2-0.5)
l.d <- l.d %>% mutate(d = V1^2 + V2^2, d.x = rep(1:16)) 
l.d <- l.d %>%  mutate(pi = (180*atan(V2/V1))/pi)
l.save2 <- bind_cols(l.save2, l.d)
write.csv(l.save2, "g.fr.top5.csv")
mean(l.d$pi)
mean(l.d$d)
g.l.d <- ggplot(l.d) +
  geom_point(aes(x = d.x, y = d)) +
  geom_smooth(aes(x = d.x, y = d))
g.l.d

#Plot (Top 5)##
#V(net.top10)$size <- 0.5*(V(net.top10)$ment)
V(net.top10)$size <- 0.075*V(net.top10)$ment
E(net.top10)$width <- 1.1*(E(net.top10)$n)
#color.scale <- colorRampPalette(c("purple", "aquamarine"))
color.scale <- c("mediumpurple1", "aquamarine", "violetred4")
E(net.top10)$color <- ifelse(E(net.top10)$gendercombo == 0, "mediumpurple1",
                            ifelse(E(net.top10)$gendercombo == 1,"springgreen", "red"))
col <- adjustcolor(E(net.top10)$color, alpha.f = 0.4)

#l <- layout_with_graphopt(net.top10, charge = 1.2)

png(filename = "g.top10.fr.png", width = 1920, height = 1080, units = "px", pointsize = 19, res = 125)
plot(net.top10, rescale=F, margin = -2, xlim = c(-1.1,1.1), ylim = c(-1.1,1.1),
     layout=l, 
     vertex.color= c("brown1", "mediumpurple3")[1+(V(net.top10)$gender=="0")],
     vertex.shape = "circle",
     vertex.frame.color = (c( "brown1", "mediumpurple3")[1+(V(net.top10)$gender=="0")]),
     vertex.label.color="black", vertex.label.dist=4,
     vertex.label.degree = 2*pi,
     vertex.label.family="Arial", vertex.label.cex = 0.7,
     edge.arrow.size=.0001, 
     edge.color = col,
     remove.multiple = T, remove.loops = T,
     edge.attr.comb=c(weight="sum"))
legend(x=-1.5, y=-1.2, c("Women","Men"), pch = 21,
       col="#777777", pt.bg=c("brown1", "mediumpurple3"), pt.cex=1.5, cex=.8, bty="n", ncol=1, y.intersp = 1.1)
dev.off()
file.show("D:\\rwd\\g.top10.fr.png")


#Connections of 1######
pair.count.n1 <-  pair.count.all %>%
  filter(n==1) 
net.n1 <- pair.count.n1 %>%
  graph_from_data_frame()

net.n1 <- connect.neighborhood(net.n1, 50)
V(net.n1)$size <- (net.n1)$n
E(net.n1)$width <- (E(net.n1)$n)
color.scale <- colorRampPalette(c("aquamarine", "purple"))
col <- color.scale(degree(net.n1, mode="all"))

l.n1 <- layout_as_star(net.n1)
l.n1 <- norm_coords(l, ymin=-1.1, ymax=1.1, xmin=-2, xmax=2)
plot(net, rescale=F, layout=l.n1, 
     vertex.color = "lightskyblue", vertex.shape = "circle",
     vertex.frame.color = "lightskyblue",
     vertex.label.color="black", vertex.label.dist=2,
     vertex.label.degree = pi/10,
     vertex.label.family="Arial", vertex.label.cex = 0.6,
     edge.arrow.size=.01, 
     edge.color=col,
     remove.multiple = T, remove.loops = T,
     edge.attr.comb=c(weight="sum"))


#w/o Michael Brown#####
net.noMB <- delete_vertices(net, V(net)$name == "Michael Brown")

V(net.noMB)$size <- 0.25*(V(net.noMB)$ment)
E(net.noMB)$width <- ((E(net.noMB)$n))
#color.scale <- colorRampPalette(c("purple", "aquamarine"))
color.scale <- c("mediumpurple", "aquamarine", "violetred4")
E(net.noMB)$color <- ifelse(E(net.noMB)$gendercombo < 1, "mediumpurple", "darkorange2")
col <- adjustcolor(E(net.noMB)$color, alpha.f = 0.3)

l <- layout_with_graphopt(net.noMB, charge = 1.2)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-2, xmax=2)
plot(net.noMB, rescale=F, layout=l, 
     vertex.color= c("brown1", "lightskyblue")[1+(V(net.noMB)$gender=="0")],
     vertex.shape = "circle",
     vertex.frame.color = (c( "brown1", "lightskyblue")[1+(V(net.noMB)$gender=="0")]),
     vertex.label.color="black", vertex.label.dist=2,
     vertex.label.degree = pi/10,
     vertex.label.family="Arial", vertex.label.cex = 0.7,
     edge.arrow.size=.001, 
     edge.color = col,
     remove.multiple = T, remove.loops = T,
     edge.attr.comb=c(weight="sum"))
legend(x=-1.5, y=-1, c("Women","Men"), pch = 21,
       col="#777777", pt.bg=c("brown1", "lightskyblue"), pt.cex=1.5, cex=.8, bty="n", ncol=1, y.intersp = 1.1)

#w/o Michael Brown or Eric Garner####
net.noMB.EG <- delete_vertices(net, V(net)$name == c("Michael Brown", "Eric Garner"))

#V(net.noMB.EG)$size <- 0.5*(V(net.noMB.EG)$ment)
V(net.noMB.EG)$size <- 0.08*V(net.noMB.EG)$ment
E(net.noMB.EG)$width <- 1.25*(E(net.noMB.EG)$n)
#color.scale <- colorRampPalette(c("purple", "aquamarine"))
color.scale <- c("mediumpurple1", "aquamarine", "violetred4")
E(net.noMB.EG)$color <- ifelse(E(net.noMB.EG)$gendercombo < 1, "mediumpurple1", "darkorange2")
col <- adjustcolor(E(net.noMB.EG)$color, alpha.f = 0.4)

#l <- layout_with_graphopt(net.noMB.EG, charge = 1.2)

l <- layout_with_fr(net.noMB.EG,dim = 2)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1.5, xmax=1.5)
png(filename = "g.noMB.EG.fr3.png", width = 1820, height = 980, units = "px", pointsize = 25)
plot(net.noMB.EG, rescale=F, layout=l*1.2, 
     vertex.color= c("brown1", "mediumpurple3")[1+(V(net.noMB.EG)$gender=="0")],
     vertex.shape = "circle",
     vertex.frame.color = (c( "brown1", "mediumpurple3")[1+(V(net.noMB.EG)$gender=="0")]),
     vertex.label.color="black", vertex.label.dist=2,
     vertex.label.degree = pi/10,
     vertex.label.family="Arial", vertex.label.cex = 0.7,
     edge.arrow.size=.0001, 
     edge.color = col,
     remove.multiple = T, remove.loops = T,
     edge.attr.comb=c(weight="sum"))
legend(x=-1.5, y=-1, c("Women","Men"), pch = 21,
       col="#777777", pt.bg=c("brown1", "mediumpurple3"), pt.cex=1.5, cex=.8, bty="n", ncol=1, y.intersp = 1.1)
dev.off()
file.show("C:\\Users\\Ian Davis\\Documents\\g.noMB.EG.fr3.png")

## Attempt at two center star########
TwoCenters = union(s1, s2)
LO1 <- layout_as_star(net)
LO2 = LO1
LO2[1,] = c(-0.2, 0)
LO2 = rbind(LO2, c(0.2,0))
plot(net, layout= LO2, 
     vertex.color = "lightskyblue", vertex.shape = "circle",
     vertex.frame.color = "lightskyblue",
     vertex.label.color="black", vertex.label.dist=2,
     vertex.label.degree = pi/10,
     vertex.label.family="Arial", vertex.label.cex = 0.6,
     edge.arrow.size=.01, 
     edge.color=col,
     remove.multiple = T, remove.loops = T,
     edge.attr.comb=c(weight="sum"))

net.mb <- pair.count.all %>%
  filter(item1 != "eric.garner"|
           item2 != "eric.garner") %>%
  graph_from_data_frame()

net.eg <- pair.count.all %>%
  filter(item1 != "michael.brown"|
           item2 != "michael.brown") %>%
  graph_from_data_frame

net.mb <- connect.neighborhood(net.mb, 50)
V(net.mb)$size <- (E(net.mb)$n/2)+0.25
E(net.mb)$width <- (2*(E(net.mb)$n))/3
color.scale <- colorRampPalette(c("aquamarine", "purple"))
col <- color.scale(degree(net.mb, mode="all"))
l.mb <- layout_as_star(net.mb)
l.mb <- norm_coords(l.neigh.mb, ymin=-1.5, ymax=1.5, xmin=-2, xmax=2)
plot(net.mb, rescale=F, layout=l.mb, 
     vertex.color = "lightskyblue", vertex.shape = "circle",
     vertex.frame.color = "lightskyblue",
     vertex.label.color="black", vertex.label.dist=2,
     vertex.label.degree = pi/10,
     vertex.label.family="Arial", vertex.label.cex = 0.6,
     edge.arrow.size=.01, 
     edge.color=col,
     remove.multiple = T, remove.loops = T,
     edge.attr.comb=c(weight="sum"))

net.eg <- connect.neighborhood(net, 50)
V(net.eg)$size <- (E(net.mb)$n/2)+0.25
E(net.eg)$width <- (2*(E(net.eg)$n))/3
color.scale <- colorRampPalette(c("aquamarine", "purple"))
col <- color.scale(degree(net.eg, mode="all"))
net.eg.mb <- union(net.mb, net.eg)
l.eg.mb <- layout_as_star(net.eg.mb)
l.eg.mb <- norm_coords(l.eg.mb, ymin=-1.5, ymax=1.5, xmin=-2, xmax=2)
plot(net.eg.mb, rescale=F, layout=l.eg.mb, 
     vertex.color = "lightskyblue", vertex.shape = "circle",
     vertex.frame.color = "lightskyblue",
     vertex.label.color="black", vertex.label.dist=2,
     vertex.label.degree = pi/10,
     vertex.label.family="Arial", vertex.label.cex = 0.6,
     edge.arrow.size=.01, 
     edge.color=col,
     remove.multiple = T, remove.loops = T,
     edge.attr.comb=c(weight="sum"))


## Attempts using ggraph#####
plot.pair.count.all <- pair.count.all %>%
  graph_from_data_frame() %>%
  ggraph(layout = "gem") +
  geom_edge_link(aes(edge_width = n, edge_alpha = n, color = n), 
                 show.legend = FALSE) +
  geom_node_point(aes(size = n), color = "midnightblue", size = 2) +
  geom_node_text(aes(label = name), hjust = -0.2, repel = TRUE)+
  #norm_coords(ymin=-1, ymax=1, xmin=-2, xmax=2)+
  theme(legend.position = "none")
plot.pair.count.all

plot.pair.count.all2 <- pair.count.all %>%
  graph_from_data_frame() %>%
  ggraph("grid") +
  geom_edge_link(aes(edge_width = n, edge_alpha = n, color = n), 
                 show.legend = FALSE) +
  geom_node_point(aes(size = n), color = "midnightblue", size = 2) +
  geom_node_text(aes(label = name), hjust = -0.2, repel = TRUE) +
  scale_color_viridis_d(reverse = TRUE)
plot.pair.count.all2


####Pair Correlations#########

widyr_2014 <- bfi.2014 %>% unnest_tokens(word, col)
widyr_2015 <- bfi.2015 %>% unnest_tokens(word, col)
widyr_2016 <- bfi.2016 %>% unnest_tokens(word, col)
widyr_2017 <- bfi.2017 %>% unnest_tokens(word, col)
widyr_2018 <- bfi.2018 %>% unnest_tokens(word, col)
widyr_2019 <- bfi.2019 %>% unnest_tokens(word, col)
widyr_2020 <- bfi.2020 %>% unnest_tokens(word, col)
widyr_all <- bfi.sna %>% unnest_tokens(word, col)
widyr_all$line <- as.numeric(as.character(widyr_all$line))
widyr_all$index <- as.numeric(as.character(widyr_all$index))
widyr_all$year <- as.numeric(as.character(widyr_all$year))
widyr_all$article <- as.numeric(as.character(widyr_all$article))

widyr_all2 <- bfi.sna %>% unnest_tokens(word, col, tokens = 'ngrams', n = 2)
#pairs.2014 <-  widyr_2014 %>% pairwise_count(word, article)
#pairs.2014.item1.men <- pairs.2014 %>% filter(item1 %in% unigram.names.men.v) %>%
                                        #mutate(gen = "men")
#pairs.2014.item2.men <- pairs.2014 %>% filter(item2 %in% unigram.names.men.v)%>%
                                        #mutate(gen = "men")
#pairs.2014.item1.women <- pairs.2014 %>% filter(item1 %in% unigram.names.women.v) %>%
                                         # mutate(gen = "women")
#pairs.2014.item2.women <- pairs.2014 %>% filter(item2 %in% unigram.names.women.v)%>%
                                          # mutate(gen = "women")
#df.pairs.2014 <- pairs.2014.item1.men %>% bind_rows(pairs.2014.item1.women) %>%
                                          #bind_rows(pairs.2014.item2.men) %>%
                                          #bind_rows(pairs.2014.item2.women)
pairs.2014 <- read.csv(file.choose()) #as of 10/8/21 using pairs.2014.csv

#####All Years########
pair.cors.all <- widyr_all %>%
  group_by(word) %>%
  filter(word %in% unigram.names.v) %>%
  pairwise_cor(word, article, sort = TRUE)

plot.pair.cors.all <- pair.cors.all %>%
  filter(correlation > .95) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_width = correlation, edge_alpha = correlation), color = "lightcoral", 
                 show.legend = FALSE) +
  geom_node_point(color = "midnightblue", size = 2) +
  geom_node_text(aes(label = name), hjust = -0.2, repel = TRUE) +
  theme_void()
plot.pair.cors.all


#####2014########
pair.cors.2014.women <- widyr_2014 %>%
  group_by(word) %>%
  filter(word %in% unigram.names.women.v) %>%
  pairwise_cor(word, article, sort = TRUE)

plot.pair.cors.2014.women <- pair.cors.2014.women %>%
                            filter(correlation > .4) %>%
                            graph_from_data_frame() %>%
                            ggraph(layout = "fr") +
                            geom_edge_link(aes(edge_width = correlation, edge_alpha = correlation), color = "lightskyblue", 
                                           show.legend = FALSE) +
                            geom_node_point(color = "midnightblue", size = 2) +
                            geom_node_text(aes(label = name), hjust = -0.1, repel = TRUE) +
                            theme_void()
plot.pair.cors.2014.women

boyd.cor <- pairs.2014 %>% filter(item1 == "boyd") 
brown.cor <- pairs.2014 %>% filter(item1 == "brown")%>%
                            pairwise_cor(word, article, sort = TRUE)
#####2015#######
widyr_2015 <- bfi.2015 %>% unnest_tokens(word, col)
widyr_2015$article <- as.numeric(as.character(widyr_2015$article)) 
                                      
#pairs.2015 <-  widyr_2015 %>% pairwise_count(word, article)
pairs.2015.item1.men <- pairs.2015 %>% filter(item1 %in% unigram.names.men.v) %>%
mutate(gen = "men")
pairs.2015.item2.men <- pairs.2015 %>% filter(item2 %in% unigram.names.men.v)%>%
mutate(gen = "men")
pairs.2015.item1.women <- pairs.2015 %>% filter(item1 %in% unigram.names.women.v) %>%
 mutate(gen = "women")
pairs.2015.item2.women <- pairs.2015 %>% filter(item2 %in% unigram.names.women.v)%>%
 mutate(gen = "women")
df.pairs.2015 <- pairs.2015.item1.men %>% bind_rows(pairs.2015.item1.women) %>%
bind_rows(pairs.2015.item2.men) %>%
bind_rows(pairs.2015.item2.women)

pair.cors.2015.women <- widyr_2015 %>%
  group_by(word) %>%
  filter(word %in% unigram.names.women.v) %>%
  filter(article != "") %>%
  pairwise_cor(word, article, sort = TRUE)

plot.pair.cors.2015.women <- pair.cors.2015.women %>%
  filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_width = correlation, edge_alpha = correlation), color = "lightcoral", 
                 show.legend = FALSE) +
  geom_node_point(color = "midnightblue", size = 2) +
  geom_node_text(aes(label = name), hjust = -0.2, repel = TRUE) +
  theme_void()
plot.pair.cors.2015.women

# Unigram Analysis #####

unigram.names.women <- as.data.frame(all.names.women.v)
unigram.names.women$all.names.women.v <- as.character(unigram.names.women$all.names.women.v)
unigram.names.women <- unigram.names.women %>% 
  unnest_tokens(unigram, all.names.women.v, 
                token = "ngrams", n = 1) %>%
  filter(!duplicated(unigram))
unigram.names.women.v <- unigram.names.women$unigram
unigram.names.women.v <- unigram.names.women.v[nchar(unigram.names.women.v) > 1]


unigram.names.men <- as.data.frame(all.names.men.v)
unigram.names.men$all.names.men.v <- as.character(unigram.names.men$all.names.men.v)
unigram.names.men <- unigram.names.men %>% 
  unnest_tokens(unigram, all.names.men.v, 
                token = "ngrams", n = 1) %>%
  filter(!duplicated(unigram))
unigram.names.men.v <- unigram.names.men$unigram
unigram.names.men.v <- unigram.names.men.v[nchar(unigram.names.men.v) > 1]

unigram.names.v <- c(unigram.names.women.v, unigram.names.men.v)


bigrams_count.2014 <- bfi.2014.bigram %>%
  group_by(article) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  count(word1, word2, sort = TRUE) %>%
  filter(word1 %in% unigram.names.men.v |
           word2 %in% unigram.names.men.v)
#filter(!word1 %in% stop_words$word) %>%
#filter(!word2 %in% stop_words$word) %>%

#bigrams_count.2014.word1 <- bigrams_count.2014 %>% 
#filter(word1 %in% unigram.names.men$unigram)

#bigrams_count.2014.word2 <- bigrams_count.2014 %>% 
#filter(word2 %in% unigram.names.men$unigram)
#bigrams_count.2014$word1 %in% unigram.names.men.v 

#bigrams_count.2014 %>% visualize_bigrams()

bigrams_count.2014 <- bigrams_count.2014[, 2:4]


bigram_graph <- bigrams_count.2014 %>%
  filter(n > 2) %>%
  graph_from_data_frame()


a <- grid::arrow(type = "closed", length = unit(.1, "inches"))

plot_bigram_net <- ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.001, 'inches')) +
  geom_node_point(color = "lightblue", size = 1) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 3) +
  theme_void()
#plot_bigram_net

