
### load packages, define functions, set working directory ######

library(tidyverse)
library(igraph)
library(ForceAtlas2)
library(RColorBrewer)
library(classInt) # classIntervals function - used in assigning colors

# function to extract main component from igraph object
main.component <- function(graph, ...) {
  cl <- clusters(graph, ...)
  induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
}

file.stem <- paste0("C:/Users/",
                    Sys.info()["user"],
                    "/OneDrive - University of Edinburgh/")

setwd(paste0(file.stem,"research/twitter"))

### download and restructure MP tweet data ##########

tweets <- readRDS(gzcon(url("https://github.com/cjbarrie/CTA-ED/blob/main/data/wordembed/twts_corpus_sample.rds?raw=true")))

retweets <- tweets %>%
  rename(retweet_username = reftweet_username) %>%
  mutate(year.month = format(date, "%Y-%m"),
         username = tolower(username),
         retweet_username = tolower(retweet_username)) %>%
  drop_na(retweet_username) %>%
  group_by(retweet_username, username,year.month) %>%
  summarise(n_retweets = n(), date = date) %>%
  relocate(year.month,retweet_username, username,n_retweets) %>%
  arrange(year.month)

mentions <- tweets %>%
  filter(nchar(ments)>0)

mentions.temp <- strsplit(mentions$ments, ",", fixed = T)

mentions.long <- data.frame(
  date = as.Date(unlist(mapply(rep,
                       as.list(mentions$date),
                       lapply(mentions.temp, length))),
                 format = "%Y-%m-%d",
                 origin = "1970-01-01"),
  username = unlist(mapply(rep,
                           as.list(mentions$username),
                           lapply(mentions.temp, length))),
  mentioned.account = trimws(unlist(mentions.temp)),
  stringsAsFactors = F
  ) %>%
  mutate(mentioned.account = gsub("@","",
                                mentioned.account,
                                fixed = T),
         year.month = format(date, "%Y-%m")) %>%
  filter(nchar(mentioned.account)>0) %>%
  group_by(username,mentioned.account, year.month) %>%
  summarize(n_mentions = n(), date = date)

### create dataset with party id of MPs ##########################
# this is pretty cludgy, any better ideas?

temp <- split(tweets[,c("username","party_value")],
                  tweets$username)
temp <- lapply(temp, function(x) lapply(x, unique))

# test for any party switching
# all "party_value" vectors should have length 1
table(unlist(lapply(temp, 
                    function(x) length(
                      unique(
                        x$party_value
                        )
                      )
                    )))

party.id <- data.frame(
  username = tolower(unlist(lapply(temp,'[[',"username"))),
  party = unlist(lapply(temp,'[[',"party_value"))
)

rm(temp)

# color-code parties using standard party colors 
# source for party colours:
# https://en.wikipedia.org/wiki/Wikipedia:Index_of_United_Kingdom_political_parties_meta_attributes

party.id <- party.id %>%
  mutate(party.color = recode(party,
                              "Conservative" = "#0087DC",
                              "Democratic Unionist Party" = "#D46A4C",
                              "Green Party" = "#528D6B",
                              "Labour" = "#E4003B",
                              "Labour (Co-op)" = "#E4003B",
                              "Liberal Democrat" = "#FDBB30",
                              "Plaid Cymru" = "#005B54",
                              "Scottish National Party" = "#FFFF00")
                              )


### one period analysis: March - June 2019 #############
# note: this is the period leading up to and immediately after Teresa May's resignation as prime minister

rt.samp <- retweets %>%
  filter(date >= as.Date("2019-03-01") & date <= as.Date("2019-06-30"))%>%
  group_by(retweet_username, username) %>%
  summarize(n_retweets = sum(n_retweets))

ig.samp <- graph_from_edgelist(
  as.matrix(rt.samp[,c("retweet_username","username")]),
  directed = T
)

plot(simplify(ig.samp), 
     vertex.label = NA, 
     vertex.size = 2,
     edge.arrow.size = 0)

samp.attr <- data.frame(
  username = V(ig.samp)$name,
  node.seq = 1:length(V(ig.samp)$name),
  degree.in = degree(ig.samp, mode = "in"),
  between.dir = betweenness(ig.samp, directed = T,normalized = T),
  between.undir = betweenness(ig.samp, directed = F, normalized = T)
  ) %>%
  left_join(party.id,
            by = "username")


# visualize the network with party labels - save a pdf for high resolution
pdf("results_MP/samp1-partylab.pdf", width = 12)
plot(simplify(ig.samp), 
     vertex.label = NA, 
     vertex.size = 4,
     vertex.color = samp.attr$party.color,
     edge.arrow.size = 0)
dev.off()

# explore centrality scores
# who are the most central nodes?
# what is the shape of the degree distribution?
# how does in degree vary by party (e.g. are members of any party more central?)
# how could we add other centrality measures, e.g. betweenness?
# how can we create a summary dataset using dplyr?

hist(samp.attr$degree.in)
hist(samp.attr$degree.in[samp.attr$degree.in>0])

ggplot(samp.attr, aes(y = party, x = degree.in))+
  geom_boxplot()

ggplot(samp.attr, aes(y = party, x = between.dir))+
  geom_boxplot()

ggplot(samp.attr, aes(y = party, x = between.undir))+
  geom_boxplot()

plot(simplify(ig.samp), 
     vertex.label = NA, 
     vertex.size = sqrt(1 + samp.attr$degree.in),
     vertex.color = samp.attr$party.color,
     edge.arrow.size = 0)


plot(simplify(ig.samp), 
     vertex.label = NA, 
     vertex.size = sqrt(2 + samp.attr$between.undir*1000),
     vertex.color = samp.attr$party.color,
     edge.arrow.size = 0)


plot(samp.attr$degree.in, samp.attr$between.dir)
plot(samp.attr$degree.in, samp.attr$between.undir)


# detect subgroups/communities

samp.greedy <- cluster_fast_greedy(as.undirected(simplify(ig.samp)))
samp.louvain <- cluster_louvain(as.undirected(simplify(ig.samp)))

samp.attr$member.greedy <- samp.greedy$membership
samp.attr$member.louvain <- samp.greedy$membership

pdf("results_MP/samp1-greedylab.pdf", width = 12)
plot(simplify(ig.samp), 
     #layout = l,
     vertex.label = NA, 
     vertex.size = 4,
     vertex.color = samp.greedy$membership,
     edge.arrow.size = 0)
dev.off()

pdf("results_MP/samp1-louvainlab.pdf", width = 12)
plot(simplify(ig.samp), 
     #layout = l,
     vertex.label = NA, 
     vertex.size = 4,
     vertex.color = samp.louvain$membership,
     edge.arrow.size = 0)
dev.off()


# within each subgroup, what % of nodes belong to each party?
# (among accounts that belong to MPs?)

samp.attr <- samp.attr %>%
  group_by(member.greedy) %>%
  mutate(greedy.pcnt.labour = 
           sum(party=="Labour" | party=="Labour (Co-op)", na.rm = T)/sum(!is.na(party)),
         greedy.pcnt.cons = 
           sum(party=="Conservative", na.rm = T)/sum(!is.na(party)),
         greedy.lean.cons = greedy.pcnt.cons- greedy.pcnt.labour)

# assign colors to subgroups based on this

nclr <- 10
min <- -1 # theoretical minimum
max <- 1 # theoretical maximum
breaks <- (max - min) / nclr

plotclr <- brewer.pal(nclr, "RdBu")
plotvar <- samp.attr$greedy.lean.cons
class <- classIntervals(plotvar,
                        nclr,
                        style = "fixed",
                        fixedBreaks = seq(min, max, breaks))
colcode <- findColours(class, 
                       plotclr)

pdf("results_MP/samp1-greedy-leancons.pdf", width = 12)
plot(simplify(ig.samp), 
     vertex.label = NA, 
     vertex.size = 4,
     vertex.color = colcode,
     edge.arrow.size = 0)
dev.off()

png("results_MP/samp1-greedy-leancons.png")
plot(simplify(ig.samp), 
     vertex.label = NA, 
     vertex.size = 4,
     vertex.color = colcode,
     edge.arrow.size = 0)
dev.off()

# now try repeating this analysis using the results 
# from the louvain community detection algorithm

### analysis of structure of mostly conservative and labour clusters

# identify nodes belonging to mostly conservative/labour clusters
labour.nodes <- unique(samp.attr$node.seq[samp.attr$greedy.pcnt.labour >= .5])
cons.nodes <- unique(samp.attr$node.seq[samp.attr$greedy.pcnt.cons >= .5])

# extract subnetworks of these nodes
ig.samp.labour <- induced.subgraph(ig.samp, labour.nodes)
ig.samp.cons <- induced.subgraph(ig.samp, cons.nodes)

plot(simplify(ig.samp.labour),
     vertex.size = 3,
     vertex.label = NA,
     edge.arrow.size = 0)

plot(simplify(ig.samp.cons),
     vertex.size = 3,
     vertex.label = NA,
     edge.arrow.size = 0)

edge_density(ig.samp.labour)
edge_density(ig.samp.cons)

### mention networks: one period ###########################

mentions.samp <- mentions.long %>%
  filter(date >= as.Date("2019-03-01") & date <= as.Date("2019-06-30"))%>%
  group_by(username, mentioned.account) %>%
  summarize(n_mentions = sum(n_mentions))

mentions.ig <- graph_from_edgelist(
            as.matrix(
                mentions.samp[,c("username","mentioned.account")]
                )
            )

plot(simplify(mentions.ig), 
     vertex.label = NA, 
     vertex.size = 2,
     edge.arrow.size = 0)


mentions.samp.attr <- data.frame(
  username = V(mentions.ig)$name,
  node.seq = 1:length(V(mentions.ig)$name),
  degree.in = degree(mentions.ig, mode = "in")
  ) %>%
  left_join(party.id,
            by = "username")

pdf("results_MP/mentions-samp-partylab.pdf", width = 12)
plot(simplify(mentions.ig), 
     vertex.label = NA, 
     vertex.size = 4,
     vertex.color = mentions.samp.attr$party.color,
     edge.arrow.size = 0)
dev.off()

# now explore centrality in the mentions network

### ANALYSIS BY MONTH: transform retweet dataset into network EDGELIST and then IGRAPH object #######

# step 1: create monthly edgelist (must be in matrix format with 2 columns)
el.rt.monthly <- lapply(split(retweets, retweets$year.month), 
                      function(x){
  as.matrix(x[,c("retweet_username","username")])
})

# step 2: edgelist to igraph object 
ig.rt.monthly <- lapply(el.rt.monthly,
                        graph_from_edgelist, 
                        directed = T)


# gather node-level information
# in this case we will just use a loop for convenience
# note that we pre-assign the list nodes.monthly
# as before it is important to keep track of the NODE ORDER


nodes.monthly <- list()
for(i in 1:length(ig.rt.monthly)){
  nodes.monthly[[i]] <-  data.frame(username = V(ig.rt.monthly[[i]])$name)
  nodes.monthly[[i]]$node.seq <- 1:nrow(nodes.monthly[[i]])
  nodes.monthly[[i]] <- merge(nodes.monthly[[i]],
                              party.id,
                              by = "username",
                              all.x = T)
  nodes.monthly[[i]]$party.color <- ifelse(is.na(nodes.monthly[[i]]$party.color),
                                           "grey",
                                           nodes.monthly[[i]]$party.color)
  nodes.monthly[[i]] <- nodes.monthly[[i]][order(nodes.monthly[[i]]$node.seq),]
  V(ig.rt.monthly[[i]])$vertex.color <- nodes.monthly[[i]]$party.color
  
}

### visualize all the networks ########

dates <- unique(retweets$year.month)

for(i in 1:length(ig.rt.monthly)){
pdf(paste0("results_MP/rt-monthly-", dates[i],".pdf"), width = 12)
  plot(simplify(ig.rt.monthly[[i]]), 
      vertex.label = NA, 
      vertex.size = 2,
      vertex.color = V(ig.rt.monthly[[i]])$vertex.color,
      edge.arrow.size = 0,
      main = dates[i])
  dev.off()
}  

### modularity analysis in monthly data ########

# we need to work with just the main component
ig.rt.monthly.mc <- lapply(ig.rt.monthly, main.component)

greedy.rt.monthly.mc <- lapply(ig.rt.monthly.mc,
                               function(x){
                                 cluster_fast_greedy(simplify(as.undirected(x)))
                               }
  )

# inspect the modularity values
lapply(greedy.rt.monthly.mc, modularity)
plot(1:length(dates),
     unlist(lapply(greedy.rt.monthly.mc, modularity)))

dates[15]

for(i in 1:length(ig.rt.monthly)){
  pdf(paste0("results/rt-monthly-greedy-", dates[i],".pdf"), width = 12)
  plot(simplify(ig.rt.monthly.mc[[i]]), 
       vertex.label = NA, 
       vertex.size = 5,
       vertex.color = greedy.rt.monthly.mc[[i]]$membership,
       edge.arrow.size = 0,
       main = dates[i])
  dev.off()
} 


### centrality over time ######################

indeg.monthly <- data.frame(
    index = names(unlist(lapply(ig.rt.monthly, degree, mode = "in"))),
    degree.in = unlist(lapply(ig.rt.monthly, degree, mode = "in"))
  ) %>%
  mutate(year.month = substring(index,1,7),
         username = substring(index,9,nchar(index))) %>%
  group_by(username) %>%
  mutate(ever.high.indeg = any(degree.in > 50)) %>%
  left_join(party.id, by = "username")

indeg.monthly.party <- indeg.monthly %>%
  drop_na(party) %>%
  group_by(party, year.month) %>%
  summarize(indeg.party.mean = mean(degree.in),
            indeg.party.median = median(degree.in),
            indeg.party.sd = sd(degree.in))


ggplot(filter(indeg.monthly, ever.high.indeg == T),
       aes(y = degree.in, x = year.month, group = username,
           color = username))+
  geom_line() + geom_smooth(se = F) +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1))+
  xlab("")

ggplot(indeg.monthly.party, aes(y = indeg.party.mean, 
                             x = year.month, 
                             group = party, color = party))+
  geom_line()+geom_smooth(se = F) +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1))+
  xlab("")

ggplot(indeg.monthly.party, aes(y = indeg.party.median, 
                                x = year.month, 
                                group = party, color = party))+
  geom_line()+geom_smooth(se = F) +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1))+
  xlab("")

ggplot(indeg.monthly.party, aes(y = indeg.party.sd, 
                                x = year.month, 
                                group = party, color = party))+
  geom_line()+geom_smooth(se = F) +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1))+
  xlab("")
                            
  



