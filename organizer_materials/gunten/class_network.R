
### packages, working directory

library(tidverse)
library(igraph)

setwd("C:/Users/tvg/OneDrive - University of Edinburgh/research/snr labs/class network")

# data are from students taking a social networks class in 2022
# a tie indicates that focal student (ego) nominated another student (alter)
# on a social network survey
# contact frequency is the frequency of interaction
# contact type 

contact.freq <- read.csv("contact_frequency.csv")
contact.type <- read.csv("contact_type.csv")
attributes <- read.csv("respondent_attributes.csv")

# note that 'degree' in the attributes files refers to the *degree programme*
# that students are taking - not degree in the network theory sense

### WORKED EXAMPLE ######################

# take a look at the data
View(contact.freq)
View(contact.type)
table(contact.freq$freq)
table(contact.freq$freq)/nrow(contact.freq)
table(contact.type$digital)

# create an igraph object with all network ties
# igraph expects an edgelist to be a two-column matrix
allcontact.ig <- graph_from_edgelist(
                    as.matrix(contact.freq[,c("ego.anon",
                                              "alter.anon")]
                      )
                    )

# simple plot
plot(allcontact.ig)

# drop vertex labels
plot(allcontact.ig,
     vertex.label = NA)

# decrease arrow size
plot(allcontact.ig,
     vertex.label = NA,
     edge.arrow.size = .5)


# match node attributes with network data
# NOTE that the order of nodes can be unpredictable
# I create a second id to keep track of the node order
# left_join does not reorder the data from, but merge (base R) does 

attr <- data.frame(anon.id = V(allcontact.ig)$name,
                   node.seq = 1:length(V(allcontact.ig)$name)) %>% 
  left_join(attributes, by = "anon.id") %>%                                                                    
  arrange(node.seq)

View(attr)

# visualize the network with degree programme as color

plot(allcontact.ig,
     vertex.label = NA,
     edge.arrow.size = .5,
     vertex.color = factor(attr$degree))

# scale node size to a network centrality measure

plot(allcontact.ig,
     vertex.label = NA,
     edge.arrow.size = .2,
     vertex.color = factor(attr$degree),
     vertex.size = degree(allcontact.ig, mode = "all"))

# it is sometimes helpful to use a transformation (e.g. square root or log)
# to prevent high-degree nodes from becoming too large
# notice I set a minimum node size by adding a constant
plot(allcontact.ig,
     vertex.label = NA,
     edge.arrow.size = .2,
     vertex.color = factor(attr$degree),
     vertex.size = 3 + sqrt(degree(allcontact.ig, mode = "all")))


# you can drop self-ties easily by using simplify

plot(simplify(allcontact.ig),
     vertex.label = NA,
     edge.arrow.size = .2,
     vertex.color = factor(attr$degree),
     vertex.size = 3 + sqrt(degree(allcontact.ig, mode = "all")))



### EXERCISE: using the 'contact type' data, compare #########
# the networks of 'mostly digital' and 'mostly in person" contact
# (hint: if digital == F, contact is mostly in person)
# what can we conclude about digital vs in person contact across
# degree programmes?
# similarly, compare the network of more frequent contact
# to the network of all contactr
# what can we conclude about the frequency of interaction within
# and across degree programs?

### EXERCISE: sending gossip
# suppose we want to send a piece of gossip from students on 
# on degree programme to the other, using only word of mouth
# which student(s) would have to pass on the gossip for this to work?
# what if only students who know each other well are willing to
# pass on gossip?




