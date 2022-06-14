

# don't peek until you have already tried to solve the exercises

### by contact type ####################################

digital <- graph_from_edgelist(
            as.matrix(
              contact.type[contact.type$digital==T,c("ego.anon","alter.anon")]
              )
            )

attr.digital <- data.frame(anon.id = V(digital)$name,
                           node.seq = 1:length(V(digital)$name)) %>% 
  left_join(attributes, by = "anon.id") %>%                                                                    
  arrange(node.seq)

f2f <- graph_from_edgelist(
  as.matrix(
    contact.type[contact.type$digital==F,c("ego.anon","alter.anon")]
  )
)

attr.f2f <- data.frame(anon.id = V(f2f)$name,
                           node.seq = 1:length(V(f2f)$name)) %>% 
  left_join(attributes, by = "anon.id") %>%                                                                    
  arrange(node.seq)



plot(digital,
     vertex.label = NA,
     edge.arrow.size = .5,
     vertex.color = factor(attr.digital$degree))

plot(f2f,
     vertex.label = NA,
     edge.arrow.size = .5,
     vertex.color = factor(attr.digital$degree))


### by contact type ####################################

at.least.monthly <- graph_from_edgelist(
  as.matrix(
    contact.freq[contact.freq$freq %in% c("once_week","once_month"),
                 c("ego.anon","alter.anon")]
  )
)

attr.monthly <- data.frame(anon.id = V(at.least.monthly)$name,
                       node.seq = 1:length(V(at.least.monthly)$name)) %>% 
  left_join(attributes, by = "anon.id") %>%                                                                    
  arrange(node.seq)


plot(at.least.monthly,
     vertex.label = NA,
     edge.arrow.size = .5,
     vertex.color = factor(attr.monthly$degree))

### gossip exercise ################################

# we can answer easily using visualization
# but might also use betwenness centrality to measure this
# note that betweenness by itself may not match our intuition
# because it doesn't take group structure into account
# see the 'brokerage' function in the SNA package
# for a group-sensitive brokerage measure

sort(betweenness(as.undirected(allcontact.ig)), decreasing = T)
sort(betweenness(as.undirected(at.least.monthly)), decreasing = T)

plot(simplify(allcontact.ig),
     #vertex.label = NA,
     vertex.label.dist = 1,
     edge.arrow.size = .2,
     vertex.color = factor(attr$degree),
     vertex.size = 3 + sqrt(betweenness(as.undirected(allcontact.ig))))


plot(simplify(at.least.monthly),
     #vertex.label = NA,
     vertex.label.dist = 1,
     edge.arrow.size = .2,
     vertex.color = factor(attr.monthly$degree),
     vertex.size = 3 + sqrt(betweenness(as.undirected(at.least.monthly))))
