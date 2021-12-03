#Author: Sharath Kumar Kademaru
# Dataset source: https://www.kaggle.com/henslersoftware/19560-indian-takeaway-orders (Local Indian Restaurant Takeaway Orders - London, UK) 
#Refined Data source: https://github.com/kademarsharath/restaurant_apriori_viz/blob/main/restaurant-1-orders.csv
#Let's import all the required libraries

library("readxl")
library("openxlsx")
library("data.table")
library("arules")
library("magrittr") # package installations are only needed the first time you use it
library("dplyr")
library("stringr")
library("tidyr")
library("tidyverse")
library("visNetwork")

# We need to read the transactions data in the Restuarant Shop
retail <- read.csv("Desktop/restaurant-1-orders.csv")
colnames(retail) <- tolower(colnames(retail))




rm(df)


#I'm sub-setting the columns and mutating to make changes in columns like Description

head(retail)


my_list1 <- unique(retail$city)
count <- 0
offset <-0
for (p in my_list1) {
print(p)
#df <- filter (df1,  city ==  p )
#filter for each city or branch
retail1 <- retail[retail$city == p,]
df <- retail1 %>% select(invoice, description,city)%>%
  mutate(description = str_trim(description,side = "both")) %>% 
  mutate(invoiceno = factor(invoice), description = str_replace_all(description, "[']", replacement = "")) %>% 
  mutate(description = tolower(str_replace_all(description,pattern = "[ ]", replacement = "_")))

#function to convert each order to itemset
prep_data <- function(x) {
  y <- data.frame()
  for (i in 1:n_distinct(df$invoiceno)) {
    x <- df %>% filter(invoiceno == levels(invoiceno)[i]) %>% t() %>% as.data.frame() %>% 
      slice(2) %>% mutate(invoiceno = levels(df$invoiceno)[i]) %>% select(invoiceno, 
                                                                          everything())
    colnames(x) <- c("invoiceno", paste0("item_", 1:(ncol(x) - 1)))
    #print(i)
    y <- list(y, x) %>% rbindlist(fill = T)
  }
  return(y)
}

df_prep <- prep_data()
write.csv(df_prep %>% select(-invoiceno), "transaction_retail.csv", row.names = F)
trans_retail <- read.transactions("transaction_retail.csv", sep = ",", 
                                  header = T)


LIST(head(trans_retail, 4))  #print sample to verify

#execute the rules using apriori
rules <- apriori(trans_retail, parameter = list(supp = 0.06, conf = 0.2))
subrules <- head(rules, n = 300, by = "lift")
rm(df_rules)

df_rules <- DATAFRAME(subrules) %>% rowid_to_column("rules") %>% mutate(rules = paste("Rules", 
                                                                                      rules), RHS = str_remove_all(string = RHS, pattern = "[{}]"))
#assign each city and append to df



rm(df_items)
df_items <- df_rules %>% mutate(LHS = str_remove_all(string = LHS, pattern = "[{}]")) %>% 
  separate(col = LHS, into = c(paste0("item_", 1:3)), sep = ",") %>% pivot_longer(cols = c(item_1, 
                                                                                           item_2, item_3), names_to = "antecedent", values_to = "item") %>% select(rules, 
                                                                                                                                                                 antecedent, item, RHS, everything()) %>% filter(is.na(item) == F)

#df_items <- df_items%>% filter(item != '')
#df_items <-na.omit(df_items) 

head(df_rules)
if (p == 'London'){
  col = "lightblue"
}
else if (p == 'Kensington'){
  col = "yellow"
}
else if (p == 'Islington'){
  col ="steelblue"
}
else if (p == 'Hackney'){
  col = "brown"
}
else if (p == 'Camden'){
  col ="purple"
}
else{
  col ="gold"
  
}

#Creating the recursive connection of rules with products
nodes <- data.frame(name = unique(c(df_items$item, df_items$RHS, df_items$rules))) %>% 
  rowid_to_column("id") %>%  mutate( id = c(id +offset))%>% 
  mutate(group = ifelse(str_detect(name, "Rules"), "A", "B"), label = name, 
         value = c(rep(NA, n_distinct(c(df_items$item, df_items$RHS))),df_rules$lift), 
         support = c(rep(NA, n_distinct(c(df_items$item, df_items$RHS))),df_rules$support), 
         confidence = c(rep(NA, n_distinct(c(df_items$item, df_items$RHS))),df_rules$confidence), 
         shape = ifelse(group == "A", "circle", "box"), 
         
         color = ifelse(group == "A", col, "lightgreen"), 
         #color =col,
         title = ifelse(test = group == "A", 
                        yes = paste(name,  "<br> Lift:", round(value, 2), "<br> Confidence:", 
                                    round(confidence, 2), "<br> Support:", round(support, 2)
                        ), 
                        no = as.character(name)
         )
  )

#assigning the city for each iteration
nodes <- nodes %>%mutate(city = c(p))

#Making the direction between nodes using edges
edges <- data.frame(from = df_items$item, to = df_items$rules, width = df_items$lift*4) %>% 
  bind_rows(data.frame(from = df_rules$rules, 
                       to = df_rules$RHS ,width = df_rules$lift*4)) %>% 
  left_join(nodes, by = c(from = "name")) %>% select(id, to, width) %>% 
  rename(from = id) %>% 
  left_join(nodes, by = c(to = "name")) %>% 
  select(from,id, width) %>% rename(to = id)

#For first iteration intialize the final node with the first one
if (count > 0){
  final_nodes1 <- rbind(final_nodes, nodes)
  rm(final_nodes)
  final_nodes <- final_nodes1
  
  final_edges1 <- rbind(final_edges, edges)
  rm(final_edges)
  final_edges <- final_edges1
  #break
  
}
#append nodes for subsequent iteration of loop
else{
  rm(final_nodes)
  final_nodes <- nodes
  
  rm(final_edges)
  final_edges <- edges
  
  
}
count<-count+1
print("Here is the count:")

print(count(final_nodes))
offset <-count(final_nodes)$n
}

#This is the heart of the program. We are using the nodes and edges that we;ve develooped in the visnetwork
visNetwork(nodes = final_nodes, edges = final_edges, height = "500px", width = "100%") %>% 
  visEdges(arrows = "to", selectionWidth = 1 , width = 200 ) %>% 
  visOptions(highlightNearest = T) %>% visInteraction(tooltipStyle = "position: fixed; visibility: hidden; padding: 5px; white-space: nowrap;
    font-size: 18px; color: black; background-color: white; border-color: orange")%>%
  visIgraphLayout() %>% 
  visClusteringByGroup(groups = unique(nodes$city),label = "Cluster on group : ",)%>%
  visLegend(addNodes = list(
    list(label = "London",  color = "lightblue"),
    list(label = "Kensington",   color = "yellow"),
    list(label = "Islington", color = "steelblue"),
    list(label = "Hackney", color = "brown"),
    list(label = "Camden",  color = "purple"),
    list(label = "Haringey", color = "gold")),useGroups = FALSE)




  