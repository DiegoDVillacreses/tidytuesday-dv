#data.table instead of dplyr
library(data.table)
setDTthreads(8)
#Awesome individual tidys
library(magrittr)
library(stringr)
library(purrr)
library(forcats)

#For graphs
library(ggplot2)
library(ggthemes)
library(waffle) 
library(magick) #make gifs
  #you need to install ImageMagick: http://www.imagemagick.org/script/download.php

#To import tibbles from #tidytuesday
library(tidytuesdayR)

#### Import data
tuesdata <- tidytuesdayR::tt_load(2019, week = 47)
dt =  tuesdata[["BOTY-votes-2019"]] %>% data.table()

#### Function that return most and least voted birds. 
  #Also return a data.table summarizing total votes for votes_1 to votes_5
first_c = function(v.x, d., votes){
  prop.x = d.[, .N , by  = v.x] %>% .[(get(v.x) %>% is.na)==F] %>% 
    .[, prop := N*100/sum(N)] %>% .[order(-N)]
  
  prop.x.first = prop.x[1, prop]
  first_name = prop.x[1,get(v.x)]
  min_val = min(prop.x[,N])
  last_names = prop.x[N== min_val,get(v.x)]
  
  #For ggplot
  top5 = prop.x[1:5,get(v.x)]
  #From wide to long
  dt_long = d.[, mget(votes)] %>% melt(., measure =votes,
                                       value.factor=T,na.rm = T)
  #Total votes by 
  dt_long.res = dt_long[value %in%top5, .N, by =c("variable", "value") ]
  
  #Return results
  res = list(prop = prop.x.first, last_names = last_names,
             first_name=first_name, prop.x = prop.x, 
             dtL.res=dt_long.res)
  return(res)
}


#### Loop to get votes under each IRV voting system 
## Objects for first iteration
#counter
i = 0
#character vectors with names of all possible votes: vote_1 to vote_5
all_votes = names(dt) %>% str_subset("vote_") 
#Initial state: most and least voted birds
t1 = first_c("vote_1", dt,all_votes)
#List to save summary table by bread and vote rank
s.bread.rank = list()

## While loop conditional to >50 percetage of votes
while(t1$prop<=50){
  i = i+1
  #Most and least voted birds
  t1 = first_c("vote_1", dt,all_votes)
  
  #This work like Lego: all rows whom voted for the least voted bird loss their first column
  # then, after their final column a column full of NAs is added
  tmp = dt[vote_1 %in% t1$last_names, mget(all_votes)]
  tmp[, 1:=NULL][, last:=NA]
  dt[vote_1 %in% t1$last_names, (all_votes):=tmp]
  
  #Considering that the least voted bird is no longer playing, I replace with NAs all the
  # votes corresponding to that bird
  which_erase = dt[,mget(all_votes)] %>% 
    map(function(x) x %in% t1$last_names) %>%
    do.call(cbind,.) 
  i2 = 0
  for (x in all_votes){
    i2 = i2+1
    dt[which_erase[,i2]==T,(x):=NA]
  }  #Perhaps this loop is easiest to understand than a map2 (?)
  
  #If any row has a valid vote after an NA I'll use the "Lego" approach
  tmp = dt[is.na(vote_1)==T & is.na(vote_1)==F, mget(all_votes)]
  tmp[, 1:=NULL][, last:=NA]
  dt[is.na(vote_1)==T & is.na(vote_1)==F, (all_votes):=tmp]
  
  # Save each state to graph
  s.bread.rank[[i]] = t1$dtL.res
  
  #Just looking some results after each iteration
  t1[c(1,3)] %>% do.call(cbind,.) %>% print
  t1[[2]] %>% paste("Last names: ",.,collapse = " - ") %>% print
}
i #After how many iterations did we find a winner?



##### Graphs
### Functions
# Function to get a "waffle" graph from top 5 
  # Partialy based on Based on "Edgar-Zamora/My-Work/#TidyTuesday" work
waffle_graph = function(top5_breakdown){
  g1 = ggplot(top5_breakdown, aes(fill = fct_rev(vote_rank), values = rev(n)),par(xaxs = "i" , yaxs = "i")) +
    geom_waffle(color = "white", size = .3, flip = F) +
    facet_wrap(~rev(bird_breed), nrow = 1, strip.position = "top",
               labeller = label_wrap_gen(width = 10)) + 
    scale_x_discrete( ) +
    scale_y_continuous(labels = function(x) x * 100 ,
                       expand = c(0,0),
                       breaks = waiver() ) +
    scale_fill_tableau(name = "1 sq = 100 votes",
                       labels = c("Vote 1", "Vote 2", "Vote 3", "Vote 4", "Vote 5"),
                       palette = "Classic Color Blind") +
    theme(panel.grid = element_blank(),
          plot.tag = element_text(size = 12),
          plot.tag.position = c(0.89,.68),
          strip.background = element_blank(),
          plot.caption = element_text(size = 9,hjust=0))
  return(g1)
}
#A little data wraggling to simplify waffle_graph usage 
  # I also add captions and percentages by bread
waffle_graph_perc = function(i.x){
  tmp = s.bread.rank[[i.x]] %>% data.table::setnames(c("vote_rank","bird_breed", "n")) %>% 
    .[, n:=n/100]
  tmp2 = tmp[vote_rank=="vote_1"][, perc := (n*100/sum(n)) %>% round(1)] %>% 
    .[, .(bird_breed, perc)]
  tmp %<>% merge(., tmp2, by = "bird_breed") 
  tmp[, bird_breed := paste0(bird_breed, " (",perc,"%)")]
  tmp[,perc:=NULL]
  g.x = waffle_graph(tmp) + 
    labs(tag = paste0("Iteration: ",i.x), 
         caption  = "Author: Diego Villacreses \nBased on: Edgar-Zamora/My-Work/#TidyTuesday")
  return(g.x)
}

## Create and save graphs for some (interesting) iterations
interest_i=c(1,20,30,40,50,61,68,72,75,77,78,79,80)
g.l = lapply(interest_i, waffle_graph_perc)
i.0 = 0
for (i.x in interest_i){
  i.0 = i.0 +1
  name = paste0("g_i", i.x, ".png")
  ggsave(plot = g.l[[i.0]],filename= name,scale = 1, 
         dpi = 300)
}

## Take those graphs and make a single gif
all_images = list.files(pattern = "*.png", full.names = T) %>% 
              map(image_read)

all_images.j = all_images %>% image_join()
all_images.an = all_images.j %>%   image_animate(fps=2)

all_images.an %>% image_write("summaryIRV_iterations.gif")
