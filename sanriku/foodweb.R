library(tidyverse)
library(igraph)
library(ggraph)
library(ggrepel)
library(grid)

#--------------------------
# 1. データ読み込み
#--------------------------
net <- read.csv(
  "/Users/Yuki/Library/CloudStorage/Dropbox/isodata/sanriku/07/webtest.csv",
  fileEncoding = "CP932",
  header = TRUE,
  row.names = 1,
  check.names = FALSE
)

# 行名を列にして long化
net_long <- net %>%
  tibble::rownames_to_column("consumer") %>%
  pivot_longer(
    cols = -consumer,
    names_to = "resource",
    values_to = "weight"
  ) %>%
  filter(!is.na(weight), weight > 0)

# resource -> consumer の有向重み付きグラフ
g <- graph_from_data_frame(
  d = net_long %>% dplyr::rename(from = resource, to = consumer),
  directed = TRUE
)

E(g)$weight <- net_long$weight

#--------------------------
# 2. ノード情報
#--------------------------
resource_nodes <- colnames(net)
consumer_nodes <- rownames(net)

nodes <- tibble(name = V(g)$name) %>%
  mutate(
    type = case_when(
      name %in% resource_nodes ~ "resource",
      name %in% consumer_nodes ~ "consumer",
      TRUE ~ "other"
    )
  )

# 座標列を先に作る
nodes <- nodes %>%
  mutate(
    x = NA_real_,
    y = NA_real_
  )

# consumer を上段に配置
n_cons <- sum(nodes$type == "consumer")
nodes$x[nodes$type == "consumer"] <- seq_len(n_cons)
nodes$y[nodes$type == "consumer"] <- 2

# resource を下段に配置
n_res <- sum(nodes$type == "resource")
nodes$x[nodes$type == "resource"] <- seq(
  1,
  n_cons,
  length.out = n_res
)
nodes$y[nodes$type == "resource"] <- 1

# igraph の頂点順に並べる
nodes <- nodes %>%
  slice(match(V(g)$name, name))

#--------------------------
# 3. 描画
#--------------------------
p <- ggraph(g, layout = "manual", x = nodes$x, y = nodes$y) +
  geom_edge_link(
    aes(width = weight),
    alpha = 0.35,
    colour = "grey50",
    end_cap = circle(3, "mm"),
    start_cap = circle(3, "mm"),
    arrow = arrow(length = unit(2.5, "mm"), type = "closed")
  ) +
  scale_edge_width(range = c(0.3, 3), guide = "none") +
  
  geom_node_point(
    data = nodes %>% filter(type == "consumer"),
    aes(x = x, y = y),
    inherit.aes = FALSE,
    size = 5,
    shape = 21,
    fill = "orange",
    colour = "black",
    stroke = 0.6
  ) +
  
  geom_node_point(
    data = nodes %>% filter(type == "resource"),
    aes(x = x, y = y),
    inherit.aes = FALSE,
    size = 9,
    shape = 21,
    fill = "gold",
    colour = "black",
    stroke = 0.8
  ) +
  
  geom_text_repel(
    data = nodes %>% filter(type == "consumer"),
    aes(x = x, y = y, label = name),
    inherit.aes = FALSE,
    size = 3.5,
    family = "Hiragino Sans",
    direction = "y",
    nudge_y = 0.18,
    min.segment.length = 0,
    segment.alpha = 0.5,
    box.padding = 0.2,
    point.padding = 0.15,
    max.overlaps = Inf
  ) +
  
  geom_text_repel(
    data = nodes %>% filter(type == "resource"),
    aes(x = x, y = y, label = name),
    inherit.aes = FALSE,
    size = 5,
    family = "Hiragino Sans",
    direction = "y",
    nudge_y = -0.15,
    min.segment.length = 0,
    segment.alpha = 0.5,
    box.padding = 0.25,
    point.padding = 0.2,
    max.overlaps = Inf
  ) +
  
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    plot.margin = margin(20, 80, 40, 80),
    text = element_text(family = "Hiragino Sans")
  )

p

ggsave("foodweb.png", p, width = 16, height = 8, dpi = 300)




# 縦長に ---------------------------------------------------------------------
library(tidyverse)
library(igraph)
library(ggraph)
library(ggrepel)
library(grid)
library(showtext)

#--------------------------
# 日本語フォント
#--------------------------
font_add("jp", "/System/Library/Fonts/ヒラギノ角ゴシック W3.ttc")
showtext_auto()

#--------------------------
# データ読み込み
#--------------------------
net <- read.csv(
  "/Users/Yuki/Library/CloudStorage/Dropbox/isodata/sanriku/07/webtest.csv",
  fileEncoding = "CP932",
  header = TRUE,
  check.names = FALSE
)

# UTF-8化
net$group <- enc2utf8(net$group)
net$consumer <- enc2utf8(net$consumer)
colnames(net) <- enc2utf8(colnames(net))

#--------------------------
# long形式
#--------------------------
net_long <- net %>%
  pivot_longer(
    cols = c("SOM","藻類","植食者","固着動物"),
    names_to = "resource",
    values_to = "weight"
  ) %>%
  filter(weight > 0)

g <- graph_from_data_frame(
  net_long %>% select(from = resource, to = consumer, weight),
  directed = TRUE
)

E(g)$weight <- net_long$weight

#--------------------------
# ノード情報
#--------------------------
resource_nodes <- c("SOM","藻類","植食者","固着動物")

nodes <- tibble(name = V(g)$name) %>%
  left_join(net %>% select(consumer, group),
            by = c("name" = "consumer")) %>%
  mutate(
    type = if_else(name %in% resource_nodes, "resource","consumer")
  )

# group順
group_order <- c("藻類","固着動物","植食者","肉食者")

consumer_nodes <- nodes %>%
  filter(type=="consumer") %>%
  mutate(group = factor(group, levels = group_order)) %>%
  arrange(group,name)

gap <- 2
y_vals <- c()
current_y <- 1

for(gp in group_order){
  
  n <- sum(consumer_nodes$group==gp, na.rm=TRUE)
  
  if(n>0){
    
    y_vals <- c(y_vals, seq(current_y,length.out=n))
    current_y <- current_y + n + gap
    
  }
}

consumer_nodes$y <- y_vals
consumer_nodes$x <- 1

# resource配置
y_min <- min(consumer_nodes$y)
y_max <- max(consumer_nodes$y)

resource_df <- tibble(
  name = resource_nodes,
  type = "resource",
  group = NA,
  x = 2.4,
  y = seq(y_min,y_max,length.out=length(resource_nodes))
)

nodes2 <- bind_rows(
  consumer_nodes %>% select(name,type,group,x,y),
  resource_df
)

nodes2 <- nodes2 %>%
  slice(match(V(g)$name,name))

#--------------------------
# 作図
#--------------------------
p <- ggraph(g, layout="manual", x=nodes2$x, y=nodes2$y) +
  
  geom_edge_link(
    aes(width=weight),
    alpha=0.35,
    colour="grey50",
    curvature=0.08,
    start_cap = circle(2.5,"mm"),
    end_cap   = circle(2.5,"mm"),
    arrow=arrow(length=unit(2.8,"mm"), type="closed")
  ) +
  
  scale_edge_width(range=c(0.3,3), guide="none") +
  
  geom_node_point(
    data=nodes2 %>% filter(type=="consumer"),
    aes(x=x,y=y),
    inherit.aes=FALSE,
    size=5,
    shape=21,
    fill="orange",
    colour="black"
  ) +
  
  geom_node_point(
    data=nodes2 %>% filter(type=="resource"),
    aes(x=x,y=y),
    inherit.aes=FALSE,
    size=9,
    shape=21,
    fill="gold",
    colour="black"
  ) +
  
  geom_text_repel(
    data=nodes2 %>% filter(type=="consumer"),
    aes(x=x,y=y,label=name),
    inherit.aes=FALSE,
    family="jp",
    size=15,
    direction="y",
    nudge_x=-0.12,
    min.segment.length=0,
    max.overlaps=Inf
  ) +
  
  geom_text_repel(
    data=nodes2 %>% filter(type=="resource"),
    aes(x=x,y=y,label=name),
    inherit.aes=FALSE,
    family="jp",
    size=15,
    direction="y",
    nudge_x=0.12,
    min.segment.length=0,
    max.overlaps=Inf
  ) +
  
  theme_void() +
  theme(
    text = element_text(family="jp")
  )

p

ggsave("foodweb.png", p, width = 10, height = 12, dpi = 300)
