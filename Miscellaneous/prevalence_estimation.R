library(simstudy)
library(tidyverse)
library(progressr)
library(dtplyr)
devtools::load_all("C:/Users/gmulier/Documents/R Packages/modmarg/")

handlers(handler_progress(
  format = " [:bar] :current/:total :percent",
  width = 100, complete = "-", incomplete = " ", current = ">"
))
NPat <- 500
NRep <- 10000

# Pas d'intéraction

## Les références simulées
DataDefOr <- defData(varname = "X1", formula = "30", variance = 10, dist = "normal") %>% 
  defData(varname = "T1", formula = "'A'", dist = "nonrandom") %>% 
  defData(varname = "Y1", formula = "-1 - .05 * X1 + 1.5 * (T1 == 'B')", dist = "binary", link = "logit") %>% 
  defData(varname = "T2", formula = "'B'", dist = "nonrandom") %>% 
  defData(varname = "Y2", formula = "-1 - .05 * X1 + 1.5 * (T2 == 'B')", dist = "binary", link = "logit")
set.seed(121221)
TabDonnees <- genData(NPat * NRep, DataDefOr)
TabDonnees$essai <- rep(seq_len(NRep), each = NPat)
TabDonnees %>% 
  lazy_dt() %>% 
  group_by(essai) %>% 
  summarise(moy_A = mean(Y1), moy_B = mean(Y2), .groups = "drop") %>% 
  summarise(moy_A = sprintf("%1.3f [%1.3f;%1.3f]", mean(moy_A), quantile(moy_A, prob = .025), quantile(moy_A, prob = .975)), 
            moy_B = sprintf("%1.3f [%1.3f;%1.3f]", mean(moy_B), quantile(moy_B, prob = .025), quantile(moy_B, prob = .975))) %>% 
  as_tibble()

## Application selon la méthode delta
DataDef <- defData(varname = "X1", formula = "30", variance = 10, dist = "normal") %>% 
  defData(varname = "TT", formula = "..prop;1-..prop", variance = "A;B", dist = "categorical") %>% 
  defData(varname = "Y", formula = "-1 - .05 * X1 + 1.5 * (TT == 'B')", dist = "binary", link = "logit")
set.seed(121221)
TabDonneesDelta <- map(seq(.2, .8, .2),
                       \(prop) {
                         genData(n = NPat * NRep, dtDefs = DataDef, id = "id")
                       })
TabDonneesDelta <- TabDonneesDelta %>% map(\(x) mutate(x, essai = rep(seq_len(NRep), each = NPat)) %>% as_tibble())
with_progress({
  PBarre <- progressor(steps = NRep * 4)
  TabDonneesDelta <- seq(.2, .8, .2) %>% 
    map(function(proportion) {
      .Data <- TabDonneesDelta[[match(proportion, seq(.2, .8, .2))]]
      map_dfr(unique(.Data$essai), \(.x){
        PBarre()
        Modele <- glm(Y ~ TT + X1, data = filter(.Data, essai == .x), family = binomial(link = "logit"))
        marg(Modele, var_interest = "TT") %>% bind_cols(prop = proportion)
      })
    })
})
TabDonneesDelta %>% 
  map_dfr(\(proportion) {
    proportion %>% 
      group_by(Label) %>% 
      summarise(moyenne = mean(Margin),
                ic = sprintf("%1.3f [%1.3f;%1.3f]", mean(Margin), quantile(Margin, prob = .025), quantile(Margin, prob = .975)),
                proportion = mean(prop)) %>% 
      ungroup()
  })
# It works !!!
# Estimations are close to the simulated truth and doesn't seem to vary with proportion of TT
# La variabilité semble être plus élevée, ici dans le cas où P(Y) est petit



# Intéraction entre la variable et le ttt

## Les références simulées
DataDefOr <- defData(varname = "X1", formula = "30", variance = 10, dist = "normal") %>% 
  defData(varname = "T1", formula = "'A'", dist = "nonrandom") %>% 
  defData(varname = "Y1", formula = ".2 - .05 * X1 + .5 * (T1 == 'B') + .25 * (T1 == 'B') * X1", dist = "binary", link = "logit") %>% 
  defData(varname = "T2", formula = "'B'", dist = "nonrandom") %>% 
  defData(varname = "Y2", formula = ".2 - .05 * X1 + .5 * (T2 == 'B') + .25 * (T2 == 'B') * X1", dist = "binary", link = "logit")
set.seed(121221)
TabDonnees2 <- genData(NPat * NRep, DataDefOr)
TabDonnees2$essai <- rep(seq_len(NRep), each = NPat)
TabDonnees2 %>% 
  lazy_dt() %>% 
  group_by(essai) %>% 
  summarise(moy_A = mean(Y1), moy_B = mean(Y2), .groups = "drop") %>% 
  summarise(moy_A = sprintf("%1.3f [%1.3f;%1.3f]", mean(moy_A), quantile(moy_A, prob = .025), quantile(moy_A, prob = .975)), 
            moy_B = sprintf("%1.3f [%1.3f;%1.3f]", mean(moy_B), quantile(moy_B, prob = .025), quantile(moy_B, prob = .975))) %>% 
  as_tibble()

## Application selon la méthode delta
DataDef <- defData(varname = "X1", formula = "30", variance = 10, dist = "normal") %>% 
  defData(varname = "TT", formula = "..prop;1-..prop", variance = "A;B", dist = "categorical") %>% 
  defData(varname = "Y", formula = ".2 - .05 * X1 + .5 * (TT == 'B') + .25 * (TT == 'B') * X1", dist = "binary", link = "logit")
set.seed(121221)
TabDonneesDelta2 <- map(seq(.2, .8, .2),
                        \(prop) {
                          genData(n = NPat * NRep, dtDefs = DataDef, id = "id")
                        })
TabDonneesDelta_2 <- TabDonneesDelta2 %>% map(\(x) mutate(x, essai = rep(seq_len(NRep), each = NPat)) %>% as_tibble())
with_progress({
  PBarre <- progressor(steps = NRep * 4)
  TabDonneesDelta_2 <- seq(.2, .8, .2) %>% 
    map(function(proportion) {
      .Data <- TabDonneesDelta_2[[match(proportion, seq(.2, .8, .2))]]
      map_dfr(unique(.Data$essai), \(.x){
        PBarre()
        Modele <- glm(Y ~ TT + X1, data = filter(.Data, essai == .x), family = binomial(link = "logit"))
        marg(Modele, var_interest = "TT") %>% bind_cols(prop = proportion)
      })
    })
})
TabDonneesDelta_2 %>% 
  map_dfr(\(proportion) {
    proportion %>% 
      group_by(Label) %>% 
      summarise(moyenne = mean(Margin),
                ic = sprintf("%1.3f [%1.3f;%1.3f]", mean(Margin), quantile(Margin, prob = .025), quantile(Margin, prob = .975)),
                proportion = mean(prop)) %>% 
      ungroup()
  })
set.seed(121221)
TabDonneesDelta3 <- map(seq(.2, .8, .2),
                        \(prop) {
                          genData(n = NPat * NRep, dtDefs = DataDef, id = "id")
                        })
TabDonneesDelta_3 <- TabDonneesDelta3 %>% map(\(x) mutate(x, essai = rep(seq_len(NRep), each = NPat)) %>% as_tibble())
with_progress({
  PBarre <- progressor(steps = NRep * 4)
  TabDonneesDelta_3 <- seq(.2, .8, .2) %>% 
    map(function(proportion) {
      .Data <- TabDonneesDelta_3[[match(proportion, seq(.2, .8, .2))]]
      map_dfr(unique(.Data$essai), \(.x){
        PBarre()
        Modele <- glm(Y ~ TT + X1 + TT:X1, data = filter(.Data, essai == .x), family = binomial(link = "logit"))
        marg(Modele, var_interest = "TT") %>% bind_cols(prop = proportion)
      })
    })
})
TabDonneesDelta_3 %>% 
  map_dfr(\(proportion) {
    proportion %>% 
      group_by(Label) %>% 
      summarise(moyenne = mean(Margin),
                ic = sprintf("%1.3f [%1.3f;%1.3f]", mean(Margin), quantile(Margin, prob = .025), quantile(Margin, prob = .975)),
                proportion = mean(prop)) %>% 
      ungroup()
  })

save(TabDonnees, TabDonnees2, TabDonneesDelta, TabDonneesDelta2, TabDonneesDelta_2, TabDonneesDelta_3, 
     file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/donnees_deltamethod.RData"))
