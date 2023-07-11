library(plyr)
library(tidyverse)
library(stringr)
library(rvest)
library(baseballr)
library(discordr)
library(emojifont)
library(janitor)
library(tidyr)

# OPTIONAL create a connection and webhook to a discord server if you want to receive updates
conn_obj <- create_discord_connection(webhook = 'link-to-your-webhook', 
                                      username = paste('name-of-your-bot', emoji('robot')), set_default = TRUE)

# Send message alerting you the scrape has initiated
send_webhook_message(paste(emoji('rotating_light'),"Frontier League website scrape initiated on",format(Sys.Date(), "%a %B %d, %Y"), "at", format(Sys.time(), "%d %X") ))

# Scrape the Frontier League standings
standings23<- read_html("https://frontierleague.com/sports/bsb/2022-23/standings")

tables<- standings23 %>% html_table(fill = TRUE)

standings23<-bind_rows(tables[[1]] %>% select(-GB) ) %>% # the GB column changes daily unfortunately. sometimes the site has division GB, other days it shows league GB
  # I change the name of the Windy City Thunderbolts in order to match up correctly with the Yakkertech data
  mutate(Team = gsub('ThunderBolts', "Thunderbolts",Team)) %>%
  arrange(Team) %>%
  # OPTIONAL I added a 'SEASON' column specific to my dashboard I built, since the dashboard also has 2022 data
  mutate(SEASON = 2023)  

{#  SCRAPING  # -------------------------
  
  url_1<- "https://frontierleague.com"
  
  # list of team names that are in the URLs
  front_teams <- c('schaumburgboomers', 'jolietslammers', 'windycitythunderbolts', 'gatewaygrizzlies', 'lakeeriecrushers', 'evansvilleotters',
                   'florenceyalls', 'washingtonwildthings', 'quebeccapitales', 'empirestategreys', 'newjerseyjackals', 'troisrivieresaigles',
                   'tricityvalleycats', 'sussexcountyminers','ottawatitans', 'newyorkboulders')
  
  # list of team names
  front_team_names <- c('Schaumburg Boomers', 'Joliet Slammers', 'Windy City Thunderbolts', 'Gateway Grizzlies', 'Lake Erie Crushers', 'Evansville Otters',
                        "Florence Y'alls", 'Washington Wild Things', 'Quebec Capitales', 'Empire State Greys', 'New Jersey Jackals', 'Trois-Rivieres Aigles',
                        'Tri-City ValleyCats', 'Sussex County Miners','Ottawa Titans', 'New York Boulders')
  
  # Create empty dataframes
  frontier_hitters <- data.frame()
  
  frontier_pitchers <- data.frame()
  
  front_rosters <- data.frame()
  
  # ROSTERS & HITTING STATS----
  # for each team in front_teams, execute the following code
  for (i in 1:length(front_teams)) {
    roster_url <- paste0('https://frontierleague.com/sports/bsb/2022-23/teams/', front_teams[i], '?view=lineup')
    roster <- read_html(roster_url)
    tables <- roster %>% html_table(fill = TRUE)
    
    # "urls" is to be able to get each players individual URL, similar to the way the baseballr library has that function
    urls<- as.data.frame(roster %>%
                           html_nodes("table") %>%
                           html_nodes("td") %>% 
                           html_nodes("a") %>% 
                           html_attr("href") ) %>% 
      rename(urls = 'roster %>% html_nodes(\"table\") %>% html_nodes(\"td\") %>% html_nodes(\"a\") %>% html_attr(\"href\")') %>%
      filter(grepl('/players/', urls)) %>%
      distinct(urls, .keep_all = TRUE)  
    
    # create a df of a team's roster from the roster page
    team_roster <- tables[[4]] %>%
      mutate(url= paste0(url_1,urls$urls)) %>%
      mutate(TEAM = front_team_names[i])
    
    # bind the rows from the team roster to the entire frontier league roster
    front_rosters <-bind_rows(front_rosters, team_roster)
    
    # ---
    # now we need to do the same with hitting stats.
    # I mutated G column because in tables[[6]], it read as.character instead of numeric
    team_hit_df <- full_join( tables[[5]] %>% mutate(g =as.numeric(g)),tables[[6]] %>% mutate(g =as.numeric(g)), by = c('#', "Name", "Yr", "Pos", "g")) %>%
      rename_all(., .funs = toupper)%>%
      filter(`#` != 'NA') %>%
      mutate(TEAM = front_team_names[i] ) %>%
      mutate_at(vars(5:29), ~as.numeric(gsub("[^0-9.]+", "", .)))
    
    # bind the rows from the team hitters to the entire frontier league hitters
    frontier_hitters <- bind_rows(frontier_hitters, team_hit_df)
  }
  
  # once every team's roster is pulled and compiled into one df, we do some cleaning to it.
  front_rosters <- front_rosters  %>%
    janitor::clean_names(case = "all_caps") %>%
    relocate(TEAM, .before = 1) %>%
    relocate(c(BATS,THROWS), .before = `YEAR`) %>%
    rename(`#` = 'NUMBER',
           `YR` = 'YEAR') %>%
    dplyr::select(-DOB) %>%
    mutate(SEASON = 2023)
  
  # once every team's hitters is pulled and compiled into one df, we do some cleaning to it.
  
  frontier_h <- frontier_hitters %>%
    relocate(TEAM, .before = `#`) %>%
    relocate(`#`, .before = `YR`) %>%
    mutate_at(vars(2:6), ~ str_replace(., "No players meet the minimum", "") )  %>% # needed to do this at the beginning of the season when some teams played and others did not yet
    # remove the totals and opponent rows
    filter(!NAME %in% c('Opponent', 'Totals', 'TOTALS') ) %>%
    # reomve the extra spaces between first and last name
    mutate(NAME = str_trim(str_squish(NAME))) %>%
    # change all the stat columns to numeric, and then replace them with 0
    mutate_at(c(6:30), as.numeric) %>% 
    mutate_if(is.numeric, ~replace_na(., 0))
  
  }

# pull the weighted values from te baseballr library so we can create estimated* advanced metrics
weights <- baseballr::fg_guts() %>%
  filter(season == 2022)

# ----------------------------------     FRONTIER LEAGUE TEAM TOTALS      ------------------------------------------
#frontier league team totals
# Here we scrape the team totals
FL_team_hit<-  read_html('https://frontierleague.com/sports/bsb/2022-23/teams?sort=&r=0&pos=h')
tables <- FL_team_hit %>% html_table(fill = TRUE)

# Pitching stats - team totals
FL_team_pitch <- tables[[3]] %>%
  janitor::clean_names(case = "all_caps") %>%
  filter(RK != "TOTALS") %>%
  # convert IP as 42.2 to 42 and 2/3 or 42.67
  separate(IP, into=c("Inn", "pInn"), sep= "\\.") %>%
  mutate_at(c('Inn', 'pInn') ,as.numeric) %>%
  replace(is.na(.), 0) %>%
  mutate(Outs = `Inn` * 3 + `pInn`,
         IP = round(`Outs`/3 ,2))   %>%
  dplyr::mutate(`SEASON` = 2023) %>%
  mutate_at(c(3:15), as.numeric) %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  mutate(SEASON = 2023) %>%
  adorn_totals('row')


# write.csv(FL_team_pitch, "C:/Users/tdmed/OneDrive/_Shiny/Coop2/FL_team_pitch23.csv", row.names = FALSE)
#------------
# FL team hitting stats
FL_team_hit<- full_join(tables[[1]] %>% dplyr::select(-Rk), tables[[2]] %>% dplyr::select(-Rk),
                        by = c("Name", "gp")) %>%
  filter(Name != "TOTALS") %>%
  rename_all(., .funs = toupper)%>%
  arrange(desc(H)) %>%
  mutate(RK = row_number()) %>%
  relocate(RK, .before = NAME) %>%
  filter(GP!='-') %>%
  mutate_all(~ifelse(. == "-",0,.)) %>%
  rename(TEAM = 'NAME') %>%
  mutate(TEAM = ifelse(TEAM == "-", "League Average", TEAM)   ) %>%
  mutate_at(c(3:27), as.numeric) %>% 
  adorn_totals("row") %>%
  mutate_if(is.numeric, ~replace_na(., 0))

# here is where we calculate all the fun stuff
FL_team_hit <- FL_team_hit %>%
  mutate(
    `1B` = (H - `2B`- `3B` - HR),
    AVG = as.numeric(formatC(formatC(round((H / AB), 3), format = 'f', digits = 3), format = 'f', digits = 3)),
    OBP = as.numeric(formatC(round(((H + BB + HBP) / PA) ,3), format = 'f', digits = 3)),
    SLG = as.numeric(formatC(round(((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)) / AB) ,3), format = 'f', digits = 3)),
    OPS = as.numeric(formatC(round((OBP + SLG),3),format = 'f', digits = 3)),
    #   wOBA formula per fangraphs = #((.69* uIBB)+(.719*HBP)+(.87*1B)+(1.217*2B)+(1.529*3B)+(1.94*HR))/(AB+uiBB+HBP+SF)
    wOBA= as.numeric(formatC(round((((weights$wHBP*HBP)+(BB*weights$wBB)+
                                       (weights$w1B*`1B`)+(weights$w2B*`2B`)+
                                       (weights$w3B*`3B`)+(weights$wHR*HR))/
                                      (AB+BB+HBP+SF))*1.035398 ,3), format = 'f', digits = 3)),
    `GO/FO` = round(GO/FO,1),
    `SB%` = formatC(round((((SB/(SB + CS)))*100),1),format = 'f', digits = 1),
    `SB%`= sub('NaN%','0',`SB%`),
    `SB%`= sub('NaN','0',`SB%`),
    `SB%` = as.numeric(`SB%`),
    wRAA = as.numeric(formatC(round((((wOBA - weights$lg_woba) / weights$woba_scale) * PA),2),format = 'f', digits=2)),
    RC = as.numeric(formatC(round(((((H+BB-CS)*
                                       ((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)+
                                           .26*(BB+HBP))+(.52*(SF+SH+SB))))) / 
                                     PA),2),format = 'f', digits = 2)),
    wRC = as.numeric(formatC(round(((((wOBA - weights$lg_woba) / weights$woba_scale) + 
                                       (weights$lg_r_pa))*PA),2),format = 'f', digits =2)),
    
    `wRC+` = as.numeric(formatC(round(((((((wRAA/PA)) + (weights$lg_r_pa) + 
                                            (weights$lg_r_pa-(1*weights$lg_r_pa))) / (weights$lg_r_pa))) * 100),0), 
                                format = 'f', digits = 0)),
    
    `BB%` = round((BB / PA),3)*100,
    `K%` = round((K / PA),3)*100,
    
    BattingRuns= round(wRAA +((weights$lg_r_pa -(.1*weights$lg_r_pa))*PA),2) ) %>%
  mutate_all(~ifelse(.=='NaN',0,.))


wrc_scale<- FL_team_hit$`wRC+`[nrow(FL_team_hit)]/100

lgwSB<- ((FL_team_hit$SB*weights$runSB)+(FL_team_hit$CS*weights$runCS))/(FL_team_hit$`1B` + FL_team_hit$BB + FL_team_hit$HBP)
lgwSB<- lgwSB[FL_team_hit$RK=='Total']

# (SB * runSB) + (CS * runCS) - (lgwSB * (1B + BB + HBP - IBB))


# RPW = 9*(MLB Runs Scored / MLB Innings Pitched)*1.5 + 3

RunsPerWin<-round((9*(FL_team_pitch$R[17] /FL_team_pitch$IP[17])*1.5)+3,2)

FL_team_hit<- FL_team_hit %>%
  mutate(
    wSB= round( ((SB * weights$runSB)+ (CS * weights$runCS))  -  (lgwSB*(`1B`+BB+HBP)),2),
    wSB = ifelse(wSB == 'NaN', 0, wSB) ,
    `wRC+`= round( `wRC+`/ wrc_scale ,0),
   # oWAR= round((BattingRuns + wSB)/RunsPerWin,2), ignore my failed attempt at some kind of offensive WAR
    ISO = SLG - AVG,
    BABIP = as.numeric(formatC(round(((H - HR) / (AB - HR - K + SF)),3),format = 'f', digits = 3))    ,
    BABIP = ifelse(BABIP =='NaN', 0, BABIP)) %>%
  mutate(SEASON = 2023)



{ # here we go through a lot of the same calculations, but instead of a team level its on a player level
  front_h<- frontier_h %>%
    mutate(NAME = str_squish(NAME)) %>%
    filter(NAME != "") %>%
    mutate(
      `2B` =as.numeric(gsub("-", "0", `2B`) ),
      `3B` =as.numeric(gsub("-", "0", `3B`) ),
      `HR` =as.numeric(gsub("-", "0", `HR`) ),
      `SB` =as.numeric(gsub("-", "0", `SB`) ),
      `CS` =as.numeric(gsub("-", "0", `CS`) ),
      `BB` =as.numeric(gsub("-", "0", `BB`) ),
      `K` =as.numeric(gsub("-", "0", `K`) ),
      `AVG` =as.numeric(gsub("-", "0", `AVG`) ),
      `SLG` =as.numeric(gsub("-", "0", `SLG`) ),
      `HBP` =as.numeric(gsub("-", "0", `HBP`) ),
      `SH` =as.numeric(gsub("-", "0", `SH`) ),
      `SF` =as.numeric(gsub("-", "0", `SF`) ),
      `HDP` =as.numeric(gsub("-", "0", `HDP`) ),
      `GO` =as.numeric(gsub("-", "0", `GO`) ),
      `FO` =as.numeric(gsub("-", "0", `FO`) ),
      `GO/FO` =as.numeric(gsub("-", "0", `GO/FO`) )     ) %>%
    adorn_totals("row") %>%
    mutate(NAME = ifelse(NAME == "-", "Team Totals", NAME)) %>%
    mutate(
      `1B` = (H - `2B`- `3B` - HR),
      AVG = as.numeric(formatC(formatC(round((H / AB), 3), format = 'f', digits = 3), format = 'f', digits = 3)),
      OBP = as.numeric(formatC(round(((H + BB + HBP) / PA) ,3), format = 'f', digits = 3)),
      SLG = as.numeric(formatC(round(((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)) / AB) ,3), format = 'f', digits = 3)),
      OPS = as.numeric(formatC(round((OBP + SLG),3),format = 'f', digits = 3)),
      # per FG  wOBA = #((.69* uIBB)+(.719*HBP)+(.87*1B)+(1.217*2B)+(1.529*3B)+(1.94*HR))/(AB+uiBB+HBP+SF)
      
      wOBA= as.numeric(formatC(round((((weights$wHBP*HBP)+(BB*weights$wBB)+
                                         (weights$w1B*`1B`)+(weights$w2B*`2B`)+
                                         (weights$w3B*`3B`)+(weights$wHR*HR))/
                                        (AB+BB+HBP+SF))*1.035398 ,3), format = 'f', digits = 3)),
      `SB%` = formatC(round((((SB/(SB + CS)))*100),1),format = 'f', digits = 1),
      `SB%`= sub('NaN%','',`SB%`),
      `SB%`= sub('NaN','',`SB%`),
      `SB%` = as.numeric(`SB%`),
      wRAA = as.numeric(formatC(round((((wOBA - weights$lg_woba) / weights$woba_scale) * PA),2),format = 'f', digits=2)),
      RC = as.numeric(formatC(round(((((H+BB-CS)*
                                         ((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)+
                                             .26*(BB+HBP))+(.52*(SF+SH+SB))))) / 
                                       PA),2),format = 'f', digits = 2)),
      wRC = as.numeric(formatC(round(((((wOBA - weights$lg_woba) / weights$woba_scale) + 
                                         (weights$lg_r_pa))*PA),2),format = 'f', digits =2)),
      
      `wRC+` = as.numeric(formatC(round(((((((wRAA/PA)) + (weights$lg_r_pa) + 
                                              (weights$lg_r_pa-(1*weights$lg_r_pa))) / (weights$lg_r_pa))) * 100),0), 
                                  format = 'f', digits = 0)),
      
      `BB%` = round((BB / PA),3)*100,
      `K%` = round((K / PA),3)*100,
      
      BattingRuns= round(wRAA +((weights$lg_r_pa -(.1*weights$lg_r_pa))*PA),2)    )
  
  
  wrc_scale<- front_h$`wRC+`[nrow(front_h)]/100
  
  lgwSB<- ((FL_team_hit$SB*weights$runSB)+(FL_team_hit$CS*weights$runCS))/(FL_team_hit$`1B` + FL_team_hit$BB + FL_team_hit$HBP)
  lgwSB<- lgwSB[FL_team_hit$RK=='Total']
  
  # (SB * runSB) + (CS * runCS) - (lgwSB * (1B + BB + HBP - IBB)) per fG
  
  
  # RPW = 9*(MLB Runs Scored / MLB Innings Pitched)*1.5 + 3 per FG
  
  RunsPerWin<-round((9*(FL_team_pitch$R[17] /FL_team_pitch$IP[17])*1.5)+3,2)
  
  front_h<- front_h %>%
    mutate(
      wSB= round( ((SB * weights$runSB)+ (CS * weights$runCS))  -  (lgwSB*(`1B`+BB+HBP)),2),
      `wRC+`= round( `wRC+`/ wrc_scale ,0),
      oWAR= round((BattingRuns + wSB)/RunsPerWin,2),
      ISO = SLG - AVG,
      BABIP = as.numeric(formatC(round(((H - HR) / (AB - HR - K + SF)),3),format = 'f', digits = 3))    ) %>%
    mutate(SEASON = 2023)
 
  
  }



# PTTCHING -------------------------
#  SCRAPING  #
{ # now we scrape through the pitching stats
  for (i in 1:length(front_teams)) {
    team_url <- paste0('https://frontierleague.com/sports/bsb/2022-23/teams/', front_teams[i], '?tmpl=teaminfo-network-monospace-template&sort=ab&pos=h')
    team <- read_html(team_url)
    tables <- team %>% html_table(fill = TRUE)
    
    team_pitch_df <- tables[[3]] %>%
      rename_all(toupper) %>%
      filter(`NO.` != 'NA') %>%
      mutate(TEAM = front_team_names[i]) %>%
      mutate_at(vars(3:26), ~as.numeric(gsub("[^0-9.]+", "", .)))
    
    frontier_pitchers <- bind_rows(frontier_pitchers, team_pitch_df)
    
  }
  
  frontier_p <- frontier_pitchers %>%
    relocate(TEAM, .before = `NO.`) %>%
    mutate(PLAYER = gsub("\\.","",PLAYER)) %>%
    separate(IP, into=c("Inn", "pInn"), sep= "\\.") %>%
    mutate_at(vars(4:26), as.numeric ) %>% 
    mutate_if(is.numeric, ~replace_na(., 0))  %>%
    mutate(Outs = `Inn` * 3 + `pInn`,
           TEAM = gsub("0", "-", TEAM ) ) %>%
    mutate(PLAYER = str_squish(PLAYER))
  
}

# ---------- Pitching -------------

# create the team pitching stats by grouping by player/team
FL_team_pitch<- frontier_p %>%
  group_by(TEAM)%>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  mutate(ERA = round( (`ER` / (`Outs`/3)) *9 ,2   ),
         `B/AVG` = round( (H/AB),3) ,
         IP = round(`Outs`/3 ,2), .before = Inn) %>%
  dplyr::select(-Inn, -pInn,-Outs) %>%
  rename(AVG = `B/AVG`) %>%
  dplyr::select(TEAM, W, L, APP, GS, CG, SHO, SV, IP, ERA, SO, BB,R, ER, AVG,WP, HBP, BK, AB, H, `2B`, `3B`, HR, SFA, SHA ) %>%
  arrange(ERA) %>%
  mutate(RK = row_number(), .before = TEAM )  %>%
  adorn_totals("row") %>%
  mutate(TEAM = ifelse(TEAM == "-", "League Average", TEAM))

# create all the advanced calculations 
FL_team_pitch <- FL_team_pitch %>%
  mutate(`1B` = H - `2B` - `3B` - HR,
         PA = AB + BB + HBP + SFA + SHA,
         vOBP = as.numeric(formatC(round(((H + BB + HBP) / PA) ,3), format = 'f', digits = 3)),
         vSLG = as.numeric(formatC(round(((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)) / AB) ,3), format = 'f', digits = 3)),
         vOPS = as.numeric(formatC(round((vOBP + vSLG),3),format = 'f', digits = 3)),
         vwOBA = as.numeric(formatC(round((((weights$wHBP*HBP)+(BB*weights$wBB)+
                                              (weights$w1B*`1B`)+(weights$w2B*`2B`)+
                                              (weights$w3B*`3B`)+(weights$wHR*HR))/
                                             (AB+BB+HBP+SFA))*1.035398 ,3), format = 'f', digits = 3)),
         vwRAA = as.numeric(formatC(round((((vwOBA - weights$lg_woba) / weights$woba_scale) * 
                                             PA),2),format = 'f', digits=2)),
         vRC = as.numeric(formatC(round(((((H+BB)*
                                             ((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)+
                                                 .26*(BB+HBP))+(.52*(SFA+SHA))))) / 
                                           PA),2),format = 'f', digits = 2)),
         vwRC = as.numeric(formatC(round(((((vwOBA - weights$lg_woba) / weights$woba_scale) + 
                                             (weights$lg_r_pa))*PA),2),format = 'f', digits =2)),
         `vwRC+` = as.numeric(formatC(round(((((((vwRAA/PA)) + (weights$lg_r_pa) + 
                                                  (weights$lg_r_pa-(1*weights$lg_r_pa))) / (weights$lg_r_pa))) 
                                             * 100),0), format = 'f', digits = 0)),
         `BB%` = round((BB / PA),3)*100,
         `K%` = round((SO / PA),3)*100,
         vBattingRuns= round(vwRAA +((weights$lg_r_pa -(.1*weights$lg_r_pa))*PA),2),
         # Team FIP = (((13 * HR) + (3 * (BB + HBP)) - (2 * K)) / IP) + FIP constant
         FIP = as.numeric(formatC(round(((((13 * HR) + (3 * (BB+HBP)) - (2 * SO))/ IP) + weights$cFIP),2),format = 'f',digits = 2)),
         `K/9` = formatC(round((SO / IP * 9), 2), format = 'f', digits = 2)   ,
         `BB/9` = formatC(round((BB / IP * 9), 2), format = 'f', digits = 2),
         `K%` = formatC(round((SO / PA), 2), format = 'f', digits = 2),
         #BB%
         `BB%` = formatC(round((BB / PA), 2), format = 'f', digits = 2),
         #WHIP
         WHIP = formatC(round(((BB + H) / IP), 2), format = 'f', digits = 2),
         vISO = vSLG - AVG,
         vBABIP = as.numeric(formatC(round(((H - HR) / (AB - HR - SO + SFA + SHA)),3),format = 'f', digits = 3)),
         `FIP-` = as.numeric(formatC(round(100*((FIP / FIP[RK=='Total'])     ))))
  )



# RPW = 9*(MLB Runs Scored / MLB Innings Pitched)*1.5 + 3


RunsPerWin<-round((9*(FL_team_pitch$R[FL_team_pitch$RK=='Total'] /FL_team_pitch$IP[FL_team_pitch$RK=='Total'])*1.5)+3,2)

team_games<- FL_team_pitch$W[FL_team_pitch$RK=='Total']+FL_team_pitch$L[FL_team_pitch$RK=='Total']

rep_lvl <- 0.03*(1 - FL_team_pitch$GS[FL_team_pitch$RK=='Total'] / team_games) + 0.12*(FL_team_pitch$GS[FL_team_pitch$RK=='Total'] / team_games)

avg_team_games <- team_games/(length(unique(FL_team_pitch$TEAM))-1)

FL_team_pitch <- FL_team_pitch %>%
  # mutate( # ignore attempt at  pitcher WAR
  #   pWAR = round( (
  #     ((((FL_team_pitch$FIP[RK=='Total'] - FIP) / RunsPerWin) + rep_lvl )* (IP/9)) * avg_team_games/16
  #   ),2)
  # ) %>%
  mutate(SEASON = 2023)


rc_scale<- FL_team_hit$`wRC+`[nrow(FL_team_hit)]/100


front<- frontier_p %>%
  adorn_totals("row") %>%
  rename(NAME = PLAYER) %>%
  mutate(NAME = ifelse(NAME == "-", "Team Totals", NAME)) %>%
  mutate(ERA = round( (`ER` / (`Outs`/3)) *9 ,2   ),
         `B/AVG` = round( (H/AB),3) ,
         IP = round(`Outs`/3 ,2), .before = Inn) %>%
  dplyr::select(-Inn, -pInn,-Outs) %>%
  rename(AVG = `B/AVG`) %>%
  mutate(`1B` = H - `2B` - `3B` - HR,
         PA = AB + BB + HBP + SFA + SHA,
         vOBP = as.numeric(formatC(round(((H + BB + HBP) / PA) ,3), format = 'f', digits = 3)),
         vSLG = as.numeric(formatC(round(((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)) / AB) ,3), format = 'f', digits = 3)),
         vOPS = as.numeric(formatC(round((vOBP + vSLG),3),format = 'f', digits = 3)),
         vwOBA = as.numeric(formatC(round((((weights$wHBP*HBP)+(BB*weights$wBB)+
                                              (weights$w1B*`1B`)+(weights$w2B*`2B`)+
                                              (weights$w3B*`3B`)+(weights$wHR*HR))/
                                             (AB+BB+HBP+SFA))*1.035398 ,3), format = 'f', digits = 3)),
         vwRAA = as.numeric(formatC(round((((vwOBA - weights$lg_woba) / weights$woba_scale) * 
                                             PA),2),format = 'f', digits=2)),
         vRC = as.numeric(formatC(round(((((H+BB)*
                                             ((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)+
                                                 .26*(BB+HBP))+(.52*(SFA+SHA))))) / 
                                           PA),2),format = 'f', digits = 2)),
         vwRC = as.numeric(formatC(round(((((vwOBA - weights$lg_woba) / weights$woba_scale) + 
                                             (weights$lg_r_pa))*PA),2),format = 'f', digits =2)),
         `vwRC+` = as.numeric(formatC(round(((((((vwRAA/PA)) + (weights$lg_r_pa) + 
                                                  (weights$lg_r_pa-(1*weights$lg_r_pa))) / (weights$lg_r_pa))) 
                                             * 100),0), format = 'f', digits = 0)),
         `BB%` = round((BB / PA),3)*100,
         `K%` = round((SO / PA),3)*100,
         vBattingRuns= round(vwRAA +((weights$lg_r_pa -(.1*weights$lg_r_pa))*PA),2),
         # Team FIP = (((13 * HR) + (3 * (BB + HBP)) - (2 * K)) / IP) + FIP constant
         FIP = as.numeric(formatC(round(((((13 * HR) + (3 * (BB+HBP)) - (2 * SO))/ IP) + weights$cFIP),2),format = 'f',digits = 2)),
         `K/9` = formatC(round((SO / IP * 9), 2), format = 'f', digits = 2)   ,
         `BB/9` = formatC(round((BB / IP * 9), 2), format = 'f', digits = 2),
         `K%` = formatC(round((SO / PA), 2), format = 'f', digits = 2),
         #BB%
         `BB%` = formatC(round((BB / PA), 2), format = 'f', digits = 2),
         #WHIP
         WHIP = formatC(round(((BB + H) / IP), 2), format = 'f', digits = 2),
         vISO = vSLG - AVG,
         vBABIP = as.numeric(formatC(round(((H - HR) / (AB - HR - SO + SFA + SHA)),3),format = 'f', digits = 3)),
         `FIP-` = as.numeric(round(100*((FIP /FL_team_pitch$FIP[FL_team_pitch$RK=='Total'])     )))
  )




#lgwSB<- ((FL_team_hit$SB*weights$runSB)+(FL_team_hit$CS*weights$runCS))/(FL_team_hit$`1B` + FL_team_hit$BB + FL_team_hit$HBP)
#lgwSB<- lgwSB[15]
# (SB * runSB) + (CS * runCS) - (lgwSB * (1B + BB + HBP - IBB))
#front$wSB<- round( ((front$SB * weights$runSB)+ (front$CS * weights$runCS))  -  (lgwSB*(front$`1B`+front$BB+front$HBP)),2)
# RPW = 9*(MLB Runs Scored / MLB Innings Pitched)*1.5 + 3
RunsPerWin<-round((9*(FL_team_pitch$R[FL_team_pitch$RK=='Total'] /FL_team_pitch$IP[FL_team_pitch$RK=='Total'])*1.5)+3,2)

vWRC_scale <- FL_team_pitch$`vwRC+`[nrow(FL_team_pitch)]/100


team_games<- FL_team_pitch$W[FL_team_pitch$RK=='Total']+FL_team_pitch$L[FL_team_pitch$RK=='Total']

rep_lvl <- 0.03*(1 - FL_team_pitch$GS[FL_team_pitch$RK=='Total'] / team_games) + 0.12*(FL_team_pitch$GS[FL_team_pitch$RK=='Total'] / team_games)

avg_team_games <- team_games/(length(unique(FL_team_pitch$TEAM))-1)

front <- front %>%
  mutate(
    pWAR = round( (
      ((((FL_team_pitch$FIP[FL_team_pitch$RK=='Total'] - FIP) / RunsPerWin) + rep_lvl )* (IP/9)) * avg_team_games/16  ),2),
    `vwRC+`= round( `vwRC+`/ vWRC_scale ,0)) %>%
  mutate(SEASON = 2023)

#WAR = [[([(League “FIP” – “FIP”) / Pitcher Specific Runs Per Win] + Replacement Level) * (IP/9)] * Leverage Multiplier for Relievers] + League Correction

# ----------------------

send_webhook_message("Scrape completed! Files being written to OneDrive")
# Write all dfs to CSVs
write.csv(standings23, "C:/Users/tdmed/OneDrive/Coop2 BACKUP/test/standings23.csv", row.names = FALSE)
write.csv(front_rosters, "C:/Users/tdmed/OneDrive/Coop2 BACKUP/test/front_rosters23.csv", row.names = FALSE)
write.csv(weights, "C:/Users/tdmed/OneDrive/Coop2 BACKUP/test/weights.csv", row.names = FALSE)

write.csv(FL_team_hit, "C:/Users/tdmed/OneDrive/Coop2 BACKUP/test/FL_team_hit23.csv", row.names = FALSE)
write.csv(front_h, "C:/Users/tdmed/OneDrive/Coop2 BACKUP/test/frontier_all_hitters23.csv", row.names = FALSE)
write.csv(FL_team_pitch, "C:/Users/tdmed/OneDrive/Coop2 BACKUP/test/FL_team_pitch23.csv", row.names = FALSE)
write.csv(front, "C:/Users/tdmed/OneDrive/Coop2 BACKUP/test/frontier_all_pitchers23.csv", row.names = FALSE)

send_webhook_message("Data and Stats files successfully copied to Coop 2.0 folder! Coop 2.0 is being redeployed...")