# Real time projection of 2026/27 budget balance

###########################
# Setup the R Environment #
###########################
# Common Packages
packages<-c("tidyverse","scales",
            "quantmod",
            "ggplot2","ggthemes")
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,type="binary")
  sapply(pkg, require, character.only = TRUE)
}
check.packages(packages)

# Color scheme
col<-c("#CC2529","#396ab1","#3E9651","#DA7C30","#535154","#6B4C9A","#922428","#948B3D")
scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = col)
}
scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = col)
}

# Figure theme
mytheme<-theme_minimal()+theme(
  axis.title.y = element_text(size=9),
  axis.title.x = element_text(size=9),
  panel.background = element_rect(fill='white',color=NA),
  plot.background = element_rect(fill='white',color=NA),
  legend.position = "top",
  legend.text=element_text(size=10),
  plot.caption = element_text(size = 6, color = "gray40",hjust=1),
  plot.title = element_text(face = "bold"),
  plot.subtitle = element_text(size = 8, color = "gray40"),
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank()
)

######################
# Start of main code #
######################

# Load the latest WTI prices
getSymbols("CL=F", src = "yahoo")

# Exchange rate
getSymbols("CADUSD=X", src = "yahoo")

# This can fetch the futures prices
# See: https://ca.finance.yahoo.com/quote/CL%3DF/futures/
month_codes <- c("F","G","H","J","K","M","N","Q","U","V","X","Z")
month_names <- c("Jan","Feb","Mar","Apr","May","Jun",
                 "Jul","Aug","Sep","Oct","Nov","Dec")
contracts <- c(
  paste0("CL", month_codes[4:12], "26.NYM"),   # Apr–Dec 2026
  paste0("CL", month_codes[1:3], "27.NYM")      # Jan–Mar 2027
)
results <- data.frame(contract = character(),
                      date = as.Date(character()),
                      settle = numeric(),
                      stringsAsFactors = FALSE)
for (sym in contracts) {
  tryCatch({
    getSymbols(sym, src = "yahoo", auto.assign = TRUE)
    dat <- get(sym)  # quantmod replaces . with -
    last_row <- tail(dat, 1)
    settle <- as.numeric(Cl(last_row))  # closing price
    
    # Parse the month/year from the symbol
    code_letter <- substr(sym, 3, 3)
    code_year   <- substr(sym, 4, 5)
    mo <- month_names[match(code_letter, month_codes)]
    
    results <- rbind(results, data.frame(
      contract = sym,
      label = paste(mo, paste0("20", code_year)),
      date = index(last_row),
      settle = settle,
      stringsAsFactors = FALSE
    ))
    
    Sys.sleep(1)  # be polite to Yahoo's servers
    
  }, error = function(e) {
    message("Failed to fetch ", sym, ": ", e$message)
  })
}

# If leading month is NA
results<-results %>%
  mutate(settle=ifelse(is.na(settle) & row_number()==1,settle[2],settle))

# Fiscal year dates (Apr 1 to Mar 31)
fy_start <- if (month(today()) >= 4){
  as.Date(sprintf("%d-04-01", year(today())))
} else {as.Date(sprintf("%d-04-01", year(today()) - 1))}
fy_end   <- fy_start %m+% years(1) - days(1)

# Monthly average prices for fiscal year-to-date
wti <- `CL=F`
wti_df <- tibble(
  date = as.Date(index(wti)),
  wti = as.numeric(Cl(wti))
) %>%
  filter(date >= fy_start) %>%
  mutate(label=as.yearmon(date)) %>%
  reframe(wti=mean(wti),.by='label')

# Combine futures with fiscal year-to-date
combined<-wti_df %>%
  rbind(as_tibble(results %>% mutate(label=as.yearmon(label)) %>%
                    select(label,wti=settle)))

# Waterfall graphic, changes in OpEx for 2026-27 vs Budget 2024
futures_average<-mean(c(wti_df$wti,as.numeric(Cl(wti[dim(wti)[1]])),
                             results$settle))
plotdata<-data.frame(
  type=c("Budget 2026\nDeficit",
         paste("Oil prices\naverage",dollar(mean(combined$wti))),
         "Revised\nBudget 2026\nBalance"),
  value=c(-9373,680*(mean(combined$wti)-60.5),680*(mean(combined$wti)-60.5)-9373)/1000
) %>%
  mutate(type=factor(type,levels=type),
         id=seq_along(value),
         sign=ifelse(value>0,"in","out"),
         sign=ifelse(id==n(),"net",sign),
         end=cumsum(value),
         end=ifelse(id==n(),0,end),
         start=ifelse(id>1,lag(end,1),0))
ggplot(plotdata %>% mutate(sign=ifelse(id==1,"net",sign)))+
  geom_rect(aes(x=type,xmin = id - 0.4, xmax = id + 0.4,ymin=end,ymax=start,fill=sign),show.legend = F)+
  geom_hline(yintercept=0,linewidth=1)+
  geom_segment(data=plotdata %>% filter(sign!="net"),
               aes(x=id+0.45,xend=id+0.45,y=start,yend=end),
               linewidth=0.75,arrow=arrow(type="closed",length=unit(0.1,"cm")))+
  mytheme+
  geom_text(data=filter(plotdata,(id==1 | id==max(id)) & value>0),
            aes(label=paste0(dollar(value,0.1),"B"),x=type,y=value),
            color='white',vjust=1,nudge_y=-0.5)+
  geom_text(data=filter(plotdata,(id==1 | id==max(id)) & value<0),
            aes(label=paste0(dollar(value,0.1),"B"),x=type,y=value),
            color='white',vjust=0,nudge_y=0.5)+
  geom_text(data=filter(plotdata,id>1 & id<max(id) & sign=='out'),
            aes(label=paste0(dollar(value,0.1),"B"),x=type,y=end),
            color=col[1],vjust=1,nudge_y=-0.3)+
  geom_text(data=filter(plotdata,id>1 & id<max(id) & sign=='in'),
            aes(label=paste0("+",dollar(value,0.1),"B"),x=type,y=end),
            color=col[3],vjust=0,nudge_y=0.3)+
  scale_fill_manual(name="",values=c(col[3],col[2],col[1]))+
  scale_y_continuous(breaks=pretty_breaks(n=6),label=dollar)+
  labs(x="",y="Millions of Dollars",
       title=paste("Based on fiscal year-to-date prices and\nfutures markets as of",
                   paste0(format(results[1,]$date, "%B "),
                         as.integer(format(results[1,]$date, "%d")),", ",
                         format(results[1,]$date, "%Y"))),
       subtitle="Source: Own calculations from Alberta Budget 2026 and Yahoo Finance.",
       caption="Graph by @trevortombe")
ggsave("Figures/BudgetBalanceWaterfall.png",width=8,height=4)

# Define a function
ab_oil_tracker <- function(today = Sys.Date(),
                           rev_annual = 74550e6,
                           exp_annual = 83922e6,
                           oil_assumption = 60.50,
                           oil_sensitivity = 700e6,
                           exch_assumption = 73,
                           exch_sensitivity = -440e6) {
  
  #--- fetch WTI futures from Yahoo
  suppressWarnings(
    getSymbols("CL=F", src = "yahoo", from = fy_start, auto.assign = TRUE)
  )
  wti <- `CL=F`
  exch <- `CADUSD=X`
  
  wti_df <- tibble(
    date = as.Date(index(wti)),
    wti = as.numeric(Cl(wti))
  ) %>%
    # filter(date >= fy_start, date <= today) %>%
    distinct(date, .keep_all = TRUE)
  latest_wti <- tail(na.omit(wti_df$wti), 1)-adjust
  exch_df <- tibble(
    date = as.Date(index(exch)),
    exch = as.numeric(Cl(exch))*100
  ) %>%
    filter(date >= fy_start, date <= today) %>%
    distinct(date, .keep_all = TRUE)
  latest_exch <- tail(na.omit(exch_df$exch), 1)
  
  #--- daily baseline flows from budget
  rev_day_base <- rev_annual / 365
  exp_day_base <- exp_annual / 365
  base_day_bal <- rev_day_base - exp_day_base
  oil_day_sens <- oil_sensitivity / 365
  exch_day_sens <- exch_sensitivity / 365
  
  #--- build daily fiscal-year path
  dates <- seq(fy_start, fy_end, by = "day")
  
  df <- tibble(date = seq(fy_start, fy_end, by = "day")) %>%
    left_join(wti_df, by = "date") %>%
    left_join(exch_df, by = "date") %>%
    mutate(exch=ifelse(date==as.Date("2026-04-01"),71.9,exch)) %>% # manual fix
    arrange(date) %>%
    fill(wti, .direction = "down") %>%
    fill(exch, .direction = "down") %>%
    mutate(
      phase = if_else(date <= today, "Actual/YTD", "Projected"),
      wti_used = if_else(date <= today, wti, latest_wti),
      exch_used = if_else(date <= today, exch, latest_exch),
      daily_balance = base_day_bal + oil_day_sens * (wti_used - oil_assumption) + 
        exch_day_sens * (exch_used - exch_assumption),
      cum_balance = cumsum(daily_balance)
    ) %>%
    left_join(
      results %>% mutate(date=as.Date(as.yearmon(label)),
                         settle=settle-adjust) %>%
                           select(date,settle),by='date'
    ) %>%
    fill(settle) %>%
    mutate(
      daily_balance_settle = base_day_bal + oil_day_sens * (settle - oil_assumption) + 
        exch_day_sens * (exch_used - exch_assumption),
      cum_balance_settle = cumsum(daily_balance_settle)
    )
  
  last_actual_cum <- df %>%
    filter(phase == "Actual/YTD") %>%
    slice_max(date, n = 1) %>%
    pull(cum_balance)
  fan_df <- tibble(
    date = if (today == fy_end) today else seq(today + 1, fy_end, by = "day")
  ) %>%
    mutate(
      day_index = row_number(),
      daily_low = base_day_bal + oil_day_sens * (bot_range - oil_assumption),
      daily_high = base_day_bal + oil_day_sens * (top_range - oil_assumption),
      cum_low = last_actual_cum + cumsum(daily_low),
      cum_high = last_actual_cum + cumsum(daily_high),
      
      # map cumulative balance onto left axis for plotting
      ymin = pmin(cum_low, cum_high) / 1e9 * sf,
      ymax = pmax(cum_low, cum_high) / 1e9 * sf
    )
  
  list(
    data = df,
    fan_data = fan_df,
    summary = tibble(
      fy_start = fy_start,
      fy_end = fy_end,
      today = today,
      oil_assumption = oil_assumption,
      latest_wti = latest_wti,
      ytd_avg_wti = mean(df$wti_used[df$date <= today], na.rm = TRUE),
      ytd_balance = sum(df$daily_balance[df$date <= today], na.rm = TRUE),
      projected_full_year_balance = sum(df$daily_balance, na.rm = TRUE)
    )
  )
}

sf <- 10   # adjust if needed for visual balance
adjust<-0 # amount to lower oil prices relative to futures (which are delayed)
top_range=ceiling(filter(combined,label==max(label))$wti*1.2/10)*10
bot_range=round(filter(combined,label==max(label))$wti*0.8/10)*10
results2 <- ab_oil_tracker()
s <- results2$summary
df <- results2$data
fan_df <- results2$fan_data
fan_end <- fan_df %>% slice_max(date, n = 1)
futures_proj<-df %>% filter(!is.na(settle)) %>% ungroup() %>%
  mutate(cum_balance_settle=ifelse(date==min(date),cum_balance,daily_balance_settle),
         cum_balance_settle=cumsum(cum_balance_settle)) %>% 
  slice_max(date, n = 1)
to_balance_remain<-bot_range-fan_end$cum_low/(700e6*as.numeric((fan_end$date-Sys.Date())/365))
ggplot(df %>% filter(phase=="Actual/YTD"), aes(x = date)) +
  geom_col(aes(y = daily_balance / 1e6, 
               fill=daily_balance>0),
           width = 1, alpha = 0.75, show.legend = F) +
  scale_fill_manual(values = c(`FALSE` = col[1], `TRUE` = col[2]))+
  geom_line(aes(y=cum_balance * sf / 1e9),linewidth=1.5)+
  geom_line(data=df %>% filter(!is.na(settle)) %>% ungroup() %>%
              mutate(cum_balance_settle=ifelse(date==min(date),cum_balance,daily_balance_settle),
                     cum_balance_settle=cumsum(cum_balance_settle)),
            aes(y=cum_balance_settle * sf / 1e9),linewidth=1,linetype='dotted')+
  geom_hline(yintercept = 0, linewidth=0.75)+
  geom_vline(xintercept = s$today, linetype = "dashed") +
  geom_text(
    data = df %>% filter(phase == "Actual/YTD") %>% slice_max(date, n = 1),
    aes(y = 0,x=today(),
        label = paste0("YTD: ", dollar(cum_balance / 1e9, suffix = "B", accuracy = 0.1),"\n(right axis)")),
    nudge_x = 5, vjust=1, nudge_y=-5,hjust = 0, size = 3
  ) +
  geom_text(
    data = df %>% slice_max(date, n = 1),
    aes(y = (-9.373) * sf,
        label = "Budget\nforecast: -$9.4B"),
    nudge_x = 10,hjust = 0, size = 3, colour = col[4]
  )+
  mytheme +
  scale_y_continuous(
    labels = dollar,breaks=pretty_breaks(6),
    name = "Daily balance ($ millions)",
    sec.axis = sec_axis(
      ~ . / sf,
      name = "Cumulative balance ($ billions)",
      breaks = seq(-10, 25, by = 5),
      labels = function(x) dollar(x, suffix = "B", accuracy = 0.1)
    )
  ) +
  geom_ribbon(
    data = fan_df,
    aes(x = date, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    alpha = 0.18,
    fill = "darkgreen"
  )+
  geom_point(data = df %>% filter(!is.na(settle)) %>% ungroup() %>%
               mutate(cum_balance_settle=ifelse(date==min(date),cum_balance,daily_balance_settle),
                      cum_balance_settle=cumsum(cum_balance_settle)) %>% 
               slice_max(date, n = 1),
             aes(y = (cum_balance_settle *sf / 1e9)),
             size=2.5,stroke=2.5,shape=21,fill='white')+
  geom_point(data = df %>% slice_max(date, n = 1),
             aes(y = (-9.373 *sf )),colour = col[4],
             size=2.5,stroke=2.5,shape=21,fill='white')+
  geom_point(data=(df %>% filter(phase=="Actual/YTD") %>% filter(date==max(date))),
             aes(y=cum_balance * sf / 1e9),size=2.5,stroke=2.5,shape=21,fill='white')+
  geom_text(
    data = fan_end,
    aes(x = date, y = ymax, label = paste0("$",top_range," WTI")),
    inherit.aes = FALSE,
    hjust = -0.1, vjust = 1,
    size = 3, colour = "darkgreen"
  ) +
  geom_text(data = futures_proj,
             aes(y = (cum_balance_settle *sf / 1e9)),nudge_x=10,
             label='Futures\nmarket',size=2.5,hjust=-0.1)+
  geom_text(
    data = fan_end,
    aes(x = date, y = ymin, label = paste0("$",bot_range," WTI")),
    inherit.aes = FALSE,
    hjust = -0.1, vjust = 0,
    size = 3, colour = "darkgreen"
  )+
  annotate('text',x=as.Date("2025-07-01"),y=18,
           label="Positive daily balances",size=3,color=col[2])+
  annotate('text',x=as.Date("2025-05-01"),y=-22,
           label="Negative daily balances",size=3,color=col[1])+
  annotate('text',x=as.Date("2025-12-01"),y=-30,
            label="Cumulative balance",size=3)+
  scale_x_date(
    limits = c(min(df$date), max(df$date)+50),
    breaks = seq(fy_start, fy_end+1, by = "2 month"),
    date_labels = "%b\n%Y"
  )+
  labs(
    title = paste0("Alberta Budget Balance Tracker for 2026–27 (data to ",
                   paste0(format(max(index(`CL=F`)), "%B "),
                          as.integer(format(max(index(`CL=F`)), "%d")),", ",
                          format(max(index(`CL=F`)), "%Y")),")"
                   ),
    subtitle = paste0(
      "Note: These are experimental estimates based on the latest fiscal updates and daily oil price and exchange rate data",
      "\nLatest WTI: $", round(s$latest_wti, 2),
      " | Cumulative balance: ", dollar(s$ytd_balance, scale = 1e-9, suffix = "B",0.1),
      " | Fiscal year projection (at latest WTI futures): ", dollar(futures_proj$cum_balance_settle, scale = 1e-9, suffix = "B",0.1)
    ),
    x = NULL,
    y = "Millions of dollars",
  )+
  geom_text(
    data = df %>% filter(phase == "Actual/YTD") %>% slice_max(date, n = 1),
    aes(y = 0,x=today(),
        label = paste0("Needed to balance over the\nrest of the fiscal year: ", dollar(to_balance_remain,2),"/bbl")),
    nudge_x = 5, nudge_y=max(df$daily_balance)/1000000+10,hjust = 0, size = 3,color=col[3]
  )
ggsave("Figures/BudgetBalanceProjection.png",width=9,height=4.5)


