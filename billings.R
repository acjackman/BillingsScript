billingsDBfile <- "/Users/adamjackman/Library/Application Support/Billings Pro/Storage.bipdb/Database/billingspro.bid"
plotfile <- "/Users/adamjackman/Dropbox/Library/Resources/WorkPlot"
goalfile <- "/Users/adamjackman/.timegoals.json"
max_val <- 20

# Also Depends on imageMagick
library("RColorBrewer")
library("RSQLite")
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(jsonlite)

if(file.exists(goalfile)){
    goals <- fromJSON(goalfile)
} else{
    warning("Couldn't find goal file")
    goals <- data.frame(clientName = character(), goal = numeric())
}

con <- dbConnect(RSQLite::SQLite(), billingsDBfile)

ctime <- now()
last_monday <- floor_date(ctime, "day") - days((wday(ctime)+5) %% 7 )
query <- paste0("
SELECT TimeSlip._rowid AS slipID,
       TimeSlip.name AS slipName,
       TimeSlip.activeForTiming,
       TimeSlip.typeID,
       TimeEntry.startDateTime,
       TimeEntry.endDateTime,
       rate,
       projectID,
       Project.name AS projectName,
       ClientCategory.name AS clientCategory,
       Client._rowid AS clientID,
       CASE WHEN Client.isCompany = 1
            THEN Client.company
            ELSE Client.firstName || ' ' || Client.lastName
       END AS clientName
FROM TimeSlip
LEFT JOIN TimeEntry ON timeSlipID = TimeSlip._rowid
LEFT JOIN Project ON TimeSlip.projectID = Project._rowid
LEFT JOIN Client ON Project.clientID = Client._rowid
LEFT JOIN ClientCategory ON Client.clientCategoryID = ClientCategory._rowid
WHERE TimeEntry.startDateTime >= ", as.double(last_monday),"
ORDER by TimeSlip.startDateTime DESC")

res <- dbGetQuery(con, query) %>%
dplyr::mutate(
    startDateTime = as.POSIXct(startDateTime, origin="1970-01-01"),
    endDateTime = as.POSIXct(endDateTime, origin="1970-01-01"),
    duration = as.numeric(difftime(endDateTime,startDateTime, units="hours"))
) %>%
group_by(clientCategory, clientID, projectID, slipID) %>%
summarise(
    clientName = first(clientName),
    projectName = first(projectName),
    slipName = first(slipName),
    typeID = first(typeID),
    rate = first(rate),
    duration = sum(duration),
    earned = {
        if(typeID == 1000){
            return(duration * rate)
        } else if (typeID == 2000){
            return(rate)
        }
    }
) %>%
group_by(clientCategory, clientName) %>%
summarise(time = sum(duration), earned = sum(earned)) %>%
ungroup()%>%
mutate(
    clientCategory = ifelse(is.na(clientCategory), "OTHER", clientCategory),
    clientName = ifelse(is.na(clientName), "UNFILED", clientName),
    dur_string = paste0(floor(time),":",str_pad(floor((time - floor(time)) * 60), 2, side = "left", "0"))
) %>%
arrange(clientCategory, clientName) %>%
left_join(.,goals, by="clientName") %>%
mutate(
    clientName = factor(clientName, levels=unique(clientName)[length(unique(clientName)):1])
)
dbDisconnect(con)

# Prep Colors
client_cats <- c("Classes", "OTHER", "Archived", "Work")
client_cats <- append(client_cats, unique(res$clientCategory[!res$clientCategory %in% client_cats]))
cat_colors  <- brewer.pal(length(client_cats), "Set1")
names(cat_colors) <- client_cats

# Make Plot
p <- ggplot(res, aes(x=clientName, y=time, fill=clientCategory)) +
    geom_bar(stat="identity") +
    coord_flip(ylim = c(0,max_val + 1)) +
    geom_errorbar(aes(y=goal, ymax=goal, ymin=goal), colour="#FF7F00", width=0.5) +
    geom_text(aes(y = ifelse(time > max_val, max_val, time),
                  ymax=time,
                  label=dur_string,
                  hjust=ifelse(time < 1.25, -.1, 1)
        ),
        position = position_dodge(width=1),
        color = "white"
    ) +
    scale_fill_manual(breaks = client_cats, labels= client_cats, values = cat_colors) +
    guides(fill = FALSE) +
    theme(plot.background = element_rect(fill = "transparent", colour = NA),
                    panel.background= element_rect(fill = "transparent", colour = NA), # Removes gray background
                    # panel.grid.minor=element_blank(),
                    panel.grid.minor.y=element_blank(),
                    panel.grid.major.y=element_blank(),
                    axis.text    = element_text(color='#ffffff'),
                    axis.text.y  = element_text(size=13),
                    axis.ticks  = element_blank(),
                    axis.text.x  = element_text(size=13),
                    axis.title.x = element_blank(),
                    axis.title.y = element_blank()
                    )

pdf(paste0(plotfile,".pdf"),  width=9, height=.25*nrow(res), bg = "transparent")
p
dev.off()

# system(paste0("convert -density 300 -depth 8 -quality 85 '", plotfile, ".pdf' '", plotfile, ".png'"))
