# campus-wifi-connection-count-dashboard

This data dashboard provides information about the crowd data in the Hong Kong Polytechnic University with an interactive map.

## File Structure

```bash
│   .gitignore
│   app.R               # main Shiny application script
│   building.dbf        # map related files
│   building.geojson    # map related files
│   building.prj        # map related files
│   building.shp        # map related files
│   building.shx        # map related files
│   README.md
│
├───focusGroupStudent   # data folder
│       df_allStudent.csv
│
├───server              # server folder
│       tab_1day.R
│       tab_6months.R
│
├───ui                  # ui folder
│       polyu.jpg
│       tab_1day.R
│       tab_6months.R
│
└───www                 # image folder
        logo3.png
        polyu.jpg
        polyu2.png
```

## Dependencies

- shiny==1.7.2
- shinyjs==2.1.0
- shinydashboard==0.7.2
- shinythemes==1.2.0
- shinycssloaders==1.0.0
- sqldf==0.4.11
- summarytools==1.0.1
- scales==1.2.1
- tidyverse==1.3.2
- ggplot2==3.4.0
- dplyr==1.0.9
- magrittr==2.0.3
- ggrepel==0.9.2
- plotly==4.10.1
- RColorBrewer==1.1.3
- rgdal==1.6.3
- leaflet==2.1.1
- htmltools==0.5.3