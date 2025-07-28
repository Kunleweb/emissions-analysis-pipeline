# Climate Emissions Analysis Pipeline

The analysis was developed in **R**, using a reproducible and modular pipeline built on **RMarkdown**. Key components include:

## 📦 Data Wrangling & Visualization  
- **tidyverse**: Core data manipulation and visualization (incl. `ggplot2`, `dplyr`, `readr`)  
- **scales**: Formatting large axis values, SI notation, percentage scaling  
- **viridis**: Color-blind-friendly palettes  
- **patchwork**, **gridExtra**: Multi-panel plot layout management  
- **ggpubr**: Publication-friendly plot elements and statistical annotations  

## 📈 Specialized Analysis  
- **ggraph**: Network plots (e.g., ownership–commodity relationships)  
- **ggridges**: Ridgeline plots for time series and distribution overlays  
- **zoo**: Rolling time-window statistics and historical smoothing  

## 🌍 Geospatial Mapping  
- **maps**, **rworldmap**: Geographical boundary data and plotting  
- **countrycode**: ISO conversion for country labeling  

## 🧾 Reporting & Documentation  
- **knitr**, **rmarkdown**, YAML headers: Full academic document generation (PDF, HTML, Poster source)  
- Dynamic titles, citation embedding, and plot captions for reproducibility and transparency  

## 🧠 Interpretation  
The integration of commodity-specific intensity, regional aggregation, and ownership profiles provides a multi-dimensional understanding of emission sources. From a policy standpoint, the extreme concentration of emissions among a limited number of companies and sectors suggests that high-impact reductions can be achieved with focused intervention. However, broader structural change remains essential for long-term decarbonization.
