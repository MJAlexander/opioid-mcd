## Installs all packages and fonts required to reproduce plots.
if (!require(devtools)) {
    install.packages("devtools", repos = "http://cran.us.r-project.org")
}
if (!require(tidyverse)) {
    install.packages("tidyverse", repos = "http://cran.us.r-project.org")
    require(tidyverse)
}
if (!require(ggforce)) {
    install.packages("ggforce", repos = "http://cran.us.r-project.org")
    require(ggforce)
}
if (!require(RColorBrewer)) {
    install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
    require(RColorBrewer)
}
if(!require(viridis)) {
    install.packages("viridis", repos="http://cloud.r-project.org")
    require(viridis)
}
if(!require(scales)) {
    install.packages("scales", repos = "https://cloud.r-project.org/")
    require(scales)
}
if(!require(grid)) {
    install.packages("grid", repos = "https://cloud.r-project.org/")
    require(grid)
}
if(!require(binom)) {
    install.packages("binom", repos = "https://cloud.r-project.org/")
    require(binom)
}
