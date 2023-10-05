FROM rocker/r-base:latest
ENV PATH="/root/miniconda3/bin:$PATH"
ARG PATH="/root/miniconda3/bin:$PATH"
SHELL ["/bin/bash", "-c"]
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    && rm -rf /var/lib/apt/lists/*
RUN apt-get update --fix-missing && \
    apt-get install -y wget bzip2 ca-certificates curl git && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*
RUN R -e "install.packages(c('tidyverse','ggpubr','kableExtra','lookup','readxl','DT','reticulate','data.table','shinydashboard'),dependencies=TRUE, repos='http://cran.rstudio.com/')"
COPY environment.yaml .
COPY requirements.txt .
RUN exec bash \
	&& conda init bash \
	&& conda env create -n r-reticulate --file environment.yaml
# RUN install.r --deps TRUE \
# 	shiny \
# 	tidyverse \
# 	ggpubr \
# 	kableExtra \
# 	lookup \
# 	readxl \
# 	DT \
# 	reticulate \
# 	data.table \
# 	shinydashboard
RUN addgroup --system app \
    && adduser --system --ingroup app app
RUN chown app:app -R /home
RUN chown app:app -R /root
RUN wget \
    https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh \
    && mkdir /root/.conda \
    && bash Miniconda3-latest-Linux-x86_64.sh -b \
    && rm -f Miniconda3-latest-Linux-x86_64.sh
RUN conda create -n r-reticulate
RUN conda update conda
RUN conda clean --all
RUN conda config --add channels bioconda
RUN conda config --add channels conda-forge
RUN conda config --add channels defaults
RUN conda install -y -n r-reticulate python=3.9.17
RUN conda install -y -n r-reticulate pandas=2.0.3
Run conda install -y -n r-reticulate numpy=1.25.2
WORKDIR /home/app
COPY app .
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('kassambara/ggpubr')"
SHELL ["conda", "run", "-n", "r-reticulate", "/bin/bash", "-c"]
RUN pip install pyairtable
RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/home/app')"]
