
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nhanesA

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/nhanesA)](https://cran.r-project.org/package=nhanesA)
[![LICENSE](https://img.shields.io/cran/l/nhanesA)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/nhanesA)](https://cran.r-project.org/package=nhanesA)
<!-- badges: end -->

nhanesA is an R package for browsing and retrieving data from the
National Health And Nutrition Examination Survey (NHANES). This package
is designed to be useful for research and instructional purposes.

The functions in the nhanesA package allow for fully customizable
selection and import of data directly from the [NHANES
website](https://www.cdc.gov/nchs/nhanes/) thus it is essential to have
an active network connection.

### Install from CRAN

``` r
install.packages("nhanesA")
```

### Install from the dev repo

``` r
install.packages("devtools")
devtools::install_github("cjendres1/nhanes")
```

### Use nhanesA in Docker

The Docker container hosts the data, allowing for faster access and
manipulation directly from the local Docker environment. The summary of
the differences between using the standard nhanesA and using it inside
Docker is as follows:

**Standard nhanesA:**

- When used outside of Docker, the `nhanesA` functions scrape data
  directly from the CDC website each time they are invoked.
- The advantage is simplicity; users only need to install the `nhanesA`
  package without any additional setup.
- However, the response time is contingent upon internet speed and the
  size of the requested data.

**Docker-enhanced nhanesA:**

- The Docker container locally hosts most of the NHANES data, allowing
  for significantly faster data access and manipulation.
- Initial setup requires Docker installation and downloading the Docker
  image.
- Pre-pandemic tables, DXA and the youth survey, are also not present in
  the Docker database and would similarly be fetched from the CDC
  website.

In essence, while the Docker-enhanced version offers blazing-fast access
to a majority of the data, it will fetch data in the standard `nhanesA`
manner for datasets not present in its database.

To use `nhanesA` inside Docker, you can follow the two steps below:
start Docker using the provided command and access RStudio through the
given link.

**1. Start Docker**

Start Docker on Mac or Linux

``` dockerfile
docker \
    run \
        --rm \
        --platform=linux/amd64 \
        --name nhanes-workbench \
        -v <YOUR LOCAL PATH>:/mnt/ \
        -d \
        -p 8787:8787 \
        -p 2200:22 \
        -p 1433:1433 \
        -e 'CONTAINER_USER_USERNAME=USER' \
        -e 'CONTAINER_USER_PASSWORD=PASSWORD' \
        -e 'ACCEPT_EULA=Y' \
        -e 'SA_PASSWORD=yourStrong(!)Password' \
         hmsccb/nhanes-workbench:version-0.2.0
```

Start Docker on Windows

``` dockerfile
docker ^
    run ^
        --rm ^
        --platform=linux/amd64 ^
        --name nhanes-workbench ^
  -v <YOUR LOCAL PATH>:/mnt/ ^
  -p 8787:8787 -p 2200:22 -p 1433:1433 ^
  -e "CONTAINER_USER_USERNAME=USER" ^
  -e "CONTAINER_USER_PASSWORD=PASSWORD" ^
  -e "ACCEPT_EULA=Y" ^
  -e "SA_PASSWORD=yourStrong(!)Password" ^
  hmsccb/nhanes-workbench:version-0.2.0
```

**2. Log into Rstudio**

Log into RStudio via: \<http<nolink>://localhost:8787\> and using the
username set in the command above. In the above command, the username
and password are set as `USER` and `PASSWORD`,respectively, but you can
modify them if you prefer.

More details about [NHANES Docker](https://github.com/ccb-hms/NHANES).

<br/>

**Working with nhanesA**

[Manual](https://cran.r-project.org/package=nhanesA/nhanesA.pdf)

[Vignette: Introducing
nhanesA](https://cran.r-project.org/package=nhanesA/vignettes/Introducing_nhanesA.html)

<br />
<img src="man/figures/nhanesAsticker.png" alt="drawing" width="400"/>
