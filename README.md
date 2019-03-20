CMU English R for Textmining Workshop
=========================

Scripts and data for an R workshop by David Brown and Matthew Lincoln for Carnegie Mellon University (March 2019).

You will need to bring your own laptop to participate in this workshop. You will have two options for how to access R, Rstudio, and the code we're using:

## Easy but Slightly Limited: Use the free web-based RStudio via Binder

For the purposes of this workshop, we'll have a web interface version of RStudio available at:

https://mybinder.org/v2/gh/browndw/cmu_workshop/master?urlpath=rstudio

**Note: this can take a few minutes to open up the first time that you load it; if it goes for a very long time, you can try reloading your browser window.**

This will give you virtually the same interface as you would have if you had R and RStudio installed on your local laptop, and will come with all the packages, scripts, and data files you need. However, this does have two drawbacks: 

1. Because we're sharing space on this server, it's not very powerful. We'll be working with relatively small datasets in this workshop - just a few books and a few thousand tweets - so the code will run, but it will not be particularly snappy. If you eventually want to do bigger data analysis on your own texts, you will need to install R on your own machine.

2. Binder's servers are temporary and read-only! As long as you are working in the browser window, you'll be able to edit and run code. However if you close the window, or go for more than 10 minutes without interacting with it, Binder will reclaim your space (remember, we're sharing when we use this!) and any changes you made will be discarded. You CAN download your customized code files by selecting the files you want to download from the "Files" tab in the lower right, and then clicking More... > Export... and it will download them to your laptop.

## Difficult but Powerful: Install R and R Studio yourself on your own laptop ahead of time

We definitely recommend doing this if you have the time, as you'll be able to more easily save your work from this workshop. But it's a bit more involved than clicking one link. To do this:

1. Install R: https://cran.r-project.org/

2. Install RStudio Desktop (the FREE open source version): https://www.rstudio.com/products/rstudio/download/

3. Download the workshop code at https://github.com/browndw/cmu_workshop (note that we will continue to update this code as we prepare the subsequent workshops, so you'll want to download the latest on the morning before each one.)

3. Install the tidyverse, quanteda, and other R packages as listed in the `setup.R` file in the base directory of the workshop code.

FYI: although it's pretty easy to get R and RStudio installed, quanteda relies on compiled code and can be pretty finnicky to get working if you've never set up a development environment (with compilers and other code libraries) before. They have specific advice here: https://quanteda.io/

**We won't be spending any time during this workshop helping you to set this up!** There are a lot of resources online on "how to install r and rstudio for XXX operating system".
If you run in to any difficulty, or if this sounds like way too much to start out with on workshop day 1, that's why we have option 1 described above.
