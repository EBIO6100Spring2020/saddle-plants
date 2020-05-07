Exploration
================
All
2020-05-07

    ## Warning: package 'rmarkdown' was built under R version 3.6.3

    ## Warning: package 'viridis' was built under R version 3.6.3

<b>SAMPLING:</b> We decided to use the the NIWOT saddle grid data to answer our questions. This grid was sampled intermittently since 1989 and annually or biannually since 2006. How often is each year represented? We first wanted to know how sampling differed across the years in the data set. Based on this information we excluded 1996 from our analysis

    ## 
    ##  1989  1990  1995  1996  1997  2006  2008  2010  2011  2012  2013  2014 
    ##  9600 11771 12249   195 11535 13274 15521 15513 15801 15320 15641 15919 
    ##  2015  2016  2017  2018 
    ## 16948 17117 17299 17473

we also noticed sampling design seemed to vary. In some years middle hits were taken and in others they were not. In some years top hits were recorded for every plot but not in others.

Ultimately we decide to isolate only the top hits and covert bottom only hits to top hits in the early years

![](Exploration_presentation_done_files/figure-markdown_github/r%20-1.png)![](Exploration_presentation_done_files/figure-markdown_github/r%20-2.png)

<b>SPECIES TRENDS:</b> Although intially we were primarily interested in the Deschapsia-Geum interaction we noticed that kobresia myoseroides was the second most common hit at decided to include it in our analysis.

We next wanted to visualise how each species was changing throughout time. Was Deschampsia becoming much more abundant across the whole site?

![](Exploration_presentation_done_files/figure-markdown_github/r%20species%20trends-1.png)

Within certain habitat type it seems like relative abundance of certain species changes

![](Exploration_presentation_done_files/figure-markdown_github/r%20veg%20class%20trends-1.png)

Elevation 1 with veg classes

![](Exploration_presentation_done_files/figure-markdown_github/r%20elev1-1.png)

<b>SNOW DEPTH:</b> early models showed that that there was a lot of variation in space, with somewhat less in time.The only explanatory variable we had that varied in both space and time was snow depth. Snow depth was measured at stakes associated with each plot starting in March on a biweekly (mostly) schedule, producing a mountain of data.

We felt the most biologically relavant data to be max depth and snow melt date. Due to gaps in the data it was hard to estimate snow melt date reliably. Scott tried valiantly to fit GAMs to the snow data to allow us to estimate snow melts. But after several weeks and many discussions the GAMs grew too frustrating and we opted to use mean snow depth in June - simpler metric that still included some of that information.

![](Exploration_presentation_done_files/figure-markdown_github/r%20snow%20melt-1.png)![](Exploration_presentation_done_files/figure-markdown_github/r%20snow%20melt-2.png)

We were able use temperature data at the site level from the saddle data logger. This provided us with a wealth of data at a fine temporal scale.

![](Exploration_presentation_done_files/figure-markdown_github/r%20daily%20temp-1.png)

However, we decided to use the summer mean daily maximum from June - August as our predictor. In different models we used both current year, and the previous three years seperately.

<b>SPATIAL AUTOCORELATION:</b> We also included a spatial autocorrelation term to incorporate the effect of the abuncance of each speces in the previous year weighted by distance to the plot.

    ##              1           2           3           4           5           6
    ## 1           NA 0.018605375 0.009554163 0.006517692 0.004939944 0.004062494
    ## 2  0.018605375          NA 0.019632046 0.010030774 0.006725057 0.005196821
    ## 3  0.009554163 0.019632046          NA 0.020419583 0.010212295 0.007059780
    ## 4  0.006517692 0.010030774 0.020419583          NA 0.020406318 0.010783620
    ## 5  0.004939944 0.006725057 0.010212295 0.020406318          NA 0.022866823
    ## 6  0.004062494 0.005196821 0.007059780 0.010783620 0.022866823          NA
    ## 7  0.003370893 0.004116174 0.005202918 0.006980081 0.010606854 0.019778053
    ## 8  0.002827359 0.003333498 0.004011909 0.004992096 0.006607771 0.009291998
    ## 9  0.002513205 0.002905238 0.003407239 0.004089197 0.005113192 0.006585127
    ## 10 0.002213796 0.002512565 0.002879925 0.003352160 0.004010943 0.004864126
    ##              7           8           9          10          11          12
    ## 1  0.003370893 0.002827359 0.002513205 0.002213796 0.020111952 0.013632434
    ## 2  0.004116174 0.003333498 0.002905238 0.002512565 0.013361892 0.021458996
    ## 3  0.005202918 0.004011909 0.003407239 0.002879925 0.008542482 0.015278266
    ## 4  0.006980081 0.004992096 0.004089197 0.003352160 0.006089983 0.009219438
    ## 5  0.010606854 0.006607771 0.005113192 0.004010943 0.004727268 0.006486704
    ## 6  0.019778053 0.009291998 0.006585127 0.004864126 0.003930252 0.005090444
    ## 7           NA 0.017525619 0.009871987 0.006449279 0.003283393 0.004060797
    ## 8  0.017525619          NA 0.022604965 0.010199742 0.002768339 0.003303522
    ## 9  0.009871987 0.022604965          NA 0.018540678 0.002467517 0.002884783
    ## 10 0.006449279 0.010199742 0.018540678          NA 0.002181938 0.002502956
    ##             13          14          15          16          17          18
    ## 1  0.008642535 0.006380985 0.004780996 0.003941439 0.003270848 0.002855587
    ## 2  0.014135723 0.009355845 0.006362664 0.004981289 0.003958862 0.003369010
    ## 3  0.021283542 0.015561684 0.009199009 0.006638861 0.004945854 0.004062669
    ## 4  0.014118368 0.019525083 0.014248395 0.009460919 0.006412413 0.005026434
    ## 5  0.009013797 0.013384855 0.020040418 0.015543836 0.009055335 0.006591843
    ## 6  0.006603028 0.009000350 0.015010747 0.022493221 0.013488316 0.008999216
    ## 7  0.004990514 0.006311975 0.009273131 0.015077949 0.019257377 0.014201978
    ## 8  0.003901983 0.004683458 0.006221081 0.008693973 0.013329861 0.019209997
    ## 9  0.003333691 0.003892870 0.004916644 0.006377431 0.008974730 0.013506969
    ## 10 0.002837047 0.003236204 0.003924077 0.004814235 0.006280664 0.008590933
    ##             19          20          21          22          23          24
    ## 1  0.002473949 0.002199567 0.010214867 0.008866315 0.006829080 0.005399033
    ## 2  0.002851385 0.002493353 0.009355327 0.010055010 0.008867951 0.007080310
    ## 3  0.003334524 0.002855510 0.007427582 0.009068785 0.010312827 0.009396767
    ## 4  0.003965634 0.003308728 0.005711289 0.006989265 0.008994907 0.010473709
    ## 5  0.004898704 0.003938700 0.004593455 0.005508596 0.007167094 0.009502373
    ## 6  0.006174446 0.004735736 0.003877513 0.004551028 0.005773631 0.007668570
    ## 7  0.008663972 0.006135455 0.003267002 0.003750404 0.004599109 0.005876993
    ## 8  0.014367103 0.009012311 0.002768987 0.003117463 0.003705561 0.004541282
    ## 9  0.018940500 0.013154682 0.002473552 0.002751467 0.003207864 0.003829865
    ## 10 0.014166138 0.019944504 0.002193673 0.002412875 0.002763547 0.003222333
    ##             25          26          27          28          29          30
    ## 1  0.004435161 0.003675549 0.003188457 0.002738414 0.002425225 0.002160707
    ## 2  0.005633526 0.004501569 0.003807276 0.003191552 0.002779033 0.002438452
    ## 3  0.007414653 0.005683001 0.004655054 0.003783794 0.003225681 0.002777729
    ## 4  0.009345514 0.007182761 0.005744889 0.004530557 0.003777997 0.003184518
    ## 5  0.010616256 0.009152933 0.007339130 0.005617237 0.004557922 0.003734731
    ## 6  0.009643154 0.010272935 0.009023888 0.006953160 0.005525868 0.004391181
    ## 7  0.007449096 0.009230546 0.009936376 0.008744966 0.007059244 0.005418802
    ## 8  0.005566280 0.007012435 0.008529696 0.009743522 0.009205543 0.007070575
    ## 9  0.004570666 0.005630161 0.006885191 0.008733897 0.010031577 0.008557056
    ## 10 0.003752374 0.004502106 0.005410102 0.007007117 0.009189548 0.009971596
    ##             31          32          33          34          35          36
    ## 1  0.006520436 0.006359163 0.005334040 0.004602667 0.004036932 0.003404223
    ## 2  0.006227661 0.006765081 0.006196479 0.005498134 0.004858509 0.004034046
    ## 3  0.005532633 0.006470295 0.006715356 0.006393609 0.005856040 0.004838514
    ## 4  0.004657679 0.005538594 0.006291024 0.006596158 0.006566982 0.005640032
    ## 5  0.003972718 0.004705493 0.005575673 0.006286715 0.006893421 0.006459635
    ## 6  0.003470534 0.004061311 0.004846123 0.005623012 0.006540141 0.006812343
    ## 7  0.003001093 0.003455108 0.004085726 0.004755023 0.005648003 0.006450704
    ## 8  0.002593770 0.002937497 0.003420803 0.003939119 0.004645150 0.005518627
    ## 9  0.002341641 0.002622825 0.003017276 0.003436045 0.003998534 0.004754550
    ## 10 0.002098945 0.002325973 0.002643854 0.002976284 0.003412545 0.004025794
    ##             37          38          39          40          41          42
    ## 1  0.002964050 0.002637831 0.002334785 0.002095821 0.004899521 0.004908893
    ## 2  0.003455798 0.003034438 0.002647363 0.002348306 0.004826727 0.005112710
    ## 3  0.004081445 0.003532120 0.003030478 0.002651087 0.004532801 0.005014829
    ## 4  0.004767770 0.004098714 0.003468382 0.002995281 0.004027326 0.004536812
    ## 5  0.005613247 0.004840298 0.004044420 0.003442250 0.003577329 0.004050827
    ## 6  0.006317744 0.005598725 0.004674782 0.003937760 0.003207876 0.003620652
    ## 7  0.006649709 0.006359826 0.005481919 0.004623582 0.002832580 0.003171029
    ## 8  0.006218565 0.006649073 0.006301481 0.005523198 0.002487778 0.002757072
    ## 9  0.005508705 0.006254133 0.006524755 0.006141772 0.002265582 0.002491856
    ## 10 0.004705914 0.005526679 0.006324744 0.006674668 0.002048606 0.002236043
    ##             43          44          45          46          47          48
    ## 1  0.004425288 0.004008199 0.003490267 0.003081101 0.002787610 0.002464506
    ## 2  0.004842840 0.004555425 0.004013442 0.003529459 0.003180569 0.002784533
    ## 3  0.005049435 0.005033841 0.004577282 0.004044769 0.003646618 0.003165557
    ## 4  0.004801262 0.005099422 0.004920262 0.004465950 0.004087733 0.003554606
    ## 5  0.004427030 0.004941605 0.005104787 0.004844773 0.004566657 0.004018355
    ## 6  0.004010674 0.004588621 0.004995392 0.004987413 0.004899681 0.004432901
    ## 7  0.003524409 0.004065577 0.004589015 0.004828569 0.005010411 0.004789486
    ## 8  0.003054879 0.003510721 0.004022446 0.004390622 0.004782045 0.004920263
    ## 9  0.002748004 0.003135138 0.003591802 0.003969960 0.004412543 0.004759374
    ## 10 0.002453166 0.002775038 0.003167552 0.003524027 0.003963335 0.004445728
    ##             49          50          51          52          53          54
    ## 1  0.002231060 0.002021729 0.004045189 0.003963762 0.003704233 0.003407525
    ## 2  0.002500469 0.002247231 0.003980800 0.004078191 0.003975229 0.003727576
    ## 3  0.002819788 0.002511880 0.003796697 0.004042934 0.004131461 0.003985801
    ## 4  0.003157790 0.002797235 0.003462379 0.003774175 0.004013375 0.004005097
    ## 5  0.003575069 0.003153319 0.003151603 0.003482943 0.003813198 0.003925767
    ## 6  0.003987726 0.003522856 0.002881190 0.003198803 0.003555053 0.003738634
    ## 7  0.004445587 0.003986633 0.002591189 0.002874621 0.003215799 0.003432589
    ## 8  0.004840448 0.004520201 0.002312579 0.002554819 0.002857788 0.003074157
    ## 9  0.004929419 0.004838394 0.002126613 0.002338895 0.002607833 0.002809454
    ## 10 0.004861615 0.005116755 0.001941969 0.002125628 0.002360481 0.002543742
    ##             55          56          57          58          59          60
    ## 1  0.003136640 0.002783751 0.002551218 0.002319066 0.002112378 0.001935119
    ## 2  0.003496528 0.003106557 0.002850904 0.002580207 0.002339209 0.002130392
    ## 3  0.003851476 0.003451705 0.003186738 0.002877403 0.002599213 0.002353542
    ## 4  0.004030179 0.003699158 0.003473975 0.003154044 0.002855766 0.002579729
    ## 5  0.004120335 0.003911958 0.003769173 0.003463170 0.003156959 0.002850200
    ## 6  0.004052110 0.003989452 0.003963939 0.003714852 0.003433308 0.003112309
    ## 7  0.003811762 0.003905390 0.004033391 0.003907822 0.003712723 0.003411653
    ## 8  0.003456998 0.003662457 0.003924703 0.003968178 0.003935087 0.003719914
    ## 9  0.003165009 0.003406766 0.003720611 0.003874397 0.003982651 0.003882829
    ## 10 0.002863053 0.003120194 0.003458188 0.003704965 0.003963618 0.004030178
    ##             61          62          63          64          65          66
    ## 1  0.003283962 0.003274559 0.003196865 0.002921626 0.002776659 0.002523825
    ## 2  0.003269433 0.003337371 0.003354774 0.003128522 0.003011737 0.002758672
    ## 3  0.003186311 0.003321027 0.003439269 0.003294019 0.003227150 0.002996604
    ## 4  0.002988009 0.003158610 0.003351324 0.003309794 0.003314288 0.003149385
    ## 5  0.002791263 0.002979504 0.003220742 0.003275431 0.003354442 0.003277997
    ## 6  0.002604711 0.002793182 0.003051086 0.003172045 0.003307897 0.003322317
    ## 7  0.002389025 0.002565619 0.002817867 0.002983600 0.003160640 0.003269265
    ## 8  0.002169503 0.002327595 0.002559050 0.002744037 0.002938696 0.003119518
    ## 9  0.002016141 0.002158809 0.002369443 0.002553612 0.002745822 0.002954398
    ## 10 0.001861094 0.001988240 0.002176998 0.002355663 0.002539863 0.002765052
    ##             67          68          69          70          71          72
    ## 1  0.002339794 0.002161059 0.001989951 0.001840479 0.002848376 0.002834228
    ## 2  0.002565216 0.002369641 0.002176553 0.002007046 0.002831541 0.002892995
    ## 3  0.002804843 0.002597765 0.002383215 0.002192585 0.002772439 0.002901956
    ## 4  0.002989213 0.002793920 0.002572795 0.002370410 0.002629552 0.002802303
    ## 5  0.003170181 0.003004014 0.002785845 0.002575946 0.002486997 0.002687252
    ## 6  0.003281272 0.003164913 0.002968652 0.002764799 0.002347809 0.002557125
    ## 7  0.003314311 0.003279994 0.003138703 0.002966360 0.002181161 0.002386418
    ## 8  0.003247690 0.003314306 0.003264104 0.003160468 0.002006060 0.002198014
    ## 9  0.003124831 0.003257685 0.003286106 0.003256417 0.001880165 0.002058365
    ## 10 0.002967838 0.003162270 0.003279897 0.003349641 0.001751208 0.001914803
    ##             73          74          75          76          77          78
    ## 1  0.002779654 0.002627397 0.002479483 0.002362790 0.002143989 0.002011097
    ## 2  0.002886590 0.002776593 0.002654268 0.002549103 0.002315944 0.002179328
    ## 3  0.002947303 0.002894222 0.002813245 0.002731219 0.002492249 0.002358061
    ## 4  0.002890582 0.002901982 0.002879154 0.002837576 0.002618055 0.002503235
    ## 5  0.002808349 0.002879236 0.002918112 0.002925144 0.002739580 0.002655501
    ## 6  0.002695328 0.002808044 0.002897754 0.002950894 0.002811984 0.002768623
    ## 7  0.002530643 0.002673165 0.002806472 0.002905704 0.002832629 0.002847760
    ## 8  0.002338623 0.002495605 0.002656883 0.002791609 0.002791162 0.002874230
    ## 9  0.002191384 0.002348853 0.002517676 0.002666155 0.002712009 0.002839791
    ## 10 0.002038900 0.002193650 0.002365301 0.002522539 0.002612578 0.002784608
    ##             79          80         101         201         301         401
    ## 1  0.001871677 0.001743387 0.001991793 0.001982628 0.001977997 0.001905514
    ## 2  0.002025652 0.001884286 0.002230397 0.002218705 0.002209069 0.002114794
    ## 3  0.002191534 0.002037657 0.002515174 0.002501327 0.002485055 0.002361184
    ## 4  0.002335099 0.002177523 0.002868054 0.002845126 0.002809251 0.002639722
    ## 5  0.002492043 0.002335095 0.003336976 0.003302433 0.003235023 0.002995758
    ## 6  0.002620932 0.002474097 0.003907140 0.003852347 0.003728473 0.003389557
    ## 7  0.002734790 0.002615330 0.004868392 0.004756076 0.004482725 0.003948799
    ## 8  0.002815898 0.002745879 0.006739585 0.006435220 0.005699848 0.004752535
    ## 9  0.002829117 0.002808095 0.009596580 0.008734286 0.006964944 0.005466948
    ## 10 0.002830330 0.002873767 0.019858679 0.015015277 0.009002195 0.006467223
    ##            501         601         701         801
    ## 1  0.001870649 0.001780947 0.001707354 0.001636074
    ## 2  0.002065509 0.001949237 0.001854575 0.001765264
    ## 3  0.002291782 0.002140295 0.002018411 0.001906596
    ## 4  0.002536964 0.002336719 0.002179220 0.002040098
    ## 5  0.002842010 0.002572819 0.002367323 0.002193144
    ## 6  0.003163085 0.002807899 0.002546495 0.002334230
    ## 7  0.003584801 0.003094031 0.002752168 0.002489673
    ## 8  0.004124336 0.003427827 0.002977032 0.002652977
    ## 9  0.004533910 0.003657626 0.003121454 0.002753832
    ## 10 0.005038867 0.003934616 0.003294951 0.002876926

on to models