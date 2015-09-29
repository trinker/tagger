tic()
out <-tag_pos(presidential_debates_2012$dialogue)#7.146037 mins
toc()


grp <- as.list(presidential_debates_2012[, c("person", "time")])


tag_pos("I need $54 to go to the movies.")

out %>%
    select_tags(c("NN", "NNP", "NNPS", "NNS"), TRUE) %>%
    count_pos(presidential_debates_2012[, c("person", "time")])

out %>%
    select_tags(c("NN", "NNP", "NNPS", "NNS")) %>%
    count_pos(presidential_debates_2012[, c("person", "time")])


out %>%
    select_tags(c("NN", "NNP", "NNPS", "NNS")) %>%
    count_pos()


out %>%
    select_tags(c("NN", "NNP", "NNPS", "NNS")) %>%
    as_tag




zz<- count_pos(out, list(presidential_debates_2012$person, presidential_debates_2012$time))
zz[1:20, 1:20]

length(presidential_debates_2012$person)
presidential_debates_2012[which(sapply(sapply(out, is.na), all)), ]

grep("^\\s+$", presidential_debates_2012$dialogue)

which(sapply(presidential_debates_2012$dialogue, nchar) < 3)

n<-1762;out[n];presidential_debates_2012$dialogue[n]


out[which(as.logical(zz[["-RRB-"]]))]
out[c(1096, 2103)]
x[18]
out[18]
x <- raj$dialogue
x <- sample(x, 30022, TRUE)
length(x)
x <- DATA$state
x[1:3] <- c("hello; i want-to. Hence-banished is banished. I see thee.", NA, "We don't have to settle for twenty three million people struggling to find a good job.")
length(x)


