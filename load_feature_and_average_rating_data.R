graphics.off()

#First we load up the packages we need and set knitr options:

if (!require("pacman")) install.packages("pacman")
pacman::p_load(knitr, randomForest, dplyr, tidyverse, doParallel, xtable, pracma, yaml)

opts_knit$set(fig.width = 3, fig.height = 3)
 


Xraw = rbind(
  read_csv("feature_and_average_rating_data1.csv.bz2"),
  read_csv("feature_and_average_rating_data2.csv.bz2")
)

#now we have missing data of which we can do one of two things
#1) We can drop the row with the missing data (losing n)
#2) We can drop the column with the missing data (losing p)
#The decision is arbitrary and made on a case-by-case basis. Below is missingness by column:

#num_NA_by_col = apply(Xraw, 2, function(xj){sum(is.na(xj))})
#names(num_NA_by_col) = colnames(Xraw)
#sort(num_NA_by_col[num_NA_by_col > 0])
#anything more than 1000 missing rows, we drop column
#for (j in 1 : ncol(Xraw)){
#	if (num_NA_by_col[j] > 1000){
#		Xraw[, names(num_NA_by_col)[j]] = NULL
#	}
#}
#anything less we drop row
#n_before = nrow(Xraw)
#context_ids_before = Xraw[, "context_id"]
X = na.omit(Xraw)
#context_ids_after = X[, "context_id"]


#num_NA_by_col = apply(X, 2, function(xj){sum(is.na(xj))})
#names(num_NA_by_col) = colnames(X)
#sort(num_NA_by_col[num_NA_by_col > 0])

#num_rows_lost_missing = n_before - nrow(X)
#cat(num_rows_lost_missing / n_before * 100, "% missingness\n", sep = "")
##make sure dims are reasonable
#dim(X)
#str(X, list.len = ncol(X))


#find what's missing
#context_ids_missing = setdiff(context_ids_before, context_ids_after)
#missings = Xraw[Xraw[, "context_id"] %in% context_ids_missing, ]
#dim(missings)
#tab = table(missings$target_word.x)
#tab[tab > 0] #no pattern


#now let's make sure to drop features with only one value
num_vals = array(NA, ncol(Xraw))
names(num_vals) = colnames(Xraw)
for (j in 1 : ncol(Xraw)){
	num_vals[j] = length(table(Xraw[, j]))
}
sort(num_vals)
for (j in names(num_vals[num_vals == 1])){
	X[, j] = NULL
}

#get some data out
wc = X$"num_words"

#now we start chopping off data based on things that are illegal
hist(wc, br = 1000)
X = X[wc <= 65 & wc >= 42, ]
wc = X$"num_words"
hist(wc, br = 1000)
dim(X)



#now we get a dataframe of only features

features = setdiff(colnames(X), c(
				"mean_rating",
				"inappropriate_count",
				"word",
				"target_word",
				"last_timestamp",
				"domain",
				"band",
				"context_id",
				"nwords" #duplicate feature
		))
sort(features)
length(features)

#kill some target words
word = as.factor(as.character(X$"target_word"))
sort(table(word), decreasing = TRUE)
MIN_NUM_SNIPPETS = 20
kill_words = names(table(word)[table(word) < MIN_NUM_SNIPPETS])
X = X %>%
  filter(!(word %in% kill_words))
word = as.factor(as.character(X$"target_word"))
sort(table(word), decreasing = TRUE)
dim(X)
n = nrow(X)

#pull out y and get a histogram
y = X$mean_rating

### we need to report all y's without the inappropriate.

y_without_inapp = y[y > 0]
ggplot(data.frame(y_without_inapp)-2, aes(y_without_inapp, fill = "y_without_inapp")) + 
		geom_density(alpha = 0.2) + 
		theme(legend.position="none") +
		xlab("Average Rating")

#common cutpoint to have around
medy = median(y)

# #and we want word dummified
# word_dummied = do.call(cbind, lapply(levels(word), function(lev){as.numeric(word == lev)}))
# #ensure they're named appropriately
# colnames(word_dummied) = paste("word", levels(word), sep = "_")

###some investigation into words


#word_by_count = table(word)
#sort_idx_for_num_words = sort(as.numeric(word_by_count), index.return = TRUE)$ix
#word_by_count_sorted = word_by_count[sort_idx_for_num_words]
#word_by_count_sorted[1:50]
#
#top_pcts = round(table(word[y>3])/ table(word), 2)
#word_by_count_top = table(word[y>3])
#hist(top_pcts, br = 1000)
#
#pdf("num_contexts_and_pct_rated_high_per_word.pdf", width = 60, height = 5, paper = 'special') 
#barplot(word_by_count_sorted, names = names(word_by_count_sorted), las = 2, cex.names = 0.4, col = "blue", ylim = c(0, 100), ylab = "Num contexts (blue), pct y > 3 (red)")
#barplot(word_by_count_sorted * 0.2, names = NULL, col = "white", yaxt = "n", xaxt = "n", add = TRUE)
#barplot(word_by_count_top[sort_idx_for_num_words], names = NULL, col = "red", yaxt = "n", xaxt = "n", add = TRUE)
#dev.off()  

#also binarize y
y_bin_med = as.factor(ifelse(y > medy, 1, 0))
table(y_bin_med)

y_bin_top = as.factor(ifelse(y > 3, 1, 0))
table(y_bin_top)


#word_to_cutoff = list()
#TOP_P_PCT = 0.2
#for (w in names(word_by_count)){
#	word_to_cutoff[[w]] = quantile(y[word == w], 1 - TOP_P_PCT)
#}
#sort(unlist(word_to_cutoff))
#y_bin_top_by_word = array(NA)
#for (i in 1 : nrow(Xdf)){
#	y_bin_top_by_word[i] = as.numeric(y[i] > max(word_to_cutoff[[word[i]]], median(y)))
#}
#y_bin_top_by_word = as.factor(y_bin_top_by_word)
#table(y_bin_top_by_word)

###get a matrix of features only
Xdf = as.data.frame(X[, features])
str(Xdf, list.len = ncol(Xdf))
dim(Xdf)

target_word = X$target_word
tab_words = table(target_word)
words = names(tab_words[tab_words > 0])
n_words = length(words)

table(X$inappropriate_count)
y_inapprop = factor(ifelse(X$inappropriate_count > 0, 1, 0))
table(y_inapprop)
y_approp = factor(ifelse(X$inappropriate_count > 0, 0, 1))
table(y_approp)

context_ids = X$context_id
band = X$band

word_to_band = list()
for (i in 1 : nrow(X)){
	word_to_band[[as.character(target_word[i])]] = band[i]
}
word_to_band

num_ex_by_band = array(0, 10)
for (i in 1 : nrow(X)){
	num_ex_by_band[band[i]] = num_ex_by_band[band[i]] + 1
}
num_ex_by_band


num_words_by_band = array(0, 10)
for (i in 1 : length(words)){
	num_words_by_band[word_to_band[[words[i]]]] = num_words_by_band[word_to_band[[words[i]]]] + 1
}
num_words_by_band

sd_by_band = array(0, 10)
for (i in names(band)){
	
	sd_by_band[word_to_band[[words[i]]]] = num_words_by_band[word_to_band[[words[i]]]] + 1
}
sd_by_band


c1 = data.frame(table(word))
colnames(c1) = c("word", "# contexts")
c2 = data.frame(names(unlist(word_to_band)), unlist(word_to_band))
colnames(c2) = c("word", "band")
word_band_num_contexts = merge(c1, c2, by = "word")
#print(word_band_num_contexts[order(word_band_num_contexts$band), ], row.names = FALSE)


y_and_band = data.frame(y - 2, band)
colnames(y_and_band)[1] = "y"
y_and_band_ecdf = y_and_band %>% 
  group_by(band) %>%
  summarise(
		ylessthan0 = ecdf(y)(0),
		ybetween0andpt5 = ecdf(y)(0.5) - ecdf(y)(0),
		ygreater1 = 1 - ecdf(y)(1))
xtable(t(y_and_band_ecdf), digits = 2)


y_and_band_num = y_and_band %>% 
  group_by(band) %>%
  summarize(
    ylessthan0 = sum(y < 0),
    ybetween0andpt5 = sum(y >= 0 & y < 0.5),
    ygreater1 = sum(y > 1)
  )
xtable(t(y_and_band_num), digits = 2)
