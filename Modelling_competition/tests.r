
eraar <- tryCatch({"aaa" + 1})
print(eraar)


roc1 <- Epi::ROC(form=formula(models[[1]]), data=Train, plot="ROC", lw=3, cex=1.5)
cutoff_vector <- roc1$res$lr.eta

# train set response
train_response <- Train %>% dplyr::select(response) 
levels(train_response)      
            
# prediction
scr <- c()
for (k in 1:length(cutoff_vector)) {
    # prediction using cutoff #k
    pred <- predict(models[[i]], newdata=Train, type="response")
    pred <- ifelse(pred > cutoff_vector[k], 1, 0)
    pred <- as.factor(pred)

    # confusion matrix score
    acc <- try(confusionMatrix(pred, train_response)$overall[1], silent=T)
    scr <- c(scr, acc)
}

# best model
best_k <- match(max(scr), scr)
print(best_k)

# prediction using BEST cutoff
pred <- predict(models[[i]], newdata=Test, type="response")
pred <- ifelse(pred > cutoff_vector[best_k], 1, 0)
pred <- as.factor(pred)

# target score 
real_vals <-  Test %>% dplyr::select("response")

# confusion matrix score
accuracy <- c(accuracy,confusionMatrix(pred, real_vals)$overall[1])


        # for (k in 1:length(cutoff_vector)) {
        #     # prediction using cutoff #k
        #     pred <- predict(models[[i]], newdata=data, type="response")
        #     pred <- ifelse(pred > cutoff_vector[k], 1, 0)
        #     pred <- as.factor(pred)

        #     # confusion matrix score
            
        #     acc <- try(confusionMatrix(pred, train_response)$overall[1], silent=T)
        #     scr <- c(scr, acc)
        # }


        # # best model
        # best_k <- match(max(scr), scr)
