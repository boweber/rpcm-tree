adjusted_rand_index <- function(tree,
                                cutpoint,
                                lrcutpoint,
                                tree_did_find_dif,
                                glmer_did_find_dif) {
    ## original data
    initial_covariate <- tree[[1]]$data$covariate

    if (tree_did_find_dif) {
        numeric_cutpoint <- quantile(initial_covariate, cutpoint)
        ## compute the ari for the tree

        rpcm_group_ids <- function(tree_node, group_id) {
            if (group_id == 0) {
                ## reference group
                predicted_group <- tree_node$data$covariate < numeric_cutpoint

                ## TRUE == 1, which is not the desired group_id 0. Therefore
                ## use the inverse of the result.
                predicted_group <- !predicted_group
            } else {
                predicted_group <- tree_node$data$covariate >= numeric_cutpoint
            }
            return(data.frame(
                actual_group = as.factor(
                    rep.int(group_id, length(tree_node$data$covariate))
                ),
                predicted_group = as.factor(as.numeric(predicted_group))
            ))
        }

        reference_group <- rpcm_group_ids(tree[[2]], 0)
        focal_group <- rpcm_group_ids(tree[[3]], 1)
        ## combines focal and reference group
        tree_groups <- rbind(reference_group, focal_group)
        ## computes the ari
        tree_result <- mclust::adjustedRandIndex(
            tree_groups$predicted_group,
            tree_groups$actual_group
        )
    } else {
        tree_result <- 0
    }

    if (glmer_did_find_dif) {

        ## Divides the original covariate in focal and reference group based
        ## on a cutpoint.
        ## Returns a by-id sorted vector containing group-ids, which
        ## idenitify the reference and focal group.
        ## 0 == reference group
        ## 1 == focal group
        static_group_ids <- function(quantile_prob) {
            covariate_split <- quantile(initial_covariate, quantile_prob)
            group_ids <- initial_covariate < covariate_split
            group_ids <- as.factor(as.numeric(group_ids))
            return(group_ids)
        }

        ## compute the ari for LR
        glmer_result <- mclust::adjustedRandIndex(
            static_group_ids(lrcutpoint),
            static_group_ids(cutpoint)
        )
    } else {
        glmer_result <- 0
    }

    return(data.frame(
        tree_ari = tree_result,
        glmer_ari = glmer_result
    ))
}