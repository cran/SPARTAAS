\name{hclustcompro_select_alpha}
\alias{hclustcompro_select_alpha}
\title{
Estimate the optimal value(s) of the \eqn{\alpha} parameter.}
\description{
The following criterion "balances" the weight of \eqn{D_1} and \eqn{D_2} in the final clustering. The \eqn{\alpha} value is only a point estimate but the confidence interval gives a range of possible values.
\cr\cr
Based on a resampling process, we generate clones and recalculate the criteria according to \eqn{\alpha} (see below).}

\usage{
hclustcompro_select_alpha(
    D1,
    D2,
    acc=2,
    resampling=TRUE,
    method="ward.D2",
    iter=5,
    suppl_plot=TRUE
)
}
\arguments{
  \item{D1}{
    First dissimilarity matrix or contingency table (square matrix).
  }
  \item{D2}{
    Second dissimilarity matrix or network data (square matrix) of the same size as D1.
  }
  \item{acc}{
    Number of digits after the decimal point for the alpha value.
  }
  \item{resampling}{
    Logical for estimating the confidence interval with a resampling strategy. If you have a lot of data, you can save computation time by setting this option to FALSE.
  }
  \item{method}{
    The agglomeration method to be used. This should be (an unambiguous abbreviation of) one of "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
  }
  \item{iter}{
    The number of clones checked per observation. (200 observations iter=1: ~30 sec, 1000 observations iter=1: ~40 min).
  }
  \item{suppl_plot}{
    Logical defines whether additional plots should be displayed.
  }
}
\details{
\strong{Definition of the criterion:}\cr\cr

A criterion for choosing \eqn{\alpha \in [0;1]} must be determined by balancing the weights between the two sources of information in the final classification. To obtain \eqn{\alpha}, we define the following criterion
\deqn{CorCrit_\alpha = |Cor(dist_{cophenetic},D_1) - Cor(dist_{cophenetic},D_2)|}  \deqn{Equation (1)}
The \eqn{CorCrit_\alpha} criterion in (1) represents the difference in absolute value between two cophenetic correlations (cophenetic correlation is defined as the correlation between two distance matrices. It is calculated by considering the half distance matrices as vectors. It measures how faithfully a dendrogram preserves the pairwise distances between the original unmodeled data points). The first correlation is related to the comparison between \eqn{D_1} and the ultrametric distances of the clustering with \eqn{\alpha} fixed, while the second compares \eqn{D_2} and the ultrametric distances of the clustering with \eqn{\alpha} fixed. Then, in order to compromise between the information provided by \eqn{D_1} and \eqn{D_2}, we decided to estimate \eqn{\alpha} with \eqn{\hat{\alpha}} such that:
\deqn{\hat{\alpha} = min CorCrit_\alpha}
\deqn{Equation (2)}

\strong{Resampling strategy:}\cr\cr

This is done by creating a set of "clones" for each observation \eqn{i}. A clone \eqn{c} of observation \eqn{i} is a copy of observation \eqn{i} for which the distances from the second source have been modified. The modification is made by copying the distances for the second source from another observation \eqn{j}. A clustering is then performed using the combination defined in (1) with \eqn{D_1^{(c)}} an \eqn{(n+1)\times(n+1)} matrix where observations \eqn{i} and \eqn{c} are identical and \eqn{D_2^{(c)}} an \eqn{(n+1)\times(n+1)} matrix where the clone \eqn{c} of \eqn{i} has different distances from those of \eqn{i}. A set of clones is generated by varying \eqn{j} for all observations except \eqn{i}. We can generate a set of \eqn{n-1} clones for each element \eqn{i} in \eqn{n}, so \eqn{n(n-1)} clones in total.

Intuitively, by varying \eqn{\alpha} between 0 and 1, we will be able to identify when the clone and the original observation are separated on the dendrogram. This moment will correspond to the value of alpha above which the weight given to the information about the connection between observations contained in \eqn{D_2} has too much influence on the results compared to that of \eqn{D_1}.

Let \eqn{CorCrit_\alpha^{(c)}} define the same criterion as in (1), where \eqn{D_1} and \eqn{D_2} are replaced by \eqn{D_1^{(c)}} and \eqn{D_2^{(c)}} respectively.
The estimated \eqn{\alpha} is the mean of the estimated values for each clone.\cr
For each clone \eqn{c}:
\deqn{\hat{\alpha}^{(c)} = min   CorCrit_\alpha^{(c)}}
\deqn{Equation (3)}
\eqn{\hat{\alpha}^*} is the mean of \eqn{\hat{\alpha}^{(c)}}. In the same spirit as confidence intervals based on bootstrap percentiles (Efron & Tibshirani, 1993), a percentile confidence interval based on replication is also be obtained using the empirical percentiles of the distribution of \eqn{\hat{\alpha}^{(c)}}.
\deqn{\hat{\alpha}^* = \frac{1}{n(n-1)} \sum{ \hat\alpha^{(c)} }}
\deqn{Equation (4)}
\deqn{c  \in [1; n(n-1)]}

\strong{Warnings:}\cr\cr
It is possible to observe an \eqn{\alpha} value outside the confidence interval. In some cases, this problem can be solved by increasing the number of iterations or by changing the number of axes used to construct the matrix D1 after the correspondence analysis. If the \eqn{\alpha} value remains outside the interval, it means that the data are noisy and the resampling procedure is affected.
}


\value{
The function returns a list (class: selectAlpha_obj).
  \item{alpha }{The estimated value of the alpha parameter (min CorCrit_alpha)}
  \item{alpha.plot }{The CorCrit curve for alpha between 0 and 1}
If resampling = TRUE
  \item{sd }{The standard deviation}
  \item{conf }{The confidence interval of alpha}
  \item{boxplot }{The boxplot of alpha estimation with resampling}
  \item{values }{All potential alpha values obtained from each clone}
%%  \item{comp2 }{Description of 'comp2'}
}

\author{
A. COULON

L. BELLANGER

P. HUSI
}


\examples{
#################################

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
library(SPARTAAS)
data(datangkor)

#network stratigraphic data (Network)
network <- datangkor$stratigraphy

#contingency table
cont <- datangkor$contingency

dissimilarity <- CAdist(cont,nPC="max",graph=FALSE)
constraint <- adjacency(network)

hclustcompro_select_alpha(D1 = dissimilarity, D2 = constraint)
hclustcompro_select_alpha(D1 = dissimilarity, D2 = constraint, acc = 3, resampling = TRUE)

}
