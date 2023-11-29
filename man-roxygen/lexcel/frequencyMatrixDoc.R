#' @details
#' Let \eqn{N}{N} be a set of elements, \eqn{\succsim \in \mathcal{T}(\mathcal{P})}{>= in T(P)} a power relation,
#' and \eqn{\Sigma_1 \succ \Sigma_2 \succ \dots \succ \Sigma_m}{E_1 > E_2 > ... > E_m} its corresponding quotient order.
#'
#' For an element \eqn{i \in N}{i in N}, construct a matrix \eqn{M^\succsim_i}{M^(>=)_i} with \eqn{m}{m} columns and \eqn{|N|}{|N|} rows.
#' Whereas each column \eqn{q}{q} represents an equivalence class, each row \eqn{p}{p} corresponds to the coalition size.
#'
#' \deqn{(M^\succsim_i)_{p,q} = |\lbrace S \in \Sigma_q: |S| = p \text{ and } i \in S\rbrace|}{(M^(>=)_i)_pq = |\{S in E_q: |S| = p and i in S\}}
