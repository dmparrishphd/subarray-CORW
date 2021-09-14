# formals ( arrayInd )

arrayInds <- function ( .dim )
        arrayInd (
            ind = seq_len ( prod ( .dim ) ) ,
            .dim = .dim )

        arrayInds ( c ( 2,2,2))

.subArrayInds <- function ( X , i, .dim ) ! X [ , .dim ] %in% i

        .subArrayInds (
            arrayInds ( c ( 2, 2, 2 ) ) ,
            1 ,
            1 )

        .subArrayInds (
            arrayInds ( c ( 2, 2, 2 ) ) ,
            1 ,
            2 )

        .subArrayInds (
            arrayInds ( c ( 2, 2, 2 ) ) ,
            2 ,
            3 )

subArrayInds <- function ( .dim , i ) {
    mi <- arrayInds ( .dim )
    ii <- vapply (
        seq_along ( i ) ,
        function ( k ) .subArrayInds ( mi , i [[ k ]] , k ) ,
        logical ( nrow ( mi ) ) ,
        USE.NAMES = FALSE )
    k <- vapply (
        seq_len ( nrow ( ii ) ) ,
        function ( i ) all ( ii [ i , ] ) ,
        NA ,
        USE.NAMES = FALSE )
    mi [ k , ] }

subArrayInds ( c ( 2 , 2 , 2 ) , list ( 1 , NULL , NULL ) )

subArrayInds ( c ( 3 , 2 , 2 ) , list ( 2:3 , NULL , NULL ) )

subarray <- function ( 
