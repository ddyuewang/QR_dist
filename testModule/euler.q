////////////////////////////////////////////////////////
// euler.q - an example script to demonstrate loading.
///

// solving https://projecteuler.net/problem=18
.testModule.maxiPathSum:{
    first ({y+1_(|':) x}/) reverse x
    };
