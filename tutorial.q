////////////////////////////////////////////////////////////////////////////////
// qinfra - loading, dependency
////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////
// loading
/////////////////////////////////////
// load testModule. to correct setup a module.
// 1. setup meta.q 2. setup module.q

.qr.loadModule["testModule"];

.testModule.maxiPathSum[
    (75f;
    95 64f;
    17 47 82f;
    18 35 87 10f;
    20 04 82 47 65f;
    19 01 23 75 03 34f;
    88 02 77 73 07 63 67f;
    99 65 04 28 06 16 70 92f;
    41 41 26 56 83 40 80 70 33f;
    41 48 72 33 47 32 37 16 94 29f;
    53 71 44 65 25 43 91 52 97 51 14f;
    70 11 33 28 77 73 17 78 39 68 17 57f;
    91 71 52 38 17 14 91 43 58 50 27 29 48f;
    63 66 04 68 89 53 67 30 73 16 69 87 40 31f;
    04 62 98 27 23 09 70 98 73 93 38 53 60 04 23f)]

// you can also load the single file inside a module
.qr.include ("testModule"; "euler.q");

/////////////////////////////////////
// dependency
/////////////////////////////////////
// you can also add dependency into other projects, even itself
// and later load the module based on the dependency

.qr.addDep["QR_dist"; "{PATH to your QR_dist}"]
.qr.listDep[]
.qr.loadModule[("QR_dist";"testModule")]
.qr.include (("QR_dist";"testModule"); "euler.q");

////////////////////////////////////////////////////////////////////////////////
// env - execution control, logging, param, os, type, R
////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////
// os info
/////////////////////////////////////
// following functions give you the information on the OS level

.qr.host[]
.qr.ipAddr[]
.qr.pid[]
.qr.pwd[]

/////////////////////////////////////
// exeuction control
/////////////////////////////////////
// this module provides Q execution control
// we can check if "something" exists both in memory or disk level
// try catch.. and more

// throw an error (will log)
.qr.throw "error"

// check if defintion exist
.qr.exist `:testModule // on disk level
.qr.exist `.qr.exist // in memory

// the following provides function definition.
// however it will not work on dyanmic library since it is sealed
testFunc:{x+y}
.qr.getFuncDef[`testFunc]
.qr.getFuncDef[`.qr.exist] // throw "sealed"

// this is the try-catch exeuction in Q/KDB+, the wrapper will automatically
// detect how many variables inside your function and use @ or . appropriately
.qr.trycatch[{x+y};(1;2);{'x}]
.qr.trycatch[{[x;y] '`hello};(1;2);{show "error catch: ", .qr.toString[x]}]
.qr.trycatch[{'`error};enlist (::);{show "error catch: ", .qr.toString[x]}]
.qr.trycatch[{[x] '`error};enlist 1;{show "error catch: ", .qr.toString[x]}]

// when exeucting dynamic libary, the arguments must be a list
.qr.trycatch[`.qr.exist;enlist `qr.exist;{'x}]

/////////////////////////////////////
// logging
/////////////////////////////////////
// following gives you the logging ability, by default it points to
// stdout and sterr. you can redirect it yourself. everyting is async
// exeuction overhead is minimal

.qr.setSeverity[`INFO] // set log level. SILENT<DEBUG<INFO<WARN<ERROR<FATAL
.qr.setSeverity[`ERROR]
.qr.addLogHandle["{Your Favorate logger directory}/test.log";`SILENT`DEBUG`INFO];
.qr.addLogHandle["{Your Favorate logger directory}/test.log/testErr.log";`WARN`ERROR`FATAL];
.qr.setLogConsole[] // this will default all loging to console (no logging)
.qr.removeLogHandleAll[] // this will remove all log handles you registered

.qr.console "info" // logging functions
.qr.debug "debug"
.qr.silent "silent"
.qr.warn "warning"
.qr.throw "error"
.qr.fatal "fatal error"

/////////////////////////////////////
// param
/////////////////////////////////////
// this is the parameter framework utilize the Q custom parameter
// all params past from command lines will be injected here
// for example, q -p 8080 -test world

.qr.setParams[.qr.param[`test; `$"hello"]]; // to register default parameter
.qr.listParams[]
.qr.getParam[`test] // if you pass in the parameter earlier, this would be "world" instead

/////////////////////////////////////
// embeddedR wrapper
/////////////////////////////////////
// this is the R wrapper to call embededR
// you must configure the Rserver directory instead to use it
// or just use default embeded R (might be easier)

.qr.R.open[]
.qr.R.eval "a=array(1:24,c(2,3,4))"
.qr.R.get "dim(a)"
.qr.R.get "a"
.qr.R.set["xyz";1 2 3i]
.qr.R.get "xyz"

.qr.R.install["Matrix"];
.qr.R.include["Matrix"];
.qr.R.include["lpSolve"];

.qr.R.set["A";.qr.quant.mat.rand[10;10;`.qr.quant.rng.rand]];
.qr.R.eval "M <- matrix(unlist(A), ncol=10, byrow=TRUE)";
.qr.R.get "as.list(M)";
.qr.R.eval "res <- expand(lu(M))"
`P`L`U!(.qr.R.get "as.list(res$P)";.qr.R.get "as.list(res$L)";.qr.R.get "as.list(res$U)")

.qr.R.get "as.list(res$P)"
.qr.R.get "split(M, rep(1:ncol(M), each = nrow(M)))";
.qr.R.get "split(M, col(M))"

/////////////////////////////////////
// type
/////////////////////////////////////
// the following provide type conversion between Q/KDB+
// it includes bytes and bits conversion (if supported in Q)

.qr.toString[`hello]
.qr.toString (`hello;`world)
.qr.toSymbol ("hello";"world")
.qr.mergeSym[`hello;`world]
.qr.listTypes[]
.qr.toBits[1]
.qr.bitsTo["j"] .qr.toBits[1]
.qr.toBytes each "hello" 
.qr.bytesTo["c"] each .qr.toBytes each "hello"
.qr.bytesTo["j"] .qr.toBytes 123i

// this convert the bits, tri-bits, qua-bits.... to base 10
.qr.toBase[2;(101b)]
.qr.toBase[3;(1 2 1)]

////////////////////////////////////////////////////////////////////////////////
// util - reflect, qTracer, remote (rpc), schema, shimming, table, timer
////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////
// remote
/////////////////////////////////////
// this is a remote rpc procedual call which utilize the powerful interprocess
// comunication inside Q
// it is known problem that the handle might be stale sometime in native Q
// this wrapper safely open, execute and close handle

.qr.util.remote.rpc["localhost:26041"] "show `hello"
.qr.util.remote.arpc["localhost:26041"] "show `hello"

/////////////////////////////////////
// reflect
/////////////////////////////////////
// this is a reflection module to list all Q types defined in memory (recursively)

.qr.util.ns.ls[`.testModule] // the testModule we loaded earlier
// here we try adding more functions inside the testModule
.testModule.hello:{ show "hello"};
.testModule.nested.hello:{show "hello, world"};
// magic happen :), we list all the modules definition recursively
.qr.util.ns.lsr[`.testModule]

// notice that for dynamic library, since implementation is not visible to
// outside user. it will return `sealed. if it is a projection on dynamic
// library, it will instead return "projection on sealed"
.qr.util.ns.ls[`.qr]
.qr.util.ns.lsr[`.qr]
.qr.util.ns.ls[`.qr.util]
.qr.util.ns.ls[`.qr.quant.complex]

/////////////////////////////////////
// memmoize
/////////////////////////////////////

// this is a memoize module which will cache the function calls
// internal memory management is done by integer programming optimisation
squareMatrix:{[n]
    (n;n)#til n*n
    };

squareMatrix2:{[n]
    10*(n;n)#til n*n
    };

.qr.util.mem.clear[]
.qr.util.mem.setMax[5000] //set to 5KB
.qr.util.mem.list[]
.qr.util.mem.adjust[0]
exec sum size from .qr.util.mem.list[]

.qr.util.mem.memoize[`squareMatrix] each 1+til 10;
.qr.util.mem.memoize[`squareMatrix;5];
.qr.util.mem.memoize[`squareMatrix;1];
.qr.util.mem.memoize[`squareMatrix2] each 1+til 20;
.qr.util.mem.memoize[`squareMatrix] 10

/////////////////////////////////////
// qSchema
/////////////////////////////////////
// In real life, we must be pissed when upstream system simply modifies their schema without
// telling us. This module provides you a schema to automatically format table

testTbl:([] Continent:`NorthAmerica`Asia`Asia`Europe`Europe`Africa`Asia`Africa`Asia; // first lets create a random table
	 Country:("US";"China";"japan";"Germany";"UK";"Zimbabwe";"Bangladesh";"Nigeria";"Vietnam");
	 Population:313847 1343239 127938 81308 63047 13010 152518 166629 87840;
	 GDP:15080.0 11300.0 4444.0 3114.0 2228.0 9.9 113.0 196.0 104.0;
	 GDPperCapita:`48300`8400`34700`38100`36500`413`1788`732`3359 ;
	 LifeExpectancy:`77.14`72.22`80.93`78.42`78.16`39.01`61.33`51.01`70.05)

.qr.listTypes[] // check what kind of types are avaialbe

// here we register different schema
.qr.util.schema.addTbl[`sampleTbl;
    .qr.util.schema.addCol[`ticker;"symbol"],
    .qr.util.schema.addCol[`tradetime;"datetime"],
    .qr.util.schema.addCol[`price;"float"],
    .qr.util.schema.addCol[`size;"float"],
    .qr.util.schema.addCol[`lastFivePrices;"floats"], // here we put floats ---> automatically convert this column singular value into list value
    .qr.util.schema.addCol[`ammendment;"chars"]
    ];

.qr.util.schema.addTbl[`test1;
    .qr.util.schema.addCol[`Continent;"symbol"],
    .qr.util.schema.addCol[`Country;"symbol"],
    .qr.util.schema.addCol[`Population;"symbol"],
    .qr.util.schema.addCol[`GDP;"symbol"],
    .qr.util.schema.addCol[`GDPperCapita;"symbol"],
    .qr.util.schema.addCol[`LifeExpectancy;"symbol"]
    ];

.qr.util.schema.addTbl[`test2;
    .qr.util.schema.addCol[`Continent;"symbol"],
    .qr.util.schema.addCol[`Country;"symbols"], // list conversion also works on symbols
    .qr.util.schema.addCol[`Population;"float"],
    .qr.util.schema.addCol[`GDP;"floats"],
    .qr.util.schema.addCol[`GDPperCapita;"float"],
    .qr.util.schema.addCol[`LifeExpectancy;"floats"]
    ];

.qr.util.schema.addTbl[`test3;
    .qr.util.schema.addCol[`Continent;"symbol"],
    .qr.util.schema.addCol[`Country;"symbols"],
    .qr.util.schema.addCol[`Population;"float"],
    .qr.util.schema.addCol[`GDP;"floats"],
    .qr.util.schema.addCol[`GDPperCapita;"float"],
    .qr.util.schema.addCol[`LifeExpectancy;"float"]
    ];

.qr.util.schema.addTbl[`test4;
    .qr.util.schema.addCol[`Continent;"char"],
    .qr.util.schema.addCol[`Country;"char"],
    .qr.util.schema.addCol[`Population;"int"],
    .qr.util.schema.addCol[`GDP;"int"],
    .qr.util.schema.addCol[`GDPperCapita;"int"],
    .qr.util.schema.addCol[`LifeExpectancy;"float"]
    ];

// ablove schema codes might be annoying. one can simply get the code
// from existing table. cool right?
.qr.util.schema.getSchemaCodes[testTbl]

// we can create an empty table from schema registered
.qr.util.schema.getEmptyTbl[`sampleTbl]
.qr.util.schema.getEmptyTbl[`test3]
.qr.util.schema.getEmptyTbl[`test4]

// now lets try format the table from our schemas. neat...
.qr.util.schema.formatTbl[`test1;testTbl]
.qr.util.schema.formatTbl[`test2;testTbl]
.qr.util.schema.formatTbl[`test3;testTbl]
.qr.util.schema.formatTbl[`test4;testTbl]

/////////////////////////////////////
// list
/////////////////////////////////////
// here we provides some common functionality of list operation

.qr.util.list.enlist[1] // this will check if it is a list, enlist if not; do nothing otherwise
.qr.util.list.enlist[(1;2)]

// join and fill
.qr.util.list.join[(1;2;3);(10;20;30)] // join the list each by each
.qr.util.list.fill[(0n;0n;0n;0n;3;4;0n;0n;5;6)] // fills the nulls two way

// slice the list into bins givne the size of each bin
.qr.util.list.slice[til 10; (1;3;6)]
.qr.util.list.slice[til 10; (1;3;5;6;8;10)]

// this instead slice the list into bins by even size
.qr.util.list.bin[2;10;5]

// reshape and dim
.qr.util.list.reshape[til 2*3*4; (4;3;2)] // remember reshape in python? here it is in Q
.qr.util.list.dim .qr.util.list.reshape[til 2*3*4; (4;3;2)] // dimension of your (nested list)
.qr.util.list.dim ((0 1j;2 3j;4 5 6j);(6 7j;8 9j;10 11j);(12 13j;14 15j;16 17j);(18 19j;20 21j;22 23j)); // dimension not matched. should fail


/////////////////////////////////////
// table
/////////////////////////////////////
// the followings gives some short cut to Q table operation
// safe left join, union join, prepend column name.. etc

testTbl:([] Continent:`NorthAmerica`Asia`Asia`Europe`Europe`Africa`Asia`Africa`Asia; // reuse this example
	 Country:("US";"China";"japan";"Germany";"UK";"Zimbabwe";"Bangladesh";"Nigeria";"Vietnam");
	 Population:313847 1343239 127938 81308 63047 13010 152518 166629 87840;
	 GDP:15080.0 11300.0 4444.0 3114.0 2228.0 9.9 113.0 196.0 104.0;
	 GDPperCapita:`48300`8400`34700`38100`36500`413`1788`732`3359 ;
	 LifeExpectancy:`77.14`72.22`80.93`78.42`78.16`39.01`61.33`51.01`70.05)

.qr.util.tbl.prepends[`;`prod;testTbl] // prepend the `prod into the name of the column

// lets say we are comparing data from prod server and qa server
prodTbl:.qr.util.tbl.prepends[`Continent;`prod;testTbl]
qaTbl:.qr.util.tbl.prepends[`Continent;`qa;testTbl]
.qr.util.tbl.lj[`Continent;prodTbl;qaTbl] // left join here

// split the column by delimeter. just try
test:([] x:("a|b|c";"c|e|f"); y: 12 20f; z:("hello";"world"))
test:([] x:("a|b|c";"a|b|c"); y: 12 20f; z:("hello";"world"))
.qr.util.tbl.splitCol[test;`x;"|"]

/////////////////////////////////////
// shimming
/////////////////////////////////////
// this is the cool module which will do the shimming
// it hides the existing function impelementation, overide by different functional implementation
// lets say you have implemented your (modulized) analytic you would like to provide an accessed point
// for your user to override the data source. this would be particularly useful
// qTracer later is actually built on top of shimming

testFunc:{x+y}

.qr.util.shimming.shim[`testFunc;{x*y}] // redefine function of + into *
testFunc[2;3] // try it
.qr.util.shimming.unshim[`testFunc] // now unshim it. and try to execute line above

testFunc2:.qr.quant.dist.normal.cdf;
.qr.util.shimming.shim[`testFunc2;.qr.quant.dist.lognormal.cdf]
testFunc2[0;1;0.5]
.qr.util.shimming.unshim[`testFunc2]

.qr.util.shimming.shim[`.qr.quant.dist.normal.cdf;{x+y}] // magically. it also works on dynamic library :D
.qr.util.shimming.unshimAll[] // we can unshim them all here

// check the shimming function. however this would fail to display if you ever shim dynamic library
.qr.util.shimming.list[] 

/////////////////////////////////////
// qTracer
/////////////////////////////////////
// Q does not come with a debugger
// v3.5 starts introducint such debugger but in my opnion it is still not good
// enuf (not enuf tracing). this module provides your the functionality to trace
// your code following the function call.
// it also allows you the cache the variables past into the functions for debug

testFunc:{testFunc2[x;y]}
testFunc2:{[x;y] .qr.throw "a bug :(";};

.qr.util.qtracer.wrap each `testFunc`testFunc2; // it starts by wrapping the function
testFunc[2;3] // it will throw you an error
.qr.util.qtracer.priv.x // inspect the variables here for debugging
.qr.util.qtracer.priv.y
.qr.util.qtracer.unwrap each `testFunc`testFunc2

.qr.util.qtracer.wrap[`.testModule]; // nows lets wrap the entire namespace
.testModule.maxiPathSum (3;7 4;2 4 6;8 5 9 3)

// you can unwrap them all here
.qr.util.qtracer.unwrapAll[]

/////////////////////////////////////
// raze
/////////////////////////////////////
// this is just a simple module to raze Q data types

.qr.util.raze.razeAll(1;(2;3;4);(((4;5);6);7)) // raze them all

// lets try to raze tables
tbl1:([] a:1+til 3; b:10*1+til 3; c:`a`b`c)
tbl2:([] a:4+til 3; b:10*4+til 3; c:`d`e`f)
.qr.util.raze.razeTable[(tbl1;`c`a`b xcols tbl2; `b`a`c xcols tbl1)]

// or just dictionary
dict1:(`a`b`c)!(1;2;3)
dict2:(`c`b`a)!(10;20;30)
.qr.util.raze.razeDict[(dict1;dict2)]

/////////////////////////////////////
// datetime
/////////////////////////////////////
// provide simple data time operation on Q, self-explanatory

.qr.util.dt.yearSD[.z.d]
.qr.util.dt.monthSD[.z.d]
.qr.util.dt.weekSD[.z.d]
.qr.util.dt.weekDay[.z.d;1]
.qr.util.dt.dtToDate[.z.p]
.qr.util.dt.toGMT .qr.util.dt.toEST .z.p
.qr.util.dt.toEST .qr.util.dt.addHours[.z.p;1]
.qr.util.dt.toEST .qr.util.dt.addMins[.z.p;5]
.qr.util.dt.toEST .qr.util.dt.addSec[.z.p;10]
.qr.util.dt.toEST .qr.util.dt.addMilliSec[.z.p;2]
.qr.util.dt.toEST .qr.util.dt.addSec[.z.p;0.000001]
.qr.util.dt.addSec[.z.p;0.000001] = .qr.util.dt.addMilliSec[.z.p;1]

/////////////////////////////////////
// timer
/////////////////////////////////////
// provide scheduling inside Q
// Q only has a global timer but no multi-threaded async or sync timers
// internal timmer in Q can only tick on the same frequency
// this module provides a way to manipulate the system and scheudle the timers
// on a more dynamic ticking mechanism or even scheduling
// try not to be too creative on this module, i.e. try not to schedule long run
// task in short period of time. your Q session might be locked otherwise

// known latency is due to the dynamic libary overhead.. nothing I can do about
// if time accuracy is really that important, contact the author and he will
// consider get you exposed to the source code of this

// relative timers
.qr.util.timer.start[{show "timer1"};enlist (::);3000] // start now. 3 seconds tick frequency
.qr.util.timer.start[{show "timer2"};enlist (::);5000]

// absolute timers
.qr.util.timer.startAbs[{show "timer3"};enlist (::);.z.p+.qr.util.timer.priv.getMillisecond 6000] // run in 6 seconds
.qr.util.timer.startAbs[{show "timer4"};enlist (::);.z.p+.qr.util.timer.priv.getMillisecond 20000] // run in 20 seconds

// forward start timer
.qr.util.timer.forwardStart[{show "timer5"};enlist (::);.z.p+ .qr.util.timer.priv.getMillisecond 5000;4000] // run in 5 seconds, tick frequency is 4 seconds

// list/remove timers
.qr.util.timer.list[]
.qr.util.timer.removeAll[]
.qr.util.timer.removeByFunctor[{show "timer3"}]

////////////////////////////////////////////////////////////////////////////////
// Quant - ip, lp, mat(matrix), math, numeric, probdist, random (rng), stat
////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////
// stat
/////////////////////////////////////

// the following modules provides simple descriptive statistics
// some test variables. adjust seq1 and seq2 to play the code below
seq1:til 10
seq2:reverse til 10

seq1:rand each 100#100
seq2:rand each 100#100

seq1:1+til 50
seq2:log seq1

.qr.quant.stat.pearsonCor[seq1;seq2]
.qr.quant.stat.spearmanCor[seq1;seq2]
.qr.quant.stat.sampleVar[seq1]
.qr.quant.stat.sampleCov[seq1;seq2]

// here we initialize i.i.d normal random variables using holton sequence
normSeq:.qr.quant.rng.norm[0;1;.qr.quant.rng.halton[2];.qr.quant.rng.halton[3];1000];
.qr.quant.stat.sampleVar[normSeq]
.qr.quant.stat.skew[normSeq]
.qr.quant.stat.kurt[normSeq]

/////////////////////////////////////
// rng
/////////////////////////////////////
// this modules provides the random number genrator for simple uniform and
// (log) normal distribtuion

([] x:.qr.quant.rng.uniform[`.qr.quant.rng.rand;3000];
    y:.qr.quant.rng.uniform[`.qr.quant.rng.rand;3000])

([] x:.qr.quant.rng.uniform[`.qr.quant.rng.halton[2];3000];
    y:.qr.quant.rng.uniform[`.qr.quant.rng.halton[17];3000])

flip (enlist `x)!enlist .qr.quant.rng.norm[0;1;`.qr.quant.rng.rand;`.qr.quant.rng.rand;50000]
flip (enlist `x)!enlist .qr.quant.rng.norm[0;1;.qr.quant.rng.halton[2];.qr.quant.rng.halton[3];50000]

/////////////////////////////////////
// probdist
/////////////////////////////////////
// here we have various forms of probalistic distribution
// cover most of the discrete and continous (including levy)

// poisson
([] poissonDist:.qr.quant.dist.poisson.cdf[10] each til 25)

// binomial
([] binDist:.qr.quant.dist.bin.cdf[25;0.5] each til 26)

// negative binomial
([] negBinomialDist:.qr.quant.dist.negBin.pdf[2;0.5] each til 25)

// geometric
.qr.quant.dist.geo.pdf:.qr.quant.dist.negBin.pdf[1];
.qr.quant.dist.geo.cdf:.qr.quant.dist.negBin.cdf[1];
([] geoDist:.qr.quant.dist.geo.pdf[0.5] each til 25)

// normal
([] x:.qr.quant.dist.normal.pdf[0;1] each -3 + 0.1 * 1+til 60)
([] x:.qr.quant.dist.normal.cdf[0;1] each -3 + 0.1 * 1+til 60)

.qr.quant.dist.stdNormal.pdf:.qr.quant.dist.normal.pdf[0;1];
.qr.quant.dist.stdNormal.cdf:.qr.quant.dist.normal.cdf[0;1];
.qr.quant.dist.stdNormal.cdfInv:.qr.quant.dist.normal.cdfInv[0;1];

([] x:.qr.quant.dist.stdNormal.pdf each -3 + 0.1 * 1+til 60)
([] x:.qr.quant.dist.stdNormal.cdf each -3 + 0.1 * 1+til 60)

// lognormal
([] x:.qr.quant.dist.lognormal.pdf[0;0.5] each 0.1 * 1 + til 30)
([] x:.qr.quant.dist.lognormal.cdf[0;0.5] each 0.1 * 1 + til 30)

// exponential
([] x:.qr.quant.dist.exp.pdf[1.5] each 0.1 * 1 + til 50)
([] x:.qr.quant.dist.exp.cdf[1.5] each 0.1 * 1 + til 50)

// levy
([] x:.qr.quant.dist.levy.pdf[0;0.5] each 0.01 * til 300;
    y:.qr.quant.dist.levy.pdf[0;1] each 0.01 * til 300;
    z:.qr.quant.dist.levy.pdf[0;2] each 0.01 * til 300)
([] x:.qr.quant.dist.levy.cdf[0;0.5] each 0.01 * til 300)

// t
([] x:.qr.quant.dist.t.pdf[1] each -3 + 0.1 * 1+til 60;
    y:.qr.quant.dist.t.pdf[3] each -3 + 0.1 * 1+til 60;
    z:.qr.quant.dist.t.pdf[5] each -3 + 0.1 * 1+til 60)

// beta
([] x:.qr.quant.dist.beta.pdf[0.5;0.5] each 0.01 * 1+til 99;
    y:.qr.quant.dist.beta.pdf[5;1] each 0.01 * 1+til 99;
    z:.qr.quant.dist.beta.pdf[1;3] each 0.01 * 1+til 99;
    g:.qr.quant.dist.beta.pdf[2;5] each 0.01 * 1+til 99)

// poisson-binomial
// https://en.wikipedia.org/wiki/Poisson_binomial_distribution
// implemntation is done by IDFT on characteristic function
([] binomial:.qr.quant.dist.bin.pdf[10;0.5] each til 11; // check correctness of poisson-binomial
    poissonBin:.qr.quant.dist.poissonBin.pdf[10#0.5] each til 11)

.qr.quant.dist.poissonBin.cdf[100#0.5;50]

/////////////////////////////////////
// numeric
/////////////////////////////////////
// this module provides common numerical methods

// numerical difference
.qr.quant.numeric.delta[{(x*x)-5*x};3;`central] //`forward`backward`central
.qr.quant.numeric.secondOrderDiff[{(x*x)-5*x};3]

// newton's method
.qr.quant.numeric.newton[{(x*x)-5*x};{(2*x)-5};100] // analytical gradient
.qr.quant.numeric.newton[{(x*x)-5*x};.qr.quant.numeric.delta[{(x*x)-5*x};;`central];100] // use numerical gradient

// bisection
.qr.quant.numeric.bisection[{(x*x)-5*x};3;9]
.qr.quant.numeric.bisection[{((x*x)-3*x)+2};1.5;5]

// linear interpolation
pts:("f"$1+til 10);
ptsInterp:(-1.0;1.0;1.2;2.3;3.4;4.5;5.6;6.7;10.0;11.0);
values:{(x*x)-5*x} each 1+til 10;
.qr.quant.numeric.interp[values;pts;ptsInterp]
res:([] x:pts,ptsInterp; yInt:values, .qr.quant.numeric.interp[values;pts;ptsInterp])
res:`x xasc update yAct:{(x*x)-5*x} each x from res
res:update yInt:"f"$yInt from res
select yInt, yAct from res

// numerical integral
.qr.quant.numeric.simpson[{1 % x};1;3] // simpson method
.qr.quant.numeric.integral[{1 % x};1;3] // adaptive simpson

.qr.quant.math.gamma[1.5]
.qr.quant.numeric.integral[{(x xexp 0.5) * exp neg x};0;100] // lets check the numerical integral

.qr.quant.math.gamma[0.5] // 1.7724539, the gamma function is more accurate than integral as we get close to singularity point
.qr.quant.numeric.integral[{(x xexp -0.5) * exp neg x};0;100] // singularity
.qr.quant.numeric.integral[{(x xexp -0.5) * exp neg x};1e-16;100] // 1.7724542, numerical err

/////////////////////////////////////
// math
/////////////////////////////////////
// this module provides common mathematical functions

// error function
.qr.quant.math.erf[1]
.qr.quant.math.erfInv[0.8427007] // ans its inverse
.qr.quant.math.erfInv[-0.8]

.qr.quant.math.comb[6;2] // combination

// gamma function
.qr.quant.math.gamma[0] 
.qr.quant.math.gamma[-1]
.qr.quant.math.gamma[1.5]
.qr.quant.math.gammaIncompL[2;3.0] // incomplete lower gamma
.qr.quant.math.gammaIncompH[2;3.0] // imcomplete hyper gamma

// beta function
.qr.quant.math.beta[6;4]
.qr.quant.math.gamma[6] * .qr.quant.math.gamma[4] % .qr.quant.math.gamma[10] // lets check correctness
// as we know beta(a;b) = gamma(a) * gamma(b) / gamma(a+b)

.qr.quant.math.betaIncompRegularised[50;51;0.5] // this is regularised beta function
.qr.quant.math.betaIncomp[50;51;0.5] % .qr.quant.math.beta[50;51] // same as above
sum .qr.quant.dist.bin.pdf[100;0.5] each til 51 // lets check the correctnes
// by using the expanded form for the beta function representing in binomial sequence


/////////////////////////////////////
// numeric
/////////////////////////////////////
// this module provides common complex analysis
a:.qr.quant.complex.number[1;2]
b:.qr.quant.complex.number[2;3]

.qr.quant.complex.add[a;b]
.qr.quant.complex.minus[a;b]
.qr.quant.complex.multiply[a;b]
.qr.quant.complex.multiply[a;2]
.qr.quant.complex.reciprocal[a]
.qr.quant.complex.multiply[a;.qr.quant.complex.reciprocal[b]]
.qr.quant.complex.divide[a;b]

.qr.quant.complex.exp[a]
.qr.quant.complex.sinh[a]
.qr.quant.complex.cosh[a]
.qr.quant.complex.tanh[a]
.qr.quant.complex.divide[.qr.quant.complex.sinh[a];.qr.quant.complex.cosh[a]]

// DFT and InverseDFT
x:(.qr.quant.complex.number[1;0]; .qr.quant.complex.number[2;-1]; .qr.quant.complex.number[0;-1]; .qr.quant.complex.number[-1;2])
fourierX:.qr.quant.complex.dft[x] each til count x
.qr.quant.complex.idft[fourierX] each til count fourierX


/////////////////////////////////////
// mat
/////////////////////////////////////
// provides matrix algebra analysis

.qr.quant.mat.zeros[5;3]
.qr.quant.mat.identity[5]

A:(0 1 2f;-1 0 -3f;2 1 7f);
A:flip (1 1f;5 9f;1 0f;1 0f);
A:.qr.quant.mat.diag[10#1f]; A[3;5]:0.5;
A:(2 1 0f;1 -5 3f;0 2 3f);
A:(2 1 0f;-3 -5 3f;0 2 3f);
A:(16 4 4 -4f;4 10 4 2f;4 4 6 -2f;-4 2 -2 4f)

.qr.quant.mat.dim[A]
.qr.quant.mat.isSquare[A]
.qr.quant.mat.diag[(1 2 3 4 5)]
.qr.quant.mat.diagVec .qr.quant.mat.diag[(1 2 3 4 5)]
.qr.quant.mat.diagVec[A]
.qr.quant.mat.subDiagVec[A;-1]
.qr.quant.mat.isDiagDominant[A]
.qr.quant.mat.rand[10;10;`.qr.quant.rng.rand]
.qr.quant.mat.isTriDiag[A]

A:.qr.quant.mat.rand[1000;1000;`.qr.quant.rng.rand];
LU:.qr.quant.mat.crout[A]

// most of the coes below calls QML to do linear algebra analysis
// since deep layer calls LAPACK. it is lightingly fast
// if it is easier, just use QML

LUP:.qr.quant.mat.LUP[A] // LUP decomp
sum sum (LUP[`P] mmu A) - LUP[`L] mmu LUP[`U]

PLU:.qr.quant.mat.PLU[A] // PLU decomp
sum sum A - PLU[`P] mmu PLU[`L] mmu PLU[`U]

QR:.qr.quant.mat.QR[A]  // QR decomp
sum sum A - QR[`Q] mmu QR[`R]

A:.qr.quant.mat.diag[1000#1f]; A[2;3]:0.5; A[3;2]:0.5
LU:.qr.quant.mat.chol[A] // cholesky decomp
sum sum A - LU[`L] mmu LU[`U]

SVD:.qr.quant.mat.SVD[A] // SVD decomp
sum sum A - SVD[`U] mmu SVD[`S] mmu flip SVD[`V]

.qr.quant.mat.det[(-2 2 -3f;-1 1 3f; 2 0 -1f)] // determinant
.qr.quant.mat.det[(2 5 -3 -2f;-2 -3 2 -5f; 1 3 -2 0f; -1 -6 4 0f)]
.qr.quant.mat.det[A]

A:.qr.quant.mat.diag[10#1f];
.qr.quant.mat.eigen[A]
.qr.quant.mat.minors[A]
.qr.quant.mat.isPosDef[A] // check if positive definite
A[1;1]:0f; // make the matrix not positive definite
.qr.quant.mat.isPosSemiDef[A]

/////////////////////////////////////
// lp
/////////////////////////////////////
// linear programming

// min y'b; s.t. y'A >= c & y >= 0
A:(0 1 2f;-1 0 -3f;2 1 7f); // this is the coefficient matrix
b:(3 -2 5f); // this is the coefficient on optimising function
c:(1 1 5f); // constraint
constr:`ge`ge`ge; // constraint on y'A (or Ax)
xconstr:`ge`ge`ge; // constraint on x (or y)
.qr.quant.lp.solve[b;A;c;constr;xconstr;`min] // Soln: 0 2/3 1 with v=11/3

A:flip (-1 5 2 5f; 0 3 0 1f; -1 0 1 2f);
c:(0 5 1 4f);
b:(5 2 1f);
constr:`le`eq`eq;
xconstr:`ge`ge`none`ge;
.qr.quant.lp.solve[c;A;b;constr;xconstr;`max] // Soln 1 0 -2 2 with v = 6

A:(-1 -2 1 1f; -4 1 -1 1f; 0 -3 0 1f; 1 -1 -3 1f; 1 1 1 0f);
c:(0 0 0 1f);
b:(0 0 0 0 1f);
constr:`ge`ge`ge`eq;
xconstr:`ge`ge`ge`ge`none;
.qr.quant.lp.solve[b;A;c;constr;xconstr;`min] // 14/35 8/35 0 13/35 33/35(lambda) with v = 33/35

// not possible. should be infeasible
A:flip (1 1f;5 9f;1 0f;1 0f);
c:(6 45 2 4f);
b:(5 8f);
constr:`le`le`le`ge;
xconstr:`ge`ge;
dir:`max;
.qr.quant.lp.solve[b;A;c;constr;xconstr;dir] // infeasible

// flip the sign
A:flip (1 1f;5 9f;1 0f);
c:(6 45 6);
b:(5 8f);
constr:`le`le`ge;
xconstr:`ge`ge;
dir:`max;
.qr.quant.lp.solve[b;A;c;constr;xconstr;dir] // 6 0 with v = 30

//flip the sign
A:flip (1 1f;5 9f;1 0f;0 1f);
c:(6 45 2 4f);
b:(5 8f);
constr:`le`le`le`ge;
xconstr:`ge`ge;
dir:`max;
.qr.quant.lp.solve[b;A;c;constr;xconstr;dir] // 1.8 4 with v = 41

// all equal constraint
A:flip (1 -2 1 0f;2 1 0 1f);
c:(2.5 1.5f);
b:(-3 -2 0 0f);
constr:`eq`eq; // here y'A = c (Ax = b)
xconstr:`ge`ge`ge`ge;
dir:`max;
.qr.quant.lp.solve[b;A;c;constr;xconstr;dir] // 0 0 2.5 1.5 with v = 0

// super corner case
A:flip enlist (1 1 1 1 1f);
c:enlist 10f;
b:(1 1 0 0 0f);
constr:enlist `eq;
xconstr:`ge`ge`none`none`none;
.qr.quant.lp.solve[b;A;c;constr;xconstr;`min] //0 0 10 0 0 0 0 0 with v = 0

/////////////////////////////////////
// ip
/////////////////////////////////////
// (mixed/binary) integer programming

// max y'b; s.t. y'A <= c & y >= 0, y are all integers
A:(1 1f;5 9f);
c:(6 45f);
b:(5 8f);
constr:`le`le;
xconstr:`ge`ge;
dir:`max;
.qr.quant.ip.solve[b;A;c;constr;xconstr;dir] // 6 0 with v = 30

// max y'b; s.t. y'A = c & y >= 0, y2 and y3 are all integers
A:flip (1 -2 1 0f;2 1 0 1f);
c:(2.5 1.5f);
b:(-3 -2 0 0f);
constr:`eq`eq;
xconstr:`ge`ge`ge`ge;
dir:`max;
iconstr:0110b;
.qr.quant.mip.solve[b;A;c;constr;xconstr;iconstr;dir] // 0.5 0 2 0.5 with v = -1.5

// max y'b; s.t. y'A >= c & y >= 0, y2 and y3 are all integers
A:flip (1 2 1 0f;2 1 0 1f);
c:(2.5 1.5f);
b:(3 2 0 0f);
constr:`ge`ge;
xconstr:`ge`ge`ge`ge;
dir:`max;
iconstr:0110b;
.qr.quant.mip.solve[b;A;c;constr;xconstr;iconstr;dir] // unbounded feasible

// max y'b; s.t. y'A <= c, y are either 0 or 1
A:flip (-3 -3 1 2 3f; -5 -3 -2 -1 1f);
b:(-8 -2 -4 -7 -5f);
c:(-2 -4f);
constr:`le`le;
dir:`max;
.qr.quant.bip.solve[b;A;c;constr;dir;`explicit] // explicit 01100b with v = -6
.qr.quant.bip.solve[b;A;c;constr;dir;`implicit] // implicit 01100b with v = -6

// min y'b; s.t. y'A <= c, y are either 0 or 1
A:flip (-1 -1 -1 -2 -3f; -1 -1 -1 -2 -1f);
b:(-8 -2 -4 -7 -5f);
c:(-2 -4f);
constr:`le`le;
dir:`min;
.qr.quant.bip.solve[b;A;c;constr;dir;`explicit] // explicit 1 1 1 1 1f with v = -26
.qr.quant.bip.solve[b;A;c;constr;dir;`implicit] // implicit 11111b with v = -26

// extreme case
// only one constraint. if doing explicit, branch-and-bound will take long time
b:0.7700431 0.7778214 0.7856781 0.7936143 8.7817807 0.8097279 0.8179069 0.8261686 0.8345138 0.8429432 0.9320653 0.9414801 0.95099 0.960596 0.970299 0.9801 0.99;
c:enlist 4426;
A:"f"$flip enlist 28 58 104 166 244 338 448 574 716 874 28 58 104 166 244 338 448;
constr:enlist `le;
.qr.quant.bip.solve[b;A;c;constr;`max;`implicit] // 11111110111111111b with v = 21.9395598. FAST!
.qr.quant.bip.solve[b;A;c;constr;`max;`explicit] // way slower than implicit
.qr.quant.lp.solve[b;A;c;constr;count[b]#`ge;`max]
.qr.quant.ip.solve[b;A;c;constr;count[b]#`ge;`max] // exploded!! if you run pure integer programming. not feasible at all for this extreme case