## The original synopsis from aha0x0x (https://github.com/aha0x0x):

```
This project provides utility code to load a given file containing integers, sort, and then write sorted list to an output file.
This is part of testing I am doing to compare language performance. I have added similar projects for Rust and Java.
```

## Now, the actual introduction

SP: I was made aware of aha0x0x's benchmarking experiments to measure performance of several languages and specifically his criticism of the Haskell version he wrote.
It was much slower than the Java or Rust counterparts and it felt wrong to me that the speed difference is so gross, even considering the high level and lazy nature of Haskell.
(if anything, it's my belief that pure FP code can lead to pretty good optimized results since the code itself is more prone to being analyzed deeper and more aggressive
optimization algorithms can be applied correspondingly). So I got interested and tried to tweak the original Haskell code to see if its performance can be improved.
This is also turning out to be a good way to improve my Haskell skills by diving deeper into the important bits of the language and its ecosystem.

Here are some links to resources pointing that I'm not the only one thinking that Haskell can be pretty performant in the right hands :)

* https://wiki.haskell.org/Why_Haskell_matters#The_speed_of_Haskell
* https://benchmarksgame-team.pages.debian.net/benchmarksgame/faster/haskell.html
* https://stackoverflow.com/questions/35027952/why-is-haskell-ghc-so-darn-fast
* https://www.quora.com/Is-Haskell-as-fast-as-C++-If-not-why-not

In the updated code, ```testAha``` function essentially contains the original code from aha0x0x, just slightly reworded/reformatted.
The ```testSP``` function contains a faster implementation and is a work in progress.

Please note that the methodology to measure performance here is still very flawed:
we benchmark several things at the same time (e.g. time-bound algorithm for sorting, I/O performance for both reading and writing)
but do not measure memory consumption etc.

Here are the rough and very approximate results
(which are only good to tell an order of magnitude difference and to get a sense of the overall code performance):

* CPU (```lscpu```): Intel(R) Core(TM) i7-7700K CPU @ 4.20GHz
* Disk (```sudo hdparm -I /dev/sda1```): Samsung SSD 850 EVO 250GB

Using a generated text file with 10M random integer numbers, we get these best times:

* C (```sort -n```): (time: ```13.78s user 0.42s system 434% cpu 3.270 total```)
* Rust (https://github.com/aha0x0x/num_sort): 0.882 sec (time: ```1.85s user 0.22s system 99% cpu 2.063 total```)
* Java + Oracle JDK 10 (https://github.com/aha0x0x/NumSort): 1.71 sec (time: ```2.24s user 0.14s system 124% cpu 1.920 total```)
* Haskell (this project), "test-aha" method: 58 sec (time: ```144.03s user 99.27s system 419% cpu 58.010 total```)
* Haskell (this project), "test-sp" method: 6.19 sec (time: ```11.87s user 8.00s system 316% cpu 6.266 total```)

## Installation

Make sure to have the Haskell Stack build tool installed first (see https://docs.haskellstack.org/en/stable/README/ for details)

Then run:
```
stack update && stack upgrade && stack setup && stack build
```

## Running the benchmarks

To generate the input file and run the benchmarks:

```
time numbersort-exe generate 1000000 /tmp/input.txt
time numbersort-exe test-aha /tmp/input.txt /tmp/output.txt
time numbersort-exe test-sp /tmp/input.txt /tmp/output.txt
```

(We prepend ```time``` just to have more data points to think about;
please note that ```time``` measures time with an extra OS overhead of executable loading etc.)

## Further thoughts

FIXME TODO add profiling results here...

Here you can find some useful information on Haskell code performance, in detail:

* https://www.packtpub.com/application-development/haskell-high-performance-programming
* https://wiki.haskell.org/Performance
* https://www.cheatography.com/nash/cheat-sheets/ghc-and-rts-options/
