# User-interactive Chisel based on Sparrow

## Install with OPAM
The easiest way to install Sparrow is to use [OPAM][].
Once you have cloned the source codes, run the build script to install the prerequisites and Sparrow:
```sh
$ git clone git@github.com:aspire-project/uichisel.git
$ cd uichisel
$ ./build.sh
$ eval `opam config env`
```
After that, you can directly run ```make``` or ```make install```.

## Run the tool
You can run the tool on a pre-processed C file. A script file ```test.sh``` should exist on a current working directory.
The followings show an example use.  
```sh
$ cd test
$ cp test.orig.c test.c 
$ ../bin/sparrow test.c 
```
You will see all questions and their gains. A question of the highest gain is posed.
```
======== UI CHISEL : iteration - 1 (size: 43, #questions: 7) =========
======== Questions and Gains ========
!(argc==1) at test.c:60: 7 ?
!(!(argc==3)) at test.c:72: 1 ?
!(argc==2) at test.c:68: 1 ?
!(!c) at test.c:34: 1 ?
!(SizeOf(char )!=1UL) at test.c:11: 0 ?
!(SizeOf(short )!=2UL) at test.c:8: 0 ?
!(SizeOf(int )!=4UL) at test.c:5: 0 ?
=====================================

!(argc==1) at test.c:60: 7 ?
1
Reason?:
argc is always 3.
```
To answer Yes, enter "1". To say No, enter "0". 
In the above, the question is answered yes. You should also provide the reason. 
In the next round, you will get another set of questions. 
```
======== UI CHISEL : iteration - 2 (size: 36, #questions: 6) =========
======== Questions and Gains ========
!(!(argc==3)) at test.c:72: 1 ?
!(argc==2) at test.c:68: 1 ?
!(!c) at test.c:34: 1 ?
!(SizeOf(char )!=1UL) at test.c:11: 0 ?
!(SizeOf(short )!=2UL) at test.c:8: 0 ?
!(SizeOf(int )!=4UL) at test.c:5: 0 ?
=====================================

!(!(argc==3)) at test.c:72: 1 ?
1
Reason?:
argc is always 3.
``` 
You can stop the interaction by typing any number other than 0 or 1.
After you're done with the interaction, you should be able to find ```test.c``` has been reduced.
Under the folder ```chisel_output```, you may find other helpful data. 
Especially, ```chise_output/log_info.txt``` contains all the useful info about the interaction. 
```
test.c - #func: 6        #instrs : 43
After initial PE: #func: 6       #instrs : 43
Iter    #Questions      Max gain        Avg gain        Question        Answer  Reason  #func   #instrs
1       7       7       1.5     !(argc==1) at test.c:60?        1       argc is always 3.       5       36
2       6       1       0.5     !(!(argc==3)) at test.c:72?     1       argc is always 3.       5       35
3       5       1       0.5     !(argc==2) at test.c:68?        1       argc is always 3.       5       34
4       4       1       0.3     !(!c) at test.c:34?     0       don't know      5       34
5       3       0       0.0     !(SizeOf(char )!=1UL) at test.c:11?     -1      stop
``` 
Also, ```chisel_output/iter_{i}_[filename].c``` are intermediate result files generated after i-th question.
