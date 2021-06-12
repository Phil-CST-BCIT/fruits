# Fruits


**About the project**
The project is a terminal word guessing game that is built with Haskell.
The game consists of 11 * 15 letters in a rectangular grid.
The goal of the game is to find out the names of ten fruits

**Prerequisites**
In order to run the game on a computer, players need to install the latest Haskell tool chain stack.

**How to run the game**

1. open a terminal window
2. change your working directory to the root of the project 
3. call stack build to build the executable file of the game 
4. After build is completed, run stack exec fruits-exe

**How to play with the game**

On start, the program will create a grid like blow

ZAEYYRMGLWCRZRK  
XJCPAFIXPOPZAVJ  
RRHPAJVMMNECCEE  
JPENRRYINBAVBOP  
MRRUFWGBDGCFOTP  
PERCRHLLWJHCOGL  
MPYNNPIDLOWWGKU  
ZSPOKCMAJGQEQGM  
QBLMOPERSNCHTPD  
KEBANANABAPCPAQ  
MAQSHEZAPMTATOD  
 
and then the command prompt will ask the user to enter a name of a fruit in uppercase.
If the user enters a correct one, then the user will win 1 point, and the letters
will be replaced with '*'. 

If the user enters an incorrect one, the game will do nothing.

When all 10 fruits are found, the game quits, otherwise it will sit in a loop, 
or, if the user enter a composite keys of ctrl + c to interrupt the program and quit. 

