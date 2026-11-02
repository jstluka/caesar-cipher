# caesar-cipher
Here is a code I wrote to find any English word that is a Caesar cipher of another English word.

A "Caesar cipher" refers to shifting each letter within a word a given number of places in the alphabet. For example, if you choose to shift 1 letter, then A becomes B, B becomes C, etc., and the word CIPHER would become DJQIFS.

The majority of the ciphers will appear to be gibberish, like DJQIFS, but sometimes the output will be another word. For example, DEED can be shifted 10 times to get NOON.

This code finds all English words where one of its potential ciphers is actually another English word. Each set of words is grouped and can be found in the "result" output.
