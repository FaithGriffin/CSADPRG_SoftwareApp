### Final Machine Project in CSADPRG (Advanced Programming and Theory of Programming Languages)
- **About**:
    Finds the minimum edit distance given two input strings. The minimum edit distance problem, most commonly referred to as the Levenshtein distance problem, is concerned with finding the minimum number of single-character edits to turn one string into another. The two strings need not be of the same length and the operations involving single-character edits include deletion, insertion, and substitution; all of which had equal costs which is one, based on the machine project’s specifications. 
    A dynamic programming approach was used to compute for the minimum edit distance. This approach uses a bottom-up approach in solving a problem by dividing the main problem into smaller problems first. This approach uses a matrix to store the answers to the subproblems. This is evident when filling out the matrix wherein each of the cells actually correspond to the subproblems. Lastly, this approach also uses the same matrix to come up with the answers to succeeding subproblems based on the answers to previous subproblems. Aside from having the minimum edit distance as output, the Software Application also included the feature of printing out the specific operations used as the minimum edit distance was being computed and it also prints the strings as the different operations were applied. 
<h1 align="center"><img src="Screenshots/4.1SampleOutput.png" width="200px">

- **To run**:
    Install GNU Fortran compiler, compile, then run (linux-based OS dependent).
