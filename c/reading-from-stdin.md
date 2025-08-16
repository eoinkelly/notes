The getline function is the preferred method for reading lines of text from a
stream, including standard input. The other standard functions, including gets,
fgets, and scanf, are too unreliable.

getline will realloc if the string is bigger than the allocated chunk of memory
