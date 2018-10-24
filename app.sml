fun toFileFormat(data) = 
      foldl (fn (x1, x2) => x1  ^ "\n" ^ x2) (hd data) (tl data)

fun writeFile(path : string, data) =
	let val file = TextIO.openOut(path)
		val _ = TextIO.output(file, toFileFormat(data))
	in TextIO.closeOut(file)
	end

fun readFile(path : string) =
    let val file = TextIO.openIn(path)
    	val poem = TextIO.inputAll(file)
        val _ = TextIO.closeIn(file)
    in String.tokens (fn c => c = #"\n") poem
    end
    
val fragments = readFile("./fragments.txt");
val words = readFile("./words.txt");

writeFile("./candidates.txt", words);

fragments;
words;