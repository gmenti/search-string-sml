fun readFile(path : string) =
    let val file = TextIO.openIn path
    	val poem = TextIO.inputAll file
        val _ = TextIO.closeIn file
    in String.tokens (fn c => c = #"\n") poem
    end
    
val fragments = readFile("./fragments.txt");
val inputs = readFile("./inputs.txt");

fragments;
inputs;