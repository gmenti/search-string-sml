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

fun verify (word, []) = false
  | verify (word, x::xs) = 
  	if word = x
  	then true
  	else verify(word, xs);

fun first (x::xs) = x;

datatype list = Fragments | Candidates | Verifieds;

fun getListPath (path) =
	if path = Fragments then "./fragments.txt" else
	if path = Candidates then "./candidates.txt" else
	if path = Verifieds then "./verifieds.txt" else
  	raise Fail "Unknown file path";
  
val fragments = readFile(getListPath(Fragments));
val candidates = readFile(getListPath(Candidates));
val verifieds = [];

verify(first(candidates), fragments);
