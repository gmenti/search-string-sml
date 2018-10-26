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

datatype collection = Fragments | Candidates | Verifieds;

fun getListPath (path) =
	if path = Fragments then "./fragments.txt" else
	if path = Candidates then "./candidates.txt" else
	if path = Verifieds then "./verifieds.txt" else
  	raise Fail "Unknown file path";
  	
fun existCharInCharList (word, []) = false
  | existCharInCharList (word, x::xs) = 
  	if word = x
  	then true
  	else existCharInCharList(word, xs);

fun first (x::xs) = x;

fun convertStringListToCharList ([]) = []
  | convertStringListToCharList (x::xs) = String.explode(x) :: convertStringListToCharList(xs);

fun verify ([], col) = true
  | verify (x::xs, col) = 
  	if existCharInCharList(x, col)
  	then verify(xs, col)
  	else false;
  	
val fragments = convertStringListToCharList(readFile(getListPath(Fragments)));
val candidates = convertStringListToCharList(readFile(getListPath(Candidates)));
val verifieds = [];
 
fragments;
candidates;

String.explode(first(candidates));
convertStringListToCharList(fragments);
 
verify(
	String.explode(first(candidates)),
	convertStringListToCharList(fragments)
);

verify(first(candidates), fragments);
