fun toFileFormat(data) = 
      foldl (fn (x1, x2) => x1  ^ "\n" ^ x2) (hd data) (tl data);

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
  	
val fragments = convertStringListToCharList(readFile(getListPath(Fragments)));
val candidates = convertStringListToCharList(readFile(getListPath(Candidates)));
val verifieds = [];

fun pop ([]) = []
  | pop (x::xs) = xs;

fun diff ([], b) = b
  | diff (a, []) = a
  | diff (x::xs, b::bs) = if x = b then diff(xs, bs) else x::diff(xs, b::bs);

fun contains ([], _) = true
  | contains (_, []) = false
  | contains (a, b) = String.isSubstring(String.implode(a))(String.implode(b));

fun verify ([], _) = true
  | verify (_, []) = false
  | verify (c::cs, f::fs) = 
  	if List.length(f) = 1
  	then 
  		if c = first(f)
  		then verify(cs, fs)
  		else verify(c::cs, fs)
  	else 
  		if contains(f, c::cs)
  		then verify(diff(c::cs, f), fs)
  		else verify(c::cs, fs)
  	;
  	


fun filter (candidate) = verify(candidate, fragments);
val vers = List.filter filter candidates;

