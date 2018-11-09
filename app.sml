(*
	Nome: Giuseppe Menti
	Referências:
	http://smlnj.org/install/
	https://learnxinyminutes.com/docs/standard-ml/
	http://sml-family.org/Basis/string.html
	http://sml-family.org/Basis/list.html
	https://stackoverflow.com/questions/13430352/how-to-use-and-operator-in-if-statements-in-sml
*)

val toFileFormat = String.concatWith("\n");
fun isLineBreak (c) = c = #"\n";

fun writeFile(path, data) =
	let val file = TextIO.openOut(path)
			val _ = TextIO.output(file, toFileFormat(data))
	in TextIO.closeOut(file)
	end;

fun readFile(path) =
	let val file = TextIO.openIn(path)
			val data = TextIO.inputAll(file)
			val _ = TextIO.closeIn(file)
	in String.tokens(isLineBreak)(data)
	end;

datatype collection = Fragments | Candidates | Verifieds;

fun getListPath (path) =
	if path = Fragments then "./fragments.txt" else
	if path = Candidates then "./candidates.txt" else
	if path = Verifieds then "./verifieds.txt" else
	raise Fail "Unknown file path";
  	
fun existCharInCharList (word, []) = false
  | existCharInCharList (word, x::xs) = 
	if word = x then true else existCharInCharList(word, xs);

fun first (x::xs) = x;

fun convertStringListToCharList ([]) = []
  | convertStringListToCharList (x::xs) = String.explode(x) :: convertStringListToCharList(xs);
  	
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

val fragments = convertStringListToCharList(readFile(getListPath(Fragments)));
val candidates = convertStringListToCharList(readFile(getListPath(Candidates)));

fun filter (candidate) = verify(candidate, fragments);
val verifieds = List.filter(filter)(candidates);

writeFile(
	getListPath(Verifieds),
	List.map(String.implode)(verifieds)
);

val _ = print("Arquivo gerado com as palavras resultantes em './verifies.txt'.\n");