signature UNTYPED =
sig
    val readFile : string -> DataTypes.Term
end

structure Untyped : UNTYPED =
struct
exception UntypedError;
fun readFile (fileName) =
    let val inStream = TextIO.openIn fileName;
	val grab : int -> string =
	 fn n => if TextIO.endOfStream inStream
		 then ""
		 else TextIO.inputN (inStream, n);
	val printError : string * int * int -> unit =
	 fn (msg, line, col) =>
	    print (fileName^"["^Int.toString line^":"
		   ^Int.toString col^"] "^msg^"\n");
	val (tree, rem) = UntypedParser.parse
			      (15,
			       (UntypedParser.makeLexer grab fileName),
			       printError,
			       fileName)
			  handle UntypedParser.ParseError => raise UntypedError;
	val _ = TextIO.closeIn inStream;
    in tree
    end
end;
