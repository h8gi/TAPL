structure Main =
struct
open Untyped

fun run str =
    let
        val t = parse str
        val v = eval t
    in
        print (show v ^ "\n")
    end
end

val _ = Main.run "x"
