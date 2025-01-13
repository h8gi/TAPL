structure UntypedLrVals = UntypedLrValsFun(
    structure Token = LrParser.Token);
structure UntypedLex = UntypedLexFun(
    structure Tokens = UntypedLrVals.Tokens);
structure UntypedParser = JoinWithArg(
    structure ParserData = UntypedLrVals.ParserData
    structure Lex=UntypedLex
    structure LrParser=LrParser);
