%%% BEGIN valgol.erl %%%
%%%
%%% valgol - Parser for the Lesser-Known Language VALGOL
%%% By Chris Pressey, Cat's Eye Technologies, Oct 11 2002.
%%% This work has been placed in the public domain.

%% @doc Parser for the Lesser-Known Language VALGOL.
%%
%% <p> This is like, a parser for, like, the world's most
%% bitchen programming langugage, written in, like, y'know, the
%% world's second most bitchen programming language? </p>
%%
%% <p> This program totally demonstrates how to totally do
%% recursive descent parsing in Erlang without using a seperate
%% scanner. </p>
%%
%% <h3>The Gnarly VALGOL Grammar</h3>
%%
%% <pre>
%% Grammar:
%% Program   ::= Statement [Program].
%% Statement ::= ["LIKE,"] (Command | Expr).
%% Command   ::= "START" Program "BAG" ["THIS"] Label
%%             | "REALLY" Program "GOTO" ["THE"] Label
%%             | "IF" Expr "THEN" Statement
%%             | "FOR" Identifier "=" Expression "TO" Expression {Statement} "SURE".
%% Expr      ::= E(1) {"AND" E(1) | "OR" E(1)}.
%% E(1)      ::= E(2) {"=" E(2) | "-" E(2)}.
%% E(2)      ::= E(3) {"+" E(3) | "-" E(3)}.
%% E(3)      ::= E(4) {"*" E(4) | "/" E(4)}.
%% E(4)      ::= Prim ["**" E(4)].
%% Prim      ::= ["LIKE"] Atom ["(" Expr ")"] ["MEAN"].
%% Atom      ::= Ident
%%             | Literal
%%             | "(" Expr ")"
%%             | "MAYBE" Expr
%%             | "TOTALLY" Expr
%%             | "VALLEY" Expr.
%% </pre>
%% @end

-module(valgol).
-vsn('2002.1011').
-author('catseye@catseye.mb.ca').
-copyright('This work has been placed in the public domain.').

-export([parse/1, test/1]).

%%% UTILITY %%%

%% @spec error(string(), [term()]) -> ok
%% @doc Generates an error.  Not particularly sophisticated.

error(M, I) ->
  io:fwrite(M, I).

%% @spec expect(string(), string(), tree()) -> {string(), tree()}
%% @doc Expects a certain token in the input string.  If it is there,
%% it will be consumed; if not, an error is generated.

expect(X, " " ++ I, O) -> expect(X, I, O);
expect(X, I, O)        ->
  case lists:prefix(X, I) of
    true -> {lists:nthtail(length(X), I), O};
    _ ->
      error("Expected '~s' at '~s'\n", [X, I]),
      {I, O}
  end.

%% @spec optional(string(), string(), tree()) -> {string(), tree()}
%% @doc Allows a certain token to be in the input string.  If it is there,
%% it will be consumed.

optional(X, " " ++ I, O) -> optional(X, I, O);
optional(X, I, O)        ->
  case lists:prefix(X, I) of
    true -> {lists:nthtail(length(X), I), O};
    _    -> {I, O}
  end.

%%% RDP %%%

%% @spec program(string(), tree()) -> {string(), tree()}
%%         tree() = atom() | {atom(), tree()} | {atom(), atom(), tree()}
%% @doc Parses a VALGOL program.
%% This is the top-level production of the recursive descent parser.

program("", O) -> {"", O};
program(" " ++ I, O) -> program(I, O);
program("\n" ++ I, O) -> program(I, O);
program("\r" ++ I, O) -> program(I, O);
program("\t" ++ I, O) -> program(I, O);

program(I=("SURE" ++ I2), O) -> {I, O};
program(I=("BAG" ++ I2), O) -> {I, O};
program(I=("GOTO" ++ I2), O) -> {I, O};

program(I, O) ->
  % io:fwrite("program: ~s\n", [I]),
  {I2, O2} = statement(I, ""),
  program(I2, {program, O, O2}).

%% @spec statement(string(), tree()) -> {string(), tree()}
%% @doc Parses a VALGOL statement.

statement("", O) -> {"", O};
statement(" " ++ I, O) -> statement(I, O);
statement("\n" ++ I, O) -> statement(I, O);
statement("\r" ++ I, O) -> statement(I, O);
statement("\t" ++ I, O) -> statement(I, O);
statement("LIKE," ++ I, O) -> statement(I, O);
statement(I, O) ->
  % io:fwrite("statement: ~s\n", [I]),
  command(I, O).

%% @spec command(string(), tree()) -> {string(), tree()}
%% @doc Parses a VALGOL command.

command("", O) -> {"", O};
command(" " ++ I, O) -> command(I, O);
command("\n" ++ I, O) -> command(I, O);
command("\r" ++ I, O) -> command(I, O);
command("\t" ++ I, O) -> command(I, O);
command("START" ++ I, O) ->
  {I2, O2} = program(I, {start, O}),
  {I3, O3} = expect("BAG", I2, O2),
  {I4, O4} = optional("THIS", I3, O3),
  {I5, O5} = optional("PROGRAM", I4, [O4]);

command("REALLY" ++ I, O) -> 
  {I2, O2} = program(I, {really, O}),
  {I3, O3} = expect("GOTO", I2, O2),
  {I4, O4} = optional("THE", I3, O3),
  {I5, O5} = optional("MALL", I4, [O4]);

command("IF" ++ I, O) -> 
  {I2, O2} = expr(I, {'if', O}),
  {I3, O3} = expect("THEN", I2, O2),
  {I4, O4} = statement(I3, O3);

command("FOR" ++ I, O) -> 
  {I2, O2} = patom(I, []),
  {I3, O3} = expect("=", I2, O2),
  {I4, O4} = expr(I3, O3),
  {I5, O5} = expect("TO", I4, O4),
  {I6, O6} = expr(I5, O5),
  {I7, O7} = program(I6, O6),
  {I8, O8} = expect("SURE", I7, {for, O, O7});

command(I, O) ->
  % io:fwrite("expr: ~s\n", [I]),
  expr(I, O).

%% @spec expr(string(), tree()) -> {string(), tree()}
%% @doc Parses a VALGOL expression.

expr("", O) -> {"", O};
expr(" " ++ I, O) -> expr(I, O);
expr(I, O) -> expr(0, I, O).

expr(N, " " ++ I, O) -> expr(N, I, O);
expr(N, "\n" ++ I, O) -> expr(N, I, O);
expr(N, "\r" ++ I, O) -> expr(N, I, O);
expr(N, "\t" ++ I, O) -> expr(N, I, O);
expr(5, I, O) -> prim(I, O);
expr(N, I, O) ->
  {I2, O2} = expr(N+1, I, ""),
  % io:fwrite("Checking level ~w\n", [N]),
  {I3, O3} = expr_tail(N, I2, O2),
  {I3, {op, O, O3}}.

expr_tail(N, " " ++ I, O)     -> expr_tail(N, I, O);
expr_tail(N, "\n" ++ I, O)    -> expr_tail(N, I, O);
expr_tail(N, "\r" ++ I, O)    -> expr_tail(N, I, O);
expr_tail(N, "\t" ++ I, O)    -> expr_tail(N, I, O);

expr_tail(N=0, "AND" ++ I, O) ->
  {I2, O2} = expr(N + 1, I, {'and', O}),
  {I3, O3} = expr_tail(N, I2, O2);

expr_tail(N=0, "OR"  ++ I, O) ->
  {I2, O2} = expr(N + 1, I, {'or', O}),
  {I3, O3} = expr_tail(N, I2, O2);

expr_tail(N=1, "=" ++ I, O)   ->
  {I2, O2} = expr(N + 1, I, {equals, O}),
  {I3, O3} = expr_tail(N, I2, O2);

expr_tail(N=1, "!" ++ I, O)   -> 
  {I2, O2} = expr(N + 1, I, {doesntequal, O}),
  {I3, O3} = expr_tail(N, I2, O2);

expr_tail(N=2, "+" ++ I, O)   -> 
  {I2, O2} = expr(N + 1, I, {add, O}),
  {I3, O3} = expr_tail(N, I2, O2);

expr_tail(N=2, "-" ++ I, O)   -> 
  {I2, O2} = expr(N + 1, I, {subtract, O}),
  {I3, O3} = expr_tail(N, I2, O2);

expr_tail(N=3, "*" ++ I, O)   ->
  {I2, O2} = expr(N + 1, I, {multiply, O}),
  {I3, O3} = expr_tail(N, I2, O2);

expr_tail(N=3, "/" ++ I, O)   -> 
  {I2, O2} = expr(N + 1, I, {divide, O}),
  {I3, O3} = expr_tail(N, I2, O2);

expr_tail(N=4, "**" ++ I, O) ->
  {I2, O2} = expr(N + 1, I, {tothe, O}),
  {I3, O3} = expr_tail(N, I2, O2);

expr_tail(_, I, O) -> {I, O}.

%% @spec prim(string(), tree()) -> {string(), tree()}
%% @doc Parses a VALGOL primitive.

prim("", O) -> {"", O};
prim(" " ++ I, O) -> prim(I, O);
prim("LIKE" ++ I, O) -> prim(I, O);
prim(I=("BAG" ++ I2), O) -> {I, O}; % needed to resolve LIKE BAG ambiguity
prim(I, O) ->
  % io:fwrite("primitive: ~s\n", [I]),
  {I2, O2} = patom(I, O),
  {I3, O3} = prim_tail(I2, O2).

prim_tail(" " ++ I, O) -> prim_tail(I, O);
prim_tail("(" ++ I, O) ->
      {I2, O2} = expr(I, {index, O}),
      {I3, O3} = expect(")", I2, O2);
prim_tail("MEAN" ++ I, O) -> {I, O};
prim_tail(I, O) -> {I, O}.

%% @spec patom(string(), tree()) -> {string(), tree()}
%% @doc Parses a VALGOL atom.

patom("", O) -> {"", O};
patom(" " ++ I, O) -> patom(I, O);
patom("(" ++ I, O) ->
  {I2, O2} = expr(I, O),
  {I3, O3} = expect(")", I2, O2);

patom("MAYBE"   ++ I, O) -> {I2, O2} = expr(I, O);
patom("TOTALLY" ++ I, O) -> {I2, O2} = expr(I, O);
patom("VALLEY"  ++ I, O) -> {I2, O2} = expr(I, O);

patom(I, O) ->
  N = string:span(I, "ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
  M = string:span(I, "0123456789"),
  Token = list_to_atom(string:substr(I, 1, N)),
  Lit   = list_to_atom(string:substr(I, 1, M)),
  case N + M of
    0 -> % io:fwrite("identifier: ~s\n", [[hd(I)]]),
         {tl(I), O};
    _ ->
    case N > M of
      true ->
        % io:fwrite("identifier: ~s\n", [Token]),
        {I2, O2} = {string:substr(I, N + 1), {token, Token, O}};
      _ ->
        % io:fwrite("literal: ~s\n", [Lit]),
        {I2, O2} = {string:substr(I, M + 1), {lit, Lit, O}}
    end
  end.

%%% MAIN %%%

%% @spec parse(string()) -> tree()
%% @doc Parses a VALGOL program.
%% This is the main user interface to the parser.

parse(I) ->
  {_, O} = program(I, ""),
  O.

%%% TESTS %%%

%% @spec test(test_id()) -> tree()
%%         test_id() = std | 1 | interactive
%% @doc Runs various tests on the VALGOL parser, including the
%% original demo program from the Lesser-Known Languages VALGOL
%% fortune cookie.

test(std) -> test(1);
test(1) -> parse("
	LIKE, Y*KNOW(I MEAN)START
	IF PIZZA = LIKE BITCHEN AND GUY = LIKE TUBULAR AND
	   VALLEY GIRL = LIKE GRODY**MAX(FERSURE)**2 THEN
		FOR I = LIKE 1 TO OH*MAYBE 100
			DO*WAH - (DITTY**2)
			BARF(I)=TOTALLY GROSS(OUT)
		SURE
	LIKE BAG THIS PROGRAM
	REALLY
	LIKE TOTALLY (Y*KNOW)
	IM*SURE
	GOTO THE MALL"
);
test(interactive) ->
  L = io:get_line('valgol> '),
  case L of
    "quit" ++ T -> ok;
    _ -> io:fwrite("~w\n", [parse(L)]),
         test(interactive)
  end.

%%% END of valgol.erl %%%
