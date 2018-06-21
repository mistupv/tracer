compile:
	@erlc -o ebin src/*.erl

load: compile
	@erl -sname main -setcookie cookie -pa ebin
