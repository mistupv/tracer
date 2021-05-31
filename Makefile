compile:
	@erlc -o ebin src/*.erl

load: compile
	@erl -sname main -hidden -pa ebin
