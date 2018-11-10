compile:
	@erlc -o ebin src/*.erl

load: compile
	@erl -pa ebin
