all: code template

code: clean
	erl -make

clean:
	rm -fv ebin/*.beam *.rel *.script *.boot erl_crash.dump *.log *.access

template:
	erl -noshell -eval '[erltl:compile(F, [{outdir, "ebin"}, debug_info, show_errors, show_warnings]) || F <- filelib:wildcard("templates/*.et")].' -pa ebin -s init stop
