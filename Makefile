all: code template

code: clean
	erl -make

clean:
	rm -fv ebin/*.beam *.rel *.script *.boot erl_crash.dump *.log *.access

template:
	erl -noshell -eval '[erltl:compile(F, [{outdir, "ebin"}, debug_info, show_errors, show_warnings]) || F <- filelib:wildcard("templates/*.et")].' -pa ebin -s init stop

start:
	erl -setcookie s3imagespass -name s3imagesapp@`hostname` -pa ebin -yaws embedded true -boot start_sasl -mnesia dir 's3images.mnesia' -eval '[application:start(X) || X <- [inets, yaws, crypto, mnesia, s3images]].' -detached 

start-ctl:
	erl -setcookie s3imagespass -name s3imagesctl@`hostname` -pa ebin -remsh s3imagesapp@`hostname`
