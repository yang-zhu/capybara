all: clean
	nix-build
	mkdir result-webpage
	cp result/bin/app.jsexe/* result-webpage
	chmod u+w result-webpage/*
	cp static/* result-webpage
	rm result

clean:
	rm -rf result-webpage 