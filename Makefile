
restart:
	cabal build
	make stop
	sleep 0.5
	make start

start:
	dist/build/generate-server/generate-server -p 8000 &

stop:
	killall generate-server


# This will run forever if not cancelled
get:
	curl -v localhost:8000/forever.wav > test.wav

open:
	open -a Audacity test.wav