
start:
	dist/build/generate-server/generate-server -p 8000 &

stop:
	killall generate-server


get:
	curl -v http://localhost:8000/forever >test.raw

convert:
	sox -r 44100 -e signed -b 16 -c 1 test.raw test.wav
	open test.wav