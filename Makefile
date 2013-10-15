
start:
	dist/build/generate-server/generate-server -p 8000 &

stop:
	killall generate-server