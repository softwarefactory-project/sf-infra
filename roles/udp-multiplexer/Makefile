test: test-light test-heavy

test-light:
	echo "Test packet1" | nc -u 127.0.0.1 7000

test-heavy:
	(echo "Big packet"; dd if=/dev/urandom bs=12k count=1 | base64; sleep 0.5) | nc -u 127.0.0.1 7000

setup: setup1 setup2

setup1:
	nc -v -k -l -u -p 9091 -e "./tests/reply.sh server1" &

setup2:
	nc -v -k -l -u -p 9092 -e "./tests/reply.sh server2" &

clean:
	killall nc
