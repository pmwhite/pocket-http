  $ test() { echo $1 | $TEST_DIR/parse; }
  $ test_file() { $TEST_DIR/parse; }

  $ test_file <<EOF
  > GET /hello HTTP/1.1
  > Content-Length: 10
  > 
  > 1234567890
  > EOF
  got request

  $ test_file <<EOF
  > GET / HTTP/1.1
  > Content-Length: 10
  > 
  > 1234567890
  > EOF
  got request
