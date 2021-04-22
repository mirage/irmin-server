#dd if=/dev/urandom of=data.dump bs=1M count=1024

unix_socket () {
  echo "Unix socket"
  socat -u -b32768 UNIX-LISTEN:/tmp/unix.sock /dev/null &
  time socat -u -b32768 "SYSTEM:dd if=data.dump bs=1M count=1024" UNIX:/tmp/unix.sock
}

direct () {
 echo "---"
 echo "Direct"
 time dd if=data.dump of=/dev/null bs=1M
}

unix_socket
direct
