#!/sbin/runscript

root=__ROOT__
user=__USER__
group=__USER__

start () {
	ebegin "Starting scrobbler-server"
	start-stop-daemon --start --background --chdir $root \
		--user $user --group $group \
		--exec $root/scrobbler-server --pidfile $root/.scrobbler-server.pid --make-pidfile
	eend $?
}

stop () {
	ebegin "Stopping scrobbler-server"
	start-stop-daemon --stop \
		--pidfile $root/.scrobbler-server.pid
	eend $?
}
