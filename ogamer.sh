#Where is caster?
LOCATION=
JAR=ogamer.jar
UNI=
LOGIN=
PASS=
GMAILLOGIN=
GMAILPASS=
COMMANDS=
JAVABIN=/opt/java/bin/java
RETVAL=0

start() {
	echo -e "Starting Ogamer\n"
        if [ -f "${LOCATION}/ogamer-running" ]; then
                echo "Ogamer already started or lock
file hasn't deleted."
                exit 1
        fi
        cd $LOCATION
        rm -f ./nohup.out
        nohup ${JAVABIN} -XX:MaxPermSize=128m -Xmx2G -Xms512M -jar ${JAR} --uni ${UNI} --login ${LOGIN} --pass ${PASS} --gmail-login ${GMAILLOGIN} --gmail-pass ${GMAILPASS} --commands ${COMMANDS} > /dev/null 1>&2 &
	    touch ogamer-running
        return $RETVAL
}
stop() {
	echo -e "Stopping Ogamer\n"
	PID=`ps ax | grep ${JAR} | grep -v grep | awk '{print $1}'`
    kill -15 $PID 2> /dev/null && echo "${JAR} killed"
	rm -f ${LOCATION}/ogamer-running
	return $RETVAL
}
status() {
        echo "====== status ======"
        /bin/ps waux|/bin/grep ${JAR}|/bin/grep -v grep
        echo "===================="
        return $RETVAL
}
# See how we were called.
case "$1" in
    start)
        start
        ;;
    stop)
        stop
        ;;
    status)
        status
        ;;
*)
echo $"Usage: $0 {start|stop|status}"
exit 1
esac
exit $RETVAL
