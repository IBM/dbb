hostName=127.0.0.1
port=6789

#~ Open socket.
exec 3<>/dev/tcp/$hostName/$port


# Argument Parsing
if [ $# -eq 0 ]; then
    echo "Invalid amount of parameters."
    exit 1
fi
if [ $# -eq 1 ]; then
    if [[ $1 == "-kill" ]]; then
        echo "kill" >&3
    fi
fi 
if [ $# -ge 2 ]; then
    echo "'$*'" >&3
fi



#~ Receive msg.
while read -r response <&3
do
    echo "$response"
done
