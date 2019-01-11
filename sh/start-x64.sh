#!/bin/sh

diskdir=""
port=""

while : ; do
  case "$1" in 
    -d)
       diskdir="$2"
       shift 2 ;;
    -p)
       port="$2"
       shift 2 ;;
    *)
       break ;;
  esac
done

echo "diskdir=($diskdir) port=($port)"

cat >temp-vicerc <<EOF
[C64]
VICIIDoubleScan=1
VICIIDoubleSize=1
KeySet1NorthWest=0
KeySet1North=273
KeySet1NorthEast=0
KeySet1East=275
KeySet1SouthEast=0
KeySet1South=274
KeySet1SouthWest=0
KeySet1West=276
KeySet1Fire=306
KeySet2NorthWest=0
KeySet2North=0
KeySet2NorthEast=0
KeySet2East=0
KeySet2SouthEast=0
KeySet2South=0
KeySet2SouthWest=0
KeySet2West=0
KeySet2Fire=0
KeySetEnable=1
EOF

if [ "X$port" = "X1" ]; then
  cat >>temp-vicerc <<EOF
JoyDevice1=2
JoyDevice2=0
EOF
elif [ "X$port" = "X2" ]; then
  cat >>temp-vicerc <<EOF
JoyDevice1=0
JoyDevice2=2
EOF
fi

if [ "X$diskdir" != "X" ]; then
    x64 -config temp-vicerc -iecdevice8 -device8 1 -fs8 "$diskdir"
else
    x64 -config temp-vicerc "$@"
fi
rm -f temp-vicerc
