import sys
import re
import urllib

for line in sys.stdin:
    match = re.match(r'^(.*?)\((http://www.lemon64.com/\?mainurl=(.*?))\)(.*?)$', line)
    if match:
        line = '{}({}){}\n'.format(
            match.group(1), urllib.unquote_plus(match.group(3)), match.group(4)
        )
    else:
        match = re.match(r'^(.*?)\((http://www.lemon64.com/games/details.php\?ID=(\d+))\)(.*?)$', line)
        if match:
            line = '{}(http://www.lemon64.com/?game_id={}){}\n'.format(
                match.group(1), match.group(3), match.group(4)
            )
    sys.stdout.write(line)
