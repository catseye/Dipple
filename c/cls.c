#include <stdio.h>

/* Properly clear the screen, even on AmigaOS 1.3 --
   Move to top left corner, THEN erase to end of display. */

int main(int argc, char **argv)
{
    printf("%c[1;1H%c[2J", 27, 27);
    return 0;
}
