/*
 * A classic maze-generator in C, by Chris Pressey, sometime late in 2011.
 *
 * The implementation does not use recursion, or indeed a stack; back-
 * tracking information is stored right in the array in which we generate
 * the maze.  I encountered this idea in a BASIC maze-generator program
 * which I dimly remember from the magazine "Enter" in the 80's, but I
 * don't think I appreciated it until writing a recursive maze-generator
 * in Pascal sometime in the early 90's.  I did not try this in-place
 * method until writing the program that now stands before you.
 *
 * The coding style is particularly inelegant (observe the heavy use of
 * global variables!) because this source was meant to be an intermediate
 * step towards implementing the algorithm in assembly.
 */

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <assert.h>

#define SIZE_X 40
#define SIZE_Y 21

/*
 * top four bits is reverse
 * bottom four bits is tried
 */
unsigned char *maze;

#define UNSEEN  0x00
#define START   0x0f

#define NORTH   0x01
#define SOUTH   0x02
#define EAST    0x04
#define WEST    0x08
#define NONE    0x00
#define ALL     0x0f

unsigned char d, opp, x, y;
signed char dx, dy;
int pos;

void set_reverse(unsigned char reverse)
{
    maze[pos] = (maze[pos] & 0x0f) | (reverse << 4);
}

unsigned char get_reverse(void)
{
    return (maze[pos] & 0xf0) >> 4;
}

void mark_tried(unsigned char d)
{
    maze[pos] |= d;
}

void set_tried(unsigned char d)
{
    maze[pos] = (maze[pos] & 0xf0) | (d & 0x0f);
}

unsigned char get_tried(void)
{
    return maze[pos] & 0x0f;
}

void calc_pos(void)
{
    pos = y * SIZE_X + x;
}

void clear(void)
{
    for (x = 0; x < SIZE_X; x++)
        for (y = 0; y < SIZE_Y; y++) {
            calc_pos();
            maze[pos] = 0;
        }
}

void draw(void)
{
    for (y = 0; y < SIZE_Y; y++) {
        for (x = 0; x < SIZE_X; x++) {
            calc_pos();
            if (x % 2 == 1 && y % 2 == 1)
                assert(get_tried() == ALL);
            if (get_tried() == ALL) {
                printf(" ");
            } else {
                printf("#");
            }
        }
        printf("#\n");
    }
}

void get_delta(int d)
{
    switch (d) {
        case NORTH:
            opp = SOUTH;
            dx = 0;
            dy = -1;
            break;
        case SOUTH:
            opp = NORTH;
            dx = 0;
            dy = 1;
            break;
        case EAST:
            opp = WEST;
            dx = 1;
            dy = 0;
            break;
        case WEST:
            opp = EAST;
            dx = -1;
            dy = 0;
            break;
    }
}

int in_bounds(int nx, int ny)
{
    return nx >= 0 && ny >= 0 && nx < SIZE_X && ny < SIZE_Y;
}

void make(void)
{

again:
    calc_pos();

    if (get_tried() == ALL) {
        if (get_reverse() == START) {
            assert(x == 1 && y == 1);
            return;
        }
        get_delta(get_reverse());
        x += (dx + dx);
        y += (dy + dy);
        goto again;
    }

    d = 1 << (random() & 0x03);
    mark_tried(d);

    get_delta(d);
    if (!in_bounds(x + dx + dx, y + dy + dy))
        goto again;
    x += (dx + dx);
    y += (dy + dy);
    calc_pos();
    if (get_tried() != NONE) {
        x -= (dx + dx);
        y -= (dy + dy);
        goto again;
    }
    set_reverse(opp);
    x -= dx;
    y -= dy;
    calc_pos();
    set_tried(ALL);
    x += dx;
    y += dy;
    goto again;
}

int main(int argc, char **argv)
{
    srandom(time(NULL));
    maze = malloc(SIZE_X * SIZE_Y * sizeof(unsigned char));
    clear();
    x = 1; y = 1;
    calc_pos();
    set_reverse(START);
    make();
    draw();
    exit(0);
}
