#include "termconfig.h"

#include <errno.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <unistd.h>

#include <stdexcept>


TermConfig::TermConfig(int ifd, int ofd) : m_ifd(ifd), m_ofd(ofd) {
    if (!isatty(m_ifd)) {
        errno = ENOTTY;
        throw std::runtime_error("not a tty");
    }
    if (tcgetattr(m_ifd,&orig_termios) == -1) {
        errno = ENOTTY;
        throw std::runtime_error("cannot get terminal attributes");
    }

    auto raw = configTerm(orig_termios);

    /* put terminal in raw mode after flushing */
    if (tcsetattr(m_ifd,TCSAFLUSH,&raw) < 0) {
        errno = ENOTTY;
        throw std::runtime_error("cannot set terminal attributes");
    }
}

TermConfig::~TermConfig() {
    write(m_ifd, "\x1b[2J", 4);
    write(m_ifd, "\x1b[H", 3);
    tcsetattr(m_ifd,TCSAFLUSH,&orig_termios);
}

termios TermConfig::configTerm(termios orig_termios) {
    auto raw = orig_termios;  /* modify the original mode */
    /* input modes: no break, no CR to NL, no parity check, no strip char,
     * no start/stop output control. */
    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    /* output modes - disable post processing */
    raw.c_oflag &= ~(OPOST);
    /* control modes - set 8 bit chars */
    raw.c_cflag |= (CS8);
    /* local modes - choing off, canonical off, no extended functions,
     * no signal chars (^Z,^C) */
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    /* control chars - set return condition: min number of bytes and timer. */
    raw.c_cc[VMIN] = 0; /* Return each byte, or zero for timeout. */
    raw.c_cc[VTIME] = 1; /* 100 ms timeout (unit is tens of second). */
    return raw;
}

// more info at: https://vt100.net/docs/vt100-ug/chapter3.html#ED
static std::pair<int, int> getCursorPosition(int ifd, int ofd) {
    if (write(ofd, "\x1b[6n", 4) != 4) {
        throw std::runtime_error("Could not set cursor report");
    }
    char buf[32] = {};
    unsigned int i = 0;
    // Read the response: ESC [ rows ; cols R
    while (i < sizeof(buf)-1) {
        if (read(ifd,buf+i,1) != 1) break;
        if (buf[i] == 'R') break;
        ++i;
    }

    int rows, cols;
    if (buf[0] != ESC || buf[1] != '[' ||
        (sscanf(buf+2,"%d;%d",&rows,&cols) != 2)) {
        throw std::runtime_error("Did not get parsable response");
    }
    return { rows, cols };
}

static void setCursorPosition(int /*ifd*/, int ofd, int row, int col) {
    char seq[32];
    snprintf(seq, sizeof(seq), "\x1b[%d;%dH", row, col);
    if (write(ofd,seq,strlen(seq)) == -1) {
        throw std::runtime_error("Could not set cursor");
    }
}


// Try to get the number of columns in the current terminal. If the ioctl()
// call fails the function will try to query the terminal itself.
// Returns 0 on success, -1 on error.
std::pair<int, int> TermConfig::getWindowSize() {
    struct winsize ws;
    if (0 && ioctl(1, TIOCGWINSZ, &ws) != -1 && ws.ws_col != 0) {
        return { ws.ws_row, ws.ws_col };
    }
    // if that failed, try reading the terminal itself
    // save the cursor position
    auto orig = getCursorPosition(m_ifd,m_ofd);

    setCursorPosition(m_ifd, m_ofd, 999, 999);
    auto size = getCursorPosition(m_ifd,m_ofd);

    // restore position
    setCursorPosition(m_ifd, m_ofd, orig.first, orig.second);

    return size;
}

/* Read a key from the terminal put in raw mode, trying to handle
 * escape sequences. */
int TermConfig::ReadKey() {
    int nread;
    char c, seq[3];
    while ((nread = read(m_ifd,&c,1)) == 0);
    if (nread == -1) exit(1);

    if (c == ESC) {
        /* If this is just an ESC, we'll timeout here. */
        if (read(m_ifd,seq,1) == 0) return ESC;
        if (read(m_ifd,seq+1,1) == 0) return ESC;

        /* ESC [ sequences. */
        if (seq[0] == '[') {
            if (seq[1] >= '0' && seq[1] <= '9') {
                /* Extended escape, read additional byte. */
                if (read(m_ifd,seq+2,1) == 0) return ESC;
                if (seq[2] == '~') {
                    switch(seq[1]) {
                    case '3': return DEL_KEY;
                    case '5': return PAGE_UP;
                    case '6': return PAGE_DOWN;
                    }
                }
            } else {
                switch(seq[1]) {
                case 'A': return ARROW_UP;
                case 'B': return ARROW_DOWN;
                case 'C': return ARROW_RIGHT;
                case 'D': return ARROW_LEFT;
                case 'H': return HOME_KEY;
                case 'F': return END_KEY;
                }
            }
        }
        /* ESC O sequences. */
        else if (seq[0] == 'O') {
            switch(seq[1]) {
            case 'H': return HOME_KEY;
            case 'F': return END_KEY;
            }
        }
    }
    return c;
}


