/* Kilo -- A very simple editor in less than 1-kilo lines of code (as counted
 *         by "cloc"). Does not depend on libcurses, directly emits VT100
 *         escapes on the terminal.
 *
 * -----------------------------------------------------------------------
 *
 * Copyright (C) 2016 Salvatore Sanfilippo <antirez at gmail dot com>
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *  *  Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *  *  Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#define KILO_VERSION "0.0.1"

#define _BSD_SOURCE
#define _GNU_SOURCE

#include <termios.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <unistd.h>
#include <stdarg.h>
#include <fcntl.h>

#include <string>
#include <vector>

/* Syntax highlight types */
#define HL_NORMAL 0
#define HL_NONPRINT 1
#define HL_COMMENT 2   /* Single line comment. */
#define HL_MLCOMMENT 3 /* Multi-line comment. */
#define HL_KEYWORD1 4
#define HL_KEYWORD2 5
#define HL_STRING 6
#define HL_NUMBER 7
#define HL_MATCH 8      /* Search match. */

#define HL_HIGHLIGHT_STRINGS (1<<0)
#define HL_HIGHLIGHT_NUMBERS (1<<1)

struct editorSyntax {
    char const **filematch;
    char const **keywords;
    char const singleline_comment_start[3];
    char const multiline_comment_start[3];
    char const multiline_comment_end[3];
    int flags;
};

/* This structure represents a single line of the file we are editing. */
struct erow {
    size_t idx;            /* Row index in the file, zero-based. */
    std::string chars;        /* Row content. */
    std::string render;       /* Row content "rendered" for screen (for TABs). */
    std::vector<unsigned char> hl;  /* Syntax highlight type for each character in render.*/
    int hl_oc;          /* Row had open comment at end in last syntax highlight
                           check. */
};

struct hlcolor {
    int r,g,b;
};

struct editorConfig {
    int m_cx{};
    int m_cy{};  /* Cursor x and y position in characters */
    int m_rowoff{};     /* Offset of row displayed. */
    int m_coloff{};     /* Offset of column displayed. */
    int m_screenrows{}; /* Number of rows that we can show */
    int m_screencols{}; /* Number of cols that we can show */
    int m_rawmode{};    /* Is terminal raw mode enabled? */
    std::vector<erow> m_rows{};      /* Rows */
    int m_dirty{};      /* File modified but not saved. */
    std::string m_filename{}; /* Currently open filename */
    char m_statusmsg[80] = {};
    time_t m_statusmsg_time{};
    struct editorSyntax *m_syntax{};    /* Current syntax highlight, or NULL. */

    editorConfig();

    void disableRawMode(int fd);
    int enableRawMode(int fd);
    void editorSelectSyntaxHighlight(char *filename);
    int editorOpen(std::string const& filename);
    int editorSave();
    void editorFind(int fd);
    void editorSetStatusMessage(const char *fmt, ...);

    void editorRefreshScreen();
    void editorProcessKeypress(int fd);

    void editorUpdateSyntax(erow &row);
    void editorUpdateRow(erow &row);
    void editorRowDelChar(erow &row, int at);
    void editorDelChar();
    void editorInsertChar(int c);
    void editorDelRow(size_t at);
    void editorMoveCursor(int key);
    void editorRowAppendString(erow *row, std::string const& s);
    void editorInsertRow(size_t at, std::string const& s);
    void editorRowInsertChar(erow *row, int at, int c);
    void editorInsertNewline();

    std::string editorRowsToString() const;

};

static struct editorConfig E_;

enum KEY_ACTION{
        KEY_NULL = 0,       /* NULL */
        CTRL_C = 3,         /* Ctrl-c */
        CTRL_D = 4,         /* Ctrl-d */
        CTRL_F = 6,         /* Ctrl-f */
        CTRL_H = 8,         /* Ctrl-h */
        TAB = 9,            /* Tab */
        CTRL_L = 12,        /* Ctrl+l */
        ENTER = 13,         /* Enter */
        CTRL_Q = 17,        /* Ctrl-q */
        CTRL_S = 19,        /* Ctrl-s */
        CTRL_U = 21,        /* Ctrl-u */
        ESC = 27,           /* Escape */
        BACKSPACE =  127,   /* Backspace */
        /* The following are just soft codes, not really reported by the
         * terminal directly. */
        ARROW_LEFT = 1000,
        ARROW_RIGHT,
        ARROW_UP,
        ARROW_DOWN,
        DEL_KEY,
        HOME_KEY,
        END_KEY,
        PAGE_UP,
        PAGE_DOWN
};

/* =========================== Syntax highlights DB =========================
 *
 * In order to add a new syntax, define two arrays with a list of file name
 * matches and keywords. The file name matches are used in order to match
 * a given syntax with a given file name: if a match pattern starts with a
 * dot, it is matched as the last past of the filename, for example ".c".
 * Otherwise the pattern is just searched inside the filenme, like "Makefile").
 *
 * The list of keywords to highlight is just a list of words, however if they
 * a trailing '|' character is added at the end, they are highlighted in
 * a different color, so that you can have two different sets of keywords.
 *
 * Finally add a stanza in the HLDB global variable with two two arrays
 * of strings, and a set of flags in order to enable highlighting of
 * comments and numbers.
 *
 * The characters for single and multi line comments must be exactly two
 * and must be provided as well (see the C language example).
 *
 * There is no support to highlight patterns currently. */

/* C / C++ */
char const *C_HL_extensions[] = {".c",".cpp",NULL};
char const *C_HL_keywords[] = {
        /* A few C / C++ keywords */
        "switch","if","while","for","break","continue","return","else",
        "struct","union","typedef","static","enum","class",
        /* C types */
        "int|","long|","double|","float|","char|","unsigned|","signed|",
        "void|",NULL
};

/* Here we define an array of syntax highlights by extensions, keywords,
 * comments delimiters and flags. */
struct editorSyntax HLDB[] = {
    {
        /* C / C++ */
        C_HL_extensions,
        C_HL_keywords,
        "//","/*","*/",
        HL_HIGHLIGHT_STRINGS | HL_HIGHLIGHT_NUMBERS
    }
};

#define HLDB_ENTRIES (sizeof(HLDB)/sizeof(HLDB[0]))

/* ======================= Low level terminal handling ====================== */

static struct termios orig_termios; /* In order to restore at exit.*/

void editorConfig::disableRawMode(int fd) {
    /* Don't even check the return value as it's too late. */
    if (m_rawmode) {
        tcsetattr(fd,TCSAFLUSH,&orig_termios);
        m_rawmode = 0;
    }
}

/* Called at exit to avoid remaining in raw mode. */
void editorAtExit() {
    E_.disableRawMode(STDIN_FILENO);
}

/* Raw mode: 1960 magic shit. */
int editorConfig::enableRawMode(int fd) {
    struct termios raw;

    if (m_rawmode) return 0; /* Already enabled. */
    if (!isatty(STDIN_FILENO)) goto fatal;
    atexit(editorAtExit);
    if (tcgetattr(fd,&orig_termios) == -1) goto fatal;

    raw = orig_termios;  /* modify the original mode */
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

    /* put terminal in raw mode after flushing */
    if (tcsetattr(fd,TCSAFLUSH,&raw) < 0) goto fatal;
    m_rawmode = 1;
    return 0;

fatal:
    errno = ENOTTY;
    return -1;
}

/* Read a key from the terminal put in raw mode, trying to handle
 * escape sequences. */
int editorReadKey(int fd) {
    int nread;
    char c, seq[3];
    while ((nread = read(fd,&c,1)) == 0);
    if (nread == -1) exit(1);

    while(1) {
        switch(c) {
        case ESC:    /* escape sequence */
            /* If this is just an ESC, we'll timeout here. */
            if (read(fd,seq,1) == 0) return ESC;
            if (read(fd,seq+1,1) == 0) return ESC;

            /* ESC [ sequences. */
            if (seq[0] == '[') {
                if (seq[1] >= '0' && seq[1] <= '9') {
                    /* Extended escape, read additional byte. */
                    if (read(fd,seq+2,1) == 0) return ESC;
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
            break;
        default:
            return c;
        }
    }
}

/* Use the ESC [6n escape sequence to query the horizontal cursor position
 * and return it. On error -1 is returned, on success the position of the
 * cursor is stored at *rows and *cols and 0 is returned. */
int getCursorPosition(int ifd, int ofd, int *rows, int *cols) {
    char buf[32];
    unsigned int i = 0;

    /* Report cursor location */
    if (write(ofd, "\x1b[6n", 4) != 4) return -1;

    /* Read the response: ESC [ rows ; cols R */
    while (i < sizeof(buf)-1) {
        if (read(ifd,buf+i,1) != 1) break;
        if (buf[i] == 'R') break;
        i++;
    }
    buf[i] = '\0';

    /* Parse it. */
    if (buf[0] != ESC || buf[1] != '[') return -1;
    if (sscanf(buf+2,"%d;%d",rows,cols) != 2) return -1;
    return 0;
}

/* Try to get the number of columns in the current terminal. If the ioctl()
 * call fails the function will try to query the terminal itself.
 * Returns 0 on success, -1 on error. */
int getWindowSize(int ifd, int ofd, int *rows, int *cols) {
    struct winsize ws;

    if (ioctl(1, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
        /* ioctl() failed. Try to query the terminal itself. */
        int orig_row, orig_col, retval;

        /* Get the initial position so we can restore it later. */
        retval = getCursorPosition(ifd,ofd,&orig_row,&orig_col);
        if (retval == -1) goto failed;

        /* Go to right/bottom margin and get position. */
        if (write(ofd,"\x1b[999C\x1b[999B",12) != 12) goto failed;
        retval = getCursorPosition(ifd,ofd,rows,cols);
        if (retval == -1) goto failed;

        /* Restore position. */
        char seq[32];
        snprintf(seq,32,"\x1b[%d;%dH",orig_row,orig_col);
        if (write(ofd,seq,strlen(seq)) == -1) {
            /* Can't recover... */
        }
        return 0;
    } else {
        *cols = ws.ws_col;
        *rows = ws.ws_row;
        return 0;
    }

failed:
    return -1;
}

/* ====================== Syntax highlight color scheme  ==================== */

int is_separator(int c) {
    return c == '\0' || isspace(c) || strchr(",.()+-/*=~%[];",c) != NULL;
}

/* Return true if the specified row last char is part of a multi line comment
 * that starts at this row or at one before, and does not end at the end
 * of the row but spawns to the next row. */
int editorRowHasOpenComment(erow const &row) {
    if (row.render.size() && row.hl[row.render.size()-1] == HL_MLCOMMENT &&
        (row.render.size() < 2 || (row.render[row.render.size()-2] != '*' ||
                            row.render[row.render.size()-1] != '/'))) return 1;
    return 0;
}

/* Set every byte of row->hl (that corresponds to every character in the line)
 * to the right syntax highlight type (HL_* defines). */
void editorConfig::editorUpdateSyntax(erow &row) {
    row.hl = std::vector<unsigned char>(HL_NORMAL, row.render.size());

    if (m_syntax == NULL) return; /* No syntax, everything is HL_NORMAL. */

    int i, prev_sep, in_string, in_comment;
    char const *p;
    char const **keywords = m_syntax->keywords;
    char const *scs = m_syntax->singleline_comment_start;
    char const *mcs = m_syntax->multiline_comment_start;
    char const *mce = m_syntax->multiline_comment_end;

    /* Point to the first non-space char. */
    p = row.render.data();
    i = 0; /* Current char offset */
    while(*p && isspace(*p)) {
        p++;
        i++;
    }
    prev_sep = 1; /* Tell the parser if 'i' points to start of word. */
    in_string = 0; /* Are we inside "" or '' ? */
    in_comment = 0; /* Are we inside multi-line comment? */

    /* If the previous line has an open comment, this line starts
     * with an open comment state. */
    if (row.idx > 0 && editorRowHasOpenComment(m_rows[row.idx-1]))
        in_comment = 1;

    while(*p) {
        /* Handle // comments. */
        if (prev_sep && *p == scs[0] && *(p+1) == scs[1]) {
            /* From here to end is a comment */
            std::fill(row.hl.begin() + i, row.hl.end(), HL_COMMENT);
            return;
        }

        /* Handle multi line comments. */
        if (in_comment) {
            row.hl[i] = HL_MLCOMMENT;
            if (*p == mce[0] && *(p+1) == mce[1]) {
                row.hl[i+1] = HL_MLCOMMENT;
                p += 2; i += 2;
                in_comment = 0;
                prev_sep = 1;
                continue;
            } else {
                prev_sep = 0;
                p++; i++;
                continue;
            }
        } else if (*p == mcs[0] && *(p+1) == mcs[1]) {
            row.hl[i] = HL_MLCOMMENT;
            row.hl[i+1] = HL_MLCOMMENT;
            p += 2; i += 2;
            in_comment = 1;
            prev_sep = 0;
            continue;
        }

        /* Handle "" and '' */
        if (in_string) {
            row.hl[i] = HL_STRING;
            if (*p == '\\') {
                row.hl[i+1] = HL_STRING;
                p += 2; i += 2;
                prev_sep = 0;
                continue;
            }
            if (*p == in_string) in_string = 0;
            p++; i++;
            continue;
        } else {
            if (*p == '"' || *p == '\'') {
                in_string = *p;
                row.hl[i] = HL_STRING;
                p++; i++;
                prev_sep = 0;
                continue;
            }
        }

        /* Handle non printable chars. */
        if (!isprint(*p)) {
            row.hl[i] = HL_NONPRINT;
            p++; i++;
            prev_sep = 0;
            continue;
        }

        /* Handle numbers */
        if ((isdigit(*p) && (prev_sep || row.hl[i-1] == HL_NUMBER)) ||
            (*p == '.' && i >0 && row.hl[i-1] == HL_NUMBER)) {
            row.hl[i] = HL_NUMBER;
            p++; i++;
            prev_sep = 0;
            continue;
        }

        /* Handle keywords and lib calls */
        if (prev_sep) {
            int j;
            for (j = 0; keywords[j]; j++) {
                int klen = strlen(keywords[j]);
                int kw2 = keywords[j][klen-1] == '|';
                if (kw2) klen--;

                if (!memcmp(p,keywords[j],klen) &&
                    is_separator(*(p+klen)))
                {
                    /* Keyword */
                    std::fill(row.hl.begin()+i, row.hl.begin()+i+klen,kw2 ? HL_KEYWORD2 : HL_KEYWORD1);
                    p += klen;
                    i += klen;
                    break;
                }
            }
            if (keywords[j] != NULL) {
                prev_sep = 0;
                continue; /* We had a keyword match */
            }
        }

        /* Not special chars */
        prev_sep = is_separator(*p);
        p++; i++;
    }

    /* Propagate syntax change to the next row if the open commen
     * state changed. This may recursively affect all the following rows
     * in the file. */
    int oc = editorRowHasOpenComment(row);
    if (row.hl_oc != oc && row.idx+1 < m_rows.size())
        editorUpdateSyntax(m_rows[row.idx+1]);
    row.hl_oc = oc;
}

/* Maps syntax highlight token types to terminal colors. */
int editorSyntaxToColor(int hl) {
    switch(hl) {
    case HL_COMMENT:
    case HL_MLCOMMENT: return 36;     /* cyan */
    case HL_KEYWORD1: return 33;    /* yellow */
    case HL_KEYWORD2: return 32;    /* green */
    case HL_STRING: return 35;      /* magenta */
    case HL_NUMBER: return 31;      /* red */
    case HL_MATCH: return 34;      /* blu */
    default: return 37;             /* white */
    }
}

/* Select the syntax highlight scheme depending on the filename,
 * setting it in the global state E.syntax. */
void editorConfig::editorSelectSyntaxHighlight(char *filename) {
    for (unsigned int j = 0; j < HLDB_ENTRIES; j++) {
        struct editorSyntax *s = HLDB+j;
        unsigned int i = 0;
        while(s->filematch[i]) {
            char *p;
            int patlen = strlen(s->filematch[i]);
            if ((p = strstr(filename,s->filematch[i])) != NULL) {
                if (s->filematch[i][0] != '.' || p[patlen] == '\0') {
                    m_syntax = s;
                    return;
                }
            }
            i++;
        }
    }
}

/* ======================= Editor rows implementation ======================= */

/* Update the rendered version and the syntax highlight of a row. */
void editorConfig::editorUpdateRow(erow &row) {
    int tabs = 0, nonprint = 0, idx;

   /* Create a version of the row we can directly print on the screen,
     * respecting tabs, substituting non printable characters with '?'. */
    for (auto& j : row.chars)
        if (j == TAB) tabs++;

    row.render = std::string(row.chars.size() + tabs*8 + nonprint*9, ' ');
    idx = 0;
    for (auto j : row.chars ) {
        if (j == TAB) {
            row.render[idx++] = ' ';
            while((idx+1) % 8 != 0) row.render[idx++] = ' ';
        } else {
            row.render[idx++] = j;
        }
    }

    /* Update the syntax highlighting attributes of the row. */
    editorUpdateSyntax(row);
}

/* Insert a row at the specified position, shifting the other rows on the bottom
 * if required. */
void editorConfig::editorInsertRow(size_t at, std::string const& s) {
    if (at > m_rows.size()) return;
    m_rows.insert(m_rows.begin() + at, erow{});
    if (at != m_rows.size()) {
        for (auto j = at+1; j < m_rows.size(); j++) m_rows[j].idx++;
    }
    m_rows[at].chars = s;
    m_rows[at].hl_oc = 0;
    m_rows[at].idx = at;
    editorUpdateRow(*(m_rows.data()+at));
    m_dirty++;
}

/* Remove the row at the specified position, shifting the remainign on the
 * top. */
void editorConfig::editorDelRow(size_t at) {
    if (at >= m_rows.size()) return;
    m_rows.erase(m_rows.begin()+at);
    for (auto j = at; j < m_rows.size(); j++) m_rows[j].idx++;
    m_dirty++;
}

/* Turn the editor rows into a single heap-allocated string.
 * Returns the pointer to the heap-allocated string and populate the
 * integer pointed by 'buflen' with the size of the string, escluding
 * the final nulterm. */
std::string editorConfig::editorRowsToString() const {
    std::string buf;
    for (auto& row : m_rows) {
        buf += row.chars;
        buf += '\n';
    }
    return buf;
}

/* Insert a character at the specified position in a row, moving the remaining
 * chars on the right if needed. */
void editorConfig::editorRowInsertChar(erow *row, int at, int c) {
    row->chars.insert(at, 1, c);
    editorUpdateRow(*row);
    m_dirty++;
}

/* Append the string 's' at the end of a row */
void editorConfig::editorRowAppendString(erow *row, std::string const& s) {
    row->chars += s;
    editorUpdateRow(*row);
    m_dirty++;
}

/* Delete the character at offset 'at' from the specified row. */
void editorConfig::editorRowDelChar(erow &row, int at) {
    if ((int)row.chars.size() <= at) return;
    row.chars.erase(at, 1);
    editorUpdateRow(row);
    m_dirty++;
}

/* Insert the specified char at the current prompt position. */
void editorConfig::editorInsertChar(int c) {
    size_t filerow = m_rowoff+m_cy;
    int filecol = m_coloff+m_cx;
    erow *row = (filerow >= m_rows.size()) ? NULL : &m_rows[filerow];

    /* If the row where the cursor is currently located does not exist in our
     * logical representaion of the file, add enough empty rows as needed. */
    if (!row) {
        while(m_rows.size() <= filerow)
            editorInsertRow(m_rows.size(), "");
    }
    row = &m_rows[filerow];
    editorRowInsertChar(row,filecol,c);
    if (m_cx == m_screencols-1)
        m_coloff++;
    else
        m_cx++;
    m_dirty++;
}

/* Inserting a newline is slightly complex as we have to handle inserting a
 * newline in the middle of a line, splitting the line as needed. */
void editorConfig::editorInsertNewline() {
    size_t filerow = m_rowoff+m_cy;
    int filecol = m_coloff+m_cx;
    erow *row = (filerow >= m_rows.size()) ? NULL : &m_rows[filerow];

    if (!row) {
        if (filerow == m_rows.size()) {
            editorInsertRow(filerow, "");
            goto fixcursor;
        }
        return;
    }
    /* If the cursor is over the current line size, we want to conceptually
     * think it's just over the last character. */
    if (filecol >= (int)row->chars.size()) filecol = row->chars.size();
    if (filecol == 0) {
        editorInsertRow(filerow, "");
    } else {
        /* We are in the middle of a line. Split it between two rows. */
        editorInsertRow(filerow+1, std::string(row->chars, filecol));
        row = &m_rows[filerow];
        row->chars.erase(filecol);
        editorUpdateRow(*row);
    }
fixcursor:
    if (m_cy == m_screenrows-1) {
        m_rowoff++;
    } else {
        m_cy++;
    }
    m_cx = 0;
    m_coloff = 0;
}

/* Delete the char at the current prompt position. */
void editorConfig::editorDelChar() {
    size_t filerow = m_rowoff+m_cy;
    int filecol = m_coloff+m_cx;
    erow *row = (filerow >= m_rows.size()) ? NULL : &m_rows[filerow];

    if (!row || (filecol == 0 && filerow == 0)) return;
    if (filecol == 0) {
        /* Handle the case of column 0, we need to move the current line
         * on the right of the previous one. */
        filecol = m_rows[filerow-1].chars.size();
        editorRowAppendString(&m_rows[filerow-1],row->chars);
        editorDelRow(filerow);
        row = NULL;
        if (m_cy == 0)
            m_rowoff--;
        else
            m_cy--;
        m_cx = filecol;
        if (m_cx >= m_screencols) {
            int shift = (m_screencols-m_cx)+1;
            m_cx -= shift;
            m_coloff += shift;
        }
    } else {
        editorRowDelChar(*row,filecol-1);
        if (m_cx == 0 && m_coloff)
            m_coloff--;
        else
            m_cx--;
    }
    if (row) editorUpdateRow(*row);
    m_dirty++;
}

/* Load the specified program in the editor memory and returns 0 on success
 * or 1 on error. */
int editorConfig::editorOpen(std::string const& filename_) {
    FILE *fp;

    m_dirty = 0;
    m_filename = filename_;

    fp = fopen(m_filename.c_str(),"r");
    if (!fp) {
        if (errno != ENOENT) {
            perror("Opening file");
            exit(1);
        }
        return 1;
    }

    char *line = NULL;
    size_t linecap = 0;
    ssize_t linelen;
    while((linelen = getline(&line,&linecap,fp)) != -1) {
        if (linelen && (line[linelen-1] == '\n' || line[linelen-1] == '\r'))
            line[--linelen] = '\0';
        editorInsertRow(m_rows.size(), line);
    }
    free(line);
    fclose(fp);
    m_dirty = 0;
    return 0;
}

/* Save the current file on disk. Return 0 on success, 1 on error. */
int editorConfig::editorSave() {
    auto buf = editorRowsToString();
    int fd = open(m_filename.c_str(),O_RDWR|O_CREAT,0644);
    if (fd == -1) goto writeerr;

    /* Use truncate + a single write(2) call in order to make saving
     * a bit safer, under the limits of what we can do in a small editor. */
    if (ftruncate(fd,buf.size()) == -1) goto writeerr;
    if (write(fd,buf.data(),buf.size()) != (int)buf.size()) goto writeerr;

    close(fd);
    m_dirty = 0;
    editorSetStatusMessage("%d bytes written on disk", buf.size());
    return 0;

writeerr:
    if (fd != -1) close(fd);
    editorSetStatusMessage("Can't save! I/O error: %s",strerror(errno));
    return 1;
}

/* ============================= Terminal update ============================ */

/* This function writes the whole screen using VT100 escape characters
 * starting from the logical state of the editor in the global state 'E'. */
void editorConfig::editorRefreshScreen() {
    int y;
    erow *r;
    char buf[32];
    std::string ab;

    ab += "\x1b[?25l"; /* Hide cursor. */
    ab += "\x1b[H"; /* Go home. */
    for (y = 0; y < m_screenrows; y++) {
        size_t filerow = m_rowoff+y;

        if (filerow >= m_rows.size()) {
            if (m_rows.size() == 0 && y == m_screenrows/3) {
                char welcome[80];
                int welcomelen = snprintf(welcome,sizeof(welcome),
                    "Kilo editor -- verison %s\x1b[0K\r\n", KILO_VERSION);
                int padding = (m_screencols-welcomelen)/2;
                if (padding) {
                    ab += "~";
                    padding--;
                }
                while(padding--) ab += " ";
                ab += welcome;
            } else {
                ab += "~\x1b[0K\r\n";
            }
            continue;
        }

        r = &m_rows[filerow];

        int len = r->render.size() - m_coloff;
        int current_color = -1;
        if (len > 0) {
            if (len > m_screencols) len = m_screencols;
            char const *c = r->render.data()+m_coloff;
            unsigned char *hl = r->hl.data()+m_coloff;
            int j;
            for (j = 0; j < len; j++) {
                if (hl[j] == HL_NONPRINT) {
                    char sym;
                    ab += "\x1b[7m";
                    if (c[j] <= 26)
                        sym = '@'+c[j];
                    else
                        sym = '?';
                    ab += sym;
                    ab += "\x1b[0m";
                } else if (hl[j] == HL_NORMAL) {
                    if (current_color != -1) {
                        ab += "\x1b[39m";
                        current_color = -1;
                    }
                    ab += c[j];
                } else {
                    int color = editorSyntaxToColor(hl[j]);
                    if (color != current_color) {
                        char buf[16];
                        snprintf(buf,sizeof(buf),"\x1b[%dm",color);
                        current_color = color;
                        ab += buf;
                    }
                    ab += c[j];
                }
            }
        }
        ab += "\x1b[39m";
        ab += "\x1b[0K";
        ab += "\r\n";
    }

    /* Create a two rows status. First row: */
    ab += "\x1b[0K";
    ab += "\x1b[7m";
    char status[80], rstatus[80];
    int len = snprintf(status, sizeof(status), "%.20s - %d lines %s",
        m_filename.c_str(), (int)m_rows.size(), m_dirty ? "(modified)" : "");
    int rlen = snprintf(rstatus, sizeof(rstatus),
        "%d/%d",m_rowoff+m_cy+1,(int)m_rows.size());
    if (len > m_screencols) len = m_screencols;
    ab += status;
    while(len < m_screencols) {
        if (m_screencols - len == rlen) {
            ab += rstatus;
            break;
        } else {
            ab += " ";
            len++;
        }
    }
    ab += "\x1b[0m\r\n";

    /* Second row depends on m_statusmsg and the status message update time. */
    ab += "\x1b[0K";
    int msglen = strlen(m_statusmsg);
    if (msglen && time(NULL)-m_statusmsg_time < 5)
        ab += m_statusmsg;

    /* Put cursor at its current position. Note that the horizontal position
     * at which the cursor is displayed may be different compared to 'm_cx'
     * because of TABs. */
    int j;
    int cx = 1;
    size_t filerow = m_rowoff+m_cy;
    erow *row = (filerow >= m_rows.size()) ? NULL : &m_rows[filerow];
    if (row) {
        for (j = m_coloff; j < (m_cx+m_coloff); j++) {
            if (j < (int)row->chars.size() && row->chars[j] == TAB) cx += 7-((cx)%8);
            cx++;
        }
    }
    snprintf(buf,sizeof(buf),"\x1b[%d;%dH",m_cy+1,cx);
    ab += buf;
    ab += "\x1b[?25h"; /* Show cursor. */
    write(STDOUT_FILENO,ab.data(),ab.size());
}

/* Set an editor status message for the second line of the status, at the
 * end of the screen. */
void editorConfig::editorSetStatusMessage(const char *fmt, ...) {
    va_list ap;
    va_start(ap,fmt);
    vsnprintf(m_statusmsg,sizeof(m_statusmsg),fmt,ap);
    va_end(ap);
    m_statusmsg_time = time(NULL);
}

/* =============================== Find mode ================================ */

#define KILO_QUERY_LEN 256

void editorConfig::editorFind(int fd) {
    char query[KILO_QUERY_LEN+1] = {0};
    int qlen = 0;
    int last_match = -1; /* Last line where a match was found. -1 for none. */
    int find_next = 0; /* if 1 search next, if -1 search prev. */
    int saved_hl_line = -1;  /* No saved HL */
    std::vector<unsigned char> saved_hl;

#define FIND_RESTORE_HL do { \
    if (!saved_hl.empty()) { \
        m_rows[saved_hl_line].hl = saved_hl; \
        saved_hl.clear(); \
    } \
} while (0)

    /* Save the cursor position in order to restore it later. */
    int saved_cx = m_cx, saved_cy = m_cy;
    int saved_coloff = m_coloff, saved_rowoff = m_rowoff;

    while(1) {
        editorSetStatusMessage( 
            "Search: %s (Use ESC/Arrows/Enter)", query);
        editorRefreshScreen();

        int c = editorReadKey(fd);
        if (c == DEL_KEY || c == CTRL_H || c == BACKSPACE) {
            if (qlen != 0) query[--qlen] = '\0';
            last_match = -1;
        } else if (c == ESC || c == ENTER) {
            if (c == ESC) {
                m_cx = saved_cx; m_cy = saved_cy;
                m_coloff = saved_coloff; m_rowoff = saved_rowoff;
            }
            FIND_RESTORE_HL;
            editorSetStatusMessage("");
            return;
        } else if (c == ARROW_RIGHT || c == ARROW_DOWN) {
            find_next = 1;
        } else if (c == ARROW_LEFT || c == ARROW_UP) {
            find_next = -1;
        } else if (isprint(c)) {
            if (qlen < KILO_QUERY_LEN) {
                query[qlen++] = c;
                query[qlen] = '\0';
                last_match = -1;
            }
        }

        /* Search occurrence. */
        if (last_match == -1) find_next = 1;
        if (find_next) {
            char const *match = NULL;
            int match_offset = 0;
            int i, current = last_match;

            for (i = 0; i < (int)m_rows.size(); i++) {
                current += find_next;
                if (current == -1) current = m_rows.size()-1;
                else if (current == (int)m_rows.size()) current = 0;
                match = strstr(m_rows[current].render.data(),query);
                if (match) {
                    match_offset = match-m_rows[current].render.data();
                    break;
                }
            }
            find_next = 0;

            /* Highlight */
            FIND_RESTORE_HL;

            if (match) {
                erow *row = &m_rows[current];
                last_match = current;
                if (!row->hl.empty()) {
                    saved_hl_line = current;
                    saved_hl = row->hl;
                    std::fill(row->hl.begin()+match_offset,row->hl.begin()+match_offset+qlen, HL_MATCH);
                }
                m_cy = 0;
                m_cx = match_offset;
                m_rowoff = current;
                m_coloff = 0;
                /* Scroll horizontally as needed. */
                if (m_cx > m_screencols) {
                    int diff = m_cx - m_screencols;
                    m_cx -= diff;
                    m_coloff += diff;
                }
            }
        }
    }
}

/* ========================= Editor events handling  ======================== */

/* Handle cursor position change because arrow keys were pressed. */
void editorConfig::editorMoveCursor(int key) {
    size_t filerow = m_rowoff+m_cy;
    int filecol = m_coloff+m_cx;
    int rowlen;
    erow *row = (filerow >= m_rows.size()) ? NULL : &m_rows[filerow];

    switch(key) {
    case ARROW_LEFT:
        if (m_cx == 0) {
            if (m_coloff) {
                m_coloff--;
            } else {
                if (filerow > 0) {
                    m_cy--;
                    m_cx = m_rows[filerow-1].chars.size();
                    if (m_cx > m_screencols-1) {
                        m_coloff = m_cx-m_screencols+1;
                        m_cx = m_screencols-1;
                    }
                }
            }
        } else {
            m_cx -= 1;
        }
        break;
    case ARROW_RIGHT:
        if (row && filecol < (int)row->chars.size()) {
            if (m_cx == m_screencols-1) {
                m_coloff++;
            } else {
                m_cx += 1;
            }
        } else if (row && filecol == (int)row->chars.size()) {
            m_cx = 0;
            m_coloff = 0;
            if (m_cy == m_screenrows-1) {
                m_rowoff++;
            } else {
                m_cy += 1;
            }
        }
        break;
    case ARROW_UP:
        if (m_cy == 0) {
            if (m_rowoff) m_rowoff--;
        } else {
            m_cy -= 1;
        }
        break;
    case ARROW_DOWN:
        if (filerow < m_rows.size()) {
            if (m_cy == m_screenrows-1) {
                m_rowoff++;
            } else {
                m_cy += 1;
            }
        }
        break;
    }
    /* Fix m_cx if the current line has not enough chars. */
    filerow = m_rowoff+m_cy;
    filecol = m_coloff+m_cx;
    row = (filerow >= m_rows.size()) ? NULL : &m_rows[filerow];
    rowlen = row ? row->chars.size() : 0;
    if (filecol > rowlen) {
        m_cx -= filecol-rowlen;
        if (m_cx < 0) {
            m_coloff += m_cx;
            m_cx = 0;
        }
    }
}

/* Process events arriving from the standard input, which is, the user
 * is typing stuff on the terminal. */
#define KILO_QUIT_TIMES 3
void editorConfig::editorProcessKeypress(int fd) {
    /* When the file is modified, requires Ctrl-q to be pressed N times
     * before actually quitting. */
    static int quit_times = KILO_QUIT_TIMES;

    int c = editorReadKey(fd);
    switch(c) {
    case ENTER:         /* Enter */
        editorInsertNewline();
        break;
    case CTRL_C:        /* Ctrl-c */
        /* We ignore ctrl-c, it can't be so simple to lose the changes
         * to the edited file. */
        break;
    case CTRL_Q:        /* Ctrl-q */
        /* Quit if the file was already saved. */
        if (m_dirty && quit_times) {
            editorSetStatusMessage("WARNING!!! File has unsaved changes. "
                "Press Ctrl-Q %d more times to quit.", quit_times);
            quit_times--;
            return;
        }
        exit(0);
        break;
    case CTRL_S:        /* Ctrl-s */
        editorSave();
        break;
    case CTRL_F:
        editorFind(fd);
        break;
    case BACKSPACE:     /* Backspace */
    case CTRL_H:        /* Ctrl-h */
    case DEL_KEY:
        editorDelChar();
        break;
    case PAGE_UP:
    case PAGE_DOWN:
        if (c == PAGE_UP && m_cy != 0)
            m_cy = 0;
        else if (c == PAGE_DOWN && m_cy != m_screenrows-1)
            m_cy = m_screenrows-1;
        {
        int times = m_screenrows;
        while(times--)
            editorMoveCursor(c == PAGE_UP ? ARROW_UP:
                                            ARROW_DOWN);
        }
        break;

    case ARROW_UP:
    case ARROW_DOWN:
    case ARROW_LEFT:
    case ARROW_RIGHT:
        editorMoveCursor(c);
        break;
    case CTRL_L: /* ctrl+l, clear screen */
        /* Just refresht the line as side effect. */
        break;
    case ESC:
        /* Nothing to do for ESC in this mode. */
        break;
    default:
        editorInsertChar(c);
        break;
    }

    quit_times = KILO_QUIT_TIMES; /* Reset it to the original value. */
}

int editorFileWasModified(editorConfig& E) {
    return E.m_dirty;
}

editorConfig::editorConfig()
{
    if (getWindowSize(STDIN_FILENO,STDOUT_FILENO,
                      &m_screenrows,&m_screencols) == -1)
    {
        perror("Unable to query the screen for size (columns / rows)");
        exit(1);
    }
    m_screenrows -= 2; /* Get room for status bar. */
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr,"Usage: kilo <filename>\n");
        exit(1);
    }

    E_.editorSelectSyntaxHighlight(argv[1]);
    E_.editorOpen(argv[1]);
    E_.enableRawMode(STDIN_FILENO);
    E_.editorSetStatusMessage(
        "HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");
    while(1) {
        E_.editorRefreshScreen();
        E_.editorProcessKeypress(STDIN_FILENO);
    }
    return 0;
}
