#pragma once

#include <termios.h>
#include <utility>
#include <vector>
#include <string>
#include <functional>

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

class TermConfig {
public:
    TermConfig(int ifd, int ofd);
    ~TermConfig();

    std::pair<int, int> getWindowSize();
    int ReadKey();
    void Register(std::function<std::pair<std::string, std::vector<int>>(int)> generator);

private:
    termios configTerm(termios orig_termios);
    int m_ifd{};
    int m_ofd{};
    termios orig_termios; /* In order to restore at exit.*/
};

