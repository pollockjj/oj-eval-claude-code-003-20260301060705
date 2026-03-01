#pragma GCC optimize("O2,unroll-loops")
#include <cstdio>
#include <cstring>
#include <vector>
#include <algorithm>

static const int MAXN = 10005;
static const int MAXM = 26;

struct ProblemStatus {
    bool solved;
    short attempts_before;
    int solve_time;

    bool frozen;
    short frozen_submissions;

    bool real_solved;
    short real_attempts_before;
    int real_solve_time;
};

struct Submission {
    short problem;
    short status;
    int time;
};

struct Team {
    char name[24];
    ProblemStatus problems[MAXM];
    int solved_count;
    int penalty;
    int solve_times_sorted[MAXM]; // descending order
    int name_rank;

    short last_sub[27][5]; // last submission index for each (prob, status)
    Submission* subs;
    int num_subs;
    int subs_cap;

    int ranking;
    int frozen_count;
    bool dirty; // needs sort entry update

    void init_subs() {
        subs_cap = 32;
        subs = (Submission*)malloc(sizeof(Submission) * subs_cap);
        num_subs = 0;
    }

    void add_sub(short prob, short status, int time) {
        if (num_subs == subs_cap) {
            subs_cap *= 2;
            subs = (Submission*)realloc(subs, sizeof(Submission) * subs_cap);
        }
        int idx = num_subs;
        subs[num_subs++] = {prob, status, time};
        last_sub[prob][status] = idx;
        last_sub[prob][4] = idx;
        last_sub[26][status] = idx;
        last_sub[26][4] = idx;
    }

    void init_problem_status(int num_problems) {
        for (int i = 0; i < num_problems; i++) {
            problems[i] = {false, 0, 0, false, 0, false, 0, 0};
        }
        solved_count = 0;
        penalty = 0;
        ranking = 0;
        frozen_count = 0;
        dirty = false;
        memset(last_sub, -1, sizeof(last_sub));
    }

    void rebuild_sort_times(int num_problems) {
        int cnt = 0;
        for (int i = 0; i < num_problems; i++) {
            if (problems[i].solved) {
                solve_times_sorted[cnt++] = problems[i].solve_time;
            }
        }
        std::sort(solve_times_sorted, solve_times_sorted + cnt, [](int a, int b) { return a > b; });
    }

    void recalc_full(int num_problems) {
        solved_count = 0;
        penalty = 0;
        int cnt = 0;
        for (int i = 0; i < num_problems; i++) {
            if (problems[i].solved) {
                solved_count++;
                penalty += 20 * problems[i].attempts_before + problems[i].solve_time;
                solve_times_sorted[cnt++] = problems[i].solve_time;
            }
        }
        std::sort(solve_times_sorted, solve_times_sorted + cnt, [](int a, int b) { return a > b; });
    }
};

static int num_teams;
static int num_problems;
static bool competition_started;
static bool is_frozen;

static Team teams[MAXN];
static int scoreboard[MAXN];
static int temp_board[MAXN]; // temporary buffer for merge
static bool changed[MAXN]; // track which teams changed since last flush
static int changed_list[MAXN];
static int num_changed;

// Custom hash map
struct NameMap {
    static const int HASH_SIZE = (1 << 17);
    static const int HASH_MASK = HASH_SIZE - 1;
    struct Entry {
        char name[24];
        int id;
        int next;
    };
    int head[HASH_SIZE];
    Entry entries[MAXN];
    int num_entries;

    void init() {
        memset(head, -1, sizeof(head));
        num_entries = 0;
    }
    unsigned int hash(const char* s) const {
        unsigned int h = 0;
        while (*s) { h = h * 131 + (unsigned char)*s; s++; }
        return h & HASH_MASK;
    }
    int find(const char* s) const {
        unsigned int h = hash(s);
        for (int i = head[h]; i != -1; i = entries[i].next) {
            if (strcmp(entries[i].name, s) == 0) return entries[i].id;
        }
        return -1;
    }
    void insert(const char* s, int id) {
        unsigned int h = hash(s);
        int idx = num_entries++;
        strcpy(entries[idx].name, s);
        entries[idx].id = id;
        entries[idx].next = head[h];
        head[h] = idx;
    }
} name_map;

static int parse_status(const char* s) {
    switch (s[0]) {
        case 'A': return 0;
        case 'W': return 1;
        case 'R': return 2;
        case 'T': return 3;
    }
    return -1;
}

static const char* status_str[] = {"Accepted", "Wrong_Answer", "Runtime_Error", "Time_Limit_Exceed"};

// Compare: returns true if a should rank higher (smaller rank) than b
inline bool compare_teams(int a_idx, int b_idx) {
    const Team& a = teams[a_idx];
    const Team& b = teams[b_idx];

    if (a.solved_count != b.solved_count) return a.solved_count > b.solved_count;
    if (a.penalty != b.penalty) return a.penalty < b.penalty;

    int cnt = a.solved_count;
    for (int i = 0; i < cnt; i++) {
        if (a.solve_times_sorted[i] != b.solve_times_sorted[i])
            return a.solve_times_sorted[i] < b.solve_times_sorted[i];
    }

    return a.name_rank < b.name_rank;
}

static bool first_flush;

static void flush_scoreboard() {
    // Update sort entries for changed teams
    for (int i = 0; i < num_changed; i++) {
        int tid = changed_list[i];
        teams[tid].rebuild_sort_times(num_problems);
        teams[tid].dirty = false;
    }

    if (!first_flush || num_changed * 10 > num_teams) {
        // Full sort for first flush or when many teams changed
        std::sort(scoreboard, scoreboard + num_teams, compare_teams);
        first_flush = true;
    } else {
        // Incremental: remove changed teams, sort them, merge back
        // Collect changed team indices (their positions in scoreboard)
        // and non-changed teams
        int non_changed_count = 0;
        int changed_count = 0;
        static int non_changed[MAXN];
        static int changed_teams[MAXN];

        for (int i = 0; i < num_teams; i++) {
            int tid = scoreboard[i];
            if (changed[tid]) {
                changed_teams[changed_count++] = tid;
            } else {
                non_changed[non_changed_count++] = tid;
            }
        }

        // Sort changed teams
        std::sort(changed_teams, changed_teams + changed_count, compare_teams);

        // Merge non_changed (already sorted) with changed_teams (just sorted)
        int i = 0, j = 0, k = 0;
        while (i < non_changed_count && j < changed_count) {
            if (compare_teams(non_changed[i], changed_teams[j])) {
                temp_board[k++] = non_changed[i++];
            } else {
                temp_board[k++] = changed_teams[j++];
            }
        }
        while (i < non_changed_count) temp_board[k++] = non_changed[i++];
        while (j < changed_count) temp_board[k++] = changed_teams[j++];

        memcpy(scoreboard, temp_board, sizeof(int) * num_teams);
    }

    for (int i = 0; i < num_teams; i++) {
        teams[scoreboard[i]].ranking = i + 1;
    }

    // Reset changed tracking
    for (int i = 0; i < num_changed; i++) {
        changed[changed_list[i]] = false;
    }
    num_changed = 0;
}

static void mark_changed(int tid) {
    if (!changed[tid]) {
        changed[tid] = true;
        changed_list[num_changed++] = tid;
    }
}

// Buffered output
static char outbuf[1 << 22];
static int outpos;

inline void flush_output() {
    fwrite(outbuf, 1, outpos, stdout);
    outpos = 0;
}

inline void ensure_out(int need) {
    if (outpos + need >= (1 << 22)) flush_output();
}

inline void out_char(char c) {
    outbuf[outpos++] = c;
}

inline void out_str(const char* s) {
    ensure_out(256);
    while (*s) outbuf[outpos++] = *s++;
}

inline void out_int(int x) {
    ensure_out(16);
    if (x == 0) { outbuf[outpos++] = '0'; return; }
    if (x < 0) { outbuf[outpos++] = '-'; x = -x; }
    char tmp[12];
    int len = 0;
    while (x > 0) { tmp[len++] = '0' + x % 10; x /= 10; }
    for (int i = len - 1; i >= 0; i--) outbuf[outpos++] = tmp[i];
}

static void print_scoreboard() {
    for (int i = 0; i < num_teams; i++) {
        int idx = scoreboard[i];
        Team& t = teams[idx];
        ensure_out(512);
        out_str(t.name);
        out_char(' ');
        out_int(i + 1);
        out_char(' ');
        out_int(t.solved_count);
        out_char(' ');
        out_int(t.penalty);
        for (int j = 0; j < num_problems; j++) {
            ProblemStatus& ps = t.problems[j];
            out_char(' ');
            if (ps.frozen) {
                if (ps.attempts_before == 0) out_char('0');
                else { out_char('-'); out_int(ps.attempts_before); }
                out_char('/');
                out_int(ps.frozen_submissions);
            } else if (ps.solved) {
                out_char('+');
                if (ps.attempts_before > 0) out_int(ps.attempts_before);
            } else {
                if (ps.attempts_before == 0) out_char('.');
                else { out_char('-'); out_int(ps.attempts_before); }
            }
        }
        out_char('\n');
    }
}

// Buffered input
static char buf[1 << 22];
static int buf_pos, buf_len;

inline int readchar() {
    if (buf_pos == buf_len) {
        buf_len = fread(buf, 1, sizeof(buf), stdin);
        buf_pos = 0;
        if (buf_len == 0) return -1;
    }
    return (unsigned char)buf[buf_pos++];
}

inline void skipws() {
    int c;
    while ((c = readchar()) != -1 && (c == ' ' || c == '\n' || c == '\r'));
    if (c != -1) buf_pos--;
}

inline int readint() {
    skipws();
    int c, val = 0;
    while ((c = readchar()) != -1 && c >= '0' && c <= '9') {
        val = val * 10 + (c - '0');
    }
    if (c != -1) buf_pos--;
    return val;
}

inline int readword(char* s) {
    skipws();
    int c, len = 0;
    while ((c = readchar()) != -1 && c != ' ' && c != '\n' && c != '\r') {
        s[len++] = c;
    }
    s[len] = 0;
    if (c != -1) buf_pos--;
    return len;
}

int main() {
    num_teams = 0;
    num_changed = 0;
    competition_started = false;
    is_frozen = false;
    first_flush = false;
    outpos = 0;
    memset(changed, 0, sizeof(changed));
    name_map.init();

    char word[64];
    while (readword(word) > 0) {
        if (word[0] == 'A' && word[1] == 'D') {
            char tname[24];
            readword(tname);
            if (competition_started) {
                out_str("[Error]Add failed: competition has started.\n");
            } else {
                if (name_map.find(tname) != -1) {
                    out_str("[Error]Add failed: duplicated team name.\n");
                } else {
                    int id = num_teams++;
                    name_map.insert(tname, id);
                    strcpy(teams[id].name, tname);
                    teams[id].init_subs();
                    scoreboard[id] = id;
                    out_str("[Info]Add successfully.\n");
                }
            }
        }
        else if (word[0] == 'S' && word[1] == 'T') {
            if (competition_started) {
                readword(word); readint(); readword(word); readint();
                out_str("[Error]Start failed: competition has started.\n");
            } else {
                readword(word);
                readint();
                readword(word);
                num_problems = readint();
                competition_started = true;

                for (int i = 0; i < num_teams; i++) {
                    teams[i].init_problem_status(num_problems);
                }

                int temp[MAXN];
                for (int i = 0; i < num_teams; i++) temp[i] = i;
                std::sort(temp, temp + num_teams, [](int a, int b) {
                    return strcmp(teams[a].name, teams[b].name) < 0;
                });
                for (int i = 0; i < num_teams; i++) {
                    teams[temp[i]].name_rank = i;
                    scoreboard[i] = temp[i];
                    teams[temp[i]].ranking = i + 1;
                }

                out_str("[Info]Competition starts.\n");
            }
        }
        else if (word[0] == 'S' && word[1] == 'U') {
            char pname[8], tname[24], sstat[32];
            readword(pname);
            readword(word);
            readword(tname);
            readword(word);
            readword(sstat);
            readword(word);
            int time_val = readint();

            int prob = pname[0] - 'A';
            int status = parse_status(sstat);
            int tid = name_map.find(tname);

            teams[tid].add_sub((short)prob, (short)status, time_val);

            ProblemStatus& ps = teams[tid].problems[prob];

            if (is_frozen && !ps.solved) {
                if (!ps.frozen) {
                    ps.frozen = true;
                    teams[tid].frozen_count++;
                }
                ps.frozen_submissions++;
                if (!ps.real_solved) {
                    if (status == 0) {
                        ps.real_solved = true;
                        ps.real_solve_time = time_val;
                    } else {
                        ps.real_attempts_before++;
                    }
                }
            } else {
                if (!ps.solved) {
                    if (status == 0) {
                        ps.solved = true;
                        ps.solve_time = time_val;
                        ps.real_solved = true;
                        ps.real_solve_time = time_val;
                        ps.real_attempts_before = ps.attempts_before;
                        teams[tid].solved_count++;
                        teams[tid].penalty += 20 * ps.attempts_before + time_val;
                        mark_changed(tid);
                    } else {
                        ps.attempts_before++;
                        ps.real_attempts_before++;
                        // Note: only mark changed if this affects ranking
                        // Actually wrong answers don't change solved/penalty, so no need to mark
                    }
                }
            }
        }
        else if (word[0] == 'F' && word[1] == 'L') {
            flush_scoreboard();
            out_str("[Info]Flush scoreboard.\n");
        }
        else if (word[0] == 'F' && word[1] == 'R') {
            if (is_frozen) {
                out_str("[Error]Freeze failed: scoreboard has been frozen.\n");
            } else {
                is_frozen = true;
                out_str("[Info]Freeze scoreboard.\n");
            }
        }
        else if (word[0] == 'S' && word[1] == 'C') {
            if (!is_frozen) {
                out_str("[Error]Scroll failed: scoreboard has not been frozen.\n");
            } else {
                out_str("[Info]Scroll scoreboard.\n");

                flush_scoreboard();
                print_scoreboard();

                int pos = num_teams - 1;
                while (pos >= 0) {
                    while (pos >= 0 && teams[scoreboard[pos]].frozen_count == 0) pos--;
                    if (pos < 0) break;

                    int idx = scoreboard[pos];
                    int prob = -1;
                    for (int j = 0; j < num_problems; j++) {
                        if (teams[idx].problems[j].frozen) { prob = j; break; }
                    }

                    ProblemStatus& ps = teams[idx].problems[prob];
                    ps.frozen = false;
                    ps.frozen_submissions = 0;
                    ps.solved = ps.real_solved;
                    ps.attempts_before = ps.real_attempts_before;
                    ps.solve_time = ps.real_solve_time;
                    teams[idx].frozen_count--;

                    teams[idx].recalc_full(num_problems);

                    // Binary search for new position
                    // We want the leftmost position where idx should be placed
                    // i.e., find smallest new_pos such that compare_teams(idx, scoreboard[new_pos]) is true for all existing teams to the right
                    // Actually: find the first position from left where idx ranks higher than the team there
                    int old_pos = pos;
                    int lo = 0, hi = old_pos;
                    while (lo < hi) {
                        int mid = (lo + hi) >> 1;
                        if (compare_teams(idx, scoreboard[mid])) {
                            hi = mid;
                        } else {
                            lo = mid + 1;
                        }
                    }
                    int new_pos = lo;

                    if (new_pos < old_pos) {
                        int replaced = scoreboard[new_pos];
                        out_str(teams[idx].name);
                        out_char(' ');
                        out_str(teams[replaced].name);
                        out_char(' ');
                        out_int(teams[idx].solved_count);
                        out_char(' ');
                        out_int(teams[idx].penalty);
                        out_char('\n');

                        int saved = scoreboard[old_pos];
                        memmove(scoreboard + new_pos + 1, scoreboard + new_pos, sizeof(int) * (old_pos - new_pos));
                        scoreboard[new_pos] = saved;
                    } else {
                        if (teams[idx].frozen_count == 0) pos--;
                    }
                }

                // Update rankings after scroll
                for (int i = 0; i < num_teams; i++) {
                    teams[scoreboard[i]].ranking = i + 1;
                }

                print_scoreboard();
                is_frozen = false;
            }
        }
        else if (word[0] == 'Q' && word[6] == 'R') {
            char tname[24];
            readword(tname);
            int tid = name_map.find(tname);
            if (tid == -1) {
                out_str("[Error]Query ranking failed: cannot find the team.\n");
            } else {
                out_str("[Info]Complete query ranking.\n");
                if (is_frozen) {
                    out_str("[Warning]Scoreboard is frozen. The ranking may be inaccurate until it were scrolled.\n");
                }
                out_str(tname);
                out_str(" NOW AT RANKING ");
                out_int(teams[tid].ranking);
                out_char('\n');
            }
        }
        else if (word[0] == 'Q' && word[6] == 'S') {
            char tname[24], pname[8], sstat[32];
            readword(tname);
            readword(word);
            readword(word);
            char* eq = strchr(word, '=');
            strcpy(pname, eq + 1);
            readword(word);
            readword(word);
            eq = strchr(word, '=');
            strcpy(sstat, eq + 1);

            int tid = name_map.find(tname);
            if (tid == -1) {
                out_str("[Error]Query submission failed: cannot find the team.\n");
            } else {
                out_str("[Info]Complete query submission.\n");

                bool all_problems = (strcmp(pname, "ALL") == 0);
                int prob = all_problems ? 26 : (pname[0] - 'A');

                bool all_status = (strcmp(sstat, "ALL") == 0);
                int stat = all_status ? 4 : parse_status(sstat);

                int found_idx = teams[tid].last_sub[prob][stat];

                if (found_idx == -1) {
                    out_str("Cannot find any submission.\n");
                } else {
                    const Submission& s = teams[tid].subs[found_idx];
                    out_str(tname);
                    out_char(' ');
                    out_char((char)('A' + s.problem));
                    out_char(' ');
                    out_str(status_str[s.status]);
                    out_char(' ');
                    out_int(s.time);
                    out_char('\n');
                }
            }
        }
        else if (word[0] == 'E') {
            out_str("[Info]Competition ends.\n");
            break;
        }
    }

    flush_output();
    return 0;
}
