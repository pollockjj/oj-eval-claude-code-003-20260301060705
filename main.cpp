#include <cstdio>
#include <cstring>
#include <string>
#include <vector>
#include <algorithm>
#include <unordered_map>

using namespace std;

static const int MAXN = 10005;
static const int MAXM = 26;

struct ProblemStatus {
    bool solved;
    int attempts_before;
    int solve_time;

    bool frozen;
    int frozen_submissions;

    bool real_solved;
    int real_attempts_before;
    int real_solve_time;
};

struct Submission {
    int problem;
    int status;
    int time;
};

struct Team {
    char name[24];
    ProblemStatus problems[MAXM];
    int solved_count;
    int penalty;
    int solve_times_sorted[MAXM]; // descending
    int num_solve_times;
    int name_rank; // lexicographic rank

    vector<Submission> submissions;
    int ranking;
    int frozen_count; // number of frozen problems

    void init_problem_status(int num_problems) {
        for (int i = 0; i < num_problems; i++) {
            problems[i] = {false, 0, 0, false, 0, false, 0, 0};
        }
        solved_count = 0;
        penalty = 0;
        num_solve_times = 0;
        ranking = 0;
        frozen_count = 0;
    }

    void recalc_displayed(int num_problems) {
        solved_count = 0;
        penalty = 0;
        num_solve_times = 0;
        for (int i = 0; i < num_problems; i++) {
            if (problems[i].solved) {
                solved_count++;
                penalty += 20 * problems[i].attempts_before + problems[i].solve_time;
                solve_times_sorted[num_solve_times++] = problems[i].solve_time;
            }
        }
        sort(solve_times_sorted, solve_times_sorted + num_solve_times, [](int a, int b) { return a > b; });
    }
};

static int num_teams;
static int num_problems;
static int duration_time;
static bool competition_started;
static bool is_frozen;

static Team teams[MAXN];
static unordered_map<string, int> team_name_to_id;
static int scoreboard[MAXN];

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

    int cnt = a.num_solve_times;
    for (int i = 0; i < cnt; i++) {
        if (a.solve_times_sorted[i] != b.solve_times_sorted[i])
            return a.solve_times_sorted[i] < b.solve_times_sorted[i];
    }

    return a.name_rank < b.name_rank;
}

static void flush_scoreboard() {
    sort(scoreboard, scoreboard + num_teams, compare_teams);
    for (int i = 0; i < num_teams; i++) {
        teams[scoreboard[i]].ranking = i + 1;
    }
}

// Buffered I/O
static char outbuf[1 << 22];
static int outpos;

inline void flush_output() {
    fwrite(outbuf, 1, outpos, stdout);
    outpos = 0;
}

inline void out_char(char c) {
    if (outpos >= (1 << 22) - 1) flush_output();
    outbuf[outpos++] = c;
}

inline void out_str(const char* s) {
    while (*s) out_char(*s++);
}

inline void out_int(int x) {
    if (x == 0) {
        out_char('0');
        return;
    }
    if (x < 0) {
        out_char('-');
        x = -x;
    }
    char tmp[12];
    int len = 0;
    while (x > 0) {
        tmp[len++] = '0' + x % 10;
        x /= 10;
    }
    for (int i = len - 1; i >= 0; i--) out_char(tmp[i]);
}

static void print_scoreboard() {
    for (int i = 0; i < num_teams; i++) {
        int idx = scoreboard[i];
        Team& t = teams[idx];
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
                if (ps.attempts_before == 0) {
                    out_char('0');
                } else {
                    out_char('-');
                    out_int(ps.attempts_before);
                }
                out_char('/');
                out_int(ps.frozen_submissions);
            } else if (ps.solved) {
                out_char('+');
                if (ps.attempts_before > 0) {
                    out_int(ps.attempts_before);
                }
            } else {
                if (ps.attempts_before == 0) {
                    out_char('.');
                } else {
                    out_char('-');
                    out_int(ps.attempts_before);
                }
            }
        }
        out_char('\n');
    }
}

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
    competition_started = false;
    is_frozen = false;
    outpos = 0;

    char word[64];
    while (readword(word) > 0) {
        if (word[0] == 'A' && word[1] == 'D') {
            // ADDTEAM
            char tname[24];
            readword(tname);

            if (competition_started) {
                out_str("[Error]Add failed: competition has started.\n");
            } else {
                string sname(tname);
                if (team_name_to_id.count(sname)) {
                    out_str("[Error]Add failed: duplicated team name.\n");
                } else {
                    int id = num_teams++;
                    team_name_to_id[sname] = id;
                    strcpy(teams[id].name, tname);
                    scoreboard[id] = id;
                    out_str("[Info]Add successfully.\n");
                }
            }
        }
        else if (word[0] == 'S' && word[1] == 'T') {
            // START
            if (competition_started) {
                readword(word); readint(); readword(word); readint();
                out_str("[Error]Start failed: competition has started.\n");
            } else {
                readword(word);
                duration_time = readint();
                readword(word);
                num_problems = readint();
                competition_started = true;

                for (int i = 0; i < num_teams; i++) {
                    teams[i].init_problem_status(num_problems);
                }

                // Compute name ranks
                int temp[MAXN];
                for (int i = 0; i < num_teams; i++) temp[i] = i;
                sort(temp, temp + num_teams, [](int a, int b) {
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
            // SUBMIT
            char pname[8], tname[24], sstat[32];
            readword(pname);
            readword(word); // BY
            readword(tname);
            readword(word); // WITH
            readword(sstat);
            readword(word); // AT
            int time_val = readint();

            int prob = pname[0] - 'A';
            int status = parse_status(sstat);
            int tid = team_name_to_id[string(tname)];

            teams[tid].submissions.push_back({prob, status, time_val});

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
                    } else {
                        ps.attempts_before++;
                        ps.real_attempts_before++;
                    }
                }
            }

            teams[tid].recalc_displayed(num_problems);
        }
        else if (word[0] == 'F' && word[1] == 'L') {
            // FLUSH
            flush_scoreboard();
            out_str("[Info]Flush scoreboard.\n");
        }
        else if (word[0] == 'F' && word[1] == 'R') {
            // FREEZE
            if (is_frozen) {
                out_str("[Error]Freeze failed: scoreboard has been frozen.\n");
            } else {
                is_frozen = true;
                out_str("[Info]Freeze scoreboard.\n");
            }
        }
        else if (word[0] == 'S' && word[1] == 'C') {
            // SCROLL
            if (!is_frozen) {
                out_str("[Error]Scroll failed: scoreboard has not been frozen.\n");
            } else {
                out_str("[Info]Scroll scoreboard.\n");

                flush_scoreboard();
                print_scoreboard();

                // Scroll: process from bottom
                // Start from the last position and work backwards
                int pos = num_teams - 1;
                while (pos >= 0) {
                    // Find next team (from pos downward) with frozen problems
                    while (pos >= 0 && teams[scoreboard[pos]].frozen_count == 0) {
                        pos--;
                    }
                    if (pos < 0) break;

                    int idx = scoreboard[pos];

                    // Find smallest frozen problem
                    int prob = -1;
                    for (int j = 0; j < num_problems; j++) {
                        if (teams[idx].problems[j].frozen) {
                            prob = j;
                            break;
                        }
                    }

                    ProblemStatus& ps = teams[idx].problems[prob];
                    ps.frozen = false;
                    ps.frozen_submissions = 0;
                    ps.solved = ps.real_solved;
                    ps.attempts_before = ps.real_attempts_before;
                    ps.solve_time = ps.real_solve_time;
                    teams[idx].frozen_count--;

                    teams[idx].recalc_displayed(num_problems);

                    int old_pos = pos;
                    int new_pos = old_pos;
                    while (new_pos > 0 && compare_teams(idx, scoreboard[new_pos - 1])) {
                        new_pos--;
                    }

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
                        for (int i = old_pos; i > new_pos; i--) {
                            scoreboard[i] = scoreboard[i-1];
                        }
                        scoreboard[new_pos] = saved;

                        for (int i = new_pos; i <= old_pos; i++) {
                            teams[scoreboard[i]].ranking = i + 1;
                        }
                        // After moving up, the team at pos might have changed
                        // We need to re-check from pos (the team that was pushed down to pos)
                        // Don't decrement pos yet - re-check current position
                    } else {
                        // No ranking change, check if this team still has frozen problems
                        if (teams[idx].frozen_count > 0) {
                            // Will continue with same team next iteration (same pos)
                        } else {
                            pos--;
                        }
                    }
                }

                print_scoreboard();
                is_frozen = false;
            }
        }
        else if (word[0] == 'Q' && word[6] == 'R') {
            // QUERY_RANKING
            char tname[24];
            readword(tname);
            string sname(tname);

            if (!team_name_to_id.count(sname)) {
                out_str("[Error]Query ranking failed: cannot find the team.\n");
            } else {
                out_str("[Info]Complete query ranking.\n");
                if (is_frozen) {
                    out_str("[Warning]Scoreboard is frozen. The ranking may be inaccurate until it were scrolled.\n");
                }
                int tid = team_name_to_id[sname];
                out_str(tname);
                out_str(" NOW AT RANKING ");
                out_int(teams[tid].ranking);
                out_char('\n');
            }
        }
        else if (word[0] == 'Q' && word[6] == 'S') {
            // QUERY_SUBMISSION
            char tname[24], pname[8], sstat[32];
            readword(tname);
            readword(word); // WHERE
            readword(word); // PROBLEM=xxx
            char* eq = strchr(word, '=');
            strcpy(pname, eq + 1);
            readword(word); // AND
            readword(word); // STATUS=xxx
            eq = strchr(word, '=');
            strcpy(sstat, eq + 1);

            string sname(tname);

            if (!team_name_to_id.count(sname)) {
                out_str("[Error]Query submission failed: cannot find the team.\n");
            } else {
                out_str("[Info]Complete query submission.\n");
                int tid = team_name_to_id[sname];

                bool all_problems = (strcmp(pname, "ALL") == 0);
                int prob = -1;
                if (!all_problems) prob = pname[0] - 'A';

                bool all_status = (strcmp(sstat, "ALL") == 0);
                int stat = -1;
                if (!all_status) stat = parse_status(sstat);

                const vector<Submission>& subs = teams[tid].submissions;
                int found_idx = -1;
                for (int i = (int)subs.size() - 1; i >= 0; i--) {
                    if (!all_problems && subs[i].problem != prob) continue;
                    if (!all_status && subs[i].status != stat) continue;
                    found_idx = i;
                    break;
                }

                if (found_idx == -1) {
                    out_str("Cannot find any submission.\n");
                } else {
                    const Submission& s = subs[found_idx];
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
            // END
            out_str("[Info]Competition ends.\n");
            break;
        }
    }

    flush_output();
    return 0;
}
