#define STOP 0
#define ADD 1
#define MUL 2
#define PUSH 3

#define pop() (stack[--sp])
#define push(X) (stack[sp++] = X)

int run(char *code) {
    int stack[1000];
    int sp = 0, size = 0, val = 0;
    char *ip = code;

    while (*ip != STOP) {
        switch (*ip++) {
            case ADD: push(pop() + pop());
                      break;
            case MUL: push(pop() * pop());
                      break;
            case PUSH:
                size = *ip++;
                val = 0;
                while (size--) { val = val * 256 + *ip++; }
                push(val);
                break;
        }
    }
    return pop();
}
