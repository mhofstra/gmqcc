void(string, ...) print = #1;
string(float)     ftos  = #2;

float(float i) tail = {
    if (i < 3)
        return 0;
    print("Bar\n");
    return tail(i-1); // will not cause a recursive call
};

float(float n, float current) fact_r = {
    if (n < 1)
        return current;
    return fact_r(n-1, n*current); // will not cause a recursion
};

float(float n) fact = { return fact_r(n, 1); };

void() main = {
    tail(5);
    print(ftos(fact(3)), "\n");
};
