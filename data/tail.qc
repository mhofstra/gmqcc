void(string, ...) print = #1;

float(float i) tail = {
    if (i < 3)
        return 0;
    print("Bar\n");
    return tail(i-1);
};

void() main = {
    tail(5);
    print("Foo\n");
};
