int main() {
    int a = 1;
    int b = 0;
    if (a)
        b = 2;
    else
        b = 3;
    if (b == 3)
        return 99;
    return b;
}
