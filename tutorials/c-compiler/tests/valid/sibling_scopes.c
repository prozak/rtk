int main() {
    int total = 0;
    {
        int x = 5;
        total = total + x;
    }
    {
        int x = 7;
        total = total + x;
    }
    if (total > 10) {
        int x = 100;
        total = total + x;
    }
    return total;
}
