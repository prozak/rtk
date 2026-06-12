package test;

@Target({ ElementType.METHOD, ElementType.FIELD })
@interface T { }

@Multi(value = {1, 2, 3}, names = {"a", "b"}, empty = {}, trailing = {1, 2,})
class Annotated {
    @SuppressWarnings({"unchecked", "rawtypes"})
    void m() { }

    @Outer({ @Inner(1), @Inner(name = "x") })
    int field;

    @Wrap(@Inner(2))
    int wrapped;
}
