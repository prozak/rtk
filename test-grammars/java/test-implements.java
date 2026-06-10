class Multiple extends Base implements Comparable, Serializable, Cloneable {
    int y;

    enum Status implements Describable {
        OPEN, CLOSED
    }
}
