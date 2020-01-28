
class HelloWorld {
  public static void main(String[] args) {
    String thing = "hi there";

    {
      int x = 12;
      System.out.println(String.format("%d", x));
    }

    for (int i = 0; i < args.length; i++) {

    }
    System.out.println(String.format("%d", i));

    System.out.println(String.format("Greeting: %s", thing));

    System.exit(0);
  }
}