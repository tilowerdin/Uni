package Vorl;

import java.util.*;

public class Higher {

  interface Fun<A,B> {  // Interface for objects of functional type
    B call (A arg);     // which must provide a "call" method
  }
  
  static <A,B> List<B> map (Fun<A,B> f, List<A> xs) {
    List<B> ys = new ArrayList<B> (xs.size());
    for (A x : xs) {
      ys.add(f.call(x));   // here we apply the interface
    }
    return ys;
  }
    
  // Example: apply function (*3) on all list elements
  public static void main (String[] args) {
    List<Integer> a = Arrays.asList(1,2,3,4,5);
    a = map(x -> x * x, a);
    System.out.println(a);
  }
}