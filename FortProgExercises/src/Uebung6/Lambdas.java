package Uebung6;

import java.util.*;

public class Lambdas {

	interface FunFoldr<A,B> {
		B call(A a, B b);
	}
	
	interface FunFoldl<A,B> {
		B call(B b, A a);
	}
	
	static <A,B> B foldr(FunFoldr<A,B> f, B z, List<A> xs) {
		B result = z;
		
		for(int i = xs.size()-1; i >= 0; i--){
			result = f.call(xs.get(i), result);
		}
		
		return result;
	}
	
	static <A,B> B foldl(FunFoldl<A,B> f, B z, List<A> xs) {
		B result = z;
		
		for(A a : xs) {
			result = f.call(result, a);
		}
		
		return result;
	}
	
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
