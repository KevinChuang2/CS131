/**
 * You can edit, run, and share this code. 
 * play.kotlinlang.org 
 */

fun everyNth(v:List<Any>, N:Int):List<Any> {
    if(N<=0 || v.size<N)
    	return listOf()
    return (v.filterIndexed {index, _-> (index+1)%N==0}).toList()
}
fun main(args:Array<String>) {
     var L = listOf(1,2,3,4,5,6,7,8,9, 10)
	 var N = 3
     var res =everyNth(L,N)
     L = mutableListOf(1,2,3)
     check_equal(res, listOf(3,6,9), 1)
     
     res = everyNth(L, 10)
     L.add(0)
     check_equal(res, listOf(), 2)
     var temp = listOf(1, "hi", 20, "5", "hello", 100)
     res = everyNth(temp, 2)
     check_equal(res, listOf("hi", "5", 100), 3)
     res = everyNth(temp,0)
     check_equal(res, listOf(), 4)
    
   }
fun check_equal(x:List<Any>, y:List<Any>, num:Int)
{
    if(x==y)
    	println("Test " + num.toString() + " Success" )
    else
    	println("Test " + num.toString() + " Failure")
}