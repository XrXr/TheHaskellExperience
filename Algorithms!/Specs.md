Assignment 9 - Algorithms!

For this assignment, you will be solving some problems and implementing the solutions.  The idea is to design and implement solutions that are as fast as possible, and to analyze these solutions as well.  You will submit two files:

    a9.py contains the functions you are asked to write for each question
    a9.txt contains, for each function, a rough analysis of its running time when the argument, n, or the length of the array, a, is 10,000. For example, if your function looks like this:

    def dumbdumb(a):
        s = 0
        for x in a:
            for y in a:
                s += x*y
        return s

    Then you would write: "This function has two nested for loops. The inner loop executes 10,000 times for each step of the outer loop, which also executes 10,000 times. Therefore, this function performs roughly 10,000*10,000=100,000,000 operations."
    If your function uses the sort() method to sort a, just say that this uses about 140,000 operations.

Problems

Here are the problems to solve:

1. Write a function, fib0(n), that takes an integer, n>0 as an argument.  The function fib0(n) is defined recursively as follows:
        fib0(0) = 1
        fib0(n) = 2*fib0(n-1), if n≥1.
    Note that, although fib0(n) is defined recursively, you don't have to use recursion to implement it.
2. Write a function, fib3(n), that takes an integer, n>0 as an argument.  The function fib3(n) is defined recursively as follows:
        fib3(0) = 1
        fib3(1) = 1
        fib3(2) = 1
        fib3(n) = fib3(n-1) + fib3(n-2) + fib3(n-3), if n≥3.
    Note that, although fib3(n) is defined recursively, you don't have to use recursion to implement it.
3. Write a function, largest_two(a), that computes the sum of the two largest values in the array a.

4. Write a function, smallest_half(a), that computes the sum of the len(a)//2 smallest values in the array a

5. Write a function, median(a), that returns a value, x, such that at least half the elements in a are less than or equal to x and at least half the elements in a are greater than or equal to x

6. Write a function, majority(a), that returns a value in a that occurs at least len(a)//2 + 1 times.  If no such element exists in a, then this function returns None.

7. Write a function, canadian_change(n), that take a monetary amount in canadian cents, rounds it to the nearest five cents, and prints out the best way of making this amount using $100 bills, $50 bills, $20 bills, $10 bills, $5 bills, $2 coins, $1 coins, $.25 coins, $.10 coins, $.05 coins.  For instance, calling canadian_change(22752) should print something like:

        $227.57 gets rounded to $227.55
        2 x $100
        1 x $20
        1 x $5
        1 x $2
        2 x $.25
        1 x $.05

8. Write a function, triple_sum(a, x), that takes an array, a, and determines if there are three distinct elements in the array a whose sum is x.  If so, it returns the indices of these three elements.  If not, it returns none.  For instance, if a=[1, 5, 8, 2, 6, 55, 90] and x=103, then triple_sum(a, x) would return (1, 2, 6) because a[1]+a[2]+a[6]=5+8+90=103.

9. Bonus: Write a function bentley(a) that returns a pair of indices (i,j) such that a[i]+a[i+1]+...+a[j-1] is maximum over all such pairs (i,j). [Note that a may contain negative values, so the answer is not always (0,len(a)).]