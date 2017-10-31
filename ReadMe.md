# Software Requirement and Dependencies
This project is implemented in scala without any third party libraries. The only tool that should be pre-installed is maven( A popular dependency management and building tools for Java and Scala).
# Introduction of algorithm and data structures
I use Scala's iterator to read the file, so all the transformations(such as map, filter) before the terminal operation(foreach, reduce) are lazy, which increases the efficiency of computation and reduces the usage of memory. The procedure to produce medianvals_by_date.txt is just like Apache Spark's reduceByKey operation. I used a hashMap to implement a standalone version of reduceByKey. The procedure to produce medianvals_by_date.txt is a bit more complicated, the number of output is equal to the number of input, so it's similar to a map operation. In addition, we also need a HashMap to store the key(id and zip code) of every record. In order to calculate the median efficiently, I adopt a new data structure called MixHeap. MixHeap is a tree with a root node, a left node which is a Maxheap and a right node which is a Minheap. Values of the nodes in the left subtree should be less than the value of root while values of nodes in right subtree should be larger than the value of node. We require that this tree is balanced, which means that the absolute value of the difference between left and right subtree is less than 2. So when the number of nodes in MixHeap is odd, the median is the root node. When the number of MixHeap is even, the median is the mean of root node and root of left or right subtree. Each insertion and deletion take O(log(n)) time while median calculation takes O(1) time.
# Details about run.sh
I assume that location of the test scripts is the same as run_tests.sh, If not no, please change the run_tests.sh accordingly to make sure that $POM_PATH is the same as the location of pom file.
