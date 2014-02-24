
import scala.io.Source
import scala.annotation.tailrec



object Problems {
  val probList= getClass.getMethods().filter(_.getName.startsWith("problem")).sortBy(_.getName.substring(7).toInt).map( meth => (() => meth.invoke(this).asInstanceOf[Unit]))
  def main(args: Array[String]) = {
    args.toList match {
      case List() => probList.foreach(_())
      case List("a") => probList.foreach(_())
      case List("l") => probList.last()
      case _ => sys.error("No comprehendo")
    }
  }
  def problem1{
    val solution = (1 to 999).filter(x => x%3 == 0 || x%5 == 0).sum
    println(solution)
  }


  def problem2{
    lazy val fib : Stream[BigInt] = 0 #:: 1 #:: fib.zip(fib.tail).map{case (x, y) => x+y}
    val solution = fib.filter(x => x%2 == 0).takeWhile(_ < 4000000).sum
    println(solution)
  }


  def problem3{
    val target = BigInt("600851475143")
    def simplify(target: BigInt, possibleFactor: BigInt): BigInt =
      if(target == 1) possibleFactor
      else if(target % possibleFactor == 0) simplify(target / possibleFactor, possibleFactor)
      else simplify(target, possibleFactor +1)
    println(simplify(target, 2))
  }


  def problem4{
    val products = for(x <- 1 to 999; y <- 1 to 999) yield x*y
    val solution = products.filter(x => x.toString.reverse == x.toString).max
    println(solution)
  }


  def problem5{
    @tailrec
    def gcd(x: BigInt, y: BigInt): BigInt =
      if(x < y) gcd(y, x)
      else if(x % y == 0) y
      else gcd(y, x % y)
    def gcf(x: BigInt, y: BigInt) = x * y / gcd(x,y)
    val solution = (BigInt(1) to BigInt(20)).reduce(gcf(_,_))
    println(gcf(24, 128))
    println(solution)
  }


  def problem6{
    val solution = (1 to 100).sum*(1 to 100).sum - (1 to 100).map(x => x*x).sum
    println(solution)
  }
  def problem7{
    @tailrec
    def findNextPrime(previousPrime: List[BigInt], candidate: BigInt): BigInt = if( previousPrime.forall(candidate%_ !=0)) candidate else findNextPrime(previousPrime, candidate+1)
    @tailrec
    def findAllPrime(index: Int, acc: List[BigInt]): List[BigInt] = if( index == acc.size) acc else findAllPrime(index, findNextPrime(acc, acc.head):: acc)
    val solution = findAllPrime(10001, List(BigInt(2))).head
    println(solution)
  }
  def problem8{
    val str ="""73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450"""
    val list = str.toList.map(x => x.asDigit)
    val solution = (0 to list.size-5).map(x => list.slice(x, x+5).product).max
    println(solution)
  }
  def problem9{
    val triplet = for(a <-1 to 500; b<- 1 to 500; c <- 1 to 500; if a*a+b*b==c*c && a + b + c == 1000) println(a*b*c)
    

  }
  def problem10{
    @tailrec
    def findNextPrime(previousPrime: List[BigInt], candidate: BigInt): BigInt = if( previousPrime.forall(candidate%_ !=0)) candidate else findNextPrime(previousPrime, candidate+1)
    @tailrec
    def findAllPrime(limit: BigInt, acc: List[BigInt]): List[BigInt] ={
      val next =findNextPrime(acc, acc.head)
      if( next > limit) acc else findAllPrime(limit, findNextPrime(acc, acc.head):: acc)
    }
    def isPrime(int: BigInt) : Boolean = {
      def rec(test: BigInt): Boolean = if (test*test > int) true else if(int % test == 0) false else  rec(test+1)
      rec(2)
    }
    val solution = {
      def rec(cand: BigInt, acc: BigInt): BigInt = if(cand> 2000000) acc else if(isPrime(cand)) rec(cand +1, acc+cand) else rec(cand+1, acc)
      rec(2, 0)
    }
    println(solution)

  }

  def problem11{
    val str = """08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"""
    val table:List[List[Int]] = str.split("\n").toList.map(_.split(" ").map(_.toInt).toList)
    val row = for(x <- 0 to table(0).size-4; y <- 0 to table.size-1) yield table(x)(y)*table(x+1)(y)*table(x+2)(y)*table(x+3)(y)
    val col = for(x <- 0 to table(0).size-1; y <- 0 to table.size-4) yield table(x)(y)*table(x)(y+1)*table(x)(y+2)*table(x)(y+3)
    val dia = for(x <- 0 to table(0).size-4; y <- 0 to table.size-4) yield table(x)(y)*table(x+1)(y+1)*table(x+2)(y+2)*table(x+3)(y+3)
    val cdia =  for(x <- 3 to table(0).size-1; y <- 0 to table.size-4) yield table(x)(y)*table(x-1)(y+1)*table(x-2)(y+2)*table(x-3)(y+3)
    val solution = List(row, col, dia, cdia).map(_.max).max
    println(solution)
  }

  def problem12{
    def tri(index: BigInt)= index*(index+1)/2
    def nbDiv(target: BigInt, test: BigInt, acc: Int):BigInt = if(test*test > target) acc else if(test*test == target) acc+1 else if(target%test == 0) nbDiv(target, test +1, acc+2) else nbDiv(target, test+1, acc)
    def search(index: BigInt): BigInt = if(nbDiv(tri(index), 1, 0) >500) tri(index) else search(index+1)
    val solution = search(1)
    println(solution)
  }

  def problem13{
    val str = """37107287533902102798797998220837590246510135740250
46376937677490009712648124896970078050417018260538
74324986199524741059474233309513058123726617309629
91942213363574161572522430563301811072406154908250
23067588207539346171171980310421047513778063246676
89261670696623633820136378418383684178734361726757
28112879812849979408065481931592621691275889832738
44274228917432520321923589422876796487670272189318
47451445736001306439091167216856844588711603153276
70386486105843025439939619828917593665686757934951
62176457141856560629502157223196586755079324193331
64906352462741904929101432445813822663347944758178
92575867718337217661963751590579239728245598838407
58203565325359399008402633568948830189458628227828
80181199384826282014278194139940567587151170094390
35398664372827112653829987240784473053190104293586
86515506006295864861532075273371959191420517255829
71693888707715466499115593487603532921714970056938
54370070576826684624621495650076471787294438377604
53282654108756828443191190634694037855217779295145
36123272525000296071075082563815656710885258350721
45876576172410976447339110607218265236877223636045
17423706905851860660448207621209813287860733969412
81142660418086830619328460811191061556940512689692
51934325451728388641918047049293215058642563049483
62467221648435076201727918039944693004732956340691
15732444386908125794514089057706229429197107928209
55037687525678773091862540744969844508330393682126
18336384825330154686196124348767681297534375946515
80386287592878490201521685554828717201219257766954
78182833757993103614740356856449095527097864797581
16726320100436897842553539920931837441497806860984
48403098129077791799088218795327364475675590848030
87086987551392711854517078544161852424320693150332
59959406895756536782107074926966537676326235447210
69793950679652694742597709739166693763042633987085
41052684708299085211399427365734116182760315001271
65378607361501080857009149939512557028198746004375
35829035317434717326932123578154982629742552737307
94953759765105305946966067683156574377167401875275
88902802571733229619176668713819931811048770190271
25267680276078003013678680992525463401061632866526
36270218540497705585629946580636237993140746255962
24074486908231174977792365466257246923322810917141
91430288197103288597806669760892938638285025333403
34413065578016127815921815005561868836468420090470
23053081172816430487623791969842487255036638784583
11487696932154902810424020138335124462181441773470
63783299490636259666498587618221225225512486764533
67720186971698544312419572409913959008952310058822
95548255300263520781532296796249481641953868218774
76085327132285723110424803456124867697064507995236
37774242535411291684276865538926205024910326572967
23701913275725675285653248258265463092207058596522
29798860272258331913126375147341994889534765745501
18495701454879288984856827726077713721403798879715
38298203783031473527721580348144513491373226651381
34829543829199918180278916522431027392251122869539
40957953066405232632538044100059654939159879593635
29746152185502371307642255121183693803580388584903
41698116222072977186158236678424689157993532961922
62467957194401269043877107275048102390895523597457
23189706772547915061505504953922979530901129967519
86188088225875314529584099251203829009407770775672
11306739708304724483816533873502340845647058077308
82959174767140363198008187129011875491310547126581
97623331044818386269515456334926366572897563400500
42846280183517070527831839425882145521227251250327
55121603546981200581762165212827652751691296897789
32238195734329339946437501907836945765883352399886
75506164965184775180738168837861091527357929701337
62177842752192623401942399639168044983993173312731
32924185707147349566916674687634660915035914677504
99518671430235219628894890102423325116913619626622
73267460800591547471830798392868535206946944540724
76841822524674417161514036427982273348055556214818
97142617910342598647204516893989422179826088076852
87783646182799346313767754307809363333018982642090
10848802521674670883215120185883543223812876952786
71329612474782464538636993009049310363619763878039
62184073572399794223406235393808339651327408011116
66627891981488087797941876876144230030984490851411
60661826293682836764744779239180335110989069790714
85786944089552990653640447425576083659976645795096
66024396409905389607120198219976047599490197230297
64913982680032973156037120041377903785566085089252
16730939319872750275468906903707539413042652315011
94809377245048795150954100921645863754710598436791
78639167021187492431995700641917969777599028300699
15368713711936614952811305876380278410754449733078
40789923115535562561142322423255033685442488917353
44889911501440648020369068063960672322193204149535
41503128880339536053299340368006977710650566631954
81234880673210146739058568557934581403627822703280
82616570773948327592232845941706525094512325230608
22918802058777319719839450180888072429661980811197
77158542502016545090413245809786882778948721859617
72107838435069186155435662884062257473692284509516
20849603980134001723930671666823555245252804609722
53503534226472524250874054075591789781264330331690"""
    val solution = str.split("\n").map(BigInt(_)).sum
    println(solution)
  }

  def problem14{
    def fly(pos: BigInt, acc: BigInt) : BigInt = if(pos == 1) acc else if(pos % 2 ==0) fly(pos/2, acc+1) else fly(3*pos+1, acc+1)
    
    def find(cand: BigInt, best: (BigInt, BigInt)): BigInt = if(cand > 1000000) best._1 else { val len = fly(cand, 0); if(len > best._2) find(cand+1, (cand, len)) else find(cand +1 , best)}
    val solution = find(2, (1,1))
    println(solution)

  }

  def problem15{
    def fac(int :Int, acc: BigInt): BigInt = if(int == 0) acc else fac(int -1, acc * int)
    val solution = fac(40, 1)/fac(20,1)/fac(20,1)
    println(solution)

  }
  def problem16{
    val big = BigInt(2).pow(1000)    
    val solution = big.toString.toList.map(_.asDigit).sum
    println(solution)
  }

  def problem17{
    val smallerThan20 = List("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")
    val lThan20 = smallerThan20.map(_.length)
    val tens = List("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
    val tensLen = tens.map(_.length)
    val hun = "hundred"
    val hunLen = hun.length
    def name(int: Int): String = if(int >= 100) smallerThan20(int/100-1) + " " + hun + (if(int%100 != 0)" and" else "") + " " + name(int%100) else if(int >= 20) tens(int/10 -2)+" " + name(int%10) else if (int == 0) "" else smallerThan20(int -1)
    def findLength(int: Int): Int = if(int >= 100) hunLen+lThan20(int/100-1)+(if(int%100 != 0) 3 else 0)+findLength(int%100) else if(int >= 20) tensLen(int/10 -2) + findLength(int%10) else if(int == 0) 0 else lThan20(int -1)
    println(findLength(342))
    println(findLength(115))
    val solution = (1 to 999).map(findLength(_)).sum+11
    println(solution)
  }

  def problem18{
    val str = """75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"""
    val pyr = str.split("\n").toList.map(_.split(" ").toList.map(_.toInt))
    def findBestRoute(floor: Int): List[Int] = {
      if(floor == 0) pyr(0)
      else { 
        val lastFloor = 0 ::  findBestRoute(floor-1) ::: List(0)
        (0 to floor).map(x => List(lastFloor(x), lastFloor(x+1)).max).zip(pyr(floor)).toList.map{case (x,y) => x+y}        
      }
    }
    val solution = findBestRoute(14).max
    println(solution)


  }

  def problem19{
    def count(curYear: Int, curMonth: Int, curDay: Int, acc: Int): Int = curMonth match{
      case 0 => count(curYear, curMonth + 1, (curDay + 31) % 7, if(curDay == 0) acc + 1 else acc)
      case 1 => count(curYear, curMonth + 1, (if(curYear%4 == 0 && (curYear%100 !=0 || curYear%400 == 0)) curDay + 29 else curDay + 28) % 7, if(curDay == 0) acc + 1 else acc)
      case 2 => count(curYear, curMonth + 1, (curDay + 31) % 7, if(curDay == 0) acc + 1 else acc)
      case 3 => count(curYear, curMonth + 1, (curDay + 30) % 7, if(curDay == 0) acc + 1 else acc)
      case 4 => count(curYear, curMonth + 1, (curDay + 31) % 7, if(curDay == 0) acc + 1 else acc)
      case 5 => count(curYear, curMonth + 1, (curDay + 30) % 7, if(curDay == 0) acc + 1 else acc)
      case 6 => count(curYear, curMonth + 1, (curDay + 31) % 7, if(curDay == 0) acc + 1 else acc)
      case 7 => count(curYear, curMonth + 1, (curDay + 31) % 7, if(curDay == 0) acc + 1 else acc)
      case 8 => count(curYear, curMonth + 1, (curDay + 30) % 7, if(curDay == 0) acc + 1 else acc)
      case 9 => count(curYear, curMonth + 1, (curDay + 31) % 7, if(curDay == 0) acc + 1 else acc)
      case 10 => count(curYear, curMonth + 1, (curDay + 30) % 7, if(curDay == 0) acc + 1 else acc)
      case 11 => if(curYear == 2000) if(curDay == 0) acc+1 else acc  else count(curYear+1, 0, (curDay + 31) % 7, if(curDay == 0) acc + 1 else acc)
      case _ => sys.error("What?")
    }
    val solution = count(1901, 0, 2, 0)
    println(solution)
  }

  def problem20{
    def fac(int: Int, acc: BigInt): BigInt = if (int == 0) acc else fac(int -1, int * acc)
    val solution = fac(100, 1).toString.toList.map(_.asDigit).sum
    println(solution)
  }

  def problem21{
    def properDiv(int : Int) = (1 to int - 1).filter(int%_ == 0)
    def amicable(int: Int) = {
      val friend = properDiv(int).sum
      (friend != int) && (properDiv(friend).sum == int)
    }
    val solution = (1 to 10000).filter(amicable(_)).sum
    println(solution)


  }

  def problem22{
    val file = Source.fromFile("../names.txt").mkString
    val list = file.split(",").toList.map(x => x.replaceAll("\"",""))
    def value(name : String) = name.toList.map(x => (x-'A').toInt+1).sum
    val solution = list.sorted.map(value(_)).zipWithIndex.map{case (x, y) => BigInt(x*y + x)}.sum
    println(solution)
  }

  def problem23{
    def isAbundant(int : Int) = (1 to int-1).filter(int % _ == 0).sum > int
    val listOfAbundant = (1 to 28123).filter(isAbundant(_)).toList
    def isSum(int: Int) = {
      def search(bigger: List[Int], smaller: List[Int]): Boolean = (bigger, smaller) match {
        case (Nil, _) => false
        case (_, Nil) => false
        case (x :: xs, y::ys) =>
          if(x + y== int) true
          else if(x + y > int) search(bigger, ys)
          else search(xs, smaller)
      }
      search(listOfAbundant, listOfAbundant.reverse)
    }
    val solution = (1 to  28123).filter(!isSum(_)).sum
    println(solution)

  }

  def problem24{
    def findPerm(index: BigInt, remaining : List[Int]) : List[Int] = {
      def fac(x : Int)= {
        def facRec(x: Int, acc: BigInt) : BigInt = if(x == 0) acc else facRec(x-1, x*acc)
        facRec(x, 1)
      }
      def dropIndex[T](ls: List[T], index: BigInt) = {
        val (p1, p2) = ls splitAt index.toInt
        p1 ::: p2.tail
      }
      if(remaining.size == 1) remaining
      else {
        val jump = fac(remaining.size -1)
        println(jump)
        remaining((index / jump).toInt) :: findPerm(index % jump , dropIndex(remaining, index / jump))
      }
    }
    val solution = findPerm(BigInt(1000000 - 1) , (0 to 9).toList)
    println(solution)




  }
  def problem25{
    def find(previous: BigInt, before: BigInt, term: Int): Int = if(previous.toString.toList.size == 1000) term else find(previous + before, previous, term +1)
    val solution = find(1,0, 1)
    println(solution)
  }

  def problem26{
    def fracCycle(d : Int) : Int = {
      def rec(fd: Int) : Int = if((BigInt(10).pow(fd) -1) % d == 0) fd else rec(fd+1)
      rec(1)
    }    
    val solution =(1 to 1000).filterNot(x => x % 5 == 0 || x % 2 == 0).map(x => (x, fracCycle(x))).maxBy(_._2)._1
    println(solution)
  }

  def problem27{
    
    def poly(a: Int, b: Int): Int => Int = x => x*x +a *x +b
    def numOfPrime( f: Int => Int): Int = {
      def rec(i: Int):Int = if(isPrime(f(i))) rec(i+1) else i
      rec(0)
    }
    def isPrime(x : Int) = {
      def rec(i : Int): Boolean = {
        if (i*i > x) true
        else if(x %i == 0) false
        else rec(i+1)
      }
      if(x<2) false 
      else rec(2)
    }
    println(numOfPrime(poly(1, 41)))
    val solution = (for(a <- -999 to 999; b <- -999 to 999) yield (a * b, numOfPrime(poly(a,b)))).maxBy(_._2)._1
    println(solution)
  }

  def problem28{
    def sumInNbTurn(nbTurn: BigInt) = (16 * nbTurn * nbTurn * nbTurn + 26 * nbTurn )/ 3 + 10 * nbTurn * nbTurn + 1
    val solution = sumInNbTurn(500)
    println(solution)
  }
  def problem29{
    val solution = (2 to 100).flatMap( x => (2 to 100).map((x, _))).map(x => BigInt(x._1).pow(x._2)).distinct.size
    println(solution)
  }

  def problem30{
    def isGood(x : Int) = x.toString.toList.map(_.asDigit).map(BigInt(_).pow(5)).sum.toInt == x
    val solution = (2 to 999999).filter(isGood).sum
    println(solution)

  }


  def problem31{
    def find(poss: List[Int], goal: Int): List[List[Int]] = poss match {
      case Nil => List(List(goal))
      case x :: xs => for(num <- (0 to goal/x).toList; next<- find( xs, goal - num * x)) yield num :: next
    }
    val solution = find(List(200, 100, 50, 20, 10, 5, 2), 200)
    println(solution.size)


  }

  def problem32{
    def panDigital(x: Int, y: Int, z: Int) = List(x,y,z).flatMap(_.toString.toList.map(_.asDigit)).sorted.sameElements((1 to 9))
    def quickCheck(x: Int, y: Int) = {
      val list = x.toString.toList ::: y.toString.toList
      list.forall(el => list.count(_ == el) == 1)
    }
    def solutions = for(x <- 1 to 9876; y <- 1 to x-1; if quickCheck(x, y); if panDigital(x,y,x*y)) yield x*y
    println(solutions.distinct.sum)
  }

  def problem33{
    case class Frac(up: Int, down:Int){
      override def equals(that: Any) = that match{
        case Frac(tup, tdown) => this.up * tdown == tup * this.down
        case _ => false
      }
      def modify = (1 to 9).toList.flatMap(x => List(Frac(10* up + x, 10 * down + x),Frac(10* x + up, 10 * down + x),Frac(10* x + up, 10 * x + down),Frac(10* up + x, 10 * x + down)))
    }
    val solutions = for( down <- (2 to 9); up <- (1 to down-1); if Frac(up, down).modify.contains(Frac(up, down))) yield Frac(up, down)
    println(solutions)
  }


  def problem34{
    lazy val facList : Stream[Int] = (1 #:: facList.zipWithIndex.map{case (x,y)=> x*(y+1)}).take(10)
    def isGood(x: Int) = x.toString.toList.map(ch => facList(ch.asDigit)).sum == x
    val solution = (3 to 9999999).filter(isGood).sum    
    println(solution)
  }

  def problem35{
    def getPerm(x: Int) = x.toString.toList.
    getPerm(12344).foreach(println)
  }

}



