structure Tests =
struct

  structure T = MkTreapTable(structure HashKey = IntElt)
  structure S = T.Seq
  open S

  type edge = int * int

  (*a trivial test that has a graph containing 2 vertices and an edge*)
  (*下面为原本的测试样例*)
  val edgeseq = [(1,2)]
  val edgeseq2 = [(1,2),(2,3),(3,4),(2,4),(1,5),(5,4),(5,6),(6,7)]
  val test3 = [(2,1),(4,2),(4,3),(4,5),(1,3),(3,5),(3,2),(1,4),(2,5),(5,1)]
  val testfile = "input/thesaurus.txt"
  val testfile2 = "input/simpletest.txt"

  (*下面为自己添加的测试数据*)
  val myedgeseq1 = [(1,4),(2,6),(3,4),(5,1),(4,2),(5,3),(1,2)]
  val myedgeseq2 = [(1,2),(1,3),(1,4),(1,5),(2,3),(2,4),(2,5),(3,4),(3,5),(4,5)]

  (* The following are required *)
  val testsNum = [(*以下为本身提供的样例*)
                  edgeseq, edgeseq2,test3,
                  (*以下为自己添加的测试*)
                  myedgeseq1,myedgeseq2
                  ];

  val testsOutNeighbors = [(*以下为本身提供的样例*)
                           (edgeseq, 1), (edgeseq, 2),(test3,1),(edgeseq2,5),(edgeseq2,7),(test3,9),
                           (*以下为自己添加的测试*)
                           (edgeseq2,3),(test3,3),(test3,5),
                           (myedgeseq1,1),(myedgeseq1,3),(myedgeseq1,5),
                           (myedgeseq2,1),(myedgeseq2,3),(myedgeseq2,5)]

  val testsReport = [(*以下为本身提供的样例*)
                     ((edgeseq, 1), 2), ((edgeseq2, 1), 4), ((edgeseq2, 1), 7),((test3,4),2),
                     ((test3,1),3),((test3,6),2),((test3,1),6),
                     (*以下为自己添加的测试*)
                     ((edgeseq2,3),4),((test3,3),7),((test3,5),7),
                     ((myedgeseq1,2),6),((myedgeseq1,3),7),((myedgeseq1,4),8),
                     ((myedgeseq2,1),4),((myedgeseq2,4),1),((myedgeseq2,5),2)]

  val testsNumWords =  [testfile, testfile2]

  val testsSynonyms =
    [(*以下为原始测试样例*)
      (testfile2, "HANDSOME"),
     (testfile2, "VINCENT"),
     (testfile2, "PRETTY"),
	 (testfile2,"BADASS"),
     (testfile, "GOOD"),
     (*以下为自己添加的测试样例*)
     (testfile,"EDIT"),
     (testfile,"PRICE"),
     (testfile,"MAY"),
     (testfile2,"YOLO"),
     (testfile2,"BILL")]

  val testsQuery =
    [ (*以下为原始测试样例*)
      (testfile2, ("HANDSOME", "YOLO")),(testfile2,("BADASS","STUPID")),(testfile2,("PRETTY","CHRIS")),
      (testfile, ("GOOD", "BAD")),(testfile, ("CLEAR", "VAGUE")),(testfile, ("LOGICAL", "ILLOGICAL")),
      (testfile,("HAPPY","SAD")),(testfile,("LIBERAL","CONSERVATION")),(testfile, ("EARTHLY", "POISON")),
      (*以下为自己添加的测试样例*)
      (testfile, ("EVENT","DOZE")),(testfile,("EQUIP","LAMENTATION")),
      (testfile,("A","ONE")),(testfile,("ARC","ARCH")),(testfile,("HOME","WAX")),(testfile,("AMPLE","LESS"))]

end
