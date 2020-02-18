structure Tests =
struct
  structure Seq = ArraySequence
  open Seq

  type point = int * int

  (* Here are a couple of test structures. ordSet1 is a test ordered set
   * (table), while points1 is a test collection of points in the plane.
   *
   * Note that for an ordered table test, you should just specify a sequence
   * of keys (and values will be automatically set to unit). *)

  val ordSet1 = % [5, 7, 2, 8, 9, 1]

  val testsFirst = [
    (*以下为原始样例*)
    ordSet1,
    % [],
    (*以下为自己添加的测试*)
    % [1,3,5,7,9],
    % [8,7,6,5,4,3,2,1],
    % [100],
    % [0]
  ]
  val testsLast = [
    (*以下为原始样例*)
    ordSet1,
    % [],
    (*以下为自己添加的测试*)
    % [1,3,5,7,9],
    % [6,5,4,3,2,1],
    % [1],
    % [100000]
  ]
  val testsPrev = [
    (*以下为原始样例*)
    (ordSet1, 8),
    (ordSet1, 1),
    (% [], 8),
     (*以下为自己添加的测试*)
    (% [1,3,5,7,9], 1),
    (% [6,5,4,3,2,1], 5),
    (% [1], 2),
    (% [100000], 4)
  ]
  val testsNext = [
    (*以下为原始样例*)
    (ordSet1, 8),
    (ordSet1, 9),
    (% [], 8),
    (*以下为自己添加的测试*)
    (% [1,3,5,7,9], 6),
    (% [6,5,4,3,2,1], 3),
    (% [1], 1),
    (%[1,8,2,6,5], 3)
  ]
  val testsJoin = [
    (*以下为原始样例*)
    (ordSet1, % [100]),
    (ordSet1, % [3]),
    (% [], % [100]),
    (*以下为自己添加的测试*)
    (% [], % [1,2,3,4,5]),
    (% [1,2,3,4,5], %[]),
    (%[], %[])
  ]
  val testsSplit = [
    (*以下为原始样例*)
    (ordSet1, 7),
    (ordSet1, 100),
    (% [], 7),
    (*以下为自己添加的测试*)
    (% [], 1),
    (% [], 0),
    (% [100,99,98,97,96], 0),
    (% [1,2,3,4,5,6,7], 4)
  ]
  val testsRange = [
    (*以下为原始样例*)
    (ordSet1, (5,8)),
    (ordSet1, (10,12)),
    (% [], (5,8)),
    (*以下为自己添加的测试*)
    (% [1,2,5,3,7,4,9],(2,7)),
    (% [], (1,2)),
    (% [], (3,10)),
    (% [1,2,3,4,5,6,7,8,9,10], (1,10)),
    (% [1,2,3,4,5,6,7,8,9,10], (5,6))
  ]

  (*以下为原始测试点*)
  val points1 = % [(0,0),(1,2),(3,3),(4,4),(5,1)]
  val points2 : point seq = % []
  val points3 = % [(10000,10000),(0,0)]
  val points4 = tabulate (fn i => (i,i)) 1000

  (*以下为自己添加的测试点*)
  val mypoint1 = % [(11,0),(10,1),(9,2),(8,3),(7,4),(6,5),(5,6),(4,7),(3,8),(2,9),(1,10),(0,11)]
  val mypoint2 = % [(0,9),(1,1),(2,8),(3,2),(4,7),(5,3),(6,6),(7,4),(8,5)]


  val testsCount = [
    (*以下为原始样例*)
    (points1, ((1,3),(5,1))),
    (points1, ((2,4),(4,2))),
    (points1, ((100,101),(101,100))),

    (points2, ((0,10),(10,0))),
    (points3, ((0,10000),(10000,0))),
    (points4, ((0,500),(1000,0))),

    (*以下为自己添加的测试测试*)
    (mypoint1, ((1,9),(6,1))),
    (mypoint1, ((0,10),(10,0))),
    (mypoint1, ((3,5),(5,3))),

    (mypoint2, ((0,5),(5,0))),
    (mypoint2, ((1,4),(5,0)))

  ]


end
