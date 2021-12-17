pdf("simon2stage_design.pdf",width = 12, height = 10)
par(mar = c(0, 0, 0, 0), mai=c(0,0,0,0))
openplotmat()
elpos <- coordinates (c(1, 2, 1, 2)) #, mx = 0.1, my = -0.1)
elpos[2,] <- c(0.25, 0.6)
elpos[3,] <- c(0.75, 0.6)
elpos[5,] <- c(0.25, 0.1)
elpos[6,] <- c(0.75, 0.1)

straightarrow(from = elpos[1, ], to = elpos[2, ], arr.pos=0.8, arr.col = "blue", lcol = "blue")
straightarrow(from = elpos[1, ], to = elpos[3, ], arr.pos=0.8, arr.col = "blue", lcol = "blue")
straightarrow(from = elpos[3, ], to = elpos[4, ], arr.pos=0.76, arr.col = "blue", lcol = "blue")
straightarrow(from = elpos[2, ], to = elpos[4, ], arr.pos=0.76, arr.col = "blue", lcol = "blue")
straightarrow(from = elpos[4, ], to = elpos[5, ], arr.pos=0.8, arr.col = "blue", lcol = "blue")
straightarrow(from = elpos[4, ], to = elpos[6, ], arr.pos=0.8, arr.col = "blue", lcol = "blue")

textround(elpos[1, ], lab = "Enroll n1 subjects in first stage \n and test their response",
          cex = 1.75, radx = 0.162, rady=0.05, shadow.size=0)
textround(elpos[2, ], lab = "If x1 > r1 responses are observed, \n then another n2 subjects are enrolled",
          cex = 1.75, radx=0.162, rady=0.05, shadow.size=0)
textround(elpos[3, ], lab = "Otherwise, fail to reject H0  \n and early terminate the study",
          cex = 1.75, radx=0.162, rady=0.05, shadow.size=0)
textround(elpos[4, ], lab = "Enroll n2 subjects in second \n stage (n = n1 + n2)",
          cex = 1.75, radx=0.162, rady=0.05, shadow.size=0)
textround(elpos[5, ], lab = "If x1 + x2 > r responses are \n observed, then reject H0",
          cex = 1.75, radx=0.162, rady=0.05, shadow.size=0)
textround(elpos[6, ], lab = "Otherwise, fail to reject H0",
          cex = 1.75, radx=0.162, rady=0.05, shadow.size=0)
dev.off()