
D <- 1000
K <- 5
h <- 0.25

calc_EOQ <- function(D = 1000) {
  K <- 5
  h <- 0.25
  Q <- sqrt(2*D*K/h)
  Q
}

calc_EOQ()
#⁠#⁠ 200

calc_EOQ(D = 4000)
#⁠#⁠ 400





roll2 <- function(faces = 1:6, number_of_dice = 2) {
  dice <- sample(x = faces, size = number_of_dice, replace = TRUE)
  sum(dice)
}
roll2()
#⁠#⁠ 10

#⁠ Four Tetrahedron shaped dice (Four faces)
roll2(faces = 1:4, number_of_dice = 4)
#⁠#⁠ 11



xcode-select --install