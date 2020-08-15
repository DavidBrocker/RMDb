
# 10 Quizzes = 50pt
#  3 Tests = 300pts
#  1 Paper = 150pts

grades <- data.frame(
  items=c(
  "quiz_1",
  "quiz_2",
  "quiz_3",
  "quiz_4",
  "quiz_5",
  "quiz_6",
  "quiz_7",
  "quiz_8",
  "quiz_9",
  "quiz_10",
  "exam_1",
  "exam_2",
  "exam_3",
  "paper"),
  grade=c(
    5,5,5,5,5,5,5,5,5,5,100,100,100,150
  )
)
grades$m_grade <- sum(grades$grade)/500*100
idk <- sample(grades$m_grade,1)

ggplot(grades,aes(items,grade))+
  geom_point()+
  theme_classic()+
  geom_hline(aes(yintercept = m_grade,color="red"))+
  annotate("text",label=idk,x=10,y=50)

grades$m_grade[1]
