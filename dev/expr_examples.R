
# old/special syntax
c(
  "clinic(!=)", # -> translates to "train$clinic != test$clinic"
  "clinic(==)",
  "clinic(==)#clinic_same",
  "clinic(==)#clinic_same[c]",
  "year(-, ==, 1)",
  "year(-, >=, 1)",
  "year(-, <=, 2)",
  "year(-, %in%, 1:2)",
  "year(-; %in%; c(1, 3))"
)


# new/general format
c(
  "test$year - train$year %in% 1:2",
  "test$country != train$country",
  "test[['country']] != train[['country']]",

  "test$country != train$country #scope='obs-set' #target='relation' #type='context'",
)


# "comfortable"/"intelligent wrapper around define_constraint
specify_constraint(
  expr,
  scope = detect_scope(expr, target),
  target = detect_target(expr),
  type = detect_type(expr),
  vars = detect_vars(expr),
  name = detect_name(expr)
)

define_constraint(
  expr,
  scope = c(test="obs", train="set"), # "set-obs", "obs-set" for target = "relation", maybe use two args: scope_train, scope_test
  target = c("test", "relation", "train"),
  type = c("context", "population", "admissibility"),
  vars = detect_vars(expr),
  name = detect_name(expr)
)
