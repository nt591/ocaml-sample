open OUnit2
type day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday

let next_weekday d =
  match d with
  | Monday -> Tuesday
  | Tuesday -> Wednesday
  | Wednesday -> Thursday
  | Thursday -> Friday
  | Friday -> Saturday
  | Saturday -> Sunday
  | Sunday -> Monday

let make_next_weekday_test name first_day next_day =
  name>:: (fun _ -> assert_equal (next_weekday first_day) next_day)

let tests = "test suite for next_weekday" >::: [
  make_next_weekday_test "tuesday_after_monday" Monday Tuesday;
  make_next_weekday_test "wednesday_after_tuesday" Tuesday Wednesday;
  make_next_weekday_test "thursday_after_wednesday" Wednesday Thursday;
  make_next_weekday_test "friday_after_thursday" Thursday Friday;
  make_next_weekday_test "saturday_after_friday" Friday Saturday;
  make_next_weekday_test "sunday_after_saturday" Saturday Sunday;
  make_next_weekday_test "monday_after_sunday" Sunday Monday;
]

let _ = run_test_tt_main tests
