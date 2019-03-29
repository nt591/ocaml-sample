let valid_date month date =
  if (month = "Jan" || month = "Mar" || month = "May" || month = "Jul" || month = "Aug" || month = "Oct" || month = "Dec") && (date > 0 && date <= 31) then true else
    if (month = "Apr" || month = "Jun" || month = "Sept" || month = "Nov") && (date > 0 && date <= 30) then true else
      if (month = "Feb" && date > 0 && date <= 29) then true else false