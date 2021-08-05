library(tidyverse)
library(here)
hotels <- read_csv(here("R","data","hotel_bookings.csv"))

hotels_summary <- hotels %>%
  filter(reservation_status=="Check-Out") %>%
  select(-c(reservation_status, reservation_status_date)) %>%
  group_by(hotel, country, arrival_date_year, arrival_date_month) %>%
  summarise(
    total=n(),
    n_cancellations=sum(is_canceled),
    avg_lead_time=mean(lead_time, na.rm=T),
    n_stays_in_weekend_nights=sum(stays_in_weekend_nights,na.rm=T),
    n_stays_in_week_nights=sum(stays_in_week_nights,na.rm=T),
    avg_adults = mean(adults,na.rm=T),
    avg_children = mean(children,na.rm=T),
    avg_babies = mean(babies, na.rm=T),
    avg_lead_time = mean(lead_time, na.rm=T),
    meal = list(meal),
    market_segment = list(market_segment),
    distribution_channel = list(distribution_channel),
    n_repeat_guest=sum(is_repeated_guest, na.rm = T),
    avg_prev_cancellations=mean(previous_cancellations,na.rm=T),
    avg_prev_bookings_not_cancelled=mean(previous_bookings_not_canceled,na.rm=T),
    reserved_room_type=list(reserved_room_type),
    assigned_room_type=list(assigned_room_type),
    n_reserv_assign_diff=sum(unlist(map2(reserved_room_type, assigned_room_type, function(x,y) return(x!=y)))),
    avg_booking_changes=mean(booking_changes,na.rm=T),
    deposit_type=list(deposit_type),
    agent=list(agent),
    company = list(company),
    avg_days_in_waiting_list = mean(days_in_waiting_list,na.rm=T),
    customer_type=list(customer_type),
    avg_adr=mean(adr, na.rm=T),
    avg_required_car_parking_spaces=mean(required_car_parking_spaces, na.rm=T),
    avg_total_of_special_requests=mean(total_of_special_requests,na.rm=T)
  ) %>%
  ungroup() 

sum_fun <- function(df, id_var){
  col_names <- colnames(df)
  for(i in 1:length(col_names)){
    if(is_list(df[[col_names[i]]])){
      column <- col_names[i]
      x <- df %>%
        unnest(column) %>%
        mutate(col=seq_along(id_var))%>%
        group_by(!col) %>%
        count() %>%
        pivot_wider(names_prefix = glue("n_{column}"),
                    names_from = column,
                    values_from = n, values_fill = 0)
      df <- left_join(df, x)
    }
  }
  return(df)
}


        
res <- hotels_summary %>%
  select(hotel, reserved_room_type)

x=hotels_summary %>%
  unnest(reserved_room_type) %>%
  mutate(col=seq_along(hotel)) %>%
  group_by(hotel, country, arrival_date_month, arrival_date_year, reserved_room_type) %>%
  count() %>%
  pivot_wider(names_prefix = "reserved_room_type",
              names_from = reserved_room_type,
              values_from = n, values_fill = 0)

for(i in 1:nrow(res)){
  
}