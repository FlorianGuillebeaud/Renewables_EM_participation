# Returns the price each hour regarding the bidding price

get_schedule = function(bid_prices, elspot_prices){
  return_schedule = vector()
  for (i in 1:length(bid_prices)){
    # since we only bid at 0 (or higher !)
    if (elspot_prices[i] < 0 ) return_schedule[i] = NA
    else return_schedule[i] = elspot_prices[i] # if elspot price is >= 0 : we are schedule at this price
  }
  return(return_schedule)
}
