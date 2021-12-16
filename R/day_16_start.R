
x <- BMS::hex2bin("C20D42002ED333E7774EAAC3C2009670015692C61B65892239803536C53E2D307A600ACF324928380133D18361005B336D3600B4BF96E4A59FED94C029981C96409C4A964F995D2015DE6BD1C6E7256B004C0010A86B06A1F0002151AE0CC79866600ACC5CABC0151006238C46858200E4178F90F663FBA4FDEC0610096F8019B8803F19A1641C100722E4368C3351D0E9802D60084DC752739B8EA4ED377DE454C0119BBAFE80213F68CDC66A349B0B0053B23DDD61FF22CB874AD1C4C0139CA29580230A216C9FF54AD25A193002A2FA002AB3A63377C124205008A05CB4B66B24F33E06E014CF9CCDC3A2F22B72548E842721A573005E6E5F76D0042676BB33B5F8C46008F8023301B3F59E1464FB88DCBE6680F34C8C0115CDAA48F5EE45E278380019F9EC6395F6BE404016849E39DE2EF002013C873C8A401544EB2E002FF3D51B9CAF03C0010793E0344D00104E7611C284F5B2A10626776F785E6BD672200D3A801A798964E6671A3E9AF42A38400EF4C88CC32C24933B1006E7AC2F3E8728C8E008C759B45400B4A0B4A6CD23C4AF09646786B70028C00C002E6D00AEC1003440080024658086A401EE98070B50029400C0014FD00489000F7D400E000A60001E870038800AB9AB871005B12B37DB004266FC28988E52080462973DD0050401A8351DA0B00021D1B220C1E0013A0C0198410BE1C180370C21CC552004222FC1983A0018FCE2ACBDF109F76393751D965E3004E763DB4E169E436C0151007A10C20884000874698630708050C00043E24C188CC0008744A8311E4401D8B109A3290060BE00ACEA449214CD7B084B04F1A48025F8BD800AB4D64426B22CA00FC9BE4EA2C9EA6DC40181E802B39E009CB5B87539DD864A537DA7858C011B005E633E9F6EA133FA78EE53B7DE80")

bin2dec <- function(x) {
  l <- length(x) - 1
  sum(x * 2 ^(l:0))
}

zero_mod <- function(m, n) {
  a <- m %% n
  ifelse(a == 0, n, a)
}



deparse_literal <- function(x) {
  removed <- 0
  length_x <- length(x)
  packet_version <- bin2dec(x[1:3])
  packet_type <- bin2dec(x[4:6])
  x <- x[-(1:6)]
  removed <- removed + 6
  if(packet_type == 4) {
    val <- integer(0)
    while(x[1] == 1) {
      val <- c(val, x[2:5])
      x <- x[-(1:5)]
      removed <- removed + 5
    }
    val <- c(val, x[2:5])
    x <- x[-(1:5)]
    removed <- removed + 5
    x <- x[-(1:(4 - zero_mod(removed, 4)))] #remove the garbage characters? it says to ignore, but not sure if that is ignore for now or throw away forever
    return(list(val = bin2dec(val),
                x = x))
  } else {
    stop("Error: packet type must be 4")
  }
}

deparse_operator <- function(x) {
  removed <- 0
  length_x <- length(x)
  packet_version <- bin2dec(x[1:3])
  packet_type <- bin2dec(x[4:6])
  x <- x[-(1:6)]
  removed <- removed + 6
  if(pocket_type != 4) {
    
    length_type <- x[1]
    x <- x[-1]
    removed <- removed + 1
    if(length_type == 0) {
      total_length_of_subpackets <- bin2dec(x[1:15])
      removed <- removed + 15
      x <- x[-(1:15)]
      num_bits <- zero_mod(total_length_of_subpackets, 16)
      val <- integer(0)
      
    }
    
  } else {
    stop("Error: pocket type must not equal 4")
  }
}

df <- data.frame(packet_version = numeric(0),
                 packet_type = numeric(0),
                 length_type = numeric(0),
                 values = numeric(0),
                 rl = numeric(0))

recursive_level <- 0

ddparse <- function(x, recursive_level, debug = FALSE) {
  if(debug) {
     browser()
  }
  recursive_level <- recursive_level + 1
  #browser()
  removed <- 0
  length_x <- length(x)
  packet_version <- bin2dec(x[1:3])
  packet_type <- bin2dec(x[4:6])
  x <- x[-(1:6)]
  removed <- removed + 6
  if(packet_type == 4) {
    val <- integer(0)
    while(x[1] == 1) {
      val <- c(val, x[2:5])
      x <- x[-(1:5)]
      removed <- removed + 5
    }
    val <- c(val, x[2:5])
    x <- x[-(1:5)]
    removed <- removed + 5
    #x <- x[-(1:(4 - zero_mod(removed, 4)))] #remove the garbage characters? it says to ignore, but not sure if that is ignore for now or throw away forever
    rval <- list(packet_version = packet_version,
         packet_type = packet_type,
         length_type = NA,
         values = bin2dec(val),
         rl = recursive_level)
    df <<- bind_rows(df, rval)
    return(x)
  }
  if(packet_type != 4) {
    length_type <- x[1]
    x <- x[-1]
    removed <- removed + 1
    if(length_type == 0) {
      total_length_of_subpackets <- bin2dec(x[1:15])
      removed <- removed + 15
      x <- x[-(1:15)]
      num_bits <- zero_mod(total_length_of_subpackets, 16)
      df1 <- df
      short_x <- x[1:total_length_of_subpackets]
      x <- x[-(1:total_length_of_subpackets)]
      rf <- list(
        packet_version = packet_version,
        packet_type = packet_type,
        length_type = length_type,
        values = NA,
        rl = recursive_level
      )
      df <<- bind_rows(df, rf)
      repeat {
        short_x <- ddparse(short_x, recursive_level = recursive_level, debug)
        if(length(short_x) == 0) {
          return(x)
        }
      }
      #return(ddparse(x[1:num_bits]))
    }
    if(length_type == 1) {
      number_subpackets <- bin2dec(x[1:11])
      removed <- removed + 11
      x <- x[-(1:11)]
      if(number_subpackets == 0) {
        stop("zero subpackets not implemented")
      }
      rf <- list(
        packet_version = packet_version,
        packet_type = packet_type,
        length_type = length_type,
        values = NA,
        rl = recursive_level
      )
      df <<- bind_rows(df, rf)
      for(i in 1:number_subpackets) {
        x <- ddparse(x, recursive_level, debug)
      }
      return(x)
    }
  }
}
df <- data.frame(packet_version = numeric(0),
                 packet_type = numeric(0),
                 length_type = numeric(0),
                 values = numeric(0),
                 rl = numeric(0))
x <- BMS::hex2bin("9C0141080250320F1802104A08")
x <- BMS::hex2bin("D2FE28")
x <- BMS::hex2bin("38006F45291200")
x <- BMS::hex2bin("EE00D40C823060")
x <- BMS::hex2bin("8A004A801A8002F478")
x <- BMS::hex2bin("620080001611562C8802118E34")
x <- BMS::hex2bin("C0015000016115A2E0802F182340")
x <- BMS::hex2bin("9C0141080250320F1802104A08")
x <- BMS::hex2bin("C20D42002ED333E7774EAAC3C2009670015692C61B65892239803536C53E2D307A600ACF324928380133D18361005B336D3600B4BF96E4A59FED94C029981C96409C4A964F995D2015DE6BD1C6E7256B004C0010A86B06A1F0002151AE0CC79866600ACC5CABC0151006238C46858200E4178F90F663FBA4FDEC0610096F8019B8803F19A1641C100722E4368C3351D0E9802D60084DC752739B8EA4ED377DE454C0119BBAFE80213F68CDC66A349B0B0053B23DDD61FF22CB874AD1C4C0139CA29580230A216C9FF54AD25A193002A2FA002AB3A63377C124205008A05CB4B66B24F33E06E014CF9CCDC3A2F22B72548E842721A573005E6E5F76D0042676BB33B5F8C46008F8023301B3F59E1464FB88DCBE6680F34C8C0115CDAA48F5EE45E278380019F9EC6395F6BE404016849E39DE2EF002013C873C8A401544EB2E002FF3D51B9CAF03C0010793E0344D00104E7611C284F5B2A10626776F785E6BD672200D3A801A798964E6671A3E9AF42A38400EF4C88CC32C24933B1006E7AC2F3E8728C8E008C759B45400B4A0B4A6CD23C4AF09646786B70028C00C002E6D00AEC1003440080024658086A401EE98070B50029400C0014FD00489000F7D400E000A60001E870038800AB9AB871005B12B37DB004266FC28988E52080462973DD0050401A8351DA0B00021D1B220C1E0013A0C0198410BE1C180370C21CC552004222FC1983A0018FCE2ACBDF109F76393751D965E3004E763DB4E169E436C0151007A10C20884000874698630708050C00043E24C188CC0008744A8311E4401D8B109A3290060BE00ACEA449214CD7B084B04F1A48025F8BD800AB4D64426B22CA00FC9BE4EA2C9EA6DC40181E802B39E009CB5B87539DD864A537DA7858C011B005E633E9F6EA133FA78EE53B7DE80")

ddparse(as.integer(x), 0, debug = FALSE)
df
#get list of indices to apply
repeat {
  max_recursion <- max(df$rl)
  eq <- which(df$rl == max_recursion - 1)
  j <- 1
  i <- 1
  ind <- as.list(eq)
  for(i in 1:length(eq)) {
    while(df$rl[eq[i] + j] > max_recursion - 1 && eq[i] + j <= nrow(df)) {
      ind[[i]] <- c(ind[[i]], df$values[ind[[i]][1] + j])
      j <- j + 1
      df$rl[eq[i] + j]
      ind
    }
    j <- 1
  }
  for(i in 1:length(ind)) {
    ind[[i]][1] <- df$packet_type[eq[i]]
  }
  ind
  df
  values <- sapply(ind, function(x) {
    case_when(
      x[1] == 0 ~ sum(x[-1]),
      x[1] == 1 ~ prod(x[-1]),
      x[1] == 2 ~ min(x[-1]),
      x[1] == 3 ~ max(x[-1]),
      x[1] == 5 ~ as.numeric(x[2] > x[3]),
      x[1] == 6 ~ as.numeric(x[2] < x[3]),
      x[1] == 7 ~ as.numeric(x[2] == x[3])
    )
  })
  values
  df
  ind
  df$values[eq] <- values
  df <- filter(df, rl < max_recursion)
  if(nrow(df) == 1) {
    break
  }
}
df
