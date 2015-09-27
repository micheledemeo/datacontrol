pid=data.table(system("tasklist /V",intern = T))[grepl("cmd.exe",V1),V1]
pid=regmatches(pid, regexpr("\\d+(?=\\s*Console)",pid,perl=T))
system(paste("taskkill /pid",pid))

