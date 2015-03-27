com -complete=custom,ListUsers -nargs=1 Finger !finger <args>
fun ListUsers(A,L,P)
  return system("cut -d: -f1 /etc/passwd")
endfun

