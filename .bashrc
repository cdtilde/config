export PS1=$\  
#export JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Home
export PATH=/usr/local/bin:$PATH:.:/Applications/Xcode.app/Contents/Developer/usr/bin:/usr/local/mysql/bin
alias 'e=/usr/local/Cellar/emacs/HEAD/bin/emacsclient -n '
#alias mysql=/usr/local/mysql/bin/mysql
alias mysqladmin=/usr/local/mysql/bin/mysqladmin
alias mysql_start='sudo /Library/StartupItems/MySQLCOM/MySQLCOM start'
alias mysql_stop='sudo /Library/StartupItems/MySQLCOM/MySQLCOM stop'
export CLASSPATH=~/terajdbc4.jar:~/tdgssconfig.jar
export ALTERNATE_EDITOR=""
export EDITOR=emacsclient
alias e='emacsclient -a=\"\" $*'
