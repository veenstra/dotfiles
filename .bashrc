source ~/aliases
source ~/.aws/dev_credentials

set -o ignoreeof

# We need these variables defined here in order for "ssh localhost" to see them.
export HADOOP_HOME=$HOME/repos/vendor/hadoop_distros/current
export PATH=$PATH:$HADOOP_HOME/bin:$HOME/repos/hadoop/tools
export JAVA_HOME=`/usr/libexec/java_home`

# This is for production
#export HADOOP_CONF_DIR=$HADOOP_HOME/conf-analytics-mr.sv2
