
if (!require('rsconnect')) install.packages('rsconnect'); library('rsconnect')

rsconnect::setAccountInfo(name='kren-data-analysis',
                          token='66FE413841F3137500506CBF6BE26CC8',
                          secret='mp+YVERneFeuVFwU/xKf/RzigL9c+UJ77sQUYN3x')

rsconnect::deployApp('C:/Users/kren/OneDrive - ZHAW/LABOR/ICP_APP/APP_DEV')
