
## One-time setup of ~/.aws/config
aws configure sso


## Login
aws sso login --profile blog


## Diff
cdk diff --profile blog


## Deploy
cdk deploy --profile blog


## Read the docs
mvn javadoc:javadoc
open target/site/apidocs/index.html


