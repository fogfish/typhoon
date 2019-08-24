APP = typhoon 
# ORG = fogfish
# URI = 

ENV     ?= dev
ROLE    ?= arn:aws:iam::189549315145:role/test2-role-lambda
TIMEOUT ?= 30
MEMORY  ?= 256

# include erlang.mk
include serverless.mk
