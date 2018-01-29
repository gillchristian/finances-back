if [ -z "$7" ] || [ -z "$8" ]; then
  QUERY="{\"detail\": \"$1\", \"amount\": $2, \"isPercentage\": $3, \"isDebit\": $4, \"fromMonth\": $5, \"fromYear\": $6}" ;
else
  QUERY="{\"detail\": \"$1\", \"amount\": $2, \"isPercentage\": $3, \"isDebit\": $4, \"fromMonth\": $5, \"fromYear\": $6, \"untilMonth\": $7, \"untilYear\": $8}" ;
fi;

accept='Accept: application/json' ;

content='Content-type: application/json' ;

url='http://localhost:8080/entries'

curl -X POST -d "$QUERY" -H "$accept" -H "$content" "$url" | jq .

curl "$url" | jq .
