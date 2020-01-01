# Internal fields

$currency_symbol
$state
$process_flow
$approved
$approval {delegate, approve, reject, resubmit}
$editable
$review_process
$review

# Non-deletable fields

Last name
Contact owner
Modified by?
Created by?
Lead source
Record_Image?
Tag
Last_Activity_Time

## DONE - Google adwords information

GCLID
Click type
Ad network
Ad campaign name
Ad
Ad Group Name
Ad click date
Cost per conversion
Cost per click
Conversion export status
Conversion exported on
Reason for Conversion failure
Keyword
Device type
Search partner network
Ad group name

## DONE - Visit summary

First visit
First page visited
Referer
Most recent visit
Number of chats
Visitor score
Average time spent
Days visited


## DONE - Scoring rules
Score
Positive score
Negative score
Touch point score
Positive touch point score
Negative touch point score


curl "https://www.zohoapis.com/crm/v2/Leads?ids=410405000002264040,410405000002264025&wf_trigger=true"
-X DELETE
-H "Authorization: Zoho-oauthtoken 1000.8cb99dxxxxxxxxxxxxx9be93.9b8xxxxxxxxxxxxxxxf"


curl "https://www.zohoapis.com/crm/v2/Leads/search?criteria=((Last_Name:equals:Burns\,B)and(First_Name:starts_with:M))"
-X GET
-H "Authorization: Zoho-oauthtoken 1000.8cb99dxxxxxxxxxxxxx9be93.9b8xxxxxxxxxxxxxxxf"


curl "https://www.zohoapis.com/crm/v2/Contacts/search?email=newcrmapi@zoho.com" -X GET -H "Authorization: Zoho-oauthtoken 1000.a30f18de6cccffcbaf15284f56dae784.09c25a23e7a5eb23d9f8a3d5017665af"


./websocat_amd64-linux-static 'https://vts.zohopublic.com/watchws?x-e=app&x-s=testportal1439&cpage=https%3A%2F%2Fwww.vacationlabs.com%2F&ptitle=Vacation%20Labs%20-%20Tour%20Operator%20Website%20Builder%20%26%20Booking%20Engine&_zldp=c1OBLUxD75fIqAODSNd9qzGxki%252BH9PMpjLjgAtx9iXzyaqLu9XAO3vRG77lWrV2RodoRkLyJC2Y%253D&_zldt=e7eae4bb-ae15-4311-b09b-6df025a8bc83&localtime=GMT%2B0530%20(India%20Standard%20Time)&gmttime=GMT%2B0530&resolution=1280x800&lsid=405280000000046000&lang_embed=en&con_id=1577858234013&connection_count=1'

wget -O- --debug --header "Connection: Upgrade" --header "Upgrade: websocket" --header "Sec-WebSocket-Key: PRBs/DS9qEEkgM3Jcnu6Kg==" --header "Sec-WebSocket-Version: 13" 'https://vts.zohopublic.com/watchws?x-e=app&x-s=testportal1439&cpage=https%3A%2F%2Fwww.vacationlabs.com%2F&ptitle=Vacation%20Labs%20-%20Tour%20Operator%20Website%20Builder%20%26%20Booking%20Engine&_zldp=c1OBLUxD75fIqAODSNd9qzGxki%252BH9PMpjLjgAtx9iXzyaqLu9XAO3vRG77lWrV2RodoRkLyJC2Y%253D&_zldt=e7eae4bb-ae15-4311-b09b-6df025a8bc83&localtime=GMT%2B0530%20(India%20Standard%20Time)&gmttime=GMT%2B0530&resolution=1280x800&lsid=405280000000046000&lang_embed=en&con_id=1577858234013&connection_count=1'

while true; do wget -O- --debug --header "Connection: Upgrade" --header "Upgrade: websocket" --header "Sec-WebSocket-Key: PRBs/DS9qEEkgM3Jcnu6Kg==" --header "Sec-WebSocket-Version: 13" 'https://vts.zohopublic.com/watchws?x-e=app&x-s=testportal1439&cpage=https%3A%2F%2Fwww.vacationlabs.com%2F&ptitle=Vacation%20Labs%20-%20Tour%20Operator%20Website%20Builder%20%26%20Booking%20Engine&_zldp=c1OBLUxD75fIqAODSNd9qzGxki%252BH9PMpjLjgAtx9iXzyaqLu9XAO3vRG77lWrV2RodoRkLyJC2Y%253D&_zldt=e7eae4bb-ae15-4311-b09b-6df025a8bc83&localtime=GMT%2B0530%20(India%20Standard%20Time)&gmttime=GMT%2B0530&resolution=1280x800&lsid=405280000000046000&lang_embed=en&con_id=1577858234013&connection_count=1' &>/tmp/websocket.out &; CURL_PID=$!; sleep 10; kill $CURL_PID; done
