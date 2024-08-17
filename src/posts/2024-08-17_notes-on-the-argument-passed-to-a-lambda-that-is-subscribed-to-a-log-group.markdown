August 17, 2024

tags: aws lambda


Notes on the argument passed to a lambda that is subscribed to a log group subscription
==================================================

Input shape is `{ "awslogs" : { "data" : <String> }}`

The data string is a base64 encoded of a gzipped JSON data.
For example, if the data is in the file `t`, on the command line, you could
extract the JSON like this:

    base64 -d -i t | gunzip -c - | jq .

There is one such messages for each CloudWatch "log stream."


Some background on CloudWatch log terminology, courtesy of 
[Sandro Volpicella's post on CloudWatch](https://blog.awsfundamentals.com/aws-cloudwatch-logs-the-comprehensive-guide-for-log-analysis-and-insights#):

  - *event*: the actual log statement; e.g., what your app prints to stdout enriched by AWS with a timestamp to the closest millisecond

  - *stream*: One or more events from the same source; e.g., a lambda cold start.

  - *group*: One or more streams from the same service; e.g. a lambda function.

An example
-----------------------

The following Java lambda, subscribed to all events for a log group,

```
package echo;

import java.util.Map;

public final class Echo {

	public static Object handle(Object xs) {
		System.out.println("echo: " + xs.toString());
		return Map.of
			( "statusCode", 200
			, "headers", Map.of("Content-type", "text/plain")
			, "body", "OK"
			);
	}
}
```

logs the following event to it's log group:

```
echo: {awslogs={data=H4sIAAAAAAAA/62TW08bMRCF/8rKqsTLJpkZX8bjtyDSCqnQKkn7QhDybhy0VW7d3UAp4r9XuSAoAqlIfbTneOacT+N7tUhNE6/T+6dVFAn/XH/6mwwGvU/DVSuVrfLVKugGK03qMmwOJWr+er6U73arFVQvXjb9OZxUUxj7zq1i7tqvReM2jrFhQqKgEwPfA/5UXDx4XN/PBiNL723ZBkSoLcGUhG1YZmRj4BSsksqV82maMq6WrfVavmxmrepblS4UP35fHCTlm2jLnfTDodwca+qqQpKe2OMJnIM3hsvxAa1oNNI6DR7x5rZegcaUERrZiENTCpXbbVITRsXaxWQSQtosEBO54+oVFCn56fjq9G4Pxxnw81y+yL7nuqmWi1D9iPexEDYvSGYtC+qWX94HrJYL0O8bcIeW9g0nRSbtoMh1Ht5IHRiytIQFzbaacmFcGRxvkQdY1FKaZAlFTNHwGJLrcVH2QoK9qV1k6V6yF+i8AjstNYkjARIVoTAgREGcsaBA02ADkFYo7dvoWCC5ygOFNLPTWra02nIxMNsOk3S0X6WOsYjdwSg6ID4ON1SjgBPtA6r8LphtoRkvLNixBp2wCLOaBQHTNZuA7PzWjRqg/CGYQSQ54YPSxiyo9lqVcT6KM+O2HQdd5G7KLI9/0uGozx7goIaX40g4IzWlsQzedniR2GWHWhryLEF8uzYWUZn8a0IaP6KMDg/eS/x/+Durc/x0t1w8PXL+1di0p5s6tjulsKQ7zrJFs2kPa7m8zTNntf2hbO0WNV32aj6nUKG5LOz40l7Fn9lh8K3Ju0m7+5Pl1X7vIc1XQe7Nlsulw9/ANU3sU4HBQAA}}
```

Putting the value of `awslogs.data` into the file `t`,
running the following command extracts the JSON:

```
# base64 -d -i t | gunzip -c - | jq .
{
  "messageType": "DATA_MESSAGE",
  "owner": "715841324796",
  "logGroup": "/aws/lambda/getmyip",
  "logStream": "2024/08/17/getmyip[$LATEST]8852570e018540eba3479f28a019c76e",
  "subscriptionFilters": [
    "AllEvents"
  ],
  "logEvents": [
    {
      "id": "38444322670884892741391631216378673775860301993377923072",
      "timestamp": 1723903050263,
      "message": "INIT_START Runtime Version: java:21.v20\tRuntime Version ARN: arn:aws:lambda:us-east-1::runtime:21694cc427b5a5dc7b97a7968c13aabc9c4179ebf620795c3398a97968b78c56\n"
    },
    {
      "id": "38444322681076333297120125992060497026460603201610973185",
      "timestamp": 1723903050720,
      "message": "START RequestId: 980fdde9-38fe-4817-900b-098ad9230a00 Version: $LATEST\n"
    },
    {
      "id": "38444322687521248659495476079964319607255979676839313410",
      "timestamp": 1723903051009,
      "message": "getmyip: 'foobar', '74.67.17.199', '980fdde9-38fe-4817-900b-098ad9230a00', 1723903050131\n"
    },
    {
      "id": "38444322690643352987289763319779320165426750287676571651",
      "timestamp": 1723903051149,
      "message": "END RequestId: 980fdde9-38fe-4817-900b-098ad9230a00\n"
    },
    {
      "id": "38444322690643352987289763319779320165426750287676571652",
      "timestamp": 1723903051149,
      "message": "REPORT RequestId: 980fdde9-38fe-4817-900b-098ad9230a00\tDuration: 428.69 ms\tBilled Duration: 429 ms\tMemory Size: 128 MB\tMax Memory Used: 98 MB\tInit Duration: 454.60 ms\t\n"
    }
  ]
}
```




Cloud Development Kit (CDK) code to set this up
-------------------------------------------------

```


        //
        //                  Define the echo lambda.
        //

        LogGroup lg = LogGroup.Builder.create(this, "EchoLogGroup")
            .logGroupName("/aws/lambda/echo")
            .retention(RetentionDays.FIVE_DAYS)
            .removalPolicy(RemovalPolicy.DESTROY)
            .build();

        Function f = Function.Builder.create(this, "EchoFunction")
            .functionName("echo")
            .runtime(Runtime.JAVA_21)
            .handler("echo.Echo::handle")
            .logGroup(lg)
            .code(Code.fromAsset("../target/echo.jar"))
            .build();

        //
        //                  Define the getmyip lambda.  (We will subscribe to this.)
        //

        LogGroup lg1 = LogGroup.Builder.create(this, "GetMyIpLogGroup")
            .logGroupName("/aws/lambda/getmyip")
            .retention(RetentionDays.FIVE_DAYS)
            .removalPolicy(RemovalPolicy.DESTROY)
            .build();
        
        Function f1 = Function.Builder.create(this, "GetMyIpFunction")
            .functionName("getmyip")
            .runtime(Runtime.JAVA_21)
            .handler("getmyip.GetMyIp::handle")
            .logGroup(lg1)
            .code(Code.fromAsset("../target/getmyip.jar"))
            .build();



        //
        //                  Subscribe echo to all getmyip events.
        //

        SubscriptionFilter.Builder.create(this, "Subscription")
            .logGroup(lg1)
            .destination(new LambdaDestination(f))
            .filterPattern(FilterPattern.allEvents())
            .filterName("AllEvents")
            .build();

```
