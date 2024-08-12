package com.markbucciarelli;

import software.amazon.awscdk.Stack;
import software.amazon.awscdk.StackProps;
import software.amazon.awscdk.services.cloudfront.AllowedMethods;
import software.amazon.awscdk.services.cloudfront.BehaviorOptions;
import software.amazon.awscdk.services.cloudfront.Distribution;
import software.amazon.awscdk.services.cloudfront.ViewerProtocolPolicy;
import software.amazon.awscdk.services.s3.Bucket;
import software.amazon.awscdk.services.s3.ObjectOwnership;
import software.constructs.Construct;

public class BlogStack extends Stack {

    public BlogStack(final Construct scope, final String id) {
        this(scope, id, null);
    }

    public BlogStack(final Construct scope, final String id, final StackProps props) {

        super(scope, id, props);

        //
        //                  Create an S3 bucket for content.  Private by default.
        //

        Bucket contentBucket = Bucket.Builder.create(this, "BlogContent")
            .bucketName("blog-content-mb")
            .versioned(true)
            .build();

        //
        //                  Create an S3 bucket for logs.
        //

        Bucket loggingBucket = Bucket.Builder.create(this, "BlogLogs")
            .bucketName("blog-logs-mb")
            .objectOwnership(ObjectOwnership.BUCKET_OWNER_PREFERRED)
            .build();



        //
        //                  Define the behavior for the origin.
        //
        //                  Set the origin, cache policy and specify to redirect
        //                  all traffic from HTTP to HTTPS.
        //

        var defaultCdnBehavior = BehaviorOptions.builder()
            .allowedMethods(AllowedMethods.ALLOW_GET_HEAD)
            .origin(CloudDevelopmentKit.s3OriginSecuredWithOriginAccessIdentity(this, contentBucket))
            .cachePolicy(CloudDevelopmentKit.s3CachePolicy())
            .viewerProtocolPolicy(ViewerProtocolPolicy.REDIRECT_TO_HTTPS)
            .compress(true)
            .build();


        //
        //                  Create the CloudFront distribution.
        //

        Distribution.Builder.create(this, "BlogCDN")
            .defaultBehavior(defaultCdnBehavior)
            .logBucket(loggingBucket)
            .defaultRootObject("index.html")
            .build();
    }

}
