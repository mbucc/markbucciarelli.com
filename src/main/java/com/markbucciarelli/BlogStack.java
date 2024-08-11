package com.markbucciarelli;

import software.amazon.awscdk.Stack;
import software.amazon.awscdk.StackProps;
import software.amazon.awscdk.services.cloudfront.AllowedMethods;
import software.amazon.awscdk.services.cloudfront.BehaviorOptions;
import software.amazon.awscdk.services.cloudfront.Distribution;
import software.amazon.awscdk.services.cloudfront.ViewerProtocolPolicy;
import software.amazon.awscdk.services.s3.Bucket;
import software.amazon.awscdk.services.s3.BucketEncryption;
import software.constructs.Construct;

public class BlogStack extends Stack {

    public BlogStack(final Construct scope, final String id) {
        this(scope, id, null);
    }

    public BlogStack(final Construct scope, final String id, final StackProps props) {

        super(scope, id, props);

        //
        //                  Create an S3 bucket.  Private by default.
        //
        Bucket bucket = Bucket.Builder.create(this, "BlogContent")
            .bucketName("blog-content-for-marks-blog")
            .encryption(BucketEncryption.S3_MANAGED)
            .versioned(true)
            .build();

        //
        //                  Define the behavior for the origin.
        //

        var defaultCdnBehavior = BehaviorOptions.builder()
            .allowedMethods(AllowedMethods.ALLOW_GET_HEAD)
            .origin(CloudDevelopmentKit.s3OriginSecuredWithOriginAccessIdentity(this, bucket))
            .cachePolicy(CloudDevelopmentKit.s3CachePolicy())
            .viewerProtocolPolicy(ViewerProtocolPolicy.REDIRECT_TO_HTTPS)
            .compress(true)
            .build();

        //
        //                  Create the CloudFront distribution.
        //

        Distribution.Builder.create(this, "BlogCDN")
            .defaultBehavior(defaultCdnBehavior)
            .build();
    }

}
