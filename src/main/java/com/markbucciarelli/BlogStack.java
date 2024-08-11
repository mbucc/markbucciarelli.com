package com.markbucciarelli;

import software.amazon.awscdk.Stack;
import software.amazon.awscdk.StackProps;
import software.amazon.awscdk.services.cloudfront.AllowedMethods;
import software.amazon.awscdk.services.cloudfront.BehaviorOptions;
import software.amazon.awscdk.services.cloudfront.Distribution;
import software.amazon.awscdk.services.cloudfront.OriginAccessIdentity;
import software.amazon.awscdk.services.cloudfront.ViewerProtocolPolicy;
import software.amazon.awscdk.services.cloudfront.origins.S3Origin;
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
            //.versioned(true)
            .build();

        //
        //                  Create an identity so CloudFront can access to the S3 bucket.
        //
        var cdnIdentity = new OriginAccessIdentity(this, "cdnIdentity");

        //
        //                  Grant the CDN identity read permissions to the bucket.
        //
        bucket.grantRead(cdnIdentity);

        //
        //                  Define the origin of the content.
        //
        //                  Currently (Aug, 2024), CDK does not support origin
        //                  access control, the preferred security method.
        //
        //                  This open source RFC is tracking Amazon's progress:
        //                  https://github.com/aws/aws-cdk-rfcs/issues/617.
        //
        //                  Although the web console marks origin access identity
        //                  as "Legacy", the three features it does not support are:
        //
        //                    * All Amazon S3 buckets in all AWS Regions, including
        //                      opt-in Regions launched after December 2022
        //
        //                    * Amazon S3 server-side encryption with AWS KMS (SSE-KMS)
        //
        //                    * Dynamic requests (PUT and DELETE) to Amazon S3
        //
        //                  We do not need any of these features.
        //

        var s3Origin = S3Origin.Builder.create(bucket)
            .originAccessIdentity(cdnIdentity)
            .build();

        //
        //                  Define the behavior for the origin.
        //

        var defaultCdnBehavior = BehaviorOptions.builder()
            .allowedMethods(AllowedMethods.ALLOW_GET_HEAD)
            .origin(s3Origin)
            .cachePolicy(CloudDevelopmentKit.s3CachePolicy())
            .viewerProtocolPolicy(ViewerProtocolPolicy.REDIRECT_TO_HTTPS)
            .compress(true)
            .build();

        Distribution.Builder.create(this, "BlogCDN")
            .defaultBehavior(defaultCdnBehavior)
            .build();
    }

}
