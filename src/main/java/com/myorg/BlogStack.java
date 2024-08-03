package com.myorg;

import software.amazon.awscdk.Stack;
import software.amazon.awscdk.StackProps;
import software.amazon.awscdk.services.cloudfront.Behavior;
import software.amazon.awscdk.services.cloudfront.CloudFrontAllowedCachedMethods;
import software.amazon.awscdk.services.cloudfront.CloudFrontAllowedMethods;
import software.amazon.awscdk.services.cloudfront.CloudFrontWebDistribution;
import software.amazon.awscdk.services.cloudfront.CloudFrontWebDistributionProps;
import software.amazon.awscdk.services.cloudfront.OriginAccessIdentity;
import software.amazon.awscdk.services.cloudfront.S3OriginConfig;
import software.amazon.awscdk.services.cloudfront.SourceConfiguration;
import software.amazon.awscdk.services.cloudfront.ViewerProtocolPolicy;
import software.amazon.awscdk.services.s3.Bucket;
import software.amazon.awscdk.services.s3.BucketEncryption;
import software.constructs.Construct;

import java.util.List;

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
            .bucketName("blog_content")
            .encryption(BucketEncryption.S3_MANAGED)
            //.versioned(true)
            .build();

        //
        //                  Create an identity to give CloudFront bucket access.
        //
        var cdnIdentity = new OriginAccessIdentity(this, "cdnBucketAccess");

        //
        //                  Grant CDN identity read permissions to the bucket.
        //
        bucket.grantRead(cdnIdentity);

        //
        //                  Define the default behavior for the CDN.
        //
        var defaultCdnBehavior = Behavior.builder()
            .isDefaultBehavior(true)
            .allowedMethods(CloudFrontAllowedMethods.GET_HEAD)
            .cachedMethods(CloudFrontAllowedCachedMethods.GET_HEAD)
            .compress(true)
            .viewerProtocolPolicy(ViewerProtocolPolicy.REDIRECT_TO_HTTPS)
            .build();

        //
        //                  Define the CloudFront origin(s) and behavior(s).
        //

        var cdnSourceConfig = SourceConfiguration.builder()
            .s3OriginSource(S3OriginConfig.builder()
                    .s3BucketSource(bucket)
                    .originAccessIdentity(cdnIdentity)
                    .build())
            .behaviors(List.of(defaultCdnBehavior))
            .build();

        var dist1 = new CloudFrontWebDistribution(
            this,
            "BlogCDN",
            CloudFrontWebDistributionProps.builder()
                .originConfigs(List.of(cdnSourceConfig))
                //.geoRestriction(GeoRestriction.allowlist("US"))  // ISO 3166-1-alpha-2 codes
                .build());
    }
}
