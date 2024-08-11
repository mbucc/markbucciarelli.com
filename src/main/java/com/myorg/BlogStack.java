package com.myorg;

import software.amazon.awscdk.Stack;
import software.amazon.awscdk.StackProps;
import software.amazon.awscdk.services.cloudfront.Behavior;
import software.amazon.awscdk.services.cloudfront.CachePolicy;
import software.amazon.awscdk.services.cloudfront.CloudFrontAllowedMethods;
import software.amazon.awscdk.services.cloudfront.CloudFrontWebDistribution;
import software.amazon.awscdk.services.cloudfront.CloudFrontWebDistributionProps;
import software.amazon.awscdk.services.cloudfront.ICachePolicy;
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


        getCachePolicy();

        //
        //                  Define the behavior for the ... WHAT?
        //
        //                  A behavior is associated with a path pattern (default = '*')
        //                  and an origin or origin group.
        //
        //

        //var defaultCdnBehavior = BehaviorOptions.builder().build();


        var defaultCdnBehavior = Behavior.builder()
            .isDefaultBehavior(true)
            .allowedMethods(CloudFrontAllowedMethods.GET_HEAD)
            //.cachedMethods(CloudFrontAllowedCachedMethods.GET_HEAD)
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

    /**
     * A cache policy defines the key for the cache and other attributes.
     *
     * <p>
     *     Specifically, a cache policy defines
     * </p>
     *     <ol>
     *         <li>name,</li>
     *         <li>description,</li>
     *         <li>the cache entry time-to-live,</li>
     *         <li>any additional values to add to the default cache key, and</li>
     *         <li>if compressed objects are cached.</li>
     *     </ol>
     *
     * <p>
     *     The default cache key is (domain_name, URL path); for example,
     *     ("d111111abcdef8.cloudfront.net", "/index.html")
     * </p>
     *
     * <p>
     *     The AWS-provided "Managed-CachingOptimized"
     *     returned by this method
     *     defines the following policy:
     * </p>
     *     <ol>
     *     <li>TTL
     *     <ul>
     *         <li>min=1 second,</li>
     *         <li>max=31536000 seconds (1 year), and</li>
     *         <li>default=86400 seconds (1 day)</li>
     *     </ul></li>
     *     <li>key: the default (none, none, none), which means that no
     *         <ul>
     *             <li>header values,</li>
     *             <li>cookie values, or</li>
     *             <li>query parmameter values</li>
     *         </ul>
     *         are added to the cache key.</li>
     *     <li>cache both gzip and brotli compressed objects</li>
     *     </ol>
     *
     * @return the AWS-provided "Managed-CachingOptimized" policy.
     *
     * @see <a href="https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cache-key-understand-cache-policy.html">Understand cache policies</a>
     * @see <a href="https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html">Control the cache key with a policy</a>
     *
     */
    public static ICachePolicy getCachePolicy() {
        return CachePolicy.CACHING_OPTIMIZED;
    }
}
