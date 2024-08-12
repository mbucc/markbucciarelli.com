package com.markbucciarelli;

import software.amazon.awscdk.Stack;
import software.amazon.awscdk.StackProps;
import software.amazon.awscdk.services.certificatemanager.Certificate;
import software.amazon.awscdk.services.cloudfront.AllowedMethods;
import software.amazon.awscdk.services.cloudfront.BehaviorOptions;
import software.amazon.awscdk.services.cloudfront.Distribution;
import software.amazon.awscdk.services.cloudfront.ViewerProtocolPolicy;
import software.amazon.awscdk.services.s3.Bucket;
import software.amazon.awscdk.services.s3.ObjectOwnership;
import software.constructs.Construct;

import java.util.Collections;

import static com.markbucciarelli.CertificateRequestStack.DEVBLOG_CERT_ID;
import static com.markbucciarelli.CertificateRequestStack.DEVBLOG_DOMAIN_NAME;

public class BlogStack extends Stack {

    public static final String BLOG_DEV_CERT_ARN_ENVVAR = "BLOG_DEV_CERT_ARN";

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

        var distBuilder = Distribution.Builder.create(this, "BlogCDN")
            .defaultBehavior(defaultCdnBehavior)
            .logBucket(loggingBucket)
            .defaultRootObject("index.html");

        if (CloudDevelopmentKit.CertUtil.isInEnv(BLOG_DEV_CERT_ARN_ENVVAR)) {
            System.out.println("adding dev blog cert from envvar " + BLOG_DEV_CERT_ARN_ENVVAR);
            var cert = CloudDevelopmentKit.CertUtil.getDevBlogCert(this, BLOG_DEV_CERT_ARN_ENVVAR);
            distBuilder.certificate(cert);
            distBuilder.domainNames(Collections.singletonList(DEVBLOG_DOMAIN_NAME));
        }

        distBuilder.build();

    }

}
