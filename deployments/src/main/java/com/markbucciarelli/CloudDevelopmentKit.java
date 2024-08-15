package com.markbucciarelli;


import software.amazon.awscdk.services.cloudfront.CachePolicy;
import software.amazon.awscdk.services.cloudfront.ICachePolicy;
import software.amazon.awscdk.services.cloudfront.IOrigin;
import software.amazon.awscdk.services.cloudfront.OriginAccessIdentity;
import software.amazon.awscdk.services.cloudfront.origins.S3Origin;
import software.amazon.awscdk.services.s3.Bucket;
import software.constructs.Construct;


/**
 * CDK helper methods.
 */
public class CloudDevelopmentKit {

	/**
	 * The suggested CloudFront
	 * cache policy to use with an S3 bucket.
	 *
	 * <p>
	 *     Sample code (based on sample code in {@link software.amazon.awscdk.services.cloudfront.CachePolicy}):
	 * </p>
	 *
	 * <blockquote><pre>
	 *  import static com.markbucciarelli.CloudDevelopmentKit.s3CachePolicy;
	 *
	 *      * 
	 *      // ... then, down in your Stack definition ...
	 *      * 
	 *
	 *      S3Origin bucketOrigin;
	 *      Distribution.Builder.create
	 *         ( this
	 *         , "myDistManagedPolicy"
	 *         )
	 *         .defaultBehavior
	 *             (BehaviorOptions
	 *             .builder()
	 *             .origin(bucketOrigin)
	 *             .cachePolicy(s3CachePolicy())
	 *             .build()
	 *             )
	 *         .build();
	 * </pre></blockquote>
	 *
	 *  <p>
	 *      To define a CloudFront caching policy in CDK, you must use a
	 *      {@link software.amazon.awscdk.services.cloudfront.Distribution}.
	 *  </p>
	 *  <p>
	 *      The older
	 *      {@link software.amazon.awscdk.services.cloudfront.CloudFrontWebDistribution}
	 *      does not support caching policies.
	 *  </p>
	 *
	 * <p>
	 * A cache policy defines a
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
	 *     The policy returned by this method has the following configuration:
	 * </p>
	 *
	 * <ol>
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
	 *             <li>query parameter values</li>
	 *         </ul>
	 *         are added to the cache key.</li>
	 *     <li>cache both gzip and brotli compressed objects</li>
	 *  </ol>
	 *
	 *  <p>
	 *      While no headers are explicitly included, turning on the support
	 *      for compressed objects implicitly adds the normalized Accept-Encoding
	 *      header (source:
	 *      <a href="https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cache-key-understand-cache-policy.html#cache-policy-compressed-objects">
	 *          CloudFront Compression support</a>.)
	 *  </p>
	 *
	 *  <p>
	 *      The interface returned by this method has single method:
	 *      {@link software.amazon.awscdk.services.cloudfront.ICachePolicy#getCachePolicyId() }
	 *      and as of this writing {@code CACHING_OPTIMIZED.getCachePolicyId()}
	 *      returns 658327ea-f89d-4fab-a63d-7e88639e58f6.
	 *  </p>
	 *
	 * @return The AWS-provided "Managed-CachingOptimized" policy.
	 *
	 * @see <a href="https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cache-key-understand-cache-policy.html">Understand cache policies</a>
	 *
	 * @see <a href="https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html">Control the cache key with a policy</a>
	 */
	public static ICachePolicy s3CachePolicy() {
		// CACHING_OPTIMIZED.getCachePolicyId() = 658327ea-f89d-4fab-a63d-7e88639e58f6
		return CachePolicy.CACHING_OPTIMIZED;
	}

	/**
	 *  Secure a bucket so only CloudFront can access it.
	 *
	 *  <p>
	 *      (Of course, you can also access the bucket from the
	 *      AWS account role that created it, and thus also from
	 *      the aws command line.)
	 *  </p>
	 *
	 * <p>
	 *     An origin access identity (OAI) is a special kind of AWS user
	 *     that is specific to CloudFront.  The only place you see this
	 *     user in the AWS web console is on the CloudFront resource, in the
	 *     "Origin access" section, on the "Identities (legacy)" tab.
	 * </p>
	 *
	 * <p>
	 *     An OAI is the legacy authentication mechanism with the
	 *     newer origin access control (OAC) preferred.  However,
	 * 	   as of this writing (August, 2024) the cloud development kit
	 * 	   does not support using OAC.
	 * </p>
	 *
	 * <p>
	 *     This is not such a big deal.  The additional features provided
	 *     by OAC are:
	 * </p>
	 *
	 * <ul>
	 *     <li>support for all Amazon S3 buckets in all AWS Regions, including
	 * 	    opt-in Regions launched after December 2022</li>
	 *
	 * 	   <li>Amazon S3 server-side encryption with AWS KMS (SSE-KMS)</li>
	 *
	 * 	   <li>Dynamic requests (PUT and DELETE) to Amazon S3</li>
	 * </ul>
	 *
	 * <p>
	 *     Note that all data on S3 is by default encrypted on the server side.
	 *     The default encryption mechanism is SSE-S3, which is designed to
	 *     protect against access to your data if someone manages to access a
	 *     physical hard drive used to store your data.  If a SSE-S3 bucket
	 *     is public, anyone could access the data even though it is encrypted.
	 *     In other words, you must manage access in another way; for example,
	 *     using OAI.
	 * </p>
	 *
	 * <p>
	 *     On the other hand, if a bucket is encrypted with SSE-KMS, any
	 *     uploaded objects (after that encryption was added to the bucket)
	 *     can only be accessed by a resource or an identity that has access
	 *     to the KMS key.  Note that any object added to a bucket
	 *     <em>before</em> the SSE-KMS encryption was added will <em>still
	 *     be accessible by anyone.</em>
	 * </p>
	 *
	 * <p>
	 *     <a href="https://github.com/aws/aws-cdk-rfcs/issues/617">The cloud
	 *     development kit RFC 617</a>
	 *     was created to add support for OAC to CDK.
	 * </p>
	 *
	 * <p>
	 *     The bucket policy that is automatically created by using an OAI is:
	 * </p>
	 *
	 * <pre>
	 * {
	 *     "Version": "2012-10-17",
	 *     "Statement": [
	 *         {
	 *             "Effect": "Allow",
	 *             "Principal": {
	 *                 "AWS": "arn:aws:iam::cloudfront:user/CloudFront Origin Access Identity E3UCGS5OHCAWT3"
	 *             },
	 *             "Action": [
	 *                 "s3:GetBucket*",
	 *                 "s3:GetObject*",
	 *                 "s3:List*"
	 *             ],
	 *             "Resource": [
	 *                 "arn:aws:s3:::blog-content-mb",
	 *                 "arn:aws:s3:::blog-content-mb/*"
	 *             ]
	 *         },
	 *         {
	 *             "Effect": "Allow",
	 *             "Principal": {
	 *                 "AWS": "arn:aws:iam::cloudfront:user/CloudFront Origin Access Identity E3UCGS5OHCAWT3"
	 *             },
	 *             "Action": "s3:GetObject",
	 *             "Resource": "arn:aws:s3:::blog-content-mb/*"
	 *         }
	 *     ]
	 * }
	 * </pre>
	 *
	 * @see
	 *   <a href="https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-restricting-access-to-s3.html">
	 *       Restrict access to an Amazon Simple Storage Service origin</a>
	 *
	 * @param scope The scope for the identity.
	 * @param bucket The S3 bucket to be accessed from CloudFront.
	 * @return A S3 bucket origin secured by an origin access identity.
	 */
	public static IOrigin s3OriginSecuredWithOriginAccessIdentity(Construct scope, Bucket bucket) {
		var cdnIdentity = new OriginAccessIdentity(scope, "cdnIdentity");
		bucket.grantRead(cdnIdentity);
		return S3Origin.Builder.create(bucket)
			.originAccessIdentity(cdnIdentity)
			.build();
	}

}
