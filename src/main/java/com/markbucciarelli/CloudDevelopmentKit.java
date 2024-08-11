package com.markbucciarelli;

import software.amazon.awscdk.services.cloudfront.CachePolicy;
import software.amazon.awscdk.services.cloudfront.ICachePolicy;


/**
 * Utility methods with detailed CDK documentation.
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
	 *      //
	 *      // ... then, down in your Stack definition ...
	 *      //
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
	 *             <li>query parmameter values</li>
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

}
