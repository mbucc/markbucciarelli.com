package com.myorg;

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
	 * @return The AWS-provided "Managed-CachingOptimized" policy.
	 *
	 * @see <a href="https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cache-key-understand-cache-policy.html">Understand cache policies</a>
	 *
	 * @see <a href="https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html">Control the cache key with a policy</a>
	 */
	public static ICachePolicy s3CachePolicy() {
		return CachePolicy.CACHING_OPTIMIZED;
	}
}
