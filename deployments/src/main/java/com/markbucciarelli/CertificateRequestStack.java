package com.markbucciarelli;

import software.amazon.awscdk.Stack;
import software.amazon.awscdk.StackProps;
import software.amazon.awscdk.services.certificatemanager.Certificate;
import software.constructs.Construct;

/**
 * Request SSL certificate for my blog.
 *
 * <p>
 *     Notes:
 * </p>
 *
 * <ul>
 *     <li>Once you create a certificate with this stack don't change
 *     the certificate data used, or cloud formation will try to
 *     delete the certificate you already created.  While the delete
 *     did not succeed in my case, I would not rely on that behavior.</li>
 *
 *     <li>This certificate stack hangs when creating the certificate.
 *       But the request is created and the verification emails are sent,
 *       so just press Ctrl-C to terminate it after a minute.</li>
 *
 *     <li>There is a bunch of related discussion about hanging in the
 *       CDK bug report
 *       <a href="https://github.com/aws/aws-cdk/issues/2914">(certificatemanager):
 *       DnsValidatedCertificate timeout while waiting for certificate approval</a>.</li>
 *
 *     <li>Once the certificate is created, you can run this stack without
 *     any problem.  CDK will report that no updates are needed.</li>
 * </ul>
 *
 * <p>
 *     The full recipe is listed below.  Fortunately, this is
 *     a one-time setup and Amazon auto-renews the certificate
 *     if possible.
 * </p>
 *
 * <ol>
 *     <li><b>RUN</b> this stack.</li>
 *     <li><b>PRESS</b> Ctrl-C when this stack hangs, which
 *     happens when the certificate is first created.</li>
 *     <li><b>OPEN</b> email from AWS.</li>
 *     <li><b>CLICK</b> the approve button in the email.</li>
 *     <li><b>OPEN</b> AWS web console in browser.</li>
 *     <li><b>COPY</b> the Amazon resource number (ARN) for the new certificate.</li>
 *     <li><b>EXPORT</b> this value into your environment as {@code BLOG_CERT_ARN}.</li>
 *     <li><b>RUN</b> the BlogStack</li>
 * </ol>
 *
 * <p>
 *     The validation emails (the default verification method) were sent to:
 * </p>
 *
 * <ul>
 *     <li>webmaster@markbucciarelli.com</li>
 *     <li>hostmaster@</li>
 *     <li>administrator@</li>
 *     <li>admin@</li>
 *     <li>postmaster@</li>
 * </ul>
 */
public class CertificateRequestStack extends Stack {

    /**
     * Data needed to define an SSL certificate.
     *
     * @param id  Used in the Name tag created by CDK.
     * @param domain The domain to secure.
     * @param arn Pasted in once the certificate is created.
     *            If not null, the BlogStack adds this certificate
     *            to the cloud front distribution.
     */
    public record CertificateData(String id, String domain, String arn) {}

    /**
     * Attributes of the production blog SSL certificate.
     */
    public final static CertificateData BLOG_CERT =
        new CertificateData
             ("blog-cert"
             , "markbucciarelli.com"
             , System.getenv("BLOG_CERT_ARN")
             );

    public CertificateRequestStack(final Construct scope, final String id) {
        this(scope, id, null);
    }

    public CertificateRequestStack(final Construct scope, final String id, final StackProps props) {

        super(scope, id, props);

        Certificate.Builder.create(this, BLOG_CERT.id())
            .domainName(BLOG_CERT.domain())
            .build();
    }

}
