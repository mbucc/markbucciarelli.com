package com.markbucciarelli;

import software.amazon.awscdk.Stack;
import software.amazon.awscdk.StackProps;
import software.amazon.awscdk.services.certificatemanager.Certificate;
import software.constructs.Construct;

public class


CertificateRequestStack extends Stack {

    public static final String DEVBLOG_DOMAIN_NAME = "devblog.markbucciarelli.com";
    public static final String DEVBLOG_CERT_ID = "devblog-cert";

    public CertificateRequestStack(final Construct scope, final String id) {
        this(scope, id, null);
    }

    public CertificateRequestStack(final Construct scope, final String id, final StackProps props) {

        super(scope, id, props);

        //
        //                  Request a certificate.
        //
        //                  Running this stack stayed at "CREATE_IN_PROGRESS" state (1/3)
        //                  for around 15 minutes.  After I killed it with a Ctrl-C, the
        //                  cert request was there.  In addition, I had approval emails
        //                  that were sent when the task kicked off.
        //
        //                  Per this bug report (https://github.com/aws/aws-cdk/issues/2914),
        //                  it looks like CDK will hang until the cert is validated.
        //
        //                  The validation emails (for the domain devblog.markbucciarelli.com)
        //                  were sent to:
        //
        //                      - webmaster@markbucciarelli.com
        //                      - hostmaster@
        //                      - administrator@
        //                      - admin@
        //                      - postmaster@
        //


        Certificate.Builder.create(this, DEVBLOG_CERT_ID)
            .domainName(DEVBLOG_DOMAIN_NAME)
            .build();
    }

}
