package com.markbucciarelli;

import software.amazon.awscdk.App;
import software.amazon.awscdk.Environment;
import software.amazon.awscdk.StackProps;

public class BlogApp {
    public static void main(final String[] args) {
        App app = new App();

        new BlogStack(app, "BlogStack", StackProps.builder()

                // CDK sets these environmental variables from the AWS profile; either --profile
                // or the default.
                //      If you don't specify 'env', this stack will be environment-agnostic.
                // Account/Region-dependent features and context lookups will not work,
                // but a single synthesized template can be deployed anywhere.
                // For more info, see https://docs.aws.amazon.com/cdk/latest/guide/environments.html
                .env(Environment.builder()
                        .account(System.getenv("CDK_DEFAULT_ACCOUNT"))
                        .region(System.getenv("CDK_DEFAULT_REGION"))
                        .build())

                .build());

        new CertificateRequestStack(app, "CertificateRequestStack", StackProps.builder()
            .env(Environment.builder()
                .account(System.getenv("CDK_DEFAULT_ACCOUNT"))
                .region(System.getenv("CDK_DEFAULT_REGION"))
                .build())
            .build());

        app.synth();
    }
}

