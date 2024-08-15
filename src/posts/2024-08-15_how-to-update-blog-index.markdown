August 15, 2024

tags: runbook mkws


runbook: Update mkws blog index layout
===============================

How to edit the mix of shell and HTML used by mkws to create the
static index page on my blog homepage.

For example, instead of listing the top 15 posts, I changed the
index to list all posts and to group them by year.


1. **EXTRACT** existing shell code
   
   ```
   awk 'BEGIN {p=0} /^#!/ {p=!p; next} p==1 {print;next} {printf "echo \"%s\"\n", $0}' \
   < src/index.upphtml \
   | sed 's;posts/;work/posts/;' \
   > t.sh
   ```

2. **VERIFY** extracted code prints the generated HTML to stdout without error.
   
   ```
   chmod +x t.sh
   make
   ./t.sh
   ```

3. **EDIT** `t.sh` until generated HTML matches what you want.

4. Manually **UPDATE** `src/index.upphtml` with updated logic and HTML output.

5. **GENERATE** new index.
   
   `make`

6. **CONFIRM** generated HTML is correct.
   
   `open work/index.html`

7. **DEPLOY** new index.
   
   ```
   aws sso login --profile blog
   make deploy
   ```

8. **ADD** cache invalidation to the `/` path in the CloudFront distribution.
